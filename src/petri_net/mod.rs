//! This module defines the data structures for Petri nets
//! and provides a conversion from BPMN diagrams to Petri nets.
//!
//! The following paper is used as a reference and inspiration for the implementation:
//! [T. Murata. Petri nets: Properties, Analysis and Applications. Proceedings of the IEEE, 77(4):541–580, 1989.](http://www2.ing.unipi.it/~a009435/issw/extra/murata.pdf)
//!
//! TODO:
//!
//! - [ ] Use a tree structure for tracking marking sequences and detecting loops / unboundedness

mod reachability;
mod pnml;

pub use pnml::Pnml;

use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;
use std::str::FromStr;

use reachability::{Marking, MarkingFn, Tokens};

/// An ID for a place in the Petri net
/// This is a newtype around `usize` to ensure that we can't accidentally mix up place and transition IDs
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceId(usize);

/// An ID for a transition in the Petri net
/// This is a newtype around `usize` to ensure that we can't accidentally mix up place and transition IDs
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TransitionId(usize);

/// PlaceIDs are displayed as P followed by the ID, e.g. P0, P1, P2, ...
impl Display for PlaceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "P{}", self.0)
    }
}

/// TransitionIDs are displayed as T followed by the ID, e.g. T0, T1, T2, ...
impl Display for TransitionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "T{}", self.0)
    }
}

/// If there is an improperly formatted ID in the XML file, we return an error
#[derive(Debug)]
pub enum IdParseError {
    InvalidPrefix(char), // The ID does not start with the expected char prefix
    ParseIntError(std::num::ParseIntError), // The ID could not be parsed as an integer
}

/// Parse following the pattern P0, P1, P2, ...
impl FromStr for PlaceId {
    type Err = IdParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.strip_prefix('P')
            .ok_or(IdParseError::InvalidPrefix('P'))?
            .parse()
            .map(PlaceId)
            .map_err(IdParseError::ParseIntError)
    }
}

/// Parse following the pattern T0, T1, T2, ...
impl FromStr for TransitionId {
    type Err = IdParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.strip_prefix('T')
            .ok_or(IdParseError::InvalidPrefix('T'))?
            .parse()
            .map(TransitionId)
            .map_err(IdParseError::ParseIntError)
    }
}

/// A place has a unique ID and a name
#[derive(Debug, Clone)]
pub struct Place {
    id: PlaceId,
    name: String,
}

/// A transition has a unique ID and a name
#[derive(Debug, Clone)]
pub struct Transition {
    id: TransitionId,
    name: String,
}

/// An arc connects a place to a transition or a transition to a place
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arc {
    PlaceTransition(PlaceId, TransitionId), // Inputs to transitions
    TransitionPlace(TransitionId, PlaceId), // Outputs from transitions
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Capacity(pub usize); // TODO: Investigate whether it is worthwhile to use an enum variant for infinite capacity instead of usize::MAX

/// A capacity function which can be set for each place individually.
/// The standard capacity is 1 for EC nets and infinite for PT nets.
pub trait CapacityFn {
    const DEFAULT: Capacity;
    /// Get the capacity of the place if it is explicitly marked
    fn get(&self, _id: &PlaceId) -> Option<Capacity>;
    /// Get the capacity of the place or the default capacity if it is implicit
    fn get_or_default(&self, id: &PlaceId) -> Capacity {
        self.get(id).unwrap_or(Self::DEFAULT)
    }
}

/// A capacity function which returns a constant implicit capacity of 1 for all places
#[derive(Debug, Default, Clone)]
pub struct FixedCapacity<const N: usize = 1>;

impl<const N: usize> CapacityFn for FixedCapacity<N> {
    const DEFAULT: Capacity = Capacity(N);
    fn get(&self, _id: &PlaceId) -> Option<Capacity> {
        None
    }
}

impl<P: Into<PlaceId>, C: Into<Capacity>, const N: usize> FromIterator<(P, C)> for FixedCapacity<N> {
    /// FixedCapacity does not store any data, so we ignore the input and always return the same instance
    fn from_iter<I: IntoIterator<Item=(P, C)>>(_: I) -> Self {
        FixedCapacity
    }
}

/// A capacity function which can be set for each place individually.
/// The default capacity is generally infinite (usize::MAX), but can be set via N explicitly.
#[derive(Debug, Default, Clone)]
pub struct VariableCapacity<const N: usize = { usize::MAX }> {
    capacities: HashMap<PlaceId, Capacity, ahash::RandomState>,
}

impl<const N: usize> CapacityFn for VariableCapacity<N> {
    const DEFAULT: Capacity = Capacity(N);
    fn get(&self, id: &PlaceId) -> Option<Capacity> {
        self.capacities.get(id).copied()
    }
}

impl<P: Into<PlaceId>, C: Into<Capacity>, const N: usize> FromIterator<(P, C)> for VariableCapacity<N> {
    fn from_iter<I: IntoIterator<Item=(P, C)>>(iter: I) -> Self {
        let capacities = iter
            .into_iter()
            .map(|(id, capacity)| (id.into(), capacity.into()))
            .filter(|(_, capacity)| capacity.0 != N) // Filter out default capacities
            .collect();
        VariableCapacity { capacities }
    }
}

#[derive(Debug, Clone, Default, Copy, PartialEq, Eq)]
pub struct Weight(pub usize);

/// A weight function which can be set for each arc individually.
/// This affects how many tokens are consumed from a source place,
/// and how many tokens are added to a target place when a transition is fired.
pub trait WeightFn {
    const DEFAULT: Weight = Weight(1); // Default weight is generally 1 for any kind of petri net
    /// Get the weight of the arc if it is explicitly set
    fn get(&self, _arc: &Arc) -> Option<Weight>;
    /// Return the weight of the arc or 1 if it is not explicitly set
    fn get_or_default(&self, arc: &Arc) -> Weight {
        self.get(arc).unwrap_or(Self::DEFAULT)
    }
}

/// A weight function which returns a constant implicit weight of N (default: 1) for all arcs.
#[derive(Debug, Clone, Default)]
pub struct FixedWeight<const N: usize = 1>;

impl<const N: usize> WeightFn for FixedWeight<N> {
    const DEFAULT: Weight = Weight(N);
    fn get(&self, _arc: &Arc) -> Option<Weight> {
        None
    }
}

impl<A: Into<Arc>, W: Into<Weight>, const N: usize> FromIterator<(A, W)> for FixedWeight<N> {
    /// FixedWeight does not store any data, so we ignore the input and always return the same instance
    fn from_iter<I: IntoIterator<Item=(A, W)>>(_: I) -> Self {
        FixedWeight
    }
}

/// A weight function which can be set for each arc individually.
/// The default weight is generally 1, but can be set via N explicitly.
#[derive(Debug, Clone, Default)]
pub struct VariableWeight<const N: usize = 1> {
    weights: HashMap<Arc, Weight, ahash::RandomState>,
}

impl<const N: usize> WeightFn for VariableWeight<N> {
    const DEFAULT: Weight = Weight(N);
    fn get(&self, arc: &Arc) -> Option<Weight> {
        self.weights.get(arc).copied()
    }
}

impl<A: Into<Arc>, W: Into<Weight>, const N: usize> FromIterator<(A, W)> for VariableWeight<N> {
    fn from_iter<I: IntoIterator<Item=(A, W)>>(iter: I) -> Self {
        let weights = iter
            .into_iter()
            .map(|(arc, weight)| (arc.into(), weight.into()))
            .filter(|(_, weight)| weight.0 != N) // Filter out default weights
            .collect();
        VariableWeight { weights }
    }
}

/// A Petri net is a tuple P = (P, T, F, C, W, M_0) where
///     P is a set of places,
///     T is a set of transitions,
///     F is a set of arcs,
///     C is a capacity function,
///     W is a weight function,
///     M_0 is the initial marking.
/// TODO: Separate initial marking from the Petri net?
/// This struct is generic over the capacity function C and the weight function W.
#[derive(Debug, Clone, Default)]
pub struct PetriNet<C: CapacityFn, W: WeightFn> {
    pub id: String,
    pub places: Vec<Place>,
    pub transitions: Vec<Transition>,
    pub arcs: Vec<Arc>,
    pub capacities: C,
    pub weights: W,
    pub initial_marking: Marking,
}

/// An Event/Condition Net (EC Net) has a fixed capacity of 1 for all places and a fixed weight of 1 for all arcs
pub type ECNet = PetriNet<FixedCapacity<1>, FixedWeight<1>>;

/// A Place/Transition Net (PT Net) has settable, implicitly infinite capacities and settable, implicitly single weights
pub type PTNet = PetriNet<VariableCapacity<{ usize::MAX }>, VariableWeight<1>>;

impl<C, W> From<crate::Bpmn> for PetriNet<C, W>
where
    C: CapacityFn + Default,
    W: WeightFn + Default,
{
    fn from(bpmn: crate::Bpmn) -> Self {
        /// A Petri net element can be either a place or a transition
        #[derive(Debug, Clone)]
        enum Element {
            Place(Place),
            Transition(Transition),
        }

        /// In order to produce unique IDs for places and transitions, we use a factory
        /// This factory keeps track of the next available ID for each type of element
        #[derive(Debug, Default)]
        struct ElementFactory {
            next_place_id: usize,
            next_transition_id: usize,
        }

        impl ElementFactory {
            /// Create a new place with a unique ID and the provided name and initial marking
            fn new_place(&mut self, name: String) -> Place {
                let place = Place {
                    id: PlaceId(self.next_place_id),
                    name,
                };
                self.next_place_id += 1;
                place
            }
            /// Create a new transition with a unique ID and the provided name
            fn new_transition(&mut self, name: String) -> Transition {
                let transition = Transition {
                    id: TransitionId(self.next_transition_id),
                    name,
                };
                self.next_transition_id += 1;
                transition
            }
        }
        
        let mut factory = ElementFactory::default();
        let mut initial_marking = Marking::default();

        // Create petri net elements for each BPMN element so we can refer to them later
        let mut petri_net_elements = HashMap::with_capacity(bpmn.elements.len());
        use crate::bpmn::ElementType;
        for element in &bpmn.elements {
            let petri_net_element = match &element.element_type {
                // Start events become places with an initial marking of 1
                ElementType::StartEvent => {
                    let place = factory.new_place(element.name.clone());
                    // Note down the initial marking of the start place
                    initial_marking.set(place.id, Tokens(1));
                    Element::Place(place)
                }
                // End events and XOR gateways become places
                ElementType::EndEvent | ElementType::ExclusiveGateway => {
                    Element::Place(factory.new_place(element.name.clone()))
                },
                // Tasks and parallel gateways become transitions
                ElementType::Task | ElementType::ParallelGateway => {
                    Element::Transition(factory.new_transition(element.name.clone()))
                },
            };
            petri_net_elements.insert(element.id.clone(), petri_net_element);
        }

        // Can we come to a more accurate estimate of the number of places and transitions?
        let mut places = Vec::with_capacity(petri_net_elements.len());
        let mut transitions = Vec::with_capacity(petri_net_elements.len());
        let mut arcs = Vec::with_capacity(petri_net_elements.len());

        // Now we can iterate over the converted elements and create the petri net
        for element in &bpmn.elements {
            // Add the petri net element to the appropriate list and get its ID
            // The .expect() calls are safe because we just inserted the element into the map
            // Still, it would be nice to avoid them...
            let this = petri_net_elements.get(&element.id).expect("Element should exist");

            let inputs = element.incoming_edges
                .iter()
                .map(|edge| petri_net_elements.get(&edge.source_id).expect("Element should exist"))
                .collect::<Vec<_>>(); // Collect all incoming elements

            // Iterate over the BPMN elements which are inputs to this one, and figure out
            // how to connect them to the corresponding petri net element
            for input in inputs {
                match (input, this) {
                    (Element::Place(source), Element::Transition(target)) => {
                        arcs.push(Arc::PlaceTransition(source.id, target.id));
                    }
                    (Element::Place(source), Element::Place(target)) => {
                        // Create a silent transition to connect via
                        let tau = factory.new_transition(String::new());
                        arcs.push(Arc::PlaceTransition(source.id, tau.id));
                        arcs.push(Arc::TransitionPlace(tau.id, target.id));
                        transitions.push(tau);
                    }
                    (Element::Transition(source), Element::Transition(target)) => {
                        // This is an oversimplification.
                        // This assumes that any connected BPMN elements which become transitions
                        // (e.g. tasks and parallel gateways) should be connected via a new place.
                        // In some cases though, all source transitions should point into a shared
                        // buffer place, so as to require only one of them to fire in order to enable
                        // the target transition, rather than requiring all of them to fire.
                        // TODO: Investigate when to use shared buffer places and when to use separate buffer places
                        let buffer = factory.new_place(String::new());
                        arcs.push(Arc::TransitionPlace(source.id, buffer.id));
                        arcs.push(Arc::PlaceTransition(buffer.id, target.id));
                        places.push(buffer);
                    }
                    (Element::Transition(source), Element::Place(target)) => {
                        arcs.push(Arc::TransitionPlace(source.id, target.id));
                    }
                }
            }
        }

        // Put all elements into the net
        for (_, element) in petri_net_elements {
            match element {
                Element::Place(place) => places.push(place),
                Element::Transition(transition) => transitions.push(transition),
            }
        }

        // Sort the places and transitions by ID for later convenience
        places.sort_unstable_by_key(|p| p.id);
        transitions.sort_unstable_by_key(|t| t.id);

        let id = bpmn.id.clone();
        let capacities = C::default();
        let weights = W::default();

        PetriNet {
            id,
            places,
            transitions,
            arcs,
            capacities,
            weights,
            initial_marking,
        }
    }
}