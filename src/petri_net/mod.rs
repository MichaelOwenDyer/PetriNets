//! This module defines the data structures for Petri nets
//! and provides a conversion from BPMN diagrams to Petri nets.

mod reachability;
mod parse;

use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::str::FromStr;
use serde::Serialize;

use crate::bpmn::{Bpmn, ElementType as B};

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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "P{}", self.0)
    }
}

/// TransitionIDs are displayed as T followed by the ID, e.g. T0, T1, T2, ...
impl Display for TransitionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "T{}", self.0)
    }
}

/// If there is an improperly formatted ID in the XML file, we return an error
#[allow(unused)]
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
    PlaceTransition(PlaceId, TransitionId),
    TransitionPlace(TransitionId, PlaceId),
}

/// A marking function is a mapping from place IDs to the number of tokens in each place
/// It is used to keep track of the current state of the Petri net
pub trait MarkingFn: Clone + Eq + Hash {
    /// Get the marking at a place
    fn get(&self, id: &PlaceId) -> usize;
    /// Set the marking at a place
    fn set(&mut self, id: PlaceId, marking: usize);
}

/// A marking function which is implemented as a BTreeMap (due to its consistent ordering and hashing properties)
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Marking(BTreeMap<PlaceId, usize>);

impl MarkingFn for Marking {
    fn get(&self, id: &PlaceId) -> usize {
        // If the place is not in the marking, we assume it has 0 tokens
        self.0.get(id).copied().unwrap_or(0)
    }
    fn set(&mut self, id: PlaceId, marking: usize) {
        // If the marking is 0, we remove the place from the marking
        // We only store places with a marking > 0
        if marking == 0 {
            self.0.remove(&id);
        } else {
            self.0.insert(id, marking);
        }
    }
}

/// A capacity function which can be set for each place individually.
/// The standard capacity is 1 for EC nets and infinite for PT nets.
pub trait CapacityFn {
    const DEFAULT: usize;
    /// Get the capacity of the place if it differs from the default capacity
    fn get(&self, _id: &PlaceId) -> Option<usize> {
        None
    }
    /// Get the capacity of the place or the default capacity if it is not set
    fn get_or_default(&self, id: &PlaceId) -> usize {
        self.get(id).unwrap_or(Self::DEFAULT)
    }
    /// Set the capacity of the place, if supported. This is a no-op for EC nets.
    fn set(&mut self, _id: PlaceId, _capacity: usize) {

    }
}

/// A capacity function which returns a constant capacity of N for all places
#[derive(Debug, Default, Clone)]
pub struct ConstantCapacity<const N: usize>;

impl<const N: usize> CapacityFn for ConstantCapacity<N> {
    const DEFAULT: usize = N;
}

/// A capacity function which can be set for each place individually.
/// The default capacity is the const type parameter `N`.
#[derive(Debug, Default, Clone)]
pub struct VariableCapacity<const N: usize> {
    capacities: HashMap<PlaceId, usize>,
}

impl<const N: usize> CapacityFn for VariableCapacity<N> {
    const DEFAULT: usize = N;
    fn get(&self, id: &PlaceId) -> Option<usize> {
        self.capacities.get(id).copied()
    }
    fn set(&mut self, id: PlaceId, capacity: usize) {
        if capacity == N {
            self.capacities.remove(&id);
        } else {
            self.capacities.insert(id, capacity);
        }
    }
}

/// A weight function which can be set for each arc individually.
/// This affects how many tokens are consumed from a source place,
/// and how many tokens are added to a target place when a transition is fired.
pub trait WeightFn {
    /// Get the weight of the arc
    fn get(&self, _arc: &Arc) -> usize;
    /// Set the weight of the arc, if supported
    fn set(&mut self, _arc: Arc, _weight: usize) {

    }
}

/// A weight function which returns a constant weight of N for all arcs.
#[derive(Debug, Default, Clone)]
pub struct ConstantWeight<const N: usize>;

impl<const N: usize> WeightFn for ConstantWeight<N> {
    fn get(&self, _arc: &Arc) -> usize {
        N
    }
}

/// A weight function which can be set for each arc individually.
/// The default weight is the const type parameter `DEFAULT`.
#[derive(Debug, Default, Clone)]
pub struct VariableWeight<const N: usize> {
    weights: HashMap<Arc, usize>,
}

impl<const N: usize> WeightFn for VariableWeight<N> {
    fn get(&self, arc: &Arc) -> usize {
        self.weights.get(arc).copied().unwrap_or(N)
    }
    fn set(&mut self, arc: Arc, weight: usize) {
        if weight == N {
            self.weights.remove(&arc);
        } else {
            self.weights.insert(arc, weight);
        }
    }
}

/// A Petri net is a tuple P = (P, T, F, C, W, M_0)
/// where P is a set of places, T is a set of transitions, F is a set of arcs,
/// C is a capacity function, W is a weight function, and M_0 is the initial marking.
///
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

/// An ECNet is a Petri net with a fixed capacity of 1 for all places and a fixed weight of 1 for all arcs
pub type ECNet = PetriNet<ConstantCapacity<1>, ConstantWeight<1>>;

/// A PTNet is a Petri net with variable capacities and weights (defaulting to infinite and 1, respectively)
pub type PTNet = PetriNet<VariableCapacity<{ usize::MAX }>, VariableWeight<1>>;

/// Display a PetriNet as PNML XML
impl<C, W> Display for PetriNet<C, W>
where
    C: CapacityFn + Clone,
    W: WeightFn + Clone,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut ser = quick_xml::se::Serializer::new(f);
        ser.indent(' ', 2);
        self.serialize(ser).map_err(|_| std::fmt::Error)
    }
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

/// A Petri net element can be either a place or a transition
#[derive(Debug, Clone)]
enum Element {
    Place(Place),
    Transition(Transition),
}

/// Convert from Bpmn to PetriNet
/// This conversion still has some issues, e.g. 
/// connecting tasks via many buffer places when there should only be one shared buffer place
impl<C, W> From<Bpmn> for PetriNet<C, W>
where
    C: CapacityFn + Default,
    W: WeightFn + Default,
{
    fn from(bpmn: Bpmn) -> Self {
        let mut factory = ElementFactory::default();
        let mut initial_marking = Marking::default();

        // Create a mapping from BPMN IDs to corresponding Petri net elements
        let mut petri_net_elements = HashMap::with_capacity(bpmn.elements.len());
        for element in &bpmn.elements {
            let petri_net_element = match &element.element_type {
                B::StartEvent => {
                    let place = factory.new_place(element.name.clone());
                    // Note down the initial marking of the start place
                    initial_marking.set(place.id, 1);
                    Element::Place(place)
                }
                B::EndEvent => Element::Place(factory.new_place(element.name.clone())),
                B::Task => Element::Transition(factory.new_transition(element.name.clone())),
                B::ParallelGateway => Element::Transition(factory.new_transition(element.name.clone())),
                B::ExclusiveGateway => Element::Place(factory.new_place(element.name.clone())),
            };
            petri_net_elements.insert(element.id.clone(), petri_net_element);
        }

        let mut places = Vec::with_capacity(petri_net_elements.len());
        let mut transitions = Vec::with_capacity(petri_net_elements.len());
        let mut arcs = Vec::with_capacity(petri_net_elements.len());

        // Now we can iterate over the converted elements and create the petri net
        for element in &bpmn.elements {
            // Add the petri net element to the appropriate list and get its ID
            let mut this = match petri_net_elements.get(&element.id) {
                Some(this) => this,
                None => continue, // Unknown element, skip (should not happen)
            };

            let inputs = element.incoming_edges
                .iter()
                .filter_map(|edge| petri_net_elements.get(&edge.source_id))
                .collect::<Vec<_>>(); // Collect all incoming elements

            for input in inputs {
                match (input, &mut this) {
                    (
                        Element::Place(source),
                        Element::Transition(target),
                    ) => {
                        // Connect directly
                        arcs.push(Arc::PlaceTransition(source.id, target.id));
                    }
                    (
                        Element::Place(source),
                        Element::Place(target),
                    ) => {
                        // Create a silent transition to connect via
                        let tau = factory.new_transition(String::new());
                        arcs.push(Arc::PlaceTransition(source.id, tau.id));
                        arcs.push(Arc::TransitionPlace(tau.id, target.id));
                        transitions.push(tau);
                    }
                    ( // TODO: Only tasks should behave like this, parallel gateways should not
                        Element::Transition(source),
                        Element::Transition(target),
                    ) => {
                        // Create a new buffer place to connect via
                        let buffer = factory.new_place(String::new());
                        arcs.push(Arc::TransitionPlace(source.id, buffer.id));
                        arcs.push(Arc::PlaceTransition(buffer.id, target.id));
                        places.push(buffer);
                    }
                    (
                        Element::Transition(source),
                        Element::Place(target),
                    ) => {
                        // Connect directly
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
            initial_marking,
            capacities,
            weights,
        }
    }
}