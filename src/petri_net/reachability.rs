//! This module performs reachability analysis on a Petri net

use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use super::{Arc, CapacityFn, Marking, MarkingFn, PetriNet, PlaceId, TransitionId, WeightFn};

/// A unique ID for a marking in the reachability graph
#[derive(Debug, Clone, Copy)]
pub struct MarkingId(usize);

/// Marking IDs are displayed as M followed by the ID padded by 3 leading 0s, e.g. M000, M001, M002, ...
impl Display for MarkingId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "M{}", self.0)
    }
}

/// A continuation is a transition that can be fired from a marking, resulting in a new marking
/// If the resulting marking has been seen before, the continuation might be a loop
#[derive(Debug, Clone, Copy)]
pub enum Continuation {
    Unseen(TransitionId, MarkingId),
    Seen(TransitionId, MarkingId),
}

/// Display a continuation as T->M, e.g. T0->M000, T1->M001, ...
impl Display for Continuation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Continuation::Unseen(transition, marking) => write!(f, "{}->{}", transition, marking),
            Continuation::Seen(transition, marking) => write!(f, "{}->{}", transition, marking),
        }
    }
}

/// Describes the maximum number of tokens stored on a place at any point in time
#[derive(Debug, Clone)]
pub struct Boundedness(HashMap<PlaceId, usize>);

impl Boundedness {
    /// Returns the boundedness of a place, assuming 0 if the place is not in the map
    fn from_net<C: CapacityFn, W: WeightFn>(net: &PetriNet<C, W>) -> Self {
        let mut boundedness = HashMap::with_capacity(net.places.len());
        // Initialize the boundedness of all places with 0
        for place in &net.places {
            boundedness.insert(place.id, 0);
        }
        // Update the boundedness with the initial marking
        for (place_id, initial_tokens) in net.initial_marking.0.iter() {
            boundedness.insert(*place_id, *initial_tokens);
        }
        Self(boundedness)
    }
    /// Updates the boundedness of a place if the new value is greater than the old value
    fn update(&mut self, place_id: PlaceId, tokens: usize) {
        self.0.entry(place_id).and_modify(|t| {
            if *t < tokens {
                *t = tokens;
            }
        }).or_insert(tokens);
    }
}

#[allow(dead_code)]
/// Transition liveness classes describe how many times a transition fires
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Live {
    /// Can never fire
    L0,
    /// Fires a finite and deterministic number of times
    L1,
    /// Fires a finite but non-deterministic number of times
    L2,
    /// Fires a non-deterministically finite or infinite number of times
    L3,
    /// Fires a deterministically infinite number of times
    L4,
}

/// Intermediate struct to build the liveness of transitions
#[derive(Debug, Clone)]
struct LivenessMap {
    liveness: HashMap<TransitionId, Live>,
}

impl LivenessMap {
    /// Create a new liveness map from a list of transitions
    fn new<C: CapacityFn, W: WeightFn>(net: &PetriNet<C, W>) -> Self {
        let mut liveness = HashMap::with_capacity(net.transitions.len());
        // Initialize all transitions with L0
        for transition in &net.transitions {
            liveness.insert(transition.id, Live::L0);
        }
        Self { liveness }
    }
    /// Updates the liveness of a transition if the new value is greater than the old value
    fn update(&mut self, transition_id: TransitionId, live: Live) {
        self.liveness.entry(transition_id).and_modify(|current| {
            if *current < live {
                *current = live;
            }
        }).or_insert(live);
    }
}

/// Categorizes transitions by liveness
/// The index of the array corresponds to the liveness class, e.g. 0 -> L0, 1 -> L1, ...
#[derive(Debug, Clone, Default)]
pub struct Liveness {
    l: [Vec<TransitionId>; 5]
}

impl Liveness {
    /// Categorizes transitions into their respective liveness classes
    fn categorize(&mut self, map: LivenessMap) {
        for (transition_id, live) in map.liveness {
            // Safety: `live as usize` is always in the range 0..4
            self.l[live as usize].push(transition_id);
        }
    }
}

/// Liveness is displayed in the format L0(T1, T2); L1(T3, T4), ...
impl Display for Liveness {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn format_list(name: &'static str, list: &[TransitionId]) -> String {
            format!(
                "{} ({});",
                name,
                list.iter().map(|id| id.to_string()).collect::<Vec<_>>().join(", ")
            )
        }
        let mut print = Vec::with_capacity(5);
        if !self.l[0].is_empty() {
            print.push(format_list("L0", &self.l[0]));
        }
        if !self.l[1].is_empty() {
            print.push(format_list("L1", &self.l[1]));
        }
        if !self.l[2].is_empty() {
            print.push(format_list("L2", &self.l[2]));
        }
        if !self.l[3].is_empty() {
            print.push(format_list("L3", &self.l[3]));
        }
        if !self.l[4].is_empty() {
            print.push(format_list("L4", &self.l[4]));
        }
        write!(f, "{}", print.join(" "))
    }
}

/// A transition ID and the IDs of its input places and of its output places
/// This allows for easy checking of whether a transition can fire from a given marking
#[derive(Debug)]
struct TransitionIO {
    transition_id: TransitionId,
    inputs: Vec<PlaceId>,
    outputs: Vec<PlaceId>,
}

/// Holds some transition IO data and references to the capacity and weight functions
/// for efficient transition firing
#[derive(Debug)]
struct FiringFn<'net, C: CapacityFn, W: WeightFn> {
    transitions: Vec<TransitionIO>,
    capacities: &'net C,
    weights: &'net W,
}

impl<'net, C: CapacityFn, W: WeightFn> FiringFn<'net, C, W> {
    /// Create a new transition firing function for the given Petri net
    fn new(petri_net: &'net PetriNet<C, W>) -> Self {
        let mut transitions = Vec::with_capacity(petri_net.transitions.len());
        // For each transition in the net, collect its ID, input places, and output places
        for transition in &petri_net.transitions {
            let transition_id = transition.id;
            let mut inputs = Vec::new();
            let mut outputs = Vec::new();
            for arc in &petri_net.arcs {
                match arc {
                    Arc::PlaceTransition(source, target) if target == &transition.id => {
                        inputs.push(*source);
                    }
                    Arc::TransitionPlace(source, target) if source == &transition.id => {
                        outputs.push(*target);
                    }
                    _ => {}
                }
            }
            transitions.push(TransitionIO { transition_id, inputs, outputs });
        }
        Self {
            transitions,
            capacities: &petri_net.capacities,
            weights: &petri_net.weights,
        }
    }

    /// Fires all enabled transitions in the Petri net from the provided marking,
    /// and returns a list of the resulting markings.
    /// This attempts to fire all transitions, but silently fails for those that are not enabled.
    /// This function also updates the place boundedness and transition liveness.
    fn fire_transitions(
        &self,
        from: &Marking,
        boundedness: &mut Boundedness,
        liveness: &mut LivenessMap,
    ) -> Vec<(TransitionId, Marking)> {
        self.transitions.iter().filter_map(|TransitionIO { transition_id, inputs, outputs }| {
            // Create a clone of the start marking to modify
            let mut marking = from.clone();
            // Start by checking that all the input places have sufficient tokens to fire the transition
            inputs.iter().try_for_each(|source| {
                let current_tokens = marking.get(source);
                let token_requirement = self.weights.get(&Arc::PlaceTransition(*source, *transition_id));
                current_tokens.checked_sub(token_requirement)
                    .map(|new_tokens| marking.set(*source, new_tokens))
                    .ok_or(()) // Produce Ok if tokens were removed, Err if not enough tokens
            // Then check that all outputs have enough capacity to store the new tokens
            }).and_then(|_| outputs.iter().try_for_each(|target| {
                let current = marking.get(target);
                let output_tokens = self.weights.get(&Arc::TransitionPlace(*transition_id, *target));
                let capacity = self.capacities.get_or_default(target);
                capacity.checked_sub(output_tokens)
                    .filter(|&max_current_tokens| current <= max_current_tokens)
                    .map(|_| {
                        let new_tokens = current + output_tokens;
                        // If so, add the tokens to the target place
                        marking.set(*target, new_tokens);
                        // Since we are increasing tokens on a place, we need to update the boundedness
                        boundedness.update(*target, new_tokens);
                    }).ok_or(()) // Produce Ok if tokens were added, Err if not enough capacity
            // If the transition fired successfully, return its ID and the resulting marking
            })).ok().map(|_| {
                // This transition fired successfully, so it must be at least L1-live
                liveness.update(*transition_id, Live::L1);
                (*transition_id, marking)
            })
        }).collect() // Collect all successful firing attempts
    }
}

/// Struct for keeping track of the markings we have seen before and their IDs
#[derive(Debug, Default)]
struct Markings {
    markings: HashMap<Marking, MarkingId>,
}

impl Markings {
    /// Insert a new marking into the map and return its ID
    fn remember(&mut self, marking: Marking) -> MarkingId {
        let id = MarkingId(self.markings.len());
        self.markings.insert(marking, id);
        id
    }
    /// Get the ID of a marking, if it exists
    fn look_up(&self, marking: &Marking) -> Option<MarkingId> {
        self.markings.get(marking).copied()
    }
}

#[derive(Debug, Clone)]
pub struct IncidenceMatrix<'net, C: CapacityFn, W: WeightFn> {
    petri_net: &'net PetriNet<C, W>,
    matrix: Vec<Vec<usize>>
}

/// A reachability graph is a list of markings, each with a unique ID,
/// and each with a list of the transitions that can be fired from them and the IDs of the resulting markings
#[derive(Debug, Clone)]
pub struct ReachabilityAnalysis<'net, C: CapacityFn, W: WeightFn> {
    petri_net: &'net PetriNet<C, W>,
    pub graph: Vec<(MarkingId, Marking, Vec<Continuation>)>,
    pub boundedness: Boundedness,
    pub liveness: Liveness,
}

impl<C: CapacityFn, W: WeightFn> PetriNet<C, W> {
    /// Compute the incidence matrix for detecting unboundedness
    pub fn incidence_matrix(&self) -> IncidenceMatrix<'_, C, W> {
        let matrix: Vec<Vec<usize>> = Vec::with_capacity(self.places.len());
        for _p in &self.places {
            todo!()
        }
        IncidenceMatrix {
            petri_net: self,
            matrix
        }
    }
    /// Perform a reachability analysis on the Petri net
    pub fn reachability_analysis(&self) -> ReachabilityAnalysis<'_, C, W> {
        let mut analysis = ReachabilityAnalysis::new(self);
        let mut liveness = LivenessMap::new(self);
        let mut markings = Markings::default();
        let id = markings.remember(self.initial_marking.clone());
        let firing_fn = FiringFn::new(self);
        let mut queue = VecDeque::new();
        queue.push_back((
            id,
            self.initial_marking.clone(),
            firing_fn.fire_transitions(&self.initial_marking, &mut analysis.boundedness, &mut liveness),
        ));
        while let Some((source_marking_id, source_marking, branches_to_explore)) = queue.pop_front() {
            let mut continuations = Vec::with_capacity(branches_to_explore.len());
            for (transition_id, resulting_marking) in branches_to_explore {
                if let Some(existing_marking_id) = markings.look_up(&resulting_marking) {
                    // TODO: Fix loop detection (find path from marking to itself)
                    // TODO: Detect L3/L4 transitions
                    continuations.push(Continuation::Seen(transition_id, existing_marking_id));
                } else {
                    let new_marking_id = markings.remember(resulting_marking.clone());
                    let new_branches = firing_fn.fire_transitions(&resulting_marking, &mut analysis.boundedness, &mut liveness);
                    continuations.push(Continuation::Unseen(transition_id, new_marking_id));
                    queue.push_back((new_marking_id, resulting_marking, new_branches));
                }
            }
            analysis.graph.push((source_marking_id, source_marking, continuations));
        }
        analysis.liveness.categorize(liveness);
        analysis
    }
}

/// How to interpret a deadlock in the reachability graph
/// A final (desired) deadlock is a marking with only one token on a place with no outgoing arcs
/// Any other deadlock is a non-final (undesired) deadlock
#[derive(Debug, Clone)]
pub enum DeadlockInterpretation {
    Final,
    Deadlock
}

/// Display a deadlock interpretation as "final" or "deadlock"
impl Display for DeadlockInterpretation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DeadlockInterpretation::Final => write!(f, "final"),
            DeadlockInterpretation::Deadlock => write!(f, "deadlock"),
        }
    }
}

impl<'net, C: CapacityFn, W: WeightFn> ReachabilityAnalysis<'net, C, W> {
    /// Create a new reachability analysis for the given Petri net
    fn new(petri_net: &'net PetriNet<C, W>) -> Self {
        Self {
            petri_net,
            graph: Vec::new(),
            boundedness: Boundedness::from_net(petri_net),
            liveness: Liveness::default(),
        }
    }
    /// Returns a list of deadlocked markings and their interpretation
    fn deadlocks(&self) -> Vec<(MarkingId, DeadlockInterpretation)> {
        self.graph.iter().filter_map(|(marking_id, marking, continuations)| {
            if !continuations.is_empty() {
                return None; // Not a deadlock because there exists a continuation out of this marking
            }
            // Interpret the deadlock
            let interpretation = {
                // Find all places with tokens
                let places_with_tokens: Vec<_> = marking.0.iter()
                    .filter(|(_, &tokens)| tokens > 0)
                    .collect();
                // A final deadlock has only one place with one token and no outgoing arcs
                match places_with_tokens.as_slice() {
                    // A final deadlock must match this pattern:
                    [(place_id, 1)] if !self.petri_net.arcs.iter().any(|arc| {
                        // There is no arc with this place as source
                        matches!(arc, Arc::PlaceTransition(source, _) if source == *place_id)
                    }) => DeadlockInterpretation::Final,
                    // Otherwise, we have a regular deadlock
                    _ => DeadlockInterpretation::Deadlock,
                }
            };
            Some((*marking_id, interpretation))
        }).collect()
    }
    /// Returns the maximum boundedness of any place in the Petri net
    fn boundedness(&self) -> usize {
        self.boundedness.0.values().copied().max().unwrap_or(0)
    }
    /// Returns true if every place in the Petri net is 1-bounded
    fn is_safe(&self) -> bool {
        self.boundedness.0.iter().all(|(_, &tokens)| tokens == 1)
    }
    /// Returns true if every transition in the Petri net is L4-live
    fn is_live(&self) -> bool {
        self.liveness.l[0].is_empty()
            && self.liveness.l[1].is_empty()
            && self.liveness.l[2].is_empty()
            && self.liveness.l[3].is_empty()
            && !self.liveness.l[4].is_empty()
    }
    /// Returns true if at least one transition in the Petri net is L4-live
    /// and at least one transition is not L4-live
    fn is_quasi_live(&self) -> bool {
        !self.is_live() && !self.liveness.l[4].is_empty()
    }
    /// Returns the markings from which we can reach a previous marking,
    /// forming a loop in the reachability graph
    fn loops(&self) -> Vec<MarkingId> {
        self.graph.iter().flat_map(|(marking_id, _, continuations)| {
            continuations.iter().filter_map(|continuation| {
                match continuation {
                    Continuation::Seen(_, _) => Some(*marking_id),
                    _ => None,
                }
            })
        }).collect()
    }
    /// Returns true if all places had at least one token at some point,
    /// and all transitions fired at least once
    fn is_sound(&self) -> bool {
        self.liveness.l[0].is_empty() && self.boundedness.0.values().all(|&tokens| tokens > 0)
    }
}

/// Display a reachability analysis as a table
impl<'net, C: CapacityFn, W: WeightFn> Display for ReachabilityAnalysis<'net, C, W> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Print all transitions and their names
        for transition in &self.petri_net.transitions {
            writeln!(f, "{} ... {}", transition.id, transition.name)?;
        }
        writeln!(f)?;

        // Print the top row of the reachability graph, starting with "M"
        write!(f, "{:<7}", "M")?;
        // ...and then all place IDs
        for place in &self.petri_net.places {
            write!(f, "{:<5}", place.id.to_string())?;
        }
        // ...and then "Transitions"
        writeln!(f, "Transitions")?;

        // Print the body of the reachability graph
        for (marking_id, marking, continuations) in &self.graph {
            // Print the ID of this row's marking
            write!(f, "{:<7}", marking_id.to_string())?;
            // For each place, print the number of tokens on that place in this marking
            for place in &self.petri_net.places {
                write!(f, "{:<5}", marking.get(&place.id))?;
            }
            // Print the transitions which can fire from this marking and the markings they lead to
            let continuations = continuations
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(f, "{}", continuations)?;
        }
        writeln!(f)?;

        writeln!(f, "Interpretation")?;
        for (marking_id, interpretation) in self.deadlocks() {
            writeln!(f, "{}: {}", marking_id, interpretation)?;
        }
        writeln!(f, "Bounded: {}-Bounded", self.boundedness())?;
        writeln!(f, "Safe: {}", self.is_safe())?;
        writeln!(f, "Live: {}", self.is_live())?;
        writeln!(f, "Quasi-Live: {}", self.is_quasi_live())?;
        writeln!(f, "Sound: {}", self.is_sound())?;
        writeln!(f, "Liveness: {}", self.liveness)?;
        writeln!(f, "Loops: {}", self.loops().into_iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .join(", "))?;
        writeln!(f, "Soundness: {}", self.is_sound())?;
        Ok(())
    }
}