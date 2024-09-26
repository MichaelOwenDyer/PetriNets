//! This module provides serialization and deserialization for Petri nets in PNML format.

use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use super::{CapacityFn, MarkingFn, PetriNet, WeightFn};

const PNML_NAMESPACE: &str = "http://www.pnml.org/version-2009/grammar/pnmlcoremodel";

#[derive(Debug, Serialize, Deserialize)]
struct Name {
    #[serde(rename = "text")]
    text: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct InitialMarking {
    #[serde(rename = "text")]
    amount: usize,
}

impl From<super::Tokens> for Option<InitialMarking> {
    fn from(tokens: super::Tokens) -> Self {
        if tokens.0 == 0 {
            None
        } else {
            Some(InitialMarking { amount: tokens.0 })
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Capacity {
    #[serde(rename = "text")]
    amount: usize,
}

impl From<super::Capacity> for Capacity {
    fn from(capacity: super::Capacity) -> Self {
        Capacity { amount: capacity.0 }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Place {
    #[serde(rename = "@id")]
    id: String,
    #[serde(rename = "name")]
    name: Name,
    #[serde(rename = "initialMarking")]
    initial_marking: Option<InitialMarking>,
    #[serde(rename = "capacity")]
    capacity: Option<Capacity>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Transition {
    #[serde(rename = "@id")]
    id: String,
    #[serde(rename = "name")]
    name: Name,
}

#[derive(Debug, Serialize, Deserialize)]
struct Inscription {
    #[serde(rename = "inscription")]
    inscription: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Weight {
    #[serde(rename = "text")]
    amount: usize,
}

impl From<super::Weight> for Weight {
    fn from(weight: super::Weight) -> Self {
        Weight { amount: weight.0 }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Arc {
    #[serde(rename = "@id")]
    id: String,
    #[serde(rename = "@source")]
    source: String,
    #[serde(rename = "@target")]
    target: String,
    #[serde(rename = "name")]
    name: Inscription,
    #[serde(rename = "weight")]
    weight: Option<Weight>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum PnmlElement {
    Place(Place),
    Transition(Transition),
    Arc(Arc),
}

#[derive(Debug, Serialize, Deserialize)]
struct Net {
    #[serde(rename = "@id")]
    id: String,
    #[serde(rename = "@type")]
    r#type: String,
    #[serde(rename = "$value")]
    elements: Vec<PnmlElement>,
}

/// Internal representation of a PNML file.
/// This is the format that the PNML file is serialized to and deserialized from.
#[derive(Debug, Serialize, Deserialize)]
pub struct Pnml {
    #[serde(rename = "net")]
    net: Net,
}

/// Display a Pnml file as XML
impl Display for Pnml {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut ser = quick_xml::se::Serializer::new(f);
        ser.indent(' ', 2);
        self.serialize(ser).map_err(|_| std::fmt::Error)
    }
}

/// Convert a parsed PNML file to a Petri net
impl<C, W> From<Pnml> for PetriNet<C, W>
where
    C: CapacityFn + FromIterator<(super::PlaceId, super::Capacity)>,
    W: WeightFn + FromIterator<(super::Arc, super::Weight)>,
{
    fn from(pnml: Pnml) -> Self {
        let mut places = Vec::new();
        let mut transitions = Vec::new();
        let mut arcs = Vec::new();
        let mut capacities = Vec::new();
        let mut weights = Vec::new();
        let mut initial_marking = super::Marking::default();

        for element in pnml.net.elements {
            match element {
                PnmlElement::Place(place) => {
                    if let Ok(id) = place.id.parse() {
                        if let Some(capacity) = place.capacity {
                            capacities.push((id, super::Capacity(capacity.amount)));
                        }
                        if let Some(marking) = place.initial_marking {
                            initial_marking.set(id, super::Tokens(marking.amount));
                        }
                        places.push(super::Place { id, name: place.name.text });
                    }
                },
                PnmlElement::Transition(transition) => {
                    if let Ok(id) = transition.id.parse() {
                        transitions.push(super::Transition { id, name: transition.name.text })
                    }
                },
                PnmlElement::Arc(pnml) => {
                    // Parse the source and target IDs to determine the arc type
                    let arc = if let (Ok(source), Ok(target)) = (pnml.source.parse(), pnml.target.parse()) {
                        super::Arc::PlaceTransition(source, target)
                    } else if let (Ok(source), Ok(target)) = (pnml.source.parse(), pnml.target.parse()) {
                        super::Arc::TransitionPlace(source, target)
                    } else {
                        continue // Skip arcs with invalid source or target IDs. Might change to an error later
                    };
                    if let Some(weight) = pnml.weight {
                        weights.push((arc, super::Weight(weight.amount)));
                    }
                    arcs.push(arc);
                }
            }
        }
        
        let id = pnml.net.id;
        let capacities = capacities.into_iter().collect();
        let weights = weights.into_iter().collect();

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

/// Convert a Petri net to a PNML file
impl<C: CapacityFn, W: WeightFn> From<PetriNet<C, W>> for Pnml {
    fn from(net: PetriNet<C, W>) -> Self {
        let mut elements = Vec::new();
        for place in net.places {
            let id = format!("{}", place.id);
            let name = Name { text: place.name };
            let initial_marking = net.initial_marking.get(&place.id).into();
            let capacity = net.capacities.get(&place.id).map(Into::into);
            elements.push(PnmlElement::Place(Place { id, name, initial_marking, capacity }));
        }
        for transition in net.transitions {
            let id = format!("{}", transition.id);
            let name = Name { text: transition.name };
            elements.push(PnmlElement::Transition(Transition { id, name }));
        }
        for arc in net.arcs {
            let (source, target) = match arc {
                super::Arc::PlaceTransition(source, target) => (format!("{}", source), format!("{}", target)),
                super::Arc::TransitionPlace(source, target) => (format!("{}", source), format!("{}", target)),
            };
            let id = format!("a_{}_{}", source, target);
            let name = Inscription { inscription: String::new() }; // TODO: Add support for arc inscriptions in the petri_net module
            let weight = net.weights.get(&arc).map(Into::into);
            elements.push(PnmlElement::Arc(Arc { id, source, target, name, weight }));
        }
        Pnml {
            net: Net {
                id: net.id,
                r#type: PNML_NAMESPACE.to_string(),
                elements
            }
        }
    }
}