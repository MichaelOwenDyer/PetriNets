//! This module defines the data structures for BPMN diagrams and provides a parser for BPMN 2.0 XML files.

mod parse;

/// Supported BPMN element types
#[derive(Debug, Clone)]
pub enum ElementType {
    Task,
    StartEvent,
    EndEvent,
    ParallelGateway,
    ExclusiveGateway,
}

#[derive(Debug, Clone)]
pub struct IncomingEdge {
    /// The ID of the source element
    pub source_id: String,
    /// The writing on the edge (currently lost in the conversion to Petri net)
    pub inscription: String,
}

/// A BPMN element has a unique ID, a name, a type, and a list of incoming edges
#[derive(Debug, Clone)]
pub struct BpmnElement {
    pub id: String,
    pub name: String,
    pub element_type: ElementType,
    pub incoming_edges: Vec<IncomingEdge>,
}

/// A BPMN diagram consists of a unique ID, and a list of elements
#[derive(Debug, Clone)]
pub struct Bpmn {
    pub id: String,
    pub elements: Vec<BpmnElement>,
}