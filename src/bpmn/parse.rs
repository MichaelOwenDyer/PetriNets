//! This module is responsible for parsing BPMN 2.0 XML files into a BPMN diagram data structure.

use std::collections::HashMap;

use serde::{Deserialize, Deserializer};

use crate::bpmn::{Bpmn, BpmnElement, ElementType, IncomingEdge};

/// We just need the ID and name of each element
/// This macro generates structs with these fields
macro_rules! def_bpmn_struct {
    ($($name:ident),*) => {$(
        #[derive(Debug, Deserialize)]
        struct $name {
            #[serde(rename = "@id")]
            id: String,
            #[serde(rename = "@name")]
            name: String,
        }
    )*};
}

def_bpmn_struct!(
    StartEvent,
    EndEvent,
    Task,
    SendTask,
    ReceiveTask,
    UserTask,
    ServiceTask,
    ManualTask,
    BusinessRuleTask,
    ScriptTask,
    ExclusiveGateway,
    ParallelGateway
);

/// A sequence flow connects a source element to a target element
#[derive(Debug, Deserialize)]
struct SequenceFlow {
    #[serde(rename = "@name")]
    inscription: String,
    #[serde(rename = "@sourceRef")]
    source_id: String,
    #[serde(rename = "@targetRef")]
    target_id: String,
}

/// A BPMN process consists of a unique ID and a list of start events, end events,
/// tasks, gateways, and sequence flows
#[derive(Debug, Deserialize)]
struct Process {
    #[serde(rename = "@id")]
    id: String,
    #[serde(rename = "startEvent", default)]
    start_events: Vec<StartEvent>,
    #[serde(rename = "endEvent", default)]
    end_events: Vec<EndEvent>,
    #[serde(rename = "task", default)]
    tasks: Vec<Task>,
    #[serde(rename = "sendTask", default)]
    send_tasks: Vec<SendTask>,
    #[serde(rename = "receiveTask", default)]
    receive_tasks: Vec<ReceiveTask>,
    #[serde(rename = "userTask", default)]
    user_tasks: Vec<UserTask>,
    #[serde(rename = "serviceTask", default)]
    service_tasks: Vec<ServiceTask>,
    #[serde(rename = "manualTask", default)]
    manual_tasks: Vec<ManualTask>,
    #[serde(rename = "businessRuleTask", default)]
    business_rule_tasks: Vec<BusinessRuleTask>,
    #[serde(rename = "scriptTask", default)]
    script_tasks: Vec<ScriptTask>,
    #[serde(rename = "exclusiveGateway", default)]
    exclusive_gateways: Vec<ExclusiveGateway>,
    #[serde(rename = "parallelGateway", default)]
    parallel_gateways: Vec<ParallelGateway>,
    #[serde(rename = "sequenceFlow", default)]
    sequence_flows: Vec<SequenceFlow>,
}

/// The root element of a BPMN 2.0 XML file.
/// This is the type we serialize the XML file into, and then convert into a BPMN diagram.
#[derive(Debug, Deserialize)]
struct BpmnXml {
    #[serde(rename = "process")]
    process: Process,
}

/// Convert a serialized BPMN XML file into a BPMN diagram
impl From<BpmnXml> for Bpmn {
    fn from(file: BpmnXml) -> Self {
        macro_rules! insert {
            ($map:ident, $iter:expr, $element_type: expr) => {
                for element in $iter {
                    $map.insert(
                        element.id.clone(),
                        (element.name, $element_type, Vec::new()),
                    );
                }
            };
        }

        let mut map = HashMap::new();
        insert!(map, file.process.start_events, ElementType::StartEvent);
        insert!(map, file.process.end_events, ElementType::EndEvent);
        insert!(map, file.process.tasks, ElementType::Task);
        insert!(map, file.process.send_tasks, ElementType::Task);
        insert!(map, file.process.receive_tasks, ElementType::Task);
        insert!(map, file.process.user_tasks, ElementType::Task);
        insert!(map, file.process.service_tasks, ElementType::Task);
        insert!(map, file.process.manual_tasks, ElementType::Task);
        insert!(map, file.process.business_rule_tasks, ElementType::Task);
        insert!(map, file.process.script_tasks, ElementType::Task);
        insert!(map, file.process.exclusive_gateways, ElementType::ExclusiveGateway);
        insert!(map, file.process.parallel_gateways, ElementType::ParallelGateway);

        // Take the sequence flows and add them to the incoming edges of the target elements
        for sequence_flow in file.process.sequence_flows {
            if let Some((_, _, incoming_edges)) = map.get_mut(&sequence_flow.target_id) {
                incoming_edges.push(IncomingEdge {
                    source_id: sequence_flow.source_id,
                    inscription: sequence_flow.inscription,
                });
            }
        }

        // Convert the hashmap into a vector of BPMN elements
        let elements = map
            .into_iter()
            .map(|(id, (name, element_type, incoming_edges))| BpmnElement {
                id,
                name,
                element_type,
                incoming_edges,
            })
            .collect();
        Bpmn {
            id: file.process.id,
            elements,
        }
    }
}

/// To deserialize a BPMN diagram, we first deserialize the XML file into a BpmnXml struct
/// and then convert this struct into a Bpmn diagram
impl<'de> Deserialize<'de> for Bpmn {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        BpmnXml::deserialize(deserializer).map(Into::into)
    }
}

#[cfg(test)]
mod test {
    use std::error::Error;
    use std::fs::File;
    use std::io::BufReader;

    use super::*;

    #[test]
    fn test_deserialize() -> Result<(), Box<dyn Error>> {
        let file = File::open("../../models/Test Bpmn.bpmn").map(BufReader::new)?;
        let bpmn_file: BpmnXml = quick_xml::de::from_reader(file)?;
        println!("{:#?}", bpmn_file);
        Ok(())
    }
}
