use std::fs::File;
use std::io::BufReader;
use petri_nets::{Bpmn, ECNet, Error, Pnml, parse_xml};

fn main() -> Result<(), Error> {
    // Read the input file from the command line arguments
    let input_file = std::env::args().nth(1).ok_or(Error::NoInputFile)?;

    // Open the file
    let file = File::open(&input_file).map(BufReader::new).map_err(Error::IO)?;
    
    // Parse the file as BPMN and convert it to a Petri net, then convert the Petri net to PNML
    let bpmn: Bpmn = parse_xml(file).map_err(Error::Parse)?;
    let petri_net: ECNet = bpmn.into();
    let pnml: Pnml = petri_net.into();

    // Print the PNML
    println!("{}", pnml);

    Ok(())
}