use std::fs::File;
use std::io::BufReader;
use petri_nets::{Bpmn, ECNet, Error, parse_xml};

fn main() -> Result<(), Error> {
    // Read the input file from the command line arguments
    let input_file = std::env::args().nth(1).ok_or(Error::NoInputFile)?;

    // Open the file
    let file = File::open(&input_file).map(BufReader::new).map_err(Error::IO)?;

    // Parse the file either as BPMN or PNML, and convert it to a Petri net
    let petri_net: ECNet = {
        if input_file.ends_with(".bpmn") {
            let bpmn: Bpmn = parse_xml(file).map_err(Error::Parse)?;
            bpmn.into()
        } else if input_file.ends_with(".pnml") {
            parse_xml(file).map_err(Error::Parse)?
        } else {
            return Err(Error::UnsupportedFileExt);
        }
    };

    // Perform reachability analysis on the Petri net
    let reachability_analysis = petri_net.reachability_analysis();

    // Print the reachability analysis
    println!("{}", reachability_analysis);

    Ok(())
}
