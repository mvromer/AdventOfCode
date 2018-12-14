use crate::CommandLine;
use regex::Regex;
use std::io::{self, BufReader, Read};
use std::fs::File;

pub fn read_polymer( cli: &CommandLine ) -> io::Result<String> {
    let mut input_reader = BufReader::new( File::open( &cli.input_file_name )? );
    let mut polymer = String::new();
    input_reader.read_to_string( &mut polymer )?;
    Ok( polymer )
}

pub fn run_reactions( mut polymer: String ) -> String {
    let reactant_regex = Regex::new( &format!(
        "{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}|{}",
        "(?:Aa)|(?:aA)",
        "(?:Bb)|(?:bB)",
        "(?:Cc)|(?:cC)",
        "(?:Dd)|(?:dD)",
        "(?:Ee)|(?:eE)",
        "(?:Ff)|(?:fF)",
        "(?:Gg)|(?:gG)",
        "(?:Hh)|(?:hH)",
        "(?:Ii)|(?:iI)",
        "(?:Jj)|(?:jJ)",
        "(?:Kk)|(?:kK)",
        "(?:Ll)|(?:lL)",
        "(?:Mm)|(?:mM)",
        "(?:Nn)|(?:nN)",
        "(?:Oo)|(?:oO)",
        "(?:Pp)|(?:pP)",
        "(?:Qq)|(?:qQ)",
        "(?:Rr)|(?:rR)",
        "(?:Ss)|(?:sS)",
        "(?:Tt)|(?:tT)",
        "(?:Uu)|(?:uU)",
        "(?:Vv)|(?:vV)",
        "(?:Ww)|(?:wW)",
        "(?:Xx)|(?:xX)",
        "(?:Yy)|(?:yY)",
        "(?:Zz)|(?:zZ)"
    ) ).unwrap();

    loop {
        let reduced_polymer = reactant_regex.replace_all( &polymer, "" ).to_string();
        if reduced_polymer.len() == polymer.len() {
            break
        }
        polymer = reduced_polymer;
    }

    polymer
}
