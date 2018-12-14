use crate::CommandLine;
use crate::error::ProgramError;
use regex::Regex;
use std::io::{BufReader, BufRead};
use std::fs::File;

pub struct Claim {
    pub id: usize,
    pub left: usize,
    pub top: usize,
    pub right: usize,
    pub bottom: usize
}

pub fn read_claims( cli: &CommandLine ) -> Result<Vec<Claim>, ProgramError> {
    let input_reader = BufReader::new( File::open( &cli.input_file_name )? );
    let line_regex = Regex::new( r"^#(?P<id>\d+) @ (?P<x>\d+),(?P<y>\d+): (?P<width>\d+)x(?P<height>\d+)$" ).unwrap();
    let mut claims = Vec::new();

    for line in input_reader.lines() {
        for captures in line_regex.captures_iter( &line? ) {
            let id = crate::regex::convert_capture::<usize>( &captures, "id" )?;
            let x = crate::regex::convert_capture::<usize>( &captures, "x" )?;
            let y = crate::regex::convert_capture::<usize>( &captures, "y" )?;
            let width = crate::regex::convert_capture::<usize>( &captures, "width" )?;
            let height = crate::regex::convert_capture::<usize>( &captures, "height" )?;

            claims.push( Claim {
                id,
                left: x,
                top: y,
                right: x + width - 1,
                bottom: y + height - 1
            } );
        }
    }

    Ok( claims )
}
