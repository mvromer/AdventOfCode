use crate::CommandLine;
use crate::error::{CaptureError, ProgramError};
use regex::{Captures, Regex};
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
            let id = convert_capture::<usize>( &captures, "id" )?;
            let x = convert_capture::<usize>( &captures, "x" )?;
            let y = convert_capture::<usize>( &captures, "y" )?;
            let width = convert_capture::<usize>( &captures, "width" )?;
            let height = convert_capture::<usize>( &captures, "height" )?;

            claims.push( Claim {
                id: id,
                left: x,
                top: y,
                right: x + width - 1,
                bottom: y + height - 1
            } );
        }
    }

    Ok( claims )
}

fn convert_capture<T>( captures: &Captures, name: &str ) -> Result<T, ProgramError>
    where T: std::str::FromStr,
        crate::error::ProgramError: std::convert::From<<T as std::str::FromStr>::Err> {
    let value = captures.name( name )
        .ok_or_else( || CaptureError::new( name ) )?
        .as_str()
        .parse::<T>()?;

    Ok( value )
}
