extern crate aoc_shared;
#[macro_use] extern crate quick_error;

use aoc_shared::CommandLine;
use std::io::{self, BufReader, BufRead};
use std::fs::File;
use std::num;

quick_error! {
    #[derive( Debug )]
    enum ProgramError {
        IoError( err: io::Error ) {
            cause( err )
            from()
        }

        ParseIntError( err: num::ParseIntError ) {
            cause( err )
            from()
        }
    }
}

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 1a" );
    let result = compute_input_sum( &cli ).expect( "Failed to compute input sum." );
    println!( "Result: {}", result )
}

fn compute_input_sum( cli: &CommandLine ) -> Result<i32, ProgramError> {
    let input_file = File::open( &cli.input_file_name )?;
    let input_reader = BufReader::new( input_file );
    let mut sum = 0;

    for line in input_reader.lines() {
        sum += line?.parse::<i32>()?;
    }

    Ok( sum )
}
