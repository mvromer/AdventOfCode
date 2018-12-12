use aoc_shared::CommandLine;
use aoc_shared::error::ProgramError;
use std::io::{BufReader, BufRead};
use std::fs::File;

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
