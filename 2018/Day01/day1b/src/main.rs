extern crate aoc_shared;

use aoc_shared::{CommandLine, ProgramError};
use std::collections::HashSet;
use std::io::{BufReader, BufRead};
use std::fs::File;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code 2018 Day 1b" );
    let frequency_shifts = read_frequency_shifts( &cli ).expect( "Failed to read frequency shifts from file" );
    let result = find_final_frequency( &frequency_shifts );
    println!( "Result: {}", result )
}

fn read_frequency_shifts( cli: &CommandLine ) -> Result<Vec<i32>, ProgramError> {
    let mut frequency_shifts = Vec::new();
    let input_file = File::open( &cli.input_file_name )?;
    let input_reader = BufReader::new( input_file );

    for line in input_reader.lines() {
        frequency_shifts.push( line?.parse::<i32>()? );
    }

    Ok( frequency_shifts )
}

fn find_final_frequency( frequency_shifts: &[i32] ) -> i32 {
    let mut observed_frequencies = HashSet::new();
    let mut current_frequency = 0;

    loop {
        for current_shift in frequency_shifts.iter() {
            current_frequency += current_shift;
            if observed_frequencies.contains( &current_frequency ) {
                return current_frequency;
            }
            observed_frequencies.insert( current_frequency );
        }
    }
}
