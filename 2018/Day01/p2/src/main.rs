extern crate clap;
#[macro_use] extern crate quick_error;

use clap::{Arg, App};
use std::collections::HashSet;
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

struct CommandLine {
    input_file_name: String
}

fn main() {
    let cli = parse_command_line();
    let frequency_shifts = read_frequency_shifts( &cli ).expect( "Failed to read frequency shifts from file" );
    let result = find_final_frequency( &frequency_shifts );
    println!( "Result: {}", result )
}

fn parse_command_line() -> CommandLine {
    let matches = App::new( "AoC 2018 Day 1 Puzzle 1" )
        .arg( Arg::with_name( "input" )
            .help( "Input file to use" )
            .required( true )
            .index( 1 )
        )
        .get_matches();

    CommandLine {
        input_file_name: matches.value_of( "input" ).unwrap().to_string()
    }
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

fn find_final_frequency( frequency_shifts: &Vec<i32> ) -> i32 {
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
