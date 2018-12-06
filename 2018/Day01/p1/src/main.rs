extern crate clap;
#[macro_use] extern crate quick_error;

use clap::{Arg, App};
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
    let result = compute_input_sum( &cli ).expect( "Failed to compute input sum." );
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

fn compute_input_sum( cli: &CommandLine ) -> Result<i32, ProgramError> {
    let input_file = File::open( &cli.input_file_name )?;
    let input_reader = BufReader::new( input_file );
    let mut sum = 0;

    for line in input_reader.lines() {
        sum += line?.parse::<i32>()?;
    }

    Ok( sum )
}
