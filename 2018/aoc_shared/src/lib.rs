pub mod error;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;

mod regex;

extern crate clap;
use clap::{Arg, App};

pub struct CommandLine {
    pub input_file_name: String
}

pub fn parse_command_line( title: &str ) -> CommandLine {
    let matches = App::new( title )
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
