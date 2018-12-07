extern crate clap;
use clap::{Arg, App};
#[macro_use] extern crate quick_error;

use std::io;
use std::num::ParseIntError;

quick_error! {
    #[derive( Debug )]
    pub enum ProgramError {
        IoError( err: io::Error ) {
            cause( err )
            from()
        }

        ParseIntError( err: ParseIntError ) {
            cause( err )
            from()
        }
    }
}

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
