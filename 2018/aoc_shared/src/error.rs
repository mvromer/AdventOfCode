use quick_error::quick_error;
use std::error::Error;
use std::fmt;
use std::io;
use std::num::ParseIntError;

#[derive( Debug )]
pub struct CaptureError {
    pub name: String
}

impl CaptureError {
    pub fn new( name: &str ) -> CaptureError {
        CaptureError {
            name: String::from( name )
        }
    }
}

impl fmt::Display for CaptureError {
    fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
        write!( f, "Named capture {} not found", self.name )
    }
}

impl Error for CaptureError {
    fn description( &self ) -> &str {
        &self.name
    }
}

#[derive( Debug )]
pub struct MatchError {
    pub regex_description: String
}

impl MatchError {
    pub fn new( regex_description: &str ) -> MatchError {
        MatchError {
            regex_description: String::from( regex_description )
        }
    }
}

impl fmt::Display for MatchError {
    fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
        write!( f, "No match found for {} regex", self.regex_description )
    }
}

impl Error for MatchError {
    fn description( &self ) -> &str {
        &self.regex_description
    }
}

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

        CaptureError( err: CaptureError ) {
            cause( err )
            from()
        }

        MatchError( err: MatchError ) {
            cause( err )
            from()
        }
    }
}
