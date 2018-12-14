use crate::error::{CaptureError, ProgramError};
use regex::Captures;

pub fn convert_capture<T>( captures: &Captures, name: &str ) -> Result<T, ProgramError>
    where T: std::str::FromStr,
        ProgramError: std::convert::From<<T as std::str::FromStr>::Err> {
    let value = captures.name( name )
        .ok_or_else( || CaptureError::new( name ) )?
        .as_str()
        .parse::<T>()?;

    Ok( value )
}
