use chrono::naive::{NaiveDateTime, NaiveDate};
use crate::CommandLine;
use crate::error::{MatchError, ProgramError};
use regex::Regex;
use std::collections::BTreeMap;
use std::io::{BufReader, BufRead};
use std::fs::File;

#[derive( Debug )]
pub enum GuardEvent {
    BeginShift( i32 ),
    FallsAsleep,
    WakesUp
}

pub fn read_guard_events( cli: &CommandLine ) -> Result<BTreeMap<NaiveDateTime, GuardEvent>, ProgramError> {
    let input_reader = BufReader::new( File::open( &cli.input_file_name )? );
    let timestamp_regex = Regex::new( r"\[(?P<year>\d{4})-(?P<month>\d{2})-(?P<date>\d{2}) (?P<hour>\d{2}):(?P<minute>\d{2})\]" ).unwrap();
    let begins_shift_regex = Regex::new( r"Guard #(?P<guard_id>\d+) begins shift" ).unwrap();
    let mut guard_events = BTreeMap::new();

    for line in input_reader.lines() {
        let line = line?;
        let timestamp_captures = timestamp_regex.captures( &line )
            .ok_or_else( || MatchError::new( "timestamp" ) )?;

        let event_timestamp = NaiveDate::from_ymd(
            crate::regex::convert_capture::<i32>( &timestamp_captures, "year" )?,
            crate::regex::convert_capture::<u32>( &timestamp_captures, "month" )?,
            crate::regex::convert_capture::<u32>( &timestamp_captures, "date" )? )
            .and_hms(
            crate::regex::convert_capture::<u32>( &timestamp_captures, "hour" )?,
            crate::regex::convert_capture::<u32>( &timestamp_captures, "minute" )?,
            0
        );

        if let Some( begins_shift_captures ) = begins_shift_regex.captures( &line ) {
            let guard_id = crate::regex::convert_capture::<i32>( &begins_shift_captures, "guard_id" )?;
            guard_events.insert( event_timestamp, GuardEvent::BeginShift( guard_id ) );
        } else if line.contains( "falls asleep" ) {
            guard_events.insert( event_timestamp, GuardEvent::FallsAsleep );
        } else if line.contains( "wakes up" ) {
            guard_events.insert( event_timestamp, GuardEvent::WakesUp );
        }
    }

    Ok( guard_events )
}
