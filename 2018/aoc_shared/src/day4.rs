use chrono::Timelike;
use chrono::naive::{NaiveDateTime, NaiveDate};
use crate::CommandLine;
use crate::error::{MatchError, ProgramError};
use regex::Regex;
use std::collections::{BTreeMap, HashMap};
use std::io::{BufReader, BufRead};
use std::fs::File;

#[derive( Debug )]
pub enum GuardEvent {
    BeginShift( i32 ),
    FallsAsleep,
    WakesUp
}

use self::GuardEvent::*;

pub struct SleepCounts {
    pub total_minutes_asleep: HashMap<i32, i64>,
    pub per_minute_sleep_counts: HashMap<i32, [i32; 60]>
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
            guard_events.insert( event_timestamp, BeginShift( guard_id ) );
        } else if line.contains( "falls asleep" ) {
            guard_events.insert( event_timestamp, FallsAsleep );
        } else if line.contains( "wakes up" ) {
            guard_events.insert( event_timestamp, WakesUp );
        }
    }

    Ok( guard_events )
}

pub fn get_sleep_counts( guard_events: &BTreeMap<NaiveDateTime, GuardEvent> ) -> SleepCounts {
    let mut current_guard = 0;
    let mut total_minutes_asleep = HashMap::new();
    let mut per_minute_sleep_counts = HashMap::new();
    let mut nap_start = NaiveDate::from_ymd( 1518, 1, 1 ).and_hms( 0, 0, 0 );

    // Count up how many minutes each guard slept.
    for (current_timestamp, guard_event) in guard_events.iter() {
        match guard_event {
            BeginShift( guard_id ) => current_guard = *guard_id,
            FallsAsleep => nap_start = *current_timestamp,
            WakesUp => {
                // Add the number of minutes for the most recent nap.
                let number_current_nap_minutes = (*current_timestamp - nap_start).num_minutes();
                total_minutes_asleep.entry( current_guard )
                    .and_modify( |current_minutes_asleep| {
                        *current_minutes_asleep += number_current_nap_minutes;
                    } )
                    .or_insert( number_current_nap_minutes );

                // We're told in the puzzle that all naps happen between 00:00 and 00:59, so for
                // the current guard increment a per-minute counter so we track for which minute the
                // guard was asleep the longest.
                let start_minute = nap_start.minute() as usize;
                let stop_minute = current_timestamp.minute() as usize;
                let current_guard_per_minute_sleep_counts = per_minute_sleep_counts.entry( current_guard )
                    .or_insert_with( || [0; 60] );

                for minute in start_minute..stop_minute {
                    (*current_guard_per_minute_sleep_counts)[minute] += 1;
                }
            }
        }
    }

    SleepCounts {
        total_minutes_asleep,
        per_minute_sleep_counts
    }
}
