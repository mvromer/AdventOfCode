use aoc_shared::day4::{self, GuardEvent, GuardEvent::*};
use chrono::naive::{NaiveDateTime, NaiveDate};
use chrono::Timelike;
use std::collections::{BTreeMap, HashMap};

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 4a" );
    let guard_events = day4::read_guard_events( &cli ).expect( "Failed to read guard events" );
    let sleepiest_guard_checksum = find_sleepiest_guard_checksum( &guard_events );

    println!( "Sleepiest guard checksum: {}", sleepiest_guard_checksum );
}

fn find_sleepiest_guard_checksum( guard_events: &BTreeMap<NaiveDateTime, GuardEvent> ) -> i32 {
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

    // Find the sleepiest guard, i.e., the one who slept the most number of minutes.
    let sleepiest_guard = total_minutes_asleep.iter()
        .max_by_key( |&(_, minutes_asleep)| minutes_asleep )
        .map( |(guard_id, _)| guard_id )
        .unwrap();

    // Find which minute the sleepiest guard slept the longest.
    let most_slept_minute = &per_minute_sleep_counts[&sleepiest_guard]
        .iter()
        .enumerate()
        .max_by_key( |&(_, sleep_count)| sleep_count )
        .map( |(minute, _)| minute as i32 )
        .unwrap();

    sleepiest_guard * most_slept_minute
}
