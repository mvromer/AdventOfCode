use aoc_shared::day4::{self, GuardEvent, SleepCounts};
use chrono::naive::NaiveDateTime;
use std::collections::BTreeMap;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 4a" );
    let guard_events = day4::read_guard_events( &cli ).expect( "Failed to read guard events" );
    let sleepiest_guard_checksum = find_sleepiest_guard_checksum( &guard_events );

    println!( "Sleepiest guard checksum: {}", sleepiest_guard_checksum );
}

fn find_sleepiest_guard_checksum( guard_events: &BTreeMap<NaiveDateTime, GuardEvent> ) -> i32 {
    let SleepCounts {
        total_minutes_asleep,
        per_minute_sleep_counts
    } = day4::get_sleep_counts( guard_events );

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
