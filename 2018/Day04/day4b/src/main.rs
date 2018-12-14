use aoc_shared::day4::{self, GuardEvent, SleepCounts};
use chrono::naive::NaiveDateTime;
use std::collections::BTreeMap;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 4b" );
    let guard_events = day4::read_guard_events( &cli ).expect( "Failed to read guard events" );
    let sleepiest_guard_checksum = find_sleepiest_guard_checksum( &guard_events );

    println!( "Sleepiest guard checksum: {}", sleepiest_guard_checksum );
}

fn find_sleepiest_guard_checksum( guard_events: &BTreeMap<NaiveDateTime, GuardEvent> ) -> i32 {
    let SleepCounts {
        per_minute_sleep_counts,
        ..
    } = day4::get_sleep_counts( guard_events );

    // Find the guard who had the highest per minute sleep count.
    let (sleepiest_guard, minute_most_frequently_asleep) = per_minute_sleep_counts.iter()
        .map( |(guard_id, current_per_minute_sleep_counts)| {
            (guard_id,
            current_per_minute_sleep_counts.iter()
                .enumerate()
                .max_by_key( |&(_, sleep_count)| sleep_count )
                .unwrap())
        } )
        .max_by_key( |&(_, (_, max_sleep_count))| max_sleep_count )
        .map( |(&guard_id, (minute, _))| (guard_id, minute as i32) )
        .unwrap();

    sleepiest_guard * minute_most_frequently_asleep
}
