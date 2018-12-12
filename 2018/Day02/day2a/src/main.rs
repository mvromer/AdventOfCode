use aoc_shared::day2;
use std::collections::HashMap;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 2a" );
    let box_ids = day2::read_box_ids( &cli ).expect( "Failed to read box IDs." );
    let checksum = compute_checksum( &box_ids );
    println!( "Checksum: {}", checksum );
}

fn compute_checksum( box_ids: &[String] ) -> i32 {
    let mut two_counts = 0;
    let mut three_counts = 0;

    for box_id in box_ids {
        let mut letters = HashMap::new();
        for letter in box_id.chars() {
            letters.entry( letter ).and_modify( |letter_count| { *letter_count += 1 } ).or_insert( 1 );
        }

        let mut has_two_count = false;
        let mut has_three_count = false;

        for &letter_count in letters.values() {
            has_two_count |= letter_count == 2;
            has_three_count |= letter_count == 3;

            if has_two_count && has_three_count {
                break;
            }
        }

        if has_two_count { two_counts += 1; }
        if has_three_count { three_counts += 1; }
    }

    two_counts * three_counts
}
