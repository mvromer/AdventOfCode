extern crate aoc_shared;

use aoc_shared::CommandLine;
use std::io::{self, BufReader, BufRead};
use std::fs::File;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 2b" );
    let box_ids = read_box_ids( &cli ).expect( "Failed to read box IDs." );
    let solution = find_solution( &box_ids ).expect( "Failed to find prototype fabric." );
    println!( "Solution: {}", solution );
}

fn read_box_ids( cli: &CommandLine ) -> io::Result<Vec<String>> {
    let input_reader = BufReader::new( File::open( &cli.input_file_name )? );
    input_reader.lines().collect()
}

fn find_solution( box_ids: &[String] ) -> Option<String> {
    for (first_index, first_box_id) in box_ids.iter().enumerate() {
        for second_box_id in box_ids.iter().skip( first_index + 1 ) {
            // We want box IDs that are exactly one character off.
            if compute_hamming_distance( first_box_id, second_box_id ) == 1 {
                return Some( first_box_id.chars()
                    .zip( second_box_id.chars() )
                    .filter( |&(c1, c2)| c1 == c2 )
                    .map( |(c1, _)| c1 )
                    .collect() );
            }
        }
    }

    None
}

fn compute_hamming_distance( s1: &str, s2: &str ) -> usize {
    s1.chars()
        .zip( s2.chars() )
        .filter( |&(c1, c2)| c1 != c2 )
        .count()
}
