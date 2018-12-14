use aoc_shared::{self, day5};
use regex::Regex;
use std::collections::HashSet;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 5b" );
    let polymer = find_shortest_polymer( &day5::read_polymer( &cli ).expect( "Unable to read polymer" ) );
    println!( "{}", polymer.len() );
}

fn find_shortest_polymer( original_polymer: &str ) -> String {
    let mut reduced_polymers = HashSet::new();

    for c in (b'A' ..= b'Z').map( char::from ) {
        let remove_regex = Regex::new( &format!( "{}|{}", c, c.to_ascii_lowercase() ) ).unwrap();
        let reduced_polymer = day5::run_reactions( remove_regex.replace_all( &original_polymer, "" ).to_string() );
        reduced_polymers.insert( reduced_polymer );
    }

    reduced_polymers.into_iter()
        .min_by_key( |polymer| polymer.len() )
        .unwrap()
}
