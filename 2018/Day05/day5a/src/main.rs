use aoc_shared::{self, day5};

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 5a" );
    let polymer = day5::run_reactions( day5::read_polymer( &cli ).expect( "Unable to read polymer" ) );
    println!( "{}", polymer.len() );
}
