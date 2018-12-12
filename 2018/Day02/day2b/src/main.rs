use aoc_shared::day2;

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 2b" );
    let box_ids = day2::read_box_ids( &cli ).expect( "Failed to read box IDs." );
    let solution = find_solution( &box_ids ).expect( "Failed to find prototype fabric." );
    println!( "Solution: {}", solution );
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
