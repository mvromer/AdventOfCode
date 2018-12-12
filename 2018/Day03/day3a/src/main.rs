use aoc_shared::day3::{self, Claim};

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 3a" );
    let claims = day3::read_claims( &cli ).expect( "Failed to read claims" );
    let square_inches_overlapped = count_overlap( &claims );
    println!( "Number square inches overlapped: {}", square_inches_overlapped );
}

// The basic method simply allocates a giant 2D array representing the entire piece of fabric. Each element represents
// how many claims cover the corresponding square inch of fabric. After each claim is examined, it scans the 2D array
// and finds all element that are covered by two or more claims. It's not particularly memory efficient and requires an
// entire scan of the potential search area when complete, but it's simple.
fn count_overlap( claims: &[Claim] ) -> usize {
    const FABRIC_HEIGHT: usize = 1000;
    const FABRIC_WIDTH: usize = 1000;
    let mut fabric = vec![vec![0; FABRIC_WIDTH]; FABRIC_HEIGHT];

    for claim in claims {
        for y in claim.top..=claim.bottom {
            for x in claim.left..=claim.right {
                fabric[y][x] += 1;
            }
        }
    }

    fabric.iter().flat_map( |row| row.iter() ).filter( |&square| *square > 1 ).count()
}
