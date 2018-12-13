use aoc_shared::day3::{self, Claim};

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 3b" );
    let claims = day3::read_claims( &cli ).expect( "Failed to read claims" );
    let claim_id = find_nonoverlapped_claim( &claims ).unwrap();
    println!( "Claim that is not overlapped: {}", claim_id );
}

fn find_nonoverlapped_claim( claims: &[Claim] ) -> Result<usize, String> {

    'first_claim: for first_claim in claims.iter() {
        for second_claim in claims.iter() {
            // If the two claims intersect, no need checking all other claims.
            if first_claim.id != second_claim.id && intersects( first_claim, second_claim ) {
                continue 'first_claim;
            }
        }

        return Ok( first_claim.id );
    }

    Err( String::from( "Failed to find the claim that isn't overlapped" ) )
}

fn intersects( first_claim: &Claim, second_claim: &Claim ) -> bool {
    !(first_claim.right < second_claim.left ||
    first_claim.left > second_claim.right ||
    first_claim.top > second_claim.bottom ||
    first_claim.bottom < second_claim.top)
}
