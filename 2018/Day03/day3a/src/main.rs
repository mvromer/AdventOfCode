use aoc_shared::{CommandLine, ProgramError, RuntimeError};
use regex::Regex;
use std::io::{BufReader, BufRead};
use std::fs::File;

struct Claim {
    left: usize,
    top: usize,
    right: usize,
    bottom: usize
}

fn main() {
    let cli = aoc_shared::parse_command_line( "Advent of Code Day 3a" );
    let claims = read_claims( &cli ).expect( "Failed to read claims" );
    let square_inches_overlapped = count_overlap( &claims );
    println!( "Number square inches overlapped: {}", square_inches_overlapped );
}

fn read_claims( cli: &CommandLine ) -> Result<Vec<Claim>, ProgramError> {
    let input_reader = BufReader::new( File::open( &cli.input_file_name )? );
    let line_regex = Regex::new( r"^#(?P<id>\d+) @ (?P<x>\d+),(?P<y>\d+): (?P<width>\d+)x(?P<height>\d+)$" ).unwrap();
    let mut claims = Vec::new();

    for line in input_reader.lines() {
        for captures in line_regex.captures_iter( &line? ) {
            let id = captures.name( "id" ).ok_or_else( || RuntimeError::new( "No id found" ) )?.as_str().parse::<usize>()?;
            let x = captures.name( "x" ).ok_or_else( || RuntimeError::new( "No x found" ) )?.as_str().parse::<usize>()?;
            let y = captures.name( "y" ).ok_or_else( || RuntimeError::new( "No y found" ) )?.as_str().parse::<usize>()?;
            let width = captures.name( "width" ).ok_or_else( || RuntimeError::new( "No width found" ) )?.as_str().parse::<usize>()?;
            let height = captures.name( "height" ).ok_or_else( || RuntimeError::new( "No height found" ) )?.as_str().parse::<usize>()?;

            claims.push( Claim {
                id: id,
                left: x,
                top: y,
                right: x + width - 1,
                bottom: y + height - 1
            } );
        }
    }

    Ok( claims )
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
