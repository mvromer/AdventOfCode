use crate::CommandLine;
use std::io::{self, BufReader, BufRead};
use std::fs::File;

pub fn read_box_ids( cli: &CommandLine ) -> io::Result<Vec<String>> {
    let input_reader = BufReader::new( File::open( &cli.input_file_name )? );
    input_reader.lines().collect()
}
