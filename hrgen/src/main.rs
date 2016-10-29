extern crate clap;
extern crate hs_type_parser;
use clap::{Arg, App};
use std::io::prelude::*;
use std::fs::File;
use std::process::exit;

fn main() {
	let matches = App::new("hrgen")
						  .version("0.1.0")
						  .author("Michael Gattozzi <mgattozzi@gmail.com>")
						  .about("Parse Haskell and Rust files to generate bindings")
						  .arg(Arg::with_name("HFILE")
							   .help("Haskell to Rust Conversion")
							   .short("h")
							   .takes_value(true))
						  .arg(Arg::with_name("RFILE")
							   .help("Rust to Haskell Conversion")
							   .short("r")
							   .takes_value(true))
						  .get_matches();
	let mut output_buffer = String::new();
	let mut file: File;

	if let Some(f) = matches.value_of("HFILE") {
		if f.ends_with(".hs") {
			file = match File::open(f){
				Ok(f) => f,
				Err(e) => panic!("{}",e),
			};
			output_buffer.push_str("extern {\n");
		} else {
			println!("Wrong file type. Needs to end in .hs");
			exit(0);
		}
	} else if let Some(_) = matches.value_of("RFILE") {
		unimplemented!();
	} else {
		println!("No conversion specified");
		exit(0);
	}
	// Define our buffers
	let mut file_buf = String::new();


	// Read the given file into our our buffer
	let _ = file.read_to_string(&mut file_buf);

	// Parse each line and if it contains foreign export
	// parse it and make the Rust equivalent
	for line in file_buf.lines() {
		if line.contains("foreign export") {
			parse_hs_export(&mut output_buffer,String::from(line));
		}
	}

	output_buffer.push('}');
	println!("{}",output_buffer);
}

fn parse_hs_export(buffer: &mut String, line: String) {
	// List that we draw from for function headers
	let symbols = ['a','b','c','d','e','f','g','h','i','j','k','l','m',
					   'n','o','p','q','r','s','t','u','v','w','x','y','z'];
	// Add spaces before each line in the extern
	buffer.push_str("   ");

	// Split the signature header into before and after the :: symbol
	let mut sig_split: Vec<&str> = line.split(" :: ").collect();
	// Split the types of the function by the -> symbol so we can iterate over them
	let signature: Vec<&str> = sig_split.pop().unwrap().trim().split(" -> ").collect();
	// replace the export call into the Rust equivalent import call
	let func_name = sig_split.pop().unwrap().replace("foreign export ccall","pub fn");

	//Push the first part of the function header into the buffer
	buffer.push_str(&func_name.trim());
	buffer.push('(');

	// Iterate over each type in the signature header use enumerate to grab
	// items from the symbol array. Will fail if function header has more than
	// 26 items to call it but let's be real, a function that large is a monster and
	// should be broken up.
	for (i,j) in signature.iter().enumerate() {
		// This is the last item meaning it's the return type
		// We handle it a bit differently to make sure it gets
		// put in right
		if i == signature.len() - 1 {
			buffer.push_str("-> ");
			buffer.push_str(j);
			buffer.push(';');
		} else {
			// Otherwise push in a new symbol for the function
			// then push in the type and either a , or ) depending
			// on whether it's the last type before the return type
			// or not
			buffer.push(symbols[i]);
			buffer.push_str(": ");
			buffer.push_str(j);
			if i == signature.len() - 2 {
				buffer.push_str(") ");
			} else {
				buffer.push_str(", ");
			}
		}
	}
	// Finally we've finished parsing the line push in a newline to the buffer.
	buffer.push('\n');
}
