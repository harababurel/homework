extern crate zenda;
extern crate clap;

use zenda::Transformer;
use clap::{App, AppSettings, Arg, SubCommand};
use std::process;

fn validate_kernel_size(s: &str) -> usize {
    match s.parse::<usize>() {
        Ok(x) => { return x; }
        Err(e) => {
            println!("Bad kernel size: {}", e);
            process::exit(1);
        }
    };
}

fn main() {
    let matches = App::new("zenda")
        .version("0.1.0")
        .about("Zenda applies convolutional filters on images, such as:
\tblur (box/gaussian/motion)
\tsharpness
\temboss (color and grayscale)
\tedge detection")
        .author("Sergiu Puscas (srg.pscs@gmail.com)")
        .arg(Arg::with_name("input-file")
            .required(true)
            .index(1))
        .arg(Arg::with_name("output-file")
            .help("optional; overwrites the input file if not specified")
            .takes_value(true)
            .short("o")
            .long("output"))
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(SubCommand::with_name("blur")
            .about("Apply a blur filter")
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .subcommand(SubCommand::with_name("box")
                .about("box blur")
                .arg(Arg::with_name("size")
                    .help("The kernel size (in pixels)")
                    .required(true)))
            .subcommand(SubCommand::with_name("gaussian")
                .about("gaussian blur")
                .arg(Arg::with_name("size")
                    .help("The kernel size (in pixels)")
                    .required(true))
                .arg(Arg::with_name("sigma")
                    .required(true)))
            .subcommand(SubCommand::with_name("motion")
                .about("motion blur")
                .arg(Arg::with_name("size")
                    .help("The kernel size (in pixels)")
                    .required(true))))
        .subcommand(SubCommand::with_name("sharpen")
            .about("Sharpen the image"))
        .subcommand(SubCommand::with_name("emboss")
            .about("Emboss the image")
            .arg(Arg::with_name("size")
                .help("The kernel size (in pixels)")
                .required(true))
            .arg(Arg::with_name("grayscale")
                .help("remove color completely")
                .conflicts_with("color")
                .long("grayscale")
                .short("g"))
            .arg(Arg::with_name("color")
                .help("keep color")
                .conflicts_with("grayscale")
                .long("color")
                .short("c")))
        .subcommand(SubCommand::with_name("edges")
            .about("Find edges in the image"))
        .get_matches();

    let input_file = matches.value_of("input-file").unwrap();
    let output_file = matches.value_of("output-file").unwrap_or(input_file.clone());

    let mut transformer: Transformer;
    match Transformer::load(&input_file) {
        Ok(t) => { transformer = t }
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    };

    match matches.subcommand() {
        ("blur", Some(matches)) => {
            match matches.subcommand() {
                (blur_type, Some(matches)) => {
                    let kernel_size = validate_kernel_size(matches.value_of("size").unwrap());
                    match blur_type {
                        "box" => { transformer.box_blur(kernel_size); }
                        "motion" => { transformer.motion_blur(kernel_size); }
                        "gaussian" => {
                            let sigma: f64;
                            match matches.value_of("sigma").unwrap().parse::<f64>() {
                                Ok(x) => { sigma = x; }
                                Err(e) => {
                                    println!("Bad sigma: {}", e);
                                    process::exit(1);
                                }
                            };
                            transformer.gaussian_blur(kernel_size, sigma);
                        }
                        _ => unreachable!()
                    };
                }
                _ => unreachable!()
            }
        }
        ("sharpen", Some(_)) => {
            println!("Sharpening the image...");
            transformer.sharpen();
        }
        ("emboss", Some(matches)) => {
            println!("Embossing the image...");

            let kernel_size = validate_kernel_size(matches.value_of("size").unwrap());
            let grayscale = matches.is_present("grayscale");
            transformer.emboss(kernel_size, grayscale);
        }
        ("edges", Some(_)) => {
            println!("Finding edges...");
            transformer.find_edges();
        }
        _ => unreachable!()
    }

    match transformer.save(&output_file) {
        Ok(_) => { println!("Saved image to {}", &output_file); }
        Err(e) => {
            println!("Could not save image to {}: {}", &output_file, e);
            process::exit(1);
        }
    };
}
