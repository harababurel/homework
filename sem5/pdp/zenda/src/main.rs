extern crate zenda;
extern crate clap;

use zenda::Transformer;
use clap::{App, AppSettings, Arg, SubCommand};
use std::process;

fn main() {
    let matches = App::new("zenda")
        .version("0.1")
        .about("Zenda applies convolutional filters on images.")
        .author("Sergiu Puscas (srg.pscs@gmail.com)")
        .arg(Arg::with_name("input-file")
            .required(true))
        .arg(Arg::with_name("output-file")
            .help("optional; overwrites the input file if not specified")
            .takes_value(true)
            .short("o")
            .long("output"))
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(SubCommand::with_name("blur")
            .about("Apply a blur filter")
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .subcommand(SubCommand::with_name("box")  // Subcommands can have thier own subcommands,
                .about("box blur")
                .arg(Arg::with_name("amount")
                    .help("The amount of blur")
                    .required(true)))
            .subcommand(SubCommand::with_name("gaussian")  // Subcommands can have thier own subcommands,
                .about("gaussian blur")
                .arg(Arg::with_name("amount")
                    .help("The amount of blur")
                    .required(true)))
            .subcommand(SubCommand::with_name("motion")  // Subcommands can have thier own subcommands,
                .about("motion blur")
                .arg(Arg::with_name("amount")
                    .help("The amount of blur")
                    .required(true))))
//        .subcommand(SubCommand::with_name("gaussian")
//            .about("Apply a gaussian blur filter")
//            .arg(Arg::with_name("amount")
//                .help("The amount of blur")
//                .required(true)))
//        .subcommand(SubCommand::with_name("motion")
//            .about("Apply a motion blur filter")
//            .arg(Arg::with_name("amount")
//                .help("The amount of blur")
//                .required(true)))
        .subcommand(SubCommand::with_name("sharpen")
            .about("Sharpen the image"))
        .subcommand(SubCommand::with_name("emboss")
            .about("Emboss the image"))
        .subcommand(SubCommand::with_name("edges")
            .about("Find edges in the image"))
        .get_matches();

    let input_file = matches.value_of("input-file").unwrap();
    let output_file = matches.value_of("output-file").unwrap_or(input_file.clone());

    println!("Input file: {}", input_file);
    println!("Output file: {}", output_file);

    let mut transformer: Transformer;
    match Transformer::load(&input_file) {
        Ok(t) => { transformer = t }
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    };


//    transformer.find_edges();
//    transformer.save("/tmp/output.png").expect("Could not save image.");

    match matches.subcommand() {
        ("blur", Some(matches)) => {
            match matches.subcommand() {
                (blur_type, Some(matches)) => {
                    let amount: usize;
                    match matches.value_of("amount").unwrap().parse::<usize>() {
                        Ok(x) => { amount = x; }
                        Err(e) => {
                            println!("Bad blur amount: {}", e);
                            process::exit(1);
                        }
                    }

                    println!("Applying {} blur...", blur_type);
                    match blur_type {
                        "box" => { transformer.blur(amount); }
                        "gaussian" => { transformer.gaussian_blur(amount); }
                        "motion" => { transformer.motion_blur(amount); }
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
        ("emboss", Some(_)) => {
            println!("Embossing the image...");
            transformer.emboss();
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
            println!("{}", e);
            process::exit(1);
        }
    };
}
