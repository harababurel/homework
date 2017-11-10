extern crate carmichael;

fn main() {
    let n = 5;

    let xs: Vec<u32> = (1u32..)
        .filter(|x| carmichael::is_carmichael(*x))
        .take(n)
        .collect();

    println!("{:?}", xs);

}
