use std::{env, fs};

struct Pair {
    min: i32,
    max: i32,
}
fn to_pair(pair: &str) -> Pair {
    let pair_string: Vec<&str> = pair.split("-").collect();
    let pair = match pair_string.len() {
        2 => Ok(Pair {
            min: pair_string[0].parse::<i32>().unwrap(),
            max: pair_string[1].parse::<i32>().unwrap(),
        }),
        _ => Err("Unexpected parameters"),
    };
    return pair.unwrap();
}

fn contains_fully(pair1: &Pair, pair2: &Pair) -> bool {
    return (pair1.min >= pair2.min && pair1.min <= pair2.max)
        && (pair1.max >= pair2.min && pair1.max <= pair2.max);
}

fn overlaps(pair1: &Pair, pair2: &Pair) -> bool {
    return (pair1.min >= pair2.min && pair1.min <= pair2.max)
        || (pair1.max >= pair2.min && pair1.max <= pair2.max);
}

fn bad_pair<F>(pair: &str, is_bad_pair: F) -> bool
where
    F: Fn(&Pair, &Pair) -> bool,
{
    let pairs: Vec<Pair> = pair.split(",").map(to_pair).collect();
    if pairs.len() == 2 {
        let first = &pairs[0];
        let second = &pairs[1];
        return is_bad_pair(first, second) || is_bad_pair(second, first);
    } else {
        println!("Unexpected number of parameters");
        return false;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let all_backpacks = contents.split("\n");

    let fully_overlaping_num: u32 = all_backpacks
        .clone()
        .map(|x| if bad_pair(x, contains_fully) { 1 } else { 0 })
        .sum();

    println!("The fully overlapping number is {}", fully_overlaping_num);

    let partly_overlapping_num: u32 = all_backpacks
        .clone()
        .map(|x| if bad_pair(x, overlaps) { 1 } else { 0 })
        .sum();
    println!(
        "The partly overlapping number is {}",
        partly_overlapping_num
    );
}
