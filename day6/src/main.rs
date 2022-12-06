use std::{
    collections::{HashSet, VecDeque},
    env, fs,
};

fn find_start(contents: String, distinct_amount: usize) -> i32 {
    let mut four: VecDeque<char> = VecDeque::new();
    let mut i = 0;
    for c in contents.chars() {
        i += 1;
        if four.len() == distinct_amount {
            four.pop_front();
        }
        four.push_back(c);
        let set: HashSet<char> = four.clone().drain(..).collect();
        if set.len() == distinct_amount {
            return i;
        }
    }
    return i;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let start_packet = find_start(contents.clone(), 4);

    println!("Start packet is at {}", start_packet);

    let start_message = find_start(contents.clone(), 14);

    println!("Start message is at {}", start_message);
}
