use regex::Regex;
use std::{env, fs};

struct Move {
    amount: u32,
    from: usize,
    to: usize,
}

fn create_stack(line: &str) -> Vec<&str> {
    let stack: Vec<&str> = line.split(" ").collect();

    return stack;
}

fn create_move(line: &str) -> Option<Move> {
    let re = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    return re.captures(line).map(|captures| Move {
        amount: captures[1].parse::<u32>().unwrap(),
        from: captures[2].parse::<usize>().unwrap(),
        to: captures[3].parse::<usize>().unwrap(),
    });
}

fn move_stack(contents: String, move_all: bool) {
    let stacks_and_move: Vec<&str> = contents.split("\n\n").collect();
    if stacks_and_move.len() == 2 {
        let stacks_part = stacks_and_move[0];
        let moves_part = stacks_and_move[1];

        let mut stacks: Vec<Vec<&str>> = stacks_part.split("\n").map(create_stack).collect();
        let moves: Vec<Move> = moves_part.split("\n").flat_map(create_move).collect();

        for mov in moves {
            let from = mov.from;
            let to = mov.to;
            let amount = mov.amount;

            if move_all {
                let mut tmp: Vec<&str> = Vec::new();

                for _ in 0..amount {
                    let popped = stacks[from - 1].pop();
                    popped.map(|x| tmp.push(x));
                }

                for _ in 0..amount {
                    let popped = tmp.pop();
                    popped.map(|x| stacks[to - 1].push(x));
                }
            } else {
                for _ in 0..amount {
                    let popped = stacks[from - 1].pop();
                    popped.map(|x| stacks[to - 1].push(x));
                }
            }
        }
        let tops: Vec<&str> = stacks.iter().map(|x| *x.last().unwrap()).collect();
        println!("The result is {}", tops.concat());
    } else {
        println!("Wrong number of sections")
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    move_stack(contents.clone(), false);
    move_stack(contents.clone(), true);
}
