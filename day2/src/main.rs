use std::env;
use std::fs;
use std::str::FromStr;
use std::str::Split;

#[derive(PartialEq, Eq)]
enum Gesture {
    Rock,
    Paper,
    Scissors,
}

impl FromStr for Gesture {
    type Err = ();

    fn from_str(input: &str) -> Result<Gesture, Self::Err> {
        match input {
            "A" => Ok(Gesture::Rock),
            "B" => Ok(Gesture::Paper),
            "C" => Ok(Gesture::Scissors),
            "X" => Ok(Gesture::Rock),
            "Y" => Ok(Gesture::Paper),
            "Z" => Ok(Gesture::Scissors),
            _ => Err(()),
        }
    }
}

fn naive(all_matches: Split<&str>) {
    let mut points: i32 = 0;
    for mtch in all_matches {
        let mut split: Split<&str> = mtch.split(" ");
        let oponent = Gesture::from_str(split.next().unwrap()).unwrap();
        let myGesture = Gesture::from_str(split.next().unwrap()).unwrap();

        if oponent.eq(&myGesture) {
            points = points + 3;
        }

        match myGesture {
            Gesture::Rock => points += 1,
            Gesture::Paper => points += 2,
            Gesture::Scissors => points += 3,
        }

        match (myGesture, oponent) {
            (Gesture::Rock, Gesture::Scissors) => points += 6,
            (Gesture::Paper, Gesture::Rock) => points += 6,
            (Gesture::Scissors, Gesture::Paper) => points += 6,
            _ => {}
        }
    }

    println!("The total points are using naive strategy {}", points);
}

#[derive(PartialEq, Eq)]
enum Strategy {
    Loss,
    Draw,
    Win,
}

impl FromStr for Strategy {
    type Err = ();

    fn from_str(input: &str) -> Result<Strategy, Self::Err> {
        match input {
            "X" => Ok(Strategy::Loss),
            "Y" => Ok(Strategy::Draw),
            "Z" => Ok(Strategy::Win),
            _ => Err(()),
        }
    }
}

fn advanced(all_matches: Split<&str>) {
    let mut points: i32 = 0;
    for mtch in all_matches {
        let mut split: Split<&str> = mtch.split(" ");
        let oponent = Gesture::from_str(split.next().unwrap()).unwrap();
        let strategy = Strategy::from_str(split.next().unwrap()).unwrap();

        match strategy {
            Strategy::Loss => {
                points += 0;
                match oponent {
                    Gesture::Paper => points += 1,
                    Gesture::Scissors => points += 2,
                    Gesture::Rock => points += 3,
                }
            }
            Strategy::Draw => {
                points = points + 3;
                match oponent {
                    Gesture::Paper => points += 2,
                    Gesture::Scissors => points += 3,
                    Gesture::Rock => points += 1,
                }
            }
            Strategy::Win => {
                points = points + 6;
                match oponent {
                    Gesture::Paper => points += 3,
                    Gesture::Scissors => points += 1,
                    Gesture::Rock => points += 2,
                }
            }
        }
    }

    println!("The total points using advanced strategy {}", points);
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let all_matches = contents.split("\n");
    naive(all_matches.clone());
    advanced(all_matches.clone());
}
