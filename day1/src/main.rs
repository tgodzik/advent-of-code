use std::env;
use std::fs;


fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let all_elfs = contents.split("\n\n");

    let mut all_sums: Vec<i32> = vec![]; 
    for calories in all_elfs {
        let mut sum = 0;
        for x in calories.split("\n") {
            sum = sum + x.parse::<i32>().unwrap();
        }
        all_sums.push(sum)
    }
    all_sums.sort_by(|a, b| b.cmp(a));

    println!("The highest calories is {}", all_sums[0] );


    println!(
        "The sum of three highest calories is {}",
        all_sums[0] + all_sums[1] + all_sums[2]
    );
}
