use std::{collections::HashSet, env, fs, str::Split};

fn priority(x: &char) -> u32 {
    if x.is_uppercase() {
        return *x as u32 - 'A' as u32 + 27;
    } else {
        return *x as u32 - 'a' as u32 + 1;
    }
}

fn backpack_cost(backpack: &str) -> u32 {
    let (compartment1, compartment2) = backpack.split_at(backpack.len() / 2);

    let set_comp1: HashSet<char> = HashSet::from_iter(compartment1.chars());
    let set_comp2: HashSet<char> = HashSet::from_iter(compartment2.chars());
    let intersection = set_comp1.intersection(&set_comp2);

    let priorities: u32 = intersection.map(priority).sum();
    return priorities;
}

fn group_cost(chunk: &[&str]) -> u32 {
    if chunk.len() != 3 {
        return 0;
    } else {
        let set_group1: HashSet<char> = HashSet::from_iter(chunk[0].chars());
        let set_group2: HashSet<char> = HashSet::from_iter(chunk[1].chars());
        let set_group3: HashSet<char> = HashSet::from_iter(chunk[2].chars());

        let first: HashSet<char> = set_group1.intersection(&set_group2).map(|x| *x).collect();

        let common = first.intersection(&set_group3);
        return priority(common.clone().next().unwrap());
    }
}

fn groups(backpacks: Split<&str>) -> u32 {
    let backpack_vec: Vec<&str> = backpacks.collect();

    let groups_cost: u32 = backpack_vec.chunks(3).map(group_cost).sum();
    return groups_cost;
}
fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    let all_backpacks = contents.split("\n");

    let overall_cost: u32 = all_backpacks.clone().map(backpack_cost).sum();

    println!("The overall cost is {}", overall_cost);

    let all_groups_costs = groups(all_backpacks.clone());

    println!("The overall groups cost is {}", all_groups_costs)
}
