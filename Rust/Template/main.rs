use std::fs;

fn main() {
    let str = fs::read_to_string("puzzle.txt")
        .expect("Should have been able to read the file");

    let part1: i32 =
        str.split("\n\n").into_iter().enumerate()
            .map(|(_, s)| s.split("\n").into_iter().enumerate()
                    .map(|(_, calorie)| {
                        return str::parse::<i32>(calorie).unwrap_or_default()
                    })
                    .sum()
            )
            .max()
            .unwrap();

    println!("{:?}", part1);

    let mut calories =
        str.split("\n\n").into_iter().enumerate()
            .map(|(_, s)| s.split("\n").into_iter().enumerate()
                    .map(|(_, calorie)| {
                        return str::parse::<i32>(calorie).unwrap_or_default()
                    })
                    .sum()
            )
            .collect::<Vec<i32>>();
    
    calories.sort_by(|a, b| b.cmp(a));


    let part2 : i32 = calories.into_iter()
            .take(3)
            .sum();

    println!("{:?}", part2);
}