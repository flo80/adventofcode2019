use std::fs;

fn main() {
    println!("Required fuel: ");
    let numbers = read_input();

    let sum: u32 = numbers.iter().map(|n| calc_fuel(*n)).sum();

    println!("{:?}", sum);
}

fn read_input() -> Vec<u32> {
    let contents = fs::read_to_string("input")
        .expect("could not read input")
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();

    return contents;
}

fn calc_fuel(mass: u32) -> u32 {
    mass / 3 - 2
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn samples() {
        assert_eq!(2, calc_fuel(12));
        assert_eq!(2, calc_fuel(14));
        assert_eq!(654, calc_fuel(1969));
        assert_eq!(33583, calc_fuel(100756));
    }
}
