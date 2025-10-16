from dataclasses import dataclass
import math


@dataclass
class Ingredient:
    name: str
    capacity: int
    durability: int
    flavour: int
    texture: int
    calories: int


def get_input(file_name="fifteen.input") -> list[Ingredient]:
    puzzle_input = []
    with open(file_name, "r") as f:
        for line in f:
            line = line.replace(":", "")
            line = line.replace(",", "")
            name, _, capacity, _, durability, _, flavour, _, texture, _, calories = (
                line.split()
            )
            puzzle_input.append(
                Ingredient(
                    name,
                    int(capacity),
                    int(durability),
                    int(flavour),
                    int(texture),
                    int(calories),
                )
            )

    return puzzle_input


def get_combinations(target):
    """
    Assumption: Always 4 ingredients as input
    """
    combinations = []
    for x in range(0, target):
        for y in range(0, target):
            for z in range(0, target):
                if target - (x + y + z) > 0:
                    combinations.append((x, y, z, (target - (x + y + z))))
    return combinations


def part_one_and_two(puzzle_input: list[Ingredient]):
    biggest = 0
    biggest_500_cal = 0
    for combination in get_combinations(100):
        capacity = 0
        durability = 0
        flavour = 0
        texture = 0
        calories = 0

        for i in range(len(combination)):
            capacity += puzzle_input[i].capacity * combination[i]
            durability += puzzle_input[i].durability * combination[i]
            flavour += puzzle_input[i].flavour * combination[i]
            texture += puzzle_input[i].texture * combination[i]
            calories += puzzle_input[i].calories * combination[i]

        sol = math.prod(
            [max(0, elem) for elem in [capacity, durability, flavour, texture]]
        )

        if sol > biggest:
            biggest = sol
        if calories == 500 and sol > biggest_500_cal:
            biggest_500_cal = sol

    print(f"Part One Solution {biggest}")
    print(f"Part Two Solution {biggest_500_cal}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one_and_two(puzzle_input)
