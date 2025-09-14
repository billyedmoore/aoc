import json


def get_input(file_name="twelve.input") -> list | dict:
    with open(file_name, "r") as f:
        puzzle_input = json.load(f)

    return puzzle_input


def find_numbers(puzzle_input: list | dict | int | str) -> int:
    if isinstance(puzzle_input, int):
        return puzzle_input
    is_list = isinstance(puzzle_input, list)
    if is_list or isinstance(puzzle_input, dict):
        children = puzzle_input if is_list else puzzle_input.values()
        return sum([find_numbers(child) for child in children])
    return 0


def find_numbers_without_red(puzzle_input: list | dict | int | str) -> int:
    if isinstance(puzzle_input, int):
        return puzzle_input
    is_dict = isinstance(puzzle_input, dict)
    if is_dict or isinstance(puzzle_input, list):
        if is_dict and ("red" in puzzle_input.values() or "red" in puzzle_input.keys()):
            return 0
        children = puzzle_input.values() if is_dict else puzzle_input
        return sum([find_numbers_without_red(child) for child in children])
    return 0


def part_one(puzzle_input: list | dict):
    print(f"Part One Solution {find_numbers(puzzle_input)}")


def part_two(puzzle_input: list | dict):
    print(f"Part Two Solution {find_numbers_without_red(puzzle_input)}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
