def get_input():
    file_name = "one.input"

    with open(file_name, "r") as f:
        result = f.read().strip()

    return result


def part_one(puzzle_input: str):
    key = {"(": 1, ")": -1}
    running_total = 0

    for c in puzzle_input:
        running_total += key[c]

    print(f"Part One Solution {running_total}")


def part_two(puzzle_input: str):
    key = {"(": 1, ")": -1}
    running_total = 0
    solution = -1

    for i, c in enumerate(puzzle_input):
        running_total += key[c]
        if running_total == -1:
            # +1 as indexed from 1
            solution = i + 1
            break

    assert solution != -1

    print(f"Part Two Solution {solution}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
