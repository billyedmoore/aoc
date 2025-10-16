def get_input(file_name="eight.input") -> list[str]:
    with open(file_name, "r") as f:
        lines = [line.strip() for line in f.readlines()]

    return lines


def part_one(
    puzzle_input: list[str],
):
    tot = 0
    for line in puzzle_input:
        tot += len(line)
        tot -= len(eval(line))
    print(f"Part One Solution {tot}")


def part_two(
    puzzle_input: list[str],
):
    tot = 0
    for line in puzzle_input:
        tot -= len(line)
        # " won't be escaped as ' can be used to
        # bookend the string, one char would have
        # been added for each (" -> \")
        tot += len(repr(line)) + line.count('"')
    print(f"Part Two Solution {tot}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
