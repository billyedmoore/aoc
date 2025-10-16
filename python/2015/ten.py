def get_input(file_name="ten.input") -> str:
    with open(file_name, "r") as f:
        puzzle_input = f.read().strip()

    return puzzle_input


def look_and_say(input_string: str):
    new_seq = ""
    count = 1
    current = input_string[0]
    for c in input_string[1:]:
        if c == current:
            count += 1
        else:
            new_seq += str(count)
            new_seq += current
            current = c
            count = 1

    if count != 0:
        new_seq += str(count)
        new_seq += current

    return new_seq


def part_one(puzzle_input: str):
    for _ in range(40):
        puzzle_input = look_and_say(puzzle_input)

    print(f"Part One Solution {len(puzzle_input)}")


def part_two(puzzle_input: str):
    for _ in range(50):
        puzzle_input = look_and_say(puzzle_input)

    print(f"Part Two Solution {len(puzzle_input)}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
