def get_input(file_name="five.input"):
    with open(file_name, "r") as f:
        puzzle_input = [ln.strip() for ln in f.readlines()]
    return puzzle_input


def part_one_is_nice_string(string: str) -> bool:
    # not containing these substrings
    if "ab" in string or "cd" in string or "pq" in string or "xy" in string:
        return False

    # more than 3 vowels
    if len([c for c in string if c in "aeiou"]) < 3:
        return False

    # repeated char
    for i in range(1, len(string)):
        if string[i] == string[i - 1]:
            return True

    return False


def part_one(puzzle_input: list[str]):
    count = sum([part_one_is_nice_string(s) for s in puzzle_input])

    print(f"Part One Solution {count}")


def part_two_is_nice_string(string: str) -> bool:
    # repeated char with gap ie [char][other char][char]
    for i in range(2, len(string)):
        if string[i] == string[i - 2]:
            break
    else:
        return False

    # if pair is repeated (with no overlap)
    for i in range(0, len(string) - 1):
        pair = string[i : i + 2]
        rest = string[:i] + "!!" + string[i + 2 :]

        if pair in rest:
            return True

    return False


def part_two(puzzle_input: list[str]):
    count = sum([part_two_is_nice_string(s) for s in puzzle_input])

    print(f"Part Two Solution {count}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
