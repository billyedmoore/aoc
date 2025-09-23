def get_input(file_name="sixteen.input"):
    puzzle_input = []
    with open(file_name, "r") as f:
        for line in f:
            sue = {}
            line = "".join(line.split(":")[1:])
            for pair in line.split(","):
                item, count = pair.split()
                sue[item] = int(count)
            puzzle_input.append(sue)
    return puzzle_input


def compare(sue_val, golden_val, key):
    if golden_val == None:
        return True

    greater_than = ["cats", "trees"]
    less_than = ["pomeranians", "goldfish"]

    if key in greater_than:
        return sue_val > golden_val
    elif key in less_than:
        return sue_val < golden_val
    else:
        return sue_val == golden_val


def part_one_and_two(puzzle_input: list[dict], golden: dict):
    part_one = False
    part_two = False

    for i, sue in enumerate(puzzle_input):
        if not part_one and all([golden.get(key, sue[key]) == sue[key] for key in sue]):
            part_one = i+1
        if not part_two and all([compare(sue[key], golden.get(key), key) for key in sue]):
            part_two = i+1
        if part_two and part_one:
            break
    print(f"Part One Solution {part_one}")
    print(f"Part Two Solution {part_two}")

if __name__ == "__main__":
    wrapping_paper = {
        "children": 3,
        "cats": 7,
        "samoyeds": 2,
        "promerainians": 3,
        "akitas": 0,
        "vizslas": 0,
        "goldfish": 5,
        "trees": 3,
        "cars": 2,
        "perfumes": 1,
    }
    puzzle_input = get_input()
    part_one_and_two(puzzle_input, wrapping_paper)
