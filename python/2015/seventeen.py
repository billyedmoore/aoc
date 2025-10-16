def get_input(file_name="seventeen.input"):
    puzzle_input = []
    with open(file_name, "r") as f:
        for line in f:
            puzzle_input.append(int(line))
    return puzzle_input


def traverse(
    used: list[int], available: list[int], combinations: list[tuple[int]]
) -> list[tuple[int]]:
    target = 150
    if sum(used) == target:
        combinations.append(tuple(used))
        return combinations
    if sum(used) > target:
        return combinations

    for i, box in list(enumerate(available)):
        traverse(used + [box], available[i + 1 :], combinations)

    return combinations


def part_one_and_two(puzzle_input: list[int]):
    combinations = traverse([], sorted(puzzle_input), [])
    print(f"Part One Solution {len(combinations)}")
    lengths = [len(combination) for combination in combinations]
    shortest_length = min(lengths)
    result = sum([shortest_length == length for length in lengths])
    print(f"Part Two Solution {result}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one_and_two(puzzle_input)
