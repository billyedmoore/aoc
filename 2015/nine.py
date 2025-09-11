from enum import Enum


class Direction(Enum):
    SHORTEST = "shortest"  # Part One
    LONGEST = "longest"  # Part Two


def get_input(file_name="nine.input") -> dict[str, dict[str, int]]:
    """
    Format:
        keys are origins
        values are dicts with {destination: distance}
    """
    puzzle_input = {}

    with open(file_name, "r") as f:
        for line in f:
            origin, _, dest, _, dist = line.split()
            puzzle_input[origin] = puzzle_input.get(origin, {})
            puzzle_input[dest] = puzzle_input.get(dest, {})
            puzzle_input[origin][dest] = int(dist)
            puzzle_input[dest][origin] = int(dist)

    return puzzle_input


def traverse(
    puzzle_input: dict[str, dict[str, int]],
    dist: int,
    visted: list[str],
    direction: Direction = Direction.SHORTEST,
) -> int | float:
    if len(visted) == len(puzzle_input):
        return dist

    dists = [
        traverse(
            puzzle_input,
            dist + puzzle_input[visted[-1]][dest],
            visted + [dest],
            direction=direction,
        )
        for dest in puzzle_input[visted[-1]]
        if dest not in visted
    ]

    if dists:
        return {Direction.SHORTEST: min, Direction.LONGEST: max}[direction](dists)
    else:
        return {Direction.SHORTEST: float("inf"), Direction.LONGEST: float("-inf")}[
            direction
        ]


def part_one(
    puzzle_input: dict[str, dict[str, int]],
):
    sol = min(
        [
            traverse(puzzle_input, 0, [origin], direction=Direction.SHORTEST)
            for origin in puzzle_input
        ]
    )

    print(f"Part One Solution {sol}")


def part_two(
    puzzle_input: dict[str, dict[str, int]],
):
    sol = max(
        [
            traverse(puzzle_input, 0, [origin], direction=Direction.LONGEST)
            for origin in puzzle_input
        ]
    )

    print(f"Part Two Solution {sol}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
