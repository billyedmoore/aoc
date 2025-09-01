import math


def get_input(file_name="two.input"):
    shapes = []
    with open(file_name, "r") as f:
        for line in f:
            dims = [int(x) for x in line.strip().split("x")]
            assert len(dims) == 3
            shapes.append(tuple(dims))
    return shapes


def calculate_surface_area(dims: tuple[int, int, int]):
    l, w, h = dims
    return 2 * l * w + 2 * w * h + 2 * h * l


def calculate_extra(dims: tuple[int, int, int]):
    l, w, h = dims
    return min(l * w, w * h, h * l)


def part_one(puzzle_input: list[tuple[int, int, int]]):
    running_total = 0
    for dims in puzzle_input:
        running_total += calculate_surface_area(dims)
        running_total += calculate_extra(dims)

    print(f"Part One Solution {running_total}")


def calculate_shortest_distance(dims: tuple[int, int, int]):
    l, w, h = dims
    return 2 * (min(w + h, l + h, l + w))


def calculate_volume(dims: tuple[int, int, int]):
    return math.prod(dims)


def part_two(puzzle_input: list[tuple[int, int, int]]):
    running_total = 0
    for dims in puzzle_input:
        running_total += calculate_volume(dims)
        running_total += calculate_shortest_distance(dims)

    print(f"Part Two Solution {running_total}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
