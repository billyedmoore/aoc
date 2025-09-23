import copy


def get_input(file_name="eighteen.input") -> dict[tuple[int, int], bool]:
    puzzle_input = {}
    with open(file_name, "r") as f:
        lines = f.readlines()
        for y in range(len(lines)):
            for x in range(len(lines[0])):
                if lines[y][x] == "#":
                    puzzle_input[(x, y)] = True

    return puzzle_input


def numb_neighbors(puzzle_input, x, y):
    offsets = [(0, 1), (0, -1), (-1, 0), (-1, 1), (-1, -1), (1, 1), (1, -1), (1, 0)]
    return sum(
        [puzzle_input.get((x + x_off, y + y_off), False) for x_off, y_off in offsets]
    )


def part_one(puzzle_input: dict[tuple[int, int], bool]):
    side_length = 100

    for _ in range(side_length):
        to_turn_off = []
        to_turn_on = []
        for x in range(0, side_length):
            for y in range(0, side_length):
                if puzzle_input.get((x, y)):
                    if numb_neighbors(puzzle_input, x, y) not in [2, 3]:
                        to_turn_off.append((x, y))
                else:
                    if numb_neighbors(puzzle_input, x, y) == 3:
                        to_turn_on.append((x, y))

        for light in to_turn_off:
            del puzzle_input[light]
        for light in to_turn_on:
            puzzle_input[light] = True

    print(f"Part One Solution {sum(puzzle_input.values())}")


def part_two(puzzle_input: dict[tuple[int, int], bool]):
    side_length = 100
    corners = [
        (0, 0),
        (0, side_length - 1),
        (side_length - 1, side_length - 1),
        (side_length - 1, 0),
    ]

    for corner in corners:
        puzzle_input[corner] = True

    for _ in range(side_length):
        to_turn_off = []
        to_turn_on = []
        for x in range(0, side_length):
            for y in range(0, side_length):
                if puzzle_input.get((x, y)):
                    if (x, y) in corners:
                        pass
                    elif numb_neighbors(puzzle_input, x, y) not in [2, 3]:
                        to_turn_off.append((x, y))
                else:
                    if numb_neighbors(puzzle_input, x, y) == 3:
                        to_turn_on.append((x, y))

        for light in to_turn_off:
            del puzzle_input[light]
        for light in to_turn_on:
            puzzle_input[light] = True

    print(f"Part Two Solution {sum(puzzle_input.values())}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(copy.deepcopy(puzzle_input))
    part_two(copy.deepcopy(puzzle_input))
