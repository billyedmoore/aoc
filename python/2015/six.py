from enum import Enum


class Instruction(Enum):
    ON = "on"
    OFF = "off"
    TOGGLE = "toggle"


def parse_coord(coord: str):
    coord_as_list = coord.split(",")

    assert len(coord_as_list) == 2
    return int(coord_as_list[0]), int(coord_as_list[1])


def get_input(file_name="six.input"):
    puzzle_input = []
    with open(file_name, "r") as f:
        for line in f:
            words = line.split(" ")

            assert len(words) in [4, 5]

            end = parse_coord(words[-1])
            start = parse_coord(words[-3])

            if words[0] == "toggle":
                instruction = Instruction.TOGGLE
            elif words[1] == "on":
                instruction = Instruction.ON
            elif words[1] == "off":
                instruction = Instruction.OFF
            else:
                raise ValueError()

            puzzle_input.append((instruction, (start, end)))

    return puzzle_input


def part_one(
    puzzle_input: list[tuple[Instruction, tuple[tuple[int, int], tuple[int, int]]]],
):
    lights = {}
    for inp in puzzle_input:
        instruction, (start, end) = inp
        for x in range(start[0], end[0] + 1):
            for y in range(start[1], end[1] + 1):
                if instruction == Instruction.TOGGLE:
                    lights[(x, y)] = not lights.get((x, y), False)
                elif instruction == Instruction.ON:
                    lights[(x, y)] = True
                elif instruction == Instruction.OFF:
                    lights[(x, y)] = False

    print(f"Part One Solution {sum(lights.values())}")


def part_two(
    puzzle_input: list[tuple[Instruction, tuple[tuple[int, int], tuple[int, int]]]],
):
    lights = {}
    for inp in puzzle_input:
        instruction, (start, end) = inp
        for x in range(start[0], end[0] + 1):
            for y in range(start[1], end[1] + 1):
                if instruction == Instruction.TOGGLE:
                    lights[(x, y)] = lights.get((x, y), 0) + 2
                elif instruction == Instruction.ON:
                    lights[(x, y)] = lights.get((x, y), 0) + 1
                elif instruction == Instruction.OFF:
                    lights[(x, y)] = max(lights.get((x, y), 0) - 1, 0)

    print(f"Part Two Solution {sum(lights.values())}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
