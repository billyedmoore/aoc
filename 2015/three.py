def get_input(file_name="three.input"):
    with open(file_name, "r") as f:
        puzzle_input = f.read().strip()
    return puzzle_input


def part_one(puzzle_input: str):
    directions = {"^": (0, 1), "v": (0, -1), ">": (1, 0), "<": (-1, 0)}
    # A note on thought process:
    #   Dict over set to cover off on the second part being about the total
    #   number of visits
    visited = {}

    x, y = (0, 0)
    visited[(0, 0)] = True
    for arrow in puzzle_input:
        d_x, d_y = directions[arrow]
        x += d_x
        y += d_y
        visited[(x, y)] = True

    print(f"Part One Solution {sum(visited.values())}")


def part_two(puzzle_input: str):
    directions = {"^": (0, 1), "v": (0, -1), ">": (1, 0), "<": (-1, 0)}

    visited = {(0, 0): True}

    santa_x, santa_y = (0, 0)
    robot_x, robot_y = (0, 0)
    for i, arrow in enumerate(puzzle_input):
        d_x, d_y = directions[arrow]
        if i % 2 == 0:
            santa_x += d_x
            santa_y += d_y
            visited[(santa_x, santa_y)] = True
        else:
            robot_x += d_x
            robot_y += d_y
            visited[(robot_x, robot_y)] = True

    print(f"Part Two Solution {sum(visited.values())}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
