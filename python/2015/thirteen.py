def get_input(file_name="thirteen.input") -> dict:
    puzzle_input = {}
    with open(file_name, "r") as f:
        for line in f:
            (
                person_one,
                _,
                direction_str,
                magnitude_str,
                _,
                _,
                _,
                _,
                _,
                _,
                person_two,
            ) = line.strip()[:-1].split()
            direction = -1 if direction_str == "lose" else 1
            number = int(magnitude_str) * direction

            puzzle_input[person_one] = puzzle_input.get(person_one, {})
            puzzle_input[person_one][person_two] = number

    return puzzle_input


def traverse(puzzle_input: dict[str, dict[str, int]], visited: list, total: int):
    if len(visited) == len(puzzle_input):
        return (
            total
            + puzzle_input[visited[-1]][visited[0]]
            + puzzle_input[visited[0]][visited[-1]]
        )

    sols = []
    for name in puzzle_input:
        if name in visited:
            continue

        if visited:
            new_tot = (
                total
                + puzzle_input[name][visited[-1]]
                + puzzle_input[visited[-1]][name]
            )
        else:
            new_tot = total

        sols.append(traverse(puzzle_input, [*visited, name], new_tot))

    return max(sols)


def part_one(puzzle_input: dict):
    print(f"Part One Solution {traverse(puzzle_input,["Alice"],0)}")


def part_two(puzzle_input: dict):
    puzzle_input["Billy"] = {}
    for name in puzzle_input:
        puzzle_input["Billy"][name] = 0
        puzzle_input[name]["Billy"] = 0

    print(f"Part One Solution {traverse(puzzle_input,["Alice"],0)}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(puzzle_input)
    part_two(puzzle_input)
