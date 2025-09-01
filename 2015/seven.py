def get_input(file_name="seven.input"):
    puzzle_input = {}  # key: wire_name, value: gate_string
    with open(file_name, "r") as f:
        for line in f:
            words = line.strip().split(" -> ")

            puzzle_input[words[1]] = words[0]

    return puzzle_input


def resolve_operand(puzzle_input: dict[str, str], operand: str) -> int:
    # Operand is a number
    if operand.isnumeric():
        return int(operand)
    # Operand is a ref to another wire
    else:
        evaluated = evalute_gate_string(puzzle_input, puzzle_input[operand])
        # Replace the gate string with the cacluated answer
        puzzle_input[operand] = str(evaluated)
        return evaluated


def evalute_gate_string(puzzle_input: dict[str, str], string: str) -> int:
    words = string.split(" ")
    if len(words) == 1:
        return resolve_operand(puzzle_input, words[0])
    if len(words) == 2:
        if words[0] == "NOT":
            return ~resolve_operand(puzzle_input, words[1])
        else:
            raise ValueError("Length two should be NOT.")
    if len(words) == 3:
        if words[1] == "AND":
            return resolve_operand(puzzle_input, words[0]) & resolve_operand(
                puzzle_input, words[2]
            )
        elif words[1] == "OR":
            return resolve_operand(puzzle_input, words[0]) | resolve_operand(
                puzzle_input, words[2]
            )
        elif words[1] == "LSHIFT":
            return resolve_operand(puzzle_input, words[0]) << resolve_operand(
                puzzle_input, words[2]
            )
        elif words[1] == "RSHIFT":
            return resolve_operand(puzzle_input, words[0]) >> resolve_operand(
                puzzle_input, words[2]
            )
        else:
            raise ValueError("Invalid Operand.")
    else:
        raise ValueError("Expected 1-3 words.")


def part_one(
    puzzle_input: dict[str, str],
):
    result = evalute_gate_string(puzzle_input.copy(), "a")
    print(f"Part One Solution {result}")
    return result


def part_two(puzzle_input: dict[str, str], part_one_result: int):
    puzzle_input = puzzle_input.copy()
    puzzle_input["b"] = str(part_one_result)
    result = evalute_gate_string(puzzle_input, "a")
    print(f"Part Two Solution {result}")


if __name__ == "__main__":
    puzzle_input = get_input()
    result = part_one(puzzle_input)
    part_two(puzzle_input, result)
