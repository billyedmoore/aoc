import hashlib


def get_input(file_name="four.input"):
    with open(file_name, "r") as f:
        puzzle_input = f.read().strip()
    return puzzle_input


def solve(puzzle_input: str, numb_zeros: int):
    # Shocked this just worked :)
    i = 0
    while True:
        i += 1
        hash = hashlib.md5((puzzle_input + str(i)).encode())

        hash_as_hex = hash.hexdigest()
        if hash_as_hex[:numb_zeros] == ("0" * numb_zeros):
            break

    print(f"Part {'One' if numb_zeros == 5 else 'Two'} Solution {i}")


if __name__ == "__main__":
    puzzle_input = get_input()
    solve(puzzle_input, 5)
    solve(puzzle_input, 6)
