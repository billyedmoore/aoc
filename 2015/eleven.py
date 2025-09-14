def get_input(file_name="eleven.input") -> str:
    with open(file_name, "r") as f:
        puzzle_input = f.read().strip()

    return puzzle_input

def increment(password: str):
    for i in reversed(range(0,len(password))):
        if password[i] == "z":
            password = password[:i] + "a" + password[min(i+1,len(password)):]
        else:
            password = password[:i] + chr(ord(password[i])+1) + password[min(i+1,len(password)):]
            break
    return password

def consecutive_run_of_chars(password: str) -> bool:
    run_length = 1
    prev_code = ord(password[0])

    for c in password[1:]:
        ascii_code = ord(c)
        if ascii_code == (prev_code+1):
            run_length += 1
            prev_code += 1
            if run_length == 3:
                return True
        else:
            run_length = 1
            prev_code = ascii_code
    else:
        return False

def repeated_pairs(password: str) -> bool:
    for j in range(2):
        prev = password[0]
        for i,c in enumerate(password[1:]):
            i += 1
            if c == prev:
                if j == 0:
                    password = password[:i-1] + "!?" + password[i+1:]
                break
            prev = c
        else:
            return False
    return True


def is_valid(password: str) -> bool:
    if "i" in password or "o" in password or "l" in password:
        return False
 
    if not consecutive_run_of_chars(password):
        return False
    
    if not repeated_pairs(password):
        return False

    return True

def part_one_and_two(puzzle_input: str):
    final = False
    while True:
        puzzle_input = increment(puzzle_input)
        if is_valid(puzzle_input):
            if not final:
                print(f"Part One Solution {puzzle_input}")
                final = True
            else:
                print(f"Part Two Solution {puzzle_input}")
                break


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one_and_two(puzzle_input)
