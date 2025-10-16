from dataclasses import dataclass
import copy


@dataclass
class Reindeer:
    # From the input
    name: str
    speed: int
    flight_time: int
    rest_time: int

    # To manage the state
    flight_countdown: int
    rest_countdown: int = 0
    distance_travelled: int = 0

    # For part two
    score: int = 0


def get_input(file_name="fourteen.input") -> list[Reindeer]:
    puzzle_input = []
    with open(file_name, "r") as f:
        for line in f:
            (
                name,
                _,
                _,
                speed_str,
                _,
                _,
                flight_time_str,
                _,
                _,
                _,
                _,
                _,
                _,
                rest_time_str,
                _,
            ) = line.strip()[:-1].split()

            puzzle_input.append(
                Reindeer(
                    name=name,
                    speed=int(speed_str),
                    flight_time=int(flight_time_str),
                    rest_time=int(rest_time_str),
                    flight_countdown=int(flight_time_str),
                )
            )

    return puzzle_input


def interate_reindeer(deer: Reindeer):
    if deer.rest_countdown:
        deer.rest_countdown -= 1
        if not deer.rest_countdown:
            deer.flight_countdown = deer.flight_time
    elif deer.flight_countdown:
        deer.flight_countdown -= 1
        deer.distance_travelled += deer.speed
        if not deer.flight_countdown:
            deer.rest_countdown = deer.rest_time
    else:
        raise ValueError(
            "Reindeer must be flying or resting," " anything else would be silly."
        )


def part_one(puzzle_input: list[Reindeer]):
    for _ in range(2503):
        [interate_reindeer(deer) for deer in puzzle_input]
    print(
        f"Part One Solution {max([deer.distance_travelled for deer in puzzle_input])}"
    )


def part_two(puzzle_input: list[Reindeer]):
    for _ in range(2503):
        [interate_reindeer(deer) for deer in puzzle_input]
        top_dist = max(
            puzzle_input, key=lambda deer: deer.distance_travelled
        ).distance_travelled
        for deer in puzzle_input:
            if deer.distance_travelled == top_dist:
                deer.score += 1

    print(f"Part Two Solution {max([deer.score for deer in puzzle_input])}")


if __name__ == "__main__":
    puzzle_input = get_input()
    part_one(copy.deepcopy(puzzle_input))
    part_two(copy.deepcopy(puzzle_input))
