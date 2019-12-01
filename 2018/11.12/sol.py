from utils.decorators import time_it

puzzle_input = 3214

grid = []


def fill_grid(n):
    grid.clear()
    for x in range(300):
        grid.append([])
        for y in range(300):
            grid[x].append(set_power(n, x + 1, y + 1))


def set_power(n, x, y):
    rack_id = x + 10
    power = rack_id * y + n
    power *= rack_id
    power = (power // 100) % 10
    power -= 5
    return power


def get_power(x, y, size):
    power = 0
    for i in range(size):
        for j in range(size):
            power += grid[x + i][y + j]
    return power


@time_it
def part_one():
    max_power = 0
    cell = None
    for x in range(297):
        for y in range(297):
            power = get_power(x, y, 3)
            if power > max_power:
                max_power = power
                cell = (x+1, y+1)
    return cell


@time_it
def part_two():
    max_power = 0
    cell = None
    for size in range(1, 20):
        max_x = 300 - size
        max_y = 300 - size
        for x in range(max_x):
            for y in range(max_y):
                power = get_power(x, y, size)
                if power > max_power:
                    max_power = power
                    cell = (x + 1, y + 1, size)
    return cell

fill_grid(puzzle_input)
print('Part one:', part_one())

fill_grid(puzzle_input)
print('Part two:', part_two())