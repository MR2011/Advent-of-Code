from collections import defaultdict
def score_at_generation(current_gen, generations):
    last_sum = sum(current_gen.keys())
    last_delta = 0
    for generation in range(generations):
        key_min = min(current_gen.keys()) - 2
        key_max = max(current_gen.keys()) + 3
        temp = {i: rules[tuple([current_gen[idx] for idx in range(i - 2, i + 3)])] for i in range(key_min, key_max)}
        current_gen = defaultdict(lambda: '.', {i: char for i, char in temp.items() if char == '#'})
        new_sum = sum(current_gen.keys())
        new_delta = new_sum - last_sum
        if new_delta - last_delta == 0:
            return (generations - generation) * new_delta + new_sum
        last_sum = new_sum
        last_delta = new_delta
    return last_sum


with open('input.txt') as f:
    initial = f.readline().strip().split(': ')[1]
    f.readline()
    rules = defaultdict(lambda: '.', {tuple(rule[0]): rule[1] for rule in [line.strip().split(' => ') for line in f]})
first_gen = defaultdict(lambda: '.', {i: char for i, char in enumerate(initial) if char == '#'})
print(score_at_generation(first_gen, 20))
print(score_at_generation(first_gen, 50000000000))