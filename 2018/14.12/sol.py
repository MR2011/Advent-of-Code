def part1(inp):
    scores = [3, 7]
    n = int(inp)

    a, b = 0, 1

    for _ in range(n+10):
        asd = str(scores[a] + scores[b])
        scores.extend(map(int, asd))
        a += scores[a] + 1
        b += scores[b] + 1
        a %= len(scores)
        b %= len(scores)
    
    print(*scores[n:n+10],sep="")


def part2(inp):
    scores = [3, 7]
    blah = list(map(int, inp))

    a, b = 0, 1

    while True:
        asd = str(scores[a] + scores[b])
        scores.extend(map(int, asd))
        a += scores[a] + 1
        b += scores[b] + 1
        a %= len(scores)
        b %= len(scores)
        if scores[-len(blah):] == blah or scores[-len(blah)-1:-1] == blah:
            break
    
    if scores[-len(blah):] == blah:
        print(len(scores) - len(blah))
    else:
        print(len(scores) - len(blah) - 1)

part2("047801")