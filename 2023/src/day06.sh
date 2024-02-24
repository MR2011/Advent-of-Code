#!/bin/bash

input="../input/day06.txt"

declare -a times=()
declare -a distances=()

parse_input_p1() {
    times=($(awk '/Time:/ {print $2,$3,$4,$5}' "$input"))
    distances=($(awk '/Distance:/ {print $2,$3,$4,$5}' "$input"))
}

parse_input_p2() {
    times=($(sed -n 's/Time: *\([0-9]*\) *\([0-9]*\) *\([0-9]*\) *\([0-9]*\)/\1\2\3\4/p' "$input"))
    distances=($(sed -n 's/Distance: *\([0-9]*\) *\([0-9]*\) *\([0-9]*\) *\([0-9]*\)/\1\2\3\4/p' "$input"))
}

simulate() {
    total_wins=1
    for ((i=0; i<${#times[@]}; i++)); do
        total_time="${times[i]}"
        max_distance="${distances[i]}"
        wins=0
        for ((v=0; v<total_time; v++)); do
            ((t=total_time - v))
            ((s=v*t))
            if (( s > max_distance )); then
                ((wins++))
            fi
        done
        (( wins > 0 )) && ((total_wins*=wins))
    done
    echo "$total_wins"
}

part1() {
    parse_input_p1
    simulate
}

part2() {
    parse_input_p2
    simulate
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"