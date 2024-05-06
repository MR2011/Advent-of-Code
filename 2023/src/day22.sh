#!/bin/bash
input="../input/day22.txt"

declare -a bricks
declare -a bricks_org
moved=0

parse_input() {
    while read -r line; do
        bricks_org+=("$line")
    done < <(sort -t ',' -k 3,3n "$input" | tr '~,' ' ')
}

# https://topaz.github.io/paste/#XQAAAQCXAgAAAAAAAAA0m0pnuFI8c82uPD0wiI6r5tRTRja96TusHl5ReBJJC7KCbUs/uPAxPCyeFRncTH4KSezn0s9OEeUfCdOimpjNUZg9a95wUsa2Cc+FXPd7fCDXVM14VBcfexoQkM6rZJGO6iXlIAM8wFIAQXWMDuQR9rI2zldbcMWuTcbXBhETy4cPpqDG88EFx85seACyxiWiLIpISUF4AJS+07Qk4D3Lz7kCXkqEx63zHFyvWLkewY0THFzteBbFoADrZoMUTb4Zk8bYJYs935Ve61D5Npx5Y1O+1W6WRVF7+Kob89VrIRy/mUDG4HruImYJ9rto8majXP0yQKRk3CmYmdmQreNDddKPFS8S8cZE49TkUmTc/utR/h0X68vX62mR4VororqzxvyO77QAf8IEux2aB3UkTNHzKBVewBVahC1wJKG3ALCWGN1pXqKdPaqLOtcpJCh0pSqBXTvo9/YxXIdalWoxS88882ctGrnIO2/k0DeiyhwaoPrrBbY1LxQpFdHrT31hWq5Ff//Gg8Wi
drop() {
    skip="$1"
    declare -A peaks
    declare -a area
    moved=0
    for ((i=0; i<${#bricks[@]}; i++)); do
        if [[ "$skip" == "$i" ]]; then
            continue
        fi

        read x1 y1 z1 x2 y2 z2 <<< "${bricks["$i"]}"

        area=()
        for ((x=x1; x<=x2; x++)); do
            for ((y=y1; y<=y2; y++)); do
                area+=("$x,$y")
            done
        done

        peak=0
        for a in "${area[@]}"; do
            p="${peaks["$a"]}"
            peak=$(( p > peak ? p : peak))
        done
        ((peak++))

        z3=$((peak + z2 - z1))
        for a in "${area[@]}"; do
            peaks["$a"]="$z3"
        done

        bricks["$i"]="$x1 $y1 $peak $x2 $y2 $z3"
        (( peak < z1 ? moved++ : moved))
    done
}

parse_input
# let bricks fall down
bricks=("${bricks_org[@]}")
drop "-1"
bricks_org=("${bricks[@]}")

part1=0
part2=0
for ((b=0; b<${#bricks_org[@]}; b++)); do
    bricks=("${bricks_org[@]}")
    drop "$b"
    if [[ "$moved" == 0 ]]; then
        ((part1++))
    fi
    ((part2+=moved))
done

echo "Part 1: $part1"
echo "Part 2: $part2"
