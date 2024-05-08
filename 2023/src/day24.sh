#!/bin/bash
input="../input/day24.txt"

declare -a hailstones

parse_input() {
    while read -r line; do
        hailstones+=("$line")
    done < <(cat "$input" | tr '@' ',' | tr -d ' ')
}

# https://topaz.github.io/paste/#XQAAAQDXAQAAAAAAAAAzHIoib6p4r/McpYgEEgWhHoa5LSRMkVi92ASWXgRJn/53WGzZWzJlNzR/LeXSZEQnBkC+jD+efAupRol5bOfXbJwxvGQUitWOYhQnNhp1IIb+hfC8AaLOFmv4wIp5CzBQKrm28BEIBOYFbMPy6M2OXUGYq6JCT3QdyxTKD9DDQMSJxrkOB+NWjG5qDNsvSBbbPvS8lG4/FpPx+2veExcgoc1tAwTU3Qm0SgsCtVQWHI8I9jxt0YehRiYZefxqvZeYrbI8+6F96APmhePvuzdZx6sQKKM7WruVIJMd2iHAHtqgpWUDcatGq6vrkXen6cKjBzq8duXJkYDM8SV3HoxzxXUgJfO9HTRN5uEMWvpTuvENABmirX26SLG3RvsnrTclw2wWBwFbEjx1XcLavGTkik//6bcLRA==
part1() {
    parse_input
    local count=0
    local min="200000000000000"
    local max="400000000000000"
    for ((i=0; i<${#hailstones[@]}; i++)); do
        for ((j=i+1; j<${#hailstones[@]}; j++)); do
            a="${hailstones["$i"]}"
            b="${hailstones["$j"]}"

            IFS="," read px1 py1 pz1 vx1 vy1 vz1 <<< "$a"
            IFS="," read px2 py2 pz2 vx2 vy2 vz2 <<< "$b"

            if (( vy1*vx2 == vx1*vy2 )); then
                continue
            fi

            t1=$(((vy2*(px1-px2) - vx2*(py1-py2)) / (vy1*vx2 - vx1*vy2)))
            t2=$(((vy1*(px2-px1) - vx1*(py2-py1)) / (vy2*vx1 - vx2*vy1)))

            p=$((px1+t1*vx1))
            q=$((py1+t1*vy1))
            if [[ "$t1" -gt "0" && "$p" -gt "$min" && "$p" -lt "$max" ]]; then
                if [[ "$t2" -gt "0" && "$q" -gt "$min" && "$q" -lt "$max" ]]; then
                    ((count++))
                fi
            fi
        done
    done
    echo "$count"
}

calculate() {
    local -a params=("$@")
    instructions="
    vax=${params[0]};
    vbx=${params[1]};
    vcx=${params[2]};
    vdx=${params[3]};
    vex=${params[4]};
    pax=${params[5]};
    pbx=${params[6]};
    pcx=${params[7]};
    pdx=${params[8]};
    pex=${params[9]};
    vay=${params[10]};
    vby=${params[11]};
    vcy=${params[12]};
    vdy=${params[13]};
    vey=${params[14]};
    pay=${params[15]};
    pby=${params[16]};
    pcy=${params[17]};
    pdy=${params[18]};
    pey=${params[19]};

    denominator = (((vay - vby) * (pbx - pcx) - (vby - vcy) * (pax - pbx)) * ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx)) - ((vby - vcy) * (pcx - pdx) - (vcy - vdy) * (pbx - pcx)) * ((pay-pby) * (pbx - pcx) - (pby-pcy) * (pax - pbx))) * (((vbx - vcx) * (pcx - pdx) - (vcx - vdx) * (pbx - pcx)) * ((pcy-pdy) * (pdx - pex) - (pdy-pey) * (pcx - pdx)) - ((vcx - vdx) * (pdx - pex) - (vdx - vex) * (pcx - pdx)) * ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx))) - (((vby - vcy) * (pcx - pdx) - (vcy - vdy) * (pbx - pcx)) * ((pcy-pdy) * (pdx - pex) - (pdy-pey) * (pcx - pdx)) - ((vcy - vdy) * (pdx - pex) - (vdy - vey) * (pcx - pdx)) * ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx))) * (((vax - vbx) * (pbx - pcx) - (vbx - vcx) * (pax - pbx)) * ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx)) - ((vbx - vcx) * (pcx - pdx) - (vcx - vdx) * (pbx - pcx)) * ((pay-pby) * (pbx - pcx) - (pby-pcy) * (pax - pbx)));
    numerator = (((pay-pby) * (pbx - pcx) - (pby-pcy) * (pax - pbx)) * (((pbx*vby - pcx*vcy) - (pby*vbx - pcy*vcx)) * (pcx - pdx) - ((pcx*vcy - pdx*vdy) - (pcy*vcx - pdy*vdx)) * (pbx - pcx)) - ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx)) * (((pax*vay - pbx*vby) - (pay*vax - pby*vbx)) * (pbx - pcx) - ((pbx*vby - pcx*vcy) - (pby*vbx - pcy*vcx)) * (pax - pbx))) * (((vbx - vcx) * (pcx - pdx) - (vcx - vdx) * (pbx - pcx)) * ((pcy-pdy) * (pdx - pex) - (pdy-pey) * (pcx - pdx)) - ((vcx - vdx) * (pdx - pex) - (vdx - vex) * (pcx - pdx)) * ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx))) - (((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx)) * (((pcx*vcy - pdx*vdy) - (pcy*vcx - pdy*vdx)) * (pdx - pex) - ((pdx*vdy - pex*vey) - (pdy*vdx - pey*vex)) * (pcx - pdx)) - ((pcy-pdy) * (pdx - pex) - (pdy-pey) * (pcx - pdx)) * (((pbx*vby - pcx*vcy) - (pby*vbx - pcy*vcx)) * (pcx - pdx) - ((pcx*vcy - pdx*vdy) - (pcy*vcx - pdy*vdx)) * (pbx - pcx))) * (((vax - vbx) * (pbx - pcx) - (vbx - vcx) * (pax - pbx)) * ((pby-pcy) * (pcx - pdx) - (pcy-pdy) * (pbx - pcx)) - ((vbx - vcx) * (pcx - pdx) - (vcx - vdx) * (pbx - pcx)) * ((pay-pby) * (pbx - pcx) - (pby-pcy) * (pax - pbx)));
    - numerator / denominator;
    "
    echo "$instructions" | bc
}

# https://www.reddit.com/r/adventofcode/comments/18qabia/2023_day_24_part_2_3_am_on_a_christmas_morning/#lightbox
# https://github.com/cgsdev0/aoc-2023/blob/main/day24/p2.sh
part2() {
    local -a px py pz vx vy vz
    while IFS="," read ipx ipy ipz ivx ivy ivz; do
        px+=("$ipx")
        py+=("$ipy")
        pz+=("$ipz")
        vx+=("$ivx")
        vy+=("$ivy")
        vz+=("$ivz")
    done < <(cat "$input" | tr '@' ',' | tr -d ' ')

    a=$(calculate "${vx[0]}" "${vx[1]}" "${vx[2]}" "${vx[3]}" "${vx[4]}" "${px[0]}" "${px[1]}" "${px[2]}" "${px[3]}" "${px[4]}" "${vy[0]}" "${vy[1]}" "${vy[2]}" "${vy[3]}" "${vy[4]}" "${py[0]}" "${py[1]}" "${py[2]}" "${py[3]}" "${py[4]}")
    b="$(calculate "${vy["0"]}" "${vy["1"]}" "${vy["2"]}" "${vy["3"]}" "${vy["4"]}" "${py["0"]}" "${py["1"]}" "${py["2"]}" "${py["3"]}" "${py["4"]}" "${vz["0"]}" "${vz["1"]}" "${vz["2"]}" "${vz["3"]}" "${vz["4"]}" "${pz["0"]}" "${pz["1"]}" "${pz["2"]}" "${pz["3"]}" "${pz["4"]}")"
    c="$(calculate "${vz["0"]}" "${vz["1"]}" "${vz["2"]}" "${vz["3"]}" "${vz["4"]}" "${pz["0"]}" "${pz["1"]}" "${pz["2"]}" "${pz["3"]}" "${pz["4"]}" "${vx["0"]}" "${vx["1"]}" "${vx["2"]}" "${vx["3"]}" "${vx["4"]}" "${px["0"]}" "${px["1"]}" "${px["2"]}" "${px["3"]}" "${px["4"]}")"
    echo "$a+$b+$c" | bc
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"