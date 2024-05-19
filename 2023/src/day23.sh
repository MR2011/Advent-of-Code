#!/bin/bash
input="../input/day23.txt"

declare -A grid
declare -a DIRECTIONS=("0,-1" "1,0" "-1,0" "0,1")

create_grid() {
    row=0
    columns=0
    while read -r line; do
        for ((col=0; col<${#line}; col++)); do
            grid["$row,$col"]=${line:$col:1}
            columns=${#line}
        done
        ((row++))
    done < "$input"
    grid["rows"]="$row"
    grid["columns"]="$columns"
}

solve() {
    local -a queue
    local -A costs
    local src="$1"
    local dst="$2"
    local size="${grid["rows"]}"
    costs["src"]="0"
    queue+=("$src:")
    while [[ ${#queue[@]} -gt 0 ]]; do
        current="${queue[0]}"
        queue=("${queue[@]:1}")
        IFS=":" read -r n path <<< "$current"

        if [[ "$n" == "$dst" ]]; then
            continue
        fi

        unset neighbors
        local -a neighbors
        IFS="," read -r row col <<< "$n"
        local value="${grid["$n"]}"
        
        if [[ "$value" == "v" ]]; then
            neighbors+=("$((row+1)),$col")
        elif [[ "$value" == "^" ]]; then
            neighbors+=("$((row-1)),$col")
        elif [[ "$value" == "<" ]]; then
            neighbors+=("$row,$((col-1))")
        elif [[ "$value" == ">" ]]; then
            neighbors+=("$row,$((col+1))")
        else
            for dir in "${DIRECTIONS[@]}"; do
                IFS="," read dr dc <<< "$dir"
                ((r=row+dr))
                ((c=col+dc))
                nv="${grid["$r,$c"]}"
                if [[ "$nv" != "#" && "$nv" != "" ]]; then
                    neighbors+=("$r,$c")
                fi
            done
        fi

        for neighbor in "${neighbors[@]}"; do
            if echo "$path" | grep -q ";$neighbor;"; then
                continue
            fi

            cost="${costs["$row,$col"]}"
            if [[ "$cost" == "" ]]; then
                cost="0"
            fi
            ((cost++))

            neighbor_cost="${costs["$neighbor"]}"

            if [[ "$neighbor_cost" == "" || "$cost" -gt "$neighbor_cost" ]]; then
                costs["$neighbor"]="$cost"
                queue+=("$neighbor:$path;$neighbor;")
            fi
        done

    done
    echo "${costs["$dst"]}"
}
# https://topaz.github.io/paste/#XQAAAQDnBwAAAAAAAAAyGUj/T2CE9ViTmgMBck9vbIRP1S57/NLKT41s2yThPmtP4XCNLVkH5204yDdXWbfm+AHSpAP01s1EUYD6riIaAcXqcFQnzu/MZ6N84k1GyH3FOI8iRqQFuZFR/Liw6WG2HydL6lZKHcjq4EzavHr6NC9mwJDB7E1WhfoRTFG0u0YUsEVLaygGmfE6e9Oqnoi53teA1w7q/CXW6HfswljDgYNB4jsKyW3S3QI3B1Q5gwkPh5NPh4xQI4MxYSNrVkyDzGFXwQk4dhgu5ak9h4WQi46SvWNzG2oMZDSSJ+9Nkc4+ZnNlLudNIiL/FsKO4of2IKZNE9HIlF3Irr2QcNHNrPozBIe6jp2OWQn2aIUE95xkYpS8FmLJ+nBbXmFYHi1JB8hs8Y1f5InCmi9bD6nMmuvl+SluyLmyBPETE/uONt2sdGl5R8KHvDTcse70y14BJLSw0ZXUzNr8gYpBWA5Jw2GRk8mrY37gzOdHpt7XeTZ1JfzpXOAHMJEHWdbCsHukoW0QN+AZYeb3spv0yshOUlDyR2n7QztmUHXAZRNotapMQVGrMi/0vnUR7Pv2wQhHinyEQe6me4tj6Y3JEwBs+49nzzPmwvZtkaaKoOUjle9sbtvHepnxrOR8wut6+7EYtrhvA/8349my5dERC3+QFAzGGOcrb34LBYYNJ7qsh0XDXH3ET0SP7AdZOfgEj8616nKwM+hEAhysbBvkj66FBYtp/Xzydqr9Qd3thIPOmcPOhkSCdYimjBUqdy2SZJHDFwH/fxwdr6BWeaJRjZTEbV2b2hGrwgFqHH6x4UwrKh/Yncx5p790pxnTO+ZokgyQkSv8McZc1Lqb4FqKaIJP/+L5WGQ=
part1() {
    create_grid "$input"
    solve "0,1" "140,139"
}

part2() {
    echo "2"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"