#!/bin/bash
input="../input/day25.txt"

declare -A wiring
declare -A edge_count
declare -a nodes
nodes_size=0

add_item() {
    local key="$1"
    local value="$2"

    local s="${wiring["$key"]}"

    if [[ -z "$s" ]]; then
        wiring["$key"]="$value"
    else
        if [[ "$s" != *"$value"* ]]; then
            wiring["$key"]="$s $value"
        fi
    fi
}

remove_item() {
    local key="$1"
    local subs="$2"

    local s="${wiring["$key"]}"
    local modified="${s/$subs/}"
    modified=$(echo "$modified" | tr -s ' ')
    wiring["$key"]="$modified"
}

parse_input() {
    while read -r line; do
        IFS=":" read key values <<< "$line"
        for v in ${values:1}; do
            add_item "$key" "$v"
            add_item "$v" "$key"
        done
    done < "$input"
    nodes=("${!wiring[@]}")
    nodes_size="${#nodes[@]}"
}

remove() {
    read -a edges <<< "$@"
    for e in "${edges[@]}"; do
        IFS="," read a b <<< "$e"
        remove_item "$a" "$b"
        remove_item "$b" "$a"
    done
}

count() {
    unset seen
    local -A seen
    local -a queue=("${nodes[$((RANDOM%nodes_size))]}")
    while [[ ${#queue[@]} -gt 0 ]]; do
        current="${queue[0]}"
        queue=("${queue[@]:1}")
        seen["$current"]="true"
        for n in ${wiring["$current"]}; do
            if [[ "${seen["$n"]}" == "true" ]]; then
                continue
            fi
            queue+=("$n")
        done
    done
    local seen_keys=("${!seen[@]}")
    local seen_size="${#seen_keys[@]}"
    r=$(( (nodes_size-seen_size) * seen_size ))
    echo "$r"
}


solve() {
    for ((i=0; i<50; i++)); do
        unset seen
        declare -A seen
        src="${nodes[$((RANDOM%nodes_size))]}"
        dst="${nodes[$((RANDOM%nodes_size))]}"
        local -a queue=("$src:$src")

        while [[ ${#queue[@]} -gt 0 ]]; do
            current="${queue[0]}"
            queue=("${queue[@]:1}")
            IFS=":" read -r n path <<< "$current"
            path_s=$(echo "$path" | tr ',' ' ')
            local -a path_list=($path_s)

            if [[ "$n" == "$dst" ]]; then
                for ((j=0; j<${#path_list[@]}-1; j++)); do
                    a="${path_list["$j"]}"
                    b="${path_list[$((j+1))]}"
                    if [[ "$a" > "$b" ]]; then
                        minimum="$b"
                        maximum="$a"
                    else
                        minimum="$a"
                        maximum="$b"
                    fi
                    if [[ -n "${edge_count["$minimum,$maximum"]}" ]]; then
                        ((edge_count["$minimum,$maximum"]+=1))
                    else
                        edge_count["$minimum,$maximum"]="1"
                    fi
                done
                break
            fi
            seen["$n"]="true"

            for m in ${wiring["$n"]}; do
                if [[ "${seen["$m"]}" == "true" ]]; then
                    continue
                fi
                if [[ -n "$path" ]]; then
                    queue+=("$m:$path,$m")
                else
                    queue+=("$m:$m")
                fi
            done
        done
    done
}

get_top_3() {
    output=""
    for key in ${!edge_count[@]}; do
        output="$output $key ${edge_count[$key]}\n"
    done
    sorted_output=$(echo -e "$output" | sort -k2,2nr)
    echo "$sorted_output" | head -n 3
}

# https://topaz.github.io/paste/#XQAAAQAwCgAAAAAAAAA0m0pnuFI8c/TuXLvn2ExEwX2i7cC7Xc7wv1MViZ31/dFtNL/lNqHPoxwRtpnst7nlBIAJTwr9j71T5iClSHOU1S59eurqzHxH/9JMRCk0IsfPenbxMEcelIYeK1ioHwycHIi0QkTWAM62bJ91lt2UjktXuvCMQBBnOeFnheFEcnjZ/uMWMYurw2pxd/GVegcGl5iKHznkl6ekmQYijdxVf85Ygdc63EyRlyGKZJ7kIYSKKvRzuKbfzHgvrQpxaXSWMt4aNTX04um/BZ7MynfGggJ78pAHSQsFOCSfSycjNf+f3GoQi3TOQA3SOxWnTZoIwrKveB8hURVS3ndpF9S6iSJXwVZXSOws32Ml2pFPTwoSouyNg3PCI9o9YvcxTThvGXJr6R1bJbBwJzkUkPrA7ucAR4OeEUYo/YHrm0OVIFpEdz/dKIqUNeumxah7iLHZXaEzK4cPi+DdZnaTpwyEdiibe/Sr3UulhZRpkXUDIu01Mmlb1xZUTQBILfm4s7FC4DgLR7wmjxjO/av5Xssh+aBZfrSe1DtrLJ8cUVRYNnfDtbgXNXERN5iR/IGvkWNBC7SQUr5IuOC6gVJ7zd9vyphN5+2zCYXpJcpxCQGLLXYVhXMnr4C2CfSFbkwfOXMR+Lm2FYjC3lWMsvcU4XcCoZFf2o7659h9FD9N16rYgNomtWGqbJ66Huu+i9huOZP+uJ0fFc8Wy/zvkfrg/mTIbavgJBNJV/fbOvnp1f0jeGbPIDIOX2XodOI3KpbzNoYrXDyBVvHeezdd5kj5DJ6jHTY9jlrGG79g33TDwfBQywfFCUVcHNrs/SOL8JwCWWcX37mvfQwA2cdjs+6T5XTbc9NRna6vzNOz5dZOK2fOqDV6PglD1Avf4DDVki7zY60qpehjo/K1Fg+EXGL4hnWngnbhnqy/cnbwwZ7ot3nIeEUAYi0jP5WkGGSOWT04RHYLIM29Yz9QGF7IKNe+u70UJKRUHmPE/Lat32j2+UkKS+9yTEKxDXi6k6DPg4Aua4vZ5/jhc8+t2nRktlkPvfPN9IahHdgQUyLyRGM0wVYvzeni067HHR3Iv54+wV8Ewk9QxJeSDWXOzRRXqljBRyknU8aY0WFOfgjyeSUKN420tOatSgBWzEk2woas9d5or00GIp759SbtbyUUlyefpS8cCAKW1B86bk6yXoUdeMGvtR113BiCx0uF9o8RLxtqplcbxLFX8qf2XrCgMRLRG8wugEqlSm7QSkLLCsDhtZbrF9kN6t6EsDTXqGH8sdW1EHIj4S29PgzKhLNtWo0EqI+6/El02kRfgankgP/1EVVh
part1() {
    parse_input
    solve
    local -a edges
    while read line; do
        read a b <<< "$line"
        edges+=("$a")
    done <<< $(get_top_3)
    remove "${edges[@]}"
    count
}

part2() {
    echo "2"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"