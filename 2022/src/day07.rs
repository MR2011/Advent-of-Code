use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;

#[derive(Default, Clone, Debug)]
struct Directory {
    files: Vec<usize>,
    dirs: HashSet<PathBuf>,
}

type Filesystem = HashMap<PathBuf, Directory>;

fn get_size(path: PathBuf, fs: Filesystem) -> usize {
    let dir = fs.get(&path).unwrap();
    let fsizes = dir.files.iter().sum::<usize>();
    let dsizes = dir
        .dirs
        .iter()
        .map(|d| get_size(d.clone(), fs.clone()))
        .sum::<usize>();
    fsizes + dsizes
}

fn build_filesystem(input: &str) -> Filesystem {
    let mut pwd = PathBuf::new();
    let mut fs: Filesystem = HashMap::new();
    input.lines().for_each(|l| match &l[0..4] {
        "$ cd" => {
            let name = l[5..].to_string();
            if name.starts_with("..") {
                pwd.pop();
            } else {
                pwd.push(name);
                fs.insert(pwd.clone(), Directory::default());
            }
        }
        "$ ls" => {}
        "dir " => {
            let name = l[4..].to_string();
            let mut subdir = pwd.clone();
            subdir.push(name);
            fs.get_mut(&pwd).unwrap().dirs.insert(subdir.clone());
        }
        _ => {
            let (first, _) = l.split_once(" ").unwrap();
            let size = first.parse::<usize>().unwrap();

            fs.get_mut(&pwd).unwrap().files.push(size);
        }
    });
    fs
}

#[aoc(day7, part1)]
pub fn solve_part1(input: &str) -> usize {
    let fs = build_filesystem(input);
    fs.keys()
        .map(|k| get_size(k.clone(), fs.clone()))
        .filter(|&x| x < 100000)
        .sum()
}

#[aoc(day7, part2)]
pub fn solve_part2(input: &str) -> usize {
    let fs = build_filesystem(input);
    let total_space = 70000000;
    let needed_space = 30000000;
    let used_space = get_size(PathBuf::from("/"), fs.clone());
    let free_space = total_space - used_space;
    let to_delete = needed_space - free_space;
    let size = fs
        .keys()
        .map(|k| get_size(k.clone(), fs.clone()))
        .filter(|s| s >= &to_delete)
        .min_by(|a, b| a.cmp(&b))
        .unwrap();
    size
}
