const WIDTH: usize = 25;
const HEIGHT: usize = 6;
const LAYER_SIZE: usize = WIDTH * HEIGHT;

pub struct Layer {
    // data: [[char; WIDTH]; HEIGHT],
    data: Vec<char>,
}

impl Layer {

    pub fn new(input: Vec<char>) -> Layer {
        Layer {
            data: input,
        }
    }

    pub fn count_char(&self, c: char) -> usize {
        self.data.iter()
            .filter(|&x| *x == c)
            .count()
    }

    pub fn char_at(&self, row: usize, col: usize) -> char {
        self.data[row *  WIDTH + col]
    }
}

pub struct Image {
    layers: Vec<Layer>,
}

impl Image {
    pub fn new(input: &str) -> Image {
        Image {
            layers: input.chars()
            .collect::<Vec<char>>()
            .chunks(LAYER_SIZE)
            .map(|chunk| Layer::new(chunk.to_vec()))
            .collect()
        }
    }

    pub fn checksum(&self) -> usize {
        let layer = self.layers.iter()
            .min_by_key(|x| x.count_char('0'))
            .unwrap();
        layer.count_char('1') * layer.count_char('2')
    }

    pub fn render(&self) -> String {
        let mut output: String = String::from("\n");
        for row in 0..HEIGHT {
            for col in 0..WIDTH {
                output.push(self.render_pixel(0, row, col));
            }
            output.push('\n');
        }
        output
    }

    fn render_pixel(&self, layer: usize, row: usize, col: usize) -> char {
        match self.layers[layer].char_at(row, col) {
            '0' => '█',
            '1' => ' ',
            '2' => self.render_pixel(layer+1, row, col),
            _ => '_',
        }
    }
}

#[aoc_generator(day8)]
pub fn prepare_input(input: &str) -> Image {
    Image::new(input)
}

#[aoc(day8, part1)]
pub fn solve_part1(img: &Image) -> usize {
    img.checksum()
}

#[aoc(day8, part2)]
pub fn solve_part2(img: &Image) -> String {
    img.render()
}
