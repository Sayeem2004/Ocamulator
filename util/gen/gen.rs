// SOURCE: https://github.com/bugzmanov/nes_ebook/blob/master/code/ch6.3/src/snake.rs
use std::fs::File;
use std::io::prelude::*;

fn create_fake_rom(file_name: String) {
    let mut buffer = File::create(file_name).unwrap();
    let header = vec![0xa2, 0x00, 0x86, 0x20, 0xa9, 0x05, 0x85, 0x21, 0xa5, 0xfe, 0x85, 0x22, 0xc6, 0x21, 0xa5, 0x21, 0xf0, 0x03, 0x4c, 0x8b, 0x06, 0xa9, 0x05, 0x85, 0x21, 0xa2, 0x21, 0xe6, 0x22, 0xa5, 0x22, 0x29, 0x07, 0xa8, 0xbd, 0x00, 0x10, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x00, 0x10, 0xbd, 0x40, 0x10, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x40, 0x10, 0xbd, 0x80, 0x10, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x80, 0x10, 0xbd, 0xc0, 0x10, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0xc0, 0x10, 0xbd, 0x00, 0x11, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x00, 0x11, 0xbd, 0x40, 0x11, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x40, 0x11, 0xbd, 0x80, 0x11, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x80, 0x11, 0xbd, 0xc0, 0x11, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0xc0, 0x11, 0xbd, 0x00, 0x12, 0xf0, 0x06, 0xb9, 0xfb, 0x06, 0x9d, 0x00, 0x12, 0xe8, 0x8a, 0x29, 0x3f, 0xd0, 0x97, 0xe6, 0x20, 0xa5, 0x20, 0x29, 0x3f, 0xa8, 0x29, 0x1f, 0xaa, 0xbd, 0xdb, 0x06, 0xaa, 0xa9, 0x00, 0x9d, 0xe0, 0x02, 0x9d, 0xe0, 0x03, 0xb9, 0x00, 0x10, 0x9d, 0x00, 0x03, 0xb9, 0x80, 0x10, 0x9d, 0x20, 0x03, 0xb9, 0x00, 0x11, 0x9d, 0x40, 0x03, 0xb9, 0x80, 0x11, 0x9d, 0x60, 0x03, 0xb9, 0x00, 0x12, 0x9d, 0x80, 0x03, 0xb9, 0x80, 0x12, 0x9d, 0xa0, 0x03, 0xb9, 0x00, 0x13, 0x9d, 0xc0, 0x03, 0xb9, 0x80, 0x13, 0x9d, 0xc0, 0x03, 0xe8, 0xc8, 0x8a, 0x29, 0x1f, 0xd0, 0xc1, 0x4c, 0x0c, 0x06, 0x00, 0x00, 0x00, 0x00, 0x20, 0x20, 0x20, 0x40, 0x40, 0x60, 0x80, 0xa0, 0xa0, 0xc0, 0xc0, 0xc0, 0xe0, 0xe0, 0xe0, 0xe0, 0xc0, 0xc0, 0xc0, 0xa0, 0xa0, 0x80, 0x60, 0x40, 0x40, 0x20, 0x20, 0x20, 0x07, 0x08, 0x09, 0x02, 0x04, 0x06, 0x0e, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ];

    let pre = [0; 0x600];
    let code = vec![];

    let mut pos = 0;
    while pos < header.len() {
        let bytes_written = buffer.write(&header[pos..]).unwrap();
        pos += bytes_written;
    }

    pos = 0;
    while pos < header.len() {
        let bytes_written = buffer.write(&pre[pos..]).unwrap();
        pos += bytes_written;
    }

    pos = 0;
    while pos < header.len() {
        let bytes_written = buffer.write(&code[pos..]).unwrap();
        pos += bytes_written;
    }

    pos = 0x600 + code.len();
    while pos < (0xFFFC - 0x8000) {
        buffer.write(&[0]).unwrap();
        pos += 1;
    }

    buffer.write(&[0x0, 0x86,0,0]).unwrap();
    buffer.flush().unwrap();
}

fn main() {
    create_fake_rom(String::from("6502wave.nes"));
}