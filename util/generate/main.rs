// SOURCE: https://github.com/bugzmanov/nes_ebook/blob/master/code/ch6.3/src/snake.rs

use std::fs::File;
use std::io::prelude::*;


fn create_fake_rom(file_name: String) {
    let mut buffer = File::create(file_name).unwrap();
    let header = vec![0x4E, 0x45, 0x53, 0x1A, 0x02, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ];

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
    create_fake_rom(String::from("NAME OF FILE.nes"));
}
