use crate::cartridges::Mirroring;
use crate::ppu::NesPPU;

#[rustfmt::skip]
pub static SYSTEM_PALLETE: [(u8,u8,u8); 64] = [
   (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96), (0xA1, 0x00, 0x5E),
   (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00), (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00),
   (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E), (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05),
   (0x05, 0x05, 0x05), (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA),
   (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00), (0xC4, 0x62, 0x00),
   (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55), (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21),
   (0x09, 0x09, 0x09), (0x09, 0x09, 0x09), (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF),
   (0xD4, 0x80, 0xFF), (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12),
   (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4), (0x05, 0xFB, 0xFF),
   (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D), (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF),
   (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB), (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0),
   (0xFF, 0xEF, 0xA6), (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA),
   (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11)
];

pub struct Frame {
    pub data: Vec<u8>,
}

impl Frame {
    const WIDTH: usize = 256;
    const HIGHT: usize = 240;

    pub fn new() -> Self {
        Frame {
            data: vec![0; (Frame::WIDTH) * (Frame::HIGHT) * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        let base = y * 3 * Frame::WIDTH + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = rgb.0;
            self.data[base + 1] = rgb.1;
            self.data[base + 2] = rgb.2;
        }
    }

    pub fn get_pixel(&mut self, x: usize, y: usize) -> (u8, u8, u8) {
        let base = y * 3 * Frame::WIDTH + x * 3;
        (self.data[base], self.data[base + 1], self.data[base + 2])
    }
}

impl Default for Frame {
    fn default() -> Self {
        Self::new()
    }
}

struct Rect {
    x1: usize,
    y1: usize,
    x2: usize,
    y2: usize,
}

impl Rect {
    fn new(x1: usize, y1: usize, x2: usize, y2: usize) -> Self {
        Rect { x1, y1, x2, y2 }
    }
}

impl Default for Rect {
    fn default() -> Self {
        Self::new(0, 0, 0, 0)
    }
}

fn bg_pallette(
    ppu: &NesPPU,
    attribute_table: &[u8],
    tile_column: usize,
    tile_row: usize,
) -> [u8; 4] {
    let attr_table_idx = tile_row / 4 * 8 + tile_column / 4;
    let attr_byte = attribute_table[attr_table_idx]; // note: still using hardcoded first nametable

    let pallet_idx = match (tile_column % 4 / 2, tile_row % 4 / 2) {
        (0, 0) => attr_byte & 0b11,
        (1, 0) => (attr_byte >> 2) & 0b11,
        (0, 1) => (attr_byte >> 4) & 0b11,
        (1, 1) => (attr_byte >> 6) & 0b11,
        (_, _) => panic!("should not happen"),
    };

    let pallete_start: usize = 1 + (pallet_idx as usize) * 4;
    [
        ppu.palette_table[0],
        ppu.palette_table[pallete_start],
        ppu.palette_table[pallete_start + 1],
        ppu.palette_table[pallete_start + 2],
    ]
}

fn sprite_palette(ppu: &NesPPU, pallete_idx: u8) -> [u8; 4] {
    let start = 0x11 + (pallete_idx * 4) as usize;
    [
        0,
        ppu.palette_table[start],
        ppu.palette_table[start + 1],
        ppu.palette_table[start + 2],
    ]
}

pub fn render_visible_scanline(ppu: &mut NesPPU, frame: &mut Frame) {
    if ppu.mask.render_background() {
        render_bg_visible_scanline(ppu, frame);
    }
    if ppu.mask.render_sprites() {
        render_sprite_visible_scanline(ppu, frame);
    }
}

fn render_bg_visible_scanline(ppu: &NesPPU, frame: &mut Frame) {
    let scroll_x = (ppu.scroll.scroll_x()) as usize;
    let scroll_y = (ppu.scroll.scroll_y()) as usize;

    let (main_nametable, second_nametable) = match (&ppu.mirroring, ppu.nametable_addr()) {
        (Mirroring::VERTICAL, 0x2000)
        | (Mirroring::VERTICAL, 0x2800)
        | (Mirroring::HORIZONTAL, 0x2000)
        | (Mirroring::HORIZONTAL, 0x2400) => {
            (&ppu.vram[0..0x400], &ppu.vram[0x400..0x800])
        },
        (Mirroring::VERTICAL, 0x2400)
        | (Mirroring::VERTICAL, 0x2C00)
        | (Mirroring::HORIZONTAL, 0x2800)
        | (Mirroring::HORIZONTAL, 0x2C00) => {
            (&ppu.vram[0x400..0x800], &ppu.vram[0..0x400])
        },
        (_, _) => {
            panic!("Not supported mirroring type {:?}", ppu.mirroring);
        }
    };
    render_background_scanline(
        ppu,
        frame,
        main_nametable,
        Rect::new(scroll_x, scroll_y, 256, 240),
        -(scroll_x as isize),
        -(scroll_y as isize),
    );
    if scroll_x > 0 {
        render_background_scanline(
            ppu,
            frame,
            second_nametable,
            Rect::new(0, 0, scroll_x, 240),
            (256 - scroll_x) as isize,
            0,
        );
    } else if scroll_y > 0 {
        render_background_scanline(
            ppu,
            frame,
            second_nametable,
            Rect::new(0, 0, 256, scroll_y),
            0,
            (240 - scroll_y) as isize,
        );
    }
}

fn render_background_scanline(
    ppu: &NesPPU,
    frame: &mut Frame,
    name_table: &[u8],
    view_port: Rect,
    shift_x: isize,
    shift_y: isize,
) {
    let pixel_y = ppu.scanline - 1; // subtract one since actual scanline starts at -1 but usize must be >0
    let tile_row = pixel_y / 8;
    let offset_y = pixel_y % 8;
    let bank = ppu.ctrl.bank();

    let attribute_table = &name_table[0x3c0..0x400];

    for tile_column in 0..32 {
        let tile_idx = name_table[tile_row * 32 + tile_column] as u16;
        let tile =
            &ppu.chr_rom[(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];
        let palette = bg_pallette(ppu, attribute_table, tile_column, tile_row);
        let mut lower = tile[offset_y];
        let mut upper = tile[offset_y + 8];
        for x in (0..8).rev() {
            let value = (1 & upper) << 1 | (1 & lower);
            upper >>= 1;
            lower >>= 1;
            let rgb = match value {
                0 => SYSTEM_PALLETE[ppu.palette_table[0] as usize],
                1 => SYSTEM_PALLETE[palette[1] as usize],
                2 => SYSTEM_PALLETE[palette[2] as usize],
                3 => SYSTEM_PALLETE[palette[3] as usize],
                _ => panic!("can't be"),
            };
            let pixel_x = tile_column * 8 + x;
            if pixel_x >= view_port.x1
                && pixel_x < view_port.x2
                && pixel_y >= view_port.y1
                && pixel_y < view_port.y2
            {
                frame.set_pixel(
                    (shift_x + pixel_x as isize) as usize,
                    (shift_y + pixel_y as isize) as usize,
                    rgb,
                );
            }
        }
    }
}

fn render_sprite_visible_scanline(ppu: &mut NesPPU, frame: &mut Frame) {
    for i in (0..ppu.oam_data.len()).step_by(4).rev() {
        let pixel_y = ppu.scanline - 1;
        let tile_y = ppu.oam_data[i] as usize;
        if tile_y <= pixel_y && pixel_y < (tile_y + 8) {
            let tile_x = ppu.oam_data[i + 3] as usize;
            let tile_idx = ppu.oam_data[i + 1] as u16;

            let flip_vertical = ppu.oam_data[i + 2] & 0b1000_0000 != 0;
            let flip_horizontal = ppu.oam_data[i + 2] & 0b0100_0000 != 0;
            let pallette_idx = ppu.oam_data[i + 2] & 0b11;
            let sprite_palette = sprite_palette(ppu, pallette_idx);
            let bank: u16 = ppu.ctrl.sprt_pattern_addr();
            let tile =
                &ppu.chr_rom[(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];

            for y in 0..8 {
                let mut lower = tile[y];
                let mut upper = tile[y + 8];
                'ololo: for x in (0..8).rev() {
                    let value = (1 & upper) << 1 | (1 & lower);
                    upper >>= 1;
                    lower >>= 1;
                    let rgb = match value {
                        0 => continue 'ololo, // skip coloring the pixel
                        1 => SYSTEM_PALLETE[sprite_palette[1] as usize],
                        2 => SYSTEM_PALLETE[sprite_palette[2] as usize],
                        3 => SYSTEM_PALLETE[sprite_palette[3] as usize],
                        _ => panic!("can't be"),
                    };
                    let (x,y) = match (flip_horizontal, flip_vertical) {
                        (false, false) => (tile_x + x, tile_y + y),
                        (true, false) => (tile_x + 7 - x, tile_y + y),
                        (false, true) => (tile_x + x, tile_y + 7 - y),
                        (true, true) => (tile_x + 7 - x, tile_y + 7 - y),
                    };
                    if i == 0 && (ppu.mask.0 & 0b00001000 != 0) && (y == pixel_y)
                    && (frame.get_pixel(x,y) != SYSTEM_PALLETE[ppu.palette_table[0] as usize]) 
                    {
                        ppu.status.0 |= 0b0100_0000;
                    }
                    if y == pixel_y {
                        frame.set_pixel(x, y, rgb);
                    }
                }
            }
    }

    }
}
