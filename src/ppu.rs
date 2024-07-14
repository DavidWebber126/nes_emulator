use crate::cartridges::Mirroring;
use crate::render;

pub struct NesPPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub mirroring: Mirroring,
    scanline: u16,
    cycles: usize,

    internal_data_buf: u8,
    pub nmi_interrupt: Option<u8>,

    pub addr: AddrRegister,
    pub ctrl: ControlRegister,
    pub mask: MaskRegister,
    pub status: StatusRegister,
    pub scroll: ScrollRegister,
    pub oam_addr: u8,
    pub oam_data: [u8; 256],
    pub oam_dma: u8,
}

impl NesPPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        NesPPU {
            chr_rom,
            mirroring,
            vram: [0; 2048],
            palette_table: [0; 32],
            scanline: 0,
            cycles: 0,

            internal_data_buf: 0,
            nmi_interrupt: None,

            addr: AddrRegister::new(),
            ctrl: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),
            scroll: ScrollRegister::new(),
            oam_addr: 0,
            oam_data: [0; 64 * 4],
            oam_dma: 0,
        }
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    pub fn write_to_ctrl(&mut self, value: u8) {
        let before_nmi_status = self.ctrl.0 & 0b1000_0000;
        self.ctrl.update(value);
        let in_vblank = self.status.0 & 0b1000_0000;
        let nmi_status = self.ctrl.0 & 0b1000_0000;
        if (!before_nmi_status & nmi_status & in_vblank) != 0 {
            self.nmi_interrupt = Some(1);
        }
    }

    pub fn write_to_mask(&mut self, value: u8) {
        self.mask.update(value);
    }

    pub fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3eff => panic!(
                "addr space 0x3000..0x3eff is not expected to be used, requested = {} ",
                addr
            ),
            0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize],
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    pub fn write_to_data(&mut self, data: u8) {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                panic!("attempt to write to chr rom space {}", addr)
            }
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = data;
            }
            0x3000..=0x3eff => panic!(
                "addr space 0x3000..0x3eff is not expected to be used, requested = {} ",
                addr
            ),
            //Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3f00) as usize] = data;
            }
            0x3f00..=0x3fff => {
                self.palette_table[(addr - 0x3f00) as usize] = data;
            }
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    pub fn read_status(&mut self) -> u8 {
        let status = self.status.0;
        self.status.0 &= 0b0111_1111;
        self.addr.reset_latch();
        self.scroll.reset_latch();
        status
    }

    pub fn write_to_scroll(&mut self, data: u8) {
        self.scroll.write(data);
    }

    pub fn write_to_oam_addr(&mut self, data: u8) {
        self.oam_addr = data;
    }

    pub fn write_to_oam_data(&mut self, data: u8) {
        self.oam_data[self.oam_addr as usize] = data;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    pub fn read_from_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    // Index $XX will upload 256 bytes from CPU page $XX00-$XXFF to internal PPU OAM.
    // Starting at oam_addr within OAM memory (wrapping around if needed)
    pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
        for x in data.iter() {
            self.oam_data[self.oam_addr as usize] = *x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    // Horizontal:
    //   [ A ] [ a ]
    //   [ B ] [ b ]

    // Vertical:
    //   [ A ] [ B ]
    //   [ a ] [ b ]
    pub fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0b10_1111_1111_1111; // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
        let vram_index = mirrored_vram - 0x2000; // to vram vector
        let name_table = vram_index / 0x400; // to the name table index
        match (&self.mirroring, name_table) {
            (Mirroring::VERTICAL, 2) | (Mirroring::VERTICAL, 3) => vram_index - 0x800,
            (Mirroring::HORIZONTAL, 2) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 1) => vram_index - 0x400,
            (Mirroring::HORIZONTAL, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }

    pub fn tick(&mut self, cycles: u8) -> bool {
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            if self.is_sprite_0_hit(self.cycles) {
                self.status.0 |= 0b0100_0000;
            }

            self.cycles -= 341;
            self.scanline += 1;

            match self.scanline {
                1..=240 | 242..=261 => {}
                241 => {
                    self.status.0 |= 0b1000_0000;
                    self.status.0 &= 0b1011_1111;
                    if self.ctrl.0 & 0b1000_0000 != 0 {
                        self.nmi_interrupt = Some(1);
                    } 
                }
                262 => {
                    self.scanline = 0;
                    self.status.0 &= 0b0111_1111;
                    self.status.0 &= 0b1011_1111;
                    self.nmi_interrupt = None;
                    return true;
                }
                _ => panic!("Scanline {} is supposed to be unreachable", self.scanline)
            }
        }
        false
    }

    fn is_sprite_0_hit(&self, cycle: usize) -> bool {
        let y = self.oam_data[0] as usize;
        let x = self.oam_data[3] as usize;
        (y == self.scanline as usize) && x <= cycle && (self.mask.0 & 0b00001000 != 0)
    }
}

pub struct AddrRegister {
    value: (u8, u8),
    hi_ptr: bool,
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            value: (0, 0), // high byte first, lo byte second
            hi_ptr: true,
        }
    }

    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xff) as u8;
    }

    pub fn update(&mut self, data: u8) {
        if self.hi_ptr {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }

        if self.get() > 0x3fff {
            //mirror down addr above 0x3fff
            self.set(self.get() & 0b11111111111111);
        }
        self.hi_ptr = !self.hi_ptr;
    }

    pub fn increment(&mut self, inc: u8) {
        let lo = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);
        if lo > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }
        if self.get() > 0x3fff {
            self.set(self.get() & 0b11111111111111); //mirror down addr above 0x3fff
        }
    }

    pub fn reset_latch(&mut self) {
        self.hi_ptr = true;
    }

    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | (self.value.1 as u16)
    }
}

impl Default for AddrRegister {
    fn default() -> Self {
        Self::new()
    }
}

// 7  bit  0
// ---- ----
// VPHB SINN
// |||| ||||
// |||| ||++- Base nametable address
// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
// |||| |     (0: add 1, going across; 1: add 32, going down)
// |||| +---- Sprite pattern table address for 8x8 sprites
// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
// |||+------ Background pattern table address (0: $0000; 1: $1000)
// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
// |+-------- PPU master/slave select
// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
// +--------- Generate an NMI at the start of the
//            vertical blanking interval (0: off; 1: on)
pub struct ControlRegister(u8);

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister(0)
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if self.0 & 0b0000_0100 == 0 {
            1
        } else {
            32
        }
    }

    pub fn update(&mut self, data: u8) {
        self.0 = data;
    }

    pub fn bank(&self) -> u16 {
        if self.0 & 0b0001_0000 != 0 {
            0x1000
        } else {
            0
        }
    }

    pub fn sprt_pattern_addr(&self) -> u16 {
        if self.0 & 0b1000 != 0 {
            0x1000
        } else {
            0
        }
    }

    pub fn nametable_addr(&self) -> u16 {
        match self.0 & 0b11 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => panic!("Impossible nametable"),
        }
    }
}

impl Default for ControlRegister {
    fn default() -> Self {
        Self::new()
    }
}

// 7  bit  0
// ---- ----
// BGRs bMmG
// |||| ||||
// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
// |||| +---- 1: Show background
// |||+------ 1: Show sprites
// ||+------- Emphasize red (green on PAL/Dendy)
// |+-------- Emphasize green (red on PAL/Dendy)
// +--------- Emphasize blue
pub struct MaskRegister(u8);

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister(0)
    }

    pub fn update(&mut self, data: u8) {
        self.0 = data;
    }
}

impl Default for MaskRegister {
    fn default() -> Self {
        Self::new()
    }
}

//  7  bit  0
// ---- ----
// VSO. ....
// |||| ||||
// |||+-++++- PPU open bus. Returns stale PPU bus contents.
// ||+------- Sprite overflow. The intent was for this flag to be set
// ||         whenever more than eight sprites appear on a scanline, but a
// ||         hardware bug causes the actual behavior to be more complicated
// ||         and generate false positives as well as false negatives; see
// ||         PPU sprite evaluation. This flag is set during sprite
// ||         evaluation and cleared at dot 1 (the second dot) of the
// ||         pre-render line.
// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
// |          a nonzero background pixel; cleared at dot 1 of the pre-render
// |          line.  Used for raster timing.
// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
//            Set at dot 1 of line 241 (the line *after* the post-render
//            line); cleared after reading $2002 and at dot 1 of the
//            pre-render line.

pub struct StatusRegister(u8);

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister(0)
    }

    pub fn update(&mut self, data: u8) {
        self.0 = data
    }
}

impl Default for StatusRegister {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ScrollRegister {
    value: (u8, u8),
    x_ptr: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            value: (0, 0), // first is x, second is y
            x_ptr: true,
        }
    }

    pub fn reset_latch(&mut self) {
        self.x_ptr = true;
    }

    pub fn write(&mut self, data: u8) {
        if self.x_ptr {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }
        self.x_ptr = !self.x_ptr;
    }

    pub fn scroll_x(&self) -> u8 {
        self.value.0
    }

    pub fn scroll_y(&self) -> u8 {
        self.value.1
    }
}

impl Default for ScrollRegister {
    fn default() -> Self {
        Self::new()
    }
}
