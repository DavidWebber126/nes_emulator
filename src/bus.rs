use crate::{
    cartridges::Rom,
    joypad::Joypad,
    ppu::{NesPPU, TickStatus},
    {render, render::Frame},
};

const RAM: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
//const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);
    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos);
        let hi = self.mem_read(pos + 1);
        u16::from_le_bytes([lo, hi])
    }
    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let lo_hi = data.to_le_bytes();
        self.mem_write(pos, lo_hi[0]);
        self.mem_write(pos + 1, lo_hi[1]);
    }
}

pub struct Bus<'call> {
    pub cpu_vram: [u8; 2048],
    pub rom: Vec<u8>,
    pub ppu: NesPPU,
    frame: Frame,
    joypad1: Joypad,

    cycles: usize,
    gameloop_callback: Box<dyn FnMut(&mut Frame, &mut Joypad) + 'call>,
}

impl<'a> Bus<'a> {
    pub fn new<'call, F>(rom: Rom, gameloop_callback: F) -> Bus<'call>
    where
        F: FnMut(&mut Frame, &mut Joypad) + 'call,
    {
        let ppu = NesPPU::new(rom.chr_rom, rom.screen_mirroring);

        Bus {
            cpu_vram: [0; 2048],
            rom: rom.prg_rom,
            ppu,
            frame: Frame::new(),
            joypad1: Joypad::new(),
            cycles: 0,
            gameloop_callback: Box::from(gameloop_callback),
        }
    }

    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.nmi_interrupt.take()
    }

    pub fn tick(&mut self, cycles: u8) {
        self.cycles += cycles as usize;

        let render_status = self.ppu.tick(cycles * 3);
        match render_status {
            TickStatus::NewFrame => {
                //render::render_sprites(&self.ppu, &mut self.frame);
                (self.gameloop_callback)(&mut self.frame, &mut self.joypad1);
            }
            TickStatus::NewScanline => {
                render::render_visible_scanline(&mut self.ppu, &mut self.frame);
            }
            TickStatus::SameScanline => {}
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr %= 0x4000;
        }
        self.rom[addr as usize]
    }

    // These are functions that read address locations without side effects
    pub fn trace_mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                //panic!("Attempt to read from write-only PPU address {:x}", addr);
                0
            }
            0x2002 => self.ppu.status.0,
            0x2004 => self.ppu.read_from_oam_data(),
            0x2007 => self.ppu.trace_read_data(),
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }
            0x4016 => self.joypad1.trace_read(),
            0x4017 => 0,
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem access at {:04X}", addr);
                0
            }
        }
    }
}

impl Mem for Bus<'_> {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                //panic!("Attempt to read from write-only PPU address {:x}", addr);
                0
            }
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_from_oam_data(),
            0x2007 => self.ppu.read_data(),
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }
            0x4016 => self.joypad1.read(),
            0x4017 => 0,
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => {
                println!("Ignoring mem access at {:04X}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize] = data;
            }
            0x2000 => {
                //println!("Write to ctrl: {:08b}, PPU Status: {:08b}, PPU Scanline: {}", data, self.ppu.read_status(), self.ppu.scanline);
                self.ppu.write_to_ctrl(data)
            },
            0x2001 => self.ppu.write_to_mask(data),
            0x2002 => panic!("attempt to write to PPU status register"),
            0x2003 => self.ppu.write_to_oam_addr(data),
            0x2004 => self.ppu.write_to_oam_data(data),
            0x2005 => self.ppu.write_to_scroll(data),
            0x2006 => {
                self.ppu.write_to_ppu_addr(data);
            }
            0x2007 => {
                self.ppu.write_to_data(data);
            }
            0x4014 => {
                let page_start_addr = (data as u16) << 8;
                let mut page_data: [u8; 256] = [0; 256];
                for i in 0..256 {
                    page_data[i] = self.mem_read(page_start_addr + i as u16)
                }
                self.ppu.write_oam_dma(&page_data);
            }
            0x4000..=0x4013 | 0x4015 => {
                //ignore APU
            }
            0x4016 => self.joypad1.write(data),
            0x4017 => {}
            0x8000..=0xFFFF => {
                panic!("Attempt to write to Cartridge ROM space")
            }
            _ => {
                println!("Ignoring mem write-access at {:04X}", addr)
            }
        }
    }
}
