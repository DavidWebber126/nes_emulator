use crate::{
    cartridges::Rom,
    joypad::Joypad,
    ppu::{NesPPU, TickStatus},
    {render, render::Frame},
    apu::Apu,
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
    pub apu: Apu,
    frame: Frame,
    joypad1: Joypad,

    gameloop_callback: Box<dyn FnMut(&mut Frame, &mut Joypad, &mut Vec<f32>) + 'call>,
}

impl<'a> Bus<'a> {
    pub fn new<'call, F>(rom: Rom, gameloop_callback: F) -> Bus<'call>
    where
        F: FnMut(&mut Frame, &mut Joypad, &mut Vec<f32>) + 'call,
    {
        let ppu = NesPPU::new(rom.chr_rom, rom.screen_mirroring);

        Bus {
            cpu_vram: [0; 2048],
            rom: rom.prg_rom.clone(),
            ppu,
            apu: Apu::new(rom.prg_rom),
            frame: Frame::new(),
            joypad1: Joypad::new(),
            gameloop_callback: Box::from(gameloop_callback),
        }
    }

    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.nmi_interrupt.take()
    }

    pub fn poll_irq_status(&mut self) -> Option<u8> {
        if self.apu.status & 0b1100_0000 != 0 {
            Some(1)
        } else {
            None
        }
    }

    pub fn tick(&mut self, cycles: u8) {
        self.apu.tick(cycles);
        let render_status = self.ppu.tick(cycles * 3);
        match render_status {
            TickStatus::SameScanline => {}
            TickStatus::NewFrame     => {
                (self.gameloop_callback)(&mut self.frame, &mut self.joypad1, &mut self.apu.buffer);
            }
            TickStatus::NewScanline  => {
                render::render_visible_scanline(&mut self.ppu, &mut self.frame);
            }
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
                0
            }
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_from_oam_data(),
            0x2007 => self.ppu.read_data(),
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }
            0x4015 => self.apu.read_status(),
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
            // APU Calls
            0x4000 => self.apu.pulse1.write_to_envelope(data),
            0x4001 => self.apu.pulse1.write_to_sweep(data),
            0x4002 => self.apu.pulse1.write_to_timer_low(data),
            0x4003 => self.apu.pulse1.write_to_timer_high(data),
            0x4004 => self.apu.pulse2.write_to_envelope(data),
            0x4005 => self.apu.pulse2.write_to_sweep(data),
            0x4006 => self.apu.pulse2.write_to_timer_low(data),
            0x4007 => self.apu.pulse2.write_to_timer_high(data),
            0x4008 => self.apu.triangle.write_to_linear_counter(data),
            0x400A => self.apu.triangle.write_to_timer_low(data),
            0x400B => self.apu.triangle.write_to_timer_high(data),
            0x400C => self.apu.noise.write_to_envelope(data),
            0x400E => self.apu.noise.mode_and_period(data),
            0x400F => self.apu.noise.write_to_length_counter(data),
            0x4010 => {
                self.apu.dmc.flags_and_rate(data);
                if data & 0b1000_0000 == 0 {
                    self.apu.status &= 0b0111_1111;
                }
            }
            0x4011 => self.apu.dmc.direct_load(data),
            0x4012 => self.apu.dmc.sample_address(data),
            0x4013 => self.apu.dmc.sample_length(data),
            0x4015 => self.apu.write_status(data),
            0x4017 => self.apu.write_fc(data),
            // End of APU Calls
            0x4016 => self.joypad1.write(data),
            0x8000..=0xFFFF => {
                panic!("Attempt to write to Cartridge ROM space at {:04X} with {:02X}", addr, data)
            }
            _ => {
                println!("Ignoring mem write-access at {:04X} with {:02X}", addr, data)
            }
        }
    }
}
