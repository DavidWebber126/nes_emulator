use crate::cpu::{CPU, AddressingMode};
use std::collections::HashMap;
use crate::opcodes;
struct Trace {
    pc: u16,
    opcodes: Vec<u8>,
    opcode_assembly: String,
    register_a: u8,
    register_x: u8,
    register_y: u8,
    p: u8,
    sp: u8,
    ppu_scanline: usize,
    ppu_cycle: usize,
}

impl Trace {
    fn new(cpu: &mut CPU) -> Trace {
        // get ppu scanline and cycle
        let ppu_scanline = cpu.bus.ppu.scanline;
        let ppu_cycle = cpu.bus.ppu.cycles;

        let opcodes: &HashMap<u8, opcodes::Opcode> = &opcodes::CPU_OP_CODES;

        // get all bytes used in opcode
        let opcode = opcodes.get(&cpu.trace_mem_read(cpu.program_counter)).unwrap();
        let mut opcodes: Vec<u8> = Vec::new();
        for i in 0..(opcode.bytes) {
            opcodes.push(cpu.trace_mem_read(cpu.program_counter + i));
        }

        // write opcode in assembly text
        let opcode_text = opcode.name;
        let assembly_text = match opcode.addressing_mode {
            AddressingMode::Immediate => {
                format!("{: >4} #${:02X}", opcode_text, opcodes[1])
            }
            AddressingMode::ZeroPage => {
                let value = cpu.trace_mem_read(opcodes[1] as u16);
                format!("{: >4} ${:02X} = {:02X}", opcode_text, opcodes[1], value)
            }
            AddressingMode::ZeroPage_X => {
                let offset = opcodes[1].wrapping_add(cpu.register_x) as u16;
                let addr = cpu.trace_mem_read(offset);
                format!(
                    "{: >4} ${:02X},X @ {:02X} = {:02X}",
                    opcode_text, opcodes[1], offset, addr
                )
            }
            AddressingMode::ZeroPage_Y => {
                let offset = opcodes[1].wrapping_add(cpu.register_y) as u16;
                let addr = cpu.trace_mem_read(offset);
                format!(
                    "{: >4} ${:02X},Y @ {:02X} = {:02X}",
                    opcode_text, opcodes[1], offset, addr
                )
            }
            AddressingMode::Absolute => {
                // Trace formats JSR and JMP Absolutes differently, i.e without the equal sign
                // which feels dumb/weird
                if (opcodes[0] == 0x20) | (opcodes[0] == 0x4c) {
                    format!("{: >4} ${:02X}{:02X}", opcode_text, opcodes[2], opcodes[1])
                } else {
                    let lo = opcodes[1];
                    let hi = opcodes[2];
                    let addr = u16::from_le_bytes([lo, hi]);
                    let value = cpu.trace_mem_read(addr);
                    format!("{: >4} ${:04X} = {:02X}", opcode_text, addr, value)
                }
            }
            AddressingMode::Absolute_X => {
                let offset = u16::from_le_bytes([opcodes[1], opcodes[2]]) + cpu.register_x as u16;
                let value = cpu.trace_mem_read(offset);
                format!(
                    "{: >4} ${:02X}{:02X},X @ {:04X} = {:02X}",
                    opcode_text, opcodes[2], opcodes[1], offset, value
                )
            }
            AddressingMode::Absolute_Y => {
                let offset = u16::from_le_bytes([opcodes[1], opcodes[2]])
                    .wrapping_add(cpu.register_y as u16);
                let value = cpu.trace_mem_read(offset);
                format!(
                    "{: >4} ${:02X}{:02X},Y @ {:04X} = {:02X}",
                    opcode_text, opcodes[2], opcodes[1], offset, value
                )
            }
            AddressingMode::Indirect => {
                let ptr = u16::from_le_bytes([opcodes[1], opcodes[2]]);
                let ptr_lo = (ptr & 0x00FF) as u8;
                let ptr_hi = ptr & 0xFF00;

                let lo = cpu.trace_mem_read(ptr);
                let hi = cpu.trace_mem_read(ptr_lo.wrapping_add(1) as u16 | ptr_hi);
                let addr = u16::from_le_bytes([lo, hi]);
                format!("{: >4} (${:04X}) = {:04X}", opcode_text, ptr, addr)
            }
            AddressingMode::Indirect_X => {
                let ptr = opcodes[1].wrapping_add(cpu.register_x);
                let lo = cpu.trace_mem_read(ptr as u16);
                let hi = cpu.trace_mem_read(ptr.wrapping_add(1) as u16);
                let addr = u16::from_le_bytes([lo, hi]);
                let value = cpu.trace_mem_read(addr);
                format!(
                    "{: >4} (${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    opcode_text, opcodes[1], ptr, addr, value
                )
            }
            AddressingMode::Indirect_Y => {
                let base = opcodes[1];

                let lo = cpu.trace_mem_read(base as u16);
                let hi = cpu.trace_mem_read((base).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(cpu.register_y as u16);
                let value = cpu.trace_mem_read(deref);

                format!(
                    "{: >4} (${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    opcode_text, opcodes[1], deref_base, deref, value
                )
            }
            AddressingMode::NoneAddressing => {
                format!("{: >4}", opcode_text)
            }
            AddressingMode::Relative => {
                let offset = (opcodes[1] as i8).wrapping_add(2);
                let new_addr = cpu.program_counter.wrapping_add_signed(offset.into());
                format!("{: >4} ${:04X}", opcode_text, new_addr)
            }
            AddressingMode::Accumulator => {
                format!("{: >4} A", opcode_text)
            }
        };

        Trace {
            pc: cpu.program_counter,
            opcodes,
            opcode_assembly: assembly_text,
            register_a: cpu.register_a,
            register_x: cpu.register_x,
            register_y: cpu.register_y,
            p: cpu.status,
            sp: (cpu.stack_pointer & 0x00FF) as u8,
            ppu_scanline,
            ppu_cycle,
        }
    }

    fn format_log(&self) -> String {
        // Program counter section
        let mut log = format!("{:04X} ", self.pc);

        // CPU OPCODE section
        for i in 0..3 {
            let opcode = self.opcodes.get(i);
            if let Some(op) = opcode {
                log = format!("{} {:02X}", log, op);
            } else {
                log = format!("{}   ", log);
            }
        }

        // CPU opcode in assembly language and real addresses
        // Work done in new function
        log = format!("{} {:31}", log, self.opcode_assembly);

        // CPU Registers
        log = format!(
            "{}  A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU: {:3}, {:3}",
            log, self.register_a, self.register_x, self.register_y, self.p, self.sp,
            self.ppu_scanline, self.ppu_cycle,
        );

        log
    }
}

pub fn trace(cpu: &mut CPU) -> String {
    let trace = Trace::new(cpu);
    trace.format_log()
}