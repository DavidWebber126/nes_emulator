use crate::bus::{Bus, Mem};
use crate::opcodes;

use std::time::Instant;

use std::collections::HashMap;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    Indirect, // only used by JMP opcode
    NoneAddressing,
    Relative,
    Accumulator,
}

pub struct CPU<'a> {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8, //0bNV1B_DIZC
    pub program_counter: u16,
    pub stack_pointer: u16, // between 0x0100 and 0x01FF. Is decremented when item added to the stack
    pub bus: Bus<'a>,
}

impl<'a> CPU<'a> {
    pub fn new(bus: Bus) -> CPU {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0b0010_0100,
            program_counter: 0,
            stack_pointer: 0x01FD,
            bus,
        }
    }

    pub fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data)
    }

    pub fn mem_read_u16(&mut self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    pub fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data)
    }

    // This function reads mem without side effects
    pub fn trace_mem_read(&mut self, addr: u16) -> u8 {
        self.bus.trace_mem_read(addr)
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = 0x01FD;
        self.status = 0b0010_0100;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn load(&mut self, program: Vec<u8>) {
        for i in 0..(program.len() as u16) {
            self.mem_write(0x0600 + i, program[i as usize]);
        }

        //self.mem_write_u16(0xFFFC, 0x0600);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    fn interrupt_irq(&mut self) {
        let mut flag = self.status;
        flag &= 0b1110_1111;
        flag |= 0b0010_0000;

        //pushes stack pointer and status onto stack
        self.stack_pointer = self.stack_pointer.wrapping_sub(3);
        let [lo, hi] = self.program_counter.to_le_bytes();
        self.mem_write(self.stack_pointer + 3, hi);
        self.mem_write(self.stack_pointer + 2, lo);
        self.mem_write(self.stack_pointer + 1, flag);

        self.status |= 0b0000_0100; //disable interrupt

        self.program_counter = self.mem_read_u16(0xfffe);
    }

    fn interrupt_nmi(&mut self) {
        let mut flag = self.status;
        flag &= 0b1110_1111;
        flag |= 0b0010_0000;

        self.stack_pointer = self.stack_pointer.wrapping_sub(3);
        let [lo, hi] = self.program_counter.to_le_bytes();
        self.mem_write(self.stack_pointer + 3, hi);
        self.mem_write(self.stack_pointer + 2, lo);
        self.mem_write(self.stack_pointer + 1, flag);

        self.status |= 0b0000_0100; //disable interrupt

        self.bus.tick(2);
        self.program_counter = self.mem_read_u16(0xfffa);
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            if let Some(_nmi) = self.bus.poll_nmi_status() {
                self.interrupt_nmi();
            }

            if self.bus.poll_irq_status().is_some() && (self.status & 0b0000_0100 == 0) {
                self.interrupt_irq();
            }

            let start = Instant::now();

            let opcodes: &HashMap<u8, opcodes::Opcode> = &opcodes::CPU_OP_CODES;

            callback(self);

            let opcode = self.mem_read(self.program_counter);
            let opcode = opcodes.get(&opcode).unwrap();
            self.program_counter += 1;

            match opcode.name {
                "ADC" => self.adc(&opcode.addressing_mode),
                "AND" => self.and(&opcode.addressing_mode),
                "ASL" => self.asl(&opcode.addressing_mode),
                "BCC" => self.bcc(),
                "BCS" => self.bcs(),
                "BEQ" => self.beq(),
                "BIT" => self.bit(&opcode.addressing_mode),
                "BMI" => self.bmi(),
                "BNE" => self.bne(),
                "BPL" => self.bpl(),
                "BVC" => self.bvc(),
                "BVS" => self.bvs(),
                "CLC" => self.clc(),
                "CLD" => self.cld(),
                "CLI" => self.cli(),
                "CLV" => self.clv(),
                "CMP" => self.cmp(&opcode.addressing_mode),
                "CPX" => self.cpx(&opcode.addressing_mode),
                "CPY" => self.cpy(&opcode.addressing_mode),
                "DEC" => self.dec(&opcode.addressing_mode),
                "DEX" => self.dex(),
                "DEY" => self.dey(),
                "EOR" => self.eor(&opcode.addressing_mode),
                "INC" => self.inc(&opcode.addressing_mode),
                "INX" => self.inx(),
                "INY" => self.iny(),
                "JMP" => self.jmp(&opcode.addressing_mode),
                "JSR" => self.jsr(&opcode.addressing_mode),
                "LDA" => self.lda(&opcode.addressing_mode),
                "LDX" => self.ldx(&opcode.addressing_mode),
                "LDY" => self.ldy(&opcode.addressing_mode),
                "LSR" => self.lsr(&opcode.addressing_mode),
                "NOP" => self.nop(&opcode.addressing_mode),
                "ORA" => self.ora(&opcode.addressing_mode),
                "PHA" => self.pha(),
                "PHP" => self.php(),
                "PLA" => self.pla(),
                "PLP" => self.plp(),
                "ROL" => self.rol(&opcode.addressing_mode),
                "ROR" => self.ror(&opcode.addressing_mode),
                "RTI" => self.rti(),
                "RTS" => self.rts(),
                "SBC" => self.sbc(&opcode.addressing_mode),
                "SEC" => self.sec(),
                "SED" => self.sed(),
                "SEI" => self.sei(),
                "STA" => self.sta(&opcode.addressing_mode),
                "STX" => self.stx(&opcode.addressing_mode),
                "STY" => self.sty(&opcode.addressing_mode),
                "TAX" => self.tax(),
                "TAY" => self.tay(),
                "TSX" => self.tsx(),
                "TXA" => self.txa(),
                "TXS" => self.txs(),
                "TYA" => self.tya(),
                "BRK" => return,
                // Illegal opcodes
                "*ANC" => self.anc(&opcode.addressing_mode),
                "*ARR" => self.arr(&opcode.addressing_mode),
                "*ASR" => self.asr(&opcode.addressing_mode),
                "*DCP" => self.dcp(&opcode.addressing_mode),
                "*ISB" => self.isb(&opcode.addressing_mode),
                "*JAM" => break,
                "*LAS" => self.las(&opcode.addressing_mode),
                "*LAX" => self.lax(&opcode.addressing_mode),
                "*NOP" => self.nop(&opcode.addressing_mode),
                "*RLA" => self.rla(&opcode.addressing_mode),
                "*RRA" => self.rra(&opcode.addressing_mode),
                "*SAX" => self.sax(&opcode.addressing_mode),
                "*SBC" => self.sbc(&opcode.addressing_mode),
                "*SBX" => self.sbx(&opcode.addressing_mode),
                "*SHA" => self.sha(&opcode.addressing_mode),
                "*SHS" => self.shs(&opcode.addressing_mode),
                "*SHX" => self.shx(&opcode.addressing_mode),
                "*SHY" => self.shy(&opcode.addressing_mode),
                "*SLO" => self.slo(&opcode.addressing_mode),
                "*SRE" => self.sre(&opcode.addressing_mode),
                _ => panic!("No implementation for {}", opcode.name),
            }

            self.bus.tick(opcode.cycles);
            self.program_counter = self.program_counter.wrapping_add(opcode.bytes - 1);

            while start.elapsed().as_nanos() < opcode.cycles as u128 * 500 {
                // wait
            }
        }
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> (u16, bool) {
        match mode {
            AddressingMode::Immediate => (self.program_counter, false),

            AddressingMode::ZeroPage => (self.mem_read(self.program_counter) as u16, false),

            AddressingMode::Absolute => (self.mem_read_u16(self.program_counter), false),

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                (addr, false)
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                (addr, false)
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                let page_crossed = (base & 0xff00) != (addr & 0xff00);
                (addr, page_crossed)
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                let page_crossed = (base & 0xff00) != (addr & 0xff00);
                (addr, page_crossed)
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                ((hi as u16) << 8 | (lo as u16), false)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                let page_crossed = (deref_base & 0xff) != (deref & 0xff00);
                (deref, page_crossed)
            }

            AddressingMode::Indirect => {
                let ptr = self.mem_read_u16(self.program_counter);
                let ptr_lo = (ptr & 0x00FF) as u8;
                let ptr_hi = ptr & 0xFF00;

                let lo = self.mem_read(ptr);
                let hi = self.mem_read(ptr_lo.wrapping_add(1) as u16 | ptr_hi);
                (u16::from_le_bytes([lo, hi]), false)
            }

            AddressingMode::Accumulator => {
                panic!(
                    "mode {:?} is not supported", 
                    mode
                );
            }

            AddressingMode::NoneAddressing => {
                panic!(
                    "mode {:?} should never be used to get a memory address",
                    mode
                );
            }

            AddressingMode::Relative => {
                panic!(
                    "mode {:?} should never be used to get a memory address",
                    mode
                );
            }
        }
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.add_to_reg_a(value);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn and(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn asl(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => {
                self.status = (self.status & 0b1111_1110) | (self.register_a >> 7);
                self.register_a <<= 1;
                self.update_zero_and_negative_flags(self.register_a);
            }
            _ => {
                let (addr, _) = self.get_operand_address(mode);
                let value = self.mem_read(addr);
                self.status = (self.status & 0b1111_1110) | (value >> 7);
                let new_value = value << 1;

                self.mem_write(addr, new_value);
                self.update_zero_and_negative_flags(new_value);
            }
        }
    }

    fn bcc(&mut self) {
        if (self.status & 0b0000_0001) == 0 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn bcs(&mut self) {
        if (self.status & 0b0000_0001) == 1 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn beq(&mut self) {
        if (self.status & 0b0000_0010) != 0 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if (value & self.register_a) == 0 {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        };
        self.status = (self.status & 0b0011_1111) | (value & 0b1100_0000);
    }

    fn bmi(&mut self) {
        if (self.status & 0b1000_0000) == 0b1000_0000 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn bne(&mut self) {
        if (self.status & 0b0000_0010) == 0 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn bpl(&mut self) {
        if (self.status & 0b1000_0000) == 0 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn bvc(&mut self) {
        if (self.status & 0b0100_0000) == 0 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn bvs(&mut self) {
        if (self.status & 0b0100_0000) == 0b0100_0000 {
            let addr = self.program_counter;
            let value = self.mem_read(addr) as i8;

            self.program_counter = self.program_counter.wrapping_add_signed(value.into());
            self.bus.tick(1);
            if (addr & 0xff00) != (self.program_counter.wrapping_add(1) & 0xff00) {
                self.bus.tick(1);
            }
        }
    }

    fn clc(&mut self) {
        self.status &= 0b1111_1110;
    }

    fn cld(&mut self) {
        self.status &= 0b1111_0111;
    }

    fn cli(&mut self) {
        self.status &= 0b1111_1011;
    }

    fn clv(&mut self) {
        self.status &= 0b1011_1111;
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if self.register_a >= value {
            self.status |= 0b0000_0001;
            self.status &= 0b0111_1111;
        } else {
            self.status &= 0b1111_1110;
            self.status |= 0b1000_0000;
        }

        self.update_zero_and_negative_flags(self.register_a.wrapping_sub(value));
        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if self.register_x >= value {
            self.status |= 0b0000_0001;
            self.status &= 0b0111_1111;
        } else {
            self.status &= 0b1111_1110;
            self.status |= 0b1000_0000;
        }

        self.update_zero_and_negative_flags(self.register_x.wrapping_sub(value));
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if self.register_y >= value {
            self.status |= 0b0000_0001;
            self.status &= 0b0111_1111;
        } else {
            self.status &= 0b1111_1110;
            self.status |= 0b1000_0000;
        }

        self.update_zero_and_negative_flags(self.register_y.wrapping_sub(value));
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(0xff);

        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_add(0xff);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_add(0xff);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        // add -2 to pc so that PC is at addr at end of cycle
        self.program_counter = addr.wrapping_add(0b1111_1111_1111_1110);
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);

        self.stack_pointer -= 1;
        self.program_counter = self.program_counter.wrapping_add(1);
        self.mem_write_u16(self.stack_pointer, self.program_counter);
        self.stack_pointer -= 1;
        self.program_counter = addr.wrapping_add(0b1111_1111_1111_1110);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => {
                self.status = (self.status & 0b1111_1110) | (self.register_a & 0b0000_0001);
                self.register_a >>= 1;
                self.update_zero_and_negative_flags(self.register_a);
            }
            _ => {
                let (addr, _) = self.get_operand_address(mode);
                let value = self.mem_read(addr);

                self.status = (self.status & 0b1111_1110) | (value & 0b0000_0001);
                let result = value >> 1;
                self.mem_write(addr, result);
                self.update_zero_and_negative_flags(result);
            }
        }
    }

    fn nop(&mut self, mode: &AddressingMode) {
        // does nothing but increment pc which is already done.
        match mode {
            AddressingMode::NoneAddressing => {
                // No cycle adjustment to be done
            }
            _ => {
                // adjust cycle if page crossed
                let (_, page_crossed) = self.get_operand_address(mode);
                if page_crossed {
                    self.bus.tick(1);
                }
            }
        }
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a |= value;
        self.update_zero_and_negative_flags(self.register_a);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn pha(&mut self) {
        self.mem_write(self.stack_pointer, self.register_a);
        self.stack_pointer -= 1;
    }

    fn php(&mut self) {
        let mut value = self.status;
        value |= 0b0011_0000;
        self.mem_write(self.stack_pointer, value);
        self.stack_pointer -= 1;
    }

    fn pla(&mut self) {
        self.register_a = self.mem_read(self.stack_pointer + 1);
        self.stack_pointer += 1;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        let value = self.mem_read(self.stack_pointer + 1);
        self.status = value & 0b1100_1111;
        self.status |= 0b0010_0000;
        self.stack_pointer += 1;
    }

    fn rol(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => {
                let carry = self.status & 0b0000_0001;
                // set carry to old bit 7
                self.status = (self.status & 0b1111_1110) | (self.register_a >> 7);
                self.register_a = (self.register_a << 1) | carry;
                self.update_zero_and_negative_flags(self.register_a);
            }
            _ => {
                let (addr, _) = self.get_operand_address(mode);
                let value = self.mem_read(addr);

                let carry = self.status & 0b0000_0001;
                self.status = (self.status & 0b1111_1110) | (value >> 7);
                let new_value = (value << 1) | carry;
                self.update_zero_and_negative_flags(new_value);
                self.mem_write(addr, new_value);
            }
        }
    }

    fn ror(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::Accumulator => {
                // preserve current carry to be moved into bit 7 of result
                let carry = (self.status & 0b0000_0001) << 7;
                // set carry in status to old bit 0
                self.status = (self.status & 0b1111_1110) | (self.register_a & 0b0000_0001);
                self.register_a = (self.register_a >> 1) | carry;
                self.update_zero_and_negative_flags(self.register_a);
            }
            _ => {
                let (addr, _) = self.get_operand_address(mode);
                let value = self.mem_read(addr);

                let carry = (self.status & 0b0000_0001) << 7;
                self.status = (self.status & 0b1111_1110) | (value & 0b0000_0001);
                let new_value = (value >> 1) | carry;
                self.update_zero_and_negative_flags(new_value);
                self.mem_write(addr, new_value);
            }
        }
    }

    fn rti(&mut self) {
        self.status = self.mem_read(self.stack_pointer + 1);
        let lo = self.mem_read(self.stack_pointer + 2);
        let hi = self.mem_read(self.stack_pointer + 3);
        self.stack_pointer += 3;

        self.program_counter = u16::from_le_bytes([lo, hi]);
        self.status |= 0b0010_0000;
        self.status &= 0b1110_1111;
    }

    fn rts(&mut self) {
        //pop stack pointer - 1 off the stack
        // increment stack by two bytes since pc is two bytes
        self.program_counter = self.mem_read_u16(self.stack_pointer + 1).wrapping_add(1);
        self.stack_pointer += 2;
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        // A = A-M-(1-C)
        // which is just A = A+!M+C
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.add_to_reg_a(!value);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn sec(&mut self) {
        self.status |= 0b0000_0001;
    }

    fn sed(&mut self) {
        self.status |= 0b0000_1000;
    }

    fn sei(&mut self) {
        self.status |= 0b0000_0100;
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = (self.stack_pointer & 0x00ff) as u8;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x as u16 | 0x0100;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    // Illegal opcodes

    fn anc(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);

        if self.register_a & 0b1000_0000 != 0 {
            self.status |= 0b0000_0001;
        } else {
            self.status &= 0b1111_1110;
        }
    }

    fn arr(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a & value;
        let carry = result & 0b0000_0001;
        let bit_six = self.register_a & 0b0100_0000;

        self.register_a = (result >> 1) | (carry << 7);
        self.update_zero_and_negative_flags(self.register_a);

        if self.status & 0b0000_1000 != 0 {
            if (self.register_a & 0b0100_0000) ^ bit_six != 0 {
                self.status |= 0b0100_0000;
            } else {
                self.status &= 0b1011_1111;
            }

            if (value & 0xf0) + (value & 0x10) > 0x50 {
                self.status |= 0b0000_0001;
            } else {
                self.status &= 0b1111_1110;
            }
        } else {
            if ((self.register_a & 0b0100_0000) >> 1) ^ (self.register_a & 0b0010_0000) != 0 {
                self.status |= 0b0100_0000;
            } else {
                self.status &= 0b1011_1111;
            }

            if self.register_a & 0b0100_0000 != 0 {
                self.status |= 0b0000_0001;
            } else {
                self.status &= 0b1111_1110;
            }
        }
    }

    fn asr(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = (self.register_a & value).rotate_right(1);
        // Bit 7 (Bit 0 before rotation) is set to 0, having moved to the carry flag instead.
        self.register_a = result & 0b0111_1111;

        self.update_zero_and_negative_flags(self.register_a);
        // negative flag is always resetted
        self.status &= 0b0111_1111;
        // set carry flag to bit 0 in the accumalator before rotation (bit 7 after rotation)
        self.status = (self.status & 0b1111_1110) | (result >> 7)
    }

    fn dcp(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let dec = value.wrapping_sub(1);

        self.mem_write(addr, dec);
        let cmp = self.register_a.wrapping_sub(dec);

        self.update_zero_and_negative_flags(cmp);
        if self.register_a < dec {
            self.status &= 0b1111_1110;
        } else {
            self.status |= 0b0000_0001;
        }
    }

    fn isb(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let inc = value.wrapping_add(1);
        self.mem_write(addr, inc);
        self.add_to_reg_a(!inc);

        if self.register_a & 0b1000_0000 != 0 {
            self.status |= 0b0000_0001;
        } else {
            self.status &= 0b1111_1110;
        }
    }

    fn las(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value & (self.stack_pointer & 0x00ff) as u8;
        self.update_zero_and_negative_flags(result);
        self.register_a = result;
        self.register_x = result;
        self.stack_pointer = 0x01ff & result as u16;

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn lax(&mut self, mode: &AddressingMode) {
        let (addr, page_crossed) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.register_x = value;
        self.update_zero_and_negative_flags(value);

        if page_crossed {
            self.bus.tick(1);
        }
    }

    fn rla(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let new_carry = (value & 0b1000_0000) >> 7;
        let result = (value << 1) | (self.status & 0b0000_0001);
        self.mem_write(addr, result);
        self.register_a &= self.register_a;
        self.status = (self.status & 0b1111_1110) | new_carry;

        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rra(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let new_carry = value & 0b0000_0001;
        let result = (value >> 1) | ((self.status & 0b0000_0001) << 7);
        self.mem_write(addr, result);
        self.status = (self.status & 0b1111_1110) | new_carry;
        self.add_to_reg_a(result);
    }

    fn sax(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a & self.register_x);
    }

    fn sbx(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let old_reg_a = self.register_a;
        self.register_a &= self.register_x;
        self.add_to_reg_a(!value);
        self.register_x = self.register_a;
        self.register_a = old_reg_a;
    }

    fn sha(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = match &mode {
            AddressingMode::Absolute_Y => {
                let upper = (self.mem_read_u16(self.program_counter) & 0xff00) >> 8;
                (upper as u8).wrapping_add(1)
            }
            AddressingMode::Indirect_Y => self.mem_read(self.program_counter).wrapping_add(1),
            _ => panic!(
                "SHA Opcode does not take any addressing mode other than Absolute Y and Indirect_Y"
            ),
        };

        self.mem_write(addr, self.register_a & self.register_x & value);
    }

    fn shs(&mut self, mode: &AddressingMode) {
        self.stack_pointer = (self.register_a & self.register_x) as u16 | 0x0100;

        let upper = (self.mem_read_u16(self.program_counter) & 0xff00) >> 8;
        let value = (upper as u8).wrapping_add(1);

        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.stack_pointer as u8 & value);
    }

    fn shx(&mut self, mode: &AddressingMode) {
        let upper = (self.mem_read_u16(self.program_counter) & 0xff00) >> 8;
        let value = (upper as u8).wrapping_add(1);

        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x & value);
    }

    fn shy(&mut self, mode: &AddressingMode) {
        let upper = (self.mem_read_u16(self.program_counter) & 0xff00) >> 8;
        let value = (upper as u8).wrapping_add(1);

        let (addr, _) = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y & value);
    }

    fn slo(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let new_carry = (value & 0b1000_0000) >> 7;
        let shifted = value << 1;
        self.mem_write(addr, shifted);
        self.register_a |= shifted;

        self.update_zero_and_negative_flags(self.register_a);
        self.status = (self.status & 0b1111_1110) | new_carry;
    }

    fn sre(&mut self, mode: &AddressingMode) {
        let (addr, _) = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let new_carry = value & 0b0000_0001;
        let shifted = value >> 1;
        self.mem_write(addr, shifted);
        self.register_a ^= shifted;

        self.update_zero_and_negative_flags(self.register_a);
        self.status = (self.status & 0b1111_1110) | new_carry;
    }

    // fn xaa(&mut self, mode: &AddressingMode) {
    //     panic!("XAA is not implemented yet")
    // }

    // helper functions

    fn add_to_reg_a(&mut self, value: u8) {
        let sum = self.register_a as u16 + value as u16 + (self.status & 0b0000_0001) as u16;
        self.update_carry_flag(sum);
        // set overflow bit if sign of result differs from sign of operands
        // XOR sum's bit 7 with each operand bit 7 to determine if signs do not match
        // if sum does not match both operands then set overflow. AND with 0x80 to extract sign bit
        let overflow = (self.register_a ^ (sum & 0x80) as u8) & (value ^ (sum & 0x80) as u8) & 0x80;

        if overflow != 0 {
            self.status |= 0b0100_0000;
        } else {
            self.status &= 0b1011_1111;
        }

        self.register_a = (sum & 0x00ff) as u8;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_carry_flag(&mut self, result: u16) {
        if result > 0xff {
            self.status |= 0b0000_0001;
        } else {
            self.status &= 0b1111_1110;
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }
}



#[cfg(test)]
mod test {
    //    use std::vec;
    //    use crate::cartridges::test::test_rom;
    //    use super::*;

    //    #[test]
    //    fn test_format_trace() {
    //        let mut bus = Bus::new(test_rom(), {});
    //        bus.mem_write(100, 0xa2);
    //        bus.mem_write(101, 0x01);
    //        bus.mem_write(102, 0xca);
    //        bus.mem_write(103, 0x88);
    //        bus.mem_write(104, 0x00);

    //        let mut cpu = CPU::new(bus);
    //        cpu.program_counter = 0x64;
    //        cpu.register_a = 1;
    //        cpu.register_x = 2;
    //        cpu.register_y = 3;
    //        let mut result: Vec<String> = vec![];
    //        cpu.run_with_callback(|cpu| {
    //            result.push(trace(cpu));
    //        });
    //        assert_eq!(
    //            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
    //            result[0]
    //        );
    //        assert_eq!(
    //            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
    //            result[1]
    //        );
    //        assert_eq!(
    //            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
    //            result[2]
    //        );
    //    }

    //   #[test]
    //    fn test_format_mem_access() {
    //        let mut bus = Bus::new(test_rom(), {});
    //        // ORA ($33), Y
    //        bus.mem_write(100, 0x11);
    //        bus.mem_write(101, 0x33);

    //        //data
    //        bus.mem_write(0x33, 00);
    //        bus.mem_write(0x34, 04);

    //        //target cell
    //        bus.mem_write(0x400, 0xAA);

    //        let mut cpu = CPU::new(bus);
    //        cpu.program_counter = 0x64;
    //        cpu.register_y = 0;
    //        let mut result: Vec<String> = vec![];
    //        cpu.run_with_callback(|cpu| {
    //            result.push(trace(cpu));
    //        });
    //        assert_eq!(
    //            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
    //            result[0]
    //        );
    //    }

    //    #[test]
    //    fn test_tya() {
    //     let bus = Bus::new(test_rom());
    //     let mut cpu = CPU::new(bus);
    //     // load 0xff into Y. A = Y. Done
    //     cpu.load_and_run(vec![0xa0, 0xff, 0x98]);

    //     assert_eq!(cpu.register_a, 0xff);
    //     assert_eq!(cpu.register_y, 0xff);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_txs() {
    //     let mut cpu = CPU::new();
    //     // Load 0x01 into X. Push X onto stack. Load 0x02 into X. Push X onto stack
    //     // Pull stack into A.
    //     cpu.load_and_run(vec![0xa2, 0x01, 0x9a, 0xa2, 0x02, 0x9a, 0x68]);

    //     assert_eq!(cpu.register_a, 0x02);
    //     assert_eq!(cpu.stack_pointer, 0x01fe);
    //     assert_eq!(cpu.mem_read(cpu.stack_pointer + 1), 0x01);
    //    }

    //    #[test]
    //    fn test_txa() {
    //     let mut cpu = CPU::new();
    //     // load 0xff into X. A = X. Done
    //     cpu.load_and_run(vec![0xa2, 0xff, 0x8a]);

    //     assert_eq!(cpu.register_a, 0xff);
    //     assert_eq!(cpu.register_x, 0xff);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_tsx() {
    //     let mut cpu = CPU::new();
    //     // Transfer stack pointer to X. Store X in 0x2010. Push status on stack. Pull value of stack into A.
    //     // Transfer A to Y.
    //     // Set A = 0xff. Push A onto stack
    //     // Transfer stack pointer to X.
    //     cpu.load_and_run(vec![0xba, 0x8e, 0x10, 0x20, 0x08, 0x68, 0xa8, 0xa9, 0xff, 0x48, 0xba]);

    //     //nothing in stack
    //     assert_eq!(cpu.mem_read(0x2010), 0xff);
    //     // status flag from above
    //     assert_eq!(cpu.register_y, 0b1000_0000);
    //     // one item on the stack
    //     assert_eq!(cpu.register_x, 0xfe);
    //     assert_eq!(cpu.stack_pointer, 0x01fe);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_sty() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0xa9, 0xff, 0xa8, 0x8c, 0x10, 0x20]);

    //     assert_eq!(cpu.mem_read(0x2010), 0xff);
    //     assert_eq!(cpu.register_y, 0xff);
    //    }

    //    #[test]
    //    fn test_stx() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0x8e, 0x10, 0x20]);

    //     assert_eq!(cpu.mem_read(0x2010), 0xff);
    //     assert_eq!(cpu.register_x, 0xff);
    //    }

    //    #[test]
    //    fn test_sta() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0xa9, 0xff, 0x8d, 0x10, 0x20]);

    //     assert_eq!(cpu.mem_read(0x2010), 0xff);
    //     assert_eq!(cpu.register_a, 0xff);
    //    }

    //    #[test]
    //    fn test_sei() {
    //     let mut cpu1 = CPU::new();
    //     cpu1.load_and_run(vec![0x78]);

    //     assert_eq!(cpu1.status & 0b0000_0100, 0b0000_0100);

    //     let mut cpu2 = CPU::new();
    //     cpu2.load_and_run(vec![0x78, 0x78]);

    //     assert_eq!(cpu2.status & 0b0000_0100, 0b0000_0100);
    //    }

    //    #[test]
    //    fn test_sed() {
    //     let mut cpu1 = CPU::new();
    //     cpu1.load_and_run(vec![0xf8]);

    //     assert_eq!(cpu1.status & 0b0000_1000, 0b0000_1000);

    //     let mut cpu2 = CPU::new();
    //     cpu2.load_and_run(vec![0xf8, 0xf8]);

    //     assert_eq!(cpu2.status & 0b0000_1000, 0b0000_1000);
    //    }

    //    #[test]
    //    fn test_sec_off() {
    //     let mut cpu1 = CPU::new();
    //     cpu1.load_and_run(vec![0x38]);

    //     assert_eq!(cpu1.status & 0b0000_0001, 1);
    //    }

    //    #[test]
    //    fn test_sbc_overflow_without_carry() {
    //     // overflow occurs when positive - negative = negative or negative - positive = positive
    //     let mut cpu = CPU::new();
    //     // Load 0xff into A. Subtract 0x02 and !C from A. Break.
    //     cpu.load_and_run(vec![0xa9, 0x80, 0xe9, 0x00]);

    //     // Note that carry bit is 0 so 1-C = 1.
    //     assert_eq!(cpu.register_a, 0x7f);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0100_0000, 0b0100_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //    }

    //    #[test]
    //    fn test_sbc_negative_result_without_carry() {
    //     let mut cpu = CPU::new();
    //     // Load 0x03 into A. Subtract 0x10 and !C from A. Break.
    //     cpu.load_and_run(vec![0xa9, 0x03, 0xe9, 0x10]);

    //     // Note that carry bit is 0 so 1-C = 1.
    //     assert_eq!(cpu.register_a, 0xf2);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0100_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //    }

    //    #[test]
    //    fn test_sbc_positive_result_with_carry() {
    //     let mut cpu = CPU::new();
    //     // Load 0x04 into A. Add 0xff to A. Subtract 0x01 and !C from A. Break.
    //     cpu.load_and_run(vec![0xa9, 0x04, 0x69, 0xff, 0xe9, 0x01]);

    //     // Note result is 0x02 since carry bit is 1 so 1-C = 0.
    //     assert_eq!(cpu.register_a, 0x02);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0100_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //    }

    //    #[test]
    //    fn test_rts() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x2010, 0xa9);
    //     cpu.mem_write(0x2011, 0x01);
    //     cpu.mem_write(0x2012, 0x60);
    //     // Jump to subroutine at address 0x2010. After subroutine, set A = A + 0x01. Break
    //     cpu.load_and_run(vec![0x20, 0x10, 0x20, 0x69, 0x01]);

    //     assert_eq!(cpu.register_a, 0x02);
    //     assert_eq!(cpu.program_counter, 0x8006);
    //     assert_eq!(cpu.stack_pointer, 0x1ff);
    //    }

    //    #[test]
    //    fn test_rti() {

    //    }

    //    #[test]
    //    fn test_ror_memory() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x2010, 0x70);
    //     //Load 0x81 into A. Add 0x80 to A. Rotate value at 0x2010 right.
    //     cpu.load_and_run(vec![0xa9, 0x81, 0x69, 0x80, 0x6e, 0x10, 0x20]);

    //     assert_eq!(cpu.mem_read(0x2010), 0xb8);
    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_ror_accumulator() {
    //     let mut cpu = CPU::new();
    //     //Load 0x81 into A. Rotate A right. Break
    //     cpu.load_and_run(vec![0xa9, 0x81, 0x6a]);

    //     assert_eq!(cpu.register_a, 0x40);
    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_rol_memory() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x2010, 0x70);
    //     //Load 0x81 into A. Rotate A left. Add 0x80 to A. Rotate value at 0x2010 left.
    //     cpu.load_and_run(vec![0xa9, 0x81, 0x69, 0x80, 0x2e, 0x10, 0x20]);

    //     assert_eq!(cpu.mem_read(0x2010), 0xe1);
    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_rol_accumulator() {
    //     let mut cpu = CPU::new();
    //     //Load 0x81 into A. Rotate A left. Break
    //     cpu.load_and_run(vec![0xa9, 0x81, 0x2a]);

    //     assert_eq!(cpu.register_a, 0x02);
    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_plp() {
    //     let mut cpu = CPU::new();

    //     // Load 0x00 into A. Store status on stack. Load 0xff into A. Pop status off the stack
    //     cpu.load_and_run(vec![0xa9, 0x00, 0x08, 0xa9, 0xff, 0x28]);

    //     assert_eq!(cpu.status, 0b0000_0010);
    //     assert_eq!(cpu.stack_pointer, 0x1ff);
    //    }

    //    #[test]
    //    fn test_pla() {
    //     let mut cpu = CPU::new();

    //     // load 0xff into A. Store A on the stack. Load 0x00 into A. Pop top value off the stack into A.
    //     cpu.load_and_run(vec![0xa9, 0xff, 0x48, 0xa9, 0x00, 0x68]);

    //     assert_eq!(cpu.register_a, 0xff);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.stack_pointer, 0x1ff);
    //    }

    //    #[test]
    //    fn test_php() {
    //     let mut cpu = CPU::new();

    //     cpu.mem_write(0x2010, 0xff);
    //     // load 0x10 into A. Add A with value at 0x2010. Store status byte on stack. Break
    //     cpu.load_and_run(vec![0xa9, 0x80, 0x6d, 0x10, 0x20, 0x08]);

    //     assert_eq!(cpu.register_a, 0x7f);
    //     assert_eq!(cpu.mem_read(cpu.stack_pointer+1), 0b0100_0001);
    //    }

    //    #[test]
    //    fn test_pha() {
    //     let mut cpu = CPU::new();

    //     //load 0x01 into A. Store A on the stack. Load 0x02 into A. Store A onto stack...
    //     cpu.load_and_run(vec![0xa9, 0x01, 0x48, 0xa9, 0x02, 0x48, 0xa9, 0x03, 0x48]);

    //     assert_eq!(cpu.stack_pointer, 0x1fc);
    //     assert_eq!(cpu.mem_read(cpu.stack_pointer+1), 0x03);
    //     assert_eq!(cpu.mem_read(cpu.stack_pointer+2), 0x02);
    //     assert_eq!(cpu.mem_read(cpu.stack_pointer+3), 0x01);
    //    }

    //    #[test]
    //    fn test_ora() {
    //     let mut cpu = CPU::new();
    //     // load 0xaa into A. ORA with 0xf0. Break
    //     cpu.load_and_run(vec![0xa9, 0xaa, 0x09, 0xf0]);

    //     assert_eq!(cpu.register_a, 0xfa);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_nop() {
    //     let mut cpu = CPU::new();
    //     // Start at 0x8000. NOP 5 times to get to 0x8005. Then 0x00 increments PC one more time so 0x8006
    //     cpu.load_and_run(vec![0xea, 0xea, 0xea, 0xea, 0xea, 0x00]);

    //     assert_eq!(cpu.program_counter, 0x8006);
    //    }

    //    #[test]
    //    fn test_lsr_memory() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x2010, 0xf0);
    //     // Shift value at address 0x2010 to the right one bit. Break.
    //     cpu.load_and_run(vec![0x4e, 0x10, 0x20, 0x00]);

    //     assert_eq!(cpu.mem_read(0x2010), 0b0111_1000);
    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_lsr_accumulator() {
    //     let mut cpu = CPU::new();
    //     // Load 0x01 into A. Shift A right by one bit. Break.
    //     cpu.load_and_run(vec![0xa9, 0x01, 0x4a, 0x00]);

    //     assert_eq!(cpu.register_a, 0);
    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_ldy() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0xa0, 0x00]);

    //     assert_eq!(cpu.register_y, 0x00);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //    }

    //    #[test]
    //    fn test_ldx() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0xa2, 0xf0]);

    //     assert_eq!(cpu.register_x, 0xf0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_jsr() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0x20, 0x10, 0x20]);

    //     assert_eq!(cpu.program_counter, 0x2011);
    //     assert_eq!(cpu.stack_pointer, 0x01fd);
    //     assert_eq!(cpu.mem_read_u16(cpu.stack_pointer+1), 0x8002);
    //    }

    //    #[test]
    //    fn test_jmp_absolute() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x2010, 0xa9);
    //     cpu.mem_write(0x2011, 0x01);
    //     // jmp to 0x2010. load 0x01 into A. Stop.
    //     cpu.load_and_run(vec![0x4c, 0x10, 0x20]);

    //     assert_eq!(cpu.program_counter, 0x2013);
    //     assert_eq!(cpu.register_a, 0x01);
    //    }

    //    #[test]
    //    fn test_jmp_indirect() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x2010, 0x10);
    //     cpu.mem_write(0x2011, 0x30);
    //     cpu.mem_write(0x3010, 0xa9);
    //     cpu.mem_write(0x3011, 0x01);
    //     // jmp to 0x2010. load 0x01 into A. Stop.
    //     cpu.load_and_run(vec![0x6c, 0x10, 0x20]);

    //     assert_eq!(cpu.program_counter, 0x3013);
    //     assert_eq!(cpu.register_a, 0x01);
    //    }

    //    #[test]
    //    fn test_iny() {
    //     let mut cpu = CPU::new();
    //     cpu.load_and_run(vec![0xa9, 0x7f, 0xa8, 0xc8, 0x00]);

    //     assert_eq!(cpu.register_y, 0x80);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_iny_wrap() {
    //     let mut cpu = CPU::new();
    //     // Load 0xff into A. Set Y = A. Increment Y
    //     cpu.load_and_run(vec![0xa9, 0xff, 0xa8, 0xc8, 0x00]);

    //     assert_eq!(cpu.register_y, 0x00);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //    }

    //    #[test]
    //    fn test_inc() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x1010, 0x7f);
    //     cpu.load_and_run(vec![0xee, 0x10, 0x10]);

    //     assert_eq!(cpu.mem_read(0x1010), 0x80);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_inc_wrap() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x1010, 0xff);
    //     cpu.load_and_run(vec![0xee, 0x10, 0x10]);

    //     assert_eq!(cpu.mem_read(0x1010), 0x00);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //    }

    //    #[test]
    //    fn test_eor() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x1010, 0x0f);
    //     // store 0x55 in A. EOR/XOR with value at address 0x1010
    //     cpu.load_and_run(vec![0xa9, 0x55, 0x4d, 0x10, 0x10, 0x00]);

    //     assert_eq!(cpu.register_a, 0b01011010);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_dey_five() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x05);

    //     // load into A the value 0x05. Put A into X. Decrement X.
    //     cpu.load_and_run(vec![0xa9, 0x05, 0xa8, 0x88, 0x00]);

    //     assert_eq!(cpu.register_y, 4);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_dey_one() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x01);

    //     // load into A the value 0x01. Put A into X. Decrement X.
    //     cpu.load_and_run(vec![0xa9, 0x01, 0xa8, 0x88, 0x00]);

    //     assert_eq!(cpu.register_y, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //    }

    //    #[test]
    //    fn test_dey_zero() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x00);

    //     // load into A the value 0x00. Put A into Y. Decrement Y.
    //     cpu.load_and_run(vec![0xa9, 0x00, 0xa8, 0x88, 0x00]);

    //     assert_eq!(cpu.register_y, 0xff);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_dex_five() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x05);

    //     // load into A the value 0x05. Put A into X. Decrement X.
    //     cpu.load_and_run(vec![0xa9, 0x05, 0xaa, 0xca, 0x00]);

    //     assert_eq!(cpu.register_x, 4);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_dex_one() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x01);

    //     // load into A the value 0x01. Put A into X. Decrement X.
    //     cpu.load_and_run(vec![0xa9, 0x01, 0xaa, 0xca, 0x00]);

    //     assert_eq!(cpu.register_x, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //    }

    //    #[test]
    //    fn test_dex_zero() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x00);

    //     // load into A the value 0x00. Put A into X. Decrement X.
    //     cpu.load_and_run(vec![0xa9, 0x00, 0xaa, 0xca, 0x00]);

    //     assert_eq!(cpu.register_x, 0xff);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_dec_five() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x05);

    //     cpu.load_and_run(vec![0xce, 0x10, 0x00, 0x00]);

    //     assert_eq!(cpu.mem_read(0x10), 4);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_dec_one() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x01);

    //     cpu.load_and_run(vec![0xce, 0x10, 0x00, 0x00]);

    //     assert_eq!(cpu.mem_read(0x10), 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //    }

    //    #[test]
    //    fn test_dec_zero() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0x00);

    //     cpu.load_and_run(vec![0xce, 0x10, 0x00, 0x00]);

    //     assert_eq!(cpu.mem_read(0x10), 0xff);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_cpy_equal() {
    //     let mut cpu = CPU::new();

    //     // load 0x01 into A. Load A into Y. Compare Y to 0x01
    //     cpu.load_and_run(vec![0xa9, 0x01, 0xa8, 0xc0, 0x01]);

    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_cpy_greater_than() {
    //     let mut cpu = CPU::new();

    //     // load 0x30 into A. Load A into Y. Compare Y to 0x01
    //     cpu.load_and_run(vec![0xa9, 0x30, 0xa8, 0xc0, 0x01]);

    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_cpy_less_than() {
    //     let mut cpu = CPU::new();

    //     // load 0x01 into A. Load A into Y. Compare Y to 0x30
    //     cpu.load_and_run(vec![0xa9, 0x01, 0xa8, 0xc0, 0x30]);

    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //    }

    //    #[test]
    //    fn test_cpx_equal() {
    //     let mut cpu = CPU::new();

    //     // load 0x01 into A. Load A into X. Compare X to 0x01
    //     cpu.load_and_run(vec![0xa9, 0x01, 0xaa, 0xe0, 0x01]);

    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b10);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_cpx_greater_than() {
    //     let mut cpu = CPU::new();

    //     // load 0x30 into A. Load A into X. Compare X to 0x01
    //     cpu.load_and_run(vec![0xa9, 0x30, 0xaa, 0xe0, 0x01]);

    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_cpx_less_than() {
    //     let mut cpu = CPU::new();

    //     // load 0x01 into A. Load A into X. Compare X to 0x30
    //     cpu.load_and_run(vec![0xa9, 0x01, 0xaa, 0xe0, 0x30]);

    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //    }

    //    #[test]
    //    fn test_cmp_equal() {
    //     let mut cpu = CPU::new();

    //     cpu.load_and_run(vec![0xa9, 0xff, 0xc9, 0xff, 0x00]);

    //     assert_eq!(cpu.status & 0b0000_0001, 0b0000_0001);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //    }

    //    #[test]
    //    fn test_cmp_less_than() {
    //     let mut cpu = CPU::new();

    //     cpu.load_and_run(vec![0xa9, 0xff, 0xc9, 0xf0, 0x00]);

    //     assert_eq!(cpu.status & 0b0000_0001, 0b0000_0001);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000);
    //    }

    //    #[test]
    //    fn test_cmp_greater_than() {
    //     let mut cpu = CPU::new();

    //     cpu.load_and_run(vec![0xa9, 0xf0, 0xc9, 0xff, 0x00]);

    //     assert_eq!(cpu.status & 0b0000_0001, 0b0000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //    }

    //    #[test]
    //    fn test_clv() {
    //     let mut cpu = CPU::new();

    //     cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x01, 0xb8, 0x00]);

    //     assert_eq!(cpu.status & 0b0000_0100, 0);
    //    }

    //    #[test]
    //    fn test_cli() {
    //     let mut cpu = CPU::new();

    //     cpu.load(vec![0x58, 0x00]);
    //     cpu.reset();
    //     cpu.status = 0b0000_0100;
    //     cpu.run();

    //     assert_eq!(cpu.status & 0b0000_0100, 0);
    //    }

    //    #[test]
    //    fn test_cld() {
    //     let mut cpu = CPU::new();

    //     cpu.load(vec![0xd8, 0x00]);
    //     cpu.reset();
    //     cpu.status = 0b0000_1000;
    //     cpu.run();

    //     assert_eq!(cpu.status & 0b0000_1000, 0);
    //    }

    //    #[test]
    //    fn test_clc() {
    //     let mut cpu = CPU::new();

    //     cpu.mem_write(0x10, 0x01);
    //     cpu.load_and_run(vec![0xa9, 0xff, 0x65, 0x10, 0x18, 0x00]);

    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //    }

    //    #[test]
    //    fn test_bvs() {
    //         let mut cpu = CPU::new();
    //         // PC is 8001 when 0x30 is called. Will add 0x03 to it and increment by one (since two bytes are read)
    //         // hence PC will be at 8005 which is break so final PC is 8006.
    //         cpu.load(vec![0x70, 0x03]);
    //         cpu.reset();
    //         cpu.status = 0b0100_0000;
    //         cpu.run();

    //         assert_eq!(cpu.program_counter, 0x8006);
    //    }

    //    #[test]
    //    fn test_bvc() {
    //         let mut cpu = CPU::new();
    //         // PC is 8001 when 0x30 is called. Will add 0x03 to it and increment by one (since two bytes are read)
    //         // hence PC will be at 8005 which is break so final PC is 8006.
    //         cpu.load(vec![0x50, 0x03]);
    //         cpu.reset();
    //         cpu.status = 0b0000_0000;
    //         cpu.run();

    //         assert_eq!(cpu.program_counter, 0x8006);
    //    }

    //    #[test]
    //    fn test_bpl() {
    //         let mut cpu = CPU::new();
    //         // PC is 8002 when 0x30 is called. Will add 0x03 to it and increment by two (since two bytes are read)
    //         // hence PC will be at 8007 which is break so final PC is 8008.
    //         cpu.load_and_run(vec![0xa9, 0x0f, 0x10, 0x03]);

    //         assert_eq!(cpu.program_counter, 0x8008);
    //    }

    //    #[test]
    //    fn test_bne() {
    //         let mut cpu = CPU::new();

    //         cpu.load_and_run(vec![0xa9, 0x01, 0xd0, 0x03]);

    //         assert_eq!(cpu.program_counter, 0x8008);
    //    }

    //    #[test]
    //    fn test_bmi() {
    //         let mut cpu = CPU::new();
    //         // PC is 8002 when 0x30 is called. Will add 0x03 to it and increment by two (since two bytes are read)
    //         // hence PC will be at 8007 which is break so final PC is 8008.
    //         cpu.load_and_run(vec![0xa9, 0xf0, 0x30, 0x03]);

    //         assert_eq!(cpu.program_counter, 0x8008);
    //    }

    //    #[test]
    //    fn test_bit_absolute() {
    //         let mut cpu = CPU::new();
    //         cpu.mem_write(0x10, 0b1100_0000);

    //         cpu.load_and_run(vec![0xa9, 0xf0, 0x2c, 0x10, 0x00]);

    //         assert_eq!(cpu.status >> 6, 0b0000_0011);
    //    }

    //     #[test]
    //     fn test_beq() {
    //         let mut cpu = CPU::new();

    //         cpu.load(vec![0xf0,0x03, 0x00, 0x00, 0x00, 0x00]);
    //         cpu.reset();
    //         cpu.status = 0b0000_0010;
    //         cpu.run();

    //         assert_eq!(cpu.program_counter, 0x8006)
    //     }

    //     #[test]
    //     fn test_bcs() {
    //     let mut cpu = CPU::new();

    //     // memory locations 0x8000, 0x8001, ...
    //     // PC is increment before instruction is ran so the 0x03 byte at location 0x8001
    //     // tells the PC to go to 0x8002 + 0x0003 = 0x8005. Then BRK is ran which increments PC by 1.
    //     cpu.load(vec![0x90, 0x03, 0x00, 0x00, 0x00, 0x00]);
    //     cpu.reset();
    //     cpu.status = 0b0000_0001;
    //     cpu.run();

    //     assert_eq!(cpu.program_counter, 0x8006);
    //     }

    //    #[test]
    //    fn test_bcc() {
    //     let mut cpu = CPU::new();

    //     // memory locations 0x8000, 0x8001, ...
    //     // PC is increment before instruction is ran so the 0x03 byte at location 0x8001
    //     // tells the PC to go to 0x8002 + 0x0003 = 0x8005. Then BRK is ran which increments PC by 1.
    //     cpu.load_and_run(vec![0x90, 0x03, 0x00, 0x00, 0x00, 0x00]);

    //     assert_eq!(cpu.program_counter, 0x8006);
    //    }

    //    #[test]
    //    fn test_asl_accumulator() {
    //     let mut cpu = CPU::new();

    //     cpu.load(vec![0x0a, 0x00]);
    //     cpu.reset();
    //     cpu.register_a = 0b1010_1010;
    //     cpu.run();

    //     assert_eq!(cpu.register_a, 0b0101_0100);
    //     assert_eq!(cpu.status & 0b0000_0001, 1);
    //     assert_eq!(cpu.status & 0b1000_0000, 0);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_asl_memory() {
    //     let mut cpu = CPU::new();

    //     cpu.mem_write(0x10, 0b0111_0000);
    //     cpu.load_and_run(vec![0x06, 0x10, 0x00]);
    //     assert_eq!(cpu.mem_read(0x10), 0b1110_0000);
    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0000_0010, 0);
    //    }

    //    #[test]
    //    fn test_and_immediate() {
    //     let mut cpu = CPU::new();

    //     cpu.load(vec![0x29, 0xf0, 0x00]);
    //     cpu.reset();
    //     cpu.register_a = 0b1011_1101;
    //     cpu.run();

    //     assert_eq!(cpu.register_a, 0b1011_0000);
    //    }

    //    #[test]
    //    fn test_adc_carry_overflow() {
    //     let mut cpu = CPU::new();
    //     // Load 0xff into A. Set carry to 1. Then A = A + M + C where M is at 0x2010 (which is 0).
    //     cpu.load_and_run(vec![0xa9, 0x7f, 0x38, 0x6d, 0x10, 0x20]);

    //     assert_eq!(cpu.register_a, 0x80);
    //     assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    //     assert_eq!(cpu.status & 0b0100_0000, 0b0100_0000);
    //     assert_eq!(cpu.status & 0b0000_0001, 0);
    //    }

    //    #[test]
    //    fn test_adc_immediate() {
    //        let mut cpu = CPU::new();

    //        cpu.load(vec![0x69, 0x55, 0x00]);
    //        cpu.reset();
    //        cpu.status = 0b0000_0001;
    //        cpu.run();

    //        assert_eq!(cpu.register_a, 0x56);
    //        assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000);
    //        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000);
    //        assert_eq!(cpu.status & 0b0100_0000, 0b0000_0000);
    //    }

    //    #[test]
    //    fn test_adc_from_memory() {
    //     let mut cpu = CPU::new();
    //     cpu.mem_write(0x10, 0xa0);

    //     cpu.load(vec![0x65, 0x10, 0x00]);
    //     cpu.reset();
    //     cpu.register_a = 0x82;
    //     cpu.status = 0b0000_0001;
    //     cpu.run();

    //     assert_eq!(cpu.register_a, 0x23);
    //     // carry bit should be set
    //     assert_eq!(cpu.status & 0b0000_0001, 0x01);
    //     // sum is positive
    //     assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000);
    //     // sum is nonzero
    //     assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000);
    //     // overflow occured since A and M were negative and sum is positive
    //     assert_eq!(cpu.status & 0b0100_0000, 0b0100_0000);
    //    }

    //    #[test]
    //    fn test_lda_from_memory() {
    //        let mut cpu = CPU::new();
    //        cpu.mem_write(0x10, 0x55);

    //        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

    //        assert_eq!(cpu.register_a, 0x55);
    //    }

    //    #[test]
    //    fn test_0xa9_lda_immediate_load_data() {
    //        let mut cpu = CPU::new();
    //        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
    //        assert_eq!(cpu.register_a, 0x05);
    //        assert!(cpu.status & 0b0000_0010 == 0b00);
    //        assert!(cpu.status & 0b1000_0000 == 0);
    //    }

    //     #[test]
    //     fn test_0xa9_lda_zero_flag() {
    //         let mut cpu = CPU::new();
    //         cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
    //         assert!(cpu.status & 0b0000_0010 == 0b10);
    //     }

    //     #[test]
    //     fn test_0xaa_tax_move_a_to_x() {
    //         let mut cpu = CPU::new();
    //         cpu.load(vec![0xaa, 0x00]);
    //         cpu.reset();
    //         cpu.register_a = 10;
    //         cpu.run();

    //         assert_eq!(cpu.register_x, 10)
    //     }

    //    #[test]
    //    fn test_5_ops_working_together() {
    //        let mut cpu = CPU::new();
    //        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

    //        assert_eq!(cpu.register_x, 0xc1)
    //    }

    //     #[test]
    //     fn test_inx_overflow() {
    //         let mut cpu = CPU::new();
    //         cpu.load(vec![0xe8, 0xe8, 0x00]);
    //         cpu.reset();
    //         cpu.register_x = 0xff;
    //         cpu.run();

    //         assert_eq!(cpu.register_x, 1)
    //     }
}
