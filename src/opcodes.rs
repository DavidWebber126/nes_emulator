use crate::cpu::AddressingMode;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub struct Opcode {
    pub opcode: u8,
    pub name: &'static str,
    pub bytes: u16,
    pub cycles: u8,
    pub addressing_mode: AddressingMode,
}

impl Opcode {
    fn new(
        opcode: u8,
        name: &'static str,
        bytes: u16,
        cycles: u8,
        addressing_mode: AddressingMode,
    ) -> Opcode {
        Opcode {
            opcode,
            name,
            bytes,
            cycles,
            addressing_mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OP_CODES: HashMap<u8, Opcode> = {
        let mut map = HashMap::new();
        map.insert(0x00, Opcode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing));

        map.insert(0x69, Opcode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate));
        map.insert(0x65, Opcode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x75, Opcode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x6D, Opcode::new(0x6D, "ADC", 3, 4, AddressingMode::Absolute));
        map.insert(0x7D, Opcode::new(0x7D, "ADC", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x79, Opcode::new(0x79, "ADC", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0x61, Opcode::new(0x61, "ADC", 2, 6, AddressingMode::Indirect_X));
        map.insert(0x71, Opcode::new(0x71, "ADC", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0x29, Opcode::new(0x29, "AND", 2, 2, AddressingMode::Immediate));
        map.insert(0x25, Opcode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x35, Opcode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x2D, Opcode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute));
        map.insert(0x3D, Opcode::new(0x3D, "AND", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x39, Opcode::new(0x39, "AND", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0x21, Opcode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X));
        map.insert(0x31, Opcode::new(0x31, "AND", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0x0a, Opcode::new(0x0a, "ASL", 1, 2, AddressingMode::Accumulator));
        map.insert(0x06, Opcode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x16, Opcode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x0e, Opcode::new(0x0e, "ASL", 3, 6, AddressingMode::Absolute));
        map.insert(0x1e, Opcode::new(0x1e, "ASL", 3, 7, AddressingMode::Absolute_X));

        map.insert(0x90, Opcode::new(0x90, "BCC", 2, 2, AddressingMode::Relative));
        map.insert(0xb0, Opcode::new(0xb0, "BCS", 2, 2, AddressingMode::Relative));
        map.insert(0xf0, Opcode::new(0xf0, "BEQ", 2, 2, AddressingMode::Relative));

        map.insert(0x24, Opcode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x2c, Opcode::new(0x2c, "BIT", 3, 4, AddressingMode::Absolute));

        map.insert(0x30, Opcode::new(0x30, "BMI", 2, 2, AddressingMode::Relative));
        map.insert(0xd0, Opcode::new(0xd0, "BNE", 2, 2, AddressingMode::Relative));
        map.insert(0x10, Opcode::new(0x10, "BPL", 2, 2, AddressingMode::Relative));
        map.insert(0x50, Opcode::new(0x50, "BVC", 2, 2, AddressingMode::Relative));
        map.insert(0x70, Opcode::new(0x70, "BVS", 2, 2, AddressingMode::Relative));

        map.insert(0x18, Opcode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xd8, Opcode::new(0xd8, "CLD", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x58, Opcode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xb8, Opcode::new(0xb8, "CLV", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0xc9, Opcode::new(0xc9, "CMP", 2, 2, AddressingMode::Immediate));
        map.insert(0xc5, Opcode::new(0xc5, "CMP", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xd5, Opcode::new(0xd5, "CMP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0xcd, Opcode::new(0xcd, "CMP", 3, 4, AddressingMode::Absolute));
        map.insert(0xdd, Opcode::new(0xdd, "CMP", 3, 4, AddressingMode::Absolute_X));
        map.insert(0xd9, Opcode::new(0xd9, "CMP", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0xc1, Opcode::new(0xc1, "CMP", 2, 6, AddressingMode::Indirect_X));
        map.insert(0xd1, Opcode::new(0xd1, "CMP", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0xe0, Opcode::new(0xe0, "CPX", 2, 2, AddressingMode::Immediate));
        map.insert(0xe4, Opcode::new(0xe4, "CPX", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xec, Opcode::new(0xec, "CPX", 3, 4, AddressingMode::Absolute));

        map.insert(0xc0, Opcode::new(0xc0, "CPY", 2, 2, AddressingMode::Immediate));
        map.insert(0xc4, Opcode::new(0xc4, "CPY", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xcc, Opcode::new(0xcc, "CPY", 3, 4, AddressingMode::Absolute));

        map.insert(0xc6, Opcode::new(0xc6, "DEC", 2, 5, AddressingMode::ZeroPage));
        map.insert(0xd6, Opcode::new(0xd6, "DEC", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0xce, Opcode::new(0xce, "DEC", 3, 6, AddressingMode::Absolute));
        map.insert(0xde, Opcode::new(0xde, "DEC", 3, 7, AddressingMode::Absolute_X));

        map.insert(0xca, Opcode::new(0xca, "DEX", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0x88, Opcode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0x49, Opcode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate));
        map.insert(0x45, Opcode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x55, Opcode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x4d, Opcode::new(0x4d, "EOR", 3, 4, AddressingMode::Absolute));
        map.insert(0x5d, Opcode::new(0x5d, "EOR", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x59, Opcode::new(0x59, "EOR", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0x41, Opcode::new(0x41, "EOR", 2, 6, AddressingMode::Indirect_X));
        map.insert(0x51, Opcode::new(0x51, "EOR", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0xe6, Opcode::new(0xe6, "INC", 2, 5, AddressingMode::ZeroPage));
        map.insert(0xf6, Opcode::new(0xf6, "INC", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0xee, Opcode::new(0xee, "INC", 3, 6, AddressingMode::Absolute));
        map.insert(0xfe, Opcode::new(0xfe, "INC", 3, 7, AddressingMode::Absolute_X));

        map.insert(0xe8, Opcode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0xc8, Opcode::new(0xc8, "INY", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0x4c, Opcode::new(0x4c, "JMP", 3, 3, AddressingMode::Absolute));
        map.insert(0x6c, Opcode::new(0x6c, "JMP", 3, 5, AddressingMode::Indirect));

        map.insert(0x20, Opcode::new(0x20, "JSR", 3, 6, AddressingMode::Absolute));

        map.insert(0xa9, Opcode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate));
        map.insert(0xa5, Opcode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xb5, Opcode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0xad, Opcode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute));
        map.insert(0xbd, Opcode::new(0xbd, "LDA", 3, 4, AddressingMode::Absolute_X));
        map.insert(0xb9, Opcode::new(0xb9, "LDA", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0xa1, Opcode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X));
        map.insert(0xb1, Opcode::new(0xb1, "LDA", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0xa2, Opcode::new(0xa2, "LDX", 2, 2, AddressingMode::Immediate));
        map.insert(0xa6, Opcode::new(0xa6, "LDX", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xb6, Opcode::new(0xb6, "LDX", 2, 4, AddressingMode::ZeroPage_Y));
        map.insert(0xae, Opcode::new(0xae, "LDX", 3, 4, AddressingMode::Absolute));
        map.insert(0xbe, Opcode::new(0xbe, "LDX", 3, 4, AddressingMode::Absolute_Y));

        map.insert(0xa0, Opcode::new(0xa0, "LDY", 2, 2, AddressingMode::Immediate));
        map.insert(0xa4, Opcode::new(0xa4, "LDY", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xb4, Opcode::new(0xb4, "LDY", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0xac, Opcode::new(0xac, "LDY", 3, 4, AddressingMode::Absolute));
        map.insert(0xbc, Opcode::new(0xbc, "LDY", 3, 4, AddressingMode::Absolute_X));

        map.insert(0x4a, Opcode::new(0x4a, "LSR", 1, 2, AddressingMode::Accumulator));
        map.insert(0x46, Opcode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x56, Opcode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x4e, Opcode::new(0x4e, "LSR", 3, 6, AddressingMode::Absolute));
        map.insert(0x5e, Opcode::new(0x5e, "LSR", 3, 7, AddressingMode::Absolute_X));

        map.insert(0xea, Opcode::new(0xea, "NOP", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0x09, Opcode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate));
        map.insert(0x05, Opcode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x15, Opcode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x0d, Opcode::new(0x0d, "ORA", 3, 4, AddressingMode::Absolute));
        map.insert(0x1d, Opcode::new(0x1d, "ORA", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x19, Opcode::new(0x19, "ORA", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0x01, Opcode::new(0x01, "ORA", 2, 6, AddressingMode::Indirect_X));
        map.insert(0x11, Opcode::new(0x11, "ORA", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0x48, Opcode::new(0x48, "PHA", 1, 3, AddressingMode::NoneAddressing));
        map.insert(0x08, Opcode::new(0x08, "PHP", 1, 3, AddressingMode::NoneAddressing));
        map.insert(0x68, Opcode::new(0x68, "PLA", 1, 4, AddressingMode::NoneAddressing));
        map.insert(0x28, Opcode::new(0x28, "PLP", 1, 4, AddressingMode::NoneAddressing));

        map.insert(0x2a, Opcode::new(0x2a, "ROL", 1, 2, AddressingMode::Accumulator));
        map.insert(0x26, Opcode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x36, Opcode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x2e, Opcode::new(0x2e, "ROL", 3, 6, AddressingMode::Absolute));
        map.insert(0x3e, Opcode::new(0x3e, "ROL", 3, 7, AddressingMode::Absolute_X));

        map.insert(0x6a, Opcode::new(0x6a, "ROR", 1, 2, AddressingMode::Accumulator));
        map.insert(0x66, Opcode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x76, Opcode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x6e, Opcode::new(0x6e, "ROR", 3, 6, AddressingMode::Absolute));
        map.insert(0x7e, Opcode::new(0x7e, "ROR", 3, 7, AddressingMode::Absolute_X));

        map.insert(0x40, Opcode::new(0x40, "RTI", 1, 6, AddressingMode::NoneAddressing));
        map.insert(0x60, Opcode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing));

        map.insert(0xe9, Opcode::new(0xe9, "SBC", 2, 2, AddressingMode::Immediate));
        map.insert(0xe5, Opcode::new(0xe5, "SBC", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xf5, Opcode::new(0xf5, "SBC", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0xed, Opcode::new(0xed, "SBC", 3, 4, AddressingMode::Absolute));
        map.insert(0xfd, Opcode::new(0xfd, "SBC", 3, 4, AddressingMode::Absolute_X));
        map.insert(0xf9, Opcode::new(0xf9, "SBC", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0xe1, Opcode::new(0xe1, "SBC", 2, 6, AddressingMode::Indirect_X));
        map.insert(0xf1, Opcode::new(0xf1, "SBC", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0x38, Opcode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xf8, Opcode::new(0xf8, "SED", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x78, Opcode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing));

        map.insert(0x85, Opcode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x95, Opcode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x8d, Opcode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute));
        map.insert(0x9d, Opcode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X));
        map.insert(0x99, Opcode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y));
        map.insert(0x81, Opcode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X));
        map.insert(0x91, Opcode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y));

        map.insert(0x86, Opcode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x96, Opcode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPage_Y));
        map.insert(0x8e, Opcode::new(0x8e, "STX", 3, 4, AddressingMode::Absolute));
        
        map.insert(0x84, Opcode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x94, Opcode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x8c, Opcode::new(0x8c, "STY", 3, 4, AddressingMode::Absolute));

        map.insert(0xaa, Opcode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xa8, Opcode::new(0xa8, "TAY", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xba, Opcode::new(0xba, "TSX", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x8a, Opcode::new(0x8a, "TXA", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x9a, Opcode::new(0x9a, "TXS", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x98, Opcode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing));

        // Undocumented Opcodes
        map.insert(0x0b, Opcode::new(0x0b, "*ANC", 2, 2, AddressingMode::Immediate));
        map.insert(0x2b, Opcode::new(0x2b, "*ANC", 2, 2, AddressingMode::Immediate));

        map.insert(0x6b, Opcode::new(0x6b, "*ARR", 2, 2, AddressingMode::Immediate));

        map.insert(0x4b, Opcode::new(0x4b, "*ASR", 2, 2, AddressingMode::Immediate));

        map.insert(0xc7, Opcode::new(0xc7, "*DCP", 2, 5, AddressingMode::ZeroPage));
        map.insert(0xd7, Opcode::new(0xd7, "*DCP", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0xcf, Opcode::new(0xcf, "*DCP", 3, 6, AddressingMode::Absolute));
        map.insert(0xdf, Opcode::new(0xdf, "*DCP", 3, 7, AddressingMode::Absolute_X));
        map.insert(0xdb, Opcode::new(0xdb, "*DCP", 3, 7, AddressingMode::Absolute_Y));
        map.insert(0xc3, Opcode::new(0xc3, "*DCP", 2, 8, AddressingMode::Indirect_X));
        map.insert(0xd3, Opcode::new(0xd3, "*DCP", 2, 8, AddressingMode::Indirect_Y));

        map.insert(0xe7, Opcode::new(0xe7, "*ISB", 2, 5, AddressingMode::ZeroPage));
        map.insert(0xf7, Opcode::new(0xf7, "*ISB", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0xef, Opcode::new(0xef, "*ISB", 3, 6, AddressingMode::Absolute));
        map.insert(0xff, Opcode::new(0xff, "*ISB", 3, 7, AddressingMode::Absolute_X));
        map.insert(0xfb, Opcode::new(0xfb, "*ISB", 3, 7, AddressingMode::Absolute_Y));
        map.insert(0xe3, Opcode::new(0xe3, "*ISB", 2, 8, AddressingMode::Indirect_X));
        map.insert(0xf3, Opcode::new(0xf3, "*ISB", 2, 8, AddressingMode::Indirect_Y));

        map.insert(0x02, Opcode::new(0x02, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x12, Opcode::new(0x12, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x22, Opcode::new(0x22, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x32, Opcode::new(0x32, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x42, Opcode::new(0x42, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x52, Opcode::new(0x52, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x62, Opcode::new(0x62, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x72, Opcode::new(0x72, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0x92, Opcode::new(0x92, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0xb2, Opcode::new(0xb2, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0xd2, Opcode::new(0xd2, "*JAM", 1, 0, AddressingMode::NoneAddressing));
        map.insert(0xf2, Opcode::new(0xf2, "*JAM", 1, 0, AddressingMode::NoneAddressing));

        map.insert(0xbb, Opcode::new(0xbb, "*LAS", 3, 4, AddressingMode::Absolute_Y));

        map.insert(0xab, Opcode::new(0xab, "*LAX", 2, 2, AddressingMode::Immediate));
        map.insert(0xa7, Opcode::new(0xa7, "*LAX", 2, 3, AddressingMode::ZeroPage));
        map.insert(0xb7, Opcode::new(0xa3, "*LAX", 2, 4, AddressingMode::ZeroPage_Y));
        map.insert(0xaf, Opcode::new(0xaf, "*LAX", 3, 4, AddressingMode::Absolute));
        map.insert(0xbf, Opcode::new(0xbf, "*LAX", 3, 4, AddressingMode::Absolute_Y));
        map.insert(0xa3, Opcode::new(0xa3, "*LAX", 2, 6, AddressingMode::Indirect_X));
        map.insert(0xb3, Opcode::new(0xb3, "*LAX", 2, 5, AddressingMode::Indirect_Y));

        map.insert(0x1a, Opcode::new(0x1a, "*NOP", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x3a, Opcode::new(0x3a, "*NOP", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x5a, Opcode::new(0x5a, "*NOP", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x7a, Opcode::new(0x7a, "*NOP", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xda, Opcode::new(0xda, "*NOP", 1, 2, AddressingMode::NoneAddressing));
        // Official NOP: map.insert(0xea, Opcode::new(0xea, "NOP", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0xfa, Opcode::new(0xfa, "*NOP", 1, 2, AddressingMode::NoneAddressing));
        map.insert(0x80, Opcode::new(0x80, "*NOP", 2, 2, AddressingMode::Immediate));
        map.insert(0x82, Opcode::new(0x82, "*NOP", 2, 2, AddressingMode::Immediate));
        map.insert(0x89, Opcode::new(0x89, "*NOP", 2, 2, AddressingMode::Immediate));
        map.insert(0xc2, Opcode::new(0xc2, "*NOP", 2, 2, AddressingMode::Immediate));
        map.insert(0xe2, Opcode::new(0xe2, "*NOP", 2, 2, AddressingMode::Immediate));
        map.insert(0x04, Opcode::new(0x04, "*NOP", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x44, Opcode::new(0x44, "*NOP", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x64, Opcode::new(0x64, "*NOP", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x14, Opcode::new(0x14, "*NOP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x34, Opcode::new(0x34, "*NOP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x54, Opcode::new(0x54, "*NOP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x74, Opcode::new(0x74, "*NOP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0xd4, Opcode::new(0xd4, "*NOP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0xf4, Opcode::new(0xf4, "*NOP", 2, 4, AddressingMode::ZeroPage_X));
        map.insert(0x0c, Opcode::new(0x0c, "*NOP", 3, 4, AddressingMode::Absolute));
        map.insert(0x1c, Opcode::new(0x1c, "*NOP", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x3c, Opcode::new(0x3c, "*NOP", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x5c, Opcode::new(0x5c, "*NOP", 3, 4, AddressingMode::Absolute_X));
        map.insert(0x7c, Opcode::new(0x7c, "*NOP", 3, 4, AddressingMode::Absolute_X));
        map.insert(0xdc, Opcode::new(0xdc, "*NOP", 3, 4, AddressingMode::Absolute_X));
        map.insert(0xfc, Opcode::new(0xfc, "*NOP", 3, 4, AddressingMode::Absolute_X));

        map.insert(0x27, Opcode::new(0x27, "*RLA", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x37, Opcode::new(0x37, "*RLA", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x2f, Opcode::new(0x2f, "*RLA", 3, 6, AddressingMode::Absolute));
        map.insert(0x3f, Opcode::new(0x3f, "*RLA", 3, 7, AddressingMode::Absolute_X));
        map.insert(0x3b, Opcode::new(0x3b, "*RLA", 3, 7, AddressingMode::Absolute_Y));
        map.insert(0x23, Opcode::new(0x23, "*RLA", 2, 8, AddressingMode::Indirect_X));
        map.insert(0x33, Opcode::new(0x33, "*RLA", 2, 8, AddressingMode::Indirect_Y));

        map.insert(0x67, Opcode::new(0x67, "*RRA", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x77, Opcode::new(0x77, "*RRA", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x6f, Opcode::new(0x6f, "*RRA", 3, 6, AddressingMode::Absolute));
        map.insert(0x7f, Opcode::new(0x7f, "*RRA", 3, 7, AddressingMode::Absolute_X));
        map.insert(0x7b, Opcode::new(0x7b, "*RRA", 3, 7, AddressingMode::Absolute_Y));
        map.insert(0x63, Opcode::new(0x63, "*RRA", 2, 8, AddressingMode::Indirect_X));
        map.insert(0x73, Opcode::new(0x73, "*RRA", 2, 8, AddressingMode::Indirect_Y));

        map.insert(0x87, Opcode::new(0x87, "*SAX", 2, 3, AddressingMode::ZeroPage));
        map.insert(0x97, Opcode::new(0x97, "*SAX", 2, 4, AddressingMode::ZeroPage_Y));
        map.insert(0x8f, Opcode::new(0x8f, "*SAX", 3, 4, AddressingMode::Absolute));
        map.insert(0x83, Opcode::new(0x83, "*SAX", 2, 6, AddressingMode::Indirect_X));

        map.insert(0xeb, Opcode::new(0xeb, "*SBC", 2, 2, AddressingMode::Immediate));

        map.insert(0xcb, Opcode::new(0xcb, "*SBX", 2, 2, AddressingMode::Immediate));

        map.insert(0x9f, Opcode::new(0x9f, "*SHA", 3, 5, AddressingMode::Absolute_Y));
        map.insert(0x93, Opcode::new(0x93, "*SHA", 2, 6, AddressingMode::Indirect_Y));

        map.insert(0x9b, Opcode::new(0x9b, "*SHS", 3, 5, AddressingMode::Absolute_Y));

        map.insert(0x9e, Opcode::new(0x9e, "*SHX", 3, 5, AddressingMode::Absolute_Y));

        map.insert(0x9c, Opcode::new(0x9c, "*SHY", 3, 5, AddressingMode::Absolute_X));

        map.insert(0x07, Opcode::new(0x07, "*SLO", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x17, Opcode::new(0x17, "*SLO", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x0f, Opcode::new(0x0f, "*SLO", 3, 6, AddressingMode::Absolute));
        map.insert(0x1f, Opcode::new(0x1f, "*SLO", 3, 7, AddressingMode::Absolute_X));
        map.insert(0x1b, Opcode::new(0x1b, "*SLO", 3, 7, AddressingMode::Absolute_Y));
        map.insert(0x03, Opcode::new(0x03, "*SLO", 2, 8, AddressingMode::Indirect_X));
        map.insert(0x13, Opcode::new(0x13, "*SLO", 2, 8, AddressingMode::Indirect_Y));

        map.insert(0x47, Opcode::new(0x47, "*SRE", 2, 5, AddressingMode::ZeroPage));
        map.insert(0x57, Opcode::new(0x57, "*SRE", 2, 6, AddressingMode::ZeroPage_X));
        map.insert(0x4f, Opcode::new(0x4f, "*SRE", 3, 6, AddressingMode::Absolute));
        map.insert(0x5f, Opcode::new(0x5f, "*SRE", 3, 7, AddressingMode::Absolute_X));
        map.insert(0x5b, Opcode::new(0x5b, "*SRE", 3, 7, AddressingMode::Absolute_Y));
        map.insert(0x43, Opcode::new(0x43, "*SRE", 2, 8, AddressingMode::Indirect_X));
        map.insert(0x53, Opcode::new(0x53, "*SRE", 2, 8, AddressingMode::Indirect_Y));

        map.insert(0x8b, Opcode::new(0x8b, "*XAA", 2, 2, AddressingMode::Immediate));

        map
    };
}
