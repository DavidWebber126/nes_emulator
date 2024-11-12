//const AUDIO_QUEUE_SIZE: usize = 8 * 1024;

pub struct Apu {
    pub pulse1: PulseRegister,
    pub pulse2: PulseRegister,
    pub triangle: TriangleRegister,
    pub noise: NoiseRegister,
    pub dmc: DmcRegister,
    pub status: u8,
    pub frame_counter: FrameCounter,
    pub irq_interrupt: Option<u8>,
    pub buffer: Vec<f32>,
    cycles: usize,
}

impl Apu {
    pub fn new(cartridge_rom: Vec<u8>) -> Self {
        Self {
            pulse1: PulseRegister::new(1),
            pulse2: PulseRegister::new(0),
            triangle: TriangleRegister::new(),
            noise: NoiseRegister::new(),
            dmc: DmcRegister::new(cartridge_rom),
            status: 0,
            frame_counter: FrameCounter::new(),
            irq_interrupt: None,
            cycles: 0,
            buffer: Vec::new(),
        }
    }

    pub fn write_status(&mut self, data: u8) {
        self.status = (self.status & 0b1110_0000) | (data & 0b0001_1111);
        if data & 1 != 0 {
            self.pulse1.unmute();
        } else {
            self.pulse1.mute();
        }
        if data & 0b10 != 0 {
            self.pulse2.unmute();
        } else {
            self.pulse2.mute();
        }
        if data & 0b100 != 0 {
            self.triangle.unmute();
        } else {
            self.triangle.mute();
        }
        if data & 0b1000 != 0 {
            self.noise.unmute();
        } else {
            self.noise.mute();
        }
        if data & 0b1_0000 == 0 {
            self.dmc.mute();
        } else if self.dmc.bytes_remaining == 0 {
            self.dmc.restart();
            self.dmc.unmute();
        } else {
            self.dmc.unmute();
        }
        self.status &= 0b0111_1111;
    }

    pub fn read_status(&mut self) -> u8 {
        if self.pulse1.length_counter.value > 0 {
            self.status |= 0b1;
        } else {
            self.status &= 0b1111_1110;
        }
        if self.pulse2.length_counter.value > 0 {
            self.status |= 0b10;
        } else {
            self.status &= 0b1111_1101;
        }
        if self.triangle.length_counter.value > 0 {
            self.status |= 0b100;
        } else {
            self.status &= 0b1111_1011;
        }
        if self.noise.length_counter.value > 0 {
            self.status |= 0b1000;
        } else {
            self.status &= 0b1111_0111;
        }
        if self.dmc.bytes_remaining > 0 {
            self.status |= 0b1_0000;
        } else {
            self.status &= 0b1110_1111;
        }
        let old_status = self.status;
        self.status &= 0b1011_1111;
        old_status
    }

    pub fn write_fc(&mut self, data: u8) {
        if data & 0b1000_0000 != 0 {
            self.quarter_frame_tick();
            self.half_frame_tick();
        }
        if data & 0b0100_0000 != 0 {
            self.status &= 0b1011_1111;
        }
        self.frame_counter.write(data);
    }

    pub fn frame_counter_tick(&mut self) {
        let frame = self.frame_counter.tick();
        match frame {
            None => {}
            Some(Frame::QuarterFrame) => self.quarter_frame_tick(),
            Some(Frame::HalfFrame) => {
                self.quarter_frame_tick();
                self.half_frame_tick();
            }
        }
        if self.frame_counter.frame_interrupt {
            self.status |= 0b0100_0000;
            self.frame_counter.frame_interrupt = false;
        }
    }

    fn quarter_frame_tick(&mut self) {
        self.pulse1.envelope.clock();
        self.pulse2.envelope.clock();
        self.triangle.linear_counter.clock();
        self.noise.envelope.clock();
    }

    fn half_frame_tick(&mut self) {
        // clock length counters
        self.pulse1.length_counter.clock();
        self.pulse2.length_counter.clock();
        self.triangle.length_counter.clock();
        self.noise.length_counter.clock();

        // clock sweep units
        self.pulse1.sweep_clock();
        self.pulse2.sweep_clock();
    }

    fn dmc_tick(&mut self) {
        self.dmc.tick();
        
        if self.dmc.interrupt {
            self.status |= 0b1000_0000;
            self.dmc.interrupt = false;
        }
    }

    pub fn tick(&mut self, cycles: u8) {
        let cycles = cycles as usize;
        for _ in 0..cycles {
            self.cycles += 1;
            // Triangle ticks every CPU clock. Rest tick at every other CPU clock tick
            self.triangle.tick();
            self.frame_counter_tick();
            if self.cycles % 2 == 1 {
                self.pulse1.tick();
                self.pulse2.tick();
                self.noise.tick();
                self.dmc_tick();
            }
            let output = self.output();
            self.buffer.push(output);
        }
        self.cycles %= 2;
    }

    fn output(&self) -> f32 {
        let p1 = self.pulse1.output();
        let p2 = self.pulse2.output();
        let tr = self.triangle.output();
        let nc = self.noise.output();
        let dmc = self.dmc.output();

        let pulse_out: f32;
        let tnd_out: f32;
        if p1 == 0 && p2 == 0 {
            pulse_out = 0.0;
        } else {
            pulse_out = 95.88 / (100.0 + (8128.0 / (p1 as f32 + p2 as f32)));
        }
        if tr == 0 && nc == 0 && dmc == 0 {
            tnd_out = 0.0; 
        } else {
            tnd_out = 159.79 / (100.0 + (1.0 / ((tr as f32 / 8227.0) + (nc as f32 / 12241.0) + (dmc as f32 / 22638.0)))); 
        }
        pulse_out + tnd_out
    }
}

struct Envelope {
    constant_flag: bool,
    divider_count: u8,
    decay_level_counter: u8,
    loop_flag: bool,
    start_flag: bool,
    volume: u8,
}

impl Envelope {
    pub fn new() -> Self {
        Self {
            constant_flag: false,
            divider_count: 0,
            decay_level_counter: 0,
            loop_flag: true,
            start_flag: true,
            volume: 0,
        }
    }

    pub fn write(&mut self, data: u8) {
        self.loop_flag = data & 0b0010_0000 != 0;
        self.constant_flag = data & 0b0001_0000 != 0;
        self.volume = data & 0b1111;
    }

    pub fn clock(&mut self) {
        if self.start_flag {
            self.start_flag = false;
            self.decay_level_counter = 15;
            self.divider_count = self.volume;
        } else if self.divider_count > 0 {
            self.divider_count -= 1;
        } else {
            self.divider_count = self.volume;
            if self.decay_level_counter > 0 {
                self.decay_level_counter -= 1;
            } else if self.loop_flag {
                self.decay_level_counter = 15;
            }
        }
    }

    pub fn volume(&self) -> u8 {
        if self.constant_flag {
            self.volume
        } else {
            self.decay_level_counter
        }
    }
}

struct Sweep {
    enabled: bool,
    divider_count: u8,
    negate_flag: bool,
    period: u8,
    reload_flag: bool,
    shift_count: u8,
}

impl Sweep {
    pub fn new() -> Self {
        Self {
            enabled: false,
            divider_count: 0,
            negate_flag: false,
            period: 0,
            reload_flag: false,
            shift_count: 0,
        }
    }

    pub fn write(&mut self, data: u8) {
        self.enabled = data & 0b1000_0000 != 0;
        self.period = (data & 0b0111_0000) >> 4;
        self.negate_flag = data & 0b0000_1000 != 0;
        self.shift_count = data & 0b0000_0111;
    }

    pub fn divider_tick(&mut self) {
        if self.divider_count == 0 {
            self.divider_count = self.period;
        } else {
            self.divider_count -= 1;
        }
    }
}

// Only bottom 11 bits are used but no u11 type.
struct Timer{
    period: u16,
    value: u16,
}

impl Timer {
    pub fn new() -> Self {
        Self {
            value: 0,
            period: 0
        }
    }

    pub fn set_low_byte(&mut self, byte: u8) {
        self.period = (self.period & 0b111_0000_0000) | byte as u16;
    }

    // takes bottom 3 bits of input param. Ignores the rest
    pub fn set_high_3_bits(&mut self, bits: u8) {
        let top_bits = ((bits & 0b111) as u16) << 8;
        self.period = (self.period & 0b000_1111_1111) | top_bits;
    }

    pub fn tick(&mut self) -> bool {
        if self.value == 0 {
            self.value = self.period;
            true
        } else {
            self.value -= 1;
            false
        }
    }
}

struct LengthCounter {
    enabled: bool,
    halt_flag: bool,
    value: u8,
}

impl LengthCounter {
    const LENGTH_TABLE: [u8; 32] = [
        10,  254, 20, 2,  40, 4,  80, 6,
        160, 8,   60, 10, 14, 12, 26, 14,
        12,  16,  24, 18, 48, 20, 96, 22,
        192, 24,  72, 26, 16, 28, 32, 30,
    ];

    pub fn new() -> Self {
        Self {
            enabled: false,
            halt_flag: false,
            value: 0,
        }
    }

    pub fn clock(&mut self) {
        if self.value > 0 && !self.halt_flag {
            self.value -= 1;
        }
    }

    pub fn set_index(&mut self, index: u8) {
        if self.enabled {
            self.value = LengthCounter::LENGTH_TABLE[index as usize];
        }
    }

    pub fn mute(&mut self) {
        self.value = 0;
        self.enabled = false;
    }

    pub fn unmute(&mut self) {
        self.enabled = true;
    }
}

pub struct PulseRegister {
    channel: u16,
    duty: u8,
    envelope: Envelope,
    length_counter: LengthCounter,
    muted: bool,
    sweep: Sweep,
    sequencer_index: usize,
    timer: Timer,
}

impl PulseRegister {
    const PULSE_WAVEFORM: [[u8;8];4] = [
        [0, 1, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 1, 0, 0, 0],
        [1, 0, 0, 1, 1, 1, 1, 1]
    ];

    pub fn new(channel: u16) -> Self {
        Self {
            channel,
            duty: 0,
            envelope: Envelope::new(),
            length_counter: LengthCounter::new(),
            muted: false,
            sweep: Sweep::new(),
            sequencer_index: 0,
            timer: Timer::new(),
        }
    }

    pub fn write_to_envelope(&mut self, data: u8) {
        self.duty = data & 0b1100_0000 >> 6;
        self.length_counter.halt_flag = data & 0b0010_0000 != 0;
        self.envelope.write(data);
    }

    pub fn write_to_sweep(&mut self, data: u8) {
        self.sweep.write(data);
        self.sweep.reload_flag = true;
    }

    pub fn write_to_timer_low(&mut self, data: u8) {
        self.timer.set_low_byte(data);
    }

    pub fn write_to_timer_high(&mut self, data: u8) {
        self.timer.set_high_3_bits(data);
        self.length_counter.set_index((data & 0b1111_1000) >> 3); // bottom 3 bits go to timer. Rest goes to length counter
        // side effects
        self.sequencer_index = 0; // sequencer restart
        self.envelope.start_flag = true;
        //self.duty = 0;
    }

    pub fn mute(&mut self) {
        self.length_counter.mute();
        self.muted = true;
    }

    pub fn unmute(&mut self) {
        self.length_counter.unmute();
        self.muted = false;
    }

    fn compute_target_period(&self) -> u16 {
        let change_amount = self.timer.period >> self.sweep.shift_count;
        let mut target_period = self.timer.period;
        if self.sweep.negate_flag {
            target_period = target_period.saturating_sub(change_amount + self.channel);
        } else {
            target_period += change_amount;
        }
        target_period
    }

    pub fn tick(&mut self) {
        let timer_clocked = self.timer.tick();
        if timer_clocked {
            self.sequencer_index += 1;
            self.sequencer_index %= 8;
        }
    }

    pub fn output(&self) -> u8 {
        if PulseRegister::PULSE_WAVEFORM[self.duty as usize][self.sequencer_index] == 0
            || self.compute_target_period() > 0x7ff
            || self.length_counter.value == 0
            || self.timer.period < 8
            || self.muted
        {
            0
        } else {
            self.envelope.volume()
        }
    }

    pub fn sweep_clock(&mut self) {
        if self.sweep.divider_count == 0 && self.sweep.enabled && self.sweep.shift_count != 0 {
            let target_period = self.compute_target_period();
            if self.timer.period < 8 || target_period > 0x7FF {
                //self.sweep.divider_tick()
            } else {
                self.timer.period = target_period;
                //self.timer.value = target_period;
            }
        }

        if self.sweep.divider_count == 0 || self.sweep.reload_flag {
            self.sweep.divider_count = self.sweep.period;
            self.sweep.reload_flag = false;
        } else {
            self.sweep.divider_tick();
        }
    }
}

pub struct LinearCounter {
    reload_flag: bool,
    reload_value: u8,
    control_flag: bool,
    value: u8,
}

impl LinearCounter {
    pub fn new() -> Self {
        Self {
            reload_flag: false,
            reload_value: 0,
            control_flag: false,
            value: 0,
        }
    }

    pub fn write(&mut self, data: u8) {
        self.control_flag = data & 0b1000_0000 != 0;
        self.reload_value = data & 0b0111_1111;
        //self.value = self.reload_value;
    }

    pub fn clock(&mut self) {
        if self.reload_flag {
            self.value = self.reload_value;
        } else if self.value > 0 {
            self.value -= 1;
        }

        if !self.control_flag {
            self.reload_flag = false;
        }
    }
}

pub struct TriangleRegister {
    length_counter: LengthCounter,
    linear_counter: LinearCounter,
    muted: bool,
    timer: Timer,
    output_level: usize, // In [0,31]
}

impl TriangleRegister {
    // These volume values form the triangle shape
    const TRIANGLE_WAVEFORM: [u8; 32] = [
        15, 14, 13, 12, 11, 10, 9,  8,
        7,  6,  5,  4,  3,  2,  1,  0,
        0,  1,  2,  3,  4,  5,  6,  7,
        8,  9,  10, 11, 12, 13, 14, 15,
    ];

    pub fn new() -> Self {
        Self {
            length_counter: LengthCounter::new(),
            linear_counter: LinearCounter::new(),
            muted: false,
            timer: Timer::new(),
            output_level: 0,
        }
    }

    pub fn write_to_linear_counter(&mut self, data: u8) {
        self.linear_counter.write(data);
        self.length_counter.halt_flag = data & 0b1000_0000 != 0;
    }

    pub fn write_to_timer_low(&mut self, data: u8) {
        self.timer.set_low_byte(data);
    }

    pub fn write_to_timer_high(&mut self, data: u8) {
        self.timer.set_high_3_bits(data);
        self.length_counter.set_index((data & 0b1111_1000) >> 3); // bottom 3 bits go to timer. Rest goes to length counter
        self.linear_counter.reload_flag = true; // side effect of writing to register
    }

    pub fn mute(&mut self) {
        self.length_counter.mute();
        self.muted = true;
    }

    pub fn unmute(&mut self) {
        self.length_counter.unmute();
        self.muted = false;
    }

    pub fn tick(&mut self) {
        let timer_clocked = self.timer.tick();
        if timer_clocked && self.linear_counter.value > 0 && self.length_counter.value > 0 {
            self.output_level += 1;
            self.output_level %= 32;
        };
    }

    pub fn output(&self) -> u8 {
        // if self.muted {
        //     0
        // } else if self.timer.period < 2
        // {
        //     7
        // } else {
        //     TriangleRegister::TRIANGLE_WAVEFORM[self.output_level]
        // }

        if self.muted || self.length_counter.value == 0 || self.linear_counter.value == 0 {
            0
        } else if self.timer.period < 2 {
            7
        } else {
            TriangleRegister::TRIANGLE_WAVEFORM[self.output_level]
        }
    }
}

pub struct ShiftRegister {
    mode: bool,
    reg: u16, // 15 bits wide
}

impl ShiftRegister {
    pub fn new() -> Self {
        Self{ mode: false, reg: 1 } //Shift Reg is initialized with 0x01
    }

    pub fn clock(&mut self) {
        let feedback: u16;
        if self.mode {
            feedback = ((self.reg >> 6) & 0b1) ^ (self.reg & 0b1);
        } else {
            feedback = ((self.reg >> 1) & 0b1) ^ (self.reg & 0b1);
        }
        self.reg >>= 1;
        self.reg |= feedback << 14;
    }
}

pub struct NoiseRegister {
    envelope: Envelope,
    length_counter: LengthCounter,
    muted: bool,
    shift_reg: ShiftRegister,
    timer: Timer,
}

impl NoiseRegister {
    pub fn new() -> Self {
        Self {
            envelope: Envelope::new(),
            length_counter: LengthCounter::new(),
            muted: false,
            shift_reg: ShiftRegister::new(),
            timer: Timer::new(),
        }
    }

    pub fn write_to_envelope(&mut self, data: u8) {
        self.length_counter.halt_flag = data & 0b0010_0000 != 0;
        self.envelope.write(data);
    }

    pub fn mode_and_period(&mut self, data: u8) {
        self.shift_reg.mode = data & 0b1000_0000 != 0;
        self.timer.period = match data & 0b1111 {
            0 => 4,
            1 => 8,
            2 => 16,
            3 => 32,
            4 => 64,
            5 => 96,
            6 => 128,
            7 => 160,
            8 => 202,
            9 => 254,
            10 => 380,
            11 => 508,
            12 => 762,
            13 => 1016,
            14 => 2034,
            15 => 4068,
            _ => panic!("Should not be possible")
        };
        self.timer.period = self.timer.period / 2 - 1;
        //self.timer.value = self.timer.period;
    }

    pub fn write_to_length_counter(&mut self, data: u8) {
        self.length_counter.set_index((data & 0b1111_1000) >> 3);
        self.envelope.start_flag = true;
    }

    pub fn mute(&mut self) {
        self.length_counter.mute();
        self.muted = true;
    }

    pub fn unmute(&mut self) {
        self.length_counter.unmute();
        self.muted = false;
    }

    pub fn tick(&mut self) {
        let timer_clocked = self.timer.tick();
        if timer_clocked {
            self.shift_reg.clock();
        };
    }

    pub fn output(&self) -> u8 {
        if self.length_counter.value == 0 
        || self.shift_reg.reg & 0b1 == 1
        || self.muted
        {
            0
        } else {
            self.envelope.volume()
        }
    }
}

pub struct DmcRegister {
    bits_remaining: u8,
    bytes_remaining: usize,
    cartridge_rom: Vec<u8>,
    disabled: bool,
    irq_flag: bool,
    interrupt: bool,
    loop_flag: bool,
    output_level: u8,
    sample_buffer: Option<u8>,
    sample_address: u16,
    shift_register: u8,
    current_address: u16,
    sample_length: usize,
    silence_flag: bool,
    timer: Timer,
}

impl DmcRegister {
    pub fn new(cartridge_rom: Vec<u8>) -> Self {
        Self {
            bits_remaining: 0,
            bytes_remaining: 0,
            cartridge_rom,
            disabled: true,
            irq_flag: false,
            interrupt: false,
            loop_flag: false,
            output_level: 0,
            sample_buffer: None,
            sample_address: 0xc000,
            shift_register: 0,
            current_address: 0xc000,
            sample_length: 0,
            silence_flag: true,
            timer: Timer::new(),
        }
    }

    pub fn flags_and_rate(&mut self, data: u8) {
        self.irq_flag = data & 0b1000_0000 != 0;

        if !self.irq_flag {
            self.interrupt = false;
        }

        self.loop_flag = data & 0b0100_0000 != 0;
        self.timer.period = match data & 0b1111 {
            0 => 428,
            1 => 380,
            2 => 340,
            3 => 320,
            4 => 286,
            5 => 254,
            6 => 226,
            7 => 214,
            8 => 190,
            9 => 160,
            10 => 142,
            11 => 128,
            12 => 106,
            13 => 84,
            14 => 72,
            15 => 54,
            _ => panic!("Should not reach here")
        };
        self.timer.period = self.timer.period / 2 - 1;
    }

    pub fn direct_load(&mut self, data: u8) {
        self.output_level = data & 0b0111_1111;
    }

    pub fn sample_address(&mut self, data: u8) {
        self.sample_address = 0xc000 + ((data as u16) << 6);
    }

    pub fn sample_length(&mut self, data: u8) {
        self.sample_length = (16 * data as usize) + 1;
    }

    pub fn restart(&mut self) {
        self.current_address = self.sample_address;
        self.bytes_remaining = self.sample_length;
        if self.sample_buffer.is_none() && self.bytes_remaining > 0 {
            self.sample_refill();
        }
    }

    pub fn mute(&mut self) {
        self.bytes_remaining = 0;
        self.disabled = true;
    }

    pub fn unmute(&mut self) {
        self.disabled = false;
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.cartridge_rom.len() == 0x4000 && addr >= 0x4000 {
            //mirror if needed
            addr %= 0x4000;
        }
        self.cartridge_rom[addr as usize]
    }

    pub fn new_cycle(&mut self) {
        self.bits_remaining = 8;
        if self.sample_buffer.is_none() {
            self.silence_flag = true;
        } else {
            self.silence_flag = false;
            self.shift_register = self.sample_buffer.take().unwrap();
        }
        if self.sample_buffer.is_none() && self.bytes_remaining > 0 {
            self.sample_refill();
        }
    }

    pub fn sample_refill(&mut self) {
        // TODO: stall cpu for 1-4 cpu cycles
        self.sample_buffer = Some(self.read_prg_rom(self.current_address));
        if self.current_address < 0xFFFF {
            self.current_address += 1;
        } else {
            self.current_address = 0x8000;
        }

        self.bytes_remaining -= 1;
        if self.bytes_remaining == 0 && self.loop_flag {
            self.restart();
        } else if self.bytes_remaining == 0 && self.irq_flag {
            self.interrupt = true;
        }
    }

    fn output_level_adjust(&mut self) {
        let bit0 = self.shift_register & 0b1;
        if bit0 == 1 && self.output_level < 126 {
            self.output_level += 2;
        }
        if bit0 == 0 && self.output_level > 1 {
            self.output_level -= 2;
        }
    }

    pub fn tick(&mut self) {
        let timer_clocked = self.timer.tick();
        if timer_clocked {
            if !self.silence_flag {
                self.output_level_adjust()
            }
            self.shift_register >>= 1;
            
            if self.bits_remaining > 1 {
                self.bits_remaining -= 1;
            } else {
                self.new_cycle();
            }
        };
    }

    pub fn output(&self) -> u8 {
        if self.disabled {
            0
        } else {
            self.output_level
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum FCMode {
    FourStep,
    FiveStep,
}

pub enum Frame {
    QuarterFrame,
    HalfFrame,
}

pub struct FrameCounter {
    mode: FCMode,
    step_level: usize,
    inhibit_irq: bool,
    frame_interrupt: bool,
    cycle_count: usize,
    reset_counter: usize,
}

impl FrameCounter {
    const FOURSTEP_STEP_CYCLES: [usize; 6] = [7457, 14913, 22371, 29828, 29829, 29830]; //29828 works
    const FIVESTEP_STEP_CYCLES: [usize; 6] = [7457, 14913, 22371, 29829, 37281, 37282];

    pub fn new() -> Self {
        Self {
            mode: FCMode::FourStep,
            step_level: 0,
            inhibit_irq: false,
            frame_interrupt: false,
            cycle_count: 0,
            reset_counter: 0,
        }
    }

    pub fn write(&mut self, data: u8) {
        if data & 0b1000_0000 == 0 {
            self.mode = FCMode::FourStep;
        } else {
            self.mode = FCMode::FiveStep;
        }
        self.inhibit_irq = data & 0b0100_0000 != 0;
        if self.inhibit_irq {
            self.frame_interrupt = false;
        }
        if self.cycle_count % 2 == 0 {
            self.reset_counter = 5;
        } else {
            self.reset_counter = 4;
        }
    }

    pub fn reset(&mut self) {
        self.cycle_count = 0;
        self.step_level = 0;
    }

    pub fn tick(&mut self) -> Option<Frame> {
        if self.reset_counter > 0 {
            self.reset_counter -= 1;
            if self.reset_counter == 0 {
                self.reset();
            }
        }
        self.cycle_count += 1;
        let step = match self.mode {
            FCMode::FourStep => {
                FrameCounter::FOURSTEP_STEP_CYCLES[self.step_level]
            }
            FCMode::FiveStep => {
                FrameCounter::FIVESTEP_STEP_CYCLES[self.step_level]
            }
        };

        if self.cycle_count >= step {
            self.step_level += 1;
            if self.step_level >= 4 && !self.inhibit_irq && self.mode == FCMode::FourStep {
                self.frame_interrupt = true;
            }
            if self.step_level == 6 {
                self.step_level = 0;
                self.cycle_count = 0;
            }
            match self.step_level {
                1 | 3 => return Some(Frame::QuarterFrame),
                2 | 5 => return Some(Frame::HalfFrame),
                4 | 0 => {}
                _ => panic!("Impossible State")
            }
        }
        None
    }
}
