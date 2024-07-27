// 0 - A
// 1 - B
// 2 - Select
// 3 - Start
// 4 - Up
// 5 - Down
// 6 - Left
// 7 - Right
pub struct JoypadButton(u8);

impl JoypadButton {
    pub fn new(value: u8) -> Self {
        JoypadButton(value)
    }
}

pub struct Joypad {
    strobe: bool,
    button_index: u8,
    button_status: JoypadButton,
}

impl Joypad {
    pub fn new() -> Self {
        Joypad {
            strobe: false,
            button_index: 0,
            button_status: JoypadButton(0),
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = (data & 0b1) == 1;
        if self.strobe {
            self.button_index = 0
        };
    }

    pub fn read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 1;
        }
        let response = (self.button_status.0 & (1 << self.button_index)) >> self.button_index;
        if !self.strobe && self.button_index <= 7 {
            self.button_index += 1;
        }
        response
    }

    // Returns same as above but without side effects
    pub fn trace_read(&mut self) -> u8 {
        if self.button_index > 7 {
            return 1;
        }
        let response = (self.button_status.0 & (1 << self.button_index)) >> self.button_index;
        response
    }

    pub fn set_button_pressed_status(&mut self, button: &JoypadButton, pressed: bool) {
        if pressed {
            self.button_status.0 |= button.0
        } else {
            self.button_status.0 &= !button.0
        }
    }
}

impl Default for Joypad {
    fn default() -> Self {
        Self::new()
    }
}
