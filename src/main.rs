use std::collections::HashMap;

use bus::Bus;
//use ppu::NesPPU;
use render::Frame;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

pub mod bus;
pub mod cartridges;
pub mod cpu;
pub mod joypad;
pub mod opcodes;
pub mod ppu;
pub mod render;
pub mod snake;
pub mod trace;
use cartridges::Rom;
use cpu::CPU;
use trace::trace;

fn main() {
    // init sdl2
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("NES Emulator", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .unwrap();

    let mut key_map = HashMap::new();
    key_map.insert(Keycode::Down, joypad::JoypadButton::new(0b0010_0000));
    key_map.insert(Keycode::Up, joypad::JoypadButton::new(0b0001_0000));
    key_map.insert(Keycode::Right, joypad::JoypadButton::new(0b1000_0000));
    key_map.insert(Keycode::Left, joypad::JoypadButton::new(0b0100_0000));
    key_map.insert(Keycode::Space, joypad::JoypadButton::new(0b0000_0100));
    key_map.insert(Keycode::Return, joypad::JoypadButton::new(0b0000_1000));
    key_map.insert(Keycode::A, joypad::JoypadButton::new(0b0000_0001));
    key_map.insert(Keycode::S, joypad::JoypadButton::new(0b0000_0010));

    //load the game
    let bytes: Vec<u8> = std::fs::read("roms/nestest.nes").unwrap();
    let rom = Rom::new(&bytes).unwrap();

    let bus = Bus::new(
        rom,
        move |frame: &mut Frame, joypad: &mut joypad::Joypad| {
            texture.update(None, &frame.data, 256 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => std::process::exit(0),
                    Event::KeyDown { keycode, .. } => {
                        if let Some(key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad.set_button_pressed_status(key, true);
                        }
                    }
                    Event::KeyUp { keycode, .. } => {
                        if let Some(key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad.set_button_pressed_status(key, false);
                        }
                    }
                    _ => { /* do nothing */ }
                }
            }
        },
    );

    let mut cpu = CPU::new(bus);
    cpu.reset();
    cpu.run_with_callback(move |_cpu| {
        //println!("{}", trace::trace(cpu));
    });
}
