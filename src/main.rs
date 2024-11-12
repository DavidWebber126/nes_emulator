use std::collections::HashMap;

use bus::Bus;
//use ppu::NesPPU;
use render::Frame;
use sdl2::audio::{AudioQueue, AudioSpecDesired};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Canvas;
use sdl2::video::Window;
use sdl2::EventPump;

pub mod bus;
pub mod cartridges;
pub mod cpu;
pub mod joypad;
pub mod opcodes;
pub mod ppu;
pub mod render;
pub mod snake;
pub mod trace;
pub mod apu;
mod filter;
use filter::{lowpass_filter, highpass_filter};
use cartridges::Rom;
use cpu::CPU;
//use trace::trace;

fn setup_sld2() -> (Canvas<Window>, EventPump, AudioQueue<f32>) {
    // init sdl2
    let sdl_context = sdl2::init().unwrap();

    // Video system
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("NES Emulator", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let event_pump = sdl_context.event_pump().unwrap();
    canvas.set_scale(3.0, 3.0).unwrap();

    //Audio system
    let audio_subsystem = sdl_context.audio().unwrap();
    let desired_spec = AudioSpecDesired {
        freq: Some(44_100),
        channels: Some(2),
        samples: Some(1024),
    };
    let audio_device = audio_subsystem.open_queue::<f32,_>(None, &desired_spec).unwrap();
    audio_device.resume();
    //audio_device.queue_audio(&samples);

    (canvas, event_pump, audio_device)
}

fn main() {
    let (mut canvas, mut event_pump, audio_device) = setup_sld2();
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
    let bytes: Vec<u8> = std::fs::read("roms/super mario bros.nes").unwrap();
    let rom = Rom::new(&bytes).unwrap();

    let bus = Bus::new(
        rom,
        move |frame: &mut Frame, joypad: &mut joypad::Joypad, samples: &mut Vec<f32>| {
            // Display new frame
            texture.update(None, &frame.data, 256 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();

            //println!("{}", samples.len());
            let mut filtered = highpass_filter(samples, 0.996039);
            let mut filtered = highpass_filter(&mut filtered, 0.999835);
            let filtered = lowpass_filter(&mut filtered);
           
            let decimated_samples: Vec<f32> = filtered.iter()
                .enumerate()
                .filter(|&(i, _)| i % 19 == 0 )
                .map(|(_, &s)| s)
                .collect();
            audio_device.queue_audio(&decimated_samples).unwrap();
            samples.clear();

            // Get player's input
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
