// Skeleton code for your Rust projects
// I added several comments and annotations to this file.
// _Please_ read them carefully. They are very important.
// The most important comments are all annotated with "NOTE/WARNING:"

// I will grade your code quality primarily on how "idiomatic" your Rust
// code is, and how well you implemented the "safe unsafety" guidelines.

extern crate libc;
extern crate time;
extern crate ctrlc;
#[macro_use]
extern crate simple_error;
extern crate shuteye;
extern crate mmap;
extern crate nix;
extern crate gif;

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::{fs::OpenOptions, os::unix::fs::OpenOptionsExt, thread, cmp};
use std::error::Error;
use std::os::unix::io::AsRawFd;
use std::path::Path;
use std::io::prelude::*;
use std::fs::{File, read};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use shuteye::sleep;
use mmap::{MemoryMap, MapOption};
use std::io::Cursor;
use std::sync::mpsc::RecvTimeoutError::Timeout;
use std::ptr::null;
use time::Timespec;
use std::panic::resume_unwind;
use std::ffi::OsStr;
use gif::SetParameter;

#[derive(Copy, Clone)]
pub struct Pixel {
    r: u16,
    g: u16,
    b: u16,
}

pub struct Gif {
    delay: usize,
    images: Vec<Image>,
}

pub struct GPIO {
    gpio_map_: Option<MemoryMap>,
    output_bits_: u32,
    input_bits_: u32,
    slowdown_: u32,
    // Please refer to the GPIO_SetBits and GPIO_ClearBits functions in the reference implementation to see how this is used.
    gpio_port_: *mut u32,
    // A raw pointer that points to the base of the GPIO register file
    gpio_set_bits_: *mut u32,
    // A raw pointer that points to the pin output register (see section 2.1 in the assignment)
    gpio_clr_bits_: *mut u32,
    // A raw pointer that points to the pin output clear register (see section 2.1)
    gpio_read_bits_: *mut u32,
    // A raw pointer that points to the pin level register (see section 2.1)
    row_mask: u32,
    bitplane_timings: [u32; COLOR_DEPTH],
}

// This is a representation of the "raw" image
pub struct Image {
    width: usize,
    height: usize,
    pixels: Vec<Vec<Pixel>>,
}

// This is a representation of the frame we're currently rendering
pub struct Frame {
    pos: usize,
    pixels: Vec<Vec<Pixel>>,
}


// ============================================================================
// GPIO configuration parameters for the raspberry pi 3
// ============================================================================

const BCM2709_PERI_BASE: u64 = 0x3F000000;
const GPIO_REGISTER_OFFSET: u64 = 0x200000;
const TIMER_REGISTER_OFFSET: u64 = 0x3000;
const REGISTER_BLOCK_SIZE: u64 = 4096;
const COLOR_DEPTH: usize = 8;

const PIN_OE: u64 = 4;
const PIN_CLK: u64 = 17;
const PIN_LAT: u64 = 21;
const PIN_A: u64 = 22;
const PIN_B: u64 = 26;
const PIN_C: u64 = 27;
const PIN_D: u64 = 20;
const PIN_E: u64 = 24;
const PIN_R1: u64 = 5;
const PIN_G1: u64 = 13;
const PIN_B1: u64 = 6;
const PIN_R2: u64 = 12;
const PIN_G2: u64 = 16;
const PIN_B2: u64 = 23;

// Convenience macro for creating bitmasks. See comment above "impl GPIO" below
macro_rules! GPIO_BIT {
    ($bit:expr) => {
        1 << $bit
    };
}
macro_rules! current_time_micros {
    ()=>{SystemTime::now().duration_since(UNIX_EPOCH).expect("Time went backwards").as_micros()};
}

// Use this bitmask for sanity checks
const VALID_BITS: u64 = GPIO_BIT!(PIN_OE) | GPIO_BIT!(PIN_CLK) | GPIO_BIT!(PIN_LAT) |
    GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_C) | GPIO_BIT!(PIN_D) | GPIO_BIT!(PIN_E) |
    GPIO_BIT!(PIN_R1) | GPIO_BIT!(PIN_G1) | GPIO_BIT!(PIN_B1) |
    GPIO_BIT!(PIN_R2) | GPIO_BIT!(PIN_G2) | GPIO_BIT!(PIN_B2);

// ============================================================================
// mmap_bcm_register - convenience function used to map the GPIO register block
// ============================================================================

fn mmap_bcm_register(register_offset: usize) -> Option<MemoryMap> {
    let mem_file =
        match OpenOptions::new()
            .read(true)
            .write(true)
            .custom_flags(libc::O_SYNC)
            .open("/dev/mem") {
            Err(why) => panic!("couldn't open /dev/mem: {}", why.description()),
            Ok(file) => file
        };

    let mmap_options = &[
        MapOption::MapNonStandardFlags(libc::MAP_SHARED),
        MapOption::MapReadable,
        MapOption::MapWritable,
        MapOption::MapFd(mem_file.as_raw_fd()),
        MapOption::MapOffset(BCM2709_PERI_BASE as usize + register_offset as usize)
    ];

    let result = MemoryMap::new(REGISTER_BLOCK_SIZE as usize, mmap_options).unwrap();

    return match result.data().is_null() {
        true => {
            eprintln!("mmap error: {}", std::io::Error::last_os_error());
            eprintln!("Pi3: MMapping from base 0x{:X}, offset 0x{:X}", BCM2709_PERI_BASE, register_offset);
            None
        }
        false => Some(result)
    };

    // NOTE/WARNING: When a MemoryMap struct is dropped, the mapped
    // memory region is automatically unmapped!
}

//
// NOTE/WARNING: In many cases, particularly those where you need to set or clear
// multiple bits at once, it is convenient to store multiple pin numbers in one bit
// mask value. If you want to simultaneously set PIN_A and PIN_C to high, for example,
// you should probably create a bit mask with the positions of PIN_A and PIN_C set to 1,
// and all other positions set to 0. You can do this using the GPIO_BIT! macro.
//
// In this example, you would do something like:
//     let pin_mask = GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_C);
//     io.set_bits(pin_mask);
//
impl GPIO {
    //
    // configures pin number @pin_num as an output pin by writing to the
    // appropriate Function Select register (see section 2.1).
    //
    // NOTE/WARNING: This method configures one pin at a time. The @pin_num argument
    // that is expected here is really a pin number and not a bitmask!
    //
    // Doing something like:
    //     io.configure_output_pin(VALID_BITS);
    // Would be WRONG! This call would make the program crash.
    //
    // Doing something like:
    //     if GPIO_BIT!(PIN_A) & VALID_BITS {
    //         io.configure_output_pin(PIN_A);
    //     }
    // Would be OK!
    //
    fn configure_output_pin(self: &mut GPIO, pin_num: u64) {
        let register_num = (pin_num / 10) as isize;
        let register_ref = unsafe { self.gpio_port_.offset(register_num) };
        // NOTE/WARNING: When reading from or writing to MMIO memory regions, you MUST
        // use the std::ptr::read_volatile and std::ptr::write_volatile functions
        let current_val = unsafe { std::ptr::read_volatile(register_ref) };
        println!("{:#034b},{}", current_val, pin_num);
        // the bit range within the register is [(pin_num % 10) * 3 .. (pin_num % 10) * 3 + 2]
        // we need to set these bits to 001
        println!("{:#034b}", !(7 << ((pin_num % 10) * 3)));
        println!("{:#034b}", (1 << ((pin_num % 10) * 3)));
        println!("{:#034b}", ((current_val & !(7 << ((pin_num % 10) * 3)) as u32)));
        println!("{:#034b}", ((current_val & !(7 << ((pin_num % 10) * 3)) as u32) | (1 << ((pin_num % 10) * 3)) as u32));
        let new_val = (current_val & !(7 << ((pin_num % 10) * 3))) | (1 << ((pin_num % 10) * 3));
        println!("{},{}", new_val, pin_num);

        // NOTE/WARNING: When reading from or writing to MMIO memory regions, you MUST
        // use the std::ptr::read_volatile and std::ptr::write_volatile functions
        unsafe { std::ptr::write_volatile(register_ref, new_val) };
    }


    fn activate_pins(self: &mut GPIO, bitmask: &u32) {
        let mut pin_output_set = self.gpio_set_bits_;

        unsafe { *pin_output_set = *bitmask; }
    }
    fn clear_pins(self: &mut GPIO, bitmask: &u32) {
        let mut pin_output_clear = self.gpio_clr_bits_;

        unsafe { *pin_output_clear = *bitmask; }
    }
    fn clear_all_pins(self: &mut GPIO) {
        let mut pin_output_clear = self.gpio_clr_bits_;

        unsafe { *pin_output_clear = ((GPIO_BIT!(PIN_R1) | GPIO_BIT!(PIN_R2) | GPIO_BIT!(PIN_B1) | GPIO_BIT!(PIN_B2) | GPIO_BIT!(PIN_G1) | GPIO_BIT!(PIN_G2)) as u32); }
    }
    fn clear_all_pins_and_activate(self: &mut GPIO, bitmask: &u32) {
        self.clear_all_pins();
        let mut pin_output_set = self.gpio_set_bits_;
        unsafe { *pin_output_set = *bitmask }
    }
    fn shutdown(self: &mut GPIO) {
        self.clear_all_pins();
        self.activate_pins(&mut (GPIO_BIT!(PIN_LAT) as u32));

        self.clear_pins(&mut (GPIO_BIT!(PIN_LAT) as u32));


        self.activate_pins(&mut ((GPIO_BIT!(PIN_OE)) as u32));
    }

    fn show_image(self: &mut GPIO, image: &Image) {
        for i in 0..image.width {
            let mut lasttime = current_time_micros!();
            let mut timerinterval = 1;
            let mut framenumber = 0;
            while current_time_micros!() < (lasttime + 64000) {
                framenumber = framenumber % 8;
                timerinterval = (1 * (2 ^ framenumber)) as u128;

                let mut lastframetime = current_time_micros!();
                while current_time_micros!() < (lastframetime + timerinterval) {
                    for x in 0usize..8 {
                        let rowMask = match x {
                            1 => GPIO_BIT!(PIN_A),
                            2 => GPIO_BIT!(PIN_B),
                            3 => GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_B),
                            4 => GPIO_BIT!(PIN_C),
                            5 => GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_C),
                            6 => GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_C),
                            7 => GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_C),
                            _ => 0,
                        };
                        self.set_bits(rowMask as u32, image, x, i, framenumber)
                    }
                }
                framenumber += 1;
            }
        }
    }

    fn show_gif(self: &mut GPIO, gif: &Gif) {
        for img in 0..gif.images.len() {
            let mut lasttime = current_time_micros!();
            let mut timerinterval = 1;
            let mut framenumber = 0;
            while current_time_micros!() < (lasttime + gif.delay as u128) {
                framenumber = framenumber % 8;
                timerinterval = (1 * (2 ^ framenumber)) as u128;

                let mut lastframetime = current_time_micros!();
                while current_time_micros!() < (lastframetime + timerinterval) {
                    for x in 0usize..8 {
                        let rowMask = match x {
                            1 => GPIO_BIT!(PIN_A),
                            2 => GPIO_BIT!(PIN_B),
                            3 => GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_B),
                            4 => GPIO_BIT!(PIN_C),
                            5 => GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_C),
                            6 => GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_C),
                            7 => GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_C),
                            _ => 0,
                        };
                        self.set_bits(rowMask as u32, &gif.images[img], x, 0, framenumber)
                    }
                }
                framenumber += 1;
            }
        }
    }


    fn set_bits(self: &mut GPIO, rowMask: u32, image: &Image, rowNumber: usize, start: usize, framenumber: u16) {
        // self.clearAllPins();
        self.clear_pins(&mut (GPIO_BIT!(PIN_OE) as u32));
        let framemask: u16 = 1 << framenumber;
        for c in (start)..(32 + start) {
            self.clear_all_pins();
            let rgbmask1: u32 = (if (image.pixels[rowNumber][c % image.width].r & framemask) != 0 { GPIO_BIT!({PIN_R1}) } else { 0 } | if (image.pixels[rowNumber][c % image.width].g & framemask) != 0 { GPIO_BIT!({PIN_G1}) } else { 0 } | if (image.pixels[rowNumber][c % image.width].b & framemask) != 0 { GPIO_BIT!({PIN_B1}) } else { 0 }) as u32;
            let rgbmask2: u32 = (if (image.pixels[rowNumber + 8][c % image.width].r & framemask) != 0 { GPIO_BIT!({PIN_R2}) } else { 0 } | if (image.pixels[rowNumber + 8][c % image.width].g & framemask) != 0 { GPIO_BIT!({PIN_G2}) } else { 0 } | if (image.pixels[rowNumber + 8][c % image.width].b & framemask) != 0 { GPIO_BIT!({PIN_B2}) } else { 0 }) as u32;
            let rgbmask: u32 = rgbmask1 | rgbmask2;
            self.activate_pins(&rgbmask);

            self.activate_pins(&mut (GPIO_BIT!(PIN_CLK) as u32));
            self.clear_pins(&mut (GPIO_BIT!(PIN_CLK) as u32));
        }


        self.clear_pins(&mut ((GPIO_BIT!(PIN_R1) | GPIO_BIT!(PIN_R2) | GPIO_BIT!(PIN_B1) | GPIO_BIT!(PIN_B2) | GPIO_BIT!(PIN_G1) | GPIO_BIT!(PIN_G2)) as u32));
        self.clear_pins(&mut ((GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_C) | GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_D) | GPIO_BIT!(PIN_E)) as u32));

        self.activate_pins((&(rowMask)));
//        thread::sleep(Duration::new(0, 1000000 * 500));


        self.activate_pins(&mut (GPIO_BIT!(PIN_LAT) as u32));

        self.clear_pins(&mut (GPIO_BIT!(PIN_LAT) as u32));


        self.clear_pins(&mut (GPIO_BIT!(PIN_OE) as u32));
        self.activate_pins(&mut (GPIO_BIT!(PIN_OE) as u32));
    }


    fn new(slowdown: u32) -> GPIO {

        // Map the GPIO register file. See section 2.1 in the assignment for details
        let map = mmap_bcm_register((GPIO_REGISTER_OFFSET) as usize);

        // Initialize the GPIO struct with default values
        let mut io: GPIO = GPIO {
            gpio_map_: None,
            output_bits_: 0,
            input_bits_: 0,
            slowdown_: slowdown,
            gpio_port_: 0 as *mut u32,
            gpio_set_bits_: 0 as *mut u32,
            gpio_clr_bits_: 0 as *mut u32,
            gpio_read_bits_: 0 as *mut u32,
            row_mask: 0,
            bitplane_timings: [0; COLOR_DEPTH],
        };

        match &map {
            Some(m) => {
                unsafe {
                    io.gpio_port_ = m.data() as *mut u32;
                    io.gpio_set_bits_ = (io.gpio_port_.offset(7));
                    io.gpio_clr_bits_ = (io.gpio_port_.offset(10));
                    io.gpio_read_bits_ = (io.gpio_port_.offset(13));
                }
            }
            None => {}
        }

        io.gpio_map_ = map;
        io
    }

    fn get_row_bits(self: &GPIO, double_row: u8) -> u32 {
        0
    }
}


pub fn read_bmp() -> Result<Image, std::io::Error> { // {
    let args: Vec<String> = std::env::args().collect();

    let img = bmp::open(&args[1]).unwrap_or_else(|e| {
        panic!("Failed to open: {}", e);
    });
    let mut image = Image {
        width: img.get_width() as usize,
        height: img.get_height() as usize,
        pixels: vec![],
    };
    image.pixels = vec![vec![Pixel { r: 0, g: 0, b: 0 }; image.width as usize]; image.height as usize];
    for (i, j) in img.coordinates() {
        image.pixels[j as usize][i as usize] = Pixel { r: img.get_pixel(i as u32, j as u32).r as u16, g: img.get_pixel(i as u32, j as u32).g as u16, b: img.get_pixel(i as u32, j as u32).b as u16 }
    }


    Result::Ok(image)
}


fn decode_ppm_image(cursor: &mut Cursor<Vec<u8>>) -> Result<Image, std::io::Error> {
    let mut image = Image {
        width: 0,
        height: 0,
        pixels: vec![],
    };

    let mut c: [u8; 1] = [0; 1];
    let mut array: [String; 4] = Default::default();

    let mut count = 0;
    let mut comment = false;

    loop {
        cursor.read(&mut c)?;
        match &c {
            b"\n" => {
                comment = false;
                break;
            }
            b"#" => comment = true,
            b" " => if !comment { count += 1 },
            _ => {
                if !comment {
                    array[count].push(char::from(c[0]));
                    continue;
                }
            }
        }
    }
    println!("{:?}", array);
    assert_eq!(array[0], "P6");
    assert_eq!(array[3], "255");

    image.width = array[1].parse::<u32>().unwrap() as usize;
    image.height = array[2].parse::<u32>().unwrap() as usize;
    image.pixels = vec![vec![Pixel { r: 0, g: 0, b: 0 }; image.width as usize]; image.height as usize];
    let mut count = 0;
    let mut pixelcounter = 0;
    let mut pixel_array: [u8; 3] = Default::default();
    comment = false;

    loop {
        cursor.read(&mut c)?;
        match &c {
            b"\n" => {
                comment = false;
            }

            b"#" => {
                comment = true;
                continue;
            }
            _ => {
                if !comment {
                    pixel_array[pixelcounter] = c[0];
                    pixelcounter = (pixelcounter + 1) % 3;
                    if pixelcounter == 0 {
                        image.pixels[(count / image.width) as usize][(count % image.width) as usize] = Pixel {
                            r: pixel_array[0] as u16,
                            g: pixel_array[1] as u16,
                            b: pixel_array[2] as u16,
                        };

                        count += 1;
                        if count == image.height * image.width { break; }
                    }
                }
                continue;
            }
        }
    }

    Ok(image)
}

pub fn read_gif() -> Result<Gif, std::io::Error> {
    use std::fs::File;
    use gif::SetParameter;
    let args: Vec<String> = std::env::args().collect();

    let mut decoder = gif::Decoder::new(File::open(&args[1]).unwrap());
    decoder.set(gif::ColorOutput::RGBA);
    let mut decoder = decoder.read_info().unwrap();
    let mut counter = 0;
    let mut gif = Gif { delay: 0, images: vec![] };
    while let Some(frame) = decoder.read_next_frame().unwrap() {
        println!("{}", counter);
        counter = counter + 1;
        gif.delay = 10000 * frame.delay as usize;
        let cow = &frame.buffer;

        let mut image = Image {
            width: frame.width as usize,
            height: frame.height as usize,
            pixels: vec![],
        };
        image.pixels = vec![vec![Pixel { r: 0, g: 0, b: 0 }; image.width as usize]; image.height as usize];
        for h in 0..(frame.height as usize) {
            for w in 0..(frame.width as usize) {
                image.pixels[h][w] = Pixel {
                    r: *match cow.get((h * frame.width as usize + w) * 4 + 0) {
                        Some(x) => x,
                        None => &(0 as u8)
                    } as u16,
                    g: *match cow.get((h * frame.width as usize + w) * 4 + 1) {
                        Some(x) => x,
                        None => &(0 as u8)
                    } as u16,
                    b: *match cow.get((h * frame.width as usize + w) * 4 + 2) {
                        Some(x) => x,
                        None => &(0 as u8)
                    } as u16,
                };
            }
        }
        gif.images.push(image);
    }
    Ok(gif)
}

pub fn read_ppm() -> Result<Image, std::io::Error> {
    let args: Vec<String> = std::env::args().collect();


    let path = Path::new(&args[1]);
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("Could not open file: {} (Reason: {})",
                           display, why),
        Ok(file) => file
    };

    let mut raw_file = Vec::new();
    file.read_to_end(&mut raw_file).unwrap();

    let mut cursor = Cursor::new(raw_file);
    let image = match decode_ppm_image(&mut cursor) {
        Ok(img) => img,
        Err(why) => panic!("Could not parse PPM file - Desc: {}", why),
    };
    Ok(image)
}

impl Image {
    fn rescale(self: &Image) -> Image {
        if self.height > 16 {
            let mut rescaled_image = Image {
                width: self.width * 16 / self.height,
                height: 16,
                pixels: vec![],
            };
            rescaled_image.pixels = vec![vec![Pixel { r: 0, g: 0, b: 0 }; rescaled_image.width as usize]; rescaled_image.height as usize];
            let pixels = (self.height / 16);
            let extra_height = (self.height % 16);
            let mut count_height = 0;

            let width_interval = self.height / 16;
            let extra_width = (self.height % 16);

            let mut count_width = 0;
            let mut extra_uses_width = 0;
            for y in 0..rescaled_image.width {
                count_height = 0;

                let extra_width_single = if count_width < (extra_width) / 2 {
                    1
                } else { if count_width > (rescaled_image.width - ((extra_width) / 2)) { 1 } else { 0 } };

                for x in 0..rescaled_image.height {
                    let extra_height_single = if count_height < extra_height { 1 } else { 0 };
                    let mut r: u64 = 0;
                    let mut g: u64 = 0;
                    let mut b: u64 = 0;
                    let mut total_number_of_pixels = 0;

                    for i in 0..(width_interval + extra_width_single) {
                        for j in 0..(pixels + extra_height_single) {
                            total_number_of_pixels += 1;

                            r += self.pixels[x * pixels + j + cmp::min(count_height, extra_height)][y * width_interval + i + cmp::min(extra_uses_width as usize, extra_width as usize)].r as u64;
                            g += self.pixels[x * pixels + j + cmp::min(count_height, extra_height)][y * width_interval + i + cmp::min(extra_uses_width as usize, extra_width as usize)].g as u64;
                            b += self.pixels[x * pixels + j + cmp::min(count_height, extra_height)][y * width_interval + i + cmp::min(extra_uses_width as usize, extra_width as usize)].b as u64;
                        }
                    }


                    r = r / total_number_of_pixels;
                    g = g / total_number_of_pixels;
                    b = b / total_number_of_pixels;

                    rescaled_image.pixels[x][y] = Pixel {
                        r: r as u16,
                        g: g as u16,
                        b: b as u16,
                    };
                    count_height += 1;
                }
                extra_uses_width += extra_width_single;
                count_width += 1
            }

            rescaled_image
        } else {
            let mut img = Image { width: self.width, height: self.height, pixels: vec![] };
            img.pixels = vec![vec![Pixel { r: 0, g: 0, b: 0 }; self.width as usize]; self.height as usize];
            for y in 0usize..img.width as usize {
                for x in 0usize..img.height as usize {
                    img.pixels[x][y] = Pixel { r: self.pixels[x][y].r, g: self.pixels[x][y].g, b: self.pixels[x][y].b };
                }
            }
            img
        }
    }
}

fn get_extension_from_filename(filename: &str) -> Option<&str> {
    Path::new(filename)
        .extension()
        .and_then(OsStr::to_str)
}

pub fn main() {
    let args: Vec<String> = std::env::args().collect();
    let interrupt_received = Arc::new(AtomicBool::new(false));

    if nix::unistd::Uid::current().is_root() == false {
        eprintln!("Must run as root to be able to access /dev/mem\nPrepend \'sudo\' to the command");
        std::process::exit(1);
    } else if args.len() < 2 {
        eprintln!("Syntax: {:?} [image]", args[0]);
        std::process::exit(1);
    }


    let args: Vec<String> = std::env::args().collect();
    if get_extension_from_filename(&args[1]) == Some("gif") {
        let mut gif = match read_gif() {
            Ok(img) => img,
            Err(why) => panic!("Could not parse gif file - Desc: {}", why),
        };
        for x in 0..gif.images.len() {
            gif.images[x] = gif.images[x].rescale();
        }


        let mut GPIO = GPIO::new(500);


        let int_recv = interrupt_received.clone();
        ctrlc::set_handler(move || {
            int_recv.store(true, Ordering::SeqCst);
        }).unwrap();
        while interrupt_received.load(Ordering::SeqCst) == false {
            GPIO.show_gif(&gif);
        }
        println!("Exiting.");
        if interrupt_received.load(Ordering::SeqCst) == true {
            GPIO.shutdown();
            println!("Received CTRL-C");
        } else {
            println!("Timeout reached");
        }
    } else {
        let image = if get_extension_from_filename(&args[1]) == Some("bmp") { read_bmp().unwrap() } else if get_extension_from_filename(&args[1]) == Some("ppm") {
            match read_ppm() {
                Ok(img) => img,
                Err(why) => panic!("Could not parse PPM file - Desc: {}", why),
            }
        } else { panic!("wrong file extention") };
        let rescaled_image = image.rescale();

        let mut GPIO = GPIO::new(500);


        let int_recv = interrupt_received.clone();
        ctrlc::set_handler(move || {
            int_recv.store(true, Ordering::SeqCst);
        }).unwrap();
        while interrupt_received.load(Ordering::SeqCst) == false {
            GPIO.show_image(&rescaled_image);
        }
        println!("Exiting.");
        if interrupt_received.load(Ordering::SeqCst) == true {
            GPIO.shutdown();
            println!("Received CTRL-C");
        } else {
            println!("Timeout reached");
        }
    }
}

