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

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::{fs::OpenOptions, os::unix::fs::OpenOptionsExt, thread, cmp};
use std::error::Error;
use std::os::unix::io::AsRawFd;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use shuteye::sleep;
use mmap::{MemoryMap, MapOption};
use std::io::Cursor;
use std::sync::mpsc::RecvTimeoutError::Timeout;
use std::ptr::null;
use time::Timespec;
use std::panic::resume_unwind;

#[derive(Copy, Clone)]
pub struct Pixel {
    r: u16,
    g: u16,
    b: u16,
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

// Use this struct to implement high-precision nanosleeps
struct Timer {
    _timemap: Option<MemoryMap>,
    timereg: *mut u32, // a raw pointer to the 1Mhz timer register (see section 2.5 in the assignment)
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
macro_rules! currenttimemicros {
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

    fn init_outputs(self: &mut GPIO, mut outputs: u32) -> u32 {
        self.configure_output_pin(PIN_OE);
        0
        // TODO: Implement this yourself. Note: this function expects          a bitmask as the @outputs argument
    }

    fn activatePins(self: &mut GPIO, bitmask: &u32) {
        let mut pinOutputSet = self.gpio_set_bits_;

        unsafe { *pinOutputSet = *bitmask; }
    }
    fn clearPins(self: &mut GPIO, bitmask: &u32) {
        let mut pinOutputClear = self.gpio_clr_bits_;

        unsafe { *pinOutputClear = *bitmask; }
    }
    fn clearAllPins(self: &mut GPIO) {
        let mut pinOutputClear = self.gpio_clr_bits_;

        unsafe { *pinOutputClear = ((GPIO_BIT!(PIN_R1) | GPIO_BIT!(PIN_R2) | GPIO_BIT!(PIN_B1) | GPIO_BIT!(PIN_B2) | GPIO_BIT!(PIN_G1) | GPIO_BIT!(PIN_G2)) as u32); }
    }
    fn clearAllPinsAndActivate(self: &mut GPIO, bitmask: &u32) {
        self.clearAllPins();
        let mut pinOutputSet = self.gpio_set_bits_;
        unsafe { *pinOutputSet = *bitmask }
        // println!("{:#034b},set", unsafe { *pinOutputSet });
        // println!("{:#034b},clear", unsafe { *pinOutputClear });
        // println!("{:?},adr",self.gpio_read_bits_);
    }
    fn shutdown(self: &mut GPIO) {
        self.clearAllPins();
        self.activatePins(&mut (GPIO_BIT!(PIN_LAT) as u32));

        self.clearPins(&mut (GPIO_BIT!(PIN_LAT) as u32));


        self.activatePins(&mut ((GPIO_BIT!(PIN_OE)) as u32));
    }
    fn showImage(self: &mut GPIO, image: &Image) {
        for i in 0..image.width {
            let mut lasttime = currenttimemicros!();
            let mut timerinterval =1000;
            let mut framenumber = 0;
            while currenttimemicros!() < (lasttime + 128000) {
                framenumber = framenumber % 8;
                timerinterval = (1000 * (2 ^ framenumber)) as u128;

                let mut lastframetime = currenttimemicros!();
                while currenttimemicros!() < (lastframetime + timerinterval) {
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
                    framenumber += 1;
                }
            }
        }
    }


    fn set_bits(self: &mut GPIO, rowMask: u32, image: &Image, rowNumber: usize, start: usize, framenumber: u16) {
        // self.clearAllPins();
        self.clearPins(&mut (GPIO_BIT!(PIN_OE) as u32));
        let framemask: u16 = 1 << framenumber;
        for c in (start)..(32 + start) {
            self.clearAllPins();
            let rgbmask1: u32 = (if (image.pixels[rowNumber][c % image.width].r & framemask) != 0 { GPIO_BIT!({PIN_R1}) } else { 0 } | if (image.pixels[rowNumber][c % image.width].g & framemask) != 0 { GPIO_BIT!({PIN_G1}) } else { 0 } | if (image.pixels[rowNumber][c % image.width].b & framemask) != 0 { GPIO_BIT!({PIN_B1}) } else { 0 }) as u32;
            let rgbmask2: u32 = (if (image.pixels[rowNumber + 8][c % image.width].r & framemask) != 0 { GPIO_BIT!({PIN_R2}) } else { 0 } | if (image.pixels[rowNumber + 8][c % image.width].g & framemask) != 0 { GPIO_BIT!({PIN_G2}) } else { 0 } | if (image.pixels[rowNumber + 8][c % image.width].b & framemask) != 0 { GPIO_BIT!({PIN_B2}) } else { 0 }) as u32;
            let rgbmask: u32 = rgbmask1 | rgbmask2;
            self.activatePins(&rgbmask);

            self.activatePins(&mut (GPIO_BIT!(PIN_CLK) as u32));
            self.clearPins(&mut (GPIO_BIT!(PIN_CLK) as u32));
        }


        self.clearPins(&mut ((GPIO_BIT!(PIN_R1) | GPIO_BIT!(PIN_R2) | GPIO_BIT!(PIN_B1) | GPIO_BIT!(PIN_B2) | GPIO_BIT!(PIN_G1) | GPIO_BIT!(PIN_G2)) as u32));
        self.clearPins(&mut ((GPIO_BIT!(PIN_A) | GPIO_BIT!(PIN_C) | GPIO_BIT!(PIN_B) | GPIO_BIT!(PIN_D) | GPIO_BIT!(PIN_E)) as u32));

        self.activatePins((&(rowMask)));
//        thread::sleep(Duration::new(0, 1000000 * 500));


        self.activatePins(&mut (GPIO_BIT!(PIN_LAT) as u32));

        self.clearPins(&mut (GPIO_BIT!(PIN_LAT) as u32));


        self.clearPins(&mut (GPIO_BIT!(PIN_OE) as u32));
        self.activatePins(&mut (GPIO_BIT!(PIN_OE) as u32));

        // println!("{:#034b},read oe", unsafe { *self.gpio_read_bits_ });
        // thread::sleep(Duration::new(0, 1000000 * 10));
        //     println!("{:#034b},read oe", unsafe { *self.gpio_read_bits_ });

        /*self.configure_output_pin(PIN_OE);
        self.configure_output_pin(PIN_C);
        self.configure_output_pin(PIN_R1);
        self.configure_output_pin(PIN_R2);
        self.configure_output_pin(PIN_B1);
        self.configure_output_pin(PIN_B2);
        self.configure_output_pin(PIN_G1);


        self.configure_output_pin(PIN_LAT);
        self.configure_output_pin(PIN_LAT);
        self.configure_output_pin(PIN_CLK);
        self.configure_output_pin(PIN_CLK);*/

        // TODO: Implement this yourself. Remember to take the slowdown_ value into account!
        // This function expects a bitmask as the @value argument
    }

    fn clear_bits(self: &mut GPIO, value: u32) {
        // TODO: Implement this yourself. Remember to take the slowdown_ value into account!
        // This function expects a bitmask as the @value argument
    }

    // Write all the bits of @value that also appear in @mask. Leave the rest untouched.
// @value and @mask are bitmasks
    fn write_masked_bits(
        self: &mut GPIO,
        value: u32,
        mask: u32,
    ) {
        // TODO: Implement this yourself.
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
                    println!("{:?}", io.gpio_port_);
                    println!("{:?}", io.gpio_set_bits_);
                    println!("{:?}", io.gpio_clr_bits_);
                    println!("{:?}", io.gpio_read_bits_);

                    // TODO: Calculate the correct values of the other raw pointers here.
                    // You should use the offset() method on the gpio_port_ pointer.
                    // Keep in mind that Rust raw pointer arithmetic works exactly like
                    // C pointer arithmetic. See the course slides for details
                }

                // TODO: Implement this yourself.
            }
            None => {}
        }

        io.gpio_map_ = map;
        io
    }

    // Calculates the pins we must activate to push the address of the specified double_row
    fn get_row_bits(self: &GPIO, double_row: u8) -> u32 {
        0
        // TODO: Implement this yourself.
    }
}

impl Timer {
    // Reads from the 1Mhz timer register (see Section 2.5 in the assignment)
    unsafe fn read(self: &Timer) -> u32 {
        *self.timereg
    }

    fn new() -> Timer {
        let mut var = (BCM2709_PERI_BASE + TIMER_REGISTER_OFFSET + 0x4) as *mut u32;
        Timer { _timemap: mmap_bcm_register((GPIO_REGISTER_OFFSET + TIMER_REGISTER_OFFSET) as usize), timereg: var }
// TODO: timemap?.
    }

    // High-precision sleep function (see section 2.5 in the assignment)
// NOTE/WARNING: Since the raspberry pi's timer frequency is only 1Mhz,
// you cannot reach full nanosecond precision here. You will have to think
// about how you can approximate the desired precision. Obviously, there is
// no perfect solution here.
    fn nanosleep(self: &Timer, mut nanos: u32) {
//todo sleep
// libc::nanosleep(&Timespec::new(0, nanos as i32),&Timespec::new(0, nanos as i32));
    }
}

// TODO: Implement your frame calculation/updating logic here.
// The Frame should contain the pixels that are currently shown
// on the LED board. In most cases, the Frame will have less pixels
// than the input Image!
impl Frame {}

//First implementation of BMP parser
pub fn read_bmp() { //-> Result<Image, std::io::Error> {
    let img = bmp::open("dog.bmp").unwrap_or_else(|e| {
        panic!("Failed to open: {}", e);
    });

    for (x, y) in img.coordinates() {
        let px = img.get_pixel(x, y);
        println!("X: {}, y: {} \n", x, y);
        println!("R: {}, G: {}, B: {} \n", px.r, px.g, px.b);
    };
}


// TODO: Add your PPM parser here
// NOTE/WARNING: Please make sure that your implementation can handle comments in the PPM file
// You do not need to add support for any formats other than P6
// You may assume that the max_color value is always 255, but you should add sanity checks
// to safely reject files with other max_color values

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

    /*   for _ in 0..image.height {
           let mut row = Vec::new();
           for _ in 0..image.width {
               row.push(Pixel { r: cursor.read_u8()?, g: cursor.read_u8()?, b: cursor.read_u8()? })
           }
           image.pixels.push(row);
       }*/
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

pub fn read_ppm() -> Result<Image, std::io::Error> {
    let args: Vec<String> = std::env::args().collect();


    let path = Path::new(&args[1]);
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("Could not open file: {} (Reason: {})",
                           display, why),
        Ok(file) => file
    };

// read the full file into memory. panic on failure
    let mut raw_file = Vec::new();
    file.read_to_end(&mut raw_file).unwrap();

// construct a cursor so we can seek in the raw buffer
    let mut cursor = Cursor::new(raw_file);
    let image = match decode_ppm_image(&mut cursor) {
        Ok(img) => img,
        Err(why) => panic!("Could not parse PPM file - Desc: {}", why),
    };
    Ok(image)
}

impl Image {
    fn rescale(self: &Image) -> Image {
        let mut rescaledImage = Image {
            width: self.width * 16 / self.height,
            height: 16,
            pixels: vec![],
        };
        rescaledImage.pixels = vec![vec![Pixel { r: 0, g: 0, b: 0 }; rescaledImage.width as usize]; rescaledImage.height as usize];
        let pixels = (self.height / 16);
        let extrapixels = (self.height % 16);
        let mut count = 0;

//self.width/(resised.width)
        let widthInterval = self.height / 16;

        for y in 0..rescaledImage.width {
            for x in 0..rescaledImage.height {
                let extra = if count < extrapixels { 1 } else { 0 };
                let mut r: u64 = 0;
                let mut g: u64 = 0;
                let mut b: u64 = 0;
                let mut totalNumberOfPixels = 0;
                for i in 0..widthInterval {
                    for j in 0..(pixels + extra) {
                        totalNumberOfPixels += 1;
                        r += self.pixels[x * pixels + j + cmp::min(count, extrapixels)][y * widthInterval + i].r as u64;
                        g += self.pixels[x * pixels + j + cmp::min(count, extrapixels)][y * widthInterval + i].g as u64;
                        b += self.pixels[x * pixels + j + cmp::min(count, extrapixels)][y * widthInterval + i].b as u64;
                    }
                }
                r = r / totalNumberOfPixels;
                g = g / totalNumberOfPixels;
                b = b / totalNumberOfPixels;

                rescaledImage.pixels[x][y] = Pixel {
                    r: r as u16,
                    g: g as u16,
                    b: b as u16,
                };
                count += 1;
            }
        }

        rescaledImage
    }
}

pub fn main() {
    let args: Vec<String> = std::env::args().collect();
    let interrupt_received = Arc::new(AtomicBool::new(false));

// sanity checks
    if nix::unistd::Uid::current().is_root() == false {
        eprintln!("Must run as root to be able to access /dev/mem\nPrepend \'sudo\' to the command");
        std::process::exit(1);
    } else if args.len() < 2 {
        eprintln!("Syntax: {:?} [image]", args[0]);
        std::process::exit(1);
    }

// TODO: Read the PPM file here. You can find its name in args[1]
    let image = match read_ppm() {
        Ok(img) => img,
        Err(why) => panic!("Could not parse PPM file - Desc: {}", why),
    };
    let rescaledImage = image.rescale();
    println!("{}", image.height);
    println!("{}", image.pixels.len());
    println!("{}", unsafe { image.pixels.get_unchecked(0) }.len());//todo fix

    let mut GPIO = GPIO::new(500);
    let mut timer = Timer::new();


// This code sets up a CTRL-C handler that writes "true" to the
// interrupt_received bool.
    let int_recv = interrupt_received.clone();
    ctrlc::set_handler(move || {
        int_recv.store(true, Ordering::SeqCst);
    }).unwrap();
    GPIO.init_outputs(0);
    while interrupt_received.load(Ordering::SeqCst) == false {
        GPIO.showImage(&rescaledImage);

// TODO: Implement your rendering loop here
    }
    println!("Exiting.");
    if interrupt_received.load(Ordering::SeqCst) == true {
        GPIO.shutdown();
        println!("Received CTRL-C");
    } else {
        println!("Timeout reached");
    }

// TODO: You may want to reset the board here (i.e., disable all LEDs)
}

