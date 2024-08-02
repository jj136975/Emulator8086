
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Mess1 {
    pub i1: u16, // int
    pub i2: u16, // int
    pub i3: u16, // int
    pub p1: u16, // char *
    pub p2: u16, // char *
    pub p3: u16, // char *
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Mess2 {
    pub i1: u16, // int
    pub i2: u16, // int
    pub i3: u16, // int
    pub l1: u32, // long
    pub l2: u32, // long
    pub p1: u16, // char *
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Mess3 {
    pub i1: u16, // int
    pub i2: u16, // int
    pub p1: u16, // char *
    pub ca1: [u8; 14]
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Mess4 {
    pub l1: u32, // long
    pub l2: u32, // long
    pub l3: u32, // long
    pub l4: u32, // long
    pub l5: u32, // long
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Mess5 {
    pub c1: char,
    pub c2: char,
    pub i1: u16, // int
    pub i2: u16, // int
    pub l1: u32, // long
    pub l2: u32, // long
    pub l3: u32, // long
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct Mess6 {
    pub i1: u16, // int
    pub i2: u16, // int
    pub i3: u16, // int
    pub l1: u32, // long
    // sighandler_t f1;
}

pub union MessageParam {
    pub m1: Mess1,
    pub m2: Mess2,
    pub m3: Mess3,
    pub m4: Mess4,
    pub m5: Mess5,
    pub m6: Mess6,
}
pub struct Message {
    pub m_source: u16,                 /* who sent the message */
    pub m_type: u16,                   /* what kind of message is it */
    pub m_u: MessageParam
}