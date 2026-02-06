#pragma once

#define NUMBER_TPUS (16)

typedef struct {
   void      *s;     // our parent device
   QEMUTimer *timer;
   qemu_irq   irq;

   uint32_t   channel;
   uint32_t   function;
   uint32_t   priority;
   uint32_t   host_sequence_code;
   uint32_t   host_service_request;
   uint32_t   ier;
   uint32_t   is_running;
} tpu_t;

struct mcf_tpu_state {
   SysBusDevice  parent_obj;
   MemoryRegion  iomem;
   DeviceState  *intc_dev;

   QEMUTimer    *timer;
   uint32_t      timer_count;

   QEMUTimer    *neg_x_timer;
   uint32_t      neg_x_timer_count;

   //
   // Register storage
   //
   uint16_t tpumcr; // e00
   uint16_t tcr;    // e02
   uint16_t dscr;   // e04
   uint16_t dssr;   // e06
   uint16_t ticr;   // e08
   uint16_t cier;   // e0a
   uint16_t cfsr0;  // e0c
   uint16_t cfsr1;  // e0e
   uint16_t cfsr2;  // e10
   uint16_t cfsr3;  // e12
   uint16_t hsqr0;  // e14
   uint16_t hsqr1;  // e16
   uint16_t hsrr0;  // e18
   uint16_t hsrr1;  // e1a
   uint16_t cpr0;   // e1c
   uint16_t cpr1;   // e1e
   uint16_t cisr;   // e20
};

enum {
   TPUMCR = 0,
   TICR   = 8,
   CIER   = 0xa,
   CFSR0  = 0xc,
   CFSR1  = 0xe,
   CFSR2  = 0x10,
   CFSR3  = 0x12,
   HSQR0  = 0x14,
   HSQR1  = 0x16,
   HSRR0  = 0x18,
   HSRR1  = 0x1a,
   CPR0   = 0x1c,
   CPR1   = 0x1e,
   CISR   = 0x20,
};

enum {
   FUNCTION_IDLE = 0,
   FUNCTION_DIO  =  8,
   FUNCTION_PWM  =  9,
   FUNCTION_OC   = 14
};
