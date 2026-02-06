m68k_ss = ss.source_set()
m68k_ss.add(when: 'CONFIG_AN5206', if_true: files('an5206.c', 'mcf5206.c'))
m68k_ss.add(when: 'CONFIG_MCF5208', if_true: files('mcf5208.c', 'mcf_intc.c'))
m68k_ss.add(when: 'CONFIG_NEXTCUBE', if_true: files('next-kbd.c', 'next-cube.c'))
m68k_ss.add(when: 'CONFIG_Q800', if_true: files('q800.c', 'q800-glue.c'))
m68k_ss.add(when: 'CONFIG_M68K_VIRT', if_true: files('virt.c'))
m68k_ss.add(when: 'CONFIG_M68332', if_true: files('m68332.c',
						  'm68332_intc.c',
						  'm68332_sim.c',
						  'm68332_qsm.c',
						  'm68332_tpu.c',
						  'm68332_tpu_ram.c'))


hw_arch += {'m68k': m68k_ss}
