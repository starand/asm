#include "stdlib.h"
#include "memory_manager.h"
#include "interrupts.h"
#include "tty.h"

typedef struct {
	uint64 base;
	uint64 size;
} BootModuleInfo;

void kernel_main(uint8 boot_disk_id, void *memory_map, BootModuleInfo *boot_module_list) {
	init_memory_manager(memory_map);
	init_interrupts();
	init_tty();
	set_text_attr(15);
	printf("Welcome to MyOS!\n");
	
	printf("kernel_page_dir = 0x%x\n", kernel_page_dir);
	printf("memory_size = %d MB\n", memory_size / 1024 / 1024);
        printf("get_page_info(kernel_page_dir, 0xB8000) = 0x%x\n", get_page_info(kernel_page_dir, (void*)0xB8000));

} 
