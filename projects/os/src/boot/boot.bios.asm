; Початковий завантажувач ядра в архітектурі х86
format Binary as "bin"
org 0x7C00 ; вказуємо базовий адрес програми, від якого потім відраховуються адреси міток, а заодно і виділяє місце для стеку
	jmp boot ; пропускаємо структури даних і переходимо на код завантажувача - команда займає 3 байти памяті
	
; Заголовок ListFS - нашої файлової системи
align 4 ;  вирівняти стуктуру по 4байтній границі.
fs_magic dd ?
fs_version dd ?
fs_flags dd ?
fs_base dq ?
fs_size dq ?
fs_map_base dq ?
fs_map_size dq ?
fs_first_file dq ?
fs_uid dq ?
fs_block_size dd ?
; Заголовок файлу
virtual at 0x800
f_info:
	f_name rb 256
	f_next dq ?
	f_prev dq ?
	f_parent dq ?
	f_flags dq ?
	f_data dq ?
	f_size dq ?
	f_ctime dq ?
	f_mtime dq ?
	f_atime dq ?
end virtual

; Дані початкового завантажувача - перевкриваємо стартову команду jmp
label sector_per_track word at $$ ; 2 байти - змінна sector_per_track розміщується по адресу заданому в останньому операторі org
label head_count byte at $$ + 2 ; 1 байти - base addr + 2
label disk_id byte at $$ + 3 ; 1 байт - base addr + 3 - тут збергіатиметься номер завантажувального диску
reboot_msg db "Press any key...",13,10,0
boot_file_name db "boot.bin",0

; Процедура виводить на екран рядок який розміщений по адресі DS:SI
write_str:
	push si
	mov ah, 0x0E
	; 0eH писать символ на активную видео страницу (эмуляция телетайпа)
    ; вход:  AL = записываемый символ (использует существующий атрибут)
    ;        BL = цвет переднего плана (для графических режимов)
 @@:
	lodsb
	test al, al ; перевіряємо чи поточний символ не 0
	jz @f ; якщо так то виходимо з циклу
	int 0x10 ; відео сервіс

	jmp @b
 @@:
	pop si
	ret
	
; Викликається при виникненні критичної помилки
error:
	pop si ; витягуємо адрес повернення з функції (тобто наступний рядок після виклику функції) де розміщується повідомлення про помилку
	call write_str
	
; Перезавантаження
reboot:
	mov si, reboot_msg ; виводимо повідомлення "Press any key..."
	call write_str
	
	; чекаємо натиснення клавіші
	xor ah, ah
	int 0x16 ; сервіс введення виведення клавіатури
	; 00H читать (ожидать) следующую нажатую клавишу
    ;  выход: AL = ASCII символ (если AL=0, AH содержит расширенный код ASCII )
    ;         AH = сканкод  или расширенный код ASCII
	
	jmp 0xFFFF:0
	
; Завантаження сектора за адресою DX:AX в буфер з адресом ES:DI
load_sector:
	push dx
	add ax, word[fs_base] ;  записати в  АХ значення змінної fs_base - 2 байти 
	adc dx, word[fs_base + 2] ; додати до DX [fs_base + 2]
	cmp byte[sector_per_track], 0xFF
	je .use_EDD
	push bx cx si
	div [sector_per_track]
	mov cl, dl
	inc cl
	div [head_count]
	mov dh, ah
	mov ch, al
	mov dl, [disk_id]
	mov bx, di
	mov al, 1
	mov si, 3 ; 3 спроби прочитати з диску
 @@:
	mov ah, 2 ; читати сектор
	int 0x13 ; дисковий ввід/вивід
		; вход: DL = номер диска (0=диск A...; 80H=тв.диск 0; 81H=тв.диск 1)
		;            DH = номер головки чтения/записи
		;            CH = номер дорожки (цилиндра)(0-n) =¬
		;            CL = номер сектора (1-n) ===========¦== См. замечание ниже.
		;            AL = число секторов (в сумме не больше чем один цилиндр)
		;            ES:BX => адрес буфера вызывающей программы
		;            0:0078 => таблица параметров дискеты (для гибких дисков)
		;            0:0104 => таблица параметров тв.диска (для твердых дисков)
		;    выход: Carry-флаг=1 при ошибке и код ошибки диска в AH.
		;           ES:BX буфер содержит данные, прочитанные с диска
		;           замечание: на сектор и цилиндр отводится соответственно 6 и 10 бит:
		;                  1 1 1 1 1 1
		;                 +5-4-3-2-1-0-9-8-7-6-5-4-3-2-1-0+
		;             CX: ¦c c c c c c c c C c S s s s s s¦
		;                 +-+-+-+-+-+-+-+-¦-+-+-+-+-+-+-+-+
		;                                 +======> исп. как старшие биты номера цилиндра 
		
	jnc @f
	xor ah, ah ;
		; 00H Сброс устройства. вызывает рекалибрацию контроллера.
		; если DL равен 80H или 81H, выполнен сброс контр тверд диска, иначе FDC.
	int 0x13 ; дисковий ввід/вивід
	dec si
	jnz @b ; тепер залишилося на 1 спробу менше
 .error:
	call error ; виводимо помилку з тексто з наступного рядка
	db "DISK ERROR",13,10,0
 @@:
	pop si cx bx dx
	ret
 .use_EDD:
	push si
	
	; спеціальна структура - пакет дискового простору
	mov byte[0x600], 0x10 ; розмір стуктури - повинен бути не менше 0х10
	mov byte[0x601], 0 ; не використовується
	mov word[0x602], 1 ; кількість секторів для читання
	mov [0x604], di ; адрес буферу для читення - DS:SI
	push es ; ES вказує на сегмент даних - тому може замінити DS
	pop word[0x606]
	mov [0x608], ax ; 64 бітний номер сектору - всього 8 байт = перших 2 байти
	mov [0x60A], dx ; ще 2 байти
	mov word[0x60C], 0 ; і ще 4 байти
	mov word[0x60E], 0 ;
		
	mov ah, 0x42 ; прочитати сектор
	mov dl, [disk_id] ; тут вказується номер диска з якого відбудеться читання
	mov si, 0x600 ; адрес структури - пакет дискового простору
	int 0x13 ; дисковий ввід/вивід
	
	jc .error
	pop si dx
	ret
	
; Пошук файлу з іменем DS:SI в каталоге DX:AX
find_file:
	push cx dx di
 .find:
	cmp ax, -1
	jne @f
	cmp dx, -1
	jne @f
 .not_found:
	call error
	db "NOT FOUND",13,10,0
 @@:
	mov di, f_info
	call load_sector
	push di
	mov cx, 0xFFFF
	xor al, al
	repne scasb
	neg cx
	dec cx
	pop di
	push si
	repe cmpsb
	pop si
	je .found
	mov ax, word[f_next]
	mov dx, word[f_next + 2]
	jmp .find
 .found:
	pop di dx cx
	ret
	
; Завантаження поточного (знайденого) файлу по адресу BX:0. В АХ повертається кількість завантажених секторів
load_file_data:
	push bx cx dx si di
	mov ax, word[f_data]
	mov dx, word[f_data + 2]
 .load_list:
	cmp ax, -1
	jne @f
	cmp dx, -1
	jne @f
 .file_end:
	pop di si dx cx
	mov ax, bx
	pop bx
	sub ax, bx
	shr ax, 9 - 4
	ret
 @@:
	mov di, 0x8000 / 16
	call load_sector
	mov si, di
	mov cx, 512 / 8 - 1
 .load_sector:
	lodsw
	mov dx, [si]
	add si, 6
	cmp ax, -1
	jne @f
	cmp dx, -1
	je .file_end	
 @@:
	push es
	mov es, bx
	xor di, di
	call load_sector
	add bx, 0x200 / 16
	pop es
	loop .load_sector
	lodsw
	mov dx, [si]
	jmp .load_list
	
; точка входу в початковий завантажувач
boot:
	; налаштування сегментних регістрів
	jmp 0:@f
 @@:
	mov ax, cs
	mov ds, ax ; ds і es вказують туди куди і cs
	mov es, ax
	
	; налаштування стеку
	mov ss, ax ; стековий сегмент - той що і сегмент з кодом
	mov sp, $$ ; "дно" стеку вказує на значення задане директивою org. Стек росте в напрямі зменшення адресів
	; тому org 0x7C00 - задає 0x7C00 - максимальний розмір стеку
	
	; дозволити апаратні переривання
	sti
	; запамятаємо номер завантажувального диску
	mov [disk_id], dl
	
	mov ah, 0x41 ; визначити параметри завантажувального диску
	mov bx, 0x55AA
	int 0x13 ; дискове введення/вивдення
	jc @f ; якщо такий сервіс не доступний переходимо до звичайного
	mov byte[sector_per_track], 0xFF ; якщо доступний
	jmp .disk_detected ; переходимо далі
 @@:
	mov ah, 0x08 ; номер функції для визначення параметрів диску
		;  вход: DL = диск
		; выход: DL = число тв. дисков на первом контроллере
		;        DH = максимальный номер головки
		;        CH = максимальный номер цилиндра (младшие 8 бит)
		;        CL = максим. номер сектора (и старшие биты макс. номера цилиндра)
	xor di, di
	push es
	int 0x13  ; дискове введення/вивдення
	pop es
	jc load_sector.error
	inc dh
	mov [head_count], dh
	and cx, 111111b
	mov [sector_per_track], cx
 .disk_detected:
	; завантажимо продовження початкового завантажувача
	; пошук файлу boot.bin
	mov si, boot_file_name
	mov ax, word[fs_first_file]
	mov dx, word[fs_first_file + 2]
	call find_file
	mov bx, 0x7E00 / 16
	call load_file_data ; завантаження файлу boot.bin
	
	; пепреходимо на завантажувач другого рівня
	jmp boot2
; пустий простір 
rb 510 - ($ - $$)
db 0x55,0xAA ; сигнатура - якщо її не буде BIOS може вважати завантажувач не коректним

; додаткові дані завантажувача
load_msg_preffix db "Loading '",0
load_msg_suffix db "'...",0
ok_msg db "OK",13,10,0
config_file_name db "boot.cfg",0
start16_msg db "Starting 16 bit kernel...",13,10,0
start32_msg db "Starting 32 bit kernel...",13,10,0
label module_list at 0x6000
label memory_map at 0x7000

; розбиття рядка DS:SI по символу слеш
split_file_name:
	push si
 @@:
	lodsb
	cmp al, "/"
	je @f
	test al, al
	jz @f
	jmp @b
 @@:
	mov byte[si - 1], 0
	mov ax, si
	pop si
	ret
; завантаження файлу з імям DS:SI в буфер BX:0. Розмір файлу в сеторах повертається в AX
load_file:
	push si
	mov si, load_msg_preffix
	call write_str
	pop si
	call write_str
	push si
	mov si, load_msg_suffix
	call write_str
	pop si
	push si bp
	mov dx, word[fs_first_file + 2]
	mov ax, word[fs_first_file]
 @@:
	push ax
	call split_file_name
	mov bp, ax
	pop ax
	call find_file
	test byte[f_flags], 1
	jz @f
	mov si, bp
	mov dx, word[f_data + 2]
	mov ax, word[f_data]
	jmp @b	
 @@:
	call load_file_data
	mov si, ok_msg
	call write_str
	pop bp si
	ret
	
; отримання карти памяті
get_memory_map:
	mov di, memory_map
	xor ebx, ebx ; 0 - зміщення від початку карти памяті
 @@:
	mov eax, 0xE820 ; визначити карту памяті
	mov edx, 0x534D4150 ; "SMAP"
	mov ecx, 24 ; розмір буферу
	mov dword[di + 20], 1
	int 0x15 ; рощирений сервіс АТ
	jc @f
	add di, 24
	test ebx, ebx
	jnz @b
 @@:
	cmp di, 0x7000
	ja .ok
	mov dword[di], 0x100000
	mov dword[di + 4], 0
	mov dword[di + 12], 0
	mov dword[di + 16], 1
	mov dword[di + 20], 0
	mov ax, 0xE801 ; визначити розмір памяті до 15 Мб
	int 0x15 ; рощирений сервіс АТ
	jnc @f
	mov ah, 0x88 ; повернути розмір розширеної памяті
	int 0x15 ; рощирений сервіс АТ
	jc .ok
	mov cx, ax
	xor dx, dx
 @@:
	test cx, cx
	jz @f
	mov ax, cx
	mov bx, dx
 @@:
	movzx eax, ax
	movzx ebx, bx
	mov ecx, 1024
	mul ecx
	push eax
	mov eax, ebx
	mov ecx, 65536
	mul ecx
	pop edx
	add eax, edx
	mov [di + 8], eax
	add di, 24
	jmp .ok
 .ok:
	xor ax, ax
	mov cx, 24 / 2
	rep stosw
	ret
	
; завантажувач другого етапу
boot2:
	; завантажуємо конфіг файл boot.cfg
	mov si, config_file_name
	mov bx, 0x1000 / 16
	call load_file
	; виконати завантажувальний скрипт
	mov bx, 0x9000 / 16
	mov bp, module_list
	mov dx, 0x1000
 .parse_line:
	mov si, dx
 .parse_char:
	lodsb
	test al, al
	jz .config_end
	cmp al, 10
	je .run_command
	cmp al, 13
	je .run_command
	jmp .parse_char
 .run_command:
	mov byte[si - 1], 0
	xchg dx, si
	cmp byte[si], 0
	je .parse_line ; пустий рядок
	cmp byte[si], "#"
	je .parse_line ; коментар
	cmp byte[si], "L"
	je .load_file ; завантаження файлу
	cmp byte[si], "S"
	je .start ; Запуск ядра
	; Незнайома команда
	mov al, [si]
	mov [.cmd], al
	call error
	db "Unknown boot script command '"
	.cmd db ?
	db "'!",13,10,0
 .config_end: ; якщо все правильно сюдщи не можна попасти
	; завершення
	jmp reboot
	
; завантаження файлу
 .load_file:
	push dx
	inc si
	call load_file
	push ax
	mov cx, 512
	mul cx
	mov word[bp + 8], ax
	mov word[bp + 10], dx
	mov word[bp + 12], 0
	mov word[bp + 14], 0
	mov ax, bx
	mov cx, 16
	mul cx
	mov word[bp], ax
	mov word[bp + 2], dx
	mov word[bp + 4], 0
	mov word[bp + 6], 0
	pop ax
	shr ax, 9 - 4
	add bx, ax
	add bp, 16
	pop dx
	jmp .parse_line
	
; запуск ядра
 .start:
	; перевіримо чи завантажений хочаб один файл
	cmp bx, 0x9000 / 16
	ja @f
	call error
	db "NO KERNEL LOADED",13,10,0	
 @@:
	; заповнюємо останній елемент списку файлів
	xor ax, ax
	mov cx, 16
	mov di, bp
	rep stosw
	; перехід до процедури ініціалізація ядра потрібної розрядності
	inc si
	cmp word[si], "16"
	je .start16
	cmp word[si], "32"
	je .start32
	;cmp word[si], "64"
	;je .start64
	; невірно вказана розрядність
	call error
	db "Invalid start command argument",13,10,0
	
; Запуск 16-розрядного ядра
 .start16:
	mov si, start16_msg
	mov bx, module_list
	mov dl, [disk_id]
	jmp 0x9000
	
; Запуск 32-розрядного ядра
 .start32:
	; Виводимо повідомлення про запуск 32-бітного ядра
	mov si, start32_msg
	call write_str
	; перевіряємо чи процесор не старіший за i386
	mov ax, 0x7202
	push ax
	popf
	pushf
	pop bx
	cmp ax, bx
	je @f
	call error
	db "Required i386 or better",13,10,0	
 @@:
	; отримати карту памяті
	call get_memory_map
	; очистка таблиці сторінок
	xor ax, ax
	mov cx, 3 * 4096 / 2
	mov di, 0x1000
	rep stosw
	; заповнюємо каталог сторінок
	mov word[0x1000], 0x2000 + 111b
	mov word[0x1FFC], 0x3000 + 111b
	; заповнюємо першу сторінку
	mov eax, 11b
	mov cx, 0x100000 / 4096
	mov di, 0x2000
 @@:
	stosd
	add eax, 0x1000
	loop @b
	; Заполвним останню сторінку
	mov di, 0x3000
	mov eax, dword[module_list]
	or eax, 11b
	mov ecx, dword[module_list + 8]
	shr ecx, 12
 @@:
	stosd
	add eax, 0x1000
	loop @b
	mov word[0x3FF4], 0x4000 + 11b ; Kernel stack
	mov word[0x3FF8], 0x3000 + 11b ; Kernel page table
	; завантажимо значення в CR3
	mov eax, 0x1000
	mov cr3, eax
	; завантажимо значення в GDTR
	lgdt [gdtr32]
	; Запретим прерывания
	cli
	; перейдемо в захищений режим
	mov eax, cr0
	or eax, 0x80000001
	mov cr0, eax
	; перейдемо на 32-бітный код
	jmp 8:start32
; таблиця дескрипторів сегментів яка розміщується тепер в 32 бітному ядрі
align 16
gdt32: ; Мітка - початок таблиці дескрипторів
	dq 0                  ; NULL - 0  - нульовий дескриптор - завжди має бути 0
	dq 0x00CF9A000000FFFF ; CODE - 8 - дескриптор сегменту коду - 0x00000000 - 0xFFFFFFFF
	dq 0x00CF92000000FFFF ; DATA - 16 - дескриптор на сегменту даних - 0x00000000 - 0xFFFFFFFF
gdtr32: ; структура яка завантажуються при lgdt - load global descritor table
	dw $ - gdt32 - 1 ; розмір таблиці - в памяті займає 2 байти
	dd gdt32 ; адрес таблиці дескрипторів - в памяті займає 4 байти
; 32-бітний код
use32
start32:
	; налаштовуємо сегментні регісти та стек
	mov eax, 16 ; зміщення до дескриптора даних в табиці дескрипторів - gdt32
	mov ds, ax ; всі сегментні регістри окрім CS вказують на один сегмент даних
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	mov esp, 0xFFFFDFFC ; адреса стеку
	; Запис параметрів для функції kernel_main
	; записуємо в DL номер завантажувального диску
	mov dl, [disk_id]
	; в EBX адрес списку завантажених файлов
	mov ebx, module_list
	; в ESI адрес карти памяті
	mov esi, memory_map
	; Переходимо на ядро - на код із  startup.asm
	jmp 0xFFC00000
