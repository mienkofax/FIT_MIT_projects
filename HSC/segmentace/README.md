# Projekt do predmetu HSC

## Postup

### 1. bod

- Rozbalit projekt do adresara:
```sh
C:\Fitkit\apps\vga\segmentace\...
```
- V súbore ```cpu/common.h``` prepísať login na svoj login.


### 2. bod

- Prekopírovať všetky funkcie okrem ```gen_pixel()``` a ```update_base_pos()``` zo súboru ```cpu.c``` do ```main_sw.c```.

### 3. bod
- Upraviť funkciu ```print_results()``` nasledovne, aby mohla byť použitá na mcu:
```c++
void print_results(int frame, int threshold, int *hist, int n)
{
	term_send_str("Frame: ");
	term_send_num(frame);
	term_send_crlf();

	term_send_str("Histogram: ");
	term_send_num(hist[0]);

	int i;
	for(i = 1; i < n; i++) {
		term_send_str(", ");
		term_send_num(hist[i]);
	}

	term_send_crlf();

	term_send_str("Threshold: ");
	term_send_num(threshold);
	term_send_crlf();
}
```

- Vo funkcii ```pixel_processing()``` inicializovať frame na 100 a inkrementovať ho o 100.
```c++
	static int  frame = 100; // static int  frame = 1;
	...
	frame += 100; // frame++;
```

- Všade kde sa nachádza ```int *hist```, prípadne ```int histogram[..]``` upraviť na  ```long *hist```.

- Upraviť main v časti pre upravu nasledovne:
```c++
	int         r, c, f;
	t_pixel_sw  pix_input, pix_output;
	int         pix_output_vld;

	for (f = 0; f < FRAMES; f += 100) {
		gen_pixel(99);

		for (r = 0; r < FRAME_ROWS; r++) {
			for (c = 0; c < FRAME_COLS; c++) {
				pix_input = gen_pixel(0);

				#ifdef PROFILE
					start_time = get_time();
				#endif

				pixel_processing(pix_input, &pix_output, &pix_output_vld);

				#ifdef PROFILE
					end_time = get_time();
					term_send_str("Time diff (us): ");
					term_send_num((long)(((float)(end_time-start_time))*TIMER_TICK));
					term_send_crlf();
				#endif
			}
		}
	}
```

 - Následne je treba skompilovať na linuxe kód v adresári v cpu a overiť, či sa na fitkite a v terminaly vypíšu rovnaké hodnoty histogramu (chvíľu to trvá) a je nutné zakomentovať ```#define PROFILE```.


