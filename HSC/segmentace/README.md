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

### 4. bod

- Prepísať v project.xml ```main_sw.c``` na ```main_swhw.c```.
- Skopírovať funkcie ```otsu(), print_results``` do ```main_swhw.c```.
- Do súboru ```main_swhw.c``` doplniť ```#include "../cpu/common.h"```.
- V maine v súbore ```main_swhw.c``` upraviť main nasledovne:
```c++
	int new = 0, prev = 0, threshold = 0;
	long unsigned his[PIXELS];
	unsigned i;

	while (1) {
		new = fpga_read(FPGA_FRAME_CNT);
		if (new != prev) {
			if (new % 10 == 0) {
				for (i = 0; i < PIXELS; i++){
					his[i] = fpga_read(FPGA_HISTOGRAM + i);
					fpga_write(FPGA_HISTOGRAM + i, 0);
				}

				threshold = otsu(his, PIXELS);
				fpga_write(FPGA_THRESHOLD, threshold);
			}

			if (new % 100 == 0 && new <= FRAMES)
				print_results(new, threshold, his, PIXELS);

			prev = new;
		}
	}
```

### 5. bod

- Do ```filter.cpp``` skopírovať z ```main_sw.c``` nasledujúce funkcie: ```buffer(),
 clip_window(), shift_window(), system_input(), thresholding()``` a ```void pixel_processing()``` s nasledujúcim predpisom:
```c++
	void pixel_processing(t_pixel data_in, t_pixel &data_out, t_mcu_data mcu_data[MCU_SIZE])
	{
		static   ac_int<3,false>   threshold = 4;
		static   ac_int<32,false>  frame = 1;
		static   ac_int<1,false>   last_pixel;
		t_pixel                    pix_filtered, window[9];

		ac_int<4,false> mod10 = frame % 10;

		if (!system_input(data_in, window, last_pixel))
			return;

		pix_filtered = median(window);

		if (mod10 == 0)
			mcu_data[FPGA_HISTOGRAM + pix_filtered]++;

		data_out = thresholding(pix_filtered, threshold);

		if (last_pixel) {
			if (mod10 == 1 && frame != 1)
				threshold = mcu_data[FPGA_THRESHOLD];

			mcu_data[FPGA_FRAME_CNT] = frame;
			frame++;
		}
	}
```

- ```filter()``` upraviť, tak aby volal metódu ```pixel_processing()``` nasledovne:
```c++
	else if (in_data_vld) {
		pixel_processing(in_data, out_data, mcu_data);
	}
```

- Upraviť dátové typy.

