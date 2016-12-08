/*
 * @author Peter Tisovèík <xtisov00@stud.fit.vutbr.cz>
 * @brief Funkcie print_user_help, decode_user_cm a fpga_initialized
 * bez zmeny. Funkcia main a keyboard_idle upravene z 90%. Doplnene
 * funkcie cleanBuffer, copyArray, map_keyboard_char, efekt1, efekt2
 * efekt3, efekt4 naprogramove samostatne bez predlohy. V main-e 
 * doplnene inicializacie citaca, odstranenie blikania ledky, pridane
 * inkrementovanie citaca. Vo funkcii keyboard_idle pridane prepinanie
 * medzi pismenami abecedy po stlaceni rovnakej klavesy + timeout,
 * po ktoorm je mozme aj pri stlaceni rovnakej klavesy zapisat novy znak.
 *
 * @modification 90%
 * @last-modified 12.12.2016
 *
 * */
/*******************************************************************************
   main.c: LCD + keyboard demo
   Copyright (C) 2009 Brno University of Technology,
                      Faculty of Information Technology
   Author(s): Zdenek Vasicek <vasicek AT stud.fit.vutbr.cz>

   LICENSE TERMS

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.
   3. All advertising materials mentioning features or use of this software
      or firmware must display the following acknowledgement:

        This product includes software developed by the University of
        Technology, Faculty of Information Technology, Brno and its
        contributors.

   4. Neither the name of the Company nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

   This software or firmware is provided ``as is'', and any express or implied
   warranties, including, but not limited to, the implied warranties of
   merchantability and fitness for a particular purpose are disclaimed.
   In no event shall the company or contributors be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability, whether
   in contract, strict liability, or tort (including negligence or
   otherwise) arising in any way out of the use of this software, even
   if advised of the possibility of such damage.

   $Id$


*******************************************************************************/

#include <fitkitlib.h>
#include <keyboard/keyboard.h>
#include <lcd/display.h>
#include <stdbool.h>

#define NEXT_DELAY      120 // 120 * 10ms = 1.2s
#define BUFFER_SIZE     32
#define END_STRING      '\0'
#define INTERRUPT_TIME  0x4000  //0,5s

char last_ch; //naposledy precteny znak
int pocitadloStlaceni;
char data[BUFFER_SIZE];
char saveData[BUFFER_SIZE];
char out[BUFFER_SIZE];
char dataIndex;
char lastChar;
char oldChar;
int counter;
char efekt;
char smer; //r - doprava, l - dolava
int iterator = 0;

/*
 * Uzivatelska napoveda. 
 */
void print_user_help(void)
{
}

/*
 * Nastavi v zadanom poli zadany znak.
 */
void cleanBuffer(char *buf, int size, char newChar)
{
	int i;
	for (i = 0; i < size; i++)
		buf[i] = newChar;

	iterator = 0;
}

/*
 * Kopirovanie jedneho pola do druheho.
 */
void copyArray(char *from, char *to, int size)
{
	int i;
	for (i = 0; i < size; i++)
		to[i] = from[i];
}

/*
 * Namapovanie cisiel klavesnice, pripadne pismen na znaky abecedy, efekty.
 * @param *ch znak, ktory sa ma namapovat.
 * @return True ak je znak mozne zobrazit na klavesnici, False ak je to nak,
 * ktory robi efekt, ulozenie a vymazanie displaya.
 */
bool map_keyboard_char(char *ch)
{
	bool isPrintChar = true;

	switch(*ch){
	case '1':
		data[dataIndex] = '\0';
		if (dataIndex > 0)
			dataIndex--;
		LCD_write_string(data);
		isPrintChar = false;
		break;
	case '2':
		*ch = 'A';
		break;
	case '3':
		*ch = 'D';
		break;
	case '4':
		*ch = 'G';
		break;
	case '5':
		*ch = 'J';
		break;
	case '6':
		*ch = 'M';
		break;
	case '7':
		*ch = 'P';
		break;
	case '8':
		*ch = 'T';
		break;
	case '9':
		*ch = 'W';
		break;
	case '0':
		*ch = 'Z';
		break;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
		iterator = 0;
		copyArray(saveData, out, BUFFER_SIZE);
		efekt = *ch;
		isPrintChar = false;
		break;
	case '*':
		copyArray(data, saveData, BUFFER_SIZE);
		isPrintChar = false;
		break;
	case '#':
		dataIndex = 0;
		efekt = 0;
		cleanBuffer(data, BUFFER_SIZE, END_STRING);
		cleanBuffer(saveData, BUFFER_SIZE, END_STRING);

		LCD_write_string(data);
		LCD_append_string("Zadajte text");

		isPrintChar = false;
		break;
	default:
		isPrintChar = false;
		break;
	}

	return isPrintChar;
}

/*
 * Obsluha klavesnice. Ak sa zadal znak, ktory reprezentuje znak abecedy, vypise sa.
 * Ak sa zadal rovnaky znak viackrat prepina sa v raci jeho rozsahu abecedy a upravuje
 * sa posledny zadany znak. Ak sa zadal rovnaky vo vacsom rozsahuhu ako je NEXT_DELAY
 * zapise sa na dalsiu poziciu a neprepisuj sa.
 */
void keyboard_idle()
{
	char ch;     // Aktualny znak
	char ch2;    // Neprevedeny stlaceny znak
	int i;

	ch = key_decode(read_word_keyboard_4x4());
	if (ch != last_ch) {
		last_ch = ch;
		if (ch != 0) {
			ch2 = ch;

			if (!map_keyboard_char(&ch))
				return;

			// Opakovanie stlacenie znaku klaves 2-9
			if (pocitadloStlaceni%3 == 0 && pocitadloStlaceni != 0 && ch2 != '0')
				pocitadloStlaceni = 0;

			// Opakovane stlacenie znaku klaves 0 zapis len znaku Z
			if (pocitadloStlaceni > 0 && ch2 == '0')
				pocitadloStlaceni = 0;
	
			if (counter > NEXT_DELAY) {
				counter = 0;

				if (data[dataIndex] != END_STRING)
				  	dataIndex++;
				pocitadloStlaceni = 0;
			}

			// Buffer na vypis dat na lcd je obmedzeny
			if (dataIndex != BUFFER_SIZE) {
				if (oldChar == ch2) {
					data[dataIndex] = ch + pocitadloStlaceni;
					pocitadloStlaceni++;
				}
				else {
					if (data[dataIndex] != END_STRING)
						dataIndex++;

					data[dataIndex] = ch;
					pocitadloStlaceni = 0;
				}
			}

			LCD_write_string(data);

			oldChar = ch2; // predchadzajuci znak
			counter = 0;
		}
	}

	return;
}

/*
 * Dekodovani a vykonani uzivatelskych prikazu
 */
unsigned char decode_user_cmd(char *cmd_ucase, char *cmd)
{
	return CMD_UNKNOWN;
}

/*
 * Inicializace periferii/komponent po naprogramovani FPGA
 */
void fpga_initialized()
{
	LCD_init();
	LCD_clear();
	LCD_append_string("Zadajte text");
}

/*
 * Efekt postupneho rozsvecovania zadanych znakov a nasledne ich zhasinania.
 * Najprv sa zvoli smer pohybu ci sa budu znaky pridavat alebo odoberat. Ak
 * sa nudu znaky odoberat z lcd, tak sa prechadza polom a vzdy na aktualny
 * index sa zapise medzera. Ak sa budu znaky pridavat nakopiruje sa na
 * aktualnu poziciu znak s ulozeneho bufferu a vypise sa na lcd.
 */
void efekt1(void)
{
	if (smer == 'r') {
		if (data[iterator] == END_STRING) {
			out[iterator] = ' ';
			smer = 'l';
			return;
		}
		else {
			out[iterator] = data[iterator] + BUFFER_SIZE;
		}

		iterator++;

		if (iterator == BUFFER_SIZE)
			smer = 'l';
	}
	else {
		out[iterator] = ' ';

		if (iterator == BUFFER_SIZE)
			smer = 'r';

		iterator--;
	}

	LCD_write_string(out);
}

/*
 * Rotacia textu dolava.
 * Nacita sa prvy znak do premennej a ked sa dostane na koniec riadku lcd
 * zapise sa tam. Medzi tymito operaciami sa kopiruje znak z aktualnej
 * pozicie na predchadzajucu poziciu. Takto sa to opakuje v obidvoch
 * riadkov lcd(v obidvoch poloviciach lcd).
 */
void efekt2(void)
{
	int i = 0;
	char znak;

	for (i = 0; i < BUFFER_SIZE; i++) {
		if (out[i] == END_STRING)
			out[i] = ' ';

		if (i == 0 || i == 16) {
			znak = out[i];
			continue;
		}

		out[i-1] = out[i];
		if (i == 31 || i == 15) {
			out[i] = znak;
			continue;
		}

	}

	LCD_write_string(out);
}

/*
 * Postupny prechod zadanym textom na lcd a aktualny znak na ktorym,
 * je kurzor sa zobrazi ako maly, napr "ZADAnYTEXT", "ZADANyTEXT".
 * Postupne sa prechadza 32-znakove pole a meni sa hodnota predchadzajuceho
 * znaku na velky znak, potom sa aktualny velky znak zmeni na maly a vypise
 * na lcd.
 */
void efekt3(void)
{
	if (iterator != 0)
		out[iterator-1] -= 32;

	if (iterator == BUFFER_SIZE || out[iterator] == END_STRING)
		iterator = 0;
	out[iterator] += 32;

	iterator++;

	LCD_write_string(out);
}

/*
 * Rotacia riadkov zhoda dole.
 * Najprv sa odstrani skopiruje prva cast 32-znakoveho pola do druhej polovici
 * a prva polovica sa nahradi medzerami. Potom sa vypise pri druhom prechode
 * sa nakopiruje do prvej polovici druha polovica 32-znakoveho pola. Nasledne
 * sa zobrazia obidva riadky na lcd, potom sa operacia opakuje.
 */
void efekt4(void)
{
	int i = 0;
	switch (iterator){
	case 0:
		for (; i < 16; i++) {
			out[i+16] = out[i];
			out[i] = ' ';
		}
		iterator++;
		break;
	case 1:
		copyArray(saveData, out, BUFFER_SIZE);

		for (i = 16; i < BUFFER_SIZE; i++) {
			out[i-16] = out[i];
			out[i] = ' ';
		}
		iterator++;
		break;
	default:
		for (; i < BUFFER_SIZE; i++)
			out[i] = saveData[i];
		iterator = 0;
		break;
	}
	LCD_write_string(out);
}

/*
 * Prerusenie od casovaca na kanaly 0. Sluzi na opakovane spustanie
 * jednotlivych krokov v jednotlivych efektoch.
 */
interrupt (TIMERA0_VECTOR) Timer_A (void)
{
	flip_led_d6();
	CCR0 += INTERRUPT_TIME;

	switch (efekt) {
	case 'A':
		efekt1();
		break;
	case 'B':
		efekt2();
		break;
	case 'C':
		efekt3();
		break;
	case 'D':
		efekt4();
		break;
	default:
		break;
	}
}

/*
 * Hlavna funkcia
 */
int main(void)
{
	last_ch = 0;
	pocitadloStlaceni = 0;
	counter = 0;
	dataIndex = 0;
	lastChar = 0;
	oldChar = 0;
	efekt = 0;
	smer = 'r';

	cleanBuffer(data, BUFFER_SIZE, END_STRING);
	cleanBuffer(saveData, BUFFER_SIZE, END_STRING);
	cleanBuffer(out, BUFFER_SIZE, END_STRING);

	initialize_hardware();
	keyboard_init();

	// Nastavenie kanala casovaca
	CCTL0 = CCIE;
	CCR0 = INTERRUPT_TIME;
	TACTL = TASSEL_1 + MC_2;

	set_led_d6(1);
  	
	while (1) {
		delay_ms(10);

		keyboard_idle();
		terminal_idle();

		if (counter > NEXT_DELAY) {
			set_led_d5(1);
		}
		else {
			set_led_d5(0);
		}

		counter++;
	}
}
