/**
 * @file gif2bmp.h
 * @Author Peter Tisovčík (xtisov00) <xtisov00@stud.fit.vutbr.cz>
 * @date February, 2018
 * @brief 18 Subor so struktuami a hlavickami metod pre prevedenie gif
 * obrazku do bmp.
 */

#pragma once

#include <cstdint>
#include <stdio.h>
#include <vector>
#include <exception>
#include <string>
#include <cmath>
#include <iostream>
#include <map>
#include <deque>
#include <bitset>
#include <fstream>
#include <zconf.h>

/**
 * Logovaci vypis, pre ich jeho vypis je to potrebne nastavit na true.
 */
#define DEBUG_ENABLE false
#define LOG if (DEBUG_ENABLE)

typedef struct {
	int64_t bmpSize;
	int64_t gifSize;
} tGIF2BMP;

struct GifHeader {
	std::string signature;
	std::string version;

	GifHeader():
		signature(""),
		version("")
	{
	}
};

struct GifScreenDescriptor {
	uint16_t width;                // Logical Screen Width (Unsigned)
	uint16_t height;               // Logical Screen Height (Unsigned)
	bool isGlobalColorTable;       // Global Color Table Flag (1 Bit)
	uint8_t colorResolution;       // Color Resolution (3 Bits)
	bool tableIsSorted;            // Sort Flag (1 Bit)
	int16_t globalColorTableSize;  // Size of Global Color Table (3 Bits), 2^(N+1) = 8bits (max N = 8)
	int8_t bgColorIndex;           // Background Color Index (Byte)
	int8_t pixelAspectRatio;       // Pixel Aspect Ratio (Byte)

	GifScreenDescriptor():
		width(0),
		height(0),
		isGlobalColorTable(false),
		colorResolution(0),
		tableIsSorted(false),
		globalColorTableSize(0),
		bgColorIndex(0),
		pixelAspectRatio(0)
	{
	}
};

struct GifGraphicsControl
{
	uint8_t blockSize;
	uint8_t disposalMethod;
	bool userInput;
	bool isTransparentColor;
	uint16_t delayTime;
	uint8_t transparentColorIndex;

	GifGraphicsControl():
		blockSize(0),
		disposalMethod(0),
		userInput(false),
		isTransparentColor(false),
		delayTime(0),
		transparentColorIndex(0)
	{
	}
};

struct GifImageDescriptor {
	uint16_t left;
	uint16_t top;
	uint16_t width;
	uint16_t height;
	bool localColorTable;          //
	bool interlace;                //
	bool isSorted;                 //
	uint16_t localColorTableSize;  //
};

struct Rgb {
	uint8_t red;
	uint8_t green;
	uint8_t blue;
	bool isAlpha;

	Rgb(uint8_t r, uint8_t g, uint8_t b):
		red(r),
		green(g),
		blue(b),
		isAlpha(false)
	{
	}

	std::string toString(const std::string &separator)
	{
		return std::to_string(unsigned(red))
			   + separator + std::to_string(unsigned(green))
			   + separator + std::to_string(unsigned(blue));
	}
};

struct BMPHeader {
	uint16_t bfType;
	uint32_t bfSize;
	uint16_t bfReserved1;
	uint16_t bfReserved2;
	uint32_t bfOffBits;

	BMPHeader():
		bfType(0x4d42),
		bfSize(0x0),
		bfReserved1(0),
		bfReserved2(0),
		bfOffBits(0x7a) //122
	{
	}
};

struct BMPInfoHeader {
	uint32_t biSize;
	uint32_t biWidth;
	uint32_t biHeight;
	uint16_t biPlanes;
	uint16_t biBitCount;
	uint32_t biCompression;
	uint32_t biSizeImage;
	uint32_t biXPelsPerMeter;
	uint32_t biYPelsPerMeter;
	uint32_t biClrUsed;
	uint32_t biClrimportant;
	uint32_t biRedChannelBitMask;
	uint32_t biGreenChannelBitMask;
	uint32_t biBlueChannelBitMask;
	uint32_t biAlphaChanelBitMask;
	uint8_t empty[52];

	BMPInfoHeader():
		biSize(0x6c), //Eh 108bit hlavicka
		biWidth(0),
		biHeight(0),
		biPlanes(0x01),
		biBitCount(0x20),   // 32
		biCompression(0x03),
		biSizeImage(0),
		biXPelsPerMeter(0x0b13),// 0x0b13
		biYPelsPerMeter(0x0b13),
		biClrUsed(0),
		biClrimportant(0),
		biRedChannelBitMask(0x00ff0000),
		biGreenChannelBitMask(0x0000ff00),
		biBlueChannelBitMask(0x000000ff),
		biAlphaChanelBitMask(0xff000000),
 		empty{0}
	{
	}

};

/**
 * Trieda pre spracovanie gif obrazka.
 *
 * @see: https://stackoverflow.com/questions/33594336/gif-image-getting-distorted-on-interlacing?lq=1
 */
enum Interlace {
	EVERY_8TH_LINE,           // 1/8 of size
	EVERY_MISSING_4TH_LINE,   // 1/8 of size
	EVERY_MISSING_EVEN_LINE,  // 1/4 of size
	EVERY_MISSING_ODD_LINE,   // 1/2 of size
};

static const int BMP_HEADER_SIZE = 14;
static const int BMP_INFO_HEADER_SIZE = 108;

static const std::string GIF_SIGNATURE = "GIF";
static const std::string GIF_VERSION_87 = "87a";
static const std::string GIF_VERSION_89 = "89a";
static const int GIF_GRAPHICS_CONTROL_EXTENSION = 0xf9;
static const int GIF_IMAGE_DESCRIPTOR = 0x2C;
static const int GIF_GRAPHICS_CONTROL_EXTENSION_SIZE = 7;
static const int GIF_IMAGE_DESCRIPTOR_SIZE = 10;
static const int GIF_LOGICAL_SCREEN_DESCRIPTOR_SIZE = 7;
static const int GIF_HEADER_SIZE = 6;
static const int GIF_TRAILER = 0x3B;
static const int GIF_EXTENSION = 0x21;
static const int GIF_PLAIN_TEXT_EXTENSION = 0x01;
static const int GIF_COMMENT_EXTENSION = 0xfe;
static const int GIF_APPLICATION_EXTENSION = 0xff;

/**
 * Vyimka spolu so spravou.
 */
class CustomException : public std::exception {
public:
	CustomException(const std::string &message):
		m_message(message)
	{
	}

	std::string message() const
	{
		return m_message;
	}

private:
	std::string m_message;
};
/**
 * @see https://www.w3.org/Graphics/GIF/spec-gif89a.txt
 * @see http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp
 * @see http://www.matthewflickinger.com/lab/whatsinagif/lzw_image_data.asp
 * @see http://www.di.unito.it/~marcog/SM/BMPformat-Wiki.pdf
 * @see https://www.root.cz/clanky/graficky-format-bmp-pouzivany-a-pritom-neoblibeny/
 */
class GIFImage {
public:
	/**
	 * Rozparsovanie vstupnych dat, postupne sa rozparsuje header block a overi sa
	 * signatura a verzia gif suboru. Naslende sa rozparsuje logical screen descriptor.
	 * Potom sa zisti, ci existuje globalna tabulka farieb ak ano nacita sa. Naslene sa
	 * nacita zvysok suboru a to bud jednotlive rozsirenia az pokial nepride koniec.
	 */
	GIFImage(std::vector<uint8_t> &rawData, FILE *output);

	/**
	 * Metoda zisti o ake rozsirenie sa jedna a rozparsuje ho ak je to potrebne.
	 */
	void parseExtensions(std::vector<uint8_t>::iterator &index);

	/**
	 * Metoda nacita image descriptor blok, zisti ci sa v subore nachadza lokalna
	 * tabulka farieb. Nasledne sa zavola metoda fillLocalColorTable(), ktora nacita
	 * do premennej bud lokalnu tabulku farieb alebo globalnu tabulku farieb. Nasledne
	 * sa prejde k zisteniu velkosti LWZ slova. V cyklu sa nasledne zisti dlzka
	 * daneho sub-bloku vytvori sa hashTable s danymi farbami a indexami. Potom
	 * sa len vstupne data preved do binarnej podoby a ulozia ako string. Z tohto
	 * stringu sa potupne odoberaju jednotlive bity na zaklade dlzky LWZ slova.
	 * Ak je tabulka plna zvacsi sa LWZ slovo o jedna az do dlzky 12bitov.
	 */
	void doLZW(FILE *output, std::vector<uint8_t>::iterator &index);

	/**
	 * Naplnenie tabulky farieb, v ktorej sa budu vyhladavat jednotlive
	 * indexy farieb.
	 */
	void fillLocalColorTable(std::vector<uint8_t>::iterator &index);

	/**
	 * Vytvorenie bmp suboru, na zaklade rozparsovaneho obrazku s gif datami.
	 * Ak je potrebne metoda urobi interlace a zavola metodu na ulozenie hlavicky
	 * a dat to vystupneho suboru.
	 */
	void exportToBMP(FILE* output);

	/**
	 * Metoda pre preusporiadanie farieb aby sa odstranilo prehladanie.
	 *
	 * Metoda musi byt volana postupne s jednotlivymi prekladaniami a prve musi byt
	 * EVERY_8TH_LINE,... presne v tom poradi ako v strukture Interlace.
	 *
	 * see: http://www.fileformat.info/format/gif/egff.htm#X058-9-GIF-FG-2
	 * see: https://en.wikipedia.org/wiki/Interlacing_(bitmaps)
	 */
	void doInterlaceStep(std::vector<Rgb> &in, std::vector<Rgb> &out, Interlace interlace);

	/**
	 * Metoda skopiruje data z jedneho vektora a prida ho do druheho.
	 *
	 * Pouzite pri kopirovani dat do vystupneho vektora farieb.
	 */
	void appendColor(const std::vector<Rgb> &colors, std::vector<Rgb> &outputColors);

	/**
	 * Metoda zapise hlavicky a data do vystupneho suboru.
	 *
	 * Struktura BMPHeader nie je zarovnana a preto ju nie je mozne priamo zapisat do vystupneho suboru
	 * ako BMPInfoHeader.
	 */
	void toBMP(const std::vector<Rgb> &colors, FILE *output, uint16_t width, uint16_t height);

	/**
	 * Metoda zapise farby + zarovnanie do vystupneho suboru, pred pouzitim tejto
	 * metody je nutne inicializovat hlavicky BMP suboru.
	 *
	 * Vektor s farbami je usporiadany postupne ako pri pisani, zacina sa v lavom hornom
	 * rohu a konci sa v pravom dolnom rohu.
	 * BMP format obsahuje farby usporiadane tak, ze sa zacina v pravom dolnom rohu a konci
	 * sa v pravom hornom rohu.
	 *
	 * Preusporiadanie prebieha v dvoch cykloch, prvy cyklus prechadza jednotlive riadky a
	 * vnoreny cyklus prechadza jednotlive stlpce v danom riadku. Rozsahy jednotlivych riadkov
	 * su vypocitane tak, ze sa od celkoveho poctu farieb odpocita sirka obrazku (k - width),
	 * toto cislo je zaciatok stlpca riadku a koniec riadku je ako celkovy pocet farieb.
	 * Nasledne sa len hodnoty 'od' a 'do' dekrementuju o sirku obrazka (k = k - width).
	 *
	 * BMP uklada jednotlive bajty farieb v opacnom poradi avsak vzdy posledny bajt je nula.
	 * Farby musia byt zapisane, tak aby bol riadok zarovnany.
	 */
	void BMPWriteColors(const std::vector<Rgb> &colors, FILE *output, uint16_t width, size_t colorResolution);

	/**
	 * Celkovo velkost bmp obrazku spolu zo vsetkymi hlavickami.
	 */
	size_t bmpSize() const;

	/**
	 * Celkovo velkos gif obrazku, zo vsetkymi hlavickami.
	 */
	size_t gifSize() const;

protected:
	/**
	 * Prazdny konstruktor pre moznost testovania.
	 */
	GIFImage();

	/**
	 * Rozparsovanie header segmentu.
	 */
	GifHeader parseHeader(const std::vector<uint8_t> &data);

	/**
	 * Rozparsovanie logical screen descriptor segmentu.
	 */
	GifScreenDescriptor parseLogicalScreenDescriptor(const std::vector<uint8_t> &data);

	/**
	 * Nacitanie farieb z tabulky (globalna alebo lokalna) do vektoru farieb.
	 */
	void parseColors(const std::vector<uint8_t> &data, std::vector<Rgb> &colors);

	/**
	 * Rozparsovanie image descriptor segmentu.
	 */
	GifImageDescriptor parseImageDescriptor(const std::vector<uint8_t> &data);

	/**
	 * Rozparsovanie graphics control rozsirenia.
	 */
	GifGraphicsControl parseGraphicsControl(const std::vector<uint8_t> &data);

	/**
	 * Vypocet zarovania riadku na 32 bitov (4 bajty).
	 *
	 * Vyraz "(coloResolution*width) % 4" vracia pocet bajtov, ktore su navyse.
	 * Avsak mi hladame opacnu hodnotu a to taku, ze kolko bajtov musime pridat
	 * aby bol riadok presne zarovnany ? Toto docielime upravov vyrazu na:
	 * "(-1 * coloResolution * width) % 4".
	 *
	 * @return Pocet bajtov pre zarovnanie.
	 */
	size_t calculateAlign(size_t width, size_t coloResolution) const;

private:
	GifHeader m_header;
	GifScreenDescriptor m_screenDescriptor;
	std::vector<Rgb> m_globalTable;
	std::vector<Rgb> m_localColorTable;
	GifGraphicsControl m_graphicsControl;
	GifImageDescriptor m_imageDescriptor;

	std::vector<Rgb> m_outputColors;

	size_t m_gifSize{};
};

/**
 *
 * @param gif2bmp zaznam o prevode
 * @param inputFile vstupny subor (GIF)
 * @param outputFile vystupny subor (BMP)
 * @return 0 prevod prebehol v poriadku, -1 pri prevodu doslo k chybe,
 * pripadne nepodporovany dany format GIF suboru
 */
int gif2bmp(tGIF2BMP *gif2bmp, FILE *inputFile, FILE *outputFile);
