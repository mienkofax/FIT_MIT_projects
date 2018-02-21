#include <cmath>

#include <cppunit/extensions/HelperMacros.h>

#include "gif2bmp.h"

using namespace std;

class gif2bmpTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(gif2bmpTest);
	CPPUNIT_TEST(testParseHeader);
	CPPUNIT_TEST(testParseLogicalScreenDescriptor);
	CPPUNIT_TEST(testParseColors);
	CPPUNIT_TEST(testParseImageDescriptor);
	CPPUNIT_TEST(testParseGIF);
	CPPUNIT_TEST_SUITE_END();

public:
	void testParseHeader();
	void testParseLogicalScreenDescriptor();
	void testParseColors();
	void testParseImageDescriptor();
	void testParseGIF();
};

class TestabeGIF : public GIF {
public:
	using GIF::GIF;
	using GIF::parseHeader;
	using GIF::parseLogicalScreenDescriptor;
	using GIF::parseColors;
	using GIF::parseImageDescriptor;

	TestabeGIF():
		GIF()
		{}
};

CPPUNIT_TEST_SUITE_REGISTRATION(gif2bmpTest);

/*
// invalid gif version and signature
	CPPUNIT_ASSERT_THROW(
		gif.parseHeader({0x48, 0x49, 0x46, 0x39, 0x39, 0x61}),
		CustomException
	);

 */

/**
 * Test na overenie ci hlavicka obsahuje spravne hodnoty pri spravnych datach.
 */
void gif2bmpTest::testParseHeader()
{
	std::vector<uint8_t> data;
	TestabeGIF gif(data, NULL);

	// empty data
	CPPUNIT_ASSERT_THROW(gif.parseHeader({}), CustomException);

	// valid gif version and signature
	const GifHeader head1 =
		gif.parseHeader({0x47, 0x49, 0x46, 0x38, 0x39, 0x61});
	CPPUNIT_ASSERT_EQUAL(GIF_SIGNATURE, head1.signature);
	CPPUNIT_ASSERT_EQUAL(GIF_VERSION_89, head1.version);
}

void gif2bmpTest::testParseLogicalScreenDescriptor()
{
	std::vector<uint8_t> data;
	TestabeGIF gif(data, NULL);;
	const vector<uint8_t> raw = {0x0A, 0x00, 0x0A, 0x00, 0x91, 0x00, 0x00};

	// empty data
	CPPUNIT_ASSERT_THROW(gif.parseLogicalScreenDescriptor({}), CustomException);

	GifScreenDescriptor desc = gif.parseLogicalScreenDescriptor(raw);

	CPPUNIT_ASSERT_EQUAL(uint16_t(10), desc.width);
	CPPUNIT_ASSERT_EQUAL(uint16_t(10), desc.height);

	CPPUNIT_ASSERT_EQUAL(true, desc.isGlobalColorTable);
	CPPUNIT_ASSERT_EQUAL(uint8_t(2), desc.colorResolution);
	CPPUNIT_ASSERT_EQUAL(false, desc.tableIsSorted);
	CPPUNIT_ASSERT_EQUAL(int16_t(4), desc.globalColorTableSize);

	CPPUNIT_ASSERT_EQUAL(int8_t(0), desc.bgColorIndex);
	CPPUNIT_ASSERT_EQUAL(int8_t(0), desc.pixelAspectRatio);
}

void gif2bmpTest::testParseColors()
{
	std::vector<uint8_t> data;
	TestabeGIF gif(data, NULL);
	const vector<uint8_t> raw =
		{0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00};

	// empty data
	vector<Rgb> empty;
	gif.parseColors({}, empty);
	CPPUNIT_ASSERT_EQUAL(size_t(0), empty.size());

	// data with 2 colors only
	CPPUNIT_ASSERT_THROW(gif.parseColors({0x00, 0x00}, empty), CustomException);

	// parse valid colors
	vector<Rgb> colors;
	gif.parseColors(raw, colors);
	CPPUNIT_ASSERT_EQUAL(size_t(4), colors.size());
}

void gif2bmpTest::testParseImageDescriptor()
{
	std::vector<uint8_t> data;
	TestabeGIF gif(data, NULL);
	const vector<uint8_t> raw =
		{0x2C, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x00, 0x0A, 0x00, 0x00};

	const vector<uint8_t> invalid =
		{0x2A, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x00, 0x0A, 0x00, 0x00};

	// empty data
	CPPUNIT_ASSERT_THROW(gif.parseImageDescriptor({}), CustomException);

	// invalid image descriptor separator
	CPPUNIT_ASSERT_THROW(gif.parseImageDescriptor(invalid), CustomException);

	GifImageDescriptor desc = gif.parseImageDescriptor(raw);

	CPPUNIT_ASSERT_EQUAL(uint16_t(0), desc.left);
	CPPUNIT_ASSERT_EQUAL(uint16_t(0), desc.top);
	CPPUNIT_ASSERT_EQUAL(uint16_t(10), desc.width);
	CPPUNIT_ASSERT_EQUAL(uint16_t(10), desc.height);
	CPPUNIT_ASSERT_EQUAL(false, desc.localColorTable);
	CPPUNIT_ASSERT_EQUAL(false, desc.interlace);
	CPPUNIT_ASSERT_EQUAL(false, desc.isSorted);
	CPPUNIT_ASSERT_EQUAL(uint16_t(0), desc.localColorTableSize);
}

void gif2bmpTest::testParseGIF()
{
	const std::vector<uint8_t> raw = {
		0x47, 0x49, 0x46, 0x38, 0x39, 0x61, // header
		0x0A, 0x00, 0x0A, 0x00, 0x91, 0x00, 0x00, // logical screen descriptor
		0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, // global color table
		0x21, 0xF9, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, // graphical control extension
		0x2C, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x00, 0x0A, 0x00, 0x00, // image descriptor
		0x02, 0x16, 0x8C, 0x2D, 0x99,
		0x87, 0x2A, 0x1C, 0xDC, 0x33, 0xA0, 0x02, 0x75,
		0xEC, 0x95, 0xFA, 0xA8, 0xDE, 0x60, 0x8C, 0x04,
		0x91, 0x4C, 0x01, 0x00, //image data
		0x3B //trailer
	};

	//Todo postupne otestovanie vsetkych casti
}


