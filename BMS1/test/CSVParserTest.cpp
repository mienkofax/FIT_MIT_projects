#include <cppunit/extensions/HelperMacros.h>

#include "CSVParser.h"

using namespace std;

class CSVParserTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(CSVParserTest);
	CPPUNIT_TEST(testParseCSV00WithoutEOFCRLF);
	CPPUNIT_TEST(testParseCSV00WithoutEOFLF);
	CPPUNIT_TEST(testParseCSV01WithEOFLF);
	CPPUNIT_TEST(testParseCSV01WithEOFCRLF);
	CPPUNIT_TEST(testParseCSV02);
	CPPUNIT_TEST(testParseCSV03BTS);
	CPPUNIT_TEST(testParseCSV04WithoutSomeNumberOfColumn);
	CPPUNIT_TEST_SUITE_END();

public:
	void testParseCSV00WithoutEOFCRLF();
	void testParseCSV00WithoutEOFLF();
	void testParseCSV01WithEOFLF();
	void testParseCSV01WithEOFCRLF();
	void testParseCSV02();
	void testParseCSV03BTS();
	void testParseCSV04WithoutSomeNumberOfColumn();
};

CPPUNIT_TEST_SUITE_REGISTRATION(CSVParserTest);

static const string csvPath = "./test/csv/";

/**
 * Test parse valid input without EOF and CRLF end of line.
 */
void CSVParserTest::testParseCSV00WithoutEOFCRLF()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "00-crlf.csv"));
	CPPUNIT_ASSERT(!parser.eof());

	auto const row1 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row1.size());

	CPPUNIT_ASSERT("one" == row1.at(0));
	CPPUNIT_ASSERT("two" == row1.at(1));
	CPPUNIT_ASSERT("three" == row1.at(2));

	auto const row2 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row2.size());

	CPPUNIT_ASSERT("four" == row2.at(0));
	CPPUNIT_ASSERT("five" == row2.at(1));
	CPPUNIT_ASSERT("six" == row2.at(2));

	CPPUNIT_ASSERT(parser.eof());
}

/**
 * Test parse valid input without EOF and LF end of line.
 */
void CSVParserTest::testParseCSV00WithoutEOFLF()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "00-lf.csv"));
	CPPUNIT_ASSERT(!parser.eof());

	auto const row1 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row1.size());

	CPPUNIT_ASSERT("one" == row1.at(0));
	CPPUNIT_ASSERT("two" == row1.at(1));
	CPPUNIT_ASSERT("three" == row1.at(2));

	auto const row2 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row2.size());

	CPPUNIT_ASSERT("four" == row2.at(0));
	CPPUNIT_ASSERT("five" == row2.at(1));
	CPPUNIT_ASSERT("six" == row2.at(2));

	CPPUNIT_ASSERT(parser.eof());
}

/**
 * Test parse valid input with EOF and CRLF end of line.
 */
void CSVParserTest::testParseCSV01WithEOFCRLF()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "00-crlf.csv"));
	CPPUNIT_ASSERT(!parser.eof());

	auto const row1 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row1.size());

	CPPUNIT_ASSERT("one" == row1.at(0));
	CPPUNIT_ASSERT("two" == row1.at(1));
	CPPUNIT_ASSERT("three" == row1.at(2));

	auto const row2 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row2.size());

	CPPUNIT_ASSERT("four" == row2.at(0));
	CPPUNIT_ASSERT("five" == row2.at(1));
	CPPUNIT_ASSERT("six" == row2.at(2));

	auto const row3 = parser.nextRow();
	CPPUNIT_ASSERT(0 == row3.size());

	CPPUNIT_ASSERT(parser.eof());
}

/**
 * Test parse valid input with EOF and LF end of line.
 */
void CSVParserTest::testParseCSV01WithEOFLF()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "00-lf.csv"));
	CPPUNIT_ASSERT(!parser.eof());

	auto const row1 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row1.size());

	CPPUNIT_ASSERT("one" == row1.at(0));
	CPPUNIT_ASSERT("two" == row1.at(1));
	CPPUNIT_ASSERT("three" == row1.at(2));

	auto const row2 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row2.size());

	CPPUNIT_ASSERT("four" == row2.at(0));
	CPPUNIT_ASSERT("five" == row2.at(1));
	CPPUNIT_ASSERT("six" == row2.at(2));

	auto const row3 = parser.nextRow();
	CPPUNIT_ASSERT(0 == row3.size());

	CPPUNIT_ASSERT(parser.eof());
}

/**
 * Test parse valid input with column separator and CRLF.
 * File is from:
 * https://www.fit.vutbr.cz/study/courses/BMS/public/proj2014/p1.html (in1.csv)
 */
void CSVParserTest::testParseCSV02()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "02.csv", ';'));

	auto const firstLine = parser.nextRow();
	CPPUNIT_ASSERT(6 == firstLine.size());

	CPPUNIT_ASSERT("LAC" == firstLine.at(0));
	CPPUNIT_ASSERT("CID" == firstLine.at(1));
	CPPUNIT_ASSERT("RSSI" == firstLine.at(2));
	CPPUNIT_ASSERT("Signal" == firstLine.at(3));
	CPPUNIT_ASSERT("ant H" == firstLine.at(4));
	CPPUNIT_ASSERT("power" == firstLine.at(5));

	// skip to last line
	for (size_t i = 1; i <= 6; i++)
		parser.nextRow();

	auto const lastLine = parser.nextRow();
	CPPUNIT_ASSERT(6 == lastLine.size());

	CPPUNIT_ASSERT("8030" == lastLine.at(0));
	CPPUNIT_ASSERT("37276" == lastLine.at(1));
	CPPUNIT_ASSERT("-1" == lastLine.at(2));
	CPPUNIT_ASSERT("-91" == lastLine.at(3));
	CPPUNIT_ASSERT("20" == lastLine.at(4));
	CPPUNIT_ASSERT("10" == lastLine.at(5));
}

void CSVParserTest::testParseCSV03BTS()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "bts.csv", ';'));

	auto const firstLine = parser.nextRow();
	CPPUNIT_ASSERT(5 == firstLine.size());

	CPPUNIT_ASSERT("CID" == firstLine.at(0));
	CPPUNIT_ASSERT("LAC" == firstLine.at(1));
	CPPUNIT_ASSERT("BCH" == firstLine.at(2));
	CPPUNIT_ASSERT("Localization" == firstLine.at(3));
	CPPUNIT_ASSERT("GPS" == firstLine.at(4));

	auto const firstDataLine = parser.nextRow();
	CPPUNIT_ASSERT(5 == firstDataLine.size());

	CPPUNIT_ASSERT("10127" == firstDataLine.at(0));
	CPPUNIT_ASSERT("8050" == firstDataLine.at(1));
	CPPUNIT_ASSERT("777" == firstDataLine.at(2));
	//CPPUNIT_ASSERT("Brno - Kohoutovice, Axmanova 531/13, panel�k, ex 1020x (+GSM)" == firstDataLine.at(3));
	//CPPUNIT_ASSERT("49�11'23.10N,16�32'13.38E" == firstDataLine.at(4));
}

void CSVParserTest::testParseCSV04WithoutSomeNumberOfColumn()
{
	CSVParser parser;
	CPPUNIT_ASSERT(parser.open(csvPath + "04.csv", ','));

	auto row1 = parser.nextRow();
	CPPUNIT_ASSERT(3 == row1.size());

	CPPUNIT_ASSERT_THROW(parser.nextRow(), std::exception);
}
