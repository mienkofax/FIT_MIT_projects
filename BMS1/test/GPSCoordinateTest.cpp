#include <cppunit/extensions/HelperMacros.h>
#include <cmath>

#include "GPSCoordinate.h"

using namespace std;

class GPSCoordinateTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(GPSCoordinateTest);
	CPPUNIT_TEST(testParse);
	CPPUNIT_TEST(testParseBTS);
	CPPUNIT_TEST_SUITE_END();

public:
	void testParse();
	void testParseBTS();
};

CPPUNIT_TEST_SUITE_REGISTRATION(GPSCoordinateTest);

void GPSCoordinateTest::testParse()
{
	auto gps1 = GPSCoordinate::parseDMS("49°11'23.10\"N,16°32'13.38\"E");

	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps1->DDLatitude(), 49.18975, pow(10, -6));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps1->DDLongitude(), 16.53705, pow(10, -6));

	CPPUNIT_ASSERT(gps1->toDMSLatitudeString() == "49°11'23.10\"N");
	CPPUNIT_ASSERT(gps1->toDMSLongitudeString() == "16°32'13.38\"E");

	auto gps2 = GPSCoordinate::parseDMS("49°11'23.10\"S,16°32'13.38\"W");

	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps2->DDLatitude(), -49.18975, pow(10, -6));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps2->DDLongitude(), -16.53705, pow(10, -6));

	CPPUNIT_ASSERT(gps2->toDMSLatitudeString() == "49°11'23.10\"S");
	CPPUNIT_ASSERT(gps2->toDMSLongitudeString() == "16°32'13.38\"W");
}

void GPSCoordinateTest::testParseBTS()
{
	auto gps = GPSCoordinate::parseDMS("49�11'23.10N,16�32'13.38E");

	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps->DDLatitude(), 49.18975, pow(10, -6));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps->DDLongitude(), 16.53705, pow(10, -6));

	gps = GPSCoordinate::parseDMS("49�11'23.10S,16�32'13.38W");

	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps->DDLatitude(), -49.18975, pow(10, -6));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(gps->DDLongitude(), -16.53705, pow(10, -6));
}
