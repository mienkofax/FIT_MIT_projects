#include <cppunit/extensions/HelperMacros.h>

#include <cmath>
#include <UTMWrapper.h>

class UTMWrapperTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(UTMWrapperTest);
	CPPUNIT_TEST(testConvertFromLLToUTM);
	CPPUNIT_TEST(testConvertFromUTMToLL);
	CPPUNIT_TEST_SUITE_END();

public:
	void testConvertFromLLToUTM();
	void testConvertFromUTMToLL();
};

CPPUNIT_TEST_SUITE_REGISTRATION(UTMWrapperTest);

void UTMWrapperTest::testConvertFromLLToUTM()
{
	auto gps1 = GPSCoordinate::parseDMS("49°11'23.10\"N,16°32'13.38\"E");
	auto utmPoint = UTMWrapper::LLtoUTM(gps1);

	CPPUNIT_ASSERT_DOUBLES_EQUAL(611994, utmPoint.UTMEasting, 1);
	CPPUNIT_ASSERT_DOUBLES_EQUAL(5449687, utmPoint.UTMNorthing, 1);
}

void UTMWrapperTest::testConvertFromUTMToLL()
{
	UTMPoint utmPoint = {611994, 5449687};

	auto gps1 = UTMWrapper::UTMtoLL(utmPoint);

	CPPUNIT_ASSERT_DOUBLES_EQUAL(49.18975, gps1->DDLatitude(), pow(10, -5));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(16.53705, gps1->DDLongitude(), pow(10, -5));
}
