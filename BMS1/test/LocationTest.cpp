#include <cppunit/extensions/HelperMacros.h>

#include <cmath>
#include <UTM.h>
#include <UTMWrapper.h>

#include "Location.h"

class LocationTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(LocationTest);
	CPPUNIT_TEST(testSolveMiddlePoint);
	CPPUNIT_TEST_SUITE_END();

public:
	void testSolveMiddlePoint();
};

CPPUNIT_TEST_SUITE_REGISTRATION(LocationTest);

void LocationTest::testSolveMiddlePoint()
{
	Location loc;

	PointWithDistance p1 = {1, 1, 2};
	PointWithDistance p2 = {1, 3, 2};

	const auto p3 = loc.solveMiddlePoint(p1, p2);
	CPPUNIT_ASSERT_EQUAL(2, (int) p3.size());

	CPPUNIT_ASSERT_DOUBLES_EQUAL(-0.732051, p3.at(0).x, pow(10, 5));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(2, p3.at(0).y, pow(10, 5));

	CPPUNIT_ASSERT_DOUBLES_EQUAL(2.73205, p3.at(1).x, pow(10, 5));
	CPPUNIT_ASSERT_DOUBLES_EQUAL(2, p3.at(1).y, pow(10, 5));
}
