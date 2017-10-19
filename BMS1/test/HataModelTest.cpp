#include <cmath>

#include <cppunit/extensions/HelperMacros.h>

#include "HataModel.h"

class HataModelTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(HataModelTest);
	CPPUNIT_TEST(testSolveMediumCities);
	CPPUNIT_TEST(testSolveLargeCities);
	CPPUNIT_TEST(testSolveDistance);
	CPPUNIT_TEST(testSolveLu);
	CPPUNIT_TEST_SUITE_END();

public:
	void testSolveMediumCities();
	void testSolveLargeCities();
	void testSolveDistance();
	void testSolveLu();
};

CPPUNIT_TEST_SUITE_REGISTRATION(HataModelTest);

static const double f = 900;
static const double Hm = 1.5; // Height of mobile station

void HataModelTest::testSolveMediumCities()
{
	const auto mediumCities = HataModel::mediumSizedCity();

	const double Lu = HataModel::solveLu(
		f,
		25,
		mediumCities(f, Hm),
		10000
	);

	CPPUNIT_ASSERT_DOUBLES_EQUAL(163.2410642444338, Lu, pow(10, -12));
}

void HataModelTest::testSolveLargeCities()
{
	const auto largeCities = HataModel::largeCities();

	const double Lu = HataModel::solveLu(
		f,
		25,
		largeCities(f, Hm),
		10000
	);

	CPPUNIT_ASSERT_DOUBLES_EQUAL(163.25786511723783, Lu, pow(10, -12));
}

void HataModelTest::testSolveDistance()
{
	const auto mediumCities = HataModel::mediumSizedCity();
	const double Lu = HataModel::pathLoss(10, -60);

	const double solvedDistance = HataModel::solveDistanceFromLu(
		Lu,
		f,
		25,
		mediumCities(f, Hm)
	);

	CPPUNIT_ASSERT_DOUBLES_EQUAL(170.097190844304, solvedDistance, pow(10, -12));
}

void HataModelTest::testSolveLu()
{
	const auto mediumCities = HataModel::mediumSizedCity();

	const double Lu2 = HataModel::pathLoss(10, -60);
	const double Lu1 = HataModel::solveLu(
		f,
		25,
		mediumCities(f, Hm),
		170
	);

	CPPUNIT_ASSERT_DOUBLES_EQUAL(Lu1, Lu2, pow(10, -2));
}
