#include <cmath>

#include <cppunit/extensions/HelperMacros.h>

#include <Application.h>

#include "HataModel.h"

using namespace std;

struct SolveStruct {
	int countOfMesuredBTS;
	vector<double> distances;
	string link;
	string BTSFileName;
	string MeasuredBTSFileName;
	string OutputFileName;
	double f;
	double Hm;
};

class ApplicationTest : public CppUnit::TestFixture {
	CPPUNIT_TEST_SUITE(ApplicationTest);
	CPPUNIT_TEST(testParseDistance2014In1);
	CPPUNIT_TEST(testParseDistance2017In1);
	CPPUNIT_TEST(testParseDistance2017In2);
	CPPUNIT_TEST(testParseDistance2017In3);
	CPPUNIT_TEST_SUITE_END();

public:
	void testParseDistance2014In1();
	void testParseDistance2017In1();
	void testParseDistance2017In2();
	void testParseDistance2017In3();

private:
	void solve(const SolveStruct &solveStruct);
};

CPPUNIT_TEST_SUITE_REGISTRATION(ApplicationTest);

static const double f2014 = 900;
static const double Hm2014 = 1.5; // Height of mobile station

static const double f2017 = 900;
static const double Hm2017 = 1.2; // Height of mobile station

class TestableApplication : public Application {
public:
	vector<BTSInfo::Ptr> measured() const
	{
		return m_inputBTS;
	}
};

void ApplicationTest::solve(const SolveStruct &solveStruct)
{
	TestableApplication app;
	app.setBTSFileName(solveStruct.BTSFileName);
	app.setMeasuredBTSFileName(solveStruct.MeasuredBTSFileName);
	app.setOutputFileName(solveStruct.OutputFileName);

	app.setFrequency(f2014);
	app.setMobileStationHeight(Hm2014);

	app.start();

	const auto measured = app.measured();
	CPPUNIT_ASSERT_EQUAL(solveStruct.countOfMesuredBTS, (int) measured.size());

	const auto mediumCities = HataModel::mediumSizedCity();

	for (size_t i = 0; i < measured.size(); i++) {
		const auto bts = measured.at(i);

		const double Lu = HataModel::pathLoss(
			bts->power(),
			bts->signal()
		);

		const double solvedDistance = HataModel::solveDistanceFromLu(
			Lu,
			solveStruct.f,
			bts->antheneHeight(),
			mediumCities(solveStruct.f, solveStruct.Hm)
		);

		CPPUNIT_ASSERT_DOUBLES_EQUAL(solveStruct.distances.at(i), solvedDistance, pow(10,-2));
	}

	cout << app.toRadiusAroundPointMap() << endl << endl;
	CPPUNIT_ASSERT_EQUAL(solveStruct.link, app.middlePointGPS()->toGoogleMapLink()
	);
}

void ApplicationTest::testParseDistance2014In1()
{
	solve(
		{
			7,
			{
				170.097,
				284.783,
				303.732,
				1010.38,
				1156.97,
				447.048,
				1146.73,
			},
			"https://www.google.com/maps/place/49°13'30.12\"N16°35'43.72\"E",
			"test/csv/bts.csv",
			"test/csv/02.csv",
			"/tmp/tmp.gps0",
			f2014,
			Hm2014
		}
	);
}

void ApplicationTest::testParseDistance2017In1()
{
	solve(
		{
			7,
			{
				161.919,
				271.091,
				289.129,
				962.627,
				1097.801,
				425.554,
				1092.536,
			},
			"https://www.google.com/maps/place/49°13'30.12\"N16°35'43.72\"E",
			"test/csv/bts.csv",
			"test/csv/in1.csv",
			"/tmp/tmp.gps1",
			f2017,
			Hm2017
		}
	);
}

void ApplicationTest::testParseDistance2017In2()
{
	solve(
		{
			6,
			{
				111.901,
				544.575,
				153.560,
				643.528,
				496.557,
				579.409,
			},
			"https://www.google.com/maps/place/49°13'40.47\"N16°35'31.88\"E",
			"test/csv/bts.csv",
			"test/csv/in2.csv",
			"/tmp/tmp.gps2",
			f2017,
			Hm2017
		}
	);
}

void ApplicationTest::testParseDistance2017In3()
{
	solve(
		{
			6,
			{
				85.174,
				124.135,
				218.410,
				634.989,
				271.068,
				343.307,
			},
			"https://www.google.com/maps/place/49°13'24.42\"N16°35'30.88\"E",
			"test/csv/bts.csv",
			"test/csv/in3.csv",
			"/tmp/tmp.gps3",
			f2017,
			Hm2017
		}
	);
}
