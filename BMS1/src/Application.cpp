#include <iostream>
#include <Poco/NumberParser.h>
#include "Application.h"
#include "CSVParser.h"
#include "HataModel.h"
#include "Location.h"

using namespace Poco;
using namespace std;

static const string MAP_LINK = "https://www.freemaptools.com/radius-around-point.htm?mt=r";

Application::Application():
	m_frequency(0),
	m_mobileStationHeight(0)
{
}

void Application::setBTSFileName(const string &fileName)
{
	m_btsFileName = fileName;
}

void Application::setMeasuredBTSFileName(const string &fileName)
{
	m_measuredBTSFileName = fileName;
}

void Application::setOutputFileName(const string fileName)
{
	m_outputFileName = fileName;
}

void Application::start()
{
	installBTSList();
	installMeasuredBTS();

	if (m_inputBTS.empty())
		throw InvalidArgumentException("no input BTS file for calculation");

	calculateDistance(m_frequency, m_mobileStationHeight);

	Location loc;
	const auto gps = loc.find(m_measured);

	m_middlePointGPS = gps;

	if (!gps->isValid())
		throw RangeException("GPS coordinate is out of range");

	saveToFile(gps);
}

void Application::installBTSList()
{
	CSVParser parser;

	if (!parser.open(m_btsFileName, ';')) {
		cerr << "problem with open bts file " + m_btsFileName << endl;
		exit(MISSING_BTS_FILE);
	}

	parser.nextRow(); // skip header of CSV file

	while (!parser.eof()) {
		const auto row = parser.nextRow();

		// fix problem with empty last line
		if (row.empty())
			continue;

		const int CID = NumberParser::parse(row[0]);
		const int LAC = NumberParser::parse(row[1]);

		BTSInfo::Ptr bts = new BTSInfo(CID, LAC);
		bts->setGPSCoordinate(GPSCoordinate::parseDMS(row[4]));

		m_btsInfo.emplace(CID, bts);
	}
}

void Application::installMeasuredBTS()
{
	CSVParser parser;

	if (!parser.open(m_measuredBTSFileName, ';')) {
		cerr << "problem with open measured file " + m_btsFileName << endl;
		exit(MISSING_MEASURED_BTS_FILE);
	}

	parser.nextRow(); // skip header of CSV file

	while (!parser.eof()) {
		const auto row = parser.nextRow();

		// skip last empty line
		if (row.size() == 0)
			continue;

		const int CID = NumberParser::parse(row[1]);

		auto it = m_btsInfo.find(CID);
		if (it == m_btsInfo.end()) {
			cerr << "not found BTS, skip" << endl;
			continue;
		}

		it->second->setSignal(NumberParser::parse(row[3]));
		it->second->setAntheneHeight(NumberParser::parseFloat(row[4]));
		it->second->setPower(NumberParser::parse(row[5]));

		m_inputBTS.push_back(it->second);
	}
}

void Application::calculateDistance(double f, double Hm)
{
	const auto mediumCities = HataModel::mediumSizedCity();

	for (auto bts : m_inputBTS) {
		const double Lu = HataModel::pathLoss(
			bts->power(),
			bts->signal()
		);

		const double solvedDistance = HataModel::solveDistanceFromLu(
			Lu,
			f,
			bts->antheneHeight(),
			mediumCities(f, Hm)
		);

		m_measured.push_back({bts, solvedDistance});
	}
}

void Application::saveToFile(GPSCoordinate::Ptr gps)
{
	ofstream out(m_outputFileName);

	if (out.is_open()) {
		out << gps->toGoogleMapLink() << endl;
		out.close();
	}
	else {
		cerr << "problem with save data to " + m_outputFileName << endl;
	}
}

GPSCoordinate::Ptr Application::middlePointGPS() const
{
	return m_middlePointGPS;
}

void Application::setFrequency(double f)
{
	m_frequency = f;
}

void Application::setMobileStationHeight(double Hm)
{
	m_mobileStationHeight = Hm;
}

string Application::toRadiusAroundPointMap() const
{
	string out = MAP_LINK;

	for (size_t i = 0; i < m_measured.size(); i++) {
		string data = "&r" + to_string(i + 1) + "=";

		data += to_string(m_measured.at(i).first->gps()->DDLatitude());
		data += "|";

		data += to_string(m_measured.at(i).first->gps()->DDLongitude());
		data += "|";

		data += to_string(m_measured.at(i).second / 1000);
		data += "|00FF00|1|FF0000";

		out += data;
	}

	return out;
}
