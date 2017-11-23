#pragma once

#include <list>
#include <map>
#include <string>
#include <vector>

#include "BTSInfo.h"

class Application {
public:
	typedef std::pair<BTSInfo::Ptr, double> BTSDistance;

	enum EXIT_CODE {
		MISSING_BTS_FILE = 1,
		MISSING_MEASURED_BTS_FILE = 2,
	};

	Application();

	void start();

	void setBTSFileName(const std::string &fileName);
	void setMeasuredBTSFileName(const std::string &fileName);
	void setOutputFileName(const std::string fileName);

	void setFrequency(double  i);
	void setMobileStationHeight(double d);

	GPSCoordinate::Ptr middlePointGPS() const;

	std::string toRadiusAroundPointMap() const;

protected:
	/**
	 * Save BTS from input file.
	 */
	void installBTSList();

	/**
	 * Save measured information from mobile station.
	 */
	void installMeasuredBTS();

	void calculateDistance(double f, double Hm);
	void saveToFile(GPSCoordinate::Ptr gps);

protected:
	std::string m_btsFileName;
	std::string m_measuredBTSFileName;
	std::string m_outputFileName;
	std::map<int, BTSInfo::Ptr> m_btsInfo;
	std::vector<BTSInfo::Ptr> m_inputBTS;
	std::vector<BTSDistance> m_measured;
	GPSCoordinate::Ptr m_middlePointGPS;

	int m_frequency;
	int m_mobileStationHeight;
};
