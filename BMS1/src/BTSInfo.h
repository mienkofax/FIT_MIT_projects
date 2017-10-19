#pragma once

#include <string>

#include <Poco/SharedPtr.h>

#include "GPSCoordinate.h"

/**
 * Map key for BTS.
 */
struct BTSInfoKey final {
	int CID;
	int LAC;

	bool operator <(const BTSInfoKey &id) const
	{
		return CID < id.CID || (CID == id.CID && LAC < id.LAC);
	}
};

class BTSInfo final {
public:
	typedef Poco::SharedPtr<BTSInfo> Ptr;

	BTSInfo(int CID, int LAC);

	int CID() const;
	int LAC() const;

	void setGPSCoordinate(GPSCoordinate::Ptr gps);
	GPSCoordinate::Ptr gps() const;

	void setSignal(double signal);
	double signal() const;

	void setAntheneHeight(double height);
	double antheneHeight() const;

	void setPower(double power);
	double power() const;

	bool isValid();

	std::string toString() const;

private:
	int m_CID;
	int m_LAC;
	GPSCoordinate::Ptr m_gps;

	Poco::Nullable<double> m_signal; //dBm
	Poco::Nullable<double> m_antheneHeight; //in m
	Poco::Nullable<double> m_power; //in W
};
