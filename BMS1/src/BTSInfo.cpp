#include "BTSInfo.h"

using namespace std;

BTSInfo::BTSInfo(int CID, int LAC):
	m_CID(CID),
	m_LAC(LAC)
{
}

int BTSInfo::CID() const
{
	return m_CID;
}

int BTSInfo::LAC() const
{
	return m_LAC;
}

GPSCoordinate::Ptr BTSInfo::gps() const
{
	return m_gps;
}

void BTSInfo::setGPSCoordinate(GPSCoordinate::Ptr gps)
{
	m_gps = gps;
}

void BTSInfo::setSignal(double signal)
{
	m_signal = signal;
}

double BTSInfo::signal() const
{
	return m_signal;
}

void BTSInfo::setAntheneHeight(double height)
{
	m_antheneHeight = height;
}

double BTSInfo::antheneHeight() const
{
	return m_antheneHeight;
}

void BTSInfo::setPower(double power)
{
	m_power = power;
}

double BTSInfo::power() const
{
	return m_power;
}

bool BTSInfo::isValid()
{
	if (m_signal.isNull() || m_antheneHeight.isNull() || m_power.isNull())
		return false;

	if (m_gps.isNull())
		return false;

	return true;
}

string BTSInfo::toString() const
{
	string data;
	data += "CID:      " + to_string(m_CID) + "\n";
	data += "LAC:      " + to_string(m_LAC) + "\n";
	data += "GPS lati: " + m_gps->toDMSLatitudeString() + "\n";
	data += "GPS long: " + m_gps->toDMSLongitudeString() + "\n";
	data += "Signal:   " + to_string(m_signal.value()) + "\n";
	data += "Ant H:    " + to_string(m_antheneHeight.value()) + "\n";
	data += "Power:    " + to_string(m_power.value()) + "\n";
	data += "Google:   " + m_gps->toGoogleMapLink() + "\n";

	return data;
}
