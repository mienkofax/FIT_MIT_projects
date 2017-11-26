#include <Poco/NumberFormatter.h>
#include <Poco/NumberParser.h>
#include <Poco/RegularExpression.h>
#include <Poco/StringTokenizer.h>

#include "GPSCoordinate.h"

using namespace Poco;
using namespace std;

static const string GOOGLE_LINK_PREFIX = "www.google.com/maps/place/";

GPSCoordinate::GPSCoordinate()
{
}

double GPSCoordinate::DDLatitude() const
{
	return m_latitude;
}

double GPSCoordinate::DDLongitude() const
{
	return m_longitude;
}

void GPSCoordinate::setDMSLatitude(double latitude)
{
	m_latitude = latitude;
}

void GPSCoordinate::setDMSLongitude(double longitude)
{
	m_longitude = longitude;
}

string GPSCoordinate::toGoogleMapLink() const
{
	return GOOGLE_LINK_PREFIX
		   + toDMSLatitudeString()
		   + "+"
		   + toDMSLongitudeString();
}

GPSCoordinate::Ptr GPSCoordinate::parseDMS(const string &coordinates)
{
	StringTokenizer tokens(coordinates, ",");

	if (tokens.count() != 2)
		throw InvalidArgumentException("unsupported GPS coordinate");

	GPSCoordinate::Ptr gps = new GPSCoordinate;
	gps->setDMSLatitude(parseDMSLatitude(tokens[0]));
	gps->setDMSLongitude(parseDMSLongitude(tokens[1]));

	return gps;
}

double GPSCoordinate::parseDMSLatitude(const string &data)
{
	const RegularExpression re("([0-9]+)[^0-9]*([0-9]+)[^0-9]*([0-9]+.[0-9]+)[^0-9]*([N,S])");
	RegularExpression::MatchVec match;

	if (re.match(data, 0, match) != 5)
		RangeException("unknown number of latitude items");

	double latitude = 0;
	latitude += NumberParser::parseFloat(data.substr(match[1].offset, match[1].length));
	latitude += NumberParser::parseFloat(data.substr(match[2].offset, match[2].length)) / 60;
	latitude += NumberParser::parseFloat(data.substr(match[3].offset, match[3].length)) / 3600;

	if (data.substr(match[4].offset, match[4].length) == "S")
		latitude *= -1;

	return latitude;
}

double GPSCoordinate::parseDMSLongitude(const string &data)
{
	const RegularExpression re("([0-9]+)[^0-9]*([0-9]+)'([0-9]+.[0-9]+)[^0-9]*([E,W])");
	RegularExpression::MatchVec match;

	if (re.match(data, 0, match) != 5)
		RangeException("unknown number of longitude items");

	double longitude = 0;
	longitude += NumberParser::parseFloat(data.substr(match[1].offset, match[1].length));
	longitude += NumberParser::parseFloat(data.substr(match[2].offset, match[2].length)) / 60;
	longitude += NumberParser::parseFloat(data.substr(match[3].offset, match[3].length)) / 3600;

	if (data.substr(match[4].offset, match[4].length) == "W")
		longitude *= -1;

	return longitude;
}

string GPSCoordinate::toDMSLatitudeString() const
{
	if (m_latitude.isNull())
		throw InvalidAccessException("latitude is not set");

	double latitude = m_latitude;

	if (latitude < 0)
		latitude *= -1;

	int iDegree = int(latitude);
	int iMinute = int((latitude - iDegree) * 60);
	double iSecond = (latitude - iDegree - iMinute/60.0) * 3600;

	return to_string(iDegree) + "°"
	       + to_string(iMinute) + "'"
	       + NumberFormatter::format(iSecond, 2) + "\""
	       + ((m_latitude.value() < 0) ? "S" : "N");
}

string GPSCoordinate::toDMSLongitudeString() const
{
	if (m_longitude.isNull())
		throw InvalidAccessException("longitude is not set");

	double longitude = m_longitude;

	if (longitude < 0)
		longitude *= -1;

	int iDegree = int(longitude);
	int iMinute = int((longitude - iDegree) * 60);
	double iSecond = (longitude - iDegree - iMinute/60.0) * 3600;

	return to_string(iDegree) + "°"
	       + to_string(iMinute) + "'"
	       + NumberFormatter::format(iSecond, 2) + "\""
	       + ((m_longitude.value() < 0) ? "W" : "E");
}

bool GPSCoordinate::isValid() const
{
	return !(m_latitude.isNull() || m_longitude.isNull()
	       || isnan(m_latitude.value()) || isnan(m_longitude.value()));
}
