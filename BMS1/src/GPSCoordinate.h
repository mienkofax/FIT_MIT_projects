#pragma once

#include <Poco/Nullable.h>
#include <Poco/SharedPtr.h>

#include <string>

/**
 * Trieda reprezentuje GPS suradnice a pracu s nimi. Umoznuje konvertovat suradnice
 * na cislo v tvare DD (decimal degrees) a do stringu v tvare DMS (degrees, minutes,
 * seconds).
 *
 * @see https://www.gps-coordinates.net/
 * @see https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#Coordinate_format_conversion
 */
class GPSCoordinate final {
public:
	typedef Poco::SharedPtr<GPSCoordinate> Ptr;

	GPSCoordinate();

	void setDMSLatitude(double latitude);
	void setDMSLongitude(double longitude);

	/**
	 * DD (decimal degrees) - latitude.
	 *
	 * Example: 40.741895
	 */
	double DDLatitude() const;

	/**
	 * DD (decimal degrees) - longitude.
	 *
	 * Example: -73.989307
	 */
	double DDLongitude() const;

	/**
	 * Convert DD (decimal degrees) latitude to DMS (degrees, minutes, seconds).
	 *
	 * Example: 49°11'23.10"N
	 */
	std::string toDMSLatitudeString() const;

	/**
	 * Convert DD (decimal degrees) latitude to DMS (degrees, minutes, seconds).
	 *
	 * Example: 16°32'13.38"E
	 */
	std::string toDMSLongitudeString() const;

	/**
	 * Creates google map link.
	 *
	 * Example:
	 *  https://www.google.com/maps/place/latitude+longitude
	 *  https://www.google.com/maps/place/49°13'30.6"N+16°35'43.9"E
	 */
	std::string toGoogleMapLink() const;

	/**
	 * Example of input: 49°11'23.10"N,16°32'13.38"E
	 */
	static GPSCoordinate::Ptr parseDMS(const std::string &coordinates);

	bool isValid() const;

private:
	/**
	 * Example of input: 49°11'23.10"N
	 *
	 * Regex match example:
	 *   match[0] = "49°11'23.10"N"
	 *   match[1] = "49"
	 *   match[2] = "11"
	 *   match[3] = "23.10"
	 *   match[4] = "N"
	 */
	static double parseDMSLatitude(const std::string &data);

	/**
	 * Example of input: 16°32'13.38"E
	 *
	 * Regex match example:
	 *   match[0] = "16°32'13.38"E"
	 *   match[1] = "16"
	 *   match[2] = "32"
	 *   match[3] = "13.38"
	 *   match[4] = "E"
	 */
	static double parseDMSLongitude(const std::string &data);

private:
	Poco::Nullable<double> m_latitude;  //zemepisna sirka
	Poco::Nullable<double> m_longitude; //zemepisna dlzka
};
