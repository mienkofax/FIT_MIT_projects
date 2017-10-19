#include <string>

#include "UTM.h"
#include "UTMWrapper.h"

static const std::string UTM_ZONE = "33U";

GPSCoordinate::Ptr UTMWrapper::UTMtoLL(const UTMPoint &point)
{
	GPSCoordinate::Ptr gps = new GPSCoordinate;

	double latitude;
	double longitude;

	UTM::UTMtoLL(
		point.UTMNorthing,
		point.UTMEasting,
		UTM_ZONE.c_str(),
		latitude,
		longitude
	);

	gps->setDMSLatitude(latitude);
	gps->setDMSLongitude(longitude);

	return gps;
}

UTMPoint UTMWrapper::LLtoUTM(GPSCoordinate::Ptr gps)
{
	double UTMNorthing;
	double UTMEasting;

	char UTMZone = UTM::UTMLetterDesignator(gps->DDLatitude());

	UTM::LLtoUTM(
		gps->DDLatitude(),
		gps->DDLongitude(),
		UTMNorthing,
		UTMEasting,
		&UTMZone
	);

	return UTMPoint{UTMEasting, UTMNorthing};
}
