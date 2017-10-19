#pragma once

#include "GPSCoordinate.h"

/**
 * Struktura obsahuju UTM vzdialenosti.
 */
struct UTMPoint final {
	double UTMEasting;
	double UTMNorthing;
};

/**
 * Wrapper pre pracu s UTM externou kniznicou.
 */
class UTMWrapper final {
public:
	/**
	 * Prepocet latitude a longitude do UTM pomocou externej kniznice.
	 */
	static UTMPoint LLtoUTM(GPSCoordinate::Ptr gps);

	/**
	 * Prepocet UTM na latitude a longitude.
	 */
	static GPSCoordinate::Ptr UTMtoLL(const UTMPoint &point);
};

