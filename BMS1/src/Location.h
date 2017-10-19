#pragma once

#include <vector>
#include <list>
#include "BTSInfo.h"

/**
 * Reprezentacia vzdialenosti pomocou suradnic a vzdialenosti k prijmacu.
 */
struct PointWithDistance {
	double x;
	double y;
	double distance;
};

/**
 * Algoritmus pre vypocet pribliznej polohy prijmaca.
 */
class Location final {
public:
	typedef std::pair<BTSInfo::Ptr, double> BTSDistance;

	/**
	 * Vypocet suradnic prijmaca na zaklade priesecnikov jednotlivych bts.
	 *
	 * Vypocet prebieha pomocou permutacii vsetkych vypocitanych stredov,
	 * medzi jednotlivymi BTS.
	 */
	GPSCoordinate::Ptr find(const std::vector<BTSDistance> &measuredBTS);

	/**
	 * Vypocet stredu prijmaca medzi dvomi BTS.
	 *
	 * @see https://cs.wikibooks.org/wiki/Geometrie/Numerick%C3%BD_v%C3%BDpo%C4%8Det_pr%C5%AFniku_dvou_kru%C5%BEnic
	 */
	std::vector<PointWithDistance> solveMiddlePoint(
			PointWithDistance p1, PointWithDistance p2);
};
