#include <cmath>
#include "Location.h"

#include "UTMWrapper.h"

using namespace std;

GPSCoordinate::Ptr Location::find(
	const vector<Location::BTSDistance> &measuredBTS)
{
	vector<PointWithDistance> points;

	for (size_t i = 1; i < measuredBTS.size(); i++) {
		const GPSCoordinate::Ptr gps1 = measuredBTS.at(i - 1).first->gps();
		const UTMPoint utmPoint1 = UTMWrapper::LLtoUTM(gps1);

		const PointWithDistance p1 = {
			utmPoint1.UTMEasting,
			utmPoint1.UTMNorthing,
			measuredBTS.at(i - 1).second
		};

		for (size_t j = 1; j < measuredBTS.size(); j++) {
			const GPSCoordinate::Ptr gps2 = measuredBTS.at(j).first->gps();
			const UTMPoint utmPoint2 = UTMWrapper::LLtoUTM(gps2);

			const PointWithDistance p2 = {
				utmPoint2.UTMEasting,
				utmPoint2.UTMNorthing,
				measuredBTS.at(j).second
			};

			for (const auto &it : solveMiddlePoint(p1, p2)) {
				if (std::isnan(it.x) || std::isnan(it.y))
					continue;

				points.push_back(it);
			}
		}
	}

	double x = 0;
	double y = 0;

	for (const auto &it : points) {
		x += it.x;
		y += it.y;
	}

	x /= points.size();
	y /= points.size();

	return UTMWrapper::UTMtoLL({x, y});
}

vector<PointWithDistance> Location::solveMiddlePoint(
		PointWithDistance p1, PointWithDistance p2)
{
	vector<PointWithDistance> points;

	double Ax = p1.x;
	double Ay = p1.y;
	double Ar = p1.distance;

	double Bx = p2.x;
	double By = p2.y;
	double Br = p2.distance;

	double d = sqrt(pow(Ax - Bx, 2) + pow(Ay - By, 2));
	double m = (pow(Ar, 2) - pow(Br, 2)) / (2 * d) + (d / 2);
	double v = sqrt(abs(pow(Ar, 2) - pow(m, 2)));

	double Sx = Ax + (m / d) * (Bx - Ax);
	double Sy = Ay + (m / d) * (By - Ay);

	double Cx = (v / d) * (Ay - By);
	double Cy = (v / d) * (Ax - Bx);

	points.push_back(PointWithDistance{Sx + Cx, Sy - Cy, 0});
	points.push_back(PointWithDistance{Sx - Cx, Sy + Cy, 0});

	return points;
}
