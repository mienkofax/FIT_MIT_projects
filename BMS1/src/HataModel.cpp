#include <cmath>

#include <Poco/Exception.h>

#include "HataModel.h"

double HataModel::solveLu(double f, double Hb, double Ch, double d)
{
	d /= 1000; //m to km
	return 69.55 + 26.16 * log10(f) - 13.82 * log10(Hb)
	       - Ch + (44.9 - 6.55 * log10(Hb)) * log10(d);
}

HataModel::Func HataModel::mediumSizedCity()
{
	return [](double f, double Hm) {
		return 0.8 + (1.1 * log10(f) - 0.7) * Hm - 1.56 * log10(f);
	};
}

HataModel::Func HataModel::largeCities()
{
	return [](double f, double Hm) {
		if (f >= 150 && f <= 200)
			return 8.29 * pow((log10(1.54 * Hm)), 2) - 1.1;
		else if (f >= 200 && f <= 1500)
			return 3.2 * pow((log10(11.75 * Hm)), 2) - 4.97;

		throw Poco::InvalidAccessException("unsupported large Cities frequency");
	};
}

double HataModel::solveDistanceFromLu(double Lu, double f, double Hb, double Ch)
{
	const double numerator = Lu - 26.16 * log10(f) + 13.82 * log10(Hb) + Ch - 69.55;
	const double divisor = 44.9 - 6.55 * log10(Hb);

	return pow(10, numerator / divisor) * 1000; // km to m
}

double HataModel::pathLoss(double power, double signal)
{
	return 10 * log10(1000 * power) - signal;
}
