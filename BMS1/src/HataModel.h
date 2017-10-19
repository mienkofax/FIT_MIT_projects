#pragma once

#include <functional>

class HataModel final {
public:
	/**
	 * @param f - frequency of transmission [Mhz]
	 * @param Hm - height of base station antenna [m]
	 */
	typedef std::function<double(double f, double Hm)> Func;

	/**
	 * Vypocet Path loss s pozadovanym prostredim
	 *
	 * @see https://en.wikipedia.org/wiki/https://en.wikipedia.org/wiki/Hata_Model#Urban_environments
	 *
	 * @param f - frequency of transmission[Mhz]
	 * @param Hb - height of base station antenna [m]
	 * @param Ch - environment
	 * @param d - distance [m]
	 */
	static double solveLu(double f, double Hb, double Ch, double d);

	/**
	 * Vypocet Lu z prijateho signalu a vysielaneho signalu.
	 * @param power [W]
	 * @param signal [dBm]
	 */
	static double pathLoss(double power, double signal);

	/**
	 * @param Lu - Path loss
	 * @param f - frequency of transmission[Mhz]
	 * @param Hb - height of base station antenna [m]
	 * @param Ch - environment
	 * @param d - distance [m]
	 */
	static double solveDistanceFromLu(double Lu, double f, double Hb, double Ch);

	/**
	 * @see https://en.wikipedia.org/wiki/https://en.wikipedia.org/wiki/Hata_Model#Urban_environments
	 * @see http://www.wirelesscommunication.nl/reference/chaptr03/pel/loss.htm
	 */
	static Func mediumSizedCity();
	static Func largeCities();
};
