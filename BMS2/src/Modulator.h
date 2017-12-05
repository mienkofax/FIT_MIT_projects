#pragma once

#include <queue>

#include "AbstractModulator.h"

class Modulator final : public AbstractModulator {
public:
	Modulator();
	~Modulator();

	void process() override;
	void save() override;

private:
	/**
	 * Nacitanie suboru do fronty, ktora bude obsahovat vstupne symboly.
	 */
	void loadData(std::queue<char> &q);

	/**
	 *   00 | 01
	 *   -------
	 *   10 | 11
	 *
	 * 00 - 1pi/4
	 * 01 - 3pi/4
	 * 10 - 5pi/4
	 * 11 - 7pi/4
	 *
	 */
	void modulate(char c1, char c2, long index);

	/**
	 * Generate of synchronization sequence.
	 */
	void setSyncSequence(std::queue<char> &q);

private:
	int *m_buffer;
	long m_length;
};
