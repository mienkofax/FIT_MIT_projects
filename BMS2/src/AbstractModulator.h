#pragma once

#include <cmath>
#include <memory>
#include <string>

const double PHASE_SHIFT_1p4 = 1 * M_PI / 4;
const double PHASE_SHIFT_3p4 = 3 * PHASE_SHIFT_1p4;
const double PHASE_SHIFT_5p4 = 5 * PHASE_SHIFT_1p4;
const double PHASE_SHIFT_7p4 = 7 * PHASE_SHIFT_1p4;

const std::string SYNC_SEQUENCE = "00110011";

/**
 * Trieda poskytuje rozhranie pre Modulator a demodulator, umoznuje spojit
 * podobne metody a volania.
 */
class AbstractModulator {
public:
	typedef std::unique_ptr<AbstractModulator> Ptr;

	virtual ~AbstractModulator();

	/**
	 * Set Input file name with data or waw file.
	 */
	void setInputFileName(const std::string &filename);
	std::string inputFileName() const;

	virtual void process() =0;
	virtual void save() =0;

protected:
	std::string rawFileName(const std::string &filename, const std::string &extension) const;

private:
	std::string m_inputFileName;
};
