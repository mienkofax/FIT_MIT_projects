#pragma once

#include <iosfwd>
#include <vector>

#include <cppunit/Outputter.h>

namespace CppUnit {
	class Exception;
	class Test;
	class TestFailure;
	class TestResultCollector;
}

class TapOutputter : public CppUnit::Outputter {
public:
	TapOutputter(CppUnit::TestResultCollector *collector);
	TapOutputter(CppUnit::TestResultCollector *collector,
		std::ostream &output);

	void write() override;

protected:
	void reportSuccess(unsigned int id, CppUnit::Test *test);
	void reportFailures(unsigned int id, CppUnit::Test *test,
		const std::vector<CppUnit::TestFailure *> &fails);
	void reportException(const CppUnit::Exception *e);

private:
	CppUnit::TestResultCollector *m_collector;
	std::ostream &m_output;
};
