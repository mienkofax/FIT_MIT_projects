#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestRunner.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TextTestProgressListener.h>
#include <cppunit/TextOutputter.h>

#include "TapOutputter.h"

using namespace CppUnit;

int main(int, char**)
{
	Test *suite = TestFactoryRegistry::getRegistry().makeTest();

	TestRunner runner;
	TestResult controller;
	TestResultCollector collector;

	runner.addTest(suite);
	controller.addListener(&collector);

	runner.run(controller);

	TapOutputter tapOutputter(&collector, std::cout);
	tapOutputter.write();

	return collector.wasSuccessful()? 0 : 1;
}
