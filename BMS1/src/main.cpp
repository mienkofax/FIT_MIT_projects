#include <cstdlib>

#include "Application.h"

using namespace std;

static const int FREQUENCY = 900;
static const double MOBILE_STATION_HEIGHT = 1.2;

int main(int argc, char *argv[])
{
	if (argc != 2) {
		cerr << "missing file with measured signal from BTS" << endl;
		exit(Application::MISSING_MEASURED_BTS_FILE);
	}

	Application app;

	app.setBTSFileName("bts.csv");
	app.setMeasuredBTSFileName(argv[1]);
	app.setOutputFileName("out.txt");

	app.setFrequency(FREQUENCY);
	app.setMobileStationHeight(MOBILE_STATION_HEIGHT);

	app.start();

	return EXIT_SUCCESS;
}
