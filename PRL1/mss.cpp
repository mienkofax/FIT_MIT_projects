/**
 * Peter Tisovčík (xtisov00@stud.fit.vutbr.cz)
 */
#include <algorithm>
#include <chrono>
#include <cmath>
#include <fstream>
#include <string>
#include <vector>

#include <mpi.h>

using namespace std;
//using namespace std::chrono;

static const string INPUT_FILE = "numbers";
//high_resolution_clock::time_point t_start;

/**
 * Struktura s informaceami o potrebnych datech.
 */
struct TData {
	// vektor s daty na usporadani
	vector<int> numbers;

	// pocet procesoru
	int countProcessors;

	// id procesoru
	int id;

	// pocet cisel na procesoru
	int countNumbers;

	// pocet cisel v procesoru v pripade, ze pocet hodnot se neda rozdelit
	// rovnomerne na vsechny procesory
	int lessCountNumbers;

	// pocet cisel na aktualnim procesoru
	int currentNumberCount;

	// index ve vektoru, od ktereho se maji cisla odeslat
	int index;

	TData():
		countProcessors(0),
		id(0),
		countNumbers(0),
		lessCountNumbers(0),
		currentNumberCount(0),
		index(0)
	{
	}
};

/**
 * Nacitani cisel ze vstupniho souboru. Jeden znak == jedno cislo.
 */
vector<int> fileContents(const string &filename)
{
	vector<int> numbers;
	int number;

	ifstream is(filename);
	while (is.good()) {
		number = is.get();

		if (!is.good())
			break;

		numbers.push_back(number);
	}

	is.close();
	return numbers;
}

/**
 * Prevedeni vektoru cisel na retezec, kde jsou cisla oddelena oddelovacem,
 * ktery je mozne definovat. Posledni oddelovac je odstraneny.
 */
string numbersAsString(const vector<int> &numbers, const char &delimiter)
{
	string str;
	for (const auto &num : numbers)
		str +=  to_string(num) + delimiter;

	return str.substr(0, str.length() - 1);
}

/**
 * Odoslani hodnot levemu sousedovi
 */
void toLeftNeighbor(vector<int> &numbers, int count, int id)
{
	const int leftID = id - 1;

	MPI_Send(&count, 1, MPI_INT, leftID, 0, MPI_COMM_WORLD);
	MPI_Send(numbers.data(), count, MPI_INT, leftID, 0, MPI_COMM_WORLD);

	MPI_Recv(&count, 1, MPI_INT, leftID, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	MPI_Recv(numbers.data(), count, MPI_INT, leftID, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}

/**
 * Odoslani hodnot zpet levemu sousedovi, ktori predtim odeslal svoje data.
 */
void sendSplitBackToNeighbor(
	vector<int> &merged, vector<int> &neighbors, int id, int index)
{
	neighbors.assign(merged.begin() + index, merged.end());

	MPI_Send(&neighbors, 1, MPI_INT, id, 0, MPI_COMM_WORLD);
	MPI_Send(neighbors.data(), neighbors.size(), MPI_INT, id, 0, MPI_COMM_WORLD);
}

/**
 * Prijeti vektoru dat od sousedniho procesoru jeho spojeni usporadani
 * a nasledne rozdeleni a odeslani dat sousedovi.
 */
void neighborMergeAndSplit(vector<int> &numbers, int id)
{
	const int rightId = id + 1;
	int size = 0;

	MPI_Recv(&size, 1, MPI_INT, rightId, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	vector<int> neighbors(size, 0);
	MPI_Recv(neighbors.data(), size, MPI_INT, rightId, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	// merge
	vector<int> merged(numbers.size() + neighbors.size());
	merge(numbers.begin(), numbers.end(), neighbors.begin(), neighbors.end(), merged.begin());

	// rozdeleni na dve priblizne stejne casti
	const int index = ceil(merged.size() / 2.0);
	numbers.assign(merged.begin(), index +  merged.begin());

	// rozdeleni
	sendSplitBackToNeighbor(merged, neighbors, rightId, index);
}

/**
 * Inicializace pocatecnych hodnot jako pocet procesoru, pocet cisel.
 */
void init(TData *t)
{
	if (t->countProcessors > t->numbers.size())
		t->countProcessors = t->numbers.size();

	t->countNumbers = ceil(t->numbers.size() / float(t->countProcessors));

	if (t->numbers.size() % t->countProcessors != 0) {
		t->lessCountNumbers = t->countProcessors;
		t->lessCountNumbers -= t->numbers.size() % t->countProcessors;
	}

	//t_start = high_resolution_clock::now();
	for (size_t i = 0; i < t->countProcessors; i++) {
		if (t->countProcessors - i == t->lessCountNumbers)
			t->countNumbers--;

		const int countNumbers = t->countNumbers;
		MPI_Send(&countNumbers, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
	}
}

/**
 * Odoslani casti dat jednotlivym procesorum.
 */
vector<int> recvNumbers(TData *t)
{
	vector<int> currentNumbers (t->currentNumberCount, 0);

	t->countNumbers = ceil(float(t->numbers.size()) / float(t->countProcessors));

	for (size_t i = 0; i < t->countProcessors; i++) {
		if (t->countProcessors - i == t->lessCountNumbers)
			t->countNumbers--;

		if (i != 0)
			MPI_Send(t->numbers.data() + t->index, t->countNumbers, MPI_INT, i, 0, MPI_COMM_WORLD);
		else
			memcpy(static_cast<void*>(currentNumbers.data()), static_cast<void*>(t->numbers.data() + t->index), sizeof(int) * t->countNumbers);

		t->index += t->countNumbers;
	}

	return currentNumbers;
}

/**
 * Paralelne setrideni postupne jednotlivych casti mezi procesory.
 */
void parallelSorting(TData *t, vector<int> &currentNumbers)
{
	const size_t count = ceil(float(t->countProcessors) / 2.0) + 1;

	for (size_t i = 0; i < count; i++) {
		const bool isIDEven = t->id % 2 == 0; // True ak je id parne
		const bool isIDOdd = t->id % 2 == 1; // True ak je id neparne
		const bool isCurrentProcessor = t->id + 1 == t->countProcessors;

		if (isIDOdd)
			toLeftNeighbor(currentNumbers, t->currentNumberCount, t->id);
		else if (isIDEven && !isCurrentProcessor)
			neighborMergeAndSplit(currentNumbers, t->id);

		if (isIDEven && t->id > 0)
			toLeftNeighbor(currentNumbers, t->currentNumberCount, t->id);
		else if (isIDOdd && !isCurrentProcessor)
			neighborMergeAndSplit(currentNumbers, t->id);
	}
}

/**
 * Root postupne prijme vsechny castecne vysledky a vrati seradene pole.
 */
vector<int> recvResults(TData *t)
{
	vector<int> sortedNumbers;
	int countNumbers = 0;
	int tmp = 0;

	for (size_t i = 0; i < t->countProcessors; i++) {
		MPI_Recv(&countNumbers, 1, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		MPI_Recv(t->numbers.data() + tmp, countNumbers, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		tmp += countNumbers;
	}

	//high_resolution_clock::time_point t_end = high_resolution_clock::now();
	//auto duration = duration_cast<microseconds>(t_end - t_start).count();
	//cout << "time: " << duration << endl;

	return sortedNumbers;
}

int main(int argc, char *argv[])
{
	TData t;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &t.countProcessors);
	MPI_Comm_rank(MPI_COMM_WORLD, &t.id);

	if (t.id == 0) {
		t.numbers = fileContents(INPUT_FILE);
		cout << numbersAsString(t.numbers, ' ') << endl;
	}

	if (t.id == 0)
		init(&t);

	MPI_Bcast(&t.countProcessors, 1, MPI_INT, 0, MPI_COMM_WORLD);
	if (t.id >= t.countProcessors) {
		MPI_Finalize();
		return 0;
	}

	MPI_Recv(&t.currentNumberCount, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	vector<int> currentNumbers(t.currentNumberCount, 0);
	if (t.id == 0)
		currentNumbers = recvNumbers(&t);
	else
		MPI_Recv(currentNumbers.data(), t.currentNumberCount, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	sort(currentNumbers.begin(), currentNumbers.end());
	parallelSorting(&t, currentNumbers);

	MPI_Send(&t.currentNumberCount, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
	MPI_Send(currentNumbers.data(), t.currentNumberCount, MPI_INT, 0, 0, MPI_COMM_WORLD);

	vector<int> sortedNumbers;
	if (t.id == 0) {
		sortedNumbers = recvResults(&t);
		cout << numbersAsString(t.numbers, '\n') << endl;
	}

	MPI_Finalize();
	return 0;
}
