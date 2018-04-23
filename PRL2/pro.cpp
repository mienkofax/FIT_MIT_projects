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

/**
 *
 */
struct TPacket {
	int weight;
	int successor;

	string toString()
	{
		string repr;
		repr += "weight: " + to_string(weight);
		repr += ",";

		repr += "successor: " + to_string(successor);

		return repr;
	}
};

struct TRecvFlag {
	bool flag;

	string toString()
	{
		string repr;
		repr += "flag: " + (flag) ? "T" : "F";
	}
};

struct TData{
	int inputSize;
	int cpuCount;
	int cpuID;
	int successor;
	int weight;

	string toString() const
	{
		string repr;
		repr += "inputSize: " + to_string(inputSize);
		repr += ", ";

		repr += "cpuCount: " + to_string(cpuCount);
		repr += ", ";

		repr += "cpuID: " + to_string(cpuID);
		repr += ", ";

		repr += "successor: " + to_string(successor);
		repr += ", ";

		repr += "weight: " + to_string(weight);
		repr += ", ";

		return repr;
	}
};

int calculareWeight(TData *t)
{
	return t->cpuID < t->inputSize - 1;
}

/**
 * Vyhladanie naslednika v Eulerovej ceste.
 * */
static const int SUCCESSOR_END = -1;
int calculateSuccessor(TData *t)
{
	// naslednik poslednej hrany neexistuje
	if (t->inputSize == t->cpuID)
		return SUCCESSOR_END;

	// naslednik poslednej hrany v lavom podstrome
	// je rovny velkosti vstupu - 1
	if (t->cpuID == t->inputSize - 1)
		return 1;

	// sprava s rodicmi, k dalsiemu rodicovi
	if (t->cpuID >= t->inputSize - 1 && (t->cpuID == t->cpuCount - 1 || (t->cpuID - (t->inputSize - 1)) % 2 == 1)) {
		if ((t->cpuID - (t->inputSize - 1)) % 2 == 0)
			return (t->cpuID - (t->inputSize - 1) - 2) / 2 + t->inputSize - 1;

		return (t->cpuID - (t->inputSize - 1) - 3) / 2 + t->inputSize - 1;
	}

	// k pravemu potomkovi rodica smerom z laveho potomka
	if (t->cpuID >= t->inputSize - 1)
		return t->cpuID - (t->inputSize - 2);

	// len jediny lavy potomok, spat k rodicovi
	if (t->cpuID + t->inputSize - 1 >= t->cpuCount - ceil(t->inputSize / 2.0))
		return t->cpuID + t->inputSize - 1;

	// k lavemu potomkovi
	if (t->cpuID < t->inputSize - 1)
		return 2 * t->cpuID + 2;
}

int calculateSuffixSum(TData *t, bool recvFlag)
{
	int send[2] = {t->successor, t->weight};
	int recv[2] = {0};
	int tmp;
	MPI_Status status;

	int tmpRecvFlag;

	for (size_t i = 0; i < 1; i++) {
		if (recvFlag && (t->successor != t->inputSize && t->successor != SUCCESSOR_END))
			MPI_Send(&recvFlag, 1, MPI_CXX_BOOL, t->successor, 0, MPI_COMM_WORLD);
		else if (t->successor == t->inputSize && !recvFlag)
			MPI_Recv(&tmpRecvFlag, 1, MPI_CXX_BOOL, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
		else if (!recvFlag && (t->successor != t->inputSize && t->successor != SUCCESSOR_END))
			MPI_Sendrecv(&tmpRecvFlag, 1, MPI_CXX_BOOL, t->successor, 0,
			             &tmpRecvFlag, 1, MPI_INT, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);

		MPI_Barrier(MPI_COMM_WORLD);

		MPI_Recv(&tmp, 1, MPI_INT, t->successor, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		cout << t->cpuID << ": " << tmp << endl;
	}

	return -1;
}

int main(int argc, char *argv[])
{
	TData t;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &t.cpuCount);
	MPI_Comm_rank(MPI_COMM_WORLD, &t.cpuID);

	t.inputSize = strlen(argv[1]);
	t.successor = calculateSuccessor(&t);
	t.weight = calculareWeight(&t);

	cout << t.toString() << endl;

	// TODO osetrit na kratke vstupy

	cout << calculateSuffixSum(&t, (t.cpuID == 0) ? true : false);

	MPI_Finalize();
	return EXIT_SUCCESS;
}
