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
	size_t step;

	string toString() const
	{
		string repr;
		repr += "step: " + to_string(step);
		repr += ", ";

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
	if (t->inputSize == 2 && t->cpuID == 0)
		return 1;

	if (t->inputSize == 2 && t->cpuID == 1)
		return -1;

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

/**
 * Vzdialenost hrany od konca zoznamu.
 */
void calculateSuffixSum(TData *t, bool recvFlag)
{
	int send[2] = {t->successor, t->weight};
	int recv[2] = {0};
	MPI_Status status;

	bool tmpRecvFlag = recvFlag;

	if (t->inputSize < 3)
		return;

	const int limit = static_cast<int>(ceilf(log2(t->cpuCount)));
	for (size_t i = 0; i < limit; i++) {
		if (recvFlag && (t->successor != t->inputSize && t->successor != SUCCESSOR_END))
			MPI_Send(&recvFlag, 1, MPI_CXX_BOOL, t->successor, 0, MPI_COMM_WORLD);
		else if (t->successor == t->inputSize && !recvFlag)
			MPI_Recv(&tmpRecvFlag, 1, MPI_CXX_BOOL, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
		else if (!recvFlag && (t->successor != t->inputSize && t->successor != SUCCESSOR_END))
			MPI_Sendrecv(&recvFlag, 1, MPI_CXX_BOOL, t->successor, 0,
			             &tmpRecvFlag, 1, MPI_CXX_BOOL, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);

		MPI_Barrier(MPI_COMM_WORLD);

		if (recvFlag && (t->successor != t->inputSize && t->successor != SUCCESSOR_END))
			MPI_Recv(&recv, 2, MPI_INT, t->successor, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		else if (t->successor == t->inputSize && !recvFlag)
			MPI_Send(&send, 2, MPI_INT, status.MPI_SOURCE, 0, MPI_COMM_WORLD);
		else if (!recvFlag && (t->successor != t->inputSize && t->successor != SUCCESSOR_END))
			MPI_Sendrecv(&send, 2, MPI_INT, status.MPI_SOURCE, 0,
				&recv, 2, MPI_INT, t->successor, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		if (t->successor != SUCCESSOR_END && t->successor != t->inputSize)
			t->successor = recv[0];

		t->weight += recv[1];
		recvFlag = tmpRecvFlag;
		t->step = i;

		send[0] = t->successor;
		send[1] = t->weight;
		recv[0] = 0;
		recv[1] = 0;

		//cout << t->toString() << endl;
		MPI_Barrier(MPI_COMM_WORLD);
	}
}

void preorder(TData *t, char **argv)
{
	if (t->cpuID< t->inputSize - 1) {
		int position = t->inputSize - t->weight + 1;
		MPI_Send(&position, 1, MPI_INT, t->inputSize - 1, 0, MPI_COMM_WORLD);
	}

	if (t->cpuID == t->inputSize - 1) {
		char out[t->inputSize + 1] = {0};
		out[0] = argv[1][0];
		out[t->inputSize] = '\0';

		for (size_t i = 1; i < t->inputSize; i++) {
			int pos;
			MPI_Recv(&pos, 1, MPI_INT, i-1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

			out[pos-1] = argv[1][i];
		}

		cout << out << endl;
	}
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
	t.step = 0;

	//cout << t.toString() << endl;

	calculateSuffixSum(&t, t.cpuID == 0);
	preorder(&t, argv);

	MPI_Finalize();
	return EXIT_SUCCESS;
}
