/**
 * Peter Tisovckk (xtisov00@stud.fit.vutbr.cz)
 */

#include <cmath>
#include <fstream>
#include <string>

#include <mpi.h>

using namespace std;

/** 
 * Struktura pro ulozeni delky vstupniho retezce, poctu procesoru, ID procesoru,
 * jeho vahy a naslednika.
 */ 
struct TData {
	int inputLength;
	int cpuCount;
	int cpuID;
	int successor;
	int weight;

	string toString() const
	{
		string data;

		data += "inputLength: " + to_string(inputLength);
		data += ", ";

		data += "cpuCount: " + to_string(cpuCount);
		data += ", ";

		data += "cpuID: " + to_string(cpuID);
		data += ", ";

		data += "successor: " + to_string(successor);
		data += ", ";

		data += "weight: " + to_string(weight);
		data += ", ";

		return data;
	}
};

/**
 * Vypocita vahu hrany.
 * Zpetna hrana - 0, dopredna hrana - 1.
 */
int calculateWeight(TData *t)
{
	return t->cpuID < t->inputLength - 1;
}

/**
 * Vypocita naslednika (nasledujici hranu) v Eulerove ceste.
 */
static const int SUCCESSOR_END = -1;
int calculateSuccessor(TData *t)
{
	// 2 uzly
	if (t->inputLength == 2) {
		// naslednikem 0 je 1
		if (t->cpuID == 0)
			return 1;
		// naslednikem 1 je -1 (zadny naslednik)
		if (t->cpuID == 1)
			return SUCCESSOR_END;
	}
	
	// od leveho potomka korene k pravemu
	if(t->cpuID + 1 == t->inputLength)
		return 1;
	
	// posledni hrana nema zadneho naslednika 
	if(t->cpuID == t->inputLength)
		return SUCCESSOR_END;

	// od praveho potomka k rodici
	if (t->cpuID + 1 >= t->inputLength && (t->cpuID + 1 == t->cpuCount || (t->cpuID + 1 - t->inputLength) % 2 == 1)) {
		if ((t->cpuID + 1 - t->inputLength) % 2 != 0)
			return (t->cpuID + 1 - t->inputLength - 3) / 2 + t->inputLength - 1;	
		return (t->cpuID + 1 - t->inputLength - 2) / 2 + t->inputLength - 1;
	}

	// od leveho potomka k pravemu potomkovi
	if (t->cpuID + 1 >= t->inputLength)
		return t->cpuID - t->inputLength + 2;

	// od sebe (dopradna hrana) zpet k sobe (zpetna hrana)
	if (t->cpuID - 1 + t->inputLength >= t->cpuCount - ceil(t->inputLength / 2.0))
		return t->cpuID + t->inputLength - 1;

	// k levemu potomkovi
	if (t->cpuID + 1 < t->inputLength)
		return 2 * t->cpuID + 2;
}

/**
 * Vypocita suffix sum.
 */
void calculateSuffixSum(TData *t, bool recvFlag)
{
	int send[2] = {t->weight, t->successor};
	int recv[2] = {0};
	MPI_Status status;
	bool tmpRecvFlag = recvFlag;

	// 2 a mene uzlu, vaha uzlu zustane nezmenena
	if (t->inputLength <= 2)
		return;

	// vypocet suffix sum
	const int limit = static_cast<int>(ceilf(log2(t->cpuCount)));
	for (size_t i = 0; i < limit; i++) {
		// odeslani priznaku recvFlag, prijem vahy naslednika a jeho naslednika 
		if ((t->successor != t->inputLength && t->successor != SUCCESSOR_END) && recvFlag) {
			MPI_Send(&recvFlag, 1, MPI_CXX_BOOL, t->successor, 0, MPI_COMM_WORLD);
			MPI_Barrier(MPI_COMM_WORLD);
			MPI_Recv(&recv, 2, MPI_INT, t->successor, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}
		// prijem priznaku recvFlag, odeslani vahy naslednika a jeho naslednika
		else if (t->successor == t->inputLength && !recvFlag) {
			MPI_Recv(&tmpRecvFlag, 1, MPI_CXX_BOOL, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
			MPI_Barrier(MPI_COMM_WORLD);
			MPI_Send(&send, 2, MPI_INT, status.MPI_SOURCE, 0, MPI_COMM_WORLD);
		}
		// odeslani a prijem priznaku recvFlag, pote vahy naslednika a jeho naslednika
		else if (!recvFlag && (t->successor != t->inputLength && t->successor != SUCCESSOR_END)) {
			MPI_Sendrecv(&recvFlag, 1, MPI_CXX_BOOL, t->successor, 0,
				&tmpRecvFlag, 1, MPI_CXX_BOOL, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &status);
			MPI_Barrier(MPI_COMM_WORLD);
			MPI_Sendrecv(&send, 2, MPI_INT, status.MPI_SOURCE, 0,
				&recv, 2, MPI_INT, t->successor, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}
		else {
			MPI_Barrier(MPI_COMM_WORLD);
		}

		// ulozeni prijatych hodnot
		recvFlag = tmpRecvFlag;
		t->weight += recv[0];
		if (t->successor != SUCCESSOR_END && t->successor != t->inputLength)
			t->successor = recv[1];
		send[0] = t->weight;
		send[1] = t->successor;
		// vynulovani pole pro prijem dat
		recv[0] = 0;
		recv[1] = 0;

		MPI_Barrier(MPI_COMM_WORLD);
	}
}

/**
 * Vypocita pozici v preorder pruchodu.
 */
void calculatePrintPreorder(TData *t, char *argv)
{
    int pos;
	// v pripade dopredne hrany - vypocet pozice v preorder pruchodu a jeji odeslani (pozici neposila procesor s ID 0)
	if (t->cpuID + 1 < t->inputLength ) {
		pos = t->inputLength - t->weight + 1;
        	if (t->cpuID != 0)  
			MPI_Send(&pos, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
	}

	// procesor s ID 0 prijme pozice a vypise uzly (znaky vstupniho retezce) "po preorder pruchodu"
	if (t->cpuID == 0) {
		char out[t->inputLength + 1] = {0};
		// prvnim uzlem je koren
		out[0] = argv[0];
		out[t->inputLength] = '\0';

		if (t->inputLength == 0)
			return;

		if (t->inputLength == 1) {
			cout << argv[0] << endl;
			return;
		}
		size_t i;
		for (i = 1; i < t->inputLength - 1; i++) {
			int recvPos;
			MPI_Recv(&recvPos, 1, MPI_INT, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			out[recvPos - 1] = argv[i + 1];
		}
		// ulozeni pozice uzlu 0
		out[pos - 1] = argv [1];
		
		for (i = 0; i < t->inputLength; i++)
			cout << out[i];
		cout << out[i] << endl;
	}
}

/**
 * Hlavni program.
 */
int main(int argc, char *argv[])
{
	TData t;

	// inicializace MPI
	MPI_Init(&argc, &argv);
	// pocet procesoru
	MPI_Comm_size(MPI_COMM_WORLD, &t.cpuCount);
	// ID procesoru
	MPI_Comm_rank(MPI_COMM_WORLD, &t.cpuID);

	// zjisteni delky vstupniho retezce (poctu uzlu)
	t.inputLength = strlen(argv[1]);
	// vypocitani naslednika hrany
	t.successor = calculateSuccessor(&t);
	//vypocitani vahy hrany
	t.weight = calculateWeight(&t);
	// vypocitani suffix sum hrany
	calculateSuffixSum(&t, t.cpuID == 0);
	// vypocitani a vypis preorder poradi uzlu
	calculatePrintPreorder(&t, argv[1]);

	MPI_Finalize();
	return EXIT_SUCCESS;
}
