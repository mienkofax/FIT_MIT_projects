 /**
  * File:    pro.cpp
  * Date:    April 2018
  * Author:  Klara Necasova
  * Email:   xnecas24@stud.fit.vutbr.cz
  * Project: Implementation of preorder order assignment to vertexes - project 3 for PRL 
  */

#include <cmath>
#include <cstring>
#include <iostream> 
#include <mpi.h>  
  
#define TAG 0

using namespace std;

int getNextEdge(int myId, int numOfNodes, int numOfProcs) 
{
	int next;
	// finds next node in Euler path
	// the last node has no next node
	if (myId == numOfNodes) {
		next = -1;
	}
	// go to right half of tree
	else if (myId == numOfNodes - 1) {
		next = 1;
	}
	// go to parent from right side
	else if (myId >= numOfNodes - 1 && (myId == numOfProcs - 1 || (myId - (numOfNodes - 1)) % 2 == 1)) { 
		if ((myId - (numOfNodes - 1)) % 2 == 0)
			next = (myId - (numOfNodes - 1) - 2)/2 + numOfNodes - 1;
		else
			next = (myId - (numOfNodes - 1) - 3)/2 + numOfNodes - 1;
	}
	// go to right child from left side
	else if (myId >= numOfNodes - 1) {
		next = myId - (numOfNodes - 2);
	}
	// go to parent from down
	else if (myId + (numOfNodes - 1) >= numOfProcs - ceil((float)numOfNodes/2)) {
		next = myId + numOfNodes - 1;
	}
	// go to left child
	else if (myId < numOfNodes - 1 /*&& myId % 2 == 0*/) {
		next = 2 * myId + 2;
	}
	
	return next;
}

int getWeight(int myId, int numOfNodes)
{
	// 0 - reverse edge, 1 - forward edge
	if(myId >= 0 && myId <= numOfNodes - 2)
		return 1;
	return 0;
}

int countSuffixSum(int myId, int numOfNodes, int numOfProcs, int next)
{
	// TODO: delete allocated memory
	int *sendData = new int[2];
	int *receiveData = new int[2];
	int stopReceiving = 0;
	int stopReceivingTmp = 0;
	MPI_Status status;
	int weight;
	
	weight = getWeight(myId, numOfNodes);

	if (myId == 0) {
		stopReceiving = 1;
		stopReceivingTmp = 1;
	}
	sendData[0] = next;
	sendData[1] = weight;
	receiveData[0] = 0;
	receiveData[1] = 0;
	
	cout << "limit: " << ceil(log2((float)numOfNodes)) + 1 << endl;
	// counts suffix sum	
	for (int i = 0; i < (int)ceil(log2(numOfProcs)); i++) {
		// sends/receives stopReceiving flag, if stopReceiving == true, node will not receive any data
		cout << "myId: " << myId << " inside for: " << i << endl;
		if (stopReceiving == 1 && (next != numOfNodes && next != -1)) {
			cout << "11 myId: " << myId << " it: " << i << endl;
			MPI_Send(&stopReceiving, 1, MPI_INT, next, TAG, MPI_COMM_WORLD);
			cout << "myId: " << myId << " next: " << next << " data sent: " << stopReceiving << endl;
		}
		else if (next == numOfNodes && stopReceiving != 1) {
			cout << "12 myId: " << myId << " it: " << i << endl;
			MPI_Recv(&stopReceivingTmp, 1, MPI_INT, MPI_ANY_SOURCE, TAG, MPI_COMM_WORLD, &status);
			cout << "myId: " << myId << " next: " << status.MPI_SOURCE << " data recv: " << stopReceivingTmp << endl;
		}
		else if (next != numOfNodes && next != -1 && stopReceiving != 1) {
			cout << "13 myId: " << myId << " it: " << i << endl;
			MPI_Sendrecv(&stopReceiving, 1, MPI_INT, next, TAG, &stopReceivingTmp, 1, MPI_INT, MPI_ANY_SOURCE, TAG, MPI_COMM_WORLD, &status);
			cout << "myId: " << myId << " next: " << next << " previous: "<< status.MPI_SOURCE << " data sent: " << stopReceiving << " data recv:" << stopReceivingTmp << endl;
		}
		
		MPI_Barrier(MPI_COMM_WORLD);

		// sends/receives next node and weight
		if (stopReceiving == 1 && (next != numOfNodes && next != -1)) {
			cout << "21 myId: " << myId << " it: " << i << endl;;
			MPI_Recv(receiveData, 2, MPI_INT, next, TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			cout << "myId: " << myId << " previous: " << next << " receiveData[0]: " << receiveData[0] << " receiveData[1]: " << receiveData[1] << endl;
		}
		else if (next == numOfNodes && stopReceiving != 1) {
			cout << "22 myId: " << myId << " it: " << i << endl;
			MPI_Send(sendData, 2, MPI_INT, status.MPI_SOURCE, TAG, MPI_COMM_WORLD);
			cout << "myId: " << myId << " next: " << status.MPI_SOURCE << " sendData[0]: " << sendData[0] << " sendData[1]: " << sendData[1] << endl;
		}
		else if (next != numOfNodes && next != -1 && stopReceiving != 1) {
			cout << "23 myId: " << myId << " it: " << i << endl;
			MPI_Sendrecv(sendData, 2, MPI_INT, status.MPI_SOURCE, TAG, receiveData, 2, MPI_INT, next, TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			cout << "myId: " << myId << " next: " << status.MPI_SOURCE << " sendData[0]: " << sendData[0] << " sendData[1]: " << sendData[1] << " previous: " << next << " receiveData[0]: " << receiveData[0] << " receiveData[1]: " << receiveData[1] << endl;
		}
		// updates stopReceiving flag, weight and next node
		stopReceiving = stopReceivingTmp;
		if (next != -1 && next != numOfNodes)
			next = receiveData[0];
		weight += receiveData[1];
		cout << "myId: " << myId << " next: " << next <<  " suffixSum: " << weight << endl;
		
		// stores updated next node and weight that will send in next iteration
		sendData[0] = next;
		sendData[1] = weight;
		// cleans buffer for data receiving
		receiveData[0] = 0;
		receiveData[1] = 0;
	
		MPI_Barrier(MPI_COMM_WORLD);
	}
	
	delete[] sendData;
	delete[] receiveData;
	return weight;
}

void countAndPrintPreorderPosition(int myId, int numOfNodes, int weight, char *argv[])
{
	int master = numOfNodes - 1;
	// counts position in preorder
	if (myId < numOfNodes - 1) {
		int preorderPosition = numOfNodes - weight + 1;
		cout << "myId: " << myId << " pos: " << preorderPosition << endl;
		MPI_Send(&preorderPosition, 1, MPI_INT, master, TAG, MPI_COMM_WORLD);
	}

    // root prints node order
	if (myId == master) {
		char *output = new char[numOfNodes];
		//char* output = (char*)malloc(numOfNodes*sizeof(char));
		output[0] = argv[1][0];
		for (int i = 0; i < numOfNodes - 1; i++) {
			int position;
            MPI_Recv(&position, 1, MPI_INT, i, TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			cout << "proces: " << i << " position: " << position - 1 << endl;
            output[position - 1] = argv[1][i+1];
		cout << "out: " << output[position - 1] << endl; 
        }
		int i;
        for (i = 0; i < numOfNodes - 1; i++) {
            cout << output[i];
        }
        cout << output[i] << endl;
		delete[] output;
	}
}
  
int main(int argc, char *argv[])
{
	if (argc != 2) {
		cerr << "invalid number of arguments" << endl;
		return EXIT_FAILURE;
	}
	
	int myId;
	int numOfProcs;	
	int next;
	int weight;
	
	int numOfNodes = strlen(argv[1]);
	//cout << "number of nodes: " << numOfNodes << endl;

    // MPI initialization     
    MPI_Init(&argc, &argv);
    // number of running processes
    MPI_Comm_size(MPI_COMM_WORLD, &numOfProcs);
    // id of my process
    MPI_Comm_rank(MPI_COMM_WORLD, &myId);
	//cout << "number of processes: " << numOfProcs << endl;
	
	// prints node order of tree with one node 
	if (numOfNodes == 1 && myId == 0) {
		cout << argv[1][0] << endl;
		MPI_Finalize();
		return EXIT_SUCCESS;
	}
	// prints node order of tree with two nodes 
	if (numOfNodes == 2 && myId == 0) {
		cout << argv[1][0] << argv[1][1] << endl;
		MPI_Finalize();
		return EXIT_SUCCESS;
	}
	// terminates processes in case of tree with one or two nodes
	if (numOfNodes == 1 || numOfNodes == 2) {
		MPI_Finalize();
		return EXIT_SUCCESS;
	}
	// get next edge in Euler path
	next = getNextEdge(myId, numOfNodes, numOfProcs);
	cout << "myId: " << myId << " next: " << next << endl;	
	// counts suffix sum
	weight = countSuffixSum(myId, numOfNodes, numOfProcs, next);
	// counts preorder position and prints it
	countAndPrintPreorderPosition(myId, numOfNodes, weight, argv);

	MPI_Finalize();
	
	return EXIT_SUCCESS;
}