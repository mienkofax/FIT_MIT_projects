#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>

#include <iostream>
#include <sstream>
#include <fstream>
using namespace std;
int main(void) {
	std::string host= "www.google.sk";
	int port_number = 80;
	char buffer[1024];

	int client_socket = 0; //socket
	struct sockaddr_in sin;
	struct hostent *hostPtr;

	// vytvorenie socketu
	if ((client_socket = socket(PF_INET, SOCK_STREAM, 0)) <= 0)
		printf("error");

	sin.sin_family = PF_INET; //protokol
	sin.sin_port = htons(port_number);

	//zistenie mena hostu
	if ((hostPtr = gethostbyname(host.c_str())) == NULL)
		printf("zly host");

	memcpy(&sin.sin_addr, hostPtr->h_addr, hostPtr->h_length);

	if (connect(client_socket, (struct sockaddr *) &sin, sizeof(sin)) != 0)
		printf("connect err");

		int pocetRecv;
		string sprava;
	while((pocetRecv = recv(client_socket, buffer, sizeof(buffer), 0) ) > 0 )
     {
       if(pocetRecv < 0)
       {
         cerr << "Chyba prijmana dat!" << endl;

       }
       sprava.append(buffer,pocetRecv);
       break;
     }
	 cout << buffer;





/*
	bzero((char *) &server_address, sizeof(server_address));
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);
	server_address.sin_port = htons((unsigned short) port_number);

	if (bind(server_))
*/
while (1);

	return 0;
}
