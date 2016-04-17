/**
 * @author Peter Tisovcik <xtisov00@fit.vutbr.cz
 * @description Jednoduchy komunikacia server <-> klient
 */
#include "common.h"
#include <signal.h>
#include <sys/wait.h>

/* Parsovanie argumentov */
int parseArg(TUrlData *args, int argc, char *argv[]) {
	/* Kontrola poctu argumentov */
	if (argc != 3)
		printErrMsg(E_ARGV);

	int c;
	while ((c = getopt(argc, argv, "p:")) != -1) {
		switch(c) {
			case 'p':
				if (args->port >= 0) return E_ARGV;

				//Parsovanie portu
				char *endptr;
				args->port = strtol(optarg, &endptr, 10);

				/* AK to nebolo cislo alebo ak je mimo rozsahu*/
				if ((*endptr) != '\0' || !(args->port >= 0 && args->port <= 65535))
					return E_ARGV;

				break;
			default:
				return E_ARGV;
				break;
		}
	}
	return 0;
}

/* Pripojenie klienta k serveru */
int connectServer(TUrlData url_data, int *server_socket, struct sockaddr_in *sin) {
	//vytvorenie socketu
	if (((*server_socket) = socket(PF_INET, SOCK_STREAM, 0)) < 0)
		return E_SOCK;

	sin->sin_family = PF_INET;
	sin->sin_port = htons(url_data.port);
	sin->sin_addr.s_addr  = INADDR_ANY;

	//bind
	if (bind((*server_socket), (struct sockaddr *)sin, sizeof(*sin)) < 0)
		return E_BIND;

	// spustenie pocuvania na danom porte
	if (listen(*server_socket, 10))
		return E_LISTEN;
	return 0;
}

/* Zisti nazov suboru zo spravy */
string msg_filename(string msg) {
	int i;

	//Najdenie informacie o nazve suboru
	i = msg.find("File");
	msg.erase(0, i+6);

	//Odstranenie zvysku spravy
	i = msg.find("\r");
	msg.erase(i);
	return msg;
}

/* Odoslanie suboru klientovi */
int send_file(string msg, int sock) {
	char *buff;
	string data, filename;
	int request;
	streampos size;

	//Nacitanie nazvu suboru a jeho otvorenie
	filename = msg_filename(msg);
	ifstream infile (filename.c_str(), ios::in | ios::binary);

	//Chyba v pripade problemu s otvorenim suboru
	if (!infile.is_open())
		return E_FILE;

	//Velkost suboru
	infile.seekg (0, infile.end);
	size = infile.tellg();
	infile.seekg (0, infile.beg);

	buff = new char[size];		    //inicializacia buffru
	bzero(buff, size);
	infile.read(buff, size);		//precitanie suboru do buffru
	infile.close();

	//Sprava o dlzke suboru, ktory sa bude posielat
	data = create_msg_text("Msg", DW_LEN);
	data += create_msg_text("Size", size);

	//Poslanie spravy o dlzke suboru
	if(send(sock, data.c_str(), strlen(data.c_str()), 0) < 0) {
		delete [] buff;
		return E_SEND;
	}

	//Nacitanie odpovedi od clienta
	data = read_msg(sock);
	request = msg_type(data);

	//Ak prislo ACK, posle sa subor inak chyba
	if (request == ACK) {
		if (send(sock, buff, size, 0 ) < 0) {
			delete [] buff;
			return E_SEND;
		}
		cout << "\tSend: " << filename << "\n\tSize: " << size << endl;
	} else {
		delete [] buff;
		return E_SEND;
	}

	delete [] buff;
	return 0;
}

/* Prijatie suboru od klienta */
int receive_file(string msg, int sock) {
	char *buff;
	int size, i;
	string data, filename;

	filename = msg_filename(msg); 	//Nacitanie nazvu suboru
	size = msg_size(msg);			//velkost suboru, ktory sa ma prijat
	buff = new char [size];     	//inicializacia buffru

	//Prevod stringu na cislo
	ostringstream ss; ss << size;

	data = data = create_msg_text("Msg", ACK);
	if(send(sock, data.c_str(), strlen(data.c_str()), 0) < 0) {
		delete [] buff;
		return E_SEND;
	}

	while ((i = read(sock, buff, size)) > 0) {
		(msg).append(buff, i);
		break;
	}
	cout << "\tReceive: " << filename << "\n\tSize: " << size << endl;

	//Ulozenie prijateho suboru
	FILE *f;
	if ((f = fopen(filename.c_str(), "wb+")) == NULL) {
		delete [] buff;
		return E_SAVE;
	}
	fwrite(buff, sizeof(char), size, f);
	fclose(f);

	//Potvrdenie nahratia suboru
	data = data = create_msg_text("Msg", ACK);
	if(send(sock, data.c_str(), strlen(data.c_str()), 0) < 0)
		return E_SEND;

	delete [] buff;
	return 0;
}

void exit_child(int n) {
	pid_t pid;
	int status;
	while((pid = waitpid(-1,&status,WNOHANG)) > 0);
	//cout << "Exiting\n";
}

int main(int argc, char *argv[]) {
	int welcome_socket, comm_socket, err;
	TUrlData args = {-1, ""};
	struct sockaddr_in sa;
	struct sockaddr_in sa_client;
	char str[INET_ADDRSTRLEN];

	/* Kontrola argumentov */
	if ((err = parseArg(&args, argc, argv)) != OK)
		printErrMsg(err);

	/* Bind server */
	if ((err = connectServer(args, &welcome_socket, &sa)) != OK)
		printErrMsg(err);

	signal(SIGCHLD, exit_child);

	while (1) {
		socklen_t len = sizeof(sa_client);
		if ((comm_socket = accept(welcome_socket, (struct sockaddr *)&sa_client, &len)) < 0)
			printErrMsg(E_ACCEPT);

		if (comm_socket > 0)
			cout << "con" << endl;

		int pid = fork();
		if (pid < 0)
			printErrMsg(E_FORK);

		if (pid == 0)	// new process to maintain client's requests:
		{
			int tmp_sock = comm_socket;
			int child_pid = getpid();
			close(welcome_socket);
			cout << ">>>New connection (maintained by" << child_pid << ")\n";

			/* Cakanie na poziadavku*/
			while(1) {
				string msg = read_msg(tmp_sock);
				int request = msg_type(msg);

				//Poziadavka na stiahnutie suboru
				if (request == 0) {
					if ((err = send_file(msg, tmp_sock)) != OK) {
						msg = create_msg_text("Msg", err);
						send(tmp_sock, msg.c_str(), strlen(msg.c_str()),0);
					}
				}
				else if (request == UPLOAD) {

					if ((err = receive_file(msg, tmp_sock)) != OK) {
						msg = create_msg_text("Msg", err);
						send(tmp_sock, msg.c_str(), strlen(msg.c_str()),0);
					}
				}
				else
					break;
			}

			cout << ">>>Connection closed (maintained by" << child_pid << ")\n";
			close(comm_socket);
			exit(EXIT_FAILURE);
		}
		else
			close(comm_socket);
	}
}
