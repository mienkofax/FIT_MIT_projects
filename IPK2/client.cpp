/**
 * @author Peter Tisovcik <xtisov00@fit.vutbr.cz
 * @description Jednoduchy komunikacia server <-> klient
 */
#include "common.h"

/* Parsovanie argumentov */
int parseArg(TUrlData *args, int argc, char *argv[]) {
	/* Kontrola poctu argumentov */
	if (argc != 7 )
		printErrMsg(E_ARGV);

	int c;
	while ((c = getopt(argc, argv, "h:p:u:d:")) != -1) {
		switch(c) {
			case 'h':
				if (args->hostname != "") return E_ARGV;

				//Parsovanie url
				args->hostname = optarg;
				break;
			case 'p':
				if (args->port >= 0) return E_ARGV;

				//Parsovanie portu
				char *endptr;
				args->port = strtol(optarg, &endptr, 10);

				/* AK to nebolo cislo alebo ak je mimo rozsahu*/
				if ((*endptr) != '\0' || !(args->port >= 0 && args->port <= 65535))
					return E_ARGV;

				break;
			case 'u':
				if (args->upload || args->download) return E_ARGV;

				args->upload = true;
				args->filename = optarg;
				break;
			case 'd':
				if (args->upload || args->download) return E_ARGV;

				args->download = true;
				args->filename = optarg;
				break;
			default:
				return E_ARGV;
				break;
		}
	}
	return 0;
}

/* Pripojenie klienta k serveru */
int connectClient(TUrlData url_data, int *client_socket) {
	struct sockaddr_in sin;
	struct hostent *hostPtr;

	//vytvorenie socketu
	if (((*client_socket) = socket(PF_INET, SOCK_STREAM, 0)) < 0)
		return E_SOCK;

	sin.sin_family = PF_INET;
	sin.sin_port = htons(url_data.port);

	//preklad nazvu na ip
	if ((hostPtr = gethostbyname(url_data.hostname.c_str())) == NULL)
		return E_HOST;

	memcpy(&sin.sin_addr, hostPtr->h_addr, hostPtr->h_length);

	//pripojenie
	if (connect((*client_socket), (struct sockaddr *)&sin, sizeof(sin)) < 0)
		return E_CONN;

	return 0;
}

/* Odoslanie suboru na server */
int send_file(int sock, TUrlData *args) {
	char *buff;
	int size, request;
	string data;

	//Otvorneie suboru
	ifstream inFile (args->filename.c_str(), ios::in|ios::binary|ios::ate);
	size = inFile.tellg();

	//Chyba v pripade problemu s otvorenim suboru
	if (!inFile.is_open())
		return E_FILE;

	inFile.seekg (0, ios::beg);     //presun pointra na zaciatok suboru
	buff = new char [size];     //inicializacia buffru
	inFile.read(buff, size);     //precitanie suboru do buffru
	inFile.close();

	//Prevod cisla na string
	ostringstream ss; ss << size;

	//Informacia pre server o posielanom subore
	data = create_msg_text("Msg", UPLOAD);
	data += create_msg_text("Size", size);
	data += create_msg_text("File", args->filename);

	//Poslanie informacie o posielanom subore
	if (send(sock, data.c_str(), data.size(), 0) < 0 ) {
		delete [] buff;
		return E_SEND;
	}

	//Nacitanie odpovedi od clienta
	data = read_msg(sock);
	request = msg_type(data);

	//Ak prislo potvrdenie tak sa poslu udaje
	if (request == ACK) {
		if (send(sock, buff, strlen(buff), 0 ) < 0) {
			delete [] buff;
			return E_SEND;
		}
		cout << "Send: "<< args->filename << endl << "Size: " << size << endl;
	} else {
		delete [] buff;
		return E_ACK;
	}

	//Potvrdenie ulozenia suboru
	//Nacitanie odpovedi od clienta
	data = read_msg(sock);
	request = msg_type(data);
	if (request != ACK) {
		delete [] buff;
		return request;
	}

	delete [] buff;
	return 0;
}

/* Prijatie suboru od serveru */
int receive_file(int sock, TUrlData *args) {
	string msg;
	char *buff;
	int err, size;

	//Sprava na odoslanie suboru
	msg = create_msg_text("Msg", DOWNLOAD);
	msg += create_msg_text("File", args->filename);

	//Odoslanie spravy
	if (write(sock, msg.c_str(), msg.size()) < 0 )
		return E_CONN;

	//Velkost prijateho suboru
	msg = read_msg(sock);
	size = msg_size(msg);

	//Kontrola ci prisla spravna sprava ak nie, prisla sprava s kodom chyby
	if ((err = msg_type(msg)) != DW_LEN)
		return err;

	//Potvrdenie prijatia dlzky suboru
	msg = create_msg_text("Msg", ACK);
	if (write(sock, msg.c_str(), msg.size()) < 0 )
		return E_CONN;

	//Prijatie dat
	buff = new char [size];
	bzero(buff, size);
	msg = recv(sock, buff, size, 0);

	//Ulozenie prijateho suboru
	FILE *f;
	if ((f = fopen((args->filename).c_str(), "wb+")) == NULL) {
		delete [] buff;
		return E_FILE;
	}
	fwrite(buff, sizeof(char), size, f);
	fclose(f);

	cout << "Receive: " << args->filename << endl << "Size: " << size << endl;

	delete [] buff;
	return 0;
}

int main(int argc, char *argv[]) {
	TUrlData args = {-1, "", false, false, ""};
	int sock, err = 0;

	/* Kontrola argumentov */
	if ((err = parseArg(&args, argc, argv)) != OK)
		printErrMsg(err);

	/* Pripojenie clienta na server */
	if ((err = connectClient(args, &sock)) != OK)
		printErrMsg(err);

	if (args.upload) {
		//Poslanie suboru na server
		if ((err = send_file(sock, &args)) != OK)
			printErrMsg(err);
	}
	else if (args.download) {
		//Prijatie suboru od servera
		if ((err = receive_file(sock, &args)) != OK)
			printErrMsg(err);
	}
	else {
		printErrMsg(E_ARGV);
	}
}
