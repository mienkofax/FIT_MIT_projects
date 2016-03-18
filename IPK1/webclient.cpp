#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <stdlib.h>

#include <iostream>
#include <sstream>
#include <fstream>

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <sstream>

#include <cstring>
#include <unistd.h>


#include <sstream>

using namespace std;

#define BUFFER_SIZE 1024

/* info o url a serveru */
typedef struct Data {
	int port;
	string url;
	string path;
	string file;
	string http_ver; // HTTP/1.1 | HTTP/1.0
	string method_type; // GET | HEAD
} TUrlData;


/* Navratove kody */
enum code {
	OK,
	REDIR,
	E_SOCK,
	E_HOST,
	E_CONN,
	E_REQ,
	E_CLOSESOCK,
	E_NOTFOUND,
	E_SERVER,
	E_OTHER_ERR,
	E_NOT_SUPP,
	E_LINK,
	E_ARGV,
	E_UNKNOWN
};

/* Vypis chybovej spravy na stderr */
void printErrMsg(int err_code) {
	//cerr << msg;
	cout << "====Chyba====" << err_code << endl;
	exit(10);
}

/* Rozparsovanie vstupnej url adresy */
int parseURL(TUrlData *url_data, string tmp) {
	int index, index2;

	/* Odstranenie http */
	index = tmp.find("http");
	index2 = tmp.find("https");

	/* Ak sa nenachadza http v odkaze, tak chyba*/
	if (index < 0)
		return E_LINK;

	if (index2 < 0)
		tmp.erase(0, 7); // odstranenie http://
	else
		tmp.erase(0,8); // odstranenie https://

	/* Vyhladanie hostname a path */
	index = tmp.find(":");
	index2 = tmp.find("/");

	/* www.example.com:80 */
	if (index2 < index ) {
		url_data->url = tmp.substr(0, index);
		tmp.erase(0, index);

		/* :80 */
		if (index2 < 0 && index > 0) {
			stringstream(tmp.substr(1, index2-1)) >> url_data->port;
			tmp.erase(0, index2);
		}
	}
	/* www.example.com/link | www.example.com/ */
	else if (index < 0 && index2 > 0) {
		url_data->url = tmp.substr(0, index2);
		tmp.erase(0, index2);
	}
	/* www.example.com | www.example.com:80/ | www.example.com:80/link */
	else {
		url_data->url = tmp.substr(0, index);
		tmp.erase(0, index);

		/* :80/ */
		index = tmp.find("/");
		if (index >= 0) {
			stringstream(tmp.substr(1, index-1)) >> url_data->port;
			tmp.erase(0, index);
		}
	}

	/* Ulozenie path cesty */
	if (tmp.length())
		url_data->path = tmp;
	else
		url_data->path = "/";

	return 0;
}

int parseArg(TUrlData *url_data, int argc, char *argv[]) {

	if (argc < 2 || argc >3)
		return E_ARGV;

	if (argc == 2)
		parseURL(url_data, argv[1]);

	if (argc == 3)
		url_data->file = argv[2];

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
	if ((hostPtr = gethostbyname(url_data.url.c_str())) == NULL)
		return E_HOST;

	memcpy(&sin.sin_addr, hostPtr->h_addr, hostPtr->h_length);

	//pripojenie
	if (connect((*client_socket), (struct sockaddr *)&sin, sizeof(sin)) < 0)
		return E_CONN;

	return 0;
}

/* Odosle poziadavku na server */
int sendRequest(TUrlData url_data, int socket, string *s, int *chunked) {
	char buffer[BUFFER_SIZE];
	string req = "";
	int i = 0;

	memset (buffer, 0, sizeof(char)*BUFFER_SIZE);

	/* Vytvorenie hlavicky poziadavky */
	req.append(url_data.method_type + " " + url_data.path);
	req.append(" " + url_data.http_ver + "\r\n");
	req.append("Host: " + url_data.url + "\r\n");
	req.append("Connection: close\r\n");
	req.append("User-agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36 OPR/35.0.2066.92\r\n\r\n");

	/* Odoslanie poziadavky */
	if (write(socket, req.c_str(), req.size()) < 0)
		return E_REQ;

	/* Cakanie na odpoved */
	while ((i = read(socket, buffer, BUFFER_SIZE)) > 0)
		(*s).append(buffer, i);

	/* Zatvorenie socketu */
	if (close(socket) < 0)
		return E_CLOSESOCK;

	//kontrola ci su data chunked
	return 0;
}

int waitResponse(string msg) {
	msg.erase(0, 9);
	int i = msg.find("\n");
	msg.erase(i);

	/* Zdroj: https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html */
	if (msg.find("200") != string::npos) //OK
		return OK;
	else if (msg.find("301") != string::npos) //Move permanently
		return REDIR;
	else if (msg.find("302") != string::npos) //Found & Redirect/Move temporarily
		return REDIR;
	else if (msg.find("404") != string::npos) //Not Found
		return E_NOTFOUND;
	else if (msg.find("500") != string::npos) //Internal server ERR
		return E_SERVER;
	else if (msg.find("505") != string::npos) //Version not supported
		return E_NOT_SUPP;
	else
		return E_OTHER_ERR;

	return 0;
}

/* Spracovanie odkazu pre presmerovanie*/
int redirect(string data, TUrlData *url_data) {
	int index = data.find("Location:");
	data.erase(0, index+10);

	index = data.find("\n");
	data.erase(index);

	return parseURL(url_data, data);
}

/* Ulozenie dat do suboru */
int saveData(string msg) {
	ofstream file;
	file << msg;
	file.write(msg.c_str(), msg.length());
	file.close();

	return 0;
}

int main(int argc, char *argv[]) {
	int sock, chunked = 0;
	string msg = "";
	TUrlData url_data = {80, "", "/", "index.html", "HTTP/1.0", "GET"};
	int err = 0;
	int redir = 0;

	if ((err = parseArg(&url_data, argc, argv)) != OK)
		printErrMsg(err);

	while (redir < 5) {
		/* Pripojenie */

		if ((err = connectClient(url_data, &sock)) != OK)
			printErrMsg(err);

		// Odoslanie poziadavky
		if ((err = sendRequest(url_data, sock, &msg, &chunked)) != OK)
			printErrMsg(err);

		/* Defaultne odoslanie ako HTTP 1.1 v pripade chyby 505, pouzitie HTTP 1.0 */
		if ((err = waitResponse(msg)) == OK) {
			//pre 1.1 a zaroven vsetko v poriadkus

			//odstranenie hlavicky
			size_t index;
			if ((index = msg.find("\r\n\r\n")) != string::npos)
				msg.erase(0, index+4);

			saveData(msg);
			break;
		} else if (err == E_NOT_SUPP) {
			if ((err = waitResponse(msg)) != OK)
				printErrMsg(err);

			/* Nastavenie hlavicky pre HTTP/1.0 */
			url_data.http_ver = "HTTP/1.0";
		} else if (err == REDIR) {
			if ((err = redirect(msg, &url_data)) != OK)
				printErrMsg(err);

		} else
			printErrMsg(E_UNKNOWN);

		redir++;
		msg = "";
	}
}
//TODO dopisat help
//TODO dopisat chybove spravy
//TODO dorobit ukladanie
//TODO dorobit aby to bralo nazov suboru z url
