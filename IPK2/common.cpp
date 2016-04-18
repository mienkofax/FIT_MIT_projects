#include <string>
#include <iostream>
#include "common.h"


/* Vypis chybovej spravy na stderr */
void printErrMsg(int err_code) {
	cerr << "Error: " << errMsg[err_code] << endl;
	exit(err_code);
}

/* Precita spravu od clienta a vrati jej textovu reprezentaciu */
string read_msg(int tmp_sock) {
	char buffer[BUFFER_SIZE];
	bzero(buffer, BUFFER_SIZE);
	int i = 0;
	string msg = "";
	size_t index;
	while ((i = read(tmp_sock, buffer, BUFFER_SIZE)) > 0) {
		(msg).append(buffer, i);
		break;
	}
	return msg;
}

/* Typ poziadavky */
int msg_type(string data) {
	int i;
	if (data.length() < 5)
		return 5;

	//Odstranenie retazca Msg: + medzera
	data.erase(0, 5);
	//Odstranenie vsetkeho okrem cisla poziadavky
	i = data.find("\r");
	data.erase(i);

	char *endptr;
	int number = strtol(data.c_str(), &endptr, 10);

	/* AK to nebolo cislo alebo ak je mimo rozsahu */
	if ((*endptr) != '\0')
		return E_UNKNOWN;

	return number;
}

/* Vytvori riadok spravy, ktory sa neskor posle */
string create_msg_text(string name, int data) {
	ostringstream ss; ss << data; //Prevod cisla na string
	return (name + ": " + ss.str() + "\r\n");
}
/* Vytvori riadok spravy, ktory sa neskor posle */
string create_msg_text(string name, string data) {
	return (name + ": " + data + "\r\n");
}

int msg_size(string msg) {
	int i;
	char *endptr;

	//Najdenie informacie o velkosti suboru
	i = msg.find("Size");
	msg.erase(0, i+6);

	//Odstranenie zvysku spravy
	i = msg.find("\r");
	msg.erase(i);

	i = strtol(msg.c_str(), &endptr, 10);

	/* AK to nebolo cislo alebo ak je mimo rozsahu */
	//if ((*endptr) != '\0')
	//	return E_UNKNOWN;

	return i;
}
