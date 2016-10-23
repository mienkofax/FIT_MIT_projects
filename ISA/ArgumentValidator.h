/**
 * @file ArgumentValidator.h
 * @Author Peter Tisovčík <xtisov00@stud.fit.vutbr.cz>
 * @date October, 2016
 */

#include <string>

/*
 * Trieda validuje vstupne data z retazca a vracia true/false, ci boli
 * zadane udaje daneho typu spravne alebo nie.
 */
class ArgumentValidator {
public:
	/*
	 * Kontrola ci bola zadana validna mac adresa. Kontroluje sa ci
	 * boli zadane len hexadecimalne znaky a dvojbodka na spravnych
	 * miestach v adrese.
	 * @param &value mac adresa
	 * @return true ak je adresa validna
	 */
	static bool macAddress(const std::string &value);

	/*
	 * Kontrola ci bola zadana validna IPv4 adresa.
	 * @param &value IPv4 adresa
	 * @return true ak je adresa validna
	 */
	static bool ipv4Address(const std::string &value);

	/*
	 * Kontrola ci bola zadana validna IPv6 adresa.
	 * @param &value IPv6 adresa
	 * @return true ak je adresa validna
	 */
	static bool ipv6Address(const std::string &value);

	/*
	 * Kontrola ci bolo zadane validne cislo portu. Kontroluje sa
	 * rozsah portov a ci to je vobec cislo.
	 * @param &value cislo portu
	 * @return true ak bolo zadane spravne cislo portu
	 */
	static bool port(const std::string &value);
};

/*
 * Trieda zjednocuje format akym sa ukladaju jednotlive typy do stringu.
 * Je to z dvovodu, aby sa pri porovnani predoslo chybam k nespravnemu
 * porovnaniu adries, ktore su totozne, ale su zapisane roznym stylom.
 * Je to nutne pouzit pri parsovani hodnot filtrov a pri uladani hodnot
 * vyparsovanych v zadanom protokole.
 * Jednoduchym prikladom je mac adresa ktora sa moze
 * zadat s velkymi pismenami do filtra ale v ramci programu sa vyparsuje
 * na linkovej vrstve ako adresa s malymi pismenami.
 */
class Normalization {
public:
	/*
	 * Vrati jednotny typ IPv4 adresy.
	 * @param &ip retazec obahujuci ip adresu, ktora sa ma normalizovat
	 * @return normalizovana IPv4 adresa
	 */
	static std::string getIPv4(const std::string &ip);

	/*
	 * Vrati sa jedotny typ IPv6 adresy.
	 * @param &ip retazec obasahujuci ip adresu, ktora sa ma normalizovat
	 * @return normalizovana IPv4 adresa
	 */
	static std::string getIPv6(const std::string &ip);

	/*
	 * Vrati sa jednotny typ MAC adresy. Vrati sa adresa s malymi pismenami.
	 * @param &mac retazec obsahujuci mac adresu, ktora sa ma normalizvoat
	 * @return normalizovana MAX adresa
	 */
	static std::string getMac(std::string mac);
};
