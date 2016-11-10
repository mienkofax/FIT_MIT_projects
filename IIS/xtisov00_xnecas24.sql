-- Projekt predmetu ISS
-- Autori:
--		Klara Necasova (xnecas24)
--		Peter Tisovcik (xtisov00)
-- Datum: November 2016
drop database lekarna;
CREATE DATABASE lekarna CHARACTER SET utf8 COLLATE utf8_general_ci;
USE lekarna;

-- dodavatele
CREATE TABLE dodavatele
(
	ID_dodavatele INTEGER NOT NULL AUTO_INCREMENT,
	nazev_dodavatele VARCHAR(128) NOT NULL,
	ulice VARCHAR(128),
	mesto VARCHAR(128),
	PSC VARCHAR(5),
	zeme VARCHAR(128),
	telefonni_cislo VARCHAR(20),
	email VARCHAR(128),
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT PK_dodavatele PRIMARY KEY (ID_dodavatele)
);

-- pobocky
CREATE TABLE pobocky
(
	ID_pobocky INTEGER NOT NULL AUTO_INCREMENT,
	nazev_pobocky VARCHAR(128) NOT NULL,
	ulice VARCHAR(128),
	mesto VARCHAR(128),
	PSC VARCHAR(5),
	telefonni_cislo VARCHAR(20),
	email VARCHAR(128),
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT PK_pobocky PRIMARY KEY (ID_pobocky)
);

-- uzivatele
CREATE TABLE uzivatele
(
	ID_uzivatele INTEGER NOT NULL AUTO_INCREMENT,
	login VARCHAR(128) NOT NULL,
	heslo VARCHAR(128) NOT NULL,
	jmeno VARCHAR(128),
	prijmeni VARCHAR(128),
	opravneni ENUM('member', 'seller', 'admin') DEFAULT 'member',
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

	UNIQUE (login),
	CONSTRAINT PK_zamestnanci PRIMARY KEY (ID_uzivatele)
);

-- leky
CREATE TABLE leky
(
	ID_leku INTEGER NOT NULL AUTO_INCREMENT,
	nazev_leku VARCHAR(128) NOT NULL,
	cena FLOAT(6, 2) NOT NULL,
	doplatek FLOAT(6, 2),
	hradene enum('hradene', 'nehradene', 'doplatok') NOT NULL, -- 0 - hradene , 1 - nehradene
	typ_leku BOOLEAN NOT NULL, -- 0 - bez predpisu, 1 - na predpis
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT PK_leky PRIMARY KEY (ID_leku)
);

-- pojistovny
CREATE TABLE pojistovny
(
	ID_pojistovny INTEGER NOT NULL AUTO_INCREMENT,
	nazev_pojistovny VARCHAR(128) NOT NULL,
	ulice VARCHAR(128),
	mesto VARCHAR(128),
	PSC VARCHAR(5),
	zeme VARCHAR(128),
	telefonni_cislo VARCHAR(20) NOT NULL,
	email VARCHAR(128) NOT NULL,
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT PK_pojistovny PRIMARY KEY (ID_pojistovny)
);

-- zakaznici
CREATE TABLE zakaznici
(
	ID_zakaznika INTEGER NOT NULL AUTO_INCREMENT,
	rodne_cislo VARCHAR(11) NOT NULL,
	jmeno VARCHAR(128),
	prijmeni VARCHAR(128),
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT PK_zakaznici PRIMARY KEY (ID_zakaznika)
);

-- rezervace_leku
CREATE TABLE rezervace_leku
(
	ID_rezervace INTEGER NOT NULL AUTO_INCREMENT,
	stav_rezervace ENUM('prijata', 'rozpracovana', 'pripravena', 'dokoncena') DEFAULT 'prijata',
	ID_pobocky INTEGER NOT NULL,
	ID_zakaznika INTEGER NOT NULL,
	date_time datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,

	CONSTRAINT PK_rezervace PRIMARY KEY (ID_rezervace)
);

CREATE TABLE dodavatel_pobocka
(
	ID_dodavatele INTEGER NOT NULL,
	ID_pobocky INTEGER NOT NULL,

	CONSTRAINT PK_dodavatel_pobocka PRIMARY KEY (ID_dodavatele, ID_pobocky)
);

CREATE TABLE pobocka_zamestnanec
(
	ID_pobocky INTEGER NOT NULL,
	ID_uzivatele INTEGER NOT NULL,

	CONSTRAINT PK_pobocka_zamestnanec PRIMARY KEY (ID_pobocky, ID_uzivatele)
);

CREATE TABLE pobocka_lek
(
	ID_pobocky INTEGER NOT NULL,
	ID_leku INTEGER NOT NULL,
	pocet_na_sklade INTEGER NOT NULL,
	pocet_prodanych INTEGER NOT NULL,

	CONSTRAINT PK_pobocka_lek PRIMARY KEY (ID_pobocky, ID_leku)
);

CREATE TABLE lek_pojistovny
(
	ID_pojistovny INTEGER NOT NULL,
	ID_leku INTEGER NOT NULL,

	CONSTRAINT PK_lek_pojistovny PRIMARY KEY (ID_pojistovny, ID_leku)
);

CREATE TABLE rezervace_leku_lek
(
	ID_rezervace INTEGER NOT NULL,
	ID_leku INTEGER NOT NULL,

	CONSTRAINT PK_rezervace_leku_lek PRIMARY KEY (ID_rezervace, ID_leku)
);

-- pobocky - dodavatele
ALTER TABLE dodavatel_pobocka ADD CONSTRAINT FK_dodavatel_pobocka1 FOREIGN KEY (ID_pobocky)
	REFERENCES pobocky (ID_pobocky) ON DELETE CASCADE;
ALTER TABLE dodavatel_pobocka ADD CONSTRAINT FK_dodavatel_pobocka2 FOREIGN KEY (ID_dodavatele)
	REFERENCES dodavatele (ID_dodavatele) ON DELETE CASCADE;

-- pobocky - zamestnanci
ALTER TABLE pobocka_zamestnanec ADD CONSTRAINT FK_pobocka_zamestnanec1 FOREIGN KEY (ID_pobocky)
	REFERENCES pobocky (ID_pobocky) ON DELETE CASCADE;
ALTER TABLE pobocka_zamestnanec ADD CONSTRAINT FK_pobocka_zamestnanec2 FOREIGN KEY (ID_uzivatele)
	REFERENCES uzivatele (ID_uzivatele) ON DELETE CASCADE;

-- pobocky - leky
ALTER TABLE pobocka_lek ADD CONSTRAINT FK_pobocka_lek1 FOREIGN KEY (ID_pobocky)
	REFERENCES pobocky (ID_pobocky) ON DELETE CASCADE;
ALTER TABLE pobocka_lek ADD CONSTRAINT FK_pobocka_lek2 FOREIGN KEY (ID_leku)
	REFERENCES leky (ID_leku) ON DELETE CASCADE;

-- rezervace_leku - leky
ALTER TABLE rezervace_leku_lek ADD CONSTRAINT FK_rezervace_leku_lek1 FOREIGN KEY (ID_rezervace)
	REFERENCES rezervace_leku (ID_rezervace) ON DELETE CASCADE;
ALTER TABLE rezervace_leku_lek ADD CONSTRAINT FK_rezervace_leku_lek2 FOREIGN KEY (ID_leku)
	REFERENCES leky (ID_leku) ON DELETE CASCADE;

-- leky - pojistovny
ALTER TABLE lek_pojistovny ADD CONSTRAINT FK_lek_pojistovny1 FOREIGN KEY (ID_leku)
	REFERENCES leky (ID_leku) ON DELETE CASCADE;
ALTER TABLE lek_pojistovny ADD CONSTRAINT FK_lek_pojistovny2 FOREIGN KEY (ID_pojistovny)
	REFERENCES pojistovny (ID_pojistovny) ON DELETE CASCADE;

-- rezervace_leku
ALTER TABLE rezervace_leku ADD CONSTRAINT FK_pobocky FOREIGN KEY (ID_pobocky)
	REFERENCES pobocky (ID_pobocky) ON DELETE CASCADE;
ALTER TABLE rezervace_leku ADD CONSTRAINT FK_zakaznika FOREIGN KEY (ID_zakaznika)
	REFERENCES zakaznici (ID_zakaznika) ON DELETE CASCADE;

-- pobocky
INSERT INTO pobocky
VALUES(1, 'Lékárna Salvia', 'Banskobystrická 3', 'Brno', 62100, '541226066', 'lekarna.salvia@email.cz', CURRENT_TIMESTAMP);
INSERT INTO pobocky
VALUES(2, 'Dr. Max LÉKÁRNA', 'Bašty 5', 'Brno', 60200, '542213531', 'brno.basty@drmax.cz', CURRENT_TIMESTAMP+1);
INSERT INTO pobocky
VALUES(3, 'Novolékárna Líšeň', 'Bednaříkova 23', 'Brno', 62800, '544210084', 'lisen.brno@novolekarna.cz', CURRENT_TIMESTAMP+2);
INSERT INTO pobocky
VALUES(4, 'AVENTA', 'Česká 8', 'Brno', 60200, '542215189', 'aventa@volny.cz', CURRENT_TIMESTAMP+3);
INSERT INTO pobocky
VALUES(5, 'Lékárna FORTE', 'Cimburkova 45', 'Brno', 61200, '541246292', 'lekarna.forte@seznam.cz', CURRENT_TIMESTAMP+4);
INSERT INTO pobocky
VALUES(6, 'Lékárna Dobrovského', 'Dobrovského 21', 'Brno', 61200, '549248721', 'lekarna.dobrovskeho@seznam.cz', CURRENT_TIMESTAMP+5);
INSERT INTO pobocky
VALUES(7, 'MEDEA', 'Ečerova 39', 'Brno', 63500, '546215061', 'lekarna@medeabrno.eu', CURRENT_TIMESTAMP+6);
ALTER TABLE pobocky AUTO_INCREMENT = 8;

-- uzivatele
INSERT INTO uzivatele
VALUES(1, 'xpokor00', ' ', 'Jana', 'Pokorná', 'admin', CURRENT_TIMESTAMP);
INSERT INTO uzivatele
VALUES(2, 'xmatou05', ' ', 'Petr', 'Matoušek', 'member', CURRENT_TIMESTAMP+1);
INSERT INTO uzivatele
VALUES(3, 'xmlynar33', ' ', 'Jan', 'Mlynář', 'member', CURRENT_TIMESTAMP+2);
INSERT INTO uzivatele
VALUES(4, 'xbradac28', ' ', 'Kateřina', 'Bradáčová', 'member', CURRENT_TIMESTAMP+3);
INSERT INTO uzivatele
VALUES(5, 'xkubin09', ' ', 'Lucie', 'Kubínová', 'member', CURRENT_TIMESTAMP+4);
INSERT INTO uzivatele
VALUES(6, 'xsebes25', ' ', 'Martin', 'Šebestík', 'member', CURRENT_TIMESTAMP+5);
INSERT INTO uzivatele
VALUES(7, 'peto', '$2y$10$h8vmMU0yHJ4jFOpfxrZO0eIW3qgnRFXsdi4G9DKzXaHuo9OLPuPJu', '', '', 'admin', CURRENT_TIMESTAMP+6);
ALTER TABLE uzivatele AUTO_INCREMENT = 8;

-- dodavatele
INSERT INTO dodavatele
VALUES(1, 'A-Pharma s.r.o.', 'U Albrechtova vrchu 2', 'Praha', '15000', 'Česká republika', '603773034', 'lahovska@a-pharma.cz', CURRENT_TIMESTAMP);
INSERT INTO dodavatele
VALUES(2, 'AVICEL s.r.o.', 'Haštalská 29', 'Praha', '11000', 'Česká republika', '731444131', 'vanzura@pechmann@aveniie.cz', CURRENT_TIMESTAMP+1);
INSERT INTO dodavatele
VALUES(3, 'Balmir s.r.o.', 'Vrázova 5', 'Praha', '15000', 'Česká republika', '602454757', 'info@balmir.cz', CURRENT_TIMESTAMP+2);
INSERT INTO dodavatele
VALUES(4, 'GlucoPharma s.r.o.', 'Tvrdého 8', 'Praha', '19000', 'Česká republika', '777555113', 'david.valek@glucopharma.cz', CURRENT_TIMESTAMP+3);
INSERT INTO dodavatele
VALUES(5, 'HARTMANN - RICO a.s.', 'Masarykovo náměstí 77', 'Veverská Bítýška', '66471', 'Česká republika', '549456330', 'eva.butorova@hartmann.info', CURRENT_TIMESTAMP+4);
INSERT INTO dodavatele
VALUES(6, 'PILULKA, s.r.o.', 'Ponávka 2', 'Brno', '66471', 'Česká republika', '603173579', 'ismidova@click.cz', CURRENT_TIMESTAMP+5);
INSERT INTO dodavatele
VALUES(7, 'URSAPHARM SPOL. s.r.o.', 'Černokostelecká 52', '�?íčany', '25101', 'Česká republika', '323622750', 'milos.hermanek@ursapharm.cz', CURRENT_TIMESTAMP+6);
ALTER TABLE dodavatele AUTO_INCREMENT = 8;


-- leky
-- leky bez predpisu
INSERT INTO leky
VALUES(1, 'Ibalgin Rapid', 140, NULL, 'nehradene', 0, CURRENT_TIMESTAMP);
INSERT INTO leky
VALUES(2, 'ACC LONG', 165, NULL, 'nehradene', 0, CURRENT_TIMESTAMP+4);
INSERT INTO leky
VALUES(3, 'ASPIRIN', 165, NULL, 'nehradene', 0, CURRENT_TIMESTAMP+8);

-- leky na predpis
INSERT INTO leky
VALUES(4, 'Pamycon', 60, NULL, 'hradene', 1, CURRENT_TIMESTAMP+10);
INSERT INTO leky
VALUES(5, 'Klacid', 266.20, NULL, 'hradene', 1, CURRENT_TIMESTAMP+11);
INSERT INTO leky
VALUES(6, 'Lipobase Repair', 166, 47.52, 'doplatok', 1, CURRENT_TIMESTAMP+12);
INSERT INTO leky
VALUES(7, 'Clarinase Repetabs', 140.50, 30.40, 'doplatok', 1, CURRENT_TIMESTAMP+13);
ALTER TABLE leky AUTO_INCREMENT = 8;

-- zakaznici
INSERT INTO zakaznici
VALUES(1, '761222/1001', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP);
INSERT INTO zakaznici
VALUES(2, '770919/6407', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP+1);
INSERT INTO zakaznici
VALUES(3, '801101/4374', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP+2);
INSERT INTO zakaznici
VALUES(4, '850218/6968', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP+3);
INSERT INTO zakaznici
VALUES(5, '581223/1128', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP+4);
INSERT INTO zakaznici
VALUES(6, '790123/8642', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP+5);
INSERT INTO zakaznici
VALUES(7, '810209/7399', 'jmeno', 'prijmeni', CURRENT_TIMESTAMP+6);
ALTER TABLE zakaznici AUTO_INCREMENT = 8;

-- rezervace_leku
-- predvedeni triggeru pro automaticke generovani hodnot primarniho klice
INSERT INTO rezervace_leku(stav_rezervace, ID_pobocky, ID_zakaznika, date_time)
VALUES('prijata', 2, 1, CURRENT_TIMESTAMP);
INSERT INTO rezervace_leku(stav_rezervace, ID_pobocky, ID_zakaznika, date_time)
VALUES('prijata', 2, 5, CURRENT_TIMESTAMP+1);
INSERT INTO rezervace_leku(stav_rezervace, ID_pobocky, ID_zakaznika, date_time)
VALUES('prijata',  3, 4, CURRENT_TIMESTAMP+2);
INSERT INTO rezervace_leku(stav_rezervace, ID_pobocky, ID_zakaznika, date_time)
VALUES('prijata', 5, 4, CURRENT_TIMESTAMP+3);
INSERT INTO rezervace_leku(ID_pobocky, ID_zakaznika, date_time)
VALUES(6, 2, CURRENT_TIMESTAMP+4);
INSERT INTO rezervace_leku(ID_pobocky, ID_zakaznika, date_time)
VALUES(6, 6, CURRENT_TIMESTAMP+5);
INSERT INTO rezervace_leku(ID_pobocky, ID_zakaznika, date_time)
VALUES(4, 6, CURRENT_TIMESTAMP+6);
INSERT INTO rezervace_leku(ID_pobocky, ID_zakaznika, date_time)
VALUES(4, 6, CURRENT_TIMESTAMP+7);

-- pojistovny
INSERT INTO pojistovny
VALUES(1, 'Všeobecná zdravotní pojišťovna', 'Benešova 10', 'Brno', 65914, 'Česká republika', '952222222', 'info@vzp.cz', CURRENT_TIMESTAMP);
INSERT INTO pojistovny
VALUES(2, 'Vojenská zdravotní pojišťovna České republiky', 'Banskobystrická 40', 'Brno', 62100, 'Česká republika', '541429811', 'pob-brn@vozp.cz', CURRENT_TIMESTAMP+1);
INSERT INTO pojistovny
VALUES(3, 'Česká průmyslová zdravotní pojišťovna', 'Anglická 26', 'Praha', 12000, 'Česká republika', '261387112', 'pojistovna@cpzp.cz', CURRENT_TIMESTAMP+2);
INSERT INTO pojistovny
VALUES(4, 'Zdravotní pojišťovna ministerstva vnitra ČR', 'Cejl 5', 'Brno', 65816, 'Česká republika', '545543111', 'brno@zpmvcr.cz', CURRENT_TIMESTAMP+3);
INSERT INTO pojistovny
VALUES(5, 'Odborová zdravotní pojišťovna', 'Příkop 4', 'Brno', 60435, 'Česká republika', '545175682', 'brno@ozp.cz', CURRENT_TIMESTAMP+4);
INSERT INTO pojistovny
VALUES(6, 'Zaměstnanecká pojišťovna Škoda', 'Husova 302', 'Mladá Boleslav', 29301, 'Česká republika', '326579111', 'zpskoda@zpskoda.cz', CURRENT_TIMESTAMP+5);
INSERT INTO pojistovny
VALUES(7, 'Revírní bratrská pojišťovna', 'Masarykova 34', 'Brno', 60200, 'Česká republika', '547217930', 'exp-brno@rbp-zp.cz', CURRENT_TIMESTAMP+6);
ALTER TABLE pojistovny AUTO_INCREMENT = 8;

-- pobocka_lek
INSERT INTO pobocka_lek
VALUES('1', '7', 10, 1);
INSERT INTO pobocka_lek
VALUES('2', '5', 10, 1);
INSERT INTO pobocka_lek
VALUES('2', '7', 10, 1);
INSERT INTO pobocka_lek
VALUES('3', '4', 10, 1);
INSERT INTO pobocka_lek
VALUES('4', '1', 10, 1);
INSERT INTO pobocka_lek
VALUES('6', '2', 10, 1);
INSERT INTO pobocka_lek
VALUES('5', '3', 10, 1);
INSERT INTO pobocka_lek
VALUES('5', '1', 10, 1);
INSERT INTO pobocka_lek
VALUES('5', '4', 10, 1);
INSERT INTO pobocka_lek
VALUES('7', '2', 10, 1);
INSERT INTO pobocka_lek
VALUES('7', '6', 10, 1);

-- rezervace_leku_lek
INSERT INTO rezervace_leku_lek
VALUES('6', '3');
INSERT INTO rezervace_leku_lek
VALUES('1', '4');
INSERT INTO rezervace_leku_lek
VALUES('6', '5');
INSERT INTO rezervace_leku_lek
VALUES('6', '7');

-- pobocka_zamestnanec
INSERT INTO pobocka_zamestnanec
VALUES('2', '2');
INSERT INTO pobocka_zamestnanec
VALUES('1', '4');
INSERT INTO pobocka_zamestnanec
VALUES('3', '6');
INSERT INTO pobocka_zamestnanec
VALUES('3', '5');

-- lek_pojistovny
INSERT INTO lek_pojistovny
VALUES('2', '4');
INSERT INTO lek_pojistovny
VALUES('1', '5');
INSERT INTO lek_pojistovny
VALUES('6', '7');
INSERT INTO lek_pojistovny
VALUES('3', '6');
INSERT INTO lek_pojistovny
VALUES('2', '6');
