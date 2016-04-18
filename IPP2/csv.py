#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#CSV:xtisov00

import argparse
import sys, codecs
import re
import lxml.etree as ET
import _csv as csv_module
import copy

from xml.dom import minidom

# Nahrazeni puvodni chybove zpravy pro parsovani v argparse
class customArgumentParser(argparse.ArgumentParser):
	def error(self, _):
		sys.stderr.write("Spatny prepinac.\n")
		sys.exit(1)

# Hlavni trida
class csv2xml:
	_substVal = False

	def __init__(self):
		argv = self.parseArg()
		spamreader = csv_module.reader(argv.inputFile, delimiter = argv.separator,  quotechar='"')

		# -r=root-element
		xmlData = []
		if argv.root:
			xmlData.append(ET.Element(argv.root))
		else:
			xmlData.append(ET.Element("root"))

		header = True
		colName = []
		substData = ""
		for row in spamreader:
			# Bez -h=subst se pouzije hodnota v -c=column-element
			if argv.subst == None and header:
				col = "col"
				# Prirazeni nazvu sloupce -- -c=column-element
				if argv.colElem != None:
					col = argv.colElem

				argv.colElem = col
				# Zapis default nazvu sloupce
				for i in range(0,len(row)):
					colName.append(col)
				header = False

				if self._substVal:
					continue
			# -h=subst -- nacita se hlavicka, -h --nacita se hlavicka a nevypise se
			elif argv.subst != None and header:
				col = "col"
				# Prirazeni nazvu sloupce -- -c=column-element
				if argv.colElem != None:
					col = argv.colElem

				argv.colElem = col
				# Prevod nepovolenych znaku
				for item in row:
					colName.append(self.replaceElement(item, argv.subst))
				header = False
				if self._substVal:
					continue

			# Tag oznacujici jeden radek v csv souboru
			# -l=line-element
			xmlData.append(ET.SubElement(xmlData[0], argv.lineElem))
			lineElementIndex = len(xmlData)-1

			# -i -- prirazeni indexu jako atributu do line-elementu
			if argv.index:
				xmlData[lineElementIndex].set("index", str(argv.start))
				argv.start +=  1

			if argv.errRecovery:
				val = ""
				# --missing-field=val
				if argv.missingField != None:
					val = argv.missingField

				# Pouziti missingField pokud je definovano
				# -e,--error-recovery, vypise se jen pozadovany pocet sloupcu
				if len(colName) >= len(row):
					# Pruchod jednotlivymi sloupci a ulozeni jejich hodnot
					for i in range(0, len(colName)):
						# Pridame -c=column-elementX
						if argv.subst == None:
							col = colName[i] + str(i+1)
						else:
							col = colName[i]

						if i+1 > len(row):
							xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
							xmlData[len(xmlData)-1].text = val
						else:
							xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
							xmlData[len(xmlData)-1].text = row[i]

				# Osetrit situaci kdyz je hlavicka kratsi
				if len(colName) < len(row):
					if argv.allColumns:
						count = len(row)
					else:
						count = len(colName)

					# Pruchod jednotlivymi sloupci a ulozeni jejich hodnot
					for i in range(0, count):
						# Pridame -c=column-elementX

						if i+1 > len(colName):
							if len(colName) < len(row):
								val = row[i]
							else:
								val = ""
							xmlData.append(ET.SubElement(xmlData[lineElementIndex], argv.colElem + str(i+1)))
							xmlData[len(xmlData)-1].text = val
						else:
							if argv.subst == None:
								col = colName[i] + str(i+1)
							else:
								col = colName[i]

							xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
							xmlData[len(xmlData)-1].text = row[i]

			else:
				if len(colName) != len(row):
					self.printErr("Chybejici nebo prebyvajici sloupec.", 32)
				# Pruchod jednotlivymi sloupci a ulozeni jejich hodnot
				for i in range(0, len(colName)):
					if argv.subst == None:
						col = colName[i] + str(i+1)
					else:
						col = colName[i]

					xmlData.append(ET.SubElement(xmlData[lineElementIndex], col))
					xmlData[len(xmlData)-1].text = row[i]

		# Data = ET.SubElement(xmlData[0], "data")
		self.generateXML(xmlData[0], argv)

	def generateXML(self, root, argv):
		if argv.header:
			output = ET.tostring(root, xml_declaration=False, encoding="UTF-8", pretty_print=True)
		else:
			output = ET.tostring(root, xml_declaration=True, encoding="UTF-8", pretty_print=True)

		# Prevod bytes -> str
		output = output.decode("utf-8")

		# Odstraneni root elementu pokud je to potrebne
		lineByLine = output.split("\n")
		lineByLine[0] = lineByLine[0].replace("\'", "\"")

		# Odstraneni posledniho prazdneho prvku
		lineByLine.pop()

		if not argv.root:
			if argv.header:
				output = lineByLine[:0] + lineByLine[1:]
			else:
				output = lineByLine[:1] + lineByLine[2:]
			# Odstraneni ukoncujiciho root elementu
			if len(output) != 0:
				output.pop()
			# Spojeni vysledneho retezce
			output = "\n".join(output)
			output += "\n"

		argv.outputFile.write(output)

	# Vypis chybove zpravy
	def printErr(self, msg, exitCode):
		sys.stderr.write(msg + "\n")
		sys.exit(exitCode)

	# Validace xml elementu row/col nebo root element
	def validElement(self, elem, exitCode):
		firstChar = True
		for char in elem:
			char = ord(char)

			# Osetreni prvniho znaku elementu
			if firstChar:
				if (char <= 64 or char in range(91,95) or char == 96 or
				char in range(123,192) or char == 215  or char == 247 or
				char in range (768, 880) or char == 894 or char in range(8192,8204) or
				char in range(8206,8304) or char in range (8592, 11264) or
				char in range(12272,12289) or char in range(55296, 63744) or
				char in range(64976,65008) or char in range(65534,1114111)):
					self.printErr("Nevalidni znak. ", exitCode)
				firstChar = False
				continue

			# Osetreni dalsich znaku v elementu
			if (char <= 44 or char == 47 or char in range(59,65) or
			char in range(91,95) or char == 96 or char in range(123,183) or
			char in range(184,192) or char == 215 or char == 247 or char == 894 or
			char in range(8192,8204) or char in range(8206,8255) or
			char in range(8257,8304) or char in range (8592, 11264) or
			char in range(12272,12289) or char in range(55296, 63744) or
			char in range(64976,65008) or char in range(65534,1114111)):
				self.printErr("Nevalidni znak. ", exitCode)

	# Konvertuje problematicke znaky
	def convertElementMis(self, elem):
		return elem

	# Nahradi problematicke znaky pri
	# Nahrazeni u row/col nebo root elementu
	def replaceElement(self, elem, pattern):
		elem = elem.replace(" ", pattern)
		elem = elem.replace("\n", pattern)
		elem = elem.replace("\r", pattern)
		elem = elem.replace("\t", pattern)
		elem = elem.replace(",", pattern)
		elem = elem.replace(".", pattern)

		# Kontrola zda i po nahrazeni nezustal nepovoleny znak
		self.validElement(elem, 31)
		return elem

	def parseArg(self):
		parser = customArgumentParser(add_help = False)
		parser.add_argument("--help", action = "store_true", default = False, dest = "help", help = "Zobrazeni help zpravy.")
		parser.add_argument("--input", action = "store", dest = "inputFile", help = "Vstupni CSV soubor.")
		parser.add_argument("--output", action = "store", dest = "outputFile", help = "Vystupni XML soubor")
		parser.add_argument("-n", action = "store_true", default = False, dest = "header", help = "Zakazani vypisu hlavicky XML vystupu.")
		parser.add_argument("-r", action = "store", dest = "root", help = "Element obalujici vysledek XML vystupu.")
		parser.add_argument("-s", action = "store", dest = "separator", help = "Jeden znak/idetifikat TAB, na zaklade ktereho se deli jednotlive sloupce v CSV.")
		parser.add_argument("-h", action = "store", dest = "subst", nargs = "?", const = "-",help = "Prvni radek CSV souboru bude slouzit jako hlavicka pro elementy v XML. Nepovoleny znak se nahradi zadanym retezcem.")
		parser.add_argument("-c", action = "store", dest = "colElem", help = "Urcuje prefix jmena elementu, ktery bude obalovat nepojmenovane bunky.")
		parser.add_argument("-l", action = "store", dest = "lineElem", help = "Jmeno elementu, ktery obaluje zvlast kazdy radek.")
		parser.add_argument("-i", action = "store_true", dest = "index", default = False, help = "Vlozi atribut index s ciselnou hodnotou do line-elementu.")
		parser.add_argument("--start", action = "store", type = int, dest = "start", help = "Cislo pro pocitadlo v prepinaci -i.")
		parser.add_argument("-e", "--error-recovery", action = "store_true", default = False, dest = "errRecovery", help = "Zotaveni z chyby.")
		parser.add_argument("--missing-field", action = "store", dest = "missingField", help = "Pokud nektery sloupec chybi, je doplneny touto hodnotou.")
		parser.add_argument("--all-columns", action = "store_true", dest = "allColumns", default=False, help = "Zamezi ignorovani nekorektnich hodnot.")

		argv = parser.parse_args()
		err = False

		# Osetreni duplicitnich prepinacu
		params = ["--help", "--output", "--input", "-n", "-r", "-s", "-h", "-c", "-l", "-i", "--start", "-e", "--error-recovery", "--missing-field", "--all-columns"]
		tmp = [arg.split('=')[0] for arg in sys.argv]
		for param in params:
			if tmp.count(param) > 1:
				self.printErr("Duplicitni prepinace.", 1)

		# Help nemuze byt s jinym prepinacem
		if (argv.help and len(sys.argv) != 2):
			self.printErr("Prepinac help nemuze byt pouzity s jinymi prepinaci.", 1)
		elif argv.help:
			parser.print_help()
			sys.exit(0)

		# Kontrola: -r=root-element
		if argv.root != None:
			self.validElement(argv.root, 30)

		# Kontrola separatoru - jen jeden znak
		if argv.separator:
			if len(argv.separator) > 1 and argv.separator != "TAB":
				self.printErr("Prepinac -s muze obsahovat jen jeden znak.", 1)

		# Definovani oddelovace
		if argv.separator == None:
			argv.separator = ","
		elif argv.separator == "TAB":
			argv.separator = "\t"
		elif len(argv.separator) == 0:
			self.printErr("Oddelovac musi mit jeden znak.", 1);

		# Kombinace parametru -i a -l
		if argv.index and argv.lineElem == None:
			self.printErr("Tento parametr musi byt zadan spolecne s prepinacem -l.",1)

		# Nahrazeni nevalidnich znaku v hlavicce vstupniho souboru -h=subst
		if argv.subst != None:
			pass
			# TODO dodelat validaci
			# self.validElement(argv.subst, 31)

		# Kontrola zda byla hodnota zadana nebo ne
		for item in sys.argv:
			if "-h" in item:
				self._substVal = True;

		# Kontrola: -c=column-element
		if argv.colElem != None:
			self.validElement(argv.colElem, 30)


		# Kontrola: --start=n
		if argv.start == None:
			argv.start = 1
		else:
			if argv.start < 0:
				self.printErr("Zaporne cislo.", 1)
			# Kombinace jen s -i a -l, chyba
			if not argv.index or argv.lineElem == None:
				self.printErr("Prepinac musi byt kombinovany s prepinaci: -i a -l.", 1)
		# Kontrola: -i
		if argv.index and argv.lineElem == None:
			self.printErr("Chybna kombinace prepinacu.", 1)

		# Kontrola: -l=line-element
		if argv.lineElem == None:
			argv.lineElem = "row"
		else:
			self.validElement(argv.lineElem, 30)

		# Kontrola: --missing-field=val
		if argv.missingField != None:
			if argv.errRecovery:
				argv.missingField = self.convertElementMis(argv.missingField)
			else:
				self.printErr("Neni zadany prepinac -e, --error-recovery.", 1);

		# Kontrola: -e,--error-recovery
		if argv.errRecovery and argv.missingField == None:
			argv.missingField = ""

		# Kontrola: --all-columns
		if argv.allColumns and not argv.errRecovery:
			self.printErr("Neni zadany prepinac -e, --error-recovery.", 1);

		# Kontrola vstupniho souboru, zda existuje a jestli se da otevrit
		if argv.inputFile:
			try:
				argv.inputFile = open(argv.inputFile, "r", newline = "", encoding = "utf-8")
			except:
				sys.stderr.write("Nelze nacist vstupni soubor.\n")
				sys.exit(2)
		else:
			argv.inputFile = sys.stdin

		# Kontrola vystupniho souboru, zda se da vytvorit
		if argv.outputFile:
			try:
				argv.outputFile = open(argv.outputFile, "w", newline = "", encoding = "utf-8")
			except:
				sys.stderr.write("Nelze vytvorit vystupni soubor.\n")
				sys.exit(3)
		else:
			argv.outputFile = sys.stdout

		return argv

if __name__ == "__main__":
	csv2 = csv2xml()
