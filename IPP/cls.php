<?php

#CLS:xtisov00

/**
 * @author Peter Tisovcik <xtisov00@stud.fit.vutbr.cz>
 */

/**
 * Zdedena trida v save implementuje vypisovani chybovych hlasek
 * na STDERR, umoznuje vypsat kod a chybu.
 */
class customException extends Exception {
		public $message;
		public $code;

		public function __construct($message, $code) {
			$this->message = $message."\n";
			$this->code = $code;
		}

		/* Vypis chyby na stderr */
		public function printError() {
			fwrite(STDERR, $this->message);
		}
}

/**
 * Trida implementuje parsovani jednotlivych argumentu. Osetruje stavy,
 * kdy je zadany neplatny argument, kdy je zadany nespravny format argumentu a
 * osetruje stavy, kdyz jsou zadane dlouhe a kratke prepinace, jejich kombinace.
 * Vypisuje help message.
 */
class InputArgv {
	const OK = 0;
	const E_ARG = 1;
	const E_INPUT_FILE = 2;
	const E_OUTPUT_FILE = 3;

	public $help =  null;
	public $input = "php://stdin";
	public $output = "php://stdout";
	public $prettyXml = 4;
	public $details = false;
	public $detailName = NULL;
	public $search = NULL;
	public $conflicts = false;

	/**
	 * Naplni vnitrni strukturu zadanymi argumenty.
	 */
	public function __construct($argv) {
		$this->parseArguments($argv);
	}

	/**
	 * Vypis help message.
	 */
	private function getHelp() {
echo "CLS: Skript pro analyzu hlavickovych souboru jazyka C++11 a nasledny XML vystup.\nAutor: Peter Tisovcik <xtisov00@fit.vutbr.cz>\n
	--help, -h
		vypis napovedy

	--input=file, -i=file
		vstupni hlavickovy soubor jazyka C++11

	--output=file, -o=file
		vystupni soubor, do ktereho se ulozi XML vystup, pokud neni zadany pouzije se stdout

	--pretty-xml=n, -p=n
		pocet mezer v zanoreni XML souboru, pokud nebude zadany n=4

	--details=class, -d=class
		vypise detaily o zadane tride, pokud neni zadana class tak je vypsan detail o vsech tridach

	--search=XPATH, -s=XPATH
		retezec, ktery reprezentuje XPATH vyraz

	--conflicts, -c
		vypis konfliktnich clenu\n";
	}

	/**
	 * Vraci hodnotu podle toho jestli byl pouzity argument pri spusteni.
	 * @return		bool
	 */
	public function isDetails() {
		if (!is_null($this->details))
			return true;
		return false;
	}

	/**
	 * Vraci hodnotu podle toho jestli byl pouzity argument pri spusteni.
	 * @return		bool
	 */
	private function isSearch() {
		if (!is_null($this->search))
			return true;
		return false;
	}

	/**
	 * Rozparsovani argumentu na zaklade stanovenych pravidel
	 * @param 		array
	 * @return		bool
	 */
	private function parseArguments($argv) {
		$shortopts = "h::i:o:p::d::s:c::";

		$longopts = array(
			"help::", // --help
			"input:", // --input=file
			"output:", // --output=file
			"pretty-xml::", // --pretty-xml=k
			"details::", // --details=class_uses
			"search:", // --search=XPATH
			"conflicts::" // --conflicts
		);

		// Parsovani argumentu
		$options = getopt($shortopts, $longopts);

		// Pokud byl zadan neplatny argument typu: -hxxxxx
		if (array_key_exists('h', $options))
			if (is_bool($options['h']) === false)
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);

		// Osetreni opakujicich se prepinacu
		foreach ($options as $value) {
			if (is_array($value))
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);
		}

		// Osetreni neznameho argumentu
		$optionsLen = count($options);
		if ($optionsLen != (count($argv) - 1))
			throw new customException("Bad input arguments. Try --help.", self::E_ARG);

		// Kontrola kratkych prepinacu a dlouhych prepinacu: --output=file -o=file
		// Sjednoceni formatu na dlouhe prepinace, pro jednotne pouzivani dale
		foreach ($longopts as $value) {
			$firstChar = substr($value, 0, 1);
			$value = str_replace(":", "", $value);

			// Duplikace prepinacu
			if (array_key_exists($value, $options) && array_key_exists($firstChar, $options))
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);
			else {
				// Sjednoceni na dlouhe prepinace: -o=file >>> --output=file
				if (array_key_exists($firstChar, $options)) {
					$options[$value] = $options[$firstChar];
					unset($options[$firstChar]);
				}
			}
		}

		// Test zda byl zadany --help|-h prepinac bez ostatnich
		if (array_key_exists("help", $options)) {
			if ($optionsLen > 1)
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);

			$this->getHelp();
			exit(self::OK);
		}

		// Kontrola vstupu, zda existuje a jestli se da z daneho souboru cist
		if (array_key_exists("input", $options)) {
			if (!file_exists($options['input']) || !is_readable($options['input']))
				throw new customException("Bad input file.", self::E_INPUT_FILE);

			$this->input = $options['input'];
		}

		// Kontrola vystupniho souboru, zda se da vytvorit a zapsat do neho
		if (array_key_exists("output", $options)) {
			if (is_writable($options['output'])) {
				// Vytvori soubor pro zapis, pokud se nepodari skonci chybou
				if ( !($fw = fopen($options['output'], "w")) )
					throw new customException("Bad output file.", self::E_OUTPUT_FILE);
			} else {
				// Pokud existuje ale neni pro zapis, skonci chybou
				if (file_exists($options['output']))
					throw new customException("Bad output file.", self::E_OUTPUT_FILE);

				// Soubor neexistuje, vytvori se a pokud se nepodari, skonci chybou
				if ( !($fw = fopen($options['output'], "w")) )
					throw new customException("Bad output file.", self::E_OUTPUT_FILE);
			}
			$this->output = $options['output'];
		}

		// Kontrola pretty-xml, kontrola zda je to kladne cele cislo,
		// pokud nebylo zadane, nastavi se na predvolenou hodnotu
		if (array_key_exists("pretty-xml", $options)) {
			// Kontrola zda obsahuje jen cisla
			if (ctype_digit($options['pretty-xml']))
				$this->prettyXml = intval($options['pretty-xml']);

			// Pokud nebyla nastavena prepinaci hodnota, pouzije se predvolena
			elseif (is_bool($options['pretty-xml']) === true)
				$this->prettyXml = 4;
			else
			 	throw new customException("Bad input format.", self::E_ARG);
		}

		// Kontrola details, zda byl zadany a jestli byla zadana trida
		if (array_key_exists("details", $options)) {
			$this->details = true;

			if ($options['details'] != "")
				$this->detailName = $options['details'];
		}

		// Zpracovani XPATH vyrazu
		if (array_key_exists("search", $options)) {
			if ($options['search'] == "")
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);

			$this->search = $options['search'];
		}

		// Zpracovani konfliktu a jejich vygenerovani do souboru
		if (array_key_exists("conflicts", $options)) {
			$this->conflicts = true;
			if (is_bool($options['conflicts']) === false)
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);
		}
	}
}

/**
 * Sablona pro jednotlive tridy, ve ktere se budou ukladat vsechny potrebne
 * informace o tride, ktere se daji ziskat ze vstupniho souboru.
 */
 class TemplateClass{
	private $conflicts = false;
	/* Nazev zpracovane tridy */
	public $name;

	/* Typ abstract pokud je abstraktni | concreate - pokud neni abstraktni */
	public $kind = false;

	/* Pole obsahuje informace o tom, ktere tridy z ni dedily */
	public $childrens = array();

	/* Pole nazvu trid, ze kterych trida dedi - jen primi potomkove */
	public $parentClassName = array();

	/* Pole metod patricich k dane tride. Priota urcuje vahu pri */
	public $methodArray = array();

	/* Pole argumentu jednotlivych metod. Priota urcuje vahu pri */
	public $argsMethodArray = array();

	/* Pole atributu dane tridy */
	public $attributesArray = array();

	function __construct($name, $conflicts) {
		$this->name = $name;
		$this->conflicts = $conflicts;
	}

	/* Nastaveni zda je trida abstraktni */
	function setKind($kind) {
		$this->kind = $kind;
	}

	/* Ziskani typu tridy */
	function getKind() {
		if ($this->kind)
			return "abstract";
		return "concrete";
	}

	/* Pridani potomka do tridy odkud se dedi */
	function addChild($name)  {
		array_push($this->childrens, $name);
	}

	/* Pridani atributu do tridy */
	function addAttributeClass($accessModifier, $attributeType, $dataType, $name) {
		array_push($this->attributesArray, array("modifier" => $accessModifier, "attributeType" => $attributeType, "type" => $dataType, "name" => $name, "priority" => true, "parentClass" => $this->name, "conflicts" => false, "show" => true));
	}

	/* Pridani metody do tridy */
	function addClassMethod($accessModifier, $methodType, $dataType, $name) {
		// Konstruktor
		if ($this->name == $dataType) {
			$name = $dataType;
			$dataType = "";
		}

		// Destruktor
		if (("~".$this->name) == $dataType) {
			$name = $dataType;
			$dataType = "";
		}

		array_push($this->methodArray, array("modifier" => $accessModifier, "methodType" => $methodType, "type" => $dataType, "name" => $name, "virtualPure" => false, "priority" => true, "parentClass" => $this->name, "conflicts" => false, "show" => true));
	}

	/* Pridani tridy, ze ktere se dedi */
	function addParentClass($accessModifier, $parentClassName) {
		$this->parentClassName[$parentClassName] = array("modifier" => $accessModifier, "name" => $parentClassName);
	}

	/* Pridani argumentu do metody */
	function addMethodArg($actMethod, $type, $name) {
		if ($type == "void" || (empty($type) && empty($name) ))
			return;
		$methodIndex = $this->getLastMethodIndex();

		// Vytvoreni pole pokud jeste neexistuje
		if (!is_array(@$this->argsMethodArray[$methodIndex]))
			$this->argsMethodArray[$methodIndex] = array();

		array_push($this->argsMethodArray[$methodIndex], array("name" => $name, "type" => $type));
	}

	/* Definovani ze ktere tridy se ma dedit atribut/metoda */
	function addUsing($usingClass, $using, $modifier) {
		// Odstraneni nezadouci metody
		foreach ($this->methodArray as $key => $value) {
			if ($value["name"] ==  $using) {
				$this->methodArray[$key]["modifier"] = $modifier;
				if ($value["parentClass"] == $usingClass->name) {
					//TODO osetrit aby se rovnaly i argumenty
				} else
					unset($this->methodArray[$key]);
			}
		}

		// Odstraneni nezadouciho atributu
		foreach ($this->attributesArray as $key => $value) {
			if ($value["name"] ==  $using ) {
				$this->attributesArray[$key]["modifier"] = $modifier;

				if ($value["parentClass"] != $usingClass->name)
					unset($this->attributesArray[$key]);
			}
		}
	}

	/* Ziskani indexu posledni pridane metody */
	private function getLastMethodIndex() {
		end($this->methodArray);
		return key($this->methodArray);
	}

	/* Ziskani indexu posledniho pridaneho indexu */
	private function getLastAttributeIndex() {
		end($this->attributesArray);
		return key($this->attributesArray);
	}

	/* Pokud je metoda virtualPure, nastaveni i trid */
	public function setVirtualPure() {
		$methodIndex = $this->getLastMethodIndex();
		$this->methodArray[$methodIndex]["virtualPure"] = true;
	}

	/* Postupne se zkopiruji jednotlive atributy a metody s jejich argumenty */
	function CopyClass($from, $accessModifier) {
		// Kopirovani atributu
		foreach ($from->attributesArray as $key => $value) {
			if ($value["conflicts"]) continue;
			array_push($this->attributesArray, $value);

			$lastIndex = $this->getLastAttributeIndex();
			$this->attributesArray[$lastIndex]["priority"] = false;

			/* Zamezeni vypisu zdedenych private clenu */
			if ($this->attributesArray[$lastIndex]["modifier"] == "private")
				$this->attributesArray[$lastIndex]["show"] = false;

			/* Modifikace modifikatoru pristupu */
			if ($accessModifier == "private")
				$this->attributesArray[$lastIndex]["modifier"] = $accessModifier;
			if ($accessModifier == "protected") {
				if ($this->attributesArray[$lastIndex]["modifier"] != "private")
					$this->attributesArray[$lastIndex]["modifier"] = $accessModifier;
			}
		}

		// Kopirovani metod a argumentu metod
		foreach ($from->methodArray as $key => $value) {
			if ($from->name == $value["type"] || ("~".$from->name) == $value["type"]) continue;

			if ($value["conflicts"]) continue;
			array_push($this->methodArray, $value);

			$lastIndex = $this->getLastMethodIndex();
			$this->methodArray[$lastIndex]["priority"] = false;

			/* Zamezeni vypisu zdedenych private clenu */
			if ($this->methodArray[$lastIndex]["modifier"] == "private" && !$this->methodArray[$lastIndex]["virtualPure"])
				$this->methodArray[$lastIndex]["show"] = false;

			/* Modifikace modifikatoru pristupu */
			if ($accessModifier == "private")
				$this->methodArray[$lastIndex]["modifier"] = $accessModifier;
			if ($accessModifier == "protected") {
				if ($this->methodArray[$lastIndex]["modifier"] != "private")
					$this->methodArray[$lastIndex]["modifier"] = $accessModifier;
			}

			// Kopirovani argumentu metody
			if (array_key_exists($key, $from->argsMethodArray))
			$this->argsMethodArray[$lastIndex] = $from->argsMethodArray[$key];
		}
	}

	/* Kontrola, zda se nektere promenne a metody neredefinovaly, nastaveni kind */
	function CheckClass() {
		foreach ($this->methodArray as $key => $value) {
			foreach ($this->methodArray as $key2 => $value2) {
				// Odstraneni prepsane metody
				if ($value["name"] == $value2["name"] && $value["priority"] != $value2["priority"])  {
					//TODO osetrit pokud se nerovnaji argumenty
					if (!$value["priority"])
						unset($this->methodArray[$key]);
					if (!$value2["priority"])
						unset($this->methodArray[$key2]);
				}
			}
		}

		// Kontrola zda se neredefinovaly atributy
		foreach ($this->attributesArray as $key => $value) {
			foreach ($this->attributesArray as $key2 => $value2) {
				// Odstraneni prepsane metody
				if ($value["name"] == $value2["name"] && $value["priority"] != $value2["priority"])  {
					if (!$value["priority"])
						unset($this->attributesArray[$key]);
					if (!$value2["priority"])
						unset($this->attributesArray[$key2]);
				}
			}
		}

		// Jestli se mezi metodami objevi virtual, nastavi se class jako abstraktni
		foreach ($this->methodArray as $key => $value) {
			if ($value["virtualPure"])
				$this->setKind(true);
		}
		return $this->CheckConflict();
	}

	/* Kontrola konfliktu pri dedeni */
	function CheckConflict() {
		$conflictClass = false;

		/* Kontrola zda jsou shodne jmena, zda jsou zdedene z ruznych trid */
		foreach ($this->methodArray as $key => $value) {
			foreach ($this->methodArray as $key2 => $value2) {
				if ($key != $key2 &&
					$value["name"] == $value2["name"] &&
					$value["parentClass"] != $this->name &&
					$value2["parentClass"] != $this->name ) {
						if ($this->conflicts) {
							$this->methodArray[$key]["conflicts"] = true;
							$conflictClass = true;
						} else
							throw new customException("Conflict.", 21);
					}
			}
		}

		/* Kontrola zda jsou shodne jmena, zda jsou zdedene z ruznych trid */
		foreach ($this->attributesArray as $key => $value) {
			foreach ($this->attributesArray as $key2 => $value2) {
				// Kontrola konfliktu
				if ($key != $key2 &&
					$value["name"] == $value2["name"] &&
					$value["parentClass"] != $this->name &&
					$value2["parentClass"] != $this->name )
					if ($this->conflicts) {
						$this->attributesArray[$key]["conflicts"] = true;
						$conflictClass = true;
					} else
						throw new customException("Conflict.", 21);
			}
		}

		/* Kontrola zda se nejedna o konflikt mezi metodou a atributem */
		foreach ($this->attributesArray as $key => $value) {
			foreach ($this->methodArray as $key2 => $value2) {
				// Kontrola konfliktu
				if ($value["name"] == $value2["name"] &&
					$value["parentClass"] != $this->name &&
					$value2["parentClass"] != $this->name ) {
						if ($this->conflicts) {
							$this->attributesArray[$key]["conflicts"] = true;
							$this->methodArray[$key2]["conflicts"] = true;
							$conflictClass = true;
						} else
							throw new customException("Conflict.", 21);
					}
			}
		}

		return $conflictClass;
	}
} // Konec tridy

/**
 * Zpracovani vstupniho souboru pomoci konecneho automatu. Zpracovani probiha v
 * jednotlivych stavech, ve kterych se zpracovavaji informace o jednotlivych polozkach.
 * Nacitavani probiha po znacich, ktere se skladaji do bloku a jez dale vyuziva funkce,
 * ktera prijima na vstup pole znaku a v pripade, ze se aktualne zpracovany znak
 * nachazi v tomto poli, zastavi se zpracovavani a vrati se string slozeny z nactenych
 * znaku.
 */
class ParseInputFile {
	const S_START = 0;
	const S_CLASS_NAME = 1;
	const S_INHERITANCE = 2;
	const S_ACCESS_MODIFIERS = 3;
	const S_BODY = 5;
	const S_METHOD_TYPE = 6;
	const S_DATA_TYPE_AND_NAME = 7;
	const S_METHOD_ARGV = 8;
	const S_ABSTRACT_METHOD = 9;
	const S_END_BODY_CLASS = 10;
	const S_USING_CLASS = 11;

	const E_UNKNOWN = 4;

	private $positionInFile = 0;
	private $EOF = false;
	private $file;
	private $conflicts = false;

	private $whiteSpace = array(" ", "\t", "\n", "\r", "\0", "\x0B");
	private $accessModifiers = array("public", "protected", "private");
	private $methodType = array("static", "virtual");
	private $dataTypes = array("void", "int", "float", "double", "bool",
								"char", "wchar_t", 	"signed", "long",
								"unsigned", "short");

	private $conflictClass = array();

	private $model;

	/**
	 * Ulozeni deskriptoru na aktualne zpracovavany soubor.
	 * @param		$file		vstupni soubor
	 */
	public function __construct($file, $conflicts) {
		$this->file = file_get_contents($file);
		$this->conflicts = $conflicts;
	}

	/**
	 * Vrati naposledy precteny znak ze souboru (podobne ungetc).
	 * @return		char		posledne nacteny znak
	 */
	private function GetPreviousChar() {
		if ($this->positionInFile > 0)
			$this->positionInFile--;

		return $this->ReadChar($this->file);
	}

	/**
	 * Presune ukazatel v souboru o zadanou hodnotu dozadu.
	 * @param		int			posun ukazatele v souboru
	 */
	private function ReturnCharToFile($count) {
		while($count) {
			if ($this->positionInFile > 0)
				$this->positionInFile--;
			$count--;
		}
	}

	/**
	 * Odstraneni bilych znaku po nejblizsi 'normalni' znak.
	 */
	private function RemoveWhiteSpace() {
		while (in_array($this->ReadChar(), $this->whiteSpace));
		$this->ReturnCharToFile(1);
	}

	/**
	 * Vraci true v pripade, ze se ukazatel v souboru dostal na konec souboru.
	 * @return 		bool
	 */
	public function isEOF() {
		return $this->EOF;
	}

	/**
	 * Precte znak ze souboru a vrati ho. V pripade, ze se precetl konec souboru,
	 * nastavi se promenna $EOF na true a vraci se jen prazdny retezec.
	 * @return 		string			nacteny znak
	 */
	private function ReadChar() {
		if (($char = substr($this->file, $this->positionInFile, 1)) !== false) {
			$this->positionInFile++;
			return $char;
		}

		$this->EOF = true;
		return "";
	}

	/**
	 * Nacteni znaku ze souboru, pokud se nektery z nej nebude nachazet
	 * v zadanem poli.
	 * @param		array 			pole znaku, po ktere se ma nacist
	 * @return 		string			nacteny retezec
	 */
	public function ReadTo($to) {
		$this->RemoveWhiteSpace();
		$data = "";

		while (1) {
			$char = $this->ReadChar();

			if (in_array($char, $to))
				break;

			if ($char == "")
				break;

			$data .= $char;
		}
		return $data;
	}

	/**
	 * Rozparsovani vstupniho souboru na potrebne informace pomoci
	 * konecneho automatu.
	 */
	public function ParseInput() {
		$actState = self::S_START;
		$actClass = "";
		$accessModifier = "private";
		$actMethod = "";
		$type = NULL; // Method | atribut type

		while (!$this->isEOF()) {
			// Zacatek konecneho automatu
			if ($actState == self::S_START) {
				$to = $this->whiteSpace;
				$this->ReadTo($to);

				$actState = self::S_CLASS_NAME;
			}
			// Nacteni nazvu tridy
			else if ($actState == self::S_CLASS_NAME) {
				$to = array("{", ":");
				$data = $this->ReadTo($to);
				$data = preg_replace('/\s+/', '', $data);

				// Pridany nazev tridy do objektu
				$this->model[$data] = new TemplateClass($data, $this->conflicts);

				$actClass = $data;
				$this->dataTypes[] = $data;
				$this->dataTypes[] = "~".$data;
				if ($this->GetPreviousChar() == ":")
					$actState = self::S_INHERITANCE;
				else
					$actState = self::S_BODY;
			}
			// Nacteni trid, ze kterych se dedi
			else if ($actState == self::S_INHERITANCE) {
				$to = array(",", "{");
				$data = $this->ReadTo($to);
				$data = preg_split('/\s+/', $data,  NULL, PREG_SPLIT_NO_EMPTY);

				// Ulozeni informaci o tride, ze ktere se dedi
				// Osetreni pokud se dedi z nezname tridy
				if (count($data) == 2) {
					if (!array_key_exists($data[1], $this->model))
						throw new customException("Input file error", self::E_UNKNOWN);

					$this->model[$data[1]]->addChild($actClass);
					$this->model[$actClass]->CopyClass($this->model[$data[1]], $data[0]);
					$this->model[$actClass]->AddParentClass($data[0], $data[1]);
				} else {
					if (!array_key_exists($data[0], $this->model))
						throw new customException("Input file error", self::E_UNKNOWN);

					$this->model[$data[0]]->addChild($actClass);
					$this->model[$actClass]->CopyClass($this->model[$data[0]], "private");
					$this->model[$actClass]->AddParentClass("private", $data[0]);
				}

				if ($this->GetPreviousChar() == ",")
					$actState = self::S_INHERITANCE;
				else
					$actState = self::S_BODY;
			}
			// Nacitavani tela tridy
			else if ($actState == self::S_BODY) {
				$this->RemoveWhiteSpace(); // Odstraneni bilych znaku pred ctenim
				$data = "";
				$search = false;

				// Kontrola na klicove slova
				// private|public|protected OR using OR static|virtual OR int,float...
				while (true) {
					$char = $this->ReadChar();
					$data .= $char;

					// Pokud je prazdne telo, preskoci se na konec tridy.
					if ($char == "}") {
						$actState = self::S_END_BODY_CLASS;
						break;
					}
					// Using
					else if ($data == "using") {
						$actState = self::S_USING_CLASS;
						break;
					}
					// Modifikatory pristupu - public|private|protected
					else if (in_array($data, $this->accessModifiers)) {
						$actState = self::S_ACCESS_MODIFIERS;
						$search = true;
					}
					// Typ metody
					else if (in_array($data, $this->methodType)) {
						$actState = self::S_METHOD_TYPE;
						$search = true;
					}
					// Datove typy
					else if (in_array($data, $this->dataTypes)) {
						$actState = self::S_DATA_TYPE_AND_NAME;
						$search = true;
					} else if ($char == ";")
						throw new customException("Input file error.", self::E_UNKNOWN);

					// Posun ukazatele v souboru na stav pred zacatkem nacitavani
					if ($search) {
						$this->ReturnCharToFile(strlen($data));
						break;
					}
				}
			}
			// Zpracovani konce tridy
			else if ($actState == self::S_END_BODY_CLASS) {
				$to = array(";");
				$this->ReadTo($to);

				if ($this->GetPreviousChar() == ";")
					$actState = self::S_START;

				if ($this->model[$actClass]->CheckClass())
					$this->conflictClass[] = $actClass;

				$accessModifier = "private";
			}
			// Zpracovani modifikatoru pristupu
			else if ($actState == self::S_ACCESS_MODIFIERS) {
				$to = array(":");
				$data = $this->ReadTo($to);

				$accessModifier = $data;
				$actState = self::S_BODY;
			}
			// Zpracovani typu metody|atributu - virtual|static
			else if ($actState == self::S_METHOD_TYPE) {
				$to = $this->whiteSpace;
				$type = $this->ReadTo($to);

				$actState = self::S_BODY;
			}
			// Zpracovani datoveho typu a nazvu promenne/metody
			else if ($actState == self::S_DATA_TYPE_AND_NAME) {
				$to = array(";", "(");
				$data = $this->parseMethodArg($this->ReadTo($to));

				if ($this->GetPreviousChar() == "(") {
					$this->model[$actClass]->addClassMethod($accessModifier, $type, $data[0], $data[1]);
					$actState = self::S_METHOD_ARGV;
				} else {
					$this->model[$actClass]->addAttributeClass($accessModifier, $type, $data[0], $data[1]);
					$actState = self::S_BODY;
				}
				$actMethod = $data[1];
				$type = NULL;
			}
			// Prechod do zpristupneni metody|atributu
			else if ($actState == self::S_USING_CLASS) {
				$to = array(";");
				$data = $this->ReadTo($to);
				$data = str_replace("::", " ", $data);
				$data = preg_split('/\s+/', $data,  NULL, PREG_SPLIT_NO_EMPTY);

				$this->model[$actClass]->addUsing($this->model[$data[0]], $data[1], $accessModifier);

				$actState = self::S_BODY;
			}
			// Zpracovani argumentu metody
			else if ($actState == self::S_METHOD_ARGV) {
				$to = array(",", ")");

				$data =$this->parseMethodArg($this->ReadTo($to));

				$this->model[$actClass]->addMethodArg($actMethod, $data[0], $data[1]);

				if ($this->GetPreviousChar() == "," && count($data) != 0)
					$actState = self::S_METHOD_ARGV;
				else
					$actState = self::S_ABSTRACT_METHOD;
			}
			// Zpracovani abstraktni metody
			else if ($actState == self::S_ABSTRACT_METHOD) {
				$to = array(";", "}");
				$data = $this->ReadTo($to);
				$data = preg_replace('/\s+/', '', $data);

				if ($data == "=0")
					$this->model[$actClass]->setVirtualPure();

				$actState = self::S_BODY;
			}
			else
				$actState = self::S_BODY;
		}
	} // Konec metody

	// Generovani vystupniho XML - B
	function generateB($xmlB, $className, $conflict) {
		if (!count($this->model)) return;

		if (is_null($className))
			$xmlB->startElement("model");
		else {
			// Otestovani, zda existuje trida pro prepinac details
			if (!array_key_exists($className, $this->model))
				return;
		}

		foreach($this->model as $name => $tmp) {
		if (!is_null($className) && $className != $name) continue;
		$xmlB->startElement('class');
		$xmlB->writeAttribute("name", $this->model[$name]->name);
		$xmlB->writeAttribute("kind", $this->model[$name]->getKind());

		// Dedicnost
		if (count($this->model[$name]->parentClassName)) {
			$xmlB->startElement("inheritance");
			foreach ($this->model[$name]->parentClassName as $inheritance) {
				$xmlB->startElement("from");
				$xmlB->writeAttribute("name", $inheritance["name"]);
				$xmlB->writeAttribute("privacy", $inheritance["modifier"]);
				$xmlB->endElement();
			}
			$xmlB->endElement();
		}

		$conflictExists = false;
		foreach ($tmp->attributesArray as  $value) {
			if ($value["conflicts"])
				$conflictExists = true;
		}
		foreach ($tmp->methodArray as  $value) {
			if ($value["conflicts"])
				$conflictExists = true;
		}

		// Generovani konfliktu
		if ($conflict && $conflictExists) {
			$xmlB->startElement("conflicts");
			$data = array();

			// Nacteni konfliktnich metod/atributu do pole
			foreach ($this->model as $tmp) {
				foreach ($tmp->attributesArray as $key => $value) {
					if ($value["conflicts"])
						$data[$name ][] = array($key, $value["name"], "attr", $value["modifier"]);
				}

				foreach ($tmp->methodArray as $key => $value) {
					if ($value["conflicts"])
						$data[$name ][] = array($key, $value["name"], "met", $value["modifier"]);
				}
			}

			// Odstraneni duplicit z konfliktnich clenu
			foreach ($data[$name] as $key => $value) {
				foreach ($data[$name] as $key2 => $value2) {
					if (@$this->model[$name]->attributesArray[$value[0]]["parentClass"] == @$this->model[$name]->attributesArray[$value2[0]]["parentClass"] && $key != $key2)
						unset($data[$name][$key]);
				}
			}

			$oldName = array();

			// Pruchod zpracovavanymi konflikty
			foreach ($data[$name] as $key2 => $conflictItem) {
				if (in_array($conflictItem[1], $oldName)) continue;

				$xmlB->startElement("member");
				$xmlB->writeAttribute("name", $conflictItem[1]);

				$oldParentClassA = "";
				$oldParentClassM = "";
				$onlyBaseClass = false;
				$indexParent = 0;

				// Pruchod vsemi tridami
				foreach ($this->model[$name]->parentClassName as $key => $value) {
					if ($onlyBaseClass) {
						$onlyBaseClass = false;
						continue;
					}

					if (count($data[$name]) == 1 && $indexParent == 1)
						break;
					$indexParent++;

					$parentName = $value["name"];
					$xmlB->startElement("class");
					$xmlB->writeAttribute("name", $value["name"]);

					// Pruchod jednotlivymi modifikatory
					// Pruchod jednotlivymi konfliktnimy metodami/atributy
					foreach ($this->accessModifiers as $modifier) {

						$attributeCount = 0;
						foreach ($this->model[$parentName]->attributesArray as $attr) {
							if ($attr["name"] != $conflictItem[1]) continue;
							if ($attr["modifier"] != $modifier) continue;
							$attributeCount++;
						}

						$methodCount = 0;
						foreach ($this->model[$parentName]->methodArray as $method) {
							if ($method["name"] != $conflictItem[1]) continue;
							if ($method["modifier"] != $modifier) continue;
							$methodCount++;
						}

						if (!$attributeCount && !$methodCount) continue;
						$xmlB->startElement($modifier);

						if ($attributeCount) {
							foreach ($this->model[$parentName]->attributesArray as $attribute) {
								if ($attribute["name"] != $conflictItem[1]) continue;
								//if ($attribute["parentClass"] != $parentName) continue;
								if ($oldParentClassA == $attribute["parentClass"]) {
									$onlyBaseClass = true;
									continue;
								}

								$xmlB->startElement("attribute");
								$xmlB->writeAttribute("name", $attribute["name"]);
								$xmlB->writeAttribute("type", $attribute["type"]);
								$xmlB->writeAttribute("scope", $this->getScope($attribute["attributeType"]));

								$xmlB->endElement(); // Konec atributu
								$oldParentClassA = $attribute["parentClass"];
							}
						}

						if ($methodCount) {
							foreach ($this->model[$parentName]->methodArray as $index => $method) {
								if ($method["name"] != $conflictItem[1]) continue;
								//if ($method["parentClass"] != $parentName) continue;
								if ($oldParentClassM == $method["parentClass"]) {
									$onlyBaseClass = true;
									continue;

								}

								$xmlB->startElement("method");
								$xmlB->writeAttribute("name", $method["name"]);
								$xmlB->writeAttribute("type", $method["type"]);
								$xmlB->writeAttribute("scope", $this->getScope($method["methodType"]));

								$xmlB->endElement(); // Konec metody
								$oldParentClassM = $method["parentClass"];
							}
						}
						$xmlB->endElement();

					} // Konec pruchodu jednotlivymi modifikatory
					$xmlB->endElement();
				}

				$xmlB->endElement();
				$oldName[] = $conflictItem[1];
			}


			$xmlB->endElement();
		}
		// konec generovani konfliktu

		foreach ($this->accessModifiers as $modifier) {
			$methodModifier = false;
			$attrModifier = false;

			// Zjisteni zda existuje alespon jedna metoda s danym modifikatorem
			foreach ($this->model[$name]->methodArray as $method)
				if ($method["modifier"] == $modifier && !$method["conflicts"] && $method["show"])
					$methodModifier = true;

			// Zjisteni zda existuje alespon jeden atribut s danym modifikatorem
			foreach ($this->model[$name]->attributesArray as $attribute)
				if ($attribute["modifier"] == $modifier && !$attribute["conflicts"] && $attribute["show"])
					$attrModifier = true;

			if (!$methodModifier && !$attrModifier)
				continue;

			$xmlB->startElement($modifier);

			if ($attrModifier) {
				$xmlB->startElement("attributes");
				foreach ($this->model[$name]->attributesArray as $attribute) {
					if ($attribute["modifier"] != $modifier || $attribute["conflicts"] || !$attribute["show"]) continue;
					$xmlB->startElement("attribute");
					$xmlB->writeAttribute("name", $attribute["name"]);
					$xmlB->writeAttribute("type", $attribute["type"]);
					$xmlB->writeAttribute("scope", $this->getScope($attribute["attributeType"]));

					if ($attribute["parentClass"] != $this->model[$name]->name) {
						$xmlB->startElement("from");
						$xmlB->writeAttribute("name", $attribute["parentClass"]);
						$xmlB->endElement();
					}
					$xmlB->endElement(); // Konec atributu
				}
				$xmlB->endElement(); //Konec Atributu
			}

			if ($methodModifier) {
				$xmlB->startElement("methods");
				foreach ($this->model[$name]->methodArray as $index => $method) {
					if ($method["modifier"] != $modifier || $method["conflicts"] || !$method["show"]) continue;
					$xmlB->startElement("method");
					$xmlB->writeAttribute("name", $method["name"]);
					$xmlB->writeAttribute("type", $method["type"]);
					$xmlB->writeAttribute("scope", $this->getScope($method["methodType"]));

					if ($method["parentClass"] != $this->model[$name]->name) {
						$xmlB->startElement("from");
						$xmlB->writeAttribute("name", $method["parentClass"]);
						$xmlB->endElement();
					}

					if ($method["methodType"] == "virtual") {
						$xmlB->startElement("virtual");
						if ($method["virtualPure"])
							$xmlB->writeAttribute("pure", "yes");
						else
							$xmlB->writeAttribute("pure", "no");
						$xmlB->endElement();
					}

					$xmlB->startElement("arguments");
					if (array_key_exists($index, $this->model[$name]->argsMethodArray)){
						foreach ($this->model[$name]->argsMethodArray[$index] as $arg) {
							$xmlB->startElement("argument");
							$xmlB->writeAttribute("name", $arg["name"]);
							$xmlB->writeAttribute("type", $arg["type"]);
							$xmlB->endElement();
						}
					}
					$xmlB->endElement(); // Konec argumentu
					$xmlB->endElement(); // Konec metody
				}
				$xmlB->endElement(); // Konec metod
			}
			$xmlB->endElement(); // Konec modifikatoru pristupu
		}
		$xmlB->endElement(); //Konec tridy
		}
	}

	// Funkce prevede boolean vyraz na scope
	function getScope($type) {
		if ($type == "static")
			return "static";
		else
			return "instance";
	}

	// Vygenerovani xml souboru podle zadani - XML A
	function generateA($xmlA) {
		if (is_null($this->model)) return;

		$xmlA->startElement('model');

		foreach ($this->model as $value) {
			if (!count($value->parentClassName)) {
				$xmlA->startElement("class");
				$xmlA->writeAttribute("name", $value->name);
				$xmlA->writeAttribute("kind", $value->getKind());
				$this->generateInheritanceTree($value->name, $xmlA);
				$xmlA->endElement();
			}
		}
		$xmlA->endElement();
	}

	// Rekurzivni generovani stromu dedicnosti pro xml A
	function generateInheritanceTree($name, $xmlA) {
		foreach ($this->model[$name]->childrens as $value) {
				$xmlA->startElement("class");
				$xmlA->writeAttribute("name", $value);
				$xmlA->writeAttribute("kind", $this->model[$value]->getKind());
				$this->generateInheritanceTree($value, $xmlA);
				$xmlA->endElement();
		}
	}

	// Rozparsovani argumentu a argumentu metody
	function parseMethodArg($data) {
		preg_match('/(.*?)\s*([^\s\*\&]*)(\s)*$/', $data, $data);
		$argType = NULL;
		$argName = NULL;

		if (in_array($data[2],$this->dataTypes)) {
			$argType = $data[0]; // Jen typ
		} else {
			if ((strpos($data[0], "*") || strpos($data[0], "&"))) {
				if (empty($data[2])) {
					$argType = $data[1]; // Typ + reference/dereference

				} else {
					$argType = $data[1];
					$argName = $data[2]; // Typ + promenna + */&
				}
			}
			else if (!empty($data[0])) {
				$argType = $data[1]; // Typ + promenna
				$argName = $data[2];
			}
		}

		$argType = str_replace("*", " *", $argType);
		$argType = preg_replace('/\s+/', ' ', trim($argType));
		return array($argType, trim($argName));
	}
} // Konec tridy

// Spusteni programu
try {
	$args = new InputArgv($argv); // Parsovani argumentu

	if ($args->conflicts && $args->details)
		$showConflicts = true;
	else
		$showConflicts = false;

	$par = new ParseInputFile($args->input, $showConflicts);
	$par->ParseInput();

	$xml = new XMLWriter();
	$xml->openMemory();
	$xml->startDocument( '1.0', 'UTF-8' );
	$xml->setIndent(true);
	$xml->setIndentString(str_repeat(" ", $args->prettyXml));

	if (!$args->details) {
		$par->generateA($xml);
	} else {
		$par->generateB($xml, $args->detailName, $showConflicts);
	}

	$xml->endDocument();
	$xml = $xml->flush();

	// Vygenerovani vystupu pro XPATH
	if (!is_null($args->search)) {
		// Prevod simpleXML na DOOMDocument
		$sxe = simplexml_load_string($xml);
		$dom_sxe = dom_import_simplexml($sxe);

		// Ulozenie
		$dom2 = new DOMDocument('1.0', 'utf-8');
		$dom_sxe = $dom2->importNode($dom_sxe, true);
		$dom_sxe = $dom2->appendChild($dom_sxe);

		// Xpath query
		$xpath = new DOMXpath($dom2);
		$elements = $xpath->query($args->search); //$args->search

		// Dokument pre vystup
		$dom = new DOMDocument('1.0', 'utf-8');
		$dom->formatOutput = true;
		$text = $dom;

		// Vypis tagu result
		$attr = false;
		$elem = false;
		if(($elements->length > 1) || ($elements->length > 0 && $elements->item(0) instanceof DOMAttr)) {
			$root = $dom->createElement('result');
			$text = $dom->appendChild($root);

			$text->nodeValue = PHP_EOL;
		}

		// Prevod vystupnych udajov do xml
		foreach ($elements as $element) {
			if (is_a($element, "DOMAttr")) {
				$text->nodeValue = $text->textContent.str_repeat(" ", $args->prettyXml).$element->value.PHP_EOL;
				$attr = true;
			} else if (is_a($element, "DOMelement")) {
				$text->appendChild($dom->importNode($element, true));
				$elem = true;
			}
		}

		$doc = new DOMDocument();
		$doc->formatOutput = true;

		if (substr_count($dom->saveXML(), "\n") <= 1) {
			file_put_contents($args->output, $dom->saveXML());
			return;
		}

		$xml = preg_replace("/(.?>)\s*/", "$1", $dom->saveXML());
		$doc->loadXML($xml);

		$lines = explode(PHP_EOL, $doc->saveXML());

		// Pridanie medzier
		// Zistkanie poctu medzier pred elementom, ich uprava na spravny pocet
		// odstranenie povodnych medzier a pridanie elementu
		$output = "";
		foreach($lines as $line) {
			preg_match("/\s*/", $line, $output_array);

			$output .= str_repeat(" ",(strlen($output_array[0])/2 * $args->prettyXml));
			$output .= preg_replace("/\s*(.?<)/", "$1", $line).PHP_EOL;
		}

		// Vypis spraveho xml
		if ($attr && $elem)
			$xml = $dom->saveXML();
		else
			$xml = $output;
	}

	file_put_contents($args->output, $xml);
}
catch (customException $e)
{
	echo $e->printError();
	exit($e->getCode());
}
?>
