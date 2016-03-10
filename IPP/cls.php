<?php

#CLS:xtisov00
/**
 * @author Peter Tisovcik <xtisov00@stud.fit.vutbr.cz>
 */

/**
 * Zdedena trieda v seve implementuje vypisovanie chybovych hlasok
 * na STDERR, umoznuje vypisat kod a chybu.
 */
class customException extends Exception {
		public $message;
		public $code;

		public function __construct($message, $code) {
			$this->message = $message."\n";
			$this->code = $code;
		}
		public function printError() {
			fwrite(STDERR, $this->message);
		}
}


/**
 * Trieda implementuje parsovanie jednotlivych argumentov. Osetruje stavy,
 * ked je zadany neplatny argument, ked je zadany nespravny format argumentu a
 * osetruje stavy, ked su zadane dlhe a kratke prepinace, ich kombinacia.
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
	 * Naplni vnutornu strukturu zadanymi argumentami.
	 */
	public function __construct($argv) {
		$this->parseArguments($argv);
	}

	/**
	 * Vypis help message.
	 */
	private function getHelp() {
		echo "HELP MSG";
	}

	/**
	 * Vracia hodnotu podla toho ci bol pouzity argument pri spustani.
	 * @return		bool
	 */
	public function isDetails() {
		if (!is_null($this->details))
			return true;
		return false;
	}

	/**
	 * Vracia hodnotu podla toho ci bol pouzity argument pri spustani.
	 * @return		bool
	 */
	private function isSearch() {
		if (!is_null($this->search))
			return true;
		return false;
	}

	/**
	 * Rozparsovanie argumentova na zaklade stanovenych pravidiel
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

		// Parsovanie argumentov
		$options = getopt($shortopts, $longopts);

		// Ak sa zadal neplatny argument typu: -hxxxxx
		if (array_key_exists('h', $options))
			if (is_bool($options['h']) === false)
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);

		// Osetrenie ak sa opakuje nejaky prepinac
		foreach ($options as $value) {
			if (is_array($value))
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);
		}

		// Osetrenie ak sa zadal neznamy argument
		$optionsLen = count($options);
		if ($optionsLen != (count($argv) - 1))
			throw new customException("Bad input arguments. Try --help.", self::E_ARG);

		// Test, ci bol zadany --help|-h prepinac bez ostatnych
		if (array_key_exists("help", $options)) {
			if ($optionsLen > 1)
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);

			$this->getHelp();
			exit(self::OK);
		}

		// Kontrola kratkych prepinacov a dlhyzch prepinacov: --output=file -o=file
		// Zjednotenie formatu na dlhe prepinace, pre jednotne pouzivanie dalej
		foreach ($longopts as $value) {
			$firstChar = substr($value, 0, 1);
			$value = str_replace(":", "", $value);

			// Duplikacia prepinacov
			if (array_key_exists($value, $options) && array_key_exists($firstChar, $options))
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);
			else {
				// Zjednotenie na dlhe prepinace: -o=file >>> --output=file
				if (array_key_exists($firstChar, $options)) {
					$options[$value] = $options[$firstChar];
					unset($options[$firstChar]);
				}
			}
		}

		// Kontrola vstupu, ci existuje a ci sa da z daneho suboru citat
		if (array_key_exists("input", $options)) {
			if (!file_exists($options['input']) || !is_readable($options['input']))
				throw new customException("Bad input file.", self::E_INPUT_FILE);

			$this->input = $options['input'];
		}

		// Kontrola vystupneho suboru, ci sa da vytvorit a zapisat do neho
		if (array_key_exists("output", $options)) {
			if (is_writable($options['output'])) {
				// Vytvori subor pre zapis, ak sa nepodari skonci chybou
				if ( !($fw = fopen($options['output'], "w")) )
					throw new customException("Bad output file.", self::E_OUTPUT_FILE);
			} else {
				// Ak existuje ale nie je pre zapis, skonci chybou
				if (file_exists($options['output']))
					throw new customException("Bad output file.", self::E_OUTPUT_FILE);

				// Subor neexistuje, vytvori sa ak sa nepodari, skonci chybou
				if ( !($fw = fopen($options['output'], "w")) )
					throw new customException("Bad output file.", self::E_OUTPUT_FILE);
			}
			$this->output = $options['output'];
		}

		// Kontrola pretty-xml, kontrola ci je to kladne cele cislo,
		// ak nebolo zadane nastavi sa na prevolenu velkost
		if (array_key_exists("pretty-xml", $options)) {
			// Kontrola ci obsahuje iba cisla
			if (ctype_digit($options['pretty-xml']))
				$this->prettyXml = intval($options['pretty-xml']);

			// Ak nebola nastavena prepinacu hodnota, pouzije sa predvolena
			elseif (is_bool($options['pretty-xml']) === true)
				$this->prettyXml = 4;
			else
			 	throw new customException("Bad input format.", self::E_ARG);
		}

		if (array_key_exists("details", $options)) {
			$this->details = true;

			if ($options['details'] != "")
				$this->detailName = $options['details'];
		}

		if (array_key_exists("search", $options)) {
			if ($options['search'] == "")
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);

			$this->search = $options['search'];
		}

		if (array_key_exists("conflicts", $options)) {
			$this->conflicts = true;
			if (is_bool($options['conflicts']) === false)
				throw new customException("Bad input arguments. Try --help.", self::E_ARG);
		}
	}
}

/**
 * Sablona pre jednotlive triedy, v ktorej sa budu ukladat vsetky potrebne
 * informacie o triede, ktore sa daju ziskat zo vstupneho suboru.
 */
 class TemplateClass{
	private $conflicts = false;
	/* Nazov spracovanej triedy */
	public $name;

	/* Typ abstract ak je abstraktna | concreate - ak nie je abstraktna */
	public $kind = false;

	/* Pole obsahuje, ktore triedy z nej dedili */
	public $childrens = array();

	/**
	 * Pole nazvov tried, s ktorych trieda dedi - len priamy potomkovia
	 * modifikator pristupu | nazov
	 */
	public $parentClassName = array();

	/**
	 * Pole metod patriace k danej triede. Priota urcuje vahu pri dedeni/pridani
	 * priorita: 0 - using, pridanie; 1 - dedenie
	 * modifikator pristupu | typ metody | datovy typ | nazov | virtual pure | priorita
	 */
	public $methodArray = array();

	/**
	 * Pole argumentov jednotlivych metod. Priota urcuje vahu pri dedeni/pridani
	 * priorita: 0 - using, pridanie; 1 - dedenie
	 * index metody | nazov | typ | priorita
	 */
	public $argsMethodArray = array();

	/**
	 * Pole atributov danej triedy
	 * modifikator pristupu | typ premennej | datovy typ | nazov | priorita
	 */
	public $attributesArray = array();

	function __construct($name, $conflicts) {
		$this->name = $name;
		$this->conflicts = $conflicts;
	}

	/* Nastavenie ci je trieda abstraktna */
	function setKind($kind) {
		$this->kind = $kind;
	}

	function getKind() {
		if ($this->kind)
			return "abstract";
		return "concrete";
	}

	/* Pridanie potomka do triedy s kade sa dedi */
	function addChild($name)  {
		array_push($this->childrens, $name);
	}

	/* Pridanie atributu do triedy */
	function addAttributeClass($accessModifier, $attributeType, $dataType, $name) {
		array_push($this->attributesArray, array("modifier" => $accessModifier, "attributeType" => $attributeType, "type" => $dataType, "name" => $name, "priority" => true, "parentClass" => $this->name, "conflicts" => false));
	}

	/* Pridanie metody do triedy */
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

		array_push($this->methodArray, array("modifier" => $accessModifier, "methodType" => $methodType, "type" => $dataType, "name" => $name, "virtualPure" => false, "priority" => true, "parentClass" => $this->name, "conflicts" => false));
	}

	/* Pridanie tiredy, z ktorej sa dedi */
	function addParentClass($accessModifier, $parentClassName) {
		$this->parentClassName[$parentClassName] = array("modifier" => $accessModifier, "name" => $parentClassName);
	}

	/* Pridanie argumentu do metody */
	function addMethodArg($actMethod, $type, $name) {
		if ($type == "void" || (empty($type) && empty($name) ))
			return;
		$methodIndex = $this->getLastMethodIndex();

		// Vytvorenie pola ak este neexistuje
		if (!is_array(@$this->argsMethodArray[$methodIndex]))
			$this->argsMethodArray[$methodIndex] = array();

		array_push($this->argsMethodArray[$methodIndex], array("name" => $name, "type" => $type));
	}

	/* Definovanie, z ktorej triedy sa ma dedit atribut/metoda */
	function addUsing($usingClass, $using, $modifier) {
		// Odstraneni neziaducej metody
		foreach ($this->methodArray as $key => $value) {
			if ($value["name"] ==  $using) {
				$this->methodArray[$key]["modifier"] = $modifier;
				if ($value["parentClass"] == $usingClass->name) {
					//TODO osetrit aby sa rovnali aj argumenty
				} else
					unset($this->methodArray[$key]);
			}
		}

		// Odstraneni neziaduceho atributu
		foreach ($this->attributesArray as $key => $value) {
			if ($value["name"] ==  $using ) {
				$this->attributesArray[$key]["modifier"] = $modifier;

				if ($value["parentClass"] != $usingClass->name)
					unset($this->attributesArray[$key]);
			}
		}
	}

	/* Ziskanie idexu poslednej pridanej metody */
	private function getLastMethodIndex() {
		end($this->methodArray);
		return key($this->methodArray);
	}

	/* Ziskanie idexu posledneho pridaneho indexu */
	private function getLastAttributeIndex() {
		end($this->attributesArray);
		return key($this->attributesArray);
	}

	/* Ak je metoda virtualPure nastavenie aj tried*/
	public function setVirtualPure() {
		$methodIndex = $this->getLastMethodIndex();
		$this->methodArray[$methodIndex]["virtualPure"] = true;
	}

	/* Postupne sa skopiruju jednotlive atributy a metody s ich argumentami */
	function CopyClass($from, $accessModifier) {
		// Kopirovanie atributov
		foreach ($from->attributesArray as $key => $value) {
			if ($value["conflicts"]) continue;
			array_push($this->attributesArray, $value);

			$lastIndex = $this->getLastAttributeIndex();
			$this->attributesArray[$lastIndex]["priority"] = false;

			if ($accessModifier == "private")
				$this->attributesArray[$lastIndex]["modifier"] = $accessModifier;
			if ($accessModifier == "protected") {
				if ($this->attributesArray[$lastIndex]["modifier"] != "private")
					$this->attributesArray[$lastIndex]["modifier"] = $accessModifier;
			}
		}

		// Kopirovanie metod a argumentov metod
		foreach ($from->methodArray as $key => $value) {
			if ($from->name == $value["type"] || ("~".$from->name) == $value["type"]) continue;

			if ($value["conflicts"]) continue;
			array_push($this->methodArray, $value);

			$lastIndex = $this->getLastMethodIndex();
			$this->methodArray[$lastIndex]["priority"] = false;

			if ($accessModifier == "private")
				$this->methodArray[$lastIndex]["modifier"] = $accessModifier;
			if ($accessModifier == "protected") {
				if ($this->methodArray[$lastIndex]["modifier"] != "private")
					$this->methodArray[$lastIndex]["modifier"] = $accessModifier;
			}

			// Kopirovanie argumentov metody
			if (array_key_exists($key, $from->argsMethodArray))
			$this->argsMethodArray[$lastIndex] = $from->argsMethodArray[$key];
		}
	}

	/* Kontrola, ci sa niektore premenne a metody neredefinovali, nastavevnie kind */
	function CheckClass() {
		foreach ($this->methodArray as $key => $value) {
			foreach ($this->methodArray as $key2 => $value2) {
				// Odstranenie prepisanej metody
				if ($value["name"] == $value2["name"] && $value["priority"] != $value2["priority"])  {
					//TODO osetrit ak sa nerovnaju argumenty
					if (!$value["priority"])
						unset($this->methodArray[$key]);
					if (!$value2["priority"])
						unset($this->methodArray[$key2]);
				}
			}
		}

		// Kontrola ci sa neredefinovali atributy
		foreach ($this->attributesArray as $key => $value) {
			foreach ($this->attributesArray as $key2 => $value2) {
				// Odstranenie prepisanej metody
				if ($value["name"] == $value2["name"] && $value["priority"] != $value2["priority"])  {
					if (!$value["priority"])
						unset($this->attributesArray[$key]);
					if (!$value2["priority"])
						unset($this->attributesArray[$key2]);
				}
			}
		}

		// Ak sa medzi metodami objavi virtual, nastavi sa class ako abstraktna
		foreach ($this->methodArray as $key => $value) {
			if ($value["virtualPure"])
				$this->setKind(true);
		}
		return $this->CheckConflict();
	}

	/* Kontrola konflikotov pri dedine */
	function CheckConflict() {
		$conflictClass = false;

		foreach ($this->methodArray as $key => $value) {
			foreach ($this->methodArray as $key2 => $value2) {
				// Kontrola konfliktov
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

		foreach ($this->attributesArray as $key => $value) {
			foreach ($this->attributesArray as $key2 => $value2) {
				// Kontrola konfliktov
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

		foreach ($this->attributesArray as $key => $value) {
			foreach ($this->methodArray as $key2 => $value2) {
				// Kontrola konfliktov
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
} // Koniec triedy

/**
 * Spracovanie vstupneho suboru pomocou konecneho automatu. Spracovanie prebieha v
 * jednotlivych stavov, v ktorych sa spracuvaju informacie o jednodlivych polozkach.
 * Nacitavanie prebieha po znaku, ktore sa skladaju do blokov, ktore vyuziva funkcia,
 * ktora prijma na vstup pole znakov a v pripade, ze sa aktualne spracovany znak
 * nachadza v tomto poli zastavi sa spracovanie a vrati sa string zlozeny z nacitanych
 * znakov.
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
	 * Ulozenie deskriptora na aktualne spracovany subor.
	 * @param		$file		vstupny subor
	 */
	public function __construct($file, $conflicts) {
		$this->file = file_get_contents($file);
		$this->conflicts = $conflicts;
	}

	/**
	 * Vrati naposledy precitany znak zo suboru (podobne ungetc).
	 * @return		char		posledne nacitany znak
	 */
	private function GetPreviousChar() {
		if ($this->positionInFile > 0)
			$this->positionInFile--;

		return $this->ReadChar($this->file);
	}

	/**
	 * Presuenie ukazovatel v subore o zadanu hodnotu dozadu.
	 * @param		int			posun ukazovatela v subore
	 */
	private function ReturnCharToFile($count) {
		while($count) {
			if ($this->positionInFile > 0)
				$this->positionInFile--;
			$count--;
		}
	}

	/**
	 * Odstranenie bielych znakov po najblizsi 'normalny' znak.
	 */
	private function RemoveWhiteSpace() {
		while (in_array($this->ReadChar(), $this->whiteSpace));
		$this->ReturnCharToFile(1);
	}

	/**
	 * Vracia true, v pripade, ze sa ukazovatel v subore dostal na koniec suboru.
	 * @return 		bool
	 */
	public function isEOF() {
		return $this->EOF;
	}

	/**
	 * Precita znak zo suboru a vrati ho. V pripade, ze sa precital koniec suboru,
	 * nastavi sa premenna $EOF na true a vracia sa iba prazdny retazec.
	 * @return 		string			nacitany znak
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
	 * Nacitanie znakov zo suboru, pokial sa niektori z nebude nachadzat
	 * v zadanom poli.
	 * @param		array 			pole znakov, po ktore sa ma nacitavat
	 * @return 		string			nacitany retazec
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
	 * Rozparsovanie vstupneho suboru na potrebne informacie pomocu
	 * pomocou konecneho automatu.
	 */
	public function ParseInput() {
		$actState = self::S_START;
		$actClass = "";
		$accessModifier = "private";
		$actMethod = "";
		$type = NULL; // Method | atribut type

		while (!$this->isEOF()) {
			// Zaciatok konecneho automatu
			if ($actState == self::S_START) {
				$to = $this->whiteSpace;
				$this->ReadTo($to);

				$actState = self::S_CLASS_NAME;
			}
			// Nacitanie nazvu triedy
			else if ($actState == self::S_CLASS_NAME) {
				$to = array("{", ":");
				$data = $this->ReadTo($to);
				$data = preg_replace('/\s+/', '', $data);

				// Pridany nazov triedy do objektu
				$this->model[$data] = new TemplateClass($data, $this->conflicts);

				$actClass = $data;
				$this->dataTypes[] = $data;
				$this->dataTypes[] = "~".$data;
				if ($this->GetPreviousChar() == ":")
					$actState = self::S_INHERITANCE;
				else
					$actState = self::S_BODY;
			}
			// Nacitanie triedy, z ktorych sa dedi
			else if ($actState == self::S_INHERITANCE) {
				$to = array(",", "{");
				$data = $this->ReadTo($to);
				$data = preg_split('/\s+/', $data,  NULL, PREG_SPLIT_NO_EMPTY);

				// Ulozenie informacii o triede s kade sa dedi
				if (count($data) == 2) {
					$this->model[$data[1]]->addChild($actClass);
					$this->model[$actClass]->CopyClass($this->model[$data[1]], $data[0]);
					$this->model[$actClass]->AddParentClass($data[0], $data[1]);
				} else {
					$this->model[$data[0]]->addChild($actClass);
					$this->model[$actClass]->CopyClass($this->model[$data[0]], "private");
					$this->model[$actClass]->AddParentClass("private", $data[0]);
				}

				if ($this->GetPreviousChar() == ",")
					$actState = self::S_INHERITANCE;
				else
					$actState = self::S_BODY;
			}
			// Nacitavanie tela triedy
			else if ($actState == self::S_BODY) {
				$this->RemoveWhiteSpace(); // Odstranenie bielych znakov pred citanim
				$data = "";
				$search = false;

				// Kontrola na klucove slova
				// private|public|protected OR using OR static|virtual OR int,float...
				while (true) {
					$char = $this->ReadChar();
					$data .= $char;

					// Ak je prazdne telo preskoci sa na koniec triedy.
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
					}

					// Posun ukozovatela v subore, na stav pred zacatim nacitavania
					if ($search) {
						$this->ReturnCharToFile(strlen($data));
						break;
					}
				}
			}
			// Spracovanie konca triedy
			else if ($actState == self::S_END_BODY_CLASS) {
				$to = array(";");
				$this->ReadTo($to);

				if ($this->GetPreviousChar() == ";")
					$actState = self::S_START;


				if ($this->model[$actClass]->CheckClass())
					$this->conflictClass[] = $actClass;

				$accessModifier = "private";
			}
			// Spracovanie modifikatorov pristuptu
			else if ($actState == self::S_ACCESS_MODIFIERS) {
				$to = array(":");
				$data = $this->ReadTo($to);

				$accessModifier = $data;
				$actState = self::S_BODY;
			}
			// Spracovanie typu metody|atributu - virtual|static
			else if ($actState == self::S_METHOD_TYPE) {
				$to = $this->whiteSpace;
				$type = $this->ReadTo($to);

				$actState = self::S_BODY;
			}
			// Spracovanie datoveho typu a nazvu premennej/metody
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
			// Prechod do spristupnenia metody|atributu
			else if ($actState == self::S_USING_CLASS) {
				$to = array(";");
				$data = $this->ReadTo($to);
				$data = str_replace("::", " ", $data);
				$data = preg_split('/\s+/', $data,  NULL, PREG_SPLIT_NO_EMPTY);

				$this->model[$actClass]->addUsing($this->model[$data[0]], $data[1], $accessModifier);

				$actState = self::S_BODY;
			}
			// Spracovanie argumentov metody
			else if ($actState == self::S_METHOD_ARGV) {
				$to = array(",", ")");

				$data =$this->parseMethodArg($this->ReadTo($to));

				$this->model[$actClass]->addMethodArg($actMethod, $data[0], $data[1]);

				if ($this->GetPreviousChar() == "," && count($data) != 0)
					$actState = self::S_METHOD_ARGV;
				else
					$actState = self::S_ABSTRACT_METHOD;
			}
			// Spracovanie abstraktnej metody
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
	} // Koniec metody

	// Generovanie vystupnute XML - B
	function generateB($xmlB, $className, $conflict) {
		if (!count($this->model)) return;

		if (is_null($className))
			$xmlB->startElement("model");

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

		// Generovanie konfliktov
		if ($conflict && $conflictExists) {
			$xmlB->startElement("conflicts");
			$data = array();

			// Nacitanie konfliktnych metod/atributov do pola
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
			foreach ($data[$name] as $key => $value) {
				foreach ($data[$name] as $key2 => $value2) {
					if (@$this->model[$name]->attributesArray[$value[0]]["parentClass"] == @$this->model[$name]->attributesArray[$value2[0]]["parentClass"] && $key != $key2)
						{
							unset($data[$name][$key]);
						}
				}
			}

			$oldName = array();

			foreach ($data[$name] as $key2 => $conflictItem) {
				if (in_array($conflictItem[1], $oldName)) continue;

				$xmlB->startElement("member");
				$xmlB->writeAttribute("name", $conflictItem[1]);

				$oldParentClassA = "";
				$oldParentClassM = "";
				$onlyBaseClass = false;
				$indexParent = 0;

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

					/////////////////////////////////////////////
					// Prechod jednotlvymi modifikatormi
					// Prechod jednotlivymi konfliktnymi metodami/atributami
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

								$xmlB->endElement(); // Koniec atributu
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

								$xmlB->endElement(); // Koniec metody
								$oldParentClassM = $method["parentClass"];
							}
						}
						$xmlB->endElement();

					} // Koniec prechodu jednotlivymi modifikatormi
					/////////////////////////////////////////////

					$xmlB->endElement();
				}

				$xmlB->endElement();
				$oldName[] = $conflictItem[1];
			}


			$xmlB->endElement();
		}
		// koniec generovania konfliktu

		foreach ($this->accessModifiers as $modifier) {
			$methodModifier = false;
			$attrModifier = false;

			// Zistenie ci existuje aspon jedna metoda s danym modifikatorom
			foreach ($this->model[$name]->methodArray as $method)
				if ($method["modifier"] == $modifier && !$method["conflicts"])
					$methodModifier = true;

			// Zistenie ci existuje aspon jeden atribut s danym modifikatorom
			foreach ($this->model[$name]->attributesArray as $attribute)
				if ($attribute["modifier"] == $modifier && !$attribute["conflicts"])
					$attrModifier = true;

			if (!$methodModifier && !$attrModifier)
				continue;

			$xmlB->startElement($modifier);

			if ($attrModifier) {
				$xmlB->startElement("attributes");
				foreach ($this->model[$name]->attributesArray as $attribute) {
					if ($attribute["modifier"] != $modifier || $attribute["conflicts"]) continue;
					$xmlB->startElement("attribute");
					$xmlB->writeAttribute("name", $attribute["name"]);
					$xmlB->writeAttribute("type", $attribute["type"]);
					$xmlB->writeAttribute("scope", $this->getScope($attribute["attributeType"]));

					if ($attribute["parentClass"] != $this->model[$name]->name) {
						$xmlB->startElement("from");
						$xmlB->writeAttribute("name", $attribute["parentClass"]);
						$xmlB->endElement();
					}
					$xmlB->endElement(); // Koniec atributu
				}
				$xmlB->endElement(); //Koniec Atributov
			}

			if ($methodModifier) {
				$xmlB->startElement("methods");
				foreach ($this->model[$name]->methodArray as $index => $method) {
					if ($method["modifier"] != $modifier || $method["conflicts"]) continue;
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
					$xmlB->endElement(); // Koniec argumentov
					$xmlB->endElement(); // Koniec metody
				}
				$xmlB->endElement(); // Koniec metod
			}
			$xmlB->endElement(); // Koniec modifikatora pristupu
		}
		$xmlB->endElement(); //Koniec triedy
		}
	}

	// Funkcia prevedia boolean vyraz na scope
	function getScope($type) {
		if ($type == "static")
			return "static";
		else
			return "instance";
	}

	// Vygenerovanie xml suboru podla zadania - XML A
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

	// Rekurzivne generovanie stromu dedicnosti pre xml A
	function generateInheritanceTree($name, $xmlA) {
		foreach ($this->model[$name]->childrens as $value) {
				$xmlA->startElement("class");
				$xmlA->writeAttribute("name", $value);
				$xmlA->writeAttribute("kind", $this->model[$value]->getKind());
				$this->generateInheritanceTree($value, $xmlA);
				$xmlA->endElement();
		}
	}

	// Rozparsovanie argumentov a argumentov metody
	function parseMethodArg($data) {
		preg_match('/(.*?)\s*([^\s\*\&]*)(\s)*$/', $data, $data);
		$argType = NULL;
		$argName = NULL;

		if (in_array($data[2],$this->dataTypes)) {
			$argType = $data[0]; // Len typ
		} else {
			if ((strpos($data[0], "*") || strpos($data[0], "&"))) {
				if (empty($data[2])) {
					$argType = $data[1]; // Typ + referencia/dereferencia

				} else {
					$argType = $data[1];
					$argName = $data[2]; // Typ + premenna + */&
				}
			}
			else if (!empty($data[0])) {
				$argType = $data[1]; // Typ + premenna
				$argName = $data[2];
			}
		}

		$argType = str_replace("*", " *", $argType);
		$argType = preg_replace('/\s+/', ' ', trim($argType));
		return array($argType, trim($argName));
	}
} // Koniec triedy

// Spustenie programu
try {
	$args = new InputArgv($argv); // Parsovanie argumentov

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

	// Vygenerovanie vystupu pre XPATH
	if (!is_null($args->search)) {
		$xmlTmp = new SimpleXMLElement($xml);
		$result = $xmlTmp->xpath($args->search);

		$xml = new XMLWriter();
		$xml->openMemory();
		$xml->startDocument( '1.0', 'UTF-8' );
		$xml->setIndent(true);
		$xml->setIndentString(str_repeat(" ", $args->prettyXml));

		$str = "\n";
		while(list(,$node) = each($result))
			$str .= str_repeat(" ", $args->prettyXml).$node."\n";
		$xml->startElement("result");
		$xml->text($str);
		$xml->endElement();
		$xml->endDocument();
		$xml = $xml->flush();
	}

	file_put_contents($args->output, $xml);
}
catch (customException $e)
{
	echo $e->printError();
	exit($e->getCode());
}

//TODO dorobit kontrolu argumentov
?>
