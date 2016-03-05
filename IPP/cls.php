<?php

/**
 * @author Peter Tisovcik <xtisov00@stud.fit.vutbr.cz>
 *
 */

/* Define error code */
const OK = 0;
const E_ARG = 1;
const E_INPUT_FILE = 2;
const E_OUTPUT_FILE = 3;
const E_INPUT_FORMAT = 4;

/**
 * Write message to stderr and exit
 * @param int $exitCode

 */
function errorMessage($exitCode) {
	$message = "";

	switch($exitCode) {
		case E_ARG:
			$message = "Bad input arguments. Try --help.";
			break;
		case E_INPUT_FILE:
			$message = "Bad input file.";
			break;
		case E_OUTPUT_FILE:
			$message = "Bad output file.";
			break;
		case E_INPUT_FORMAT:
			$message = "Bad input format.";
			break;
		default:
			$message = "Undefined error.";
			break;
	}

	$message .= "\n";
	$stderr = fopen("php://stderr", "w+");
	fwrite($stderr, $message);

	exit($exitCode);
}

/**
 * Parse arguments
 * @param mixed $argv input arguments
 * @return mixed
 */
function parseArgv($argv) {
	$shortopts = "h::i:o:p::d:s:";

	$longopts = array(
		"help::", // --help
		"input:", // --input=file
		"output:", // --output=file
		"pretty-xml::", // --pretty-xml=k
		"details:", // --details=class_uses
		"search:" // --search=XPATH
	);

	//parse argument
	$options = getopt($shortopts, $longopts);
	//var_dump($options);
	//var_dump($argv);

	#if contain -hxxxxx
	if (array_key_exists('h', $options))
		if ($options['h'])
			errorMessage(E_ARG);

	//repeat arg
	foreach ($options as $value) {
		if (is_array($value))
			errorMessage(E_ARG);
	}

	//unknown argument
	$optionsLen = count($options) ;
	if ($optionsLen != (count($argv) - 1))
		errorMessage(E_ARG);

	if (array_key_exists("help", $options)) {
		//only help parameter
		if ($optionsLen > 1)
			errorMessage(E_ARG);
		//help();
		exit(OK);
	}

	//check short and long arguments: --output=file -o=file
	//change short to long arguments
	foreach ($longopts as $value) {
		$firstChar = substr($value, 0, 1);
		$value = str_replace(":", "", $value);

		//duplicate arguments
		if (array_key_exists($value, $options) && array_key_exists($firstChar, $options))
			errorMessage(E_ARG);
		else {
			if (array_key_exists($firstChar, $options)) {
				$options[$value] = $options[$firstChar];
				unset($options[$firstChar]);
			}
		}

	}
	//var_dump($options);

	//input, check file exist and readable
	if (array_key_exists("input", $options)) {
		if (!file_exists($options['input']))
			errorMessage(E_INPUT_FILE);

		if (!is_readable($options['input']))
			errorMessage(E_INPUT_FILE);
	} else
		$options['input'] = "php://stdin";

	//output, check writable and overwrite
	if (array_key_exists("output", $options)) {
		if (is_writable($options['output'])) {
			//create file for write or exit
			if ( !($fw = fopen($options['output'], "w")) )
				errorMessage(E_OUTPUT_FILE);
		} else {
			//file exist but is'n writable
			if (file_exists($options['output']))
				errorMessage(E_OUTPUT_FILE);

			//doesn't exist, try creating
			if ( !($fw = fopen($options['output'], "w")) )
				errorMessage(E_OUTPUT_FILE);
		}
	} else
		$options['output'] = "php://stdout";

	//pretty-xml, check that number is posive number and is'n string/float...
	if (array_key_exists("pretty-xml", $options)) {
		if (ctype_digit($options['pretty-xml']))
			$options['pretty-xml'] = intval($options['pretty-xml']);
		elseif (is_bool($options['pretty-xml']))
			$options['pretty-xml'] = 4;
		else
		 	errorMessage(E_ARG);
	}
	var_dump($options[]);
	return $options;
}

$options = parseArgv($argv);



if (($files = file_get_contents("test.input")) === false)
	echo "zo suboru sme nedostali co sme chceli";




function readTo($to, $file) {
	$data = "";
	$file = str_split($file);

	foreach ($file as $value) {
		if (in_array($value, $to))
			break;

		$data .= $value;
	}
	$files = "";

	return strlen($data);
}



$to = array(" ");
$number = readTo($to, $files);
echo substr($files, 0, $number);
$files = substr($files, $number);
echo $files;
echo "---------";





?>
