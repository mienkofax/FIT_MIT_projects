<?php

namespace App\Model;

use Nette\Database\Context;
use Nette\Object;

/**
 * Rozhranie pre modely, spristupnuje pracu s databazou.
 */
abstract class BaseManager extends Object
{
	protected $database;

	const
		SORT_TYPE = array(
			'desc' => 'DESC',
			'asc' => 'ASC'
		);

	public function __construct(Context $database)
	{
		$this->database = $database;
	}

	/**
	 * Vlozi cudzie kluce do zadanych stlpcov a tabulky.
	 * @param int @primaryKey Identifikator, ktory tvori jeden z klucov,
	 * ktore sa maju vlozit do databaze
	 * @param ActiveRow $data informacie o db scheme a o aktualnej tabulke
	 * @param mixed $foreignKey Druhas cast kluca, ktory sa pridava do db
	 * @param string $column1 Nazov prveho stlpca, kde sa ma vlozit
	 * jedna polozka z pola $foreignKey
	 * @param string $column2 Nazov druheho stlpca, kde sa ma vlozit $primaryKey
	 * @param string $mnTable Nazov tabulky do, ktorej sa mju vlozit udaje
	 */
	public function insertFKKey($primaryKey, $data, $foreignKey, $column1, $column2, $mnTable)
	{
		$result = array();

		foreach ($foreignKey as $item)
			$result[] = array($column1 => $item, $column2 => $primaryKey);

		if ($result)
			$this->database->table($mnTable)->insert($result);
	}

	/**
	 * Odstranenie pozadovanych klucov z tabulky, ktora spaja dve tabulky tupu M:N.
	 * @param ActiveRow $data informacie o db scheme a o aktualnej tabulke
	 * @param string destinationFKColumn Stlpec tabulky, kde sa ma skontrolovat
	 * a pripadne odstranit cudzi kluc
	 * @return mixed Pole cudzich klucov, tieto kluce je mozne vlozit ako nove
	 */
	public function removeFKKey($data, $destinationFKColumn, $fKey)
	{
		foreach ($data as $key => $item) {
			if (!in_array($item[$destinationFKColumn], $fKey))
				$item->delete();

			if(($key2 = array_search($item[$destinationFKColumn], $fKey)) !== false)
				unset($fKey[$key2]);
		}

		return $fKey;
	}

	/**
	 * Ak sa jedna o prazdny string vrati 0 inak zadane cislo
	 */
	public function defaultNumberZero($number)
	{
		return ($number == "") ? 0 : $number;
	}
}
