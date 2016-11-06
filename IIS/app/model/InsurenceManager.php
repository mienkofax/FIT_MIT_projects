<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;

/**
 * Model pre pracu s jednotlivymi poistovnami. Umoznuje, editovanie a
 * vypis poistovni. Vypis poistovni moze byt ako detail jednej poistovne
 * alebo zoznam poistovni.
 */
class InsurenceManager extends BaseManager
{
	const
		TABLE_NAME = 'pojistovny',
		COLUMND_ID = 'ID_pojistovny',
		SORT_TABLE = array(
			'nazov' => 'nazev_pojistovny',
			'cas' => 'date_time'
		);

	/**
	 * Vyber vsetkych pojistovni z databaze a ich pripadne zotriedenie.
	 * @param string $column Nazov stlpca podla, ktoreho sa ma triedit
	 * @param string $sort Typ akym sa ma urobit triedenie ASC alebo DESC
	 * @return mixed Zoznam pojistovni
	 */
	public function getInsurences($column, $sort)
	{
		$default_column = self::SORT_TABLE['nazov'];
		$default_sort = self::SORT_TYPE['asc'];

		// Kontrola ci bol zadany validny slpec pre zotriedenie
		if (isset(self::SORT_TABLE[$column]))
			$default_column = self::SORT_TABLE[$column];

		// Kontrola ci bolo zadane v akom poradi sa ma maju vypisat pobocky
		if (isset(self::SORT_TYPE[$sort]))
			$default_sort = self::SORT_TYPE[$sort];

		return $this->database->table(self::TABLE_NAME)
			->order($default_column . ' ' . $default_sort);
	}

	/**
	 * Vyber pozadovanej pojistovne z databaze.
	 * @param int $id Identifikator pojistovne v databaze
	 * @return mixed Poistovna
	 */
	public function getInsurence($id)
	{
		return $this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->fetch();
	}

	/**
	 * Ulozi pojistovnu do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o pojistovni, inak sa edituje existujuca pojistovna.
	 * @param mixed $office Poistovna, ktora sa ma upravit alebo vlozit
	 */
	public function saveInsurence($office)
	{
		if (!$office[self::COLUMND_ID]) {
			unset($office[self::COLUMND_ID]);
			$this->database->table(self::TABLE_NAME)->insert($office);
		} else
			$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
				$office[self::COLUMND_ID])->update($office);
	}

	/**
	 * Odstrani poistovnu z databaze.
	 * @param int $id Identifikator poistovne v databaze
	 */
	public function removeInsurence($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->delete();
	}

}
