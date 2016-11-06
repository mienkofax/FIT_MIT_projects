<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;

/**
 * Model pre pracu s jednotlivymi datavatelmi. Umoznuje, editovanie a
 * vypis dadavatelov. Vypis dodavatelov moze byt ako detail jedneho dodavatela
 * alebo zoznam dodavatelov.
 */
class SupplierManager extends BaseManager
{
	const
		TABLE_NAME = 'dodavatele',
		COLUMND_ID = 'ID_dodavatele',
		SORT_TABLE = array(
			'nazov' => 'nazev_dodavatele',
			'cas' => 'date_time'
		);

	/**
	 * Vyber vsetkych dodavatelov z databaze a ich pripadne zotriedenie.
	 * @param string $column Nazov stlpca podla, ktoreho sa ma triedit
	 * @param string $sort Typ akym sa ma urobit triedenie ASC alebo DESC
	 * @return mixed Zoznam dodavatelov
	 */
	public function getSuppliers($column, $sort)
	{
		$default_column = self::SORT_TABLE['nazov'];
		$default_sort = self::SORT_TYPE['asc'];

		// Kontrola ci bol zadany validny slpec pre zotriedenie
		if (isset(self::SORT_TABLE[$column]))
			$default_column = self::SORT_TABLE[$column];

		// Kontrola ci bolo zadane v akom poradi sa ma maju vypisat
		if (isset(self::SORT_TYPE[$sort]))
			$default_sort = self::SORT_TYPE[$sort];

		return $this->database->table(self::TABLE_NAME)
			->order($default_column . ' ' . $default_sort);
	}

	/**
	 * Vyber pozadovaneho dodavatela z databaze.
	 * @param int $id Identifikator dodavatela v databaze
	 * @return mixed Dodavatel
	 */
	public function getSupplier($id)
	{
		return $this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->fetch();
	}

	/**
	 * Ulozi dodavatela do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o dodavatelovi, inak sa edituje existujuci dodavatel.
	 * @param mixed $office Dodavatelo, ktory sa ma upravit alebo vlozit
	 */
	public function saveSupplier($office)
	{
		if (!$office[self::COLUMND_ID]) {
			unset($office[self::COLUMND_ID]);
			$this->database->table(self::TABLE_NAME)->insert($office);
		} else
			$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
				$office[self::COLUMND_ID])->update($office);
	}

	/**
	 * Odstrani dodavatela z databaze.
	 * @param int $id Identifikator dodavatela v databaze
	 */
	public function removeSupplier($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->delete();
	}

}
