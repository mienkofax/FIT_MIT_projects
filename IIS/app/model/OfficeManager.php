<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;

/**
 * Model pre pracu s jednotlivymi pobockami. Umoznuje vytvorenie, editovanie a
 * vypis pobočiek. Vypis pobociek moze byt ako detail jednej pobocky alebo
 * zoznam pobociek.
 */
class OfficeManager extends BaseManager
{
	const
		TABLE_NAME = 'pobocky',
		COLUMND_ID = 'ID_pobocky',
		SORT_TABLE = array(
			'nazov' => 'nazev_pobocky',
			'cas' => 'date_time'
		);

	/**
	 * Vyber vsetkych pobociek z databaze a ich pripadne zotriedenie.
	 * @param string $column Nazov stlpca podla, ktoreho sa ma triedit
	 * @param string $sort Typ akym sa ma urobit triedenie ASC alebo DESC
	 * @return mixed Zoznam pobociek
	 */
	public function getOffices($column, $sort)
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
	 * Vyber pozadovanej pobocky z databaze.
	 * @param int $id Identifikator pobocky v databaze
	 * @return mixed Pobocka
	 */
	public function getOffice($id)
	{
		return $this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->fetch();
	}

	/**
	 * Ulozi pobocku do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o pobocke, inak sa edituje existujuca pobocka.
	 * @param mixed $office Pobocka, ktora sa ma upravit alebo vlozit
	 */
	public function saveOffice($office)
	{
		if (!$office[self::COLUMND_ID]) {
			unset($office[self::COLUMND_ID]);
			$this->database->table(self::TABLE_NAME)->insert($office);
		} else
			$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
				$office[self::COLUMND_ID])->update($office);
	}

	/**
	 * Odstrani pobocku z databaze.
	 * @param int $id Identifikator pobocky v databaze
	 */
	public function removeOffice($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->delete();
	}

	public function relatedMedicines($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		foreach ($dat->related('pobocka_lek.ID_pobocky') as $med)
			$pole[] = $med->ID_leku;

		return $this->database->table('leky')->where('ID_leku', $pole);
	}
}
