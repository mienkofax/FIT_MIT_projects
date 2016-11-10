<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;

/**
 * Model pre pracu s jednotlivymi liekmi. Umoznuje vytvorenie, editovanie a
 * vypis liekov. Vypis liekov moze byt ako detail jedneho lieku alebo
 * zoznam liekov.
 */
class MedicineManager extends BaseManager
{
	const
		TABLE_NAME = 'leky',
		COLUMND_ID = 'ID_leku',
		SORT_TABLE = array(
			'nazov' => 'nazev_leku',
			'cas' => 'date_time',
			'na-sklade' => 'pocet_na_sklade',
			'predanych' => 'pocet_predanych'
		);

	/**
	 * Vyber vsetkych liekov z databaze a ich pripadne zotriedenie.
	 * @param string $column Nazov stlpca podla, ktoreho sa ma triedit
	 * @param string $sort Typ akym sa ma urobit triedenie ASC alebo DESC
	 * @return mixed Zoznam liekov
	 */
	public function getMedicines($column, $sort)
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
	 * Vyber pozadovaneho lieku z databaze.
	 * @param int $id Identifikator lieku v databaze
	 * @return mixed Liek
	 */
	public function getMedicine($id)
	{
		return $this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->fetch();
	}

	/**
	 * Ulozi liek do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o lieku, inak sa edituje existujuci liek.
	 * @param mixed $office Liek, ktora sa ma upravit alebo vlozit
	 */
	public function saveMedicine($medicine)
	{
		if (!$medicine[self::COLUMND_ID]) {
			unset($medicine[self::COLUMND_ID]);
			$this->database->table(self::TABLE_NAME)->insert($medicine);
		} else
			$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
				$medicine[self::COLUMND_ID])->update($medicine);
	}

	/**
	 * Odstrani liek z databaze.
	 * @param int $id Identifikator lieku v databaze
	 */
	public function removeMedicine($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->delete();
	}

	public function medicinesPaid($paid)
	{
		if ($paid)
			return $this->database->table(self::TABLE_NAME)->where('hradene', 'hradene');
		else
			return $this->database->table(self::TABLE_NAME)->where('hradene', 'nehradene');
	}

	public function medicinesAdditionalCharge()
	{
		return $this->database->table(self::TABLE_NAME)->where('hradene', 'doplatok');
	}

	public function medicinePrescription($withPrescription)
	{
		return $this->database->table(self::TABLE_NAME)->where('typ_leku', $withPrescription);
	}
}
