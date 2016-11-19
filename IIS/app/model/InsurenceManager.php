<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;
use Nette\Databse\DriverException;

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
		if (self::SORT_TABLE[$column] !== null)
			$default_column = self::SORT_TABLE[$column];

		// Kontrola ci bolo zadane v akom poradi sa ma maju vypisat
		if (self::SORT_TYPE[$sort] !== null)
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
	 * zaznam o pojistovni, inak sa edituje existujuca pojistovna. Pri pridavani
	 * sa kontroluje ci boli oznacene lieky a tym padom sa vlozi/upravi
	 * aj tabulka, ktora obsahuje spojenie medzi liekom a pobockou.
	 * Operacie v ramci vkladania su spracovane ako tranzakcie.
	 * @param mixed $insurence Poistovna, ktora sa ma upravit alebo vlozit
	 * @throws DriverException Problem pri praci s db
	 */
	public function saveInsurence($insurence)
	{
		$medicines = $insurence['medicines'];
		unset($insurence['medicines']);
		$primaryKey;

		$this->database->beginTransaction();
		try {
			// Pridanie zaznamu do databaze
			if (!$insurence[self::COLUMND_ID]) {
				unset($insurence[self::COLUMND_ID]);
				$primaryKey = $this->database->table(self::TABLE_NAME)->insert($insurence);
			}
			// Aktualizacia zaznamu v databaze a odstranenie starych M:N spojeni
			else {
				$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
					$insurence[self::COLUMND_ID])->update($insurence);

				$this->database->table('lek_pojistovny')->where('ID_pojistovny', $insurence['ID_pojistovny'])->delete();
				$primaryKey = $insurence['ID_pojistovny'];
			}

			// Ulozenie novych spojeni v tabulke M:N
			foreach ($medicines as $key => $item) {
				$this->database->table('lek_pojistovny')->insert(
					array('ID_pojistovny' => $primaryKey,
					'ID_leku' => $item['ID_leku'],
					'cena' => $this->defaultNumberZero($item['cena']),
					'doplatek' => $this->defaultNumberZero($item['doplatek']),
					'hradene' => $item['hradene']));
			}
		}
		catch (DriverException $ex) {
			$this->database->rollback();
			throw $ex;
		}
		$this->database->commit();
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

	/**
	 * Zoznam liekov, ktore su priradene k danej poistovni a su hradene/nehradene.
	 * Poskytne vsetky informacie o danom lieku potrebne pri zobrazovani liekov,
	 * pri detaile poistovne.
	 * @param int $id Identifikator poistovne v databaze
	 * @param boolean $paid True ak sa maju vypisat hradene, false ak nehradene
	 * @return mixed pole udajov o liekov
	 */
	public function relatedMedicinesPaid($id, $paid)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);
		$pole = [];
		if ($paid) {
			foreach ($dat->related('lek_pojistovny.ID_pojistovny')->where('hradene', 'hradene') as $med)
				$pole[] = $med->ID_leku;
		} else {
			foreach ($dat->related('lek_pojistovny.ID_pojistovny')->where('hradene', 'nehradene') as $med)
				$pole[] = $med->ID_leku;
		}

		return $this->database->table('leky')->where('ID_leku', $pole);
	}

	/**
	 * Zoznam liekov, ktore su priradene k danej poistovni a su na doplatok.
	 * Poskytne vsetky informacie o danom lieku potrebne pri zobrazovani liekov,
	 * pri detaile poistovne.
	 * @param int $id Identifikator poistovne v databaze
	 * @return mixed pole udajov o liekoch
	 */
	public function relatedMedicinesAdditionalCharge($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);
		$pole = [];

		foreach ($dat->related('lek_pojistovny.ID_pojistovny')->where('hradene', 'doplatok') as $med)
			$pole[] = $med->ID_leku;

		return $this->database->table('leky')->where('ID_leku', $pole);
	}

	/**
	 * Zisti pocet jednodlivych udajov v databaze. Tieto data sa viazu k danej
	 * poistovne. Zisti kolko liekov je hradenych, nehradenych a na doplatok.
	 * @param int $id Identifikator poistovne v databaze
	 * @return mixed Pocet jednotlivych udajov v databaze
	 */
	public function countDBItem($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		foreach ($dat->related('lek_pojistovny.ID_pojistovny') as $med)
			$pole[] = $med->ID_leku;

		$countData['hradene'] = $this->database->table('lek_pojistovny')->where('hradene', 'hradene')->where('ID_pojistovny', $id)->count();
		$countData['nehradene'] = $this->database->table('lek_pojistovny')->where('hradene', 'nehradene')->where('ID_pojistovny', $id)->count();
		$countData['doplatok'] = $this->database->table('lek_pojistovny')->where('hradene', 'doplatok')->where('ID_pojistovny', $id)->count();

		return $countData;
	}

	/**
	 * Vyberie z databaze lieky, ktore boli pridane k danej poistovni.
	 * Pouziva sa pri editacii poistovne, aby sa nacitali udaje o liekoch,
	 * ktore dana poistovna preplaca a v akom rozsahu ich preplaca.
	 * @param int $id Identifikator poistovne
	 * @return mixed Zoznam liekov na danej poistovni
	 */
	public function getMedicinesEditValues($id)
	{
		$result = [];
		$data = $this->database->table('lek_pojistovny')
			->where('ID_pojistovny', $id);

		foreach ($data as $row) {
			$result[] = array(
				'ID_leku' => $row->ID_leku,
				'cena' => $row->cena,
				'doplatek' => $row->doplatek,
				'hradene' => $row->hradene
			);
		}

		return $result;
	}

	/**
	 * Zoznam vsetkych poistovni pre formular.
	 * @return mixed Zoznam vsetkych poistovni.
	 */
	public function getInsurenceToSelectBox() {
		$data = $this->database->table(self::TABLE_NAME)->fetchAll();
		$result = [];

		foreach ($data as $key => $value)
			$result[$value->ID_pojistovny] = $value->nazev_pojistovny;

		return $result;
	}

	/**
	 * Odstranie lieku z urcitej poistovne.
	 * @param int $idInsurence Identifikator poistovne
	 * @param int $idMedicine Identifikator lieku, ktory sa ma odstranit
	 */
	public function removeOfficeFromSupplier($idInsurence, $idMedicine)
	{
		$this->database->table('lek_pojistovny')
			->where('ID_pojistovny', $idInsurence)
			->where('ID_leku', $idMedicine)->delete();
	}
}
