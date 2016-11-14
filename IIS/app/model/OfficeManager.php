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
		$fKey = $office['ID_dodavatele'];
		$fKeyUser = $office['ID_uzivatele'];
		$medicines = $office['medicines'];
		unset($office['ID_dodavatele']);
		unset($office['ID_uzivatele']);
		unset($office['medicines']);
		$primaryKey;

		$this->database->beginTransaction();
		try {
			if (!$office[self::COLUMND_ID]) {
				unset($office[self::COLUMND_ID]);

				$primaryKey = $this->database->table(self::TABLE_NAME)->insert($office);

				$this->insertFKKey($primaryKey, $office, $fKey,
					"ID_dodavatele", "ID_pobocky", "dodavatel_pobocka");

				$this->insertFKKey($primaryKey, $office, $fKeyUser,
					"ID_uzivatele", "ID_pobocky", "pobocka_zamestnanec");
			}
			else {
				$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
					$office[self::COLUMND_ID])->update($office);

				$data = $this->database->table('dodavatel_pobocka')->where('ID_pobocky', $office['ID_pobocky']);
				$fKey = $this->removeFKKey($data, "ID_dodavatele", $fKey);
				foreach ($fKey as $item)
					$this->database->table('dodavatel_pobocka')->insert(array('ID_pobocky' => $office['ID_pobocky'], 'ID_dodavatele' => $item));

				$data = $this->database->table('pobocka_zamestnanec')->where('ID_pobocky', $office['ID_pobocky']);
				$fKeyUser = $this->removeFKKey($data, "ID_uzivatele", $fKeyUser);
				foreach ($fKeyUser as $item)
					$this->database->table('pobocka_zamestnanec')->insert(array('ID_pobocky' => $office['ID_pobocky'], 'ID_uzivatele' => $item));

				$this->database->table('pobocka_lek')->where('ID_pobocky', $office['ID_pobocky'])->delete();
				$primaryKey = $office['ID_pobocky'];
			}

			// Ulozenie novych spojeni v tabulke M:N
			foreach ($medicines as $key => $item) {
				$this->database->table('pobocka_lek')->insert(
					array('ID_pobocky' => $primaryKey,
					'ID_leku' => $item['ID_leku'],
					'pocet_na_sklade' => $this->defaultNumberZero($item['pocet_na_sklade'])));
			}
		}
		catch (Nette\Databse\DriverException $ex) {
			$this->database->rollback();
			throw $ex;
		}
		$this->database->commit();
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

	/**
	 * Zoznam liekov, ktore su priradene k danej pobocke. Poskytuje vsetky
	 * informacie o danom lieku potrebne pri zobrazeni liekov. Pouziva sa pri
	 * detaile pobocky.
	 * @param int $id Identifikator pobocky
	 * @return mixed Pole udajov o liekoch
	 */
	public function relatedMedicines($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('pobocka_lek.ID_pobocky') as $med)
			$pole[] = $med->ID_leku;
		return $this->database->table('leky')->where('ID_leku', $pole);
	}

	/**
	 * Zoznam dodavatelov, ktori su priradeni k danej pobocke. Poskytuje vsetky
	 * informacie o danom dodavatelovi potrebne pri zobrazeni dodavatela.
	 * Pouziva sa pri detaile pobocky.
	 * @param int $id Identifikator pobocky
	 * @return mixed Pole udajov o dodavateloch
	 */
	public function relatedSupplier($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('dodavatel_pobocka.ID_pobocky') as $med)
			$pole[] = $med->ID_dodavatele;
		return $this->database->table('dodavatele')->where('ID_dodavatele', $pole);
	}

	/**
	 * Zoznam uzivatelov, ktori su priradeni k danej pobocke. Poskytuje vsetky
	 * informacie o uzivatelovi potrebne pri zobrazeni uzivatela.
	 * Pouziva sa pri detaile pobocky.
	 * @param int $id Identifikator pobocky
	 * @return mixed Pole udajov o uzivateloch
	 */
	public function relatedUser($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('pobocka_zamestnanec.ID_pobocky') as $med)
			$pole[] = $med->ID_uzivatele;
		return $this->database->table('uzivatele')->where('ID_uzivatele', $pole);
	}

	public function relatedReservation($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('rezervace_leku_lek.ID_pobocky') as $med)
			$pole[] = $med->ID_rezervace;
		return $this->database->table('rezervace_leku')->where('ID_rezervace', $pole);
	}

	/**
	 * Zistenie poctu jednotlivych udajov v databaze. Tieto data sa viazu k danej
	 * pobocke. Zistenie kolko je na pobocke uzivatelov, dodavatelov a liekov.
	 * @param int $id Identifikator pobocky
	 * @return mixed Pocet jednotlivych udajov v databaze
	 */
	public function countDBItem($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$countData['user'] = $dat->related('pobocka_zamestnanec.ID_pobocky')->count();
		$countData['supplier'] = $dat->related('dodavatel_pobocka.ID_pobocky')->count();
		$countData['medicine'] =  $dat->related('pobocka_lek.ID_pobocky')->count();

		return $countData;
	}

	/**
	 * Zoznam vsetkych pobociek pre formular.
	 * @return mixed Zoznam vsetkych pobociek.
	 */
	public function getOfficesToSelectBox() {
		$data = $this->database->table('pobocky')->fetchAll();
		$result = [];

		foreach ($data as $key => $value)
			$result[$value->ID_pobocky] = $value->nazev_pobocky;

		return $result;
	}

	/**
	 * Vyberie z databaze dodavatelov, ktori boli priradeni k danej pobocke.
	 * Pouzivate sa pri editacii pobocky, aby sa nacitali udaje o dodavateloch,
	 * ktori danu pobocku zasobuju.
	 * @param int $id Identifikator pobocky
	 * @return mixed Zoznam dodavatelov k danej pobocke
	 */
	public function getSuppliersEditValues($id)
	{
		$result = [];
		$data = $this->database->table('dodavatel_pobocka')->select('ID_dodavatele')
			->where('ID_pobocky', $id);

		foreach ($data as $id)
			$result[] = $id->ID_dodavatele;
		return $result;
	}

	/**
	 * Vyberie z databaze uzivatelov, ktori boli priradeni k danej pobocke.
	 * Pouziva sa pri editacii pobocky, aby sa nacitali udaje o uzivateloch,
	 * ktori pracuju na danej pobocke.
	 */
	public function getUsersEditValues($id)
	{
		$result = [];
		$data = $this->database->table('pobocka_zamestnanec')->select('ID_uzivatele')
			->where('ID_pobocky', $id);

		foreach ($data as $id)
			$result[] = $id->ID_uzivatele;
		return $result;
	}

	/**
	 * Vyberie z databaze lieky, ktore boli priradene k danej pobocke.
	 * Pouziva sa pri editacii pobocky, aby sa nacitali udaje o liekoch,
	 * ktore sa predavaju na danej pobocke.
	 * @param int $id Identifikator pobocky
	 * @return mixed Zoznam liekov na danej pobocke.
	 */
	public function getMedicinesEditValues($id)
	{
		$result = [];
		$data = $this->database->table('pobocka_lek')
			->where('ID_pobocky', $id);

		foreach ($data as $item)
			$result[] = array(
				"ID_leku" => $item->ID_leku,
				"ID_pobocky" => $item->ID_pobocky,
				"pocet_na_sklade" => $item->pocet_na_sklade
			);

		return $result;
	}

	/**
	 * Odstranie rezervacie z pobocky.
	 * @param int $idOffice Identifikator pobocky
	 * @param int $idReservation Identifikator rezervacie, ktora sa ma odstranit
	 */
	public function removeReservationFromOffice($idOffice, $idReservation)
	{
		$this->database->table('rezervace_leku_lek')
			->where('ID_rezervace', $idReservation)
			->where('ID_pobocky', $idOffice)->delete();
	}
}
