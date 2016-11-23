<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;

/**
 * Model pre pracu s jednotlivymi liekmi. Umoznuje editovat a vyberat
 * udaje z db, bud pre jeden alebo viacero liekov. Obsahuje metody
 * pre ziskanie dostupnych pobociek, rezervacii, poistovni k danemu lieku.
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
		if (array_key_exists($column, self::SORT_TABLE))
			$default_column = self::SORT_TABLE[$column];

		// Kontrola ci bolo zadane v akom poradi sa ma maju vypisat
		if (array_key_exists($sort, self::SORT_TYPE))
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
	public function saveMedicine($item)
	{
		$offices = $item['offices'];
		$insurences = $item['insurences'];
		unset($item['offices']);
		unset($item['insurences']);
		$primaryKey;

		$this->database->beginTransaction();
		try {
			// Pridanie zaznamu do databaze
			if (!$item[self::COLUMND_ID]) {
				unset($item[self::COLUMND_ID]);
				$primaryKey = $this->database->table(self::TABLE_NAME)->insert($item);
			}
			// Aktualizacia zaznamu v databaze a odstranenie starych M:N spojeni
			else {
				$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
					$item[self::COLUMND_ID])->update($item);

				$this->database->table('pobocka_lek')->where('ID_leku', $item['ID_leku'])->delete();
				$this->database->table('lek_pojistovny')->where('ID_leku', $item['ID_leku'])->delete();
				$primaryKey = $item['ID_leku'];
			}

			// Ulozenie novych spojeni v tabulke M:N
			foreach ($offices as $key => $row) {
				$this->database->table('pobocka_lek')->insert(
					array('ID_leku' => $primaryKey,
					'ID_pobocky' => $row['ID_pobocky'],
					'pocet_na_sklade' => $row['pocet_na_sklade']));
			}

			// Ulozenie novych spojeni v tabulke M:N
			foreach ($insurences as $key => $row) {
				$this->database->table('lek_pojistovny')->insert(
					array('ID_leku' => $primaryKey,
					'ID_pojistovny' => $row['ID_pojistovny'],
					'cena' => $row['cena'],
					'doplatek' => $row['doplatek'],
					'hradene' => $row['hradene']));
			}

		}
		catch (DriverException $ex) {
			$this->database->rollback();
			throw $ex;
		}
		$this->database->commit();
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

	/**
	 * Zoznam liekov, ktore su alebo nie su hradene
	 * @param boolean $paid True ak sa maju zobrazit hradene liek,
	 * False ak sa maju zobrazit nehradene lieky.
	 * @return mixed Zoznam liekov
	 */
	public function medicinesPaid($paid)
	{
		$pole = [];
		if ($paid) {
			$medicines = $this->database->table('lek_pojistovny')
				->where('hradene', 'hradene')->select('ID_leku');
		}
		else {
			$medicines = $this->database->table('lek_pojistovny')
				->where('hradene', 'nehradene')->select('ID_leku');
		}

		foreach ($medicines as $item)
			$pole[] = $item->ID_leku;

		return $this->database->table(self::TABLE_NAME)->where('ID_leku', $pole);
	}

	/**
	 * Zoznam liekov s doplatkom.
	 * @return mixed Zoznam liekov s doplatkom.
	 */
	public function medicinesAdditionalCharge()
	{
		$medicines = $this->database->table('lek_pojistovny')
			->where('hradene', 'doplatok')->select('ID_leku');

		foreach ($medicines as $item)
			$pole[] = $item->ID_leku;

		return $this->database->table(self::TABLE_NAME)->where('ID_leku', $pole);
	}

	/**
	 * Zoznam liekov na predpis/bez predpisu.
	 * @param boolean $withPrescription True ak sa maju zobrazit lieky na predpis,
	 * False ak sa maju zobrazit lieky bez predpisu.
	 * @return mixed Zoznam liekov.
	 */
	public function medicinePrescription($withPrescription)
	{
		return $this->database->table(self::TABLE_NAME)->where('typ_leku', $withPrescription);
	}

	/**
	 * Zoznam vsetkych liekov pre formular.
	 * @return mixed Zoznam vsetkych liekov.
	 */
	public function getMedicinesToSelectBox() {
		$data = $this->database->table(self::TABLE_NAME)->fetchAll();
		$result = [];

		foreach ($data as $key => $value)
			$result[$value->ID_leku] = $value->nazev_leku;

		return $result;
	}

	/**
	 * Zoznam pobociek, kde sa da dany lieky kupit/rezervovat. Pouzite
	 * pre formular.
	 * @param int $id Identifikator lieku, ktory sa ma hladat na pobockach
	 * @return mixed Zoznam pobociek
	 */
	public function getOfficesEditValues($id)
	{
		$result = [];
		$data = $this->database->table('pobocka_lek')
			->where('ID_leku', $id);

		foreach ($data as $item)
			$result[] = array(
				"ID_leku" => $item->ID_leku,
				"ID_pobocky" => $item->ID_pobocky,
				"pocet_na_sklade" => $item->pocet_na_sklade
			);

		return $result;
	}

	/**
	 * Zoznam poistovni, ktora dany liek preplaca. Pouzite pre formular.
	 * @param int $id Identifikator lieky, ktory ma preplacat poistovna.
	 * @return mixed Zoznam poistovni
	 */
	public function getInsurencesEditValues($id)
	{
		$result = [];
		$data = $this->database->table('lek_pojistovny')
			->where('ID_leku', $id);

		foreach ($data as $item)
			$result[] = array(
				"ID_leku" => $item->ID_leku,
				"ID_pojistovny" => $item->ID_pojistovny,
				"cena" => $item->cena,
				"doplatek" => $item->doplatek,
				"hradene" => $item->hradene
			);

		return $result;
	}

	/**
	 * Zoznam rezervacii k danemu lieku. Pouzite pri vypise detailu.
	 * @param int $id Idetifikator lieku
	 * @return mixed Zoznam rezervacii.
	 */
	public function relatedReservation($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('rezervace_leku_lek.ID_leku') as $med)
			$pole[] = $med->ID_rezervace;
		return $this->database->table('rezervace_leku')->where('ID_rezervace', $pole);
	}

	/**
	 * Zoznam pobociek, na ktorych je mozne objednat liek. Pouzite
	 * pri detaile lieku.
	 * @param int $id Identifikator lieku.
	 * @return mixed Zoznam pobociek.
	 */
	public function relatedOffices($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('pobocka_lek.ID_leku') as $med)
			$pole[] = $med->ID_pobocky;
		return $this->database->table('pobocky')->where('ID_pobocky', $pole);
	}

	/**
	 * Zoznam poistovni, ktora dany liek preplaca. Pouzite pri detaile lieku.
	 * @param int $id Identifikator lieku
	 * @return mixed Zoznam poistovni.
	 */
	public function relatedInsurences($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('lek_pojistovny.ID_leku') as $med)
			$pole[] = $med->ID_pojistovny;
		return $this->database->table('pojistovny')->where('ID_pojistovny', $pole);
	}

	/**
	 * Odstranenie lieku z rezervacie.
	 * @param int $idMedicine Identifikator lieku, ktory sa ma odstrnait
	 * @param int $idReservation Identifikator rezervacie s kade sa ma
	 * odstranit liek
	 */
	public function removeMedicineFromReservation($idMedicine, $idReservation)
	{
		$this->database->table('rezervace_leku_lek')
			->where('ID_rezervace', $idReservation)
			->where('ID_leku', $idMedicine)->delete();
	}

	/**
	 * Odstranenie lieku z danej pobocky.
	 * @param int $idMedicine Identifikator lieku, ktory sa ma odstranit
	 * @param int $idOffice Identifikator pobocky, z ktorej sa ma liek odstranit
	 */
	public function removeMedicineFromOffice($idMedicine, $idOffice)
	{
		$this->database->table('pobocka_lek')
			->where('ID_pobocky', $idOffice)
			->where('ID_leku', $idMedicine)->delete();
	}

	/**
	 * Odstranenie lieku z poistovne.
	 * @param int $idMedicine Identifikator lieku, ktory sa ma odstranit
	 * @param int $idInsurence Identifikator pobocky, z ktorej sa ma odstranit
	 */
	public function removeMedicineFromInsurence($idMedicine, $idInsurence)
	{
		$this->database->table('lek_pojistovny')
			->where('ID_pojistovny', $idInsurence)
			->where('ID_leku', $idMedicine)->delete();
	}
}
