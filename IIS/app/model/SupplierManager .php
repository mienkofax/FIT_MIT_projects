<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;
use Nette\Database\DriverException;

/**
 * Model pre pracu s jednotlivymi datavatelmi. Umoznuje, editovanie a
 * vypis dadavatelov. Vypis dodavatelov moze byt ako detail jedneho dodavatela
 * alebo zoznam dodavatelov. Pri ukladani dodavatelov sa ukladaju aj udaje o
 * pobockach na, ktore sa dodava.
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
		if (self::SORT_TABLE[$column] !== null)
			$default_column = self::SORT_TABLE[$column];

		// Kontrola ci bolo zadane v akom poradi sa ma maju vypisat
		if (self::SORT_TYPE[$sort] !== null)
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
	 * zaznam o dodavatelovi, inak sa edituje existujuci dodavatel. Pri pridavani
	 * sa kontroluje ci boli oznacene aj pobocky a tym padom sa vlozi/upravi
	 * aj tabulka, ktora obsahuje spojenie medzi dodavatelom a pobockou.
	 * Operacie v ramci vkladania su spracovane ako tranzakcie.
	 * @param mixed $office Dodavatelo, ktory sa ma upravit alebo vlozit
	 * @throws DriverException Problem pri praci s db
	 */
	public function saveSupplier($supplier)
	{
		$fKey = $supplier['ID_pobocky'];
		unset($supplier['ID_pobocky']);

		$this->database->beginTransaction();
		try {
			if (!$supplier[self::COLUMND_ID]) {
				unset($supplier[self::COLUMND_ID]);

				$result = array();
				$primaryKey = $this->database->table(self::TABLE_NAME)->insert($supplier);
				$this->insertFKKey($primaryKey, $supplier, $fKey,
					"ID_pobocky", self::COLUMND_ID, "dodavatel_pobocka");
			}
			else {
				$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
					$supplier[self::COLUMND_ID])->update($supplier);

				// Odstranenie vsetkych starych spojeni v db tabulke a pridanie novych
				$data = $this->database->table('dodavatel_pobocka')
					->where(self::COLUMND_ID, $supplier[self::COLUMND_ID]);
				$fKey = $this->removeFKKey($data, "ID_pobocky", $fKey);

				foreach ($fKey as $item)
					$this->database->table('dodavatel_pobocka')->insert(array('ID_pobocky' => $item, self::COLUMND_ID  => $supplier['ID_dodavatele']));
			}
		}
		catch (DriverException $ex) {
			$this->database->rollback();
			throw $ex;
		}

		$this->database->commit();
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

	/**
	 * Zoznam pobociek, ktore su priradene k dodavatelovi. Poskytne
	 * vsetky informacie o danej pobocke potrebne pri zobrazovani zosobovanych
	 * pobociek, aby bolo mozne vypisat vsetky potrebne informacie o nich.
	 * Pouziva sa pri detaile dodavatela.
	 * @param int $id Identifikator dodavatela
	 * @return mixed pole identifikatorov pobociek
	 */
	public function relatedOffices($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('dodavatel_pobocka.ID_dodavatele') as $med)
			$pole[] = $med->ID_pobocky;

		return $this->database->table('pobocky')->where('ID_pobocky', $pole);
	}

	/**
	 * Zoznam identifikator pobociek, ktore su aktualne zasobovane dodavatelom.
	 * Potrebne pre vypis pri editacii dodavatela. Vracia pole pobociek
	 * na zaklade, ktoreho sa oznacia v select potrebne pobocky, ktore su ulozene
	 * v databaze k danemu dodavatelovi.
	 * @param int $id Identifikator dodavatela
	 * @return mixed Pole identifikatorov pobociek prisluchajucich dodavatelovi
	 */
	public function getOfficesID($id)
	{
		$result = [];
		$data = $this->database->table('dodavatel_pobocka')->select('ID_pobocky')
			->where('ID_dodavatele', $id);

		foreach ($data as $id)
			$result[] = $id->ID_pobocky;
		return $result;
	}

	/**
	 * Zoznam vsetkych dodavatelov.
	 * @return mixed Zoznam vsetkych dodavatelov.
	 */
	public function getSuppliersToSelectBox() {
		$data = $this->database->table('dodavatele')->fetchAll();
		$result = [];

		foreach ($data as $key => $value)
			$result[$value->ID_dodavatele] = $value->nazev_dodavatele;

		return $result;
	}

	/**
	 * Odstranie dodavania na urcitu pobocku.
	 * @param int $idSupplier Identifikator dodavatela
	 * @param int $idOffice Identifikator pobocky
	 */
	public function removeOfficeFromSupplier($idSupplier, $idOffice)
	{
		$this->database->table('dodavatel_pobocka')
			->where('ID_dodavatele', $idSupplier)
			->where('ID_pobocky', $idOffice)->delete();
	}
}
