<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;
use Nette\Databse\DriverException;

/**
 * Model pre pracu s jednotlivymi rezervaciami. Umoznuje vytvorenie, editovanie a
 * vypis rezervacii. Vypis rezervacii moze byt ako detail jednej rezervacii alebo
 * zoznam rezervacii.
 */
class ReservationManager extends BaseManager
{
	const
		TABLE_NAME = 'rezervace_leku',
		COLUMND_ID = 'ID_rezervace',
		SORT_TABLE = array(
			'stav' => 'stav_rezervace',
			'cas' => 'date_time'
		);

	/**
	 * Vyber vsetkych rezervacii z databaze a ich pripadne zotriedenie.
	 * @param string $column Nazov stlpca podla, ktoreho sa ma triedit
	 * @param string $sort Typ akym sa ma urobit triedenie ASC alebo DESC
	 * @return mixed Zoznam rezervacii
	 */
	public function getReservations($column, $sort)
	{
		$default_column = self::SORT_TABLE['stav'];
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
	 * Vyber pozadovanej rezervacii z databaze.
	 * @param int $id Identifikator rezervacii v databaze
	 * @return mixed Rezervacia
	 */
	public function getReservation($id)
	{
		return $this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->fetch();
	}

	/**
	 * Ulozi rezer do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o rezervacii, inak sa edituje existujuca rezervacia. Pri pridavani
	 * sa kontroluje, ci boli oznacene lieky a pobocky tym padom sa vlozi/upravi
	 * aj tabulka, ktora obsahuje spojenie medzi liekom, pobockou a rezervaciou.
	 * Operacia v ramci vkladania su spracovane ako tranzakcie.
	 * @param mixed $item Rezervacia, ktora sa ma upravit alebo vlozit
	 */
	public function saveReservation($item)
	{
		$reservations = $item['reservations'];
		unset($item['reservations']);
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

				$this->database->table('rezervace_leku_lek')->where('ID_rezervace', $item['ID_rezervace'])->delete();
				$primaryKey = $item['ID_rezervace'];
			}

			// Ulozenie novych spojeni v tabulke M:N
			foreach ($reservations as $key => $item) {
				$this->database->table('rezervace_leku_lek')->insert(
					array('ID_rezervace' => $primaryKey,
					'ID_leku' => $item['ID_leku'],
					'pocet_rezervovanych' => $item['pocet_rezervovanych'],
					'ID_pobocky' => $item['ID_pobocky']));
			}

		}
		catch (DriverException $ex) {
			$this->database->rollback();
			throw $ex;
		}
		$this->database->commit();
	}

	/**
	 * Odstrani pobocku z databaze.
	 * @param int $id Identifikator pobocky v databaze
	 */
	public function removeReservation($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->delete();
	}

	/**
	 * Vyberie z databaze lieky a pobocku, ktore boli priradene k danej
	 * rezervacii. Pouziva sa pri editacii rezervacie, aby sa nacitalu udaje
	 * o liekoch a pobockach, ktore dana rezervacia obsahuje.
	 * @param int $id Identifikator rezervacii
	 * @return mixed Informacie o lieku/liekoch k danej pobocke
	 */
	public function getReservationEditValues($id)
	{
		$result = [];
		$data = $this->database->table('rezervace_leku_lek')
			->where('ID_rezervace', $id);

		foreach ($data as $id)
			$result[] = array(
				"ID_rezervace" => $id->ID_rezervace,
				"ID_leku" => $id->ID_leku,
				"pocet_rezervovanych" => $id->pocet_rezervovanych,
				"ID_pobocky" => $id->ID_pobocky
			);

		return $result;
	}

	/**
	 * Nacita lieky k danej rezervacii.
	 * @param int $id Identifikator rezervacie, podla ktorej sa vyhladaju lieky
	 * @return mixed Zoznam rezervovanych liekov
	 */
	public function relatedMedicines($id)
	{
		$dat = $this->database->table(self::TABLE_NAME)->get($id);

		$pole = array();
		foreach ($dat->related('rezervace_leku_lek.ID_rezervace') as $med)
			$pole[] = $med->ID_leku;
		return $this->database->table('leky')->where('ID_leku', $pole);
	}

	/**
	 * Odstranenie lieku z rezervacie.
	 * @param int @idReservation Identifikator rezervacie, z ktorej sa ma
	 * @param int @idMedicine Identifikator lieku, ktory sa ma odstranit
	 */
	public function removeMedicineFromReservation($idReservation, $idMedicine)
	{
		$this->database->table('rezervace_leku_lek')
			->where('ID_rezervace', $idReservation)
			->where('ID_leku', $idMedicine)->delete();
	}
}
