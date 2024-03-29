<?php

namespace App\Model;

use Nette\Database\UniqueConstraintViolationException;
use Nette\Security\AuthenticationException;
use Nette\Security\IAuthenticator;
use Nette\Security\Identity;
use Nette\Security\Passwords;

/**
 * Model pre pracu s uzivatelmi. Kontroluje ci suhlasia prihlasovacie a
 * registracne udaje. Ak sa jedna o registraciu vlozi noveho uzivatela
 * do databaze. Umoznuje pridavanie, editovanie a odstranovanie uzivatelov.
 */
class UserManager extends BaseManager
{
	const
		TABLE_NAME = 'uzivatele',
		COLUMN_ID = 'ID_uzivatele',
		SORT_TABLE = array(
			'nazov' => 'jmeno',
			'priezvisko' => 'prijmeni',
			'cas' => 'date_time'
		);

	/**
	 * Vyber vsetkych uzivatelov z databaze a ich pripadne zotriedenie.
	 * @param string $column Nazov stlpca podla, ktoreho sa ma triedit
	 * @param string $sort Typ akym sa ma urobit triedenie ASC alebo DESC
	 * @return mixed Zoznam uzivatelov
	 */
	public function getUsers($column, $sort)
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
	 * Vyber pozadovaneho uzivatela z databaze.
	 * @param int $id Identifikator uzivatela v databaze
	 * @return mixed Uzivatel
	 */
	public function getUser($id)
	{
		return $this->database->table(self::TABLE_NAME)
			->where(self::COLUMN_ID, $id)->fetch();
	}

	/**
	 * Ulozi uzvivatela do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o uzivatelovi, inak sa edituje existujuci uzivatel.
	 * @param mixed $user Uzivatel, ktory sa ma upravit alebo vlozit
	 */
	public function saveUser($user)
	{
		$office = $user['ID_pobocky'];
		unset($user['heslo_repeat']);
		unset($user['ID_pobocky']);

		$user['heslo'] = Passwords::hash($user['heslo']);
		if (!$user[self::COLUMN_ID]) {
			unset($user[self::COLUMN_ID]);
			$primaryKey = $this->database->table(self::TABLE_NAME)->insert($user);

			if ($office) {
				foreach ($office as $key => $item)
					$this->database->table('pobocka_zamestnanec')
						->insert(array('ID_pobocky' => $item, 'ID_uzivatele' => $primaryKey));
			}

		} else {
			$this->database->table(self::TABLE_NAME)->where(self::COLUMN_ID,
				$user[self::COLUMN_ID])->update($user);

			$primaryKey = $user['ID_uzivatele'];
			$this->database->table('pobocka_zamestnanec')
				->where('ID_uzivatele', $primaryKey)->delete();

			if ($office) {
				foreach ($office as $key => $item)
					$this->database->table('pobocka_zamestnanec')
						->insert(array('ID_pobocky' => $item, 'ID_uzivatele' => $primaryKey));
			}
		}
	}

	/**
	 * Odstrani uzivatela z databaze.
	 * @param int $id Identifikator uzivatela v databaze
	 */
	public function removeUser($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMN_ID, $id)->delete();
	}

	/**
	 * Zoznam vsetkych uzivatelov pre formular.
	 * @return mixed Zoznam vsetkych uzivatelov.
	 */
	public function getUsersToSelectBox() {
		$data = $this->database->table('uzivatele')->fetchAll();
		$result = [];

		foreach ($data as $key => $value)
			$result[$value->ID_uzivatele] = $value->jmeno . " " . $value->prijmeni ;

		return $result;
	}

	/**
	 * Odstranie uzivatela z pobocky.
	 * @param int $idUser Identifikator uzivatela
	 * @param int $idOffice Identifikator pobocky, ktora sa ma zmazat
	 */
	public function removeUserFromOffice($idUser, $idOffice)
	{
		$this->database->table('pobocka_zamestnanec')
			->where('ID_pobocky', $idOffice)
			->where('ID_uzivatele', $idUser)->delete();
	}

	public function getRelatedOffice($id) {

		$data = $this->database->table('pobocka_zamestnanec')->where('ID_uzivatele', $id);
		$result = [];
		$result2 = [];
		$office = [];

		foreach ($data as $key => $value)
			$office[] = $value['ID_pobocky'];

		foreach ($this->database->table('pobocky')->where('ID_pobocky', $office) as $key => $value)
			$result2[] = $value->ID_pobocky;

		return $result2;
	}
}
