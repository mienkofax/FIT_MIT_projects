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
		if (isset(self::SORT_TABLE[$column]))
			$default_column = self::SORT_TABLE[$column];

		// Kontrola ci bolo zadane v akom poradi sa ma maju vypisat
		if (isset(self::SORT_TYPE[$sort]))
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
		unset($user['heslo_repeat']);
		if (!$user[self::COLUMN_ID]) {
			unset($user[self::COLUMN_ID]);
			$this->database->table(self::TABLE_NAME)->insert($user);
		} else
			$this->database->table(self::TABLE_NAME)->where(self::COLUMN_ID,
				$user[self::COLUMN_ID])->update($user);
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
}
