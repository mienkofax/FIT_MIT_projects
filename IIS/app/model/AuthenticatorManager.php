<?php

namespace App\Model;

use Nette\Database\UniqueConstraintViolationException;
use Nette\Security\AuthenticationException;
use Nette\Security\IAuthenticator;
use Nette\Security\Identity;
use Nette\Security\Passwords;

/**
 * Vynimka v pripade, ze bolo zadane uz existujuce uzivatelske meno.
 */
class DuplicateColumnException extends AuthenticationException
{
	public function __construct()
	{
		parent::__construct();
		$this->message = 'Používateľské meno je už obasdené, zvoľte iné.';
	}
}

/**
 * Model pre pracu s uzivatelmi. Kontroluje ci suhlasia prihlasovacie a
 * registracne udaje. Ak sa jedna o registraciu vlozi noveho uzivatela
 * do databaze. Umoznuje pridavanie, editovanie a odstranovanie uzivatelov.
 */
class AuthenticatorManager extends BaseManager implements IAuthenticator
{
	const
		TABLE_NAME = 'uzivatele',
		COLUMN_ID = 'ID_uzivatele',
		COLUMN_NAME =  'jmeno',
		COLUMN_SURNAME =  'prijmeni',
		COLUMN_LOGIN = 'login',
		COLUMN_PASSWORD_HASH = 'heslo',
		COLUMN_ROLE = 'opravneni',
		SORT_TABLE = array(
			'nazov' => 'nazev_pobocky',
			'cas' => 'date_time'
		);

	/**
	 * Overi ci su zadane spravne udaje pre prihlasenie uzivatela.
	 * @param array $credentials Obsahuje zadane meno a heslo
	 * @return Identity identita prihlaseneho uzivatela
	 * @throws AuthenticationException V pripade nespravnych prihlasovacihc udajov.
	 */
	public function authenticate(array $credentials)
	{
		list($username, $password) = $credentials;

		// Vyber uzivatela z databaze na zaklade uzivatelskeho mena
		$user = $this->database->table(self::TABLE_NAME)
			->where(self::COLUMN_LOGIN, $username)->fetch();

		// Kontrola ci bol sa v databaze nachadza zadae uzivatelske heslo
		if (!$user)
			throw new AuthenticationException('Bolo zadané neexistujúce užívateľské meno',
				self::IDENTITY_NOT_FOUND);

		// Kontrola ci zadane heslo suhlasi s heslom v databaze
		if (!Passwords::verify($password, $user[self::COLUMN_PASSWORD_HASH]))
			throw new AuthenticationException('Bolo zadané nesprávne heslo', self::INVALID_CREDENTIAL);

		// Kontrola ci nie je potrebne prehashovat heslo
		if (Passwords::needsRehash($user[self::COLUMN_PASSWORD_HASH]))
			$user->update(array(self::COLUMN_PASSWORD_HASH => Passwords::hash($password)));

		$userData = $user->toArray();

		// Odstranenie hesla z uzivatelkych dat kvoli bezpecnosti
		unset($userData[self::COLUMN_PASSWORD_HASH]);

		return new Identity($user[self::COLUMN_ID], $user[self::COLUMN_ROLE], $userData);
	}

	/**
	 * Registracia noveho uzivatela. Ulozi noveho uzivatela do databaze.
	 * @param string $username Uzivatelske meno
	 * @param string $passowrd Heslo
	 * @param string @name Meno pouzivatela
	 * @param string @surname Priezvisko pouzivatela
	 * @throws DuplicateNameException V pripade, ze sa v databaze nachadza
	 * meno z rovnakym nazvom
	 */
	public function register($username, $password, $name, $surname)
	{
		try {
			$this->database->table(self::TABLE_NAME)->insert(array(
				self::COLUMN_LOGIN => $username,
				self::COLUMN_NAME => $name,
				self::COLUMN_SURNAME => $surname,
				self::COLUMN_PASSWORD_HASH => Passwords::hash($password),
			));
		}
		catch (UniqueConstraintViolationException $e) {
			throw new DuplicateColumnException;
		}
	}

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
			->where(self::COLUMND_ID, $id)->fetch();
	}

	/**
	 * Ulozi uzvivatela do databaze. V pripade, ze nie je nastavene ID vlozi sa novy
	 * zaznam o uzivatelovi, inak sa edituje existujuci uzivatel.
	 * @param mixed $user Uzivatel, ktory sa ma upravit alebo vlozit
	 */
	public function saveUser($user)
	{
		if (!$user[self::COLUMND_ID]) {
			unset($user[self::COLUMND_ID]);
			$this->database->table(self::TABLE_NAME)->insert($user);
		} else
			$this->database->table(self::TABLE_NAME)->where(self::COLUMND_ID,
				$user[self::COLUMND_ID])->update($user);
	}

	/**
	 * Odstrani uzivatela z databaze.
	 * @param int $id Identifikator uzivatela v databaze
	 */
	public function removeUser($id)
	{
		$this->database->table(self::TABLE_NAME)
			->where(self::COLUMND_ID, $id)->delete();
	}
}
