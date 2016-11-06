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
 * do databaze.
 */
class UserManager extends BaseManager implements IAuthenticator
{
	const
		TABLE_NAME = 'uzivatele',
		COLUMN_ID = 'ID_uzivatele',
		COLUMN_NAME =  'jmeno',
		COLUMN_SURNAME =  'prijmeni',
		COLUMN_LOGIN = 'login',
		COLUMN_PASSWORD_HASH = 'heslo',
		COLUMN_ROLE = 'opravneni';

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
}
