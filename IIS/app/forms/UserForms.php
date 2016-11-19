<?php

namespace App\Forms;

use Nette\Application\UI\Form;
use Nette\Object;
use Nette\Security\AuthenticationException;
use Nette\Security\User;
use Nette\Utils\ArrayHash;

/**
 * Obsahuje prvky pre pre orihlasenie a registraciu uzivatela.
 */
class UserForms extends Object
{
	/** @var User Uživatel. */
	private $user;

	/**
	 * Konstruktor, ktory injektuje uzivatela.
	 * @param User $user automaticky injektovana trieda uzivatel
	 */
	public function __construct(User $user)
	{
		$this->user = $user;
	}

	/**
	 * Prihlasenie alebo registracia uzivatela.
	 * @param Form $form formular, s kade je volana tato metoda
	 * @param ArrayHash $instructions uzivatelska operacia
	 * @param bool $register registracia noveho uzivatela
	 */
	private function login($form, $instructions, $register = false)
	{
		// Ziskanie presenteru, kde je umiestneny formular
		$presenter = $form->getPresenter();

		// Kontrola ci sa spravne vykonalo prihlasenie alebo registracia
		try {
			$username = $form->getValues()->username;
			$password = $form->getValues()->password;

			// Registracia noveho uzivatela
			if ($register) {
				$name = $form->getValues()->jmeno;
				$surname = $form->getValues()->prijmeni;
				$this->user->getAuthenticator()->register($username, $password,
					$name, $surname);
			}

			// Prihlasenie uzivatela
			$this->user->login($username, $password);

			if ($instructions && $presenter) {
				// Preposlanie instrukcie do pozadovaneho presenteru
				if (isset($instructions->message))
					$presenter->flashMessage($instructions->message);

				// Presmerovanie na zadanom presenteri
				if (isset($instructions->redirection))
					$presenter->redirect($instructions->redirection);
			}
		}
		catch (AuthenticationException $ex) {
			// Posli chybovu spravu, pokial je formular v presenteru
			if ($presenter) {
				$presenter->flashMessage($ex->getMessage());
				$presenter->redirect('this');
			}
			else {
				// Ak nie je formular v presenteru vypise sa sprava do formulara
				$form->addError($ex->getMessage());
			}
		}
	}

	/**
	 * Cast formulara, ktora je spolocna pre prihlasenie a pre registraciu
	 * @param Form $form formular, do ktoreho sa prida spolocna cast
	 * @return Form aktualizovany formular
	 */
	private function createBasicForm(Form $form = null)
	{
		$form = $form ? $form : new Form;

		$form->addText('username', 'Meno')
			->addRule(Form::FILLED, "Musí byť zadané prihlasovanie meno");
		$form->addPassword('password', 'Heslo')
			->addRule(Form::FILLED, "Musí byť zadané heslo");

		$form->elementPrototype->addAttributes(array('novalidate' => 'novalidate'));
		$form->elementPrototype->addAttributes(array('class' => 'form-horizontal'));

		$renderer = $form->getRenderer();
		$renderer->wrappers['controls']['container'] = '';
		$renderer->wrappers['pair']['container'] = "div class='form-group'";
		$renderer->wrappers['label']['container'] = "label class='control-label col-xs-4'";
		$renderer->wrappers['control']['container'] = "div class='col-xs-6'";
		$renderer->wrappers['control']['.text'] = 'form-control';
		$renderer->wrappers['control']['.password'] = 'form-control';
		$renderer->wrappers['control']['.email'] = 'form-control';
		$renderer->wrappers['control']['.number'] = 'form-control';
		$renderer->wrappers['control']['.submit'] = 'btn btn-primary';

		return $form;
	}

	/**
	 * Cast formulara s prihlasovacimi prvkami
	 * @param Form $form formular, do ktoreho sa prida prihlasovacia cast
	 * @param ArrayHash $instructions uzivatelke instrukcie pre prihlasenie
	 * @return Form aktualizovany formular s prihalsenim
	 */
	public function createLoginForm($instructions = null, Form $form = null)
	{
		$form = $this->createBasicForm($form);
		$form->addSubmit('submit', 'Prihlásiť');
		$form->onSuccess[] = function (Form $form) use ($instructions) {
				$this->login($form, $instructions);
		};

		return $form;
	}

	/**
	 * Komponenta formulara s prvkami pre registraciu noveho uzivatela
	 * @param Form $form formular, ktory sa rozsiri o nove prvky
	 * @param ArrayHash $instructions uzivatelke instrukcie pre registraciu
	 * @return Form aktualizovany formular s registrovanim
	 */
	public function createRegisterForm($instructions = null, Form $form = null)
	{
		$form = $this->createBasicForm($form);
		$form->addPassword('password_repeat', 'Heslo znova')
			->addRule(Form::FILLED, "Musí byť zadané heslo")
			->addRule(Form::EQUAL, 'Heslá sa nezhodujú', $form['password']);
		$form->addText('y', 'Aktuálny rok (antispam)')
			->setType('number')->setRequired()
			->addRule(Form::EQUAL, 'Nesprávny rok', date("Y"))
			->addRule(Form::FILLED, "Zadajte aktuálny rok");
		$form->addText('jmeno', 'Meno užívateľa')
			->addRule(Form::FILLED, 'Zadajte meno');
		$form->addText('prijmeni', 'Prezvisko užívateľa')
			->addRule(Form::FILLED, 'Zadajte priezvisko');
		$form->addSubmit('register', 'Registrovať');
		$form->onSuccess[] = function (Form $form) use ($instructions) {
			$this->login($form, $instructions, true);
		};

		return $form;
	}
}
