<?php

namespace App\Presenters;

use App\Model\UserManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;

/*
 * Spracovanie vykreslenia formularov.
 */
class UserPresenter extends BasePresenter
{
	const
		PERMISSION = array(
			'member' => 'Registrovaný užívateľ',
			'manager' => 'Manažér',
			'admin' => 'Administrátor'
		);
	/** @var UserManager Informacie o uzivatelovi a praca s nim */
	protected $userManager;

	public function __construct(UserManager $userManager)
	{
		parent::__construct();
		$this->userManager = $userManager;
	}

	/**
	 * Nacita uzivatela z databaze a vykresli ho podla zadanej url.
	 * @param int $id Id clanku, ktory sa ma vypisat
	 * @throws BadRequestException Neexistujuxe id uzivatela
	 */
	public function renderDetail($id)
	{
		if (!$id)
			$id = 1;

		if (!($user = $this->userManager->getUser($id)))
			throw new BadRequestException();

		$this->template->userData = $user;
	}

	/**
	 * Zobrazenie zoznamu uzivatelov a zotriedenie podla parametrov.
	 * @param string $column Nazov stlpca pomocou, ktoreho sa bude zoradovat
	 * @param string $sort Typ triedenia, zostupne alebo vzostupne
	 */
	public function renderList($column, $sort)
	{
		$this->template->users = $this->userManager->getUsers($column, $sort);
	}

	/**
	 * Odstranenie uzivatela z databaze a presmerovanie na zoznam uzivatelov.
	 * @param ID uzivatela, ktory sa ma odstranit
	 */
	public function actionRemove($id)
	{
		$this->userManager->removeUser($id);
		$this->flashMessage('Užívateľ bol odstránený.');
		$this->redirect('User:list');
	}

	/**
	 * Uprava užívateľa na zaklade zadaneho ID. V pripade, ze nie je zadane
	 * ID uzivatela vykresli sa formular na vytvorenie noveho uzivatela.
	 * @param int $id ID uzivatela, ktory sa ma editovat
	 */
	public function actionEdit($id)
	{
		if (!$id) {
			$this->template->isEditForm = false;
			return;
		}

		if ($user = $this->userManager->getUser($id)) {
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($user);
		}
		else {
			$this->flashMessage('Užívateľ nebol nájdený.');
			$this->redirect('User:list');
		}
	}

	/**
	 * Vytvori formular pre editovanie užívateľa.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addGroup('');
		$form->addHidden('ID_uzivatele');
		$form->addText('login', 'Prihlasovacie meno')
			->addRule(Form::FILLED, "Musí byť zadané prihlasovanie meno");
		$form->addText('jmeno', 'Meno užívateľa')
			->addRule(Form::FILLED, 'Zadajte meno');
		$form->addText('prijmeni', 'Prezvisko užívateľa')
			->addRule(Form::FILLED, 'Zadajte priezvisko');
		$form->addPassword('heslo', 'Heslo')
			->addRule(Form::FILLED, "Musí byť zadané heslo");
		$form->addPassword('heslo_repeat', 'Heslo znova')
			->addRule(Form::FILLED, "Musí byť zadané heslo")
			->addRule(Form::EQUAL, 'Heslá sa nezhodujú', $form['heslo']);
		$form->addSelect('opravneni', 'Oprávnenie užívateľa', self::PERMISSION)
			->setAttribute('class', 'form-control')
			->setPrompt('Vyberte oprávnenie')
			->setRequired(TRUE);
		$form->addGroup('');
		$form->addSubmit('submit', "Uložiť užívateľa")
			->setAttribute('class', 'btn-primary');
		$form->onSuccess[] = [$this, 'editFormSuccessed'];

		return $this->bootstrapFormRender($form);
	}

	/**
	 * Spracovanie hodnot z formulara. Ulozenie do databaze a informovanie o
	 * uspesnom ulozeni.
	 * @param Form $form formular, s ktoreho sa maju spracovat udaje
	 * @param array $value Obsahuje informacie, ktore sa maju ulozit do databaze
	 */
	public function editFormSuccessed($form, $value)
	{
		$this->userManager->saveUser($value);
		$this->flashMessage('Užívateľ ' .$value['jmeno']. ' bol uložený.');
		$this->redirect('User:list');
	}
}
