<?php

namespace App\Presenters;

use App\Model\OfficeManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;

/**
 * Spracovanie vykreslenie formularov.
 */
class OfficePresenter extends BasePresenter
{
	/** @var OfficePresenter Informacie o pobockach a praca s nimi */
	protected $officeManager;

	public function __construct(OfficeManager $officeManager)
	{
		parent::__construct();
		$this->officeManager = $officeManager;
	}

	/**
	 * Nacita pobocku z databaze a vykresli ju podla zadanej url.
	 * @param int $id Id clanku, ktory sa ma vypisat
	 * @throws BadRequestException Neexistujuxe id pobocky
	 */
	public function renderDetail($id)
	{
		if (!$id)
			$id = 1;

		if (!($office = $this->officeManager->getOffice($id)))
			throw new BadRequestException();

		$this->template->office = $office;
	}

	/**
	 * Zobrazenie zoznamu pobociek a zotriedenie podla parametrov.
	 * @param string $column Nazov stlpca pomocou, ktoreho sa bude zoradovat
	 * @param string $sort Typ triedenia, zostupne alebo vzostupne
	 */
	public function renderList($column, $sort)
	{
		$this->template->offices = $this->officeManager->getOffices($column, $sort);
	}

	/**
	 *	Odstranenie pobocky z databaze a presmerovanie na zoznam pobociek.
	 * @param ID pobocky, ktora sa ma odstranit
	 */
	public function actionRemove($id)
	{
		$this->officeManager->removeOffice($id);
		$this->flashMessage('Pobočka bola odstranená.');
		$this->redirect(':Office:list');
	}

	/**
	 * Uprava pobocky na zaklade zadaneho ID. V pripade, ze nie je zadane
	 * ID pobocky vykresli sa formular na vytvorenie novej pobocky.
	 * @param int $id ID pobocky, ktora sa ma editovat
	 */
	public function actionEdit($id)
	{
		if (!$id) {
			$this->template->isEditForm = false;
			return;
		}

		if ($office = $this->officeManager->getOffice($id)) {
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($office);
		}
		else {
			$this->flashMessage('Pobočka nebola nebola nájdená.');
			$this->redirect('Office:list');
		}
	}

	/**
	 * Vytvori formuar pre editovanie pobociek.
	 * @param Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addHidden('ID_pobocky');
		$form->addText('nazev_pobocky', 'Názov pobočky')
			->addRule(Form::FILLED, 'Zadajte názov pobočky');
		$form->addText('ulice', 'Ulica')
			->setRequired(FALSE);
		$form->addText('mesto', 'Mesto')
			->setRequired(FALSE);
		$form->addText('PSC', 'PSČ')
			->setRequired(FALSE)
			->addRule(Form::PATTERN, 'PSČ musí mať 5 číslic', '([0-9]\s*){5}');
		$form->addText('telefonni_cislo', 'Telefónne číslo')
			->setRequired(FALSE)
			->addRule(Form::INTEGER, 'Telefónne číslo musí být číslo');
		$form->addText('email', 'E-mail')
			->setRequired(FALSE)
			->addRule(Form::EMAIL, 'Nesprávny tvar adresy');
		$form->addSubmit('submit', "Uložiť pobočku");
		$form->onSuccess[] = [$this, 'editFormSuccessed'];

		return $this->customFormRender($form);
	}

	/**
	 * Spracovanie hodnot z formulara. Ulozenie do databaze a informovanie o
	 * uspesnom ulozeni.
	 */
	public function editFormSuccessed($form, $value)
	{
		$this->officeManager->saveOffice($value);
		$this->flashMessage('Pobočka ' .$value['nazev_pobocky']. ' bola uložená');
		$this->redirect('Office:list');
	}
}
