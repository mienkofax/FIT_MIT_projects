<?php

namespace App\Presenters;

use App\Model\MedicineManager;
use App\Model\SupplierManager;
use App\Model\OfficeManager;
use App\Model\UserManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;
use Nette\Forms\Container;
use Nette\Forms\Controls\SubmitButton;
use Nette\Callback;

/**
 * Spracovanie vykreslenie formularov.
 */
class OfficePresenter extends BasePresenter
{
	const
		RESERVATION_STATE = array(
			'prijata' => 'Prijatá',
			'rozpracovana' => 'Rozpracovaná',
			'pripravena' => 'Pripravená',
			'dokoncena' => 'Dokončená'
		);

	/** @var OfficePresenter Informacie o pobockach a praca s nimi */
	protected $officeManager;

	/***/
	protected $supplierManager;

	protected $medicineManager;

	protected $userManager;

	public function __construct(OfficeManager $officeManager)
	{
		parent::__construct();
		$this->officeManager = $officeManager;
	}

	public function injectMedicineManager(MedicineManager $medicineManager)
	{
		$this->medicineManager = $medicineManager;
	}

	public function injectSupplierManager(SupplierManager $supplierManager)
	{
		$this->supplierManager = $supplierManager;
	}

	public function injectUserManger(UserManager $userManager)
	{
		$this->userManager = $userManager;
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
		$this->template->medicines = $this->officeManager->relatedMedicines($id);
		$this->template->suppliers = $this->officeManager->relatedSupplier($id);
		$this->template->users = $this->officeManager->relatedUser($id);
		$this->template->reservations = $this->officeManager->relatedReservation($id);
		$this->template->CountDBItem = $this->officeManager->countDBItem($id);
		$this->template->reservationState = self::RESERVATION_STATE;
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
	 * Odstranenie pobocky z databaze a presmerovanie na zoznam pobociek.
	 * @param ID pobocky, ktora sa ma odstranit
	 */
	public function actionRemove($idd, $table, $id)
	{
		if ($table == 'pobocka') {
			$this->officeManager->removeOffice($id);
			$this->flashMessage('Pobočka bola odstranená.');
			$this->redirect('Office:list');
		}
		else if ($table == 'dodavatel') {
			$this->supplierManager->removeOfficeFromSupplier($id, $idd);
			$this->redirect('Office:detail', $idd);
		}
		else if ($table == 'liek') {
			$this->medicineManager->removeMedicineFromOffice($id, $idd);
			$this->redirect('detail', $idd);
		}
		else if ($table == 'uzivatel') {
			$this->userManager->removeUserFromOffice($id, $idd);
			$this->redirect('detail', $idd);
		}
		else if ($table == 'rezervacia') {
			$this->officeManager->removeReservationFromOffice($idd, $id);
			$this->redirect('detail', $idd);
		}
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

		if ($office = $this->officeManager->getOffice($id)->toArray()) {
			$office['ID_dodavatele'] = $this->officeManager->getSuppliersEditValues($id);
			$office['ID_uzivatele'] = $this->officeManager->getUsersEditValues($id);
			$office['medicines'] = $this->officeManager->getMedicinesEditValues($id);
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($office);
		}
		else {
			$this->flashMessage('Pobočka nebola nájdená.');
			$this->redirect('Office:list');
		}
	}

	/**
	 * Vytvori formuar pre editovanie pobociek.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addGroup('');
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
			->setAttribute('placeholder', '+420 123 456 789')
			->addRule(Form::PATTERN, 'Nesprávny tvar telefónneho čísla', '(\+?\(?((?:\d[\s\)]*){3})?(?:\d[\s\-]*){9})');
		$form->addText('email', 'E-mail')
			->setRequired(FALSE)
			->addRule(Form::EMAIL, 'Nesprávny tvar adresy');
		$form->addGroup('Dodávatelia');
		$form->addMultiSelect('ID_dodavatele', 'Dodávatelia', $this->supplierManager->getSuppliersToSelectBox())
			->setAttribute('class', 'form-control');
		$form->addGroup('Zamestnanci');
		$form->addMultiSelect('ID_uzivatele', 'Zamestnanci', $this->userManager->getUsersToSelectBox())
			->setAttribute('class', 'form-control');

		$form->addGroup('Lieky');
		$removeEvent = [$this, 'removeElementClicked'];
		$medicines = $form->addDynamic(
			'medicines',
			function (Container $medicine) use ($removeEvent) {
				$medicine->addHidden('ID_pojistovny');
				$medicine->addSelect('ID_leku', 'Lieky', $this->medicineManager->getMedicinesToSelectBox())
					->setPrompt('Zvoľte liek')
					->setAttribute('class', 'form-control');
				$medicine->addText('pocet_na_sklade', 'Počet kusov na sklade')
					->setDefaultValue('1')
					->setRequired(FALSE)
					->addRule(Form::FLOAT, 'Počet musí byť číslo')
					->addRule(Form::RANGE, 'Počet kusov musí byť kladné číslo', array(0, null));

				$removeBtn = $medicine->addSubmit('remove', 'Odstrániť liek')
					->setAttribute('class', 'btn-danger')
					->setValidationScope(false);
				$removeBtn->onClick[] = $removeEvent;
			}, 1
		);

		$medicines->addSubmit('add', 'Pridať liek')
			->setAttribute('class', 'btn-success')
			->setValidationScope(false)
			->onClick[] = [$this, 'addElementClicked'];

		$form->addGroup('');
		$form->addSubmit('submit', 'Uložiť pobočku')
			->setAttribute('class', 'btn-primary')
			->onClick[] = [$this, 'submitElementClicked'];

		return $this->bootstrapFormRender($form);
	}

	/**
	 * Spracovanie hodnot z formulara. Ulozenie do databaze a informovanie o
	 * uspesnom ulozeni.
	 * @param SubmitButton $button
	 */
	public function submitElementClicked(SubmitButton $button)
	{
		$this->officeManager->saveOffice($button->getForm()->getValues(true));
		$this->flashMessage('Pobočka bola uložená');
		$this->redirect('Office:list');
	}
}
