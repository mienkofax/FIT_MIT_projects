<?php

namespace App\Presenters;

use App\Model\InsurenceManager;
use App\Model\MedicineManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;

/*
 * Spracovanie vykreslenia formularov.
 */
class InsurencePresenter extends BasePresenter
{
	/** @var InsurenceManager Informacie o poistovni a praca s nou */
	protected $insurenceManager;

	protected $medicineManager;

	public function injectMedicineManager(MedicineManager $medicineManager)
	{
		$this->medicineManager = $medicineManager;
	}

	public function __construct(InsurenceManager $insurenceManager)
	{
		parent::__construct();
		$this->insurenceManager = $insurenceManager;
	}

	/**
	 * Nacita pojistovnu z databaze a vykresli ju podla zadanej url.
	 * @param int $id Id clanku, ktory sa ma vypisat
	 * @throws BadRequestException Neexistujuxe id pojistovne
	 */
	public function renderDetail($id)
	{
		if (!$id)
			$id = 1;

		if (!($insurence = $this->insurenceManager->getInsurence($id)))
			throw new BadRequestException();

		$this->template->insurence = $insurence;
		$this->template->medicinesWithPaid = $this->insurenceManager->relatedMedicinesPaid($id, true);
		$this->template->medicinesWithoutPaid = $this->insurenceManager->relatedMedicinesPaid($id, true);
		$this->template->medicinesAdditionalCharge = $this->insurenceManager->relatedMedicinesAdditionalCharge($id, true);
		$this->template->CountDBItem = $this->insurenceManager->countDBItem($id);
	}

	/**
	 * Zobrazenie zoznamu pojistovni a zotriedenie podla parametrov.
	 * @param string $column Nazov stlpca pomocou, ktoreho sa bude zoradovat
	 * @param string $sort Typ triedenia, zostupne alebo vzostupne
	 */
	public function renderList($column, $sort)
	{
		$this->template->insurences = $this->insurenceManager->getInsurences($column, $sort);
	}

	/**
	 * Odstranenie pojistovne z databaze a presmerovanie na zoznam poistovni.
	 * @param ID poistovne, ktora sa ma odstranit
	 */
	public function actionRemove($idd, $table, $id)
	{
		if ($table == 'poistovna') {
			$this->insurenceManager->removeInsurence($id);
			$this->flashMessage('Poisťovňa bola odstranená.');
			$this->redirect('Insurence:list');
		} else if ($table == 'liek') {
			$this->flashMessage('Lek bola odstranená.');
			$this->medicineManager->removeMedicine($id);
			$this->redirect('Insurence:detail');
		}
	}

	/**
	 * Uprava poistovne na zaklade zadaneho ID. V pripade, ze nie je zadane
	 * ID poistovne vykresli sa formular na vytvorenie novej poistovne.
	 * @param int $id ID poistovne, ktora sa ma editovat
	 */
	public function actionEdit($id)
	{
		if (!$id) {
			$this->template->isEditForm = false;
			return;
		}

		if ($insurence = $this->insurenceManager->getInsurence($id)) {
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($insurence);
		}
		else {
			$this->flashMessage('Poisťovňa nebola nájdená.');
			$this->redirect('Insurence:list');
		}
	}

	/**
	 * Vytvori formuar pre editovanie pojistovni.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addHidden('ID_pojistovny');
		$form->addText('nazev_pojistovny', 'Názov poisťovne')
			->addRule(Form::FILLED, 'Zadajte názov poisťovne');
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
		$form->addSubmit('submit', "Uložiť poisťovňu");
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
		$this->insurenceManager->saveInsurence($value);
		$this->flashMessage('Poisťovňa ' .$value['nazev_pojistovny']. ' bola uložená');
		$this->redirect('Insurence:list');
	}
}
