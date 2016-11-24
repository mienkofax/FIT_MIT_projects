<?php

namespace App\Presenters;

use App\Model\InsurenceManager;
use App\Model\MedicineManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;
use Nette\Forms\Container;
use Nette\Forms\Controls\SubmitButton;
use Nette\Callback;

/*
 * Spracovanie vykreslenia formularov.
 */
class InsurencePresenter extends BasePresenter
{
	/** @var InsurenceManager Informacie o poistovni a praca s nou */
	protected $insurenceManager;

	/** @var MedicineManager Informacie o liekoch a praca s nimi */
	protected $medicineManager;

	public function __construct(InsurenceManager $insurenceManager)
	{
		parent::__construct();
		$this->insurenceManager = $insurenceManager;
	}

	public function injectMedicineManager(MedicineManager $medicineManager)
	{
		$this->medicineManager = $medicineManager;
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
		$this->template->medicinesWithoutPaid = $this->insurenceManager->relatedMedicinesPaid($id, false);
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
			$this->insurenceManager->removeOfficeFromSupplier($idd, $id);
			$this->flashMessage('Liek bol odstranený z danej poisťovne.');
			$this->redirect('Insurence:detail', $idd);
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

		if ($insurence = $this->insurenceManager->getInsurence($id)->toArray()) {
			$this->template->isEditForm = true;
			$insurence['medicines'] = $this->insurenceManager->getMedicinesEditValues($id);
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
		$form->addGroup('');
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
			->setAttribute('placeholder', '+420 123 456 789')
			->addRule(Form::PATTERN, 'Nesprávny tvar telefónneho čísla', '(\+?\(?((?:\d[\s\)]*){3})?(?:\d[\s\-]*){9})');
		$form->addText('email', 'E-mail')
			->setRequired(FALSE)
			->addRule(Form::EMAIL, 'Nesprávny tvar adresy');

		$form->addGroup('Hradené lieky');
		$removeEvent = [$this, 'removeElementClicked'];
		$medicines = $form->addDynamic(
			'medicines',
			function (Container $medicine) use ($removeEvent) {
				$medicine->addHidden('ID_pojistovny');
				$medicine->addSelect('ID_leku', 'Liek', $this->medicineManager->getMedicinesToSelectBox())
					->setPrompt('Zvoľte liek')
					->setRequired(TRUE)
					->setAttribute('class', 'form-control');
				$medicine->addText('cena', 'Cena lieku')
					->setRequired(FALSE)
					->addRule(Form::FLOAT, 'Cena musí byť číslo');
				$medicine->addText('doplatek', 'Doplatok za liek')
					->setRequired(FALSE)
					->addRule(Form::FLOAT, 'Doplatok musí byť číslo');
				$medicine->addSelect('hradene', 'Typ lieku', array('hradene' => 'Hradený', 'nehradene' => 'Nehradený', 'doplatok' => 'Liek s doplatkom'))
					->setPrompt('Zvoľte typ lieku')
					->setRequired(TRUE)
					->setAttribute('class', 'form-control');

				$removeBtn = $medicine->addSubmit('remove', 'Odstrániť liek')
					->setAttribute('class', 'btn-danger')
					->setValidationScope(false);
				$removeBtn->onClick[] = $removeEvent;
			}, 1
		);


		$medicines->addSubmit('add', 'Pridať liek')
			->setValidationScope(false)
			->setAttribute('class', 'btn-success')
			->onClick[] = [$this, 'addElementClicked'];

		$form->addGroup('');
		$form->addSubmit('submit', 'Uložiť poisťovňu')
			->setAttribute('class', 'btn-primary')
			->onClick[] = [$this, 'submitElementClicked'];

		return $this->bootstrapFormRender($form);
	}

	/**
	 * Spracovanie hodnot z formulara. Ulozenie do databaze a informovanie o
	 * uspesnom ulozeni.
	 * @param SubmitButton
	 */
	public function submitElementClicked(SubmitButton $button)
	{
		$this->insurenceManager->saveInsurence($button->getForm()->getValues(true));
		$this->flashMessage('Poisťovňa bola uložená');
		$this->redirect('Insurence:list');
	}

}
