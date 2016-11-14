<?php

namespace App\Presenters;

use App\Model\MedicineManager;
use App\Model\InsurenceManager;
use App\Model\OfficeManager;
use App\Model\ReservationManager;
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
class MedicinePresenter extends BasePresenter
{
	const
		MEDICINE_TYPE = array(
			'0' => 'Bez predpisu',
			'1' => 'Na predpis'
		);

	/** @var MedicineManager Informacie o lieku a praca s nim */
	protected $medicineManager;

	protected $insurenceManager;

	protected $officeManager;

	protected $reservationManager;

	public function __construct(MedicineManager $medicineManager)
	{
		parent::__construct();
		$this->medicineManager = $medicineManager;
	}

	public function injectInsurenceManager(InsurenceManager $insurenceManager)
	{
		$this->insurenceManager = $insurenceManager;
	}

	public function injectOfficeManager(OfficeManager $officeManager)
	{
		$this->officeManager = $officeManager;
	}

	public function injectReservationManager(ReservationManager $reservationManager)
	{
		$this->reservationManager = $reservationManager;
	}

	/**
	 * Nacita liek z databaze a vykresli ho podla zadanej url.
	 * @param int $id Id clanku, ktory sa ma vypisat
	 * @throws BadRequestException Neexistujuce id lieku
	 */
	public function renderDetail($id)
	{
		if (!$id)
			$id = 1;

		if (!($medicine = $this->medicineManager->getMedicine($id)))
			throw new BadRequestException();

		$this->template->medicine = $medicine;
		$this->template->reservations = $this->medicineManager->relatedReservation($id);
		$this->template->offices = $this->medicineManager->relatedOffices($id);
		$this->template->insurences = $this->medicineManager->relatedInsurences($id);
	}

	/**
	 * Zobrazenie zoznamu liekov a zotriedenie podla parametrov.
	 * @param string $column Nazov stlpca pomocou, ktoreho sa bude zoradovat
	 * @param string $sort Typ triedenia, zostupne alebo vzostupne
	 */
	public function renderList($column, $sort)
	{
		$this->template->medicines = $this->medicineManager->getMedicines($column, $sort);
		$this->template->relatedMedineWithPaid = $this->medicineManager->medicinesPaid(true);
		$this->template->relatedMedineWithoutPaid = $this->medicineManager->medicinesPaid(false);
		$this->template->medicinesAdditionalCharge = $this->medicineManager->medicinesAdditionalCharge();
		$this->template->medicineWithPrescription = $this->medicineManager->medicinePrescription(true);
		$this->template->medicineWithoutPrescription = $this->medicineManager->medicinePrescription(false);
	}

	/**
	 * Odstranenie lieku z databaze a presmerovanie na zoznam liekov.
	 * @param ID lieku, ktory sa ma odstranit
	 */
	public function actionRemove($idd, $table, $id)
	{
		if ($table == 'liek') {
			$this->medicineManager->removeMedicine($id);
			$this->flashMessage('Liek bol odstránený.');
			$this->redirect('Medicine:list');
		}
		else if ($table == 'odstranenie-lieku') {
			$this->medicineManager->removeMedicineFromReservation($idd, $id);
			$this->flashMessage('Liek bol odstránený z rezervácie.');
			$this->redirect('Medicine:detail', $idd);
		}
		else if ($table == 'pobocka') {
			$this->medicineManager->removeMedicineFromOffice($idd, $id);
			$this->flashMessage('Liek bol odstránený z pobočky.');
			$this->redirect('Medicine:detail', $idd);
		}
		else if ($table == 'poistovna') {
			$this->medicineManager->removeMedicineFromInsurence($idd, $id);
			$this->flashMessage('Liek bol odstránený z poisťovne.');
			$this->redirect('Medicine:detail', $idd);
		}
	}

	/**
	 * Uprava lieku na zaklade zadaneho ID. V pripade, ze nie je zadane
	 * ID lieku vykresli sa formular na vytvorenie noveho lieku.
	 * @param int $id ID lieku, ktory sa ma editovat
	 */
	public function actionEdit($id)
	{
		if (!$id) {
			$this->template->isEditForm = false;
			return;
		}

		if ($medicine = $this->medicineManager->getMedicine($id)->toArray()) {
			$medicine['offices'] = $this->medicineManager->getOfficesEditValues($id);
			$medicine['insurences'] = $this->medicineManager->getInsurencesEditValues($id);
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($medicine);
		}
		else {
			$this->flashMessage('Liek nebol nájdený.');
			$this->redirect('Medicine:list');
		}
	}

	/**
	 * Vytvori formular pre editovanie lieku.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addGroup();
		$form->addHidden('ID_leku');
		$form->addText('nazev_leku', 'Názov lieku')
			->addRule(Form::FILLED, 'Zadajte názov lieku');
		$form->addSelect('typ_leku', 'Typ lieku', self::MEDICINE_TYPE)
			->setPrompt('Zvoľte typ lieku')
			->setRequired(TRUE)
			->setAttribute('class', 'form-control');

		$form->addGroup("Poisťovne");
		$removeEvent = [$this, 'removeElementClicked'];
		$insurences = $form->addDynamic(
			'insurences',
			function (Container $insurence) use ($removeEvent) {
				$insurence->addHidden('ID_leku');
				$insurence->addSelect('ID_pojistovny', 'Poisťovňa', $this->insurenceManager->getInsurenceToSelectBox())
					->setRequired(TRUE)
					->setPrompt('Zvoľte poisťovňu')
					->setAttribute('class', 'form-control');
				$insurence->addText('cena', 'Cena lieku')
					->setRequired(FALSE)
					->addRule(Form::FLOAT, 'Cena musí byť číslo');
				$insurence->addText('doplatek', 'Doplatok na liek')
					->setRequired(FALSE)
					->addRule(Form::FLOAT, 'Doplatok musí byť číslo');
				$insurence->addSelect('hradene', 'Typ lieku', array('hradene' => 'Hradený', 'nehradene' => 'Nehradený', 'doplatok' => 'Liek s doplatkom'))
					->setPrompt('Zvoľte typ lieku')
					->setRequired(TRUE)
					->setAttribute('class', 'form-control');
				$removeBtn = $insurence->addSubmit('remove', 'Odstrániť poisťovňu')
					->setAttribute('class', 'btn-danger')
					->setValidationScope(false);
				$removeBtn->onClick[] = $removeEvent;
			}, 1
		);

		$insurences->addSubmit('add', 'Pridať poisťovňu')
			->setAttribute('class', 'btn-success')
			->setValidationScope(false)
			->onClick[] = [$this, 'addElementClicked'];

		$form->addGroup("Pobočky");
		$offices = $form->addDynamic(
			'offices',
			function (Container $office) use ($removeEvent) {
				$office->addHidden('ID_leku');
				$office->addSelect('ID_pobocky', 'Pobočka', $this->officeManager->getOfficesToSelectBox())
					->setRequired(TRUE)
					->setPrompt('Zvoľte pobočku')
					->setAttribute('class', 'form-control');
				$office->addText('pocet_na_sklade', 'Počet kusov')
					->setRequired(TRUE)
					->setDefaultValue('1')
					->addRule(Form::INTEGER, 'Počet kusov musí byť číslo')
					->addRule(Form::RANGE, 'Počet kusov musí byť kladné číslo', array(0, null));

				$removeBtn = $office->addSubmit('remove', 'Odstrániť pobočku')
					->setAttribute('class', 'btn-danger')
					->setValidationScope(false);
				$removeBtn->onClick[] = $removeEvent;
			}, 1
		);

		$offices->addSubmit('add', 'Pridať pobočku')
			->setAttribute('class', 'btn-success')
			->setValidationScope(false)
			->onClick[] = [$this, 'addElementClicked'];

		$form->addGroup('');
		$form->addSubmit('submit', 'Uložiť liek')
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
		$this->medicineManager->saveMedicine($button->getForm()->getValues(true));
		$this->flashMessage('Liek bol uložený.');
		$this->redirect('Medicine:list');
	}
}
