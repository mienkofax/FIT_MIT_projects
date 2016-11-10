<?php

namespace App\Presenters;

use App\Model\MedicineManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;

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

	public function __construct(MedicineManager $medicineManager)
	{
		parent::__construct();
		$this->medicineManager = $medicineManager;
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

		if ($medicine = $this->medicineManager->getMedicine($id)) {
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
		$form->addHidden('ID_leku');
		$form->addText('nazev_leku', 'Názov lieku')
			->addRule(Form::FILLED, 'Zadajte názov lieku');
		$form->addSelect('typ_leku', 'Typ lieku', self::MEDICINE_TYPE)
			->setPrompt('Zvoľte typ lieku')
			->setAttribute('class', 'form-control');
		$form->addText('cena', 'Cena lieku')
			->setRequired(FALSE)
			->addRule(Form::FLOAT, 'Cena musí byť číslo');
		$form->addText('pocet_na_sklade', 'Počet kusov')
			->setRequired(FALSE)
			->addRule(Form::INTEGER, 'Počet kusov musí bzť číslo');
		$form->addSubmit('submit', "Uložiť liek");
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
		$this->medicineManager->saveMedicine($value);
		$this->flashMessage('Liek ' .$value['nazev_leku']. ' bol uložený.');
		$this->redirect('Medicine:list');
	}
}
