<?php

namespace App\Presenters;

use App\Model\SupplierManager;
use App\Model\OfficeManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;
use Nette\Forms\Container;

/*
 * Spracovanie vykreslenia formularov.
 */
class SupplierPresenter extends BasePresenter
{
	/** @var SupplerManager Informacie o dodavatelovi a praca s nou */
	protected $supplierManager;

	/** @var OfficeManager Informacie o pobocke a praca s nou */
	protected $officeManager;

	public function __construct(SupplierManager $supplierManager)
	{
		parent::__construct();
		$this->supplierManager = $supplierManager;
	}

	public function injectOfficeManager(OfficeManager $officeManager)
	{
		$this->officeManager =$officeManager;
	}

	/**
	 * Nacita dodavatela z databaze a vykresli ho podla zadanej url.
	 * @param int $id Id clanku, ktory sa ma vypisat
	 * @throws BadRequestException Neexistujuxe id dodavatela
	 */
	public function renderDetail($id)
	{
		if (!$id)
			$id = 1;

		if (!($supplier = $this->supplierManager->getSupplier($id)))
			throw new BadRequestException();

		$this->template->supplier = $supplier;
		$this->template->offices = $this->supplierManager->relatedOffices($id);
	}

	/**
	 * Zobrazenie zoznamu dodavatelov a zotriedenie podla parametrov.
	 * @param string $column Nazov stlpca pomocou, ktoreho sa bude zoradovat
	 * @param string $sort Typ triedenia, zostupne alebo vzostupne
	 */
	public function renderList($column, $sort)
	{
		$this->template->suppliers = $this->supplierManager->getSuppliers($column, $sort);
	}

	/**
	 * Odstranenie dodavatela z databaze a presmerovanie na zoznam dodavatelov.
	 * @param ID dodavatela, ktora sa ma odstranit
	 */
	public function actionRemove($idd, $table, $id)
	{
		if ($table == 'dodavatel') {
			$this->supplierManager->removeSupplier($id);
			$this->flashMessage('Dodávateľ bol odstránený.');
			$this->redirect('Supplier:list');
		}
		else if ($table == 'pobocka') {
			$this->supplierManager->removeOfficeFromSupplier($idd, $id);
			$this->flashMessage('Dodávanie na danú pobočku bolo odstránené.');
			$this->redirect('Supplier:detail', $idd);
		}
	}

	/**
	 * Uprava dodavatela na zaklade zadaneho ID. V pripade, ze nie je zadane
	 * ID dodavatela vykresli sa formular na vytvorenie noveho dodavatela.
	 * @param int $id ID dodavatela, ktora sa ma editovat
	 */
	public function actionEdit($id)
	{
		if (!$id) {
			$this->template->isEditForm = false;
			return;
		}

		if ($supplier = $this->supplierManager->getSupplier($id)->toArray()) {
			$supplier['ID_pobocky'] = $this->supplierManager->getOfficesID($id);
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($supplier);
		}
		else {
			$this->flashMessage('Dodávateľ nebol nájdený.');
			$this->redirect('Supplier:list');
		}
	}

	/**
	 * Vytvori formular pre editovanie dodavatelov.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addGroup('');
		$form->addHidden('ID_dodavatele');
		$form->addText('nazev_dodavatele', 'Názov dodávateľa')
			->addRule(Form::FILLED, 'Zadajte názov dodávateľa');
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
		$form->addGroup('');
		$form->addMultiSelect('ID_pobocky', 'Pobočky', $this->officeManager->getOfficesToSelectBox())
			->setAttribute('class', 'form-control');
		$form->addSubmit('submit', "Uložiť dodávateľa")
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
		$this->supplierManager->saveSupplier($value);
		$this->flashMessage('Dodávateľ ' .$value['nazev_dodavatele']. ' bol uložený.');
		$this->redirect('Supplier:list');
	}
}
