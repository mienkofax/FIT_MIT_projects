<?php

namespace App\Presenters;

use App\Model\SupplierManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;

/*
 * Spracovanie vykreslenia formularov.
 */
class SupplierPresenter extends BasePresenter
{
	/** @var SupplerManager Informacie o dodavatelovi a praca s nou */
	protected $supplierManager;

	public function __construct(SupplierManager $supplierManager)
	{
		parent::__construct();
		$this->supplierManager = $supplierManager;
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
	public function actionRemove($id)
	{
		$this->supplierManager->removeSupplier($id);
		$this->flashMessage('Dodávateľ bol odstránený.');
		$this->redirect('Supplier:list');
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

		if ($supplier = $this->supplierManager->getSupplier($id)) {
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
		$form->addHidden('ID_dodavatele');
		$form->addText('nazev_dodavatele', 'Názov poisťovne')
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
		$form->addSubmit('submit', "Uložiť dodávateľa");
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
