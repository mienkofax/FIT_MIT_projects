<?php

namespace App\Presenters;

use App\Model\MedicineManager;
use App\Model\OfficeManager;
use App\Model\ReservationManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;
use Nette\Application\UI\Form;
use Nette\Forms\Controls\SubmitButton;
use Nette\Callback;
use Nette\Forms\Container;

/**
 * Spracovanie vykreslenie zoznamu rezervacii, detail rezervacie a formulara
 * pre pridanie alebo editovanie rezervácie.
 */
class ReservationPresenter extends BasePresenter
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

	/** @var ReservationManager Informacie o rezervaciach a praca s nimi */
	protected $reservationManager;

	/** @var MedicineManager Informacie o lekoch a praca s nimi */
	protected $medicineManager;

	public function __construct(ReservationManager $reservationManager)
	{
		parent::__construct();
		$this->reservationManager = $reservationManager;
	}

	public function injectMedicineManager(MedicineManager $medicineManager)
	{
		$this->medicineManager = $medicineManager;
	}

	public function injectOfficeManager(Officemanager $officeManager)
	{
		$this->officeManager = $officeManager;
	}

	/**
	 * Nacita rezervaciu z databaze a vykresli ju podla zadanej url.
	 * @param int $id Id rezervacie, ktory sa ma vypisat
	 * @throws BadRequestException Neexistujuxe id rezervacie
	 */
	public function renderDetail($id)
	{
		if (!$id)
			$id = 1;

		if (!($reservation = $this->reservationManager->getReservation($id)))
			throw new BadRequestException();

		$this->template->reservation = $reservation;
		$this->template->reservationState = self::RESERVATION_STATE;
		$this->template->medicines = $this->reservationManager->relatedMedicines($id);
	}

	/**
	 * Zobrazenie zoznamu rezervacii a zotriedenie podla parametrov.
	 * @param string $column Nazov stlpca pomocou, ktoreho sa bude zoradovat
	 * @param string $sort Typ triedenia, zostupne alebo vzostupne
	 */
	public function renderList($column, $sort)
	{
		$this->template->reservations = $this->reservationManager->getReservations($column, $sort);
		$this->template->reservationState = self::RESERVATION_STATE;
	}

	/**
	 * Odstranenie rezervacie z databaze a presmerovanie na zoznam rezervacii.
	 * @param ID rezervacie, ktora sa ma odstranit
	 */
	public function actionRemove($idd, $table, $id)
	{
		if ($table == 'rezervacia') {
			$this->reservationManager->removeReservation($id);
			$this->flashMessage('Rezervácia bola odstranená.');
			$this->redirect('Reservation:list');
		}
		else if ($table == 'rezervovany-liek') {
			$this->reservationManager->removeMedicineFromReservation($idd, $id);
			$this->flashMessage('Liek bol odstránený z rezervácie.');
			$this->redirect('Reservation:detail', $id);
		}

		echo $table;
		exit();
	}

	/**
	 * Uprava rezervacie na zaklade zadaneho ID. V pripade, ze nie je zadane
	 * ID rezervacie vykresli sa formular na vytvorenie novej rezervacie.
	 * @param int $id ID rezervacie, ktora sa ma editovat
	 */
	public function actionEdit($id)
	{
		if (!$id) {
			$this->template->isEditForm = false;
			return;
		}

		if ($office = $this->reservationManager->getReservation($id)->toArray()) {
			$office['reservations'] = $this->reservationManager->getReservationEditValues($id);
			$this->template->isEditForm = true;
			$this['editForm']->setDefaults($office);
		}
		else {
			$this->flashMessage('Rezervcia nebola nájdená.');
			$this->redirect('Reservation:list');
		}
	}

	/**
	 * Vytvori formuar pre editovanie rezervacii.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentEditForm()
	{
		$form = new Form;
		$form->addGroup('');
		$form->addHidden('ID_rezervace');
		$form->addSelect('stav_rezervace', 'Stav rezervácie lieku', self::RESERVATION_STATE)
			->setPrompt('Zvoľte stav rezervácie lieku')
			->setRequired(TRUE)
			->setAttribute('class', 'form-control');
		$form->addText('jmeno', 'Meno zákazníka')
			->addRule(Form::FILLED, 'Zadajte meno');
		$form->addText('rodne_cislo', 'Rodné číslo zákazníka')
			->setRequired(TRUE)
			->setAttribute('placeholder', 'XXXXXX/XXXX')
			->addRule(Form::PATTERN, 'Nesprávny tvar rodného čísla', '([0-9]{6}\/[0-9]{4})');
		$form->addText('prijmeni', 'Priezvisko zákazníka')
			->addRule(Form::FILLED, 'Zadajte priezvisko');

		$form->addGroup('Rezervované lieky');
		$removeEvent = [$this, 'removeElementClicked'];
		$reservations = $form->addDynamic(
			'reservations',
			function (Container $reservation) use ($removeEvent) {
				$reservation->addHidden('ID_rezervace');
				$reservation->addSelect('ID_leku', 'Lieky', $this->medicineManager->getMedicinesToSelectBox())
					->setPrompt('Zvoľte liek')
					->setRequired(TRUE)
					->setAttribute('class', 'form-control');
				$reservation->addText('pocet_rezervovanych', 'Počet kusov')
					->setRequired(TRUE)
					->addRule(Form::INTEGER, 'Počet kusov musí být číslo')
					->addRule(Form::RANGE, 'Počet kusok musí byť kladné číslo', array(0, null))
					->setDefaultValue('1');
				$reservation->addSelect('ID_pobocky', 'Pobočky', $this->officeManager->getOfficesToSelectBox())
					->setPrompt('Zvoľte pobočku')
					->setRequired(TRUE)
					->setAttribute('class', 'form-control');

				$removeBtn = $reservation->addSubmit('remove', 'Odstrániť liek')
					->setAttribute('class', 'btn-danger')
					->setValidationScope(false);
				$removeBtn->onClick[] = $removeEvent;
			}, 1
		);

		$form->addGroup('');
		$reservations->addSubmit('add', 'Pridat liek do rezervácie')
			->setValidationScope(false)
			->setAttribute('class', 'btn-success')
			->onClick[] = [$this, 'addElementClicked'];

		$form->addGroup('');
		$form->addSubmit('submit', 'Uložiť rezerváciu')
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
		$this->reservationManager->saveReservation($button->getForm()->getValues(true));
		$this->flashMessage('Rezervácia bola uložená');
		$this->redirect('Reservation:list');
	}
}
