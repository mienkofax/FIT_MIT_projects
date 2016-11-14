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
use Nette\Forms\Controls\SubmitButton;
use Nette\Callback;
/*
 * Spracovanie vykreslenia formularov.
 */
class TestPresenter extends BasePresenter
{
	public function createComponentTestForm()
	{
		$form = new Form();
		$form->addGroup();
		$removeEvent = [$this, 'MyFormRemoveElementClicked'];
		$reservations = $form->addDynamic(
			'reservations',
			function (Container $reservation) use ($removeEvent) {
				$reservation->addSelect('ID_leku', 'Liek', array(0 => "liek 1"))
					->setAttribute('class', 'form-control');
				$reservation->addText('pocet_rezervovanych', 'Počet rezervovaných')
					->setRequired(TRUE)
					->addRule(Form::INTEGER, 'Počet kusok musí být číslo');
				$reservation->addSelect('ID_pobocky', 'Pobočky', array(0 => "pobocka"))
					->setAttribute('class', 'form-control');

				$addBtn->onClick[] = \Nette\Utils\Callback::closure($this, 'MyFormAddElementClicked');
				$removeBtn = $reservation->addSubmit('remove', 'Odstranit liek')
					->setValidationScope(false);
				$removeBtn->onClick[] = $removeEvent;
			},
			1
		);

		$reservations->addSubmit('add', 'Pridat liek na rezervaciu')
			->setValidationScope(false)
			->onClick[] = [$this, 'MyFormAddElementClicked'];

		$form->addGroup();
		$form->addSubmit('submit', 'Send')
			->onClick[] = [$this, 'saveData'];

		return $this->bootstrapFormRender($form);
	}

	public function MyFormAddElementClicked(SubmitButton $button)
	{
		$button->parent->createOne();
	}

	public function MyFormRemoveElementClicked(SubmitButton $button)
	{
		// first parent is container
		// second parent is it's replicator
		$users = $button->parent->parent;
		$users->remove($button->parent, true);
	}

	public function saveData(SubmitButton $button)
	{
//		print_r($button->getForm()->getValues(true));

		//$this->reservationManager->saveReservation($value);
		$this->flashMessage('Pobočka  bola uložená');
		$this->redirect('Office:list');
	}


}
