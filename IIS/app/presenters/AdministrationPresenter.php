<?php

namespace App\Presenters;

use App\Presenters\BasePresenter;
use App\Forms\UserForms;
use App\Model\AdministrationManager;
use Nette\Application\UI\Form;
use Nette\Utils\ArrayHash;
use Nette\Forms\Container;
use Nette\Forms\Controls\SubmitButton;
use Nette\Callback;

/**
 * Spracovanie vykreslenia sekcie prihlasenia. Vykreslenie formularov.
 */
class AdministrationPresenter extends BasePresenter
{
	const
		JSON = '{
		  "doplatky": [
			{
			  "ID_pojistovny": "1",
			  "ID_leku": "5",
			  "cena": "165.00",
			  "doplatek": "NULL",
			  "hradene": "nehradene"
			},
			{
			  "ID_pojistovny": "2",
			  "ID_leku": "4",
			  "cena": "140.00",
			  "doplatek": "NULL",
			  "hradene": "nehradene"
			},
		]
}';
	/** @var UserForms Tovaren pre formulare pre prihlasenie a registraciu */
	private $userFormsFactory;

	/** @var array Inforacie o prihlaseni alebo registracii */
	private $instructions;

	private $administrationManager;

	public function __construct(UserForms $userForms)
	{
		$this->userFormsFactory = $userForms;
	}

	public function injectAdministrationManager(AdministrationManager $administrationManager)
	{
		$this->administrationManager = $administrationManager;
	}

	/**
	 * Nastavenie premennych pred kazdym volanim renderovanim.
	 */
	public function startup()
	{
		parent::startup();
		$this->instructions = array(
			'message' => null,
			'redirection' => 'Administration:'
		);
	}

	/**
	 * Prihlasenie pouzivatela a presmerovanie.
	 */
	public function actionLogin()
	{
		if ($this->getUser()->isLoggedIn())
			$this->redirect('Administration:default');
	}

	/**
	 * Odhalsenie pouzivatela a presmerovanie na prihlasenie.
	 */
	public function actionLogout()
	{
		$this->getUser()->logout();
		$this->redirect($this->loginPresenter);
	}

	/**
	 * Vykreslenia stranky s dashboardom po prihlaseni.
	 */
	public function renderDefault()
	{
		$identity = $this->getUser()->getIdentity();
	}

	/**
	 * Vratenie formularu pre registrovanie uzivatelov.
	 */
	protected function createComponentLoginForm()
	{
		$this->instructions['message'] = 'Boli ste úspešne prihlásený.';
		return $this->userFormsFactory->createLoginForm(ArrayHash::from($this->instructions));
	}

	public function createComponentImportForm()
	{
		$form = new Form;
		$form->addTextArea('imp', 'Doplatky na lieky')
			->setAttribute('class', 'form-control')
			->setAttribute('rows', '20')
			->setAttribute('placeholder', self::JSON)
			->setRequired(TRUE);
		$form->addSubmit('submit', 'Naskladniť')
			->setAttribute('class', 'btn-primary')
			->onClick[] = [$this, 'submitElementSubmitClicked'];

		return $this->bootstrapFormRender($form);
	}


	/**
	 * Spracovanie hodnot z formulara. Ulozenie do databaze a informovanie o
	 * uspesnom ulozeni.
	 * @param SubmitButton $button
	 */
	public function submitElementSubmitClicked(SubmitButton $button)
	{
		if ($this->administrationManager->update($button->getForm()->getValues(true)['imp'], true)) {
			$this->flashMessage('Import prebehol úspešne.');
			$this->redirect('Administration:import');
		}
		else {
			$this->flashMessage('Problém pri importe.');
		}
	}
}
