<?php

namespace App\Presenters;

use App\Presenters\BasePresenter;
use App\Forms\UserForms;
use Nette\Application\UI\Form;
use Nette\Utils\ArrayHash;

/**
 * Spracovanie vykreslenia sekcie prihlasenia. Vykreslenie formularov pre
 * prihlásenie/registrovanie.
 */
class AdministrationPresenter extends BasePresenter
{
	/** @var UserForms Tovaren pre formulare pre prihlasenie a registraciu */
	private $userFormsFactory;

	/** @var array Inforacie o prihlaseni alebo registracii */
	private $instructions;

	public function __construct(UserForms $userForms)
	{
		$this->userFormsFactory = $userForms;
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

	/**
	 * Vratenie formulara pre registrovanie noveho uzivatela.
	 */
	protected function createComponentRegisterForm()
	{
		$this->instructions['message'] = 'Boli ste úspešne zaregistrovaný.';
		return $this->userFormsFactory->createRegisterForm(ArrayHash::from($this->instructions));
	}
}
