<?php

namespace App\Presenters;

use Nette\Application\BadRequestException;
use Nette\Application\UI\Presenter;
use App\Model\MenuManager;

/**
 * Rozhranie pre spracovanie vykreslenia urciteho modelu.
 */
abstract class BasePresenter extends Presenter
{
	/** @var string Adresa prezenteru pre prihlasenie */
	protected $loginPresenter = 'Administration:login';

	/** @var MenuManager model pre pracu s menu */
	protected $menuManager;

	/**
	 * Kontrola opravneni pri kazdej akcii. Ak uzivatel nie je prihlaseny
	 * bude presmerovany na prihlasovaci formular.
	 */
	protected function startup()
	{
		parent::startup();

		if (!$this->getUser()->isAllowed($this->getName(), $this->getAction())) {
			$this->flashMessage('Daná sekcia je dostupná len po prihlásení.
				Ak ste prihlásený požiadajte administrátora o pridelenie
				oprávnení pre túto sekciu.');

			if ($this->loginPresenter)
				$this->redirect($this->loginPresenter);
		}
	}

	/**
	 * Automaticke injektovanie potrebnej triedy.
	 * @param MenuManger automaticky injektovana trieda uzivatel
	 *
	 */
	public function inject(MenuManager $menuManager)
	{
		$this->menuManager = $menuManager;
	}

	/**
	 * Akcie, ktore sa maju vykonat pred renderovanim sablony. Nastavuje
	 * premenne sablony, ktore su dostupne v @layout.latte, cize globalne
	 * pre vsetky sablony. Ulozi informaciu o pocte zaznamov v tabulkach.
	 */
	protected function beforeRender()
	{
		parent::beforeRender();
		$this->template->count = $this->menuManager->getTableRecord();
	}

	public function customFormRender($form)
	{
		$form->elementPrototype->addAttributes(array('novalidate' => 'novalidate'));
		$form->elementPrototype->addAttributes(array('class' => 'form-horizontal'));

		$renderer = $form->getRenderer();
		$renderer->wrappers['controls']['container'] = '';
		$renderer->wrappers['pair']['container'] = "div class='form-group'";
		$renderer->wrappers['label']['container'] = "label class='control-label col-xs-4'";
		$renderer->wrappers['control']['container'] = "div class='col-xs-6'";
		$renderer->wrappers['control']['.text'] = 'form-control';
		$renderer->wrappers['control']['.password'] = 'form-control';
		$renderer->wrappers['control']['.email'] = 'form-control';
		$renderer->wrappers['control']['.number'] = 'form-control';
		$renderer->wrappers['control']['.submit'] = 'btn btn-primary';

		return $form;
	}
}
