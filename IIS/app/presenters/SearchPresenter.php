<?php

namespace App\Presenters;

use App\Model\SearchManager;
use App\Presenters\BasePresenter;
use Nette\Application\BadRequestException;
use Nette\Application\UI\Form;
use Nette\Database\UniqueConstraintViolationException;
use Nette\Utils\Arrayhash;
use Nette\Forms\Container;
use Nette\Forms\Controls\SubmitButton;

/**
 * Spracovanie vykreslenie formulara pre vyhladanie.
 */
class SearchPresenter extends BasePresenter
{
	/** @var SearchManager Obsahuje metodu pre vyhladanie */
	protected $searchManager;

	public function __construct(SearchManager $searchManager)
	{
		parent::__construct();
		$this->searchManager = $searchManager;
	}

	public function renderList($data)
	{
		$this->template->medicines = $this->searchManager->search($data);
	}

	/**
	 * Vytvori formuar pre vyhladavanie liekov.
	 * @return Form vytvoreny formular, ktory sa ma vykreslit
	 */
	public function createComponentSearchForm()
	{
		$form = new Form;
		$form->addGroup('');
		$form->addText('search', 'Hľadaný výraz');
		$form->addGroup('');
		$form->addSubmit('submit', 'Vyhľadať')
			->setAttribute('class', 'btn-primary')
			->onClick[] = [$this, 'submitElementClicked'];

		return $this->bootstrapFormRender($form);
	}

	/**
	 * Vyhladanie zadanej hodnoty v liekoch.
	 * uspesnom ulozeni.
	 * @param SubmitButton $button
	 */
	public function submitElementClicked(SubmitButton $button)
	{
		$this->redirect('Search:list', $button->getForm()->getValues(true)['search']);
	}
}
