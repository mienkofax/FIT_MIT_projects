<?php

namespace App\Presenters;

use Nette;

/**
 *
 */
class Error4xxPresenter extends Nette\Application\UI\Presenter
{
	public function startup()
	{
		parent::startup();
		if (!$this->getRequest()->isMethod(Nette\Application\Request::FORWARD)) {
			$this->error();
		}
	}

	public function renderDefault(Nette\Application\BadRequestException $exception)
	{
		$file = __DIR__ . "/templates/Error/{$exception->getCode()}.latte";
		$this->template->setFile(is_file($file) ? $file : __DIR__ . '/templates/Error/4xx.latte');
	}
}
