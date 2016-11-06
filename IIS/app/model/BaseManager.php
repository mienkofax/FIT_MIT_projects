<?php

namespace App\Model;

use Nette\Database\Context;
use Nette\Object;

/**
 * Rozhranie pre modely, spristupnuje pracu s databazou.
 */
abstract class BaseManager extends Object
{
	protected $database;

	const
		SORT_TYPE = array(
			'desc' => 'DESC',
			'asc' => 'ASC'
		);

	public function __construct(Context $database)
	{
		$this->database = $database;
	}
}
