<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;
use Nette\Databse\DriverException;

/**
 * Model pre vyhladanie lieku.
 */
class SearchManager extends BaseManager
{
	public function search($search)
	{
		return $this->database->table('leky')->where('nazev_leku LIKE ?', "%".$search."%");
	}
}
