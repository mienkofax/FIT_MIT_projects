<?php

namespace App\Model;

use Nette\Database\Context;
use Nette\Object;
use App\Model\BaseManager;

/**
 * Model, pre zistenie poctu zaznamov pre danu tabulku
 */
class MenuManager extends BaseManager
{

	/**
	 * Pocet jednotlivych zaznamov v databaze k tabulkam.
	 * @return Array Pole obsahujuce pocet zaznamov k danej tabulke
	 */
	public function getTableRecord()
	{
		$count['pobocky'] = $this->database->table('pobocky')->count('*');
		$count['pojistovny'] = $this->database->table('pojistovny')->count('*');
		$count['dodavatelia'] = $this->database->table('dodavatele')->count('*');
		$count['uzivatelia'] = $this->database->table('uzivatele')->count('*');
		$count['lieky'] = $this->database->table('leky')->count('*');
		$count['rezervacie'] = $this->database->table('rezervace_leku')->count('*');
		return $count;
	}
}
