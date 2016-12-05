<?php

namespace App\Model;

use App\Model\BaseManager;
use Nette\Database\Table\IRow;
use Nette\Database\Table\Selection;
use Nette\Utils\ArrayHash;
use Nette\Databse\DriverException;

/**
 * Model importuje doplatky na lieky z JSON.
 */
class AdministrationManager extends BaseManager
{
	public function update($data)
	{
		$data = json_decode($data, true);
		if ($data == "")
			return false;

		$data = $data['doplatky'];

		$this->database->beginTransaction();
		try {
			foreach($data as $item) {
				$this->database->query('UPDATE lek_pojistovny SET cena = ?, doplatek = ?, hradene = ? WHERE ID_pojistovny = ? and ID_leku = ?',
					$item['cena'], $item['doplatek'], $item['hradene'], $item['ID_pojistovny'], $item['ID_leku']);
			}

		}
		catch (Nette\Databse\DriverException $ex) {
			$this->database->rollback();
			return false;
		}
		$this->database->commit();

		return true;
	}
}
