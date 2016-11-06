<?php

namespace App;

use Nette\Security\Permission;

class AccessListFactory extends Permission
{
	public function __construct()
	{
		//$permission = new Permission();

		// Uzivatelske role
		$this->addRole('guest');
		$this->addRole('member', 'guest');
		$this->addRole('manager', 'member');
		$this->addRole('admin', 'manager');

		// Zdroje ku, ktorym je mozne udelit pristup
		$this->addResource('Administration');
		$this->addResource('Office');

		// Zoznam pravidiel pre opravnenia
		$this->allow('guest', 'Administration', array('login', 'logout', 'register'));
		$this->allow('member', 'Administration', array('default'));

		// Administrator ma prava na vsetko
		$this->allow('admin', Permission::ALL, Permission::ALL);
	}
}
