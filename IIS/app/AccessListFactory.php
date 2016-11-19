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
		$this->addResource('Insurence');
		$this->addResource('Supplier');
		$this->addResource('User');
		$this->addResource('Medicine');
		$this->addResource('Reservation');

		// Zoznam pravidiel pre opravnenia
		$this->allow('guest', 'Administration', array('login', 'logout', 'register', 'default'));

		$this->allow('member', 'Medicine', array('list', 'detail', 'edit', 'edit'));
		$this->allow('member', 'Reservation', array('list', 'detail', 'edit', 'remove'));
		$this->allow('member', 'Office', array('list', 'detail'));

		$this->allow('manager', 'Office', array('edit', 'remove'));
		$this->allow('manager', 'Insurence', array('list', 'detail', 'edit', 'remove'));
		$this->allow('manager', 'Supplier', array('list', 'detail', 'edit', 'remove'));
		$this->allow('manager', 'User', array('list', 'detail', 'edit', 'remove'));

		// Administrator ma prava na vsetko
		$this->allow('admin', Permission::ALL, Permission::ALL);
	}
}
