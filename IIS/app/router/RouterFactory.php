<?php

namespace App;

use Nette\Application\Routers\RouteList;
use Nette\Application\Routers\Route;

class RouterFactory
{
	/**
	 * @return RouteList
	 */
	public static function createRouter()
	{
		$router = new RouteList;
		$router[] = new Route('<action>', array(
			'presenter' => 'Administration',
			'action' => array(
				Route::FILTER_TABLE => array(
					'administracia' => 'default',
					'prihlasenie' => 'login',
					'odhlasit' => 'logout',
					'registracia' => 'register'
				),
				Route::FILTER_STRICT => true
			)
		));
		$router[] = new Route('<presenter>/<action>[/<id [0-9]+>]', array(
			'presenter' => 'Office',
			'action' => array(
					Route::FILTER_TABLE => array(
						'editor' => 'edit',
						'odstranit' => 'remove',
						'detail' => 'detail'
					),
					Route::FILTER_STRICT => true
			),
		));
		$router[] = new Route('<presenter>/<action>[/<column>][/<sort>]', array(
			'presenter' => 'Office',
			'action' => array(
				Route::FILTER_TABLE => array(
					'seznam-clanku' => 'list'
				),
				Route::FILTER_STRICT => true
			),
			'column' => null,
			'sort' => null
		));
		$router[] = new Route('[url]', 'Administration:default');

		return $router;
	}
}
