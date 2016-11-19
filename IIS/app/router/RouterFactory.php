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
		$router[] = new Route('[url]', 'Administration:default');
		$router[] = new Route('<presenter=Administration>/<action=default>', array(
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
			'action' => array(
					Route::FILTER_TABLE => array(
						'editor' => 'edit',
						'odstranit' => 'remove',
						'detail' => 'detail',
						'pridat' => 'add'
					),
					Route::FILTER_STRICT => true
			),
		));
		$router[] = new Route('<presenter>/<action>/<idd [0-9]+>/<table>/<id [0-9]+>/[<sect>]', array(
			'action' => array(
				Route::FILTER_TABLE => array(
					'odstranit' => 'remove',
					'editor' => 'edit',
				),
				Route::FILTER_STRICT => true
			),
			'column' => null,
			'sort' => null
		));
		$router[] = new Route('<presenter>/<action>[/<column>][/<sort>]', array(
			'action' => array(
				Route::FILTER_TABLE => array(
					'zoznam' => 'list'
				),
				Route::FILTER_STRICT => true
			),
			'column' => null,
			'sort' => null
		));

		return $router;
	}
}
