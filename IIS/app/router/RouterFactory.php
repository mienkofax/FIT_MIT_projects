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
		$router[] = new Route('administration/<action>', 'Administration:import');
		$router[] = new Route('search/<list>/<search>', 'Search:list');
		$router[] = new Route('search', 'Search:search');
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
						'naskladnit' => 'placed',
						'odstranit' => 'remove',
						'detail' => 'detail',
						'predat' => 'sell',
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
