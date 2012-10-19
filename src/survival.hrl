%%% header includes for survival modules

-define(MAX_WOUNDS, 6).

-define(WEAPON_LIST, [hands, spear, laser_carbine, auto_pistol, 
					  lightsword, rifle, grenade_launcher, energy_blaster]).

-define(ANIMAL_LIST, [maizar, corydal, ceekal, shenthe, teklek, genebrach, jalait, zeget]).

-define(STARTS, [{1, 3}, {4, 1}, {8, 1}, {13, 1}, {17, 1}, {19, 4}]).

-record(player,  {ws = ?MAX_WOUNDS, weapons = [], pname = "TestPlayer", loc = {1,1}}).

-record(monster, {mname}).

-record(weapon,  {displayname, weight, melee, range, rounds, maxrounds}).


