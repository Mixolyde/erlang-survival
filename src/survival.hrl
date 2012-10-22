%%% header includes for survival modules

-define(MAX_WOUNDS, 6).

-define(MAX_WEIGHT, 8).

-define(WEAPON_LIST, [hands, spear, laser_carbine, auto_pistol, 
					  lightsword, rifle, grenade_launcher, energy_blaster]).

-define(ANIMAL_LIST, [maizar, corydal, ceekal, shenthe, teklek, genebrach, jalait, zeget]).

-record(player,  {ws = ?MAX_WOUNDS, weapons = [], 
                  pname = "TestPlayer", loc = {1,1}, maxweight = ?MAX_WEIGHT}).

-record(monster, {atom, mname}).

-record(weapon,  {atom, displayname, weight, melee, range, rounds, maxrounds}).

-record(smap, {size, terrain, stationloc, starts}).
