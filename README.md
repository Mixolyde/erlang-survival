# Survival in Erlang!
## Erlang implementation of an old Task Force game called Survival

The player is an explorer who has crashed on an alien planet, and must
cross a dangerous landscape to reach safe haven in a research station.

## Game rules/details/inspiration
 * Atari PC game and manual scan: http://www.atarimania.com/game-atari-400-800-xl-xe-survival_5201.html
 * Board Game http://boardgamegeek.com/boardgame/3042/survival-the-barbarian

## Completed
 * Rebar setup of application according to OTP style
 * Minimal FSM behavior for running the game
 * Some records and lists in the header
 * New weapon and player record creators
 * Eunit tests for current functions and start/stop of FSM
 * Basic map data structure and validity tests
 * Text map display with Player position
 * Legend display for direction choice map characters
 * Status display for player and game info
 * Simple interface client for playing from the shell
 * Win condition check after move
 * Checked Movement, done moving
 * Turn/Day Completion with starvation check
 * Started Combat mechanics and weapon selection
 
## TODO
 * Combat mechanics
 * Initial weapon loadout choosing
 * State transitions in/out of combat
 * Multi-player server and client
 * Event handler for tracking errors
 * Graphics
 * Advanced game modes
 * Options
 * Procedural map generation
 * cecho based client
 * ANSI colors for map, legend
 * Web client
 
## Roadmap
 0.1.0 Current work
 0.2.0 Basic game completely implemented with simple client
 0.3.0 Advanced game options implemented
 0.4.0 Advanced scenarios implemented
 1.0.0 Multiplayer with simple client
 1.2.0 Random map generation
 
## Getting Started
 1. Get rebar, build it and replace the rebar that's in the repo
 2. Command line build and test: `make clean compile eunit`
 3. In the Erlang shell: `cd("path/to/repo/directory/survival").` 
 3. `code:add_path("ebin").`
 4. `Game = simple_client:start().`
 5. `simple_client:choose_direction(Game, 4).`
 6. `simple_client:quit(Game).` 
 