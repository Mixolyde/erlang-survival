# Survival in Erlang!
## Erlang implementation of an old Task Force game called Survival

The player is an explorer who has crashed on an alien planet, and must
cross a dangerous landscape to reach safe haven in a research station.

## Game details/inspiration
 * Atari PC game: http://www.atarimania.com/game-atari-400-800-xl-xe-survival_5201.html
 * Board Game http://boardgamegeek.com/boardgame/3042/survival-the-barbarian

## Completed
 * Rebar setup of application according to OTP style
 * Basic FSM for running the game
 * Some records and lists in the header
 * New weapon and player record creators
 * Eunit tests for current functions and start/stop of FSM
 * Basic map data structure and map line length tests
 * Text map display with Player position
 * Legend display for direction choice map characters
 * Status display for player and game info
 
## TODO
 * Movement
 * Win condition test
 * FSM states
 * FSM transitions
 * Single-player client
 * Combat mechanics
 * Multi-player server and client
 * Event handler for tracking errors
 * Graphics
 * Advanced game modes
 * Options
 * Procedural map generation
 * cecho based client
 * ANSI colors for map, legend
 * Web client
 * Pretty much everything
 
## Getting Started
 1. Get rebar, build it and replace the rebar that's in the repo
 2. build and test: `make clean compile eunit` 