---
created:
  - " 29-06-2025 20:21"
aliases:
  - "Project:"
tags:
  - Project/
---

# Project -> Tic Tac Toe Game Two Ways
___
## Project Description
- This project is about learning higher level concepts in [[Haskell]], namely [[IO]]. 
- This is accomplished by implementing the game Tic Tac Toe as a cli, and as a GUI based application. 
---
## Project information
Created::  28-06-2025 20:21
Deadline:: 06-08-2025
Hibernating:: No
Completion date expected:: 28-07-2025
Completed:: No
Type:: programming
Tags:: #gamedev #haskell #functionalProgramming #recreationalProgramming
___
## Objective

1. Ideal project result
	1. Both versions should include full game logic, and a replay/quit option, as well as a running scoreboard. 
2. Acceptable result
	1. Full game logic, replay functionality.
## Expectations
1. Helpful to the project
	1. Learning Cabal
	2. Writing idiomatic code
2. Roadblocks
	1. Gloss graphics are challenging
3. Naivety
	1. Proper drawing of graphics with Gloss
4. Insights
	1. --
___
## Screenshots
![[Pasted image 20250701003837.png]]
___

## Tasks 
- Refactor Tasks
	- [x] Refactor `xWin` and `oWin` function to use the (`any all`) pattern
	- [x] Investigate possible redundancy in `positionToCell` function
	- [x] Refactor `isDraw`, remove case expression. 
- [x] Comment the codebase.
	- [x] GUI version commented
	- [x] cli version commented 
- [ ] UI Improvements
	- [ ] Fix issues with text placement when displaying game over state. 
	- [ ] Add close game button
- [ ] Add more game functionality

## Resources 
- Haskell Packages
	- GUI Version
		- Data.Maybe 
		- Graphics.Gloss
		- Graphics.Gloss.Interface.Pure.Game
		
	- Cli Version
		- Data.Maybe 
		- Text.Read
- Cabal
## Project Logs 
- 2025-06-30
	- First repo is live, and have the game running on two [[Linux]] machines.
	- Game has basic functionality, and is suitable for playing one round of tic tac toe. 
	- Easy next step is cleaning up the cli version. 

- 2025-07-01
	- Updated documentation for GUI version of project, and completed a refactoring task
	- added new 
	- 01-07-2025 11:42
		- did not find redundancy in `positionToCell` function. 
