%
% Call of Duty Warzone Game
% warzone.erl
%


-module(warzone).
-author('Nick').
-define(else, true).  

-type direction() :: north | south | east | west.

%--------
% Public
%--------

-export([start/0]).

start() ->
   % -- Spawn the server process.
   io:fwrite("Starting Warzone server.~n",[]),
   io:fwrite("Roam the map to pick up weapons and eliminate enemies.~n", []),
   ServerPid = spawn(fun serverLoop/0),
   % -- Display the initial location description by moving north from -1.
   {_NewLocale, Description} = processCommand(-1, "north", ServerPid, []),
   io:fwrite("~n~s~n~n", [Description]),

   % -- Kick off the game loop with the ServerPID, location = 0, turn = 1
   % Start inventory with items necessary for game because picking them up doesn't actually add them
   gameLoop(ServerPid, 0, 1, [compass, sniper, uav, grenade, armor]).


%---------
% Private
%---------

gameLoop(ServerPid, CurrentLocale, Turn, InventoryList) ->
   % -- Show the map and get input from the player.
   io:fwrite("~s", [showMap(CurrentLocale)]),
   io:fwrite("Turn ~w ] ", [Turn]),
   {ok, Input} = io:fread("Enter a command (or help) -] ", "~s"),  % Input gets returned as a list from io:fread.
   [Command | _] = Input,   % (Because Input is a list.)
   %
   % -- Process the player's input/command into a NewLocale and Description.
   {NewLocale, Description} = processCommand(CurrentLocale, Command, ServerPid, InventoryList),
   
   %
   % -- Update the display.
   io:fwrite("~n~s~n~n", [Description]),
   %
   % -- Quit or Recurse/Loop.
   if (NewLocale < 0) ->
     io:fwrite("Goodbye.~n",[]);
   ?else ->
		%ends the game after 50 turns
		if (Turn > 49) ->
	      io:fwrite("Thanks for playing.", []);
	   ?else ->
		   gameLoop(ServerPid, NewLocale, Turn+1, InventoryList)
		end
   end. % if                                                            


processCommand(CurrentLocale, Command, ServerPid, Inventory) ->
   case Command of
      % -- Compass directions - Get the new location from the server.
      "north" -> move(ServerPid, {CurrentLocale, north});
      "n"     -> move(ServerPid, {CurrentLocale, north});
      "south" -> move(ServerPid, {CurrentLocale, south});
      "s"     -> move(ServerPid, {CurrentLocale, south});
      "east"  -> move(ServerPid, {CurrentLocale, east});
      "e"     -> move(ServerPid, {CurrentLocale, east});
      "west"  -> move(ServerPid, {CurrentLocale, west});
      "w"     -> move(ServerPid, {CurrentLocale, west});
      % -- Other commands - Handle non-movement commands.
      "quit"      -> {-1, "Thank you for playing."};
      "q"         -> {-1, "Thank you for playing."};
      "look"      -> {CurrentLocale, locationDesc(CurrentLocale)};
      "l"         -> {CurrentLocale, locationDesc(CurrentLocale)};
      "help"      -> {CurrentLocale, helpText()};
	  "h"         -> {CurrentLocale, helpText()};
	  "pickup"    -> {CurrentLocale, addToInventory(CurrentLocale, Inventory)};
	  "p"         -> {CurrentLocale, addToInventory(CurrentLocale, Inventory)};
	  "apply"     -> {CurrentLocale, applyArmor(Inventory)};
	  "a"         -> {CurrentLocale, applyArmor(Inventory)};
 	  "shoot"     -> {CurrentLocale, shootEnemy(Inventory)};
	  "throw"     -> {CurrentLocale, throwLethal(Inventory)};
	  "t"         -> {CurrentLocale, throwLethal(Inventory)};
	  "use"       -> {CurrentLocale, useUAV(Inventory)};
	  "u"         -> {CurrentLocale, useUAV(Inventory)};
      "map"       -> {CurrentLocale, showMap(CurrentLocale)};
      "show map"  -> {CurrentLocale, showMap(CurrentLocale)};
      "inventory" -> {CurrentLocale, showInventory(Inventory)};
      "i"         -> {CurrentLocale, showInventory(Inventory)};
      % -- Otherwise...
      _Else   -> {CurrentLocale, "I do not understand."}  % Starting _Else with "_" prevents the "unused" warning.
   end.


helpText() -> io_lib:format("You can enter compass directions: [n] or [north], [s] or [south], [e] or [east], ", []) ++
              io_lib:format("[w] or [west], as well as [quit], [look], [pickup], [shoot], [apply], and other commands.", []).


% Send the move message (a tuple) to the server.
-spec move(pid(), {integer(), direction()}) -> integer(). %  This is not enforced at runtime. It's for Dializer and Typer.
move(ServerPid, MoveTuple) ->
   ServerPid ! {self(), MoveTuple},
   receive
      {ServerPid, Response} -> Response  % This waits for a response from ToPid.
   end.


% This is the process spawned at the start.
serverLoop() ->
   receive
      {FromPid, {CurrentLocale, Direction}} ->
         NewLocaleNumber = mapper(CurrentLocale, Direction),
         if NewLocaleNumber > -1 ->
            % Valid move.
            NewLocaleDesciption = locationDesc(NewLocaleNumber),
            NewLocaleItems      = locationItems(NewLocaleNumber),
            FromPid ! {self(), {NewLocaleNumber, io_lib:format("~s You see a ~w in front of you.", [NewLocaleDesciption, NewLocaleItems])}},
            serverLoop();
         ?else ->
            % Invalid move.
            FromPid ! {self(), {CurrentLocale, "You cannot go that way."}},
            serverLoop()
         end;

      {FromPid, _} ->
         FromPid ! {self(), "Internal error: You are lost."},
         serverLoop()
   end.


% Mapper. Double-chcek with showMap().
mapper(-1, north) -> 0;
mapper(0, west) -> 1;
mapper(0, east) -> 2;
mapper(0, south) -> 4;
mapper(1, south) -> 3;
mapper(1, north) -> 9;
mapper(1, east) -> 0;
mapper(2, north) -> 6;
mapper(2, west) -> 0;
mapper(2, south) -> 5;
mapper(3, north) -> 1;
mapper(3, east) -> 4;
mapper(4, north) -> 0;
mapper(4, west) -> 3;
mapper(5, north) -> 2;
mapper(6, south) -> 2;
mapper(6, west) -> 7;
mapper(7, east) -> 6;
mapper(7, west) -> 8;
mapper(8, east) -> 7;
mapper(8, south) -> 9;
mapper(9, north) -> 8;
mapper(9, south) -> 1;
mapper(9, west) -> 10;
mapper(10, east) -> 9;
mapper(_, _) -> -1.


% Show map. Double-check with mapper().
showMap(CurrentLocale) ->
   io_lib:format(".............................. ~n", []) ++
   io_lib:format("...... ~s ---- ~s ---- ~s........ ~n", [dispLocale(CurrentLocale, 8), dispLocale(CurrentLocale, 7), dispLocale(CurrentLocale, 6)]) ++
   io_lib:format(".......|.............|........ ~n", []) ++
   io_lib:format("~s --- ~s.............|........ ~n", [dispLocale(CurrentLocale, 10), dispLocale(CurrentLocale, 9)]) ++
   io_lib:format(".......|.............|........ ~n", []) ++
   io_lib:format("...... ~s ---- ~s ---- ~s........~n", [dispLocale(CurrentLocale, 1), dispLocale(CurrentLocale, 0), dispLocale(CurrentLocale, 2)]) ++
   io_lib:format(".......|......|......|........ ~n", []) ++
   io_lib:format(".......|......|......|........ ~n", []) ++
   io_lib:format("...... ~s ---- ~s .... ~s........ ~n", [dispLocale(CurrentLocale, 3), dispLocale(CurrentLocale, 4), dispLocale(CurrentLocale, 5)]) ++
   io_lib:format(".............................. ~n", []).  


dispLocale(CurrentLocale, MapLoc) ->
   if CurrentLocale == MapLoc ->
      "@";
   ?else ->
      integer_to_list(MapLoc)  % Remember, strings are lists of ASCII/Unicode values in Erlang.
   end.


% Location Descriptions
% These location descriptions DO NOT end with ~n newlines. The newline is taken care of in the display code.
locationDesc(0)    -> io_lib:format("0. STADIUM~nYou find yourself inside a large football stadium in the center of the map.~nPick up the uav for aerial coverage of where the enemies are.", []);
locationDesc(1)    -> io_lib:format("1. SUPERSTORE~nYou have come to a large supermarket...", []);
locationDesc(2)    -> io_lib:format("2. LUMBERYARD~nYou find yourself looking at a huge lumberyard with pallets of wood covering the ground.", []);
locationDesc(3)    -> io_lib:format("3. TRAIN STATION~nYou have entered a busy train station. All tracks got blown up besides paths to the north and east.~nYou see an enemy advancing on the tracks. Take him out!", []);
locationDesc(4)    -> io_lib:format("4. HOSPITAL~nYou find yourself in a hospital where soldiers are being treated.", []);
locationDesc(5)    -> io_lib:format("5. FARMLAND~nYou have come across acres and acres of farmland. None of the animals survived, and corn is the only crop still growing.", []);
locationDesc(6)    -> io_lib:format("6. Quarry~nYou find yourself looking at a dried up quarry surrounded by a thick layer of limestone.", []);
locationDesc(7)    -> io_lib:format("7. MILITARY BASE~nYou have come to the entrance of a military base. Tanks and thousands of boxes of ammunition surround you.~nThere is an enemy climbing into the gunman spot of the tank. Take the shot!", []);
locationDesc(8)    -> io_lib:format("8. DAM~nYou find yourself overlooking a 500 foot drop on the top of a dam. ", []);
locationDesc(9)    -> io_lib:format("9. AIRPORT~nYou have come across a packed airport where the rest of the civilians are trying to exit the warzone.", []);
locationDesc(10)   -> io_lib:format("10. STORAGE TOWN~nYou find yourself in an outdoor storage facility with garages full of miscellaneous items.~nYou see that an enemy sniper has you scoped. Shoot him!", []);
locationDesc(Loc)  -> io_lib:format("Oops! Unknown locale: ~w.", [Loc]).


% Location Items
locationItems(0)    -> [uav];
locationItems(1)    -> [armor];
locationItems(2)    -> [ak47];
locationItems(3)    -> [sniper];
locationItems(4)    -> [m4];
locationItems(5)    -> [knife];
locationItems(6)    -> [tomahawk];
locationItems(7)    -> [ammunition];
locationItems(8)    -> [grenade];
locationItems(9)    -> [];
locationItems(10)    -> [rpg];
locationItems(_Loc) -> [].  

% Add items to inventory
addToInventory(CurrentLocale, Inventory) -> 
	NewItem = locationItems(CurrentLocale),
	NewInventory = Inventory ++ NewItem,
	io_lib:format("Your new inventory is ~w", [NewInventory]).

% Displays the player's inventory
showInventory([])            -> io_lib:format("You are not carrying anything of use.", []);
showInventory(InventoryList) -> io_lib:format("You are carrying ~w.", [InventoryList]).

% Shoot enemy using weapon in inventory
shootEnemy(Inventory) ->
	Bool1 = lists:member(ak47, Inventory),
	Bool2 = lists:member(m4, Inventory),
	Bool3 = lists:member(sniper, Inventory),
	Bool4 = lists:member(rpg, Inventory),
	if (Bool1 or Bool2 or Bool3 or Bool4) ->
		io_lib:format("You have successfully taken out an enemy.", []);
	?else ->
		io_lib:format("You can't shoot an enemy without a weapon!", [])
	end.

throwLethal(Inventory) ->
	Bool1 = lists:member(tomahawk, Inventory),
	Bool2 = lists:member(grenade, Inventory),
	if (Bool1 or Bool2) ->
		io:fwrite("You killed an enemy with your lethal equipment.", []);
	?else ->
		io:fwrite("You don't have a lethal to throw.", [])
	end.
	
applyArmor(Inventory) ->
	Bool = lists:member(armor, Inventory),
	if (Bool) ->
		io:fwrite("You have applied armor.", []);
	?else ->
		io:fwrite("You don't have any armor to apply.", [])
	end.

useUAV(Inventory) ->
	Bool = lists:member(uav, Inventory),
	if (Bool) ->
		io:fwrite("The UAV has spotted three enemies on the map. There is one at train station, one at military base and one at storage town.~nFind weapons and eliminate them!", []);
	?else ->
		io:fwrite("You don't have a UAV to use.", [])
	end.
		