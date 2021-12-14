% warzoneClient.erl - A Distributed Adventure Game Client

-module(warzoneClient).
-author('Nick Petrilli').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game client: ").


%--------
% Public
%--------

-export([start/0, start/1]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this game client process.
   io:fwrite("~sStarting Distributed Warzone Game Client (pid ~w) on node ~w.~n",[?id, self(), node()]),
   GameClientPid = spawn(fun clientLoop/0),
   io:fwrite("~sSpawned game client with pid ~w",[?id, GameClientPid]),
   % We want to publish this process in Erlang's local process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(gameClient),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(gameClient);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's local process registry.
   register(gameClient, GameClientPid),
   io:fwrite(", registered as ~w.~n",[gameClient]),
   %Send the client to the game server so the server can monitor clients
   io:fwrite("~sNotifying server on node ~w.~n",[?id, ServerNode]),
   {gameServer, ServerNode} ! {node(), registerNewClient, client},
   % Initialize server monitoring.
   gameClient ! {monitor, ServerNode},
   % -- Begin the play loop
   playLoop(ServerNode).


%---------------------------------
% Private, but accepting messages.
%---------------------------------
clientLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         clientLoop();

      {nodedown, Node} ->
         % This client monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
		 halt();

      {FromNode, _Any, Inventory} ->
		 io:fwrite("~sRecieved message [~p] from node ~w with new inventory ~w.~n", [?id, _Any, FromNode, Inventory]),
		 clientLoop();

      {FromNode, _Any}  ->
         io:fwrite("~sReceived message [~p] from node ~w.~n",[?id, _Any, FromNode]),
         clientLoop()
   end.


%---------
% Private
%---------

playLoop(ServerNode) ->
   % -- Get a line of input from the user.
   Line = io:get_line(io_lib:format("~s[play] Enter action or help -] ", [?id])),  % Line is returned as a string.
   Inventory = [compass, uav, sniper, armor, tomahawk],
   {ResultAtom, ResultText, Inventory} = processCommand(Line, ServerNode, Inventory),
   %
   % -- Update the display.
   io:fwrite("~s~w~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit) ->
      io:fwrite("~sThank you for playing.~n", [?id]);
   ?else ->
     playLoop(ServerNode)  % This is tail recursion, so it's really a jump to the top of playLoop.
   end. % if


processCommand(Line , ServerNode, Inventory) ->
   % Do some elementary parsing of the line in two parts:
   % 1. Remove the trailing newline charater.
   Command = lists:sublist(Line, length(Line)-1),  % (Because Line is a character list ending with a linefeed.)
   % 2. Break the line into two parts: before the space and after the space (if there's even a space)
   Verb = lists:takewhile( fun(Element) -> Element /= 32 end, Command),
   Noun = lists:dropwhile( fun(Element) -> Element /= 32 end, Command),
   %
   case Verb of
      "help"   -> {help,   helpText(), Inventory};
      "quit"   -> {quit,   "Quitting.", Inventory};
      "q"      -> {quit,   "Quitting.", Inventory};
      "nodes"  -> {nodes,  listNodes(), Inventory};
      "server" -> {server, server(ServerNode), Inventory};
      "go"     -> {go,     go(Noun, ServerNode), Inventory};
	  "pickup" -> {pickup, pickup(Noun, ServerNode, Inventory), Inventory}; % command is pickup [location] because each location has a different item
	  "use"    -> {use, useUAV(ServerNode, Inventory), Inventory};
	  "apply"  -> {applyarmor, applyArmor(ServerNode, Inventory), Inventory};
	  "shoot"  -> {shoot, shootEnemy(ServerNode, Inventory), Inventory};
	  "throw"  -> {throwlethal, throwLethal(ServerNode, Inventory), Inventory};
	  "stab"   -> {stab, stabEnemy(ServerNode, Inventory), Inventory};
      % -- Otherwise...
      _Else  -> {unknownCommand, "Silly human."}
   end.

helpText() ->
   io_lib:format("Commands: [help], [quit], [nodes], [server], [go <location>]", []).

listNodes() ->
   io_lib:format("This node: ~w~n", [node()]) ++   % No ?id here because it will be supplied when printed above.
   io_lib:format("~sOther nodes in our cluster: ~w", [?id, nodes()]).

server(ServerNode) ->
   KnownNode = lists:member(ServerNode, nodes()),
   if KnownNode ->
      io_lib:format("Talking to game server on node ~w, which is known to be in our cluster.", [ServerNode]);
   ?else ->
      io_lib:format("Talking to game server on node ~w, which is NOT known to be in our cluster, and that may be a problem.", [ServerNode])
   end. % if

go([_Space | Destination], ServerNode) ->
   DestAtom = list_to_atom(Destination),
   io:fwrite("~s[debug] Going to location [~w].~n", [?id, DestAtom]),
   {gameServer, ServerNode} ! {node(), goToLocation, DestAtom},
   ok;
go([], _ServerNode) ->
   io_lib:format("Where do you want to go?", []).
  
%Pickup item sends a message to the server to find out where the player is, then sends a message to that location that the player is picking up an item
%Each location has a different item the player can pick up
pickup([_Space | Destination], ServerNode, Inventory) ->
   DestAtom = list_to_atom(Destination),
   %Client only starts with a compass in inventory
   io:fwrite("~sPicking up item at location [~w].~n", [?id, DestAtom]),
   {gameServer, ServerNode} ! {node(), pickup, DestAtom, Inventory}.
   
pickup([], _ServerNode) ->
   io_lib:format("Give a location for where you want to pickup an item at.", []).
   
useUAV(ServerNode, Inventory) ->
	HasUAV = lists:member(uav, Inventory),
	if (HasUAV) ->
		%send message to server which will display where the enemies are
		{gameServer, ServerNode} ! {node(), useUAV};
	?else ->
		io:fwrite("You don't have a UAV to use.", [])
	end.

applyArmor(ServerNode, Inventory) ->
	HasArmor = lists:member(armor, Inventory),
	if (HasArmor) ->
		%send message to server in order to apply armor
		{gameServer, ServerNode} ! {node(), applyArmor};
	?else ->
		io:fwrite("You don't have any armor to apply.", [])
	end.

shootEnemy(ServerNode, Inventory) ->
	HasAk47 = lists:member(ak47, Inventory),
	HasM4 = lists:member(m4, Inventory),
	HasSniper = lists:member(sniper, Inventory),
	HasRPG = lists:member(rpg, Inventory),
	
	if (HasAk47 or HasM4 or HasSniper or HasRPG) ->
		%send message to server to eliminate an enemy
		{gameServer, ServerNode} ! {node(), shootEnemy};
	?else ->
		io:fwrite("You don't have any weapons to shoot.", [])
	end.
	
throwLethal(ServerNode, Inventory) ->
	HasGrenade = lists:member(grenade, Inventory),
	HasTomahawk = lists:member(tomahawk, Inventory),
	
	if (HasGrenade or HasTomahawk) ->
		%send message to server to eliminate an enemy with lethal equipment
		{gameServer, ServerNode} ! {node(), throwLethal};
	?else ->
		io:fwrite("You don't have any lethal equipment to throw.", [])
	end.

stabEnemy(ServerNode, Inventory) ->
	HasKnife = lists:member(knife, Inventory),
	if (HasKnife) ->
		{gameServer, ServerNode} ! {node(), stabEnemy};
	?else ->
		io:fwrite("You don't have a knife to use.", [])
	end.
	
		