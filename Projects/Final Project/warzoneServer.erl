% gameServer.erl - A Distributed Adventure Game Server

-module(warzoneServer).
-author('Nick Petrilli').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- game server: ").


%--------
% Public
%--------

-export([start/0]).

start() ->
   % -- Spawn the server process.
   io:fwrite("~sStarting Distributed Warzone Server (pid ~w) on node ~w.~n",[?id, self(), node()]),
   GameServerPid = spawn(fun serverLoop/0),
   io:fwrite("~sSpawned game server with pid ~w",[?id, GameServerPid]),
   % We want to publish this process in Erlang's local process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(gameServer),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(gameServer);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's local process registry.
   register(gameServer, GameServerPid),
   io:fwrite(", registered as ~w.~n",[gameServer]),
   % -- Erase the existing local process dictionary.
   erase(),
   % -- Begin the admin loop
   adminLoop().


%---------------------------------
% Private, but accepting messages.
%---------------------------------
serverLoop() ->
   receive

      % Local requests

      {local, listClientLocations} ->
         io:fwrite("~sThis server knows about the following (client) locations: ~w~n", [?id, get()]),
         serverLoop();

      {local, listNodes} ->
         io:fwrite("~sThis node: ~w~n", [?id, node()]),
         io:fwrite("~sOther nodes in our cluster: ~w~n", [?id, nodes()]),
         serverLoop();

      {local, endProcess} ->
         io:fwrite("~sThis server processes is ending. Good bye.~n", [?id]),
         exit(normal);


      % Remote requests

      {FromNode, registerNewLocation, LocId}  ->
         io:fwrite("~sReceived registerNewLocation message from node ~w for ~w.~n",[?id, FromNode, LocId]),
         % Record this clientLocation and node in our process dictionary.
         io:fwrite("~sPutting {~w,~w} in the local process dictionary.~n", [?id, LocId, FromNode]),
         put(LocId, FromNode),
         io:fwrite("~sWe are now monitoring ~w.~n", [?id, FromNode]),
         monitor_node(FromNode, true),
         serverLoop();
		 
	%Monitor the client in order to keep track of their movements to different locations 
	  {FromNode, registerNewClient, LocId}  ->
         io:fwrite("~sReceived registerNewClient message from node ~w for ~w.~n",[?id, FromNode, LocId]),
         % Record this clientLocation and node in our process dictionary.
         io:fwrite("~sPutting {~w,~w} in the local process dictionary.~n", [?id, LocId, FromNode]),
         put(LocId, FromNode),
         io:fwrite("~sWe are now monitoring ~w.~n", [?id, FromNode]),
         monitor_node(FromNode, true),
         serverLoop();

      {nodedown, Node} ->
         % This server monitors location nodes.
         % A location node has gone down. Notify the admin console...
         io:fwrite("~sNode ~w has left our cluster and is no longer reachable.~n",[?id, Node]),
         % ...  and remove it from our process dictionary.
         LocIdList = get_keys(Node),      % We know the node but we need the LocId to
         erase(hd(LocIdList)),            % erase it from our process dictionary.
         serverLoop();

      {FromNode, goToLocation, ClientLocId}  ->
         io:fwrite("~sReceived goToLocation message from node ~w for location [~w].~n",[?id, FromNode, ClientLocId]),
         % Look up the ClientLocId in our local process dictionary
         io:fwrite("~sGetting node for location [~w] from the local process dictionary.~n", [?id, ClientLocId]),
         ClientLocNode = get(ClientLocId),
         if ClientLocNode == undefined ->
            io:fwrite("~sNode not found in the local process dictionary.~n", [?id]),
            % Use only FromPid here because we don't know the registered name of the process (because there is none).
            {gameClient, FromNode} ! {node(), "You cannot go that way."};
         ?else ->
            io:fwrite("~sFound node in the local process dictionary: [~w].~n", [?id, ClientLocNode]),
            {gameClient, FromNode} ! {node(), "[debug] You CAN go that way."},
            % Tell the ClientLocId on ClientLocNode that a gameClient on FromNode is entering.
            {ClientLocId, ClientLocNode} ! {node(), enter, FromNode}
			
         end, % if
         serverLoop();
	  {FromNode, pickup, ClientLocId, Inventory} ->
		 io:fwrite("~sRecieved pickup message from node ~w. ~n", [?id, FromNode]),
		 ClientLocNode = get(ClientLocId),
		 io:fwrite("~sGetting node for location [~w] from the local process dictionary.~n", [?id, ClientLocId]),
		 if ClientLocNode == undefined ->
		    io:fwrite("~sNode not found in the local process dictionary.~n", [?id]),
			{gameClient, FromNode} ! {node(), "You cannot pickup an item at that location"};
		?else ->
		  io:fwrite("~sFound node in the local process dictionary: [~w].~n", [?id, ClientLocNode]),
		  %send message to location where the client wants to pick an item up at
		  {ClientLocId, ClientLocNode} ! {node(), pickup, FromNode, Inventory}
		  
		 end, %if
		 serverLoop();
		 
	  {FromNode, useUAV} ->
		 io:fwrite("~sRecieved useUAV message from node ~w. ~n", [?id, FromNode]),
		 {gameClient, FromNode} ! {node(), "The UAV has spotted three enemies on the map. There is one at train station, one at military base and one at storage town.~nFind weapons and eliminate them!"},
		 serverLoop();
		 
	  {FromNode, applyArmor} ->
		 io:fwrite("~sRecieved applyArmor message from node ~w. ~n", [?id, FromNode]),
		 {gameClient, FromNode} ! {node(), "You have applied armor, go out and get in the action!"},
		 serverLoop();
		
	  {FromNode, shootEnemy} ->
		 io:fwrite("~sRecieved shootEnemy message from node ~w. ~n", [?id, FromNode]),
		 {gameClient, FromNode} ! {node(), "You have successfully eliminated a player. Find the others!"},
		 serverLoop();
		 
	  {FromNode, throwLethal} ->
		 io:fwrite("~sRecieved throwLethal message from node ~w. ~n", [?id, FromNode]),
		 {gameClient, FromNode} ! {node(), "You hit and killed an enemy with your lethal equipment."},
		 serverLoop();
		 
	  {FromNode, stabEnemy} ->
		 io:fwrite("~sRecieved stabEnemy message from node ~w. ~n", [?id, FromNode]),
		 {gameClient, FromNode} ! {node(), "You have stabbed an enemy with your knife."},
		 serverLoop();
		 
%      {FromNode, _Any}  ->
%        io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
%         serverLoop();

      {FromNode, _Any1, _Any2}  ->
         io:fwrite("~sReceived unknown request [~p, ~p] from node ~w.~n",[?id, _Any1, _Any2, FromNode]),
         serverLoop()
   end.


%---------
% Private
%---------

adminLoop() ->
   % -- Get input from the user.  We need the formatted prompt inside the fread to
   %    prevent line breaks and keep the text together when receiving messages.
   {ok, Input} = io:fread(io_lib:format("~s[admin] Enter command or help -] ", [?id]), "~s"),  % Input is returned as a list.
   Command = hd(Input),  % (Because Input is a list.)
   %
   % -- Process the input/command
   {ResultAtom, ResultText} = processCommand(Command),
   %
   % -- Update the display.
   io:fwrite("~s~s~n", [?id, ResultText]),
   %
   % -- Quit or Recurse/Loop.
   if (ResultAtom == quit) ->
     gameServer ! {local, endProcess},
	 halt();
   ?else ->
     adminLoop()  % This is tail recursion, so it's really a jump to the top of adminLoop.
   end. % if


processCommand(Command) ->
   case Command of
      "help"      -> {help, helpText()};
      "locations" -> {procList, listLocations()};
      "nodes"     -> {procList, listNodes()};
      "quit"      -> {quit, "Sending [quit] request..."};
      "q"         -> {quit, "Sending [quit] request..."};
      % -- Otherwise...
      _Else  -> {unknownCommand, "I'm sorry Dave, I do not understand."}  % Starting _Else with "_" prevents the "unused" warning.
   end.

helpText() ->
   io_lib:format("Commands: [help], [locations], [nodes], [quit]", []).

listLocations() ->
   % Send a message to the local gameServer telling it to list the
   % client locations it knows about. Since the gameServer is running
   % as a different process (becasue we spawned) its process dictionary
   % is seperate from ours and we cannot access it here. That's why
   % we're sending a message to it with this request.
   gameServer ! {local, listClientLocations},
   ok.

listNodes() ->
   % Send a message to the local gameServer telling it to list the
   % nodes it knows about.
   gameServer ! {local, listNodes},
   ok.