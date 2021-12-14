% airport.erl - Distributed Adventure Game Location Airport

-module(airport).
-author('Nick Petrilli').
-define(else, true).  % -- This is to make the if statements (somewhat) readable.
-define(id, "-- airport: ").


%--------
% Public
%--------

-export([start/0, start/1, locationLoop/0]).

start() ->
   io:fwrite("You must supply a game sever node.~n", []).

start(ServerNode) ->
   % -- Spawn this location process.
   io:fwrite("~sStarting Location 2 (pid ~w) on node ~w.~n",[?id, self(), node()]),
   LocPid = spawn(airport, locationLoop, []),
   io:fwrite("~sSpawned location with pid ~w",[?id, LocPid]),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(airport),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(airport);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(airport, LocPid),
   io:fwrite(", registered as ~w.~n",[airport]),
   % Send ourselves to the gameServer.
   io:fwrite("~sNotifying server on node ~w.~n",[?id, ServerNode]),
   {gameServer, ServerNode} ! {node(), registerNewLocation, airport},
   % Initialize server monitoring.
   airport ! {monitor, ServerNode},
   ok.


%---------------------------------
% Private, but accepting messages.
%---------------------------------

locationLoop() ->
   receive
      {monitor, ServerNode} ->
         io:fwrite("~sMonitoring game server on node ~w.~n",[?id, ServerNode]),
         monitor_node(ServerNode, true),
         locationLoop();

      {nodedown, Node} ->
         % This location monitors the server node.
         % The server node has gone down. Notify the admin console...
         io:fwrite("~sServer node ~w has left our cluster and is no longer reachable. Shutting down.~n",[?id, Node]),
         % ...  and shut down.
         exit(normal);

      {_FromNode, enter, GameClientNode}  ->
         io:fwrite("~sA gameClient on ~w is entering airport.~n",[?id, GameClientNode]),
         {gameClient, GameClientNode} ! {node(), describe()},
         locationLoop();

      {_FromNode, pickup, GameClientNode, Inventory}  ->
         io:fwrite("~sA gameClient on ~w is picking up a flashbang.~n",[?id, GameClientNode]),
		 NewInventory = pickupItem(Inventory),
         {gameClient, GameClientNode} ! {node(), pickupDescription(), NewInventory},
         locationLoop();
		 
      {FromNode, _Any}  ->
         io:fwrite("~sReceived request [~p] from node ~w.~n",[?id, _Any, FromNode]),
         locationLoop()
   end.


%--------
% Private
%--------
describe() ->
   io_lib:format("(9) You have come across a packed airport where the rest of the civilians are trying to exit the warzone.", []).
   
pickupItem(Inventory) ->
	Inventory ++ flashbang.

pickupDescription() ->
	io_lib:format("You have picked up a flashbang. Throw it at enemies to temporarily blind them.", []).