%
% tttServer.erl
%
-module(tttServer).
-author('Nick Petrilli').

-define(else, true).
-define(id, "-- server: ").


%
% Public
%
-export([start/0]).

start() ->
   io:fwrite("~sTTT server started on node ~w (pid ~w) ", [?id, node(), self()]),
   ServerPid = spawn(fun serverLoop/0),
   % We want to publish this process in Erlang's process registry.
   % Before we do that, we need to un-register it if it's already been registered.
   SomePlace = whereis(tttServer),
   if (SomePlace /= undefined) ->  % "not undefined" = "is defined"  (This is horrible, I know.)
      unregister(tttServer);
   ?else ->
      % The proccess was NOT already published/registered, so we don't really want to do anything here.
      true    % This is dumb, but I couldn't think of a better "no-op".
   end,
   % Publish this process in Erlang's process registry.
   register(tttServer, ServerPid),
   io:fwrite("with pid ~w registered as ~w.~n", [ServerPid, tttServer]).


%
% Private, but accepting messages sent to serverLoop because of the way it was spawned.
%
serverLoop() -> receive
                   {FromNode, start_game} ->
                      io:fwrite("~sReceived [start_game] request from node ~w.~n",[?id, FromNode]),
                      io:fwrite("~sSending [player_turn] response to node ~w.~n",[?id, FromNode]),
                      InitialBoard = [0,0,0, 0,0,0, 0,0,0],
                      {tttClient, FromNode} ! {node(), player_turn, InitialBoard},
                      serverLoop();

                   {FromNode, process_player_turn, Board, PlayerPos} ->
                      io:fwrite("~sReceived [process_player_turn] request from node ~w with player move ~w.~n",[?id, FromNode, PlayerPos]),
                      NewBoard = processPlayerMove(PlayerPos, Board),
					  drawBoard(NewBoard),
					  %test for player win here, if they win then terminate game, else send message for next computer turn and recall server loop
					  case checkForWin(player, NewBoard) of true -> playerWin();
														   false -> ThereIsATie = thereIsATie(NewBoard),
																	if ThereIsATie ->
																		nobodyWins();
																	?else ->
																		{tttClient, FromNode} ! {node(), request_computer_turn, NewBoard},
																		serverLoop()
																	end % if
						end; % case

                   {FromNode, computer_turn, Board} ->
                      io:fwrite("~sReceived [computer_turn] request from node ~w~n",[?id, FromNode]),
					  NewBoard = turn(computer, Board),
					  drawBoard(NewBoard),
					  
					  {tttClient, FromNode} ! {node(), player_turn, NewBoard},
                      serverLoop();

                      
                   {FromNode, _Any} ->
                      io:fwrite("~sReceived unknown request [~p] from node ~w.~n",[?id, _Any, FromNode]),
                      serverLoop()
                end.


%
% Private (not even accepting messages)
%
processPlayerMove(Position, Board) ->
   Target = lists:nth(Position, Board),
   if(Target == 0) ->
      io:fwrite("~sPlacing an X into position ~w.~n", [?id, Position]),
      UpdatedBoard = replaceInList(1, Position, Board),
      UpdatedBoard;
   ?else ->
      io:fwrite("~sCannot place an X into position ~w.~n", [?id, Position]),
	  Board
   end. % if
   

replaceInList(Value, Position, List) ->
   {Part1, Part2} = lists:split(Position-1, List),     % Break the list in two just before the specified Position.
   [_ | Tail] = Part2,                                 % Separate Part2 into Head and Tail, discarding the Head.
   Part1 ++ [Value] ++ Tail.                           % CONS together the result: Part1 ++ the new Value ++ the Tail from Part2.
											
turn(computer, Board) -> 
	drawBoard(Board),
	case checkForWin(computer, Board) of true  -> computerWin();
									   false -> ThereIsATie = thereIsATie(Board),
												if ThereIsATie ->
													nobodyWins();
												?else ->
													NewBoard = makeMove(Board),
													NewBoard
												end % if
	end. % case

drawBoard(Board) ->
	io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board, 1), getDisplay(Board, 2), getDisplay(Board, 3)]),
	io:fwrite("---+---+---~n", []),
	io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board, 4), getDisplay(Board, 5), getDisplay(Board, 6)]),
	io:fwrite("---+---+---~n", []),
	io:fwrite(" ~s | ~s | ~s ~n", [getDisplay(Board, 7), getDisplay(Board, 8), getDisplay(Board, 9)]).
	
getDisplay(Board, Position) ->
	case lists:nth(Position, Board) of
		-1 -> ["O"];
		0  -> [" "];
		1  -> ["X"]
	end. % case

thereIsATie(Board) ->
	not lists:member(0, Board).
	
checkForWin(player, Board) ->
	doTheyAddUpTo(3, Board);
	
checkForWin(computer, Board) ->
	doTheyAddUpTo(-3, Board).
	
doTheyAddUpTo(TargetSum, Board) ->
	Sums = sumBoard(Board),
	%Check to see if any of the winning possibilities add up to TargetSum
	lists:member(TargetSum, Sums).
	
sumBoard(Board) ->
	TopRow    = lists:nth(1, Board) + lists:nth(2, Board) + lists:nth(3, Board),
	MiddleRow = lists:nth(4, Board) + lists:nth(5, Board) + lists:nth(6, Board),
	BottomRow = lists:nth(7, Board) + lists:nth(8, Board) + lists:nth(9, Board),
	
	LeftColumn   = lists:nth(1, Board) + lists:nth(4, Board) + lists:nth(7, Board),
	CenterColumn = lists:nth(2, Board) + lists:nth(5, Board) + lists:nth(8, Board),
	RightColumn  = lists:nth(3, Board) + lists:nth(6, Board) + lists:nth(9, Board),
	
	LeftRightDiagonal = lists:nth(1, Board) + lists:nth(5, Board) + lists:nth(9, Board),
	RightLeftDiagonal  = lists:nth(3, Board) + lists:nth(5, Board) + lists:nth(7, Board),
	
	[TopRow, MiddleRow, BottomRow, LeftColumn, CenterColumn, RightColumn, LeftRightDiagonal, RightLeftDiagonal].
	
getMove(Board) ->
	%need to change this because it is on tttClient
	{ok, PlayerMove} = io:fread("Where do you want to move [1-9]? ", "~d"),
	[Position | _] = PlayerMove,
	checkPlayerMove(Position, Board).
	
checkPlayerMove(Position, Board) ->
	if (Position >= 1 and (Position =< 9)) ->
		processPlayerMove(Position, Board);
	?else ->
		io:fwrite("That is not a valid move: ~w.~n", [Position]),
		getMove(Board)
	end. % if

makeMove(Board) -> 
	io:fwrite("Calculating computer move...", []),
	ComputerMove = computeMove(Board),
	io:fwrite("Placing an O into position ~w~n", [ComputerMove]),
	UpdatedBoard = replaceInList(-1, ComputerMove, Board),
	UpdatedBoard.
	
computeMove(Board) ->
	WinningMove = getWinningMove(Board),
	if (WinningMove > 0) ->
		WinningMove;
	?else ->
		BlockingMove = getBlockingMove(Board),
		if (BlockingMove) ->
			BlockingMove;
		?else ->
			findFirst(0, Board)
		end % if
	end. % if
	
getWinningMove(Board) ->
	ThereIsAWinningMove = doTheyAddUpTo(-2, Board),
	% A row/column/diagonal summing to -2 means there is a winning move
	if ThereIsAWinningMove ->
		io:fwrite("~nThere is a winning computer move.~n", []),
		findMove(-2, Board);
	?else ->
		0
	end.

getBlockingMove(Board) ->
	ThereIsABlockingMove = doTheyAddUpTo(2, Board),
	%Summing to 2 means there is a blocking move
	if ThereIsABlockingMove ->
		io:fwrite("~nThere is a blocking computer move. ~n", []),
		findMove(2, Board);
	?else ->
		0
	end. % if

findMove(TargetSum, Board) -> 
	Sums = sumBoard(Board),
	%Note the first Seq that adds up to TargetSum
	Seq = findFirst(TargetSum, Sums),
	io:fwrite("Target move in sequence ~w, ", [Seq]),

	case Seq of
	1 -> io:fwrite("Top Row.~n", []),
		 X = lists:nth(1, Board),
		 Y = lists:nth(2, Board),
		 if X == 0 ->
			1;
		?else ->
			if Y == 0 ->
				2;
			?else ->
				3
			end % if
		end; % if
		
	2 -> io:fwrite("Middle Row.~n", []),
		 X = lists:nth(4, Board),
		 Y = lists:nth(5, Board),
		 if X == 0 ->
			4;
		?else ->
			if Y == 0 ->
				5;
			?else ->
				6
			end % if
		end; % if

	3 -> io:fwrite("Bottom Row.~n", []),
		 X = lists:nth(7, Board),
		 Y = lists:nth(8, Board),
		 if X == 0 ->
			7;
		?else ->
			if Y == 0 ->
				8;
			?else ->
				9
			end % if
		end; % if		
		
	4 -> io:fwrite("Left Column.~n", []),
		 X = lists:nth(1, Board),
		 Y = lists:nth(4, Board),
		 if X == 0 ->
			1;
		?else ->
			if Y == 0 ->
				4;
			?else ->
				7
			end % if
		end; % if
		
	5 -> io:fwrite("Center Column.~n", []),
		 X = lists:nth(2, Board),
		 Y = lists:nth(5, Board),
		 if X == 0 ->
			2;
		?else ->
			if Y == 0 ->
				5;
			?else ->
				8
			end % if
		end; % if
		
	6 -> io:fwrite("Right Column.~n", []),
		 X = lists:nth(3, Board),
		 Y = lists:nth(6, Board),
		 if X == 0 ->
			3;
		?else ->
			if Y == 0 ->
				6;
			?else ->
				9
			end % if
		end; % if
		
	7 -> io:fwrite("Left-Right Diagonal.~n", []),
		 X = lists:nth(1, Board),
		 Y = lists:nth(5, Board),
		 if X == 0 ->
			1;
		?else ->
			if Y == 0 ->
				5;
			?else ->
				9
			end % if
		end; % if
	
	8 -> io:fwrite("Right-Left Diagonal.~n", []),
		 X = lists:nth(3, Board),
		 Y = lists:nth(5, Board),
		 if X == 0 ->
			3;
		?else ->
			if Y == 0 ->
				5;
			?else ->
				7
			end % if
		end % if
	end. % case
		
% This is ugly because if the Target is never found in the list this function will return length(List)+1.
% At least it's not a valid value, but this is not a standard convention, -1 would be better
findFirst(Target, [Head | _ ])   when (Target == Head) -> 1;
findFirst(Target, [Head | Tail]) when (Target /= Head) -> 1 + findFirst(Target, Tail);
findFirst(_, [])                                       -> 1.
	
playerWin() -> 
	io:fwrite("You win. ~n", []).

computerWin() ->  
	io:fwrite("Computer win. ~n", []).

nobodyWins() ->
	io:fwrite("Nobody wins, there is a tie ~n", []).
