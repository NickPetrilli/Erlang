%
% Tic-Tac-Toe
%
-module(game).
-export([start/0]).
-define(else, true).

%
% --Public--
%
		
start() ->
	Board = newBoard(),
	Player = 1,
	gameLoop(Board, Player).

%
% --Private-- 
%

gameLoop(Board, Player) ->
			%first display the board
			displayBoard(Board),
			if(Player == 0) ->
				CpuBoard = cpuMove(Board, Player),
				CheckBoard = checkWin(Board),
				case CheckBoard of 
					{victory, x} -> io:fwrite("The user won the game!");
					{victory, o} -> io:fwrite("The computer won the game!");
					{draw, null} -> io:fwrite("The game ended in a tie");
					{_, _} -> gameLoop(CpuBoard, 1)
				end;
				
			?else -> 
				{ok, [Input]} = io:fread("Choose a spot on the board 1-9 ", "~d"),
				io:fwrite("You chose spot ~w, which is ", [Input]),
				%check if the move is valid
				CheckMove = checkMove(Input, Board),
				if(CheckMove) ->
					io:fwrite("a valid move. Nice going!~n"),
					%create a new board with the user's new move
					NewBoard = move(Board, Input, Player),
					%after the move is considered valid and is made, the new board needs to be checked
					CheckWin = checkWin(NewBoard),
					case CheckWin of
						{victory, x} -> io:fwrite("The user won the game!");
						{victory, o} -> io:fwrite("The computer won the game!");
						{draw, null} -> io:fwrite("The game ended in a tie");
						{_, _} -> gameLoop(NewBoard, 0)
					end;
				
				%if the move is invalid, the game loop is recalled with the same board 
				?else ->
					io:fwrite("an invalid move.~n"),
					gameLoop(Board, 1)
				end
			end.
			
newBoard() ->
	[undefined, undefined, undefined,
	 undefined, undefined, undefined,
	 undefined, undefined, undefined].
	 
displayBoard(Board) ->
				NewBoard = subst(1, x, Board),
				NextBoard = subst(-1, o, NewBoard),
				FinalBoard = subst(undefined, "", NextBoard),
				io:fwrite("~w | ~w | ~w  ~n~w | ~w | ~w  ~n~w | ~w | ~w~n", FinalBoard).
				
subst(_, _, []) -> [];
subst(This, That, [Head | Tail]) when (Head == This) -> 
										[That | subst(This, That, Tail)];
subst(This, That, [Head | Tail]) when (Head /= This) -> 
										[Head | subst(This, That, Tail)].

cpuMove(Board, Player) -> 
					%List1 = lists:sublist(Board, 1, 3),
					%List2 = lists:sublist(Board, 1, 3),
					%List3 = lists:sublist(Board, 1, 3),
					%Sum1 = lists:sum(List1),
					%Sum2 = lists:sum(List2),
					%Sum3 = lists:sum(List3),
					%tried to split up the lists and find sum in order to know where to block the player from winning
					%couldn't figure it out so random values are chosen just so the game can run
				
					Random = rand:uniform(9),
					Check = checkMove(Random, Board),
					if (Check) ->
						move(Board, Random, Player);
					?else ->
						cpuMove(Board, Player)
					end.

checkMove(Input, Board) -> 
				Nth = lists:nth(Input, Board),
				if (Nth =/= undefined) ->
					false;
				?else -> 	
					true
				end.
	
move(Board, Input, Player) ->
					%change the undefined values on the board to 1 or zero depending on the player
					%(only for internal testing) when board is displayed user will see x's and o's
					if(Player == 1) ->
						NewElement = 1;
					?else ->
						NewElement = -1
					end,
					%split the list and remove the undefined where the player specified, and add in a zero or one
					{Head, [_| Tail]} = lists:split(Input - 1, Board),
					lists:append([Head, [NewElement | Tail]]).
					
checkWin(Board) ->
	case Board of
		[1, 1, 1,
		 _, _, _,
		 _, _, _] -> {victory, x};
	
		[_, _, _,
		 1, 1, 1,
		 _, _, _] -> {victory, x};
		 
		[_, _, _,
		 _, _, _,
		 1, 1, 1] -> {victory, x};
		 
		[1, _, _,
		 1, _, _,
		 1, _, _] -> {victory, x};
		 
		[_, 1, _,
		 _, 1, _,
		 _, 1, _] -> {victory, x};
		 
		[_, _, 1,
		 _, _, 1,
		 _, _, 1] -> {victory, x};
		 
		[1, _, _,
		 _, 1, _,
		 _, _, 1] -> {victory, x};
		 
		[_, _, 1,
		 _, 1, _,
		 1, _, _] -> {victory, x};
		 
		[-1, -1, -1,
		 _, _, _,
		 _, _, _] -> {victory, o};
		 
		[_, _, _,
		 -1, -1, -1,
		 _, _, _] -> {victory, o};
		 
		[_, _, _,
		 _, _, _,
		 -1, -1, -1] -> {victory, o};
		 
		[-1, _, _,
		 -1, _, _,
		 -1, _, _] -> {victory, o};
		 
		[_, -1, _,
		 _, -1, _,
		 _, -1, _] -> {victory, o};
		 
		[_, _, -1,
		 _, _, -1,
		 _, _, -1] -> {victory, o};
		 
		[-1, _, _,
		 _, -1, _,
		 _, _, -1] -> {victory, o};
		 
		[_, _, -1,
		 _, -1, _,
		 -1, _, _] -> {victory, o};	

		[A, B, C,
		 D, E, F, 
		 G, H, I] when A =/= undefined, B =/= undefined, C =/= undefined, 
					D =/= undefined, E =/= undefined, F =/= undefined, G =/= undefined, 
					H =/= undefined, I =/= undefined -> {draw, null};
					%when all the values aren't undefined and a win isn't detected, its a tie
				
		%all other patterns weren't matched, so the game is still going on 
		_ -> {continue, null}
	end.
