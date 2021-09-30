%
% Recursion
%

-module(list).

-export([makeLists/2]).

%
% --Public--
%


makeLists(0, _) -> [];
makeLists(NumOfLists, Size) when NumOfLists > 0 -> 
						[create(Size, NumOfLists)] ++ makeLists(NumOfLists - 1, Size).

%
% --Private--
%

create(0, _) -> [];
create(Size, NumOfLists) when Size > 0 -> create(Size - 1, NumOfLists) ++ [Size * NumOfLists ]. 



