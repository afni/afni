function [N] = WordCount (S,D)
%
%   [N] = WordCount (S,[D])
%
%Purpose:
%   Counts the number of words in S that are delimited by D
%
%
%Input Parameters:
%   S a string, trailing blanks are removed
%   D (optional) one or more characters to use as delimter, for example
%     'ab' for either 'a' or 'b' as a delimeter, and  '|' for | as a
%     delimiter only. default is ' '
%
%Output Parameters:
%   N number of words
%
%
%More Info :
%   see also GetWord, WordNumber
%   S = 'Hi Ya  |  Freak '
%   WordCount(S,'|') -> 2
%   S = 'Hi Ya    Freak '
%   WordCount(S) -> 3
%
%     Author : Ziad Saad
%     Date : Mon Apr 13 15:53:41 CDT 1998


%Define the function name for easy referencing
FuncName = 'WordCount';

if (nargin < 2),
	D = ' ';
end

S = deblank (S);

matching=any(bsxfun(@eq,S(:),D(:)'),2);

% avoid repetitions
matching(matching(2:end) & matching(1:(end-1)))=false;

N=sum(matching)+1;


return;

