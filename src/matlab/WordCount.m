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
%   D (optional) a single character to use as delimter, for example
%     '|' for | as a delimiter only. default is ' '
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
elseif ~ischar(D) || numel(D)~=1
    error('Second input, when provided, must be a single character');
end

S = deblank (S);

N=numel(strfind(S,D))+1;


return;

