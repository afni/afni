function [err,p,f] = GetPath (s)
%
%   [err,PathString,FileString] = GetPath (s)
%
%Purpose:
%   Breaks the string s into a path and file part
%   
%   
%Input Parameters:
%   s is a string like How/Didley/Doo
%   
%   
%Output Parameters:
%   err : 0 No Problem
%       : 1 Mucho Problems
%   
%   PathString is a string like How/Didley
%   FileString is a string like Doo
%      
%More Info :
%   
%   
%   
%
%     Author : Ziad Saad
%     Date : Thu Apr 23 11:41:06 CDT 1998 


%Define the function name for easy referencing
FuncName = 'GetPath';

%initailize return variables
err = 1;
p = '';
f = '';

N = length(s);
if (N == 0),
	err = ErrEval(FuncName,'Err_Emtpy string');	return;
end

[i] = findstr(s,'/');
if (isempty(i)),
	p = '';
	f = s;
elseif (max(i)==N);
	p = s;
	f = '';
else
	p = s(1:max(i));
	f = s(max(i)+1:N);
end


err = 0;
return;

