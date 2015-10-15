function [Sd] = zdeblank (S)
%
%   [Sd] = zdeblank (s)
%
%Purpose:
%   removes blanks surrounding a word in S
%
%
%Input Parameters:
%   S : a string
%
%
%Output Parameters:
%  Sd : S without the surrounding blanks
%
%
%Key Terms:
%
%More Info :
%   deblank, zdeblankall
%
%
%
%     Author : Ziad Saad
%     Date : Tue Sep 12 12:15:24 PDT 2000
%     LBC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'zdeblank';

space_or_null=['[' char(0) ' \t\r\n\f\v]*'];
Sd_pre=regexprep(S,['^' space_or_null],'');
Sd=regexprep(Sd_pre,[space_or_null '$'],'');


return;

