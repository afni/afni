function [err,ErrMessage, NameList] = zglobb (Identifiers, Opt)
%
%   [err, ErrMessage, List] = zglobb (Identifiers, [Opt])
%
%Purpose:
%  returns the list of files specified in Identifiers 
%   
%   
%Input Parameters:
%   Identifiers is a cellstr identifying which briks to use
%     Identifiers = {'ADzst2r*ir.alnd2AAzs+orig.HEAD' , 'AFzst2r*ir.alnd2AAzs+orig.HEAD'} 
%   Opt is an optional Options structure
%   	.LsType : type of output List
%         'l' --> (default) Create a Nx1 structure vector with fields identical to those returned by dir
%         '|' --> Create a | delimited string with al the filenames in it
%     .NoExt : (default is '') a string containing a | delimited list of filename extensions to remove
%            example '.HEAD|.BRIK'
%   
%Output Parameters:
%   err : 0 No Problem
%       : 1 Mucho Problems
%   ErrMessage : Any error or warning messages
%   List is the list of files with a format depending on Opt.LsType
%      
%Key Terms:
%   
%More Info :
%   ? as a wildcard is not woring
%   
%   
%
%     Author : Ziad Saad
%     Date : Fri Feb 09 09:55:01 EST 2001
%     LBC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'zglobb';

%Debug Flag
DBG = 1;

%initailize return variables
err = 1;
ErrMessage = 'Undetermined';
NameList = [];

if (nargin == 1),
	Opt.LsType = 'l';
	Opt.NoExt = '';
else
	if (~isfield (Opt, 'LsType') | isempty(Opt.LsType)), Opt.LsType = 'l'; end
	if (~isfield (Opt, 'NoExt') | isempty(Opt.NoExt)), Opt.NoExt = ''; end
end


switch Opt.LsType,
	case 'l',
		NameList = []; 
	case '|',
		NameList = '';
	otherwise,
		ErrMessage = sprintf('%s is an unknown Opt.LsType value', Opt.LsType);
		err = 1;
		return;  
end

%get the names of the BRIKS identified in Identifiers
N_ID = length(Identifiers);

cnt = 0;
for (i=1:1:N_ID)
	sd = dir (char(Identifiers(i)));
	ns = length(sd);
	for (j=1:1:ns)
		if (~strcmp(sd(j).name, '.') & ~strcmp(sd(j).name, '..'))
			cnt = cnt + 1;
			if (~isempty(Opt.NoExt)), 
				sd(j).name = RemoveExtension (sd(j).name, Opt.NoExt);
			end
			switch Opt.LsType,
				case 'l', 
					NameList(cnt).name =  sd(j).name;
					NameList(cnt).date =  sd(j).date;
					NameList(cnt).bytes = sd(j).bytes;
					NameList(cnt).isdir = sd(j).isdir;
				case '|'
					NameList = sprintf('%s|%s',NameList,sd(j).name);
			end
		end 
	end
end
	
if (Opt.LsType == '|'),
	NameList = NameList(2:length(NameList)); %get rid of first |
end

if (cnt == 0),
	ErrMessage = 'No match';
	return;
end


err = 0;
ErrMessage = '';
return;

