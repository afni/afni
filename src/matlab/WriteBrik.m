function [err, ErrMessage, Info] = WriteBrik (M, Info, Opt)
%
%   [err, ErrMessage, Info] = WriteBrik (M, Info, Opt)
%
%Purpose:
%
%    Writes a data vector or matrix in AFNI's  format
%
%     
%Input Parameters:
%   M is the brick data (in vector or matrix format)
%   Info, the header structure (see BrikInfo)
%      if BYTEORDER_STRING is not specified, native is the default format
%      if BRICK_TYPES is not specified, short is the default
%
%   Opt an options structure with the following fields, [default]
%     .Scale: ([0]/1) scales values to a |maximum| of 32700. This is useful for writing bricks as shorts
%     .Prefix : filename prefix (mandatory field)
%     .View : [+orig], +acpc, +tlrc 
%     .verbose: ([0]/1)
%     .AppendHistory: (0/[1]) adds to the history field
%   
%   
%Output Parameters:
%   err : 0 No Problem
%       : 1 Mucho Problems
%   ErrMessage : the error or warning message
%   Info : the header nfo of the written brick
%      
%Key Terms:
%   
%More Info :
%   BrikInfo, BrikLoad
%     
%     need a FormHEAD function to create a minimal Info structure for a data vector
%
%     version 1.0 (keep in sync. with WriteBrikHEAD)
%		 In this version, Info.BRICK_STATS is cleared before writing. 
%
%     Author : Ziad Saad
%     Date : Fri Apr 6 16:03:02 PDT 2001
%     LBC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'WriteBrik';

%Debug Flag
DBG = 1;

%initailize return variables
	err = 1;
	ErrMessage = '';
	
%check on options
if (~isfield(Opt, 'verbose') | isempty (Opt.verbose)), Opt.verbose = 0; end
if (Opt.verbose), fprintf(1,'%s verbose: Checking input data ...', FuncName); end
if (~isfield(Opt, 'Prefix') | isempty (Opt.Prefix)), 
	err = 1; ErrMessage = sprintf('Error %s: You must specify Opt.Prefix.', FuncName);  errordlg(ErrMessage); return;
end
if (~isfield(Opt, 'Scale') | isempty (Opt.Scale)), Opt.Scale = 0; end
if (~isfield(Opt, 'View') | isempty(Opt.View)), Opt.View = 'orig'; end
if (~isempty(findstr('orig', lower(Opt.View)))),
	Opt.Views = '+orig';
elseif (~isempty(findstr('acpc', lower(Opt.View)))),
	Opt.Views = '+acpc';
elseif (~isempty(findstr('tlrc', lower(Opt.View)))),
	Opt.Views = '+tlrc';
else
	err = 1; ErrMessage = sprintf('Error %s: Bad value (%s) for Opt.View', FuncName, Opt.View); errordlg(ErrMessage); return;
end
if (~isfield(Opt, 'AppendHistory') | isempty (Opt.AppendHistory)), Opt.AppendHistory = 1; end

%form the flename based on the stuff in Opt.Prefix, just use the option
Fname = sprintf('%s%s', Opt.Prefix, Opt.Views);
FnameHEAD = sprintf('%s%s.HEAD', Opt.Prefix, Opt.Views);
FnameBRIK = sprintf('%s%s.BRIK', Opt.Prefix, Opt.Views);

if (exist(FnameHEAD) == 2 | exist(FnameBRIK) == 2),
	err = 1; ErrMessage = sprintf('Error %s: Output data set %s exists.', FuncName, Fname); errordlg(ErrMessage); return;
end



%make sure there is no clash in input data
	%is M a 4D or 1D
	N = zeros(1,4);
	[N(1), N(2), N(3), N(4)] = size(M);
	nd = ndims(M);
 	if (nd <= 2)
		isVect = 1;
	else
		isVect = 0;
	end

	if (isfield(Info, 'DATASET_DIMENSIONS') & length(Info.DATASET_DIMENSIONS) < 3 & length(Info.DATASET_DIMENSIONS) > 0),
		err = 1; ErrMessage = sprintf('Error %s: If you specify DATASET_DIMENSIONS it must be a vector of three elements', FuncName); errordlg(ErrMessage); return;
	end
	if (isfield(Info, 'DATASET_RANK') & length(Info.DATASET_RANK) < 2),
		err = 1; ErrMessage = sprintf('Error %s: If you specify DATASET_RANK it must be a vector of two elements', FuncName); errordlg(ErrMessage); return;
	end
	
	if ((~isfield(Info, 'DATASET_DIMENSIONS') |  isempty(Info.DATASET_DIMENSIONS)) & isVect)
		err = 1; ErrMessage = sprintf('Error %s: If M is a vector, you must specify DATASET_DIMENSIONS in Info', FuncName); errordlg(ErrMessage); return;
	end
	if ((~isfield(Info, 'DATASET_RANK') |  isempty(Info.DATASET_RANK)) & isVect)
		err = 1; ErrMessage = sprintf('Error %s: If M is a vector, you must specify DATASET_RANK in Info', FuncName); errordlg(ErrMessage); return;
	end
	
	if (isfield(Info, 'DATASET_DIMENSIONS') & ~isempty(Info.DATASET_DIMENSIONS) & ~isVect)
		if (N(1) ~= Info.DATASET_DIMENSIONS(1) |  N(2) ~= Info.DATASET_DIMENSIONS(2) | N(3) ~= Info.DATASET_DIMENSIONS(3) | N(4) ~= Info.DATASET_RANK(2))
			err = 1; ErrMessage = sprintf('Error %s: Dimensions mismatch between dimensions of M and Info.DATASET_DIMENSIONS, Info.DATASET_RANK.', FuncName); errordlg(ErrMessage); return;
		end
	end 

%OK, setup .DATASET_DIMENSIONS and .DATASET_RANK if needed 
	if (~isfield(Info, 'DATASET_DIMENSIONS') | isempty(Info.DATASET_DIMENSIONS)),
		Info.DATASET_DIMENSIONS = N(1:3); 
	end
	if (~isfield(Info, 'DATASET_RANK') | isempty(Info.DATASET_RANK)),
		Info.DATASET_RANK = [3 N(4)];
	end

%any Mandatory variables have now been set, check on the Header content
	
%Check out the values in Info
if (Opt.verbose), fprintf(1,'HEAD Info structure ...'); end
[err, ErrMessage, Info] = CheckBrikHEAD(Info);
if (err),
	ErrMessage = sprintf ('%s: Error in CheckBrikHEAD.\n(%s)', ErrMessage);
	return;
end

%reshape to a vector
if (~isVect | nd == 2),
	M = reshape(M, prod(N), 1);
end

%Delete the Brick_Stats, let afni take care of them
Info.BRICK_STATS=[]; 

%figure out the ouput format
if (~isfield(Info, 'BRICK_TYPES') | isempty(Info.BRICK_TYPES)),
	B_type = 1; %short
else
	B_type = Info.BRICK_TYPES;
end

if (~isfield(Info, 'BYTEORDER_STRING') | isempty(Info.BYTEORDER_STRING)),
	ByteOrder = 'native';
else
	if (~isempty(strmatch('MSB_FIRST', Info.BYTEORDER_STRING))),
		ByteOrder = 'ieee-be'; %Big Endian
	else 
		if (~isempty(strmatch('LSB_FIRST', Info.BYTEORDER_STRING))),
				ByteOrder = 'ieee-le'; %Little Endian
		else
			err = 1; ErrMessage = sprintf('Error %s: %s byte order is ambiguous.', FuncName, Info.BYTEORDER_STRING); 
			return;	
		end
	end
end

itype = unique(B_type);
if (length(itype) > 1),
	err =  1; ErrMessage = sprintf('Error %s: Not set up to write sub-bricks of multiple sub-types', FuncName); errordlg(ErrMessage); return;
end
switch itype,
	case 0
		typestr = 'ubit8';
	case 1
		typestr = 'short';
	case 2
		typestr = 'int';
	case 3
		typestr = 'float';
	otherwise
		err = ErrEval(FuncName,'Err_Cannot write this data type');
		return;
end

%figure out if scaling is required
Info.BRICK_FLOAT_FACS = zeros(1,Info.DATASET_RANK(2));
if (Opt.Scale),
		if (Opt.verbose), fprintf(1,'Scaling ...'); end
		NperBrik = Info.DATASET_DIMENSIONS(1) .* Info.DATASET_DIMENSIONS(2) .* Info.DATASET_DIMENSIONS(3);
		for (j=1:1:Info.DATASET_RANK(2)),
			istrt = 1+ (j-1).*NperBrik;
			istp = istrt + NperBrik - 1;
			[max, imax] = max(abs(M(istrt:istp)));
			Info.BRICK_STATS(2.*(j-1)+1)= min(M(istrt:istp));
			[max2, imax2] = max(M(istrt:istp));
			Info.BRICK_STATS(2.*j)= max2;
			Info.BRICK_FLOAT_FACS(j) = max ./ 32700; % a bit lower than 32000
			M(istrt:istp) = M(istrt:istp) ./ Info.BRICK_FLOAT_FACS(j);
		end
end


%open file for writing based on the type specified in Info

[fid, mess] = fopen (FnameBRIK, 'w', ByteOrder);
if (fid < 0), 
	err = 1; ErrMessage = sprintf('Error %s: Could not open %s for writing \n(%s)', FuncName, FnameBRIK, mess); errordlg(ErrMessage); return;
end

%write the file
if (Opt.verbose), fprintf(1,'Writing .BRIK to disk ...'); end
cnt = fwrite(fid, M, typestr);
if (cnt ~= prod(size(M))), 
	err = 1; ErrMessage = sprintf('Error %s: Could not write all %d values to %s\n. Deleting %s ...', FuncName, FnameBRIK, prod(size(M)), FnameBRIK); errordlg(ErrMessage); 
	fclose (fid);
	if (filexist(FnameBRIK) == 2),
		scom = sprintf('rm %s',FnameBRIK);
		unix(scom);
	end  
	return;
end

%close the file
fclose (fid);
[ST,I] = dbstack;

%add the history note if needed
if (Opt.AppendHistory),
	OptHist.AFNI_Format = 1;
	[tmp, OptHist.PerSig] = unix('whoami');
	%remove this annoying tset message (some bug ....)
	[err, snl, Nlines] = GetNextLine(OptHist.PerSig, 2);
	if (Nlines >= 2),
		[err, OptHist.PerSig] = GetNextLine(OptHist.PerSig,Nlines);
	end 
	if (tmp), 
		OptHist.PerSig = sprintf('DunnoWho');	
		else
		OptHist.PerSig = zdeblank(OptHist.PerSig); 
	end
	[err,S] = HistoryTrace (OptHist);
	if (~isfield(Info,'HISTORY_NOTE') |isempty(Info.HISTORY_NOTE)), Info.HISTORY_NOTE = ''; end
	Info.HISTORY_NOTE = sprintf('%s\\n%s', Info.HISTORY_NOTE, S);
end

%call for the function to write the header
if (Opt.verbose), fprintf(1,'Writing .HEAD to disk ...'); end
[err, ErrMessage] = WriteBrikHEAD (FnameHEAD, Info);
if (err),
	err = 1; ErrMessage = sprintf('Error %s: An error occurred in WriteBrikHEAD.\n%s', FuncName, ErrMessage); errordlg(ErrMessage); return;	
end
if (Opt.verbose), fprintf(1,'Done.\n'); end

err = 0;
return;

