function [err, v, Info] = Read_1D (fname, p1)
%
%   [err,M, Info] = Read_1D (fname, [opt])
%
%Purpose:
%   Reads an AFNI 1D file into M
%   
%   The file is to contain an ASCII table
%   of numbers. All rows must have the same number of entries
%   or the function will fail.
%   Lines starting with a '#' are considered
%   comment lines and are ignored.
%
%   The function will create a temporary file with
%   methods 0 and 2. If your files are huge, consider
%   disk space issues.
%
%Input Parameters:
%   fname : the name of the 1D file
%   Opt: An optional options structure
%      .verb (0/1) verbose mode, default is 1
%      .method (0/1/2) default is 0. 
%           0: use matlab to interpret 1D files
%              that have comments and other
%              1D formatting gimmicks (slow for large files)
%           1: use matlab to load a pure 1D file.
%              i.e. one that has only numbers in it.
%           2: use ConvertDset program to purify 1D file
%              then read it into matlab.
%
%Output Parameters:
%   err : 0 No Problem
%       : 1  Problems
%   M : Matrix containing values in fname
%   Info: An  Info header structure to pass along to 
%         the WriteBrik function
%   
%      
%Key Terms:
%   
%More Info :
%   
%   
%   
%
%     Author : Ziad Saad
%     Date : Fri Jul 23 10:11:34 EDT 2004
%     SSCC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'Read_1D';

%Debug Flag
DBG = 1;

if (nargin == 1),
   verb = 1; Opt = [];
else 
   if (isstruct(p1)),
      Opt = p1;
      if (isfield(Opt, 'verb') && ~isempty(Opt.verb)) verb = Opt.verb;
      else verb = 1; end
   else
      verb = p1;
      Opt.method = 0;
   end
end
if (~isfield(Opt, 'method') || isempty(Opt.method)), Opt.method = 0; end
if (~isfield(Opt, 'verb') || isempty(Opt.verb)), verb = 1; else verb = Opt.verb; end


%initailize return variables
err = 1;

v = [];
Info = [];
if (Opt.method < 0 | Opt.method > 2),
   fprintf(2,'Opt.method must be an integer between 0 and 2\n');
   return;
end
if (~filexist(fname)), % try with extension
	fname2 = sprintf('%s.1D', fname);
   fname3 = sprintf('%s.1D.dset', fname);
   if (verb), fprintf(1,'Trying for %s or %s\n', fname2, fname3); end
   if (filexist(fname2)),
      fname = sprintf('%s', fname2);
   elseif (filexist(fname3)),
      fname = sprintf('%s', fname3);
   else
      fprintf (2, 'Error %s:\n %s not found\n', FuncName, fname);
      return;
   end
else 
	if (verb), fprintf(1,'File %s exists and will be read.\n', fname); end
end

if (Opt.method == 0), 
   %load the 1D file
   if (verb), fprintf(1,'Reading file\n'); end
   fid = fopen(fname,'r');
   if (fid < 0),
      fprintf(1,'Error %s:\nCould not read %s.\nCheck for file existence\n', FuncName, fname);
      err = 1;
      return;
   end
   c = fscanf(fid,'%c');
   fclose(fid);

   %purge comments
   if (verb > 1), fprintf(1,'Purging comments\n'); end
   c = PurgeComments(c, '#');
   nc = length(c);
   %remove line breaks and the following new line
   if (verb > 1), fprintf(1,'Removing line breaks\n'); end
   ib = find (c == '\');
   nib = length(ib);
   for (i=1:1:nib), 
      c(ib(i)) = ' '; 
      if (c(ib(i)+1) ~= 10),
         fprintf(1,'Error %s:\nline break not followed by newline.\n', FuncName);
         err = 1;
         return;
      else c(ib(i)+1) = ' ';
      end
   end
   if (verb > 1), fprintf(1,'Replacing @\n'); end
   %work the @ cats
   ia = find (c == '@');
   lst = 1;
   nia = length(ia);
   cnew = '';
   if (nia),
      for (i=1:1:nia),
         %bracket @
         found = 0;
         j = ia(i)-1;
         while (j>0 & ~found),
            if (c(j) >= '0' & c(j) <= '9'), 
               j = j -1;
            else 
               found = j;
            end
         end
         cnew = [cnew ' ' c(lst:found )]; % Copy OK stuff
         if (found),
             prod = str2num(c(found:ia(i)-1));
         else 
            fprintf(1,'Error %s:\nno number preceding @\n', FuncName);
            err = 1;
            return;
         end
         %Now find the value to replicate
         if (ia(i)+100 < nc) nxtchunk = ia(i)+100;
         else nxtchunk = nc; end
         [vr, cnt, err, nxt] = sscanf(c(ia(i)+1:nxtchunk), '%f', 1);
         %insert the replacement
         cinsert = num2str(ones(1,prod)*vr);
         cnew = [cnew ' ' cinsert ' '];
         lst = ia(i)+nxt;c(lst);
      end
      cnew = [cnew ' ' c(lst:nc)];
      c = cnew;
   end


   meth = 2;
   switch (meth),
      case 1
         %this one's slow as hell
         if (verb > 1), fprintf(1,'The str2num operation ...\n'); end
         v = str2num(c); % str2double fails miserably where str2num does not
      case 2
         if (verb > 1), fprintf(1,'The round about way ...\n'); end
         ftmp = sprintf('%s_Read_1D_tmp_', fname);
         fid = fopen(ftmp,'w');
         if (fid < 0),
			   fprintf(1,'Error %s:\nFailed to open tempfile %s for writing\n', FuncName, ftmp);
            err = 1;
		   end
         fprintf(fid,'%c',c);
         v = load(ftmp);
         fclose(fid);
         rmcom = sprintf('rm -f %s', ftmp);
         unix(rmcom);
   end
elseif (Opt.method == 1),
   if (verb) fprintf(1,'1D file is expected not to have any comments.\n'); end
   v = load(fname);
   if (isempty(v)), fprintf(2,'Failed to read 1D file. If file exists Try method 0\n'); err = 1; return; end
elseif (Opt.method == 2),
   if (verb) fprintf(1,'Running ConvertDset to purging 1D file of bells and whistles\n'); end
   ftmp = sprintf('%s_Read_1D_tmp_', fname);
   convcom = sprintf('ConvertDset -o_1dp -input %s -i_1D -prefix %s', fname, ftmp);
   unix(convcom);
   ftmp = sprintf('%s.1D.dset',ftmp);
   v = load(ftmp);
   rmcom = sprintf('rm -f %s', ftmp);
   unix(rmcom); 
end

%some fake Info stuff
if (nargout == 3),
   [err, Info] = Info_1D(v, fname);
end

err = 0;
return;

