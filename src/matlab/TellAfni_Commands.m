function [l] = TellAfni_Commands (f)
%
%   [l] = TellAfni_Commands (f)
%
%Purpose:
%   Extract, all the commands from README_driver
%   No guarantee that you'll get all the commands extracted, 
%   Use this as a guide but read the entire README_driver for the full picture
%   
%Input Parameters:
%   f: Full path and name of README.driver file
%      pass no parameters if you want the function to try and
%      locate the file automatically. 
%      Use '' if you want to look for the file interactively.
%     
%   
%Output Parameters:
%   l a cell string of the possible parameters
%   
%   
%      
%Key Terms:
%   
%More Info :
%   
%     README.driver
%     NewCs
%     TellAfni
%
%     Author : Ziad Saad
%     Date : Wed Dec 7 15:34:57 EST 2005
%     SSCC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'TellAfni_Commands';

%Debug Flag
DBG = 1;

%initailize return variables
err = 1;

if (nargin == 0), %try to find README.driver
   f = '';
   [s,w] = unix('locate README.driver');
   if (~isempty(w)), %got a hit
      i = find(isspace(w));
      f = w(1:i(1)-1);
   end
end

if (~isempty(f) & exist(f,'file') == 2),
   fprintf(1,'Using file %s for info.\n', f);
else 
   f = ''
end

if (isempty(f)),
   [f, p] = uigetfile('README.driver', 'Pick a README.driver');
   if (f == 0),
      return;
   end
end

fid = fopen(f, 'r');
if (fid < 0),
   fprintf(2,'Failed to read %s\n', f);
   return;
end
   
s = fscanf(fid,'%c');
ns = length(s);
fclose(fid);

[err,inl] = FindChar(s,'NewLine');
ninl = length(inl);

ncm = 50;
cnt = 1;
S = char(zeros(ninl, ncm));
if (ninl),
   for (i=1:1:ninl-1),
      [err,w] = GetWord(s(inl(i):min(inl(i)+ncm, ns)),1);
      w = zdeblank(w);
      nw = length(w);
      if (nw > 1),
         if ( (sum(abs(double(upper(w))-double(w))) == 0) & sum(isletter(w)) > nw/2),
            % fprintf(2,'>%s<\t',w); pause
            S(cnt,1:nw) = w;
            cnt = cnt + 1;
         end
      end
   end
else
   fprintf(2,'Failed to understand file.\n');
   return;
end

if (cnt < 2),
   fprintf(2,'Failed to understand file, found nothing.\n');
   return;
end

l = unique(cellstr (S(1:cnt-1,:)));

return;

