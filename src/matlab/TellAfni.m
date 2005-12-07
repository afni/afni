function [err] = TellAfni (cs, opt)
%
%   [err] = TellAfni (cs, opt)
%
%Purpose:
%   Drive AFNI
%   
%   
%Input Parameters:
%   cs: An Nx1 vector of communication command structures
%       It is obtained using NewCs structure
%   opt: An optional options structure
%        .QuitOnErr (0/[1]): Return from function if any of cs(i) is malformed
%        .Verbose (0/[1]/2): 0 = mute, 1 = Yak, Yak (default), 2 = YAK YAK
%Output Parameters:
%   err : 0 No Problem
%       : N  N Problems
%   
%      
%More Info :
%   
%     NewCs
%     AFNI's README.driver file and the program plugout_drive
%     
%     Author : Ziad Saad
%     Date : Tue Dec 6 10:38:23 EST 2005
%     SSCC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'TellAfni';

%Debug Flag
DBG = 1;

%initailize return variables
err = 1;


if (nargin == 1) opt.QuitOnErr = []; end

if (~isfield(opt, 'QuitOnErr') || isempty(opt.QuitOnErr)) opt.QuitOnErr = 1; end
if (~isfield(opt, 'Verbose') || isempty(opt.Verbose)) opt.Verbose = 1; end

ncs = length(cs);
if (ncs == 0) err = 0; return; end

com = '';
ncom = 0;
for (i=1:1:ncs),
   if (cs(i).err),
      fprintf(2,'Error in command structure %d (%s).\n', i,cs(i).c);
      if (opt.QuitOnErr),
         fprintf(2,'Quitting.\n');
         return;
      else
         fprintf(2,'Ignoring command.\n');
         cs(i).c = '';
      end
   end
   
   if (cs(i).c),
      switch (cs(i).c),
         case 'START_AFNI',
            scom = sprintf('afni -yesplugouts %s &', cs(i).v);
            [s,w] = unix(scom);
            if (opt.Verbose & ~isempty(w)), 
               fprintf(1,'Command output:\n%s\n', w);
            end
            if (s),
               fprintf(2,'Error launching afni\n');
               return;
            end
         otherwise,
            com = sprintf('%s -com ''%s %s''', com, cs(i).c, cs(i).v);
            ncom = ncom+1;
      end
   end
end

b = 0;
if (~isempty(com)),
   scom = sprintf('plugout_drive -v %s -quit', com);
   if (opt.Verbose > 1),
      fprintf(1, 'making call:\n%s\n', scom);
   end
   [s,w] = unix(scom);
   if (isempty(strfind(scom,'QUIT'))), %Do not check in cases of QUIT
      [err, g, b] = TellAfniCheck(w);
      if (ncom ~= err + g + b ),
         fprintf(2,'Warning: Unexpected parsing trouble (ncom=%d, err+g+b=%d).\n', ncom, err+g+b);
      end
      if (err),
         fprintf(2,'Warning: Failed in parsing plugout_drive output.\nCannot confirm how %d out of %d commands executed\n', err, ncom);
      end
      if (g && opt.Verbose),
         fprintf(1,'%d out of %d commands OK\n', g, ncom);
      end
      if (b),
         fprintf(2,'Warning: %d out of %d commands failed in AFNI.\n', b, ncom);
      end
   end
   if (opt.Verbose > 1), 
      fprintf(1,'Command output:\n%s\n', w);
   end   
   if (s),
      fprintf(2,'Error telling afni\n');
      return;
   end
end

err = b;
return;
