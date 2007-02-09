function [err,Info] = New_HEAD (opt)
%
%   [err,Info] = New_HEAD (Opt)
%
%Purpose:
%   
%   
%   
%Input Parameters:
%   
%   
%   
%Output Parameters:
%   err : 0 No Problem
%       : 1  Problems
%   
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
%     Date : Fri Feb 9 16:29:51 EST 2007
%     SSCC/NIMH/ National Institutes of Health, Bethesda Maryland


%Define the function name for easy referencing
FuncName = 'New_HEAD';

%Debug Flag
DBG = 1;

%initailize return variables
err = 1;
Info = [];

%work options
if (ischar(opt)), %perhaps test mode
   New_HEAD_test(opt);
   return;
end

%standardize input
if (isfield(opt,'dimen')),
   if (ischar(opt.dimen)),
      v = str2num(opt.dimen);
      rmfield(opt, 'dimen');
      opt.dimen = v;
   end
end


%create command
tmp_suf = '___NeW_hEaD_';
sopt = '3dUndump -head_only';
if (isfield(opt,'dimen')),
   sopt = sprintf('%s -dimen %s', sopt, num2str(opt.dimen(1:3)));
end
if (isfield(opt,'orient')),
   sopt = sprintf('%s -orient %s', sopt, opt.orient);
end
if (isfield(opt,'master')),
   [Status, mPrefix, mView] = PrefixStatus (opt.master);
else
   mView = '+orig';
end
if (~isfield(opt,'view')),
   opt.view = '+orig';
end
if (isfield(opt,'prefix')),
   [Status, Prefix, View] = PrefixStatus (opt.prefix);
   if (Status < 1),
      fprintf(1,'Error %s:\nLooks like %s exists already.\n', FuncName, opt.prefix);
      return;
   end
   opt.prefix = Prefix;
   ohead = sprintf('%s%s', tmp_suf, opt.prefix);
   sopt = sprintf('%s -prefix %s%s', sopt, ohead);
else
   fprintf(1,'Error %s:\nNeed a .prefix option.\n', FuncName);
   return;
end

[e,w] = unix(sopt);
if (e),
   fprintf(1,'Error %s:\nHeader creating command %s failed.\nSee this function''s help and 3dUndump -help\n3dUndump''s output was:\n%s\n', ...
            FuncName, sopt, w); 
   New_HEAD_CLEAN(tmp_suf); 
   return;  
end

[err,Info]  = BrikInfo(sprintf('%s%s', ohead,mView));
New_HEAD_CLEAN(tmp_suf);   

%take care of prefix business
Info.RootName = sprintf('%s%s', opt.prefix, opt.view);

%take care of view
if (strcmp(mView, opt.view) == 0), %different
   if (strcmp(opt.view,'+orig')) Info.SCENE_DATA(1) = 0;
   elseif (strcmp(opt.view,'+acpc')) Info.SCENE_DATA(1) = 1;
   elseif (strcmp(opt.view,'+tlrc')) Info.SCENE_DATA(1) = 2;
   else
      fprintf(1,'Error %s:\nBad view %s\n', FuncName, opt.view);
      return;
   end 
end

%take care of dimen
if (length(opt.dimen) > 3),
   Info.DATASET_RANK(2) = opt.dimen(4);
end

%take care of TR
if (isfield(opt,'tr')),
   Info.TAXIS_NUMS(1) = Info.DATASET_RANK(2);
   Info.TAXIS_NUMS(2) = 0; %no time offset for slices at the moment
   Info.TAXIS_NUMS(3) = 1; %units in seconds for tr (below)

   Info.TAXIS_FLOATS(1) = 0;  %time origin 0
   Info.TAXIS_FLOATS(2) = opt.tr;   %TR in units of Info.TAXIS_NUMS(3)
   Info.TAXIS_FLOATS(3) = 0; %duration of acquisition
   Info.TAXIS_FLOATS(4) = 0; %no time offset please
   Info.TAXIS_FLOATS(5) = 0; %no time offset please
   
   Info.TAXIS_OFFSETS = zeros(1,Info.TAXIS_NUMS(1));  %no bloody time offset
end

err = 0;
return;

function New_HEAD_CLEAN(sss)
   unix(sprintf('rm -f %s*.HEAD >& /dev/null', sss));
return;

function New_HEAD_test(opt)
   FuncName = sprintf('New_HEAD_%s', opt);
   if (strcmp(opt,'test1')),
      %say you have a matrix of a particular size
      M = flow(100);
      %now to put it in a dataset
      optt.prefix = sprintf('test1_%s', FuncName);
      unix(sprintf('rm -f %s*.????  >& /dev/null', optt.prefix));
      optt.dimen  = size(M);
      optt.orient = 'RAI';
      [err,Info] = New_HEAD(optt);
      if (err),
         fprintf(1,'Error %s:\nFailed in New_HEAD\n', FuncName);
         return;
      end
      [e,m,i] = WriteBrik(M,Info,optt);
      fprintf(1,'\nTo view results, try:\n  unix(''afni %s.HEAD &'');\nOpen all three views and click on "See Overlay"\n', Info.RootName);
      return;
   else
      fprintf(1,'Error %s:\nOpt = %s is invalid\n', FuncName, opt);
      return;
   end
return;
