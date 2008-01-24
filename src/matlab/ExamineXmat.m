function ExamineXmat(fname, polort, dt, nrun, nmot)
%  ExamineXmat([XMAT])
%
%  A function to aid in examining the design matrix output by 3dDeconvolve
%  
% XMAT: Name of xmat file, such as EA.xmat.1D. If unspecified,
%        function will open a file browser to pick one.
% The function will begin by displaying all the columns (regressors)
% in XMAT.
% The title of the graph shows:
%   name of the file
%   TR
%   condition numbers for:
%      entire matrix (Full)
%      entire matrix without motion regressors (NoMot)
%      entire matrix without motion and without baseline (NoMotNoBase)
%      plotted columns only (Viewed)
%
% Labels of various columns are printed to the left, if there are not
% too many columns displayed. Labels contain the column index and the label, 
% separated by a ":"
%
% The function will prompt you for column selections, which would allow
% you to examine a section of the design matrix at a time. Looking at 
% the 'Viewed' condition number would help you find which task
% regressors may be causing multicollinearity. Column selection 
% can be done using strings that match one or a set of regressors or 
% using column indices. The prompt interface has selection examples. 
%

% old use: ExamineXmat(fname, polort, dt, nrun, nmot)
if (nargin < 1 | isempty(fname) | (ischar(fname) & ~filexist(fname))),
   fname = uigetfile('*.xmat.1D','Pick an Xmat');
end
if (nargin < 2),
   csstims = cellstr('');
   polort = -1;
   nrun = 0;
   cntstims = 0;
   %get the info from 3dSynthesize
   com = sprintf('3dSynthesize -dry_info -matrix %s -select all', fname);
   [s1, s2] = unix(com);
   cnt = 1;
   while (~isempty(s2)),
      [d,c,e,n] = sscanf(s2,'%s ',1);
      s2 = s2(n:length(s2));
      if (strncmp(d,'TR:',3)),
         [tt,dt] = strread(d,'%s%f','delimiter',':');
      elseif (~isempty(strfind(d,'Run#'))),
         [col,tt] = strread(d,'%d%s','delimiter',':'); 
         if (strncmp(tt,'Run#1',5)), 
            polort = polort+1;   
         elseif (strncmp(tt,'Run#',4)),
            nrun = nrun+1;
         elseif (strncmp(tt(length(tt):-1:1),'0#',2)),
            cntstims = cntstims+1;
            csstims(cntstims) = cellstr(tt);
         end 
         cs(cnt) = cellstr(d);
         cnt = cnt + 1;
      elseif (~isempty(strfind(d,'#'))),
         if (strncmp(d([length(d):-1:1]),'0#',2)),
            cntstims = cntstims+1;
            [col,tt] = strread(d,'%d%s','delimiter',':'); 
            csstims(cntstims) = cellstr(tt);
         end 
         cs(cnt) = cellstr(d);
         cnt = cnt + 1;
      end
   end
   nrun = nrun/(polort+1)+1;
   nmot = 0;
end
if (polort < 0),
   polort = [];
   while(isempty(polort) | ~isnumeric(polort) | polort < 0),
      polort = input('Enter polort: ');
   end
end
if (dt < 0.0),   
   dt = [];
   while(isempty(dt) | ~isnumeric(dt) | dt < 0.0),
      dt = input('Enter dt: ');
   end
end
if (nrun < 0.0),   
   nrun = [];
   while(isempty(nrun) | ~isnumeric(nrun) | nrun < 0.0),
      nrun = input('Enter number of runs: ');
   end
end
if (nmot < 0.0),   
   nmot = [];
   while(isempty(nmot) | ~isnumeric(nmot) | nmot < 0.0),
      nmot = input('Enter number of motion regressors (assumed at the very end): ');
   end
end

Opt.verb=0;
if (ischar(fname)) [e,Xabi] = Read_1D(fname, Opt);
else Xabi = fname; fname = 'matrix in mem.';
end

s = 'ddd';

%remove baseline and, assuming motion is last, remove last 6 regressors
%but we do not reliably know where motion lies, so for now, default is nmot = 0

istim = [1+(polort+1)*nrun, size(Xabi,2)-nmot];
trimmed = Xabi(:,[istim(1):1:istim(2)]);
CondFull = cond(Xabi);
CondNoMotion = cond(Xabi(:, [1:size(Xabi,2)-nmot]));
CondNoMotionNoBase = cond(Xabi(:,[1+(polort+1)*nrun:size(Xabi,2)-nmot]));

mshow = Xabi;  
UseMult = 1;
dogui = 1;

v = [1:1:size(mshow,2)];
t = 0:dt:(size(mshow,1)-1)*dt;
while (~isempty(s)),
   if (isempty(v)), %maybe bad parsing
      snote = sprintf('No proper selection, showing entire matrix.');
      if (dogui),
         warndlg(snote);
      else
         fprintf(2,'\n%s\n', snote);
      end
      v = [1:1:size(Xabi,2)];
   end
   figure(1); clf
   if (length(v) < 10 & UseMult),
      for (i=1:1:length(v)),
         if (i==1),
            titplot = subplot (length(v), 1, i);
         else
            subplot (length(v), 1, i);
         end
         if (length(v) < 7),
            plot (t, mshow(:, v(i)), 'b-', t, mshow(:, v(i)), 'ro');
         else
            plot (t, mshow(:, v(i)), 'b-');
         end
         if (~isempty(cs) & length(v)<20),
            %ylabel(char(cs(v(i))), 'interpreter', 'none');
            str = char(cs(v(i)));
         else 
            %ylabel(sprintf('R%d', v(i)+1),  'interpreter', 'none');
            str = sprintf('R%d', v(i)+1);
         end
         ms = mshow(:,v(i));
         offs = mean(ms(find(ms > 0)));
         if (length(v) < 35),
            xt = get(gca,'XTick');
            text( xt(1)-0.1.*xt(2), offs, char(cs(v(i))), ...
                  'HorizontalAlignment', 'right',...
                  'interpreter', 'none');
         end
      end
   else
      offs = 0;
      titplot = subplot (1,1,1);
      for (i=1:1:length(v)),
         ms = mshow(:,v(i));
         offs = offs+mean(ms(find(ms > 0)));
         plot (t, mshow(:,v(i))+offs, 'Color', ROIcol); hold on
         if (length(v) < 35),
            xt = get(gca,'XTick');
            text( xt(1)-0.1.*xt(2), offs, char(cs(v(i))), ...
                  'HorizontalAlignment', 'right',...
                  'interpreter', 'none');
         end
      end
      set(gca,'YTickLabel',[]);
   end
   xlabel('time (sec)');
   subplot(titplot);
   title(sprintf(['Xmat %s, TR %.3f \t'...
                           'Condition #: '...
                           'Full %d\t NoMot %g\t NoMotNoBase %g \t'...
                           'Viewed %g\n'],...
                            fname, dt, CondFull, CondNoMotion,...
                            CondNoMotionNoBase, cond(mshow(:, [v]))),...
                  'interpreter', 'none'); 
   
   smpl = char(csstims(1));
   if (length(csstims)>2),
      sss =  char(csstims(length(csstims)));
      smpl2 = sprintf(' or "%s %s"',...
                      smpl(1:min(4,length(smpl))),...
                      sss(1:min(4,length(sss))));
   else
      smpl2 = '';
   end
   if (istim(2) - istim(1) > 5),
      smplcol = sprintf('"%d", "[%d,%d,%d]", or "[%d:%d]"',...
                        istim(1)-1,...
                        istim(1), istim(1)+2, istim(2)-1,...
                        istim(1)-1, istim(2)-1);
   else,
      smplcol = sprintf('"3", "[0,1]", or "[2:4]"');
   end
   sprompt = sprintf([ 'Enter which regressors (columns) you want to see. \n'...
                       '-------------------------------------------------\n'...
                       'You can use text such as "%s"%s that matches one or '...
                       'a few labels,\n'...
                       'or column indices in matlab''s vector notation '...
                       'such as %s\n'...
                       'Column indices start at 0 as with AFNI.\n'...
                       'In this XMAT stimuli are in columns between %d and %d: '...
                       '\n'...
                       ], ...
                       smpl(1:min(4,length(smpl))),...
                       smpl2,...
                       smplcol,...
                       istim(1)-1, istim(2)-1);
   if (~dogui),
      s = zdeblank(input (sprompt, 's'));
   else,
      Opt.Resize='on';
      Opt.WindowStyle='normal';
      s = zdeblank(char(inputdlg(cellstr(sprompt),...
                                 'Select Regressors',...
                                 1,...
                                 cellstr(''),...
                                 Opt)));
   end 
   if (isempty(s)), 
      return;
   else
      if (isdigit(s(1)) | s(1) == '[' | s(1) == '(')
         eval(sprintf('v=%s;', s)); 
         if (0 & isempty(v)),
            return;
         end
         v = v + 1;
      else
         v = [];
         stmp = s;
         while (~isempty(stmp)),
            [d,c,e,n] = sscanf(stmp,'%s ',1);
            stmp = stmp(n:length(stmp));
            d = zdeblank(d);
            for(i=1:1:length(cs)),
               [col,tt] = strread(char(cs(i)),'%d%s','delimiter',':');
               if (strncmp(tt,d, length(d))),
                  v = [v i];
               end
            end
         end
         if (0 & isempty(v)),
            return;
         end
      end
   end
end
