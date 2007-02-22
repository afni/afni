function ExamineXmat(fname, polort, dt)
% a function to examine the design matrix produced by AFNI's 3dDeconvolve
% parameters not specified in function call 
if (nargin < 1 | isempty(fname) | ~filexist(fname)),
   fname = uigetfile('*.1D','Pick an Xmat');
end
if (nargin < 2 | polort == -1),
   polort = [];
   while(isempty(polort) | ~isnumeric(polort) | polort < 0),
      polort = input('Enter polort: ');
   end
end
if (nargin < 3 | dt < 0.0),   
   dt = [];
   while(isempty(dt) | ~isnumeric(dt) | dt < 0.0),
      dt = input('Enter dt: ');
   end
end

[e,Xabi] = Read_1D(fname);

s = 'ddd';

%remove baseline and, assuming motion is last, remove last 6 regressors
trimmed = Xabi(:,[1+(polort+1)*8:size(Xabi,2)-6]);
CondFull = cond(Xabi);
CondNoMotion = cond(Xabi(:, [1:size(Xabi,2)-6]));
CondNoMotionNoBase = cond(Xabi(:,[1+(polort+1)*8:size(Xabi,2)-6]));

v = [1:1:size(trimmed,2)];
t = 0:dt:(size(trimmed,1)-1)*dt;
while (~isempty(s) & ~isempty(v)),
   figure(1); clf
   for (i=1:1:length(v)),
      subplot (length(v), 1, i);
      plot (t, trimmed(:, v(i))); ylabel(sprintf('R%d', v(i)));
      if (i==1) title(sprintf('Xmat %s\tCondition #: Full %d\t NoMot %g\t NoMotNoBase %g\t Viewed %g\n',...
                               fname, CondFull, CondNoMotion,...
                               CondNoMotionNoBase, cond(trimmed(:, [v]))),...
                     'interpreter', 'none'); end
   end
   xlabel('time (sec)');
   s = input (sprintf('Enter what you want see (%d--%d): ', 1, size(trimmed,2)), 's');
   eval(sprintf('v=%s;', s));
end
