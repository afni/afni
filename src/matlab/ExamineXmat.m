function [Xabi] = ExamineXmat(fname, polort, dt, nrun, nmot)
% a function to examine the design matrix produced by AFNI's 3dDeconvolve
% parameters not specified in function call  
if (nargin < 1 | isempty(fname) | (ischar(fname) & ~filexist(fname))),
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
if (nargin < 4 | nrun < 0.0),   
   nrun = [];
   while(isempty(nrun) | ~isnumeric(nrun) | nrun < 0.0),
      nrun = input('Enter number of runs: ');
   end
end
if (nargin < 5 | nmot < 0.0),   
   nmot = [];
   while(isempty(nmot) | ~isnumeric(nmot) | nmot < 0.0),
      nmot = input('Enter number of motion regressors (assumed at the very end): ');
   end
end

if (ischar(fname)) [e,Xabi] = Read_1D(fname);
else Xabi = fname; fname = 'matrix in mem.';
end

s = 'ddd';

%remove baseline and, assuming motion is last, remove last 6 regressors
istim = [1+(polort+1)*nrun, size(Xabi,2)-nmot]
trimmed = Xabi(:,[istim(1):1:istim(2)]);
CondFull = cond(Xabi);
CondNoMotion = cond(Xabi(:, [1:size(Xabi,2)-6]));
CondNoMotionNoBase = cond(Xabi(:,[1+(polort+1)*nrun:size(Xabi,2)-6]));

mshow = Xabi;  %trimmed

v = [1:1:size(mshow,2)];
t = 0:dt:(size(mshow,1)-1)*dt;
while (~isempty(s) & ~isempty(v)),
   figure(1); clf
   for (i=1:1:length(v)),
      subplot (length(v), 1, i);
      plot (t, mshow(:, v(i)), 'b-', t, mshow(:, v(i)), 'ro'); ylabel(sprintf('R%d', v(i)));
      if (i==1) title(sprintf('Xmat %s\tCondition #: Full %d\t NoMot %g\t NoMotNoBase %g\t Viewed %g\n',...
                               fname, CondFull, CondNoMotion,...
                               CondNoMotionNoBase, cond(mshow(:, [v]))),...
                     'interpreter', 'none'); end
   end
   xlabel('time (sec)');
   s = input (sprintf('Enter what you want see (stimuli between %d--%d): ', istim(1), istim(2)), 's');
   if (isempty(s)), 
      return;
   else
      eval(sprintf('v=%s;', s)); v
   end
end
