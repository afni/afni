function [M] = ROIcmap(nc,opt)
% [M] = ROIcmap([nc],[opt]);
% creates a colormap with 
%           no color too close to grayscale,
%           no two consecutive colors too close
%           no colors exeedingly close to another in the map
%
%   nc: number of colors in map. Default is 64
%   opt: optional options structure
%      .show: Figure handle in which to show the map
%             Default is 1. Set to 0 for no show.
%             In show mode, you get to pick colors with mouse
%             and read in their values. Hit enter to exit from
%             this mode.
%      .state: State of the random number generator.
%              Default is 0. 
%      .write: Name of file to write colormap into
%              Default is '', no writing. Use something like
%              ROI64s0.1D.cmap, for a 64 cols, seed 0 colormap.
% returns
%   M: The colormap. 
%
%see also readXcol, rgbdectohex, and ScaleToMap
% Ziad S. Saad SSCC/NIMH/NIH, ziad@nih.gov

if (nargin == 0),
   nc = 64;
   opt.show = 1;
elseif (nargin == 1),
   opt.show = 1;
end

if (isempty(nc)), nc = 64; end

if (~isfield(opt,'show') | isempty(opt.show)), opt.show = 1; end
if (~isfield(opt,'state') | isempty(opt.state)), opt.state = 0; end
if (~isfield(opt,'write') | isempty(opt.write)), opt.write = ''; end

%initialize rng 
rand('state',opt.state);

M = zeros(nc,3);
for (i=1:1:nc),
   M(i,:) = rand(1,3);
   %reject if too gray or too close to previous color
   while (toogray(M(i,:)) || tooclose(M,i)),
      M(i,:) = rand(1,3);
   end
end

if (~isempty(opt.write)),
   optw.OverWrite = 'p';
   wryte3(M, opt.write, optw);
end

if (opt.show),
   ShowCmap(M, opt.show);
end
   
return;


function [a] = toogray(c)

   a = 0;
   dc = abs(c - mean(c));
   if (dc(1) < 0.1 & dc(2) < 0.1 & dc(3) < 0.1), a = 1; end
   return;

function [a] = tooclose(M,i)

   if (i==1), a = 0; return; end
   
   a = 1;
   
   %too close to previous ?
   dc = abs(M(i,:)-M(i-1,:));
   if (sum(dc) < 0.4), return; end
   
   %too close to one before?
   if (i > 2), 
      for (j=1:1:i-2),
         dc = abs(M(i,:)-M(j,:));
         if (dc(1) < 0.1 & dc(2) < 0.1 & dc(3) < 0.1), return; end  
      end
   end
   %OK if you get here
   a = 0;
   return;   
