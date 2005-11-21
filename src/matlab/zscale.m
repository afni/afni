function [y] = zscale (x,ub,lb)
% [Y] = ZSCALE (X,UB,LB)
% This function scales  X into Y such that 
%	its maximum value is UB 
%	and minimum value is LB
%
% If X is all constants, it gets scaled to UB;
%
%			Ziad, Oct 30 96 / modified March 18 97

xmin = min ( min(x(:)) );
xmax = max ( max(x(:)) );

if (xmin == xmax),
	y = ones(size(x)).*ub;
else
	y = (((x - xmin) ./ (xmax - xmin)) .* (ub - lb)) + lb;
end
