function [err, Qd, s, termname, nterms, sindices, dfbothSS, modw, modwo, tnames, dfterm, dfe, Contr] = PreProc(n, NF, group, varnames, FL, Contr, cov, unbalanced)
%
%   [err, Qd, Rd, sindices, dfboth, modw, modwo, dfx] = PreProc(n,group,varnames)
%
%Purpose:
%   
%  Returns QR decomposition results for the design matrix projected to null space 
%   
%Input Parameters:
%   n: number of datasets = total # of combinations including repeats
%   group: cell array of factor levels
%   varnames: 
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
%     Author : Gang Chen
%     Date : Tue Mar 23 13:57:52 EST 2004
%     SSCC/NIMH/ NIH, Bethesda MD 20892


%Define the function name for easy referencing
FuncName = 'PreProc.m';

%Debug Flag
DBG = 1;

%initailize return variables
err = 1;

% Don't worry about NaN's at this point.
group = group(:);    % what's it for?
ng = length(group);  % number of factors
termlist = makemodel(ng, ng);  % Generate terms for all main effects plus various interactions

for j=1:ng
   gj = group{j};
   if (size(gj,1) == 1), gj = gj(:); end
   if (size(gj,1) ~= n)
      error('Factor %d must have %d elements.',j,n);
   end
   if (ischar(gj)), gj = cellstr(gj); end
   group{j} = gj;
end

gdum = cell(ng,1);
dfvar = zeros(ng,1);

if (unbalanced.yes == 0), % Balanced designs
   vconstr = cell(ng,1);
   %vmean = cell(ng,1);  %vmean is never used!!!
end

for j=1:ng   % for each factor
   gj = group{j};
   [gij,gnj] = grp2idx(gj);   % Create index vector from a grouping variable: gij is a vector 
	                           % taking integer values from 1 up to the number of unique entries in gj
										% gnj is a cell array of names, so that gnj(gij) reproduces gj
   nlevels = size(gnj,1);     % levels for this factor
   dfvar(j) = nlevels - 1;    % D. F. for this factor

   if (unbalanced.yes == 0),	
   if (cov.do & j==cov.marker)        
	   gdum{j} = gj;
      dfvar(j) = 1;           % D. F. = 1
      vconstr{j} = zeros(0,1);
%     vmean{j} = 1;		
   else
      gdum{j} = idummy(gij, 3);
      vconstr{j} = idummy(1:nlevels)';
%     vmean{j} = ones(1,nlevels) / nlevels;  % array (1Xnlevels) of one ones, but vmean is never used in the code!!!!!!!!!1
	end
	else gdum{j} = idummy(gij, 3);  % Unbalanced designs	
	end % if (unbalanced.yes == 0): Only for balanced designs
		
end

% Create dummy variable arrays for each term in the model.
nterms = size(termlist,1);             % Number of rows (1st dimension) in termlist
[sterms,sindex] = sortrows(termlist);  % Sort terms in ascending order. 
ncols = 1;
nconstr = 0;

termdum = cell(nterms, 1);        % cell array of dummy variables which are design matrix cols
termconstr = cell(nterms,1);      % constraints to make each term well defined
levelcodes = cell(nterms, 1);     % codes for levels of each M row
tnames = cell(nterms, 1);         % name for entire term, e.g. A*B
dfterm0 = zeros(nterms, 1);       % nominal d.f. for each term
termvars = cell(nterms, 1);       % list of vars in each term
termlength = zeros(size(sindex)); % length of each term (number of columns)

%randomterm = find(termlist*randomvar > 0);  % indices of random terms

% For each term,
for j=1:nterms
   % Loop over elements of the term
   df0 = 1;
   tm = sterms(j,:);
   tdum = [];         % empty term so far
   tconstr = 1;       % empty constraints so far
   tn = '';           % blank name so far
   vars = find(tm);   % Find indices of nonzero elements
   for varidx = 1:length(vars)
      % Process each variable participating in this term
      varnum = vars(varidx);          % variable name
      tm(varnum) = 0;                 % term without this variable
      df0 = df0 * dfvar(varnum);      % d.f. so far

      % Combine its dummy variable with the part computed so far
      G = gdum{varnum};           % dummy vars for this grouping var
      nlevterm = size(tdum,2);    % levels for term so far
      nlevgrp  = size(G,2);       % levels for this grouping var
      tdum = termcross(G,tdum);   % combine G into term dummy vars

      % Construct the term name and constraints matrix
%      if (ismember(varnum, NF) & cov.do),    % for the covariate, which is the last factor
		if (ismember(varnum, cov.marker) & cov.do),    % for the covariate, which is the last factor
         vconstr = ones(0,1);
      else
         vconstr = ones(1, nlevgrp);
      end
      if (isempty(tn))
         tn = varnames{varnum};
         tconstr = vconstr;
      else
         tn = [varnames{varnum} '*' tn];
         tconstr = [kron(vconstr,eye(size(tconstr,2)));
                    kron(eye(length(vconstr)),tconstr)];   % Kronecker 
      end

      % If the rest of this term is computed, take advantage of that
      prevterms = sterms(1:j-1,:);
      oldtm = find((prevterms * tm') == sum(tm));      % same vars in old term
      oldtm = oldtm((prevterms(oldtm,:) * ~tm') == 0); % and no others
      if (length(oldtm) > 0)
         k = sindex(oldtm(1));
         tdum = termcross(termdum{k}, tdum);
         oconstr = termconstr{k};
         tconstr = [kron(tconstr,              eye(size(oconstr,2)));
                    kron(eye(size(tconstr,2)), oconstr)];
         tn = [tn '*' tnames{k}];
         df0 = df0 * dfterm0(k);
         break;
      end
   end

   % Store this term's dummy variables and name
   k = size(tdum, 2);
   termlength(sindex(j),1) = k;
   ncols = ncols + k;
   sj = sindex(j);
   termdum{sj} = tdum;
   termconstr{sj} = tconstr;
   termvars{sj} = vars;
   levelcodes{sj} = fullfact(dfvar(vars)+1);
   if (isempty(tn)), tn = 'Constant'; end
   tnames{sj,1} = tn;
   dfterm0(sj) = df0;
   nconstr = nconstr + size(tconstr,1);
end
tnames{length(tnames)+1,1} = 'Error';

% Create the full design matrix
dmat = ones(n, ncols);        % to hold design matrix of nXncols
cmat = zeros(nconstr,ncols);  % to hold constraints matrix
cbase = 0;                    % base from which to fill in cmat
termname = zeros(ncols,1);
termstart = cumsum([2; termlength(1:end-1)]);
termend = termstart + termlength - 1;
for j=1:nterms
   clist = termstart(j):termend(j);
   dmat(:, clist) = termdum{j};
   C = termconstr{j};
   nC = size(C,1);
   cmat(cbase+1:cbase+nC,clist) = C;
   termname(clist) = j;
   cbase = cbase + nC;
end

[err, Qd, dfx, dmat2] = QRDecom(dmat, cmat);
%dfe = n - dfx;

% Determine which models to compare for testing each term
ssw  = -ones(nterms, 1);      % sum of squares with this term
sswo = ssw;                   % sum of squares without this term
dfw  = ssw;                   % residual d.f. with this term
dfwo = ssw;                   % residual d.f. without this term

if (unbalanced.yes == 1)
   modw = tril(ones(nterms)); % get model with this term
   k = nterms;                % locations of model with all terms
else

% Only apply type III for sums of squares
modw = ones(nterms);
%k = 1:nterms;
%TnotC = termsnotcontained(termlist);

end % if (unbalanced.yes == 1)

modw = logical(modw);                  % get model with this term
modwo = logical(modw - eye(nterms));   % get model without this term

dfw(1:nterms) = dfx;

% Fit each model separately
dfboth = [dfw; dfwo];
dfbothSS = dfboth;  % For usage in ss.m

% Consider interactions before their components for type 3 ss, so
% examine the terms in decreasing order of the number factors in the term


if (unbalanced.yes == 1)
   sindices = [(1:size(termlist,1)), (1:size(termlist,1))]';	
else	
termsize = sum(termlist,2)';      %sum of all the elements along 2nd dimension (row)
[stermsize,sindices] = sort(termsize); % sort in ascending order, output is stored in stermszie; sindices is the index for the new array
sindices = [sindices(:); sindices(:)];

end % if (unbalanced.yes == 1)

% Here QR decomposition is done, which is voxel independent

s(length(sindices)).Qdt = [];

for j=length(sindices):-1:1
   % Find the next model index to fit
   k = sindices(j);
   
   % Look in unsorted arrays to see if we have already fit this model
   if j>nterms
      k0 = k+nterms;
   else
      k0 = k;
   end
   if dfboth(k0)~=-1
      continue
   end
   
   % Find the model with this index
   if (j > nterms)
      thismod = modwo(k, :);
   else
      thismod = modw(k, :);
   end
   
   % Get the design matrix for this model
   keepterms = find(thismod);
   clist = ismember(termname, [0 keepterms]);
   X = dmat2(:,clist);
   C = cmat(:,clist);

   % Fit this term 
	[err, s(j).Qdt, dfx0] = QRDecom(X, C);	

   % Use these results for each term that requires them

   mod0 = repmat(thismod, nterms, 1);
   k = find(all(modw == mod0, 2));    
   dfw(k) = dfx0;
   dfboth(k) = 0;
	
   k = find(all(modwo == mod0, 2));
   dfwo(k) = dfx0;
   dfboth(nterms+k) = 0;
end
clear mod0

dfterm = dfw - dfwo;
dfe = n-dfx;   %residual degrees of freedom

if (Contr.do == 1),

% In design matrix dmat, the first column is all one's, for the total mean. Then there are totally
% FL(1).N_level + FL(2).N_level + FL(3).N_level + FL(4).N_level columns for the main effects. 
% Next 2nd order interactions, 3rd order interaction, and 4th order interactions.

% Store only those mean columns in design matrix. 

num_col0 = 1;  % the 1st column is for grand mean (0 order)

num_col1 = 0;
for (i = 1:1:NF),
   num_col1 = num_col1 + FL(i).N_level;   %
end

%  Ignore 1st col since it is total mean
%dmat_mean = dmat(:, (num_col0 + 1):(num_col0 + num_col1));

% Get the number in the sum for each mean, which happens to be in the diagonal of X'X.
% This can also be otained through the user input variables, but it is generic with
% the matrix operation, especially for unbalanced design.
%sum_num = diag(dmat_mean' * dmat_mean);


if (NF > 1),
   num_col2 = 0;
   for (i = 1:1:(NF-1)),
   for (j = (i+1):1:NF),
      num_col2 = num_col2 + FL(i).N_level*FL(j).N_level;    %Columns for 2nd order interactions
   end
   end
end

if (NF > 2),
   num_col3 = 0;
   for (i = 1:1:(NF-2)),
   for (j = (i+1):1:(NF-1)),
	for (k = (j+1):1:NF),
      num_col3 = num_col3 + FL(i).N_level*FL(j).N_level*FL(k).N_level;    %Columns for 3rd order interactions
   end
   end
	end
end

if (NF == 4),
   num_col4 = 1;
	for (i = 1:1:NF),
	   num_col4 = num_col4*FL(i).N_level;  %Columns for 4th order interactions
	end	
end


switch NF
   case 1,	
		if (Contr.ord1.tot > 0),		   
         for (i = 1:1:Contr.ord1.tot),
	         Contr.ord1.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
     	      for (j = 1:1:Contr.ord1.cnt(i).NT),
               first = 0;
		         Contr.ord1.cnt(i).code(j).pos = 0;
					shift = num_col0;   % Grand mean 
		         for (k = 1:1:NF),
                  if (Contr.ord1.cnt(i).code(j).str(k) >= 'a' & Contr.ord1.cnt(i).code(j).str(k) <= 'z'), 
						   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(k) - 'a';
						else tmpv = Contr.ord1.cnt(i).code(j).str(k) - '0'; end						
						
						if (tmpv ~= 0), 
	                  first = first + 1;  % the first non-zero index backward!
				         if (first == 1),
				            Contr.ord1.cnt(i).idx1 = k;
					         tmp = FL(k).N_level;
		               elseif (first > 1), 
		                  fprintf('\nError in contrast coding: more than one non-zero index in 1st order constrasts!\n');
		                  fprintf(2,'Halted: Ctrl+c to exit'); pause;
				         end
		            end
	            end
					if (Contr.ord1.cnt(i).idx1 > 1),
					for (k = 1:1:Contr.ord1.cnt(i).idx1-1),
					   shift = shift + FL(k).N_level;
					end					
					end
					
					if (Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) >= 'a' & Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) <= 'z'),
					   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - 'a';
					else tmpv = Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - '0'; end
					
		         Contr.ord1.cnt(i).code(j).pos = shift + tmpv; 
		         tmp = FL(Contr.ord1.cnt(i).idx1).N_level/n;
		         Contr.ord1.cnt(i).scalar = tmp * Contr.ord1.cnt(i).coef*Contr.ord1.cnt(i).coef';		
		         Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec + Contr.ord1.cnt(i).coef(j) * dmat(:, Contr.ord1.cnt(i).code(j).pos)';			
            end	
	      Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec*tmp;   % assuming balanced design!!!
         end   %for (i = 1:1:Contr1.tot)
      end   % if (Contr1.tot > 0)	
   
	case 2,
		if (Contr.ord1.tot > 0),		   
         for (i = 1:1:Contr.ord1.tot),
	         Contr.ord1.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
     	      for (j = 1:1:Contr.ord1.cnt(i).NT),
               first = 0;
		         Contr.ord1.cnt(i).code(j).pos = 0;
					shift = num_col0;   % Grand mean 
		         for (k = 1:1:NF),
                  if (Contr.ord1.cnt(i).code(j).str(k) >= 'a' & Contr.ord1.cnt(i).code(j).str(k) <= 'z'), 
						   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(k) - 'a';
						else tmpv = Contr.ord1.cnt(i).code(j).str(k) - '0'; end
						
						if (tmpv ~= 0), 
	                  first = first + 1;  % the first non-zero index backward!
				         if (first == 1),
				            Contr.ord1.cnt(i).idx1 = k;
					         tmp = FL(k).N_level;
		               elseif (first > 1), 
		                  fprintf('\nError in contrast coding: more than one non-zero index in 1st order constrasts!\n');
		                  fprintf(2,'Halted: Ctrl+c to exit'); pause;
				         end
		            end
	            end
					if (Contr.ord1.cnt(i).idx1 > 1),
					for (k = 1:1:Contr.ord1.cnt(i).idx1-1),
					   shift = shift + FL(k).N_level;
					end					
					end
					
					if (Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) >= 'a' & Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) <= 'z'),
					   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - 'a';
					else tmpv = Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - '0'; end
					
		         Contr.ord1.cnt(i).code(j).pos = shift + tmpv; 
		         tmp = FL(Contr.ord1.cnt(i).idx1).N_level/n;
		         Contr.ord1.cnt(i).scalar = tmp * Contr.ord1.cnt(i).coef*Contr.ord1.cnt(i).coef';		
		         Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec + Contr.ord1.cnt(i).coef(j) * dmat(:, Contr.ord1.cnt(i).code(j).pos)';			
            end	
	      Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec*tmp;   % assuming balanced design!!!
         end   %for (i = 1:1:Contr1.tot)
      end   % if (Contr1.tot > 0)		

	   if (Contr.ord2.tot > 0),
		   shift = num_col0 + num_col1;   % Grand mean plus factor means
         for (i = 1:1:Contr.ord2.tot),   
	         
            % for the two non-zero indices
	         Contr.ord2.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
	         for (j = 1:1:Contr.ord2.cnt(i).NT),
               first = 0;
		         Contr.ord2.cnt(i).code(j).pos = 0;
		         for (k = 1:1:NF),
                  if (Contr.ord2.cnt(i).code(j).str(k) >= 'a' & Contr.ord2.cnt(i).code(j).str(k) <= 'z'), 
						   tmpv = 10 + Contr.ord2.cnt(i).code(j).str(k) - 'a';
						else tmpv = Contr.ord2.cnt(i).code(j).str(k) - '0'; end
						
						if (tmpv ~= 0), 
         	         first = first + 1;  % the first non-zero index backward!
			         	if (first == 1),
				            Contr.ord2.cnt(i).idx1 = k;
         					tmp = FL(k).N_level;
		               elseif (first == 2), Contr.ord2.cnt(i).idx2 = k;
         				end
		            end
	            end  %for (k = 1:1:NF)
		         switch Contr.ord2.cnt(i).idx1
		            case 1,
			            switch Contr.ord2.cnt(i).idx2
				            case 2, 
					            if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
									   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					            else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
									if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
									   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					            else tmpv2 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
									
									Contr.ord2.cnt(i).code(j).pos = shift + (tmpv1 - 1) * FL(2).N_level + tmpv2; % AB
						         tmp = FL(1).N_level*FL(2).N_level/n;
					         case 3, 
					            if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
									   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					            else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
									if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
									   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					            else tmpv2 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end									
									
									Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*FL(2).N_level+ ... 
					               (tmpv1 - 1) * FL(3).N_level + tmpv2; % AC	
						         tmp = FL(1).N_level*FL(3).N_level/n;	
				         end				
			         case 2,							
							if (Contr.ord2.cnt(i).idx2 == 3),
					            if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
									   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					            else tmpv1 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
									if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
									   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					            else tmpv2 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end	
									
									Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*(FL(2).N_level + FL(3).N_level) + ...
					               (tmpv1 - 1) * FL(3).N_level + tmpv2; % BC
						         tmp = FL(2).N_level*FL(3).N_level/n; 
							else fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause; end
			         case 3, 
			            fprintf('\nSomething is wrong in the contrast coding!\n');
		               fprintf(2,'Halted: Ctrl+c to exit'); pause;			
		         end  %switch Contr.ord2.cnt(i).idx1
		         Contr.ord2.cnt(i).scalar = tmp * Contr.ord2.cnt(i).coef*Contr.ord2.cnt(i).coef';		
		         Contr.ord2.cnt(i).vec = Contr.ord2.cnt(i).vec + Contr.ord2.cnt(i).coef(j) * dmat(:, Contr.ord2.cnt(i).code(j).pos)';			
            end	 %for (j = 1:1:Contr.ord2.cnt(i).NT),
	         Contr.ord2.cnt(i).vec = Contr.ord2.cnt(i).vec*tmp;   % assuming balanced design!!!
         end   %for (i = 1:1:Contr2.tot)
      end   % if (Contr2.tot > 0)		
	
	case 3,
	   if (Contr.ord1.tot > 0),		   
         for (i = 1:1:Contr.ord1.tot),
	         Contr.ord1.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
     	      for (j = 1:1:Contr.ord1.cnt(i).NT),
               first = 0;
		         Contr.ord1.cnt(i).code(j).pos = 0;
					shift = num_col0;   % Grand mean 
		         for (k = 1:1:NF),
                  if (Contr.ord1.cnt(i).code(j).str(k) >= 'a' & Contr.ord1.cnt(i).code(j).str(k) <= 'z'), 
						   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(k) - 'a';
						else tmpv = Contr.ord1.cnt(i).code(j).str(k) - '0'; end
						
						if (tmpv ~= 0), 
	                  first = first + 1;  % the first non-zero index backward!
				         if (first == 1),
				            Contr.ord1.cnt(i).idx1 = k;
					         tmp = FL(k).N_level;
		               elseif (first > 1), 
		                  fprintf('\nError in contrast coding: more than one non-zero index in 1st order constrasts!\n');
		                  fprintf(2,'Halted: Ctrl+c to exit'); pause;
				         end
		            end
	            end
					if (Contr.ord1.cnt(i).idx1 > 1),
					for (k = 1:1:Contr.ord1.cnt(i).idx1-1),
					   shift = shift + FL(k).N_level;
					end					
					end
					if (Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) >= 'a' & Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) <= 'z'),
					   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - 'a';
					else tmpv = Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - '0'; end
					
		         Contr.ord1.cnt(i).code(j).pos = shift + tmpv; 
		         tmp = FL(Contr.ord1.cnt(i).idx1).N_level/n;
		         Contr.ord1.cnt(i).scalar = tmp * Contr.ord1.cnt(i).coef*Contr.ord1.cnt(i).coef';		
		         Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec + Contr.ord1.cnt(i).coef(j) * dmat(:, Contr.ord1.cnt(i).code(j).pos)';			
            end	
	      Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec*tmp;   % assuming balanced design!!!
         end   %for (i = 1:1:Contr1.tot)
      end   % if (Contr1.tot > 0)
		
	   if (Contr.ord2.tot > 0),
		   shift = num_col0 + num_col1;   % Grand mean plus factor means
         for (i = 1:1:Contr.ord2.tot),   
	         
            % for the two non-zero indices
	         Contr.ord2.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
	         for (j = 1:1:Contr.ord2.cnt(i).NT),
               first = 0;
		         Contr.ord2.cnt(i).code(j).pos = 0;
		         for (k = 1:1:NF),
                  if (Contr.ord2.cnt(i).code(j).str(k) >= 'a' & Contr.ord2.cnt(i).code(j).str(k) <= 'z'), 
						   tmpv = 10 + Contr.ord2.cnt(i).code(j).str(k) - 'a';
						else tmpv = Contr.ord2.cnt(i).code(j).str(k) - '0'; end
						
						if (tmpv ~= 0), 
         	         first = first + 1;  % the first non-zero index backward!
			         	if (first == 1),
				            Contr.ord2.cnt(i).idx1 = k;
         					tmp = FL(k).N_level;
		               elseif (first == 2), Contr.ord2.cnt(i).idx2 = k;
         				end
		            end
	            end  %for (k = 1:1:NF)
		         switch Contr.ord2.cnt(i).idx1
		            case 1,
			            switch Contr.ord2.cnt(i).idx2
				            case 2, 
					            if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
									   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					            else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
									if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
									   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					            else tmpv2 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
									
									Contr.ord2.cnt(i).code(j).pos = shift + (tmpv1 - 1) * FL(2).N_level + tmpv2; % AB
						         tmp = FL(1).N_level*FL(2).N_level/n;
					         case 3, 
					            if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
									   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					            else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
									if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
									   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					            else tmpv2 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end									
									
									Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*FL(2).N_level+ ... 
					               (tmpv1 - 1) * FL(3).N_level + tmpv2; % AC	
						         tmp = FL(1).N_level*FL(3).N_level/n;	
				         end				
			         case 2,
			            if (Contr.ord2.cnt(i).idx2 == 3),
					            if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
									   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					            else tmpv1 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
									if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
									   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					            else tmpv2 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end	
									
									Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*(FL(2).N_level + FL(3).N_level) + ...
					               (tmpv1 - 1) * FL(3).N_level + tmpv2; % BC
						         tmp = FL(2).N_level*FL(3).N_level/n; 
							else fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause; end
			         case 3, 
			            fprintf('\nSomething is wrong in the contrast coding!\n');
		               fprintf(2,'Halted: Ctrl+c to exit'); pause;			
		         end  %switch Contr.ord2.cnt(i).idx1
		         Contr.ord2.cnt(i).scalar = tmp * Contr.ord2.cnt(i).coef*Contr.ord2.cnt(i).coef';		
		         Contr.ord2.cnt(i).vec = Contr.ord2.cnt(i).vec + Contr.ord2.cnt(i).coef(j) * dmat(:, Contr.ord2.cnt(i).code(j).pos)';			
            end	 %for (j = 1:1:Contr.ord2.cnt(i).NT),
	         Contr.ord2.cnt(i).vec = Contr.ord2.cnt(i).vec*tmp;   % assuming balanced design!!!
         end   %for (i = 1:1:Contr2.tot)
      end   % if (Contr2.tot > 0)	
	
	if (Contr.ord3.tot > 0),
shift = num_col0 + num_col1 + num_col2;   % skip grand mean, factor means and 2nd-roder terms
for (i = 1:1:Contr.ord3.tot),   

% for the  non-zero indices
	Contr.ord3.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
	for (j = 1:1:Contr.ord3.cnt(i).NT),
      first = 0;
		Contr.ord3.cnt(i).code(j).pos = 0;
		for (k = 1:1:NF),
         if (Contr.ord3.cnt(i).code(j).str(k) >= 'a' & Contr.ord3.cnt(i).code(j).str(k) <= 'z'), 
				tmpv = 10 + Contr.ord3.cnt(i).code(j).str(k) - 'a';
			else tmpv = Contr.ord3.cnt(i).code(j).str(k) - '0'; end
			
			if (tmpv ~= 0), 
	         first = first + 1;  % the first non-zero index backward!
				if (first == 1),
				   Contr.ord3.cnt(i).idx1 = k;
					tmp = FL(k).N_level;
		      elseif (first == 2), 
				   Contr.ord3.cnt(i).idx2 = k;
				end
				if (first == 3), Contr.ord3.cnt(i).idx3 = k; end
		   end
	   end
		switch Contr.ord3.cnt(i).idx1
		   case 1,
			   switch Contr.ord3.cnt(i).idx2
				   case 2,
					   if (Contr.ord3.cnt(i).idx3 == 3),
						   if (Contr.ord3.cnt(i).code(j).str(1) >= 'a' & Contr.ord3.cnt(i).code(j).str(1) <= 'z'),
								tmpv1 = 10 + Contr.ord3.cnt(i).code(j).str(1) - 'a';
					      else tmpv1 = Contr.ord3.cnt(i).code(j).str(1) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(2) >= 'a' & Contr.ord3.cnt(i).code(j).str(2) <= 'z'),
							   tmpv2 = 10 + Contr.ord3.cnt(i).code(j).str(2) - 'a';
					      else tmpv2 = Contr.ord3.cnt(i).code(j).str(2) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(3) >= 'a' & Contr.ord3.cnt(i).code(j).str(3) <= 'z'),
							   tmpv3 = 10 + Contr.ord3.cnt(i).code(j).str(3) - 'a';
					      else tmpv3 = Contr.ord3.cnt(i).code(j).str(3) - '0'; end
							
							Contr.ord3.cnt(i).code(j).pos = shift + (tmpv1 - 1) * FL(2).N_level * FL(3).N_level + ...
					         (tmpv2 - 1) * FL(3).N_level + tmpv3; % ABC
							tmp = FL(1).N_level*FL(2).N_level*FL(3).N_level/n;
						else fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause;		
						end  %if (Contr.ord3.cnt(i).idx3 == 3)
					case 3, fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause;					
				end	%switch Contr.ord3.cnt(i).idx2			
			case 2,
			   fprintf('\nSomething is wrong in the contrast coding!\n');
		      fprintf(2,'Halted: Ctrl+c to exit'); pause;			
		end %switch Contr.ord3.cnt(i).idx1
		Contr.ord3.cnt(i).scalar = tmp * Contr.ord3.cnt(i).coef*Contr.ord3.cnt(i).coef';		
		Contr.ord3.cnt(i).vec = Contr.ord3.cnt(i).vec + Contr.ord3.cnt(i).coef(j) * dmat(:, Contr.ord3.cnt(i).code(j).pos)';			
   end	%for (j = 1:1:Contr.ord3.cnt(i).NT)
	Contr.ord3.cnt(i).vec = Contr.ord3.cnt(i).vec*tmp;   % assuming balanced design!!!
end   %for (i = 1:1:Contr3.tot)
end   % if (Contr3.tot > 0)
	
	case 4,

% dmat(:, num_col0+1:num_col0+num_col1) is the matrix for 1st order contrasts
	   if (Contr.ord1.tot > 0),		   
         for (i = 1:1:Contr.ord1.tot),
	         Contr.ord1.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
     	      for (j = 1:1:Contr.ord1.cnt(i).NT),
               first = 0;
		         Contr.ord1.cnt(i).code(j).pos = 0;
					shift = num_col0;   % Grand mean 
		         for (k = 1:1:NF),
                  if (Contr.ord1.cnt(i).code(j).str(k) >= 'a' & Contr.ord1.cnt(i).code(j).str(k) <= 'z'), 
						   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(k) - 'a';
						else tmpv = Contr.ord1.cnt(i).code(j).str(k) - '0'; end
						
						if (tmpv ~= 0), 
	                  first = first + 1;  % the first non-zero index backward!
				         if (first == 1),
				            Contr.ord1.cnt(i).idx1 = k;
					         tmp = FL(k).N_level;
		               elseif (first > 1), 
		                  fprintf('\nError in contrast coding: more than one non-zero index in 1st order constrasts!\n');
		                  fprintf(2,'Halted: Ctrl+c to exit'); pause;
				         end
		            end
	            end
					if (Contr.ord1.cnt(i).idx1 > 1),
					for (k = 1:1:Contr.ord1.cnt(i).idx1-1),
					   shift = shift + FL(k).N_level;
					end					
					end
		         if (Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) >= 'a' & Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) <= 'z'),
					   tmpv = 10 + Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - 'a';
					else tmpv = Contr.ord1.cnt(i).code(j).str(Contr.ord1.cnt(i).idx1) - '0'; end
					
		         Contr.ord1.cnt(i).code(j).pos = shift + tmpv;
		         tmp = FL(Contr.ord1.cnt(i).idx1).N_level/n;
		         Contr.ord1.cnt(i).scalar = tmp * Contr.ord1.cnt(i).coef*Contr.ord1.cnt(i).coef';		
		         Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec + Contr.ord1.cnt(i).coef(j) * dmat(:, Contr.ord1.cnt(i).code(j).pos)';			
            end	
	      Contr.ord1.cnt(i).vec = Contr.ord1.cnt(i).vec*tmp;   % assuming balanced design!!!
         end   %for (i = 1:1:Contr1.tot)
      end   % if (Contr1.tot > 0)



% dmat(:, num_col0+num_col1+1:num_col0+num_col1+num_col2) is the matrix for 2nd order contrasts

if (Contr.ord2.tot > 0),
shift = num_col0 + num_col1;   % Grand mean plus factor means
for (i = 1:1:Contr.ord2.tot),   

% when the 1st factor is collapsed 
%   shift1 = (FL(1).N_level*(FL(2).N_level + FL(3).N_level + FL(4).N_level))*(str2num(Contr2.cnt(i).code(1).str(1)) == 0);
% when the 2nd factor is collapsed 
%   shift2 = (FL(2).N_level*(FL(3).N_level + FL(4).N_level))*(str2num(Contr2.cnt(i).code(1).str(2)) == 0);
% when the 3rd factor is collapsed 
%   shift3 = (FL(3).N_level*FL(4).N_level)*(str2num(Contr2.cnt(i).code(1).str(3)) == 0);
%
% when the 4th factor is collapsed 
%   shift4 = FL(4).N_level*(str2num(Contr2.cnt(i).code(1).str(4)) == 0);
%
%   Contr2.cnt(i).shift = num_col0 + num_col1 + 1 + shift1 + shift2 + shift3 + shift4;  % position shifting for the two collapsed indices

% for the two non-zero indices
	Contr.ord2.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
	for (j = 1:1:Contr.ord2.cnt(i).NT),
      first = 0;
		Contr.ord2.cnt(i).code(j).pos = 0;
		for (k = 1:1:NF),
         if (Contr.ord2.cnt(i).code(j).str(k) >= 'a' & Contr.ord2.cnt(i).code(j).str(k) <= 'z'), 
			   tmpv = 10 + Contr.ord2.cnt(i).code(j).str(k) - 'a';
			else tmpv = Contr.ord2.cnt(i).code(j).str(k) - '0'; end
						
			if (tmpv ~= 0), 						 
	         first = first + 1;  % the first non-zero index backward!
		      % if (first == 1), tmp = str2num(Contr2.cnt(i).code(j).str(k)) - 1;	  % (level# - 1) for the first collapsed index	   		
				% elseif (first == 2), Contr2.cnt(i).code(j).shift = Contr2.cnt(i).code(j).shift + tmp * FL(k).N_level str2num(Contr2.cnt(i).code(j).str(k);
				if (first == 1),
				   Contr.ord2.cnt(i).idx1 = k;
					%Contr2.cnt(i).code(j).pos = Contr2.cnt(i).shift + str2num(Contr2.cnt(i).code(j).str(k)) - 1; 
					tmp = FL(k).N_level;
		      elseif (first == 2), 
				   Contr.ord2.cnt(i).idx2 = k;
					%Contr2.cnt(i).code(j).pos = Contr2.cnt(i).code(j).pos + (str2num(Contr2.cnt(i).code(j).str(k)) - 1) * tmp;
		      %else fprintf('\nError in contrast coding: more than two non-zero indices!\n');
		      %   fprintf(2,'Halted: Ctrl+c to exit'); pause;
				end
		   end
	   end
		switch Contr.ord2.cnt(i).idx1
		   case 1,
			   switch Contr.ord2.cnt(i).idx2
				   case 2, 
					   if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
						   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					   else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
						if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
						   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					   else tmpv2 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
									
						Contr.ord2.cnt(i).code(j).pos = shift + (tmpv1 - 1) * FL(2).N_level + tmpv2; % AB
						tmp = FL(1).N_level*FL(2).N_level/n;
					case 3, 
					   if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
						   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					   else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
						if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
						   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					   else tmpv2 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end									
									
						Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*FL(2).N_level+ ... 
					   (tmpv1 - 1) * FL(3).N_level + tmpv2; % AC	
						tmp = FL(1).N_level*FL(3).N_level/n;	
					case 4, 
					   if (Contr.ord2.cnt(i).code(j).str(1) >= 'a' & Contr.ord2.cnt(i).code(j).str(1) <= 'z'),
						   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(1) - 'a';
					   else tmpv1 = Contr.ord2.cnt(i).code(j).str(1) - '0'; end
						if (Contr.ord2.cnt(i).code(j).str(4) >= 'a' & Contr.ord2.cnt(i).code(j).str(4) <= 'z'),
						   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(4) - 'a';
					   else tmpv2 = Contr.ord2.cnt(i).code(j).str(4) - '0'; end	
						
						Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*(FL(2).N_level+FL(3).N_level) + ...
					      (tmpv1 - 1) * FL(4).N_level + tmpv2; % AD
						tmp = FL(1).N_level*FL(4).N_level/n;
				end				
			case 2,
			   switch Contr.ord2.cnt(i).idx2
				   case 3, 
					   if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
						   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					   else tmpv1 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
						if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
						   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					   else tmpv2 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end	
									
						Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*(FL(2).N_level + FL(3).N_level) + ...
					               (tmpv1 - 1) * FL(3).N_level + tmpv2; % BC
						tmp = FL(2).N_level*FL(3).N_level/n; 
					case 4, 
					   if (Contr.ord2.cnt(i).code(j).str(2) >= 'a' & Contr.ord2.cnt(i).code(j).str(2) <= 'z'),
						   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(2) - 'a';
					   else tmpv1 = Contr.ord2.cnt(i).code(j).str(2) - '0'; end
						if (Contr.ord2.cnt(i).code(j).str(4) >= 'a' & Contr.ord2.cnt(i).code(j).str(4) <= 'z'),
						   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(4) - 'a';
					   else tmpv2 = Contr.ord2.cnt(i).code(j).str(4) - '0'; end
						
						Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*(FL(2).N_level + FL(3).N_level + FL(4).N_level) + ...
					      FL(2).N_level*FL(3).N_level + (tmpv1 - 1) * FL(4).N_level + tmpv2; % BD
						tmp = FL(2).N_level*FL(4).N_level/n;
			   end			
			case 3, 
			   if (Contr.ord2.cnt(i).idx2 == 4),
				   if (Contr.ord2.cnt(i).code(j).str(3) >= 'a' & Contr.ord2.cnt(i).code(j).str(3) <= 'z'),
						   tmpv1 = 10 + Contr.ord2.cnt(i).code(j).str(3) - 'a';
					   else tmpv1 = Contr.ord2.cnt(i).code(j).str(3) - '0'; end
						if (Contr.ord2.cnt(i).code(j).str(4) >= 'a' & Contr.ord2.cnt(i).code(j).str(4) <= 'z'),
						   tmpv2 = 10 + Contr.ord2.cnt(i).code(j).str(4) - 'a';
					   else tmpv2 = Contr.ord2.cnt(i).code(j).str(4) - '0'; end					
					
					Contr.ord2.cnt(i).code(j).pos = shift + FL(1).N_level*(FL(2).N_level + FL(3).N_level + FL(4).N_level) + ...
					   FL(2).N_level*(FL(3).N_level + FL(4).N_level) + (tmpv1 - 1) * FL(4).N_level + tmpv2; % CD
					tmp = FL(3).N_level*FL(4).N_level/n; 
				else fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause; end 			
			case 4,
			   fprintf('\nSomething is wrong in the contrast coding!\n');
		      fprintf(2,'Halted: Ctrl+c to exit'); pause;			
		end
		Contr.ord2.cnt(i).scalar = tmp * Contr.ord2.cnt(i).coef*Contr.ord2.cnt(i).coef';		
		Contr.ord2.cnt(i).vec = Contr.ord2.cnt(i).vec + Contr.ord2.cnt(i).coef(j) * dmat(:, Contr.ord2.cnt(i).code(j).pos)';			
   end	
	Contr.ord2.cnt(i).vec = Contr.ord2.cnt(i).vec*tmp;   % assuming balanced design!!!
end   %for (i = 1:1:Contr2.tot)
end   % if (Contr2.tot > 0)

if (Contr.ord3.tot > 0),
shift = num_col0 + num_col1 + num_col2;   % skip grand mean, factor means and 2nd-roder terms
for (i = 1:1:Contr.ord3.tot),   

% for the  non-zero indices
	Contr.ord3.cnt(i).vec = zeros(1, n);  % n = ntot in anovan.m: total number of files, and no. of arrows in dmat
	for (j = 1:1:Contr.ord3.cnt(i).NT),
      first = 0;
		Contr.ord3.cnt(i).code(j).pos = 0;
		for (k = 1:1:NF),
         if (Contr.ord3.cnt(i).code(j).str(k) >= 'a' & Contr.ord3.cnt(i).code(j).str(k) <= 'z'), 
				tmpv = 10 + Contr.ord3.cnt(i).code(j).str(k) - 'a';
			else tmpv = Contr.ord3.cnt(i).code(j).str(k) - '0'; end
			
			if (tmpv ~= 0),  
	         first = first + 1;  % the first non-zero index backward!
		      % if (first == 1), tmp = str2num(Contr3.cnt(i).code(j).str(k)) - 1;	  % (level# - 1) for the first collapsed index	   		
				% elseif (first == 2), Contr3.cnt(i).code(j).shift = Contr3.cnt(i).code(j).shift + tmp * FL(k).N_level str2num(Contr3.cnt(i).code(j).str(k);
				if (first == 1),
				   Contr.ord3.cnt(i).idx1 = k;
					%Contr3.cnt(i).code(j).pos = Contr3.cnt(i).shift + str2num(Contr3.cnt(i).code(j).str(k)) - 1; 
					tmp = FL(k).N_level;
		      elseif (first == 2), 
				   Contr.ord3.cnt(i).idx2 = k;
					%Contr3.cnt(i).code(j).pos = Contr3.cnt(i).code(j).pos + (str2num(Contr3.cnt(i).code(j).str(k)) - 1) * tmp;
		      %else fprintf('\nError in contrast coding: more than two non-zero indices!\n');
		      %   fprintf(2,'Halted: Ctrl+c to exit'); pause;
				end
				if (first == 3), Contr.ord3.cnt(i).idx3 = k; end
		   end
	   end
		switch Contr.ord3.cnt(i).idx1
		   case 1,
			   switch Contr.ord3.cnt(i).idx2
				   case 2,
					   switch Contr.ord3.cnt(i).idx3
						   case 3, % ABC
							
							if (Contr.ord3.cnt(i).code(j).str(1) >= 'a' & Contr.ord3.cnt(i).code(j).str(1) <= 'z'),
								tmpv1 = 10 + Contr.ord3.cnt(i).code(j).str(1) - 'a';
					      else tmpv1 = Contr.ord3.cnt(i).code(j).str(1) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(2) >= 'a' & Contr.ord3.cnt(i).code(j).str(2) <= 'z'),
							   tmpv2 = 10 + Contr.ord3.cnt(i).code(j).str(2) - 'a';
					      else tmpv2 = Contr.ord3.cnt(i).code(j).str(2) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(3) >= 'a' & Contr.ord3.cnt(i).code(j).str(3) <= 'z'),
							   tmpv3 = 10 + Contr.ord3.cnt(i).code(j).str(3) - 'a';
					      else tmpv3 = Contr.ord3.cnt(i).code(j).str(3) - '0'; end
							
							Contr.ord3.cnt(i).code(j).pos = shift + (tmpv1 - 1) * FL(2).N_level * FL(3).N_level + ...
					         (tmpv2 - 1) * FL(3).N_level + tmpv3; % ABC
							tmp = FL(1).N_level*FL(2).N_level*FL(3).N_level/n;
							
							case 4, % ABD
							
							if (Contr.ord3.cnt(i).code(j).str(1) >= 'a' & Contr.ord3.cnt(i).code(j).str(1) <= 'z'),
								tmpv1 = 10 + Contr.ord3.cnt(i).code(j).str(1) - 'a';
					      else tmpv1 = Contr.ord3.cnt(i).code(j).str(1) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(2) >= 'a' & Contr.ord3.cnt(i).code(j).str(2) <= 'z'),
							   tmpv2 = 10 + Contr.ord3.cnt(i).code(j).str(2) - 'a';
					      else tmpv2 = Contr.ord3.cnt(i).code(j).str(2) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(4) >= 'a' & Contr.ord3.cnt(i).code(j).str(4) <= 'z'),
							   tmpv3 = 10 + Contr.ord3.cnt(i).code(j).str(4) - 'a';
					      else tmpv3 = Contr.ord3.cnt(i).code(j).str(4) - '0'; end
							
							
							Contr.ord3.cnt(i).code(j).pos = shift + FL(1).N_level * FL(2).N_level * FL(3).N_level + ...
							   (tmpv1 - 1) * FL(2).N_level * FL(4).N_level + (tmpv2 - 1) * FL(4).N_level + tmpv3; % ABD
							   tmp = FL(1).N_level*FL(2).N_level*FL(4).N_level/n;
						end
					case 3, 
					   if (Contr.ord3.cnt(i).idx3 == 4),  % ACD
						   if (Contr.ord3.cnt(i).code(j).str(1) >= 'a' & Contr.ord3.cnt(i).code(j).str(1) <= 'z'),
								tmpv1 = 10 + Contr.ord3.cnt(i).code(j).str(1) - 'a';
					      else tmpv1 = Contr.ord3.cnt(i).code(j).str(1) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(3) >= 'a' & Contr.ord3.cnt(i).code(j).str(3) <= 'z'),
							   tmpv2 = 10 + Contr.ord3.cnt(i).code(j).str(3) - 'a';
					      else tmpv2 = Contr.ord3.cnt(i).code(j).str(3) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(4) >= 'a' & Contr.ord3.cnt(i).code(j).str(4) <= 'z'),
							   tmpv3 = 10 + Contr.ord3.cnt(i).code(j).str(4) - 'a';
					      else tmpv3 = Contr.ord3.cnt(i).code(j).str(4) - '0'; end
							
							Contr.ord3.cnt(i).code(j).pos = shift + FL(1).N_level * FL(2).N_level * FL(3).N_level + FL(1).N_level * FL(2).N_level * FL(4).N_level + ...
							   (tmpv1 - 1) * FL(3).N_level * FL(4).N_level + (tmpv2 - 1) * FL(4).N_level + tmpv3; % ACD
							tmp = FL(1).N_level*FL(3).N_level*FL(4).N_level/n;
						else fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause;		
						end
					case 4, fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause;					
				end				
			case 2,
			   switch Contr3.cnt(i).idx2
				   case 3, 
					   if (Contr.ord3.cnt(i).idx3 == 4),	% BCD				
					      if (Contr.ord3.cnt(i).code(j).str(2) >= 'a' & Contr.ord3.cnt(i).code(j).str(2) <= 'z'),
								tmpv1 = 10 + Contr.ord3.cnt(i).code(j).str(2) - 'a';
					      else tmpv1 = Contr.ord3.cnt(i).code(j).str(2) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(3) >= 'a' & Contr.ord3.cnt(i).code(j).str(3) <= 'z'),
							   tmpv2 = 10 + Contr.ord3.cnt(i).code(j).str(3) - 'a';
					      else tmpv2 = Contr.ord3.cnt(i).code(j).str(3) - '0'; end
							if (Contr.ord3.cnt(i).code(j).str(4) >= 'a' & Contr.ord3.cnt(i).code(j).str(4) <= 'z'),
							   tmpv3 = 10 + Contr.ord3.cnt(i).code(j).str(4) - 'a';
					      else tmpv3 = Contr.ord3.cnt(i).code(j).str(4) - '0'; end
							
							Contr3.cnt(i).code(j).pos = shift + FL(1).N_level*FL(2).N_level*FL(3).N_level + FL(1).N_level*FL(2).N_level*FL(4).N_level + ...
						      FL(1).N_level*FL(3).N_level*FL(4).N_level + (tmpv1 - 1) * FL(3).N_level * FL(4).N_level +...
								(tmpv2 - 1)* FL(4).N_level + tmpv3; % BCD
						   tmp = FL(2).N_level*FL(3).N_level*FL(4).N_level/n; 
						else fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause;		
						end	
					case 4, fprintf('\nSomething is wrong in the contrast coding!\n'); fprintf(2,'Halted: Ctrl+c to exit'); pause;
				end	
			case 3, 
			   fprintf('\nSomething is wrong in the contrast coding!\n');
		      fprintf(2,'Halted: Ctrl+c to exit'); pause;			
			case 4,
			   fprintf('\nSomething is wrong in the contrast coding!\n');
		      fprintf(2,'Halted: Ctrl+c to exit'); pause;			
		end
		Contr.ord3.cnt(i).scalar = tmp * Contr.ord3.cnt(i).coef*Contr.ord3.cnt(i).coef';		
		Contr.ord3.cnt(i).vec = Contr.ord3.cnt(i).vec + Contr.ord3.cnt(i).coef(j) * dmat(:, Contr.ord3.cnt(i).code(j).pos)';			
   end	
	Contr.ord3.cnt(i).vec = Contr.ord3.cnt(i).vec*tmp;   % assuming balanced design!!!
end   %for (i = 1:1:Contr3.tot)
end   % if (Contr3.tot > 0)

end % switch NF

end % if (Contr.do == 1)

err = 0;
return;


% --------------------------

% --------------------------
