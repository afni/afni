function [R] = PeakFinder(vvec, Opt)
%Example: PeakFinder('Resp*.1D');
% or PeakFinder(v) where v is a column vector
% if v is a matrix, each column is processed separately.
%
%clear all but vvec (useful if I run this function as as script)
keep('vvec', 'Opt');

if (nargin < 2) Opt = struct(); end
if (~isfield(Opt,'fs')  | isempty(Opt.fs)),
   Opt.fs= 1/0.025; %sampling frequency
end
if (~isfield(Opt,'zerophaseoffset') | isempty(Opt.zerophaseoffset) ),
   Opt.zerophaseoffset = 0.5;  %Fraction of the period that corresponds 
                           %to a phase of 0
                       %0.5 means the middle of the period, 0 means the 1st peak
end
if (~isfield(Opt,'quiet') | isempty(Opt.quiet)),
   Opt.quiet = 0;
end
if (~isfield(Opt,'sTR') | isempty(Opt.slcTR)),
   Opt.slcTR = 1./Opt.fs;
end
if (~isfield(Opt,'fcutoff') | isempty(Opt.fcutoff)),
   Opt.fcutoff = 10;
end
if (~isfield(Opt,'fir_order') | isempty(Opt.fir_order)),
   Opt.fir_order = 40;
end
if (~isfield(Opt,'resam_kernel') | isempty(Opt.resam_kernel)),
   Opt.resam_kernel = 'linear';
end
if (~isfield(Opt,'demo') | isempty(Opt.demo)),
   Opt.demo = 0;
end

if (Opt.demo),
   Opt.quiet = 0; 
else 
   pause off 
end

%some filtering
fnyq = Opt.fs./2;
w = Opt.fcutoff/fnyq;    % cut off frequency normalized
b = fir1(Opt.fir_order, w);     %FIR filter of order 40
NoDups = 1; % remove duplicates that might come up when improving peak location
  
if (ischar(vvec)),
   l = zglobb(vvec);
   nl = length(l);
else
   l = [];
   nl = size(vvec,2);
end

R(nl) = struct( 'vname', '',...
            't', [], ...
            'X', [],...
            'iz', [],...   %zero crossing (peak) locations
            'ptrace', [], 'tptrace', [],...
            'ntrace', [], 'tntrace', [],...
            'prd', [], 'tmidprd', [], 'ptracemidprd', [],...
            'phz', [],...
            'RV', [], 'RVT', [] ...
             );
             
for (icol = 1:1:nl),

   if (~isempty(l) && ~l(icol).isdir),
      R(icol).vname = l(icol).name;
      v = Read_1D(R(icol).vname);
   else,
      R(icol).vname = sprintf('vector input col %d', icol);
      v = vvec(:,icol);
   end

   windwidth = 0.2; %window for adjusting peak location in seconds

   
   %remove the mean
   v = (v - mean(v));
   R(icol).v = v;      %store it for debugging
   
   %filter both ways to cancel phase shift
   v = filter(b,1,v); v = flipud(v); v = filter(b,1,v); v = flipud(v);
   
   %get the analytic signal
   R(icol).X = analytic_signal(v); %using local version to illustrate, 
                                 %can use hilbert
   
   nt = length(R(icol).X);
   R(icol).t = [0:1/Opt.fs:(nt-1)/Opt.fs];
   iz = find( imag(R(icol).X(1:nt-1)).*imag(R(icol).X(2:nt)) <= 0);
   polall = -sign(imag(R(icol).X(1:nt-1)) - imag(R(icol).X(2:nt)));

   pk = real(R(icol).X(iz));
   pol = polall(iz);
   tiz = R(icol).t(iz);


   ppp = find(pol>0);
   ptrace = pk(ppp);
   tptrace = tiz(ppp);
   ppp = find(pol<0);
   ntrace = pk(ppp);
   tntrace = tiz(ppp);
   if (~Opt.quiet),
      fprintf(2,[ '--> Load signal\n',...
                  '--> Smooth signal\n',...
                  '--> Calculate analytic signal Z\n',...
                  '--> Find zero crossing of imag(Z)\n',...
                  '\n']);
      
      figure(1); clf
      subplot(211);
      plot (R(icol).t, real(R(icol).X),'g'); hold on
      %plot (R(icol).t, imag(R(icol).X),'g'); 
      plot (tptrace, ptrace, 'ro'); 
      plot (tntrace, ntrace, 'bo'); 
      %plot (R(icol).t, abs(R(icol).X),'k');   
      
      subplot (413); 
      vn = real(R(icol).X)./(abs(R(icol).X)+eps);
      plot (R(icol).t, vn, 'g'); hold on
      ppp = find(pol>0);
      plot (tiz(ppp), vn(iz(ppp)), 'ro'); 
      ppp = find(pol<0);
      plot (tiz(ppp), vn(iz(ppp)), 'bo'); 
      
      pause;
   end
   


   %Some polishing
   if (1),
      nww = ceil(windwidth/2 * Opt.fs);
      pkp = pk;
      R(icol).iz = iz;
      for (i=1:1:length(iz)),
         n0 = max(2,iz(i)-nww);
         n1 = min(nt,iz(i)+nww);
         if (pol(i) > 0),
            [xx, ixx] = max((real(R(icol).X(n0:n1))));
         else,
            [xx, ixx] = min((real(R(icol).X(n0:n1))));
         end
         R(icol).iz(i) = n0+ixx-2;
         pkp(i) = xx;
      end
      tizp = R(icol).t(R(icol).iz);

      ppp = find(pol>0);
      R(icol).ptrace = pkp(ppp);
      R(icol).tptrace = tizp(ppp);
      ppp = find(pol<0);
      R(icol).ntrace = pkp(ppp);
      R(icol).tntrace = tizp(ppp);
      
      if (NoDups),
      %remove duplicates
         [R(icol).tptrace, R(icol).ptrace] = ...
                     remove_duplicates(R(icol).tptrace, R(icol).ptrace);
         [R(icol).tntrace, R(icol).ntrace] = ...
                     remove_duplicates(R(icol).tntrace, R(icol).ntrace);
      end
      
      if (~Opt.quiet),
         fprintf(2,[ '--> Improved peak location\n',...
                     '--> Removed duplicates (not necessary)?\n',...
                     '\n']);
         subplot(211);
         plot( R(icol).tptrace, R(icol).ptrace,'r+',...
               R(icol).tptrace, R(icol).ptrace,'r');
         plot( R(icol).tntrace, R(icol).ntrace,'b+',...
               R(icol).tntrace, R(icol).ntrace,'b');
         pause;
      end
   else
      tizp = tiz;
      R(icol).iz = iz;
      pkp = pk;
      R(icol).ptrace = ptrace;
      nR(icol).ptrace = nptrace;
   end

   
   %Calculate the period
   nptrc = length(R(icol).tptrace);
   R(icol).prd = (R(icol).tptrace(2:nptrc) - R(icol).tptrace(1:nptrc-1) );
   R(icol).ptracemidprd = (   R(icol).ptrace(2:nptrc) ...
                            + R(icol).ptrace(1:nptrc-1) ) ./2.0;
   R(icol).tmidprd = (  R(icol).tptrace(2:nptrc) ...
                      + R(icol).tptrace(1:nptrc-1)) ./2.0;
   if (~Opt.quiet),
         fprintf(2,[ '--> Calculated the period (from beat to beat)\n',...
                     '\n']);
      plot (R(icol).tmidprd, R(icol).ptracemidprd,'kx');
      for (i=1:1:length(R(icol).prd)),
       text( R(icol).tmidprd(i), R(icol).ptracemidprd(i),...
             sprintf('%.2f', R(icol).prd(i)));
      end
      pause;
   end
   
   if (~isempty(Opt.resam_kernel)),
      %interpolate to slice sampling time grid:
      R(icol).tR = [0:Opt.slcTR:max(R(icol).t)];
      R(icol).ptraceR = interp1( R(icol).tptrace', R(icol).ptrace, ... 
                                 R(icol).tR,Opt.resam_kernel);
      R(icol).ntraceR = interp1( R(icol).tntrace', R(icol).ntrace, ... 
                                 R(icol).tR,Opt.resam_kernel);
      R(icol).prdR = interp1(R(icol).tmidprd, R(icol).prd, ... 
                             R(icol).tR,Opt.resam_kernel);
      %you get NaN when tR exceeds original signal time, so set those 
      %to the last interpolated value
      R(icol).ptraceR = clean_resamp(R(icol).ptraceR);
      R(icol).ntraceR = clean_resamp(R(icol).ntraceR);
      R(icol).prdR = clean_resamp(R(icol).prdR);
   end
   
   %Calculate the phase of the trace, with the peak  
   %to be the start of the phase
   R(icol).phz=-2.*ones(size(R(icol).t));
   i=1;
   j=1;
   while (i <= nptrc-1),
      while(R(icol).t(j) < R(icol).tptrace(i+1)),
         if (R(icol).t(j) >= R(icol).tptrace(i)),
            %Note: Using a constant244 period for each interval 
            %causes slope discontinuity within a period.
            %One should resample prd(i) so that it is 
            %estimated at each time in R(icol).t(j)
            %dunno if that makes much of a difference in the end however.
            R(icol).phz(j) = (R(icol).t(j) - (R(icol).tptrace(i))) ...
                              ./ R(icol).prd(i) + Opt.zerophaseoffset;
            if (R(icol).phz(j) < 0) R(icol).phz(j) = -R(icol).phz(j); end
            if (R(icol).phz(j) > 1) R(icol).phz(j) = R(icol).phz(j)-1; end
         end
         j = j + 1;
      end
      i = i + 1;
   end
   
   %remove the points flagged as unset
   R(icol).phz(find(R(icol).phz<-1)) = 0.0;
   %change phase to radians
   R(icol).phz = R(icol).phz.*2.*pi;
   
   
   %Now get the phase at each of the slices:
   slcoff = [0:Opt.volTR./Opt.Nsclices:Opt.volTR-Opt.volTR./Opt.Nsclices]; 
      %this timing vector ought to be extracted from .HEAD
   R(icol).tst = [0:Opt.volTR:max(R(icol).t)-Opt.volTR]; %time series time vector
   R(icol).phz_slc = zeros(length(R(icol).tst),Opt.Nsclices);
   R(icol).phz_slc_reg = zeros(length(R(icol).tst),4,Opt.Nsclices);
   for (isl=1:1:Opt.Nsclices),
      tslc = R(icol).tst+slcoff(isl);
      for (i=1:1:length(R(icol).tst)),
         [mi,imin] = min(abs(tslc(i)-R(icol).t));
         R(icol).phz_slc(i,isl) = R(icol).phz(imin);
      end
      %and make regressors for each slice
      R(icol).phz_slc_reg(:,1, isl) = sin(R(icol).phz_slc(:,isl));
      R(icol).phz_slc_reg(:,2, isl) = cos(R(icol).phz_slc(:,isl));
      R(icol).phz_slc_reg(:,3, isl) = sin(2.*R(icol).phz_slc(:,isl));
      R(icol).phz_slc_reg(:,4, isl) = cos(2.*R(icol).phz_slc(:,isl));
   end
   
   if (~Opt.quiet),
      fprintf(2,[ '--> Resampled envelopes and period time series\n',...
                  '--> Calculated phase\n',...
                  '\n']);

      subplot (413);
      plot (R(icol).t, R(icol).phz./2./pi, 'm');
      if (isfield(R(icol),'phzR')),
         plot (R(icol).tR, R(icol).phzR./2./pi, 'm-.');
      end
      
      subplot (414);
      plot (R(icol).tst, R(icol).phz_slc(:,1), 'ro', ...
            R(icol).tst, R(icol).phz_slc(:,2), 'bo', ...
            R(icol).tst, R(icol).phz_slc(:,2), 'b-'); hold on;
      plot (R(icol).t, R(icol).phz, 'k');
      grid on
      %title it
      title (R(icol).vname, 'Interpreter', 'None');
      drawnow;
      pause;
   end
 if (icol ~= nl), input ('Hit enter to proceed...','s'); end

end
   if (~Opt.quiet),   plotsign2(1); end
   
return;

function v = clean_resamp(v)
   inan = find(isnan(v));   %the bad
   igood = find(isfinite(v)); %the good
   for(i=1:1:length(inan)),
      if (inan(i) < igood(1)), 
         v(inan(i))= v(igood(1));
      elseif (inan(i) > igood(length(igood))),
         v(inan(i))= v(igood(length(igood)));
      else
         fprintf(2,'Error: Unexpected NaN case\n');
         v(inan(i))= 0;
      end 
   end
   return;

function [t,v] =  remove_duplicates(t,v)
   j = 1;
   for (i=2:1:length(t)),
      if (  t(i) ~= t(i-1)  & ...
            t(i) - t(i-1) > 0.3), %minimum time
                                  %before next beat
         j = j + 1;
         t(j) = t(i);
         v(j) = v(i);
      end
   end
   t = t(1:j);
   v = v(1:j);
   return;

function h = analytic_signal(v),
   nv = length(v);
   fv = fft(v); 
   wind = zeros(size(v));
   %zero negative frequencies, double positive frequencies
   if (iseven(nv)), 
      wind([1 nv/2+1]) = 1; %keep DC
      wind([2:nv/2]) = 2;   %double pos. freq
   else
      wind([1]) = 1;
      wind([2:(nv+1)/2]) = 2;
   end
   h = ifft(fv.*wind);
   return

