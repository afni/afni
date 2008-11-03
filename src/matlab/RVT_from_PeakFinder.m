function R = RVT_from_PeakFinder(R, Opt)
   if (~isfield(Opt,'Demo') | isempty(Opt.Demo)),
      Opt.Demo = 0;
   end

   if (Opt.Demo),
      Opt.Quiet = 0; 
   else 
      pause off 
   end
   for (icol=1:1:length(R)),      
      %calculate RVT
      R(icol).RV = (R(icol).ptrace-R(icol).ntrace); 
                           %NEED TO consider which starts first and
                           %Whether to initialize first two vlues by means
                           %and also, what to do when we are left with one 
                           %incomplete pair at the end
      
      nptrc = length(R(icol).tptrace);
      R(icol).RVT = R(icol).RV(1:nptrc-1)./R(icol).prd';
      if (isfield(R(icol),'ptraceR')),
         R(icol).RVR = (R(icol).ptraceR-R(icol).ntraceR); 
         R(icol).RVTR = R(icol).RVR./R(icol).prdR;
         %smooth RVT so that we can resample it at VolTR later
         fnyq = Opt.PhysFS./2; %nyquist of physio signal
         fcut = 2./Opt.VolTR ;%cut below nyquist for volume TR
         w = Opt.fcutoff/fnyq;    % cut off frequency normalized
         b = fir1(Opt.FIROrder, w) ;    
         v = R(icol).RVTR; mv  =   mean(v);  
         %remove the mean
         v = (v - mv);
         %filter both ways to cancel phase shift
         v = filter(b,1,v); v = flipud(v); v = filter(b,1,v); v = flipud(v);
         R(icol).RVTRS = v+mv;
      end
      
      %create RVT regressors
      R(icol).RVTRS_slc = zeros(length(R(icol).tst), length(Opt.RVTshifts));
      for (i=1:1:length(Opt.RVTshifts)),
         shf = Opt.RVTshifts(i);
         nsamp = round(shf*Opt.PhysFS);
         sind = [1:1:length(R(icol).t)]+nsamp;
         sind(find(sind<1)) = 1;
         sind(find(sind>length(R(icol).t))) = length(R(icol).t);
         rvt_shf = interp1(R(icol).t, R(icol).RVTRS(sind),...
                           R(icol).tst, Opt.ResamKernel);
         R(icol).RVTRS_slc(:,i) = rvt_shf;
      end

      if (~Opt.Quiet),
         fprintf(2,[ '--> Calculated RVT \n',...
                     '--> Created RVT regressors\n',...
                     '\n']);
         subplot (211);
         plot (R(icol).tmidprd,...
               zscale(R(icol).RVT, max(R(icol).ptrace),...
               min(R(icol).ptrace)), 'k');
         if (isfield(R(icol),'ptraceR')),
            plot (R(icol).tR,...
                  zscale(R(icol).RVTRS, max(R(icol).ptrace),...
                  min(R(icol).ptrace)), 'm');
         end
         pause;
      end
   end
return;
