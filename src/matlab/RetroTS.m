%This script creates slice-based regressors for regressing out 
% components of heart rate, respiration and respiration volume per time
%
% The script is intended as a prototype for development in C or Python 
% The important routines are:
%    hilbert: Easily implemented with fft and ifft
%    interp1: A table lookup interpolation
%    fir: Tool for designing filters (we can just take it's coefficients)
%    filter: function to apply fir filter parameters (easy)
%    
% All of the above can be easily implemented in C. However, I find it
% very useful to be able to plot the various steps in the process as we
% will undoubtedly face problems in the future. So I would vote for 
% Python, assuming library vintage is not an issue. It looks like the 
%
iscan = 12;

lll = zglobb({ sprintf('Resp*%d*',iscan),...
               sprintf('ECG*%d*',iscan),...
               sprintf('scan*%d*', iscan)});

%Get some info from header file and set params
f = fopen(lll(3).name, 'r');
s = fscanf(f,'%c');             
fclose(f);
ns = length(s);
pat = 'RT Physio:\W*sampling\W*';                 
Opt.fs = 1000/str2num(strtok(s(regexp(s,pat,'end'):ns)));        
Opt.Nsclices = 20;
Opt.volTR = 2;
Opt.slcTR = 0.04; %upsampling period in secs
Opt.fcutoff = 10; %cut off frequency for filter
Opt.quiet = 1;
Opt.resam_kernel = 'linear';   %resampling filter for envelopes and phase
Opt.zerophaseoffset = 0;
Opt.fir_order = 40;  %order of fir filter
Opt.RVTshifts = [0:5:20];  %shifts, in seconds, applied to RVT curve
Opt.demo = 0;

clear ('s');

OptR = Opt;
OptE = Opt; OptE.fcutoff = 3; %lots of noise, cut at 3Hz

%Get the peaks for R and E
R = PeakFinder(lll(1).name,OptR); 
E = PeakFinder(lll(2).name,OptE);
%get the phase
OptR.AmpPhase = 1;            %amplitude based phase for respiration
R = PhaseEstimator(R,OptR);
OptR.AmpPhase = 0;            %time based phase for cardiac signal
E = PhaseEstimator(E,OptE);

%Now do the RVT for Respiration
R = RVT_from_PeakFinder(R, OptR);

%Show some results
Show_RVT_Peak(R,1);
Show_RVT_Peak(E,2);

%write retroicor regressors
for (i=1:1:Opt.Nsclices),
   fname = sprintf('RetroCard_%d.slc%02d.1D', iscan, i);
   wryte3(E.phz_slc_reg(:,:,i), fname);
   fname = sprintf('RetroResp_%d.slc%02d.1D', iscan, i);
   wryte3(R.phz_slc_reg(:,:,i), fname);
end

%and write the RVT puppy, plus or minus a few seconds delay
fname = sprintf('RetroRVT_%d.1D', iscan);
wryte3(R.RVTRS_slc, fname);

