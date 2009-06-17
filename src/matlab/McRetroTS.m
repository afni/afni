function [R] = McRetroTS(Respfile, Cardfile, VolTR, Nslices, PhysFS, ShowGraphs)
% Version of RetroTS made for Matlab compiler
%   No variables predefined and takes simple options on command line
% 
% function [Opt, R, E] = RetroTS(SN)
%    [Opt, OptR, OptE] = RetroTS(Opt)
%This function creates slice-based regressors for regressing out 
% components of heart rate, respiration and respiration volume per time.
%
%  Opt is the options structure with the following fields
%     Mandatory:
%     ----------
%     Respfile: Respiration data file
%     Cardfile: Cardiac data file
%     PhysFS: Physioliogical signal sampling frequency in Hz.
%     Nslices: Number of slices
%     VolTR: Volume TR in seconds
%     Optional:
%     ---------
%     Prefix: Prefix of output file
%     SliceOffset: Vector of slice acquisition time offsets.
%                  (default is equivalent of alt+z)
%     RVTshifts: Vector of shifts in seconds of RVT signal. 
%                (default is [0:5:20])
%     RespCutoffFreq: Cut off frequency in Hz for respiratory lowpass filter
%                     (default 3 Hz)
%     CardCutoffFreq: Cut off frequency in Hz for cardiac lowpass filter
%                     (default 3 Hz)
%     ResamKernel: Resampling kernel. 
%                 (default is 'linear', see help interp1 for more options)
%     FIROrder: Order of FIR filter. (default is 40)
%     Quiet: 1/0  flag. (defaut is 1)
%     Demo: 1/0 flag. (default is 0)
%     
%Example:
%
%  Opt.Respfile = 'Resp_epiRT_scan_14.dat'
%  Opt.Cardfile = 'ECG_epiRT_scan_14.dat'
%  Opt.VolTR = 2
%  Opt.Nslices = 20;
%  Opt.PhysFS = 1./0.02;   %20 msec sampling period
%  RetroTS(Opt);
%

%Implementation Notes:
%%%%%%%%%%%%%%%%%%%%%%
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

if (nargin < 1), 
   fprintf(2,'Need some input.\n');
   fprintf(2,'McRetroTS Respfile Cardfile VolTR Nslices PhysFS Graph\n');
   fprintf(2,'Example: \n');
   fprintf(2,'  McRetroTS Resp_epiRT_scan_14.dat ECG_epiRT_scan_14.dat 2 20 50 0\n');
   fprintf(2,'See RetroTS.m for more details\n');
   R=1
   return;
end

Opt.Respfile = Respfile;
Opt.Cardfile = Cardfile;
if (ischar(VolTR))
   Opt.VolTR=str2num(VolTR);
else
   Opt.VolTR = VolTR
end
if (ischar(Nslices))
   Opt.Nslices=str2num(Nslices);
else
   Opt.Nslices = Nslices
end

if (ischar(PhysFS))
   Opt.PhysFS = str2num(PhysFS);
else
   Opt.PhysFS = PhysFS;
end

if (ischar(ShowGraphs))
   Opt.ShowGraphs = str2num(ShowGraphs);
else
   Opt.ShowGraphs = ShowGraphs
end

Opt.Quiet = 1;
Opt.Demo = 0;

% call the original
RetroTS(Opt);
if (Opt.ShowGraphs)
   user_input = input('Close all figures to exit\n');
   %uiwait(gcf);
end

R=0;
return;
