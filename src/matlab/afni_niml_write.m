function afni_niml_write(p,fn)
% simple function to write niml files in ASCII format
%
% AFNI_NIML_WRITE(P,FN) writes a NIML struct to file FN. P should be in the 
% form as returned by AFNI_NIML_READ.
% 
% The order of the arguments may be reversed. If FN is numeric, then it is
% assumed to be a file identifier. If P is omitted, output is written to
% stdout.
%
% NNO Dec 2009 <n.oosterhof@bangor.ac.uk>

if nargin<2
    fn=1; 
end

if isstruct(fn) % allow for wrong order of arguments
    tmp=p;
    p=fn;
    fn=tmp;
end

% conversion
s=afni_niml_print(p);

% see if output is a file identifier
if isnumeric(fn) && round(fn)==fn
    fid=fn;
else
    fid=fopen(fn,'w');
end

if fid==0
    error('Could not write to %s\n', fn);
end

c=fprintf(fid,s);

fclose(fid);
if c ~= numel(s)
    error('Something went wrong when writing %s\n', fn);
else
    fprintf('Written NIML struct to %s\n', fn);
end