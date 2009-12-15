function afni_niml_test_io(fns)
% simple test function for i/o capabilities of afni_niml_{read,parse,print,write}
%
% AFNI_NIML_TEST_IO(FNS), where FNS is a cell with filenames, reads each of
% the files indicated in FNS, tests whether parsing and printing are 
% reverse operations, and writes the files to a new file. If a file 
% contains a matrix with floats, then these are replaced by random numbers.
%
% AFNI_NIML_TEST_IO(FN), where FN is a single filename, is also allowed.
% AFNI_NIML_TEST_IO() uses filenames as defined in the body of this
% function.
%
% Each file named in FNS should be in ASCII NIML format.
%
% NNO Dec 2009 <n.oosterhof@bangor.ac.uk>

if nargin<1
    % set defaults
    fns={'testfiles/test_ROI2.niml.dset', ...
         'testfiles/you_look_marvellous.niml.dset'};
end

if ischar(fns) % single filename
    fn=fns;
    fns=cell(1);
    fns{1}=fn;
end

n=numel(fns);

for k=1:n
    fn=fns{k};
    
    [p,s]=afni_niml_read(fn);

    % print a few lines
    fprintf('First 1000 characters:\n%s\n', s(1:1000));
    
    % switch a few times between parsing and printing, results should
    % converge to 'fixed points'
    fprintf('Check whether parsing and printing are mutually inverse\n');
    s2=afni_niml_print(p);
    p2=afni_niml_parse(s2);
    s3=afni_niml_print(p2);
    
    % check convergence
    if ~isequal(s2,s3)
        error('Wrong string representation for %s', fn);
    end
    
    if ~isequal(p,p2)
        error('Wrong struct representation for %s', fn);
    end
    
    fprintf('Good, data seems to match\n');
    
    % make a new id code to keep suma happy
    p.self_idcode=['XYZ_' char(rand(1,24)*26+65)];
    
    % generate some random data, if there is a matrix with floats
    % (we don't want to mess with node indices)
    if isfield(p,'nodes')
        for j=1:numel(p.nodes)
            nk=p.nodes{j};
            if ~isempty(strfind(nk.ni_type,'float'))
                fprintf('Generating some random data\n');
                
                % ensure that we're in about the same range as the original
                % dataset (with some overshoot because of gaussianness)
                left=min(nk.data(:));
                right=max(nk.data(:));
                p.nodes{j}.data=left+(right-left)*randn(size(nk.data));
            end
        end
    end
        
    % convert to string
    [pth,f,e]=fileparts(fn);
    newfn=[pth '/niml_io_test_' f  e];
   
    afni_niml_write(newfn,p);
    
end
    