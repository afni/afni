function s=afni_niml_print(p, format)
% takes a NIML data structure and converts it to string representation
%
% S=AFNI_NIML_PRINT(P) takes an NIML datastructure P and returns a string
% representation S.
%
% This function is more or less the inverse of AFNI_NIML_PARSE.
%
% The current implementation will take any struct that has the required
% NIML fields; it does not check for any data consisentence.
%
% Please note that this function is *VERY EXPERIMENTAL*.
%
% NNO Dec 2009 <n.oosterhof@bangor.ac.uk>

if nargin<2
    format='ascii';
end

if iscell(p)
    ss=cell(1,numel(p));
    % simple recursion
    for k=1:numel(p)
        ss{k}=afni_niml_print(p{k},format);
    end
    s=[ss{:}]; % concatenate
else
    headername=p.name;
    p=rmfield(p,'name'); % we don't want this as a field

    if isfield(p,'nodes')
        % is a group, do recursion
        sbody=afni_niml_print(p.nodes,format);
        p=rmfield(p,'nodes'); % 'nodes' is not a standard NIML field
    elseif isfield(p,'data')
        if ~isfield(p,'vec_typ')
            % in case the type is not specified, we try
            % to derive it based on the type of p.data
            ps=derive_vec_type(p.data);

            % set field names if not set
            fns=fieldnames(ps);
            for k=1:numel(fns)
                if ~isfield(p,fns{k})
                    p.(fns{k})=ps.(fns{k});
                end
            end
        elseif p.vec_typ<0;
            error('vec_typ=%d not supported (yet)', p.vec_typ);
        end

        [sbody,ni_form]=afni_niml_print_data(p,format);
        if ~isempty(ni_form)
            p.ni_form=ni_form;
        end

        % some fields are not standard NIML (I think), we remove these
        removefields=strvcat('vec_typ','vec_len','vec_num','name','data');
        fns=fieldnames(p);
        for k=1:numel(fns)
            fn=fns{k};
            if ~isempty(strmatch(fn,removefields,'exact'))
                p=rmfield(p,fn);
            end
        end
    else
        disp(p)
        error('Do not understand this struct');
    end

    headertext=afni_niml_print_header(p);
    s=sprintf('<%s\n%s >%s</%s>\n',headername,headertext,sbody,headername);
end


function [s, ni_form]=afni_niml_print_data(p,format)
    ni_form=[];
    pat=get_print_format(p.vec_typ,p.data);

    is_string=strcmp(pat,'%s');
    is_ascii=strcmp(format,'ascii');
    if is_string || is_ascii
        if is_string
            around='"';
        else
            around='';
            %transpose to fix major row vs. major column order
            p.data=p.data';
        end

        s=sprintf([pat '\n'],p.data);
        s=[ around s(1:(end-1)) around ]; % remove last newline
        ni_form=[];
    else
        s=afni_niml_print_body_binary(p,format);

        [unused, unused, endian]=computer();
        ni_form=sprintf('binary.%ssbfirst',lower(endian));
    end

function binary_data=afni_niml_print_body_binary(p, format)
    if ~strcmp(format, 'binary')
        error('format must be ''ascii'' or ''binary''');
    end

    vec_typ=p.vec_typ;
    if numel(vec_typ)>1 && any(vec_typ(1)~=vec_typ)
        error('binary only supported for uniform vec_type');
    end

    ni_defs=afni_ni_defs();

    switch vec_typ(1)
        case ni_defs.NI_BYTE
            converter=@char;

        case ni_defs.NI_SHORT
            converter=@int16;

        case ni_defs.NI_INT
            converter=@int32;

        case ni_defs.NI_FLOAT32
            converter=@single;

        case ni_defs.NI_FLOAT64
            converter=@double;

        otherwise
            error('unsupported vec_type %d', vec_typ(1));
    end

    data=converter(p.data');

    binary_data=typecast(data(:)','uint8');



function s=afni_niml_print_header(p)
pre='  ';           % a bit of indendationn
post=sprintf('\n'); % every header gets a new line
fns=fieldnames(p);
n=numel(fns);

ss=cell(1,n);
for k=1:n
    fn=fns{k};
    val=strtrim(p.(fn));

    if isnumeric(val)
        warning('Converting numeric to string for field %s\n', fn);
        val=num2str(val);
    end

    % surround by quotes, if that's not the case yet
    if val(1) ~= '"' && val(end) ~= '"'
        val=['"' val '"'];
    end
    if k==n
        post=''; %no new line at the very end
    end
    ss{k}=sprintf('%s%s=%s%s',pre,fn,val,post);
end

s=[ss{:}]; % concatenate results

function p=derive_vec_type(data)
% sets the vector type, in case this is not given.
%
% data should be either a string or numeric
% TODO: support structs and cells, and mixed data types
% TODO: allow other fields missing (maybe use the whole struct rather than
%       just the data so that we can be 'smart' and use converging
%       evidence?
    nidefs=afni_ni_defs();
    if islogical(data)
        data=single(data); % convert to numeric
    end
    if isnumeric(data)
        if isequal(round(data),data)
            tp=nidefs.NI_INT;
        else
            tp=nidefs.NI_FLOAT;
        end
        [ln,nm]=size(data);
    elseif ischar(data)
        tp=nidefs.NI_STRING;
        ln=1;
        nm=1;
    else
        disp(data)
        error('Unknown data type');
    end

    if nm>1
        prefix=sprintf('%d*',nm);
    else
        prefix='';
    end

    tps=strtrim(nidefs.type_name(tp+1,:)); % tricky base0 vs base1

    p.ni_type=[prefix tps];
    p.ni_dimen=sprintf('%d',ln); %NNO Jan 2010 fix

    p.vec_typ=repmat(tp,1,nm); % tricky base0 vs base1 % Jan 2010: removed 'tp+1'
    p.vec_len=ln;
    p.vec_num=nm;


function f=get_print_format(vec_typ,data,nidefs)
% use vec_typ to create a format string for printf
% data is used in case we print floats; we use the least possible number of
% floating point positions

    if nargin<3
        nidefs=afni_ni_defs();
    end

    n=numel(vec_typ);
    if n>1
        if size(data,2) ~= n
            error('Mismatch between data and vec_typ: %d ~= %d', n, size(data,2));
        end
        fs=cell(1,n);
        for k=1:n
            if k<n
                addend=' '; % spacing between elements ...
            else
                addend='';  % ... except for the last element
            end
            % recursion
            fs{k}=[get_print_format(vec_typ(k),data(:,k),nidefs) addend];
        end
        f=[fs{:}]; % concatenate result
    else
        vec_str=strtrim(nidefs.type_name(vec_typ+1,:)); %tricky again, base0 vs base1

        switch vec_str
            case 'String'
                f='%s';
            case {'byte','short','int'} % CHECKME don't know if this is ok for bytes and shorts...
                f='%d';
            otherwise
                precision=get_required_precision(data);
                f=sprintf('%%.%df',precision);
        end
    end




