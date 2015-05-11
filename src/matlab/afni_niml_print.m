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

s=afni_niml_print_helper(p, format);

function s=afni_niml_print_helper(p, format)
    if iscell(p)
        ss=cell(1,numel(p));
        % simple recursion
        for k=1:numel(p)
            ss{k}=afni_niml_print_helper(p{k},format);
        end
        s=[ss{:}]; % concatenate
    else
        headername=p.name;
        p=rmfield(p,'name'); % we don't want this as a field

        if isfield(p,'nodes')
            % is a group, do recursion
            sbody=afni_niml_print_helper(p.nodes,format);
            p=rmfield(p,'nodes'); % 'nodes' is not a standard NIML field
        elseif isfield(p,'data')

            % in case the type is not specified, we try
            % to derive it from the data
            p=set_niml_vec_typ_attributes(p);

            [sbody,ni_form]=afni_niml_print_data(p,format);
            if ~isempty(ni_form)
                p.ni_form=ni_form;
            end

            % some fields are not standard NIML (I think), we remove these
            illegal_fields={'vec_typ','vec_len','vec_num','name','data'};

            to_remove=intersect(fieldnames(p),illegal_fields);
            p=rmfield(p, to_remove);
        else
            disp(p)
            error('Do not understand this struct');
        end

        headertext=afni_niml_print_header(p);
        s=sprintf('<%s\n%s >%s</%s>\n',headername,...
                                       headertext,...
                                       sbody,...
                                       headername);
    end


function [s, ni_form]=afni_niml_print_data(p,format)
    pat=get_ascii_printer(p.vec_typ,p.data);

    is_string=strcmp(pat,'%s');
    is_ascii=strcmp(format,'ascii');
    if is_string || is_ascii
        s=afni_niml_print_body_ascii(p);
        ni_form=[];
    else
        s=afni_niml_print_body_binary(p,format);

        [unused, unused, endian]=computer();
        ni_form=sprintf('binary.%ssbfirst',lower(endian));
    end


function string_data=afni_niml_print_body_ascii(p)
    printer=get_ascii_printer(p.vec_typ,p.data);
    string_data=printer(p.data);




function binary_data=afni_niml_print_body_binary(p, format)
    if ~strcmp(format, 'binary')
        error('format must be ''ascii'' or ''binary''');
    end

    vec_typ=p.vec_typ;
    if numel(vec_typ)>1 && any(vec_typ(1)~=vec_typ)
        error('binary only supported for uniform vec_type');
    end

    ni_defs=afni_ni_defs();
    converter=ni_defs.type(vec_typ(1)+1); % base 1
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

function niml=set_niml_vec_typ_attributes(niml)
% sets the vector type, in case this is not given.
    if ~isfield(niml,'vec_typ') || ~isfield(niml,'vec_len')
        [niml.vec_type, niml.vec_len]=get_vec_type_len_from_data(...
                                                            niml.data);
    end
    niml.vec_num=numel(niml.vec_typ);

    niml.ni_type=vec_type2string(niml.vec_typ);
    niml.ni_dimen=sprintf('%d',niml.vec_len);

function ni_type=vec_type2string(vec_typ)
    % converts numeric vec_typ to string representation
    ni_defs=afni_ni_defs();
    type_name=cellstr(ni_defs.type_name);

    n=numel(vec_typ);
    ni_type_cell=cell(1,2*n);

    pos=0;

    count=0;
    for k=1:n
        tp=vec_typ(k);
        count=count+1;
        next_is_same=k<n && vec_typ(k+1)==tp;

        if next_is_same
            continue;
        end

        assert(count>0);

        if count>1
            pos=pos+1;
            ni_type_cell{pos}=sprintf('%d*',count);
        end

        pos=pos+1;
        ni_type_cell{pos}=type_name{tp+1};

        count=0;
    end

    ni_type=cat(2,ni_type_cell{1:pos});


function [vec_typ, vec_len]=get_vec_type_len_from_data(data)
    % helper function to get vec type from data
    if iscell(data)
        % do recursive call
        vec_typ=cell2mat(cellfun(@get_vec_type_len_from_data,data));

        if ~isvector(vec_typ)
            error('Unrecognized data');
        end

        vec_len_s=cell2mat(cellfun(@numel,data));
        if ~all(vec_len_s(1)==vec_len_s)
            error('data has different sizes');
        end

        vec_len=vec_len_s(1);

        return
    end

    vec_num=size(data,2);

    if islogical(data)
        data=single(data); % convert to numeric
    end

    ni_defs=afni_ni_defs();

    type_string_s=cellfun(func2str, ni_defs.type);

    % check builtin types
    for k=1:numel(type_strings)
        type_string=type_string_s{k};
        if isa(data, type_string)
            vec_typ=k-1;
            return
        end
    end

    if isnumeric(data)
        if isequal(round(data),data)
            tp=ni_defs.NI_INT;
        else
            tp=ni_defs.NI_FLOAT;
        end

    elseif ischar(data) || iscellstr(data)
        tp=ni_defs.NI_STRING;

    else
        error('Data not understood');
    end

    vec_len=size(data,1);
    vec_typ=repmat(tp,1,vec_num);


function f=get_ascii_printer(vec_typ,data)
% use vec_typ to create a format string for printf
% data is used in case we print floats; we use the least possible number of
% floating point positions

    ni_defs=afni_ni_defs();

    n_vec_typ=numel(vec_typ);
    has_numeric_vec_typ=all(vec_typ(1)~=ni_defs.NI_STRING) ...
                                                && ~iscell(data);

    if has_numeric_vec_typ
        pat1=get_single_element_ascii_print_format(vec_typ,data,ni_defs);
        pat=repmat([pat1 ' '],1,n_vec_typ);
        pat(end)=sprintf('\n');

        f=@(x)sprintf(pat,x');
        return;
    else
        if ~iscell(data)
            error('illegal data');
        end

        n_col=numel(data);
        pats=cell(1,n_col);
        for k=1:n_col
            pats{k}=get_single_element_ascii_print_format(vec_typ(k),...
                                                        data{k},ni_defs);
        end

        f=@(x)print_cell_ascii(pats,data);
    end




function s=print_cell_ascii(pats, data_cell)
% mixed data types, or anything with a string
    string_mask=cellfun(@iscellstr,data_cell);
    n_col=numel(pats);

    n_row_s=cellfun(@numel,data_cell);

    n_row=n_row_s(1);
    if any(n_row~=n_row_s);
        error('data has different number of elements');
    end

    col_sep=' ';
    row_sep=sprintf('\n');

    s_cell=cell(1,n_row*n_col);
    pos=0;
    for row=1:n_row
        for col=1:n_col
            d_col=data_cell{col};

            if string_mask(col)
                d=niml_escape_string(d_col{row});
            else
                d=d_col(row);
            end

            pos=pos+1;

            if col==n_col
                sep=row_sep;
            else
                sep=col_sep;
            end

            s_cell{pos}=[sprintf(pats{col}, d) sep];

        end
    end

    s=[row_sep cat(2,s_cell{:})];



function s_escaped=niml_escape_string(s)
    ni_defs=afni_ni_defs();
    escape=ni_defs.escape;

    s_escaped=char(s);

    n=size(escape,1);
    for k=1:n
        s_escaped=strrep(s_escaped,escape{2},escape{1});
    end




function f=get_single_element_ascii_print_format(vec_typ,data,ni_defs)
    vec_str=strtrim(ni_defs.type_name(vec_typ+1,:));

    switch vec_str
        case 'String'
            f='"%s"';

        case {'byte','short','int'}
            % CHECKME don't know if this is ok for bytes and shorts...
            f='%d';

        otherwise
            precision=get_required_precision(data);
            f=sprintf('%%.%df',precision);
    end


