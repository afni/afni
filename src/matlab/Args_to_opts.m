function [Opt] = Args_to_opts(Opt, autoconvert, varargin)
% Convert arguments to items in the Opt structure
% This ability should be particularly useful for creating compiled
% functions in Matlab, where there is no desktop environment to set
% variables before running a program
% the compiled program should take a variable number of arguments
% with the varargin argument as the last argument

for(i=1:1:nargin-2)
    % check if this argument is an option
    if(strncmp(varargin{i},'Opt.',4))
        %Opt.something=xxxyyy
        remain = varargin{i};
        [chopped, remain] = strtok(remain, '.');
        [chopped, remain] = strtok(remain, '=');
        optname = chopped(2:length(chopped));
        optval = remain(2:length(remain));
        % check if this is numeric, and convert automatically
        % otherwise keep it text
        if(autoconvert==1)
            [val, status] = str2num(optval);
            if(status)
                optval = val;
            end
        end
        Opt = setfield(Opt, optname, optval);
    end
end
