function [flg] = filexist (filename)
%
%	[flg] = filexist (filename)
%
%  if filename exists then flg = 1, else flg = 0
%

fid = fopen (filename,'r');
if (fid == -1),
	flg = 0;
else
	flg = 1;
	fclose (fid);
end


return;
