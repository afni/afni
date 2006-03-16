function mtc = readmtc (filename)
% Read BrainVoyager's mesh time point (mtc) files
% By ZSS, SSCC/NIMH/NIH 
% based on Hester's positing at
%
% http://www.brainvoyager.com/ubb/Forum8/HTML/000059.html
%
%The format of MTC files
%
%Mesh Time Course (*.mtc) files are generated from BrainVoyager QX version 
%1.0. The files are saved in little endian byte order.
%
%MTC header
%
%BYTES : DATA TYPE : (DEFAULT) : DESCRIPTION
%
%4 : int: (1) : version number
%4 : int: number of vertices
%4 : int: number of volumes
%x * 1 : char : name of source VTC file' (ends with '0')
%x * 1 : char : name of linked protocol file; if not available: ''; (ends with '0')
%4 : int : hemodynamic delay
%4 : float : TR - Repetition Time
%4 : float : delta parameter for hemodynamic response function
%4 : float : tau parameter for hemodynamic response function
%4 : int : segment size (intervals per stimulus block/event)
%4 : int : segment offset - first datapoint used with 
%protocol
%1 : char : (1) : datatype of MTC data; 1 = 'float'
%
%MTC data
%
%The data are organized per vertex: all datapoints per vertex are saved 
%together, so time runs fastest (has the smallest loop).
%
%Nr of vertices
%
%Nr of timepoints
%
%***
%
%The *.mtc file format description is also added to the updated file format documentation as available on our ftp server.
%
%[This message has been edited by Hester Breman (edited 29 October 2004).]
%

fp = fopen(filename,'r');
if (fp == -1) 
	fprintf(1,'\nError opening %s\n',filename);
	return;
end


%% read some header fields 
mtc.version = fread(fp,1,'int32',0,'ieee-le');
mtc.numvert = fread(fp,1,'int32',0,'ieee-le');
mtc.numvol = fread(fp,1,'int32',0,'ieee-le');
cnt = 1;
mtc.vtcname = '';
c=fread(fp,1,'char',0,'ieee-le');
%fprintf(2,'%d-->%c\n',c,c);
while ( c ~= 0 ),
   mtc.vtcname(cnt) = c;
   c=fread(fp,1,'char',0,'ieee-le');
   %fprintf(2,'%d-->%c\n',c,c);
   cnt = cnt + 1;
end
cnt = 1;
mtc.lpfname = '';
c=fread(fp,1,'char',0,'ieee-le');
%fprintf(2,'%d-->%c\n',c,c);
while ( c ~= 0 ),
   mtc.lpfname(cnt) = c;
   c=fread(fp,1,'char',0,'ieee-le');
   %fprintf(2,'%d-->%c\n',c,c);
  cnt = cnt + 1;
end

mtc.hemdelay = fread(fp,1,'int32',0,'ieee-le');
mtc.tr = fread(fp,1,'float32',0,'ieee-le');
mtc.hrfdelta = fread(fp,1,'float32',0,'ieee-le');
mtc.hrftau = fread(fp,1,'float32',0,'ieee-le');
mtc.segsize = fread(fp,1,'int32',0,'ieee-le');
mtc.sefoffset = fread(fp,1,'int32',0,'ieee-le'); 
mtc.datatype = fread(fp,1,'char',0,'ieee-le');

if (mtc.datatype ~= 1),
   fprintf(2,'Datatype unexpected.\n');
   return;
end
mtc.data = zeros(mtc.numvert, mtc.numvol);
for (i=1:1:mtc.numvert),
   mtc.data(i,:) = fread(fp,mtc.numvol,'float32',0,'ieee-le');
end
fclose(fp);
