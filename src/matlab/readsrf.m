function [srf] = readsrf(filename)
%
% function [srf] = readsrf(filename)
%
% readsrf attempts to read the BrainVoyagerQX v. 4 srf surface files
% to display:
% colormap(srf.cmap)
% trisurf(srf.triangles, srf.VX, srf.VY, srf.VZ, srf.mesh_color)
% but that would probably grind your machine to a halt, just to
% get the idea you could do
% trisurf(srf.triangles(1:100:end, srf.VX, srf.VY, srf.VZ, srf.mesh_color)
%
% Kate Fissell 3/06
% Modified by ZSS, SSCC/NIMH/NIH


fp = fopen(filename,'r');
if (fp == -1) 
	fprintf(1,'\nError opening %s\n',filename);
	return;
end


%% read some header fields 
srf.version = fread(fp,1,'float32',0,'ieee-le');
srf.reserve = fread(fp,1,'int32',0,'ieee-le');
srf.numvert = fread(fp,1,'int32',0,'ieee-le');
srf.numtri = fread(fp,1,'int32',0,'ieee-le');
srf.meshcenXYZ = fread(fp,3,'float32',0,'ieee-le');

%% print some header fields 
fprintf(1,'\nVersion: %.6f',srf.version);
fprintf(1,'\nNumber of vertices: %d',srf.numvert);
fprintf(1,'\nNumber of srf.triangles: %d',srf.numtri);
fprintf(1,'\nMesh center: %.3f %.3f %.3f',srf.meshcenXYZ);
fprintf(1,'\n');


%% read vertices and normals
srf.VX = fread(fp,srf.numvert,'float32',0,'ieee-le');
srf.VY = fread(fp,srf.numvert,'float32',0,'ieee-le');
srf.VZ = fread(fp,srf.numvert,'float32',0,'ieee-le');

srf.NX = fread(fp,srf.numvert,'float32',0,'ieee-le');
srf.NY = fread(fp,srf.numvert,'float32',0,'ieee-le');
srf.NZ = fread(fp,srf.numvert,'float32',0,'ieee-le');


%% read color stuff
srf.cmap = zeros(2,3);
srf.cmap(1,:) = fread(fp,3,'float32',0,'ieee-le');
srf.alpha_convex = fread(fp,1,'float32',0,'ieee-le');
srf.cmap(2,:) = fread(fp,3,'float32',0,'ieee-le');
srf.alpha_concave = fread(fp,1,'float32',0,'ieee-le');
srf.mesh_color = fread(fp,srf.numvert,'float32',0,'ieee-le');


%% read srf.neighbors of vertices
for i=1:srf.numvert
	srf.numneigh = fread(fp,1,'int32',0,'ieee-le');
	srf.neighbors = fread(fp,srf.numneigh,'int32',0,'ieee-le');
end


%% read srf.triangles
srf.triangles = fread(fp,srf.numtri*3,'int32',0,'ieee-le');
srf.triangles = reshape(srf.triangles, [3 srf.numtri]);
srf.triangles = srf.triangles' + 1;	%% matlab uses 1 based indices so inc vertex indices

srf.tristrip = fread(fp,1,'int32',0,'ieee-le');
if (srf.tristrip > 0)
	srf.tristripseq = fread(fp,srf.tristrip,'int32',0,'ieee-le');
end

[omega cnt] = fread(fp,1,'uchar',0,'ieee-le');
if (cnt ~= 0) 
	fprintf(1,'\nWarning: extra elements at end of file.');

	c = 0;
	while (cnt ~= 0)
		[omega cnt] = fread(fp,1,'uchar',0,'ieee-le');
		c = c + 1;	
        end
	fprintf(1,'\nread %d more chars.\n',c);
end

fclose(fp);
