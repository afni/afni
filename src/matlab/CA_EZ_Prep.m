function [MapMPM] = CA_EZ_Prep()
% A function to process a new Zilles CA toolbox
%
%
FuncName = 'CA_EZ_Prep';

toolbox_dir = '/Volumes/elrond0/home4/users/ziad/Programs/matlab/spm2/toolbox/Anatomy_13b';
if (exist(toolbox_dir) ~= 7),
   fprintf(2,'Toolbox directory %s not found\n', toolbox_dir);
   return;
end

%First get the MPM info
   prf = sprintf('%s%cAllAreas*MPM.mat', toolbox_dir, filesep);
   [err, ErrMessage, MPM_file] = zglobb ({prf});
   if (size(MPM_file,1) ~= 1),
      fprintf(2,'Could not find unique MPM map list\n', toolbox_dir);
      for (i=1:1:size(MPM_file,1)), MPM_file(i), end
      return;
   end

   MPM_file(1).name

   %load the MPM map structure
   MapMPM = load(MPM_file(1).name);
   MapMPM = MapMPM.MAP;

   %checks
   if (~isstruct(MapMPM)),
      fprintf(2,'MapMPM is not a struct\n');
      return;
   end

   fld = 'name';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'GV';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'ref';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'smoothed';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'VOL';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'MaxMapMPM';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'XYZ';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'XYZmm';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'orient';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'Z';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'LR';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'allXYZ';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'allZ';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end
   fld = 'allLR';
   if (~isfield (MapMPM, fld)), fprintf(2,'%s field is not in MapMPM\n', fld); return; end

%Now the MacroLabels
   prf = sprintf('%s%cMacro.mat', toolbox_dir, filesep);
   [err, ErrMessage, ML_file] = zglobb ({prf});
   if (size(ML_file,1) ~= 1),
      fprintf(2,'Could not find unique ML map list\n', toolbox_dir);
      for (i=1:1:size(ML_file,1)), ML_file(i), end
      return;
   end

   ML_file(1).name

   %load the MacroLabels
   MapML = load(ML_file(1).name);
   MapML = MapML.Labels;

%create the strcutres needed by AFNI
%first create MPM structure
fid = fopen ('thd_ttatlas_CA_EZ-auto.c','w');
if (fid < 0),
   fprintf(2,'Failed to open output file\n');
   return;
end

fprintf(fid,'/*! Data for atlases from Eickhoff''s SPM toolbox.\nAutomatically compiled from %s by function %s\nDate: %s*/\n', MPM_file(1).name, FuncName, date);
fprintf(fid,'ML_EZ_point ML_EZ_list[ML_EZ_COUNT] = {\n');
for (i=1:1:
