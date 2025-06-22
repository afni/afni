#!/bin/tcsh

# asymm_report
# make report on list of regions with volumes, surface areas, asymmetry

@global_parse `basename $0` "$*" ; if ($status) exit 0

set progname = asymm_report

set version   = "0.90";   set rev_dat   = "Jun 12, 2025"
#     + [DRG] 


# start with cerebellum regions from HCA_lr_v0.9.nii.gz

# assume region list from left and right have ranges that correspond
# e.g. 1-21 31-51

# set input right range, input left range
set right_range = (`count_afni -digits 2 1 21`)
set left_range = (`count_afni -digits 2 31 51`)
 
#set inset = HCA_lr_v0.9.nii.gz
set inset = ""

set isosurfs = ""
set isosurf_dir = "./"
set isosurf_base = ""
set isosurf_labels = ""
set make_isosurfs = ""

set right_range = ""
set left_range = ""
set reportfile = /dev/stdout

set patch_areas = ""
set fullatlas_surf = ""
set make_patch_surf = ""
set patch_labels = ""
set patchsurf = "fullpatchsurf.gii"

if ("$#" <  "1") then
   goto HELP
endif

set ac = 1
while ($ac <= $#argv)
    if ("$argv[$ac]" == "-help" || "$argv[$ac]" == "-h") then
        goto HELP

    else if ("$argv[$ac]" == "-ver") then
        echo $version
        exit 0

    # -------------------

    else if ("$argv[$ac]" == "-input") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set inset =  $argv[$ac]

    else if ("$argv[$ac]" == "-right_range") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set rightmin =  $argv[$ac]
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set rightmax =  $argv[$ac]
        set right_range = `count_afni -digits 3 $rightmin $rightmax`

    else if ("$argv[$ac]" == "-left_range") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set leftmin =  $argv[$ac]
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set leftmax =  $argv[$ac]
        set left_range = `count_afni -digits 3 $leftmin $leftmax`

    else if ("$argv[$ac]" == "-isosurf_dir") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set isosurf_dir =  $argv[$ac]
        set isosurfs = 1

    else if ("$argv[$ac]" == "-isosurf_base") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set isosurf_base =  $argv[$ac]
        set isosurfs = 1

    else if ("$argv[$ac]" == "-make_isosurfs") then
        set make_isosurfs = 1
        set isosurfs = 1

    # compute surface patch areas for each region
    else if ("$argv[$ac]" == "-surf_patch") then
        set patch_areas = 1

    # make patch surface, implies computing surface patch areas
    else if ("$argv[$ac]" == "-make_patch_surface") then
        set make_patch_surf = 1
        set patch_areas = 1

    # use existing patch surface, implies computing surface patch areas
    else if ("$argv[$ac]" == "-surf_patch_surface") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set fullatlas_surf =  $argv[$ac]
        set patch_areas = 1

    else if ("$argv[$ac]" == "-reportfile") then
        set this_opt = "$argv[$ac]"
        @ ac ++
        if ( $ac > $#argv ) then
            echo "** missing parameter for option '${this_opt}'"
            exit 1
        endif
        set reportfile = $argv[$ac]

   
   # ---------- fin ------------------

    else
        echo "** unknown option $argv[$ac]"
        exit 1
    endif
    @ ac ++

end

# checks for good inputs
if ($inset == "") then
   echo "Must specify input dataset with -input
   exit 1
endif

if ("$#right_range" == "0") then
   echo "Must specify both left and right ranges"
   exit 1
endif

if (($isosurfs == "1") && ($isosurf_base == "")) then
  echo "Must supply base of gifti file name shared among all gifti files"
   exit 1
endif

if ($isosurfs == "1") then
   set isosurf_labels = "  right_area left_area ra/la_asymm"
endif

# check if making patches or using patches
# may allow for making new patch surface if specified one doesn't exist
if ($patch_areas == "1") then
   set patch_labels = "   right_patch_area left_patch_area rpa/lpa_asymm"
   if (($make_patch_surf == "1") && ($fullatlas_surf != "")) then
      echo "Cannot specify both surf_patch_surface and make_patch_surface"
      exit 1
   endif
   if (($make_patch_surf == "") && ($fullatlas_surf == "")) then
      echo "Neither surf_patch_surface nor make_patch_surf specified"
      echo "Assuming you want a patch surface created and continuing"
      echo "Using default name of $patchsurf for patch surface"
      set fullatlas_surf = $patchsurf
      set make_patch_surf = 1
   endif
endif

# make isosurfaces for all the individual regions
if ($make_isosurfs == "1") then
   mkdir -p $isosurf_dir 
   cp $inset $isosurf_dir/
   cd $isosurf_dir
   IsoSurface -isorois+dsets -input $inset -o $isosurf_base.gii -Tsmooth 0.1 1000
   cd -
endif

# if computing patch areas, project volumetric atlas onto full surface
if ($patch_areas == "1") then
   if ($make_patch_surf == "1") then
      IsoSurface -isorange 1 100000 -input $inset -o $fullatlas_surf -overwrite      
   endif

   quickspec -spec asymmquick_patch.spec -overwrite -tn gii $fullatlas_surf
   3dVol2Surf -spec asymmquick_patch.spec -surf_A $fullatlas_surf -sv $inset \
      -grid_parent $inset -gp_index 0 -map_func nzmode     \
      -f_steps 5 -f_index nodes -use_norms -norm_len 5     \
      -oob_value 0 -reverse_norm_dir                       \
      -out_niml roi_proj.niml.dset -overwrite
   SurfLocalstat  -hood 2 -stat mode                       \
           -i_gii $fullatlas_surf                          \
           -input roi_proj.niml.dset                       \
           -prefix roi_proj_smooth.niml.dset
   ConvertDset -i roi_proj_smooth.niml.dset -o roi_proj_smooth.1D.dset

endif

# -cmask '-a HCA_lr_v0.9.nii.gz+tlrc[0] -expr astep(a,0.000000)'

echo "#index label right_vol left_vol r/l_asymm $isosurf_labels $patch_labels" > $reportfile
foreach roi (`count -digits 2 1 $#right_range`)
   set righti = $right_range[$roi] 
   set lefti  = $left_range[$roi] 

   set resultline = $righti

   # get the label of the roi, stripping off any leading Right_
   set roilab = \
   `whereami -DAFNI_ATLAS_NAME_TYPE=name \
    -dset $inset -index_to_label $righti |sed 's/Right_//'` 

   set resultline = "$resultline $roilab"

   # compute right and left volumes
   set rightv = `3dBrickStat -volume -non-zero $inset"<$righti>"`
   set leftv  = `3dBrickStat -volume -non-zero $inset"<$lefti>"`

   # compute asymmetry as ratio of right to left volumes
   set asymm = `ccalc "$rightv/$leftv"`

   set rightv = `ccalc -form "%.1f" $rightv`
   set leftv = `ccalc -form "%.1f" $leftv`
   set asymm = `ccalc -form "%.3f" $asymm`

   set resultline = "$resultline $rightv $leftv $asymm"

   if ( $isosurfs ) then
      set righti_1d = `ccalc -int $righti`
      set lefti_1d = `ccalc -int $lefti`
      SurfaceMetrics -area -prefix temp.1D -overwrite \
        -i ${isosurf_dir}/${isosurf_base}*.k${righti_1d}.gii > /dev/null
      set righta = `3dTstat -sum  -prefix stdout temp.1D.area'[1]'\' `
      SurfaceMetrics -area -prefix temp.1D -overwrite \
        -i ${isosurf_dir}/${isosurf_base}*.k${lefti_1d}.gii > /dev/null
      set lefta = `3dTstat -sum  -prefix stdout temp.1D.area'[1]'\' `
      # compute asymmetry as ratio of right to left volumes
      set surf_asymm = `ccalc "$righta/$lefta"`
   
      set righta = `ccalc -form "%.1f" $righta`
      set lefta = `ccalc -form "%.1f" $lefta`
      set surf_asymm = `ccalc -form "%.3f" $surf_asymm`
      set resultline = "$resultline     $righta $lefta $surf_asymm"
   endif

   if ($patch_areas == "1") then
      SurfMeasures                                           \
            -spec       asymmquick_patch.spec                      \
            -sv         $inset                               \
            -surf_A     $fullatlas_surf                      \
            -func       n_area_A                             \
            -nodes_1D   'roi_proj_smooth.1D.dset'       \
            -cmask "-a roi_proj_smooth.1D.dset[0] -expr equals(a,$righti)"  \
            -out        temppatch_rightarea.1D.dset -overwrite                        
      set rightparea = `3dBrickStat -sum temppatch_rightarea.1D.dset'[1]'`
      SurfMeasures                                           \
            -spec       asymmquick_patch.spec                      \
            -sv         $inset                               \
            -surf_A     $fullatlas_surf                      \
            -func       n_area_A                             \
            -nodes_1D   'roi_proj_smooth.1D.dset[0]'       \
            -cmask "-a roi_proj_smooth.1D.dset[0] -expr equals(a,$lefti)"  \
            -out        temppatch_leftarea.1D.dset -overwrite                        
      set leftparea = `3dBrickStat -sum temppatch_leftarea.1D.dset'[1]'`
      set parea_asymm = `ccalc "$rightparea/$leftparea"`
   
      set rightparea = `ccalc -form "%.1f" $rightparea`
      set leftparea = `ccalc -form "%.1f" $leftparea`
      set parea_asymm = `ccalc -form "%.3f" $parea_asymm`

      set resultline = "$resultline     $rightparea $leftparea $parea_asymm"
   endif

   echo $resultline >> $reportfile

end

exit 0


HELP:

cat << SCRIPT_HELP_STRING

Overview ~1~

This is a script to compute volumes and asymmetry ratios between
left and right regions from labeled datasets. These can be in a 
native subject space or in a standard template space, as for an atlas.

Usage Example ~1~

   asymm_report.csh                                 \
      -input HCA_lr_v0.9.nii.gz                     \
      -right_range 1 21                             \
      -left_range 31 51                             \
      -isosurf_dir ~/HCA_subjects/sub-101/surfs     \
      -isosurf_base HCA                             \
      -reportfile HCA_asymm_report.txt

   Note only the input dset and the left and right ranges are *required*.


Options ~1~

  -input input_dset    :required input dataset for relative region sizes
  -right_range min max :minimum and maximum index values for right regions
  -left_range  min max :minimum and maximum index values for left regions
  -isosurf_dir dir     :directory with gifti surface files for regions 
                        (default current directory)
  -isosurf_base prefix :beginning part of gifti file names shared across regions   
  -make_isosurfs       :make isosurfaces from all regions in input volume
                        uses isosurf_dir for directory and isosurf_base for
                        prefix for output, e.g. isosurf_base.gii
  -surf_patch          :compute areas of patches on surfaces
  -surf_patch_surface  surfdset :use one isosurface from labeled volume for patches
                        (different than isosurfaces for individual regions)
  -make_patch_surface  :generate new surface for patch area computations
  -reportfile textout  :specify filename for text output (default is stdout)

SCRIPT_HELP_STRING

   exit 0

# -------------
