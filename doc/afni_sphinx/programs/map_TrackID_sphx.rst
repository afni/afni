***********
map_TrackID
***********

.. _map_TrackID:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
      Supplementary code for 3dTrackID, written by PA Taylor, part of FATCAT
      (Taylor & Saad, 2013) in AFNI.
      
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      USAGE: This program maps the track file (*.trk) output of 3dTrackID to
      another space, such as MNI standard, using the 1Dmatrix_save info of
      3dAllineate.  The scalar values are not changed or interpolated to within,
      the new space, but instead they just migrate along-- in practice, this
      should be fine, since they should move along with associated/underlying
      voxels as one used the 1Dmatrix to shift, e.g., the 3D FA, MD, etc. data
      sets.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      COMMAND: map_TrackID -prefix FILE -in_trk FILE -in_map FILE -ref FILE \
               {-verb  -line_only_num -already_inv}
      
    
      OUTPUTS (named using prefix, PREF):  
        1) TRK file, named PREF.trk, mapped to new space (view in TrackVis).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      RUNNING, need to provide:
        -prefix  OUT_PREF :this will be the prefix of the output track file,
                           `OUT_PREF.trk'.
        -in_trk  TRK_FILE :the name of the *.trk file to be mapped. Must be a
                           TrackVis readable file, and probably one created
                           by using 3dTrackID with the (newly added) `-rec_orig'
                           option (see 3dTrackID help for description, short
                           reason being: TrackVis currently doesn't use origin
                           info, and to have image pop up in the middle of the 
                           TrackVis viewer, I left default origin being 0,0,0).
        -in_map  1D_MATR  :single line of matrix values for the transformation
                           of old x-coor to new x'-coor via:
                                 x' = Ux+V.
                           Have only tested this with the 1D_MATR file coming
                           from `3dAllineate -1Dmatrix_save 1D_MATR...' command.
                           NB: map_TrackID has been written to just use the
                           aformentioned 1D_MATR file spewed out by 3dAllineate,
                           which has a line of text followed by 12 params in a 
                           single line (see 3dAllineate help for more info):
                           u11 u12 u13 v1 u21 u22 u23 v2 u31 u32 u33.
                           However, you can also use outputs of cat_matvec,
                           which don't have a text line, so you would then want
                           to then use the `-line_only_num' option (below).
                           A more subtle point: for whatever reason, when 
                           the U-matrix and V-vector are applied in this code, 
                           they actually have to be applied as if they had been
                           given for the inverse transform x and x', i.e.:
                                x' = U^{-1}x - U^{-1}V, 
                           where U^{-1} is the inverse of U.  Therefore, if you
                           get your transformation from non-3dAllineate usage,
                           you might have to invert what you mean by U and V
                           here. If you use your `backward' matrix/vectors, or
                           if you use cat_matvec to invert your matrix or
                           something, then use the `-already_inv' switch 
                           (below). 
                           HOWEVER, to avoid confusion, and to not cause worry
                           if the preceding discussion didn't make sense, the
                           *default* running of the code is to just use the
                           standard 1D_MATR of 3dAllineate to give appropriate
                           transform.  If you use another program, and if your
                           results look inverted/flipped/rotated/translated,
                           then consider the above!
        -ref     TO_FILE  :3D data set in space to which TRK_FILE is being
                           mapped. Mainly to read the header for necessary info.
        and the following options (all are just switches):
        -verb             :Verbose output. 
        -orig_zero        :put (0,0,0) as the origin in the output *.trk file,
                           as opposed to having the `real' values recorded.
                           TrackVis does not really use the origin for much,
                           but having a nonzero-origin will cause the location
                           of the tracks in the viewer window to be off-center,
                           and it sets the rotation-in-space axis about the,
                           origin with the combined effect that a nonzero-origin
                           can be a bit more difficult to view and manipulate;
                           however, if you might want to map the tracks again
                           later, then you would want to have the `real' origin
                           values recorded. (Default: off.)
        -line_only_num    :if your 1D_MATR file is just 12 numbers in a row,
                           like after using cat_matvec or some other program.
                           Default is to skip the little verbiage in the first
                           line, as included in `3dAllineate -1Dmatrix_save...'.
        -already_inv      :if you have inverted a mapping or use some other
                           program than 3dAllineate, whose transformation matrix
                           and vector get applied a bit differently than one
                           (i.e., me) might have thought (and see long `-in_map'
                           description above for more in depth info); as guide,
                           one might try this option if transform looks to be
                           backwards, flipped or shifted oddly, esp. if not just
                           making use of output of 3dAllineate.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
     EXAMPLE (with view toward PTaylor_TractDemo files, using MNI as ref):
          map_TrackID                                       \
            -prefix TEST_FILES/DTI/o.TRACK_to_MNI           \
            -in_trk TEST_FILES/DTI/o.TRACK_ballFG.trk       \
            -in_map TEST_FILES/DTI/map_to_refMNI.aff12.1D   \
            -ref TEST_FILES/DTI/MNI_3mm+tlrc 
       which could be run after, for example:
          3dAllineate                                       \
            -1Dmatrix_save TEST_FILES/DTI/map_to_refMNI     \
            -input TEST_FILES/DTI/DT_FA+orig.               \
            -base TEST_FILES/DTI/MNI_3mm+tlrc               \
            -mi                                             \
            -prefix TEST_FILES/DTI/MNI_DT_FAn
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
