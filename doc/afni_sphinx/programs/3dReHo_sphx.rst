.. _ahelp_3dReHo:

******
3dReHo
******

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
      REHO/Kendall W code, written by PA Taylor (July, 2012), part of FATCAT
      (Taylor & Saad, 2013) in AFNI.
    
      ReHo (regional homogeneity) is just a renaming of the Kendall's W
      (or Kendall's coefficient of concordance, KCC, (Kendall & Babington
      Smith, 1939)) for set of time series.  Application to fMRI data was
      described in paper: <<Regional homogeneity approach to fMRI data
      analysis>> by Zang, Jiang, Lu, He, and Tiana (2004, NeuroImage),
      where it was applied to the study of both task and resting state
      functional connectivity (RSFC).
      
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
      + USAGE: This program is made to read in data from 4D time series data set
             and to calculate Kendall's W per voxel using neighborhood voxels. 
             Instead of the time series values themselves, Kendall's W uses the
             relative rank ordering of a 'hood over all time points to evaluate
             a parameter W in range 0-1, with 0 reflecting no trend of agreement
             between time series and 1 reflecting perfect agreement. From W, one
             can simply get Friedman's chi-square value (with degrees of freedom
             equal to `the length of the time series minus one'), so this can
             also be calculated here and returned in the second sub-brick:
             chi-sq = (N_n)*(N_t - 1)*W,   with N_dof = N_t - 1,
             where N_n is the size of neighborhood; N_t is the number of 
             time points; W is the ReHo or concordance value; and N_dof is the
             number of degrees of freedom. A switch is included to have the 
             chi-sq value output as a subbrick of the ReHo/W. (In estimating W,
             tied values are taken into account by averaging appropriate 
             rankings and adjusting other factors in W appropriately, which 
             only makes a small difference in value, but the computational time
             still isn't that bad).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND:  3dReHo -prefix PREFIX -inset FILE {-nneigh 7|19|27}    \
                     {-chi_sq}  {-mask MASK}  {-in_rois INROIS} 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + RUNNING, need to provide:
        -prefix PREFIX  :output file name part.
        -inset  FILE    :time series file. 
    
        -chi_sq         :switch to output Friedman chi-sq value per voxel
                         as a subbrick.
        -mask   MASK    :can include a whole brain mask within which to
                         calculate ReHo. Otherwise, data should be masked
                         already.
    
        -nneigh NUMBER  :number of voxels in neighborhood, inclusive; can be: 
                         7   (for facewise neighbors, only),
                         19  (for face- and edge-wise neighbors),
                         27  (for face-, edge-, and node-wise neighbors).
                         The default is: 27.
        -neigh_RAD   R  :for additional voxelwise neighborhood control, the 
                         radius R of a desired neighborhood can be put in; R is
                         a floating point number, and must be >1. Examples of
                         the numbers of voxels in a given radius are as follows
                         (you can roughly approximate with the ol' 4*PI*(R^3)/3
                         thing): 
                                 R=2.0 -> V=33,
                                 R=2.3 -> V=57, 
                                 R=2.9 -> V=93, 
                                 R=3.1 -> V=123, 
                                 R=3.9 -> V=251, 
                                 R=4.5 -> V=389, 
                                 R=6.1 -> V=949,
                         but you can choose most any value.
        -neigh_X   A    
        -neigh_Y   B    :as if *that* weren't enough freedom, you can even have
        -neigh_Z   C     ellipsoidal volumes of voxelwise neighbors.  This is
                         done by inputing the set of semi-radius lengths you
                         want, again as floats/decimals. The 'hood is then made
                         according to the following relation:
                             (i/A)^2 + (j/B)^2 + (k/C)^2 <=1.
                         which will have approx. V=4*PI*A*B*C/3. The impetus for
                         this freedom was for use with data having anisotropic 
                         voxel edge lengths.
        -box_RAD   BR   :for additional voxelwise neighborhood control, the
                         one can make a cubic box centered on a given voxel;
                         BR specifies the number of voxels outward in a given
                         cardinal direction, so the number of voxels in the
                         volume would be as follows:
                                 BR=1 -> V=27,
                                 BR=2 -> V=125, 
                                 BR=3 -> V=343, 
                         etc. In this case, BR should only be integer valued.
        -box_X   BA    
        -box_Y   BB    :as if that *still* weren't enough freedom, you can have
        -box_Z   BC     box volume neighborhoods of arbitrary dimension; these
                        values put in get added in the +/- directions of each
                        axis, so the volume in terms of number of voxels would
                        be calculated:
                              if BA = 1, BB = 2 and BC = 4, 
                              then V = (1+2*1)*(1+2*2)*(1+2*4) = 135.
             --> NB: you can't mix-n-match '-box_*' and '-neigh_*' settings.
                     Mi dispiace (ma sol'un po).
    
        -in_rois INROIS :can input a set of ROIs, each labelled with distinct
                         integers. ReHo will be calculated per ROI. The output
                         will be similar to the format of 3dROIstats: one row
                         of numbers per INROIS subbrick, and the number of 
                         columns determined by the number of ROIs per subbrick
                         (but only numbers are output). The output of this is
                         in a file called PREFIX_ROI_reho.vals, and if
                         `-chi_sq' values are being output, then those for the
                         ROI values will be output in an analogously formatted
                         file called PREFIX_ROI_reho.chi.
                         Voxelwise ReHo will still be calculated and output.
    
      + OUTPUT: 
             [A] single file with name, e.g., PREFIX+orig.BRIK, which may have
                  two subbricks (2nd subbrick if `-chi_sq' switch is used):
                  [0] contains the ReHo (Kendall W) value per voxel;
                  [1] contains Friedman chi-square of ReHo per voxel (optional);
                      note that the number of degrees of freedom of this value
                      is the length of time series minus 1.
             [B] can get list of ROI ReHo values, as well (optional).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
           3dReHo                         \
             -mask MASK+orig.             \
                  -inset REST+orig        \
                  -prefix REST_REHO       \
                  -neigh_RAD 2.9          \
                  -chi_sq
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
