**************
fat_mat_sel.py
**************

.. _ahelp_fat_mat_sel.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    
     
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
        ++ Oct, 2014.  Written by PA Taylor (UCT/AIMS).  
        ++ Perform simple matrix plotting operations (e.g., correlation or 
           structural property matrices) from outputs of FATCAT programs
           3dNetCorr and 3dTrackID.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
       INPUT:   a matrix file and a selection of one or more parameter names.
     
       OUTPUT:  1) one or more individual images of matrix plots
                   + can have colorbar 
                   + can edit various color/font/size/filetype features
                   + probably a matrix will flash on the screen and pass, even
                     if it's saved to a file (but you can preserve it, waiting for
                     a button to be pushed, if you wish);
                2) individual matrix grid or 1D.dset file, which might be useful 
                     for viewing specific properties or for importing to other 
                     programs for further analysis.
    
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
       COMMAND OPTIONS:
    
        fat_mat_sel.py  --Pars='T S R ...'                                 \
            { --matr_in=MATR_FILES | --list_match=LIST }                   \
            { --out_ind_matr | --out_ind_1ddset }  --ExternLabsNo          \ 
            --Hold_image  --type_file=TYPE  --dpi_file=DPI                 \
            --xlen_file=LX  --ylen_file=LY  --Tight_layout_on  --Fig_off   \
            --Size_font=S  --Lab_size_font=S2   --Cbar_off                 \
            --A_plotmin=MIN  --B_plotmax=MAX  --width_cbar_perc=W          \
            --Map_of_colors=MAP  --cbar_int_num=N  --specifier=STR         \
            --Xtick_lab_off
    
    
        -P, --Pars='T S R ...'   :supply names of parameters, separated by 
                                  whitespace, for selecting from a matrix file
        -m, --matr_in=MATR_FILES :one way of providing the set of matrix
                                  (*.grid or *.netcc) files- by searchable
                                  path.  This can be a globbable entry in
                                  quotes containing wildcard characters,
                                  such as 'DIR1/*/*000.grid'.
                                  If this option is used instead of '-l',
                                  below, then this program tries to match
                                  each CSV subj ID to a matrix file by 
                                  finding which matrix file path in the
                                  MATR_FILES contains a given ID string;
                                  this method may not always find unique
                                  matches, in which case, use '-l'
                                  approach.
        -l, --list_match=LIST    :another way of inputting the matrix
                                  (*.grid or *.netcc) files-- by explicit
                                  path, matched per file with a CSV
                                  subject ID.
                                  The LIST text file contains two columns:
                                  col 1: path to subject matrix file.
                                  col 2: CSV IDs,
                                  (first line can be a '#'-commented one.
        -o, --out_ind_matr       :output individual matrix files of properties,
                                  which might be useful for viewing or entering
                                  into other programs for analysis.  The new file
                                  will have the same prefix as the old, with the
                                  name of the parameter appended at the end of the
                                  handle, such as, e.g.: PREFIX_000.grid -> 
                                  PREFIX_000_FA.grid  or  PREFIX_000_CC.netcc.
        -O, --Out_ind_1ddset     :output as a 1D dset, more easily readable by
                                  other AFNI (or just plain 'other' programs); 
                                  element labels are commented, and filenames 
                                  are similar to those of '--out_ind_matr', but
                                  endings with '1D.dset', such as 
                                  PREFIX_000_FA.1D.dset.
    
        -H, --Hold_image         :switch to hold the Python-produced image on the
                                  output screen until a key has been hit; it puts
                                  a 'raw_input()' line in, if you are curious  
                                  (default:  not to do so -> meaning the image 
                                  flashes briefly when running from a commandline, 
                                  and not from, for example, ipython). Even without
                                  this switch used, the image can be saved.
        -E, --ExternLabsNo       :switch to turn off the writing/usage of 
                                  user-defined labels in the *.grid/*.netcc 
                                  files.  Can't see why this would be desired,
                                  to be honest.
    
        -t, --type_file=TYPE     :Can select from a full range of image formats:
                                  jpg (default), pdf, png, tif, etc. (whatever
                                  your computer will allow).           
        -d, --dpi_file=DPI       :set resolution (dots per inch) of output image 
                                  (default = 80).
        -x, --xlen_file=LX       :horizontal dimension of output saved image, in 
                                  units of inches (default = 3.5).
        -y, --ylen_file=LY       :vertical dimension of output saved image, in 
                                  units of inches (default = 3.5 if no colorbar
                                  is used, and 2.8 if colorbar is used).
        -T, --Tight_layout_on    :use matplotlib's tight_layout() option, to ensure
                                  no overlap of features (hopefully) in the image.
        -F, --Fig_off            :switch if you *don't* want a matrix figure output
                                  (default is to save one).
    
        -S, --Size_font=S1       :set font size for colorbar and title 
                                  (default = 10).
        -L, --Lab_size_font=S2   :set font size for x- and y-axis labels 
                                  (default = 10).
        -A, --A_plotmin=MIN      :minimum colorbar value (default is the minimum
                                  value found in the matrix).
        -B, --B_plotmax=MAX      :maximum colorbar value (default is the maximum
                                  value found in the matrix).
        -C, --Cbar_off           :switch to not include a colorbar at the right
                                  side of the plot (default is to have one).
        -M, --Map_of_colors=MAP  :change the colormap style used in the plot; a 
                                  full list of options for Python-matplotlib is
                                  currently available here:
                                  wiki.scipy.org/Cookbook/Matplotlib/Show_colormaps
                                  (default: 'jet')
        -c, --cbar_int_num=N     :set the number of intervals on the colorbar; the
                                  number of numbers shown will be N+1 (default: 
                                  N = 4).
        -w, --width_cbar_perc=W  :width of colorbar as percentage (0, 100) of width
                                  of correlation matrix (default = 5).
        -s, --specifier=STR      :format the numbers in the colorbar; these can be
                                  used to specify numbers of decimal places on 
                                  floats (e.g., '%.4f' has four) or to use 
                                  scientific notation ('%e') (default: trying to 
                                  guess int or float, the latter using three 
                                  decimal places.)
        -X, --Xtick_lab_off      :switch to turn off labels along x- (horizontal)
                                  axis but to leave those along y- (vertical) axis.
                                  Can be used in conjunction with other label-
                                  editing/specifying options (default: show labels
                                  along both axes).
    
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
       EXAMPLE:
           $ fat_mat_sel.py -m 'o.NETS_AND_000.grid' -P 'FA' -A 1 -T -H -o
    
         or, equivalently:
           $ fat_mat_sel.py --matr_in 'o.NETS_AND_000.grid' --Pars 'FA'     \
                -A_plotmin 1 --Tight_layout_on --Hold_image --out_ind_matr
    
    
    
