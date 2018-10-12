#!/usr/bin/env python


import lib_fat_funcs as GR
import lib_fat_plot_sel as PS
from numpy import set_printoptions
import getopt, sys 
from glob import glob

# wiki.scipy.org/Cookbook/Matplotlib/Show_colormaps

def main(argv):
    '''Basic plotting and copying of files.'''

    help_line = ''' \

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

    fat_mat_sel.py  --Pars='T S R ...'                                 \\
        { --matr_in=MATR_FILES | --list_match=LIST }                   \\
        { --out_ind_matr | --out_ind_1ddset }  --ExternLabsNo          \\ 
        --Hold_image  --type_file=TYPE  --dpi_file=DPI                 \\
        --xlen_file=LX  --ylen_file=LY  --Tight_layout_on  --Fig_off   \\
        --Size_font=S  --Lab_size_font=S2   --Cbar_off                 \\
        --A_plotmin=MIN  --B_plotmax=MAX  --width_cbar_perc=W          \\
        --Map_of_colors=MAP  --cbar_int_num=N  --specifier=STR         \\
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
       $ fat_mat_sel.py --matr_in 'o.NETS_AND_000.grid' --Pars 'FA'     \\
            -A_plotmin 1 --Tight_layout_on --Hold_image --out_ind_matr



'''


    file_matr_glob = ''
    file_listmatch = ''
    par_str = ''
    SWITCH_ExternLabsOK = 1

    DO_COLORBAR = 1
    DO_PLOT = 1
    TIGHT_LAY = 0
    OUT_GRID = 0
    DO_hold_image = 0
    DO_XTICK_LABS = 1

    FS = 10                        # fontsize of whole plot
    N_CBAR_INT = 4
    FTYPE = 'jpg'
    MATDPI = 80
    MAT_X = -1.
    MAT_Y = -1.
    WIDTH_CBAR_PERC  = "5"
    LAB_SIZE_FONT = 10 

    MATMIN_str = ''
    MATMAX_str = ''
    SPEC_FORM = ''
    MAP_of_COL = 'jet'

    # allow status 0 on -help   24 Sep 2018 [rickr]
    if "-help" in argv:
        print help_line
        sys.exit()

    try:
        opts, args = \
         getopt.getopt(argv,
                       "hECFTXoHOS:M:c:t:d:x:y:m:l:P:A:B:s:w:L:",
                       [ "help", "ExternLabsNo",
                         "Cbar_off",
                         "Fig_off",
                         "Tight_layout_on",
                         "Xtick_lab_off",
                         "out_ind_matr",
                         "Hold_image",
                         "Out_ind_1ddset",
                         "Size_font=",
                         "Map_of_colors="
                         "cbar_int_num=",
                         "type_file=",
                         "dpi_file=",
                         "xlen_file=",
                         "ylen_file=",
                         "matr_in=",
                         "list_match=",
                         "Pars=",
                         "A_plotmin=",
                         "B_plotmax=",
                         "specifier="
                         "width_cbar_perc=",
                         "Lab_size_font="])

    except getopt.GetoptError:
        print help_line
        sys.exit(2)
    for opt, arg in opts:
        if opt in ("-h", "--help", "-help"):
            print help_line
            sys.exit()
        elif opt in ("-m", "--matr_in"):
            file_matr_glob = arg
        elif opt in ("-l", "--list_match"):
            file_listmatch = arg
        elif opt in ("-P", "--Pars"):
            par_str = arg
        elif opt in ("-E", "--ExternLabsNo"):
            SWITCH_ExternLabsOK = 0
        elif opt in ("-C", "--Cbar_off"):
            DO_COLORBAR = 0
        elif opt in ("-F", "--Fig_off"):
            DO_PLOT = 0
        elif opt in ("-T", "--Tight_layout_on"):
            TIGHT_LAY = 1
        elif opt in ("-X", "--Xtick_lab_off"):
            DO_XTICK_LABS = 0
        elif opt in ("-S", "--Size_font"):
            FS = int(arg)
        elif opt in ("-H", "--Hold_image"):
            DO_hold_image = 1
        elif opt in ("-c", "--cbar_int_num"):
            N_CBAR_INT = int(arg)
        elif opt in ("-t", "--type_file"):
            FTYPE = arg
        elif opt in ("-d", "--dpi_file"):
            MATDPI = int(arg)
        elif opt in ("-x", "--xlen_file"):
            MAT_X = float(arg)
        elif opt in ("-y", "--ylen_file"):
            MAT_Y = float(arg)
        elif opt in ("-o", "--out_ind_matr"):
            OUT_GRID = 1
        elif opt in ("-O", "--Out_ind_1ddset"):
            OUT_GRID = 2
        elif opt in ("-A", "--A_plotmin"):
            MATMIN_str = arg
        elif opt in ("-B", "--B_plotmax"):
            MATMAX_str = arg
        elif opt in ("-s", "--specifier"):
            SPEC_FORM = arg
        elif opt in ("-M", "--Map_of_colors"):
            MAP_of_COL = arg
        elif opt in ("-w", "--width_cbar_perc"):
            WIDTH_CBAR_PERC = arg
        elif opt in ("-L", "--Lab_size_font"):
            LAB_SIZE_FONT = int(arg)

    if ( file_matr_glob == '' ) and ( file_listmatch == '' ):
        print "** ERROR: missing a necessary matrix file input."
        print "\t Need to use either '-m' or '-l'."
        print "\t Use 'fat_mat_sel.py -h' for viewing helpfile."
        sys.exit()
    if not( file_matr_glob == '' ) and not( file_listmatch == '' ):
        print "*+ Warning: both a path for globbing *and* a listfile have",
        print " been input for the matrix file."
        print "\tThe glob one after '-m' will be ignored."
    if not(par_str):
        print "** Error! Got no parameters (-P, --Pars)."
        sys.exit(5)
    if MAT_X < 0 :  # default option
        MAT_X = 3.5
    if MAT_Y < 0 :
        if DO_COLORBAR : 
            MAT_Y = 0.8*MAT_X
        else:
            MAT_Y = MAT_X
    if N_CBAR_INT < 0:
        print "** Error! Bad number of colorbar divisions: ", N_CBAR_INT
        sys.exit(3)
    if MATDPI < 0:
        print "** Error! Bad DPI choice: ", MATDPI
        sys.exit(3)
    if FS < 0:
        print "** Error! Bad font size: ", FS
        sys.exit(3)
    if (MAT_X < 0) or (MAT_Y < 0) :
        print "** Error! Negative dimension?: ", MAT_X, " by ", MAT_Y
        sys.exit(3)

    if not(DO_PLOT) and not( OUT_GRID):
        print "** Error! Nothing to do: all outputfunctionality is off!"
        print "\tDid you either want to:"
        print "\t\t-> view a parameter's matrix (default; don't use: ",
        print "-F, --File_off), or"
        print "\t\t-> select matrices to a text file (-o, --out_ind_matr)?\n"
        sys.exit(2)


    return file_matr_glob, file_listmatch, SWITCH_ExternLabsOK, \
     par_str, FS, DO_COLORBAR, N_CBAR_INT, FTYPE, DO_PLOT, \
     TIGHT_LAY, MATDPI, MAT_X, MAT_Y, OUT_GRID, MATMIN_str, MATMAX_str, \
     SPEC_FORM, MAP_of_COL, DO_hold_image, WIDTH_CBAR_PERC, LAB_SIZE_FONT, \
     DO_XTICK_LABS

########################################################################

if __name__=="__main__":
    set_printoptions(linewidth=200)
    print "\n"
    file_matr_glob, file_listmatch, ExternLabsOK, \
     par_str, FS, DO_COLORBAR, N_CBAR_INT, FTYPE, DO_PLOT, \
     TIGHT_LAY, MATDPI, MAT_X, MAT_Y, OUT_GRID, MATMIN_str, MATMAX_str, \
     SPEC_FORM, MAP_of_COL, DO_hold_image, WIDTH_CBAR_PERC, LAB_SIZE_FONT, \
     DO_XTICK_LABS = main(sys.argv[1:])
    

    # parameter names
    if par_str:
        par_list = par_str.split()
        USER_LIST = 1
    else:
        print "** Error! Got no parameters."
        sys.exit(5)


    # get file list from either of two ways.
    if file_listmatch:
        list_all = GR.ReadSection_and_Column(file_listmatch, 0)
    elif file_matr_glob:
        list_all = glob(file_matr_glob)
    else:
        print "** Error! Cannot read in matrix files."
        sys.exit(4)

    if not(list_all):
        print "** Error! Could not find/read in any matrix files."
        sys.exit(4)

    # this one gets the matched pair name.
    if GR.IsFirstUncommentedSection_Multicol(file_listmatch):
        list_all_out = GR.ReadSection_and_Column(file_listmatch, 1)
    else:
        # just parsing off endings, attaching par+postfix later
        list_all_out, types_all_out = PS.DefaultNamingPrefType(list_all)


    print list_all
    print list_all_out
    print types_all_out

    image_list = []
    matfile_list = []
    
    # for each file
    for i in range(len(list_all)):
        x = list_all[i]
        xpref = list_all_out[i]
        xtype = types_all_out[i]
        # get the matrix file's tuple of properties
        Xtup = GR.LoadInGridOrNetcc(x)

        # for each parameter
        for p in par_list:

            if DO_PLOT:
                image_out = PS.Fat_Mat_Plot( Xtup, 
                                             p, 
                                             xpref,
                                             xtype,
                                             FS,
                                             DO_COLORBAR,
                                             MAP_of_COL,
                                             N_CBAR_INT,
                                             WIDTH_CBAR_PERC, 
                                             LAB_SIZE_FONT,
                                             FTYPE,
                                             DO_PLOT,
                                             TIGHT_LAY,
                                             DO_XTICK_LABS,
                                             MATDPI,
                                             MAT_X,
                                             MAT_Y,
                                             MATMIN_str, 
                                             MATMAX_str,
                                             SPEC_FORM,
                                             ExternLabsOK,
                                             DO_hold_image)
                image_list.append(image_out)

            if OUT_GRID:
                xsub = PS.Get_Subset_of_inMatrix( Xtup, [p] )
                matfile_out = PS.Write_Out_Matrix( xsub, xpref, 
                                                   xtype, OUT_GRID,
                                                   ExternLabsOK )
                matfile_list.append(matfile_out)

    if image_list:
        print '++ Finished saving images:'
        for x in image_list:
            print '\t %s' % (x)
    if matfile_list:
        print '\n++ Finished writing matrix files:'
        for x in matfile_list:
            print '\t %s' % (x)

    print "\n++ Done!\n"
