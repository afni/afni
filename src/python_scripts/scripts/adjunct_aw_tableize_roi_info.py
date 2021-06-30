#!/usr/bin/env python

# python3 status: compatible

# simple program for parsing 3dROIstats output for a file of interest,
# as well as a reference file

AUTHOR    = "PA Taylor (NIMH, NIH)"
#VERSION   = "1.0" ; VER_DATE  = "May 19, 2020"
# [PT] birth
#
#VERSION   = "1.1" ; VER_DATE  = "May 20, 2020"
# [PT] require masks as input
#
#VERSION   = "1.2" ; VER_DATE  = "May 20, 2020"
# [PT] header+table complete
#    + forms working beta version, rollable
#
#VERSION   = "1.21" ; VER_DATE  = "May 20, 2020"
# [PT] fixed logic in creating new_inp* lists (merging inp/ref info)
# 
#VERSION   = "1.3" ; VER_DATE  = "May 20, 2020"
# [PT] Glenian updates
#    + rename some cols in output
#    + add more header info: any lost ROIs
#
#VERSION   = "1.4" ; VER_DATE  = "May 21, 2020"
# [PT] adjust 3dROIstats call in afni_3droistats, to account for ROIs
#      having both long and short labels: use env var in cmd line to go
#      for "names" (i.e., the short version) only
# 
#VERSION   = "1.5" ; VER_DATE  = "May 21, 2020"
# [PT] require ${modesmooth} as input arg (-> modesmoo)
#
#VERSION   = "1.51" ; VER_DATE  = "May 21, 2020"
# [PT] if there are LOST_ROI_VALUES, now output an AFNI-style encoded
#      string in the footer of the table, that could be grepped for and
#      used
#
#VERSION   = "1.52" ; VER_DATE  = "May 30, 2020"
# [PT] have to just use comma-separated list for ROI-value selector that
#      goes inside <>
#
#VERSION   = "1.6" ; VER_DATE  = "June 1, 2020"
# [PT] new table format
#    + {U,W} --> {A,B}
#    + put in a KEY section, defining cols
#    + remove a couple (once useful, but now annoying) print statements
#
#VERSION   = "1.61" ; VER_DATE  = "June 28, 2021"
# [PT] fix table misnomer---thanks, Adam Messinger!
#    + also change/simplify report col head names 4 clrty
#
VERSION   = "1.62" ; VER_DATE  = "June 30, 2021"
# [PT] more key term tweaks
#
# =================================================================

import sys       as sys
from   afnipy    import afni_util as au    
from   afnipy    import afni_base as BASE

THIS_PROG = 'adjunct_aw_tableize_roi_info.py'
NUM_ARGS  = 6
EPS_FLT   = 10.**-6

help_string = '''

--------------------------------------------------------------------
Helpfile for:    ***  %s  ***
Version num:     %s
Version dat:     %s
Written by:      %s

Just a simple helper function for the fat_proc* scripts.  

Takes >= %d arguments: 
   1) an output file name;
   2) an (warped) atlas of interest, with subbrick selector, if necessary;
   3) a mask for the (warped) atlas (same grid)
   4) a reference atlas (i.e., same one but unwarped), with (same) 
      subbrick selector, if necessary.
   5) a mask for the reference atlas (same grid)
   6) a "modesmooth" value, from modal smoothing used after warping

The output file name will be simple text, containing ROI count/size
information.

--------------------------------------------------------------------

'''  % (THIS_PROG, VERSION, VER_DATE, AUTHOR, NUM_ARGS)

# ================================================================

def get_arg(aa):
    Narg = len(aa)
    
    if Narg == 0 or \
       (Narg == 1 and ["-h", "-hview", "-help"].__contains__(aa[0])):
        print(help_string)
        sys.exit(0)
    elif Narg != NUM_ARGS :
        sys.exit("\n** ERROR: wrong number of args! Need {}, not {}.\n"
                 "          See help file."
                 "".format(NUM_ARGS, Narg))
    else:
        ofile    = aa[0]         # output fname
        atl_inp  = aa[1]         # warped input file
        mask_inp = aa[2]         # warped input file
        atl_ref  = aa[3]         # reference input file
        mask_ref = aa[4]         # warped input file
        modesmoo = aa[5]         # ${mode_smooth} value in @animal_warper

        print("++ Output file : {}".format(ofile))
        #print("++ atlas input :", atl_inp)
        #print("++ mask input  :", mask_inp)
        #print("++ atlas ref   :", atl_ref)
        #print("++ mask ref    :", mask_ref)

    return ofile, atl_inp, mask_inp, atl_ref, mask_ref, modesmoo

# -------------------------------------------------------------

def afni_3dinfo_is_single_vol( fname1, verb=0 ) :
    '''Has one volume been specified (either a 3D vol, or a 4D, with
    subbrick selectors)?

    Returns True or False

    '''

    fname1.replace("[", "'[") ; fname1.replace("]", "]'")

    cmd = '''3dinfo            \
                -nv            \
                {dset1}        \
    '''.format( dset1=fname1 )

    status, so, se = BASE.simple_shell_exec(cmd, capture=1)
    
    SINGLE_VOL = int(so.split()[0]) == 1

    if verb > 0 :
        print("++ Is this a single volume?\n"
              "     {}\n"
              "   --> {}"
              "".format( fname1, SINGLE_VOL ))

    return SINGLE_VOL

# -------------------------------------------------------------

def afni_3dinfo_same_grid( fname1, fname2 ) :
    '''Are 2 dsets on the same grid? 

    Returns True or False

    '''

    fname1.replace("[", "'[") ; fname1.replace("]", "]'")
    fname2.replace("[", "'[") ; fname2.replace("]", "]'")

    cmd = '''3dinfo            \
                -same_grid     \
                {dset1}        \
                {dset2}
    '''.format( dset1=fname1, dset2=fname2 )

    status, so, se = BASE.simple_shell_exec(cmd, capture=1)
    
    SAME_GRID = bool(int(so.split()[0]))

    print("++ Are these dsets on same grid?\n"
          "     {}\n     {}\n"
          "   --> {}"
          "".format( fname1, fname2, SAME_GRID ))

    return SAME_GRID

# -------------------------------------------------------------

def afni_3dinfo_ad3( fname  ) :
    '''Get voxelsize information.
    
    output:
    list of dims 
    total volume (float)

    '''

    fname.replace("[", "'[")
    fname.replace("]", "]'")

    cmd = '''3dinfo      \
                -ad3     \
                {dset}
    '''.format( dset=fname )

    status, so, se = BASE.simple_shell_exec(cmd, capture=1)
    
    print("++ Voxel dimensions of {} are (in mm):\n"
          "       {}".format(fname, '  '.join(so.split())))
    dims = so.split()

    voxvol  = 1.0
    dims_fl = []
    for ad in dims:
        adfl = float(ad)
        voxvol*= adfl
        dims_fl.append(adfl)
    print("   ... so volume is (in mm**3): {}"
          "".format(voxvol))

    return dims_fl, voxvol

# -------------------------------------------------------------

def afni_3dBrickStat_nz_count( fname  ) :
    '''Get mask size information.

    Return int: number of nonzero voxels

    Check for zero voxels in mask (exit with error).

    '''

    fname.replace("[", "'[")
    fname.replace("]", "]'")

    cmd = '''3dBrickStat          \
                -non-zero         \
                -count            \
                {dset}
    '''.format( dset=fname )

    status, so, se = BASE.simple_shell_exec(cmd, capture=1)

    Nnzvox = int(so.strip())

    print("++ Voxel count of mask '{}' is (in mm): {}"
          "".format(fname, Nnzvox))

    if not(Nnzvox) :
        print("** ERROR: zero voxels in mask '{}'"
              "".format(fname))
        sys.exit(6)

    return Nnzvox

# -------------------------------------------------------------

def afni_3droistats( fname  ) :
    '''Get ROI size information.

    Now using the 'AFNI_ATLAS_NAME_TYPE = name' env variable, so even
    with both short and long labels present for ROIs, only the short
    will be used (and numbers will match).

    '''

    fname.replace("[", "'[")
    fname.replace("]", "]'")

    cmd = '''3dROIstats      \
                -DAFNI_ATLAS_NAME_TYPE=name \
                -nzvoxels    \
                -nobriklab   \
                -mask {dset} \
                {dset}
    '''.format( dset=fname )

    status, so, se = BASE.simple_shell_exec(cmd, capture=1)
    
    # split by line
    so_lined = so.split("\n")

    if len(so_lined) > 3 :
            print("** ERROR: unexpected length in 3dROIstats output: \n"
              "{}".format(so_lined))
            sys.exit(4)

    # ['File', 'Sub-brick', 'Mean_ROI', 'NZcount_ROI', ... ]
    so_labs = so_lined[0].split() ; Nlabs = len(so_labs)
    # ['FILE_NAME', 'SUBBRICK_IDX', 'ROI_IDX', 'ROI_COUNT', ...]
    so_vals = so_lined[1].split() ; Nvals = len(so_vals)

    #print("HEY")
    #print(so_lined[0])
    #print(so_lined[1])

    if Nlabs != Nvals :
        print("** ERROR: length of labels {} does not match that of vals {}"
              "".format(Nlabs, Nvals))
        sys.exit(2)
    elif Nlabs % 2 :
        print("** ERROR: expected even number of values per row, not {}"
              "".format(Nlabs))
        sys.exit(2)

    so_null  = so_lined[2].strip()
    if so_null :
        print("** ERROR: non-null 3rd line: \n"
              "{}".format(so_null))
        sys.exit(3)

    return so_labs, so_vals

def parse_roistats_row0( labs, n_ignore=2 ):
    '''Take [0]th row of 3dROIstats output (with specific options used)
and return a list of ROI labels, in order input.

    n_ignore = number of initial values to ignore (depends on specific
               3dROIstat options used).

    Output
    ------
    simple list of string values, each ROI label

    '''
    
    N = len(labs)
    
    list_labs = []
    # go to every other one, because there is both Mean and NZcount
    # info
    for ii in range( n_ignore, N, 2):
        # chop off initial "Mean_"
        list_labs.append( labs[ii][5:] )

    return list_labs

# ------------------------------------------------------------------

def parse_roistats_row1( vals, n_ignore=2 ):
    '''Take [1]th row of 3dROIstats output (with specific options used)
and return 2 lists: ROI indices, and the number of voxels of each, in
order input.

    n_ignore = number of initial values to ignore (depends on specific
               3dROIstat options used).

    Output
    ------
    2 simple lists of ints

    '''
    
    N = len(vals)
    
    list_idx  = []
    list_nvox = []
    # go to every other one, get info from both
    for ii in range( n_ignore, N, 2):
        jj = ii+1

        # check that values really are int
        val_int = int(float(vals[ii]))
        val_flt = float(vals[ii]) 

        if abs(val_flt - val_int) > EPS_FLT :
            print("** ERROR: ROI value does not appear to be int: {}"
                  "".format(vals[ii]))
            sys.exit(6)
        else:
            list_idx.append( val_int )

        list_nvox.append( int(vals[jj]) )

    return list_idx, list_nvox

# ---------------------------------------------------------------------

def get_all_roi_info(fname):
    '''Execute functions to run 3dROIstats and parse the output into
    lists of useful info: ROI labels, ROI indices, and ROI counts.

    '''

    list_row0, list_row1 = afni_3droistats( fname )
    roi_labs             = parse_roistats_row0( list_row0 )
    roi_vals, roi_nvox   = parse_roistats_row1( list_row1 )

    if len(roi_labs) != len(roi_vals) and \
       len(roi_labs) != len(roi_nvox) :
        print("** ERROR: list lengths don't match: \n"
              "   labels = {} \n"
              "   values = {} \n"
              "   nvoxes = {} \n"
              "".format(len(roi_labs), len(roi_vals), len(roi_nvox)))
        sys.exit(3)

    return roi_labs, roi_vals, roi_nvox

# --------------------------------------------------------------------

def pad_input_if_necessary( inp_labs, inp_vals, inp_nvox,
                            ref_labs, ref_vals, ref_nvox ):
    '''Take each set of input and ref lists; go through and see if the
input ones are missing any ROIs that the ref list has, and if so,
insert zeros there.

    Check/find match ROIs based on ROI index.  Could also tell by ROI
    label... but just using index here.

    Return: 3 new lists of input information, now the same length as
    the ref lists

    '''

    print("++ Start to check/zeropad input and ref ROI lists")

    new_inp_labs = [] ; new_inp_vals = [] ; new_inp_nvox = []

    lost_ref_vals = [] # keep track of ones that didn't make it through
    lost_ref_labs = []

    Ninp = len(inp_labs)
    Nref = len(ref_labs)

    ii = 0
    rr = 0
    # 1) Walk through both as much as possible
    while ii<Ninp and rr<Nref :
        if inp_vals[ii] == ref_vals[rr] :
            # labs *should* match
            if inp_labs[ii] != ref_labs[rr] :
                print("** WARNING: idx={:5d} labels mismatch '{}' and '{}'"
                      "".format(rr, inp_labs[ii], ref_labs[rr]))
            new_inp_labs.append( inp_labs[ii] )
            new_inp_vals.append( inp_vals[ii] )
            new_inp_nvox.append( inp_nvox[ii] )
            ii+= 1
            rr+= 1
        else:
            new_inp_labs.append( ref_labs[rr] )
            new_inp_vals.append( ref_vals[rr] )
            new_inp_nvox.append( 0 )
            lost_ref_labs.append( ref_labs[rr] )
            lost_ref_vals.append( ref_vals[rr] )
            rr+= 1
    # 2) ... and in case there are just some at end of 'ref' list
    while rr<Nref :
        new_inp_labs.append( ref_labs[rr] )
        new_inp_vals.append( ref_vals[rr] )
        new_inp_nvox.append( 0 )
        lost_ref_labs.append( ref_labs[rr] )
        lost_ref_vals.append( ref_vals[rr] )
        rr+= 1

    print("++ Check: final ii is {} out of {}"
          "".format(ii, Ninp))
    print("++ Check: final rr is {} out of {}"
          "".format(rr, Nref))
    if ii != Ninp or rr != Nref :
        print("\n** ERROR: problem matching input and ref lists:\n"
              "   Inconsistency with matching")
        sys.exit(8)

    # check
    for rr in range(Nref):
        if new_inp_vals[rr] != ref_vals[rr] :
            print("\n** ERROR: mismatch in input and ref lists:\n"
                  "   {} != {}".format( new_inp_vals[rr], ref_vals[rr] ))
            sys.exit(8)


    return new_inp_labs, new_inp_vals, new_inp_nvox, \
        lost_ref_labs, lost_ref_vals

# ------------------------------------------------------------------------------

def nvox2phys_vol( vals, voxvol ):
    '''
    Inputs
    ------
    vals   = list or int of voxel counts
    voxvol = phys volume of 1 voxel

    Output
    ------
    list (same len as 'vals') or int of physical volumes 

    '''

    if type(vals) == list :
        vols = []
        for x in vals:
            vols.append( x*voxvol )
    else:
        vols = vals*voxvol

    return vols

# ------------------------------------------------------------------------------

def nvox2frac_size( vals, Nmask ):
    '''
    Inputs
    ------
    vals   = list or int of voxel counts
    Nmask  = number of voxels in mask (already checked that val is >0)

    Output
    ------
    list (same len as 'vals') or float of fractional sizes

    '''

    if type(vals) == list :
        fracs = []
        for x in vals:
            fracs.append( float(x)/Nmask )
    else:
        fracs = float(vals)/Nmask

    return fracs


# =================================================================

if __name__=="__main__":

    # --------------------- get input ------------------------

    print("++ Command line:\n   ", ' '.join(sys.argv))
    ( ofile, atl_inp, mask_inp, atl_ref, mask_ref, \
      modesmoo ) = get_arg(sys.argv[1:])

    # check if inp atl+mask are on same grid;  check same for ref 
    inp_samegrid  = afni_3dinfo_same_grid( atl_inp, mask_inp )
    ref_samegrid  = afni_3dinfo_same_grid( atl_ref, mask_ref )
    if not(inp_samegrid) :
        print("\n** ERROR: need input dsets on same grid;\n"
              "   these do not match:\n"
              "     {}\n     {}"
              "".format(atl_inp, mask_inp))
        sys.exit(7)
    if not(ref_samegrid) :
        print("\n** ERROR: need input dsets on same grid;\n"
              "   these do not match:\n"
              "     {}\n     {}"
              "".format(atl_ref, mask_ref))
        sys.exit(7)

    FOUND_BAD = ''
    for fff in [atl_inp, mask_inp, atl_ref, mask_ref]:
        if not( afni_3dinfo_is_single_vol( fff ) ) :
            FOUND_BAD+= "     {}\n".format(fff)
    if FOUND_BAD :
        print("\n** ERROR: need to specify single vol for these inputs:\n"
              "{}".format(FOUND_BAD))
        sys.exit(8)

    # get Nvox count masks for inp and ref
    inp_mask_size = afni_3dBrickStat_nz_count( mask_inp )
    ref_mask_size = afni_3dBrickStat_nz_count( mask_ref )

    # get voxel sizes and volume
    inp_voxdims, inp_voxvol = afni_3dinfo_ad3( atl_inp )
    ref_voxdims, ref_voxvol = afni_3dinfo_ad3( atl_ref )

    inp_voxdims_str = ', '.join(['{:.3f}'.format(x) for x in inp_voxdims])
    ref_voxdims_str = ', '.join(['{:.3f}'.format(x) for x in ref_voxdims])

    # First, get raw info from 3dROIstats, and then parse that into
    # lists of numbers: ROI str labels, ROI idx values, ROI voxel
    # counts
    inp_labs, inp_vals, inp_nvox = get_all_roi_info( atl_inp )
    ref_labs, ref_vals, ref_nvox = get_all_roi_info( atl_ref )

    # how many ROIs in each; might not match, bc some ROIs might get
    # lost in mapping process;  however, should/must have:
    #   Nroi_inp <= Nroi_ref
    Nroi_inp  = len(inp_labs)
    Nroi_ref  = len(ref_labs)
    Nroi_diff = Nroi_ref - Nroi_inp

    # the inp dset might be missing some ROIs that the ref set has;
    # check, and insert zeros if that is the case. Some consistency
    # checks are also performed
    new_inp_labs, new_inp_vals, new_inp_nvox, \
        lost_ref_labs, lost_ref_vals          \
        = pad_input_if_necessary( inp_labs, inp_vals, inp_nvox,
                                  ref_labs, ref_vals, ref_nvox )
    
    all_lost_labs = ''
    all_lost_vals = ''
    all_lost_vals_list = [] 
    for ii in range( len(lost_ref_labs) ):
        if not(ii % 5) :
            all_lost_labs+= '\n# '
            all_lost_vals+= '\n# '
        all_lost_labs+= '{:<10s} '.format(lost_ref_labs[ii])
        all_lost_vals+= '{:<10d} '.format(lost_ref_vals[ii])
        all_lost_vals_list.append(lost_ref_vals[ii])

    # for later reporting; makes a string of AFNI-encoded items
    # separated with .. and , (which is NOT used right now), and a
    # simple, comma-separated list in a string
    all_lost_vals_enc_1D = au.encode_1D_ints(all_lost_vals_list)
    all_lost_vals_comma  = ','.join([str(x) for x in all_lost_vals_list])

    # calc physical volumes size of ROIs and masks
    new_inp_vols     = nvox2phys_vol( new_inp_nvox, inp_voxvol )
    ref_vols         = nvox2phys_vol( ref_nvox,     ref_voxvol )

    inp_mask_vol     = nvox2phys_vol( inp_mask_size, inp_voxvol )
    ref_mask_vol     = nvox2phys_vol( ref_mask_size, ref_voxvol )
    rat_mask_vol     = inp_mask_vol / float(ref_mask_vol)

    # calc fractional volumes of ROIs in mask: vals / N_mask
    new_inp_fracs = nvox2frac_size( new_inp_nvox, inp_mask_size )
    ref_fracs     = nvox2frac_size( ref_nvox,     ref_mask_size )

    # -------------------- make HEADER of file ------------------------

    # Note about naming in report:
    #     'input'  --> 'Warped', 'W', 'A'
    #     'ref'    --> 'Unwarped', 'U', 'B'

    hh = []
    hh.append( ' A atlas dset            : {}'.format(atl_inp) )
    hh.append( ' A mask dset             : {}'.format(mask_inp) )
    hh.append( ' B atlas dset            : {}'.format(atl_ref) )
    hh.append( ' B mask dset             : {}'.format(mask_ref) )
    hh.append( ' Mode_smooth size (nvox) : {}'.format(modesmoo) )
 
    hh.append( ' A vox dims (mm)         : {}'.format(inp_voxdims_str)) 
    hh.append( ' B vox dims (mm)         : {}'.format(ref_voxdims_str))
    hh.append( ' A mask Nvox             : {:>9}'.format(inp_mask_size) )
    hh.append( ' B mask Nvox             : {:>9}'.format(ref_mask_size) )
    hh.append( ' A mask volume (mm^3)    : {:>13.3f}'.format(inp_mask_vol) )
    hh.append( ' B mask volume (mm^3)    : {:>13.3f}'.format(ref_mask_vol) )
    hh.append( ' MaskVol_A / MaskVol_B   : {:>13.3f}'.format(rat_mask_vol) )
    hh.append( ' A atlas Nroi            : {:>9}'.format(Nroi_inp) )
    hh.append( ' B atlas Nroi            : {:>9}'.format(Nroi_ref) )
    hh.append( ' Nroi difference         : {:>9}'.format(Nroi_diff) )
    if Nroi_diff :
        hh.append( 'Selector of lost ROIs    : {:}'.format(all_lost_vals_comma))
        hh.append( '(And see list of lost ROIs at bottom of file.)') 
    hh.append( ' ' )

    # column labels
    cl         = ['ROI_value', 
                  'Nvox_A', 
                  'Nvox_B', 
                  'Vol_A' , 
                  'Vol_B',  
                  'RelVol_A2B',
                  'Frac_A', 
                  'Frac_B',
                  'RelFrac_A2B', 
                  'Label_str'     ]
    col_labs   = ['{:>12s}'.format(x) for x in cl]
    Ncol       = len(cl)
    table_div  = '  '.join(['-'*12]*Ncol)
    table_div2 = '='*len(table_div)
    
    key = '''  -- KEY --

    ROI_value     = integer value of ROI
    Nvox_A        = number of voxels in ROI in dset A
    Nvox_B        = number of voxels in ROI in dset B
    Vol_A         = ROI volume in dset A (mm^3)
    Vol_B         = ROI volume in dset B (mm^3)
    RelVol_A2B    = relative ROI volume, Vol_A / Vol_B
    Frac_A        = ROI mask fraction, Vol_A / MaskVol_A
    Frac_B        = ROI mask fraction, Vol_B / MaskVol_B
    RelFrac_A2B   = relative ROI mask fraction, Frac_A / Frac_B
    Label_str     = string label of ROI (if present) 
    '''
    hh.append( table_div2 )
    hh.append( '\n#  '.join([x.strip() for x in key.split("\n")]))
    
    hh.append( table_div2 )
    hh.append( '  '.join(col_labs) )
    hh.append( table_div )

    header = '# ' + '\n# '.join(hh)

    # -------------------- make TABLE of file ------------------------

    tt = []

    for ii in range(Nroi_ref):
        row = []
        row.append('{:14}'.format(ref_vals[ii]))
        row.append('{:12}'.format(new_inp_nvox[ii]))
        row.append('{:12}'.format(ref_nvox[ii]))
        row.append('{:12.3f}'.format(new_inp_vols[ii]))
        row.append('{:12.3f}'.format(ref_vols[ii]))
        row.append('{:12.3f}'.format(float(new_inp_vols[ii])/ref_vols[ii]))
        row.append('{:12.3e}'.format(new_inp_fracs[ii]))
        row.append('{:12.3e}'.format(ref_fracs[ii]))
        row.append('{:12.3f}'.format(new_inp_fracs[ii]/ref_fracs[ii]))
        row.append('#')
        row.append('{}'.format(ref_labs[ii]))
        tt.append ( '  '.join(row) )

    table = '\n'.join(tt)

    # ----------------- make FOOTER of lost ROIs (if any) ------------------

    ff = []

    if Nroi_diff :
        ff.append( '# LOST_ROI_LABELS: {}'.format(all_lost_labs) )
        ff.append( '# LOST_ROI_VALUES: {}'.format(all_lost_vals) )
    footer = '\n'.join(ff)

    # ------------------- make FINAL FULL FILE ----------------------------

    full_file = header + '\n' + table + '\n' 
    if footer :
        full_file+= footer + '\n'

    print("\n"*2 + full_file + "\n"*2 )

    # ------------------- WRITE file and finish ----------------------------

    f = open(ofile, 'w')
    f.write(full_file)
    f.close()

    print("++ DONE.\n"
          "   Wrote output file:\n"
          "   {}".format(ofile))

    sys.exit(0)
