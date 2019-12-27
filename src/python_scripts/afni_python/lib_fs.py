#
#
#
#ver='1.0' ; date='Oct 22, 2019'
# + [PT] start
#
#ver='1.2' ; date='Oct 23, 2019'
# + [PT] helpful helpfile
#      - also updating way prog works: can have finer-grained criteria
#        calling
#
#ver='1.3' ; date='Dec 26, 2019'
# + [PT] fix "-is_mat_even" test
#      - thanks, S. Torrisi, for pointing this out!
#
ver='1.4' ; date='Dec 27, 2019'
# + [PT] new options for '-fix_all' functionality
#      - can output new/fixed dsets.  Bonne idee, D Glen!
#
#
###############################################################################

import sys, os
import json
import afni_base as BASE
import afni_util as UTIL

import lib_info_dict  as lid

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

ddefs = {
    'DEF_ver'           : ver,
    'DEF_date'          : date,
    'DEF_eps_iso'       : 1.0*10**-2,
    'DEF_eps_size'      : 1.0*10**-2,
    'DEF_dset_tmp_base' : '__tmp_fs_check_vox',
}

# ----------------------------------------------------------------------------

all_opts = {
    'verb'               : '-verb',
    'verbose'            : '-verbose',
    'input'              : '-input',
    'ver'                : '-ver',
    'is_mat_even'        : '-is_mat_even',
    'is_vox_iso'         : '-is_vox_iso',
    'is_vox_1mm_max'     : '-is_vox_1mm_max',
    'is_vox_05mm_min'    : '-is_vox_05mm_min',
    'check_all'          : '-check_all',
    'fix_all'            : '-fix_all',
    'fix_out_vox_dim'    : '-fix_out_vox_dim',
    'fix_out_prefix'     : '-fix_out_prefix',
    'eps_iso'            : '-eps_iso',
    'eps_size'           : '-eps_size',
    'overwrite'          : '-overwrite',
    'date'               : '-date',
    'h'                  : '-h',
    'help'               : '-help',
    'hview'              : '-hview',
}

all_opts_vals = list(all_opts.values())

# ----------------------------------------------------------------------------

# this should make a single dict in py23; 
# preferred {**all_opts, **ddefs} is only in py3
help_dict = all_opts.copy()
help_dict.update(ddefs)     

help_string_this_prog = '''
  PURPOSE ~1~

  This program examines whether an input dset is ready-to-go for
  running FreeSurfer's (FS's) "recon-all".  The dataset in this case
  is typically an anatomical volume.  This applies to (at least) FS
  versions 5.* and 6.*.

  We have found that there are a number of unofficial properties that
  a dset should have in order for all of the the FS outputs to align
  well with the input dset.  These include:
     + having isotropic voxel dimensions
     + having matrix dimensions that are all even
     + having voxels sizes in a range 0.5-1.0 mm.
       - and note that for sub-millimeter voxels, you might have to
         use the "-hires" option in FS's recon-all; please see their
         documentation, such as:
           https://surfer.nmr.mgh.harvard.edu/fswiki/SubmillimeterRecon
         for more information about this point.

  NB: 'recon-all' is a program that we find helpful and use a lot, but
      we are *not* its developers, so this program should be viewed as
      an potentially helpful guide, but not a be-all and end-all to
      running it.  Please see FreeSurfer documentation for more
      information about using it, including options, other quality
      control, etc.

  Ver  : {DEF_ver}
  Date : {DEF_date}


  INPUTS ~1~

    + An anatomical dset, which the user wants to put into FS's
      "recon-all" program (and likely bring into the AFNI/SUMA realm
      thereafter with AFNI's "@SUMA_Make_Spec_FS").


  OUTPUTS ~1~

    + At its most basic functioning, this program outputs a True/False
      (= 1/0, respectively) response to this question: 

         Is the input dset ready for use in FS's "recon-all" program?

      This is evaluated with several empirical criteria that we have
      found useful in order for the output results to overlap well
      with the input.  The user can output the check results in a more
      "verbose" manner, too, seeing a breakdown of the information in
      more detail.

    See the "NOTES" (below) for recommendations on what to do if your
    dset fails the FS/recon-all safety check.


  RUNNING ~1~

  {input}  INP          : (req) input file name

  {verbose}             : output colon-separated table of info, including:
                          dset information (voxel size and matrix dims);
                          results of individual criteria; utilized tolerance
                          values; and the overall evaluation.
                          With this option, all criteria are displayed.
                          (def: just output a 1 or 0 for 'pass' or 'fail',
                           respectively)

  {verb}                : hip slang for '{verbose}'

  {is_mat_even}         : output contains the result of this test (can be
                          combined with others for final omnibus):
                            is each matrix dimension even?
  {is_vox_iso}          : output contains the result of this test (can be
                          combined with others for final omnibus):
                            is the voxel size isotropic (within tolerance)?

  {is_vox_1mm_max}      : output contains the result of this test (can be
                          combined with others for final omnibus):
                            is each voxel edge <1 mm (within tolerance)?

  {is_vox_05mm_min}     : output contains the result of this test (can be
                          combined with others for final omnibus):
                            is each voxel edge >0.5 mm (within tolerance)?

  {check_all}           : specify that *all* currently available criteria
                          should be employed.  This option should be 
                          unnecessary to use, because this is in fact default
                          behavior of the program. 

  {fix_all}             : flag to tell program to output a dset with fixed
                          properties;  will output a dset *even if there is
                          nothing to be fixed*, for ease of scripting.
                          Must also provide output file name, see 
                          '{fix_out_prefix}'

  {fix_out_prefix} FFF  : if using '{fix_all}', you must provide output name
                          for fixed dset; again, this file will be written
                          even if no changes were necessary.

  {fix_out_vox_dim} LL  : if using '{fix_all}', you can specify edge length of
                          voxels in output dset (dimensions will be isotropic).
                          Otherwise, the minimum voxel dimension of input dset
                          will be used (if resampling is needed); if this opt
                          is NOT used and min voxel dimension is <0.5mm, then
                          output voxel size will be 0.5mm; or, if this opt
                          is NOT used and min voxel dimension is >1mm, then
                          output voxel size will be 1mm.  Probably safest to 
                          just specify LL, if using '{fix_all}'.

  {eps_iso} EI          : specify tolerance for defining isotropy of voxels;
                          since the values are floating point numbers, exact
                          equality is not tested, but instead pairwise checks
                          of: 
                              abs(dx - dy) < eps_iso,
                          etc., are made. (def: eps_iso = {DEF_eps_iso})

  {eps_size}  ES        : specify tolerance for defining size of
                          voxels; as noted above, exact equality is
                          not tested, but instead pairwise checks of:
                              dx < 1 + eps_size, 
                              dx > 0.5 - eps_size,
                          etc., are made. (def: eps_size = {DEF_eps_size})

  {help}                : display program help in terminal (consider
                         '-hview' to open help in a separate text editor)
  {ver}                 : display program version number in terminal 
  {date}                : display date of program's last update in terminal 


  NOTES ~1~

  If your dset fails the check (i.e., a 0 is returned by the program),
  what can you do to "fix" your dset?  Well, it depends what aspect of
  the check failed (which can be seen in detail by running with
  '{verb}' or '{verbose}'.  Here we provide a table of suggestions.
                            
  ... but note that you can also use the '{fix_all}' option (with
  accompanying specifications) to output a fixed dataset
  automatically, too.

  "Are voxels isotropic" 
  ----------------------
      Consider resampling the data.  In order to introduce a minimal
      amount smoothing, use the wsinc5 interpolation kernel. Consider
      the following example (note that 'IDENTITY' is actually a
      keyword to use; the DSET and NEW_NAME would be replaced for your
      darling dset):
                      3dAllineate                    \\
                          -1Dmatrix_apply  IDENTITY  \\
                          -mast_dxyz       1         \\
                          -final           wsinc5    \\
                          -source          DSET      \\
                          -prefix          NEW_NAME

  "Are voxels 1.0 mm (max)"
  -------------------------
      Resample; basically the same command as the solution to "Are
      voxels isotropic" woes.

  "Are voxels 0.5 mm (min)"
  -------------------------
      Resample; basically the same command as the solution to "Are
      voxels isotropic" woes (maybe using a different '-mast_dxyz ..'
      size, since you must have put in some mighty effort to get
      voxels of such a high resolution).
      Note that if you have very small voxels still, you might 
      need to use the "-hires" option in recon-all;  please check
      FS documentation for more about this.

  "Are matrix dims even"  
  ----------------------
      Use 3dZeropad to add or subtract slices.  That program's new
      option '-pad2evens' makes this process pretty automatic:
                      3dZeropad                      \\
                          -pad2evens                 \\
                          -prefix          NEW_NAME  \\
                          DSET

  NB: if several criteria are failing, you might want to consider
  solving the voxel-sized ones (isotropy, over/undersizedness)
  *first*, because the resampling there might change the matrix
  dimensions.

  More criteria may be added over time.  Who knows?


  EXAMPLES ~1~

    1. Vanilla mode.  Output is just a number: 1 (pass) or 0 (fail).

           check_dset_for_fs.py  -input DSET
      
    2. Spicier mode.  Output is a table of values and criteria; the 
       'omnibus' line carries the final evaluation.

           check_dset_for_fs.py  -input DSET -verbose

    3. Mirchi mode.  Check just a subset of criteria AND control the 
       fineness of the test tolerance.

             check_dset_for_fs.py            \\
                 -input DSET                 \\
                 -eps_iso 0.001              \\
                 -is_vox_iso                 \\
                 -verb
    
    4. Today's special.  Run the check, AND output a fixed dset.

             check_dset_for_fs.py            \\
                 -input DSET                 \\
                 -fix_all                    \\
                 -fix_out_prefix  DSET_FIXED \\
                 -fix_out_vox_dim 1          \\
                 -verb 

'''.format( **help_dict )

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

def check_for_shell_com_failure( com, cmd, 
                                 exit_on_fail=True,
                                 jump_home_on_exit='',
                                 disp_so=True, disp_se=True ):

    '''Wrapper for check_for_shell_exec_failure()

    '''

    so = '\n'.join(com.so)
    se = '\n'.join(com.se)

    check_for_shell_exec_failure( com.status, so, se, cmd, 
                                  exit_on_fail,
                                  jump_home_on_exit,
                                  disp_so, disp_se )

# ----------------------------------------------------------------------------

def check_for_shell_exec_failure( status, so, se, cmd, 
                                  exit_on_fail=True,
                                  jump_home_on_exit='',
                                  disp_so=True, disp_se=True ):
    '''When using afni_base.py's simple_shell_exec(), parse the output
    status/so/se and decide what to do.  

    By default, this will exit the program on failure.

    '''

    if status:
        print("** ERROR executing this command in the shell:\n"
              "   {}\n"
              "".format(cmd))
        print("** Exit status    : {}".format(status))
        print("** Standard error : {}".format(se))

        if exit_on_fail :

            if jump_home_on_exit :
                os.chdir( jump_home_on_exit )
            sys.exit(22)
    else:
        if disp_so :
            print(so)
        if disp_se :
            print(se)

# ==========================================================================

# read in inputs for this program, and store info in this obj
class iopts_this_prog:

    def __init__(self):

        self.full_cmd       = ''           # existential...
        self.prog_ver       = ddefs['DEF_ver']
        self.afni_ver       = ''
        self.is_verbose     = False

        self.fix_all         = False
        self.fix_out_vox_dim = None
        self.fix_out_prefix  = None
        self.fix_mat         = False
        self.fix_vox         = False
        self.fix_tmp_fname   = ''
        self.fix_overwrite   = ''
        self.fix_make_changes = None
        self.fix_out_info        = None
        self.fix_out_vox_dim_str = ''
        self.fix_out_mat_dim_str = ''

        self.dset            = ''
        self.dset_info       = ''
        self.rep_vox_iso     = False
        self.rep_mat_even    = False
        self.rep_vox_1mm_max = False
        self.rep_vox_05mm_min = False
        self.vox_dim         = []
        self.mat_dim         = []
        self.vox_dim_str     = ''
        self.mat_dim_str     = ''
        self.eps_iso         = ddefs['DEF_eps_iso']
        self.eps_size        = ddefs['DEF_eps_size']
        self.vox_dim_min     = -1.

        self.check_all         = -1        # def: do all 
        self.stat_mat_even     = -1        # status
        self.stat_vox_iso      = -1        # status
        self.stat_vox_1mm_max  = -1        # status
        self.stat_vox_05mm_min = -1        # status
        self.stat_fs_safe      = -1        # status

        # This is here basically to just initialize the 'comm' obj
        cmd = 'pwd'
        self.comm = BASE.shell_com( cmd, capture=1 )
        self.comm.run()
        check_for_shell_com_failure( self.comm, cmd,
                                     disp_so=False, disp_se=False )


    # -------------- methods to populate -----------------

    def set_afni_version( self ):
        # version number and package type
        cmd = '''afni -vnum -package'''
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )
        self.afni_ver = ", ".join(com.so)
        
    def set_full_cmd( self, dd ):
        self.full_cmd = dd

    def set_verbose( self, bb ):
        self.is_verbose = bb

    def set_eps_iso( self, ss ):
        self.eps_iso = float(ss)

    def set_eps_size( self, ss ):
        self.eps_size = float(ss)

    def set_rep_vox_iso( self, bb ):
        self.rep_vox_iso = bb

    def set_rep_mat_even( self, bb ):
        self.rep_mat_even = bb

    def set_rep_vox_1mm_max( self, bb ):
        self.rep_vox_1mm_max = bb

    def set_rep_vox_05mm_min( self, bb ):
        self.rep_vox_05mm_min = bb
 
    def set_input( self, ss ):
        self.dset      = ss
        self.dset_info = lid.get_all_3dinfo_dset_neatly(self.dset)

        self.vox_dim   = [ float(x) for x in self.dset_info['-ad3'] ]
        self.mat_dim   = [   int(x) for x in self.dset_info['-n4'][:3] ]

        self.vox_dim_min = min(self.vox_dim)

        self.vox_dim_str = '  '.join(self.dset_info['-ad3'])
        self.mat_dim_str = '  '.join(self.dset_info['-n4'][:3])

    def set_fix_out_info( self ):
        self.fix_out_info = lid.get_all_3dinfo_dset_neatly(self.fix_out_prefix)

        self.fix_out_vox_dim_str = '  '.join(self.fix_out_info['-ad3'])
        self.fix_out_mat_dim_str = '  '.join(self.fix_out_info['-n4'][:3])

    def is_vox_1mm_max(self):
        '''
        Check if voxel dims are 1 mm (or less). 

        "1 mm" means:   DIM < 1 + eps
        where eps = {}, by def.

        OUTPUT
        ------
        0     : if too big
        1     : if fine
      
        '''.format(self.eps_size)
        for x in self.vox_dim:
            if x > 1.0 + self.eps_size :  
                self.stat_vox_1mm_max = 0
                return self.stat_vox_1mm_max

        self.stat_vox_1mm_max = 1
        return self.stat_vox_1mm_max

    def is_vox_05mm_min(self):
        '''
        Check if voxel dims are 0.5 mm (or greater). 

        "0.5 mm" means:   DIM > 0.5 - eps
        where eps = {}, by def.

        OUTPUT
        ------
        0     : if too small
        1     : if fine
      
        '''.format(self.eps_size)

        for x in self.vox_dim:
            if x < 0.5 - self.eps_size :  
                self.stat_vox_05mm_min = 0
                return self.stat_vox_05mm_min

        self.stat_vox_05mm_min = 1
        return self.stat_vox_05mm_min

    def is_vox_iso(self):
        '''
        Check if voxels are isotropic by diffing dimensions. 

        "Different" means:  abs(a-b)> eps,
        where eps = {}, by def.

        OUTPUT
        ------
        0     : if non-isotropic
        1     : if isotropic
      
        '''.format(self.eps_iso)

        diff_01 = abs(self.vox_dim[0] - self.vox_dim[1])
        diff_02 = abs(self.vox_dim[0] - self.vox_dim[2])
        
        if (diff_01 > self.eps_iso) or (diff_02 > self.eps_iso) :
            self.stat_vox_iso = 0
            return self.stat_vox_iso

        self.stat_vox_iso = 1
        return self.stat_vox_iso

    def is_mat_even(self):
        '''
        Check if matrix dims are all even.

        OUTPUT
        ------
        0     : if not all even
        1     : if all are even
      
        '''
        for x in self.mat_dim:
            if x % 2 : 
                self.stat_mat_even = 0
                return self.stat_mat_even

        self.stat_mat_even = 1
        return self.stat_mat_even
                
    def is_fs_safe(self):
        '''
        Omnibus check.  Currently, conditions are for:
        + even matrix dimensions
        + isotropic voxels
        + voxels in range 1.0-0.5 mm
        

        OUTPUT
        ------
        0     : if it ain't FS safe
        1     : if 'tis FS safe
      
        '''

        checks = []

        if self.rep_mat_even :
            checks.append( self.is_mat_even() )

        if self.rep_vox_05mm_min :
            checks.append( self.is_vox_05mm_min() )

        if self.rep_vox_1mm_max :
            checks.append( self.is_vox_1mm_max() )

        if self.rep_vox_iso :
            checks.append( self.is_vox_iso() )
        
        if not(checks):
            print("** ERROR: no tests selected??")
            sys.exit(15)

        for x in checks:
            if x != 1 : 
                self.stat_fs_safe = 0
                return self.stat_fs_safe

        self.stat_fs_safe = 1
        return self.stat_fs_safe

    def set_check_all_val(self, ii):
        ''' Set everything true or false, all in one swell foop.
        Should be one of: -1, 0, 1.
        '''
        self.check_all = ii

    def set_fix_all_val(self, bb):
        ''' Turn on/off switch to output a fixed dset.
        Should be one of: True, False.
        '''
        self.fix_all = bb

    def set_fix_out_vox_dim(self, bb):
        ''' Set output vox dimension, if fixing all.
        Should be in allowed range...  
        '''
        self.fix_out_vox_dim = float(bb)

    def set_fix_out_prefix(self, ss):
        ''' Set output dset prefix, if fixing all.
        Will output even if nothing changed (easier for scripting?)
        '''
        self.fix_out_prefix = ss

    def set_fix_overwrite(self, ss):
        self.fix_overwrite = ss

    def do_check_all(self):
        self.set_rep_vox_05mm_min(True)
        self.set_rep_vox_1mm_max(True)
        self.set_rep_mat_even(True)
        self.set_rep_vox_iso(True)

    def run_fix_all(self):

        # all tests for resampling voxel size
        if self.stat_vox_iso      == False or \
           self.stat_vox_1mm_max  == False or \
           self.stat_vox_05mm_min == False :
            self.fix_vox = True

        # all tests for changing matrix dims-- note that if we
        # resample the vox sizes, then we will also do the
        # pad_to_evens, just to be sure the grid hasn't changed (and
        # to have consistent output naming); so, leave this check
        # after the 'fix_vox' one.
        if self.stat_mat_even == False or \
           self.fix_vox == True :
            self.fix_mat = True

        # have this be a bool, for verbose output
        if self.fix_mat or self.fix_vox :
            self.fix_make_changes = True
        else: 
            self.fix_make_changes = False

        # determine output voxel size; user might have entered a value
        # (and that will have already been checked for validity at input)
        if self.fix_vox :
            if self.fix_out_vox_dim == None :   # then it hasn't been set by user
                if self.vox_dim_min < 0.5 :
                    self.fix_out_vox_dim = 0.5
                elif 1.0 < self.vox_dim_min :
                    self.fix_out_vox_dim = 1.0
                else:
                    self.fix_out_vox_dim = self.vox_dim_min
        
        # do resampling and/or matrix padding, in that order:
        if self.fix_vox :
            # generate name with random string for fix_vox
            # use this function here, because this file won't exist yet!
            pp = os.path.dirname(self.fix_out_prefix)
            if not(pp) :
                pp = '.' # the above returns '' for local dir-- not cool
            cmd = '''3dnewid -fun11'''
            com = BASE.shell_com(cmd, capture=1, save_hist=0)
            com.run()
            check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )
            self.fix_tmp_fname = pp + '/' + ddefs['DEF_dset_tmp_base'] 
            self.fix_tmp_fname+= com.so[0] + '.nii'

            # resampling to iso vox
            cmd = '''3dAllineate {ow} \
            -1Dmatrix_apply  IDENTITY \
            -mast_dxyz       {dxyz}   \
            -final           wsinc5   \
            -source          {src}    \
            -prefix          {pref}
            '''.format( ow=self.fix_overwrite,
                        dxyz=self.fix_out_vox_dim, 
                        src=self.dset,
                        pref=self.fix_tmp_fname )
            com = BASE.shell_com(cmd, capture=1, save_hist=0)
            com.run()
            check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )

        # any fixing will lead to having this performed
        if self.fix_mat :
            # decide if we already did one correction step or not
            if self.fix_tmp_fname :
                input_dset = self.fix_tmp_fname
            else:
                input_dset = self.dset

            # resampling to iso vox
            cmd = '''3dZeropad {ow} \
            -pad2evens              \
            -prefix          {pref} \
            {src}  
            '''.format( ow=self.fix_overwrite,
                        src=input_dset,
                        pref=self.fix_out_prefix)
            com = BASE.shell_com(cmd, capture=1, save_hist=0)
            com.run()
            check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )

        # clean up, if intermed dset exists
        if self.fix_vox :
            cmd = '''\\rm {pref}
            '''.format( pref=self.fix_tmp_fname )
            com = BASE.shell_com(cmd, capture=1, save_hist=0)
            com.run()
            check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )


        if not(self.fix_make_changes) :
            # no changes, but output a dset with the correct file name
            cmd = '''3dcalc {ow} \
            -a      {src}        \
            -expr   'a'          \
            -prefix {pref} 
            '''.format( ow=self.fix_overwrite,
                        src=self.dset,
                        pref=self.fix_out_prefix)
            com = BASE.shell_com(cmd, capture=1, save_hist=0)
            com.run()
            check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )

        # get info about output set
        self.set_fix_out_info()



    # ---------- fill in vals ----------------

    def finish_defs(self):

        self.set_afni_version()

        # default behavior: if user specifies nothing (check_all ==
        # -1), then we check all; otherwise, user can explicitly set
        # all (check_all == 1); else, user has specified subset
        # (check_all == 0)
        if self.check_all == -1 or self.check_all == 1 :
            self.do_check_all()

    # ---------- check ----------------

    def check_req(self):
        ''' Check for and point out any missing inputs.'''
        MISS = 0

        if not(self.dset) :
            print("")
            print("** ERROR: missing '{input}' dset"
                  "".format(**all_opts))
            MISS+=1
            
        if self.fix_all :
            if not(self.fix_out_prefix) :
                print("")
                print("** ERROR: you want to output a fixed dset, but you\n"
                      "   did not provide a '{fix_out_prefix} ..' filename"
                      "".format(**all_opts))
                MISS+=1

        return MISS

    # ---------------------------------------------------------------------
    # ---------------------------------------------------------------------



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

def parse_args_this_prog(full_argv):

    argv = full_argv[1:]
    Narg = len(argv)

    if not(Narg):
        print(help_string_this_prog)
        sys.exit(0)
    
    # initialize objs
    iopts  = iopts_this_prog()

    iopts.set_full_cmd(full_argv)

    i = 0
    while i < Narg:
        if argv[i] == "{ver}".format(**all_opts) :
            print(ver)
            sys.exit(0)

        elif argv[i] == "{date}".format(**all_opts) :
            print(date)
            sys.exit(0)

        elif argv[i] == "{help}".format(**all_opts) or \
             argv[i] == "{h}".format(**all_opts) :
            print(help_string_this_prog)
            sys.exit(0)

        elif argv[i] == "{hview}".format(**all_opts) :
            prog = os.path.basename(full_argv[0])
            cmd = 'apsearch -view_prog_help {}'.format( prog )
            BASE.simple_shell_exec(cmd)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "{input}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_input(argv[i])

        elif argv[i] == "{eps_iso}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_eps_iso(argv[i])

        elif argv[i] == "{eps_size}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_eps_size(argv[i])

        # can be outweighed by "-check_all"
        elif argv[i] == "{is_vox_iso}".format(**all_opts) :
            if iopts.check_all != 1:
                iopts.set_rep_vox_iso(True)
                iopts.set_check_all_val( 0 )

        # can be outweighed by "-check_all"
        elif argv[i] == "{is_mat_even}".format(**all_opts) :
            if iopts.check_all != 1:
                iopts.set_rep_mat_even(True)
                iopts.set_check_all_val( 0 )

        # can be outweighed by "-check_all"
        elif argv[i] == "{is_vox_1mm_max}".format(**all_opts) :
            if iopts.check_all != 1:
                iopts.set_rep_vox_1mm_max(True)
                iopts.set_check_all_val( 0 )

        # can be outweighed by "-check_all"
        elif argv[i] == "{is_vox_05mm_min}".format(**all_opts) :
            if iopts.check_all != 1:
                iopts.set_rep_vox_05mm_min(True)
                iopts.set_check_all_val( 0 )

        # default behavior; and/or will supercede indiv opts
        elif argv[i] == "{check_all}".format(**all_opts) :
            iopts.set_check_all_val( 1 )

        elif argv[i] == "{fix_all}".format(**all_opts) :
            iopts.set_fix_all_val( True )

        # if not entered, will use min vox dim, floored at min allowed val
        elif argv[i] == "{fix_out_vox_dim}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            fovd = float(argv[i])
            if fovd < 0.5 or 1.0 < fovd :
                print("** ERROR: value for output voxel size ({}) is"
                      "   outside the allowed range [0.5, 1.0]"
                      "".format(fovd))
                sys.exit(3)
            else:
                iopts.set_fix_out_vox_dim( fovd )

        elif argv[i] == "{fix_out_prefix}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_fix_out_prefix(argv[i])

        elif argv[i] == "{overwrite}".format(**all_opts) :
            iopts.set_fix_overwrite( '-overwrite' )

        elif argv[i] == "{verbose}".format(**all_opts) or \
             argv[i] == "{verb}".format(**all_opts) :
            iopts.set_verbose(True)

        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    # --------------------------------------------------------------------

    if iopts.check_req():
        print("   -------------------------------")
        print("** ERROR with input arguments (see detailed whining above).")
        sys.exit(1)

    iopts.finish_defs()  # make some paths we need later

    return iopts


# --------------------------------------------------------------------------


