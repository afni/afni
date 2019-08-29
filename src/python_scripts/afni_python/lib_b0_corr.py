#
#
#
#ver='1.0' ; date='July 18, 2019'
# + [PT] translating Vinai's original afniB0() function from here:
#   https://github.com/nih-fmrif/bids-b0-tools/blob/master/distortionFix.py
#
#ver='1.1' ; date='July 22, 2019'
# + [PT] updated I/O, help, defaults, dictionaries
#
#ver='1.2' ; date='July 22, 2019'
# + [PT] redone shell exec
# + [PT] use diff opts/params for distortion direction and scale
#
#ver='1.3' ; date='July 24, 2019'
# + [PT] add in 3dinfo info to use
# + [PT] expand 3dROIstats options
# + [PT] write out *_cmds.tcsh file, recapitulate utilized param info at top
#
#ver='1.4' ; date='July 25, 2019'
# + [PT] change where scaling is applied-- now separate from 'polarity' issue
# + [PT] updated help (included examples); put beta warning messages!
#
#ver='1.41' ; date='July 26, 2019'
# + [PT] update help; include JSON description
#
#ver='1.5' ; date='July 31, 2019'
# + [PT] rename several variables and opts, to undo my misunderstanding...
# + [PT] EPI back to being required
#
#ver='1.6' ; date='Aug 2, 2019'
# + [PT] added in obliquity checks: should be able to deal with relative 
#        obl diffs between EPI and freq dset (if they exist)
# + [PT] final WARP dset will now be in EPI grid
# + [PT] *still need to check on scaling&recentering of Siemens data*
#
#ver='1.6' ; date='Aug 8, 2019'
# + [PT] update/correct help about Siemens scaling, post-discussion-with-Vinai
#
#ver='1.7' ; date='Aug 12, 2019'
# + [PT] *really* correct help @ Siemens scaling
# + [PT] change internal scaling: *really* demand units of ang freq (rad/s)
# + [PT] py23 compatability of help file-- single dictionary usage!
#
#ver='2.0' ; date='Aug 15, 2019'
# + [PT] new parameter scaling of freq dset from Vinai-- better params
# + [PT] apply obliquity info to output
# + [PT] fixed ocmds_fname, if opref contains path
# + [PT] output a useful params file
# + [PT] another py23 compatability fix
#
#ver='2.1' ; date='Aug 16, 2019'
# + [PT] change default number of erodes: 3 -> 1.  Vinai concurs!
#
#ver='2.2' ; date='Aug 23, 2019'
# + [PT] fix examples (use correct/newer opt names)
# + [PT] fix 'eff echo sp' -> 'bwpp' calculation ('matr len', not 'vox dim')
#
ver='2.21' ; date='Aug 27, 2019'
# + [PT] update help file and descriptions (param text, for example)
# + [PT] add in more fields to param text output
#
###############################################################################

import sys, os
import json
import afni_base as BASE
import afni_util as UTIL
import lib_msar  as lmsar

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

ddefs = {
    'DEF_ver'        : ver,
    'DEF_date'       : date,
    'DEF_npeels'     : 2,
    'DEF_nerode'     : 1,
    'DEF_bsigma'     : 9,  #blur to match the '--smooth3=9' opt in FSL's fugue
    'DEF_meth_recenter_freq' : 'mode',
    'DEF_wdir_pref'  : '__work_B0_corr_',
    'DEF_aff12_obl_epi_freq' : 'mat_obl_epi_freq.aff12.1D',
}

# See also 'A NOTE ON THE DIRECTIONALITY OF THE PHASE CORRECTION' below
ORI_POS = ['RL', 'AP', 'IS']
ORI_NEG = ['LR', 'PA', 'SI']
ORI_ALL = ORI_POS + ORI_NEG

IND_DICT = { 'i' : 0, 'j' : 1, 'k' : 2 }

# ----------------------------------------------------------------------------

all_opts = {
    'prefix'             : '-prefix',
    'out_cmds'           : '-out_cmds',
    'out_pars'           : '-out_pars',
    'in_freq'            : '-in_freq',
    'in_mask'            : '-in_mask',
    'in_magn'            : '-in_magn',
    'in_epi'             : '-in_epi',
    'in_epi_json'        : '-in_epi_json',
    'wdir_name'          : '-wdir_name',
    'epi_pe_bwpp'        : '-epi_pe_bwpp',
    'epi_pe_voxdim'      : '-epi_pe_voxdim',
    'epi_pe_echo_sp'     : '-epi_pe_echo_sp',
    'epi_pe_dir'         : '-epi_pe_dir',
    'scale_freq'         : '-scale_freq', 
    'do_recenter_freq'   : '-do_recenter_freq', 
    'blur_sigma'         : '-blur_sigma',
    'automask_peels'     : '-automask_peels',
    'automask_erode'     : '-automask_erode', 
    'no_clean'           : '-no_clean',
    'overwrite'          : '-overwrite',
    'ver'                : '-ver',
    'date'               : '-date',
    'h'                  : '-h',
    'help'               : '-help',
}

# ----------------------------------------------------------------------------

# this should make a single dict in py23; 
# preferred {**all_opts, **ddefs} is only in py3
help_dict = all_opts.copy()
help_dict.update(ddefs)     

help_string_b0_corr = '''

  PURPOSE ~1~

  This program performs B0 distortion correction along the phase encode
  (PE) direction, using an acquired frequency (phase) image.  It was
  initially written by Vinai Roopchansingh (NIMH, NIH).

  Ver  : {DEF_ver}
  Date : {DEF_date}


  INPUTS ~1~

  + frequency dset : (req) phase volume, which should be of similar
                     spatial resolution/FOV of EPI dset to which it
                     will be applied. Expected units are: 
                         angular freq = rad/s = 2*PI*Hz.
                     If your dataset is in different units,
                     you can apply an appropriate scaling via the
                     command line, as discussed in the 'NOTES', below.

  + EPI dset       : (req) EPI dset to which the B0 distortion correction
                     is applied.

  + mask dset      : (req) binary mask of subject's brain
       OR
  + magnitude dset : (req) volume in same space as frequency dset for
                     automasking, to create brain mask

  + PE parameters  : (req) a number of parameters related to the
                     EPI vol are required to be input, such as its
                     - PE direction (AP, PA, RL, etc.)
                     - bandwidth per pixel OR effective TE
                     Optional scaling can be applied to the freq dset
                     (e.g., if units need to be adjusted appropriately).

                     These parameters can be provided either
                     individually, or by providing an accompanying JSON
                     that might/should contain all necessary
                     information.

                     NB: If you input a parameter on the command line,
                     it will take precedence over one found in the
                     EPI's JSON, if you are also using that.  Thus, if
                     you know the JSON has *wrong* information, you
                     can selectively ignore that when running this
                     program.


  OUTPUTS ~1~

  + WARP dset           : a file called PREFIX_WARP.nii.gz, containing  
                          the warp along the phase encode axis (on the 
                          EPI dset's grid, with its obliquity info)

  + script of commands  : a script of the commands used to generate the 
                          WARP dset (and EPI)

  + text file of params : a text file of parameters either input or
                          derived from inputs and the dsets.  This is
                          useful for verifying the consistency of
                          analysis (i.e., as a sanity check).  Can be
                          converted to a JSON, if needed.  Units are
                          given for all; the 'Warp (mm) in mask,
                          20-100 %ile' field might be the most cryptic
                          entrant-- it is a histogram of values of the
                          final warp field within the mask, at the
                          20th, 40th, 60th, 80th and 100th %iles.
                          Cryptic no more!

  + EPI (un)warped dset : the EPI dset with the estimated distortion
                          correction applied to it (and obliquity info
                          matching the original EPI's); hopefully it
                          is unwarped...


  RUNNING ~1~

  {prefix}           PP : (req) prefix of output files; can include path

  {in_freq}   DSET_FREQ : (req) phase dset (frequency volume).  Should
                         be of similar spatial resolution and FOV as
                         EPI dset to which it will be applie d; also,
                         must be scaled appropriately, where the
                         expected units are: Hz.

  {in_epi}     DSET_EPI : (req) EPI dset to which the B0 distortion 
                         correction that I have spent so much time   
                         calculating will be applied

  {in_mask}   DSET_MASK : (req) mask of brain volume
       or
  {in_magn}   DSET_MAGN : (req) magnitude dset from which to estimate brain
                         mask

  {in_epi_json}  FJSON  : (opt) Several parameters about the EPI
                         dset must be known for processing; these MIGHT
                         be encoded in a JSON file accompanying the
                         frequency dset.  If so, you can input the file
                         and let The Program try to find+interpret them.
                         At present, desirable keys/tags in the JSON
                         (with the keyword args you would otherwise use
                         when running this program) are:
                           PhaseEncodingDirection (or use '{epi_pe_dir}')
                         and then either of the following:
                           BandwidthPerPixelPhaseEncode (or use '{epi_pe_bwpp}')
                              OR
                           EffectiveEchoSpacing   (or use '{epi_pe_echo_sp}')

  {epi_pe_dir}       DD : (req) direction (axis) of phase encoding, 
                         e.g., AP, PA, RL, ...
                         NB: the order matters, providing the PE direction
                         (and not just PE axis); thus, 'AP' implies the 
                         PE direction is A>>P, and 'PA' that it is P>>A, etc.
                         (Can come from EPI's JSON; see '{in_epi_json}'.)

  {epi_pe_bwpp}      BW : (req) bandwidth per pixel (in Hz) in the EPI
                         dset along the phase encode direction.  
                         (Can come from EPI's JSON; see '{in_epi_json}'.)
      OR
  {epi_pe_echo_sp}   ES : (req) *effective* TE spacing of phase encoded
                         volume, in units of 's'
                         (Can come from EPI's JSON; see '{in_epi_json}'.)

  {epi_pe_voxdim}   FOV : (opt) voxel size along the EPI dset's phase
                         encode axis, in units of 'mm'; should just be
                         determined internally from the EPI dataset

  {scale_freq}       SF : (opt) scale to apply to frequency volume, 
                         for example to change units to match. 
                         NB: a negative value would invert the warp
                         (probably would not want that...?)  See the
                         'NOTES ..' below for more information about
                         scaling, esp. for particular vendors or known
                         units, like physical frequency (Hz). (def: SF=1.0)

  {out_cmds}         OC : (opt) name of output script, recording
                         commands that were run during the processing
                         (def: script is output to file using entered
                         prefix PP: PP_cmds.tcsh).  If user uses
                         this option, then 'OC' is treated as the full
                         filename, including path

  {out_pars}         OP : (opt) name of output parameters, recording
                         some relevant values that were input, found or 
                         calculated during the processing; the file is 
                         a colon-separated list that can be turned
                         into a JSON with abids_json_tool.py, if desired.
                         (def: pars are output to file using entered
                         prefix PP: PP_pars.txt).  If user uses
                         this option, then 'OP' is treated as the full
                         filename, including path

  {wdir_name}        WD : working directory name (no path, will be located
                         in directory with output dsets); if not
                         provided, will be given automatic name,
                         starting '{DEF_wdir_pref}' and ending with a
                         random alphanumeric string, e.g.,
                         '{DEF_wdir_pref}_9huoXQ7c0AV'

  {blur_sigma}       BS : amount of blurring to apply to masked, phase 
                         encode dset (def: BS = {DEF_bsigma})

  {do_recenter_freq}  MC : method for 3dROIstats to recenter the phase
                         (=freq) volume within the brain mask.  If the
                         value of MC is 'NONE', then the phase dset
                         will not be recentered
                         (def: MC = {DEF_meth_recenter_freq})

  {automask_peels}   AP : if automasking a magnitude image to create a
                         brain mask, AP is the 'peels' value of 3dAutomask
                         (def: AP = {DEF_npeels})

  {automask_erode}   AE : if automasking a magnitude image to create a
                         brain mask, AE is the 'erode' value of 3dAutomask
                         (def: AE = {DEF_nerode})

  {no_clean}            : don't remove the temporary directory of intermed 
                          files

  {help}                : display program help in terminal (consider
                         '-hview' to open help in a separate text editor)
  {ver}                 : display program version number in terminal 
  {date}                : display date of program's last update in terminal 


  NOTES ~1~

  Units of frequency/phase/fieldmap ~2~

  It is important to have your input phase/frequency volume contain
  the correct units for this program.  Here, we expect them to be in
  units of angular frequency: "radians/second" (rad/s).

  Re. fieldmaps in Hz
  -------------------
    If your frequency map has units of physical frequency, 'cycles per
    second' (= Hz), then you just provide a command line argument to
    internally scale your data to the appropriate angular frequency
    unit we desire to use.

    Physicists tell us that angular frequency 'w' is related to
    physical frequency 'f' as follows:  
       w = 2*PI*f
         ~ 6.2831853 * f
    Therefore, if you are *sure* that your frequency (phase) volume is
    really in units of Hz, then you can use the following command line
    argument to set things right for using it here: 
       '{scale_freq} 6.2831853' 

    Not too painful!

  Re. Siemens fieldmaps 
  ---------------------
    If your frequency map is one output by Siemens, then consider the
    following (but doublecheck that it really applies to your darling
    dataset!):

    The standard range of fieldmap values in that case appears to be
    either [-4096, 4095] or [0, 4095], depending on how your data were
    converted.  You can check the range on your dset with, e.g.:
      3dinfo -dmin -dmax FREQ_DSET
    will will likely *approximately* match one of those ranges.

    These ranges come from dividing the measured phases by 2*PI (one
    full phase) and then multiplying by either 2*4096 or 4096,
    respectively.  One could multiply by that inverse ratio, putting
    the dataset into units of radians ('rad'); however, we ultimately
    want the input frequency volume to be in units of angular
    frequency: 'rad/s' ('2*PI*Hz').  Therefore, we also want to divide
    by the EPI's echo time difference (which might be saved as
    'EchoTimeDifference' in an EPI's JSON sidecar).  For example, the
    standard value of this at 3T is about 2.46 ms (= 0.00246 s), but
    check what it is in your own data!

    *Therefore*, in many cases of Siemens 3T data, one should be able
    to convert the scaled freq dset into the the desired units of ang
    freq by scaling the fieldmap by 2*PI/(2*4096*0.00246) ~ 0.311785
    or by 2*PI/(4096*0.00246) ~ 0.623569, respectively.  This could be
    done using, say, 3dcalc to make a new freq dset; or, you could
    provide this magic value to the present command with the scaling
    option:
         FREQ DSET ~RANGE     (potential) PROGRAM OPTION
         ----------------     --------------------------
         [-4096, 4095]     :  '{scale_freq} 0.311785' 
         [0, 4095]         :  '{scale_freq} 0.623569'

    It is worth repeating: be sure that these numbers *really* apply to
    your data!


  EXAMPLES ~1~

    # Ex 1:  With mask supplied, created earlier from magnitude image
      epi_b0_correct.py                                \\
          -epi_pe_echo_sp  0.00031                     \\
          -epi_pe_dir      AP                          \\
          -in_freq  sub-001_frequency.nii.gz           \\
          -in_mask  sub-001_magnitude_MASK.nii.gz      \\
          -in_epi   epiRest-sub-001.nii.gz             \\
          -prefix   b0_corr

    # Ex 2:  Input *magnitude* dset, from which to calculate mask
      epi_b0_correct.py                                \\
          -epi_pe_echo_sp  0.00031                     \\
          -epi_pe_dir      AP                          \\
          -in_freq  sub-001_frequency.nii.gz           \\
          -in_magn  sub-001_magnitude.nii.gz           \\
          -in_epi   epiRest-sub-001.nii.gz             \\
          -prefix   b0_corr

    # Ex 3:  Same as above, but freq dset was in units of Hz (convert
    #        to angular freq, scaling by 2*PI~6.283185)
      epi_b0_correct.py                                \\
          -epi_pe_echo_sp  0.00031                     \\
          -epi_pe_dir      AP                          \\
          -scale_freq      6.283185                    \\
          -in_freq  sub-001_frequency.nii.gz           \\
          -in_magn  sub-001_magnitude.nii.gz           \\
          -in_epi   epiRest-sub-001.nii.gz             \\
          -prefix   b0_corr

    # Ex 4: Input a JSON file (sidecar) accompanying the freq volume,
    #       and hope that it has all the necessary parameters/fields for
    #       this program. 
      epi_b0_correct.py                                \\
          -in_epi_json   sub-001_frequency.json        \\
          -in_freq       sub-001_frequency.nii.gz      \\
          -in_magn       sub-001_magnitude.nii.gz      \\
          -in_epi        epiRest-sub-001.nii.gz        \\
          -prefix        b0_corr


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

# ----------------------------------------------------------------------------

def afni_rand_newid(  ) :
    '''Generate the 11 character alpha/numeric random number using AFNI's
 3dnewid.

    '''

    cmd = '''3dnewid -fun11
    '''

    status, so, se = BASE.simple_shell_exec(cmd, capture=1)
    check_for_shell_exec_failure( status, so, se, cmd, 
                                  disp_so=False, disp_se=False )

    rand_alphanum  = so.strip()
    return rand_alphanum

# ==========================================================================

# Store some useful 3dinfo info about a dset; can grow over time.
# Each variable NAME comes from its opt:  '3dinfo -NAME ...'
class dset_3dinfo:

    def __init__(self, fname, label=''):

        self.dset   = fname

        self.label  = label

        # Get the orientation, voxel size and dims of the dset
        # might use these later.  Or not.
        cmd = '''3dinfo         \
        -orient -ad3 -n4        \
        -prefix_noext           \
        -datum                  \
        -is_oblique             \
        -obliquity              \
        -dmin                   \
        -dmax                   \
        {dset_name}
        '''.format( dset_name=fname )
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )

        expected_vals = 14 # !have to add to this as more items are
                           # added to save-- keeps a safe-check!

        ainfo = com.so[0].split()
        if len(ainfo) != expected_vals :
            # we have to know how many values to expect for this
            print("** ERROR: should be getting {exp_vals} numbers!\n"
                  "   Not reading file {fname} properly? Is the path correct?"
                  "".format(fname=self.dset_freq, exp_vals=expected_vals))
            sys.exit(3)

        # ... and add all the info in to the obj
        self.orient   =         ainfo[0]
        self.ad3      = [ float(ainfo[1]), 
                          float(ainfo[2]), 
                          float(ainfo[3]) ]
        self.n4       = [ int(  ainfo[4]), 
                          int(  ainfo[5]), 
                          int(  ainfo[6]),
                          int(  ainfo[7]) ]
        self.prefix_noext =     ainfo[8]
        self.datum        =     ainfo[9]
        self.is_oblique = bool(ainfo[10])
        self.obliquity = float(ainfo[11]) 
        self.dmin   = intfloat(ainfo[12]) 
        self.dmax   = intfloat(ainfo[13]) 
        # index of last ainfo[] element here should be: expected_vals - 1


# -------------------------------------------------------------------------

def intfloat(x, remove_pipes=True):

    # this is to deal with an annoyance for dmin and dmax when a 4D
    # dataset is entered.
    if remove_pipes :
        x = x.replace('|', '')

    try:
        out = int(x)
    except:
        out = float(x)
    return out

# -------------------------------------------------------------------------

# read in inputs for this program, and store info in this obj
class iopts_b0_corr:

    def __init__(self):

        self.full_cmd       = ''           # existential...

        self.dset_freq      = ''           # freqOrig
        self.dset_freq_name = ''           # ... with no path
        self.dset_epi       = ''           # epiRest-{eachSubSes}{defaultExt}
        self.dset_epi_name  = ''           # ... with no path

        self.dset_magn      = ''           # magOrig
        self.dset_magn_name = ''           #  ... with no path
        self.dset_mask      = ''           # maskOrig
        self.dset_mask_name = ''           #  ... with no path

        self.freq_info      = False        # will be an obj of 3dinfo
        self.epi_info       = False        # will be an obj of 3dinfo

        self.epi_pe_ind     = -1           # index of PE direction {0|1|2}

        self.epi_json       = ''           # can be input to parse for params
        self.epi_jdict      = {}           # JSON file as dict
        self.epi_jdict_pe_dir = ''         # for reporting, if used

        # path info, derived from I/O files
        self.origdir        = os.getcwd()  # where cmd is run from
        self.prefix         = ''           # 'opref', with path
        self.prefix_name    = ''           # 'opref', no path
        self.ipath          = ''           # dir of input dset_freq
        self.outdir         = ''           # output dir
        self.wdir_name      = ''           # (pathless) name of tmp wdir
        self.wdir           = ''           # (path-bearing) tmp workdir
        self.ocmds_fname    = ''           # output script of cmds
        self.opars_fname    = ''           # output colon sep'ed file of pars

        self.odset_epi      = ''           # the warped EPI
        self.odset_warp     = ''           # the actual warp applied to EPI

        self.dext           = '.nii.gz'    # default file ext

        # intermed file names for B0_corr(): 'bc' = 'B0 corr'
        self.dset_int_00    = 'bc_00'
        self.dset_int_01    = 'bc_01_smoo'
        self.dset_int_02    = 'bc_02_smoo_wrpd2epi'
        self.dset_int_03    = 'epi_corr'
        self.warp_00        = ''
        self.warp_01        = ''

        self.list_histog_int = ''          # 3dBrickstat info of intermed warp

        # intermediate names for mask_B0(): 'mb' = 'mask B0'
        self.dset_int_mask_00 = 'mb_00_uni'
        self.dset_int_mask_01 = 'mb_01_mask'

        # params 
        self.freq_scale           = 1.0 # user can change units&sign
        self.epi_pe_echo_sp       = 0   # epiPhaseEncodeEchoSpacing
        self.epi_pe_bwpp          = 0   # BandwidthPerPixelPhaseEncode
        self.epi_pe_voxdim        = 0   # voxdim along PE (in mm)
        self.epi_pe_matrlen       = 0   # matrix len along PE (count)
        self.epi_pe_dir           = ''  # distDirections: PE direction, e.g. AP
        self.epi_pe_dir_nwarp_pol = 0   # polarity for 3dNwarpCat notation
        self.epi_pe_dir_nwarp_opp = 0   # opp of polarity, for 3dNwarpCat
        self.blur_sigma       = ddefs['DEF_bsigma']  # 'blurSigma'

        # about obliquity: first check for EPI-freq obl diff (doesn't
        # really matter); then make a mat of any EPI-freq obl diff
        self.is_same_obl_epi_freq  = False  
        self.aff12_obl_epi_freq = ddefs['DEF_aff12_obl_epi_freq'] 

        # params, calc'ed during runtime
        self.freq_ctr      = 0.0           # (freqOut); for centering B0
        self.meth_recenter_freq = ddefs['DEF_meth_recenter_freq'] # how to ctr B0

        # masking params
        self.npeels   = ddefs['DEF_npeels'] # param for mask_B0 (3dAutomask)
        self.nerode   = ddefs['DEF_nerode'] # param for mask_B0 (3dAutomask)

        self.do_clean       = True         # clean up intermed dir or not
        self.overwrite      = ''           # pass along overwrite flag (or not)

        # This is here basically to just initialize the 'comm' obj
        cmd = 'pwd'
        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


    # -------------- methods to populate -----------------

    def set_dset( self, dd, dset_type='' ):

        dd_namedict = BASE.parse_afni_name(dd)
        dd_name = '{prefix}{view}{extension}'.format(**dd_namedict)

        if dset_type == 'freq' :
            self.dset_freq      = dd
            self.dset_freq_name = dd_name
            self.freq_info      = dset_3dinfo(dd, label=dset_type)
            self.ipath          = dd_namedict['path']

        elif dset_type == 'epi' :
            self.dset_epi      = dd
            self.dset_epi_name = dd_name
            self.epi_info      = dset_3dinfo(dd, label=dset_type)

        elif dset_type == 'magn' :
            self.dset_magn      = dd
            self.dset_magn_name = dd_name
        elif dset_type == 'mask' :
            self.dset_mask      = dd
            self.dset_mask_name = dd_name
        else: 
            print( "** ERROR: don't know what type of input {} is"
                   "".format(dd) )
            sys.exit(3)

    def set_epi_pe_echo_sp( self, cc ):
        self.epi_pe_echo_sp = float(cc)

    def set_epi_pe_voxdim( self, cc ):
        self.epi_pe_voxdim = float(cc)

    def set_epi_pe_matrlen( self, cc ):
        self.epi_pe_matrlen = cc # should always be int... int(cc)

    def set_epi_pe_bwpp( self, cc ):
        self.epi_pe_bwpp = float(cc)

    def set_epi_pe_dir( self, cc ):
        self.epi_pe_dir = cc
        # set the polarity as well, needed for 3dNwarpCat
        self.set_epi_pe_dir_polar( )

    def set_epi_pe_dir_polar( self ):
        # A NOTE ON THE DIRECTIONALITY OF THE PHASE CORRECTION: in
        # mri_nwarp.c, the special notation for warps (e.g.:
        # AP:1.0:DSET_NAME) treats opposite directions the same (i.e.,
        # invoking AP and PA yield similar results); the second
        # parameter differentiates directionality (AKA polarity).
        # Moreover, I think those were made with RAI in mind, so
        # AP:1.0 translates to 'A>>P', AP:-1.0 to 'P>>A', RL:1.0 to
        # 'R>>L', etc.  Therefore, we set the polarity here, 

        if ORI_POS.__contains__( self.epi_pe_dir ) :
            self.epi_pe_dir_nwarp_pol =  1.0
            self.epi_pe_dir_nwarp_opp = -1.0
        elif ORI_NEG.__contains__( self.epi_pe_dir ) :
            self.epi_pe_dir_nwarp_pol = -1.0
            self.epi_pe_dir_nwarp_opp =  1.0
        else:
            print("** ERROR: unrecognized axis {} from which to get polarity."
                  "".format(self.epi_pe_dir))
            sys.exit(5)

    def set_phaseDistScale( self, cc ):
        self.freq_scale = float(cc)

    def set_list_histog_int(self, cc ):
        aa = cc.split()
        self.list_histog_int = aa[1::2]

    def set_npeels( self, cc ):
        self.npeels = int(cc)

    def set_nerode( self, cc ):
        self.nerode = int(cc)

    def set_prefix( self, ppp ):
        self.prefix = ppp
        self.prefix_name = BASE.parse_afni_name(ppp)['prefix']

    def set_clean( self, ll ):
        self.do_clean = ll

    def set_overwrite( self, ll ):
        # 'll' is just true or false here
        if ll :
            self.overwrite = '-overwrite'
        else:
            self.overwrite = ''

    def set_wdir_name( self, dd ):
        self.wdir_name = dd

    def set_ocmds_fname( self, dd ):
        self.ocmds_fname = dd

    def set_opars_fname( self, dd ):
        self.opars_fname = dd

    def set_full_cmd( self, dd ):
        self.full_cmd = dd

    def set_freq_ctr( self, dd ):
        self.freq_ctr = float(dd)

    def set_meth_recenter_freq( self, dd ):
        self.meth_recenter_freq = dd
        
    def set_epi_json( self, dd ):
        self.epi_json = dd

    def set_is_same_obl_epi_freq( self, dd ):
        self.is_same_obl_epi_freq = bool(dd)

    # ---------------------------------------------------------------------

    def set_epi_pe_voxdim_from_dset(self):
        # we have the 3dinfo info from the freq dset to use
        
        if self.epi_pe_ind < 0 :
            # then we don't have it from either a JSON or the user, so
            # go to The Dataset
            try:
                self.epi_pe_ind = self.epi_info.orient.index(self.epi_pe_dir[0])
            except:
                self.epi_pe_ind = self.epi_info.orient.index(self.epi_pe_dir[1])
            # ... and will whine horribly if THAT fails!

        PE_voxdim  = self.epi_info.ad3[ self.epi_pe_ind ]
        PE_matrlen = self.epi_info.n4[ self.epi_pe_ind ]
        #PE_length  = PE_voxdim * PE_matrdim

        self.set_epi_pe_voxdim( PE_voxdim ) 
        self.set_epi_pe_matrlen( PE_matrlen ) 

    # ---------------------------------------------------------------------

    def read_in_json(self):
        # proof of existence+readability
        if not(os.path.isfile(self.epi_json)) :
            print("** ERROR: can't find/read EPI's JSON: {}"
                  "".format(self.epi_json))
            sys.exit(3)

        with open(self.epi_json, 'r') as fff:
            self.epi_jdict = json.load(fff)    

    # ------------

    def parse_epi_json(self):
        '''Try to get necessary info from input JSON.  

        What we try to get from here for freq scaling:
        + BandwidthPerPixelPhaseEncode, OR
        + effective echo spacing

        What else we try to get, if necessary:
        + EPI PE dir

        In each case, the User's choice will take precedence (if they
        have entered one).

        '''

        self.read_in_json()

        # --------- Get phase encode direction, if possible -------------

        if self.epi_pe_dir :
            print("++ Ignore json for PE dir; already have it: {}"
                  "".format(self.epi_pe_dir))
            # and set PE index, for later use
            try:
                self.epi_pe_ind = self.epi_info.orient.index(self.epi_pe_dir[0])
            except:
                self.epi_pe_ind = self.epi_info.orient.index(self.epi_pe_dir[1])
            self.set_epi_pe_voxdim_from_dset()

        elif self.epi_jdict.__contains__('PhaseEncodingDirection') :
            PED = self.epi_jdict['PhaseEncodingDirection'] # e.g., j, j-, ...
            self.epi_jdict_pe_dir = PED
            self.epi_pe_ind = IND_DICT[ PED[0] ] # {0|1|2}

            # get the orientation associated with this index
            phase0 = self.epi_info.orient[ self.epi_pe_ind ]
            phase = [aaa for aaa in ORI_ALL if aaa.startswith(phase0)][0]

            # invert it, if need be
            if PED[-1] == '-' :
                phase = phase[::-1]
            self.set_epi_pe_dir(phase)
            self.set_epi_pe_voxdim_from_dset()

            print("++ Using json for PE dir: {}"
                  "".format(self.epi_pe_dir))

        else:
            print("+* WARNING: {} file does not contain field: {}"
            "".format(self.epi_json, 'PhaseEncodingDirection'))


        # ------ For 3dcalc calculation: convert freq to warp (mm) ------

        # The first+only thing we would need for this calculation
        if self.epi_pe_bwpp :
            print("++ Ignore json for BWPP; already have it: {}"
                  "".format(self.epi_pe_bwpp))
        elif self.epi_jdict.__contains__('BandwidthPerPixelPhaseEncode') :
            PEBWPP = self.epi_jdict['BandwidthPerPixelPhaseEncode'] # e.g., 36.44
            self.set_epi_pe_bwpp( PEBWPP )
            print("++ Using json for BWPP: {}"
                  "".format(self.epi_pe_bwpp))
        else:
            if self.epi_jdict.__contains__('EffectiveEchoSpacing') :
                PEES = self.epi_jdict['EffectiveEchoSpacing'] # e.g., 0.00031
                self.set_epi_pe_echo_sp( PEES )

                # do the calc later for BWPP
                print("++ Will use json for BWPP, via EffectiveEchoSpacing: {}"
                      "".format(self.epi_pe_echo_sp))

            else:
                print("+* WARNING: {} file does not contain field: {}"
                "".format(self.epi_json, 'BandwidthPerPixelPhaseEncode'))
                print("+* WARNING: {} file does not contain field: {}"
                "".format(self.epi_json, 'EffectiveEchoSpacing'))
        

    # ---------- fill in vals ----------------

    def finish_defs(self):

        # use this function here, because this file won't exist yet!
        pp = os.path.dirname(self.prefix)
        if not(pp) :
            pp = '.' # the above returns '' for local dir-- not cool
        self.outdir = pp

        if not(self.wdir_name) :
            self.wdir_name = ddefs['DEF_wdir_pref'] + afni_rand_newid()
        self.wdir   = self.outdir + '/' + self.wdir_name

        self.warp_00 = ':'.join([ self.epi_pe_dir, 
                                  str(self.epi_pe_dir_nwarp_pol), 
                                  self.dset_int_01 ])
        self.warp_01 = ':'.join([ self.epi_pe_dir, 
                                  str(self.epi_pe_dir_nwarp_opp), 
                                  self.dset_int_02 ])

        self.odset_epi  = self.prefix_name + '_EPI'
        self.odset_warp = self.prefix_name + '_WARP'

        if not(self.ocmds_fname) :
            self.ocmds_fname = self.outdir + '/' 
            self.ocmds_fname+= self.prefix_name + '_cmds.tcsh'

        if not(self.opars_fname) :
            self.opars_fname = self.outdir + '/' 
            self.opars_fname+= self.prefix_name + '_pars.txt'

        # check relative obliquity diff bt EPI and freq dsets
        cmd = '''3dinfo         \
        -same_obl               \
        {0}                     \
        {1}
        '''.format( self.dset_epi, self.dset_freq )
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )
        self.set_is_same_obl_epi_freq(int(com.so[0]))

        print("++ EPI and phase have same obliq?  {}"
              "".format(self.is_same_obl_epi_freq) )

        if not(self.epi_pe_bwpp):
            if self.epi_pe_echo_sp :
                # [PT: Aug 23, 2019] Fixed this equation
                PEBWPP = 1.0/(self.epi_pe_echo_sp * self.epi_pe_matrlen)
                self.set_epi_pe_bwpp( PEBWPP )
                print("++ Using PE effective echo spacing to calc BWPP: {}"
                      "".format(self.epi_pe_bwpp))
            else: 
                print("** ERROR: Can't calculate bandwidth per pix?")
                print("   How did this come about?")
                print("   Not using '{epi_pe_bwpp}' or '{epi_pe_echo_sp}'?"
                      "".format(**all_opts))
                sys.exit(6)


    # ---------- check ----------------

    def check_req(self):
        ''' Check for and point out any missing inputs.'''
        MISS = 0

        if not(self.dset_freq) :
            print("** ERROR: missing '{in_freq}' dset"
                  "".format(**all_opts))
            MISS+=1

        if not(self.dset_epi) :
            print("** ERROR: missing '{in_epi}' dset"
                  "".format(**all_opts))
            MISS+=1

        if not(self.dset_mask) and not(self.dset_magn) :
            print("** ERROR: need either a '{in_mask}' or '{in_magn}' dset,\n"
                  "   from which to calculate a mask."
                  "".format(**all_opts))
            MISS+=1

        if not(self.epi_pe_bwpp) and not(self.epi_pe_echo_sp) :
            print("** ERROR: missing some necessary info to scale freq data;\n"
                  "   one of the following is necessary about the EPI :\n"
                  "   '{epi_pe_bwpp}', bandwidth per pix in PE dir\n"
                  "       -> e.g., 'BandwidthPerPixelPhaseEncode' in JSON\n"
                  "   '{epi_pe_echo_sp}', effective echo spacing in PE dir\n"
                  "       -> e.g., 'EffectiveEchoSpacing' in JSON.\n"
                  "".format(**all_opts))
            MISS+=1

        if not(self.epi_pe_voxdim) :
            print("** ERROR: missing '{epi_pe_voxdim}', the voxel dimension "
                  "(in mm) along the EPI phase axis"
                  "".format(**all_opts))
            MISS+=1

        if not(self.epi_pe_dir) :
            print("** ERROR: missing '{epi_pe_dir}', distortion axis "
                  "".format(**all_opts))
            MISS+=1
        else:
            if not(ORI_ALL.__contains__(self.epi_pe_dir)) :
                print("** ERROR: value '{aa}' after '{bb}' is not \n"
                      "   allowed.  Must be one of:\n"
                      "   {cc}"
                      "".format(aa=self.epi_pe_dir, 
                                bb=all_opts['epi_pe_dir'],
                                cc=ORI_ALL))
                MISS+=1

        if not(self.epi_pe_dir_nwarp_pol) :
            print("** ERROR: nwarp polarity (here, {epi_pe_dir_nwarp_pol}) "
                  "has somehow not been set?"
                  "".format(**all_opts))
            MISS+=1

        if (self.freq_info.datum == 'float') and \
           (self.meth_recenter_freq == 'mode') :
            print("** ERROR: can't have freq dset datum type be 'float' "
                  "and then\n"
                  "   use (def) 'mode' to recenter its values;\n"
                  "   see {do_recenter_freq} for recentering in other ways \n"
                  "   (like 'median'?)."
                  "".format(**all_opts))
            MISS+=1

        if not(self.prefix) :
            print("** ERROR: missing '{prefix}' info"
                  "".format(**all_opts))
            MISS+=1

        return MISS

    # ---------------------------------------------------------------------
    # ---------------------------------------------------------------------

    def copy_inps_to_wdir( self ):
        '''Basically, prep for running B0 correction:

        + make output+working dir,

        + copy the dsets there, for easier scripting (+possible
          viewing later).

        Everything gets the same name, just in a new location.

        '''

        # Make a dictionary of all variables:values, so all text
        # substitutions can be done more simply
        self_vars = vars( self ) 

        print("\n++ -------------------- copy inputs -----------------------")
        print("   wdir: {wdir}\n".format( **self_vars ))

        # Make working directory, which is a subset out output dir, so
        # that will be created automatically, too.
        cmd = '''
        \mkdir -p {wdir}
        '''.format( **self_vars )
        status, so, se = BASE.simple_shell_exec(cmd, capture=1)
        check_for_shell_exec_failure(status, so, se, cmd )

        # ... and copy dsets over

        cmd = '''3dcalc {overwrite}         \
        -echo_edu                  \
        -a {dset_freq}             \
        -expr 'a'                  \
        -prefix {wdir}/{dset_freq_name}
        '''.format( **self_vars )
        status, so, se = BASE.simple_shell_exec(cmd, capture=1)
        check_for_shell_exec_failure(status, so, se, cmd )

        cmd = '''3dcalc {overwrite}         \
        -echo_edu                  \
        -a {dset_epi}              \
        -expr 'a'                  \
        -prefix {wdir}/{dset_epi_name}
        '''.format( **self_vars )
        status, so, se = BASE.simple_shell_exec(cmd, capture=1)
        check_for_shell_exec_failure(status, so, se, cmd )

        if self.dset_magn :
            cmd = '''3dcalc {overwrite}         \
            -echo_edu                  \
            -a {dset_magn}             \
            -expr 'a'                  \
            -prefix {wdir}/{dset_magn_name}
            '''.format( **self_vars )
            status, so, se = BASE.simple_shell_exec(cmd, capture=1)
            check_for_shell_exec_failure(status, so, se, cmd )

        if self.dset_mask :
            cmd = '''3dcalc {overwrite}         \
            -echo_edu                  \
            -a {dset_mask}             \
            -expr 'a'                  \
            -prefix {wdir}/{dset_mask_name}
            '''.format( **self_vars )
            status, so, se = BASE.simple_shell_exec(cmd, capture=1)
            check_for_shell_exec_failure(status, so, se, cmd )

        return 0

    # ---------------------------------------------------------------------

    # This function does all the work:  originally created by Vinai R.
    def B0_corr( self ):
        ''' 
        
        UNITS
        ------
        EPI PE echo spacing : sec
        EPI FOV or voxdim   : mm

        '''

        # Make a dictionary of all variables:values, so all text
        # substitutions can be done more simply
        self_vars = vars( self ) 

        print("\n++ -------------------- calc B0 corr -----------------------")
        print("   freq dset: {dset_freq_name}\n".format( **self_vars ))

        cmd = '''# ----- Convert phase/freq dset into warp '''
        cmd+= '''along the PE axis'''
        if self.dset_epi :
            cmd+= ''',\n#       and apply it to an EPI dset.'''
        cmd+= '\n'

        self.comm.history.append(cmd)


        # Go to wdir for all processing, since dsets have been
        # copied/made here.
        os.chdir(self.wdir)

        if self.meth_recenter_freq != 'NONE' :
            cmd = '''3dROIstats           \
            -quiet                        \
            -nomeanout                    \
            -mask {dset_mask_name}        \
            -{meth_recenter_freq}         \
            {dset_freq_name}
            '''.format( **self_vars )

            # Find the mode of the frequency distribution in the brain and
            # subtract this value from the field map. This is from potential
            # vendor offsets in F0.
            self.comm = BASE.shell_com(cmd, capture=1)
            self.comm.run()
            check_for_shell_com_failure(self.comm, cmd )

            # Add this centering value into the obj
            self.set_freq_ctr( self.comm.so[0].strip() )


        # Recenter, scale (def=1.0), and convert units with effective
        # echo spacing and length of PE axis. 
        # Freq dset units: ang freq (rad/s).
        cmd = '''3dcalc {overwrite}                              \
        -echo_edu                                                \
        -a      {dset_freq_name}                                 \
        -b      {dset_mask_name}                                 \
        -expr   "({freq_scale}/6.2831853)*(a-{freq_ctr})/{epi_pe_bwpp}*{epi_pe_voxdim}*b" \
        -datum  float                                            \
        -prefix {dset_int_00}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # Use the '-1blur_sigma 9' option to match the '--smooth3=9' option
        # in FSL's fugue
        cmd = '''3dmerge {overwrite}       \
        -echo_edu                          \
        -doall                             \
        -1blur_sigma  {blur_sigma}         \
        -datum        float                \
        -prefix       {dset_int_01}{dext}  \
        {dset_int_00}{dext} 
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )

        
        # For informational purposes: to report in final params, a
        # ~histograph of shift values
        cmd = '''3dBrickStat              \
        -mask {dset_mask_name}            \
        -percentile 20 20 100             \
        {dset_int_01}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )

        # Add this centering value into the obj
        self.set_list_histog_int( self.comm.so[0] )

        # -------------------------------------------------------------
        # Calculate any difference in coordinate axes between
        # freq/phase and EPI dsets.  That is, it is *possible* that
        # they were acquired with different axes: one might have
        # oblique axes and the other not, or they might have been
        # acquired with different oblique axes.  The header
        # information of each is used: 
        # + IJK_TO_DICOM_REAL
        #   - exists in all dset headers
        #   - maps:  ijk --> scanner/cardinal axes
        # + IJK_TO_DICOM
        #   - exists in AFNI extensions (OK to rely on here, because
        #     all dsets were AFNI-3dcalc'ed into working dir)
        #   - maps:  ijk --> acquired/(possibly) oblique axes
        # NOTE: this works even if the phase and EPI dsets have the
        # same obliquity/acquisition axes. Thus, this step may not be
        # necessary, but it also should do no harm.

        # Here, we go from EPI oblique coors to EPI ijk to EPI scanner
        # coors (which are the same as freq dset scanner coors) to
        # freq ijk to freq oblique
        cmd = '''cat_matvec                     \
        -ONELINE                                \
        {dset_epi_name}::IJK_TO_DICOM -I        \
        {dset_epi_name}::IJK_TO_DICOM_REAL      \
        {dset_freq_name}::IJK_TO_DICOM_REAL -I  \
        {dset_freq_name}::IJK_TO_DICOM          \
        > {aff12_obl_epi_freq}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # -------------------------------------------------------------
        # Self-warp field map to match EPI distortions {warp_00} comes
        # from {dset_int_01} (while not obvious/apparent here, it was
        # defined to do so in object methods above) output here will
        # be in the EPI coors+space-- that is, any relative obliquity
        # diff gets accounted for here!
        # Not using wsinc5 interpolation for the output here, to avoid
        # ringing... have to consider that
        cmd = '''3dNwarpApply {overwrite} \
        -echo_edu                         \
        -warp   "{aff12_obl_epi_freq} {warp_00}{dext}"  \
        -master {dset_epi_name} \
        -ainterp quintic \
        -prefix {dset_int_02}{dext}   \
        -source {dset_int_01}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # ... and finish the job by applying obliquity in the new dset
        cmd = '''3drefit                  \
        -echo_edu                         \
        -atrcopy {dset_epi_name} IJK_TO_DICOM_REAL \
        {dset_int_02}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # calculate the final warp and output it (so it can be
        # concatenated later); {warp_01} comes from {dset_int_02}
        # (while not obvious/apparent here, it was defined to do so in
        # object methods above).
        cmd = '''3dNwarpCat  {overwrite} \
        -echo_edu                   \
        -warp1  {warp_01}{dext}     \
        -prefix {odset_warp}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # ... and finish the job by applying obliquity in the new dset
        cmd = '''3drefit                  \
        -echo_edu                         \
        -atrcopy {dset_epi_name} IJK_TO_DICOM_REAL \
        {odset_warp}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # copy the WARP results up a directory
        cmd = '''3dcopy {overwrite} \
        {odset_warp}{dext}          \
        ../{odset_warp}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # Now apply warped field map to fix EPI
        cmd = '''3dNwarpApply  {overwrite}  \
        -echo_edu                     \
        -warp   {odset_warp}{dext}    \
        -prefix {odset_epi}{dext}     \
        -source {dset_epi_name}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # ... and finish the job by applying obliquity in the new dset
        cmd = '''3drefit                  \
        -echo_edu                         \
        -atrcopy {dset_epi_name} IJK_TO_DICOM_REAL \
        {odset_epi}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # copy the EPI results up a directory
        cmd = '''3dcopy {overwrite}  \
        {odset_epi}{dext}            \
        ../{odset_epi}{dext}
        '''.format( **self_vars )
        
        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        os.chdir(self.origdir)
        print("\n++ Done with B0_corr.\n")

        return 0

    # --------------------------------------------------------------------------

    # Make a mask from the magnitude image. Originally created by Vinai R.
    def mask_B0( self ):

        # Make a dictionary of all variables:values, so all text
        # substitutions can be done more simply
        self_vars = vars( self ) 

        print("\n++ ---------------- make mask for B0 ---------------------")
        print("   magn dset: {dset_magn_name}\n".format( **self_vars ))

        cmd = '''# ----- Make a mask from the magnitude volume.
        '''
        self.comm.history.append(cmd)


        # Go to wdir for all processing, since dsets have been
        # copied/made here.
        os.chdir(self.wdir)

        cmd = '''3dUnifize {overwrite}     \
        -echo_edu                          \
        -prefix {dset_int_mask_00}{dext}   \
        -input  {dset_magn_name}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        cmd = '''3dAutomask {overwrite}    \
        -echo_edu                          \
        -prefix {dset_int_mask_01}{dext}   \
        -erode  {nerode}                   \
        -peels  {npeels}                   \
        {dset_int_mask_00}{dext}
        '''.format( **self_vars )

        self.comm = BASE.shell_com(cmd, capture=1)
        self.comm.run()
        check_for_shell_com_failure(self.comm, cmd )


        # Save and store output name, with path to wdir (even though
        # we will hop back to wdir later)
        omask = '''{wdir}/{dset_int_mask_01}{dext}'''.format(**self_vars)
        self.set_dset( omask, 'mask' )


        os.chdir(self.origdir)
        print("\n++ Done with making mask for B0.\n")

        return 0

# --------------------------------------------------------------------------

    def write_params( self ) :

        ss = ''

        # freq dset info
        if self.freq_info :
            ss+= "{:30s} : {}  {}\n".format( 'Freq range (init)', 
                                             self.freq_info.dmin,
                                             self.freq_info.dmax )
        if self.freq_info :
            ss+= "{:30s} : {}\n".format( 'Freq obliquity (deg)', 
                                             self.freq_info.obliquity )
        if self.freq_scale :
            ss+= "{:30s} : {}\n".format( 'Freq scale factor', 
                                         self.freq_scale )
        if self.list_histog_int :
            ll = '  '.join(self.list_histog_int)
            ss+= "{:30s} : {}\n".format( 'Warp (mm) in mask, 20-100 %ile', 
                                         ll )
        if ss :
            ss+= '\n'

        # epi dset info
        if self.epi_info :
            ss+= "{:30s} : {}  {}\n".format( 'EPI range (init)', 
                                             self.epi_info.dmin,
                                             self.epi_info.dmax )
        if self.epi_info :
            ss+= "{:30s} : {}\n".format( 'EPI obliquity (deg)', 
                                             self.epi_info.obliquity )
        if self.epi_pe_echo_sp :
            ss+= "{:30s} : {}\n".format( 'EPI PE echo spacing (s)', 
                                         self.epi_pe_echo_sp )
        if self.epi_pe_bwpp :
            ss+= "{:30s} : {}\n".format( 'EPI PE bandwidth per pix (Hz)', 
                                         self.epi_pe_bwpp )
        if self.epi_pe_matrlen :
            ss+= "{:30s} : {}\n".format( 'EPI PE matrix len (nvox)', 
                                         self.epi_pe_matrlen )
        if self.epi_pe_voxdim :
            ss+= "{:30s} : {}\n".format( 'EPI PE vox dim (mm)', 
                                         self.epi_pe_voxdim )
        if self.epi_pe_dir :
            ss+= "{:30s} : {}\n".format( 'EPI PE direction', 
                                         self.epi_pe_dir )
        if self.epi_pe_dir_nwarp_pol :
            ss+= "{:30s} : {}\n".format( 'EPI PE nwarp pol', 
                                         self.epi_pe_dir_nwarp_pol )
        if self.epi_jdict_pe_dir :
            ss+= "{:30s} : {}\n".format( 'EPI PE dir from JSON', 
                                         self.epi_jdict_pe_dir )
            if self.epi_pe_ind :
                ss+= "{:30s} : {}\n".format( 'EPI PE dir index',
                                             self.epi_pe_ind )
            if self.epi_info :
                ss+= "{:30s} : {}\n".format( 'EPI orient', 
                                             self.epi_info.orient )

        f = open(self.opars_fname, 'w')
        f.write(ss)
        f.close()

        print("++ Wrote params history: {}".format(self.opars_fname))



# --------------------------------------------------------------------------
    def write_history( self ) :
        
        this_prog    = os.path.basename(self.full_cmd[0])
        full_cmd_str = this_prog + ' ' + ' '.join(self.full_cmd[1:])

        run_history = '#!/bin/tcsh\n\n'

        run_history+= '# The generative command:\n'
        run_history+= '# ' + full_cmd_str + '\n\n\n'

        run_history+= '# To re-run on the working dir (from outdir):\n'
        run_history+= 'cd ' + self.wdir_name + '\n\n\n'

        run_history+= '\n'.join(self.comm.history[1:])

        f = open(self.ocmds_fname, 'w')
        f.write(UTIL.add_line_wrappers(run_history))
        f.close()

        print("++ Wrote commands history: {}".format(self.ocmds_fname))
        

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

def parse_args_b0_corr(full_argv):
    argv = full_argv[1:]

    Narg = len(argv)

    if not(Narg):
        print(help_string_b0_corr)
        sys.exit(0)
    
    # initialize objs
    iopts  = iopts_b0_corr()

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
            print(help_string_b0_corr)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "{prefix}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_prefix(argv[i])

        # well, EPI isn't required anymore, and one needs *either*
        # mask or magn
        elif argv[i] == "{in_freq}".format(**all_opts) or \
             argv[i] == "{in_mask}".format(**all_opts) or \
             argv[i] == "{in_magn}".format(**all_opts) or \
             argv[i] == "{in_epi}".format(**all_opts) :
            dt = argv[i][4:]
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            # this both reads in dset and gets useful specs/info on it
            # via 3dinfo, which can be used subsequently
            iopts.set_dset( argv[i], dset_type=dt )

        # ---------- opt ---------------

        elif argv[i] == "{in_epi_json}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_epi_json(argv[i])

        elif argv[i] == "{epi_pe_echo_sp}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_epi_pe_echo_sp(argv[i])

        elif argv[i] == "{epi_pe_voxdim}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_epi_pe_voxdim(argv[i])

        elif argv[i] == "{epi_pe_dir}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_epi_pe_dir(argv[i])

        elif argv[i] == "{scale_freq}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_phaseDistScale(argv[i])

        elif argv[i] == "{do_recenter_freq}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_meth_recenter_freq(argv[i])

        elif argv[i] == "{out_cmds}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_ocmds_fname(argv[i])

        elif argv[i] == "{out_pars}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_opars_fname(argv[i])

        elif argv[i] == "{blur_sigma}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_blurSigma(argv[i])

        elif argv[i] == "{automask_peels}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_npeels(argv[i])

        elif argv[i] == "{automask_erode}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_nerode(argv[i])

        elif argv[i] == "{wdir_name}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_wdir_name(argv[i])

        elif argv[i] == "{no_clean}".format(**all_opts) :
            iopts.set_clean(False)

        elif argv[i] == "{overwrite}".format(**all_opts) :
            iopts.set_overwrite(True)


        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    # need to parse JSON before checking that all params have been
    # set, but this also requires having the dset_freq read in
    # already, so do this here.
    if iopts.epi_json :
        iopts.parse_epi_json()

    # this is here, but it should really be done by the time
    # iopts.parse_epi_json() has finished
    if not(iopts.epi_pe_voxdim):
        iopts.set_epi_pe_voxdim_from_dset()

    if iopts.check_req():
        print("   -------------------------------")
        print("** ERROR with input arguments (see detailed whining above).")
        sys.exit(1)

    iopts.finish_defs()  # make some paths we need later

    return iopts


# --------------------------------------------------------------------------


