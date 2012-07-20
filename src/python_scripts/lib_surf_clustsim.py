#!/usr/bin/env python

# general functions for use by uber_tool*.py

import sys, os
from time import asctime
import glob

import afni_base as BASE
import afni_util as UTIL
import lib_subjects as SUBJ

DEF_UBER_DIR = 'uber_results'        # top directory for output
DEF_TOP_DIR  = 'tool_results'     # top subject dir under uber_results

g_history = """
  slow_surf_clustsim.py history

    0.0  20 Jun, 2011: initial revision
    0.1  08 Jul, 2011: added -on_surface, which might not be so useful
    0.2  13 Jul, 2011: let plist be list of strings or floats
    0.3  14 Jul, 2011: show date per iter block and use ./ in 3dcalc prefix
    0.4  22 Jul, 2011: scale smoothed data to be normally distributed
                       (i.e. divide by stdev)
    0.5  25 Jul, 2011: added keepblocks var, to limit kept datasets
    0.6  29 Jul, 2011:
         - name z.max files by the p-values
         - suggest quick.alpha.vals.py command
    0.7  10 Feb, 2012: help update for HJ: -on_surface takes yes/no parameter
    0.8  17 Jul, 2012: removed -Niter opt from SurfSmooth (let it decide)
"""

g_version = '0.8 (July 17, 2012)'

# ----------------------------------------------------------------------
# global values to apply as defaults

# ----------------------------------------------------------------------
# global definitions of result, control and user defaults
# (as well as string versions of control and user defaults)

# ---- resulting values returned after class actions ----
g_res_defs = SUBJ.VarsObject("slow_surf_clustsim result variables")
g_res_defs.file_proc     = ''   # file name for process script
g_res_defs.output_proc   = ''   # output from running proc script

# ---- control variables: process control, not set by user in GUI

g_ctrl_defs = SUBJ.VarsObject("slow_surf_clustsim control defaults")
g_ctrl_defs.verb         = 1     # verbose level
g_ctrl_defs.proc_dir     = '.'   # process dir: holds scripts and result dir
g_ctrl_defs.time_process = 'yes' # run /usr/bin/time on commands?
                                 # rcr - default to 'no'
g_ctrl_defs.on_surface   = 'no'  # default to starting from the volume
g_ctrl_defs.nsteps       = 10
g_ctrl_defs.keepblocks   = 10    # number of iteration block outputs to keep

# ---- user variables: process control, alignment inputs and options ----

g_user_defs = SUBJ.VarsObject("slow_surf_clustsim user defaults")
g_user_defs.copy_scripts   = 'yes'      # do we make .orig copies of scripts?
g_user_defs.results_dir    = 'clust.results' # where script puts results

# required inputs
g_user_defs.spec_file      = ''
g_user_defs.surf_vol       = ''
g_user_defs.vol_mask       = ''  # required only if not on_surface

# other inputs
g_user_defs.niter          = 20         # total number of iterations
g_user_defs.itersize       = 10         # iteration block size (speed-up)

g_user_defs.pthr_list      = [ 0.1, 0.05, 0.02, 0.01 ]
g_user_defs.blur           = 4.0
g_user_defs.rmm            = -1

g_user_defs.surfA          = 'smoothwm'
g_user_defs.surfB          = 'pial'
g_user_defs.map_func       = 'ave'



# string versions of variables - used by GUI and main
# (when creating SurfClust object, string versions of vars are passed)
g_cdef_strs = g_ctrl_defs.copy(as_strings=1)
g_udef_strs = g_user_defs.copy(as_strings=1)


g_make_empty_surf_str = """
# ------------------------------
# make an empty surface dataset (for data on surface and 3dmaskave)

# get number of nodes
SurfMeasures -spec $spec_file -sv $surf_vol -surf_A $surfA -out_1D nodes.1D
set last_node = `tail -n 1 nodes.1D`
if ( $last_node <= 0 ) then
    echo "** failed to count nodes from surface $surfA"
    exit
endif

# make $empty_surf of that size
set empty_surf = empty.gii
ConvertDset -o_gii -input nodes.1D -prefix $empty_surf   \\
            -add_node_index -pad_to_node $last_node

# make an all-1 surface for 3dmaskave
3dcalc -a $empty_surf -expr 1 -prefix all_1.gii
"""

# main class definition
class SurfClust(object):
   """class for creating surf clust script

        - cvars : control variables
        - uvars : user variables
        - rvars : return variables

        ** input vars might be string types, convert on merge

        variables:
           LV            - local variables
           cvars         - control variables
           uvars         - user variables
           cmd_text      - generated alignment script
           errors            --> array of resulting error messages
           warnings          --> array of resulting warning messages
   """
   def __init__(self, cvars=None, uvars=None):

      # ------------------------------------------------------------
      # variables

      # LV: variables local to this interface, not passed
      self.LV = SUBJ.VarsObject("local AP_Subject vars")
      self.LV.indent  = 8               # default indent for main options
      self.LV.istr    = ' '*self.LV.indent
      self.LV.retdir  = ''              # return directory (for jumping around)
      self.LV.rmsets  = []              # items to delete after keepblocks

      # merge passed user variables with defaults
      self.cvars = g_ctrl_defs.copy()
      self.uvars = g_user_defs.copy()
      self.cvars.merge(cvars, typedef=g_ctrl_defs)
      self.uvars.merge(uvars, typedef=g_user_defs)

      # output variables
      self.rvars = g_res_defs.copy()    # init result vars
      self.script = ''                  # resulting script
      self.errors = []                  # list of error strings
      self.warnings = []                # list of warning strings
      # ------------------------------------------------------------

      # ------------------------------------------------------------
      # preperatory settings

      if self.check_inputs(): return    # require spec, sv, vol_mask
      if self.set_directories(): return

      if self.cvars.verb > 3: self.LV.show('ready to start script')

      # do the work
      self.create_script()

   def check_inputs(self):
      """check for required inputs: anat, epi (check existence?)"""

      if self.uvars.is_empty('spec_file'):
         self.errors.append('** unspecified spec_file')

      if self.uvars.is_empty('surf_vol'):
         self.errors.append('** unspecified surf_vol dataset')

      if self.cvars.val('on_surface') != 'yes':
         # if not on surface, a volume mask is required
         if self.uvars.is_empty('vol_mask'):
            self.errors.append('** unspecified vol_mask (result grid) dataset')

      return len(self.errors)

   def set_directories(self):
      """always use $top_dir with absolute paths to datasets
      """

      inputs = [self.uvars.surf_vol, self.uvars.spec_file]
      if self.cvars.val('on_surface') != 'yes':
         inputs.append(self.uvars.vol_mask)

      # try converting to absolute paths
      try: inputs = [ os.path.abspath(fname) for fname in inputs ]
      except:
         msg = '** cannot convert inputs to asolute path names:\n   %s' % inputs
         self.errors.append(msg)
         return 1

      top_dir, parent_dirs, short_dirs, short_names = \
                UTIL.common_parent_dirs([inputs])

      self.LV.top_dir     = parent_dirs[0]  # common parent dir
      self.LV.short_names = short_names # if top_dir is used, they are under it

      if self.cvars.verb > 2:
         if self.cvars.val('on_surface') != 'yes':
            print '-- set_dirs: top_dir         = %s\n' \
                  '             short surf_vol  = %s\n' \
                  '             short spec_file = %s\n' \
                  '             short vol_mask  = %s\n' % (self.LV.top_dir,
                     short_names[0][0], short_names[0][1], short_names[0][2])
         else: # on surface
            print '-- set_dirs: top_dir         = %s\n' \
                  '             short surf_vol  = %s\n' \
                  '             short spec_file = %s\n' % (self.LV.top_dir,
                     short_names[0][0], short_names[0][1])

      # if top_dir isn't long enough, do not bother with it
      if self.LV.top_dir.count('/') < 2:
         self.LV.top_dir = ''
         if self.cvars.verb > 2: print '   (top_dir not worth using...)'

      return 0

   def create_script(self):
      """attempt to generate an alignment script
            - write script
            - keep a list of any warnings or errors
            - 
            - if there are errors, script might not be filled
      """

      # script prep, headers and variable assignments
      self.script  = self.script_init()
      self.script += self.script_set_vars()

      # do some actual work
      self.script += self.script_results_dir()
      self.script += self.script_main_process()
      self.script += self.script_tabulate_areas()
      self.script += self.script_finish()

      # add commands ...

      if len(self.errors) > 0: return   # if any errors so far, give up

      return

   def script_finish(self):

      zfile = '$results_dir/z.max.area.$plast'

      cmd = '# done, suggest how to look at some results\n'     \
            'set plast = $pthr_list[$#pthr_list]\n\n'           \
            '@ titers = $niter * $itersize\n\n'                \
            'echo ""\n'                                         \
            'echo "finished, consider the command:"\n'          \
            'echo "  quick.alpha.vals.py -niter $titers %s"\n'  \
            'echo ""\n' % zfile

      return cmd

   def script_tabulate_areas(self):
      """for each zthr, for each clust file, extract max area"""

      cmd = SUBJ.comment_section_string('extract cluster counts') + '\n'

      cmd += '# tabulate all results for each uncorrected p-threshold\n'  \
             'set maxa_list = ()   # track all max areas\n'               \
             'set failures = ()    # track cluster failures\n'            \
             'foreach pthr ( $pthr_list )\n'                              \
             '   # make note of current file\n'                           \
             '   set zfile = z.max.area.$pthr\n'                          \
             '\n'                                                         \
             '   # create empty file for this p-value\n'                  \
             '   echo -n "" > $zfile\n'                                   \
             '\n'                                                         \
             '   set file_list = ( clust.out.*.$pthr )\n'                 \
             '   echo "-- processing $#file_list files for p = $pthr"\n'  \
             '\n'                                                         \
             '   # process each file, counting through them\n'            \
             '   foreach findex ( `count -digits 1 1 $#file_list` )\n'    \
             '      set file = $file_list[$findex]\n'                     \
             '\n'                                                         \
             '      # print pacifier every 100 files\n'                   \
             '      if ( ! ($findex % 100) ) echo -n .\n'                 \
             '      if ( ! ($findex % 5000) ) echo ""\n'                  \
             '\n'                                                         \
             '      # grab the area field from first (largest) cluster\n' \
             "      set maxa = `awk '$1 == 1 {print $3}' $file`\n"        \
             '\n'                                                         \
             '      # and append it to the max file (if results exist)\n' \
             '      if ( $maxa != "" ) echo $maxa >> $zfile\n'            \
             '\n'                                                         \
             '   end  # file index\n'                                     \
             '\n'                                                         \
             '   # grab the max area for this p-value, and add to list\n' \
             '   # (if the max.area file is not empty for some reason)\n' \
             '   set nlines = `cat $zfile | wc -l`\n'                     \
             '   if ( $nlines != 0 ) then\n'                              \
             '      set maxa = `sort -rn $zfile | head -n 1`\n'           \
             '   else\n'                                                  \
             '      set failures = ( $failures $pthr )\n'                 \
             '      set maxa = 0\n'                                       \
             '   endif\n'                                                 \
             '\n'                                                         \
             '   set maxa_list = ( $maxa_list $maxa )\n'                  \
             '\n'                                                         \
             'end  # pthr\n'                                              \
             '\n'                                                         \
             'echo ""\n'                                                  \
             'echo "p-value thresholds   : $pthr_list"\n'                 \
             'echo "z-score thresholds   : $zthr_list"\n'                 \
             'echo "maximum cluster areas: $maxa_list"\n'                 \
             '\n'                                                         \
             'if ( $#failures ) then\n'                                   \
             '   echo ""\n'                                               \
             '   echo "** no clusters for p = $failures"\n'               \
             'endif\n'                                                    \
             '\n'

      return cmd

   def script_main_process(self):
      """setup is done, actually process the data

      """

      cmd = self.script_analysis_prep()

      # prepare contents of foreach loop
      cmd_3dcalc = self.script_do_3dcalc(indent=3)
      if self.cvars.val('on_surface') != 'yes':
         cmd_v2s = self.script_do_3dv2s(indent=3)
      else: cmd_v2s = ''
      cmd_ss     = self.script_do_surfsmooth(indent=3)
      cmd_scale  = self.rescale_stdev(indent=3)
      cmd_clust  = self.script_do_surfclust(indent=3)

      cmd_keepb  = self.script_keepblocks(indent=3)

      cmd +=                                                                 \
        '# for each iteration block, process $itersize sets of p/z-scores\n' \
        'foreach iter ( `count -digits 3 1 $niter` )\n\n'                    \
        '   # track time for each iteration\n'                               \
        '   echo "== iter block $iter (size $itersize) @ `date`"\n\n'        \
        + cmd_3dcalc + cmd_v2s + cmd_ss + cmd_scale                          \
        + cmd_clust + cmd_keepb +                                            \
        'end   # of foreach iter loop\n\n'

      return cmd

   def script_keepblocks(self, indent=0):

      # are we using keepblocks and have something to delete?
      if self.cvars.verb > 1: print '-- keepblocks: kb=%d, rmsets=%d)' \
                              % (self.cvars.keepblocks, len(self.LV.rmsets))
      if self.cvars.keepblocks <= 0 or len(self.LV.rmsets) == 0: return ''

      istr = ' '*indent

      clist = [ \
        '# if we are past keepblocks iterations, delete current datasets\n',
        'set icount = `ccalc -i $iter`  # avoid octal question\n',
        'if ( $icount > $keepblocks ) then\n']

      # add all of the delete linse
      for rmset in self.LV.rmsets: clist.append('   rm -f %s\n' % rmset)

      clist.append('endif\n\n')

      return istr + istr.join(clist)

   def script_do_surfclust(self, indent=0):
      istr = ' '*indent

      # time_str use is indended
      if self.LV.time_str: tstr = '      ' + self.LV.time_str
      else:                tstr = ''

      clist = [ \
        '# compute cluster sizes (for each iteration and p/z-score)\n',
        '@ iminus1 = $itersize - 1   # want 0-based indices\n',
        'foreach index ( `count -digits 1 0 $iminus1` )\n',
        '   foreach pind ( `count -digits 1 1 $#pthr_list` )\n',
        '      # note corresponding p and z-values\n',
        '      set pthr = $pthr_list[$pind]\n',
        '      set zthr = $zthr_list[$pind]\n\n',
        tstr,
        '      SurfClust -spec $spec_file -surf_A $surfA           \\\n',
        '                -input smooth.white.$iter.gii"[$index]" 0 \\\n',
        '                -sort_area -rmm $rmm -athresh $zthr       \\\n',
        '                > clust.out.$iter.$index.$pthr\n',
        '   end\n',
        'end\n\n' ]

      return istr + istr.join(clist)

   def rescale_stdev(self, indent=0):
      istr = ' '*indent

      clist = [ \
        '# rescale noise to be normally distributed\n',
        '3dmaskave -mask all_1.gii -sigma smooth.noise.$iter.gii  \\\n',
        "          | awk '{print $2}' > t.stdev.1D\n",
        '3dcalc -a smooth.noise.$iter.gii -b t.stdev.1D -expr a/b \\\n',
        '       -prefix smooth.white.$iter.gii\n\n']

      # add current output to optional delete list
      self.LV.rmsets.append('smooth.white.$iter.gii')

      return istr + istr.join(clist)

   def script_do_surfsmooth(self, indent=0):
      istr = ' '*indent

      if self.cvars.val('on_surface') == 'yes':
         inset = 'surf.noise.$iter.gii'
         niter = 5
      else:
         inset = 'surf.noise.$iter.niml.dset'
         niter = 10

      # removed -Niter option
      clist = [ \
        '# smooth to the given target FWHM\n',
        self.LV.time_str,
        'SurfSmooth -spec $spec_file -surf_A $surfA           \\\n',
        '           -input %s         \\\n' % inset,
        '           -met HEAT_07 -target_fwhm $blur           \\\n',
        '           -blurmaster %s    \\\n' % inset,
        '           -detrend_master                           \\\n',
        '           -output smooth.noise.$iter.gii            \\\n',
        '           | tee params.surf.smooth.$iter.1D\n\n' ]

      # add current output to optional delete list
      self.LV.rmsets.append('smooth.noise.$iter.gii')

      return istr + istr.join(clist)

   def script_do_3dv2s(self, indent=3):
      istr = ' '*indent

      clist = [ '# map noise voxels to surface domain\n',
                self.LV.time_str,
                '3dVol2Surf -spec $spec_file                       \\\n',
                '           -surf_A $surfA                         \\\n',
                '           -surf_B $surfB                         \\\n',
                '           -sv $surf_vol                          \\\n',
                '           -grid_parent vol.noise.$iter+orig      \\\n',
                '           -map_func $map_func                    \\\n',
                '           -f_steps $nsteps                       \\\n',
                '           -f_index nodes                         \\\n',
                '           -oob_value 0                           \\\n',
                '           -out_niml surf.noise.$iter.niml.dset\n\n' ]

      # add current output to optional delete list
      self.LV.rmsets.append('surf.noise.$iter.niml.dset')

      return istr + istr.join(clist)

   def script_do_3dcalc(self, indent=3):
      istr = ' '*indent

      if self.cvars.val('on_surface') == 'yes': domain = 'surface'
      else:                                     domain = 'volume'

      dlist = [ '# do not include single TR "time series" in 3dcalc\n',
                'if ( $itersize > 1 ) then\n',
                '   set bset = "-b dummy.TRs.$itersize.1D"\n',
                'else\n',
                '   set bset = ""\n',
                'endif\n\n',
                '# generate noise %s (possibly of $itersize TRs)\n' % domain,
                self.LV.time_str]

      if self.cvars.val('on_surface') == 'yes':
         clist = [ \
            '3dcalc -a $empty_surf $bset -expr "gran(0,1)" \\\n',
            '       -prefix ./surf.noise.$iter.gii -datum float\n\n' ]
         rmset = 'surf.noise.$iter.gii'
      else:
         clist = [ \
            '3dcalc -a $vol_mask $bset -expr "bool(a)*gran(0,1)" \\\n',
            '       -prefix ./vol.noise.$iter -datum float\n\n' ]
         rmset = 'vol.noise.$iter+*'

      # add current output to optional delete list
      self.LV.rmsets.append(rmset)

      return istr + istr.join(dlist) + istr + istr.join(clist)

   def script_analysis_prep(self):
      """return a set of commands to:
            convert pthr_list to zthr_list
            create a dummy time series of lenth itersize (if > 1)
            divide (ceil) niter by itersize (if > 1)
      """

      cmd  = SUBJ.comment_section_string('prep: make z-scores, etc.') + '\n'

      cmd += '# make zthr_list (convert p-values to z-scores)\n'        \
             '# (2-tailed computation mirrors athresh() in SurfClust)\n'\
             'set zthr_list = ()\n'                                     \
             'foreach pthr ( $pthr_list )\n'                            \
             '   # convert from p to z (code for N(0,1) is 5)\n'        \
             '   set zthr = `ccalc "cdf2stat(1-$pthr,5,0,0,0)"`\n'      \
             '   set zthr_list = ( $zthr_list $zthr )\n'                \
             'end\n\n'

      cmd += 'echo have p-values: $pthr_list\n' \
             'echo have z-scores: $zthr_list\n\n'

      cmd += '# make a dummy time file of length $itersize for 3dcalc\n' \
             '1deval -num $itersize -expr t > dummy.TRs.$itersize.1D\n\n'

      cmd += '# divide niter by itersize (take ceiling)\n'              \
             '@ niter = ( $niter + $itersize - 1 ) / $itersize\n\n'

      # always create an empty surface, and then an all-1 surface
      cmd += g_make_empty_surf_str

      return cmd

   def script_init(self):
      cmd = '#!/bin/tcsh -xef\n\n'                             \
            '# created by slow_surf_clustsim.py: version %s\n' \
            '# creation date: %s\n\n' % (g_version, asctime())

      return cmd

   def script_results_dir(self):

      # if no results dir, just put everything here
      if self.uvars.is_trivial_dir('results_dir'): return ''

      cmd  = SUBJ.comment_section_string('test, create and enter results dir')\
             + '\n'

      cmd += '# note directory for results\n'           \
             'set results_dir = %s\n\n' % self.uvars.results_dir

      cmd += '# make sure it does not yet exist\n'      \
             'if ( -e $results_dir ) then\n'            \
             '    echo "** results dir \'$results_dir\' already exists"\n'  \
             '    exit\n'                               \
             'endif\n\n'

      cmd += '# create and enter results directory\n'   \
             'mkdir $results_dir\n'                     \
             'cd $results_dir\n\n'

      return cmd

   def script_set_vars(self):
      """use variables for inputs (anat, epi, epi_base) and for
         options (cost_main, cost_list, align_opts)
      """

      U = self.uvars    # for convenience

      # init with a section comment
      cmd = SUBJ.comment_section_string('set processing variables') + '\n'

      # maybe init with top_dir
      if not self.LV.is_trivial_dir('top_dir'):
         cmd += '# top data directory\n' \
                'set top_dir = %s\n\n' % self.LV.top_dir

      # surf_vol and vol_mask might use top_dir
      if self.LV.is_trivial_dir('top_dir'):
         if self.cvars.val('on_surface') != 'yes': self.LV.svol  = U.surf_vol
         self.LV.vmask = U.vol_mask
         self.LV.spec  = U.spec_file
      else:
         self.LV.svol  = '$top_dir/%s' % self.LV.short_names[0][0]
         self.LV.spec  = '$top_dir/%s' % self.LV.short_names[0][1]
         if self.cvars.val('on_surface') != 'yes':
            self.LV.vmask = '$top_dir/%s' % self.LV.short_names[0][2]

      cmd += '# input datasets and surface specification file\n'         \
             '# (absolute paths are used since inputs are not copied)\n' \
             'set surf_vol    = %s\n'   \
             'set spec_file   = %s\n' % (self.LV.svol, self.LV.spec)

      if self.cvars.val('on_surface') != 'yes':
         cmd += 'set vol_mask    = %s\n' % self.LV.vmask

      # as a list, these might come is as strings or floats, be generic
      plist =  [ '%s'%p for p in U.pthr_list ]
      cmd += '\n'                                       \
             '# iterations and blur/clust parameters\n' \
             'set niter       = %d\n'                   \
             'set itersize    = %d\n'                   \
             'set pthr_list   = ( %s )\n\n'             \
             'set blur        = %g\n'                   \
             'set rmm         = %g\n\n'                 \
             % (U.niter, U.itersize, ' '.join(plist), U.blur, U.rmm)

      cmd += '# surface mapping parameters\n'   \
             'set surfA       = %s\n'           \
             'set surfB       = %s\n'           \
             'set map_func    = %s\n'           \
             'set nsteps      = %d\n\n'         \
             % (U.surfA, U.surfB, U.map_func, self.cvars.nsteps)

      if self.cvars.keepblocks > 0:
         cmd += '# note how many blocks to keep output datasets for\n' \
                'set keepblocks  = %d\n\n' % self.cvars.keepblocks

      if self.cvars.time_process:
         cmd += "# prepare to possibly time programs (/usr/bin/time or '')\n" \
                "set time_str    = /usr/bin/time \n\n"
         self.LV.time_str = '$time_str \\\n'
      else: self.LV.time_str = ''

      return cmd

   def get_script(self):
      """return status, message

                status = number of error messages
                if 0: message = command
                else: message = error string

         Requests for warnings must be made separately, since they
         are not fatal.
      """

      if len(self.errors) > 0:
         return 1, SUBJ.make_message_list_string(self.errors, "errors")

      return 0, self.script

   def get_warnings(self):
      """return the number of warnings and a warnings string"""

      return len(self.warnings), \
             SUBJ.make_message_list_string(self.warnings, "warnings")

   def proc_dir_filename(self, vname):
      """file is either fname or proc_dir/fname (if results is set)
         vname : results file variable (must convert to fname)
      """
      fname = self.rvars.val(vname)
      return self.cvars.file_under_dir('proc_dir', fname)

   def nuke_old_results(self):
      """if the results directory exists, remove it"""

      if self.uvars.results_dir == '': return

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if os.path.isdir(self.uvars.results_dir):
         print '-- nuking old results: %s' % self.uvars.results_dir
         os.system('rm -fr %s' % self.uvars.results_dir)

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------


   def copy_orig_proc(self):
      """if the proc script exists, copy to .orig.SCRIPTNAME"""
      if self.rvars.file_proc == '': return
      pfile = self.rvars.file_proc

      # ------------------------- do the work -------------------------
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)
      if os.path.isfile(pfile):
         cmd = 'cp -f %s .orig.%s' % (pfile, pfile)
         if self.cvars.verb > 1: print '++ exec: %s' % cmd
         os.system(cmd)
      elif self.cvars.verb > 1: print "** no proc '%s' to copy" % pfile
      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
      # ------------------------- done -------------------------

   def write_script(self, fname=''):
      """write processing script to a file (in the proc_dir)
         - if fname is set, use it, else generate
         - set rvars.file_proc and output_proc
      """

      if not self.script:
         print '** no alignment script to write out'
         return 1
      if fname: name = fname
      else:
         # if self.svars.sid: name = 'script.align.%s' % self.svars.sid
         name = 'script.align'

      # store (intended) names for calling tool to execute with
      self.rvars.file_proc = name # store which file we have written to
      self.rvars.output_proc = 'output.%s' % name # file for command output

      if self.cvars.verb > 0: print '++ writing script to %s' % name

      # if requested, make an original copy
      self.LV.retdir = SUBJ.goto_proc_dir(self.cvars.proc_dir)

      if self.uvars.copy_scripts == 'yes': # make an orig copy
         UTIL.write_text_to_file('.orig.%s'%name, self.script, exe=1)
      rv = UTIL.write_text_to_file(name, self.script, exe=1)

      self.LV.retdir = SUBJ.ret_from_proc_dir(self.LV.retdir)
         
      return rv


# ===========================================================================
# help strings accessed both from command-line and GUI
# ===========================================================================

helpstr_todo = """
---------------------------------------------------------------------------
                        todo list:  

- include example in class data
- set up regression testing
---------------------------------------------------------------------------
"""

