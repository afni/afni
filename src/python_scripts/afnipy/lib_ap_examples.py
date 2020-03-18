#!/usr/bin/env python

from afnipy import afni_util as UTIL
import copy

# ----------------------------------------------------------------------
# This is a library for storing basic infomation regarding options and
# examples for afni_proc.py.
#
#    - examples are stored as individual dictionaries
#    - examples from afni_proc.py -help are created by:
#       - set noglob
#       - add '-optlist_show_argv_array dict' to command
#
#    - for a given example, want:
#       - name (e.g. Example 11) - want case insensitive checking
#       - source (from help, from AFNI_data6, etc.)
#       - keywords?  too hard to match
#       - description ()
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# main array of APExample instances
ap_examples = []

# ----------------------------------------------------------------------
# class definition for instances in ap_examples array
class APExample:
   def __init__(self, name, olist, source='', descrip='',
                header='', trailer=''):
      self.name     = name          # used to reference example
      self.source   = source        # from AP help, AD6, etc.
      self.descrip  = descrip       # very short description
      self.header   = header        # shown before example (in -help)
      self.trailer  = trailer       # shown after example (in -help)
      # self.keywords = keywords

      self.keys     = []            # convenience: olist[][0] entries
      self.olist    = olist         # list of options [opt, [params]]
      self.aphelp   = (source == 'afni_proc.py -help')

      if not self.valid_olist():
         return None

      self.keys = [o[0] for o in olist]

   def valid_olist(self):
      valid = 1
      for oind, opt in enumerate(self.olist):
         if len(opt) != 2:
            print("** APExample '%s' olist #%d needs opt and list, have:\n" \
                  "   %s" % (self.name, oind, opt))
            valid = 0
            continue
         if type(opt[0]) != str:
            print("** APExample '%s' olist #%d should start with string:\n" \
                  "   %s" % (self.name, oind, opt))
            valid = 0
         if type(opt[1]) != list:
            print("** APExample '%s' olist #%d should have a list:\n" \
                  "   %s" % (self.name, oind, opt))
            valid = 0
      return valid

   def copy(self, quotize=0):
      """return a deep copy
            quotize - if set, quotize any needed option parameters
      """
      cc = copy.deepcopy(self)

      if quotize:
         for entry in cc.olist:
            entry[1] = UTIL.quotize_list(entry[1])

      return cc

   def compare(self, target, eskip=[], verb=1):
      """compare against some target, which can be a 'name' string
         or another APExample instance

         for any key in eskip, do not print all details of element differences
      """
      if isinstance(target, APExample):
         return self.compare_v_instance(target, eskip=eskip, verb=verb)

      if type(target) != str:
         print("** APExample.compare: target is neither instance nor string")
         return 

      # otherwise, try to find an instance in the ap_examples list
      eg = find_eg(target)
      if eg != None:
         return self.compare_v_instance(eg, eskip=eskip, verb=verb)

      print("** comp : failed to find instance for target '%s'" % target)
      return

   def compare_v_instance(self, target, eskip=[], verb=1):
      """compare against another APExample instance

            -show missing, extra and different categories

            target  can be name (string) or APExample instance
            eskip list of keys for which elements should not be compared
            verb    0 - use short list notation
                    1 - show missing, extra and diffs
                    2 - include all parameter lists
      """
      print("="*75)
      print("==== comparing (current) '%s' vs (target) '%s'" \
            % (self.name, target.name))
      if verb == 1:
         print('     (for more detail, consider "-verb 2")')
      print("")
    
      # use more generic name
      source = self

      # get global max key len
      maxs = max([len(key) for key in source.keys])
      maxt = max([len(key) for key in target.keys])
      maxk = max(maxs, maxt)

      # first look for missing and extra
      emiss =  [e for e in target.olist if e[0] not in source.keys]
      eextra = [e for e in source.olist if e[0] not in target.keys]

      # common keys, might be fewer, more, and/or have diffs
      efewer = []   # fewer of keys than target
      emore = []    # more than keys than target
      pdiff = []    # index pairs, where keys are the same but lists differ
      ncommon = 0

      # loop over unique keys, counting number in both key lists
      uniq_target_keys = UTIL.get_unique_sublist(target.keys)

      for key in uniq_target_keys:
         if key not in source.keys: continue

         ksource = [ind for ind, e in enumerate(source.olist) if e[0]==key]
         ktarget = [ind for ind, e in enumerate(target.olist) if e[0]==key]

         # how many of each?  both must be positive
         nsource = len(ksource)
         ntarget = len(ktarget)

         # get any efewer or emore entries
         if nsource < ntarget:
            efewer.extend([target.olist[ind] for ind in range(nsource,ntarget)])
         elif ntarget < nsource:
            eextra.extend([source.olist[ind] for ind in range(ntarget,nsource)])

         # get matching e[0]'s where e[1]'s differ
         ncomp = min(nsource, ntarget)
         ncommon += ncomp
         for ind in range(ncomp):
            if source.olist[ksource[ind]] != target.olist[ktarget[ind]]:
               # just store source and target indices here
               pdiff.append([ksource[ind], ktarget[ind]])

      nindent = 3
      ind1 = ' '*nindent
      ind2 = ' '*(2*nindent)

      # possibly print compact output
      if verb == 0:
         print("== missing (%d): %s\n" % (len(emiss),  [e[0] for e in emiss ]))
         print("== extra   (%d): %s\n" % (len(eextra), [e[0] for e in eextra]))
         print("== fewer   (%d): %s\n" % (len(efewer), [e[0] for e in efewer]))
         print("== more    (%d): %s\n" % (len(emore),  [e[0] for e in emore ]))
         print("== common  (%d - with %d differing)\n" % (ncommon, len(pdiff)))
         print("== diffs   (%d): %s\n" % (len(pdiff),
                                       [source.olist[p[0]][0] for p in pdiff ]))
         return

      # more verbose output allows for full printing
      if verb > 1: lmax = 0
      else:        lmax = 45

      print("==========  missing option(s) : %d" % len(emiss))
      if len(emiss) > 0:
         for e in emiss:
             estr = ' '.join(e[1])
             self._print_opt_lin(ind1, e[0], maxk, estr, lmax=lmax)
      print("")
      
      print("==========  extra option(s) : %d" % len(eextra))
      if len(eextra) > 0:
         for e in eextra:
             estr = ' '.join(e[1])
             self._print_opt_lin(ind1, e[0], maxk, estr, lmax=lmax)
      print("")
      
      print("==========  differing option(s) : %d" % len(pdiff))
      if len(pdiff) > 0:
         skips = []
         for pair in pdiff:
             oname = source.olist[pair[0]][0]
             if oname in eskip and verb < 2:
                # only show skipping details once
                if oname in skips:
                   continue
                skips.append(oname)

                print("%s%-*s  (verb=1; skipping details)" \
                      % (ind1, maxk, oname))
             else:
                # full details
                print("%s%-*s" % (ind1, maxk, oname))

                ksstr = ' '.join(source.olist[pair[0]][1])
                ktstr = ' '.join(target.olist[pair[1]][1])
                self._print_diff_line(ind2, 'current', ksstr, lmax=lmax)
                self._print_diff_line(ind2, 'target',  ktstr, lmax=lmax)
             print("")

      print("==========  fewer applied option(s) : %d" % len(efewer))
      if len(efewer) > 0:
         for e in efewer:
             estr = ' '.join(e[1])
             self._print_opt_lin(ind1, e[0], maxk, estr, lmax=lmax)
      print("")
      
      print("==========  more applied option(s) : %d" % len(emore))
      if len(emore) > 0:
         for e in emore:
             estr = ' '.join(e[1])
             self._print_opt_lin(ind1, e[0], maxk, estr, lmax=lmax)
      print("")
      
      print("")

   def _print_opt_lin(self, indent, oname, maxlen, parstr, lmax=50):
       """if lmax > 0: restrict parstr to given length, plus elipsis
       """
       estr = ''
       kprint = parstr
       if lmax > 0:
          if len(parstr) > lmax:
             kprint = parstr[0:lmax]
             estr = ' ...'

       print("%s%-*s  %s%s" % (indent, maxlen, oname, kprint, estr))

   def _print_diff_line(self, indent, tstr, parstr, lmax=50):
       """if lmax > 0: restrict parstr to given length, plus elipsis
       """
       estr = ''
       kprint = parstr
       if lmax > 0:
          if len(parstr) > lmax:
             kprint = parstr[0:lmax]
             estr = ' ...'

       print("%s%-8s : %s%s" % (indent, tstr, kprint, estr))

   def command_string(self, wrap=0, windent=10):
      """return a string that is an afni_proc.py command
            if wrap: - return indented command
                     - indent by windent
      """
     
      clist = ['%s %s' % (e[0], ' '.join(e[1])) for e in self.olist]
      if wrap:
         return UTIL.list_to_wrapped_command('afni_proc.py', clist,
                                             nindent=14, maxlen=75)
      else:
         clist.insert(0, 'afni_proc.py')
         return ' '.join(clist)

   def display(self, verb=0, sphinx=1):
      """display a single example - use a copy for quoting
         verb: verbosity level
           0: only show example, with no wrap or formatting
           1: include name, source, descrip
           2: full verbosity: as ap -help: include descrip, header, trailer
              - terminate descrip with ~2~, for sphinxificaiton

         ponder indentation
      """
      cc = self.copy(quotize=1)

      # handle verb 0 right away, as the command string will differ
      if verb == 0:
         print("%s" % cc.command_string())
         return

      cmd = cc.command_string(wrap=1)

      indent = ' '*8

      if sphinx: sstr = ' ~2~'
      else:      sstr = ''

      # possibly show the name/description line,
      # (with trailing ~2~ for sphinx help)
      if verb > 0:
         print("%s%s. %s%s" % (indent, cc.name, cc.descrip, sstr))

      # header
      if verb > 1:
         print("%s" % cc.header)
      else:
         print("")

      # print the actual example
      print("%s" % cmd)

      # any trailer
      if verb > 1 and cc.trailer != '': 
         print("%s" % cc.trailer)

      print("")

      del(cc)

 
def populate_examples():
   """only populate the examples array if someone wants it
   """
   global ap_examples

   if len(ap_examples) > 0:
      return

   ap_examples.append( APExample( 'Example 1',
     source='afni_proc.py -help',
     descrip='Minimum use.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              merely shows how simple a command can be)

           Provide datasets and stim files (or stim_times files).  Note that a
           dataset suffix (e.g. HEAD) must be used with wildcards, so that
           datasets are not applied twice.  In this case, a stim_file with many
           columns is given, where the script to changes it to stim_times files.
           """,
     trailer=""" """,
     olist = [
        ['-dsets',                 ['epiRT*.HEAD']],
        ['-regress_stim_files',    ['stims.1D']],
       ],
     ))

   ap_examples.append( APExample('Example 2',
     source='afni_proc.py -help',
     descrip='Very simple.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper)

           Use all defaults, except remove 3 TRs and use basis
           function BLOCK(30,1).  The default basis function is GAM.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['sb23.e2.simple']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-tcat_remove_first_trs', ['3']],
        ['-regress_stim_times',    ['sb23/stim_files/blk_times.*.1D']],
        ['-regress_basis',         ['BLOCK(30,1)']],
       ],
     ))
                                      
   ap_examples.append( APExample('Example 3',
     source='afni_proc.py -help',
     descrip='Formerly a simple class example.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper)

           Copy the anatomy into the results directory, register EPI data to
           the last TR, specify stimulus labels, compute blur estimates, and
           provide GLT options directly to 3dDeconvolve.  The GLTs will be
           ignored after this, as they take up too many lines.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['sb23.blk']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-copy_anat',             ['sb23/sb23_mpra+orig']],
        ['-tcat_remove_first_trs', ['3']],
        ['-volreg_align_to',       ['last']],
        ['-regress_stim_times',    ['sb23/stim_files/blk_times.*.1D']],
        ['-regress_stim_labels',   ['tneg', 'tpos', 'tneu', 'eneg', 'epos',
                                    'eneu', 'fneg', 'fpos', 'fneu']],
        ['-regress_basis',         ['BLOCK(30,1)']],
        ['-regress_opts_3dD',      ['-gltsym', 'SYM: +eneg -fneg',
                '-glt_label', '1', 'eneg_vs_fneg', '-gltsym',
                'SYM: 0.5*fneg 0.5*fpos -1.0*fneu', '-glt_label', '2',
                'face_contrast', '-gltsym',
                'SYM: tpos epos fpos -tneg -eneg -fneg',
                '-glt_label', '3', 'pos_vs_neg']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))
                                      
   ap_examples.append( APExample( 'Example 4',
     source='afni_proc.py -help',
     descrip='Similar to 3, but specify the processing blocks.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper)

           Adding despike and tlrc, and removing tshift.  Note that
           the tlrc block is to run @auto_tlrc on the anat.  Ignore the GLTs.
           """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['sb23.e4.blocks']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-blocks',                ['despike', 'volreg', 'blur', 'mask',
                                    'scale', 'regress', 'tlrc']],
        ['-copy_anat',             ['sb23/sb23_mpra+orig']],
        ['-tcat_remove_first_trs', ['3']],
        ['-regress_stim_times',    ['sb23/stim_files/blk_times.*.1D']],
        ['-regress_stim_labels',   ['tneg', 'tpos', 'tneu', 'eneg', 'epos',
                                    'eneu', 'fneg', 'fpos', 'fneu']],
        ['-regress_basis',         ['BLOCK(30,1)']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))

   ap_examples.append( APExample( 'Example 5a',
     source='afni_proc.py -help',
     descrip='RETROICOR, resting state data.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              just a terribly simple example using ricor)

           Assuming the class data is for resting-state and that we have the
           appropriate slice-based regressors from RetroTS.py, apply the
           despike and ricor processing blocks.  Note that '-do_block' is used
           to add non-default blocks into their default positions.  Here the
           'despike' and 'ricor' processing blocks would come before 'tshift'.

           Remove 3 TRs from the ricor regressors to match the EPI data.  Also,
           since degrees of freedom are not such a worry, regress the motion
           parameters per-run (each run gets a separate set of 6 regressors).

           The regression will use 81 basic regressors (all of "no interest"),
           with 13 retroicor regressors being removed during pre-processing:

                 27 baseline  regressors ( 3 per run * 9 runs)
                 54 motion    regressors ( 6 per run * 9 runs)

           To example #3, add -do_block, -ricor_* and -regress_motion_per_run.
           """,
     trailer="""
           If tshift, blurring and masking are not desired, consider replacing
           the -do_block option with an explicit list of blocks:

                -blocks despike ricor volreg regress
           """,
     olist = [
        ['-subj_id',               ['sb23.e5a.ricor']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-do_block',              ['despike', 'ricor']],
        ['-tcat_remove_first_trs', ['3']],
        ['-ricor_regs_nfirst',     ['3']],
        ['-ricor_regs',            ['sb23/RICOR/r*.slibase.1D']],
        ['-regress_motion_per_run', []],
       ],
     ))

   ap_examples.append( APExample( 'Example 5b',
     source='afni_proc.py -help',
     descrip='RETROICOR, while running a normal regression.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              another overly simple example using ricor)

           Add the ricor regressors to a normal regression-based processing
           stream.  Apply the RETROICOR regressors across runs (so using 13
           concatenated regressors, not 13*9).  Note that concatenation is
           normally done with the motion regressors too.

           To example #3, add -do_block and three -ricor options.
           """,
     trailer="""
           Also consider adding -regress_bandpass.
           """,
     olist = [
        ['-subj_id',               ['sb23.e5b.ricor']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-do_block',              ['despike', 'ricor']],
        ['-copy_anat',             ['sb23/sb23_mpra+orig']],
        ['-tcat_remove_first_trs', ['3']],
        ['-ricor_regs_nfirst',     ['3']],
        ['-ricor_regs',            ['sb23/RICOR/r*.slibase.1D']],
        ['-ricor_regress_method',  ['across-runs']],
        ['-volreg_align_to',       ['last']],
        ['-regress_stim_times',    ['sb23/stim_files/blk_times.*.1D']],
        ['-regress_stim_labels',   ['tneg', 'tpos', 'tneu', 'eneg', 'epos',
                                    'eneu', 'fneg', 'fpos', 'fneu']],
        ['-regress_basis',         ['BLOCK(30,1)']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))

   ap_examples.append( APExample( 'Example 5c',
     source='afni_proc.py -help',
     descrip='RETROICOR: censor and band pass.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper, no BP)

           This is an example of how we might currently suggest analyzing
           resting state data.  If no RICOR regressors exist, see example 9
           (or just remove any ricor options).

           Censoring due to motion has long been considered appropriate in
           BOLD FMRI analysis, but is less common for those doing bandpass
           filtering in RS FMRI because the FFT requires one to either break
           the time axis (evil) or to replace the censored data with something
           probably inappropriate.

           Instead, it is slow (no FFT, but maybe SFT :) but effective to
           regress frequencies within the regression model, where censoring
           is simple.

           Note: band passing in the face of RETROICOR is questionable.  It may
                 be questionable in general.  To skip bandpassing, remove the
                 -regress_bandpass option line.

           Also, align EPI to anat and warp to standard space.
           """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['sb23.e5a.ricor']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-blocks',                ['despike', 'ricor', 'tshift', 'align',
                            'tlrc', 'volreg', 'blur', 'mask', 'regress']],
        ['-copy_anat',             ['sb23/sb23_mpra+orig']],
        ['-tcat_remove_first_trs', ['3']],
        ['-ricor_regs_nfirst',     ['3']],
        ['-ricor_regs',            ['sb23/RICOR/r*.slibase.1D']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-blur_size',             ['6']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_bandpass',      ['0.01', '0.1']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_run_clustsim',  ['no']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))

   ap_examples.append( APExample( 'Example 6',
     source='afni_proc.py -help',
     descrip='A simple task example, based on AFNI_data6.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              meant to be fast, but not complete, e.g. NL warp)
              (              prefer: see Example 6b)

           This example has changed to more closely correspond with the
           the class analysis example, AFNI_data6/FT_analysis/s05.ap.uber.

           The tshift block will interpolate each voxel time series to adjust
           for differing slice times, where the result is more as if each
           entire volume were acquired at the beginning of the TR.

           The 'align' block implies using align_epi_anat.py to align the
           anatomy with the EPI.  Extra options to that specify using lpc+ZZ
           for the cost function (more robust than lpc), and -giant_move (in
           case the anat and EPI start a little far apart).  This block
           computes the anat to EPI transformation matrix, which will be 
           inverted in the volreg block, based on -volreg_align_e2a.

           Also, compute the transformation of the anatomy to MNI space, using
           affine registration (for speed in this simple example) to align to
           the 2009c template.

           In the volreg block, align the EPI to the MIN_OUTLIER volume (a
           low-motion volume, determined based on the data).  Then concatenate
           all EPI transformations, warping the EPI to standard space in one
           step (without multiple resampling operations), combining:

              EPI  ->  EPI base  ->  anat  ->  MNI 2009c template

           The standard space transformation is included by specifying option
           -volreg_tlrc_warp.

           A 4 mm blur is applied, to keep it very light (about 1.5 times the
           voxel size).

           The regression model is based on 2 conditions, each lasting 20 s
           per event, modeled by convolving a 20 s boxcar function with the
           BLOCK basis function, specified as BLOCK(20,1) to make the regressor
           unit height (height 1).

           One extra general linear test (GLT) is included, contrasting the
           visual reliable condition (vis) with auditory reliable (aud).

           Motion regression will be per run (using one set of 6 regressors for
           each run, i.e. 18 regressors in this example).

           The regression includes censoring of large motion (>= 0.3 ~mm
           between successive time points, based on the motion parameters),
           as well as censoring of outlier time points, where at least 5% of
           the brain voxels are computed as outliers.

           The regression model starts as a full time series, for time
           continuity, before censored time points are removed.  The output
           errts will be zero at censored time points (no error there), and so
           the output fit times series (fitts) will match the original data.

           The model fit time series (fitts) will be computed AFTER the linear
           regression, to save RAM on class laptops.

           Create sum_ideal.1D, as the sum of all non-baseline regressors, for
           quality control.

           Estimate the blur in the residual time series.  The resulting 3 ACF
           parameters can be averaged across subjects for cluster correction at
           the group level.

           Skip running the Monte Carlo cluster simulation example (which would
           specify minimum cluster sizes for cluster significance, based on the
           ACF parameters and mask), for speed.

           Once the proc script is created, execute it.
           """,
     trailer="""
         * One could also use ANATICOR with task (e.g. -regress_anaticor_fast)
           in the case of -regress_reml_exec.  3dREMLfit supports voxelwise
           regression, but 3dDeconvolve does not.
             """,
     olist = [
        ['-subj_id',               ['FT.e6']],
        ['-copy_anat',             ['FT/FT_anat+orig']],
        ['-dsets',                 ['FT/FT_epi_r?+orig.HEAD']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'blur', 'mask', 'scale', 'regress']],
        ['-radial_correlate_blocks', ['tcat', 'volreg']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-blur_size',             ['4.0']],
        ['-regress_stim_times',    ['FT/AV1_vis.txt', 'FT/AV2_aud.txt']],
        ['-regress_stim_labels',   ['vis', 'aud']],
        ['-regress_basis',         ['BLOCK(20,1)']],
        ['-regress_opts_3dD',      ['-jobs', '2', '-gltsym', 'SYM: vis -aud',
                                    '-glt_label', '1', 'V-A']],
        ['-regress_motion_per_run',[]],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_compute_fitts', []],
        ['-regress_make_ideal_sum',['sum_ideal.1D']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
        ['-regress_run_clustsim',  ['no']],
        ['-execute',               []],
       ],
     ))

   ap_examples.append( APExample( 'Example 6b',
     source='afni_proc.py -help',
     descrip='A modern task example, with preferable options.',
     header="""
              (recommended?  yes, reasonable for a complete analysis)

           GOOD TO CONSIDER

           This is based on Example 6, but is more complete.
           Example 6 is meant to run quickly, as in an AFNI bootcamp setting.
           Example 6b is meant to process more as we might suggest.

              - apply -check_flip in align_epi_anat.py, to monitor consistency
              - apply non-linear registration to MNI template, using output
                from @SSwarper:
                  o apply skull-stripped anat in -copy_anat
                  o apply original anat as -anat_follower (QC, for comparison)
                  o pass warped anat and transforms via -tlrc_NL_warped_dsets,
                    to apply those already computed transformations
              - use -mask_epi_anat to tighten the EPI mask (for QC),
                intersecting it (full_mask) with the anat mask (mask_anat)
              - use 3dREMLfit for the regression, to account for temporal
                autocorrelation in the noise
                (-regress_3dD_stop, -regress_reml_exec)
              - generate the HTML QC report using the nicer pythonic functions
                (requires matplotlib)
           """,
     trailer="""
           To compare one's own command against this one, consider adding
                -compare_opts 'example 6b'
           to the end of (or anywhere in) the current command, as in:

                afni_proc.py ... my options ...   -compare_opts 'example 6b'
           """,
     olist = [
        ['-subj_id',               ['FT.e6b']],
        ['-copy_anat',             ['Qwarp/anat_warped/anatSS.FT.nii']],
        ['-anat_has_skull',        ['no']],
        ['-anat_follower',         ['anat_w_skull', 'anat', 'FT/FT_anat+orig']],
        ['-dsets',                 ['FT/FT_epi_r?+orig.HEAD']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'blur', 'mask', 'scale', 'regress']],
        ['-radial_correlate_blocks', ['tcat', 'volreg']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move',
                                    '-check_flip']],
        ['-tlrc_base',             ['MNI152_2009_template_SSW.nii.gz']],
        ['-tlrc_NL_warp',          []],
        ['-tlrc_NL_warped_dsets',  ['Qwarp/anat_warped/anatQQ.FT.nii',
                                    'Qwarp/anat_warped/anatQQ.FT.aff12.1D',
                                    'Qwarp/anat_warped/anatQQ.FT_WARP.nii']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-blur_size',             ['4.0']],
        ['-regress_stim_times',    ['FT/AV1_vis.txt', 'FT/AV2_aud.txt']],
        ['-regress_stim_labels',   ['vis', 'aud']],
        ['-regress_basis',         ['BLOCK(20,1)']],
        ['-regress_opts_3dD',      ['-jobs', '2', '-gltsym', 'SYM: vis -aud',
                                    '-glt_label', '1', 'V-A']],
        ['-regress_motion_per_run',[]],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_3dD_stop',      []],
        ['-regress_reml_exec',     []],
        ['-regress_compute_fitts', []],
        ['-regress_make_ideal_sum',['sum_ideal.1D']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
        ['-regress_run_clustsim',  ['no']],
        ['-html_review_style',     ['pythonic']],
        ['-execute',               []],
       ],
     ))

   ap_examples.append( APExample( 'Example 7',
     source='afni_proc.py -help',
     descrip='Apply some esoteric options.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              e.g. NL warp without @SSwarper)
              (              prefer: see Example 6b)

           a. Blur only within the brain, as far as an automask can tell.  So
              add -blur_in_automask to blur only within an automatic mask
              created internally by 3dBlurInMask (akin to 3dAutomask).

           b. Let the basis functions vary.  For some reason, we expect the
              BOLD responses to the telephone classes to vary across the brain.
              So we have decided to use TENT functions there.  Since the TR is
              3.0s and we might expect up to a 45 second BOLD response curve,
              use 'TENT(0,45,16)' for those first 3 out of 9 basis functions.

              This means using -regress_basis_multi instead of -regress_basis,
              and specifying all 9 basis functions appropriately.

           c. Use amplitude modulation.

              We expect responses to email stimuli to vary proportionally with
              the number of punctuation characters used in the message (in
              certain brain regions).  So we will use those values as auxiliary
              parameters 3dDeconvolve by marrying the parameters to the stim
              times (using 1dMarry).

              Use -regress_stim_types to specify that the epos/eneg/eneu stim
              classes should be passed to 3dDeconvolve using -stim_times_AM2.

           d. Not only censor motion, but censor TRs when more than 10% of the
              automasked brain are outliers.  So add -regress_censor_outliers.

           e. Include both de-meaned and derivatives of motion parameters in
              the regression.  So add '-regress_apply_mot_types demean deriv'.

           f. Output baseline parameters so we can see the effect of motion.
              So add -bout under option -regress_opts_3dD.

           g. Save on RAM by computing the fitts only after 3dDeconvolve.
              So add -regress_compute_fitts.

           h. Speed things up.  Have 3dDeconvolve use 4 CPUs and skip the
              single subject 3dClustSim execution.  So add '-jobs 4' to the
              -regress_opts_3dD option and add '-regress_run_clustsim no'.
           """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['sb23.e7.esoteric']],
        ['-dsets',                 ['sb23/epi_r??+orig.HEAD']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'blur', 'mask', 'scale', 'regress']],
        ['-copy_anat',             ['sb23/sb23_mpra+orig']],
        ['-tcat_remove_first_trs', ['3']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-blur_size',             ['4']],
        ['-blur_in_automask',      []],
        ['-regress_stim_times',    ['sb23/stim_files/blk_times.*.1D']],
        ['-regress_stim_types',    ['times', 'times', 'times',
                                    'AM2', 'AM2', 'AM2',
                                    'times', 'times', 'times']],
        ['-regress_stim_labels',   ['tneg', 'tpos', 'tneu',
                                    'eneg', 'epos', 'eneu',
                                    'fneg', 'fpos', 'fneu']],
        ['-regress_basis_multi',['BLOCK(30,1)', 'TENT(0,45,16)','BLOCK(30,1)',
                                 'BLOCK(30,1)', 'TENT(0,45,16)','BLOCK(30,1)',
                                 'BLOCK(30,1)', 'TENT(0,45,16)','BLOCK(30,1)']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_censor_outliers', ['0.1']],
        ['-regress_compute_fitts', []],
        ['-regress_opts_3dD',      ['-bout', '-gltsym', 'SYM: +eneg -fneg',
                            '-glt_label', '1', 'eneg_vs_fneg', '-jobs', '4']],
        ['-regress_run_clustsim',  ['no']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))

   ap_examples.append( APExample('Example 8',
     source='afni_proc.py -help',
     descrip='Surface-based analysis.',
     header="""
              (recommended?  yes, reasonable for a complete analysis)

           This example is intended to be run from AFNI_data6/FT_analysis.
           It is provided with the class data in file s03.ap.surface.

           Add -surf_spec and -surf_anat to provide the required spec and
           surface volume datasets.  The surface volume will be aligned to
           the current anatomy in the processing script.  Two spec files
           (lh and rh) are provided, one for each hemisphere (via wildcard).

           Also, specify a (resulting) 6 mm FWHM blur via -blur_size.  This
           does not add a blur, but specifies a resulting blur level.  So
           6 mm can be given directly for correction for multiple comparisons
           on the surface.

           Censor per-TR motion above 0.3 mm.

           Note that no -regress_est_blur_errts option is given, since that
           applies to the volume only (and since the 6 mm blur is a resulting
           blur level, so the estimates are not needed).

           The -blocks option is provided, but it is the same as the default
           for surface-based analysis, so is not really needed here.  Note that
           the 'surf' block is added and the 'mask' block is removed from the
           volume-based defaults.

           important options:

                -blocks         : includes surf, but no mask
                                  (default blocks for surf, so not needed)
                -surf_anat      : volume aligned with surface
                -surf_spec      : spec file(s) for surface

           Note: one would probably want to use standard mesh surfaces here.
                 This example will be updated with them in the future.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.surf']],
        ['-blocks',                ['tshift', 'align', 'volreg', 'surf',
                                    'blur', 'scale', 'regress']],
        ['-copy_anat',             ['FT/FT_anat+orig']],
        ['-dsets',                 ['FT/FT_epi_r?+orig.HEAD']],
        ['-surf_anat',             ['FT/SUMA/FTmb_SurfVol+orig']],
        ['-surf_spec',             ['FT/SUMA/FTmb_?h.spec']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-blur_size',             ['6']],
        ['-regress_stim_times',    ['FT/AV1_vis.txt', 'FT/AV2_aud.txt']],
        ['-regress_stim_labels',   ['vis', 'aud']],
        ['-regress_basis',         ['BLOCK(20,1)']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_opts_3dD',      ['-jobs', '2', '-gltsym', 'SYM: vis -aud',
                                    '-glt_label', '1', 'V-A']],
       ],
     ))
                                      
   ap_examples.append( APExample('Example 9',
     source='afni_proc.py -help',
     descrip='Resting state analysis with censoring and band passing.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              e.g. has band pass, no @SSwarper)
              (              prefer: see Example 11)

           With censoring and bandpass filtering.

           This is our suggested way to do pre-processing for resting state
           analysis, under the assumption that no cardio/physio recordings
           were made (see example 5 for cardio files).

           Censoring due to motion has long been considered appropriate in
           BOLD FMRI analysis, but is less common for those doing bandpass
           filtering in RS FMRI because the FFT requires one to either break
           the time axis (evil) or to replace the censored data with something
           probably inappropriate.

           Instead, it is slow (no FFT, but maybe SFT :) but effective to
           regress frequencies within the regression model, where censoring
           is simple.

           inputs: anat, EPI
           output: errts dataset (to be used for correlation)

           special processing:
              - despike, as another way to reduce motion effect
                 (see block despike)
              - censor motion TRs at the same time as bandpassing data
                 (see -regress_censor_motion, -regress_bandpass)
              - regress motion parameters AND derivatives
                 (see -regress_apply_mot_types)

           Note: for resting state data, a more strict threshold may be a good
                 idea, since motion artifacts should play a bigger role than in
                 a task-based analysis.

                 So the typical suggestion of motion censoring at 0.3 for task
                 based analysis has been changed to 0.2 for this resting state
                 example, and censoring of outliers has also been added, at a
                 value of 5% of the brain mask.

                 Outliers are typically due to motion, and may capture motion
                 in some cases where the motion parameters do not, because
                 motion is not generally a whole-brain-between-TRs event.

           Note: if regressing out regions of interest, either create the ROI
                 time series before the blur step, or remove blur from the list
                 of blocks (and apply any desired blur after the regression).

           Note: it might be reasonable to estimate the blur using epits rather
                 than errts in the case of bandpassing.  Both options are
                 included here.

           Note: scaling is optional here.  While scaling has no direct effect
                 on voxel correlations, it does have an effect on ROI averages
                 used for correlations.

           Other options to consider: -tlrc_NL_warp, -anat_uniform_method
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['subj123']],
        ['-dsets',                 ['epi_run1+orig.HEAD']],
        ['-copy_anat',             ['anat+orig']],
        ['-blocks',                ['despike', 'tshift', 'align', 'tlrc',
                          'volreg', 'blur', 'mask', 'scale', 'regress']],
        ['-tcat_remove_first_trs', ['3']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-blur_size',             ['4']],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_bandpass',      ['0.01', '0.1']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))

   ap_examples.append( APExample('Example 9b',
     source='afni_proc.py -help',
     descrip='Resting state analysis with ANATICOR.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              e.g. has band pass, no @SSwarper)
              (              prefer: see Example 11)

           Like example #9, but also regress out the signal from locally
           averaged white matter.  The only change is adding the option
           -regress_anaticor.

           Note that -regress_anaticor implies options -mask_segment_anat and
           -mask_segment_erode.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['subj123']],
        ['-dsets',                 ['epi_run1+orig.HEAD']],
        ['-copy_anat',             ['anat+orig']],
        ['-blocks',                ['despike', 'tshift', 'align', 'tlrc',
                          'volreg', 'blur', 'mask', 'scale', 'regress']],
        ['-tcat_remove_first_trs', ['3']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-blur_size',             ['4']],
        ['-regress_anaticor',      []],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_bandpass',      ['0.01', '0.1']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))
                                      
   ap_examples.append( APExample('Example 10',
     source='afni_proc.py -help',
     descrip='Resting state analysis, with tissue-based regressors.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              e.g. missing @SSwarper)
              (              prefer: see Example 11)

           Like example #9, but also regress the eroded white matter averages.
           The WMe mask come from the Classes dataset, created by 3dSeg via the
           -mask_segment_anat and -mask_segment_erode options.

        ** While -mask_segment_anat also creates a CSF mask, that mask is ALL
           CSF, not just restricted to the ventricles, for example.  So it is
           probably not appropriate for use in tissue-based regression.

           CSFe was previously used as an example of what one could do, but as
           it is not advised, it has been removed.

           Also, align to minimum outlier volume, and align to the anatomy
           using cost function lpc+ZZ.

           Note: it might be reasonable to estimate the blur using epits rather
                 than errts in the case of bandpassing.  Both options are
                 included here.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['subj123']],
        ['-dsets',                 ['epi_run1+orig.HEAD']],
        ['-copy_anat',             ['anat+orig']],
        ['-blocks',                ['despike', 'tshift', 'align', 'tlrc',
                                'volreg', 'blur', 'mask', 'scale', 'regress']],
        ['-tcat_remove_first_trs', ['3']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-blur_size',             ['4']],
        ['-mask_epi_anat',         ['yes']],
        ['-mask_segment_anat',     ['yes']],
        ['-mask_segment_erode',    ['yes']],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_bandpass',      ['0.01', '0.1']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_ROI',           ['WMe']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
       ],
     ))
                                      
   ap_examples.append( APExample('Example 10b',
     source='afni_proc.py -help',
     descrip='Resting state analysis, as 10a with 3dRSFC.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              prefer: see Example 11)

            This is for band passing and computation of ALFF, etc.

          * This will soon use a modified 3dRSFC.

            Like example #10, but add -regress_RSFC to bandpass via 3dRSFC.
            Skip censoring and regression band passing because of the bandpass
            operation in 3dRSFC.

            To correspond to common tractography, this example stays in orig
            space (no 'tlrc' block, no -volreg_tlrc_warp option).  Of course,
            going to standard space is an option.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['subj123']],
        ['-dsets',                 ['epi_run1+orig.HEAD']],
        ['-copy_anat',             ['anat+orig']],
        ['-blocks',                ['despike', 'tshift', 'align', 'volreg',
                                    'blur', 'mask', 'scale', 'regress']],
        ['-tcat_remove_first_trs', ['3']],
        ['-volreg_align_e2a',      []],
        ['-blur_size',             ['6.0']],
        ['-mask_apply',            ['epi']],
        ['-mask_segment_anat',     ['yes']],
        ['-mask_segment_erode',    ['yes']],
        ['-regress_bandpass',      ['0.01', '0.1']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_ROI',           ['WMe']],
        ['-regress_RSFC',          []],
        ['-regress_run_clustsim',  ['no']],
        ['-regress_est_blur_errts', []],
       ],
     ))
                                      
   ap_examples.append( APExample( 'Example 11',
     source='afni_proc.py -help',
     descrip='Resting state analysis (now even more modern :).',
     header="""
              (recommended?  yes, reasonable for a complete analysis)

         o Yes, censor (outliers and motion) and despike.
         o Align the anatomy and EPI using the lpc+ZZ cost function, rather
           than the default lpc one.  Apply -giant_move, in case the datasets
           do not start off well-aligned.  Include -check_flip for good measure.
         o Register EPI volumes to the one which has the minimum outlier
              fraction (so hopefully the least motion).
         o Use non-linear registration to MNI template (non-linear 2009c).
           * NOTE: prepare for FreeSurfer before running @SSwarper, so that the
                   FS output will stay aligned with the input.
           * This adds a lot of processing time.
           * Let @SSwarper align to template MNI152_2009_template_SSW.nii.gz.
             Then use the resulting datasets in the afni_proc.py command below
             via -tlrc_NL_warped_dsets.
                  @SSwarper -input FT_anat_FSprep.nii  \\
                            -subid FT                  \\
                            -odir  FT_anat_warped      \\
                            -base  MNI152_2009_template_SSW.nii.gz

            - The SS (skull-stripped) can be given via -copy_anat, and the 
              with-skull unifized anatU can be given as a follower.
         o No bandpassing.
         o Use fast ANATICOR method (slightly different from default ANATICOR).
         o Use FreeSurfer segmentation for:
             - regression of first 3 principal components of lateral ventricles
             - ANATICOR white matter mask (for local white matter regression)
           * For details on how these masks were created, see "FREESURFER NOTE"
             in the help, as it refers to this "Example 11".
         o Input anat was prepared for and given to FreeSurfer, so it should be
           aligned with the FS results and masks.
             - output from FS is usually not quite aligned with input
             - run "3dZeropad -pad2evens" and 3dAllineate before FreeSurfer
             - check anat input to FS using check_dset_for_fs.py
         o Erode FS white matter and ventricle masks before application.
         o Bring along FreeSurfer parcellation datasets:
             - aaseg : NN interpolated onto the anatomical grid
             - aeseg : NN interpolated onto the EPI        grid
           * These 'aseg' follower datasets are just for visualization,
             they are not actually required for the analysis.
         o Compute average correlation volumes of the errts against the
           the gray matter (aeseg) and ventricle (FSVent) masks.

           Note: it might be reasonable to use either set of blur estimates
                 here (from epits or errts).  The epits (uncleaned) dataset
                 has all of the noise (though what should be considered noise
                 in this context is not clear), while the errts is motion
                 censored.  For consistency in resting state, it would be
                 reasonable to stick with epits.  They will likely be almost
                 identical.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.11.rest']],
        ['-blocks',                ['despike', 'tshift', 'align', 'tlrc',
                          'volreg', 'blur', 'mask', 'scale', 'regress']],
        ['-radial_correlate_blocks', ['tcat', 'volreg']],
        ['-copy_anat',             ['anatSS.FT.nii']],
        ['-anat_has_skull',        ['no']],
        ['-anat_follower',         ['anat_w_skull', 'anat', 'anatU.FT.nii']],
        ['-anat_follower_ROI',     ['aaseg', 'anat', 'aparc.a2009s+aseg.nii']],
        ['-anat_follower_ROI',     ['aeseg', 'epi', 'aparc.a2009s+aseg.nii']],
        ['-anat_follower_ROI',     ['FSvent', 'epi', 'fs_ap_latvent.nii.gz']],
        ['-anat_follower_ROI',     ['FSWe', 'epi', 'fs_ap_wm.nii.gz']],
        ['-anat_follower_erode',   ['FSvent', 'FSWe']],
        ['-dsets',                 ['FT_epi_r?+orig.HEAD']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move',
                                    '-check_flip']],
        ['-tlrc_base',             ['MNI152_2009_template_SSW.nii.gz']],
        ['-tlrc_NL_warp',          []],
        ['-tlrc_NL_warped_dsets',  ['anatQQ.FT.nii', 'anatQQ.FT.aff12.1D',
                                    'anatQQ.FT_WARP.nii']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-blur_size',             ['4']],
        ['-mask_epi_anat',         ['yes']],
        ['-regress_motion_per_run', []],
        ['-regress_ROI_PC',        ['FSvent', '3']],
        ['-regress_ROI_PC_per_run', ['FSvent']],
        ['-regress_make_corr_vols', ['aeseg', 'FSvent']],
        ['-regress_anaticor_fast', []],
        ['-regress_anaticor_label', ['FSWe']],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
        ['-html_review_style',     ['pythonic']],
       ],
     ))

   ap_examples.append( APExample('Example 11b',
     source='afni_proc.py -help',
     descrip='Similar to 11, but without FreeSurfer.',
     header="""
              (recommended?  yes, reasonable for a complete analysis)
              (              if this ventricle extraction method seems okay)

         AFNI currently does not have a good program to extract ventricles.
         But it can make a CSF mask that includes them.  So without FreeSurfer,
         one could import a ventricle mask from the template (e.g. for TT space,
         using TT_desai_dd_mpm+tlrc).  For example, assuming Talairach space
         (and a 2.5 mm^3 final voxel grid) for the analysis, one could create a
         ventricle mask as follows:

                3dcalc -a ~/abin/TT_desai_dd_mpm+tlrc                       \\
                       -expr 'amongst(a,152,170)' -prefix template_ventricle
                3dresample -dxyz 2.5 2.5 2.5 -inset template_ventricle+tlrc \\
                       -prefix template_ventricle_2.5mm

         o Be explicit with 2.5mm, using '-volreg_warp_dxyz 2.5'.
         o Use template TT_N27+tlrc, to be aligned with the desai atlas.
         o No -anat_follower options, but use -mask_import to import the
           template_ventricle_2.5mm dataset (and call it Tvent).
         o Use -mask_intersect to intersect ventricle mask with the subject's
           CSFe mask, making a more reliable subject ventricle mask (Svent).
         o Ventricle principle components are created as per-run regressors.
         o Make WMe and Svent correlation volumes, which are just for
           entertainment purposes anyway.
         o Run the cluster simulation.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.11b.rest']],
        ['-blocks',                ['despike', 'tshift', 'align', 'tlrc',
                          'volreg', 'blur', 'mask', 'scale', 'regress']],
        ['-copy_anat',             ['FT_anat+orig']],
        ['-dsets',                 ['FT_epi_r?+orig.HEAD']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ']],
        ['-tlrc_base',             ['TT_N27+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-volreg_warp_dxyz',      ['2.5']],
        ['-blur_size',             ['4']],
        ['-mask_segment_anat',     ['yes']],
        ['-mask_segment_erode',    ['yes']],
        ['-mask_import',           ['Tvent', 'template_ventricle_2.5mm+tlrc']],
        ['-mask_intersect',        ['Svent', 'CSFe', 'Tvent']],
        ['-mask_epi_anat',         ['yes']],
        ['-regress_motion_per_run', []],
        ['-regress_ROI_PC',        ['Svent', '3']],
        ['-regress_ROI_PC_per_run', ['Svent']],
        ['-regress_make_corr_vols', ['WMe', 'Svent']],
        ['-regress_anaticor_fast', []],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
        ['-regress_run_clustsim',  ['yes']],
       ],
     ))

   ap_examples.append( APExample('Example 12',
     source='afni_proc.py -help',
     descrip='background: Multi-echo data processing.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              incomplete - just shows basic ME options)
              (              prefer: see Example 13)

         Processing multi-echo data should be similar to single echo data,
         except for perhaps:

            combine         : the addition of a 'combine' block
            -dsets_me_echo  : specify ME data, per echo
            -dsets_me_run   : specify ME data, per run (alternative to _echo)
            -echo_times     : specify echo times (if needed)
            -combine_method : specify method to combine echoes (if any)

         An afni_proc.py command might be updated to include something like:
            """,
     trailer=""" """,
     olist = [
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                            'mask', 'combine', 'blur', 'scale', 'regress']],
        ['-dsets_me_echo',         ['epi_run*_echo_01.nii']],
        ['-dsets_me_echo',         ['epi_run*_echo_02.nii']],
        ['-dsets_me_echo',         ['epi_run*_echo_03.nii']],
        ['-echo_times',            ['15', '30.5', '41']],
        ['-mask_epi_anat',         ['yes']],
        ['-combine_method',        ['OC']],
       ],
     ))

   ap_examples.append( APExample('Example 12a',
     source='afni_proc.py -help',
     descrip='Multi-echo data processing - very simple.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper)
              (              prefer: see Example 13)

         Keep it simple and just focus on the basic ME options, plus a few
         for controlling registration.

         o This example uses 3 echoes of data across just 1 run.
            - so use a single -dsets_me_run option to input EPI datasets
         o Echo 2 is used to drive registration for all echoes.
            - That is the default, but it is good to be explicit.
         o The echo times are not needed, as the echoes are never combined.
         o The echo are never combined (in this example), so that there
           are always 3 echoes, even until the end.
            - Note that the 'regress' block is not valid for multiple echoes.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.12a.ME']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'mask', 'blur']],
        ['-copy_anat',             ['FT_anat+orig']],
        ['-dsets_me_run',          ['epi_run1_echo*.nii']],
        ['-reg_echo',              ['2']],
        ['-tcat_remove_first_trs', ['2']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
       ],
     ))

   ap_examples.append( APExample('Example 12b',
     source='afni_proc.py -help',
     descrip='Multi-echo data processing - OC resting state.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper)
              (              prefer: see Example 13)

         Still keep this simple, mostly focusing on ME options, plus standard
         ones for resting state.

         o This example uses 3 echoes of data across just 1 run.
            - so use a single -dsets_me_run option to input EPI datasets
         o Echo 2 is used to drive registration for all echoes.
            - That is the default, but it is good to be explicit.
         o The echoes are combined via the 'combine' block.
         o So -echo_times is used to provided them.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.12a.ME']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                            'mask', 'combine', 'blur', 'scale', 'regress']],
        ['-copy_anat',             ['FT_anat+orig']],
        ['-dsets_me_run',          ['epi_run1_echo*.nii']],
        ['-echo_times',            ['15', '30.5', '41']],
        ['-reg_echo',              ['2']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-combine_method',        ['OC']],
        ['-blur_size',             ['4']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_est_blur_epits', []],
       ]
     ))
                                      
   ap_examples.append( APExample('Example 12c',
     source='afni_proc.py -help',
     descrip='Multi-echo data processing - ME-ICA resting state.',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              many missing preferences, e.g. @SSwarper)
              (              prefer: see Example 13)

         As above, but run tedana.py for MEICA denoising.

         o Since tedana.py will mask the data, it may be preferable to
           blur only within that mask (-blur_in_mask yes).
         o A task analysis using tedana might look much the same,
           but with the extra -regress options for the tasks.
            """,
     trailer="""
         Consider an alternative combine method, 'tedana_OC_tedort'.
           """,
     olist = [
        ['-subj_id',               ['FT.12a.ME']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                            'mask', 'combine', 'blur', 'scale', 'regress']],
        ['-copy_anat',             ['FT_anat+orig']],
        ['-dsets_me_run',          ['epi_run1_echo*.nii']],
        ['-echo_times',            ['15', '30.5', '41']],
        ['-reg_echo',              ['2']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ']],
        ['-tlrc_base',             ['MNI152_T1_2009c+tlrc']],
        ['-tlrc_NL_warp',          []],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-combine_method',        ['tedana']],
        ['-blur_size',             ['4']],
        ['-blur_in_mask',          ['yes']],
        ['-regress_motion_per_run',[]],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-regress_est_blur_epits',[]],
       ],
     ))
                                      
   ap_examples.append( APExample( 'Example 13',
     source='afni_proc.py -help',
     descrip='Complicated ME, surface-based resting state example.',
     header="""
              (recommended?  yes, reasonable for a complete analysis)

         Key aspects of this example:

            - multi-echo data, using "optimally combined" echoes
            - resting state analysis (without band passing)
            - surface analysis
            - blip up/blip down distortion correction
            - slice-wise regression of physiological parameters (RETROICOR)
            - ventricle principal component regression (3 PCs)
            - EPI volreg to per-run MIN_OUTLIER, with across-runs allineate
            - QC: @radial_correlate on tcat and volreg block results
            - QC: pythonic html report

            * since this is a surface-based example, the are no tlrc options

         Minor aspects:

            - a FWHM=6mm blur is applied, since blur on surface is TO is size

         Note: lacking good sample data for this example, it is simply faked
               for demonstration (echoes are identical, fake ricor parameters
               are not part of this data tree).
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.complicated']],
        ['-blocks',                ['despike', 'ricor', 'tshift', 'align',
                                    'volreg', 'mask', 'combine', 'surf',
                                    'blur', 'scale', 'regress']],
        ['-radial_correlate_blocks', ['tcat', 'volreg']],
        ['-blip_forward_dset',     ['FT/FT_epi_r1+orig.HEAD[0]']],
        ['-blip_reverse_dset',     ['FT/FT_epi_r1+orig.HEAD[0]']],
        ['-copy_anat',             ['FT/FT_anat+orig']],
        ['-anat_follower_ROI',     ['FSvent', 'epi', 'FT/SUMA/FT_vent.nii']],
        ['-anat_follower_erode',   ['FSvent']],
        ['-regress_ROI_PC',        ['FSvent', '3']],
        ['-regress_ROI_PC_per_run', ['FSvent']],
        ['-regress_make_corr_vols', ['FSvent']],
        ['-dsets_me_echo',         ['FT/FT_epi_r?+orig.HEAD']],
        ['-dsets_me_echo',         ['FT/FT_epi_r?+orig.HEAD']],
        ['-dsets_me_echo',         ['FT/FT_epi_r?+orig.HEAD']],
        ['-echo_times',            ['11', '22.72', '34.44']],
        ['-combine_method',        ['OC']],
        ['-tcat_remove_first_trs', ['2']],
        ['-tshift_interp',         ['-wsinc9']],
        ['-mask_epi_anat',         ['yes']],
        ['-ricor_regs_nfirst',     ['2']],
        ['-ricor_regs',            ['FT/fake.slibase.FT.r?.1D']],
        ['-ricor_regress_method',  ['per-run']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_post_vr_allin',  ['yes']],
        ['-volreg_pvra_base_index', ['MIN_OUTLIER']],
        ['-volreg_warp_final_interp', ['wsinc5']],
        ['-surf_anat',             ['FT/SUMA/FT_SurfVol.nii']],
        ['-surf_spec',             ['FT/SUMA/std.141.FT_?h.spec']],
        ['-blur_size',             ['6']],
        ['-regress_censor_motion', ['0.2']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_motion_per_run', []],
        ['-regress_apply_mot_types', ['demean', 'deriv']],
        ['-html_review_style',     ['pythonic']],
       ]
     ))

   ap_examples.append( APExample('s03.ap.surface',
     source='FT_analysis',
     descrip='class demo - basic surface analysis',
     header="""
              (recommended?  yes, reasonable for a complete analysis)
                             (though it is a very simple example)

           This is the surface analysis run during an AFNI bootcamp.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT.surf']],
        ['-blocks',                ['tshift', 'align', 'volreg', 'surf',
                                    'blur', 'scale', 'regress']],
        ['-copy_anat',             ['FT/FT_anat+orig']],
        ['-dsets',                 ['FT/FT_epi_r?+orig.HEAD']],
        ['-surf_anat',             ['FT/SUMA/FT_SurfVol.nii']],
        ['-surf_spec',             ['FT/SUMA/std.60.FT_?h.spec']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move',
                                    '-check_flip']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-blur_size',             ['6']],
        ['-regress_stim_times',    ['FT/AV1_vis.txt', 'FT/AV2_aud.txt']],
        ['-regress_stim_labels',   ['vis', 'aud']],
        ['-regress_basis',         ['BLOCK(20,1)']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_opts_3dD',      ['-jobs', '2', '-gltsym', 'SYM: vis -aud',
                                    '-glt_label', '1', 'V-A']],
       ]
     ))

   ap_examples.append( APExample('s05.ap.uber',
     source='FT_analysis',
     descrip='class demo - basic task analysis',
     header="""
              (recommended?  no, not intended for a complete analysis)
              (              prefer: see Example 6b)

           A basic task analysis with a pair of visual and auditory tasks.

           notable options include :
                - affine registration to the (default) TT_N27+tlrc template
                - censoring based on both motion params and outliers
                - '-regress_compute_fitts' to reduce RAM needs in 3dD
                - mask_epi_anat - intersect full_mask (epi) with mask_anat
                - QC: computing radial correlation volumes at the end
                      of the tcat (initial) and volreg processing blocks
                - QC: include -check_flip left/right consistency check
                - QC: compute sum of ideals, for evaluation
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['FT']],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'blur', 'mask', 'scale', 'regress']],
        ['-radial_correlate_blocks', ['tcat', 'volreg']],
        ['-copy_anat',             ['FT/FT_anat+orig']],
        ['-dsets',                 ['FT/FT_epi_r1+orig.HEAD',
                                    'FT/FT_epi_r2+orig.HEAD',
                                    'FT/FT_epi_r3+orig.HEAD']],
        ['-tcat_remove_first_trs', ['2']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move',
                                    '-check_flip']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-blur_size',             ['4.0']],
        ['-mask_epi_anat',         ['yes']],
        ['-regress_stim_times',    ['FT/AV1_vis.txt', 'FT/AV2_aud.txt']],
        ['-regress_stim_labels',   ['vis', 'aud']],
        ['-regress_basis',         ['BLOCK(20,1)']],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_motion_per_run', []],
        ['-regress_opts_3dD',      ['-jobs', '2', '-gltsym', 'SYM: vis -aud',
                                    '-glt_label', '1', 'V-A',
                                    '-gltsym', 'SYM: 0.5*vis +0.5*aud',
                                    '-glt_label', '2', 'mean.VA']],
        ['-regress_compute_fitts', []],
        ['-regress_make_ideal_sum', ['sum_ideal.1D']],
        ['-regress_est_blur_epits', []],
        ['-regress_est_blur_errts', []],
        ['-regress_run_clustsim',  ['no']],
        ['-execute',               []],
       ]
     ))

   ap_examples.append( APExample('NARPS',
     source='eventually mention paper reference?',
     descrip='Applied NARPS example from AFNI.',
     header="""
              (recommended?  yes, reasonable for a complete analysis)

           An amplitude modulation task analysis.
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['sid']],
        ['-script',                ['proc.sid']],
        ['-scr_overwrite',         []],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'mask', 'blur', 'scale', 'regress']],
        ['-copy_anat',             ['anatSS.sid.nii']],
        ['-anat_has_skull',        ['no']],
        ['-anat_follower',         ['anat_w_skull', 'anat', 'anatU.sid.nii']],
        ['-anat_follower_ROI',     ['FS_wm_e', 'epi',
                                    'SUMA/mask.aseg.wm.e1.nii.gz']],
        ['-anat_follower_ROI',     ['FS_REN_epi', 'epi',
                                    'SUMA/aparc+aseg_REN_all.nii.gz']],
        ['-anat_follower_ROI',     ['FS_REN_anat', 'anat',
                                    'SUMA/aparc+aseg_REN_all.nii.gz']],
        ['-anat_follower_erode',   ['FS_wm_e']],
        ['-dsets',                 ['func/sid_task-MGT_run-01_bold.nii.gz',
                                    'func/sid_task-MGT_run-02_bold.nii.gz',
                                    'func/sid_task-MGT_run-03_bold.nii.gz',
                                    'func/sid_task-MGT_run-04_bold.nii.gz']],
        ['-tcat_remove_first_trs', ['0']],
        ['-tshift_opts_ts',        ['-tpattern', 'alt+z2']],
        ['-radial_correlate',      ['yes']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move',
                                    '-check_flip']],
        ['-tlrc_base',             ['MNI152_2009_template_SSW.nii.gz']],
        ['-tlrc_NL_warp',          []],
        ['-tlrc_NL_warped_dsets',  ['anatQQ.sid.nii', 'anatQQ.sid.aff12.1D',
                                    'anatQQ.sid_WARP.nii']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-blur_size',             ['5']],
        ['-test_stim_files',       ['no']],
        ['-regress_stim_times',    ['timing/times.Resp.txt',
                                    'timing/times.NoResp.txt']],
        ['-regress_stim_labels',   ['Resp', 'NoResp']],
        ['-regress_stim_types',    ['AM2', 'AM1']],
        ['-regress_basis_multi',   ['dmBLOCK']],
        ['-regress_anaticor_fast', []],
        ['-regress_anaticor_fwhm', ['20']],
        ['-regress_anaticor_label', ['FS_wm_e']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_compute_fitts', []],
        ['-regress_opts_3dD',      ['-jobs', '8',
                        '-gltsym', 'SYM: Resp[1] -Resp[2]',
                        '-glt_label', '1', 'gain-loss', '-GOFORIT', '10']],
        ['-regress_opts_reml',     ['-GOFORIT']],
        ['-regress_3dD_stop',      []],
        ['-regress_reml_exec',     []],
        ['-regress_make_ideal_sum', ['sum_ideal.1D']],
        ['-regress_make_corr_vols', ['FS_wm_e']],
        ['-regress_est_blur_errts', []],
        ['-regress_run_clustsim',  ['no']],
        ['-html_review_style',     ['pythonic']],
       ],
     ))
                                      
   ap_examples.append( APExample('pamenc',
     source='AFNI_demos',
     descrip='ds000030.v16 parametric encoding task analysis.',
     header="""
              (recommended?  yes, reasonable for a complete analysis)

           original analysis was from:
               Gorgolewski KJ, Durnez J and Poldrack RA.
               Preprocessed Consortium for Neuropsychiatric Phenomics dataset.
               F1000Research 2017, 6:1262
               https://doi.org/10.12688/f1000research.11964.2

           downloadable from https://legacy.openfmri.org/dataset/ds000030
            """,
     trailer=""" """,
     olist = [
        ['-subj_id',               ['SID']],
        ['-script',                ['proc.SID']],
        ['-scr_overwrite',         []],
        ['-blocks',                ['tshift', 'align', 'tlrc', 'volreg',
                                    'mask', 'blur', 'scale', 'regress']],
        ['-copy_anat',             ['anatSS.SID.nii']],
        ['-anat_has_skull',        ['no']],
        ['-anat_follower',         ['anat_w_skull', 'anat', 'anatU.SID.nii']],
        ['-dsets',                 ['func/SID_task-pamenc_bold.nii.gz']],
        ['-tcat_remove_first_trs', ['0']],
        ['-tshift_opts_ts',        ['-tpattern', 'alt+z2']],
        ['-radial_correlate',      ['yes']],
        ['-align_opts_aea',        ['-cost', 'lpc+ZZ', '-giant_move',
                                    '-check_flip']],
        ['-tlrc_base',             ['MNI152_2009_template_SSW.nii.gz']],
        ['-tlrc_NL_warp',          []],
        ['-tlrc_NL_warped_dsets',  ['anatQQ.SID.nii', 'anatQQ.SID.aff12.1D',
                                    'anatQQ.SID_WARP.nii']],
        ['-volreg_align_to',       ['MIN_OUTLIER']],
        ['-volreg_align_e2a',      []],
        ['-volreg_tlrc_warp',      []],
        ['-mask_epi_anat',         ['yes']],
        ['-blur_size',             ['6']],
        ['-blur_in_mask',          ['yes']],
        ['-regress_stim_times',    ['timing/times.CONTROL.txt',
                                    'timing/times.TASK.txt']],
        ['-regress_stim_labels',   ['CONTROL', 'TASK']],
        ['-regress_stim_types',    ['AM1']],
        ['-regress_basis_multi',   ['dmBLOCK']],
        ['-regress_motion_per_run', []],
        ['-regress_censor_motion', ['0.3']],
        ['-regress_censor_outliers', ['0.05']],
        ['-regress_compute_fitts', []],
        ['-regress_fout',          ['no']],
        ['-regress_opts_3dD',      ['-jobs', '8']],
        ['-regress_3dD_stop',      []],
        ['-regress_reml_exec',     []],
        ['-regress_make_ideal_sum', ['sum_ideal.1D']],
        ['-regress_est_blur_errts', []],
        ['-regress_run_clustsim',  ['no']],
        ['-html_review_style',     ['pythonic']],
       ],
     ))
                                      
   return

def find_eg(name):
   """try to find a matching ap_examples instance

      consider things like:
        if failure, and if 'name' is substring of only 1 example, return it

      return None on failure
   """
   global ap_examples
   populate_examples()

   for eg in ap_examples:
      if eg.name.lower() == name.lower():
         return eg

   return None

def show_enames(verb=1):
   """list all ap_example names
   """
   global ap_examples
   populate_examples()

   nlist = [eg.name for eg in ap_examples]

   # basic: show list
   if verb == 0:
      print("%s" % nlist)
      return

   # nicer: show pretty list
   if verb == 1:
      istr = ' '*3
      jstr = '\n%s' % istr
      print("%s%s\n" % (istr, jstr.join(nlist)))
      return

   maxn = max([len(name) for name in nlist])
   indent = ' '*4
   for eg in ap_examples:
      print("%s%-*s : %s" % (indent, maxn, eg.name, eg.descrip))

def compare_eg_pair(eg1, eg2, eskip=[], verb=1):
   """similar to compare(), above, but compare 2 known examples"""

   # set names and get an instance for the first entry, eg1
   if isinstance(eg1, APExample):
      n1 = eg1.name
      eg = eg1
   else:
      n1 = eg1
      eg = find_eg(eg1)
      if not isinstance(eg, APExample):
         print("** compare_eg_pair: failed to find example named '%s'" % n1)
         return
      
   # so eg is the instance of eg1, just use it
   return eg.compare(eg2, eskip=eskip, verb=verb)

def display_eg_all(aphelp=1, source='', verb=0):
   """display the examples array if someone wants it
         aphelp :  1   show only AP help examples
                   0   show only others
                  -1   show all (leaving higher numbers for other cases)
         source : if set, restrict to matching
         verb   : verbosity level, pass on to eg.display
   """
   global ap_examples

   for eg in ap_examples:
      # skip what we do not want to show
      if aphelp >= 0 and aphelp != eg.aphelp:
         continue
      if source != '' and source != eg.source:
         continue
      eg.display(verb=verb)

if __name__ == '__main__':
   print('** this is not a main module')
   sys.exit(1)

