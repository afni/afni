#!/usr/bin/env python

# python3 status: started

import sys

# whine about execution as a main program
if __name__ == '__main__':
   print('** %s: not a main program' % sys.argv[0].split('/')[-1])
   sys.exit(1)

import math, os
from afnipy import afni_base as BASE, afni_util as UTIL
from afnipy import option_list as OL
from afnipy import lib_afni1D as LD
from afnipy import lib_vars_object as VO

# ---------------------------------------------------------------------------
# some globals

# help sections (see -help_section)
g_valid_help_sections = ['enew', 'eold', 'eall', 'eshow',
                         'afni_proc', 'afni_proc.py']

# types of motion simulated datasets that can be created
#    motion     : simulated motion time series - volreg base warped
#                 by inverse motion transformations (forward motion)
#    aligned    : 'motion' corrected by original volreg parameters
#    warped     : 'motion' fully warped as original volreg results
#                 note: any pre-warp (blip) would not apply
#    volreg     : 'motion' corrected by new 3dvolreg command
#    warped_vr  : ?? maybe allow a volreg version to be fully warped
motsim_types = ['motion','aligned', 'volreg', 'warped']
valid_warp_types = ['affine', 'NL']

clustsim_types = ['FWHM', 'ACF', 'both', 'yes', 'no']

# OC/MEICA/TEDANA methods
g_oc_methods = [
    'mean',               # average across echoes
    'OC',                 # default OC in @compute_OC_weights
    'OC_A',               # Javier's method
    'OC_B',               # full run method
    'OC_tedort',          # OC,        and ortvecs from tedana
    'OC_m_tedort',        # OC,        and ortvecs from MEICA tedana
    'tedana',             # dn_ts_OC.nii           from tedana
    'tedana_OC',          # ts_OC.nii              from tedana
    'tedana_OC_tedort',   # ts_OC.nii, and ortvecs from tedana
    # https://github.com/ME-ICA/tedana
    'm_tedana',           # tedana from MEICA group: dn_ts_OC.nii
    'm_tedana_OC',        # ts_OC.nii              from m_tedana
    'm_tedana_OC_tedort', # tedana --tedort, extract ortvecs for 3dD
    'm_tedana_m_tedort'   # tedana --tedort (MEICA group tedort)
    ]
g_m_tedana_site = 'https://github.com/ME-ICA/tedana'

g_despike_new_opts = [
    'yes',              # apply as default -NEW option
    'no',               # add no option
    '-NEW',             # add -NEW
    '-NEW25'            # add -NEW25
    ]

WARP_EPI_TLRC_ADWARP    = 1
WARP_EPI_TLRC_WARP      = 2
WARP_EPI_ALIGN_A2E      = 4
WARP_EPI_ALIGN_E2A      = 8

# rcr - common steps
#
# - apply_uopt_to_block('-tcat_remove_last_trs', user_opts, block)
# - if proc.surf_anat: treat as surface analysis
# - if new dir to remove: proc.rm_list.append('dir') ; proc.rm_dirs = 1
# - apply proc.sep_char?  (maybe it's time to forget that...)
# - if OL.opt_is_yes(block.opts.find_opt(oname)): ...
#   if block.opts.have_yes_opt('-regress_run_clustsim', default=1):

def apply_uopt_to_block(opt_name, user_opts, block):
    """just pass any parameters for opt_name along to the block
       return 0/1, based on whether the option was found
    """
    uopt = user_opts.find_opt(opt_name)
    if uopt:
        bopt = block.opts.find_opt(opt_name)
        # if it exists, modify, else append
        if bopt: bopt.parlist = uopt.parlist
        else:    block.opts.olist.append(uopt)

        return 1

    return 0

def apply_uopt_list_to_block(opt_name, user_opts, block):
    """pass all such opts to the block, which should start with none
       return 0/1, based on whether the option was found
       return -1 on error
    """
    bopt = block.opts.find_opt(opt_name)
    if bopt != None:
       print("** attempting to re-add all '%s' options to block??" % opt_name)
       print("   failing...")
       return -1

    found = 0
    for opt in user_opts.find_all_opts(opt_name):
        block.opts.olist.append(opt)
        found = 1

    return found

def gen_afni_name(name, **kwargs):
   """local wrapper for BASE.afni_name

      partition keywords into those for afni_name and those to add locally

      require a first name argument for this wrapper, as it's what I do anyway
   """
   # first, partition the afni_base keywords
   ankws = {}
   for k in ['do_sel', 'view']:
      if k in kwargs:
         ankws[k] = kwargs.pop(k)

   aname = BASE.afni_name(name, **ankws)

   # now, mutate as we see fit, bwahaha...

   # add a to_resam attribute, -ROI_import datasets will be resampled
   aname.ap_to_resam = 0

   return aname

def warp_item(desc='', wtype='', warpset=''):
   """desc      = description of warp
      wtype     = affine, NL
      warpset   = dataset for warp *.aff12.1D, *_WARP.nii, etc.
   """
   if warpset == '':
      print("** warp_item missing warpset, desc = '%s'" % desc)
      return None
   if wtype not in valid_warp_types:
      print('** warp for dset %s not in %s' % (warpset, valid_warp_types))
      return None

   vo = VO.VarsObject()
   vo.set_var('desc', desc)
   vo.set_var('wtype', wtype)
   vo.set_var('warpset', warpset)

   return vo

def apply_catenated_warps(proc, warp_list, base='', source='', prefix='',
                          dim=0, NN=0, NLinterp='', ewopts='', istr=''):
   """For now, warp_list should consist of an outer to inner list of warps.
      If any are non-linear, use 3dNwarpApply to apply them.  Otherwise,
      use 3dAllineate.

      Note that 3dAllineate should take only a single warp (for now).

      if NN: include options for warping using NN, such as for an all-1 dset

      if NLinterp: apply corresponding interp option to NL case
                   (speed-up for computing warps of all-1 dsets)

      ewopts: extra EPI warp opts

      allow for proc.vr_warp_fint to alter final interpolation

      return: status and a single command, indented by istr
   """

   NL = 0
   nwarps = len(warp_list)
   for warp in warp_list:
      if warp.wtype == 'NL': NL = 1

   wstr = ' '.join([w.warpset for w in warp_list])
   if nwarps > 1: wstr = '"%s"' % wstr

   if not NL and nwarps > 1:
      print('** ACW: not ready for sequence of affine warps')
      return 1, ''

   # make note of any final interpolation
   finterp = proc.vr_warp_fint

   if NL:
      if proc.vr_warp_mast is not None:
         mstr = ' -master %s' % proc.vr_warp_mast.shortinput()
      elif base: mstr = ' -master %s' % base
      else:      mstr = ''
      if dim > 0: dimstr = ' -dxyz %g' % dim
      else:       dimstr = ''

      clist = ['3dNwarpApply%s%s \\\n' % (mstr, dimstr),
               '             -source %s \\\n'   % source,
               '             -nwarp %s \\\n'    % wstr]

      # initial interpolation
      if NLinterp: clist.append('             -interp %s \\\n' % NLinterp)
      # final interpolation
      if NN:       clist.append('             -ainterp NN -quiet \\\n')
      elif finterp:
                   clist.append('             -ainterp %s \\\n'%finterp)

      # extra options
      if ewopts: clist.append('             %s \\\n' % ewopts)

      clist.append('             -prefix %s\n' % prefix)

   else: # affine
      if base: bstr = ' -base %s' % base
      else:    bstr = ''
      if dim > 0: dimstr = ' -mast_dxyz %g' % dim
      else:       dimstr = ''

      # final interpolation
      if NN:        nstr = '-final NN -quiet'
      elif finterp: nstr = '-final %s' % finterp
      else:         nstr = ''

      if dimstr or nstr:
         mastline = '           '
         if dimstr: mastline += (' -mast_dxyz %g' % dim)
         if nstr:   mastline += (' %s' % nstr)
         mastline += ' \\\n'
      else:
         mastline = ''

      indent = ' ' * 12

      clist = ['3dAllineate%s\\\n'         % bstr,
               '%s-input %s \\\n'          % (indent, source),
               '%s-1Dmatrix_apply %s \\\n' % (indent, wstr),
              ]

      # if we have a volreg warp master, apply it
      if proc.vr_warp_mast is not None:
         mstr = '%s-master %s \\\n' % (indent, proc.vr_warp_mast.shortinput())
         clist.append(mstr)

      # separate, since we cannot include empty lines
      if dimstr or nstr:
         clist.append(mastline)

      # extra options
      if ewopts: clist.append('%s%s \\\n' % (indent, ewopts))

      clist.append('%s-prefix %s\n' % (indent, prefix))

   cmd = istr + istr.join(clist)

   return 0, cmd


# --------------- tcat ---------------

# modify the tcat block options according to the user options
def db_mod_tcat(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-tcat_remove_first_trs', 1, [0], setpar=1)

    errs = 0

    uopt = user_opts.find_opt('-tcat_remove_first_trs')
    bopt = block.opts.find_opt('-tcat_remove_first_trs')
    if uopt and bopt:
      try: bopt.parlist = [int(param) for param in uopt.parlist]
      except:
          print("** ERROR: %s: invalid as integers: %s"   \
                % (uopt.name, ' '.join(uopt.parlist)))
          errs += 1
      if errs == 0 and bopt.parlist[0] > 0:
        print('** warning: removing first %d TRs from beginning of each run\n'\
              '   --> the stimulus timing files must reflect the '            \
              'removal of these TRs' % bopt.parlist[0])

    apply_uopt_to_block('-tcat_remove_last_trs', user_opts, block)
    apply_uopt_to_block('-tcat_preSS_warn_limit', user_opts, block)

    if errs == 0: block.valid = 1
    else        : block.valid = 0

# do not rely on the form of input filenames
# use 3dtcat to copy each file to od_var, then 'cd' into it
def db_cmd_tcat(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-tcat_remove_first_trs')
    flist = opt.parlist
    first = opt.parlist[0]
    # if multiple runs, length of flist should match
    if proc.runs > 1:
       if len(flist) == 1:
          flist = [first for r in range(proc.runs)]

    if len(flist) != proc.runs:
       print('** error: -tcat_remove_first_trs takes either 1 value or nruns')
       return

    # maybe the user updated our warning limit
    val, err = block.opts.get_type_opt(float, '-tcat_preSS_warn_limit')
    if err: return
    if val != None:
       if val < 0.0 or val > 1.0:
          print('** -tcat_preSS_warn_limit: limit %s outside [0,1.0]' % val)
          return
       proc.out_ss_lim = val

    # remove the last TRs?  set rmlast
    val, err = proc.user_opts.get_type_opt(int, '-tcat_remove_last_trs')
    if err: return
    if val == None: rmlast = 0
    else: rmlast = val

    if proc.have_sels: selstr = "while applying volume selectors"
    else:              selstr = "while removing the first %d TRs" % first


    cmd = cmd + "# %s\n"                                                \
                "# apply 3dTcat to copy input dsets to results dir,\n"  \
                "# %s\n"                                                \
                % (block_header('auto block: tcat'), selstr)

    # we might need to process multiple echoes
    for eind in range(proc.num_echo):
      if proc.have_me:
         if (eind+1) == proc.reg_echo:
            estr = '(fave_echo = registration driver)'
         else:
            estr = '(registration follower)'
         cmd += '\n# EPI runs for echo %d %s\n' % (eind+1, estr)

      for run in range(proc.runs):
        if rmlast == 0: final = '$'
        else:
            reps = proc.reps_all[run]
            final = '%d' % (reps-rmlast-1)
            if reps-rmlast-1 < 0:
                print('** run %d: have %d reps, cannot remove %d!' \
                      % (run+1, reps, rmlast))
                return
        # ME: set output name
        if proc.have_me: pre_form = proc.prefix_form(block,run+1,eind=(eind+1))
        else:            pre_form = proc.prefix_form(block,run+1)
        # ME: set input name
        if proc.have_me: dset = proc.dsets_me[eind][run]
        else:            dset = proc.dsets[run]

        # check if input has selectors; look for happy case before error
        if dset.selectors() == '':
           selstr = "'[%d..%s]'" % (flist[run], final)
        else:
           # okay only if not removing any at beginning or end
           if flist[run] == 0 and final == '$':
               selstr = dset.selectors()
           else:
               print("** cannot use selectors when removing first or last TRs")
               return

        cmd = cmd + "3dTcat -prefix %s/%s %s%s\n" \
                    % (proc.od_var, pre_form, dset.nice_input(), selstr)

        proc.tlist.add(dset.nice_input(sel=1), pre_form, 'tcat', ftype='dset')

      if proc.have_me: cmd += '\n'

    proc.reps -= first+rmlast   # update reps to account for removed TRs
    if proc.verb > 0:
       print("-- %s: reps is now %d" % (block.label, proc.reps))
       if proc.have_me:
          print("-- multi-echo data: have %d echoes across %d run(s)" \
                % (len(proc.dsets_me), len(proc.dsets)))

    for run in range(len(proc.reps_all)):
       proc.reps_all[run] -= (flist[run] + rmlast)
    if not UTIL.vals_are_constant(proc.reps_all):
       proc.reps_vary = 1

    # now we are ready to set polort for the analysis
    if set_proc_polort(proc): return

    cmd = cmd + '\n'                                                    \
                '# and make note of repetitions (TRs) per run\n'        \
                'set tr_counts = ( %s )\n'%UTIL.int_list_string(proc.reps_all)

    cmd = cmd + '\n'                                                          \
                '# -------------------------------------------------------\n' \
                '# enter the results directory (can begin processing data)\n' \
                'cd %s\n\n\n' % proc.od_var

    tcat_extract_vr_base(proc)

    if proc.blip_in_rev != None and proc.blip_in_for == None:
       rv, tcmd = tcat_make_blip_in_for(proc, block)
       if rv: return
       if tcmd != '': cmd += tcmd

    return cmd

def tcat_make_blip_in_for(proc, block):
    """copy a number of time points from first tcat output that
       corresponds to blip_in_rev

       Populate proc.blip_in_for and blip_dset_for.

       return status (0 on success) and command
    """
    # should we be here?
    if not isinstance(proc.blip_in_rev, BASE.afni_name): return 0, ''
    if isinstance(proc.blip_in_for, BASE.afni_name): return 0, ''

    # make forward blip from first input (after pre-SS is removed)

    # populate proc.blip_in_for
    forinput = proc.prefix_form(block, 1, view=1)
    revinput = proc.blip_in_rev.nice_input(sel=1)
    nt = UTIL.get_3dinfo_nt(revinput)
    if nt == 0: return 1, ''

    if nt == 1:
       proc.blip_in_for = gen_afni_name("%s[0]" % forinput)
    else:
       proc.blip_in_for = gen_afni_name("%s[0..%d]" % (forinput, nt-1))
    proc.blip_in_for.view = proc.view

    if proc.verb > 2:
       print('-- using auto blip forward dset, %s' \
             % proc.blip_in_for.shortinput(sel=1))

    # populate proc.blip_dset_for
    proc.blip_dset_for = gen_afni_name('blip_forward', view=proc.view)

    # make actual command
    cmd = '# -------------------------------------------------------\n' \
          '# extract initial volumes as automatic -blip_forward_dset\n' \
          '3dTcat -prefix %s %s\n\n'                                    \
          % (proc.blip_dset_for.prefix, proc.blip_in_for.shortinput(sel=1))

    return 0, cmd

def set_proc_polort(proc):
    """set proc.polort from -regress_polort or get_default_polort
       return 0 on success
    """
    rblock = proc.find_block('regress')
    if rblock:
       opt = rblock.opts.find_opt('-regress_polort')
       if opt:
          # try to set it and return
          try: proc.regress_polort = int(opt.parlist[0])
          except:
             print("** -regress_polort requires int for degree (have '%s')\n" \
                   % opt.parlist[0])
             return 1
          return 0

    # no option, figure it out

    proc.regress_polort = UTIL.get_default_polort(proc.tr, proc.reps)
    if proc.verb > 0:
        print("++ updating polort to %d, from run len %.1f s" %  \
              (proc.regress_polort, proc.tr*proc.reps))

    return 0

def tcat_extract_vr_base(proc):
    """find volreg block
       get block.opts.find_opt('-volreg_base_ind') and indices
       if necessary,
    """

    # everything should exist, if the volreg block does
    block = proc.find_block('volreg')
    if not block: return

    # if we have an external base, nothing to do
    if proc.vr_ext_base: return

    # already set if MIN_OUTLIER
    if proc.vr_int_name == '':
       bopt = block.opts.find_opt('-volreg_base_ind')
       if not bopt:
          print('** TEVB: no vr_int_name, no volreg_base_ind')
          return

       run = bopt.parlist[0]+1
       ind = bopt.parlist[1]

       # if negative, then 'last', and we need to re-extract run and index
       if run <= 0 or ind < 0:
          run = proc.runs
          if proc.reps_vary: ind = proc.reps_all[-1] - 1
          else:              ind = proc.reps - 1
          if proc.verb > 2:
             print('++ TEVB: updating run/index to %d, %d' % (run, ind))

       set_vr_int_name(block, proc, 'vr_base', runstr='%02d'%run,
                                               trstr='"[%d]"'%ind)

    # if we are extracting an internal volreg base (min outlier or index),
    extract_registration_base(block, proc)


def extract_registration_base(block, proc, prefix=''):
   """at the end of previous (to volreg) block, extract vr_int_name
      (min outlier or other) into vr_ext_pre
   """

   if proc.vr_int_name == '':
      print('** ERB: no vr_int_name')
      return 1

   # if a prefix was passed use it
   if prefix != '':
      proc.vr_ext_pre = prefix

   if proc.vr_ext_pre == '':
      print('** ERB: no vr_ext_pre')
      return 1

   # get the block to put 3dbucket at the end of
   prev_block = proc.find_block(proc.prev_lab(block))

   # if it is the tcat block, shift to after postdata
   if prev_block.label == 'tcat':
      postblock = proc.find_block('postdata')
      if postblock != None:
         prev_block = postblock

   prev_block.post_cstr += \
      '# --------------------------------\n' \
      '# extract volreg registration base\n' \
      '3dbucket -prefix %s %s\n\n' % (proc.vr_ext_pre, proc.vr_int_name)

   return 0


# --------------- post-data ---------------

# modify the tcat block options according to the user options
def db_mod_postdata(block, proc, user_opts):
    """This block is for initial processing before anything that
       affects the EPI data.
       This block will probably never get named options.
    """

    # ------------------------------------------------------------
    # note other anat followers (-anat_follower*)
    if apply_general_anat_followers(proc): return

    # ------------------------------------------------------------
    # ROI_import steps

    # ROI imports may be predecated on proc.tlrc_base, so set it now
    if 'tlrc' in proc.block_names:
       prepare_tlrc_base(proc)

    # note any ROI import user options (-ROI_import)
    #   - add to dict now
    #   - resample in regress block
    #   - initial 3dcopy is in afni_proc.py
    if apply_general_ROI_imports(proc): return

    # initialize automatic ROI options (maybe append -ROI_import opts):
    #   - if SPACE is known and we have an APQC_atlas_SPACE dset
    #     (and we have a regress block)
    #     - add APQC atlas as -ROI_import
    #     - add SPACE ALL to -regress_compute_tsnr_stats
    if init_auto_tsnr_rois(proc): return
    
    # ------------------------------------------------------------
    if len(block.opts.olist) == 0: pass
    block.valid = 1

def db_cmd_postdata(proc, block):
    """add any sub-blocks with their own headers"""

    cmd = ''

    # consider applying a uniformity correction
    umeth, rv = proc.user_opts.get_string_opt('-anat_uniform_method',
                                              default='default')
    proc.anat_unif_meth = umeth # store for later
    if umeth == 'unifize' and proc.anat:
       rv, oc = make_uniformity_commands(proc, umeth)
       if rv: return   # failure (error has been printed)
       cmd = cmd + oc

    # probably get outlier fractions
    if proc.user_opts.have_yes_opt('-outlier_count', default=1):
       if min(proc.reps_all) >= 5:
          rv, oc = make_outlier_commands(proc, block)
          if rv: return   # failure (error has been printed)
          cmd = cmd + oc
       else:
          print('** warning: 3dToutcount requires at least 5 time points\n' \
                '   (are all -dsets parameters actually time series?)')
          if proc.vr_base_MO:
             print('** cannot align to MIN_OUTLIER')
             return

    # possibly get @radial_correlate command
    if proc.user_opts.have_yes_opt('-radial_correlate', default=0):
       rv, oc = run_radial_correlate(proc, block, full=1)
       if rv: return   # failure (error has been printed)
       cmd = cmd + oc

    # add anat to anat followers?
    if proc.anat_has_skull:
       if proc.find_block('align') or proc.find_block('tlrc'):
          # add anat to own follower list (we are now after 3dcopy)
          # (no existence check)
          ff = proc.add_anat_follower(aname=proc.anat, dgrid='anat',
                                      label='anat_w_skull', check=0)
          ff.set_var('final_prefix', 'anat_w_skull_warped')

    # ---------------
    # if requested, create any anat followers
    if should_warp_anat_followers(proc, block):
       rv, tcmd = warp_anat_followers(proc, block, proc.anat, prevepi=1)
       if rv: return
       if tcmd: cmd += tcmd

    return cmd

def db_mod_post_process(proc):
    """this happens after all db_mod functions but before any db_cmd ones

       nothing yet...
    """
    return 0

def init_auto_tsnr_rois(proc):
    """initialize automatic ROI options
       - start with 'brain', and try to add an APQC atlas
       - if SPACE is known and we have an APQC_atlas_SPACE dset
         (and we have a regress block)
         (and SPACE is not already being used for an ROI label)
         - add APQC atlas as -ROI_import
         - add SPACE ALL to -regress_compute_tsnr_stats
    """

    # if this has already been turned off, we are done
    if proc.regress_auto_tsnr_rois is None:
       return 0

    # if the user does not want this, set regress_auto_tsnr_rois to None
    oname = '-regress_compute_auto_tsnr_stats'
    if proc.user_opts.have_no_opt(oname, default=0):
       if proc.verb > 1: print("-- skipping auto tsnr stats at user request")
       # disable
       proc.regress_auto_tsnr_rois = None
       return 0

    # initialize auto list with 'brain'
    label = 'brain'
    if label not in proc.regress_auto_tsnr_rois:
       proc.regress_auto_tsnr_rois.append(label)

    # do we have a regress block?
    # note: find_block() will not succeed yet, so use block_names
    if not 'regress' in proc.block_names:
       if proc.verb > 2: print("IATR: no regress block")
       return 0

    # do we have a known final space and existent dataset?
    if proc.tlrc_base is None or proc.tlrc_space == '':
       if proc.verb > 2: print("IATR: tlrc base or space")
       return 0

    if not proc.tlrc_base.locate():
       if proc.verb > 2: print("IATR: failed tlrc_base.locate()")
       return 0

    # if the user has already applied the space label, do not add
    if proc.tlrc_space in proc.roi_dict:
       print("-- will not add auto tsnr stats, label %s is in use" \
             % proc.tlrc_space)
       return 0

    # now search for APQC_atlas_SPACE.nii.gz
    label = proc.tlrc_space
    roidset = 'APQC_atlas_%s.nii.gz' % label
    rname = gen_afni_name(roidset)
    if not rname.locate():
       if proc.verb > 2: print("IATR: no APQC atlas %s" % roidset)
       return 0

    print("-- have APQC atlas %s" % roidset)

    # add to roi_dict (dupe final apply_general_ROI_imports step)
    # (change rname to be for the resulting name)
    rname = gen_afni_name('ROI_import_%s' % label)
    rname.to_resam = 1
    if proc.add_roi_dict_key(label, aname=rname):
       return 1
 
    # and actually add an import option
    proc.user_opts.add_opt('-ROI_import', 2, [label, roidset], setpar=1)
    proc.regress_auto_tsnr_rois.append(label)

    return 0


def apply_general_ROI_imports(proc):
   """for each -ROI_import option
        - create an afni_name
        - add_roi_dict_key
   """
   oname = '-ROI_import'

   # nothing to do?
   if not proc.user_opts.find_opt(oname):
      return 0

   # add roi dict keys
   for opt in proc.user_opts.find_all_opts(oname):
      label = opt.parlist[0]
      dname = opt.parlist[1]

      aname = gen_afni_name('ROI_import_%s' % label)
      # and note this dataset for resampling
      aname.to_resam = 1
      if proc.add_roi_dict_key(label, aname=aname):
         return 1

   return 0

def apply_general_anat_followers(proc):
   # add any other anat follower datasets

   elist, rv = proc.user_opts.get_string_list('-anat_follower_erode')
   if elist == None: elist = []

   # make a dictionary from -anat_follower_erode_level options
   edict = {}
   for opt in proc.user_opts.find_all_opts('-anat_follower_erode_level'):
      label = opt.parlist[0]
      level = opt.parlist[1]
      try: level = int(level)
      except:
         print("** -anat_follower_erode_level %s" \
               "   level '%s' must be an integer" \
               % (' '.join(opt.parlist), level))
         return 1
      edict[label] = level

   # and append every -anat_follower_erode label at level 1
   for label in elist:
      if label not in edict:
         edict[label] = 1

   for oname in ['-anat_follower', '-anat_follower_ROI' ]:
      for opt in proc.user_opts.find_all_opts(oname):
         label = opt.parlist[0]
         dgrid = opt.parlist[1]
         dname = opt.parlist[2]

         if oname == '-anat_follower_ROI':
            ff = proc.add_anat_follower(name=dname, dgrid=dgrid,
                                        NN=1, label=label)
            flab = 'follow_ROI'
         else:
            ff = proc.add_anat_follower(name=dname, dgrid=dgrid, label=label)
            flab = 'follow_anat'

         if ff == None:
            print('** failed to add follower %s' % dname)
            return 1

         # note whether we erode this mask
         if label in edict:
            ff.set_var('erode', edict[label])

         ff.set_var('final_prefix', '%s_%s'%(flab, label))

def make_uniformity_commands(proc, umeth):
    """apply uniformity correction to the anat, based on umeth"""

    # right now, we only work on 'unifize'
    if umeth != 'unifize': return 0, ''
    if not proc.anat:      return 0, ''

    if proc.verb > 1: print('-- unifizing anat...')

    # see if the user has provided other options
    other_opts = proc.user_opts.get_joined_strings('-anat_opts_unif',prefix=' ')

    opre = proc.anat.prefix + '_unif'
    oset = proc.anat.new(new_pref=opre)

    # -GM can lead to failures, default to off
    if proc.user_opts.have_yes_opt('-anat_unif_GM', default=0): gmstr = ' -GM'
    else:                                                       gmstr = ''

    cmd  = "# %s\n"                                                       \
           "# perform '%s' uniformity correction on anatomical dataset\n" \
           % (block_header('anat_unif'), umeth)

    cmd += "3dUnifize -prefix %s%s%s %s\n\n" \
           % (opre, gmstr, other_opts, proc.anat.pv())

    # update prefix for anat and tlrcanat
    if proc.verb > 2:
       if proc.tlrcanat:
          print('++ updating anat and tlrcanat prefix from %s, %s to %s' \
                % (proc.anat.prefix, proc.tlrcanat.prefix, opre))
       else:
          print('++ updating anat prefix from %s to %s'%(proc.anat.prefix,opre))

    proc.anat_unifized = 1      # note that this has been done
    proc.anat.prefix = opre
    if proc.tlrcanat: proc.tlrcanat.prefix = opre

    return 0, cmd

# could do this from any block, but expect to do at end of tcat
# return error code (0 = success) and string
def make_outlier_commands(proc, block):
    # ----------------------------------------
    # check for any censoring
    val, err = proc.user_opts.get_type_opt(float, '-regress_censor_outliers')
    if err: return 1, ''
    elif val == None: censor = 0.0
    elif val < 0.0 or val > 1.0:
        print('** -regress_censor_outliers value %f is not in [0,1.0]' % val)
        return 1, ''
    else: censor = val

    # check for ignoring first TRs of each run
    val, err = proc.user_opts.get_type_opt(int, '-regress_skip_first_outliers')
    if err: return 1, ''
    elif val == None: nskip = 0
    elif val < 0:
        print('** -regress_skip_first_outliers: bad value %d' % val)
        return 1, ''
    else: nskip = val

    # if ignoring first few TRs, modify the 1deval expression
    if nskip: dstr = '*step(t-%d)' % (nskip-1)
    else:     dstr = ''

    if censor > 0.0:
        cfile = 'outcount_${subj}_censor.1D'
        cs0 = '\n'                                                            \
          '    # censor outlier TRs per run, ignoring the first %d TRs\n'     \
          '    # - censor when more than %g of automask voxels are outliers\n'\
          '    # - step() defines which TRs to remove via censoring\n'        \
          '    1deval -a outcount.r$run.1D '                                  \
          '-expr "1-step(a-%g)%s" > rm.out.cen.r$run.1D\n'                    \
          % (nskip, censor, censor, dstr)
        cs1 = '\n'                                                          \
              '# catenate outlier censor files into a single time series\n' \
              'cat rm.out.cen.r*.1D > %s\n' % cfile
        if proc.censor_file:
            rv, cs2 = combine_censor_files(proc, cfile)
            if rv: return 1, ''
            cs1 += cs2
        else:
            proc.censor_file = cfile
            proc.censor_count = 1

        proc.out_cen_lim = censor       # and note outlier censor limit
    else:
        cs0 = ''
        cs1 = ''
    # ---------- end censor options ----------

    # set polort level
    val, err = proc.user_opts.get_type_opt(int, '-outlier_polort')
    if err: return
    elif val != None and val >= 0: polort = val
    else: polort = proc.regress_polort

    # use Legendre polynomials?
    opt = proc.user_opts.find_opt('-outlier_legendre')
    if not opt or OL.opt_is_yes(opt): lstr = ' -legendre'
    else:                             lstr = ''

    prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=-1)
    ofile = 'outcount.r$run.1D'
    warn  = '** TR #0 outliers: possible pre-steady state TRs in run $run'
    proc.out_wfile = 'out.pre_ss_warn.txt'
    proc.outl_rfile = ofile    # per run outlier file

    cmd  = '# %s\n'                                               \
           '# QC: compute outlier fraction for each volume\n'     \
           % block_header('auto block: outcount')

    if proc.out_ss_lim > 0.0: cmd += 'touch %s\n' % proc.out_wfile

    cmd += 'foreach run ( $runs )\n'                                      \
           '    3dToutcount -automask -fraction -polort %d%s \\\n'        \
           '                %s > %s\n'                                    \
           '%s'                                                           \
           % (polort, lstr, prev_prefix, ofile, cs0)

    if proc.out_ss_lim > 0.0:
       cmd +='\n'                                                        \
          '    # outliers at TR 0 might suggest pre-steady state TRs\n'  \
          '    if ( `1deval -a %s"{0}" -expr "step(a-%g)"` ) then\n'     \
          '        echo "%s" >> %s\n'                                    \
          '    endif\n' % (ofile, proc.out_ss_lim, warn, proc.out_wfile)

    cmd += 'end\n\n'                                                      \
           '# catenate outlier counts into a single time series\n'        \
           'cat outcount.r*.1D > outcount_rall.1D\n'                      \
           '%s\n' % cs1

    # add a check for maximum values of exactly 4095
    rv, c4095 = make_4095_check_commands(proc, block, prev_prefix)
    cmd += c4095

    return rv, cmd

def make_4095_check_commands(proc, block, input_form):
    """run 3dTto1D -method 4095_warn, saving any warnings to out.4095_all.txt
       
       Also, add out.4095_all.txt to the uvars, for use in the APQC report.

       return the status (0 on success) and command string
    """
    warnfile = 'out.4095_warn.txt'
    cmd = '# ---------------------------------------------------------\n' \
          '# check for potential 4095 saturation issues\n'                \
          'foreach run ( $runs )\n'                                       \
          '    3dTto1D -method 4095_warn -input %s\\\n'                   \
          '            |& tee -a out.4095_all.txt\n'                      \
          'end\n\n'                                                       \
          '# make a file showing any resulting warnings\n'                \
          "awk '/warning/ {print}' out.4095_all.txt | tee %s\n\n"         \
          % (input_form, warnfile)

    # and add the uvar
    proc.uvars.set_var('max_4095_warn_dset', [warnfile])

    return 0, cmd

def run_radial_correlate(proc, block, full=0):
    """
    """

    if min(proc.reps_all) < 5:
       print('** warning: @radial_correlate requires 5+ time points\n' \
             '   (are all -dsets parameters actually time series?)\n')
       return 1, ''

    olist, rv = proc.user_opts.get_string_list('-radial_correlate_opts')
    if olist and len(olist) > 0:
        other_opts = '%18s%s \\\n' % (' ', ' '.join(olist))
    else: other_opts = ''

    # note the dsets that will be applied
    # in the regress case, go after errts as a single time series
    if block.label == 'regress':
       if proc.errts_final == '':
          print("** missing errts dataset for @radial_correlate in regress")
          return 1, ''
       dsets = '%s%s.HEAD %s%s.HEAD' % (proc.all_runs, proc.view,
                                        proc.errts_final, proc.view)
    elif full:
       dsets = proc.prev_dset_form_wild(block, view=1, eind=-1)
    else:
       dsets = proc.dset_form_wild(block.label, eind=-1)

    # if block is scale or scale has happened, pass any EPI mask
    # (since we do not want to automask a scaled dataset)
    # also, mask in regress block due to errts
    mopt = ''
    if block.label == 'scale' or block.label == 'regress' \
       or (proc.blocks_ordered('scale', block.label) 
           and 'scale' in proc.block_names):

       # be sure we have created the mask
       if mask_created(proc.mask):
          mopt = '%18s-mask %s \\\n' % (' ', proc.mask.nice_input())
       else:
          print("** warning: computing radcor on scaled/errts without mask")
          print("   (might get weak results along brain edges)")

    # fail if we have entered the dreaded surface domain
    if proc.surf_anat and (block.label == 'surf' or \
                           proc.blocks_ordered('surf', block.label)):
       print("** cannot compute radcor on surface data")
       return 1, ''

    # if doing the full form, include a header, clust, etc.
    if full:
       rdir = 'radcor.pb%02d.%s.full' % (block.index, block.label)
       # rdir = 'corr_test.results.%s' % block.label

       cmd  = '# %s\n'                                                    \
              '# QC: compute correlations with spherical averages\n'      \
              % block_header('@radial_correlate (%s)' % block.label)

       cmd += '@radial_correlate -nfirst 0 -polort %s -do_clust yes \\\n' \
              '                  -rdir %s \\\n'                           \
              '%s'                                                        \
              '%s'                                                        \
              '                  %s\n\n'                                  \
              % (proc.regress_polort, rdir, mopt, other_opts, dsets)
    else:
       rdir = 'radcor.pb%02d.%s' % (block.index, block.label)
       cmd  = '# ---------------------------------------------------------\n' \
              '# QC: compute correlations with spherical ~averages\n'         \
              '@radial_correlate -nfirst 0 -polort %s -do_clean yes \\\n'     \
              '                  -rdir %s \\\n'                               \
              '%s'                                                            \
              '%s'                                                            \
              '                  %s\n\n'                                      \
              % (proc.regress_polort, rdir, mopt, other_opts, dsets)

    return 0, cmd

def run_qc_var_line_blocks(proc, block):
    """
    """
    prog = 'find_variance_lines.tcsh'
    if min(proc.reps_all) < 10:
       print('** warning: %s requires 10+ time points\n' \
             '   (are all -dsets parameters actually time series?)\n' \
             % prog)
       return 1, ''

    # todo?
    olist, rv = proc.user_opts.get_string_list('-vlines_opts')
    if olist and len(olist) > 0:
        other_opts = '%8s%s \\\n' % (' ', ' '.join(olist))
    else: other_opts = ''

    # note the dsets that will be applied
    # in the regress case, go after errts as a single time series
    if block.label == 'regress':
       print("** RCR - todo: find_variance_lines for regress block")
       return 1
       if proc.errts_final == '':
          print("** missing errts dataset for find_variance_lines in regress")
          return 1, ''
       dsets = '%s%s.HEAD %s%s.HEAD' % (proc.all_runs, proc.view,
                                        proc.errts_final, proc.view)
    else:
       dsets = proc.dset_form_wild(block.label, eind=-1)

    # set the result directory, and save as a uvar
    rdir = 'vlines.pb%02d.%s' % (block.index, block.label)
    proc.uvars.set_var('vlines_%s_dir' % block.label, [rdir])

    # no longer erode, the default has changed to -ignore_edges
    cmd  = '# ---------------------------------------------------------\n' \
           '# QC: look for columns of high variance\n'                     \
           'find_variance_lines.tcsh -polort %s            \\\n'           \
           '%s'                                                            \
           '       -rdir %s \\\n'                                          \
           '       %s |& tee out.%s.txt\n\n'                               \
           % (proc.regress_polort, other_opts, rdir, dsets, rdir)

    return 0, cmd

def combine_censor_files(proc, cfile, newfile=''):
    """create a 1deval command to multiply the 2 current censor file
       with the existing one, writing to newfile

       store newfile as censor_file

       return err, cmd_str   (where err=0 implies success)"""

    if not newfile:
        newfile = 'censor_${subj}_combined_%d.1D' % (proc.censor_count+1)
    if not proc.censor_file or not cfile:
        print('** combine_censor_files: missing input')
        return 1, ''
    cstr = '# combine multiple censor files\n'          \
           '1deval -a %s -b %s \\\n'                    \
           '       -expr "a*b" > %s\n'                  \
           % (cfile, proc.censor_file, newfile)
    proc.censor_file = newfile
    proc.censor_count += 1

    return 0, cstr

# --------------- blip block ---------------

# The blip processing must be done as a separate block (from volreg), since
# its output might be used as the input to either the anat or tlrc blocks.

def db_mod_blip(block, proc, user_opts):
   """handle -blip options, to either input or comput a warp

      - set any proc.blip_in_* input names here
      - afni_proc.py will create corresponding proc.blip_dset_* names
        when copying the datasets into the results directory
   """

   apply_uopt_to_block('-blip_forward_dset', user_opts, block)
   apply_uopt_to_block('-blip_reverse_dset', user_opts, block)
   apply_uopt_to_block('-blip_opts_qw', user_opts, block)
   apply_uopt_to_block('-blip_warp_dset', user_opts, block)

   # --- note any datasets before checking for consistency

   # first note any precomputed blip warp dataset
   bopt = block.opts.find_opt('-blip_warp_dset')
   if bopt:
      proc.blip_in_warp = gen_afni_name(bopt.parlist[0])
      if proc.verb > 0:
         print('-- have precomputed blip warp dset %s' \
               % proc.blip_in_warp.shortinput(sel=1))
      proc.blip_obl_warp = dset_is_oblique(proc.blip_in_warp, proc.verb)

   # note blip reverse input dset (if no warp is passed)
   bopt = block.opts.find_opt('-blip_reverse_dset')
   if bopt:
      proc.blip_in_rev = gen_afni_name(bopt.parlist[0])
      if proc.verb > 1:
         print('-- will compute blip up/down warp via %s' \
               % proc.blip_in_rev.shortinput(sel=1))
      proc.blip_obl_rev = dset_is_oblique(proc.blip_in_rev, proc.verb)

   # note blip forward input dset (or make one up)
   bopt = block.opts.find_opt('-blip_forward_dset')
   if bopt:
      proc.blip_in_for = gen_afni_name(bopt.parlist[0])
      if proc.verb > 1:
         print('-- have blip forward dset %s' \
               % proc.blip_in_for.shortinput(sel=1))
      proc.blip_obl_for = dset_is_oblique(proc.blip_in_for, proc.verb)

   # --- check for consistency

   # we should be doing *something* here
   if proc.blip_in_warp is None and proc.blip_in_rev is None:
      print('** have blip block without -blip_reverse_dset or -blip_warp_dset')
      return

   # do not both input and compute a warp
   if proc.blip_in_warp is not None and \
      (proc.blip_in_for is not None or proc.blip_in_rev is not None):
      print("** use either -blip_warp_dset or -blip_reverse_dset, not both")
      return

   # ME: if reverse blip, then forward is also required
   if proc.use_me and proc.blip_in_for is None and proc.blip_in_rev is not None:
      print("** when using multi-echo data and distortion correction,\n"
            "   -blip_reverse_dset requires corresponding -blip_forward_dset")
      return

   # check for consistent obliquity of either the warp or rev
   if proc.blip_in_warp is not None: 
      if proc.dsets_obl != proc.blip_obl_warp:
         print("** warning: blip warp obliquity (%d) does not match EPI (%d)" \
               % (proc.blip_obl_warp, proc.dsets_obl))
   else:
      if proc.dsets_obl != proc.blip_obl_rev:
         print("** error: reverse blip obliquity (%d) does not match EPI (%d)" \
               % (proc.blip_obl_rev, proc.dsets_obl))
         return

   # check for alignment to median forward blip base
   val, status = user_opts.get_string_opt('-volreg_align_to')
   if val == 'MEDIAN_BLIP':
      # matching the same variable in db_cmd_blip
      for_prefix = 'blip_med_for'
      inset = '%s%s' % (for_prefix, proc.view)
      set_vr_int_name(block, proc, 'vr_base_blip', inset=inset)

   block.valid = 1

# note: the input to 3dvolreg     should be the output from this
#       the input to 3dNwarpApply should be the input    to this
#       i.e. prev_prefix = proc.prev_prefix_form_run(block, view=1)
def db_cmd_blip(proc, block):
   """align median datasets for -blip_reverse_dset and current
      compute proc.blip_med_dset, proc.blip_dset_warp

      - get blip_NT from -blip_reverse_dset
      - extract that many from first current dset

      - compute forward and reverse median datasets
      - automask
      - compute warp
      - apply warp to each median masked vol, plus median forward unmasked vol
      - apply warp to EPI time series
         => INPUT to 3dNwarpApply in volreg block should be INPUT to blip block
            bblock = find_block('blip')
            inform = proc.prev_prefix_form_run(bblock, view=1)
            warp = proc.blip_dset_warp
      ** copy any obliquity information to results
   """

   if proc.blip_dset_rev == None and proc.blip_dset_warp == None:
      return ''

   blip_interp = '-quintic'

   # note the goal here, for the comment string
   if proc.blip_dset_warp is not None:
      taskstr = 'apply'
   else:
      taskstr = 'compute blip up/down'

   cmd =  "# %s\n" % block_header('blip')
   cmd += '# %s non-linear EPI distortion correction\n\n' \
          % taskstr

   # -----------------------------------------------------------------
   # actually compute the transformation (or else it was input)
   if proc.blip_dset_warp == None:
      bcmd = compute_blip_xform(proc, block, interp=blip_interp)
      if bcmd == '': return ''
   else:
      bcmd = '\n# no computation to do: have external -blip_warp_dset %s\n\n' \
             % proc.blip_dset_warp.shortinput()
   cmd += bcmd

   if proc.blip_dset_warp is None:
      print("** failed compute_blip_xform")
      return ''

   warp_for = proc.blip_dset_warp

   # -----------------------------------------------------------------
   # main result (besides actual xform): apply forward mid-warp to EPI
   inform = proc.prev_prefix_form_run(block, view=1, eind=0)
   outform = proc.prefix_form_run(block, eind=0)

   # possibly pass an obliquity dset, if the EPI is oblique
   if proc.dsets_obl:
       foblset = gen_afni_name(proc.prev_prefix_form_run(block, view=1, eind=0))
   else:
       foblset = None
   
   # potential extra indent if ME
   if proc.use_me: exind = '    '
   else:           exind = ''

   bstr = blip_warp_command(proc, warp_for.shortinput(), inform, outform,
           interp=blip_interp, oblset=foblset, indent='    '+exind)
   cmd += '# warp EPI time series data\n' \
          'foreach run ( $runs )\n'

   # ME: prepare to loop across echoes
   if proc.use_me:
      cmd += '%sforeach %s ( $echo_list )\n' % (exind, proc.echo_var[1:])

   # main loop
   cmd += bstr

   if proc.use_me:
      cmd += '%send\n' % exind

   cmd += 'end\n\n'

   return cmd

def compute_blip_xform(proc, block, interp='-quintic'):
   # compute the blip transformation

   proc.have_rm = 1            # rm.* files exist

   medf = proc.blip_dset_rev.new(new_pref='rm.blip.med.fwd')
   medr = proc.blip_dset_rev.new(new_pref='rm.blip.med.rev')

   cmd = ''
   cmd += '# create median datasets from forward and reverse time series\n' \
          '3dTstat -median -prefix %s %s\n'                                 \
          '3dTstat -median -prefix %s %s\n\n'                               \
          % (medf.out_prefix(), proc.blip_dset_for.shortinput(),
             medr.out_prefix(), proc.blip_dset_rev.shortinput())

   mmedf = medf.new(new_pref='rm.blip.med.masked.fwd')
   mmedr = medr.new(new_pref='rm.blip.med.masked.rev')
   cmd += '# automask the median datasets \n'           \
          '3dAutomask -apply_prefix %s %s\n'            \
          '3dAutomask -apply_prefix %s %s\n\n'          \
          % (mmedf.out_prefix(), medf.shortinput(),
             mmedr.out_prefix(), medr.shortinput())

   # get any 3dQwarp options
   olist, rv = block.opts.get_string_list('-blip_opts_qw')
   if olist and len(olist) > 0:
       other_opts = '%8s%s \\\n' % (' ', ' '.join(olist))
   else: other_opts = ''

   # -source is reverse, -base is forward (but does not matter, of course)
   # current prefix: simply blip_warp
   # rcr: todo add options to control Qwarp inputs
   warp_prefix = 'blip_warp'
   cmd += '# compute the midpoint warp between the median datasets\n' \
          '3dQwarp -plusminus -pmNAMES Rev For  \\\n'   \
          '        -pblur 0.05 0.05 -blur -1 -1 \\\n'   \
          '        -noweight -minpatch 9        \\\n'   \
          '%s'                                          \
          '        -source %s                   \\\n'   \
          '        -base   %s                   \\\n'   \
          '        -prefix %s\n\n'                      \
          % (other_opts, mmedr.shortinput(), mmedf.shortinput(), warp_prefix)

   # store forward warp dataset name, and note reverse warp dataset name
   warp_for = mmedf.new(new_pref=('%s_For_WARP'%warp_prefix))
   warp_rev = mmedf.new(new_pref=('%s_Rev_WARP'%warp_prefix))
   proc.blip_dset_warp = warp_for  # store for volreg block

   fobl = proc.blip_obl_for     # set earlier, since fwd might be from -dsets
   robl = proc.blip_obl_rev

   # apply mid-warp to forward median
   for_prefix = 'blip_med_for'
   rev_prefix = 'blip_med_rev'
   cmd += '# warp median datasets (forward and each masked) for QC checks\n'
   if fobl or robl: cmd += '# (and preserve obliquity)\n'

   # if oblique, pass the local copies
   if fobl: foblset = proc.blip_dset_for
   else:    foblset = None
   if robl: roblset = proc.blip_dset_rev
   else:    roblset = None

   cmd += blip_warp_command(proc, warp_for.shortinput(), medf.shortinput(),
                            for_prefix, oblset=foblset, interp=interp)
   cmd += '\n'
   proc.blip_dset_med = proc.blip_dset_warp.new(new_pref=for_prefix)

   # to forward masked median
   cmd += blip_warp_command(proc, warp_for.shortinput(), mmedf.shortinput(),
                    '%s_masked'%for_prefix, oblset=foblset, interp=interp)
   cmd += '\n'

   # to reverse masked median
   # if oblique, pass the local copy
   if fobl: oblset = proc.blip_dset_for
   else:    oblset = None
   cmd += blip_warp_command(proc, warp_rev.shortinput(), mmedr.shortinput(),
                    '%s_masked'%rev_prefix, oblset=roblset, interp=interp)
   cmd += '\n'

   return cmd

def dset_is_oblique(aname, verb):
   cmd = '3dinfo -is_oblique %s' % aname.rel_input()
   st, so, se = UTIL.limited_shell_exec(cmd)

   if verb > 2 or (verb > 1 and st):
      print('== dset_is_oblique cmd: %s' % cmd)
      print('       st = %s, so = %s, se = %s' % (st, so, se))


   if len(so) < 1:
      return 0
   elif so[0] == '1':
      return 1
   elif so[0] == 'NO-DSET':
      print("** warning: failed obliquity check for '%s'" % aname.rel_input())
      return 0
   else:
      return 0

def blip_warp_command(proc, warp, source, prefix, interp=' -quintic',
                      oblset=None, indent=''):
   if not interp:            intstr = ''
   elif interp[0] == '-':    intstr = ' %s' % interp
   else:                     intstr = interp

   cmd = '%s3dNwarpApply%s -nwarp %s \\\n' \
         '%s             -source %s \\\n'   \
         '%s             -prefix %s\n'      \
         % (indent, intstr, warp, indent, source, indent, prefix)

   if oblset:
      cmd += '%s3drefit -atrcopy %s \\\n'                \
             '%s                 IJK_TO_DICOM_REAL \\\n' \
             '%s                 %s%s\n'                 \
             % (indent, oblset.shortinput(), indent, indent, prefix, proc.view)

   return cmd


# --------------- align (anat2epi) ---------------

def db_mod_align(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-align_opts_aea', -1, [])
        block.opts.add_opt('-align_epi_strip_method', 1, ['3dAutomask'],
                           setpar=1)

    # general options for align_epi_anat.py
    uopt = user_opts.find_opt('-align_opts_aea')
    bopt = block.opts.find_opt('-align_opts_aea')
    if uopt and bopt: bopt.parlist = uopt.parlist

    # external EPI volume for align_epi_anat.py
    uopt = user_opts.find_opt('-align_epi_ext_dset')
    bopt = block.opts.find_opt('-align_epi_ext_dset')
    if uopt and not bopt:
        block.opts.add_opt('-align_epi_ext_dset', 1, uopt.parlist, setpar=1)
    elif uopt and bopt: bopt.parlist = uopt.parlist

    # check base_dset (do not allow with selector options)
    bopt = block.opts.find_opt('-align_epi_ext_dset')
    if bopt: proc.align_ebase = bopt.parlist[0]

    # maybe adjust EPI skull stripping method
    apply_uopt_to_block('-align_epi_strip_method', user_opts, block)
    apply_uopt_to_block('-align_unifize_epi', user_opts, block)
    apply_uopt_to_block('-align_opts_eunif', user_opts, block)

    block.valid = 1

# align anat to epi -> anat gets _al suffix to its prefix
#                   -> matrix is ${anat_prefix}_al.mat.aff12.1D
# (adjust prefix of proc.anat and proc.tlrcanat)
# set a2e xform matrix
def db_cmd_align(proc, block):
    if not proc.anat:
        print('** ERROR: missing anat for align block (consider -copy_anat)\n')
        return

    # first note EPI alignment base and sub-brick, as done in volreg block
    # (alignEA EPI and base might be given externally via -align_epi_base_dset)
    if proc.align_ebase != None:
        basepre = proc.align_epre
        basevol = "%s%s" % (proc.align_epre,proc.view)
        bind = 0
        # if using external anat base, run extra allineat to epi base
    elif proc.vr_ext_base != None or proc.vr_int_name != '':
        basepre = proc.vr_ext_pre
        basevol = "%s%s" % (proc.vr_ext_pre,proc.view)
        bind = 0
    else:
        rind, bind = proc.get_vr_base_indices()
        if rind < 0:
           rind, bind = 0, 0
           print('** warning: will use align base defaults: %d, %d'%(rind,bind))
           # return (allow as success now, for no volreg block)
        basepre = proc.prev_prefix_form(rind+1, block, view=0)
        basevol = proc.prev_prefix_form(rind+1, block, view=1)

    # should we unifize EPI?  if so, basevol becomes result
    ucmd = ''
    oname = '-align_unifize_epi'
    umeth, err = block.opts.get_string_opt(oname, default='no')
    if umeth != 'no' and not err:
       epi_in = gen_afni_name(basevol)
       epi_out = epi_in.new(new_pref=('%s_unif' % epi_in.prefix))
       basepre = epi_out.prefix
       basevol = epi_out.shortinput()
       # get any additional options
       opts, rv = block.opts.get_string_list('-align_opts_eunif')
       if opts:
          exopts = " \\\n    %s" % ' '.join(opts)
       else:
          exopts = ''

       # PT special: using 3dLocalUnifize
       if umeth == 'local':
           ucmd = '# run (localized) uniformity correction on EPI base\n' \
                  '3dLocalUnifize -input %s -prefix %s%s\n\n'             \
                  % (epi_in.shortinput(), epi_out.out_prefix(), exopts)
       # default = 'yes' or 'unif'
       else:
           ucmd = '# run uniformity correction on EPI base\n' \
                  '3dUnifize -T2%s -input %s -prefix %s\n\n'  \
                  % (exopts, epi_in.shortinput(), epi_out.out_prefix())

    # check for EPI skull strip method
    opt = block.opts.find_opt('-align_epi_strip_method')
    if opt and opt.parlist: essopt = "       -epi_strip %s \\\n"%opt.parlist[0]
    else:                   essopt = ""

    # add any user-specified options
    opt = block.opts.find_opt('-align_opts_aea')
    if opt and opt.parlist: extra_opts = "       %s \\\n" % \
                            ' '.join(UTIL.quotize_list(opt.parlist, '', 1))
    else:   extra_opts = ''

    has_skull = proc.anat_has_skull
    if has_skull: ss_opt = ''
    else:         ss_opt = '       -anat_has_skull no \\\n'
    # also, check for a user opt that specifies it
    if extra_opts.find('-anat_has_skull no') >= 0:
       has_skull = 0
       proc.anat_has_skull = 0
       ss_opt = ''      # user already passing it

    # note whether the aea output is expected to be used
    e2a = (proc.find_block_opt('volreg', '-volreg_align_e2a') != None)
    astr   = '' # maybe to save skullstrip dset
    if e2a: # if the option was passed, the output is junk
        suffix = '_al_junk'
        if has_skull: astr='-save_skullstrip '
    else:   # otherwise, we will use it
        suffix = '_al_keep'

    # write main command, write hdr after anat update
    cmd = 'align_epi_anat.py -anat2epi -anat %s \\\n'             \
          '       %s-suffix %s \\\n'                              \
          '       -epi %s -epi_base %d \\\n'                      \
          '%s'                                                    \
          '%s'                                                    \
          '%s'                                                    \
          '       -volreg off -tshift off\n\n'                    \
          % (proc.anat.pv(), astr, suffix, basevol, bind, essopt, ss_opt,
             extra_opts)

    # store the alignment matrix file for possible later use
    proc.a2e_mat = "%s%s_mat.aff12.1D" % (proc.anat.prefix, suffix)
    if not e2a: # store xform file
        proc.anat_warps.append(proc.a2e_mat)

    # ------------------------------------------------------------
    # if ext a2e base, align to volreg base via 'intermediate' xform
    # (a2e base should always be proc.vr_ext_pre now)
    #
    # align_epi_anat.py basically did anat2inter, append inter2epi
    if proc.align_ebase and proc.find_block('volreg'):
       if proc.verb > 0:
          print("++ align anat/epi base %s with epi/epi base %s" \
                % (basepre, proc.vr_ext_pre))

       # so aea really did anat2inter, compute inter2epi using another aea
       #  - intermediate was proc.align_ebase, real ebase is proc.vr_ext_pre
       #  - basevol is proc.align_ebase
       #  - use aea to handle potential obliquity difference
       #  - use basepre to track outputfile
       suffix = '_abase2ebase'
       # output is $basepre$suffix = ext_align_epi_abase2ebase
       acmd = '# have external anat2epi base, so align with local one\n' \
              'align_epi_anat.py -dset1to2 \\\n'    \
              '       -dset1 %s%s \\\n'             \
              '       -dset2 %s%s \\\n'             \
              '       -suffix %s \\\n'              \
              '       -dset1_strip 3dAutomask \\\n' \
              '       -dset2_strip 3dAutomask \\\n' \
              '       -giant_move -cost lpa \\\n'   \
              '       -volreg off -tshift off\n\n'  \
              % (basepre, proc.view, proc.vr_ext_pre, proc.view, suffix)

       pre_i2e   = "%s%s" % (basepre, suffix)
       mat_i2e   = '%s_mat.aff12.1D' % pre_i2e
       pre_cat   = 'anat2epi_concat'
       mat_cat   = '%s_mat.aff12.1D' % pre_cat
       mat_a2i   = proc.a2e_mat # old mat is anat to inter
       # now concatenate the xforms so EPI ~= outer(inner(anat))
       # - so xform a2e = i2e * a2i
       acmd += '# and concatenate the affine transformations\n' \
               '# (make %s the full anat to EPI base xform)\n'  \
               'cat_matvec -ONELINE \\\n'                       \
               '           %s \\\n'                             \
               '           %s \\\n'                             \
               '         > %s\n\n'                              \
               % (mat_cat, mat_i2e, mat_a2i, mat_cat)

       # now a2e_mat is the concatenated anat to epi base
       proc.a2e_mat = mat_cat
       cmd += acmd


    # ------------------------------------------------------------
    # if e2a:   update anat and tlrc to '_ss' version (intermediate, stripped)
    #           (only if skull: '-anat_has_skull no' not found in extra_opts)
    #           (not if using adwarp)
    # else a2e: update anat and tlrc to 'keep' version
    # (in either case, ss will no longer be needed)
    if e2a:
        adwarp = (proc.find_block_opt('volreg', '-volreg_tlrc_adwarp') != None)
        if has_skull and not adwarp:
            suffix = '_ns'
            proc.anat.prefix = "%s%s" % (proc.anat.prefix, suffix)
            if proc.tlrcanat:
                if not proc.tlrcanat.exist():
                   proc.tlrcanat.prefix = "%s%s" % (proc.tlrcanat.prefix,suffix)
            proc.tlrc_ss = 0
            proc.anat_has_skull = 0     # make note that skull is gone
            istr = 'intermediate, stripped,'
        else: # just set istr
            istr = 'current'
        astr = 'for e2a: compute anat alignment transformation'
    else: # a2e
        proc.anat.prefix = "%s%s" % (proc.anat.prefix, suffix)
        if proc.tlrcanat:
            if not proc.tlrcanat.exist():
               proc.tlrcanat.prefix = "%s%s" % (proc.tlrcanat.prefix, suffix)
        proc.tlrc_ss = 0        # skull-strip no longer required
        proc.anat_has_skull = 0 # make note that skull is gone

        # also, set strings for header
        istr = 'aligned and stripped,'
        astr = 'a2e: align anatomy'

    # now that proc.anat has been updated, write header, still depending
    # on e2a or a2e direction
    hdr = '# %s\n'                              \
          '# %s to EPI registration base\n'     \
          '# (new anat will be %s %s)\n'        \
          % (block_header('align'), astr, istr, proc.anat.pv())

    # ---------------
    # if requested, create any anat followers
    if should_warp_anat_followers(proc, block):
        rv, tcmd = warp_anat_followers(proc, block, proc.anat, prevepi=1)
        if rv: return
        if tcmd: cmd += tcmd

    # used 3dvolreg, so have these labels
    # note the alignment in EPIs warp bitmap (2=a2e)
    proc.warp_epi |= WARP_EPI_ALIGN_A2E

    return hdr + ucmd + cmd

# --------------- despike ---------------

def db_mod_despike(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-despike_opts_3dDes', -1, [])

    apply_uopt_to_block('-despike_opts_3dDes', user_opts, block)
    apply_uopt_to_block('-despike_mask', user_opts, block)
    apply_uopt_to_block('-despike_new', user_opts, block)

    block.valid = 1

# apply 3dDespike to each run
def db_cmd_despike(proc, block):

    cmd = ''

    # see if the user has provided other options
    opt = block.opts.find_opt('-despike_opts_3dDes')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = ' %s' %      \
               ' '.join(UTIL.quotize_list(opt.parlist, '', 1))

    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(block, view=1)

    # maybe the user wants to mask here (to speed this step up)
    #
    # was applied as -despike_opts_mask, fixed by D Plunkett 15 Nov 2017 [rickr]
    mstr = ' -nomask'
    if block.opts.find_opt('-despike_mask'):
       # require -mask_apply epi
       okay = 0
       opt = proc.find_block_opt('mask', '-mask_apply')
       if opt:
          okay = opt.parlist[0] == 'epi'
       if not okay:
          print("** option -despike_mask requires -mask_apply epi")
          return
       mstr = ''

    newstr = get_despike_new_opt_str(block, oname='-despike_new')

    # write commands
    cmd = cmd + '# %s\n'                            \
                '# apply 3dDespike to each run\n'   \
                'foreach run ( $runs )\n'           \
                % block_header('despike')

    indent = ''
    if proc.use_me:
       indent = '    '
       cmd += '%sforeach %s ( $echo_list )\n' % (indent, proc.echo_var[1:])

    cmd = cmd + '%s    3dDespike%s%s%s -prefix %s %s\n'   \
                '%send\n' %                               \
                (indent, newstr, other_opts, mstr, prefix, prev, indent)

    if proc.use_me:
       cmd += 'end\n'
    cmd += '\n'

    return cmd

def get_despike_new_opt_str(block, oname='-despike_new'):
    """return any appropriate 3dDespike option, like -NEW or -NEW25"""
    newstr = ''

    ntype, err = block.opts.get_string_opt(oname, default='-NEW')
    if err: return newstr

    if ntype not in g_despike_new_opts:
       print("** %s parameter %s not in {%s}" \
             % (oname, ntype, ','.join(g_despike_new_opts)))
       return newstr

    if ntype == 'no':
       newstr = ''
    elif ntype == 'yes':
       # default
       newstr = ' -NEW'
    else:
       # take what we are given
       newstr = ' %s' % ntype

    return newstr

# --------------- ricor: retroicor ---------------

# copy regs is call from init_script, after all db_mod functions
def copy_ricor_regs_str(proc):
    """make a string to copy the retroicor regressors to the results dir"""
    if len(proc.ricor_regs) < 1: return ''

    # maybe remove final TRs as well, do a little work here...
    trs = []
    lstr = ''
    if proc.ricor_nlast > 0:
        try:
            from afnipy import lib_afni1D as LAD
            for reg in proc.ricor_regs:
                adata = LAD.Afni1D(reg)
                trs.append(adata.nt)
        except:
            print('** failing to remove last %d TRs' % proc.ricor_nlast)
            return ''
        lstr = 'and last %d' % proc.ricor_nlast

    str = '# copy slice-based regressors for RETROICOR (rm first %d %sTRs)\n' \
          % (proc.ricor_nfirst, lstr)

    if proc.ricor_nfirst > 0: offstr = "'{%d..$}'" % proc.ricor_nfirst
    else:                     offstr = ''

    offstr = ''         # default
    lstr   = '$'
    for ind in range(len(proc.ricor_regs)):
        if proc.ricor_nfirst > 0 or proc.ricor_nlast > 0:
            if proc.ricor_nlast > 0: lstr = '%d' % (trs[ind]-1-proc.ricor_nlast)
            offstr = "'{%d..%s}'" % (proc.ricor_nfirst, lstr)
        str += '1dcat %s%s > %s/stimuli/ricor_orig_r%02d.1D\n' % \
               (proc.ricor_regs[ind], offstr, proc.od_var, ind+1)

    return str

# options:
#
# -ricor_regress_polort 0
# -ricor_regress_solver OLSQ/REML
# -ricor_opts_reml *
def db_mod_ricor(block, proc, user_opts):
    # note: regs and nfirst are passed to proc instance, not to block
    # set the regressor list
    uopt = user_opts.find_opt('-ricor_regs')
    if not uopt:
        print("** missing ricor option: '-ricor_regs'")
        return
    if len(uopt.parlist) < 1:
        print("** missing '-ricor_regs' regressor list")
        return
    proc.ricor_regs = uopt.parlist

    # delete nfirst trs from start of each run
    val, err = user_opts.get_type_opt(int, '-ricor_regs_nfirst')
    if err: return
    elif val != None and val >= 0: proc.ricor_nfirst = val

    # delete nlast trs from end of each run
    val, err = user_opts.get_type_opt(int, '-ricor_regs_rm_nlast')
    if err: return
    elif val != None and val >= 0: proc.ricor_nlast = val

    # --------- setup options to pass to block ------------
    if len(block.opts.olist) == 0:
        block.opts.add_opt('-ricor_polort', 1, [-1], setpar=1)
        block.opts.add_opt('-ricor_regress_solver', 1, ['OLSQ'], setpar=1)
        block.opts.add_opt('-ricor_regress_method', 1, ['across_runs'],setpar=1)

    # --------- process user options ------------

    uopt = user_opts.find_opt('-ricor_datum')
    bopt = block.opts.find_opt('-ricor_datum')
    if uopt: # either replace block's opt or create it
        if bopt: bopt.parlist[0] = uopt.parlist[0]
        else: block.opts.add_opt('-ricor_datum', 1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-ricor_polort')
    bopt = block.opts.find_opt('-ricor_polort')
    if uopt and bopt: bopt.parlist[0] = uopt.parlist[0]

    uopt = user_opts.find_opt('-ricor_regress_solver')
    bopt = block.opts.find_opt('-ricor_regress_solver')
    if uopt and bopt: bopt.parlist[0] = uopt.parlist[0]

    uopt = user_opts.find_opt('-ricor_regress_method')
    bopt = block.opts.find_opt('-ricor_regress_method')
    if uopt:
        if bopt: bopt.parlist[0] = uopt.parlist[0]
        else: bopt.parlist[0] = uopt.parlist[0]

    block.valid = 1

def db_cmd_ricor(proc, block):
    #----- check for problems -----
    # check regressors against num runs
    if len(proc.ricor_regs) != proc.runs:
        print('** ERROR: have %d runs but %d slice-base ricor regressors' % \
              (proc.runs, len(proc.ricor_regs)))
        return

    # ME: implement multi-echo once someone wants it...
    # hopefully ready now   3 Jun 2019

    # get datum, if set
    rdatum, err = block.opts.get_string_opt('-ricor_datum')
    if err: return
    # if no option and input was unscaled shorts, convert output back to it
    if rdatum == None:
        if proc.datatype == 1 and proc.scaled == 0:
          if proc.verb > 0:
            print('-- ricor: have unscaled short input, will revert back to it')
          rdatum = 'short' # treat as unscaled short
        else: rdatum = 'float'
    # we might want to force the -float option in 3dDeconvolve
    if rdatum == 'float': proc.datatype = 3

    # get regress method (will only currently work as 'per-run')
    rmethod, err = block.opts.get_string_opt('-ricor_regress_method')
    if err or rmethod == None:
        print("** ERROR: option -ricor_regress_method required for ricor block")
        return

    # get nslices
    err, dims = UTIL.get_typed_dset_attr_list(proc.dsets[0].rel_input(),
                                              "DATASET_DIMENSIONS", int)
    if err or len(dims) < 4:
        print('** ERROR: failed to get DIMENSIONS from %s' \
              % proc.dsets[0].rel_input())
        return
    nslices = dims[2]
    if proc.verb > 2: print('-- ricor: found nslices = %d' % nslices)

    # check regressors against nslices and nTR (also check reg[0] existence)
    adata = None
    try:
        from afnipy import lib_afni1D as LAD
        adata = LAD.Afni1D(proc.ricor_regs[0], verb=proc.verb)
    except: pass
    if not adata or not adata.ready:
        print("** ERROR: failed to read '%s' as Afni1D" % proc.ricor_regs[0])
        return
    nsr_labs = adata.labs_matching_str('s0.')
    # might have zero-padded labels now
    if len(nsr_labs) == 0: nsr_labs = adata.labs_matching_str('s000.')
    nsliregs = adata.nvec // nslices
    nlab = len(nsr_labs)
    if nlab > 0: nrslices = adata.nvec//nlab
    else:        nrslices = 0
    # if not evenly divided by nslices
    if nsliregs * nslices != adata.nvec:
        print("** ERROR: ricor nsliregs x nslices != nvec (%d, %d, %d)\n"   \
              "   (%d ricor slices, %d volume slices, %d slice 0 labels)\n" \
              % (nsliregs, nslices, adata.nvec, nrslices, nslices, nlab))
        return

    # if have apparent slices, but they seem to differ
    if nrslices > 0 and nrslices != nslices:
        print("** ERROR: %d apparent ricor slices but %d EPI slices" \
              % (nrslices, nslices))
        return

    # do not expect 13 sliregs anymore - remove warning

    if proc.verb > 0:
       print('-- ricor: nsliregs = %d, nslices = %d, # slice 0 labels = %d' \
             % (nsliregs, nslices, len(nsr_labs)))
    if proc.verb > 2: print('-- ricor: slice 0 labels: %s' % ' '.join(nsr_labs))

    # check reps against adjusted NT
    for run, rfile in enumerate(proc.ricor_regs):
        fdata = LAD.Afni1D(rfile)
        nt = fdata.nt-proc.ricor_nfirst-proc.ricor_nlast
        if proc.reps_all[run] != nt:
           print("** ERROR: run %d ricor NT != dset len (%d, %d)\n"       \
                 "   (check -ricor_regs_nfirst/-tcat_remove_first_trs)"   \
                 % (run+1, nt, proc.reps_all[run]))
           return

    # get user polort, else default based on twice the time length
    val, err = block.opts.get_type_opt(int, '-ricor_polort')
    if err: return
    elif val != None and val >= 0: polort = val
    else: polort = UTIL.get_default_polort(2*proc.tr, proc.reps)

    val, err = block.opts.get_string_opt('-ricor_regress_solver')
    if not err and val == 'REML': solver = 'R'
    else:                         solver = 'O'

    if proc.verb > 0:
        sstr = 'OLSQ'
        if solver == 'R': sstr = 'REML'
        print("-- ricor: processed '%s' using '%s' on %d sliregs" \
              % (rmethod, sstr, nsliregs))

    #----- everything seems okay, write command string -----

    if rmethod == 'per-run':
        cmd = ricor_process_per_run(proc, block, polort,solver,nsliregs,rdatum)
    else:
        cmd = ricor_process_across_runs(proc, block, polort, solver,
                                        nsliregs, rdatum)

    return cmd

def ricor_process_across_runs(proc, block, polort, solver, nsliregs, rdatum):
    """- for each run: 3dDetrend polort from regressors
       - 1dcat all s0 regressors together for "dummy" in regress process block
       - 3dD -input ALL -polort -x1D -x1D_stop
       - 3dREMLfit -input -matrix -Rerrts -Rbeta? -slibase_sm -verb?
       - 3dSynthesize -matrix -cbucket -select baseline -prefix
       - for each run: 3dcalc -a errts -b baseline -expr a+b -prefix pbXX.ricor
    """

    prev_dsets = proc.prev_dset_form_wild(block, view=1)
    cur_prefix = proc.prefix_form_run(block)
    prebase    = 'pb%02d.ricor' % block.index
    matrix     = '%s.xmat.1D' % prebase
    # do not save these files by default, except for the matrices
    prefix      = 'rm.' + prebase

    prev_x = proc.prev_dset_form_wild(block, view=1, eind=-1)

    # we have a regressor file to pass to the regress processing block
    proc.ricor_reg = 'stimuli/ricor_s0_rall.1D'
    proc.ricor_nreg = nsliregs

    cmd = '# %s\n'                                                      \
          '# RETROICOR - remove cardiac and respiratory signals\n'      \
          '#           - across runs: catenate regressors across runs\n'\
          % block_header('ricor')

    # create QC directory for variance ratios
    qcdir = 'ricor_QC'
    cmd = cmd + ricor_qc_mkdir(qcdir)

    cmd = cmd +                                                         \
        "foreach run ( $runs )\n"                                       \
        "    # detrend regressors (make orthogonal to poly baseline)\n" \
        "    3dDetrend -polort %d -prefix rm.ricor.$run.1D \\\n"        \
        "              stimuli/ricor_orig_r$run.1D\\'\n\n"              \
        "    1dtranspose rm.ricor.$run.1D rm.ricor_det_r$run.1D\n" % polort
    cmd = cmd + "end\n\n"
    proc.have_rm = 1            # rm.* files exist

    cmd = cmd +                                                 \
        "# put ricor regressors into a single file for each regression\n\n"

    cmd = cmd +                                                 \
        "# ... catenate all runs for current 'ricor' block\n"   \
        "cat rm.ricor_det_r[0-9]*.1D > stimuli/ricor_det_rall.1D\n\n"

    cmd = cmd +                                                 \
        "# ... extract slice 0, for future 'regress' block\n"   \
        "1dcat stimuli/ricor_det_rall.1D'[0..%d]' > %s\n\n"     \
        % (nsliregs-1, proc.ricor_reg)

    cmd = cmd +                                                 \
        "# create (polort) X-matrix to apply in 3dREMLfit\n"    \
        "3dDeconvolve -polort %d -input %s \\\n"                \
        "    -x1D_stop -x1D %s\n\n"                             \
        % (polort, prev_x, matrix)

    # process 3D+time data
    if proc.use_me:
       eistr = '.e$eind'
       indent = '   '
       cmd += "# process one echo at a time, across runs\n" \
              "foreach eind ( $echo_list )\n"
    else:
       eistr = ''
       indent = ''

    # set more complete variables, to include QC
    eprefix = prefix + eistr
    errset = '%s.errts' % eprefix
    detset = '%s.det' % eprefix

    cmd = cmd +                                                      \
        "%s# 3dREMLfit does not currently catenate a dataset list\n" \
        "%sset dsets = ( %s )\n\n" % (indent, indent, prev_dsets)

    cmd = cmd +                                                   \
        "%s# regress out the detrended RETROICOR regressors\n"    \
        "%s# (matrix from 3dD does not have slibase regressors)\n"\
        '%s3dREMLfit -input "$dsets" \\\n'                        \
        "%s    -matrix %s \\\n"                                   \
        "%s    -%sbeta %s.betas \\\n"                             \
        "%s    -%serrts %s \\\n"                                  \
        "%s    -slibase_sm stimuli/ricor_det_rall.1D\n\n"         \
        % (indent, indent, indent, indent, matrix,
           indent, solver, eprefix,
           indent, solver, errset, indent)

    cmd = cmd +                                                   \
        "%s# re-create polynomial baseline\n"                     \
        "%s3dSynthesize -matrix %s \\\n"                          \
        "%s    -cbucket %s.betas%s'[0..%d]' \\\n"                 \
        "%s    -select polort -prefix %s.polort\n\n"              \
        % (indent, indent, matrix,
           indent, eprefix, proc.view, (polort+1)*proc.runs-1,
           indent, eprefix)

    # short data is unscaled, float is the default, else convert to short
    if   rdatum == 'short':
       dstr = '%s           -datum short -nscale \\\n' % indent
    elif rdatum == 'float':
       dstr = ''
    else:
       dstr = '%s           -datum %s \\\n' % (indent, rdatum)

    cmd = cmd +                                                     \
        "%s# final result: add REML errts to polynomial baseline\n" \
        "%s# (and separate back into individual runs)\n"            \
        "%sset startind = 0\n"                                      \
        "%sforeach rind ( `count_afni -digits 1 1 $#runs` )\n"          \
        "%s    set run = $runs[$rind]\n"                            \
        "%s    set runlen = $tr_counts[$rind]\n"                    \
        "%s    @ endind = $startind + $runlen - 1\n"                \
        "\n" % (indent, indent, indent, indent, indent, indent, indent)

    cmd = cmd +                                                     \
        '%s    3dcalc -a %s%s"[$startind..$endind]" \\\n'           \
        '%s           -b %s.polort%s"[$startind..$endind]" \\\n'    \
        '%s'                                                        \
        '%s           -expr a+b -prefix %s\n'                       \
        "%s    @ startind = $endind + 1\n"                          \
        % (indent, errset, proc.view,
           indent, eprefix, proc.view, dstr,
           indent, cur_prefix, indent)
    cmd = cmd + '%send\n' % indent


    # QC: create string for variance maps (1 and '    ' for perrun)
    cmd = cmd + ricor_qc_varmaps(proc, qcdir, '"$dsets"', matrix,
                     '%s%s'%(errset,proc.view), detset, eistr, 0, indent)

    if proc.use_me:
       cmd = cmd + 'end\n'

    cmd = cmd + '\n'

    return cmd

def ricor_process_per_run(proc, block, polort, solver, nsliregs, rdatum):
    """for each run:
         - 3dDetrend polort from regressors
         - 3dD -input -polort -x1D -x1D_stop
         - 3dREMLfit -input -matrix -Rerrts -Rbeta? -slibase_sm -verb?
         - 3dSynthesize -matrix -cbucket -select baseline -prefix
         - 3dcalc -a errts -b baseline -expr a+b -prefix pbXX.ricor
       - 1dcat all s0 regressors together for "dummy" in regress process block
    """


    prev_prefix = proc.prev_prefix_form_run(block, view=1)
    cur_prefix  = proc.prefix_form_run(block)
    prefix      = 'pb%02d.ricor' % block.index
    matrix      = '%s.r$run.xmat.1D' % prefix
    # do not save these files by default, except for the matrices
    prefix      = 'rm.' + prefix

    # we have a regressor file to pass to the regress processing block
    proc.ricor_reg = 'stimuli/ricor_s0_rall.1D'
    proc.ricor_nreg = proc.runs * nsliregs

    cmd = '# %s\n'                                                      \
          '# RETROICOR - remove cardiac and respiratory signals\n'      \
          '#           - per run: each run uses separate regressors\n'  \
          % block_header('ricor')

    # create QC directory for variance ratios
    qcdir = 'ricor_QC'
    cmd = cmd + ricor_qc_mkdir(qcdir)

    cmd = cmd +                                                            \
        "foreach run ( $runs )\n"                                          \
        "    # detrend regressors (make orthogonal to poly baseline)\n"    \
        "    3dDetrend -polort %d -prefix rm.ricor.$run.1D \\\n"           \
        "              stimuli/ricor_orig_r$run.1D\\'\n\n"                 \
        "    1dtranspose rm.ricor.$run.1D stimuli/ricor_det_r$run.1D\n\n"  \
        "    # pad slice0 regressors across all runs (for 'regress' block)\n" \
        "    1d_tool.py -infile stimuli/ricor_det_r$run.1D'[0..%d]' \\\n"  \
        "               -set_run_lengths $tr_counts \\\n"                  \
        "               -pad_into_many_runs $run $#runs \\\n"              \
        "               -write rm.ricor_s0_r$run.1D\n\n"                 % \
        (polort, nsliregs-1)
    proc.have_rm = 1            # rm.* files exist

    # process 3D+time data

    # if ME, prev_prefix should be fave_echo
    prev_x = proc.prev_prefix_form_run(block, view=1, eind=-1)
    if proc.use_me:
       indent = '   '
       eistr = '.e$eind'
    else:
       indent = ''
       eistr = ''

    cmd = cmd +                                                   \
        "    # create (polort) X-matrix to apply in 3dREMLfit\n"\
        "    3dDeconvolve -polort %d -input %s \\\n"            \
        "        -x1D_stop -x1D %s\n\n" %                       \
        (polort, prev_x, matrix)

    if proc.use_me:
       cmd += '    # project out from each echo\n' \
              '    foreach eind ( $echo_list )\n'

    cmd = cmd +                                                   \
        "%s    # regress out the detrended RETROICOR regressors\n"\
        "%s    3dREMLfit -input %s \\\n"                          \
        "%s        -matrix %s \\\n"                               \
        "%s        -%sbeta %s.betas.r$run%s \\\n"                 \
        "%s        -%serrts %s.errts.r$run%s \\\n"                \
        "%s        -slibase_sm stimuli/ricor_det_r$run.1D\n\n"    \
        % (indent, indent, prev_prefix, indent, matrix,
           indent, solver, prefix, eistr, indent, solver, prefix, eistr, indent)
    cmd = cmd +                                                   \
        "%s    # re-create polynomial baseline\n"                 \
        "%s    3dSynthesize -matrix %s \\\n"                      \
        "%s        -cbucket %s.betas.r$run%s%s'[0..%d]' \\\n"     \
        "%s        -select polort -prefix %s.polort.r$run%s\n\n"  \
        % (indent, indent, matrix, indent, prefix, eistr, proc.view, polort,
           indent, prefix, eistr)

    # short data is unscaled, float is the default, else convert to rdatum
    if rdatum == 'short':
       dstr = '%s          -datum short -nscale \\\n' % indent
    elif rdatum == 'float':
       dstr = ''
    else:
       dstr = '%s          -datum %s \\\n' % (indent, rdatum)

    errset = '%s.errts.r$run%s%s' % (prefix, eistr, proc.view)
    detset = '%s.det.r$run%s'     % (prefix, eistr)
    cmd = cmd +                                                         \
        "%s    # final result: add REML errts to polynomial baseline\n" \
        "%s    3dcalc -a %s \\\n"                                       \
        "%s           -b %s.polort.r$run%s%s \\\n"                      \
        '%s'                                                            \
        "%s           -expr a+b -prefix %s\n"                           \
        % (indent,
           indent, errset,
           indent, prefix, eistr, proc.view,
           dstr, indent, cur_prefix)

    # QC: create string for variance maps (1 and '    ' for perrun)
    cmd = cmd + ricor_qc_varmaps(proc, qcdir, prev_prefix, matrix,
                                 errset, detset, eistr, 1, indent+'    ')

    if proc.use_me:
       cmd = cmd + "    end\n\n"

    # end of foreach loop encompassing ricor block
    cmd = cmd + "end\n\n"

    cmd = cmd +                                                            \
        "# put ricor regressors into a single file for 'regress' block\n"  \
        "1dcat rm.ricor_s0_r[0-9]*.1D > %s\n\n" % proc.ricor_reg

    return cmd

def ricor_qc_varmaps(proc, qcdir, inset, polmat, errset, detset,
                     eistr, perrun, indent):
   """create QC variance ratio dset, and intermediate stdev maps

      given:
           qcdir   - qc directory name
           inset   - orig dset
           polmat  - polort X-matrix (xmat.1D)
           errset  - errts from regression
           eistr   - echo index string, in case we write per echo
           perrun  - is this per run (do we need a $run variable)?
           indent  - indentation string
      create:
           detset  - detrend dset (no +view)
           STATS:  
              ricor.sd.{orig,ricor} : stdev of detrended time series
              ricor.vrat.m1         : variance ratio minus 1
   """

   # 4 cases for a comment string...
   cstr = ''
   if eistr != '':
      cstr = 'per echo'
   if perrun:
      if cstr == '':    # new comment
         cstr += 'per run'
      else:             # already have per echo
         cstr += ' and run'
   if cstr != '':
      cstr = ' (%s)' % cstr

   # input: prev_prefix, matrix (polort)
   qcstr = "\n"                                                        \
       "%s# -------------------------------------------------------\n" \
       "%s# QC: compute stdev and variance ratio%s\n"                  \
       "\n"                                                            \
       "%s# remove polort baseline from original (for variance)\n"     \
       "%s3dTproject -polort 0 -ort %s \\\n"                           \
       "%s           -input %s \\\n"                                   \
       "%s           -prefix %s\n\n"                                   \
       % (indent, indent, cstr, indent,
          indent, polmat, indent, inset,
          indent, detset)

   # include $run var if needed
   if perrun: rstr = '.r$run'
   else:      rstr = ''

   stdev_orig  = 'ricor.sd.orig%s%s' % (rstr, eistr)
   stdev_ricor = 'ricor.sd.ricor%s%s' % (rstr, eistr)
   var_rat     = 'ricor.vrat.m1%s%s' % (rstr, eistr)

   qcstr = qcstr +                                              \
       "%s# compute stdev maps: orig and ricor (detrended)\n"   \
       "%s3dTstat -stdevNOD -prefix %s/%s \\\n"                 \
       "%s        %s%s\n"                                       \
       "%s3dTstat -stdevNOD -prefix %s/%s \\\n"                 \
       "%s        %s\n\n"                                       \
       % (indent, indent, qcdir, stdev_orig, indent, detset, proc.view,
                  indent, qcdir, stdev_ricor, indent, errset)

   qcstr = qcstr +                                                    \
       "%s# compute variance ratio minus 1, removing small values\n"  \
       "%s# (ratio>1, so subtract 1 to show only frac improvement)\n" \
       "%s3dcalc -a %s/%s%s \\\n"                                     \
       "%s       -b %s/%s%s \\\n"                                     \
       "%s       -expr 'step(a-1)*step(b-1)*(a**2/b**2-1)' \\\n"      \
       "%s       -prefix %s/%s\n"                                     \
       % (indent, indent,
          indent, qcdir, stdev_orig, proc.view,
                  indent, qcdir, stdev_ricor, proc.view,
          indent, indent, qcdir, var_rat)

   return qcstr

def ricor_qc_mkdir(qcdir):
    return "\n# store variance maps in QC directory\n" \
           "mkdir %s\n\n" % qcdir

# --------------- tshift ---------------

def db_mod_tshift(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-tshift_align_to', -1, ['-tzero', '0'], setpar=1)
        block.opts.add_opt('-tshift_interp', 1, ['-quintic'], setpar=1)
        block.opts.add_opt('-tshift_opts_ts', -1, [])

    # check for updates to -tshift_align_to option
    uopt = user_opts.find_opt('-tshift_align_to')
    bopt = block.opts.find_opt('-tshift_align_to')
    if uopt and bopt:
        bopt.parlist = uopt.parlist     # copy new params
        # warn the user about -regress_stim_times_offset
        if user_opts.find_opt('-regress_stim_files') and proc.verb > 0   \
           and not user_opts.find_opt('-regress_stim_times_offset')      \
           and not user_opts.find_opt('-regress_no_stim_times')          \
           and not user_opts.find_opt('-regress_use_stim_files'):
          print('-----------------------------------------------------------\n'\
                '** warning: using -tshift_align_to and -regress_stim_files\n' \
                '   --> if temporal alignment is not to the beginning of the\n'\
                '       TR, consider: -regress_stim_times_offset\n'            \
                '-----------------------------------------------------------')
    # check for updates to -tshift_interp option
    uopt = user_opts.find_opt('-tshift_interp')
    bopt = block.opts.find_opt('-tshift_interp')
    if uopt and bopt:
        bopt.parlist = uopt.parlist     # copy new params

    uopt = user_opts.find_opt('-tshift_opts_ts')
    bopt = block.opts.find_opt('-tshift_opts_ts')
    if uopt and bopt: bopt.parlist = uopt.parlist

    block.valid = 1

# run 3dTshift for each run
def db_cmd_tshift(proc, block):
    cmd = ''
    # get the base options
    opt = block.opts.find_opt('-tshift_align_to')
    align_to = ' '.join(opt.parlist)  # maybe '-tzero 0'
    opt = block.opts.find_opt('-tshift_interp')
    resam = ' '.join(opt.parlist)     # maybe '-quintic'

    # note cur and prev prefix forms (with $run)
    cur_prefix = proc.prefix_form_run(block)
    prev_prefix = proc.prev_prefix_form_run(block, view=1)

    # ME updates
    if proc.use_me: indent = '    '
    else:           indent = ''

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-tshift_opts_ts')
    if not opt or not opt.parlist:
       other_opts = ''
    else:
       other_opts = '%s             %s \\\n' % (indent,' '.join(opt.parlist))
       # if -tpattern is in this list, apply it is a uvar
       oname = '-tpattern'
       if oname in opt.parlist:
          tind = opt.parlist.index(oname)
          if tind < len(opt.parlist) - 1:
             # init tpat, but if '@', try to figure it out
             mb = 1
             tpat = opt.parlist[tind+1]
             if tpat.startswith('@'):
                try:
                    adata = LD.Afni1D(filename=tpat[1:])
                    mb, tpat = adata.get_tpattern()
                    if proc.verb > 1:
                       print("-- found @ timing: mb %s, tpat %s" % (mb, tpat))
                except:
                    print("** failed to get timing pattern from", tpat)
             proc.uvars.set_var('slice_pattern', [tpat])
             if mb > 1: proc.uvars.set_var('mb_level', ['%s' % mb])

    # write commands
    cmd = cmd + '# %s\n'                                                \
                '# time shift data so all slice timing is the same \n'  \
                'foreach run ( $runs )\n'                               \
                % block_header('tshift')

    if proc.use_me:
       cmd += '%sforeach eind ( $echo_list )\n' % indent

    cmd += '%s    3dTshift %s %s -prefix %s \\\n'                  \
           '%s'                                                    \
           '%s             %s\n'                                   \
           '%send\n'                                               \
           % (indent, align_to, resam, cur_prefix, other_opts,
              indent, prev_prefix, indent)

    if proc.use_me: cmd += 'end\n'

    cmd += '\n'

    return cmd

def vr_do_min_outlier(block, proc, user_opts):
   """ set up use of min outlier volume as volreg base
   1. if not computing outliers, whine and return
   2. after outlier command: set $minoutrun, $minouttr

   this volume will now be extracted elsewhere  26 Apr 2016

   ** note: this function is run *before* any  db_cmd_ function
   """

   # 1. are we computing outliers?
   if not proc.user_opts.have_yes_opt('outlier_count', default=1):
      print('** cannot use min outlier volume without outlier counts')
      print("   (consider '-outlier_count yes')")
      return 1

   # let the user know, and init vr vars
   if proc.verb: print("-- will use min outlier volume as motion base")
   set_vr_int_name(block, proc, 'vr_base_min_outlier', runstr='$minoutrun',
                                                       trstr='"[$minouttr]"')

   # 2. assign $minout{run,tr}
   pblock = proc.find_block('postdata')
   if not block:
      print('** vr_do_min_outlier: missing postdata block')
      return 1
   outtxt = 'out.min_outlier.txt'
   pblock.post_cstr += \
      "# get run number and TR index for minimum outlier volume\n"      \
      "set minindex = `3dTstat -argmin -prefix - outcount_rall.1D\\'`\n"\
      "set ovals = ( `1d_tool.py -set_run_lengths $tr_counts \\\n"      \
      "                          -index_to_run_tr $minindex` )\n"       \
      "# save run and TR indices for extraction of %s\n"                \
      "set minoutrun = $ovals[1]\n"                                     \
      "set minouttr  = $ovals[2]\n"                                     \
      'echo "min outlier: run $minoutrun, TR $minouttr" | tee %s\n\n'   \
      % (proc.vr_ext_pre, outtxt)

   proc.vr_base_MO = 1  # note that vr_base is MIN_OUTILER

def set_vr_int_name(block, proc, prefix='', inset='', runstr='', trstr=''):
   """common usage: svin(b,p, 'vr_base_min_outlier',
                         '$minoutrun', '"[$minouttr]"')
                or: svin(b,p, 'vr_base', inset='DSET+orig')
   """

   if prefix == '' or (inset == '' and runstr == ''):
      print('** SVIN: bad run = %s, prefix = %s' % (runstr, prefix))
      return 1

   # already set?
   if proc.vr_int_name != '': return 0

   # handle ME data
   if proc.use_me: estr = '.e%s' % proc.regecho_var
   else:           estr = ''

   if inset != '':
      proc.vr_int_name = inset
   else:
      proc.vr_int_name = 'pb%02d.$subj.r%s%s.%s%s%s' \
                         % (block.index-1, runstr, estr, proc.prev_lab(block),
                            proc.view, trstr)
   proc.vr_ext_pre = prefix

   return 0

def db_mod_volreg(block, proc, user_opts):
    if len(block.opts.olist) == 0:   # init dset/brick indices to defaults
        block.opts.add_opt('-volreg_base_ind', 2, [0, 2], setpar=1)
        block.opts.add_opt('-volreg_compute_tsnr', 1, ['no'], setpar=1)
        block.opts.add_opt('-volreg_interp', 1, ['-cubic'], setpar=1)
        block.opts.add_opt('-volreg_opts_vr', -1, [])
        block.opts.add_opt('-volreg_zpad', 1, [1], setpar=1)

    # check for updates to -volreg_base option
    uopt = user_opts.find_opt('-volreg_base_ind')
    bopt = block.opts.find_opt('-volreg_base_ind')
    aopt = user_opts.find_opt('-volreg_align_to')
    baseopt = user_opts.find_opt('-volreg_base_dset')

    # no longer accepted
    if user_opts.find_opt('-volreg_regress_per_run'):
        print('** option -volreg_regress_per_run is no longer valid\n' \
              '   (please use -regress_motion_per_run, instead)')
        return 1

    # Option -volreg_base_dset sets vr_ext_base dset, which will be copied
    # locally as vr_ext_pre.
    # MIN_OUTLIER will be extracted via vr_int_name into vr_ext_pre.
    if baseopt:
        if uopt or aopt:
            print("** cannot use -volreg_base_ind or _align_to with _base_dset")
            print("   (use sub-brick selection with -volreg_base_dset DSET)")
            return 1
        bset = baseopt.parlist[0]

        # baseopt can be either MIN_OUTLIER or typical case of dataset
        if bset == 'MIN_OUTLIER':
           # min outlier setup is actually applied in other blocks,
           # done via block.post_cstr commands
           if vr_do_min_outlier(block, proc, user_opts): return 1
        else:
           # note: vr_ext_base means vr_ext_pre+view will exist
           proc.vr_ext_base = baseopt.parlist[0]

    if uopt and bopt:
        # copy new params as ints
        if aopt:
            print("** cannot use both '-volreg_base_ind' and '-volreg_align_to'")
            return 1
        errs = 0
        try: bopt.parlist[0] = int(uopt.parlist[0]) - 1  # run -> index
        except: errs += 1
        try: bopt.parlist[1] = int(uopt.parlist[1])
        except: errs += 1
        if errs > 0:
            print("** -volreg_base_ind requires integer params (have %s,%s)" % \
                  (uopt.parlist[0], uopt.parlist[1]))
            block.valid = 0
            return 1

    if aopt and bopt:
        if aopt.parlist[0] == 'first':
            bopt.parlist[0] = 0
            bopt.parlist[1] = 0
        elif aopt.parlist[0] == 'third':
            bopt.parlist[0] = 0
            bopt.parlist[1] = 2
        elif aopt.parlist[0] == 'last':
            # for this we need to know #trs and first and last to remove,
            # so if we don't know runs/reps yet, will have -1, which is okay
            # (if reps_vary is set, we should use reps_all)
            #
            # note: since we might not know the vr_base, it must be extracted
            # after we do, which is in db_cmd_tcat()
            if proc.reps_vary: reps = proc.reps_all[-1]
            else:              reps = proc.reps
            bopt.parlist[0] = proc.runs - 1     # index of last dset
            bopt.parlist[1] = reps - 1          # index of last rep
        elif aopt.parlist[0] == 'MIN_OUTLIER':
           if vr_do_min_outlier(block, proc, user_opts): return 1
        elif aopt.parlist[0] == 'MEDIAN_BLIP':
           pass
        else:
            print("** unknown '%s' param with -volreg_base_ind option" \
                  % aopt.parlist[0])
            return 1

    apply_uopt_to_block('-volreg_method', user_opts, block)
    apply_uopt_to_block('-volreg_allin_cost', user_opts, block)
    apply_uopt_to_block('-volreg_allin_auto_stuff', user_opts, block)
    apply_uopt_to_block('-volreg_allin_warp', user_opts, block)
    apply_uopt_to_block('-volreg_post_vr_allin', user_opts, block)
    apply_uopt_to_block('-volreg_pvra_base_index', user_opts, block)
    apply_uopt_to_block('-volreg_interp', user_opts, block)
    apply_uopt_to_block('-volreg_warp_final_interp', user_opts, block)
    apply_uopt_to_block('-volreg_motsim', user_opts, block)
    apply_uopt_to_block('-volreg_get_allcostX', user_opts, block)
    apply_uopt_to_block('-volreg_opts_ewarp', user_opts, block)

    if block.opts.find_opt('-volreg_pvra_base_index') and not \
       block.opts.find_opt('-volreg_post_vr_allin'):
       print("** -volreg_pvra_base_index requires -volreg_post_vr_allin")
       return 1

    zopt = user_opts.find_opt('-volreg_zpad')
    if zopt:
        bopt = block.opts.find_opt('-volreg_zpad')
        try: bopt.parlist[0] = int(zopt.parlist[0])
        except:
            print("** -volreg_zpad requires an int (have '%s')"%zopt.parlist[0])
            return 1

    apply_uopt_to_block('-volreg_opts_ms', user_opts, block)
    apply_uopt_to_block('-volreg_opts_vr', user_opts, block)

    # check for warp to tlrc space, from either auto or manual xform
    uopt = user_opts.find_opt('-volreg_tlrc_adwarp')
    bopt = block.opts.find_opt('-volreg_tlrc_adwarp')
    if uopt and not bopt:
        block.opts.add_opt('-volreg_tlrc_adwarp', 0, [])

    u2 = user_opts.find_opt('-volreg_tlrc_warp')
    bopt = block.opts.find_opt('-volreg_tlrc_warp')
    if u2 and not bopt:
        block.opts.add_opt('-volreg_tlrc_warp', 0, [])

    # allow only 1 tlrc warp option
    if uopt and u2:
        print('** cannot use both -volreg_tlrc_adwarp and -volreg_tlrc_warp')
        return 1
    if uopt or u2:
        if proc.origview == '+tlrc':
           print('** already in tlrc space: -volreg_tlrc_* is not allowed')
           return 1
        exists = 0
        # to get from -copy_anat, no view and +tlrc exists
        if proc.anat and proc.tlrcanat and not proc.anat.view:
           if proc.tlrcanat.exist(): exists = 1
        if proc.verb > 1: print('++ -copy_anat +tlrc exists = ', exists)
        if not exists and not proc.find_block('tlrc'):
           print("** cannot warp to tlrc space without +tlrc anat via")
           print("   either -copy_anat or the 'tlrc' processing block")
           return 1
        if exists:
           wpieces = UTIL.get_num_warp_pieces(proc.tlrcanat.input(),proc.verb)
           if uopt and wpieces == 1:    # warning
              print("** have auto_tlrc anat, consider '-volreg_tlrc_warp'\n" \
                    "   (in place of '-volreg_tlrc_adwarp')")
           elif u2 and wpieces == 12:   # error
              print("** -volreg_tlrc_warp does not work with manual tlrc\n" \
                    "   (consider -volreg_tlrc_adwarp instead)")
              return 1

    # do we want to omit the actual volreg step?
    apply_uopt_to_block('-volreg_no_volreg', user_opts, block)

    # if so, there is no need for an extents mask, let that opt be conditional
    if user_opts.find_opt('-volreg_no_volreg'):
        block.opts.add_opt('-volreg_no_extent_mask', 0, [])
    else:
        apply_uopt_to_block('-volreg_no_extent_mask', user_opts, block)

    uopt = user_opts.find_opt('-volreg_align_e2a')
    bopt = block.opts.find_opt('-volreg_align_e2a')
    if uopt and not bopt:
        block.opts.add_opt('-volreg_align_e2a', 0, [])

    uopt = user_opts.find_opt('-volreg_warp_dxyz')
    bopt = block.opts.find_opt('-volreg_warp_dxyz')
    if uopt:
        try: dxyz = float(uopt.parlist[0])
        except:
            print("** -volreg_warp_dxyz requires float ('%s')"%uopt.parlist[0])
            return 1
        if bopt: bopt.parlist[0] = dxyz
        else: block.opts.add_opt('-volreg_warp_dxyz', 1, [dxyz], setpar=1)

    # considerations for -volreg_warp_master :
    #   - would be nice to also allow -volreg_warp_dxyz, but as that is an
    #     isotropic voxels size, it should not be applied without user request
    #     (i.e. be able to use -master without -dxyz)
    #   - could simply require user to be sure it is appropriate
    #     (for now, and account for issues as they arise, e.g. check space)
    #   - what if oblique? even allowed? can 3dAllin/NwA output be oblique?
    #   - in volreg only case, there might not actually be a warp
    #   
    # capture any warp_master dset here, to get it with 3dcopy 
    if vr_prep_for_warp_master(proc, user_opts): return 1

    # check on tsnr
    uopt = user_opts.find_opt('-volreg_compute_tsnr')
    bopt = block.opts.find_opt('-volreg_compute_tsnr')
    if uopt: bopt.parlist = uopt.parlist

    block.valid = 1

def vr_prep_for_warp_master(proc, user_opts):
    """check for -volreg_warp_master option and prep for processing"""

    # if no such option, bail
    oname = '-volreg_warp_master'
    warp_master, rv = user_opts.get_string_opt(oname)
    if warp_master is None or warp_master == '':
       return 0

    proc.vr_wmast_in = warp_master
    view = UTIL.dset_view(warp_master)
    if view == '':
       print("** failed to get view from -volreg_warp_master, %s" % warp_master)
       return 1
    proc.vr_warp_mast = gen_afni_name('vr_warp_master', view=view)
    if proc.verb > 1:
       print("-- have warp master dataset %s in view %s"%(warp_master, view))

    return 0

def db_cmd_volreg(proc, block):
    cmd = ''
    # get the base options
    opt = block.opts.find_opt('-volreg_base_ind')
    dset_ind = opt.parlist[0]
    sub      = opt.parlist[1]

    if dset_ind == -1: dset_ind = proc.runs - 1  # may need updates
    if sub      == -1: sub = proc.reps_all[-1] - 1

    # if ext aea base, expect sub to be small
    if proc.align_ebase != None and sub > 3 and sub >= proc.reps_all[-1] - 3:
        print('** have external align EPI volume, but seem to be\n'     \
              '   aligning EPI to end the runs, this looks fishy...')

    # volreg base should now either be external or locally created
    basevol = None
    basehead = None
    if proc.vr_ext_base != None or proc.vr_int_name != '':
       proc.vr_base_dset = gen_afni_name("%s%s" % (proc.vr_ext_pre,proc.view))
       basevol = proc.vr_base_dset.nice_input()
       # save this in case we add a uvar
       basehead = proc.vr_base_dset.nice_input(head=1)
    else:
       print("** warning: basevol should always be set now")
       return

    if proc.verb > 0:
        if basevol: print("-- %s: using base dset %s" % (block.label,basevol))
        else:
           print("-- %s: base/sub indices are %d, %d"%(block.label,dset_ind,sub))

    # get base prefix (run is index+1)
    base = proc.prev_prefix_form(dset_ind+1, block, view=1)

    # get the interpolation value
    opt = block.opts.find_opt('-volreg_interp')
    resam = ' '.join(opt.parlist)     # maybe '-cubic'

    # make note of any final interpolation, as well
    finterp, rv = block.opts.get_string_opt('-volreg_warp_final_interp',
                                            default='')
    if not rv: proc.vr_warp_fint = finterp

    # get the zpad value
    opt = block.opts.find_opt('-volreg_zpad')
    if not opt or not opt.parlist: zpad = 1
    else: zpad = opt.parlist[0]

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-volreg_opts_vr')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = ' '.join(opt.parlist)

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-volreg_opts_ewarp')
    if not opt or not opt.parlist: ewarp_opts = ''
    else: ewarp_opts = ' '.join(opt.parlist)

    if basevol: vr_base_str = basevol
    else:       vr_base_str = "%s'[%d]'" % (base,sub)

    # ---------------
    # note whether we warp to tlrc, and set the prefix and flags accordingly
    doadwarp = block.opts.find_opt('-volreg_tlrc_adwarp') != None
    dowarp = block.opts.find_opt('-volreg_tlrc_warp') != None
    doe2a = block.opts.find_opt('-volreg_align_e2a') != None
    doblip = isinstance(proc.blip_dset_warp, BASE.afni_name)
    do_pvr_allin = block.opts.have_yes_opt('-volreg_post_vr_allin',default=0)

    if proc.nlw_aff_mat and not dowarp:
       print('** have NL warp to standard space, but not applying to EPI\n' \
             '   (consider -volreg_tlrc_warp)')

    # store these flags for other processing blocks
    if dowarp: proc.warp_epi |= WARP_EPI_TLRC_WARP
    if doadwarp: proc.warp_epi |= WARP_EPI_TLRC_ADWARP
    if doe2a:  # if e2a, note it and clear any a2e
        proc.warp_epi |= WARP_EPI_ALIGN_E2A
        proc.warp_epi &= ~WARP_EPI_ALIGN_A2E

    # determine whether we will limit warped EPI to its extents
    if dowarp or doe2a:                               do_extents = 1
    else:                                             do_extents = 0
    if block.opts.find_opt('-volreg_no_extent_mask'): do_extents = 0

    # make note of the per run affine/volreg transformation matrix
    volreg_xform = 'mat.r$run.vr.aff12.1D'

    cur_prefix = proc.prefix_form_run(block, eind=-1)
    cur_prefix_me = proc.prefix_form_run(block, eind=0)
    proc.volreg_prefix = cur_prefix
    cstr   = '' # appended to comment string
    if dowarp or doe2a or doblip or proc.use_me or do_pvr_allin:
        # verify that we have someplace to warp to
        if dowarp and not proc.tlrcanat:
            print('** cannot warp, need -tlrc_anat or -copy_anat with tlrc')
            return
        if doe2a and not proc.a2e_mat:
            print("** cannot align e2a at volreg, need mat from 'align' block")
            return

        # if ME, use registration echo
        if proc.use_me: estr = '.e%s' % proc.regecho_var
        else:           estr = ''

        prefix = 'rm.epi.volreg.r$run%s' % estr
        proc.have_rm = 1            # rm.* files exist
        matstr = '-1Dmatrix_save %s' % volreg_xform
        if doblip:        cstr = cstr + ', blip warp'
        if do_pvr_allin:  cstr = cstr + ', across runs'
        if doe2a:         cstr = cstr + ', to anat'
        if dowarp:        cstr = cstr + ', warp to tlrc space'
    else:
        if doadwarp: cstr = cstr + ', adwarp to tlrc space'
        prefix = cur_prefix
        matstr = ''

    prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=-1)

    if doblip: cstr += '\n# (final warp input is same as blip input)'
    cmd = cmd + "# %s\n" \
                "# align each dset to base volume%s\n" \
                % (block_header('volreg'), cstr)

    all1_input = None
    if dowarp or doadwarp or do_extents:
        cmd = cmd + '\n'

        if dowarp or doadwarp:
            afile = proc.tlrcanat.shortinput(head=1)
            cmd = cmd + \
                "# verify that we have a +tlrc warp dataset\n"    \
                "if ( ! -f %s ) then\n"                           \
                '    echo "** missing +tlrc warp dataset: %s" \n' \
                '    exit\n'                                      \
                'endif\n\n' % (afile, afile)

        if dowarp or do_extents: cmd = cmd + '# register and warp\n'

    # ------------------------------
    # maybe we need MIN_OUTLIER indices across runs (for pvra)
    # set pvra_bind_str, an integer run-based vr base index
    # (or 'MIN_OUTLIER' or '$')
    pvra_bind_str = '0'
    if do_pvr_allin:
       pvra_bind_str, rv = block.opts.get_string_opt('-volreg_pvra_base_index',
                                                     default='0')
       if rv: return

       # warnings, MO is probably the way to go
       if pvra_bind_str != 'MIN_OUTLIER':
          print("** consider '-volreg_pvra_base_index MIN_OUTLIER' " \
                "for per-run registration")
       else:
          if not proc.vr_base_MO and proc.vr_ext_base is None:
             print("** consider '-volreg_align_to MIN_OUTLIER' " \
                   "for across-run registration")

       if pvra_bind_str != 'MIN_OUTLIER' and pvra_bind_str != '$':
          # see if we have an int
          try:
             bind_val = int(pvra_bind_str)
          except:
             print("** illegal value for -volreg_pvra_base_index : '%s'" \
                   % pvra_bind_str)
             print("   should be 'MIN_OUTLIER' or integral run index")
             return

          if bind_val < 0 or bind_val >= min(proc.reps_all):
             print("** -volreg_pvra_base_index out of range for run lengths:" \
                   " %s" % pvra_bind_str)
             return

    # ------------------------------
    # start foreach run loop
    cmd = cmd + "foreach run ( $runs )\n"

    # possibly include a comment about multi-echo registration basis
    if proc.use_me:
       mestr =  '    # (registration is driven by %s)\n' % proc.regecho_var
    else:
       mestr = ''

    # ------------------------------
    # maybe run post-vr allineate, if so:
    #    - extract vr base per run
    #    - align to overall base (get xform mat)
    #    - include xform matrix in cat_matvec
    #
    # ***** what if only volreg, and no cat_matvec?
    #
    #       ==> maybe do it by setting do_cat_matvec (search for this term)
    #           no, use do_pvr_allin, so we can comment post_vr_allin
    #
    pvr_bstr_orig = ''
    pvr_matrix = ''
    pvr_autostuff = ''
    if do_pvr_allin:
       istr = '    '
       nistr = '\n' + istr
       # use a list of script lines, for global indentation
       plist = []

       # first choose volume index, either an index or MIN_OUTLIER
       subb_quote = "'" # depends on variable or possible '$'
       if pvra_bind_str == 'MIN_OUTLIER':
          if not proc.outl_rfile:
             print("** AP: missing outcount file for pvra MIN_OUTILER")
             return
          plist.append('# extract MIN_OUTLIER index for current run')
          plist.append("set min_outlier_index = `3dTstat -argmin" \
                       " -prefix - %s\\'`" % proc.outl_rfile)
          plist.append('')
          subb_quote = '"'
          pvra_bind_str = '$min_outlier_index'

       # then extract vr_base volume
       pvr_prefix = 'vr_base_per_run_r$run'
       plist.append('# extract volreg base for this run')
       plist.append("3dbucket -prefix %s %s%s[%s]%s" \
            % (pvr_prefix, prev_prefix, subb_quote, pvra_bind_str, subb_quote))
       plist.append('')

       pvr_matrix = 'mat.vr_xrun_allin.r$run.aff12.1D'
       pvr_autostuff = '-autoweight -source_automask'
       pvr_cost = '-lpa'
       pvr_resam = '-cubic'

       # register to main base
       plist.append('# and compute xforms for cross-run allin to vr_base')
       plist.append('3dAllineate -base %s \\' % vr_base_str)
       plist.append('            -source %s%s \\'%(pvr_prefix, proc.view))
       plist.append('            -1Dfile vr_xrun_allin_dfile.m12.r$run.1D \\')
       plist.append('            -1Dmatrix_save %s \\' % pvr_matrix)
       if pvr_autostuff:
          plist.append('            %s \\' % pvr_autostuff)
       plist.append('            %s %s' % (pvr_cost, pvr_resam))

       # and save the final matrix for posterity
       proc.pvr_allin_mat = pvr_matrix

       if proc.verb > 1:
          print("++ have post_vr_allin commands and xmat %s" % pvr_matrix)

       pvr_cmd = istr + nistr.join(plist) + '\n\n'

       # keep the old -base string, and replace it in the vr command
       pvr_bstr_orig = vr_base_str
       vr_base_str = pvr_prefix + proc.view

       cmd += pvr_cmd

    # ============================================================
    # include main volreg command, or an identity substitute

    # do main volreg/Allineate step, unless users wants to omit
    do_volreg = block.opts.find_opt('-volreg_no_volreg') is None
    if do_volreg:
       # run 3dvolreg or 3dAllineate
       cmnt = "    # register each volume to the base image\n%s" % (mestr)
       vrmeth = '3dvolreg'
       vv, rv = block.opts.get_string_opt('-volreg_method')
       if vv and not rv:
          if vv not in ['3dvolreg', '3dAllineate']:
             print("** bad -volreg_method %s" % vv)
             return
          if proc.verb > 1: print("++ updating -volreg_method to %s" % vv)
          vrmeth = vv

       if vrmeth == '3dAllineate':
          vrcmd = make_volreg_command_allin(block, prev_prefix, prefix,
                       vr_base_str, matstr, other_opts=other_opts, resam=resam)
          if not vrcmd: return

       else: # '3dvolreg'
          vrcmd = make_volreg_command(block, prev_prefix, prefix, vr_base_str,
                       matstr, zpad, other_opts=other_opts, resam=resam)
          if not vrcmd: return
    else:
       # skip 3dvolreg and use identity
       cmnt  = "    # skip 3dvolreg, replacing xform with identity\n"
       vrcmd = "    echo '1 0 0 0  0 1 0 0  0 0 1 0' > %s\n" % volreg_xform

    cmd = cmd + cmnt + vrcmd

    # if pvr_allin, reset the volreg base to the global one, not local per run
    if do_pvr_allin:
       vr_base_str = pvr_bstr_orig

    # ============================================================
    # include extents
    if do_extents:
       all1_input = gen_afni_name('rm.epi.all1'+proc.view)
       cmd = cmd + '\n' \
          "    # create an all-1 dataset to mask the extents of the warp\n" \
          "    3dcalc -overwrite -a %s -expr 1 \\\n"                        \
          "           -prefix %s\n"                                         \
          % (prev_prefix, all1_input.prefix)

    # if warping to new grid, note dimensions
    dim = 0
    if doadwarp or dowarp or doe2a:
        # if warping, we might want dxyz, master or both

        # delta may need to be non-isotropic
        proc.delta = [dim, dim, dim]

        opt = block.opts.find_opt('-volreg_warp_dxyz')
        if opt:
           dim = opt.parlist[0]
           proc.delta = [dim, dim, dim]
        elif proc.vr_warp_mast is not None:
            dims = UTIL.get_3dinfo_val_list(proc.vr_wmast_in, "d3", float)
            if dims is None or len(dims) != 3:
               print("** failed to get dimensions of -volreg_warp_master")
               return
            proc.delta = dims
        else:
            # truncate min dimension, but scale up slightly
            dim = UTIL.get_truncated_grid_dim(proc.dsets[0].rel_input(),
                                              scale=1.0001)
            if dim <= 0:
                print('** failed to get grid dim from %s' \
                      % proc.dsets[0].rel_input())
                return
            proc.delta = [dim, dim, dim]
        if proc.verb > 2: print("++ volreg: setting delta = %s" % proc.delta)


    # create EPI warp list, outer to inner
    epi_warps      = []
    allinbase      = None       # master grid for warp

    # if warping, multiply matrices and apply
    # (store cat_matvec entries in case of later use)
    if dowarp or doe2a or doblip or proc.use_me or do_pvr_allin:
        do_cat_matvec = 1
    else:
        do_cat_matvec = 0

    if do_cat_matvec:
        # ============================================================
        # start by creating a comment string

        # warn the user of output grid change
        pstr = '++ volreg: applying '
        cary = []       # list used to create comment string, cstr
        cstr = ''
        if doblip: cary.append('blip')

        # always
        cary.append('volreg')

        if do_pvr_allin: cary.append('post_vr_allin')

        if doe2a: cary.append('epi2anat')
        if dowarp: cary.append('tlrc')

        cstr = '/'.join(cary)

        pstr += (cstr + ' xforms')
        if dowarp or doe2a: pstr += (' to isotropic %g mm' % dim)
        if dowarp: pstr += ' tlrc'
        if dowarp or doe2a: pstr += ' voxels'

        # ============================================================
        # next create cat_matvec command, and track affine transformations
        # in e2final_mv (xforms from volreg base to final affine space)

        cmd = cmd + '\n'                        \
            '    # catenate %s xforms\n'        \
            '    cat_matvec -ONELINE \\\n' % cstr
        print('%s' % pstr)

        if dowarp:
            # either non-linear or affine warp
            if proc.nlw_aff_mat: wstr = proc.nlw_aff_mat
            else:                wstr = '%s::WARP_DATA -I' % proc.tlrcanat.pv()
            cmd = cmd + '               %s \\\n' % wstr
            proc.e2final_mv.append(wstr)

        if doe2a:
            wstr = '%s -I ' % proc.a2e_mat
            cmd = cmd + '               %s \\\n' % wstr
            proc.e2final_mv.append(wstr)

        if do_pvr_allin:
            cmd = cmd + '               %s \\\n' % proc.pvr_allin_mat
            # proc.e2final_mv is not affected by pvr_allin

        # ============================================================

        # if ME, alter prev_prefix before applying catendated warp
        if proc.use_me:
           prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=0)

        # if blip, input (prev_prefix) is from prior to blip block
        if doblip:
           bblock = proc.find_block('blip')
           if bblock:
              if proc.use_me:
                 prev_prefix = proc.prev_prefix_form_run(bblock,view=1,eind=0)
              else:
                 prev_prefix = proc.prev_prefix_form_run(bblock,view=1,eind=-1)

        # if tlrc, use that for 3dAllineate base and change view
        if dowarp:
            allinbase = proc.tlrcanat.pv()
            proc.view = '+tlrc'
        elif doe2a:
            allinbase = proc.anat.pv()
        else:
            allinbase = ''

        # resulting affine xform of orig EPI, per run (used in a few places)
        runwarpmat = 'mat.r$run.warp.aff12.1D'

        # final affine xmat is from volreg step
        cmd += '               %s > %s\n' % (volreg_xform, runwarpmat)

        if do_extents:
           if proc.use_me:
              wprefix = "rm.epi.nomask.r$run.e%s" % proc.echo_var
           else:
              wprefix = "rm.epi.nomask.r$run"
        else:
           if proc.use_me:
              wprefix = cur_prefix_me
           else:
              wprefix = cur_prefix

        # ============================================================
        # create catenated set of EPI warps (epi_warps, all1_warps)
        # - these should take EPI data from orig space to final space

        # first outer is any NL std space warp
        if dowarp and proc.nlw_aff_mat != '' and proc.nlw_type == 'NL':
           epi_warps.append(warp_item('NL std space', 'NL', proc.nlw_NL_mat))

        # next is a combined warp of volreg->std space
        epi_warps.append(warp_item(cstr, 'affine', runwarpmat))

        # most inner is blip (all1 warp need not include blip)
        all1_warps = epi_warps[:]
        if doblip:
           blipinput = proc.blip_dset_warp.shortinput()
           epi_warps.append(warp_item('blip', 'NL', blipinput))

        # finished with catenated EPI warps

        # ============================================================

        if dowarp and proc.nlw_NL_mat:
           cstr += '/NLtlrc'

        indent = '    '
        wcmd = '\n%s# apply catenated xform: %s\n' % (indent, cstr)

        # if ME, warp per echo
        ime = ''
        if proc.use_me:
           mcmd = '%s# (apply warps per echo - warps are fixed, per run)\n' \
                  '%sforeach %s ( $echo_list )\n' \
                  % (indent, indent, proc.echo_var[1:])
           ime = '   ' # ME extra indent
           wcmd += mcmd

        st, wtmp = apply_catenated_warps(proc, epi_warps, base=allinbase,
                      source=prev_prefix, prefix=wprefix, dim=dim,
                      ewopts=ewarp_opts, istr=(indent+ime))
        if st: return
        wcmd += wtmp

        if proc.use_me:
           wcmd += '%send\n' % indent

        if do_extents:
           all1_prefix = 'rm.epi.1.r$run'
           st, wtmp = apply_catenated_warps(proc, all1_warps, base=allinbase,
                         source=all1_input.shortinput(), prefix=all1_prefix,
                         dim=dim, NN=1, NLinterp='cubic', ewopts=ewarp_opts,
                         istr=indent)
           if st: return
           wcmd += '\n%s# warp the all-1 dataset for extents masking \n%s' \
                   % (indent, wtmp)

           wcmd += '\n'                                                 \
                '%s# make an extents intersection mask of this run\n'   \
                '%s3dTstat -min -prefix rm.epi.min.r$run %s%s\n'        \
                % (indent, indent, all1_prefix, proc.view)

        # wcmd = old_apply_cat_warps(proc, runwarpmat, allinbase, prev_prefix,
        #                            dowarp, wprefix, dim, all1_input, cstr)

        if wcmd == None: return
        cmd += wcmd

    cmd = cmd + "end\n\n"

    # save the motion file for motsim
    # note: proc.mot_file is either used above, or passed as different,
    #       but use the passed one (proc.mot_file) for motsim
    proc.mot_default = 'dfile_rall.1D'

    if do_volreg: 
        cmd = cmd + "# make a single file of registration params\n"  \
                    "cat dfile.r*.1D > %s\n\n" % proc.mot_default

    # if not censoring motion, make a generic motion file
    if not proc.user_opts.find_opt('-regress_censor_motion'):
        cmd = cmd +                                                         \
            "# compute motion magnitude time series: the Euclidean norm\n"  \
            "# (sqrt(sum squares)) of the motion parameter derivatives\n"

        proc.mot_enorm = 'motion_${subj}_enorm.1D'
        if proc.reps_vary :     # use -set_run_lengths aot -set_nruns
           cmd = cmd +                                                      \
               "1d_tool.py -infile %s \\\n"                                 \
               "           -set_run_lengths %s \\\n"                        \
               "           -derivative -collapse_cols euclidean_norm \\\n"  \
               "           -write %s\n\n"                                   \
               % (proc.mot_file, UTIL.int_list_string(proc.reps_all),
                  proc.mot_enorm)
        else:                   # stick with -set_nruns
           cmd = cmd +                                                      \
               "1d_tool.py -infile %s -set_nruns %d \\\n"                   \
               "           -derivative  -collapse_cols euclidean_norm \\\n" \
               "           -write %s\n\n"                                   \
               % (proc.mot_file, proc.runs, proc.mot_enorm)

    if do_extents:
        proc.mask_extents = gen_afni_name('mask_epi_extents' + proc.view)

        cmd = cmd +                                             \
            "# ----------------------------------------\n"      \
            "# create the extents mask: %s\n"                   \
            "# (this is a mask of voxels that have valid data at every TR)\n"\
            % proc.mask_extents.pv()

        if proc.runs > 1:  # if more than 1 run, create union mask
          cmd = cmd +                                                        \
            "3dMean -datum short -prefix rm.epi.mean rm.epi.min.r*.HEAD \n"  \
            "3dcalc -a rm.epi.mean%s -expr 'step(a-0.999)' -prefix %s\n\n"   \
             % (proc.view, proc.mask_extents.prefix)
        else:  # just copy the one
          cmd = cmd +                                                        \
            "# (only 1 run, so just use 3dcopy to keep naming straight)\n"   \
            "3dcopy rm.epi.min.r01%s %s\n\n"                                 \
            % (proc.view, proc.mask_extents.prefix)

        proc.mask_extents.created = 1  # so this mask 'exists' now

        cmd = cmd +                                             \
            "# and apply the extents mask to the EPI data \n"   \
            "# (delete any time series with missing data)\n"    \
            "foreach run ( $runs )\n"

        # ME updates
        if proc.use_me:
           indent = '    '
           estr = '.e%s' % proc.echo_var
           cmd += '%sforeach %s ( $echo_list )\n' % (indent, proc.echo_var[1:])
        else:
           indent = ''
           estr = ''

        cmd = cmd +                                               \
            "%s    3dcalc -a rm.epi.nomask.r$run%s%s -b %s \\\n"  \
            "%s           -expr 'a*b' -prefix %s\n"               \
            "%send\n" % (indent, estr, proc.view,
                         proc.mask_extents.pv(), indent, cur_prefix_me, indent)

        if proc.use_me: cmd += 'end\n'

        cmd += '\n'

    # ---------------
    # next, see if we want to apply a (manual) warp to tlrc space
    if block.opts.find_opt('-volreg_tlrc_adwarp'):
        if proc.view == '+tlrc':
            print('** cannot apply -volreg_tlrc_adwarp: already in tlrc space')
            return
        if not proc.tlrcanat:
            print('** need -copy_anat with tlrc for -volreg_tlrc_adwarp')
            return

        cmd = cmd +                                                      \
            "# ----------------------------------------\n"               \
            "# apply manual Talairach transformation as separate step\n" \
            "foreach run ( $runs )\n"                                    \
            "    adwarp -apar %s -dpar %s%s \\\n"                        \
            "           -dxyz %g -resam Cu\n"                            \
            "end\n\n" % (proc.tlrcanat.pv(), cur_prefix, proc.view, dim)
        proc.view = '+tlrc'  # and set a new current view

        # if extents mask, need to warp it and update to the new proc.view
        if do_extents:
           cmd = cmd +                                                      \
               "# and apply Talairach transformation to the extents mask\n" \
               "adwarp -apar %s -dpar %s \\\n"                              \
               "       -dxyz %g -resam NN\n\n"                              \
               % (proc.tlrcanat.pv(), proc.mask_extents.pv(), dim)
           proc.mask_extents.new_view(proc.view)

    # ---------------
    # make a warped volreg base dataset, if appropriate
    rv, wcmd, wapply = create_volreg_base_warp(proc, epi_warps, proc.e2final_mv)
    if rv: return
    get_allcostX = 0    # run "3dAllineate -allcostX" if we get to 2
    if wcmd and wapply:
        cmd += wcmd
        wprefix = 'final_epi_%s' % proc.vr_base_dset.prefix
        proc.epi_final = proc.vr_base_dset.new(new_pref=wprefix)
        proc.epi_final.new_view(proc.view)
        st, wtmp = apply_catenated_warps(proc, wapply, base=allinbase,
                     source=basevol, prefix=wprefix, dim=dim, ewopts=ewarp_opts)
        if st: return
        cmd += wtmp + '\n'
        get_allcostX += 1

        # and since we are warping, clear any generic variables that depend on
        # the current grid
        clear_grid_dependent_vars(proc, block)

    elif basehead:
        # we are not creating a final_epi, so pass use the base for a uvar
        proc.uvars.set_var('final_epi_dset', [basehead])

    # ---------------
    # make a copy of the "final" anatomy, called "anat_final.$subj"
    if proc.view == '+tlrc': aset = proc.tlrcanat
    else:                    aset = proc.anat
    if aset != None:
       proc.anat_final = aset.new(new_pref='anat_final.%s'%proc.subj_label)
       cmd += "# create an anat_final dataset, aligned with stats\n"    \
              "3dcopy %s %s\n\n"                                        \
              % (aset.pv(), proc.anat_final.prefix)
       get_allcostX += 1

    # ---------------
    # possibly run "3dAllineate -allcostX"
    if get_allcostX == 2 and block.opts.have_yes_opt('-volreg_get_allcostX',1):
       cmd += "# record final registration costs\n"     \
              "3dAllineate -base %s -allcostX\\\n"      \
              "            -input %s |& tee out.allcostX.txt\n\n" \
              % (proc.epi_final.shortinput(), proc.anat_final.shortinput())

    if do_extents: emask = proc.mask_extents.prefix
    else:          emask = ''

    # ---------------
    # if requested, make TSNR dataset from run 1 of volreg output
    opt = block.opts.find_opt('-volreg_compute_tsnr')
    if opt.parlist[0] == 'yes':
       tcmd = db_cmd_volreg_tsnr(proc, block, emask)
       if tcmd == None: return
       if tcmd != '': cmd += tcmd

    # ---------------
    # if requested, make motion simulation dataset
    if block.opts.find_opt('-volreg_motsim'):
        if block.opts.find_opt('-volreg_tlrc_adwarp'):
            print('** -volreg_motsim not valid with -volreg_tlrc_adwarp')
            return
        if proc.surf_anat:
            print('** -volreg_motsim not valid with surface analysis')
            return
        # mot base: if external, use 0[0], else use vr_base_str
        if basevol: bvol = '%s"[0]"' % proc.prev_prefix_form(1, block, view=1)
        else:       bvol = vr_base_str
        rv, tcmd = db_cmd_volreg_motsim(proc, block, vr_base_str)
        if rv: return
        cmd += tcmd

    # ---------------
    # if requested, create any anat followers
    if should_warp_anat_followers(proc, block):
        rv, tcmd = warp_anat_followers(proc, block, proc.anat_final)
        if rv: return
        if tcmd: cmd += tcmd

    # used 3dvolreg, so have these labels
    proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']

    return cmd


def clear_grid_dependent_vars(proc, block):
    """clear any generic proc variables that depend on the current grid,
       and which must be regenerated if it changes
    """
    proc.tsnr_dset = None


def make_volreg_command(block, prev_prefix, prefix, basestr, matstr,
                        zpad, other_opts="", resam="Cu"):
    """return the main 3dvolreg command string"""

    if matstr: matstr = '%*s%s \\\n' % (13, ' ', matstr)
    if other_opts: other_opts = '%*s%s \\\n' % (13, ' ', other_opts)
    vrcmd = "    3dvolreg -verbose -zpad %d -base %s \\\n"        \
            "             -1Dfile dfile.r$run.1D -prefix %s \\\n" \
            "             %s \\\n"                                \
            "%s"                                                  \
            "%s"                                                  \
            "             %s\n" %                                 \
            (zpad, basestr, prefix, resam, other_opts, matstr, prev_prefix)

    return vrcmd

def make_volreg_command_allin(block, prev_prefix, prefix, basestr, matstr,
                              other_opts="", resam="Cu"):
    """return the main volreg command string, but using 3dAllineate"""

    # get/set cost function
    allin_cost, rv = block.opts.get_string_opt('-volreg_allin_cost',
                                               default='lpa')
    if rv:
       print("** failed to parse -volreg_allin_cost")
       return

    # get/set warp restriction
    allin_warp, rv = block.opts.get_string_opt('-volreg_allin_warp',
                                               default='shift_rotate')
    if rv:
       print("** failed to parse -volreg_allin_cost")
       return

    # changes: no zpad, need cost, automask, source_automask
    #          1Dfile to m12, change padding
    # extract first 6 from dfile.m12.r$run.1D to make dfile.r$run.1D
    if matstr: matstr = '%*s%s \\\n' % (8, ' ', matstr)
    if other_opts: other_opts = '%*s%s \\\n' % (8, ' ', other_opts)

    # let the user override the -auto options
    autostuff = "-automask -source_automask -autoweight "
    alist, rv = block.opts.get_string_list('-volreg_allin_auto_stuff')
    if alist and len(alist) > 0:
       if alist[0] == 'NONE': autostuff = ''
       else:                  autostuff = '%s ' % ' '.join(alist)

    vrcmd = "    3dAllineate %s\\\n"                     \
            "        -base %s \\\n"                      \
            "        -source %s \\\n"                    \
            "        -prefix %s \\\n"                    \
            "        -warp %s \\\n"                      \
            "        -1Dfile dfile.m12.r$run.1D \\\n"    \
            "%s"                                         \
            "%s"                                         \
            "        -cost %s %s\n\n" %                  \
            (autostuff, basestr, prev_prefix, prefix,
             allin_warp, matstr, other_opts, allin_cost, resam)

    vrcmd += "    # and extract the 6 rigid-body params\n"   \
             "    1dcat dfile.m12.r$run.1D'[3..5,0..2]' > dfile.r$run.1D\n"

    return vrcmd

def create_volreg_base_warp(proc, ewarps, matvec_list):
   """if matvec_list is non-empty, apply all warps
         to:         proc.vr_base_dset
         for making: proc.epi_final

      Replace affine warp from ewarps with concatenated warp
      from matvec_list.

      return status (0 on success), command and new warps
   """

   # anything to do?
   if len(matvec_list) == 0: return 0, '', None

   # make sure there is no blip warp
   wapply = [w for w in ewarps if (w.desc != 'blip' and w.desc != 'pvr_allin')]

   # find affine warp to replace with that from matvec_list
   affine_ind = -1
   for ind, witem in enumerate(ewarps):
      if witem.wtype == 'affine':
         affine_ind = ind
         break

   if affine_ind < 0:
      print('** CVWBV: no affine warp to replace')
      return 1, '', None

   # create the warp to replace
   warpmat = 'mat.basewarp.aff12.1D'
   cstr = '# warp the volreg base EPI dataset to make a final version\n' \
          'cat_matvec -ONELINE'

   if len(matvec_list) == 1:
      cstr += ' %s' % matvec_list[0]
   else:
      spacing = ' \\\n           '
      cstr += spacing + spacing.join(matvec_list)

   cstr += ' > %s\n\n' % warpmat

   wapply[affine_ind] = warp_item('vr base warp', 'affine', warpmat)

   return 0, cstr, wapply

def warp_anat_followers(proc, block, anat_aname, epi_aname=None, prevepi=0):
   """apply a single catenated warp to all followers, to match that of anat

      - if nothing to do, leave
      - warps should be listed in reverse order, as A(B(C))
      - if multiple xforms in anat_warps[] and no NLwarp
          - concatenate the warps
      - if NO warps are passed, then use 3dAllineate and
         '1D: 12@0' for any epi dwarp
      - create warp string (single warp or combined NLwarp)
      - for each follower
          - if dgrid dataset is passed, warp to it (note NN, final_prefix)

      - return status and command string
   """
   if len(proc.afollowers) < 1: return 0, None

   if epi_aname == None:
      if prevepi:
         epi_aname = gen_afni_name(proc.prev_prefix_form(1, block, eind=-1))
      else:
         epi_aname = gen_afni_name(proc.prefix_form(block, 1, eind=-1))
      epi_aname.new_view(proc.view)

   warps = proc.anat_warps[:]
   warps.reverse()

   identity_warp = "'1D: 12@0'"
   nwarps = len(warps)

   # check for any non-linear warp
   donl = 0
   for warp in warps:
      if proc.looks_like_qwarp_name(warp):
         donl = 1
         break

   # affine vs NL: program, interp option, warp option, indent
   tstr = ''
   if donl:
      prog    = '3dNwarpApply'
      iopt    = '-ainterp'
      sp      = ' ' # indent needs extra space
      xform   = ''
      warpstr = '-nwarp %s' % ' '.join(warps)

   else:
      prog    = '3dAllineate'
      iopt    = '-final'
      sp      = ''

      # decide on final 'xform' to apply (identity/single/combined)
      if   nwarps == 0: xform = identity_warp
      elif nwarps == 1: xform = warps[0]
      else: # nwarps > 1, catenate them
         xall  = ' \\\n           '.join(warps)
         xform = 'warp.all.anat.aff12.1D'

         tstr = "# catenate all transformations\n" \
                "cat_matvec -ONELINE \\\n"         \
                "           %s > %s\n\n" % (xall, xform)

      if xform == identity_warp: warpstr = "-1Dparam_apply %s\\'" % xform
      else:                      warpstr = '-1Dmatrix_apply %s' % xform

   if donl:                     wtstr = 'non-linear'
   elif xform == identity_warp: wtstr = 'identity: resample'
   else:                        wtstr = 'affine'
   wstr = '# -----------------------------------------\n'  \
          '# warp anat follower datasets (%s)\n' % wtstr

   if tstr: wstr += '\n%s' % tstr

   # perform any pre-erode
   efirst = 1
   for afobj in proc.afollowers:
       if afobj.erode <= 0: continue

       if efirst: # first eroded dataset
          efirst = 0
          wstr += '\n# first perform any pre-warp erode operations\n'

       cname = afobj.cname.new(new_pref=(afobj.cname.prefix+'_erode'))
       wstr += '3dmask_tool -input %s -dilate_input -%d \\\n'   \
               '            -prefix %s\n'                       \
               % (afobj.cname.shortinput(), afobj.erode, cname.out_prefix())
       afobj.cname = cname
   if not efirst: wstr += '\n# and apply any warp operations\n\n'

   # process all followers
   wlist = []
   for afobj in proc.afollowers:
      if afobj.is_warped == 1:
         print('** calling warp_anat_followers multiple times!')
         return 1, None

      if afobj.dgrid == 'epi':    mname = epi_aname
      elif afobj.dgrid == 'anat': mname = anat_aname
      else:                       mname = afobj.cname

      if mname == None:
         print('** warp anat followers: missing %s grid for input %s' \
               % (afobj.dgrid, afobj.aname.shortinput()))
         return 1, None

      # rcr - deal with post_erode, final prefix would come later

      if afobj.final_prefix: prefix = afobj.final_prefix
      else:                  prefix = 'afwarp_%s' % afobj.label

      if   afobj.NN:          istr = 'NN'
      elif proc.vr_warp_fint: istr = proc.vr_warp_fint
      else:                   istr = 'wsinc5'

      if xform == identity_warp and afobj.dgrid != 'epi':
         if proc.verb > 1:
            print('-- no need to warp anat follower %s' % afobj.aname.prefix)
         # even if not warped, make sure dset is in dict_key list
         #    - see proc.add_roi_dict_key below
         proc.roi_dict[afobj.label] = afobj.cname
         continue # no warp needed

      # make an updated afni_name for the result (will become afobj.cname)
      anew = mname.new(new_pref=prefix)

      wstr += '# warp follower dataset %s\n' \
              '%s -source %s \\\n'           \
              '%s            -master %s\\\n' \
              '%s            %s %s %s\\\n'   \
              '%s            -prefix %s\n\n' \
              % (afobj.cname.shortinput(),
                 prog, afobj.cname.shortinput(), sp, mname.shortinput(),
                 sp, iopt, istr, warpstr,
                 sp, anew.prefix)

      # if NN interp and atlas or labeltable, try to preserve
      if afobj.is_alt and afobj.NN:
         wstr += '# and copy its label table to the warped result\n' \
                 '3drefit -copytables %s %s\n'   \
                 '3drefit -cmap INT_CMAP %s\n\n' \
                 % (afobj.cname.shortinput(), anew.shortinput(),
                    anew.shortinput())

      # update current name based on master dataset and new prefix
      afobj.cname = anew
      afobj.is_warped = 1
      wlist.append(afobj.aname.prefix)

      # and add this dataset to the ROI regression dictionary
      if afobj.dgrid == 'epi':
         if proc.add_roi_dict_key(afobj.label, afobj.cname): return 1, None

   if len(wlist) > 0:
      print('-- applying anat warps to %d dataset(s): %s' \
            % (len(wlist), ', '.join(wlist)))

   return 0, wstr

def should_warp_anat_followers(proc, block):
   if len(proc.afollowers) == 0: return 0

   # volreg gets priority
   if block.label == 'volreg':   return 1
   if proc.find_block('volreg'): return 0

   # else align gets priority
   if block.label == 'align':   return 1
   if proc.find_block('align'): return 0

   # else postdata is only option
   if block.label == 'postdata': return 1

   print('** should_warp_anat_followers: in bad block %s' % block.label)
   return 0

def old_apply_cat_warps(proc, runwarpmat, gridbase, winput, dowarp,
                          woutput, dim, all1_dset,cstr):
   """generate either 3dAllineate or 3dNwarpApply commands"""

   # non-linear case - apply proc.nlw_NL_mat along with typical mat
   if (dowarp and proc.nlw_aff_mat != '') or proc.blip_dset_warp:
      if dim > 0: dimstr = ' -dxyz %g' % dim
      else:       dimstr = ''

      if proc.blip_dset_warp != None:
         bwstr = ' %s' % proc.blip_dset_warp.shortinput()
      else:
         bwstr = ''

      cmd = '\n'                                                \
          '    # apply catenated xform: %s\n'                   \
          '    # then apply non-linear standard-space warp\n'   \
          '    3dNwarpApply -master %s%s \\\n'                  \
          '                 -source %s \\\n'                    \
          '                 -nwarp "%s %s%s" \\\n'              \
          '                 -prefix %s \n'                      \
          % (cstr, gridbase, dimstr, winput, proc.nlw_NL_mat,
             runwarpmat, bwstr, woutput)
   else: # affine case
      cmd = '\n'                                        \
          '    # apply catenated xform : %s\n'          \
          '    3dAllineate -base %s \\\n'               \
          '                -input %s \\\n'              \
          '                -1Dmatrix_apply %s \\\n'     \
          '                -mast_dxyz %g\\\n'           \
          '                -prefix %s \n'               \
          % (cstr, gridbase, winput, runwarpmat, dim, woutput)

   # intersection mask of all-1 time series is same either way
   # (forget blip warps here)
   if all1_dset != None:
      if dowarp and proc.nlw_aff_mat != '': # non-linear case
         cmd = cmd + '\n' +                                        \
             '    # warp the all-1 dataset for extents masking \n' \
             '    3dNwarpApply -master %s -dxyz %g\\\n'            \
             '                 -source %s \\\n'                    \
             '                 -nwarp "%s %s" \\\n'                \
             '                 -prefix rm.epi.1.r$run \\\n'        \
             '                 -ainterp NN -quiet \n'              \
             % (gridbase, dim, all1_dset.pv(), proc.nlw_NL_mat, runwarpmat)
      else:
         cmd = cmd + '\n' +                                        \
             '    # warp the all-1 dataset for extents masking \n' \
             '    3dAllineate -base %s \\\n'                       \
             '                -input %s \\\n'                      \
             '                -1Dmatrix_apply %s \\\n'             \
             '                -mast_dxyz %g -final NN -quiet \\\n' \
             '                -prefix rm.epi.1.r$run \n'           \
             % (gridbase, all1_dset.pv(), runwarpmat, dim)

      cmd = cmd + '\n' +                                                 \
          '    # make an extents intersection mask of this run\n'        \
          '    3dTstat -min -prefix rm.epi.min.r$run rm.epi.1.r$run%s\n' \
          % proc.view

   return cmd


# create @simulate_motion commands
def db_cmd_volreg_motsim(proc, block, basevol):
    """generate a @simulate_motion command
          - use proc.mot_file and proc.e2final_mv
          - if self.nlw_NL_mat, apply with 3dNwarpApply
       return 0 on success, along with any command string
    """
    proc.mot_simset = gen_afni_name('motsim_$subj%s' % proc.view)
    # first generate any needed cat_matvec command using proc.e2final_mv
    cmd = "# ----------------------------------------\n"      \
          "# generate simulated motion dataset: %s\n" % proc.mot_simset.pv()
    if len(proc.e2final_mv) > 0:
       proc.e2final = 'warp.e2final.aff12.1D'
       xforms = ' \\\n           '.join(proc.e2final_mv)
       cmd += "\n# first generate post volreg transformation matrix file\n" \
              "cat_matvec -ONELINE \\\n"                      \
              "           %s > %s\n\n" % (xforms, proc.e2final)

    # maybe there are extra options to add to the command
    olist, rv = block.opts.get_string_list('-volreg_opts_ms')
    if olist and len(olist) > 0:
        other_opts = '%17s%s \\\n' % (' ', ' '.join(olist))
    else: other_opts = ''

    # use proc.mot_file, in case a modified version was passed
    cmd += "@simulate_motion -epi %s -prefix %s \\\n"   \
           "                 -motion_file %s \\\n"      \
           "%s"                                         \
           % (basevol, proc.mot_simset.prefix, proc.mot_file, other_opts)

    if proc.e2final == '':
        cmd += "                 -warp_method VOLREG\n\n"
    else:
        # rcr todo - add if self.nlw_NL_mat, add -warp_NL_dset
        cmd += "                 -warp_method VOLREG_AND_WARP \\\n"     \
               "                 -warp_1D %s \\\n"                      \
               "                 -warp_master %s\n\n"                   \
               % (proc.e2final, proc.prefix_form(block,1,view=1))

    return 0, cmd

# compute temporal signal to noise before the blur (just run 1?)
def db_cmd_volreg_tsnr(proc, block, emask=''):

    # signal and error are both first run of previous output
    signal = proc.prefix_form(block, 1, eind=-1)

    return db_cmd_tsnr(proc,
           "# --------------------------------------\n" \
           "# create a TSNR dataset, just from run 1\n",
           signal, signal, proc.view, mask=emask,
           name_qual='.vreg.r01',detrend=1, oksurf=0)

    # if block.opts.find_opt('-volreg_compute_tsnr_stats'):
    #    # given ROIs and new proc.tsnr_dset
    #    rv, scmd = db_cmd_volreg_tsnr_stats(proc, block)
    #    tsnr_cmd += '\n' + scmd

# --------------- combine block ---------------

def db_mod_combine(block, proc, user_opts):
   """ponder...

   """

   if not proc.use_me:
      print("** have combine block, but no ME?")
      return 1

   apply_uopt_to_block('-combine_method', user_opts, block)
   apply_uopt_to_block('-combine_opts_tedana', user_opts, block)
   apply_uopt_to_block('-combine_opts_tedwrap', user_opts, block)
   apply_uopt_to_block('-combine_tedana_path', user_opts, block)
   apply_uopt_to_block('-combine_tedort_reject_midk', user_opts, block)

   ocmeth, rv = block.opts.get_string_opt('-combine_method', default='OC')
   if rv:
      return

   # verify that there are enough echoes, and that the method seems appropriate
   if proc.num_echo <= 1 and ocmeth != 'mean':
      print("** at least 2 -echo_times are required for non-mean combining")
      return 1
   if proc.num_echo == 2 and ocmeth in ['OC', 'OC_A']:
      print("** cannot use combine method %s with only 2 echoes" % ocmeth)
      print("   (consider method OC_B)")
      return 1

   # if using tedana for data and later blurring, suggest -blur_in_mask
   if ocmeth.find('tedana') >= 0 and \
         proc.find_block_order('combine', 'blur') == -1 :
      if not proc.user_opts.have_yes_opt('-blur_in_mask'):
         # okay, finally whine here
         print("** when using tedana results, consider '-blur_in_mask yes'")

   # note the applied method
   proc.combine_method = ocmeth

   # and if we need 'tedana', require it at execution time
   if proc.combine_method.find('m_ted') >= 0:
      proc.required_progs.append('tedana')

   block.valid = 1

def db_cmd_combine(proc, block):
   """combine all echoes

      This will grow to include options like:
         ave              : simple average
         OC               : optimally combine
         OC_tedort        : OC, but get ortvec from tedana
                            (@extract_meica_ortvec)
         tedana           : run tedana.py to get dn_ts_OC.nii
         tedana_OC        : run tedana.py to get OC result (ts_OC.nii)
         tedana_OC_tedort : tedana_OC, and get ortvec for later projection
                            (@extract_meica_ortvec)
   """

   if not proc.use_me:
      print("** creating combine block, but no ME?")
      return

   ocmeth, rv = block.opts.get_string_opt('-combine_method', default='OC')
   if rv: return

   # should not be reachable, but at least for clarity...
   if ocmeth not in g_oc_methods:
      return

   # check use of tedana.py vs. tedana from MEICA group
   ted_meth = which_tedana_method(ocmeth)
   if ted_meth < 0: return

   # do we want to use the MEICA group's tedana version?
   if ted_meth == 2:
      mcstr = '\n# (tedana from MEICA group)'
   elif ted_meth == 1:
      mcstr = '\n# (tedana.py from Prantik)'
   else:
      mcstr = ''

   # check that masking seems reasonable
   if ted_meth > 0:
      if not have_tedana_mask(proc, block, ocmeth):
         return

   # write commands
   cmd =  '# %s\n'                                                   \
          '# combine multi-echo data per run, using method %s%s\n\n' \
          % (block_header('combine'), ocmeth, mcstr)

   # handle any MEICA tedana methods separately
   if ted_meth == 2:
      if ocmeth == 'OC_m_tedort':
         # AFNI OC, but get tedorts from m_tedana
         ccmd = cmd_combine_OC(proc, block, ocmeth)
         tcmd = cmd_combine_m_tedana(proc, block, 'getorts')
         if ccmd is None or tcmd is None: return
         ccmd += tcmd
      else:
         ccmd = cmd_combine_m_tedana(proc, block, ocmeth)
   elif ocmeth == 'mean':
      ccmd = cmd_combine_mean(proc, block)
   elif ocmeth[0:2] == 'OC':
      ccmd = cmd_combine_OC(proc, block, ocmeth)
      if ocmeth == 'OC_tedort' and ccmd is not None:
         # now ALSO run tedana to get ortvecs
         tcmd = cmd_combine_tedana(proc, block, 'getorts')
         if tcmd is None: return
         ccmd += tcmd
   elif ocmeth[0:6] == 'tedana':
      ccmd = cmd_combine_tedana(proc, block, ocmeth)
   else:
      print("** invalid combine method: %s" % ocmeth)
      return ''

   if ccmd == None: return

   cmd += ccmd

   # importantly, we are now done with ME processing
   # (do not apply ME script changes anymore)
   proc.use_me = 0

   # the applied combine_method is stored in mod()

   return cmd

def have_tedana_mask(proc, block, method):
   # make sure a mask block precedes this one
   bo = proc.find_block_order('mask', block.label)
   if bo == -2 or proc.mask == None:
      print("** a mask is required for tedana")
      return 0
   if bo != -1:
      print("** processing block 'mask' should precede 'combine'\n"
            "   when running tedana (MEICA)")
      return 0

   if proc.mask != proc.mask_epi_anat:
      print('** consider option: "-mask_epi_anat yes"')

   return 1

def which_tedana_method(ocmeth):
   """There are a few ways to apply tedana now, see if we can distinguish.

      return 0: none
             1: tedana.py (via tedana_wrapper.py)
             2: tedana    (from MEICA group)
            -1: error

      Note whether tedana (i.e. MEICA group's version) is directly in PATH.
      Do we care if tedana.py is in the PATH?
   """

   # if 'ted' is not part of ocmeth, then tedana is not relevant
   if ocmeth.find('ted') < 0:
      return 0

   # does the user want to use MEICA tedana?
   want_mted = (ocmeth.find('m_ted') >= 0)

   # is 'tedana' in PATH?
   which_mted = UTIL.which('tedana')
   have_mted = (which_mted != '')

   # requesting MEICA tedana but not having it (should this be fatal?)
   # (note: being fatal would complicate my own testing)
   if want_mted and not have_mted:
      print("** WARNING: no MEICA tedana in path, but requesting it")

   # if it exists, we do not HAVE to use it
   if have_mted and not want_mted:
      print("** warning: have MEICA tedana in path, but not requesting it")
      print("   MEICA tedana is at: %s" % which_mted)

   # now just return whether we use the new or old tedana
   if want_mted:
      print("-- will use tedana from MEICA group")
      return 2
   else:
      print("-- will use tedana.py")
      return 1

def cmd_combine_m_tedana(proc, block, method='m_tedana'):
   """combine all echoes using the MEICA group's tedana

      method must currently be one of the following
         m_tedana         : default - run tedana_wrapper.py, collect output

      get orts
   """

   if not proc.use_me:
      print("** creating combine block, but no ME?")
      return

   if len(proc.echo_times) == 0:
      print("** option -echo_times is required for %s combine method" % method)
      return

   # tedort: do we project good components from bad?
   if block.opts.have_yes_opt('-combine_tedort_reject_midk', default=0):
      print("** m_tedana not ready for tedort (might not be appropriate)")
      return
      midk_opt = '1'
   else:
      midk_opt = '0'

   # init any extra tedana options
   exopts = []

   # ----------------------------------------------------------------------
   # decide what to do
   #    - what output to copy, if any (and a corresponding comment)
   #    - whether to grab the ortvec
   ready = 1            # flag what still needs to be done
   convention = 'orig'  # orig or bids convention for output
   if method == 'm_tedana':
      dataout = 'dn_ts_OC.nii.gz'
      getorts = 0
      mstr = '# (get MEICA tedana final result, %s)\n\n' % dataout
   elif method == 'm_tedana_OC':
      dataout = 'ts_OC.nii.gz'
      getorts = 0
      mstr = '# (get MEICA tedana OC result, %s)\n\n' % dataout
   elif method == 'm_tedana_m_tedort':
      exopts.append('--tedort')
      dataout = 'dn_ts_OC.nii.gz'   # same name as m_tedana
      getorts = 0
      mstr = '# (get MEICA tedana OC result, %s, plus -ortvec)\n\n' \
             % dataout
   # methods that need to extract ortvecs...
   elif method == 'm_tedana_OC_tedort':
      convention = 'bids'  # dataout and components will have bids naming
      exopts.append('--tedort')
      dataout = 'desc-optcom_bold.nii.gz'
      getorts = 1
      mstr = '# (get MEICA tedana OC result, %s, and get tedorts)\n\n' \
             % dataout
   elif method == 'getorts':
      convention = 'bids'  # dataout and components will have bids naming
      exopts.append('--tedort')
      dataout = ''
      getorts = 1
      mstr = '# (get MEICA tedana -tedort regressors)\n\n'
   else:
      print("** invalid tedana combine method, %s" % method)
      return

   # rcr - todo: remove after cases are done
   if not ready: return

   oindent = ' '*6

   # gather any extra tedana options
   oname = '-combine_opts_tedana'
   if block.opts.find_opt(oname):
      olist, rv = block.opts.get_string_list(oname)
      if rv: return
      if len(olist) > 0:
         exopts.extend(olist)
      else:
         print("** found -combine_opts_tedana without any options")
         return

   # note the tedana version
   vstr = '# note the version of tedana (only capure stdout)\n' \
          'tedana --version | tee out.tedana_version.txt\n\n'

   # input prefix has $run fixed, but uses a wildcard for echoes
   # output prefix has $run fixed, but no echo var
   # 
   cur_prefix = proc.prefix_form_run(block, eind=-9)
   prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=-2)
   fave_prefix = proc.prev_prefix_form_run(block, view=1, eind=-1)
   if len(exopts) > 0:
      exoptstr = '          %s \\\n' % ' '.join(exopts)
   else:
      exoptstr = ''

   # actually run tedana
   # rcr - todo: consider tracking --tedpca, with default of kundu-stabalize
   #             consider --png
   # note: tedana will mask if we don't, so always specify one
   cmd =                                                             \
       '# see also: https://tedana.readthedocs.io\n\n'               \
       '%s'                                                          \
       '%s'                                                          \
       '# first run tedana commands, to see if they all succeed\n'   \
       'foreach run ( $runs )\n'                                     \
       '   tedana -d %s \\\n'                                        \
       '          -e $echo_times \\\n'                               \
       '          --mask %s  \\\n'                                   \
       '%s'                                                          \
       '          --out-dir tedana_r$run --convention %s\n'          \
       'end\n\n'                                                     \
       % (vstr, mstr, prev_prefix, proc.mask.nice_input(head=1),
          exoptstr, convention)


   # ----------------------------------------------------------------------
   # only copy the results back out if dataout is set
   if dataout != '':
      cmd += '# now copy the tedana results\n'                      \
             'foreach run ( $runs )\n'                              \
             '   # copy, but get space/view from tedana input\n'    \
             '   3dcalc -a %s \\\n'                                 \
             '          -b tedana_r$run/%s \\\n'                    \
             '          -prefix %s \\\n'                            \
             '          -expr b -datum float\n'                     \
             'end\n\n'                                              \
             % (fave_prefix, dataout, cur_prefix)

   # ----------------------------------------------------------------------
   # finally, grab the orts, if desired
   if getorts:
      # here, all orts are tedort (reject, with accept projected out)
      # mix is tsv with components/regressors, metrics has component details
      mixfile = 'tedana_r$run/desc-ICAOrth_mixing.tsv'
      metfile = 'tedana_r$run/desc-tedana_metrics.tsv'
      ocmd = '# extract orthogonalized projection terms\n'              \
             'mkdir meica_orts\n'                                       \
             'foreach run ( $runs )\n'                                  \
             '   1d_tool.py -infile %s \\\n'                            \
             '              -select_cols_via_TSV_table %s \\\n'         \
             '                 Component classification=rejected \\\n'  \
             '              -write meica_orts/sorts_r$run.1D -verb 2\n' \
             '\n' % (mixfile, metfile)

      ocmd+= '   # pad single run terms across all runs\n'           \
             '   1d_tool.py -infile meica_orts/sorts_r$run.1D  \\\n' \
             '              -set_run_lengths $tr_counts        \\\n' \
             '              -pad_into_many_runs $run %d        \\\n' \
             '              -write meica_orts/morts_r$run.1D\n'      \
             'end\n\n' % (proc.runs)
      cmd += ocmd

      # now make note of the files for the regress block
      for rind in range(proc.runs):
          label = 'morts_r%02d' % (rind+1)
          ortfile = 'meica_orts/%s.1D' % label
          proc.regress_orts.append([ortfile, label])

   return cmd

def cmd_combine_tedana(proc, block, method='tedana'):
   """combine all echoes using the tedana.py wrapper, tedana_wrapper.py

      1. for each run, get weights (with run-specific prefix)
      2. average those weights across runs (nzmean? not necessary?)
      3. apply 

      method must currently be one of the following
         tedana           : default - run tedana_wrapper.py, collect output
         getorts          : get projection matrix from both rejected lists
                            (project all bad from good and store ortvecs)
                            This is an internal method (for OC_tedort).
         tedana_OC_tedort : 
   """

   if not proc.use_me:
      print("** creating combine block, but no ME?")
      return

   if len(proc.echo_times) == 0:
      print("** option -echo_times is required for 'OC' combine method")
      return

   # ----------------------------------------------------------------------
   # decide what to do
   #    - what output to copy, if any (and a corresponding comment)
   #    - whether to grab the ortvec

   if method == 'tedana':
      getorts = 0
      dataout = 'dn_ts_OC.nii'
      mstr = '# (run tedana.py and get final result, %s)\n\n' % dataout
   elif method == 'tedana_OC':
      getorts = 0
      dataout = 'ts_OC.nii'
      mstr = '# (run tedana.py and get OC result, %s)\n\n' % dataout
   elif method == 'tedana_OC_tedort':
      getorts = 1
      dataout = 'ts_OC.nii'
      mstr = '# (run tedana.py to get OC result, %s, plus -ortvec)\n\n' \
             % dataout
   elif method == 'getorts':
      getorts = 1
      dataout = ''
      mstr = '# (run tedana.py to get -ortvec results)\n\n'
   else:
      print("** invalid tedana combine method, %s" % method)
      return

   oindent = ' '*6

   # gather any extra options
   exopts = []
   oname = '-combine_opts_tedwrap'
   opt = block.opts.find_opt(oname)
   if opt:
      olist, rv = block.opts.get_string_list(oname)
      if rv: return
      if len(olist) > 0:
         exopts.append("%s%s \\\n" % (oindent,' '.join(olist)))
      else:
         print("** found -combine_opts_tedwrap without any options")
         return

   # gather any extra tedana options
   oname = '-combine_opts_tedana'
   opt = block.opts.find_opt(oname)
   if opt:
      olist, rv = block.opts.get_string_list(oname)
      if rv: return
      if len(olist) > 0:
         exopts.append("%s-tedana_opts ' %s' \\\n" % (oindent,' '.join(olist)))
      else:
         print("** found -combine_opts_tedana without any options")
         return

   # maybe the user specified a tedana.py path
   oname = '-combine_tedana_path'
   val, rv = block.opts.get_string_opt(oname)
   if val and not rv:
      if not os.path.isfile(val):
         print("** warning %s file does not seem to exist:\n   %s" \
               % (oname, val))
      exopts.append('%s-tedana_prog %s \\\n' % (oindent, val))

   # use -save_all?
   save_opt = ''
   if block.opts.have_yes_opt('-combine_tedana_save_all', 0):
      save_opt = '%s-save_all \\\n' % oindent

   # input prefix has $run fixed, but uses a wildcard for echoes
   # output prefix has $run fixed, but no echo var
   # 
   cur_prefix = proc.prefix_form_run(block, eind=-9)
   prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=-2)
   exoptstr = ''.join(exopts)

   # decide whether we need to update the view and space
   if dataout != '' and proc.view and (proc.view != '+orig'):
      spaceset = proc.prev_prefix_form(1, block, view=1, eind=1)
      spacestr = '# make note of the space, for adjusting the output\n' \
                 'set space = `3dinfo -space %s`\n\n' % spaceset
   else:
      spacestr = ''

   # actually run tedana.py
   cmd =  '# ----- method %s : generate TED (MEICA) results  -----\n'   \
          '%s'                                                          \
          '%s'                                                          \
          '# first run tedana.py commands, to see if they all succeed\n'\
          'foreach run ( $runs )\n'                                     \
          '   tedana_wrapper.py -input %s \\\n'                         \
          '      -TE $echo_times \\\n'                                  \
          '      -mask %s  \\\n'                                        \
          '      -results_dir tedana_r$run \\\n'                        \
          '      -ted_label r$run \\\n'                                 \
          '%s'                                                          \
          '%s'                                                          \
          '      -prefix tedprep\n'                                     \
          'end\n\n'                                                     \
          % (method, mstr, spacestr, prev_prefix, proc.mask.shortinput(),
             save_opt, exoptstr)
 

   # ----------------------------------------------------------------------
   # only copy the results back out if dataout is set
   if dataout != '':
      # prepare for fixing view before copying the results back
      if proc.view and (proc.view != '+orig'):
         # if we set the space variable, apply it, too
         if spacestr != '':
            spacestr = ' -space $space'
         else:
            spacestr = ''
         rcmt = ' (and fix view)'
         rcmd = '\n'                                \
                '   # and adjust view from +orig\n' \
                '   3drefit -view %s%s %s+orig\n' \
                % (proc.view[1:], spacestr, cur_prefix)
      else:
         rcmt = ''
         rcmd = ''

      # and copy the results back
      cmd += '# now get the tedana.py results%s\n'         \
             'foreach run ( $runs )\n'                     \
             '   # copy result back here\n'                \
             '   3dcopy tedana_r$run/TED.r$run/%s %s\n'    \
             '%s'                                          \
             'end\n\n'                                     \
             % (rcmt, dataout, cur_prefix, rcmd)

   # ----------------------------------------------------------------------
   # finally, grab the orts, if desired
   if getorts:
      # for now, keep default of rejecting midk
      # - be explicit about the option, so people see it
      if block.opts.have_yes_opt('-combine_tedort_reject_midk', default=1):
         midk_opt = '1'
      else:
         midk_opt = '0'

      ocmd = '# create orthogonalized projection terms\n'  \
             'mkdir meica_orts\n'                          \
             'foreach run ( $runs )\n'                     \
             '   @extract_meica_ortvec -meica_dir tedana_r$run/TED.r$run \\\n'\
             '                         -reject_midk %s \\\n'                  \
             '                         -work_dir tedana_r$run/work.orts \\\n' \
             '                         -prefix tedana_r$run/meica_orts.1D\n\n'\
             '   # pad single run terms across all runs\n' \
             '   1d_tool.py -infile tedana_r$run/meica_orts.1D \\\n' \
             '              -set_run_lengths $tr_counts        \\\n' \
             '              -pad_into_many_runs $run %d        \\\n' \
             '              -write meica_orts/morts_r$run.1D\n'      \
             'end\n\n'                                               \
             % (midk_opt, proc.runs)

      # now make note of the files for the regress block
      for rind in range(proc.runs):
          label = 'morts_r%02d' % (rind+1)
          ortfile = 'meica_orts/%s.1D' % label
          proc.regress_orts.append([ortfile, label])

      cmd += ocmd

   return cmd


def cmd_combine_OC(proc, block, method='OC'):
   """combine all echoes using OC via @compute_OC_weights

      1. for each run, get weights (with run-specific prefix)
      2. average those weights across runs (nzmean? not necessary?)
      3. apply 

      method must currently be one of OC, OC_A, OC_B
         OC     : default
         OC_A   : from Javier's notes
         OC_B   : regress from log() time series, rather than log(mean())
                  (this is the default == OC)
   """

   if not proc.use_me:
      print("** creating combine block, but no ME?")
      return

   if len(proc.echo_times) == 0:
      print("** option -echo_times is required for 'OC' combine method")
      return

   if method in ['OC', 'OC_tedort', 'OC_m_tedort']:
      mstr = ''
   elif method == 'OC_A' or method == 'OC_B':
      mstr = '        -oc_method %s   \\\n' % method
   else:
      print("** invalid OC combine method, %s" % method)
      return
     

   # input prefix has $run fixed, but uses a wildcard for echoes
   # output prefix has $run fixed, but no echo var
   cur_prefix = proc.prefix_form_run(block, eind=-9)
   prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=-2)

   cmd =  '# ----- optimally combine echoes -----\n\n'              \
          '# get weights for each run\n'                            \
          'foreach run ( $runs )\n'                                 \
          '    @compute_OC_weights -echo_times "$echo_times" \\\n'  \
          '%s'                                                      \
          '        -echo_dsets %s   \\\n'                           \
          '        -prefix oc.weights.r$run  \\\n'                  \
          '        -work_dir oc.work.r$run\n'                       \
          'end\n\n' % (mstr, prev_prefix)

   proc.OC_weightset = gen_afni_name('oc.weights.$subj%s' % proc.view)
   cmd += '# average weights across runs\n'          \
          '3dMean -prefix %s oc.weights.r*.HEAD\n\n' \
          % proc.OC_weightset.out_prefix()

   cmd += '# apply weights to each run\n'           \
          'foreach run ( $runs )\n'                    \
          '    3dMean -weightset %s \\\n'           \
          '           -prefix %s \\\n'          \
          '           %s\n'                 \
          'end\n\n' % (proc.OC_weightset.shortinput(), cur_prefix, prev_prefix)

   return cmd


def cmd_combine_mean(proc, block):
   """combine all echoes via simple mean
   """

   # input prefix has $run fixed, but uses a wildcard for echoes
   # output prefix has $run fixed, but no echo var
   cur_prefix = proc.prefix_form_run(block, eind=-9)
   prev_prefix = proc.prev_prefix_form_run(block, view=1, eind=-2)

   cmd = '# for now, just average across echo sets\n' \
         'foreach run ( $runs )\n'                    \
         '    3dMean -prefix %s %s\n'                 \
         'end\n\n' % (cur_prefix, prev_prefix)

   return cmd

# --------------- motsim block ---------------

def db_mod_motsim(block, proc, user_opts):
   """init proc.motsim_dsets keys to any found labels
      (rather than translating options)
      note: there are currently no -motsim options
   """

   # check for updates to -tshift_align_to option
   errs = 0
   oname = '-volreg_motsim_create'
   mdsets = proc.motsim_dsets   # dict of afni_name's (use None for now)
   optlist = user_opts.find_all_opts(oname)
   for opt in optlist:
      for par in opt.parlist:
         if par not in motsim_types:
            print('** invalid %s type %s, not in %s' \
                  % (oname, par, ', '.join(motsim_types)))
            errs += 1
         if par not in mdsets:
            mdsets[par] = None

   oname = '-regress_motsim_PC'
   optlist = user_opts.find_all_opts(oname)
   for opt in optlist:
      # parlist is of form: TYPE #PCs
      par = opt.parlist[0]
      if par not in motsim_types:
         print('** invalid %s type %s, not in %s' \
               % (oname, par, ', '.join(motsim_types)))
         errs += 1
      if par not in mdsets:
         mdsets[par] = None

   if errs: return 1

   # #PCs will be added to the afni_name object before db_cmd_regress

   if proc.verb > 2:
      print('-- will create motsim dset types: %s' \
            % ', '.join(list(mdsets.keys())))

   block.valid = 1

def db_cmd_motsim(proc, block):

   mdsets = proc.motsim_dsets
   mdkeys = list(proc.motsim_dsets.keys())
   if proc.verb>0: print('-- creating motsim dset types: %s'%', '.join(mdkeys))

   # first create afni_names for MS dsets
   # (view from proc.vr_base_dset, or proc.view for warped)

   return


# check all -surf options
def db_mod_surf(block, proc, user_opts):
    """initialize all main surface-based proc vars, based on user_opts
       (surf_anat should already be initialized, as it may init the Block list)

       REQUIRED vars: surf_anat, surf_spec

       also init computed vars:
         - surf_sv (surf_anat or local aligned version)
                   (also updated in update_surf_sv)
         - surf_spec_dir (spec directory, always an absolute path)
         - surf_spd_var (variable used for spec directory, e.g. $spec_dir)
         - surf_spec_var_iter (iteration variable, e.g. $hemi)
         - surf_hemilist (hemispheres to iterate over, e.g. ['lh', 'rh'])
         - surf_spec_var (spec names, but with ${hemi} for lh/rh)
         - surf_sv_dir  (initialized in update_surf_sv)
         - surf_svd_var (initialized in update_surf_sv)

       return None on success, else an error condition
    """

    if len(block.opts.olist) == 0: # init options
        pass

    # surf_anat must be set ahead of time, to signal surface analysis
    if not proc.surf_anat: return None  # nothing to do

    ### do we really need block.opts?  just the the proc vars here...

    opt = user_opts.find_opt('-surf_spec')
    if opt:
       proc.surf_spec = opt.parlist
       if not UTIL.okay_as_lr_spec_names(proc.surf_spec):
          print('\n'                                                         \
                '** error: spec files MUST contain lh or rh, and otherwise\n'\
                '   match (consider making copies, like SUBJ.stdmesh.lh.spec)')
          return None
       tjunk, pdirs, sjunk, snames = UTIL.common_parent_dirs([proc.surf_spec])
       # set spec dir (do not allow to be trivial or relative), var and dir_var
       if len(pdirs) > 0: proc.surf_spec_dir = pdirs[0]
       if UTIL.is_trivial_dir(proc.surf_spec_dir):
           proc.surf_spec_dir = os.path.abspath('.')
       else: proc.surf_spec_dir = os.path.abspath(proc.surf_spec_dir)
       proc.surf_spd_var = 'surface_dir'

       # set spec_var (and iter/ref) like 'steve.${hemi}.spec'
       proc.surf_spec_var_iter = 'hemi'
       if proc.sep_char == '.': proc.surf_svi_ref = '$hemi'
       else:                    proc.surf_svi_ref = '${hemi}'
       proc.surf_spec_var, proc.surf_hemilist = UTIL.make_spec_var(snames[0],
                                                                vname='hemi')
       if not proc.surf_spec_var or not proc.surf_hemilist:
          print('** error: failed to make spec var from %s' % snames[0])
          return None

       # note basename of first spec file for later use
       proc.surf_spec_base = snames[0]

    opt = user_opts.find_opt('-surf_anat_aligned')
    if opt: proc.surf_anat_aligned = opt.parlist[0]

    opt = user_opts.find_opt('-surf_anat_has_skull')
    if opt: proc.surf_anat_has_skull = opt.parlist[0]

    opt = user_opts.find_opt('-surf_A')
    if opt: proc.surf_A = opt.parlist[0]

    opt = user_opts.find_opt('-surf_B')
    if opt: proc.surf_B = opt.parlist[0]

    if proc.verb > 2:
        print('-- surf info\n'          \
              '   spec          : %s\n' \
              '   anat          : %s\n' \
              '   anat_aligned  : %s\n' \
              '   anat_has_skull: %s\n' \
              '   surf_A        : %s\n' \
              '   surf_B        : %s\n' \
              '   spec_dir      : %s\n' \
              '   surf_spd_var  : %s\n' \
              '   spec_var      : %s\n' \
              % (proc.surf_spec, proc.surf_anat, proc.surf_anat_aligned,
                 proc.surf_anat_has_skull, proc.surf_A, proc.surf_B,
                 proc.surf_spec_dir, proc.surf_spd_var, proc.surf_spec_var))

    errs = 0
    if not proc.surf_anat.exist():
        print('** error: missing -surf_anat dataset: %s' % proc.surf_anat.input())
        errs += 1
    if not proc.surf_spec:
        print('** error: missing -surf_spec option')
        return 1
    if not os.path.isfile(proc.surf_spec[0]):
        print('** error: missing -surf_spec file: %s' % proc.surf_spec[0])
        errs += 1
    if proc.surf_spec_dir and not os.path.isdir(proc.surf_spec_dir):
        print('** error: spec file directory not found: %s' % proc.surf_spec_dir)
        errs += 1
    if errs: return 1   # fail

    # init surf_sv to surf_anat, based on remote location
    update_surf_sv(proc, proc.surf_anat, remote_dir=1)

    block.valid = 1

def update_surf_sv(proc, dset, remote_dir=0):
    """set surf_sv, surf_sv_dir, surf_svd_var

       if remote_dir and dset has no path, use cwd

       if set, always use absolute path
    """
    if not dset: return

    # might be here just to set directories
    if proc.surf_sv != dset: proc.surf_sv = dset

    if UTIL.is_trivial_dir(dset.path):
        if remote_dir: proc.surf_sv_dir = os.path.abspath('.')
        else:          proc.surf_sv_dir = ''
    else: proc.surf_sv_dir = os.path.abspath(dset.path)

    # set or clear sv dir var, depending on spec dir, as well
    if proc.surf_sv_dir == proc.surf_spec_dir:
        proc.surf_svd_var = proc.surf_spd_var
    elif proc.surf_sv_dir:  proc.surf_svd_var = 'sv_dir'
    else:                   proc.surf_svd_var = ''

    if proc.verb > 2:
       print('-- surf_sv       : %s\n' \
             '   surf_sv_dir   : %s\n' \
             '   surf_svd_var  : %s\n' \
             % (proc.surf_sv.pv(), proc.surf_sv_dir, proc.surf_svd_var))

def db_cmd_surf(proc, block):
    """use 3dVol2Surf to map volume data to surface

         - set initial variables for spec_dir and sv_dir (if different)

         - call update_surf_sv(), depending on local alignment
         - possibly align surface volume to local anat (requires -copy_anat)
         - set variable for sv_dir (maybe spec_dir, maybe nothing)
    """

    if proc.surf_anat == None:
        print('** error: missing surf_anat')
        return None

    if not proc.surf_A or not proc.surf_B:
        print('** error: both surf_A and surf_B are currently required')
        return None

    cmd = "# %s\n"                                      \
          "# map EPI data to the surface domain\n\n"    \
          % block_header('surf (map data to surface)')

    # assign surf vol and spec file directories (might just need one)
    cmd += "# set directory variables\n"        \
           "set %s = %s\n" % (proc.surf_spd_var, proc.surf_spec_dir)
    if proc.surf_svd_var != proc.surf_spd_var:
        cmd += "set %s = %s\n" % (proc.surf_svd_var, proc.surf_sv_dir)
    cmd += '\n'

    # maybe align sv to current anat
    if proc.surf_anat_aligned != 'yes':
        scmd = cmd_surf_align(proc)
        if scmd == None: return None
        cmd += scmd

    cmd += cmd_vol2surf(proc, block)

    # add a command script for running suma
    if proc.suma_cmd_file: cmd += write_suma_script(proc, block)

    return cmd

def write_suma_script(proc, block):
    """make a convenience script for running suma, since we know inputs"""
    if not proc.suma_cmd_file: return
    if not proc.surf_sv: return
    if len(proc.surf_spec_base) < 1: return

    # set variables for spec and sv directories

    if proc.surf_spd_var: specd = '$%s/' % proc.surf_spd_var
    else:                 specd = ''

    if proc.surf_svd_var: svd = '$%s/' % proc.surf_svd_var
    else:                 svd = ''

    cmd = '# make local script for running suma, and make it executable\n' \
          'echo suma -spec %s%s \\\n'                   \
          '          -sv %s%s > %s\n'                   \
          'chmod 755 %s\n\n'                            \
          % (specd, proc.surf_spec_base[0], svd,
             proc.surf_sv.pv(), proc.suma_cmd_file, proc.suma_cmd_file)

    return cmd

def cmd_vol2surf(proc, block):
    """map volume data to each hemisphere's surface"""

    # string for foreach hemi loop
    feh_str = 'foreach %s ( %s )\n' \
              % (proc.surf_spec_var_iter, ' '.join(proc.surf_hemilist))

    # string for -spec
    spec_str = '$%s/%s' % (proc.surf_spd_var, proc.surf_spec_var)

    # string for -sv
    if proc.surf_svd_var == '': svd_str = proc.surf_sv.pv()
    else: svd_str = '$%s/%s' % (proc.surf_svd_var, proc.surf_sv.pv())

    prev   = proc.prev_prefix_form_run(block, view=1)
    proc.surf_names = 1 # from now on, we want surface based dset names
    prefix = proc.prefix_form_run(block)

    cmd = '# map volume data to the surface of each hemisphere\n'       \
          '%s'                                                          \
          '    foreach run ( $runs )\n'                                 \
          '        3dVol2Surf -spec %s \\\n'                            \
          '                   -sv %s \\\n'                              \
          '                   -surf_A %s \\\n'                          \
          '                   -surf_B %s \\\n'                          \
          '                   -f_index nodes \\\n'                      \
          '                   -f_steps 10 \\\n'                         \
          '                   -map_func ave \\\n'                       \
          '                   -oob_value 0 \\\n'                        \
          '                   -grid_parent %s \\\n'                     \
          '                   -out_niml %s \n'                          \
          '    end\n'                                                   \
          'end\n\n'                                                     \
          % (feh_str, spec_str, svd_str, proc.surf_A, proc.surf_B, prev, prefix)

    return cmd

def cmd_surf_align(proc):
    """return @SUMA_AlignToExperiment command"""

    if not proc.surf_anat: return ''

    if not proc.anat_final:
        print('** missing final anat to align to as experiment base')
        return None

    # current surf_sv is surely remote, so apply directory and variables
    if proc.surf_anat_has_skull == 'yes': sstr = ' -strip_skull surf_anat'
    else: sstr = ''

    # the new surf_sv will be the aligned one
    # (convert from anat_final rather than old surf_vol)
    newsv = proc.anat_final.new(new_pref='${subj}_SurfVol_Alnd_Exp')
    newsv.path = ''

    cmd = '# align the surface anatomy with the current experiment anatomy\n' \
          '@SUMA_AlignToExperiment -exp_anat %s \\\n'                         \
          '                        -surf_anat $%s/%s \\\n'                    \
          '                        -wd%s \\\n'                                \
          '                        -atlas_followers -overwrite_resp S\\\n'    \
          '                        -prefix %s \n\n'                           \
        % (proc.anat_final.pv(), proc.surf_svd_var, proc.surf_sv.pv(), sstr,
           newsv.prefix)

    # and apply the new surf_sv, along with directories
    update_surf_sv(proc, newsv)

    return cmd

def db_mod_blur(block, proc, user_opts):

    # handle surface data separately
    if proc.surf_anat: return mod_blur_surf(block, proc, user_opts)

    if len(block.opts.olist) == 0: # init blur option
        block.opts.add_opt('-blur_filter', 1, ['-1blur_fwhm'], setpar=1)
        block.opts.add_opt('-blur_opts_merge', -1, [])

    # check for option updates
    uopt = user_opts.find_opt('-blur_filter')
    bopt = block.opts.find_opt('-blur_filter')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]               # set filter

    # check for option updates
    uopt = user_opts.find_opt('-blur_in_mask')
    bopt = block.opts.find_opt('-blur_in_mask')
    if uopt:
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-blur_in_mask', 1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-blur_in_automask')
    bopt = block.opts.find_opt('-blur_in_automask')
    if uopt and not bopt: block.opts.add_opt('-blur_in_automask', 0, [])

    uopt = user_opts.find_opt('-blur_to_fwhm')
    if uopt: block.opts.add_opt('-blur_to_fwhm', 0, [])

    uopt = user_opts.find_opt('-blur_size')
    if uopt:
        bopt = block.opts.find_opt('-blur_size')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-blur_size', 1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-blur_opts_merge')
    bopt = block.opts.find_opt('-blur_opts_merge')
    if uopt and bopt: bopt.parlist = uopt.parlist

    # check for option updates
    uopt = user_opts.find_opt('-blur_opts_B2FW')
    if uopt:
        bopt = block.opts.find_opt('-blur_opts_B2FW')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-blur_opts_B2FW', 1, uopt.parlist, setpar=1)

    apply_uopt_to_block('-blur_opts_BIM', user_opts, block)

    block.valid = 1

def db_cmd_blur(proc, block):

    # first get blur size, then possibly handle surface case
    val, err = proc.user_opts.get_type_opt(float, '-blur_size')
    if err:
        print('** error: -blur_size requires float argument')
        return 1
    elif val is not None and val > 0.0:
        size = val
        havesize = 1
    else:
        size = 4.0
        havesize = 0

    # pass blur_size to APQC
    proc.uvars.set_var('blur_size', [str(size)])

    # --------------- handle surface data separately ---------------
    if proc.surf_anat: return cmd_blur_surf(proc, block, size, havesize)

    # check for filter update
    opt      = block.opts.find_opt('-blur_filter')
    filtname = opt.parlist[0]

    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(block, view=1)

    other_opts = ''

    # ME: might need to indent everything...
    if proc.use_me:
       indent = '    '
    else:
       indent = ''

    # if -blur_in_mask, use 3dBlurInMask (requires 1blur_fwhm)
    bopt = block.opts.find_opt('-blur_to_fwhm')
    if bopt:
       if not havesize:
           print('** warning: using default 4.0 mm FWHM as _resulting_ blur\n'\
                 '            (use -blur_size to adjust)')

       # set any mask option
       if block.opts.find_opt('-blur_in_automask'):
          mopt = ' -automask'
       else:
          mopt = ''
          mask = proc.mask
          if not mask_created(mask): mask = proc.mask_extents
          if mask_created(mask): mopt = ' -mask %s%s'%(mask.prefix, proc.view)
          else:
             print('** error: no mask for -blur_to_fwhm, failing...')
             return

       # any last request?
       opt = block.opts.find_opt('-blur_opts_B2FW')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '%s                 %s \\\n' \
                          % (indent, ' '.join(opt.parlist))

       # make command string
       cstr = "%s    3dBlurToFWHM -FWHM %s%s \\\n"        \
              "%s"                                      \
              "%s                 -input %s \\\n"         \
              "%s                 -prefix %s \n"          \
              % (indent, size, mopt, other_opts, indent, prev, indent, prefix)

    # if -blur_in_mask, use 3dBlurInMask (requires 1blur_fwhm)
    elif OL.opt_is_yes(block.opts.find_opt('-blur_in_mask')) or \
        block.opts.find_opt('-blur_in_automask'):

       # verify FWHM filter
       if filtname != '-1blur_fwhm' and filtname != '-FWHM':
          print("** error: 3dBlurInMask requires FWHM filter, have '%s'\n" \
                "   (consider scale of 1.36 for RMS->FWHM)" % (filtname))
          return

       # set any mask option
       if block.opts.find_opt('-blur_in_automask'):
           mopt = ' -automask'
       else:
          mopt = ''
          mask = proc.mask
          if not mask_created(mask): mask = proc.mask_extents
          if mask_created(mask): mopt = ' -Mmask %s%s'%(mask.prefix, proc.view)
          else:
             print('** warning: no mask for -blur_in_mask, still proceeding...')

       # any last request?
       opt = block.opts.find_opt('-blur_opts_BIM')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '%s             %s \\\n' \
                          % (indent, ' '.join(opt.parlist))

       # make command string
       cstr = "%s    3dBlurInMask -preserve -FWHM %s%s \\\n"    \
              "%s                 -prefix %s \\\n"              \
              "%s"                                              \
              "%s                 %s\n"                         \
              % (indent, str(size), mopt, indent, prefix,
                 other_opts, indent, prev)

    else: # default: use 3dmerge for blur
       # maybe there are extra options to append to the command
       opt = block.opts.find_opt('-blur_opts_merge')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '%s             %s \\\n' \
                          % (indent, ' '.join(opt.parlist))

       # if we have an extents mask, apply it if no scale block
       do_mask = proc.mask_extents != None and \
                 proc.find_block('scale',mn=block.index) == None

       if do_mask: tprefix = 'rm.%s' % prefix
       else:       tprefix = prefix

       cstr = "%s    3dmerge %s %s -doall -prefix %s \\\n"     \
              "%s"                                             \
              "%s            %s\n"                             \
              % (indent, filtname, str(size), tprefix, other_opts, indent,prev)

       if do_mask:
          cstr += "\n"                                                     \
                  "%s    # and apply extents mask, since no scale block\n" \
                  "%s    3dcalc -a %s%s -b %s \\\n"                        \
                  "%s           -expr 'a*b' -prefix %s\n"                  \
                  % (indent, indent, tprefix, proc.view,
                     proc.mask_extents.shortinput(), indent, prefix)

    cmd = "# %s\n"                              \
          "# blur each volume of each run\n"    \
          "foreach run ( $runs )\n" % block_header('blur')

    if proc.use_me:
       indent = '    '
       cmd += '%sforeach %s ( $echo_list )\n' % (indent, proc.echo_var[1:])

    cmd += cstr

    if proc.use_me:
       cmd += '%send\n' % indent

    cmd += "end\n\n"

    return cmd

def mod_blur_surf(block, proc, user_opts):

    # check for option updates
    uopt = user_opts.find_opt('-surf_smooth_niter')
    if uopt: block.opts.add_opt('-surf_smooth_niter', 1, uopt.parlist, setpar=1)

    # do not allow some options with surface-based analysis
    non_surf_opts = ['-blur_in_mask', '-blur_in_automask', '-blur_opts_merge']
    okay = 1
    for opt in non_surf_opts:
       if user_opts.find_opt(opt):
          print("** cannot use option '%s' with surface-based analysis"%opt)
          okay = 0
    if not okay:
       return

    block.valid = 1

def cmd_blur_surf(proc, block, bsize, havesize=1):
    """surface analysis: return a command to blur the data"""

    proc.surf_blur_fwhm = bsize
    if not havesize:
        print('** applying default -blur_size of %s mm FWHM' \
              % proc.surf_blur_fwhm)

    if proc.verb > 2:
       print('-- surf blur_size : %s\n' % proc.surf_blur_fwhm)

    # check for number of requested iterations
    niter, err = block.opts.get_type_opt(int, '-surf_smooth_niter')
    if err: return
    if niter != None: ss_opts = ' '*23 + '-Niter %s'%niter + ' \\\n'
    else:             ss_opts = ''

    cmd = "# %s\n" % block_header('blur (on surface)')

    # string for foreach hemi loop
    feh_str = 'foreach %s ( %s )\n' \
              % (proc.surf_spec_var_iter, ' '.join(proc.surf_hemilist))

    # string for -spec
    spec_str = '$%s/%s' % (proc.surf_spd_var, proc.surf_spec_var)

    prev   = proc.prev_prefix_form_run(block)
    prefix = proc.prefix_form_run(block)
    param_file = 'surf.smooth.params.1D'

    # get later params from smrec rather than stdout  30 Jan 2023 [rickr]
    blur_hist = '%s.1D.smrec' % prefix

    cmd +='%s'                                                          \
          '    foreach run ( $runs )\n'                                 \
          '        # to save time, estimate blur parameters only once\n'\
          '        if ( ! -f %s ) then\n'                               \
          '            SurfSmooth -spec %s \\\n'                        \
          '                       -surf_A %s \\\n'                      \
          '                       -input %s \\\n'                       \
          '                       -met HEAT_07 \\\n'                    \
          '                       -target_fwhm %s \\\n'                 \
          '                       -blurmaster %s \\\n'                  \
          '                       -detrend_master \\\n'                 \
          '                       -output %s \\\n'                      \
          '                       | tee %s\n'                           \
          '            set bfile  = %s\n'                               \
          '            set params = ( `1dcat $bfile | tail -n 1` )\n'   \
          '        else\n'                                              \
          '            # get blur params from smrec file\n'             \
          '            SurfSmooth -spec %s \\\n'                        \
          '                       -surf_A %s \\\n'                      \
          '                       -input %s \\\n'                       \
          '                       -met HEAT_07 \\\n'                    \
          '                       -Niter $params[1] \\\n'               \
          '                       -sigma $params[3] \\\n'               \
          '                       -output %s\n'                         \
          '        endif\n'                                             \
          '    end\n'                                                   \
          'end\n\n'                                                     \
          % (feh_str,
             param_file, spec_str, proc.surf_A, prev,
             proc.surf_blur_fwhm, prev, prefix, param_file,
             blur_hist, spec_str, proc.surf_A, prev, prefix)

    return cmd

def db_mod_mask(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-mask_type', 1, ['union'], setpar=1)
        block.opts.add_opt('-mask_dilate', 1, [0], setpar=1)
        block.opts.add_opt('-mask_rm_segsy', 1, ['yes'], setpar=1)
        block.opts.add_opt('-mask_test_overlap', 1, ['yes'], setpar=1)

    # check for user updates

    uopt = user_opts.find_opt('-mask_dilate')
    bopt = block.opts.find_opt('-mask_dilate')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print("** -mask_dilate requires an int nsteps (have '%s')" % \
                  uopt.parlist[0])
            block.valid = 0
            return 1

    apply_uopt_to_block('-mask_apply',        user_opts, block)
    apply_uopt_to_block('-mask_opts_automask',user_opts, block)
    apply_uopt_to_block('-mask_epi_anat',     user_opts, block)
    apply_uopt_to_block('-mask_rm_segsy',     user_opts, block)
    apply_uopt_to_block('-mask_segment_anat', user_opts, block)
    apply_uopt_to_block('-mask_segment_erode',user_opts, block)
    apply_uopt_to_block('-mask_test_overlap', user_opts, block)
    apply_uopt_to_block('-mask_type',         user_opts, block)
    apply_uopt_list_to_block('-mask_import',  user_opts, block)
    apply_uopt_list_to_block('-mask_intersect',user_opts, block)
    apply_uopt_list_to_block('-mask_union',   user_opts, block)

    proc.mask_epi = gen_afni_name('full_mask%s$subj' % proc.sep_char)

    # we have an EPI mask, add it to the roi_dict for optional regress_ROI
    if proc.mask_epi and not proc.have_roi_label('brain'):
       if proc.add_roi_dict_key('brain', proc.mask_epi): return 1

    # possibly note that we will add some automatic ROIs
    roilist = ['CSF', 'GM', 'WM']
    if block.opts.have_yes_opt('-mask_segment_anat', 0):
       for roi in roilist:
          proc.add_roi_dict_key(roi)
    if block.opts.have_yes_opt('-mask_segment_erode', 0):
       for roi in roilist:
          proc.add_roi_dict_key('%se' % roi)

    # possibly note -mask_import_ROIs
    oname = '-mask_import'
    for opt in block.opts.find_all_opts(oname):
       label = opt.parlist[0]
       aname = gen_afni_name('mask_import_%s' % label)
       if proc.add_roi_dict_key(label, aname=aname): return 1

    # add any intersection or union masks
    for oname in ['-mask_intersect', '-mask_union']:
       for opt in block.opts.find_all_opts(oname):
          label = opt.parlist[0]
          if proc.add_roi_dict_key(label): return 1

    proc.mask = proc.mask_epi   # default to referring to EPI mask

    block.valid = 1

def mask_created(mask):
    """check to see if the afni_name mask dataset was actually 'created'

       For mask datasets, the test is whether there is a created attribute,
       and it is set to 1.
    """

    if not isinstance(mask, BASE.afni_name): return 0

    if not hasattr(mask, 'created'): return 0

    # attribute exists
    return mask.created

# in this block, automatically make an EPI mask via 3dAutomask
# if possible: also make a subject anatomical mask (resampled to EPI)
#    - if -volreg_tlrc_[ad]warp, apply from tlrc anat
#    - if a2e, apply from anat_al
#    - if e2a, apply from anat_al with inverted transform
# if possible: also make a group anatomical mask
#    - only if tlrc block and -volreg_tlrc_warp
#    - apply from -tlrc_base
# add -mask_apply TYPE, TYPE in {epi, anat, group, extents}
#     (this would override -regress_apply_mask)
# if have anat_final, segment it and resample to match EPI
def db_cmd_mask(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-mask_type')
    mtype = opt.parlist[0]
    if mtype == 'union': minv = 0           # result must be greater than minv
    else:                minv = 0.999

    # if we have an EPI mask, set the view here
    if proc.mask_epi.view == '': proc.mask_epi.view = proc.view
    if not proc.mask: proc.mask = proc.mask_epi
    if not proc.mask:
        print('** ERROR: no mask dset for mask block')
        return

    opt = block.opts.find_opt('-mask_dilate')
    nsteps = opt.parlist[0]
    if nsteps > 0: dstr = '-dilate %d ' % nsteps
    else:          dstr = ''

    olist, rv = block.opts.get_string_list(opt_name='-mask_opts_automask')
    if olist != None: aopts = (' '.join(olist) + ' ')
    else:             aopts = ''

    prev = proc.prev_prefix_form_run(block, view=1, eind=-1)
    cmd = cmd + "# %s\n"                                                \
                "# create 'full_mask' dataset (%s mask)\n"              \
                "foreach run ( $runs )\n"                               \
                "    3dAutomask %s%s-prefix rm.mask_r$run %s\n"  \
                "end\n\n" % (block_header('mask'), mtype, aopts, dstr, prev)
    proc.have_rm = 1            # rm.* files exist

    # make the mask (3dMean/3dcalc/3dcopy -> 3dmask_tool 26 Jun 2014 [rickr])
    cmd = cmd + "# create union of inputs, output type is byte\n"            \
                "3dmask_tool -inputs rm.mask_r*%s.HEAD -union -prefix %s\n\n"\
                % (proc.view, proc.mask_epi.prefix)
    proc.mask_epi.created = 1  # so this mask 'exists' now

    # if possible make a subject anat mask, resampled to EPI
    # (and probably mask_epi_anat)
    if proc.warp_epi or not proc.anat_has_skull:
        mc = anat_mask_command(proc, block)
        if mc == None: return
        cmd = cmd + mc

    # warn users if -mask_epi_anat will not be applied
    if block.opts.have_yes_opt('-mask_epi_anat') and not proc.mask_epi_anat:
       print("** cannot apply -mask_epi_anat")
       if not proc.mask_epi:
          print("   (missing EPI mask)")
       if not proc.mask_anat:
          if proc.anat_has_skull:
             print("   (anat still has skull)")
          else:
             print("   (missing anat mask)")
       return

    # if possible make a group anat mask, resampled to EPI
    if proc.warp_epi & WARP_EPI_TLRC_WARP:
        mc = group_mask_command(proc, block)
        if mc == None: return
        cmd = cmd + mc

    # see if the user wants to choose which mask to apply
    opt = block.opts.find_opt('-mask_apply')
    if opt:
        mtype = opt.parlist[0]
        if   mtype == 'epi':     proc.mask = proc.mask_epi
        elif mtype == 'anat':    proc.mask = proc.mask_anat
        elif mtype == 'epi_anat':proc.mask = proc.mask_epi_anat
        elif mtype == 'group':   proc.mask = proc.mask_group
        elif mtype == 'extents': proc.mask = proc.mask_extents
        else:
           # otherwise, try as label
           aname = proc.get_roi_dset(mtype)
           if not aname:
              print("*** ERROR, invalid -mask_apply mask: %s" % mtype)
              print("    must be one of: epi, anat, epi_anat, group, extents")
              print("    or else       : be a known/imported ROI or mask")
              return
           proc.mask = aname

        if proc.verb > 1: print("++ applying mask as '%s'" % mtype)
        if proc.mask: proc.regmask = 1 # apply, if it seems to exist
        else:
            print("** ERROR: cannot apply %s mask" % mtype)
            return

    oname = '-mask_import'
    for opt in block.opts.find_all_opts(oname):
       label = opt.parlist[0]
       aname = proc.get_roi_dset(label)
       if not aname:
          print("** missing -mask_import ROI '%s'" % label)
          return
       aname = proc.roi_dict[label]
       aname.view = proc.view

       # and check grid
       dset = opt.parlist[1]
       dims = UTIL.get_3dinfo_val_list(dset, 'd3', float, verb=1)
       if not UTIL.lists_are_same(dims, proc.delta,
                    epsilon=abs(proc.delta[0]*0.01),doabs=1):
          # flag for resampling
          print("** bad dims for -mask_import dataset: \n" \
                "   %s\n"                                  \
                "   import dims = %s, analysis dims = %s"  \
                % (dset, dims, proc.delta))
          return

    scmd = mask_segment_anat(proc, block)
    if scmd == None: return
    cmd += scmd

    # create any intersection masks
    if block.opts.find_opt('-mask_intersect'):
       cc = get_cmd_mask_combine(proc, block, 'inter')
       if not cc: return
       cmd += cc

    # create any union masks
    if block.opts.find_opt('-mask_union'):
       cc = get_cmd_mask_combine(proc, block, 'union')
       if not cc: return
       cmd += cc

    if proc.verb: proc.show_roi_dict_keys(verb=(proc.verb-1))

    # do not increment block index or set 'previous' block label,
    # as there are no datasets created here

    return cmd

def mask_segment_anat(proc, block):
    """- return a string for segmenting anat
       - copy result current dir and resample
       - apply any classes to roi_dict (for possible regression)

       requires:
          - anat_final
          - ! (-mask_segment_anat == no)
          - either requested (-mask_segment_anat) or already skull-stripped

       return None on failure, else string
    """

    # ----------------------------------------------------------------------
    # make any segmentation masks

    if not block.opts.have_yes_opt('-mask_segment_anat', default=0):
       return ''        # default is now no

    if not proc.anat_final:
        if proc.verb > 1:
           print('-- no Segsy (either no anat_final or -mask_segment_anat no)')
        return ''
    # and proc.anat_has_skull:

    cin  = gen_afni_name('Segsy/Classes%s' % proc.view)
    cres = gen_afni_name('Classes_resam%s' % proc.view)
    mset = proc.prev_prefix_form(1, block, view=1)

    # maybe we will take more classes in some option...
    sclasses = ['CSF', 'GM', 'WM']
    cmd  = "# ---- segment anatomy into classes %s ----\n" % '/'.join(sclasses)

    cmd += "3dSeg -anat %s -mask AUTO -classes '%s'\n\n" \
               % (proc.anat_final.pv(), ' ; '.join(sclasses))

    if OL.opt_is_yes(block.opts.find_opt('-mask_rm_segsy')):
       proc.rm_list.append('Segsy')
       proc.rm_dirs = 1

    cmd += '# copy resulting Classes dataset to current directory\n'
    cmd += '3dcopy %s .\n\n' % cin.rpv()

    # make erosion ROIs?  (default = yes)
    erode = not OL.opt_is_no(block.opts.find_opt('-mask_segment_erode'))

    ### always continue and make ROI masks

    # list ROI labels for comments
    baseliststr = ' '.join(sclasses)
    if erode:
       eclasses = ['%se' % sc for sc in sclasses]
       commentstr = '(%s and %s)' % (baseliststr, ' '.join(eclasses))
    else:
       commentstr = '(%s)' % baseliststr

    # make ROIs per class, and erode them by default
    roiprefix = 'mask_${class}'
    cc = '# make individual ROI masks for regression %s\n' \
         'foreach class ( %s )\n' % (commentstr, baseliststr)

    # make non-eroded masks in either case
    cc += '   # unitize and resample individual class mask from composite\n' \
          '   3dmask_tool -input %s"<$class>" \\\n'                          \
          '               -prefix rm.mask_${class}\n' % (cin.rpv())
    cc += '   3dresample -master %s -rmode NN \\\n'                          \
          '              -input rm.mask_${class}%s -prefix %s_resam\n'       \
          % (mset, proc.view, roiprefix)

    # start with the default: erode by 1 voxel
    if erode:
       # generate eroded copes
       cc += '   # also, generate eroded masks\n'
       cc += '   3dmask_tool -input %s"<$class>" -dilate_input -1 \\\n' \
             '               -prefix rm.mask_${class}e\n' % (cin.rpv())
       cc += '   3dresample -master %s -rmode NN \\\n'  \
             '              -input rm.mask_${class}e%s -prefix %se_resam\n'\
             % (mset, proc.view, roiprefix)

    cc += 'end\n\n'

    cmd += cc

    # to generalize: -mask_autoROI_w_extern old_roi new_roi mask_dset
    #          e.g.: -mask_autoROI_w_extern CSFe Vent /my/vent/vmask+tlrc
    # if mask_autoclass_w_extern and have ROI label:
    #    resample extern to same master
    #       - note: extern comes via 3dcopy to proc.ext_automask_dict
    #               to check proc.ext_automask_dict.has_key(new_label)
    #    intersect with 3dmask_tool
    #    proc.add_roi_dict_key()

    proc.mask_classes = cres    # store, just in case

    # and create all dict keys
    for sc in sclasses:
       newname = gen_afni_name('mask_%s_resam%s' % (sc, proc.view))
       if proc.add_roi_dict_key(sc, newname, overwrite=1): return ''
       if erode:
          ec = '%se' % sc
          newname = gen_afni_name('mask_%s_resam%s'%(ec,proc.view))
          if proc.add_roi_dict_key(ec, newname, overwrite=1): return ''

    return cmd


def get_cmd_mask_combine(proc, block, oper='union'):
    """operation ostr should be union or inter"""
    if oper == 'union':   ostr = 'union'
    elif oper == 'inter': ostr = 'intersect'
    else:
       print('** GCMC: bad oper %s' % oper)
       return ''

    oname = '-mask_%s' % ostr
    cmd = ''
    for opt in block.opts.find_all_opts(oname):
       olist, rv = block.opts.get_string_list(opt=opt)
       if rv: return ''
       ilabel = olist[0]   # label for resulting intersection mask
       alabel = olist[1]   # label A (e.g. 3dSeg CSFe)
       blabel = olist[2]   # label B (e.g. imported ventricle mask)

       aset = proc.get_roi_dset(alabel)
       if not aset:
          print("** GCMC: no label '%s' dset A for option %s" \
                % (alabel, oname))
          return ''

       bset = proc.get_roi_dset(blabel)
       if not bset:
          print("** GCMC: no label '%s' dset B for option %s" \
                % (blabel, oname))
          return ''

       if not proc.have_roi_label(ilabel):
          print('** no %s label %s for option %s' % (ostr, ilabel, oname))
          return ''

       # be sure that we have views
       if aset.view == '': aset.view = proc.view
       if bset.view == '': bset.view = proc.view

       iset = gen_afni_name('mask_%s_%s'%(oper,ilabel), view=proc.view)
       if proc.add_roi_dict_key(ilabel, iset, overwrite=1): return ''

       cmd += '# create %s mask %s from masks %s and %s\n'      \
              "3dmask_tool -input %s %s \\\n"                   \
              "       -%s -prefix %s\n\n"                       \
              % (ostr, ilabel, alabel, blabel,
                 aset.shortinput(), bset.shortinput(), oper, iset.out_prefix())

       if proc.verb:
          print('++ making %s mask %s from %s and %s' \
                % (ostr, ilabel,alabel,blabel))
       if proc.verb > 2:
          iset.show(mesg='iset')
          aset.show(mesg='aset')
          bset.show(mesg='bset')

    return cmd


# if possible: make a group anatomical mask (resampled to EPI)
#    - only if tlrc block and -volreg_tlrc_warp
#    - apply from -tlrc_base
# return None on failure
def group_mask_command(proc, block):
    if not proc.warp_epi & WARP_EPI_TLRC_WARP or not proc.tlrc_base:
        if proc.verb>2: print("-- no group mask, warp_epi = %d" % proc.warp_epi)
        return ''

    #--- first things first, see if we can locate the tlrc base
    #    if the user didn't already tell us where it is

    print("-- masking: group anat = '%s', exists = %d"   \
          % (proc.tlrc_base.pv(), proc.tlrc_base.exist()))

    if not proc.tlrc_base.exist():
        print("** missing tlrc base: %s" % proc.tlrc_base.ppv())
        print("   (cannot create group mask)")
        return ''

    # if more than 1 volume in template dset, use sub-brick selection
    rv, nt, tr = UTIL.get_dset_reps_tr(proc.tlrc_base.input())
    if rv: nt = 1
    if nt > 1: volstr = "'[0]'"
    else:      volstr = ""

    #--- tlrc base exists, now resample and make a mask of it
    proc.mask_group = proc.mask_epi.new('mask_group')
    cmd = "# ---- create group anatomy mask, %s ----\n"  \
          "#      (resampled from tlrc base anat, %s)\n" \
          % (proc.mask_group.pv(), proc.tlrc_base.pv())

    tanat = proc.mask_group.new('rm.resam.group') # temp resampled group dset
    cmd = cmd + "3dresample -master %s -prefix ./%s \\\n" \
                "           -input %s%s\n\n"              \
                % (proc.mask.pv(), tanat.prefix,
                   proc.tlrc_base.input(), volstr)

    # convert to a binary mask via 3dmask_tool, to fill in a bit
    cmd = cmd + "# convert to binary group mask; fill gaps and holes\n"     \
                "3dmask_tool -dilate_input 5 -5 -fill_holes -input %s \\\n" \
                "            -prefix %s\n\n"                                \
                % (tanat.pv(), proc.mask_group.prefix)


    if proc.mask_anat:
       rcmd = "# note Dice coefficient of anat and template masks\n" \
              "3ddot -dodice %s %s \\\n"                             \
              "      |& tee out.mask_at_dice.txt\n\n"                \
              % (proc.mask_anat.pv(), proc.mask_group.pv())
       cmd = cmd + rcmd

    proc.mask_group.created = 1  # so this mask 'exists' now

    return cmd

# if possible make a subject anatomical mask (resampled to EPI)
#    * if -volreg_tlrc_adwarp, there is no ss anat
#    - if -volreg_tlrc_warp, apply from tlrc anat
#    - if a2e, apply from anat_al
#    - if e2a, apply from anat_ss (intermediate anat)
# return None on failure
def anat_mask_command(proc, block):
    # if the EPI is being warped (based on the anat), or the anat
    # has no skull to begin with, we are good to go
    if not proc.warp_epi and proc.anat_has_skull: return ''

    # adwarp: we cannot rely on skull-stripped anat, so just return
    if proc.warp_epi & WARP_EPI_TLRC_ADWARP: return ''

    proc.mask_anat = proc.mask_epi.new('mask_anat.$subj')
    cmd = "# ---- create subject anatomy mask, %s ----\n" % proc.mask_anat.pv()

    if proc.verb > 2:
        print('-- mask for anat based on warp_epi == %d\n'                \
              '   (1==tlrc_adwarp, 2==tlrc_warp, 4=anat2epi, 8=epi2anat)' \
              % proc.warp_epi)

    # set anat, comment string text and temporary anat
    if proc.warp_epi & WARP_EPI_TLRC_WARP:
        anat = proc.tlrcanat
        ss = 'tlrc'
    elif proc.warp_epi & (WARP_EPI_ALIGN_A2E | WARP_EPI_ALIGN_E2A):
        anat = proc.anat
        ss = 'aligned'
    # no longer invert e2a matrix to get ss anat, since the current
    # anat will already be stripped
    else: # no warp, so anat has no skull; use orig anat
        anat = proc.anat
        ss = 'orig'
    cmd = cmd + "#      (resampled from %s anat)\n" % ss
    tanat = anat.new('rm.resam.anat')   # temporary resampled anat dset

    #if proc.warp_epi == WARP_EPI_ALIGN_E2A:
    #    cmd = cmd + '\n'                                                     \
    #          "# invert a2e matrix, and warp/resample skull-stripped anat\n" \
    #          "cat_matvec -ONELINE %s -I > mat.a2e.inv.aff12.1D\n"           \
    #          "3dAllineate -input %s -master %s \\\n"                        \
    #          "            -1Dmatrix_apply mat.a2e.inv.aff12.1D \\\n"        \
    #          "            -prefix %s\n\n"                                   \
    #           % (proc.a2e_mat, anat.pv(), proc.mask_epi.pv(), tanat.prefix)
    #else:

    # resample masked anat to epi grid, output is temp anat
    cmd = cmd + "3dresample -master %s -input %s \\\n"                  \
                "           -prefix %s\n\n"                             \
                % (proc.mask_epi.pv(), anat.pv(), tanat.prefix)

    # and convert to binary mask via 3dmask_tool, to fill in a bit
    cmd = cmd + "# convert to binary anat mask; fill gaps and holes\n"      \
                "3dmask_tool -dilate_input 5 -5 -fill_holes -input %s \\\n" \
                "            -prefix %s\n\n"                                \
                % (tanat.pv(), proc.mask_anat.prefix)

    # computed tightened EPI mask, by intersecting with anat mask
    if proc.mask_epi and proc.mask_anat:
       proc.mask_epi_anat = proc.mask_epi.new('mask_epi_anat.$subj')
       if block.opts.have_yes_opt('-mask_epi_anat'):
           if proc.verb: 
              print("++ mask: using epi_anat mask in place of EPI one")
           proc.mask = proc.mask_epi_anat
       rcmd = "# compute tighter EPI mask by intersecting with anat mask\n" \
              "3dmask_tool -input %s %s \\\n"                               \
              "            -inter -prefix %s\n\n"                           \
              % (proc.mask_epi.shortinput(), proc.mask_anat.shortinput(),
                 proc.mask_epi_anat.out_prefix())

       # if 'brain' ROI is mask_epi, replace it with mask_epi_anat  2024.0222
       label = 'brain'
       mset = proc.get_roi_dset(label)
       if mset == proc.mask_epi:
          if proc.verb > 1:
             print("++ replacing '%s' ROI with mask_epi_anat" % label)
          if proc.add_roi_dict_key(label, proc.mask_epi_anat, overwrite=1):
             return 1

       proc.mask_epi_anat.created = 1  # so this mask 'exists' now
       cmd += rcmd

    if block.opts.have_yes_opt('-mask_test_overlap', default=1):
        if proc.mask_epi and proc.mask_anat:
            rcmd = "# compute overlaps between anat and EPI masks\n"  \
                   "3dABoverlap -no_automask %s %s \\\n"              \
                   "            |& tee out.mask_ae_overlap.txt\n\n"   \
                   % (proc.mask_epi.pv(), proc.mask_anat.pv())
            cmd = cmd + rcmd

            rcmd = "# note Dice coefficient of masks, as well\n"      \
                   "3ddot -dodice %s %s \\\n"                         \
                   "      |& tee out.mask_ae_dice.txt\n\n"            \
                   % (proc.mask_epi.pv(), proc.mask_anat.pv())
            cmd = cmd + rcmd

    proc.mask_anat.created = 1  # so this mask 'exists' now

    return cmd

def db_mod_scale(block, proc, user_opts):     # no options at this time
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-scale_max_val', 1, [200], setpar=1)

    # check for user updates
    uopt = user_opts.find_opt('-scale_max_val')
    bopt = block.opts.find_opt('-scale_max_val')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print("** -scale_max_val requires an int param (have '%s')" % \
                  uopt.parlist[0])
            block.valid = 0
            return 1

    # if the user does not want a max, use 0
    if user_opts.find_opt('-scale_no_max') and bopt:
        bopt.parlist[0] = 0

    block.valid = 1

def db_cmd_scale(proc, block):
    """scale based on method:
          voxelwise     : scale every voxel to a mean of 100
          global_mean   : scale every volume to a mean of 10000
          grand_mean    : entire data set to mean of 10000
    """

    # note what scaling type is bing used
    stype = 'voxelwise' # rcr - reset to voxelwise
    val, rv = block.opts.get_string_opt('-scale_type')
    if val and not rv:
       stype = val

    cmd = ''
    # check for max scale value
    opt = block.opts.find_opt('-scale_max_val')
    max = opt.parlist[0]
    if max > 100: valstr = 'min(%d, a/b*100)*step(a)*step(b)' % max
    else:         valstr = 'a/b*100*step(a)'

    # options for surface analysis
    if proc.surf_anat:
        # do not allow scale type (yet?)
        if stype != 'voxelwise':
           print('** only voxelwise is allowed for -scale_type of surface data')
           # note: easy to add
           return

        # string for foreach hemi loop
        feh_str = 'foreach %s ( %s )\n' \
                  % (proc.surf_spec_var_iter, ' '.join(proc.surf_hemilist))
        feh_end = 'end\n'

        suff = '.niml.dset'      # output suffix
        vsuff = suff             # where view might go
        istr = ' '*4             # extra indent, for foreach hemi loop
        bstr = '\\\n%s           ' % istr # extr line wrap since long names
        mean_pre = "rm.$hemi.mean" # prefix for mean datasets
    else:
        feh_str = ''
        feh_end = ''
        suff = ''
        vsuff = proc.view
        istr = ''
        bstr = ''
        mean_pre = "rm.mean"

    if proc.use_me:
        estr = '.e%s' % proc.echo_var
    else:
        estr = ''

    # choose a mask: either passed, extents, or none
    mset = None
    if   proc.surf_anat:              mset = None       # no mask on surface
    elif proc.mask and proc.regmask:  mset = proc.mask
    elif proc.mask_extents != None:   mset = proc.mask_extents

    # if have a mask, apply it, else use any extents mask
    if mset != None:
        mask_str = '%s           -c %s%s \\\n' % (istr, mset.prefix, vsuff)
        expr     = 'c * %s' % valstr
    else:
        mask_str = ''
        expr     = valstr

    if max > 100: maxstr = '# (subject to a range of [0,%d])\n' % max
    else        : maxstr = ''

    prev = proc.prev_prefix_form_run(block, view=1)
    prefix = proc.prefix_form_run(block)
    cmd += "# %s\n" % block_header('scale')

    cmd += "# scale each voxel time series to have a mean of 100\n"     \
           "# (be sure no negatives creep in)\n"                        \
           "%s" % maxstr

    # if stype is grand_mean, compute the mean now, across runs
    # (rcr - actually write this)
    if stype == 'grand_mean':
       if not proc.mask:
          print('** no EPI mask for -scale_type %s' % stype)
          return
       cstr  = '# get %s: the mean across the mask and time\n' % stype
       cstr += '%s3dmaskave -q -mask %s "%s" \\\n' \
               "%s    | 3dTstat -mean -prefix scale_%s.1D 1D:stdin\\'\n" \
               % (istr, proc.mask.shortinput(), proc.prev_dset_form_wild(block),
                  istr, stype)
       cstr += '%sset gmean = `1dcat scale_%s.1D`\n\n' % (istr, stype)
       cmd += cstr

    cmd += feh_str      # if surf, foreach hemi

    cmd += "%sforeach run ( $runs )\n" % istr

    # ME:
    if proc.use_me:
       iprev = istr
       istr += ' '*4
       cmd += '%sforeach %s ( $echo_list )\n' % (istr, proc.echo_var[1:])

    # per run loop
    cmd += "%s    3dTstat -prefix %s_r$run%s%s %s\n"                    \
           "%s    3dcalc -a %s %s-b %s_r$run%s%s \\\n"                  \
           "%s"                                                         \
           % (istr, mean_pre, estr, suff, prev,
              istr, prev, bstr, mean_pre, estr, vsuff, mask_str)

    cmd += "%s           -expr '%s' \\\n"                               \
           "%s           -prefix %s\n"                                  \
           % (istr, expr, istr, prefix)

    # end per run loop, and possibly per hemisphere one
    cmd += "%send\n" % istr

    # ME:
    if proc.use_me:
       istr = iprev
       cmd += "%send\n" % istr

    cmd += feh_end + '\n'

    proc.have_rm = 1            # rm.* files exist

    return cmd

def all_erode_labels_used(proc, block):
    """all erode labels should apply to anat followers"""
    elist, rv = proc.user_opts.get_string_list('-anat_follower_erode')
    if elist == None: return 1

    ok = 1
    for label in elist:
       af = proc.get_anat_follower(label)
       if af == None:
          print("** ERROR: erode label '%s' not in followers list" % label)
          ok = 0
          continue

       if not af.erode:
          print("** ERROR: follower label '%s' not properly eroded" % label)
          ok = 0

    return ok

def add_ROI_PC_followers(proc, block):
    """add any appropriate datasets anat followers

       We cannot check for overlaps between these labels and
       those set in mask_segment_anat until the cmd_regress(),
       but as ortvecs, do we really care?
    """

    newlabs = []
    oname = '-regress_ROI_PC'
    ROIlist = block.opts.find_all_opts(oname)
    # option form is LABEL NUM_PCs dataset (check existence?)
    for roiopt in ROIlist:
        label = roiopt.parlist[0]
        numpc = roiopt.parlist[1]
        # dname = roiopt.parlist[2]

        # make sure labels are unique and not datasets
        if label in newlabs:
           print("** error: %s label '%s' already used" % (oname, label))
           return 1
        badname = gen_afni_name(label)
        if badname.exist():
           print('** ERROR: %s label exists as a dataset, %s' % (oname, label))
           print("   format: %s LABEL NUM_PCs DATASET" % oname)
           return 1
        newlabs.append(label)

        # numpc must be a positive integer
        try: npc = int(numpc)
        except:
           print('** error: %s illegal NUM_PCs = %s' % (oname, numpc))
           print("   format: %s LABEL NUM_PCs" % oname)
           return 1
        if npc <= 0:
           print('** error: %s illegal NUM_PCs = %s' % (oname, numpc))
           print("   format: %s LABEL NUM_PCs" % oname)
           return 1

        ff = proc.get_anat_follower(label)
        if not ff and label not in proc.roi_dict:
           print("** ERROR: no anat follower or ROI dict label '%s'" % label)
           return 1

    # if we have something...
    if len(proc.afollowers) > 0:
       if proc.verb > 1:
          print('-- have %d ROI anat followers' % len(proc.afollowers))

       # rcr - allow for align or nothing
       if proc.user_opts.find_opt('-volreg_tlrc_adwarp'):
          print('** rcr - allow regress_ROI_* warp via adwarp?')
          return 1

    return 0

def db_mod_regress(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-regress_basis', 1, ['GAM'], setpar=1)
        block.opts.add_opt('-regress_censor_prev', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_compute_tsnr', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_compute_gcor', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_cormat_warnings', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_fout', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_stim_files', -1, [])
        block.opts.add_opt('-regress_stim_labels', -1, [])
        block.opts.add_opt('-regress_RONI', -1, [])
        block.opts.add_opt('-regress_stim_times', -1, [])
        block.opts.add_opt('-regress_stim_times_offset', 1, [0], setpar=1)

        block.opts.add_opt('-regress_extra_stim_files', -1, [])
        block.opts.add_opt('-regress_extra_stim_labels', -1, [])
        block.opts.add_opt('-regress_extra_ortvec', -1, [])
        block.opts.add_opt('-regress_extra_ortvec_labels', -1, [])

        block.opts.add_opt('-regress_opts_3dD', -1, [])
        block.opts.add_opt('-regress_opts_reml', -1, [])
        block.opts.add_opt('-regress_make_ideal_sum', 1, ['sum_ideal.1D'],
                                                       setpar=1)
        block.opts.add_opt('-regress_errts_prefix', 1, [])
        block.opts.add_opt('-regress_fitts_prefix', 1, ['fitts.$subj'],
                                                       setpar=1)
        block.opts.add_opt('-regress_make_cbucket', 1, ['no'], setpar=1)

    errs = 0  # allow errors to accumulate

    apply_uopt_to_block('-regress_motion_file', user_opts, block)
    apply_uopt_to_block('-regress_opts_fwhmx', user_opts, block)
    apply_uopt_to_block('-regress_show_df_info', user_opts, block)

    apply_uopt_to_block('-regress_anaticor', user_opts, block)
    apply_uopt_to_block('-regress_anaticor_radius', user_opts, block)
    apply_uopt_to_block('-regress_anaticor_fast', user_opts, block)
    apply_uopt_to_block('-regress_anaticor_fwhm', user_opts, block)
    apply_uopt_to_block('-regress_anaticor_full_gaussian', user_opts, block)
    apply_uopt_to_block('-regress_anaticor_term_frac', user_opts, block)
    apply_uopt_to_block('-regress_anaticor_label', user_opts, block)
    apply_uopt_to_block('-regress_make_corr_vols', user_opts, block)
    apply_uopt_to_block('-regress_make_corr_AIC', user_opts, block)


    # check for user updates
    uopt = user_opts.find_opt('-regress_basis')
    bopt = block.opts.find_opt('-regress_basis')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]
        # may have many basis functions, if any have unknown response
        #    curve, set iresp prefix for them
        # add iresp output for any unknown response curves
        if not UTIL.basis_has_known_response(bopt.parlist[0], warn=1):
            if not user_opts.find_opt('-regress_iresp_prefix'):
                block.opts.add_opt('-regress_iresp_prefix',1,['iresp'],setpar=1)

    # handle processing one basis functions per class
    uopt = user_opts.find_opt('-regress_basis_multi')
    if uopt:
        # either add or modify block version of option
        bopt = block.opts.find_opt('-regress_basis_multi')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_basis_multi', -1, uopt.parlist,
                                 setpar=1)
        bopt = block.opts.find_opt('-regress_basis_multi')

        # if any basis has unknown response curve, set iresp prefix
        for basis in bopt.parlist:
           if not UTIL.basis_has_known_response(basis, warn=1):
              if not user_opts.find_opt('-regress_iresp_prefix'):
                block.opts.add_opt('-regress_iresp_prefix',1,['iresp'],setpar=1)
              break

    # set basis_normall only via user option
    uopt = user_opts.find_opt('-regress_basis_normall')
    if uopt:
        norm = 1.0
        try: norm = float(uopt.parlist[0])
        except:
            print("** -regress_basis_normall requires float param (have '%s')" \
                  % uopt.parlist[0])
            errs += 1
        bopt = block.opts.find_opt('-regress_basis_normall')
        if bopt: bopt.parlist[0] = norm
        else: block.opts.add_opt('-regress_basis_normall', 1, [norm], setpar=1)

    apply_uopt_to_block('-regress_polort', user_opts, block)

    # files can have many stim classes in one file
    uopt = user_opts.find_opt('-regress_stim_files')
    bopt = block.opts.find_opt('-regress_stim_files')
    if uopt and bopt:
        if len(uopt.parlist) <= 0:
            print("** no files for -regress_stim_files?")
            errs += 1
        bopt.parlist = uopt.parlist
        proc.stims_orig = uopt.parlist   # store for initial copy

    apply_uopt_to_block('-regress_stim_labels', user_opts, block)
    apply_uopt_to_block('-regress_stim_types', user_opts, block)

    uopt = user_opts.find_opt('-regress_RONI')
    bopt = block.opts.find_opt('-regress_RONI')
    if uopt and bopt:  # check length later, when we know num stim types
        try:
            bopt.parlist = [int(par) for par in uopt.parlist]
            if proc.verb > 1:
                print("++ have RONI indices: %s" % bopt.parlist)
        except:
            print("** -regress_RONI requires integral parameters, have: %s" % \
                  uopt.parlist)
            errs += 1

    # --------------------------------------------------
    # -regress_ROI* options
    apply_uopt_list_to_block('-regress_ROI', user_opts, block)    # 04 Sep 2012
    apply_uopt_list_to_block('-regress_ROI_PC', user_opts, block) # 01 Apr 2015
    apply_uopt_to_block('-regress_ROI_per_run', user_opts, block)  # 09/21/2016
    apply_uopt_to_block('-regress_ROI_PC_per_run', user_opts, block)

    # add any appropriate datasets anat followers   01 Apr 2015
    if add_ROI_PC_followers(proc, block): errs += 1

    # times is one file per class
    uopt = user_opts.find_opt('-regress_stim_times')
    bopt = block.opts.find_opt('-regress_stim_times')
    if uopt and bopt:
        if len(uopt.parlist) <= 0:
            print("** no files for -regress_stim_times?")
            errs += 1
        # verify this doesn't go with no_stim_times
        if user_opts.find_opt('-regress_use_stim_files'):
            print('** have both -regress_use_stim_files and -regress_stim_times')
            errs += 1
        if user_opts.find_opt('-regress_no_stim_times'):
            print('** have both -regress_no_stim_times and -regress_stim_times')
            errs += 1
        bopt.parlist = uopt.parlist
        proc.stims_orig = uopt.parlist   # store for initial copy

    uopt = user_opts.find_opt('-regress_stim_times_offset')
    bopt = block.opts.find_opt('-regress_stim_times_offset')
    if uopt and bopt:
        try: bopt.parlist[0] = float(uopt.parlist[0])
        except:
            print("** stim times offset must be float, have '%s'" \
                  % uopt.parlist[0])

    uopt = user_opts.find_opt('-regress_make_ideal_sum')
    bopt = block.opts.find_opt('-regress_make_ideal_sum')
    if uopt and bopt:
        bopt.parlist = uopt.parlist

    uopt = user_opts.find_opt('-regress_no_ideal_sum')
    if uopt:
        bopt = block.opts.find_opt('-regress_no_ideal_sum')
        if not bopt: block.opts.add_opt('-regress_no_ideal_sum', 0,[],setpar=1)

    uopt = user_opts.find_opt('-regress_opts_3dD')      # 3dDeconvolve
    bopt = block.opts.find_opt('-regress_opts_3dD')
    if uopt and bopt: bopt.parlist = uopt.parlist

    uopt = user_opts.find_opt('-regress_CS_NN')         # 3dClustSim NN
    if uopt:
        print('** option -regress_CS_NN is no longer valid, as -NN is no\n' \
              '   longer supported by 3dClustSim\n')
        # bopt = block.opts.find_opt('-regress_CS_NN')
        # if bopt: bopt.parlist = uopt.parlist
        # else: block.opts.add_opt('-regress_CS_NN', 1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-regress_opts_CS')       # 3dClustSim
    if uopt:
        bopt = block.opts.find_opt('-regress_opts_CS')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_opts_CS', -1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-regress_opts_reml')      # 3dREMLfit
    bopt = block.opts.find_opt('-regress_opts_reml')
    if uopt and bopt: bopt.parlist = uopt.parlist

    # --------------------------------------------------
    # check for extra stim_files and labels
    uopt = user_opts.find_opt('-regress_extra_stim_files')
    bopt = block.opts.find_opt('-regress_extra_stim_files')
    if uopt and bopt:  # only check length against labels
        bopt.parlist = uopt.parlist
        # convert paths to the local stimulus directory
        proc.extra_stims = []
        proc.extra_stims_orig = bopt.parlist
        for file in bopt.parlist:
            proc.extra_stims.append('stimuli/%s' % os.path.basename(file))

    uopt = user_opts.find_opt('-regress_extra_stim_labels')
    bopt = block.opts.find_opt('-regress_extra_stim_labels')
    if uopt and bopt:
        bopt.parlist = uopt.parlist
        proc.extra_labs = uopt.parlist
        nxlabs = len(proc.extra_labs)
        nxstim = len(proc.extra_stims)
        if nxstim == 0:
            print("** have -regress_extra_stim_labels without" + \
                  " -regress_extra_stim_files")
            errs += 1
        elif nxstim != nxlabs:
            print("** have %d extra stims but %d extra labels" % \
                  (nxstim, nxlabs))
            errs += 1

    # check for extra ortvecs
    oname = '-regress_extra_ortvec'
    uopt = user_opts.find_opt(oname)
    bopt = block.opts.find_opt(oname)
    if uopt and bopt:  # only check length against labels
        bopt.parlist = uopt.parlist
        # convert paths to the local stimulus directory
        proc.extra_ortvec = []
        proc.extra_ortvec_orig = bopt.parlist
        for fname in bopt.parlist:
            proc.extra_ortvec.append('stimuli/%s' % os.path.basename(fname))

    oname = '-regress_extra_ortvec_labels'
    uopt = user_opts.find_opt(oname)
    bopt = block.opts.find_opt(oname)
    if uopt and bopt:
        bopt.parlist = uopt.parlist
        proc.extra_ortvec_labs = uopt.parlist
        nxlabs = len(proc.extra_ortvec_labs)
        nxorts = len(proc.extra_ortvec)
        if nxorts == 0:
            print("** have -regress_extra_ortvec_labels without" + \
                  " -regress_extra_ortvec")
            errs += 1
        elif nxorts != nxlabs:
            print("** have %d extra ortvec but %d extra ort labels" % \
                  (nxorts, nxlabs))
            errs += 1
    elif bopt and len(proc.extra_ortvec) > 0:
        # no ortvec label option, so fashion some
        print("-- auto-generating labels for extra ortvec files")
        proc.extra_ortvec_labs = \
              ['xort%02d'%ind for ind in range(len(proc.extra_ortvec))]

    # --------------------------------------------------
    # if we are here, then we should have stimulus files
    if len(proc.stims_orig) > 0:
        # create local names for stim files
        pre = ''
        oname = '-regress_stim_times_offset'
        val, err = block.opts.get_type_opt(float, oname)
        if err:
           print('** bad offset to %s' % oname)
           errs += 1
        # if we have an offset
        elif val:
           if proc.have_all_stim_times():       # good
              pre = 'offset_'
           elif proc.have_some_stim_times():    # bad
              # offset cannot go with either timing or file->times conversion
              print('** option %s applies to either all stim times\n' \
                    '   or all stim files, but not to a mix\n' % oname)
              errs += 1

        proc.stims = []
        for fname in proc.stims_orig:
            proc.stims.append('stimuli/%s%s' % \
                (pre, os.path.basename(fname)))

    # note whether this seems to be task at all (no orts)
    if len(proc.stims) + len(proc.extra_stims) > 0:
       proc.have_task_regs = 1
    else:
       proc.have_task_regs = 0

    apply_uopt_to_block('-regress_mot_as_ort', user_opts, block)

    # check for per-run regression of motion parameters
    uopt = user_opts.find_opt('-regress_motion_per_run')
    if uopt and not block.opts.find_opt('-regress_motion_per_run'):
        block.opts.add_opt('-regress_motion_per_run', 0,  [], setpar=1)

    # check for censoring of large motion
    uopt = user_opts.find_opt('-regress_censor_motion')
    bopt = block.opts.find_opt('-regress_censor_motion')
    if uopt:
      try: limit = float(uopt.parlist[0])
      except:
        print("** -regress_censor_motion limit must be float, have '%s'" \
              % uopt.parlist[0])
        errs += 1
      if limit < 0.0:
        print('** -regress_censor_motion limit must be positive, have %g'%limit)
        errs += 1
      if bopt: bopt.parlist[0] = limit
      else: block.opts.add_opt('-regress_censor_motion', 1, [limit], setpar=1)

    apply_uopt_to_block('-regress_skip_censor', user_opts, block)

    # do we also censor first N TRs per run?
    uopt = user_opts.find_opt('-regress_censor_first_trs')
    bopt = block.opts.find_opt('-regress_censor_first_trs')
    if uopt:
        if not block.opts.find_opt('-regress_censor_motion'):
            print('** -regress_censor_first_trs requires -regress_censor_motion')
            errs += 1
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_censor_first_trs', 1,
                                 uopt.parlist, setpar=1)

    # do we also censor the previous TR?
    uopt = user_opts.find_opt('-regress_censor_prev')
    bopt = block.opts.find_opt('-regress_censor_prev')
    if uopt:
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_censor_prev', 1,
                                 uopt.parlist, setpar=1)

    # maybe we do not want cormat warnings
    uopt = user_opts.find_opt('-regress_cormat_warnings')
    if uopt:
        bopt = block.opts.find_opt('-regress_cormat_warnings')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_cormat_warnings', 1,
                                 uopt.parlist, setpar=1)

    # maybe we do not want the -fout option
    uopt = user_opts.find_opt('-regress_fout')
    bopt = block.opts.find_opt('-regress_fout')
    if uopt:
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_fout', 1,
                                 uopt.parlist, setpar=1)

    # check for EPI blur estimate
    uopt = user_opts.find_opt('-regress_est_blur_epits')
    bopt = block.opts.find_opt('-regress_est_blur_epits')
    if uopt and not bopt:
        block.opts.add_opt('-regress_est_blur_epits', 0, [])

    # check for errts blur estimate
    uopt = user_opts.find_opt('-regress_est_blur_errts')
    bopt = block.opts.find_opt('-regress_est_blur_errts')
    if uopt and not bopt:
        block.opts.add_opt('-regress_est_blur_errts', 0, [])

    apply_uopt_to_block('-regress_est_blur_detrend', user_opts, block)

    # check for errts prefix
    uopt = user_opts.find_opt('-regress_errts_prefix')
    bopt = block.opts.find_opt('-regress_errts_prefix')
    if uopt and bopt:
        bopt.parlist = [uopt.parlist[0] + '.${subj}']    # add $subj to prefix

    # check for fitts prefix
    uopt = user_opts.find_opt('-regress_fitts_prefix')
    bopt = block.opts.find_opt('-regress_fitts_prefix')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0] + '.$subj'   # add $subj to prefix
    elif uopt and not bopt: # maybe deleted previously (not currently possible)
        block.opts.add_opt('-regress_fitts_prefix', 1, uopt.parlist,setpar=1)

    # maybe the user wants to delete it
    uopt = user_opts.find_opt('-regress_no_fitts')
    bopt = block.opts.find_opt('-regress_fitts_prefix')
    if uopt and bopt: block.opts.del_opt('-regress_fitts_prefix')

    # check for iresp prefix
    uopt = user_opts.find_opt('-regress_iresp_prefix')
    bopt = block.opts.find_opt('-regress_iresp_prefix')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]
    elif uopt and not bopt: # maybe it was deleted previously
        block.opts.add_opt('-regress_iresp_prefix', 1, uopt.parlist,setpar=1)

    # maybe the user does not want default ideals
    uopt = user_opts.find_opt('-regress_no_ideals')
    bopt = block.opts.find_opt('-regress_no_ideals')
    if uopt and not bopt: block.opts.add_opt('-regress_no_ideals',0,[])

    # maybe the user does not want iresp datasets
    uopt = user_opts.find_opt('-regress_no_iresp')
    bopt = block.opts.find_opt('-regress_iresp_prefix')
    if uopt and bopt: block.opts.del_opt('-regress_iresp_prefix')

    # maybe the user does not want to apply the mask to regression
    # (applies to other blocks, so note at the 'proc' level)
    uopt = user_opts.find_opt('-regress_no_mask')
    if uopt:
        print('** -regress_no_mask is now the default')
        proc.regmask = 0

    # maybe the user really does want to apply the mask to regression
    # note: no_mask is now the default    24 Mar 2009
    uopt = user_opts.find_opt('-regress_apply_mask')
    if uopt:
        print("** regress_apply_mask has been deprecated")
        print("   (consider '-mask_apply epi')")
        proc.regmask = 1

    # check for global or local stim_times
    uopt = user_opts.find_opt('-regress_global_times')
    u2   = user_opts.find_opt('-regress_local_times')
    bopt = block.opts.find_opt('-regress_global_times')
    b2   = block.opts.find_opt('-regress_local_times')
    if uopt or u2:
      if uopt and u2:
        print('** error: given -regress_global_times AND -regress_local_times')
        errs += 1
      elif uopt:
        if b2: block.opts.del_opt('-regress_local_times')
        block.opts.add_opt('-regress_global_times',0,[])
      else: # u2
        if b2: block.opts.del_opt('-regress_global_times')
        block.opts.add_opt('-regress_local_times',0,[])

    # maybe the user does not want to regress the motion parameters
    # apply uopt to bopt
    uopt = user_opts.find_opt('-regress_no_motion')
    bopt = block.opts.find_opt('-regress_no_motion')
    if uopt and not bopt: block.opts.add_opt('-regress_no_motion',0,[])
    elif not uopt and bopt: block.opts.del_opt('-regress_no_motion',0,[])

    # maybe the user wants to specify a motion file
    uopt = user_opts.find_opt('-regress_motion_file')
    if uopt:
        # make sure we have labels
        try:
            dname, fname = os.path.split(uopt.parlist[0])
            proc.mot_file = fname
        except:
            print('** failed to parse directory/file from -regress_motion_file')
            print('   (file is %s)' % uopt.parlist[0])
            errs += 1
        proc.mot_extern = uopt.parlist[0]
        proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']
        # -volreg_regress_per_run should be okay  (option removed: 20 May 2011)
        # (must still assume TR correspondence)

    # maybe the user wants to specify types of motion parameters to use
    uopt = user_opts.find_opt('-regress_apply_mot_types')
    if uopt:
        if user_opts.find_opt('-regress_no_motion_demean') or \
           user_opts.find_opt('-regress_no_motion_deriv'):
            print('** cannot use -regress_apply_mot_types with either of\n' \
                  '   -regress_no_motion_demean or -regress_no_motion_deriv')
            errs += 1
        bopt = block.opts.find_opt('-regress_apply_mot_types')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_apply_mot_types', -1, uopt.parlist,
                                 setpar=1)

    # no demean or deriv?
    uopt = user_opts.find_opt('-regress_no_motion_demean')
    if uopt:
        if not block.opts.find_opt('-regress_no_motion_demean'):
           block.opts.add_opt('-regress_no_motion_demean', 0, [])
    uopt = user_opts.find_opt('-regress_no_motion_deriv')
    if uopt:
        if not block.opts.find_opt('-regress_no_motion_deriv'):
           block.opts.add_opt('-regress_no_motion_deriv', 0, [])

    # maybe the user does not want to convert stim_files to stim_times
    uopt = user_opts.find_opt('-regress_use_stim_files')
    if not uopt: uopt = user_opts.find_opt('-regress_no_stim_times')
    bopt = block.opts.find_opt('-regress_no_stim_times')
    if uopt and not bopt:
        if proc.verb > 0: print('-- will use -stim_files in 3dDeconvolve')
        block.opts.add_opt('-regress_no_stim_times', 0, [])

    # if the user wants to compute the fitts, just pass the option along
    uopt = user_opts.find_opt('-regress_compute_fitts')
    bopt = block.opts.find_opt('-regress_compute_fitts')
    if uopt and not bopt: block.opts.add_opt('-regress_compute_fitts',0,[])

    # just pass along regress_3dD_stop
    uopt = user_opts.find_opt('-regress_3dD_stop')
    bopt = block.opts.find_opt('-regress_3dD_stop')
    if uopt and not bopt: block.opts.add_opt('-regress_3dD_stop',0,[])

    # just pass along regress_reml_exec
    uopt = user_opts.find_opt('-regress_reml_exec')
    bopt = block.opts.find_opt('-regress_reml_exec')
    if uopt and not bopt: block.opts.add_opt('-regress_reml_exec',0,[])

    # check for whether to run 3dClustSim
    uopt = user_opts.find_opt('-regress_run_clustsim')
    if uopt:
        bopt = block.opts.find_opt('-regress_run_clustsim')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_run_clustsim', 1, uopt.parlist,
                                 setpar=1)

        # if not 'no', require blur estimation
        if not OL.opt_is_no(uopt)                             and \
           not block.opts.find_opt('-regress_est_blur_errts') and \
           not block.opts.find_opt('-regress_est_blur_epits'):
            print('** blur estimation is required for ClustSim\n' \
                  '   (consider -regress_est_blur_errts (or _epits))')
            errs += 1

    # check on tsnr and gcor
    apply_uopt_to_block('-regress_compute_tsnr', user_opts, block)
    apply_uopt_list_to_block('-regress_compute_tsnr_stats',  user_opts, block)

    apply_uopt_to_block('-regress_compute_gcor', user_opts, block)
    apply_uopt_to_block('-regress_mask_tsnr', user_opts, block)

    # possibly update cbucket option
    apply_uopt_to_block('-regress_make_cbucket', user_opts, block)

    # possibly update cbucket option
    apply_uopt_to_block('-regress_apply_ricor', user_opts, block)

    # maybe do bandpass filtering in the regression
    apply_uopt_to_block('-regress_bandpass', user_opts, block)

    # maybe do bandpass filtering in the regression
    apply_uopt_to_block('-regress_RSFC', user_opts, block)

    # maybe replace 3dDeconvolve with 3dTproject
    apply_uopt_to_block('-regress_use_tproject', user_opts, block)

    # prepare to return
    if errs > 0:
        block.valid = 0
        return 1

    block.valid = 1

# possibly create stim_times files
#
# without stim_times, use stim_files to generate stim_times
# without stim_labels, use stim_times to create labels
def db_cmd_regress(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-regress_basis')
    basis = opt.parlist  # as a list, to incorporate -regress_basis_multi

    opt = block.opts.find_opt('-regress_basis_multi')
    if opt: basis = opt.parlist # override any -regress_basis

    opt = block.opts.find_opt('-regress_basis_normall')
    if opt: normall = '    -basis_normall %s' % opt.parlist[0]
    else:   normall = ''

    # if ricor, check whether the user wants to apply those regressors here
    if proc.ricor_nreg > 0:
       opt = block.opts.find_opt('-regress_apply_ricor')
       if OL.opt_is_yes(opt): proc.ricor_apply = 'yes'

    # ME: multi-echo is not allowed in the regression block
    if proc.use_me:
       print("** regression is not allowed on ME data, combine first")
       return

    # maybe we want a special prefix (do not test stims in this case)
    if proc.script_3dD:
        proc.test_stims = 0
        tmp_prefix = "${prefix_3dd}"
    else:
        tmp_prefix = ''

    # options for surface analysis
    if proc.surf_anat:
        # string for foreach hemi loop
        feh_str = 'foreach %s ( %s )\n' \
                  % (proc.surf_spec_var_iter, ' '.join(proc.surf_hemilist))
        feh_end = 'end\n'

        # suffix needs the hemisphere iteration variable
        suff = '.%s.niml.dset' % proc.surf_svi_ref
        istr = ' '*4             # extra indent, for foreach hemi loop
        vstr = ''                # was suff, but that dupes lh.niml.dset
    else:
        feh_str = ''
        feh_end = ''
        suff = ''
        istr = ''
        vstr = proc.view

    # set polort in db_cmd_tcat (section moved)

    # ---- allow no stims
    # if len(proc.stims) <= 0:   # be sure we have some stim files
    #    print "** missing stim files (-regress_stim_times/-regress_stim_files)"
    #     block.valid = 0
    #    return

    cmd = cmd + "# %s\n" % block_header('regress')

    # possibly add a make_stim_times.py command
    # (convert -stim_file to -stim_times)
    opt = block.opts.find_opt('-regress_stim_times')
    convert = (block.opts.find_opt('-regress_no_stim_times') == None) and \
                len(proc.stims) > 0
    if convert and (not opt.parlist or len(opt.parlist) == 0):
        newcmd = db_cmd_regress_sfiles2times(proc, block)
        if not newcmd: return
        cmd = cmd + newcmd

    # expand the basis list to match stims
    # (this should be done after any conversion from -stim_files)
    if len(proc.stims) != len(basis):
        # if just one basis function, duplicate for each stim, else error
        if len(basis) == 1:
            if proc.verb > 2: print('-- duplicating single basis function')
            basis = [basis[0] for ind in range(len(proc.stims))]
        else:
            print('** error: have %d basis functions but %d stim classes' \
                  % (len(basis), len(proc.stims)))
            return

    # if no stim files, default to using 3dTproject
    nstim_files = len(proc.stims) + len(proc.extra_stims)
    use_tproj = block.opts.have_yes_opt('-regress_use_tproject',
                                        default=(nstim_files==0))

    # create a stim_type list to match the stims
    opt = block.opts.find_opt('-regress_stim_types')
    # init list
    if not opt: stim_types = ['times']
    else: stim_types = opt.parlist
    if len(proc.stims) != len(stim_types):
        # if just one basis function, duplicate for each stim, else error
        if len(stim_types) == 1:
            stim_types = [stim_types[0] for i in range(len(proc.stims))]
        else:
            print('** error: have %d stim types but %d stim classes' \
                  % (len(stim_types), len(proc.stims)))
            return
    if not UTIL.vals_are_constant(stim_types, 'times'):
        print('++ applying %d stim types: %s' % (len(stim_types),stim_types))

    # ----------------------------------------
    # deal with motion (demean, deriv, per-run, censor)
    if block.opts.find_opt('-regress_no_motion'): proc.mot_labs = []
    else:
        err, newcmd = db_cmd_regress_motion_stuff(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # ----------------------------------------
    # user ortvecs, add vec/lab pairs to self.regress_orts list
    nxort = len(proc.extra_ortvec)
    if nxort > 0:
        if len(proc.extra_ortvec_labs) != nxort:
           print("** # extra_orvec != # extra_ortvec_labs")
           return
        ov = proc.extra_ortvec
        ol = proc.extra_ortvec_labs
        for ind in range(nxort):
           proc.regress_orts.append([ov[ind], ol[ind]])

    # ----------------------------------------
    # bandpass?
    if block.opts.find_opt('-regress_bandpass'):
        err, newcmd = db_cmd_regress_bandpass(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # if -regress_ROI*, a 'tlrc' block must come with -volreg_tlrc_warp
    do_roi = block.opts.find_opt('-regress_ROI') \
             or block.opts.find_opt('-regress_ROI_PC')
    anat_only_warp = \
       proc.find_block('tlrc') and not (proc.warp_epi & WARP_EPI_TLRC_WARP)
    if do_roi and anat_only_warp:
       print("** have -regress_ROI*, but anat/EPI not in same space")
       print("   either omit 'tlrc' block, or add -volreg_tlrc_warp")
       return
    
    # ----------------------------------------------------------------
    # start handling ROIs: first perform any resampling for ROI_import
    # ----------------------------------------------------------------

    # ----------------------------------------
    # check for user option -ROI_import, and resample any ROI dsets
    if proc.user_opts.find_opt('-ROI_import'):
        err, newcmd = db_cmd_resam_ROI_imports(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # ----------------------------------------
    # gmean?  change to generic -regress_ROI
    if block.opts.find_opt('-regress_ROI'):
        err, newcmd = db_cmd_regress_ROI(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # ----------------------------------------
    # last censoring is done, so possibly generate keep_trs as $ktrs
    # (also, generate keep_trs as $ktrs)
    newcmd = get_keep_trs_cmd(proc)
    if newcmd: cmd += newcmd

    # ----------------------------------------
    # regress anything from anat_followers.
    if block.opts.find_opt('-regress_ROI_PC'):
        err, newcmd = db_cmd_regress_pc_followers(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    if not all_erode_labels_used(proc, block): return

    # ----------------------------------------
    # prepare for anaticor (3dREMLfit or 3dD/3dTproject)
    if block.opts.find_opt('-regress_anaticor'):        proc.anaticor = 1
    elif block.opts.find_opt('-regress_anaticor_fast'): proc.anaticor = 2
    # rcr - if either and task, set up use of 3dREMLfit

    # ----------------------------------------
    # possibly use a mask
    if proc.mask and proc.regmask:
        mask = '    -mask %s' % proc.mask.shortinput()
    else: mask = ''

    # ----------------------------------------
    # maybe the user has specified global or local times
    # if so, verify the files against exactly that
    if block.opts.find_opt('-regress_global_times'):
        times_type = '    -global_times'
        verify_times_type = 3
    elif block.opts.find_opt('-regress_local_times'):
        times_type = '    -local_times'
        verify_times_type = 2
    else:
        times_type=''
        verify_times_type = 10 # either local or global

    # if the input datatype is float, force such output from 3dDeconvolve
    if proc.datatype == 3: datum = ' -float'
    else:                  datum = ''

    # check all input stim_times or stim_files for validity
    opt = block.opts.find_opt('-regress_stim_times')

    # maybe any original stims are as 1D, whether converting or not
    if not opt.parlist or len(opt.parlist) == 0: vtype = 1
    else:                                        vtype = verify_times_type
    goforit = block.opts.opt_has_arg('-regress_opts_3dD', arg='-GOFORIT')
    if not valid_file_types(proc, proc.stims_orig, vtype, stypes=stim_types,
                            goforit=goforit):
        return

    # check any extras against 1D only
    if not valid_file_types(proc, proc.extra_stims_orig, 1): return

    # check that AM types match married stim files
    if not married_types_match(proc, proc.stims_orig, stim_types, basis): return

    # note whether motion regs will be done via -ortvec
    # the default is now 'yes' [changed 16 Jan, 2019]
    mot_as_ort = block.opts.have_yes_opt('-regress_mot_as_ort', default=1)

    # count stim, motion counts if not as_ort
    # - extra_ortvec do not count
    if mot_as_ort: nmotion = 0
    else:          nmotion = len(proc.mot_labs) * len(proc.mot_regs)
    if proc.ricor_apply == 'yes': nricor = proc.ricor_nreg
    else:                         nricor = 0
    total_nstim =  len(proc.stims) + len(proc.extra_stims) + nmotion + nricor

    # add any censor option
    if proc.censor_file: censor_str = '    -censor %s' % proc.censor_file
    else:                censor_str = ''

    # --------------------------------------------------
    # if skip_censor, either clear censor_str or just do an early return
    # (early return via 'DONE' terminates this proc instance)
    if proc.skip_censor or block.opts.find_opt('-regress_skip_censor'):
       proc.skip_censor = 1
       if proc.censor_file: censor_str = ''
       else:                return 'DONE'

    # check for regress_orts lines
    reg_orts = []
    for ort in proc.regress_orts:
       reg_orts.append('    -ortvec %s %s' % (ort[0], ort[1]))
    if mot_as_ort and len(proc.mot_regs) > 0:
       if len(proc.mot_regs) != len(proc.mot_names):
           print('** mismatch between mot_regs and mot_names\n' \
                 '   regs = %s\n'                               \
                 '   names = %s\n\n' % (proc.mot_regs, proc.mot_names))
           return
       for ind in range(len(proc.mot_regs)):
           reg_orts.append('    -ortvec %s %s' % (proc.mot_regs[ind],
                                                  proc.mot_names[ind]))

    # make actual 3dDeconvolve command as string c3d:
    #    init c3d, add O3dd elements, finalize c3d
    #    (O3dd = list of 3dd option lines, which may need an extra indent)

    # note first input, to have a known afni_name
    proc.regress_inset = gen_afni_name(proc.prev_prefix_form(1, block, view=1),
                                        do_sel=0)

    O3dd = ['%s3dDeconvolve -input %s'%(istr, proc.prev_dset_form_wild(block)),
            mask, censor_str]
    O3dd.extend(reg_orts)
    O3dd.extend([ '    -polort %d%s' % (proc.regress_polort, datum),
                  normall, times_type,
                  '    -num_stimts %d' % total_nstim])

    # verify labels (now that we know the list of stimulus files)
    opt = block.opts.find_opt('-regress_stim_labels')
    if not opt or not opt.parlist:
        labels = []
        for ind in range(len(proc.stims)):
            labels.append('stim%02d' % (ind+1))
        if proc.verb > 0 and len(labels) > 0:
            print('++ creating new stim labels: %s' % labels)
    elif len(proc.stims) != len(opt.parlist):
        print("** cmd_regress: have %d stims but %d labels" % \
              (len(proc.stims), len(opt.parlist)))
        return
    else:  # we have the same number of labels as stims
        labels = opt.parlist

    # and extra labels, if needed
    if len(proc.extra_stims) > 0:
        if len(proc.extra_labs) > 0:
            exlabs = proc.extra_labs
        else:
            exlabs = []
            norig = len(proc.stims)
            nex   = len(proc.extra_stims)
            for ind in range(norig, norig+nex):
                exlabs.append('stim%02d' % (ind+1))
            if proc.verb > 0: print('++ adding extra labels: %s' % exlabs)

    # note the total number of regressors (of interest)
    nregsOI = len(proc.stims) + len(proc.extra_stims)

    # note any RONI (regs of no interest)
    roni_list = []
    bopt = block.opts.find_opt('-regress_RONI')
    if bopt and bopt.parlist:
        roni_list = bopt.parlist
        # check min/max
        if min(roni_list) < 1 or max(roni_list) > nregsOI:
            print("** regressor indices in RONI list must be in [1,%d]\n" \
                  "   have: %s" % (nregsOI, roni_list))
            return

    # add iresp options for basis functions without known response functions
    opt = block.opts.find_opt('-regress_iresp_prefix')
    if not opt or not opt.parlist: Liresp = []
    else:
        Liresp = []
        for index in range(len(labels)):
            if not UTIL.basis_has_known_response(basis[index]) and \
               not stim_types[index] == 'file':
                Liresp.append("    -iresp %d %s%s_%s.$subj" % \
                        (index+1, tmp_prefix, opt.parlist[0], labels[index]))

    # write out stim lines (add -stim_base to any RONI)
    # (also, accumulates files with TENT functions)
    sfiles = block.opts.find_opt('-regress_no_stim_times')
    tent_times = []
    for ind in range(len(proc.stims)):
        # rcr - allow -stim_times_AM/IM here?  make user choose one?
        #       (so maybe -stim_times can be set from proc.stim_times_opt)
        if sfiles:  # then -stim_file and no basis function
            O3dd.append("    -stim_file %d %s" % (ind+1, proc.stims[ind]))
        elif stim_types[ind] == 'file':
            O3dd.append("    -stim_file %d %s" % (ind+1, proc.stims[ind]))
            if basis[ind] != 'NONE':
               ss = os.path.basename(proc.stims[ind])
               print("** ignoring basis #%d, %s, for -stim_type 'file' = %s" \
                     % (ind+1, basis[ind], ss))
        else:
            if stim_types[ind] == 'times': st_suf = ''
            else:                          st_suf = '_%s' % stim_types[ind]
            # allow for AM2 centering via basis backdoor        30 Apr 2015
            # rcr - consider adding a more formal option later
            posn = basis[ind].find(' :')
            if stim_types[ind] == 'AM2' and posn > 3:
               bb = basis[ind]
               bstr = "'%s' %s" % (bb[0:posn], bb[posn+1:])
            else: bstr = "'%s'" % basis[ind]
            O3dd.append("    -stim_times%s %d %s %s"  % \
                        (st_suf, ind+1, proc.stims[ind], bstr))
            # accumulate timing files with TENTs for later error checks
            if basis[ind].find('TENT') >= 0: tent_times.append(proc.stims[ind])
        # and add the label
        if ind+1 in roni_list: rstr = ' -stim_base %d' % (ind+1)
        else:                  rstr = ''
        O3dd.append("    -stim_label %d %s%s" % (ind+1, labels[ind],rstr))

    # accumulate offset for current regressor list (3dD input is 1-based)
    regindex = len(proc.stims) + 1

    # maybe add extra_stims (add -stim_base to any RONI)
    if len(proc.extra_stims) > 0:
        for ind in range(len(proc.extra_stims)):
            sind = ind+regindex
            if sind in roni_list: rstr = ' -stim_base %d' % sind
            else:                 rstr = ''
            O3dd.append("    -stim_file %d %s" % (sind,proc.extra_stims[ind]))
            O3dd.append("    -stim_label %d %s%s" % (sind,exlabs[ind],rstr))
        regindex += len(proc.extra_stims)

    # write out registration param lines
    if nmotion > 0:
        nlabs = len(proc.mot_labs)
        nmf = len(proc.mot_regs)
        for findex in range(len(proc.mot_regs)):
            mfile = proc.mot_regs[findex]
            for ind in range(nlabs):
                if nmf > 1: mlab = '%s_%02d' % (proc.mot_labs[ind], findex+1)
                else:       mlab = '%s'      % (proc.mot_labs[ind])
                sind = regindex + nlabs*findex + ind
                tstr = "    -stim_file %d %s'[%d]' -stim_base %d" \
                            % (sind, mfile, ind, sind)
                ttstr = "-stim_label %d %s" % (sind, mlab)
                if proc.surf_anat: # if surf-based, put label on new line
                    O3dd.append(tstr)
                    O3dd.append('    %s' % ttstr)
                else:
                    O3dd.append('%s %s' % (tstr, ttstr))
        regindex += nmotion

    # write out ricor param lines (put labels afterwards)
    # (only apply if user requests it        30 Jan 2012)
    if proc.ricor_apply == 'yes' and proc.ricor_reg and proc.ricor_nreg > 0:
        for ind in range(proc.ricor_nreg):
            O3dd.append("    -stim_file %02d %s'[%02d]' "  \
                        "-stim_base %02d"                  \
                        % (ind+regindex, proc.ricor_reg, ind, ind+regindex))
        tlist = []
        for ind in range(proc.ricor_nreg):
            # tstr += "-stim_label %02d ricor%02d " % (ind+regindex, ind)
            tlist.append("-stim_label %02d ricor%02d" % (ind+regindex, ind))
        O3dd.append('    ' + ' '.join(tlist))
        regindex += proc.ricor_nreg

    # -------------------- fitts and errts setup --------------------

    # see if the user wants the fit time series
    opt = block.opts.find_opt('-regress_fitts_prefix')
    fitts_pre = ''
    if not opt or not opt.parlist: fitts = ''
    else:
        fitts_pre = opt.parlist[0]
        fitts = '    -fitts %s%s%s' % (tmp_prefix, fitts_pre, suff)

    # -- see if the user wants the error time series --
    opt = block.opts.find_opt('-regress_errts_prefix')
    bluropt = block.opts.find_opt('-regress_est_blur_errts')
    tsnropt = block.opts.find_opt('-regress_compute_tsnr')
    # if there is no errts prefix, but the user wants to measure blur, add one
    # (or if there are no normal regressors)
    if nregsOI == 0 or (not opt.parlist and (bluropt or tsnropt)):
        opt.parlist = ['errts.${subj}']

    if not opt or not opt.parlist: errts = ''
    else:
        # note and apply
        proc.errts_pre_3dd = '%s%s' % (opt.parlist[0], suff)
        proc.errts_pre     = proc.errts_pre_3dd
        errts = '    -errts %s%s' % (tmp_prefix, proc.errts_pre)
    # -- end errts --

    # if the user wants to compute fitts, save the prefix
    compute_fitts = 0
    if block.opts.find_opt('-regress_compute_fitts'):
        if fitts_pre == '':
            print('** regress_compute_fitts: no fitts prefix')
            return
        if proc.errts_pre == '':
            print('** regress_compute_fitts: no errts prefix')
            return
        compute_fitts = 1
        fitts = ''  # so nothing in 3dDeconvolve

    # see if the user has provided other options (like GLTs)
    opt = block.opts.find_opt('-regress_opts_3dD')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '    %s' % \
             ' '.join(UTIL.quotize_list(opt.parlist, '\\\n%s    '%istr, 1))

    # are we going to stop with the 1D matrix?
    # (either explicit option or if using 3dTproject)
    opt = block.opts.find_opt('-regress_3dD_stop')
    if opt: proc.have_olsq = 0
    if opt or use_tproj:
        stop_opt = '    -x1D_stop'
        proc.have_3dd_stats = 0
    else:
        stop_opt = ''
        proc.have_3dd_stats = 1

    # do we want F-stats
    opt = block.opts.find_opt('-regress_fout')
    if opt.parlist[0] == 'yes': fout_str = '-fout '
    else:                       fout_str = ''

    # do we want a cbucket dataset?
    opt = block.opts.find_opt('-regress_make_cbucket')
    if opt.parlist[0] == 'yes':
        cbuck_str = "    -cbucket %sall_betas.$subj%s" % (tmp_prefix, suff)
    else: cbuck_str = ""

    # add misc options
    O3dd.extend(Liresp) # Liresp is a list
    O3dd.append(other_opts)
    O3dd.append("    %s-tout -x1D %s%s -xjpeg %sX.jpg" \
                % (fout_str, tmp_prefix, proc.xmat, tmp_prefix))
    if proc.censor_file:
        newmat = 'X.nocensor.xmat.1D'
        O3dd.append("    -x1D_uncensored %s%s" % (tmp_prefix, newmat))
    O3dd.extend([fitts, errts, stop_opt, cbuck_str])
    O3dd.append("    -bucket %sstats.$subj%s\n" % (tmp_prefix, suff))

    # possibly run 3dTproject (instead of 3dDeconvolve)
    if use_tproj and proc.have_olsq:
        # inputs, -censor, -cenmode, -ort Xmat, -prefix
        if proc.censor_file: xmat = '%s%s' % (tmp_prefix, newmat)
        else:                xmat = '%s%s' % (tmp_prefix, proc.xmat)
        if errts: epre = proc.errts_pre
        else:     epre = 'errts.$subj'
        # getting ugly: alter prefix but save any extension (niml.dset?)
        aset = proc.regress_inset.new('%s%s'%(tmp_prefix, epre), parse_pref=1)
        aset.new_prefix(aset.prefix + '.tproject')

        tpcmd = db_cmd_tproject(proc, block, proc.prev_dset_form_wild(block),
                maskstr=mask, censtr=censor_str, xmat=xmat,
                prefix=aset.out_prefix())
        if not tpcmd: return
        tpcmd = '\n' + tpcmd
    else: tpcmd = ''

    # possibly run the REML script (only here in the case of surfaces)
    if block.opts.find_opt('-regress_reml_exec') and proc.surf_anat:
        rcmd = db_cmd_reml_exec(proc, block, short=1)
        if not rcmd: return
        rcmd = '\n' + rcmd
        proc.have_reml_stats = 1        # note that we ran this
    else: rcmd = ''

    # now create full 3dDeconvolve command, connecting every option
    # line with space, backslash, a newline, and possibly another indent,

    jstr = ' \\\n%s' % istr
    c3d  = '# ------------------------------\n'          \
           '# run the regression analysis\n' + feh_str + \
           jstr.join([s for s in O3dd if s])
    c3d += tpcmd + rcmd + feh_end + '\n\n'

    # done creating 3dDeconvolve command c3d, add to cmd string
    cmd += c3d

    # maybe user just wants a 3dDeconvolve command script
    if proc.script_3dD:
       header = '#!/bin/tcsh\n\n'               \
                'set subj = %s\n\n'             \
                'set prefix_3dd = %s\n\n' % (proc.subj_id, proc.prefix_3dD)
       UTIL.write_text_to_file(proc.script_3dD, header+c3d, wrap=1, exe=1)
       print('++ writing 3dDeconvolve script %s ...\n' % proc.script_3dD)
       return 'DONE'

    # if 3dDeconvolve fails, terminate the script
    # (rcr - maybe just skip this in case of surfaces)
    if not proc.surf_anat:
        cmd +=  "# if 3dDeconvolve fails, terminate the script\n"       \
                "if ( $status != 0 ) then\n"                            \
                "    echo '---------------------------------------'\n"  \
                "    echo '** 3dDeconvolve error, failing...'\n"        \
                "    echo '   (consider the file 3dDeconvolve.err)'\n"  \
                "    exit\n"                                            \
                "endif\n\n\n"

    # check the X-matrix for high pairwise correlations
    opt = block.opts.find_opt('-regress_cormat_warnings')
    if not opt or OL.opt_is_yes(opt):  # so default to 'yes'
        rcmd = "# display any large pairwise correlations from the X-matrix\n"\
               "1d_tool.py -show_cormat_warnings -infile %s"                  \
               " |& tee out.cormat_warn.txt\n\n" % proc.xmat
        cmd = cmd + rcmd

    # make a file with df_info
    if not block.opts.have_no_opt('-regress_show_df_info'):
        rcmd = "# display degrees of freedom info from X-matrix\n"  \
               "1d_tool.py -show_df_info -infile %s"                \
               " |& tee out.df_info.txt\n\n" % proc.xmat
        cmd = cmd + rcmd

    # if we have any TENT functions, check the stim files for odd timing
    if len(tent_times) > 0:
        rlens = [proc.tr*rep for rep in proc.reps_all]
        tent_str = " \\\n                             ".join(tent_times)
        outfile  = 'out.TENT_warn.txt'
        rcmd = "# look for odd timing in files for TENT functions\n"    \
               "timing_tool.py -multi_timing %s \\\n"                   \
               "               -tr %s -warn_tr_stats |& tee %s\n\n"     \
               % (tent_str, proc.tr, outfile)
        cmd = cmd + rcmd

    # if censor file, note the xmat that ignores censoring
    if proc.censor_file: proc.xmat_nocen = newmat

    # possibly run the REML script (run earlier in the case of surfaces)
    if block.opts.find_opt('-regress_reml_exec') and not proc.surf_anat:
        rcmd = db_cmd_reml_exec(proc, block)
        if not rcmd: return
        cmd = cmd + rcmd + '\n\n'
        proc.have_reml_stats = 1        # note that we ran this

    # if REML and errts_pre, append _REML to errts_pre
    if block.opts.find_opt('-regress_reml_exec') and stop_opt \
       and proc.errts_pre_3dd:
        eorig = proc.errts_pre_3dd
        if proc.surf_anat:
           proc.errts_pre = eorig.replace('.niml.dset','_REML.niml.dset')
        else:
           proc.errts_pre = eorig + '_REML'

    # create all_runs dataset
    proc.all_runs = 'all_runs%s$subj%s' % (proc.sep_char, suff)
    cmd = cmd + "# create an all_runs dataset to match the fitts, errts, etc.\n"
    cmd = cmd + feh_str + "%s3dTcat -prefix %s %s\n" % \
          (istr, proc.all_runs, proc.prev_dset_form_wild(block)) + feh_end+'\n'

    # check for invalid anaticor usage
    if proc.anaticor and nregsOI > 0 \
                     and not block.opts.find_opt('-regress_reml_exec'):
       print("** ANATICOR with task requires -regress_reml_exec")
       return

    # fast or slow anaticor via projection (resting state)
    if proc.anaticor and nregsOI == 0:
       # first set of commands: generate WMeLocal
       rv, tcmd = db_cmd_regress_anaticor(proc, block)
       if rv: return

       if proc.anaticor == 2: alabel = 'fanaticor'
       else:                  alabel = 'anaticor'
       rset = proc.regress_inset.new('errts.$subj.%s' % alabel)
       if proc.censor_file: xmat = '%s' % newmat
       else:                xmat = '%s' % proc.xmat

       if proc.anaticor == 2: alabel = 'fast ANATICOR'
       else:                  alabel = 'ANATICOR'
       acmd = '# --------------------------------------------------\n' \
              '# generate %s result: %s\n\n' % (alabel, rset.shortinput())
       acmd += tcmd

       tcmd = db_cmd_tproject(proc, block, proc.prev_dset_form_wild(block),
               maskstr=mask, censtr=censor_str, xmat=xmat,
               dsort=proc.aic_lset, prefix=rset.out_prefix())
       if not tcmd: return
       cmd += (acmd+tcmd)

    # just make sure a label exists
    roilab,rv = block.opts.get_string_opt('-regress_anaticor_label')
    if roilab and not rv:
       if not proc.have_roi_label(roilab):
          print("** -regress_anaticor_label: missing ROI label: '%s'" % roilab)
          return

    # maybe add the 3dRSFC block
    if block.opts.find_opt('-regress_RSFC'):
       rv, tcmd = db_cmd_regress_rsfc(proc, block)
       if rv: return
       cmd += tcmd

    # ---------- DONE WITH ALL REGRESSION ----------

    # note errts dataset
    if proc.errts_reml: errts = proc.errts_reml
    else:               errts = proc.errts_pre
    proc.errts_final = errts

    # if errts and scaling, maybe create tsnr volume as mean/stdev(errts)
    # (if scaling, mean should be 100)
    opt = block.opts.find_opt('-regress_compute_tsnr')
    if opt.parlist[0] == 'yes':
       if errts:
          rv, tcmd = db_cmd_regress_tsnr(proc, block, proc.all_runs, errts)
          if rv or tcmd == None: return  # error
          if tcmd != '': cmd += tcmd
       else: print('-- no errts, will not compute final TSNR')

    # if errts and epi mask, maybe compute GCOR as l2norm of maskave of
    # unit errts (leave as rm. dataset)
    opt = block.opts.find_opt('-regress_compute_gcor')
    if opt.parlist[0] == 'yes':
       bmask = proc.get_roi_dset('brain')
       if not bmask: bmask = proc.mask
       if errts and bmask and not proc.surf_anat:
          tcmd = db_cmd_regress_gcor(proc, block, errts, bmask)
          if tcmd == None: return  # error
          if tcmd != '': cmd += tcmd
       elif proc.verb > 1:
          print('-- no errts or EPI mask (or have surf), will not compute GCOR')

    # possibly create computed fitts dataset
    if compute_fitts:
        fstr = feh_str
        if stop_opt == '': # create if no -x1D_stop
            fstr += "%s# create fitts dataset from all_runs and errts\n" % istr
            fstr += "%s3dcalc -a %s%s -b %s%s -expr a-b \\\n"            \
                    "%s       -prefix %s%s\n"                            \
                    % (istr, proc.all_runs, vstr, proc.errts_pre, vstr,
                       istr, fitts_pre, suff)
        elif not block.opts.find_opt('-regress_reml_exec'):
            print('** cannot compute fitts, have 3dD_stop but no reml_exec')
            return

        # if reml_exec, make one for the REML fitts, too
        if block.opts.find_opt('-regress_reml_exec'):
            if not proc.errts_reml:
               print('** missing errts_reml for computation of fitts')
               return
            if stop_opt: fstr += '\n'
            fstr += "%s# create fitts from REML errts\n" % istr
            fstr += "%s3dcalc -a %s%s -b %s%s -expr a-b \\\n" \
                    "%s       -prefix %s\\_REML%s\n"          \
                    % (istr, proc.all_runs, vstr, proc.errts_reml, vstr,
                       istr, fitts_pre, suff)
        cmd = cmd + fstr + feh_end + '\n'

    # extract ideal regressors, and possibly make a sum
    opt = block.opts.find_opt('-regress_no_ideals')
    if not opt and len(basis) > 0:
        if len(labels) != len(basis):
            print('** internal error: label and basis arrays not equal lengths')
            print('   (%d labels, %d basis functions)'%(len(labels),len(basis)))
            return
        # while basis functions have one regressor, make ideals
        # (so no ideal after failure)
        if UTIL.basis_has_one_reg(basis[0], st=stim_types[0]):
            cmd = cmd + "# create ideal files for fixed response stim types\n"
            first = (proc.regress_polort+1) * proc.runs
            for ind in range(len(labels)):
                # once unknown or multiple regs, quit
                if not UTIL.basis_has_one_reg(basis[ind], st=stim_types[ind]):
                   break
                cmd = cmd + "1dcat %s'[%d]' > ideal_%s.1D\n" % \
                            (proc.xmat_nocen, first+ind, labels[ind])
            cmd = cmd + '\n'
        else: print('-- classes have multiple regressors, so not making ideals')

    opt = block.opts.find_opt('-regress_make_ideal_sum')
    nopt = block.opts.find_opt('-regress_no_ideal_sum')
    # opt should always be set, so let nopt override
    if opt and opt.parlist and not nopt:
        # if no task regs, skip
        if proc.have_task_regs:
           # get regressors of interest from X-matrix, rather than in python
           # (this requires check_date of 2 Nov 2010)
           xstim = 'X.stim.xmat.1D'
           cmd = cmd +                                                     \
                  "# --------------------------------------------------\n" \
                  "# extract non-baseline regressors from the X-matrix,\n" \
                  "# then compute their sum\n"                             \
                  '1d_tool.py -infile %s -write_xstim %s\n'                \
                  '3dTstat -sum -prefix %s %s\n\n'                         \
                  % (proc.xmat_nocen, xstim, opt.parlist[0], xstim)
        else:
           if proc.verb > 1: print("** no stim: skipping sum_ideal.1D")
           cmd = cmd +                                                     \
                  "# --------------------------------------------------\n" \
                  "# compute sum of baseline (all) regressors\n"           \
                  '3dTstat -sum -prefix sum_baseline.1D %s\n\n'            \
                  % (proc.xmat_nocen)

    # check for blur estimates
    bcmd = db_cmd_blur_est(proc, block)
    if bcmd == None: return  # error
    if bcmd: cmd += bcmd

    return cmd

# Run 3dTproject, akin to 3dDeconvolve.
#
# Use the same -input option and -censor options, and possibly add
# -cenmode ZERO.  Include -ort with the X-matrix and use errts prefix.
#
# return None on failure
def db_cmd_tproject(proc, block, insets, maskstr='', censtr='', cmode='ZERO',
                    xmat='X.xmat.1D', dsort='', prefix='errts.tproject'):
    """generate a simple 3dTproject command
    """

    if proc.verb > 1: print('++ creating 3dTproject command string')

    if proc.surf_anat: istr = '    '
    else:              istr = ''

    # for now, create output to match that of 3dDeconvolve

    if maskstr: mstr = maskstr.strip()
    else:       mstr = ''
    if mstr != '': mstr = '%s           %s \\\n' % (istr, mstr)

    cstr = censtr.strip()
    if cstr != '': cstr = '%s           %s -cenmode %s \\\n'%(istr,cstr,cmode)

    if dsort: dstr = '%s           -dsort %s \\\n' % (istr, dsort.pv())
    else:     dstr = ''

    cmd = '%s# -- use 3dTproject to project out regression matrix --\n' \
          '%s#    (make errts like 3dDeconvolve, but more quickly)\n'   \
          '%s3dTproject -polort 0 -input %s \\\n'                       \
          '%s%s%s'                                                      \
          '%s           -ort %s -prefix %s\n\n'                         \
          % (istr, istr, istr, insets, mstr, cstr, dstr, istr, xmat, prefix)

    proc.errts_pre = prefix

    return cmd

# create a short command to run the REML script
# The script name is currently stats.REML_cmd, based on the 'stats.' -bucket
# prefix in 3dD.
#
# return None on failure
def db_cmd_reml_exec(proc, block, short=0):
    """short version does not have the status check
       - probably used for surface analysis"""

    if proc.verb > 1: print('++ creating reml_exec command string')

    if proc.surf_anat: istr = '    '
    else:              istr = ''

    # if anaticor, first generate local white matter
    if proc.anaticor:
       rv, cmd = db_cmd_regress_anaticor(proc, block)
       if rv: return ''
       aopts = '-dsort %s ' % proc.aic_lset.shortinput()
       astr = '%s# (include ANATICOR regressors via -dsort)\n' % istr
    else:
       cmd = ''
       aopts = ''
       astr = ''

    # see if the user has provided other 3dREMLfit options
    opt = block.opts.find_opt('-regress_opts_reml')
    if not opt or not opt.parlist: reml_opts = ''
    else: reml_opts = ' '.join(UTIL.quotize_list(opt.parlist, '', 1))

    cmd +='%s# -- execute the 3dREMLfit script, written by 3dDeconvolve --\n' \
          '%s'                                                                \
          '%stcsh -x stats.REML_cmd %s%s\n' % (istr,astr, istr,aopts, reml_opts)
    # use REML errts for future computations
    if proc.surf_anat:
       errnew = proc.errts_pre_3dd.replace('.niml.dset','_REML.niml.dset')
       errnew = errnew.replace('$hemi', '${hemi}')
    else:
       errnew = proc.errts_pre_3dd + '_REML'
    proc.errts_reml = errnew

    # if 3dDeconvolve fails, terminate the script
    if not short:
        cmd += "\n"                                                    \
               "# if 3dREMLfit fails, terminate the script\n"          \
               "if ( $status != 0 ) then\n"                            \
               "    echo '---------------------------------------'\n"  \
               "    echo '** 3dREMLfit error, failing...'\n"           \
               "    exit\n"                                            \
               "endif\n"

    return cmd

# compute GCOR
def db_cmd_regress_gcor(proc, block, errts_pre, bmask):

    if not errts_pre or not bmask: return ''
    if proc.surf_anat:
       if proc.verb > 1: print("** no gcor until handle 'both' hemis")
       return ''

    # Do not handle surface until we have both hemispheres at once.

    gcor_file  = 'out.gcor.1D'
    gu_mean    = 'mean.errts.unit.1D'
    uset       = gen_afni_name('rm.errts.unit%s' % proc.view)
    errts_dset = '%s%s' % (errts_pre, proc.view)

    cmd = '# ---------------------------------------------------\n'     \
          '# compute and store GCOR (global correlation average)\n'     \
          '# (sum of squares of global mean of unit errts)\n'           \
          '3dTnorm -norm2 -prefix %s %s\n'                              \
          '3dmaskave -quiet -mask %s \\\n'                              \
          '          %s > %s\n'                                         \
          % (uset.prefix, errts_dset, bmask.pv(),
             uset.pv(), gu_mean)

    cmd += "3dTstat -sos -prefix - %s\\' > %s\n"                        \
           'echo "-- GCOR = `cat %s`"\n\n'                              \
            % (gu_mean, gcor_file, gcor_file)

    corset = 'corr_brain'
    gmean  = 'mean.errts.1D'
    cmd += '# ---------------------------------------------------\n'    \
           "# compute correlation volume\n"                             \
           "# (per voxel: correlation with masked brain average)\n"     \
           "3dmaskave -quiet -mask %s \\\n"                             \
           "          %s > %s\n"                                        \
           "3dTcorr1D -prefix %s %s%s %s\n\n"                           \
           % (bmask.pv(), errts_dset, gmean,
              corset, errts_pre, proc.view, gmean)

    # compute extra correlation volumes (assuming EPI grid ROIs followers):
    #   - This WAS average correlation ('ave unit' dot 'unit errts'),
    #     but that was basically scaled results for correlation with
    #     average, so prefer the latter.
    #
    #   unit errts -> errts, remove -dot
    #
    #   3dcalc -a ROI -b full_mask -expr 'a*b' -prefix ROI.FM
    #   3dmaskave -quiet -mask ROI.FM errts+tlrc > mean.ROI.1D
    #   3dTcorr1D -prefix corr_ROI errts+tlrc mean.ROI.1D
    oname = '-regress_make_corr_vols'
    roilist, rv = block.opts.get_string_list(oname)
    if roilist:
       rstr = '# compute %d requested correlation volume(s)\n' % len(roilist)
       for roi in roilist:
          mset = proc.get_roi_dset(roi)
          if mset == None:
             print("** %s: no matching ROI '%s'" % (oname, roi))
             return

          mpre = 'rm.fm.%s' % roi
          meants = 'mean.ROI.%s.1D' % roi
          cvol = 'corr_af_%s' % roi

          rstr += '# create correlation volume %s\n' % cvol
          rstr += "3dcalc -a %s -b %s -expr 'a*b' \\\n"         \
                  "       -prefix %s\n" \
                  % (mset.pv(), bmask.pv(), mpre)
          rstr += "3dmaskave -q -mask %s%s \\\n"                \
                  "          %s > %s\n"                         \
                  % (mpre, proc.view, errts_dset, meants)
          rstr += "3dTcorr1D -prefix %s %s %s\n\n"              \
                  % (cvol, errts_dset, meants)

       cmd += rstr

    return cmd


def set_proc_vr_vall(proc, block, parset=None, newpre='rm.all_runs',
                                               blabel='volreg'):
   """if proc.vr_vall is set, do nothing
      else return string to create it

      to create, use prefix and parent dset for new()
   """

   # create or not catenated volreg dataset
   if proc.vr_vall != None: return ''

   # use combine or volreg block, whichever block comes last
   # (i.e. require both, if they exist)
   # (if blabel is different, use it)
   def_blist = ['combine', 'volreg']
   if blabel in def_blist: blist = def_blist
   else:                   blist = [blabel]
   blabel = proc.find_latest_block(blist)
   vblock = proc.find_block_or_prev(blabel, block)
   blabel = vblock.label   # if neither exists, will use current block
   if vblock == None:
      print('** SPVV: failed to find corresponding %s block' % blabel)
      return ''
   
   proc.vr_vall_lab = blabel


   dprefix = '%s.%s' % (newpre, blabel)

   cmd = '# create catenated %s dataset\n'  \
         '3dTcat -prefix %s %s\n'           \
         % (blabel, dprefix, proc.dset_form_wild(vblock.label))

   proc.vr_vall = parset.new(dprefix)

   return cmd


# return anaticor commands, except for final 3dTproject
def db_cmd_regress_anaticor(proc, block):
    """return a string for running fast anaticor - generate localWMe dset

       result: set proc.aic_let = resulting local dataset
               return command string

       return status (0=success) and command string
    """

    if not proc.anaticor: return 0, ''

    if proc.surf_anat:
       print('** -regress_anaticor: not ready for surface analysis')
       return 1, ''

    # maybe we already have such a dataset
    if proc.aic_lset != None: return 0, ''

    if proc.anaticor == 1:
       print('** WARNING: ANATICOR output now includes zero volumes at\n' \
             '            censor points, matching fast ANATICOR and\n'    \
             '            non-ANATICOR cases')
       fstr = ''
    elif proc.anaticor == 2: fstr = 'fast '

    # note resulting local dataset
    roilab,rv = block.opts.get_string_opt('-regress_anaticor_label',
                                          default='WMe')
    rset = proc.regress_inset.new('Local_%s_rall'%roilab)
    proc.aic_lset = rset

    # note mask used to generate result
    mset = proc.get_roi_dset(roilab)
    if mset == None:
       print("** ANATICOR missing mask label: '%s' -->\n"              \
             '   see options: -mask_segment_anat, -mask_segment_erode' \
             ' -regress_ROI_*' % roilab)
       return 1, ''

    # get radius
    rad = get_anaticor_radius(proc, block)
    if rad <= 0.0: return 1, ''

    # init command
    cmd = '# --------------------------------------------------\n' \
          '# %sANATICOR: generate local %s time series averages\n' \
          % (fstr, roilab)

    # create or note catenated volreg dataset
    cmd += set_proc_vr_vall(proc, block, parset=rset)
    vall = proc.vr_vall.shortinput()

    # generate main command string
    if proc.anaticor == 2:
       vmask = '%s.mask' % proc.vr_vall.prefix
       cmd += '\n# mask white matter before blurring\n'              \
              '3dcalc -a %s -b %s \\\n'                              \
              '       -expr "a*bool(b)" -datum float -prefix %s\n\n' \
              % (vall, mset.shortinput(), vmask)

       # note the radius fraction to terminate blur at
       # (match method in @radial_correlate, default=0.5)
       oname = '-regress_anaticor_term_frac'
       merge_frad, err = block.opts.get_type_opt(float, oname, default=0.5)
       if err:
          print('** error: option %s requires a float argument' % oname)
          return 1, ''
       if merge_frad <= 0:
          print('** error: %s must be positive' % oname)
          return 1, ''

       # leave users opt to go full Gaussian, as with old _fast method
       if block.opts.have_yes_opt('-regress_anaticor_full_gaussian', default=1):
          viastr = '# via full Gaussian blur (radius = %g mm)\n' % rad
          # convert from radius back to diameter (FWHM)
          rad = 2.0 * rad
          ffstr = ''
       else:
          # scale radius by fraction of HWHM to stop at, then up to FWHM
          # (3dmerge takes a fwhm measure)
          rold = rad
          rad = 2.0 * rad / merge_frad

          # in Gaussian 1 HWHM = sqrt(2*ln(2)) sigmas = 1.17741 sigmas
          # (then scale by the 3dmerge fractional radius)
          ffscale = 1.17741
          firfac = ffscale * merge_frad

          viastr = '# via truncated blur\n'                                 \
                   '# - scale radius %g by %g (1/_term_frac), so FWHM=%d\n' \
                   '# - then truncate at %g sigmas (%g HWHM * %g S/HWHM)\n' \
                    % (rold, 1.0/merge_frad, rad, firfac, merge_frad, ffscale)
          ffstr = '-DAFNI_BLUR_FIRFAC=%g ' % firfac

       # QC: store the actual 3dmerge command to make a blurred mask
       merge_cmd = '3dmerge %s-1blur_fwhm %g -doall' % (ffstr, rad)
       cmd += '# generate ANATICOR voxelwise regressors\n' + viastr

       # avoid strange line wrapping, and similarly store the prefix form
       # (we *could* just put -prefix on the next line, as in the 2nd case)
       if ffstr == '':
          prefix_form = ' -prefix %s \\\n        %s\n\n'
       else:
          prefix_form = ' \\\n        -prefix %s %s\n\n'

       # finally, make a well-formatted 3dmerge command
       instr = '%s%s' % (vmask, proc.view)
       cmd += merge_cmd + prefix_form % (rset.out_prefix(), instr)

       # additionally,  make a corresponding blur of the mask dataset, for QC
       # (use a temporary float
       mtmp = mset.new(new_pref='rm.mask.anaticor.float')
       cmd += '# QC: similarly blur the mask to get an idea of the coverage\n'\
              '#     (use a float version of the mask for blurring)\n'
       cmd += '3dcalc -a %s -expr a -datum float \\\n' \
              '       -prefix %s\n'                    \
              % (mset.shortinput(), mtmp.prefix)
       cmd += merge_cmd + \
               prefix_form % ('fanaticor_mask_coverage', mtmp.shortinput())

    # handle non-blur method, as in original ANATICOR
    else:
       cmd += "3dLocalstat -stat mean -nbhd 'SPHERE(%g)' -prefix %s \\\n" \
              "            -mask %s -use_nonmask \\\n"                    \
              "            %s\n\n"                                        \
              % (rad, rset.out_prefix(), mset.shortinput(), vall)

    # possibly create diagnostic correlation volumes
    if block.opts.find_opt('-regress_make_corr_AIC'):
      cmd +='# diagnostic volume: voxel correlation with local white matter\n'\
            '#                    (above and beyond X-matrix regressors)\n'   \
            '3dTcorrelate -prefix %s -ort %s \\\n'                            \
            '             %s %s\n\n' %                                        \
            ('corr_AIC_%sL'%roilab, proc.xmat_nocen, vall, rset.pv())

      cmd +='# diagnostic volume: raw correlation, no X-matrix regressors\n'  \
            '3dTcorrelate -prefix %s \\\n'                                    \
            '             %s %s\n\n'                                          \
            % ('corr_AIC_%sL_raw'%roilab, vall, rset.pv())

    return 0, cmd

def get_keep_trs_cmd(proc):
    # sub-brick selection, in case of censoring
    # (only return this once)
    #
    # note: this is currently used only in TSNR computation
    #     : 3dpc is used after 3dTproject -cenmode KILL
    #     : blur estimation does $trs per run, so it might also need changing

    if proc.censor_file and proc.keep_trs == '':

       # ** 21 Aug 2023: Shruti reports failure to read sub-brick selectors
       #                 because a lot of censoring leads to file names that
       #                 are beyond our size limits.  Use text files, instead.
       #               : so do not use encoded trs, but unencoded from a file
       
       ktr_text = 'out.keep_trs_rall.txt'

       cs = '# note TRs that were not censored\n'                         \
            '# (apply from a text file, in case of a lot of censoring)\n' \
            '1d_tool.py -infile %s \\\n'                                  \
            '           -show_trs_uncensored space \\\n'                  \
            '           > %s\n'                                           \
            'set ktrs = "1dcat %s"\n\n'                                   \
            % (proc.censor_file, ktr_text, ktr_text)
       proc.keep_trs = '"[$ktrs]"'

    else:
       cs = ''

    return cs


# get radius for anaticor (fast or slow)
def get_anaticor_radius(proc, block):

   # make 30 mm the default in either case, from 45 for slow  [22 May 2019]
   rad = 30.0   

   # default option is radius
   ofwhm = '-regress_anaticor_fwhm'
   orad  = '-regress_anaticor_radius'
   if block.opts.find_opt(ofwhm):
      oname = ofwhm
   else:
      oname = orad

   val, err = block.opts.get_type_opt(float, oname, default=rad)
   if err:
      print('** error: option %s requires float argument' % oname)
      return -1.0

   # doing away with fwhm, convert to radius
   if oname == ofwhm:
      val /= 2.0

   return val

# compute temporal signal to noise after the regression
def db_cmd_regress_rsfc(proc, block):
    """execute 3dRSFC per run on errts data
          mkdir RSFC
          set b0 = 0
          set b1 = -1
          # run index is 1 digit (wary of octal), run part of file name is 2
          foreach rind ( `count_afni -digits 1 1 $#runs` )
             reps = $tr_counts[$rind]
             @ b1 += $reps
             3dRSFC -prefix RSFC/run_$runs[$rind] -nodetrend MASK \
                    BAND0 BAND1 ERRTS+VIEW'[$b0..$b1]'
             @ b0 += $reps
          end
    """

    if not proc.errts_pre: return 0, ''

    mopt = '-regress_censor_motion'
    ropt = '-regress_RSFC'
    if block.opts.find_opt(mopt):
       print('** %s is (currently) illegal with %s' % (mopt, ropt))
       return 1, ''

    if proc.surf_anat:
       print("** RSFC not yet ready for surface data, please pester rick")
       return 1, ''

    # must have exactly 2 bands
    if len(proc.bandpass) < 2:
       print("** RSFC: missing -regress_bandpass option")
       return 1, ''
    elif len(proc.bandpass) > 2:
       print("** RSFC: -regress_bandpass must have exactly 2 frequencies")
       return 1, ''

    if proc.mask and proc.regmask:
        mstr = '           -mask %s' % proc.mask.shortinput()
    else: mstr = ''

    rcmd = '    3dRSFC -prefix RSFC/run_$runs[$rind] -nodetrend\\\n' \
           '%s'                                                      \
           '           %g %g %s%s"[$b0..$b1]"\n'                     \
           % (mstr, proc.bandpass[0], proc.bandpass[1],
              proc.errts_pre, proc.view)

    proc.errts_pre = 'RSFC_LFF_rall_$subj'

    cmd = '# --------------------------------------------------\n'          \
          '# for each run, bandpass with 3dRSFC and calculate parameters\n' \
          'mkdir RSFC\n'                                                    \
          '\n'                                                              \
          'set b0 = 0\n'                                                    \
          'set b1 = -1\n'                                                   \
          '# run index is 1 digit (wary of octal), file name run is 2\n'    \
          'foreach rind ( `count_afni -digits 1 1 $#runs` )\n'                  \
          '    set reps = $tr_counts[$rind]\n'                              \
          '    @ b1 += $reps\n'                                             \
          '%s'                                                              \
          '    @ b0 += $reps\n'                                             \
          'end\n\n'                                                         \
          '# copy main LFF results out, catenated back into one dataset\n'  \
          '# (this basically replaces the input errts dataset)\n'           \
          '3dTcat -prefix %s RSFC/run*_LFF%s.HEAD\n\n'                      \
          % (rcmd, proc.errts_pre, proc.view)

    return 0, cmd

# compute temporal signal to noise after the regression
def db_cmd_regress_tsnr(proc, block, all_runs, errts_pre):
    if not all_runs or not errts_pre: return ''

    # changing default to no mask, prob want to add -regress_mask_tsnr
    # (requested by P Taylor)                              22 Feb 2021
    mask_pre = ''
    if block.opts.have_yes_opt('-regress_mask_tsnr', default=0):
       # also, no mask for surface based analysis
       if proc.mask and not proc.surf_anat:
          mask_pre = proc.mask.prefix

    tsnr_cmd = db_cmd_tsnr(proc,
           '# --------------------------------------------------\n' \
           "# create a temporal signal to noise ratio dataset \n"   \
           "#    signal: if 'scale' block, mean should be 100\n"    \
           "#    noise : compute standard deviation of errts\n",
           all_runs, errts_pre, proc.view, mask=mask_pre)

    # -----------------------------------------------------------------
    # if the user has not requested TSNR stats for the automatic ROIs,
    # but we have and can compute them, do so
    # (if we have an 'auto' list and a tsnr_dset)
    if proc.regress_auto_tsnr_rois is not None and proc.tsnr_dset is not None:
       # try to add anything not already given with compute_tsnr_stats opts
       oname = '-%s_compute_tsnr_stats' % block.label
       dlablist = [opt.parlist[0] for opt in block.opts.find_all_opts(oname)]

       # make a new list where we might remove some
       new_auto_rois = []
       for blabel in proc.regress_auto_tsnr_rois:
           # if already there, the user wants it, so exclude from auto list
           if blabel in dlablist:
              continue

           # if we do not have the label, exclude from auto list
           if not proc.have_roi_label(blabel):
              continue

           if proc.verb > 1:
              print("-- will also compute regress TSNR stats across '%s'" \
                    % blabel)

           # include the label and add a compute option
           # (use ALL_LT for non-brain auto ROIs)

           if blabel == 'brain':
              ltval = '1'
           else:
              ltval = 'ALL_LT'

           new_auto_rois.append(blabel)
           block.opts.add_opt(oname, 2, [blabel, ltval], setpar=1)

       # apply the updated list
       proc.regress_auto_tsnr_rois = new_auto_rois

    # -----------------------------------------------------------------
    # compute TSNR stats across all requested ROIs
    if block.opts.find_opt('-regress_compute_tsnr_stats'):
       # given ROIs and new proc.tsnr_dset
       rv, scmd = db_cmd_compute_tsnr_stats(proc, block)
       if rv:
          return 1, ''
       tsnr_cmd += '\n' + scmd

    return 0, tsnr_cmd

# compute temporal signal to noise after the regression
def db_cmd_compute_tsnr_stats(proc, block):
    """create a string for computing statistics on ROIs across the TSNR stats
       dataset
    """

    # make a list of ROI dset labels to work with
    oname = '-%s_compute_tsnr_stats' % block.label
    olist = block.opts.find_all_opts(oname)
    if proc.verb > 2:
       print("-- db_cmd_compute_tsnr_stats: have %s %s option(s)" \
             % (len(olist), oname))

    # note which labels we plan to apply
    dlablist = [opt.parlist[0] for opt in olist]

    # if nothing to do, bail
    if len(olist) == 0:
       return 0, ''

    # warn user of impending doom
    if proc.verb > 0:
       print("++ will compute %s TSNR stats for dsets: %s" \
             % (block.label, ', '.join(dlablist)))

    # if we don't have a tsnr_dset for some reason, panic into error
    if proc.tsnr_dset is None:
       print("** cannot compute_tsnr_stats without TSNR dset")
       return 1, ''

    # prep header, output directory and input dataset (-dset_data)
    cmd = '# --------------------------------------------------\n' \
          '# compute TSNR stats for dset labels: %s\n'             \
          % (', '.join(dlablist))
    if len(dlablist) > 1:
       cmd += '\n'

    outdir = 'tsnr_stats_%s' % block.label
    dset_data = proc.tsnr_dset.shortinput()

    # resample the given datasets
    for opt in olist:
       if len(opt.parlist) < 2:
          print("** %s %s : missing ROI values" % (oname, label))
          return 1, ''
       label = opt.parlist[0]
       aname = proc.get_roi_dset(label)
       if not aname:
          print("** compute_tsnr: missing ROI dset '%s'" % label)
          return 1, ''

       # do not let a value of 0 through
       if '0' in opt.parlist[1:]:
          print("** compute_tsnr: an ROI value of zero is illegal")
          print("   opt: %s %s" % (oname, ' '.join(opt.parlist)))
          return 1, ''

       # let the name vary based on whether it is user requested or auto
       tlab = 'user'
       if proc.regress_auto_tsnr_rois is not None:
          if label in proc.regress_auto_tsnr_rois:
             tlab = 'auto'

       stats_file = '%s/stats_%s_%s.txt' % (outdir, tlab, label)

       cmd += "compute_ROI_stats.tcsh \\\n"  \
              "    -out_dir    %s \\\n"      \
              "    -stats_file %s \\\n"      \
              "    -dset_ROI   %s \\\n"      \
              "    -dset_data  %s \\\n"      \
              "    -rset_label %s \\\n"      \
              "    -rval_list  %s\n\n"       \
              % (outdir, stats_file, aname.shortinput(),
                 proc.tsnr_dset.shortinput(),
                 label, ' '.join(opt.parlist[1:]))

    return 0, cmd

# compute temporal signal to noise after the regression
def db_cmd_tsnr(proc, comment, signal, noise, view,
                        mask='', name_qual='', detrend=0, oksurf=1):
    """return a string for computing temporal signal to noise
         comment:   leading comment string
         signal:    prefix for mean dset
         noise:     prefix for stdev dset
         view:      dset view
         mask:      (optional) prefix for mask dset
         name_qual: (optional) qualifier for name, such as '.r01'
         detrend:   (optional) if > 0,
         oksurf:    (optional) flag to allow surf analysis

         This can currently be run for either the volreg or regress blocks.
    """
    if not signal or not noise or not view:
        print('** compute TSNR: missing input')
        return None

    if mask:
       cstr = '\\\n       -c %s%s ' % (mask, view)
       estr = 'c*a/b'
    else:
       cstr = ''
       estr = 'a/b'

    if proc.surf_anat and oksurf:
        feh_str = 'foreach %s ( %s )\n' \
                  % (proc.surf_spec_var_iter, ' '.join(proc.surf_hemilist))
        feh_end = 'end\n'
        suff    = '.%s.niml.dset' % proc.surf_svi_ref
        vsuff   = '' # should be passed in
        istr    = ' '*4
    else:
        feh_str = ''
        feh_end = ''
        suff    = ''
        vsuff   = proc.view
        istr    = ''

    dname = 'TSNR%s%s$subj%s' % (name_qual, proc.sep_char, suff)
    # for now, only store this in volume mode - with $subj
    if not proc.surf_anat or not oksurf:
        proc.tsnr_dset = gen_afni_name(dname+suff, view=vsuff)
        if proc.verb > 1:
           print("++ TSNR dset: %s" % proc.tsnr_dset.shortinput(head=1))
    else:
        # clear any old dset, if the circumstance arises
        proc.tsnr_dset = None

    if detrend:
        polort=proc.regress_polort
        detcmd="%s3dDetrend -polort %d -prefix rm.noise.det%s " \
               "-overwrite %s%s\n"\
               % (istr, polort, suff, noise, vsuff)
        noise = 'rm.noise.det%s' % suff
    else: detcmd = ''

    if name_qual == '': suff = '.all%s' % suff
    else:               suff = name_qual + suff

    cmd  = comment + feh_str
    cmd += "%s3dTstat -mean -prefix rm.signal%s %s%s%s\n"       \
           "%s"                                                 \
           "%s3dTstat -stdev -prefix rm.noise%s %s%s%s\n"       \
           % (istr, suff, signal, vsuff, proc.keep_trs, detcmd,
              istr, suff, noise,  vsuff, proc.keep_trs)

    cmd += "%s3dcalc -a rm.signal%s%s \\\n"     \
           "%s       -b rm.noise%s%s %s \\\n"   \
           "%s       -expr '%s' -prefix %s\n"   \
           % (istr, suff, vsuff,
              istr, suff, vsuff, cstr,
              istr, estr, dname)

    proc.have_rm = 1
    cmd += '%s\n' % feh_end     # add final newline

    return cmd

# might estimate blur from either all_runs or errts (3dD or REML)
# need mask (from mask, or automask from all_runs)
# requires 'all_runs' dataset, in case a mask is needed
#
# return None to fail out
def db_cmd_blur_est(proc, block):
    cmd = ''
    aopt = block.opts.find_opt('-regress_est_blur_epits')
    eopt = block.opts.find_opt('-regress_est_blur_errts')
    ropt = block.opts.find_opt('-regress_reml_exec')
    sopt = block.opts.find_opt('-regress_3dD_stop')

    # allow use to turn this off    22 Jan 2019
    detrend = block.opts.have_yes_opt('-regress_est_blur_detrend', default=1)

    if not aopt and not eopt:
        if proc.verb > 0: print('-- no 3dClustSim (since no blur estimation)')
        return cmd

    if proc.surf_anat:
        print('** this blur estimation is volumetric, and is not appropriate\n'\
              '   for surface-based analysis\n'                                \
              '   (in surface analysis, a blur level is set, not added,\n'     \
              '    so the estimation is the applied level)')
        return

    # set the mask (if we don't have one, bail)
    if not proc.mask:
        print('** refusing to estimate blur without a mask dataset')
        print('   (masks are not applied without -regress_apply_mask)')
        return

    if proc.verb > 1: print('++ computing blur estimates')
    blur_file = 'blur_est.$subj.1D'
    mask_dset = '%s%s' % (proc.mask.prefix, proc.view)

    # call this a new sub-block
    cmd = cmd + '# %s\n'                                \
                '# compute blur estimates\n'            \
                'touch %s   # start with empty file\n\n'\
                % (block_header('blur estimation'), blur_file)

    if proc.ACFdir != '':
       cmd += '# create directory for ACF curve files\n' \
              'mkdir %s\n\n' % proc.ACFdir

    olist, rv = block.opts.get_string_list(opt_name='-regress_opts_fwhmx')
    if olist != None: ex_opts = ' '.join(olist)
    else:             ex_opts = ''

    if aopt:
        bstr = blur_est_loop_str(proc,
                  'all_runs%s$subj%s' % (proc.sep_char, proc.view), mask_dset,
                  'epits', blur_file, detrend=detrend, ex_opts=ex_opts)
        if not bstr: return
        cmd = cmd + bstr

    if eopt and not sopt: # want errts, and 3dD was not stopped
        bstr = blur_est_loop_str(proc, '%s%s' % (proc.errts_pre, proc.view),
                    mask_dset, 'errts', blur_file, proc.errts_cen,
                    detrend=detrend, ex_opts=ex_opts)
        if not bstr: return
        cmd = cmd + bstr
    if eopt and ropt and proc.errts_reml: # want errts and reml was executed
        # cannot use ${}, so escape the '_'
        bstr = blur_est_loop_str(proc, '%s%s' % (proc.errts_reml, proc.view),
                    mask_dset, 'err_reml', blur_file, detrend=detrend,
                    ex_opts=ex_opts)
        if not bstr: return
        cmd = cmd + bstr

    cmd = cmd + '\n'

    # maybe make string to run and apply 3dClustSim
    # note: the new default uses ACF rather than the old FWHM    15 Aug, 2016
    # clustsim_types = ['FWHM', 'ACF', 'both', 'yes', 'no']
    copt,rv = block.opts.get_string_opt('-regress_run_clustsim', default='yes')
    if rv: return
    # no longer warn about ACF on default [15 Feb 2024]
    # if copt == 'yes':
    #    print('** the default 3dClustSim method has changed from FWHM to ACF')
    #    print("   (to get FWHM, use '-regress_run_clustsim FWHM')")

    if copt == 'FWHM':   cmethods = [copt]
    elif copt == 'ACF':  cmethods = [copt]
    elif copt == 'both': cmethods = ['FWHM', 'ACF'] # put ACF last
    elif copt == 'yes':  cmethods = ['ACF']
    elif copt == 'no':   cmethods = []
    else:
      print('** invalid -regress_run_clustsim param, %s' % copt)
      print('   should be one of: %s' % ', '.join(clustsim_types))

    if len(cmethods) > 0:
      statsets = []
      if proc.have_3dd_stats: statsets.append('stats.$subj%s' % proc.view)
      if proc.have_reml_stats:statsets.append('stats.${subj}_REML%s'%proc.view)

      if proc.have_3dd_stats or proc.have_reml_stats:
         rv, bstr = make_clustsim_commands(proc, block, cmethods, blur_file,
                                           mask_dset, statsets)
         if rv: return   # failure (error has been printed)
         cmd = cmd + bstr + '\n'
      else:
         print('-- skipping 3dClustSim (no stats dsets to apply to)')

    return cmd

def make_clustsim_commands(proc, block, cmethods, blur_file, mask_dset,
                           statsets):
    if proc.verb > 0: print('-- will add 3dClustSim table to stats dset')
    if proc.verb > 1:
        print('-- make_clustsim_commands: blur = %s, methods = %s\n'  \
              '   mask = %s, stat sets = %s'                          \
              % (blur_file,', '.join(cmethods), mask_dset, ', '.join(statsets)))

    opt = block.opts.find_opt('-regress_opts_CS')
    optstr = ''
    if opt:
        if len(opt.parlist) > 0 :
           optstr = '           %s \\\n' % ' '.join(opt.parlist)

    cstr = '# add 3dClustSim results as attributes to any stats dset\n'
    if proc.CSdir != '': cstr += 'mkdir %s\n\n' % proc.CSdir

    # run cluster method(s)
    for cmeth in cmethods:
       # prefix for 3dClustSim files might be in a sub-directory
       cprefix = 'ClustSim.%s' % cmeth
       if proc.CSdir != '': cprefix = '%s/%s' % (proc.CSdir, cprefix)

       cscmdfile = '3dClustSim.%s.cmd' % cmeth
       if cmeth == 'FWHM': copt = '-fwhmxyz'
       else              : copt = '-acf'
       cstr += "# run Monte Carlo simulations using method '%s'\n"          \
               'set params = ( `grep %s %s | tail -n 1` )\n'                \
               '3dClustSim -both -mask %s %s $params[1-3] \\\n'             \
               '%s'                                                         \
               '           -cmd %s -prefix %s\n\n'                          \
               % (cmeth,cmeth, blur_file, mask_dset, copt, optstr,
                  cscmdfile,cprefix)

    # the 3drefit command is now stored in 3dClustSim.cmd
    cstr += '# run 3drefit to attach 3dClustSim results to stats\n' \
            'set cmd = ( `cat %s` )\n'                              \
            '$cmd %s\n\n' % (cscmdfile, ' '.join(statsets))

    return 0, cstr

def blur_est_loop_str(proc, dname, mname, label, outfile, trs_cen=0,
                      detrend=1, ex_opts=''):
    """return tcsh command string to compute blur from this dset
        proc     : afni_proc SubjProcStream (for reps or reps_all)
        dname    : dataset name to estimate blur on
        mname    : mask dataset name
        label    : text label for comments
        outfile  : final output filename
        trs_cen  : were censored TRs removed from this data?
    """
    dset  = gen_afni_name(dname)
    inset = dset.shortinput()
    inset = dname
    mset  = gen_afni_name(mname)
    mask  = mset.shortinput()
    tmpfile = 'blur.%s.1D' % label

    if not inset:
        print("** failed to get blur_est input name from '%s'" % dname)
        return ''
    if not mask:
        print("** failed to get mask input name from '%s'" % mname)
        return ''

    cmd = '# -- estimate blur for each run in %s --\n'          \
          'touch %s\n\n' % (label, tmpfile)

    if trs_cen:
       tstr1 = ''
       tstr2 = ''
    else:
       tstr1 = '    set trs = `1d_tool.py -infile %s '                  \
               '-show_trs_uncensored encoded \\\n'                      \
               '                          -show_trs_run $run`\n'        \
               '    if ( $trs == "" ) continue\n' % proc.xmat
       tstr2 = '"[$trs]"'

    # might put ACF curves in sub-dir
    acffile = 'out.3dFWHMx.ACF.%s.r$run.1D' % label
    if proc.ACFdir != '': acffile = '%s/%s' % (proc.ACFdir, acffile)

    # detrend?
    if detrend: detstr = '-detrend '
    else:       detstr = ''

    # any extra opts?
    if ex_opts != '': sextra = '            %s \\\n' % ex_opts
    else:             sextra = ''

    cmd = cmd +                                                 \
      '# restrict to uncensored TRs, per run\n'                 \
      'foreach run ( $runs )\n'                                 \
      '%s'                                                      \
      '    3dFWHMx %s-mask %s \\\n'                             \
      '            -ACF %s \\\n'                                \
      '%s'                                                      \
      '            %s%s >> %s\n'                                \
      'end\n\n'                                                 \
      % (tstr1, detstr, mask, acffile, sextra, inset, tstr2, tmpfile)

    btypes = ['FWHM', 'ACF']
    for bind, btype in enumerate(btypes):
       blur_str = "3dTstat -mean -prefix - %s'{%d..$(2)}'\\\'" % \
                  (tmpfile, bind)

       cmd = cmd +                                                         \
           '# compute average %s blur (from every other row) and append\n' \
           'set blurs = ( `%s` )\n'                                        \
           'echo average %s %s blurs: $blurs\n'                            \
           'echo "$blurs   # %s %s blur estimates" >> %s\n\n' %            \
           (btype, blur_str, label, btype, label, btype, outfile)

    return cmd

# convert a stim_files list into a stim_times list
def db_cmd_regress_sfiles2times(proc, block):

    # check for a stimulus timing offset
    opt = block.opts.find_opt('-regress_stim_times_offset')
    if opt and opt.parlist and opt.parlist[0] != 0:
        off_cmd = '                   -offset %s \\\n' % str(opt.parlist[0])
    else: off_cmd = ''

    cmd = ''
    if proc.verb > 0: print('-- old stim list: %s' % proc.stims)

    cmd = cmd + '\n# create -stim_times files\n'
    cmd = cmd + 'make_stim_times.py -prefix stim_times -tr %s -nruns %d' \
                ' -nt %d \\\n'                                           \
                '%s'                                                     \
                '                   -files '                             \
                % (str(proc.tr), proc.runs, proc.reps,off_cmd)
    cols = 0
    for file in proc.stims_orig:
        cmd = cmd + 'stimuli/%s ' % os.path.basename(file)      # add filename
        cols += UTIL.num_cols_1D(file)         # tally the number of cols
    cmd = cmd + '\n\n'

    # and reset proc.stims to the new file list (which is 1-based)
    proc.stims = []
    for ind in range(1,cols+1):
        proc.stims.append('stimuli/stim_times.%02d.1D' % ind)

    if proc.verb > 0: print('++ new stim list: %s' % proc.stims)

    return cmd

# from -regress_ROI_PC/maskave
def db_cmd_regress_pc_followers(proc, block):
    """regress principal components from follower datasets
       return an error code (0=success) and command string
    """

    oname = '-regress_ROI_PC'

    # make a list of [LABEL, NPC]
    roipcs = []
    roipclabs = []
    for opt in proc.user_opts.find_all_opts(oname):
       label = opt.parlist[0]
       npc   = opt.parlist[1]
       try : numpc = int(npc)
       except:
          print('** -regress_ROI_PC %s %s: bad NUM_PC = %s' % (label, npc, npc))
          return 1, ''
       if numpc < 0:
          print('** -regress_ROI_PC %s %s: bad NUM_PC = %d' % (label, numpc))
          return 1, ''
       # okay, append to the list
       roipcs.append([label, numpc])
       roipclabs.append(label)

    if len(roipcs) == 0: return 0, ''

    roinames = ', '.join(roipclabs)
    clist = ['# ------------------------------\n']
    clist.append('# create ROI PC ort sets: %s\n' % roinames)

    # note any per_run labels
    oname = '-regress_ROI_PC_per_run'
    per_run_rois, rv = block.opts.get_string_list(oname)
    if not per_run_rois:
       per_run_rois = [] # be sure it is a list
    for roi in per_run_rois:
       if not roi in roipclabs:
          print("** PC per_run ROI '%s' not in ROI list: %s" \
             % (roi, ', '.join(roipclabs)))
          return 1, ''

    # make across run regressors?  per-run regressors?
    doacross = 0
    doperrun = (len(per_run_rois) > 0)
    for roi in roipclabs:
       if not roi in per_run_rois:
          doacross = 1
          break

    # if censoring, censor each run with -cenmode KILL
    if proc.censor_file:
       censor_file = 'rm.censor.r$run.1D'
       cmd_censor = \
          '    # to censor, create per-run censor files\n'                    \
          '    1d_tool.py -set_run_lengths $tr_counts -select_runs $run \\\n' \
          '               -infile %s -write %s\n\n'                           \
          '    # do not let censored time points affect detrending\n'         \
          % (proc.censor_file, censor_file)
       opt_censor = '               -censor %s -cenmode KILL \\\n'%censor_file
    else:
       censor_file = ''
       cmd_censor = ''
       opt_censor = ''


    # rcr - PONDER

    # use combine or volreg block, whichever block comes last
    # (i.e. require both, if they exist)
    blabel = proc.find_latest_block(['combine', 'volreg'])
    vblock = proc.find_block_or_prev(blabel, block)
    blabel = vblock.label   # if neither exists, will use current block
    if vblock == None:
       print('** ROI_PC: failed to find corresponding %s block' % blabel)
       return 1, ''

    # this is from volreg block, so it should never have surf names
    vr_prefix = proc.prefix_form_run(vblock, surf_names=0)

    proc.vr_vall_lab = blabel


    # if there is no volreg prefix, get a more recent one
    #vr_prefix = proc.volreg_prefix
    #if not vr_prefix:
    #   vblock = proc.find_block_or_prev('volreg', block)
    #   vr_prefix = proc.prefix_form_run(vblock)

    tpre = 'rm.det_pcin'
    clist.append(                                                \
       '\n# create a time series dataset to run 3dpc on...\n\n'  \
       '# detrend, so principal components are not affected\n'   \
       'foreach run ( $runs )\n'                                 \
       '%s'                                                      \
       '    3dTproject -polort %d -prefix %s_r$run \\\n'         \
       '%s'                                                      \
       '               -input %s%s\n'                            \
       % (cmd_censor, proc.regress_polort, tpre, opt_censor,
          vr_prefix, proc.view) )

    if doperrun:
       rv, cnew = regress_pc_followers_regressors(proc, oname, roipcs,
                      tpre+'_r$run', censor_file=censor_file,
                      perrun=True, per_run_rois=per_run_rois)
       if rv: return 1, ''
       clist.extend(cnew)

    # finish 'foreach run loop, after any per-run regressors
    clist.append('end\n\n')

    # will be censor and uncensor
    if proc.censor_file: c1str = ', prepare to censor TRs'
    else:                c1str = ''

    if doacross:
       clist.append('# catenate runs%s\n' % c1str)
       clist.append('3dTcat -prefix %s_rall %s_r*%s.HEAD\n\n' \
                    % (tpre,tpre,proc.view) )
       rv, cnew = regress_pc_followers_regressors(proc, oname, roipcs,
                      tpre+'_rall', censor_file=proc.censor_file,
                      perrun=False, per_run_rois=per_run_rois)
       if rv: return 1, ''
       clist.extend(cnew)

    print('-- have %d PC ROIs to regress: %s' % (len(roipcs), roinames))

    return 0, ''.join(clist)


def regress_pc_followers_regressors(proc, optname, roipcs, pcdset,
        censor_file='', perrun=False, per_run_rois=[]):
   """return list of commands for 3dpc, either per run or across them
      if per_run_rois:
         if perrun: only do ROIs in per_run_rois
         else:      only do ROIs NOT in per_run_rois
      if perrun ROI:
         - indent by 4 (do it at the end)
         - censor fill per run (1d_tool.py needs current run and all lengths)
   """
   if perrun: indent = '    '
   else:      indent = ''

   clist = []
   for pcind, pcentry in enumerate(roipcs):
      label = pcentry[0]
      num_pc = pcentry[1]
      cname = proc.get_roi_dset(label)
      if cname == None:
         print('** applying %s, failed to get ROI dset for label %s' \
               % (optname, label))
         return 1, clist

      # perrun should agree with (label in per_run_rois)
      if perrun != (label in per_run_rois): continue

      # output prefix ROIPC.LABEL_00.1D ...
      # (store the pclabel and add anything for per run or censoring)
      pclabel = 'ROIPC.%s' % label
      prefix = pclabel
      if perrun: prefix = '%s.r${run}' % prefix
      if perrun or censor_file: prefix = 'rm.%s' % prefix

      if perrun:
         clist.append('\n')
         cstr = '(per run) '
      else:
         cstr = ''

      clist.append('%s# make ROI PCs %s: %s\n'   \
             '%s3dpc -mask %s -pcsave %d \\\n' \
             '%s     -prefix %s %s%s\n'        \
             % (indent, cstr, label,
                indent, cname.shortinput(), num_pc,
                indent, prefix, pcdset, proc.view))
      pcname = '%s_vec.1D' % prefix

      # append pcfiles to orts list
      # (possibly create censor file, first)
      # --- need to do all cases here (cen&pr, cen, pr)
      if censor_file:
         # possibly handle per-run here, too
         if perrun:
            newname = '%s.r$run.1D' % pclabel
            cstr = ' and further pad to fill across all runs'
            cout_name = '-'
         else:
            newname = '%s.1D' % pclabel
            cstr = ''
            cout_name = newname

         cmd = '%s# zero pad censored TRs%s\n'            \
               '%s1d_tool.py -censor_fill_parent %s \\\n' \
               '%s    -infile %s \\\n'                    \
               '%s    -write %s'                          \
            % (indent, cstr, indent, censor_file,
               indent, pcname, indent, cout_name)

         # if per run, pipe this through pad_into_many_runs
         if perrun:
            cmd += ' \\\n%s  | 1d_tool.py -set_run_lengths $tr_counts ' \
                   '-pad_into_many_runs $run %d \\\n'                   \
                   '%s               -infile - -write %s\n'             \
                   % (indent, proc.runs, indent, newname)
            for rind in range(proc.runs):
               newlab = '%s.r%02d' % (pclabel, rind+1)
               proc.regress_orts.append(['%s.1D'%newlab, newlab])
         else:
            cmd += '\n'
            proc.regress_orts.append([newname, pclabel])

         clist.append('\n')
         clist.append(cmd)

      # now just implement pad into many runs
      elif perrun:
         newname = '%s.r$run.1D' % pclabel
         cmd = \
           '%s# zero pad single run to extend across all runs\n'        \
           '%s1d_tool.py -set_run_lengths $tr_counts '                  \
           '-pad_into_many_runs $run %d \\\n'                           \
           '%s    -infile %s -write %s\n'                               \
            % (indent, indent, proc.runs,
               indent, pcname, newname)

         clist.append('\n')
         clist.append(cmd)

         for rind in range(proc.runs):
            newlab = '%s.r%02d' % (pclabel, rind+1)
            proc.regress_orts.append(['%s.1D'%newlab, newlab])

      # otherwise, just add the one PC
      else:
         proc.regress_orts.append([pcname, pclabel])

      if not perrun: clist.append('\n')

   return 0, clist

def db_cmd_resam_ROI_imports(proc, block):
    """resample any -ROI_import datasets (onto the final EPI grid)

       This could be run in the volreg block, for example, to compute
       TSNR stats from.  Or it could be run in the regress block, to compute
       PC ortvec's, or to compute regress TSNR stats.

       Perhaps we must allow this to be run in the tcat block as well...

       return an error code (0=success) and command string
    """
    oname = '-ROI_import'
    olist = proc.user_opts.find_all_opts(oname)
    # is there anything to do?
    if len(olist) == 0:
       return 0, ''

    # does anything need resampling?
    do_some = 0
    for opt in proc.user_opts.find_all_opts(oname):
       label = opt.parlist[0]
       aname = proc.get_roi_dset(label)
       if not aname:
          print("** missing -ROI_import ROI '%s'" % label)
          return 1, ''
       if aname.to_resam:
          do_some = 1

    # if nothing to resample, we are outta here...
    if not do_some:
       return 0, ''

    # get to work

    # note the final vr_base dset, and be sure one has been made
    vbase = proc.epi_final
    if vbase is None:
       vbase = proc.vr_base_dset

    # ------------------------------------------------------------
    # if no volreg block, just be sure ROIs have correct view
    if vbase is None:
       if proc.verb > 1:
           print("-- no vr_base_dset to resample ROIs, assuming on final grid")

       # be sure the view is current
       for opt in proc.user_opts.find_all_opts(oname):
          label = opt.parlist[0]
          aname = proc.get_roi_dset(label)
          if not aname.to_resam:
             continue
          aname = proc.roi_dict[label]
          aname.view = proc.view

       return 0, ''

    cmd = '# resample any -ROI_import dataset onto the EPI grid\n' \
          '# (and copy its labeltable)\n\n'

    # resample the given datasets
    for opt in proc.user_opts.find_all_opts(oname):
       label = opt.parlist[0]
       aname = proc.get_roi_dset(label)
       if not aname:
          print("** missing -ROI_import ROI '%s'" % label)
          return 1, ''

       # if we do not need to resample this dataset, skip it
       if not aname.to_resam:
          continue

       aname = proc.roi_dict[label]
       aname.view = proc.view

       # append _resam to prefix, but note current input name
       inname = aname.shortinput()
       aname.prefix += '_resam'

       cmd += '3dresample -master %s -rmode NN \\\n' \
              '           -input %s \\\n'  \
              '           -prefix %s\n\n'  \
              % (vbase.nice_input(head=0), inname, aname.out_prefix())

       cmd += '3drefit -copytables %s %s\n'   \
              '3drefit -cmap INT_CMAP %s\n\n' \
              % (inname, aname.shortinput(), aname.shortinput())

       # mark as resampled
       aname.to_resam = 0

    return 0, cmd

def db_cmd_regress_ROI(proc, block):
    """remove any regressors of no interest

        ** use orts for now, but change to simple regressors
        ** just do global signal for now

       return an error code (0=success) and command string
    """

    # maybe we shouldn't be here
    oname = '-regress_ROI'
    rois = []
    for opt in block.opts.find_all_opts(oname):
       rois.extend(opt.parlist)
    if len(rois) == 0:
       print('** have %s but no ROIs provided' % oname)
       return 1, ''

    # note any per_run labels
    oname = '-regress_ROI_per_run'
    per_run_rois, rv = block.opts.get_string_list(oname)
    if not per_run_rois:
       per_run_rois = [] # be sure it is a list
    for roi in per_run_rois:
       if not roi in rois:
          print("** per_run ROI '%s' not in ROI list: %s"%(roi,', '.join(rois)))
          return 1, ''

    # report errors for any unknown ROIs (not in roi_dict)
    keystr = ', '.join(list(proc.roi_dict.keys()))
    nerrs = 0
    segstr = 'requires -mask_segment_anat'
    erdstr = 'requires -mask_segment_anat and -mask_segment_erode'
    for roi in rois:
        if not proc.have_roi_label(roi):
            if   roi == 'brain': estr='EPI automask requires mask block'
            elif roi == 'GM'   : estr='gray matter %s'  % segstr
            elif roi == 'WM'   : estr='white matter %s' % segstr
            elif roi == 'CSF'  : estr='CSF %s'          % segstr
            elif roi == 'GMe'  : estr='eroded gray %s'  % segstr
            elif roi == 'WMe'  : estr='white matter %s' % segstr
            elif roi == 'CSFe' : estr='CSF %s'          % segstr
            else               : estr = 'not a known ROI'
            print("** ROI '%s' : %s" % (roi, estr))
            nerrs += 1
    if nerrs:
        if keystr == '': keystr = 'NONE'
        print('-- currently known ROIs include: %s' % keystr)
        return 1, ''

    if len(rois) > 1:
       cmd = '# ------------------------------\n' \
             '# create %d ROI regressors: %s\n' % (len(rois),', '.join(rois))
    else: cmd = '# create ROI regressor: %s\n' % rois[0]
    cmd += '# (get each ROI average time series and remove resulting mean)\n'

    # if there is no volreg prefix, get a more recent one
    vr_prefix = proc.volreg_prefix
    if not vr_prefix:
       # rcr - todo combine
       vblock = proc.find_block_or_prev('volreg', block)
       vr_prefix = proc.prefix_form_run(vblock)

    cmd += 'foreach run ( $runs )\n'
    doacross = 0
    for roi in rois:
        mset = proc.get_roi_dset(roi)
        # if mset == None:
        if not isinstance(mset, BASE.afni_name):
           print("** regress_ROI: missing ROI dset for '%s'" % roi)
           return 1, ''

        per_run = (roi in per_run_rois)

        # -- no more label table, masks are now unit            22 Apr 2013
        # maybe we need a label table value selector
        # if roi in ['GM', 'WM', 'CSF']: substr = '"<%s>"' % roi
        # else:                          substr = ''

        # if per run, output goes to stdout before appending pipe
        if not per_run:
           ofile = 'rm.ROI.%s.r$run.1D' % roi
           cpr = ''
           doacross = 1 # catenate across runs
        else:
           ofile = 'ROI.%s.r$run.1D' % roi
           spaces = ' '*16
           cpr = '\\\n %s -set_run_lengths $tr_counts ' \
                 '-pad_into_many_runs $run %d'  \
                 % (spaces, proc.runs)

        if per_run:
           cstr = '    # per-run ROI averages: zero-pad across all runs\n'
        else:
           cstr = ''
        cmd += '%s'                                                       \
               '    3dmaskave -quiet -mask %s \\\n'                       \
               '              %s%s \\\n'                                  \
               '            | 1d_tool.py -infile - -demean -write %s%s\n' \
               % (cstr, mset.pv(), vr_prefix, proc.view, ofile, cpr)
    cmd += 'end\n'

    if doacross:
       cmd += '# and catenate the demeaned ROI averages across runs\n'
    for roi in rois:
        if roi in per_run_rois:
           for run in range(proc.runs):
              rname = 'ROI.%s.r%02d' % (roi, run+1)
              rfile = '%s.1D' % rname
              proc.regress_orts.append([rfile, rname])
           continue
        else:
           rname = 'ROI.%s' % roi
           rfile = '%s_rall.1D' % rname
           cmd += 'cat rm.%s.r*.1D > %s\n' % (rname, rfile)
           proc.regress_orts.append([rfile, rname])
    cmd += '\n'

    proc.have_rm = 1
    print('-- have %d ROIs to regress: %s' % (len(rois), ', '.join(rois)))

    return 0, cmd

def db_cmd_regress_bandpass(proc, block):
    """apply bandpass filtering in 3dDeconvolve

         - create bandpass files per run
         - 1dcat them
         - note resulting file to use as regress_ortvec [file, label] pair

       to be dangerous, make the code differ in complexity:
         - if 1 run ==> 1 command
         - else if not proc.reps_vary: small foreach loop
         - else, more complicated loop

       return an error code (0=success) and command string
    """

    # maybe we shouldn't be here
    oname = '-regress_bandpass'
    opt = block.opts.find_opt(oname)
    if not opt: return 0, ''

    # otherwise, note the frequency band limits
    proc.bandpass, err = block.opts.get_type_list(float, opt=opt)
    freq = proc.bandpass # for ease
    nfpairs = len(freq) // 2
    if len(freq) < 2 or (len(freq) % 2) == 1:
        print('** %s requires 2+ parameters, low and high freq pairs' % oname)
        return 1, ''
    # generate -band options
    bpopts = []
    for bind in range(nfpairs):
       bbase = 2*bind
       if freq[bbase] >= freq[bbase+1]:
           print('** %s: must have low freq < high freq' % oname)
           return 1, ''
       bpopts.append('-band %g %g' % (freq[bbase], freq[bbase+1]))
    bpopt_str = ' '.join(bpopts)

    # if doing RSFC, then do not bandpass in 3dDeconvolve
    # (so we are done here)
    if block.opts.find_opt('-regress_RSFC'):
       if proc.verb > 0:
          print('-- have -regress_RSFC, bandpassing will be done in 3dRSFC')
       return 0, ''

    bfile = 'bandpass_rall.1D'
    tfile = 'rm.bpass.1D'

    cmd = '# create bandpass regressors (instead of using 3dBandpass, say)\n'
    if proc.runs != 1:
        cmd += '# (make separate regressors per run, with all in one file)\n'

    if proc.runs == 1: # simple case
        cmd += '1dBport -nodata %s %s %s -invert -nozero' \
               ' > %s\n\n' % (proc.reps, proc.tr, bpopt_str, bfile)
    else: # loop over 1dBport and 1d_tool.py
        cmd += 'foreach index ( `count_afni -digits 1 1 $#runs` )\n'              \
               '    set nt = $tr_counts[$index]\n'                            \
               '    set run = $runs[$index]\n'                                \
               '    1dBport -nodata $nt %g %s -invert -nozero >! %s\n'\
               % (proc.tr, bpopt_str, tfile)
        cmd += '    1d_tool.py -infile %s -pad_into_many_runs $run $#runs \\\n'\
               '               -set_run_lengths $tr_counts \\\n'              \
               '               -write bpass%sr$run.1D\n'                      \
               'end\n' % (tfile, proc.sep_char)
        cmd += '1dcat bpass.r*1D > bandpass_rall.1D\n\n'

    proc.regress_orts.append([bfile, 'bandpass'])

    return 0, cmd

def db_cmd_regress_motion_stuff(proc, block):
    """prepare motion parameters for regression
         - create demean and deriv files
         - set mot_regs (to mot_file and/or mot_deriv)
         - maybe regress per run
         - maybe censor based on mot_file
       return an error code (0=success) and command string
    """

    if block.opts.find_opt('-regress_no_motion'): return 0, ''
    if not proc.find_block('volreg') and \
       not block.opts.find_opt('-regress_motion_file'):
         # fail if censoring was requested
         if block.opts.find_opt('-regress_censor_motion'):
            print('** error: -regress_censor_motion requires volreg ' \
                  'block or motion file')
            return 1, ''
         return 0, ''

    cmd = ''

    # fill proc.mot_regs, as well as the proc.mot_* file names
    err, newcmd = db_cmd_regress_mot_types(proc, block)
    if err: return 1, ''
    if newcmd: cmd += newcmd

    # maybe convert the motion files to motion per run
    # if so, replace mot_regs with the new versions of them
    if block.opts.find_opt('-regress_motion_per_run'):
        pcmd = '# convert motion parameters for per-run regression\n'
        # make an option for the number of runs
        if proc.reps_vary:
           runopt = '-set_run_lengths %s' % UTIL.int_list_string(proc.reps_all)
        else:
           runopt = '-set_nruns %d' % proc.runs
        mfiles = proc.mot_regs
        proc.mot_regs = []
        proc.mot_names = []
        for mfile in mfiles:
            if   mfile == proc.mot_extern: mtype = 'extern'
            elif mfile == proc.mot_demean: mtype = 'demean'
            elif mfile == proc.mot_deriv : mtype = 'deriv'
            else:                          mtype = 'dfile'
            # note the output prefix and expected output file list
            mprefix = 'mot_%s' % mtype
            outfiles = ['%s.r%02d.1D'%(mprefix,r+1) for r in range(proc.runs)]
            names = ['%s_r%02d'%(mprefix,r+1) for r in range(proc.runs)]
            pcmd += '1d_tool.py -infile %s %s \\\n'          \
                    '           -split_into_pad_runs %s\n\n' \
                    % (mfile, runopt, mprefix)
            proc.mot_regs.extend(outfiles)
            proc.mot_names.extend(names)

        cmd += pcmd     # and add to the current command

    # if the user wants to censor large motion, create a censor.1D file
    if block.opts.find_opt('-regress_censor_motion'):
        err, newcmd = db_cmd_regress_censor_motion(proc, block)
        if err: return 1, ''
        if newcmd: cmd = cmd + newcmd

    if cmd != '': return 0, '\n' + cmd
    else: return 0, cmd

def db_cmd_regress_mot_types(proc, block):
    """return an error code (0=success) and a command(?)

       Allow use of (basic or demean) and/or deriv motion parameters.
       Adjust proc.mot_regs based on -regress_apply_mot_types.
       Unless -regress_no_motion_demean/-regress_no_motion_deriv, both
         mean and deriv files should be created, independently of
         whether they are used as regressors.
    """

    cmd = ''
    proc.mot_regs = []

    # note which types to use in regression (cannot be an empty list)
    bopt = block.opts.find_opt('-regress_apply_mot_types')
    if bopt:
       apply_types = bopt.parlist
       # then must allow computation of demean and deriv
       if block.opts.find_opt('-regress_no_motion_demean') or \
          block.opts.find_opt('-regress_no_motion_deriv'):
          print("** must allow 'demean' and 'deriv' computation when using\n" \
                "   option -regress_apply_mot_types")
          return 1, ''
       print('-- will apply motion types: %s' % ', '.join(apply_types))
    elif block.opts.find_opt('-regress_no_motion_demean'):
          apply_types = ['basic']
    else: apply_types = ['demean'] # now the default

    # note whether to use run lengths or number of runs for demean and deriv
    if proc.reps_vary :
       ropt = '-set_run_lengths $tr_counts'
    else: ropt = '-set_nruns %d' % proc.runs

    # handle 3 cases of motion parameters: 'basic', 'demean' and 'deriv'
    # (regardless of possible 'none')

    # 1. update mot_regs for 'basic' case
    if 'basic' in apply_types:
        proc.mot_regs.append(proc.mot_file)
        proc.mot_names.append('mot_%s' % 'basic')

    # 2. possibly compute de-meaned motion params
    if not block.opts.find_opt('-regress_no_motion_demean'):
       mtype = 'demean'
       mopt = 'demean'
       motfile = 'motion_%s.1D' % mtype
       # if requested for regression, add this file
       if mtype in apply_types:
          if 'basic' in apply_types:
             print("** cannot apply both 'basic' and 'demean' motion params")
             print("   (would lead to multi-collinearity)")
             return 1, ''
          proc.mot_regs.append(motfile)
          proc.mot_names.append('mot_%s' % mtype)
          pcmd = '# compute de-meaned motion parameters %s\n' \
                 % '(for use in regression)'
       else:
          pcmd = '# compute de-meaned motion parameters %s\n' \
                 % '(just to have)'

       pcmd += '1d_tool.py -infile %s %s \\\n' \
               '           -%s -write %s\n\n'  \
               % (proc.mot_file, ropt, mopt, motfile)
       proc.mot_demean = motfile
       cmd += pcmd

    # 3. possibly compute motion derivatives
    if not block.opts.find_opt('-regress_no_motion_deriv'):
       mtype = 'deriv'
       mopt = 'derivative'
       motfile = 'motion_%s.1D' % mtype
       # if requested for regression, add this file
       if mtype in apply_types:
          proc.mot_regs.append(motfile)
          proc.mot_names.append('mot_%s' % mtype)
          pcmd = '# compute motion parameter derivatives %s\n' \
                 % '(for use in regression)'
       else:
          pcmd = '# compute motion parameter derivatives %s\n' \
                 % '(just to have)'

       pcmd += '1d_tool.py -infile %s %s \\\n' \
               '           -%s -demean -write %s\n\n'  \
               % (proc.mot_file, ropt, mopt, motfile)
       proc.mot_deriv = motfile
       cmd += pcmd

    return 0, cmd

def db_cmd_regress_censor_motion(proc, block):
    """return a command to create a censor.1D file

       Require a non-negative LIMIT, consistent run length and a single
       motion file.

       As a side effect, set proc.censor_file.

       return an error code (0=success) and command string"""

    # check for a stimulus timing offset
    opt = block.opts.find_opt('-regress_censor_motion')
    if opt and opt.parlist:
        limit = opt.parlist[0]
        if limit < 0.0:
            print('** regress_censor_motion: have negative limit %g' % limit)
            return 1, ''
    else: return 0, ''

    # run lengths may now vary  16 Nov, 2009

    # maybe there is no file to use
    if proc.mot_file == '': return 0, ''

    # check for -regress_censor_first_trs
    val, err = block.opts.get_type_opt(int, '-regress_censor_first_trs')
    if err:
        print('** -regress_censor_first_trs requires integer argument')
        return 1, ''
    elif val != None and val > 0: cfstr = '    -censor_first_trs %d \\\n' % val
    else:                         cfstr = ''

    # check for censor_prev_TR
    opt = block.opts.find_opt('-regress_censor_prev')
    if opt.parlist[0] == 'yes': prev_str = '-censor_prev_TR'
    else:                       prev_str = ''

    if proc.verb > 1:
        print('-- creating motion censor command, file = %s' % proc.mot_file)

    # save string to apply in 3dDeconvolve
    mot_prefix = 'motion_${subj}'
    censor_file = '%s_censor.1D' % mot_prefix
    cmd = '# create censor file %s, for censoring motion \n' \
          % censor_file

    # if we are already censoring, make a command to combine the results
    if proc.censor_file:
        rv, cfs = combine_censor_files(proc, censor_file)
        if rv: return 1, ''
        cfs += '\n'
    else:
        proc.censor_file = censor_file
        proc.censor_count = 1
        cfs = ''

    # make command string to create censor file
    if proc.reps_vary :     # use -set_run_lengths aot -set_nruns
        cmd = cmd + '1d_tool.py -infile %s -set_run_lengths $tr_counts \\\n' \
                    % (proc.mot_file)
    else:
        cmd = cmd + '1d_tool.py -infile %s -set_nruns %d \\\n'       \
                    % (proc.mot_file, proc.runs)

    # remove useless -set_tr option  17 Oct 2012
    cmd = cmd + '    -show_censor_count %s\\\n'                 \
                '%s'                                            \
                '    -censor_motion %g %s\n\n'                  \
                % (prev_str, cfstr, limit, mot_prefix)

    proc.mot_cen_lim = limit
    proc.mot_enorm   = '%s_enorm.1D' % mot_prefix

    if cfs: cmd += cfs

    return 0, cmd

# --------------- tlrc (anat) ---------------

def db_mod_tlrc(block, proc, user_opts):

    # --------------------------------------------------
    # verify that anatomical dataset exists
    opt_anat = user_opts.find_opt('-copy_anat')
    if not opt_anat:
        print('** tlrc (anat) block requires anatomy via -copy_anat')
        return

    dset = gen_afni_name(opt_anat.parlist[0])
    if not dset.exist():  # allow for no +view
        dset = gen_afni_name(opt_anat.parlist[0]+'+orig')
        if not dset.exist():
            print("** -copy_anat dataset '%s' does not exist" % \
                  opt_anat.parlist[0])
            return

    # --------------------------------------------------
    # handle the template dataset: verify existence, etc

    # check before template init: if -tlrc_NL_warped_dsets, require -tlrc_base
    if user_opts.find_opt('-tlrc_NL_warped_dsets') and \
       not user_opts.find_opt('-tlrc_base'):
       print("** error: -tlrc_NL_warped_dsets requires option -tlrc_base")
       print("   (please verify which template was used to make warped_dsets)")
       return

    # set template
    oname = '-tlrc_base'
    if user_opts.find_opt(oname):
        apply_uopt_to_block('-tlrc_base', user_opts, block)
    else:
        block.opts.add_opt('-tlrc_base', 1, ['TT_N27+tlrc'], setpar=1)

    prepare_tlrc_base(proc)

    # --------------------------------------------------
    # add other options

    apply_uopt_to_block('-tlrc_opts_at', user_opts, block)
    apply_uopt_to_block('-tlrc_NL_warp', user_opts, block)
    apply_uopt_to_block('-tlrc_affine_warped_dsets', user_opts, block)
    apply_uopt_to_block('-tlrc_NL_warped_dsets', user_opts, block)
    apply_uopt_to_block('-tlrc_NL_force_view', user_opts, block)
    apply_uopt_to_block('-tlrc_NL_awpy_rm', user_opts, block)
    apply_uopt_to_block('-tlrc_no_ss', user_opts, block)
    apply_uopt_to_block('-tlrc_rmode', user_opts, block)
    apply_uopt_to_block('-tlrc_suffix', user_opts, block)

    # check for external warp datasets and initialize
    nlw = block.opts.find_opt('-tlrc_NL_warped_dsets') is not None
    afw = block.opts.find_opt('-tlrc_affine_warped_dsets') is not None
    if nlw and afw:
       print("** cannot use both -tlrc_affine_warped_dsets " \
             "and -tlrc_NL_warped_dsets")
       return
    if afw:
       if mod_check_tlrc_affine_warp_dsets(proc, block): return
    if nlw:
       if mod_check_tlrc_NL_warp_dsets(proc, block): return

    block.valid = 1

def mod_check_tlrc_NL_warp_dsets(proc, block):
    """if we are given NL-warped datasets, fill nlw_priors
            warped anat aname, affine warp aname, NL warp aname
       note the warp type in nlw_type
    """

    oname = '-tlrc_NL_warped_dsets'
    dslist, rv = block.opts.get_string_list(oname)
    if not dslist:
       print('** error: failed parsing option %s' % oname)
       return 1
    if len(dslist) != 3:
       print('** error: %s requires 3 elements, have %d' % (oname, len(dslist)))
       return 1

    if proc.verb > 1:
       print("-- processing pre-warp NL dsets: %s" % dslist)

    # get and check anat, 1D warp, NL warp
    aname = gen_afni_name(dslist[0])
    if aname.view == '' and aname.type == 'BRIK': aname.new_view('+tlrc')
    dims = aname.dims()
    if dims[3] != 1:
       print('** error in %s p1: tlrc anat should be 1 volume,' % oname)
       print('   but dataset %s shows %d' % (aname.shortinput(), dims[3]))
       return 1

    axname = gen_afni_name(dslist[1])
    if axname.type != '1D':
       print('** error in %s p2: affine xform %s should be 1D' \
             % (oname, axname.shortinput()))
       return 1

    nlname = gen_afni_name(dslist[2])
    if nlname.view == '' and nlname.type == 'BRIK': nlname.new_view('+tlrc')
    dims = nlname.dims()
    if dims[3] != 3:
       print('** error in %s p3: NL warp should be 3 volumes,' % oname)
       print('   but dataset %s shows %d' % (nlname.shortinput(), dims[3]))
       return 1
    if not proc.looks_like_qwarp_name(dslist[2], warn=1):
       return 1

    # store the afni_names and bolt
    proc.nlw_priors = [aname, axname, nlname]

    return 0

def mod_check_tlrc_affine_warp_dsets(proc, block):
    """if we are given affine-warped datasets, fill nlw_priors
       (sister function to mod_check_tlrc_NL_warp_dsets)
            warped anat aname, affine warp aname, NL warp aname
       note the warp type in nlw_type
    """

    oname = '-tlrc_affine_warped_dsets'
    dslist, rv = block.opts.get_string_list(oname)
    if not dslist:
       print('** error: failed parsing option %s' % oname)
       return 1
    if len(dslist) != 2:
       print('** error: %s requires 2 elements, have %d' % (oname, len(dslist)))
       return 1

    if proc.verb > 1:
       print("-- processing pre-warp NL dsets: %s" % dslist)

    # get and check anat
    aname = gen_afni_name(dslist[0])
    if aname.view == '' and aname.type == 'BRIK': aname.new_view('+tlrc')
    dims = aname.dims()
    if dims[3] != 1:
       print('** error in %s p1: tlrc anat should be 1 volume,' % oname)
       print('   but dataset %s shows %d' % (aname.shortinput(), dims[3]))
       return 1

    # get and check affine warp
    axname = gen_afni_name(dslist[1])
    if axname.type != '1D':
       print('** error in %s p2: affine xform %s should be 1D' \
             % (oname, axname.shortinput()))
       return 1

    # store the afni_names and bolt
    proc.nlw_priors = [aname, axname]

    return 0

def prepare_tlrc_base(proc):
    """if we have a tlrc block:
           - assign proc.tlrc_base
           - verify existence
           - expect it to be copied into results directory (not done here)
           - prepare to store as uvar
           - note the final space of the tlrc_base dset

        This might be run from db_mod_postdata() or db_mod_tlrc(), since we
        might want to know about the template early on.
    """

    # if no tlrc block, nothing to do
    if not 'tlrc' in proc.block_names:
       return

    # if this has already been run, we are done
    if proc.tlrc_base is not None:
       return

    # first assign based on any option
    
    opt = proc.user_opts.find_opt('-tlrc_base')
    if opt: base = opt.parlist[0]
    else:   base = 'TT_N27+tlrc'

    proc.tlrc_base = gen_afni_name(base)       # store for later

    #--- first things first, see if we can locate the tlrc base
    #    if the user didn't already tell us where it is
    # locate() will search using input path, and if not found run
    #   @FindAfniDsetPath without that original path
    have_dset = proc.tlrc_base.locate()

    # if we have a template, get the space, else whine
    if have_dset:
       cmd = '3dinfo -space %s' % proc.tlrc_base.input()
       st, so, se = UTIL.limited_shell_exec(cmd)
       if st: print("** failed to get space: %s" % cmd)
       proc.tlrc_space = so[0]
    else:
       print("** failed to find tlrc_base '%s'" % proc.tlrc_base.initname)

    print("-- template = '%s', exists = %d"   \
          % (proc.tlrc_base.pv(), proc.tlrc_base.exist()))

# create a command to run @auto_tlrc
def db_cmd_tlrc(proc, block):
    """warp proc.anat to standard space"""

    if not proc.anat.pv() :
        print("** missing dataset name for tlrc operation")
        return None

    # if we are given NL-warped datasets, just apply them
    if block.opts.find_opt('-tlrc_NL_warped_dsets'):
       return tlrc_cmd_nlwarp_priors(proc, block)
    elif block.opts.find_opt('-tlrc_affine_warped_dsets'):
       return tlrc_cmd_affwarp_priors(proc, block)

    # add any user-specified options
    opt = block.opts.find_opt('-tlrc_opts_at')
    if opt: extra_opts = opt.parlist
    else:   extra_opts = []

    # note whether skull stripping is needed
    opt = block.opts.find_opt('-tlrc_no_ss')
    if opt or not proc.anat_has_skull or not proc.tlrc_ss: strip = 0
    else:                                                  strip = 1

    # note any requested resample mode
    rmode, err = block.opts.get_string_opt('-tlrc_rmod', default='')

    # note any suffix for the tlrcanat dataset
    suffix, err = block.opts.get_string_opt('-tlrc_suffix', default='')

    # note whether doing non-linear standard space warp
    proc.tlrc_nlw = block.opts.find_opt('-tlrc_NL_warp') != None
    if proc.tlrc_nlw and rmode:
       print('** -tlrc_rmode is not valid in case of NL_warp')
       return None

    base = proc.tlrc_base.shortinput()

    if proc.tlrc_nlw:
       return tlrc_cmd_nlwarp(proc, block, proc.anat, base, strip=strip,
                              suffix=suffix, exopts=extra_opts)
    else:
       return tlrc_cmd_warp  (proc, proc.anat, base, strip=strip, rmode=rmode,
                              suffix=suffix, exopts=extra_opts)

def tlrc_cmd_nlwarp (proc, block, aset, base, strip=1, suffix='', exopts=[]):
    """return block string for case of auto_warp.py

       aset     : dataset to warp       [afni_name]
       base     : alignment base        [string]
       strip    : flag - skull strip?   [0/1]
       suffix   : result suffix         [string]
       exopts   : extra options         [list of strings]

       same options as tlrc_cmd_warp, except no rmode

       resulting files under awpy:
          PREFIX.aw.nii           : final NL-warped anat
          anat.un.aff.qw_WARP.nii : final NL warp
          anat.un.aff.Xat.1D      : @auto_tlrc warp (not -ONELINE)
          (or anat.aff.Xat.1D)
       and copy back to results dir in AFNI format?
          - use 3dbucket to "rename" anat result, preserving history
          - move the warp files out of awpy
          - by default, remove the awpy directory

       note that the -affter matrix can be either -ONELINE or not:
          cat_matvec          anat.un.aff.nii::WARP_DATA -I > at.warp.aff12.1D
          cat_matvec -ONELINE anat.un.aff.nii::WARP_DATA -I > at.warp.aff12.1D
       (-ONELINE is needed for time series of EPI warps)

       and it can be applied via:
          3dNwarpApply -nwarp anat.un.aff.qw_WARP.nii -master NWARP     \
                 -affter at.warp.aff12.1D -source e0+orig -prefix e0.at.nlw

       2014.11.07: -affter warp should be applied at end of -nwarp option
    """

    if proc.verb > 0: print('-- using non-linear template alignment')

    prog = 'auto_warp.py'

    if strip: sstr = ' -skull_strip_input yes'
    else:     sstr = ' -skull_strip_input no'

    # adjust input option and output dset names based on uniformity correction
    # - default is to unifize here, otherwise not
    if proc.anat_unif_meth == 'default':
        ustr = ''                       # allow unif correction
        uxstr = 'un.'                   # expect 'un.' in dset names
    else:
        ustr = ' -unifize_input no'     # block correction
        uxstr = ''                      # do not expect 'un.' in dset names

    # maybe a suffix was provided
    if suffix:
       suf = suffix
       sufstr = ' -suffix %s' % suf
    else:
       suf = ''
       sufstr = ''

    # store what we expect for output
    apre = aset.prefix
    if not apre:
       print("** missing dataset name for NLtlrc operation")
       return None

    if len(exopts) > 0:
        pstr  = ' \\\n%*s' % (len(prog)+1, ' ')
        exstr = "%s%s" % (pstr ,' '.join(UTIL.quotize_list(exopts, '',1)))
    else: exstr = ''

    # start with block separator
    cmd = "# %s\n" % block_header('tlrc')

    cmd += "# warp anatomy to standard space (non-linear warp)\n" \
           "auto_warp.py -base %s -input %s \\\n"                 \
           "            %s%s%s"                                   \
           "%s"                                                   \
           "\n\n"                                                 \
           % (base, aset.pv(), sstr, sufstr, ustr, exstr)

    # add commands to move or copy results out of awpy directory

    # resulting files under awpy:
    #    PREFIX.aw.nii*          : final NL-warped anat
    #    anat.un.aff.qw_WARP.nii : final NL warp
    #    anat.un.aff.Xat.1D      : @auto_tlrc warp (not -ONELINE)
    # and copy back to results dir in AFNI format?
    #    - use 3dbucket to "rename" anat result, preserving history
    #    - move the warp files out of awpy
    #    - by default, remove the awpy directory

    proc.tlrcanat = proc.anat.new(apre+suf, '+tlrc')

    # [PT: May 30, 2021] auto_warp.py to work in *.nii.gz now
    # [PT: June 2, 2021] ... and just as quickly, rolled back to take *.nii,
    #                    since RCR updated AFNI_COMPRESSOR behavior, instead.
    # if no unifize, xmat strings will not have .un
    proc.nlw_aff_mat = 'anat.%saff.Xat.1D' % uxstr
    proc.nlw_NL_mat = 'anat.%saff.qw_WARP.nii' % uxstr
    proc.nlw_type = 'NL'

    proc.anat_warps.append(proc.nlw_aff_mat)
    proc.anat_warps.append(proc.nlw_NL_mat)

    pstr = '# move results up out of the awpy directory\n'  \
           '# - NL-warped anat, affine warp, NL warp\n'     \
           '# - use typical standard space name for anat\n' \
           '# - wildcard is a cheap way to go after any .gz\n'

    # do we force view to tlrc?  default to yes
    if block.opts.have_no_opt('-tlrc_NL_force_view'):
       vstr = ''
    else:
       pstr += '# - be sure NIFTI sform_code=2 means standard space\n'
       # include next indent
       vstr = '-DAFNI_NIFTI_VIEW=tlrc \\\n' \
              '         '

    pstr += '3dbucket %s-prefix %s awpy/%s.aw.nii*\n'          \
            % (vstr, proc.tlrcanat.prefix, apre+suf)

    pstr += 'mv awpy/%s .\n'   % proc.nlw_aff_mat
    pstr += 'mv awpy/%s .\n\n' % proc.nlw_NL_mat

    # probably nuke the awpy directory
    if block.opts.have_yes_opt('-tlrc_NL_awpy_rm', default=1):
       proc.rm_list.append('awpy') ; proc.rm_dirs = 1

    return cmd + pstr

def tlrc_cmd_nlwarp_priors(proc, block):
    """NL warp datasets were passed to afni_proc.py,
       just note datasets as if they were made here

       set tlrcanat, nlw_aff_mat, nlw_NL_mat
       append the warps
    """

    if len(proc.nlw_priors) != 3: return ''

    print('-- importing NL-warp datasets')

    # this case requires -volreg_align_e2a for the xform to apply (correctly)
    vblk = proc.find_block('volreg')
    if vblk is not None:
       if vblk.opts.find_opt('-volreg_tlrc_warp') \
          and not vblk.opts.find_opt('-volreg_align_e2a'):
          print("** -tlrc_NL_warped_dsets requires -volreg_align_e2a")
          print("   (else EPI -> stdandard space will not be correct)")
          return None

    p0 = proc.nlw_priors[0]
    p1 = proc.nlw_priors[1]
    p2 = proc.nlw_priors[2]

    cmd = "# %s\n" % block_header('tlrc')
    cmd += '\n'                                                         \
           '# nothing to do: have external -tlrc_NL_warped_dsets\n\n'   \
           '# warped anat     : %s\n'                                   \
           '# affine xform    : %s\n'                                   \
           '# non-linear warp : %s\n\n'                                 \
           % (p0.shortinput(), p1.shortinput(), p2.shortinput())

    proc.tlrcanat = p0

    proc.nlw_aff_mat = p1.shortinput()
    proc.nlw_NL_mat  = p2.shortinput()
    proc.nlw_type = 'NL'

    proc.anat_warps.append(proc.nlw_aff_mat)
    proc.anat_warps.append(proc.nlw_NL_mat)

    return cmd

def tlrc_cmd_warp(proc, aset, base, strip=1, rmode='', suffix='', exopts=[]):
    """return block string for case of @auto_tlrc

       aset     : dataset to warp       [afni_name]
       base     : alignment base        [string]
       strip    : flag - skull strip?   [0/1]
       rmode    : resample mode         [string]
       suffix   : result suffix         [string]
       exopts   : extra options         [list of strings]
    """

    prog = '@auto_tlrc'

    if strip: sstr = ''
    else:     sstr = ' -no_ss'

    if rmode: rstr = ' -rmode %s' % rmode
    else:     rstr = ''

    if suffix:
       sufstr = ' -suffix %s' % suffix
       suf = suffix
    else:
       sufstr = ''
       suf = ''

    if len(exopts) > 0:
        pstr  = ' \\\n%*s' % (len(prog)+1, ' ')
        exstr = "%s%s" % (pstr ,' '.join(UTIL.quotize_list(exopts, '',1)))
    else: exstr = ''

    # store what we expect for output
    apre = aset.prefix
    if not apre:
       print("** missing dataset name for tlrc warp")
       return None
    proc.tlrcanat = proc.anat.new(apre+suf, '+tlrc')

    # start with block separator
    cmd = "# %s\n" % block_header('tlrc')

    cmd += "# warp anatomy to standard space\n"         \
           "@auto_tlrc -base %s -input %s%s%s%s"        \
           "%s"                                         \
           "\n\n"                                       \
           % (base, aset.pv(), sstr, rstr, sufstr, exstr)

    # create xform file
    wfile = 'warp.anat.Xat.1D'
    cmd += '# store forward transformation matrix in a text file\n' \
           'cat_matvec %s::WARP_DATA -I > %s\n\n' % (proc.tlrcanat.pv(),wfile)
    proc.anat_warps.append(wfile)
    proc.nlw_type = 'affine'

    return cmd

def tlrc_cmd_affwarp_priors(proc, block):
    """return block string for case of affine standard space warp,
       but when datasets were passed to afni_proc.py

       set tlrcanat, nlw_aff_mat
       - based on tlrc_cmd_nlwarp_priors()
    """

    if len(proc.nlw_priors) != 2: return ''

    # this case requires -volreg_align_e2a for the xform to apply
    vblk = proc.find_block('volreg')
    if vblk is not None:
       if vblk.opts.find_opt('-volreg_tlrc_warp') \
          and not vblk.opts.find_opt('-volreg_align_e2a'):
          print("** -tlrc_affine_warped_dsets requires -volreg_align_e2a")
          print("   (else EPI -> stdandard space will not be correct)")
          return None

    print('-- importing affine warp datasets')

    p0 = proc.nlw_priors[0]
    p1 = proc.nlw_priors[1]              

    cmd = "# %s\n" % block_header('tlrc')
    cmd += '\n'                                                           \
           '# nothing to do: have external -tlrc_affine_warped_dsets\n\n' \
           '# warped anat     : %s\n'                                     \
           '# affine xform    : %s\n\n'                                   \
           % (p0.shortinput(), p1.shortinput())

    proc.tlrcanat = p0
    proc.nlw_aff_mat = p1.shortinput()
    proc.nlw_type = 'affine'

    proc.anat_warps.append(proc.nlw_aff_mat)

    return cmd

def married_types_match(proc, stims, stypes, bases):
    """if any stim files are married, the stype should be AM1 or AM2
       some basis functions require married files
    """

    if len(stims) == 0: return 1
    if not proc.test_stims: return 1

    if proc.verb > 2:
        print('-- checking married type match for:')
        print('   stims : %s' % stims)
        print('   types : %s' % stypes)

    ok_all = 1  # assume good, and look for failure
    mtypes = ['AM1', 'AM2']

    for ind in range(len(stims)):
        fname = stims[ind]
        stype = stypes[ind]
        basis = bases[ind]

        if stype == 'file': continue

        adata = LD.AfniData(fname)
        if adata == None:
            print("** MTM: failed to load stim timing file '%s'" % fname)
            ok_all = 0
            continue
        if not adata.ready:
            print("** MTM: failed to load stimulus timing file '%s'" % fname)

        if stype in mtypes and not adata.married:
            print('** stim type %d is married (%s), but file (%s) is not' \
                  % (ind+1, stype, fname))
            ok_all = 0

        # just a warning for now, since stim_types are new

        if stype not in mtypes and adata.married:
            print('** stim type %d is not married, but file (%s) is' \
                  % (ind+1, fname))

        if UTIL.basis_is_married(basis) and not stype in mtypes:
            print('** have married basis (#%d = %s), but not married type\n' \
                  '   (consider -regress_stim_types)' % (ind+1,basis))

        if UTIL.basis_is_married(basis) and not adata.married:
            print('** have married basis (#%d = %s), but not married file %s\n'\
                  % (ind+1, basis, fname))

    if not ok_all and proc.verb > 0:
        print('   (use "-test_stim_files no" to ignore such errors)')

    return ok_all

def valid_file_types(proc, stims, file_type, stypes=[], goforit=0):
    """verify that the files are valid as 1D, local or global times

         1 : 1D
         2 : local
         3 : global
        10 : local or global

       If invalid, print messages.
       If valid, print any warning messages.
       So checks are always run twice.

       goforit: means -GOFORIT was already used

       return 1 if valid, 0 otherwise
    """

    if len(stims) == 0: return 1

    if not proc.test_stims: return 1

    if proc.verb > 2: print('-- check files of type %d: %s' % (file_type, stims))

    ok_all = 1  # assume good, and look for failure

    for findex in range(len(stims)):
        fname = stims[findex]
        ftype = file_type
        if stypes and stypes[findex] == 'file': ftype = 1

        adata = LD.AfniData(fname, verb=proc.verb)
        if adata == None:
            print("** failed to load stim timing file '%s'" % fname)
            ok_all = 0
            continue
        if not adata.ready:
            print("** failed to load stimulus timing file '%s'" % fname)
            ok_all = 0
            continue

        if adata.married and proc.verb > 1:
            print('-- have married file %s' % fname)

        ok = 0  # prudent
        if   ftype == 1: # check 1D, just compare against total reps
            ok = adata.looks_like_1D(run_lens=proc.reps_all, verb=0)
            if ok: adata.file_type_warnings_1D(run_lens=proc.reps_all)
        elif ftype == 2: # check local
            ok = adata.looks_like_local_times(run_lens=proc.reps_all,
                                                tr=proc.tr, verb=0)
            if ok: adata.file_type_warnings_local(run_lens=proc.reps_all,
                                                tr=proc.tr)
        elif ftype == 3: # check global
            ok = adata.looks_like_global_times(run_lens=proc.reps_all,
                                                tr=proc.tr, verb=0)
            if ok: adata.file_type_warnings_global(run_lens=proc.reps_all,
                                                tr=proc.tr)
        elif ftype == 10: # check both local and global
            # first test as global (a file that looks like global or local
            # should be treated as global)
            ok = adata.looks_like_global_times(run_lens=proc.reps_all,
                                            tr=proc.tr,verb=0)
            if ok:adata.file_type_warnings_global(run_lens=proc.reps_all,
                                            tr=proc.tr)
            # if not global, then check as local
            if not ok:
                ok = adata.looks_like_local_times(run_lens=proc.reps_all,
                                                    tr=proc.tr, verb=0)
                if ok: adata.file_type_warnings_local(run_lens=proc.reps_all,
                                                    tr=proc.tr)
        else: # error
            print('** valid_file_types: bad type %d' % ftype)
            return 0

        # if empty, warn user (3dD will fail)
        if ok and adata.empty:
            if goforit:
               print('** empty stim file %s (but have 3dD -GOFORIT)\n' % fname)
            else:
               print('** empty stim file %s (consider 3dD -GOFORIT ...)\n' \
                     % fname)

        # if current file is good, move on
        if ok: continue

        if ok_all: # first time: surround errors with dashed lines
           print('------------------------------------------------------------')

        # else, propagate any failure and warn user
        ok_all = 0

        # if ft=10, report local errors only
        if ftype == 10: ftype = 2

        if ftype == 1:
            adata.looks_like_1D(run_lens=proc.reps_all, verb=3)
        elif ftype == 2:
            adata.looks_like_local_times(run_lens=proc.reps_all, tr=proc.tr,
                                         verb=3)
        elif ftype == 3: # check global
            adata.looks_like_global_times(run_lens=proc.reps_all, tr=proc.tr,
                                         verb=3)
        print('------------------------------------------------------------')

    if not ok_all:
        print("-- consider use of '-test_stim_files no' if files are OK")
        print('------------------------------------------------------------')

    return ok_all

# currently nothing to verify for an 'empty' command (placeholder command)
# just return 1
def db_mod_empty(block, proc, user_opts):
    block.valid = 1
    return 1

# create a placeholder command using 3dTcat to copy the EPI data
def db_cmd_empty(proc, block):
    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(block, view=1)

    cmd = "# %s\n"                                                      \
          "# empty block: use '3dTcat' as a placeholder command\n"      \
          "foreach run ( $runs )\n"                                     \
          "    3dTcat -prefix %s %s\n"                                  \
          "end\n\n" % (block_header('empty'), prefix, prev)

    return cmd

# create a gen_epi_review.py command
def db_cmd_gen_review(proc):
    if not proc.epi_review: return None

    tblk = proc.find_block('tcat')

    # get dataset names, but be sure not to get the surface form
    # (if ME, force it here, and get all echoes - save and restore use_me)
    use_me = proc.use_me
    proc.use_me = proc.have_me
    dstr = proc.dset_form_wild('tcat', proc.origview, surf_names=0, eind=-2)
    proc.use_me = use_me
    if proc.have_me:
       mestr = "# (all echoes of all runs)\n"
    else:
       mestr = ''

    if proc.html_rev_style != 'none':
       hcom = ' and HTML report'
    else:
       hcom = ""

    cmd = "# %s\n"                                                      \
          "# generate quality control review scripts%s\n\n"             \
          "# generate a review script for the unprocessed EPI data\n"   \
          "%s"                                                          \
          "gen_epi_review.py -script %s \\\n"                           \
          "    -dsets %s\n\n"                                           \
          % (block_header('auto block: QC_review'), hcom,
             mestr, proc.epi_review, dstr)

    # if no regress block, skip gen_ss_review_scripts.py
    if not proc.find_block('regress'):
       if proc.verb:
          print('-- no regress block, skipping gen_ss_review_scripts.py')
       return cmd

    # Generate AP uvars JSON file, both for gssrs and APQC.
    # (was passing via direct opts, but change to json)
    json_prep = prep_gssrs_json_str(proc)

    # --- misc options to pass directly (gen_ss will not figure out)
    lopts = ''

    # make json prep string
    jp_str = ''
    if json_prep and proc.ap_uv_file:
       jp_str = '\n%s\n'                                            \
                '# initialize gen_ss_review_scripts.py with %s\n'   \
                % (json_prep, proc.ap_uv_file)
       lopts += ' \\\n    -init_uvars_json %s' % proc.ap_uv_file

    if proc.ssr_uvars:
       lopts += ' \\\n    -write_uvars_json %s' % proc.ssr_uvars

    gcmd = UTIL.add_line_wrappers('gen_ss_review_scripts.py -exit0%s\n' % lopts)

    cmd += '# -------------------------------------------------\n'      \
           '# generate scripts to review single subject results\n'      \
           '# (try with defaults, but do not allow bad exit status)\n'  \
           '%s'                                                         \
           '%s\n' % (jp_str, gcmd)

    return cmd

def prep_gssrs_json_str(proc):
    """instead of passing options to gssrs, generate a JSON file with
       corresponding lists.
       Generate a shell string to create the JSONN file, e.g.,

cat << EOF | afni_python_wrapper.py -eval "data_file_to_json()" \
           > stuff.json
   mot_limit   :     0.3
   out_limit   :     0.05
   copy_anat   :     FT_anat+orig.HEAD
   mask_dset   :     mask_epi_anat.$subj+tlrc.HEAD
EOF

       First generate a table, so that we can choose a reasonable column size.

       return an appropriate string, or ''
    """
    # --- get the uvars table, and return empty if table is
    aptab = ap_uvars_table(proc)
    if len(aptab) == 0:
       return ''

    # --- generate the table string

    # use the max length for ':' position
    maxlen = max([len(e[0]) for e in aptab])

    # could make this tight, but this is more readable
    slist = []
    for entry in aptab:
       slist.append("  %-*s : %s" % (maxlen, entry[0], ' '.join(entry[1])))
    uvar_str = '\n'.join(slist)

    # --- generate the complete JSON generation string
    #     (currently using afni_util.py (via apw) to convert from
    #      plain text to JSON)

    tfile = 'out.ap_uvars.txt'
    jstr = '# write AP uvars into a simple txt file\n'  \
           'cat << EOF > %s\n'                          \
           '%s\n'                                       \
           'EOF\n\n' % (tfile, uvar_str)

    jstr += '# and convert the txt format to JSON\n'                          \
            'cat %s | afni_python_wrapper.py -eval "data_file_to_json()" \\\n'\
            '  > %s\n' % (tfile, proc.ap_uv_file)

    return jstr

def ap_uvars_table(proc):
    """generate a string only table of AP uvars
       
       each entry has entry[0] = label, entry[1] = list of values
       e.g., table = [
                        ['mot_limit', ['0.3']]
                        ['cheeses', ['cheddar', 'brie', 'muenster']]
                     ]
    """

    # it seems preferable to list all uvars together, so make sure
    # anything listed here is in the main library
    g_ap_uvars = ['mot_limit', 'out_limit', 'motion_dset',
                  'copy_anat', 'combine_method', 'errts_dset', 'mask_dset',
                  'dir_suma_spec', 'suma_specs',
                  'ss_review_dset']

    from afnipy import lib_ss_review as LSSR
    ss_uvars = LSSR.def_ss_uvar_names()
    for apuv in g_ap_uvars:
       if apuv not in ss_uvars:
          print("** warning: ss uvars is missing ap uvar %s" % apuv)

    # generate a uvars table of strings
    aptab = []
    if proc.mot_cen_lim > 0.0:
       aptab.append(['mot_limit', ['%s' % proc.mot_cen_lim]])
    if proc.out_cen_lim > 0.0:
       aptab.append(['out_limit', ['%s' % proc.out_cen_lim]])
    if proc.mot_extern != '' :
       aptab.append(['motion_dset', ['%s' % proc.mot_file]])

    if proc.anat_orig is not None:
       aptab.append(['copy_anat', ['%s' % proc.anat_orig.shortinput(head=1)]])
    if proc.combine_method is not None:
       aptab.append(['combine_method', ['%s' % proc.combine_method]])
    # surface spec info
    if proc.surf_spec_dir != '':
       aptab.append(['dir_suma_spec', ['%s' % proc.surf_spec_dir]])
    if len(proc.surf_spec_base) > 0:
       aptab.append(['suma_specs', proc.surf_spec_base])

    if len(proc.stims) == 0 and proc.errts_final:       # 2 Sep, 2015
       if proc.surf_anat: ename = proc.errts_final
       else:              ename = '%s%s.HEAD' % (proc.errts_final, proc.view)
       aptab.append(['errts_dset', ['%s' % ename]])

    # specify mask dataset to be used for stats, since it might not be clear
    # (no longer def in TSNR)                                    22 Feb 2021
    if proc.mask and not proc.surf_anat:
       aptab.append(['mask_dset', ['%s' % proc.mask.shortinput(head=1)]])
    if proc.tlrc_base:
       # tlrc_base is called template
       aptab.append(['template', ['%s' % proc.tlrc_base.shortinput(head=1)]])

    if proc.ssr_b_out != '':
       aptab.append(['ss_review_dset', ['%s' % proc.ssr_b_out]])

    # add all generically specified uvars
    for attr in proc.uvars.attributes(name=0):
       aptab.append([attr, proc.uvars.val(attr)])

    return aptab

def block_header(hname, maxlen=74, hchar='=', endchar=''):
    """return a title string of 'hchar's with the middle chars set to 'name'
       if endchar is set, put at both ends of header
       e.g. block_header('volreg', endchar='##') """

    # move this function out

    return UTIL.section_divider(hname=hname, maxlen=maxlen, hchar=hchar,
                                endchar=endchar)

# ----------------------------------------------------------------------
# help strings

def show_program_help(section=''):
   # maybe print them all
   if section == '':
      print(g_help_intro)
      # now print examples from lib_ap_examples
      show_help_examples(source='eshow')
      print(g_help_notes)
      print(g_help_options)
      print(g_help_trailer)

      return 0

   # process new example string separately for now
   if section in g_valid_help_sections:
      show_help_examples(source=section)
      return 0

   # and individual sections
   rv = 0
   try:
      shelp = eval('g_help_%s' % section)
      print(shelp)
   except:
      print("** invalid help section: %s" % section)
      rv = 1

   return rv

def show_help_examples(source='eold'):
   """just print the main string, until we are ready for more"""
   if source.lower() in ['eold', 'old']:
       print(g_help_examples)
       return

   # print new-style examples
   from afnipy import lib_ap_examples as EGS
   keys_rm = []
   # if 'show' example, exclude the noshow ones
   if source == 'eshow':
      keys_rm = ['noshow']
   EGS.populate_examples(keys_rm=keys_rm)

   # print only AP examples, or all
   if source.lower() in ['enew', 'new', 'afni_proc', 'afni_proc.py']:
      aphelp = 1
   else:
      aphelp = -1

   print(g_egnew_header)
   EGS.display_eg_all(aphelp=aphelp, verb=2)
   print(g_egnew_trailer)

   return

g_egnew_header = """
==================================================
EXAMPLES (options can be provided in any order): ~1~
"""

g_egnew_trailer = """
-ask_me EXAMPLES:  ** NOTE: -ask_me is antiquated ** ~2~

            afni_proc.py -ask_me

    Perhaps at some point this will be revived.  It would be useful.
    The -ask_me methods have not been seriously updated since 2006.
"""

# ----------------------------------------------------------------------
# global help string (see end global help string)
# -- this is long, get it out of the main library

g_help_intro = """
===========================================================================
afni_proc.py        - generate a tcsh script for an AFNI process stream

Purpose: ~1~

   This program is meant to create single subject processing scripts for
   task, resting state or surface-based analyses.  The processing scripts
   are written in the tcsh language.

   The typical goal is to create volumes of aligned response magnitudes
   (stimulus beta weights) to use as input for a group analysis.

Inputs (only EPI is required): ~1~

   - anatomical dataset
   - EPI time series datasets
   - stimulus timing files
   - processing and design decisions:
       e.g. TRs to delete, blur size, censoring options, basis functions

Main outputs (many datasets are created): ~1~

   - for task-based analysis: stats dataset (and anat_final)
   - for resting-state analysis: errts datasets ("cleaned up" EPI)

Basic script outline: ~1~

   - copy all inputs to new 'results' directory
   - process data: e.g. despike,tshift/align/tlrc/volreg/blur/scale/regress
   - leave all (well, most) results there, so user can review processing
   - create quality control data (APQC HTML page, ss_review_scripts, etc.)

The exact processing steps are controlled by the user, including which main
processing blocks to use, and their order.  See the 'DEFAULTS' section for
a description of the default options for each block.

The output script (when executed) would create a results directory, copy
input files into it, and perform all processing there.  So the user can
delete the results directory and modify/re-run the script at their whim.

Note that the user need not actually run the output script.  The user
should feel free to modify the script for their own evil purposes, or to
just compare the processing steps with those in their own scripts.  Also,
even if a user is writing their own processing scripts, it is a good idea
to get some independent confirmation of the processing, such as by using
afni_proc.py to compare the results on occasion.

The text interface can be accessed via the -ask_me option.  It invokes a
question & answer session, during which this program sets user options on
the fly.  The user may elect to enter some of the options on the command
line, even if using -ask_me.  See "-ask_me EXAMPLES", below.

** However, -ask_me has not been touched in many years.  I suggest starting
   with the 'modern' examples (for task/rest/surface), or by using the
   uber_subject.py GUI (graphical user interface) to generate an initial
   afni_proc.py command script.

   See uber_subject.py -help (or just start the GUI) for details.

==================================================
SECTIONS: order of sections in the "afni_proc.py -help" output ~1~

    program introduction    : (above) basic overview of afni_proc.py
    SETTING UP AN ANALYSIS  : a guide for getting started
    PROCESSING BLOCKS       : list of possible processing blocks
    DEFAULTS                : basic default operations, per block
    EXAMPLES                : various examples of running this program
    NOTE sections           : details on various topics
        GENERAL ANALYSIS NOTE, QUALITY CONTROL NOTE,
        RESTING STATE NOTE, FREESURFER NOTE,
        TIMING FILE NOTE, MASKING NOTE,
        ANAT/EPI ALIGNMENT CASES NOTE, ANAT/EPI ALIGNMENT CORRECTIONS NOTE,
        WARP TO TLRC NOTE,
        RETROICOR NOTE, MULTI ECHO NOTE,
        RUNS OF DIFFERENT LENGTHS NOTE, SCRIPT EXECUTION NOTE
    OPTIONS                 : descriptions of all program options
        informational       : options to get quick info and quit
        general execution   : options not specific to a processing block
        block options       : specific to blocks, in default block order

==================================================
SETTING UP AN ANALYSIS: ~1~

For those new to using afni_proc.py, it is very helpful to start with an
example that is similar to what you want to do, generally taken from the help
examples (afni_proc.py -show_example_names) or prior publication.

Once satisfied with a single application of afni_proc.py, one would then loop
over subjects by running afni_proc.py on each, using subject variables to refer
to the individual set of input data and the output subject ID.

Starting up, there is a general set of choices that is good to consider:

   a. type of analysis:    task or rest/naturalistic
   b. domain of analysis:  volume or surface (possibly either as ROI)
   c. main input data:     anat, EPI runs (single or multi-echo), task timing,
                           surfaces and surface anatomical
   d. extra input data:    NL distortion warp, NL template warp, blip dsets,
                           ROI imports, anat followers, physio regressors,
                           external registration base (for volreg or anat),
                           external motion files, censor list, extra regressors
   e. processing blocks:   main EPI processing blocks and their order
                         - see "PROCESSING BLOCKS"
   f. optional processing: physio regression, tedana, ANATICOR, ROI regression,
                           bandpassing
   g. main options:        template, blur level (if any), censor levels,
                           EPI/anat cost and other alignment options
   h. other options:       there are many, e.g.: motion regressors, bandpass,
                           ANATICOR, and many that are specific to QC

   ----------

   a. type of analysis

      For a task analysis, one provides stimulus timing files and corresponding
      modeling options.  This is a large topic that centers on the use of
      3dDeconvolve.

      Options for task analysis generally start with -regress, as they pertain
      to the regress block.  However one generally includes a regress block in
      any analysis (even partial ones, such as for alignment), as it is the
      gateway to the APQC HTML report.

   b. domain of analysis

      For a surface analysis, one provides a SUMA spec file per hemisphere,
      along with a surface anatomical dataset.  Mapping from the volume to the
      surface generally happens soon after all volumetric registration is done,
      and importantly, before any blur block.  Restricting blurring to the
      surface is one of the reasons to perform such an analysis.

      In a surface analysis, no volumetric template or tlrc options are given.
      Surface analysis is generally performed on SUMA's standard meshes, though
      it need not be.

      An ROI analysis is generally performed as a typical volume or surface
      analysis, but without any applied blurring (which effectively happens
      later, when averaging over the ROIs).

   c. main input data

      EPI datasets are required, for one or more runs and one or more echoes.
      Anything else is optional.

      Typically one also includes a subject anatomy, any task timing files, and
      surface datasets (spec files an anatomy) if doing a surface analysis.

   d. extra input data

      It is common to supply a non-linear transformation warp dataset (from
      sswarper) to apply for anatomy->template alignment.  One might also have
      a pre-computed non-linear B0 distortion map or reverse phase encoding
      (blip) dataset, ROIs or other anatomical followers or physiological
      regressors.  An EPI base dataset might be provided to align the EPI to,
      and possibly one to guide alignment to the subject anatomical dataset.

      Precomputed motion parameter files could be provided (if skipping the
      volreg block), as well as an external censor time series or precomputed
      regressors (of interest or not).

      These extra inputs will affect use of other options.

   e. processing blocks

      As described in the "PROCESSING BLOCKS" section, one can specify an
      ordered list of main processing blocks.  The order of the listed blocks
      will determine their order in the processing script.  Of course, for a
      given set of blocks, there is typically a preferred order.

      Options specific to one block will generally start with that block name.
      For example, the -regress_* options apply to the regress block.

      It is logically clear (but not necessary) to provide block options in the
      same chronological order as the blocks.

   f. optional processing

      Optional processing might include things like:
        - physiological noise regression, based on use of physio_calc.py
        - tedana, or a variant, for use in combining multi-echo time series
        - ANATICOR (local white matter regression)
        - ROI regression (averages or principle components)
        - bandpassing (low pass, high pass, or single or multiple bands)

   g. main options

      One typically provides:
        - a template (and accompanying non-linear anat to template
          transformation datasets)
        - an amount to blur (or a choice to not blur, as would apply to an ROI
          analysis), or a level to blur _to_
        - censor levels (for outliers or the Euclidean norm of the motion
          parameters)
        - alignment options, such as the cost function for align_epi_anat.py
          and a local EPI unifize option - there are many options to control
          many aspects of registration
        - many quality control options are also considered appropriate for
          consistent use

   h. other options

      Each step of processing has many control options around it.  It is
      important to think through what might be appropriate for the data in
      question.

      No one analysis fits all data.

      Quality control "options" are not really considered optional.

==================================================
PROCESSING BLOCKS (of the output script): ~1~

The output script will go through the following steps, unless the user
specifies otherwise.

automatic blocks (the tcsh script will always perform these): ~2~

    setup       : check subject arg, set run list, create output dir, and
                  copy stim files
    tcat        : copy input datasets and remove unwanted initial TRs

default blocks (the user may skip these, or alter their order): ~2~

    tshift      : slice timing alignment on volumes (default is -time 0)
    volreg      : volume registration (default to third volume)
    blur        : blur each volume (default is 4mm fwhm)
    mask        : create a 'brain' mask from the EPI data
    scale       : scale each run mean to 100, for each voxel (max of 200)
    regress     : regression analysis (default is GAM, peak 1, with motion
                  params)

optional blocks (the default is to _not_ apply these blocks) ~2~

    align       : align EPI anat anatomy (via align_epi_anat.py)
    combine     : combine echoes into one
    despike     : truncate spikes in each voxel's time series
    empty       : placeholder for some user command (uses 3dTcat as sample)
    ricor       : RETROICOR - removal of cardiac/respiratory regressors
    surf        : project volumetric data into the surface domain
    tlrc        : warp anat to a standard space/specified template

implicit blocks (controlled by program, added when appropriate) ~2~

    blip        : perform B0 distortion correction
    outcount    : temporal outlier detection
    QC review   : generate QC review scripts and HTML report
    anat_unif   : anatomical uniformity correction

==================================================
DEFAULTS: basic defaults for each block (blocks listed in default order) ~1~

    A : denotes automatic block that is not a 'processing' option
    D : denotes a default processing block (others must be requested)

A   setup:    - use 'SUBJ' for the subject id
                    (option: -subj_id SUBJ)
              - create a t-shell script called 'proc_subj'
                    (option: -script proc_subj)
              - use results directory 'SUBJ.results'
                    (option: -out_dir SUBJ.results)

A   tcat:     - do not remove any of the first TRs

    despike:  - NOTE: by default, this block is _not_ used
              - automasking is not done (requires -despike_mask)

    ricor:    - NOTE: by default, this block is _not_ used
              - polort based on twice the actual run length
              - solver is OLSQ, not REML
              - do not remove any first TRs from the regressors

D   tshift:   - align slices to the beginning of the TR
              - use quintic interpolation for time series resampling
                    (option: -tshift_interp -quintic)

    align:    - align the anatomy to match the EPI
                (also required for the option of aligning EPI to anat)

    tlrc:     - use TT_N27+tlrc as the base (-tlrc_base TT_N27+tlrc)
              - no additional suffix (-tlrc_suffix NONE)
              - use affine registration (no -tlrc_NL_warp)

D   volreg:   - align to third volume of first run, -zpad 1
                    (option: -volreg_align_to third)
                    (option: -volreg_zpad 1)
              - use cubic interpolation for volume resampling
                    (option: -volreg_interp -cubic)
              - apply motion params as regressors across all runs at once
              - do not align EPI to anat
              - do not warp to standard space

    combine:  - combine methods using OC (optimally combined)

D   blur:     - blur data using a 4 mm FWHM filter with 3dmerge
                    (option: -blur_filter -1blur_fwhm)
                    (option: -blur_size 4)
                    (option: -blur_in_mask no)

D   mask:     - create a union of masks from 3dAutomask on each run
              - not applied in regression without -regress_apply_mask
              - if possible, create a subject anatomy mask
              - if possible, create a group anatomy mask (tlrc base)

D   scale:    - scale each voxel to mean of 100, clip values at 200

D   regress:  - use GAM regressor for each stim
                    (option: -regress_basis)
              - compute the baseline polynomial degree, based on run length
                    (e.g. option: -regress_polort 2)
              - do not censor large motion
              - output fit time series
              - output ideal curves for GAM/BLOCK regressors
              - output iresp curves for non-GAM/non-BLOCK regressors

    empty:    - do nothing (just copy the data using 3dTcat)
"""
g_help_examples = """
==================================================
EXAMPLES (options can be provided in any order): ~1~

    Example 1. Minimum use. ~2~

       Provide datasets and stim files (or stim_times files).  Note that a
       dataset suffix (e.g. HEAD) must be used with wildcards, so that
       datasets are not applied twice.  In this case, a stim_file with many
       columns is given, where the script to changes it to stim_times files.

            afni_proc.py -dsets epiRT*.HEAD              \\
                         -regress_stim_files stims.1D

       or without any wildcard, the .HEAD suffix is not needed:

            afni_proc.py -dsets epiRT_r1+orig epiRT_r2+orig epiRT_r3+orig \\
                         -regress_stim_files stims.1D

 **************************************************************
 *  New and improved!  Examples that apply to AFNI_data4.     *
 *  (were quickly OLD and OBSOLETE, as we now use AFNI_data6) *
 **************************************************************

    The following examples can be run from the AFNI_data4 directory, and
    are examples of how one might process the data for subject sb23.

    Example 2. Very simple. ~2~

       Use all defaults, except remove 3 TRs and use basis
       function BLOCK(30,1).  The default basis function is GAM.

            afni_proc.py -subj_id sb23.e2.simple                       \\
                    -dsets sb23/epi_r??+orig.HEAD                      \\
                    -tcat_remove_first_trs 3                           \\
                    -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                    -regress_basis 'BLOCK(30,1)'

    Example 3. (no longer) The current class example.  ~2~

       Copy the anatomy into the results directory, register EPI data to
       the last TR, specify stimulus labels, compute blur estimates, and
       provide GLT options directly to 3dDeconvolve.  The GLTs will be
       ignored after this, as they take up too many lines.

            afni_proc.py -subj_id sb23.blk                             \\
                    -dsets sb23/epi_r??+orig.HEAD                      \\
                    -copy_anat sb23/sb23_mpra+orig                     \\
                    -tcat_remove_first_trs 3                           \\
                    -volreg_align_to last                              \\
                    -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                    -regress_stim_labels tneg tpos tneu eneg epos      \\
                                         eneu fneg fpos fneu           \\
                    -regress_basis 'BLOCK(30,1)'                       \\
                    -regress_opts_3dD                                  \\
                        -gltsym 'SYM: +eneg -fneg'                     \\
                        -glt_label 1 eneg_vs_fneg                      \\
                        -gltsym 'SYM: 0.5*fneg 0.5*fpos -1.0*fneu'     \\
                        -glt_label 2 face_contrast                     \\
                        -gltsym 'SYM: tpos epos fpos -tneg -eneg -fneg'\\
                        -glt_label 3 pos_vs_neg                        \\
                    -regress_est_blur_epits                            \\
                    -regress_est_blur_errts

    Example 4. Similar to 3, but specify the processing blocks. ~2~

       Adding despike and tlrc, and removing tshift.  Note that
       the tlrc block is to run @auto_tlrc on the anat.  Ignore the GLTs.

            afni_proc.py -subj_id sb23.e4.blocks                       \\
                    -dsets sb23/epi_r??+orig.HEAD                      \\
                    -blocks despike volreg blur mask scale regress tlrc\\
                    -copy_anat sb23/sb23_mpra+orig                     \\
                    -tcat_remove_first_trs 3                           \\
                    -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                    -regress_stim_labels tneg tpos tneu eneg epos      \\
                                         eneu fneg fpos fneu           \\
                    -regress_basis 'BLOCK(30,1)'                       \\
                    -regress_est_blur_epits                            \\
                    -regress_est_blur_errts

    Example 5a. RETROICOR, resting state data. ~2~

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

            afni_proc.py -subj_id sb23.e5a.ricor            \\
                    -dsets sb23/epi_r??+orig.HEAD           \\
                    -do_block despike ricor                 \\
                    -tcat_remove_first_trs 3                \\
                    -ricor_regs_nfirst 3                    \\
                    -ricor_regs sb23/RICOR/r*.slibase.1D    \\
                    -regress_motion_per_run

       If tshift, blurring and masking are not desired, consider replacing
       the -do_block option with an explicit list of blocks:

            -blocks despike ricor volreg regress

    Example 5b. RETROICOR, while running a normal regression. ~2~

       Add the ricor regressors to a normal regression-based processing
       stream.  Apply the RETROICOR regressors across runs (so using 13
       concatenated regressors, not 13*9).  Note that concatenation is
       normally done with the motion regressors too.

       To example #3, add -do_block and three -ricor options.

            afni_proc.py -subj_id sb23.e5b.ricor                       \\
                    -dsets sb23/epi_r??+orig.HEAD                      \\
                    -do_block despike ricor                            \\
                    -copy_anat sb23/sb23_mpra+orig                     \\
                    -tcat_remove_first_trs 3                           \\
                    -ricor_regs_nfirst 3                               \\
                    -ricor_regs sb23/RICOR/r*.slibase.1D               \\
                    -ricor_regress_method 'across-runs'                \\
                    -volreg_align_to last                              \\
                    -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                    -regress_stim_labels tneg tpos tneu eneg epos      \\
                                         eneu fneg fpos fneu           \\
                    -regress_basis 'BLOCK(30,1)'                       \\
                    -regress_est_blur_epits                            \\
                    -regress_est_blur_errts

       Also consider adding -regress_bandpass.

    Example 5c. RETROICOR (modern): censor and band pass. ~2~

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

            afni_proc.py -subj_id sb23.e5a.ricor            \\
                    -dsets sb23/epi_r??+orig.HEAD           \\
                    -blocks despike ricor tshift align tlrc \\
                            volreg blur mask regress        \\
                    -copy_anat sb23/sb23_mpra+orig          \\
                    -tcat_remove_first_trs 3                \\
                    -ricor_regs_nfirst 3                    \\
                    -ricor_regs sb23/RICOR/r*.slibase.1D    \\
                    -volreg_align_e2a                       \\
                    -volreg_tlrc_warp                       \\
                    -blur_size 6                            \\
                    -regress_motion_per_run                 \\
                    -regress_censor_motion 0.2              \\
                    -regress_bandpass 0.01 0.1              \\
                    -regress_apply_mot_types demean deriv   \\
                    -regress_run_clustsim no                \\
                    -regress_est_blur_epits                 \\
                    -regress_est_blur_errts

    Example 6. A simple task example, based on AFNI_data6. ~2~

       This example has changed to more closely correspond with the
       the class analysis example, AFNI_data6/FT_analysis/s05.ap.uber.

       The tshift block will interpolate each voxel time series to adjust
       for differing slice times, where the result is more as if each
       entire volume were acquired at the beginning of the TR.

       The 'align' block implies using align_epi_anat.py to align the
       anatomy with the EPI.  Extra options specify using lpc+ZZ for the
       cost function (probably more robust than lpc), and -giant_move (in
       case the anat and EPI start a bit far apart).  This block computes
       the anat to EPI transformation matrix, which will then be inverted
       in the volreg block, based on -volreg_align_e2a.

       Also, compute the transformation of the anatomy to MNI space, using
       affine registration (for speed in this simple example) to align to
       the MNI 2009 template.

       In the volreg block, align the EPI to the MIN_OUTLIER volume (a
       low-motion volume, determined from the data).  Then concatenate all
       EPI transformations, warping the EPI to standard space in one step
       (without multiple resampling operations), combining:

          EPI  ->  EPI base  ->  anat  ->  MNI 2009 template

       The standard space transformation is applied to the EPI due to
       specifying -volreg_tlrc_warp.

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

       The regression model starts from the full time series, for time
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

          afni_proc.py                                                \\
             -subj_id FT.e6                                           \\
             -copy_anat FT/FT_anat+orig                               \\
             -dsets FT/FT_epi_r?+orig.HEAD                            \\
             -blocks tshift align tlrc volreg blur mask scale regress \\
             -radial_correlate_blocks tcat volreg                     \\
             -tcat_remove_first_trs 2                                 \\
             -align_opts_aea -cost lpc+ZZ -giant_move                 \\
             -tlrc_base MNI152_2009_template.nii.gz                   \\
             -volreg_align_to MIN_OUTLIER                             \\
             -volreg_align_e2a                                        \\
             -volreg_tlrc_warp                                        \\
             -blur_size 4.0                                           \\
             -regress_stim_times FT/AV1_vis.txt FT/AV2_aud.txt        \\
             -regress_stim_labels vis aud                             \\
             -regress_basis 'BLOCK(20,1)'                             \\
             -regress_opts_3dD -jobs 2 -gltsym 'SYM: vis -aud'        \\
                 -glt_label 1 V-A                                     \\
             -regress_motion_per_run                                  \\
             -regress_censor_motion 0.3                               \\
             -regress_censor_outliers 0.05                            \\
             -regress_compute_fitts                                   \\
             -regress_make_ideal_sum sum_ideal.1D                     \\
             -regress_est_blur_epits                                  \\
             -regress_est_blur_errts                                  \\
             -regress_run_clustsim no                                 \\
             -execute

     * One could also use ANATICOR with task (e.g. -regress_anaticor_fast)
       in the case of -regress_reml_exec.  3dREMLfit supports voxelwise
       regression, but 3dDeconvolve does not.

    Example 6b. A modern task example, with preferable options. ~2~

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
       
          afni_proc.py                                                \\
             -subj_id FT.e6                                           \\
             -copy_anat Qwarp/anat_warped/anatSS.FT.nii               \\
             -anat_has_skull no                                       \\
             -anat_follower anat_w_skull anat FT/FT_anat+orig         \\
             -dsets FT/FT_epi_r?+orig.HEAD                            \\
             -blocks tshift align tlrc volreg blur mask scale regress \\
             -radial_correlate_blocks tcat volreg                     \\
             -tcat_remove_first_trs 2                                 \\
             -align_opts_aea -cost lpc+ZZ -giant_move -check_flip     \\
             -tlrc_base MNI152_2009_template_SSW.nii.gz               \\
             -tlrc_NL_warp                                            \\
             -tlrc_NL_warped_dsets anatQQ.FT.nii                      \\
                                   anatQQ.FT.aff12.1D                 \\
                                   anatQQ.FT_WARP.nii                 \\
             -volreg_align_to MIN_OUTLIER                             \\
             -volreg_align_e2a                                        \\
             -volreg_tlrc_warp                                        \\
             -mask_epi_anat yes                                       \\
             -blur_size 4.0                                           \\
             -regress_stim_times FT/AV1_vis.txt FT/AV2_aud.txt        \\
             -regress_stim_labels vis aud                             \\
             -regress_basis 'BLOCK(20,1)'                             \\
             -regress_opts_3dD -jobs 2 -gltsym 'SYM: vis -aud'        \\
                 -glt_label 1 V-A                                     \\
             -regress_motion_per_run                                  \\
             -regress_censor_motion 0.3                               \\
             -regress_censor_outliers 0.05                            \\
             -regress_3dD_stop                                        \\
             -regress_reml_exec                                       \\
             -regress_compute_fitts                                   \\
             -regress_make_ideal_sum sum_ideal.1D                     \\
             -regress_est_blur_epits                                  \\
             -regress_est_blur_errts                                  \\
             -regress_run_clustsim no                                 \\
             -html_review_style pythonic                              \\
             -execute 

       To compare one's own command against this one, consider adding
            -compare_opts 'example 6b'
       to the end of (or anywhere in) the current command, as in:

            afni_proc.py ... my options ...   -compare_opts 'example 6b'

    Example 7. Apply some esoteric options. ~2~

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

            afni_proc.py -subj_id sb23.e7.esoteric                     \\
                    -dsets sb23/epi_r??+orig.HEAD                      \\
                    -blocks tshift align tlrc volreg blur mask         \\
                            scale regress                              \\
                    -copy_anat sb23/sb23_mpra+orig                     \\
                    -tcat_remove_first_trs 3                           \\
                    -align_opts_aea -cost lpc+ZZ                       \\
                    -tlrc_base MNI152_2009_template.nii.gz             \\
                    -tlrc_NL_warp                                      \\
                    -volreg_align_to MIN_OUTLIER                       \\
                    -volreg_align_e2a                                  \\
                    -volreg_tlrc_warp                                  \\
                    -mask_epi_anat yes                                 \\
                    -blur_size 4                                       \\
                    -blur_in_automask                                  \\
                    -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                    -regress_stim_types times times times              \\
                                        AM2   AM2   AM2                \\
                                        times times times              \\
                    -regress_stim_labels tneg tpos tneu                \\
                                         eneg epos eneu                \\
                                         fneg fpos fneu                \\
                    -regress_basis_multi                               \\
                       'BLOCK(30,1)' 'TENT(0,45,16)' 'BLOCK(30,1)'     \\
                       'BLOCK(30,1)' 'TENT(0,45,16)' 'BLOCK(30,1)'     \\
                       'BLOCK(30,1)' 'TENT(0,45,16)' 'BLOCK(30,1)'     \\
                    -regress_apply_mot_types demean deriv              \\
                    -regress_motion_per_run                            \\
                    -regress_censor_motion 0.3                         \\
                    -regress_censor_outliers 0.1                       \\
                    -regress_compute_fitts                             \\
                    -regress_opts_3dD                                  \\
                        -bout                                          \\
                        -gltsym 'SYM: +eneg -fneg'                     \\
                        -glt_label 1 eneg_vs_fneg                      \\
                        -jobs 4                                        \\
                    -regress_run_clustsim no                           \\
                    -regress_est_blur_epits                            \\
                    -regress_est_blur_errts

    Example 8. Surface-based analysis. ~2~

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

            afni_proc.py -subj_id FT.surf                            \\
                -blocks tshift align volreg surf blur scale regress  \\
                -copy_anat FT/FT_anat+orig                           \\
                -dsets FT/FT_epi_r?+orig.HEAD                        \\
                -surf_anat FT/SUMA/FTmb_SurfVol+orig                 \\
                -surf_spec FT/SUMA/FTmb_?h.spec                      \\
                -tcat_remove_first_trs 2                             \\
                -align_opts_aea -cost lpc+ZZ                         \\
                -volreg_align_to third                               \\
                -volreg_align_e2a                                    \\
                -blur_size 6                                         \\
                -regress_stim_times FT/AV1_vis.txt FT/AV2_aud.txt    \\
                -regress_stim_labels vis aud                         \\
                -regress_basis 'BLOCK(20,1)'                         \\
                -regress_motion_per_run                              \\
                -regress_censor_motion 0.3                           \\
                -regress_opts_3dD                                    \\
                    -jobs 2                                          \\
                    -gltsym 'SYM: vis -aud' -glt_label 1 V-A

    Example 9. Resting state analysis (modern): ~2~

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

            afni_proc.py -subj_id subj123                                \\
              -dsets epi_run1+orig.HEAD                                  \\
              -copy_anat anat+orig                                       \\
              -blocks despike tshift align tlrc volreg blur mask         \\
                      scale regress                                      \\
              -tcat_remove_first_trs 3                                   \\
              -tlrc_base MNI152_2009_template.nii.gz                     \\
              -tlrc_NL_warp                                              \\
              -volreg_align_e2a                                          \\
              -volreg_tlrc_warp                                          \\
              -mask_epi_anat yes                                         \\
              -blur_size 4                                               \\
              -regress_censor_motion 0.2                                 \\
              -regress_censor_outliers 0.05                              \\
              -regress_bandpass 0.01 0.1                                 \\
              -regress_apply_mot_types demean deriv                      \\
              -regress_est_blur_epits                                    \\
              -regress_est_blur_errts

    Example 9b. Resting state analysis with ANATICOR. ~2~

       Like example #9, but also regress out the signal from locally
       averaged white matter.  The only change is adding the option
       -regress_anaticor.

       Note that -regress_anaticor implies options -mask_segment_anat and
       -mask_segment_erode.

            afni_proc.py -subj_id subj123                                \\
              -dsets epi_run1+orig.HEAD                                  \\
              -copy_anat anat+orig                                       \\
              -blocks despike tshift align tlrc volreg blur mask         \\
                      scale regress                                      \\
              -tcat_remove_first_trs 3                                   \\
              -tlrc_base MNI152_2009_template.nii.gz                     \\
              -tlrc_NL_warp                                              \\
              -volreg_align_e2a                                          \\
              -volreg_tlrc_warp                                          \\
              -mask_epi_anat yes                                         \\
              -blur_size 4                                               \\
              -regress_anaticor                                          \\
              -regress_censor_motion 0.2                                 \\
              -regress_censor_outliers 0.05                              \\
              -regress_bandpass 0.01 0.1                                 \\
              -regress_apply_mot_types demean deriv                      \\
              -regress_est_blur_epits                                    \\
              -regress_est_blur_errts

    Example 10. Resting state analysis, with tissue-based regressors. ~2~

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

            afni_proc.py -subj_id subj123                                \\
              -dsets epi_run1+orig.HEAD                                  \\
              -copy_anat anat+orig                                       \\
              -blocks despike tshift align tlrc volreg blur mask         \\
                      scale regress                                      \\
              -tcat_remove_first_trs 3                                   \\
              -align_opts_aea -cost lpc+ZZ                               \\
              -tlrc_base MNI152_2009_template.nii.gz                     \\
              -tlrc_NL_warp                                              \\
              -volreg_align_to MIN_OUTLIER                               \\
              -volreg_align_e2a                                          \\
              -volreg_tlrc_warp                                          \\
              -blur_size 4                                               \\
              -mask_epi_anat yes                                         \\
              -mask_segment_anat yes                                     \\
              -mask_segment_erode yes                                    \\
              -regress_censor_motion 0.2                                 \\
              -regress_censor_outliers 0.05                              \\
              -regress_bandpass 0.01 0.1                                 \\
              -regress_apply_mot_types demean deriv                      \\
              -regress_ROI WMe                                           \\
              -regress_est_blur_epits                                    \\
              -regress_est_blur_errts

    Example 10b. Resting state analysis, as 10a with 3dRSFC. ~2~

        This is for band passing and computation of ALFF, etc.

      * This will soon use a modified 3dRSFC.

        Like example #10, but add -regress_RSFC to bandpass via 3dRSFC.
        Skip censoring and regression band passing because of the bandpass
        operation in 3dRSFC.

        To correspond to common tractography, this example stays in orig
        space (no 'tlrc' block, no -volreg_tlrc_warp option).  Of course,
        going to standard space is an option.

            afni_proc.py -subj_id subj123                                \\
              -dsets epi_run1+orig.HEAD                                  \\
              -copy_anat anat+orig                                       \\
              -blocks despike tshift align volreg blur mask scale regress \\
              -tcat_remove_first_trs 3                                   \\
              -volreg_align_e2a                                          \\
              -blur_size 6.0                                             \\
              -mask_apply epi                                            \\
              -mask_segment_anat yes                                     \\
              -mask_segment_erode yes                                    \\
              -regress_bandpass 0.01 0.1                                 \\
              -regress_apply_mot_types demean deriv                      \\
              -regress_ROI WMe                                           \\
              -regress_RSFC                                              \\
              -regress_run_clustsim no                                   \\
              -regress_est_blur_errts

    Example 11. Resting state analysis (now even more modern :). ~2~

     o Yes, censor (outliers and motion) and despike.
     o Align the anatomy and EPI using the lpc+ZZ cost function, rather
       than the default lpc one.  Apply -giant_move, in case the datasets
       do not start off well-aligned.  Include -check_flip for good measure.
     o Register EPI volumes to the one which has the minimum outlier
          fraction (so hopefully the least motion).
     o Use non-linear registration to MNI template (non-linear 2009_template).
       * This adds a lot of processing time.
       * Let @SSwarper align to template MNI152_2009_template_SSW.nii.gz.
         Then use the resulting datasets in the afni_proc.py command below
         via -tlrc_NL_warped_dsets.
              @SSwarper -input FT_anat.nii         \\
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
     o Erode FS white matter and ventricle masks before application.
     o Bring along FreeSurfer parcellation datasets:
         - aaseg : NN interpolated onto the anatomical grid
         - aeseg : NN interpolated onto the EPI        grid
       * These 'aseg' follower datasets are just for visualization,
         they are not actually required for the analysis.
     o Compute average correlation volumes of the errts against the
       the gray matter (aeseg) and ventricle (FSVent) masks.
     o Run @radial_correlate at the ends of the tcat, volreg and regress
       blocks.  If ANATICOR is being used to remove a scanner artifact, 
       the errts radcor images might show the effect of this.

       Note: it might be reasonable to use either set of blur estimates
             here (from epits or errts).  The epits (uncleaned) dataset
             has all of the noise (though what should be considered noise
             in this context is not clear), while the errts is motion
             censored.  For consistency in resting state, it would be
             reasonable to stick with epits.  They will likely be almost
             identical.

        afni_proc.py -subj_id FT.11.rest                                 \\
          -blocks despike tshift align tlrc volreg blur mask             \\
                  scale regress                                          \\
          -radial_correlate_blocks tcat volreg regress                   \\
          -copy_anat anatSS.FT.nii                                       \\
          -anat_has_skull no                                             \\
          -anat_follower anat_w_skull anat anatU.FT.nii                  \\
          -anat_follower_ROI aaseg anat aparc.a2009s+aseg_REN_all.nii.gz \\
          -anat_follower_ROI aeseg epi  aparc.a2009s+aseg_REN_all.nii.gz \\
          -anat_follower_ROI FSvent epi fs_ap_latvent.nii.gz             \\
          -anat_follower_ROI FSWe epi fs_ap_wm.nii.gz                    \\
          -anat_follower_erode FSvent FSWe                               \\
          -dsets FT_epi_r?+orig.HEAD                                     \\
          -tcat_remove_first_trs 2                                       \\
          -align_opts_aea -cost lpc+ZZ -giant_move -check_flip           \\
          -tlrc_base MNI152_2009_template_SSW.nii.gz                     \\
          -tlrc_NL_warp                                                  \\
          -tlrc_NL_warped_dsets anatQQ.FT.nii                            \\
                                anatQQ.FT.aff12.1D                       \\
                                anatQQ.FT_WARP.nii                       \\
          -volreg_align_to MIN_OUTLIER                                   \\
          -volreg_align_e2a                                              \\
          -volreg_tlrc_warp                                              \\
          -blur_size 4                                                   \\
          -mask_epi_anat yes                                             \\
          -regress_motion_per_run                                        \\
          -regress_ROI_PC FSvent 3                                       \\
          -regress_ROI_PC_per_run FSvent                                 \\
          -regress_make_corr_vols aeseg FSvent                           \\
          -regress_anaticor_fast                                         \\
          -regress_anaticor_label FSWe                                   \\
          -regress_censor_motion 0.2                                     \\
          -regress_censor_outliers 0.05                                  \\
          -regress_apply_mot_types demean deriv                          \\
          -regress_est_blur_epits                                        \\
          -regress_est_blur_errts                                        \\
          -html_review_style pythonic

    Example 11b. Similar to 11, but without FreeSurfer. ~2~

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

            afni_proc.py -subj_id FT.11b.rest                            \\
              -blocks despike tshift align tlrc volreg blur mask         \\
                      scale regress                                      \\
              -copy_anat FT_anat+orig                                    \\
              -dsets FT_epi_r?+orig.HEAD                                 \\
              -tcat_remove_first_trs 2                                   \\
              -align_opts_aea -cost lpc+ZZ                               \\
              -tlrc_base TT_N27+tlrc                                     \\
              -tlrc_NL_warp                                              \\
              -volreg_align_to MIN_OUTLIER                               \\
              -volreg_align_e2a                                          \\
              -volreg_tlrc_warp                                          \\
              -volreg_warp_dxyz 2.5                                      \\
              -blur_size 4                                               \\
              -mask_segment_anat yes                                     \\
              -mask_segment_erode yes                                    \\
              -mask_import Tvent template_ventricle_2.5mm+tlrc           \\
              -mask_intersect Svent CSFe Tvent                           \\
              -mask_epi_anat yes                                         \\
              -regress_motion_per_run                                    \\
              -regress_ROI_PC Svent 3                                    \\
              -regress_ROI_PC_per_run Svent                              \\
              -regress_make_corr_vols WMe Svent                          \\
              -regress_anaticor_fast                                     \\
              -regress_censor_motion 0.2                                 \\
              -regress_censor_outliers 0.05                              \\
              -regress_apply_mot_types demean deriv                      \\
              -regress_est_blur_epits                                    \\
              -regress_est_blur_errts                                    \\
              -regress_run_clustsim yes

    Example 12 background: Multi-echo data processing. ~2~

     Processing multi-echo data should be similar to single echo data,
     except for perhaps:

        combine         : the addition of a 'combine' block
        -dsets_me_echo  : specify ME data, per echo
        -dsets_me_run   : specify ME data, per run (alternative to _echo)
        -echo_times     : specify echo times (if needed)
        -combine_method : specify method to combine echoes (if any)

     An afni_proc.py command might be updated to include something like:

        afni_proc.py ...                                     \\
            -blocks tshift align tlrc volreg mask combine    \\
                    blur scale regress                       \\
            -dsets_me_echo epi_run*_echo_01.nii              \\
            -dsets_me_echo epi_run*_echo_02.nii              \\
            -dsets_me_echo epi_run*_echo_03.nii              \\
            -echo_times 15 30.5 41                           \\
            ...                                              \\
            -mask_epi_anat yes                               \\
            -combine_method OC                               \\
            ...                                              \\


    Example 12a. Multi-echo data processing - very simple. ~2~

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

            afni_proc.py -subj_id FT.12a.ME                 \\
              -blocks tshift align tlrc volreg mask blur    \\
              -copy_anat FT_anat+orig                       \\
              -dsets_me_run epi_run1_echo*.nii              \\
              -reg_echo 2                                   \\
              -tcat_remove_first_trs 2                      \\
              -volreg_align_to MIN_OUTLIER                  \\
              -volreg_align_e2a                             \\
              -volreg_tlrc_warp

    Example 12b. Multi-echo data processing - OC resting state. ~2~

     Still keep this simple, mostly focusing on ME options, plus standard
     ones for resting state.

     o This example uses 3 echoes of data across just 1 run.
        - so use a single -dsets_me_run option to input EPI datasets
     o Echo 2 is used to drive registration for all echoes.
        - That is the default, but it is good to be explicit.
     o The echoes are combined via the 'combine' block.
     o So -echo_times is used to provided them.

            afni_proc.py -subj_id FT.12a.ME                 \\
              -blocks tshift align tlrc volreg mask combine \\
                      blur scale regress                    \\
              -copy_anat FT_anat+orig                       \\
              -dsets_me_run epi_run1_echo*.nii              \\
              -echo_times 15 30.5 41                        \\
              -reg_echo 2                                   \\
              -tcat_remove_first_trs 2                      \\
              -align_opts_aea -cost lpc+ZZ                  \\
              -tlrc_base MNI152_2009_template.nii.gz        \\
              -tlrc_NL_warp                                 \\
              -volreg_align_to MIN_OUTLIER                  \\
              -volreg_align_e2a                             \\
              -volreg_tlrc_warp                             \\
              -mask_epi_anat yes                            \\
              -combine_method OC                            \\
              -blur_size 4                                  \\
              -regress_motion_per_run                       \\
              -regress_censor_motion 0.2                    \\
              -regress_censor_outliers 0.05                 \\
              -regress_apply_mot_types demean deriv         \\
              -regress_est_blur_epits

    Example 12c. Multi-echo data processing - ME-ICA resting state. ~2~

     As above, but run tedana.py for MEICA denoising.

     o Since tedana.py will mask the data, it may be preferable to
       blur only within that mask (-blur_in_mask yes).
     o A task analysis using tedana might look much the same,
       but with the extra -regress options for the tasks.

            afni_proc.py -subj_id FT.12a.ME                 \\
              -blocks tshift align tlrc volreg mask combine \\
                      blur scale regress                    \\
              -copy_anat FT_anat+orig                       \\
              -dsets_me_run epi_run1_echo*.nii              \\
              -echo_times 15 30.5 41                        \\
              -reg_echo 2                                   \\
              -tcat_remove_first_trs 2                      \\
              -align_opts_aea -cost lpc+ZZ                  \\
              -tlrc_base MNI152_2009_template.nii.gz        \\
              -tlrc_NL_warp                                 \\
              -volreg_align_to MIN_OUTLIER                  \\
              -volreg_align_e2a                             \\
              -volreg_tlrc_warp                             \\
              -mask_epi_anat yes                            \\
              -combine_method tedana                        \\
              -blur_size 4                                  \\
              -blur_in_mask yes                             \\
              -regress_motion_per_run                       \\
              -regress_censor_motion 0.2                    \\
              -regress_censor_outliers 0.05                 \\
              -regress_apply_mot_types demean deriv         \\
              -regress_est_blur_epits

     Consider an alternative combine method, 'tedana_OC_tedort'.

    Example 13. Complicated ME, surface-based resting state example. ~2~

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

          # use data_dir variable for tracking inputs
          set data_dir = FT
          afni_proc.py -subj_id FT.complicated                          \\
             # == include ricor, mask/combine, and surf blocks          \\
             -blocks despike ricor tshift align volreg                  \\
                     mask combine surf blur scale regress               \\
             # == QC: radial_correlate_blocks                           \\
             -radial_correlate_blocks tcat volreg                       \\
             # == reverse blip distortion correction                    \\
             -blip_forward_dset $data_dir/FT_epi_r1+orig.HEAD'[0]'      \\
             -blip_reverse_dset $data_dir/FT_epi_r1+orig.HEAD'[0]'      \\
             # == anat and anat followers, vent PC regress              \\
             -copy_anat $data_dir/FT_anat+orig                          \\
             -anat_follower_ROI FSvent epi $data_dir/SUMA/FT_vent.nii   \\
             -anat_follower_erode FSvent                                \\
             -regress_ROI_PC FSvent 3                                   \\
             -regress_ROI_PC_per_run FSvent                             \\
             -regress_make_corr_vols FSvent                             \\
             # == multi-echo EPI, times, combine method                 \\
             -dsets_me_echo $data_dir/FT_epi_r?+orig.HEAD               \\
             -dsets_me_echo $data_dir/FT_epi_r?+orig.HEAD               \\
             -dsets_me_echo $data_dir/FT_epi_r?+orig.HEAD               \\
             -echo_times 11 22.72 34.44                                 \\
             -combine_method OC                                         \\
             -tcat_remove_first_trs 2                                   \\
             -tshift_interp -wsinc9                                     \\
             -mask_epi_anat yes                                         \\
             # == RETROICOR per run                                     \\
             -ricor_regs_nfirst 2                                       \\
             -ricor_regs $data_dir/fake.slibase.FT.r?.1D                \\
             -ricor_regress_method per-run                              \\
             # == alignment opts, and post volreg allineate             \\
             -align_opts_aea -cost lpc+ZZ -giant_move                   \\
             -volreg_align_to MIN_OUTLIER                               \\
             -volreg_align_e2a                                          \\
             -volreg_post_vr_allin yes                                  \\
             -volreg_pvra_base_index MIN_OUTLIER                        \\
             -volreg_warp_final_interp wsinc5                           \\
             # == surface analysis                                      \\
             -surf_anat $data_dir/SUMA/FT_SurfVol.nii                   \\
             -surf_spec $data_dir/SUMA/std.141.FT_?h.spec               \\
             # == blur size, censoring, other regression                \\
             -blur_size 6                                               \\
             -regress_censor_motion 0.2                                 \\
             -regress_censor_outliers 0.05                              \\
             -regress_motion_per_run                                    \\
             -regress_apply_mot_types demean deriv                      \\
             -html_review_style pythonic


--------------------------------------------------
-ask_me EXAMPLES:  ** NOTE: -ask_me is antiquated ** ~2~

    a1. Apply -ask_me in the most basic form, with no other options.

            afni_proc.py -ask_me

    a2. Supply input datasets.

            afni_proc.py -ask_me -dsets ED/ED_r*.HEAD

    a3. Same as a2, but supply the datasets in expanded form.
        No suffix (.HEAD) is needed when wildcards are not used.

            afni_proc.py -ask_me                          \\
                 -dsets ED/ED_r01+orig ED/ED_r02+orig     \\
                        ED/ED_r03+orig ED/ED_r04+orig     \\
                        ED/ED_r05+orig ED/ED_r06+orig     \\
                        ED/ED_r07+orig ED/ED_r08+orig     \\
                        ED/ED_r09+orig ED/ED_r10+orig

    a4. Supply datasets, stim_times files and labels.

            afni_proc.py -ask_me                                    \\
                    -dsets ED/ED_r*.HEAD                            \\
                    -regress_stim_times misc_files/stim_times.*.1D  \\
                    -regress_stim_labels ToolMovie HumanMovie       \\
                                         ToolPoint HumanPoint

"""
g_help_notes = """
==================================================
Many NOTE sections: ~1~
==================================================

--------------------------------------------------
GENERAL ANALYSIS NOTE: ~2~

How might one run a full analysis?  Here are some details to consider.

0. Expect to re-run the full analysis.  This might be to fix a mistake, to
   change applied options or to run with current software, to name a few
   possibilities.  So...

     - keep permanently stored input data separate from computed results
       (one should be able to easily delete the results to start over)
     - keep scripts in yet another location
     - use file naming that is consistent across subjects and groups,
       making it easy to script with

1. Script everything.  One should be able to carry out the full analysis
   just by running the main scripts.

   Learning is best done by typing commands and looking at data, including
   the input to and output from said commands.  But running an analysis for
   publication should not rely on typing complicated commands or pressing
   buttons in a GUI (graphical user interface).

     - it is easy to apply to new subjects
     - the steps can be clear and unambiguous (no magic or black boxes)
     - some scripts can be included with publication
       (e.g. an afni_proc.py command, with the AFNI version)

     - using a GUI relies on consistent button pressing, making it much
       more difficult to *correctly* repeat, or even understand

2. Analyze and perform quality control on new subjects promptly.

     - any problems with the acquisition would (hopefully) be caught early
     - can compare basic quality control measures quickly

3. LOOK AT YOUR DATA.  Quality control is best done by researchers.
   Software should not be simply trusted.

     - afni_proc.py processing scripts write guiding @ss_review_driver
       scripts for *minimal* per-subject quality control (i.e. at a
       minimum, run that for every subject)
     - initial subjects should be scrutinized (beyond @ss_review_driver)

     - concatenate anat_final datasets to look for consistency
     - concatenate final_epi datasets to look for consistency
     - run gen_ss_review_table.py on the out.ss_review*.txt files
       (making a spreadsheet to quickly scan for outlier subjects)

     - many issues can be detected by software, buy those usually just come
       as warnings to the researcher
     - similarly, some issues will NOT be detected by the software
     - for QC, software can assist the researcher, not replace them

     NOTE: Data from external sites should be heavily scrutinized,
           including any from well known public repositories.

4. Consider regular software updates, even as new subjects are acquired.
   This ends up requiring a full re-analysis at the end.

   If it will take a while (one year or more?) to collect data, update the
   software regularly (weekly?  monthly?).  Otherwise, the analysis ends up
   being done with old software.

      - analysis is run with current, rather than old software
      - will help detect changes in the software (good ones or bad ones)
      - at a minimum, more quality control tools tend to show up
      - keep a copy of the prior software version, in case comparisons are
        desired (@update.afni.binaries does keep one prior version)
      - the full analysis should be done with one software version, so once
        all datasets are collected, back up the current analysis and re-run
        the entire thing with the current software
      - keep a snapshot of the software package used for the analysis
      - report the software version in any publication

5. Here is a sample (tcsh) script that might run a basic analysis on
   one or more subjects:

   ======================================================================
   sample analysis script ~3~
   ======================================================================

   #!/bin/tcsh

   # --------------------------------------------------
   # note fixed top-level directories
   set data_root = /main/location/of/all/data

   set input_root = $data_root/scanner_data
   set output_root = $data_root/subject_analysis

   # --------------------------------------------------
   # get a list of subjects, or just use one (consider $argv)
   cd $input root
   set subjects = ( subj* )
   cd -

   # or perhaps just process one subject?
   set subjects = ( subj_017 )


   # --------------------------------------------------
   # process all subjects
   foreach subj_id ( $subjects )

      # --------------------------------------------------
      # note input and output directories
      set subj_indir = $input_root/$subj_id
      set subj_outdir = $output_root/$subj_id

      # --------------------------------------------------
      # if output dir exists, this subject has already been processed
      if ( -d $subj_outdir ) then
         echo "** results dir already exists, skipping subject $subj_id"
         continue
      endif

      # --------------------------------------------------
      # otherwise create the output directory, write an afni_proc.py
      # command to it, and fire it up

      mkdir -p $subj_outdir
      cd $subj_outdir

      # create a run.afni_proc script in this directory
      cat > run.afni_proc << EOF

      # notes:
      #   - consider different named inputs (rather than OutBrick)
      #   - verify how many time points to remove at start (using 5)
      #   - note which template space is preferable (using MNI)
      #   - consider non-linear alignment via -tlrc_NL_warp
      #   - choose blur size (using FWHM = 4 mm)
      #   - choose basis function (using BLOCK(2,1), for example)
      #   - assuming 4 CPUs for linear regression
      #   - afni_proc.py will actually run the proc script (-execute)


      afni_proc.py -subj_id $subj_id                          \\
          -blocks tshift align tlrc volreg blur mask regress  \\
          -copy_anat $subj_indir/anat+orig                    \\
          -dsets                                              \\
              $subj_indir/epi_r1+orig                         \\
              $subj_indir/epi_r2+orig                         \\
              $subj_indir/epi_r3+orig                         \\
          -tcat_remove_first_trs 5                            \\
          -align_opts_aea -cost lpc+ZZ                        \\
          -tlrc_base MNI152_2009_template.nii.gz              \\
          -tlrc_NL_warp                                       \\
          -volreg_align_to MIN_OUTLIER                        \\
          -volreg_align_e2a                                   \\
          -volreg_tlrc_warp                                   \\
          -blur_size 4.0                                      \\
          -regress_motion_per_run                             \\
          -regress_censor_motion 0.3                          \\
          -regress_reml_exec -regress_3dD_stop                \\
          -regress_stim_times                                 \\
              $stim_dir/houses.txt                            \\
              $stim_dir/faces.txt                             \\
              $stim_dir/doughnuts.txt                         \\
              $stim_dir/pizza.txt                             \\
          -regress_stim_labels                                \\
              house face nuts za                              \\
          -regress_basis 'BLOCK(2,1)'                         \\
          -regress_opts_3dD                                   \\
              -jobs 4                                         \\
              -gltsym 'SYM: house -face' -glt_label 1 H-F     \\
              -gltsym 'SYM: nuts -za'    -glt_label 2 N-Z     \\
          -regress_est_blur_errts                             \\
          -execute

   EOF
   # EOF terminates the 'cat > run.afni_proc' command, above
   # (it must not be indented in the script)

      # now run the analysis (generate proc and execute)
      tcsh run.afni_proc

   # end loop over subjects
   end

   ======================================================================

--------------------------------------------------
DIRECTORY STRUCTURE NOTE: ~2~

We are working to have a somewhat BIDS-like directory structure.  If our
tools know where to be able to find processed data, many things beyond the
single subject level can be automated.

Starting with a main STUDY (ds000210 in the example) tree, the directory
structure has individual subject input trees at the top level.  Each
subject directory (e.g. sub-001) would contain all of the original data for
that subject, possibly including multiple tasks or resting state data,
anatomical, DWI, etc.  The example includes 1 run of rest, 3 runs of the
cuedSGT task data, and corresponding cuedSGT timing files.

Processed data would then go under a 'derivatives' directory under STUDY
(ds000210), with each sub-directory being a single analysis.  The example
shows a preperatory analysis to do non-linear registration, plus a resting
state analysis and the cuedSGT analysis.

In our case, assuming one is using non-linear registration, the derivatives
directory might contain directories like:

    AFNI_01_SSwarp      - single subject non-linear warp results
                          (these would be used as input to afni_proc.py
                          in any other analyses)

    AFNI_02_task_XXXX   - some main analysis, including single subject
                          (via afni_proc.py?) and possibly group results

    AFNI_03_rest        - maybe a resting state analysis, for example


      So a sample directory tree might look something like:
   
      ds000210 (main study directory)
      |        \\                \\
      sub-001  sub-002           derivatives
     /                           |            \\               \\
    --anat                    AFNI_01_SSwarp  AFNI_02_rest    AFNI_03_cuedSGT
         \\                    |               |        \\
         sub-001_T1w.nii.gz   sub-001         sub-001   sub-002  ...
                               |               |
    --func                    WARP.nii        cmd.afni_proc
         \\                                    proc.sub-001
         sub-001_task-rest_run-01.nii.gz      output.proc.sub-001
         sub-001_task-cuedSGT_run-01.nii.gz   sub-001.results
         sub-001_task-cuedSGT_run-02.nii.gz   stim_timing
         sub-001_task-cuedSGT_run-03.nii.gz
         sub-001_task-cuedSGT_run-01_events.tsv
         sub-001_task-cuedSGT_run-02_events.tsv
         sub-001_task-cuedSGT_run-03_events.tsv

--------------------------------------------------
QUALITY CONTROL NOTE: ~2~

Look at the data.

Nothing replaces a living human performing quality control checks by
looking at the data.  And the more a person looks at the data, the better
they get at spotting anomalies.

There are 3 types of QC support generated by afni_proc.py, a static QC
HTML page, scripts to help someone review the data, and individual text
or image files.

    ----------------------------------------------------------------------
    QC_$subj/index.html     - auto-generated web page

       This web page and enclosing QC_$subj directory are automatically
       generated by a sequence of programs:

            apqc_make_tcsh.py
            @ss_review_html
            apqc_make_html.py

       This web page was made to encapsulate the @ss_review_driver results
       in a static image, and will be enhanced separately.

    ----------------------------------------------------------------------
    scripts (the user can run from the results directory):

       @epi_review.FT               - view original (post-SS) EPI data
       @ss_review_basic             - show basic QC measures, in text
                                      (automatically run)
       @ss_review_driver            - minimum recommended QC review
       @ss_review_driver_commands   - same, as pure commands

       @ss_review_html              - generate HTML pages under QC_$subj
                                      (automatically run)

       Notably, the @ss_review_driver script is recommended as the minimum
       QC to perform on every subject.

    ----------------------------------------------------------------------
    other files or datasets:   (* shown or reviewed by @ss_review_driver)

    *  3dDeconvolve.err

          This contains any warnings (or errors) from 3dDeconvolve.  This
          will be created even if 3dREMLfit is run.

    *  anat_final.$subj

          This AFNI dataset should be registered with the final stats
          (including final_epi_vr_base) and with any applied template.
          There is also a version with the skull, anat_w_skull_warped.

    *  blur_est.$subj.1D

          This (text) file has the mixed-model ACF (and possibly the FWHM)
          parameter estimates of the blur.

       Classes

          If 3dSeg is run for anatomical segmentation, this AFNI dataset
          contains the results, a set of masks per tissue class.  The
          white matter mask from this might be used for ANATICOR, for
          example.

       corr_brain

          This AFNI dataset shows the correlation of every voxel with the
          global signal (average time series over brain mask).

          One can request other corr_* datasets, based on any tissue or ROI
          mask.  See -regress_make_corr_vols for details.

    *  dfile_rall.1D (and efile.r??.1D)

          This contains the 6 estimated motion parameters across all runs.
          These parameters are generally used as regressors of no interest,
          hopefully per run.  They are also used to generate the enorm time
          series, which is then used for censoring.

       files_ACF

          This directory contains ACF values at different radii per run.
          One can plot them using something like:

            set af = files_ACF/out.3dFWHMx.ACF.errts.r01.1D
            1dplot -one -x $af'[0]' $af'[1,2,3]'

    *  final_epi_vr_base

          This dataset is of the EPI volume registration base (used by
          3dvolreg), warped to the final space.  It should be in alignment
          with the anat_final dataset (and the template).

       fitts.$subj

          This dataset contains the model fit to the time series data.
          One can view these time series together in afni using the
          Dataset #N plugin.

       full_mask.$subj

          This dataset is a brain mask based on the EPI data, generated
          by 3dAutomask.  Though the default is to apply it as part of the
          main regression, it is used for computations like ACF and TSNR.

       ideal_*.1D

          These time series text files are the ideal regressors of
          interest, if appropriate to calculate.

       mat.basewarp.aff12.1D

          This is used to create the final_epi_vr_base dataset.

          Assuming no non-linear registration (including distortion
          correction), then this matrix holds the combined affine
          transformation of the EPI to anat and to standard space,
          as applied to the volume registration base (it does not contain
          motion correction transformations).

          Time series registration matrices that include motion correction
          are in mat.r*.warp.aff12.1D (i.e. one file per run).

          In the case of non-linear registration, there is no single file
          representing the combined transformation, as it is computed just
          to apply the transformation by 3dNwarpApply.  This command can be
          found in the proc script or as the last HISTORY entry seen from
          the output of "3dinfo final_epi_vr_base".

    *  motion_${subj}_enorm.1D

          This time series text file is the L2 (Euclidean) norm of the
          first (backward) differences of the motion parameters.  The
          values represent time point to time point estimated motion, and
          they are used for censoring.  Values are zero at the beginning of
          each run (motion is not computed across runs).

          A high average of these numbers, particularly after the numbers
          themselves are censored, is justification for dropping a subject.
          This average is reported by the @ss_review scripts.

       motion_${subj}_censor.1D

          This is a binary 0/1 time series (matching enorm, say), that
          distinguishes time points which would be censored (0) from those
          which would not (1).  It is based on the enorm time series and
          the -regress_censor_motion limit, with a default to censor in
          pairs of time points.  There may be a combined censor file, if
          outlier censoring is done (or if a user censor file is input).

       motion_demean.1D

          This is the same as dfile_rall.1D, the motion parameters as
          estimated by 3dvolreg, except the the mean per run has been
          removed.

       motion_deriv.1D

          This contains the first (backward) differences from either
          motion_demean.1D or dfile_rall.1D.  Values are zero at the start
          of each run.

       out.allcostX.txt

          This holds anat/EPI registration costs for all cost functions.
          It might be informational to evaluate alignment across subjects
          and cost functions.

    *  out.cormat_warn.txt

          This contains warnings about a high correlation between any pair
          of regressors in the main regression matrix, including baseline
          terms.

    *  out.gcor.1D

          This contains the global correlation, the average correlation
          between every pair of voxels in the residual time series dataset.
          This single value is reported by the @ss_review scripts.

       out.mask_ae_dice.txt

          This contains the Dice coefficient, evaluating the overlap
          between the anatomical and EPI brain masks.

       out.mask_ae_overlap.txt

          This contains general output from 3dOverlap, for evaluating the
          overlap between the anatomical and EPI brain masks.

       out.mask_at_dice.txt

          This contains the Dice coefficient evaluating the overlap
          between the anatomical and template brain masks.

    *  out.pre_ss_warn.txt

          This contains warnings about time point #0 in any run where it
          might be a pre-steady state time point, based on outliers.

    *  out.ss_review.txt

          This is the text output from @ss_review_basic.  Aside from being
          shown by the @ss_review scripts, it is useful for being compiled
          across subjects via gen_ss_review_table.py.

    *  outcount_rall.1D (and outcount.r??.1D)

          This is a time series of the fraction of the brain that is an
          outlier.  It can be used for censoring.

    *  sum_ideal.1D

          As suggested, this time series is the sum of all non-baseline
          regressors.  It is generated from X.nocensor.xmat.1D if censoring
          is done, and from X.xmat.1D otherwise.  This might help one find
          mistakes in stimulus timing, for example.

    *  TSNR_$subj

          This AFNI dataset contains the voxelwise TSNR after regression.
          The brainwise average is shown in @ss_review_basic.

      X.xmat.1D

          This is the complete regression matrix, created by 3dDeconvolve.
          One can view it using 1dplot.  It contains all regressors except
          for any voxelwise ones (e.g. for ANATICOR).

      X.nocensor.xmat.1D

          This is the same as X.xmat.1D, except the nothing is censored,
          so all time points are present.

    * X.stim.xmat.1D

          This (text) file has the non-baseline regressors (so presumably
          of interest), created by 3dDeconvolve.

--------------------------------------------------
RESTING STATE NOTE: ~2~

It is preferable to process resting state data using physio recordings
(for typical single-echo EPI data).  Without such recordings, bandpassing
is currently considered as the standard in the field of FMRI (though that
is finally starting to change).  Multi-echo acquisitions offer other
possibilities.

Comment on bandpassing:

    Bandpassing does not seem like a great method.

    Bandpassing is the norm right now.  However most TRs may be too long
    for this process to be able to remove the desired components of no
    interest.  On the flip side, if the TRs are short, the vast majority
    of the degrees of freedom are sacrificed just to do it.  Perhaps
    bandpassing will eventually go away, but it is the norm right now.

    Also, there is a danger with bandpassing and censoring in that subjects
    with a lot of motion may run out of degrees of freedom (for baseline,
    censoring, bandpassing and removal of other signals of no interest).
    Many papers have been published where a lot of censoring was done,
    many regressors of no interest were projected out, and there was a
    separate bandpass operation.  It is likely that many subjects should
    have ended up with negative degrees of freedom (were bandpassing
    implemented correctly), making the resulting signals useless (or worse,
    misleading garbage).  But without keeping track of it, researchers may
    not even know.

Bandpassing and degrees of freedom:

    Bandpassing between 0.01 and 0.1 means, from just the lowpass side,
    throwing away frequencies above 0.1.  So the higher the frequency of
    collected data (i.e. the smaller the TR), the higher the fraction of
    DoF will be thrown away.

    For example, if TR = 2s, then the Nyquist frequency (the highest
    frequency detectable in the data) is 1/(2*2) = 0.25 Hz.  That is to
    say, one could only detect something going up and down at a cycle rate
    of once every 4 seconds (twice the TR).

    So for TR = 2s, approximately 40% of the DoF are kept (0.1/0.25) and
    60% are lost (frequencies from 0.1 to 0.25) due to bandpassing.

    To generalize, Nyquist = 1/(2*TR), so the fraction of DoF kept is

        fraction kept = 0.1/Nyquist = 0.1/(1/(2*TR)) = 0.1*2*TR = 0.2*TR

    For example,

        at TR = 2 s,   0.4  of DoF are kept (60% are lost)
        at TR = 1 s,   0.2  of DoF are kept (80% are lost)
        at TR = 0.5 s, 0.1  of DoF are kept (90% are lost)
        at TR = 0.1 s, 0.02 of DoF are kept (98% are lost)

    Consider also:

        Shirer WR, Jiang H, Price CM, Ng B, Greicius MD
        Optimization of rs-fMRI pre-processing for enhanced signal-noise
            separation, test-retest reliability, and group discrimination
        Neuroimage. 2015 Aug 15;117:67-79.

        Gohel SR, Biswal BB
        Functional integration between brain regions at rest occurs in
            multiple-frequency bands
        Brain connectivity. 2015 Feb 1;5(1):23-34.

        Caballero-Gaudes C, Reynolds RC
        Methods for cleaning the BOLD fMRI signal
        Neuroimage. 2017 Jul 1;154:128-49

Application of bandpassing in afni_proc.py:

    In afni_proc.py, this is all done in a single regression model (removal
    of noise and baseline signals, bandpassing and censoring).  If some
    subject were to lose too many TRs due to censoring, this step would
    fail, as it should.

    There is an additional option of using simulated motion time series
    in the regression model, which should be more effective than higher
    order motion parameters, say.  This is done via @simulate_motion.

There are 3 main steps (generate ricor regs, pre-process, group analysis):

    step 0: If physio recordings were made, generate slice-based regressors
            using RetroTS.py.  Such regressors can be used by afni_proc.py
            via the 'ricor' processing block.

            RetroTS.m is Ziad Saad's MATLAB routine to convert the 2 time
            series into 13 slice-based regressors.  RetroTS.m requires the
            signal processing toolkit for MATLAB.

          * RetroTS.py is a conversion of RetroTS.m to python by J Zosky.
            It depends on scipy.  See "RetroTS.py -help" for details.

    step 1: analyze with afni_proc.py

            Consider these afni_proc.py -help examples:
               5b.  case of ricor and no bandpassing
               5c.  ricor and bandpassing and full registration
               9.   no ricor, but with bandpassing
               9b.  with WMeLocal (local white-matter, eroded) - ANATICOR
               10.  also with tissue-based regressors
               10b. apply bandpassing via 3dRSFC
               soon: extra motion regs via motion simulated time series
                     (either locally or not)
               11.  censor, despike, non-linear registration,
                    no bandpassing, fast ANATICOR regression,
                    FreeSurfer masks for ventricle/WM regression
                  * see "FREESURFER NOTE" for more details

        processing blocks:

            despike (shrink large spikes in time series)
            ricor   (if applicable, remove the RetroTS regressors)
            tshift  (correct for slice timing)
            align   (figure out alignment between anat and EPI)
            tlrc    (figure out alignment between anat and template)
            volreg  (align anat and EPI together, and to standard template)
            blur    (apply desired FWHM blur to EPI data)
            scale   (optional, e.g. before seed averaging)
            regress (polort, motion, mot deriv, bandpass, censor,
                     ANATICOR/WMeLocal, tedana)
                    (depending on chosen options)
                    soon: extra motion regressors (via motion simulation)

            ==> "result" is errts dataset, "cleaned" of known noise sources

    step 2: correlation analysis, perhaps with 3dGroupInCorr

        The inputs to this stage are the single subject errts datasets.

        Ignoring 3dGroupInCorr, the basic steps in a correlation analysis
        (and corresponding programs) are as follows.  This may be helpful
        for understanding the process, even when using 3dGroupInCorr.

            a. choose a seed voxel (or many) and maybe a seed radius

            for each subject:

               b. compute time series from seed
                  (3dmaskave or 3dROIstats)
               c. generate correlation map from seed TS
                  (3dTcorr1D (or 3dDeconvolve or 3dfim+))
               d. normalize R->"Z-score" via Fisher's z-transform
                  (3dcalc -expr atanh)

            e. perform group test, maybe with covariates
               (3dttest++: 1-sample, 2-sample or paired)

        To play around with a single subject via InstaCorr:

            a. start afni (maybe show images of both anat and EPI)
            b. start InstaCorr plugin from menu at top right of afni's
               Define Overlay panel
            c. Setup Icorr:
                c1. choose errts dataset
                   (no Start,End; no Blur (already done in pre-processing))
                c2. Automask -> No; choose mask dataset: full_mask
                c3. turn off Bandpassing (already done, if desired)
            d. in image window, show correlations
                d1. go to seed location, right-click, InstaCorr Set
                OR
                d1. hold ctrl-shift, hold left mouse button, drag
            e. have endless fun

        To use 3dGroupInCorr:

            a. run 3dSetupGroupIncorr with mask, labels, subject datasets
               (run once per group of subjects), e.g.

                    3dSetupGroupInCorr                \\
                        -labels subj.ID.list.txt      \\
                        -prefix sic.GROUP             \\
                        -mask EPI_mask+tlrc           \\
                        errts_subj1+tlrc              \\
                        errts_subj2+tlrc              \\
                        errts_subj3+tlrc              \\
                            ...                       \\
                        errts_subjN+tlrc

                ==> sic.GROUP.grpincorr.niml (and .grpincorr.data)

            b. run 3dGroupInCorr on 1 or 2 sic.GROUP datasets, e.g.

               Here are steps for running 3dGroupInCorr via the afni GUI.
               To deal with computers that have multiple users, consider
               specifying some NIML port block that others are not using.
               Here we use port 2 (-npb 2), just to choose one.

               b1. start afni:

                    afni -niml -npb 2

               b2. start 3dGroupInCorr

                    3dGroupInCorr -npb 2                    \\
                        -setA sic.horses.grpincorr.niml     \\
                        -setB sic.moths.grpincorr.niml      \\
                        -labelA horses -labelB moths        \\
                        -covaries my.covariates.txt         \\
                        -center SAME -donocov -seedrad 5

               b3. play with right-click -> InstaCorr Set or
                  hold ctrl-shift/hold left mouse and drag slowly

               b4. maybe save any useful dataset via
                  Define Datamode -> SaveAs OLay (and give a useful name)

            b'. alternative, generate result dataset in batch mode, by
                adding -batch and some parameters to the 3dGIC command

                e.g.  -batch XYZAVE GIC.HvsM.PFC 4 55 26

                In such a case, afni is not needed at all.  The resulting
                GIC.HvsM.PFC+tlrc dataset would be written out without any
                need to start the afni GUI.  This works well since seed
                coordinates for group tests are generally known in advance.

                See the -batch option under "3dGroupInCorr -help" for many
                details and options.

            c. threshold/clusterize resulting datasets, just as with a
               task analysis

               (afni GUI, 3dClusterize)

--------------------------------------------------
FREESURFER NOTE: ~2~

FreeSurfer output can be used for a few things in afni_proc.py:

    - simple skull stripping (i.e. instead of 3dSkullStrip)
         *** we now prefer @SSwarper ***
    - running a surface-based analysis
    - using parcellation datasets for:
       - tissue-based regression
       - creating group probability maps
       - creating group atlases (e.g. maximum probability maps)

This NOTE mainly refers to using FreeSurfer parcellations for tissue-based
regression, as is done in Example 11.


First run FreeSurfer, then import to AFNI using @SUMA_Make_Spec_FS, then
make ventricle and white matter masks from the Desikan-Killiany atlas based
parcellation dataset, aparc+aseg.nii.

Note that the aparc.a2009s segmentations are based on the Destrieux atlas,
which might be nicer for probability maps, though the Desikan-Killiany
aparc+aseg segmentation is currently used for segmenting white matter and
ventricles.  I have not studied the differences.


Example 11 brings the aparc.a2009s+aseg segmentation along (for viewing or
atlas purposes, aligned with the result), though the white matter and
ventricle masks are based instead on aparc+aseg.nii.

    # run ) FreeSurfer on FT_anat.nii (NIFTI version of FT_anat+orig)
    3dcopy FT_anat+orig FT_anat.nii
    recon-all -all -subject FT -i FT_anat.nii

    # import to AFNI, in NIFTI format
    @SUMA_Make_Spec_FS -sid FT -NIFTI


  * Note, @SUMA_Make_Spec_FS now (as of 14 Nov, 2019) outputs ventricle
    and white matter masks, for possible use with afni_proc.py:

        SUMA/fs_ap_latvent.nii.gz
        SUMA/fs_ap_wm.nii.gz

Then FT_anat.nii (or FT_anat+orig), fs_ap_latvent.nii.gz and
fs_ap_wm.nii.gz (along with the basically unused
aparc.a2009s+aseg_REN_all.nii.gz) are passed to afni_proc.py.

--------------------------------------------------
TIMING FILE NOTE: ~2~

One issue that the user must be sure of is the timing of the stimulus
files (whether -regress_stim_files or -regress_stim_times is used).

The 'tcat' step will remove the number of pre-steady-state TRs that the
user specifies (defaulting to 0).  The stimulus files, provided by the
user, must match datasets that have had such TRs removed (i.e. the stim
files should start _after_ steady state has been reached).

--------------------------------------------------
MASKING NOTE: ~2~

The default operation of afni_proc.py has changed (as of 24 Mar, 2009).
Prior to that date, the default was to apply the 'epi' mask.  As of
17 Jun 2009, only the 'extents' mask is, if appropriate.

---

There may be 4 masks created by default, 3 for user evaluation and all for
possible application to the EPI data (though it may not be recommended).
The 4th mask (extents) is a special one that will be applied at volreg when
appropriate, unless the user specifies otherwise.

If the user chooses to apply one of the masks to the EPI regression (again,
not necessarily recommended), it is done via the option -mask_apply while
providing the given mask type (epi, anat, group or extents).

--> To apply a mask during regression, use -mask_apply.

Mask descriptions (afni_proc.py name, dataset name, short description):

1. epi ("full_mask") : EPI Automask

   An EPI mask dataset will be created by running '3dAutomask -dilate 1'
   on the EPI data after blurring.  The 3dAutomask command is executed per
   run, after which the masks are combined via a union operation.

2. anat ("mask_anat.$subj") : anatomical skull-stripped mask

   If possible, a subject anatomy mask will be created.  This anatomical
   mask will be created from the appropriate skull-stripped anatomy,
   resampled to match the EPI (that is output by 3dvolreg) and changed into
   a binary mask.

   This requires either the 'align' block or a tlrc anatomy (from the
   'tlrc' block, or just copied via '-copy_anat').  Basically, it requires
   afni_proc.py to know of a skull-stripped anatomical dataset.

   By default, if both the anat and EPI masks exist, the overlap between
   them will be computed for evaluation.

3. group ("mask_group") : skull-stripped @auto_tlrc base

   If possible, a group mask will be created.  This requires the 'tlrc'
   block, from which the @auto_tlrc -base dataset is chosen as the group
   anatomy.  It also requires '-volreg_warp_epi' so that the EPI is in
   standard space.  The group anatomy is then resampled to match the EPI
   and changed into a binary mask.

4. extents ("mask_extents") : mask based on warped EPI extents

   In the case of transforming the EPI volumes to match the anatomical
   volume (via either -volreg_align_e2a or -volreg_tlrc_warp), an extents
   mask will be created.  This is to avoid a motion artifact that arises
   when transforming from a smaller volume (EPI) to a larger one (anat).

** Danger Will Robinson! **

   This EPI extents mask is considered necessary because the align/warp
   transformation that is applied on top of the volreg alignment transform
   (applied at once), meaning the transformation from the EPI grid to the
   anatomy grid will vary per TR.

   The effect of this is seen at the edge voxels (extent edge), where a
   time series could be zero for many of the TRs, but have valid data for
   the rest of them.  If this timing just happens to correlate with any
   regressor, the result could be a strong "activation" for that regressor,
   but which would be just a motion based artifact.

   What makes this particularly bad is that if it does happen, it tends to
   happen for *a cluster* of many voxels at once, possibly an entire slice.
   Such an effect is compounded by any additional blur.  The result can be
   an entire cluster of false activation, large enough to survive multiple
   comparison corrections.

   Thanks to Laura Thomas and Brian Bones for finding this artifact.

-> To deal with this, a time series of all 1s is created on the original
   EPI grid space.  Then for each run it is warped with to the same list of
   transformations that is applied to the EPI data in the volreg step
   (volreg xform and either alignment to anat or warp to standard space).
   The result is a time series of extents of each original volume within
   the new grid.

   These volumes are then intersected over all TRs of all runs.  The final
   mask is the set of voxels that have valid data at every TR of every run.
   Yay.

5. Classes and Classes_resam: GM, WM, CSF class masks from 3dSeg

   By default, unless the user requests otherwise (-mask_segment_anat no),
   and if anat_final is skull-stripped, then 3dSeg will be used to segment
   the anatomy into gray matter, white matter and CSF classes.

   A dataset named Classes is the result of running 3dSeg, which is then
   resampled to match the EPI and named Classes_resam.

   If the user wanted to, this dataset could be used for regression of
   said tissue classes (or eroded versions).


--- masking, continued...

Note that it may still not be a good idea to apply any of the masks to the
regression, as it might then be necessary to intersect such masks across
all subjects, though applying the 'group' mask might be reasonable.

Why has the default been changed?

It seems much better not to mask the regression data in the single-subject
analysis at all, send _all_ of the results to group space, and apply an
anatomically-based mask there.  That could be computed from the @auto_tlrc
reference dataset or from the union of skull-stripped subject anatomies.

Since subjects have varying degrees of signal dropout in valid brain areas
of the EPI data, the resulting EPI intersection mask that would be required
in group space may exclude edge regions that are otherwise desirable.

Also, it is helpful to see if much 'activation' appears outside the brain.
This could be due to scanner or interpolation artifacts, and is useful to
note, rather than to simply mask out and never see.

Rather than letting 3dAutomask decide which brain areas should not be
considered valid, create a mask based on the anatomy _after_ the results
have been warped to a standard group space.  Then perhaps dilate the mask
by one voxel.  Example #11 from '3dcalc -help' shows how one might dilate.

Note that the EPI data can now be warped to standard space at the volreg
step.  In that case, it might be appropriate to mask the EPI data based
on the Talairach template, such as what is used for -base in @auto_tlrc.
This can be done via '-mask_apply group'.

---

For those who have processed some of their data with the older method:

Note that this change should not be harmful to those who have processed
data with older versions of afni_proc.py, as it only adds non-zero voxel
values to the output datasets.  If some subjects were analyzed with the
older version, the processing steps should not need to change.  It is still
necessary to apply an intersection mask across subjects in group space.

It might be okay to create the intersection mask from only those subjects
which were masked in the regression, however one might say that biases the
voxel choices toward those subjects, though maybe that does not matter.
Any voxels used would still be across all subjects.

---

A mask dataset is necessary when computing blur estimates from the epi and
errts datasets.  Also, since it is nice to simply see what the mask looks
like, its creation has been left in by default.

The '-regress_no_mask' option is now unnecessary.

---

Note that if no mask were applied in the 'scaling' step, large percent
changes could result.  Because large values would be a detriment to the
numerical resolution of the scaled short data, the default is to truncate
scaled values at 200 (percent), which should not occur in the brain.

--------------------------------------------------
BLIP NOTE: ~2~

application of reverse-blip (blip-up/blip-down) registration:

   o compute the median of the forward and reverse-blip data
   o align them using 3dQwarp -plusminus
      -> the main output warp is the square root of the forward warp
         to the reverse, i.e. it warps the forward data halfway
      -> in theory, this warp should make the EPI anatomically accurate

order of operations:

   o the blip warp is computed after all initial temporal operations
     (despike, ricor, tshift)
   o and before all spatial operations (anat/EPI align, tlrc, volreg)

notes:

   o If no forward blip time series (volume?) is provided by the user,
     the first time points from the first run will be used (using the
     same number of time points as in the reverse blip time series).
   o As usual, all registration transformations are combined.

differences with unWarpEPI.py (R Cox, D Glen and V Roopchansingh):

                    afni_proc.py            unWarpEPI.py
                    --------------------    --------------------
   tshift step:     before unwarp           after unwarp
                    (option: after unwarp)

   volreg program:  3dvolreg                3dAllineate

   volreg base:     as before               median warped dset
                    (option: MEDIAN_BLIP)   (same as MEDIAN_BLIP)

   unifize EPI?     no (option: yes)        yes
   (align w/anat)

--------------------------------------------------
ANAT/EPI ALIGNMENT CASES NOTE: ~2~

This outlines the effects of alignment options, to help decide what options
seem appropriate for various cases.

1. EPI to EPI alignment (the volreg block)

    Alignment of the EPI data to a single volume is based on the 3 options
    -volreg_align_to, -volreg_base_dset and -volreg_base_ind, where the
    first option is by far the most commonly used.

    Note that a good alternative is: '-volreg_align_to MIN_OUTLIER'.

    The logic of EPI alignment in afni_proc.py is:

        a. if -volreg_base_dset is given, align to that
           (this volume is copied locally as the dataset ext_align_epi)
        b. otherwise, use the -volreg_align_to or -volreg_base_ind volume

    The typical case is to align the EPI to one of the volumes used in
    pre-processing (where the dataset is provided by -dsets and where the
    particular TR is not removed by -tcat_remove_first_trs).  If the base
    volume is the first or third (TR 0 or 2) from the first run, or is the
    last TR of the last run, then -volreg_align_to can be used.

    To specify a TR that is not one of the 3 just stated (first, third or
    last), -volreg_base_ind can be used.

    To specify a volume that is NOT one of those used in pre-processing
    (such as the first pre-steady state volume, which would be excluded by
    the option -tcat_remove_first_trs), use -volreg_base_dset.

2. anat to EPI alignment cases (the align block)

    This is specific to the 'align' processing block, where the anatomy is
    aligned to the EPI.  The focus is on which EPI volume the anat gets
    aligned to.  Whether this transformation is inverted in the volreg
    block (to instead align the EPI to the anat via -volreg_align_e2a) is
    an independent consideration.

    The logic of which volume the anatomy gets aligned to is as follows:
        a. if -align_epi_ext_dset is given, use that for anat alignment
        b. otherwise, if -volreg_base_dset, use that
        c. otherwise, use the EPI base from the EPI alignment choice

    To restate this: the anatomy gets aligned to the same volume the EPI
    gets aligned to *unless* -align_epi_ext_dset is given, in which case
    that volume is used.

    The entire purpose of -align_epi_ext_dset is for the case where the
    user might want to align the anat to a different volume than what is
    used for the EPI (e.g. align anat to a pre-steady state TR but the EPI
    to a steady state one).

    Output:

       The result of the align block is an 'anat_al' dataset.  This will be
       in alignment with the EPI base (or -align_epi_ext_dset).

       In the default case of anat -> EPI alignment, the aligned anatomy
       is actually useful going forward, and is so named 'anat_al_keep'.

       Additionally, if the -volreg_align_e2a option is used (thus aligning
       the EPI to the original anat), then the aligned anat dataset is no
       longer very useful, and is so named 'anat_al_junk'.  However, unless
       an anat+tlrc dataset was copied in for use in -volreg_tlrc_adwarp,
       the skull-striped anat (anat_ss) becomes the current one going
       forward.  That is identical to the original anat, except that it
       went through the skull-stripping step in align_epi_anat.py.

       At that point (e2a case) the pb*.volreg.* datasets are aligned with
       the original anat or the skull-stripped original anat (and possibly
       in Talairach space, if the -volreg_tlrc_warp or _adwarp option was
       applied).

     Checking the results:

       The pb*.volreg.* volumes should be aligned with the anat.  If
       -volreg_align_e2a was used, it will be with the original anat.
       If not, then it will be with anat_al_keep.

       Note that at the end of the regress block, whichever anatomical
       dataset is deemed "in alignment" with the stats dataset will be
       copied to anat_final.$subj.

       So compare the volreg EPI with the final anatomical dataset.

--------------------------------------------------
ANAT/EPI ALIGNMENT CORRECTIONS NOTE: ~2~

Aligning the anatomy and EPI is sometimes difficult, particularly depending
on the contrast of the EPI data (between tissue types).  If the alignment
fails to do a good job, it may be necessary to run align_epi_anat.py in a
separate location, find options that help it to succeed, and then apply
those options to re-process the data with afni_proc.py.

1. If the anat and EPI base do not start off fairly close in alignment,
   the -giant_move option may be needed for align_epi_anat.py.  Pass this
   option to AEA.py via the afni_proc.py option -align_opts_aea:

        afni_proc.py ... -align_opts_aea -giant_move

2. The default cost function used by align_epi_anat.py is lpc (local
   Pearson correlation).  If this cost function does not work (probably due
   to poor or unusual EPI contrast), then consider cost functions such as
   lpa (absolute lpc), lpc+ (lpc plus fractions of other cost functions) or
   lpc+ZZ (approximate with lpc+, but finish with pure lpc).

   The lpa and lpc+ZZ cost functions are common alternatives.  The
   -giant_move option may be necessary independently.

   Examples of some helpful options:

     -align_opts_aea -cost lpa
     -align_opts_aea -giant_move
     -align_opts_aea -cost lpc+ZZ -giant_move
     -align_opts_aea -check_flip
     -align_opts_aea -cost lpc+ZZ -giant_move -resample off
     -align_opts_aea -skullstrip_opts -blur_fwhm 2

3. Testing alignment with align_epi_anat.py directly.

   When having alignment problems, it may be more efficient to copy the
   anat and EPI alignment base to a new directory, figure out a good cost
   function or other options, and then apply them in a new afni_proc.py
   command.

   For testing purposes, it helps to test many cost functions at once.
   Besides the cost specified by -cost, other cost functions can be applied
   via -multi_cost.  This is efficient, since all of the other processing
   does not need to be repeated.  For example:

     align_epi_anat.py -anat2epi                    \\
            -anat subj99_anat+orig                  \\
            -epi pb01.subj99.r01.tshift+orig        \\
            -epi_base 0 -volreg off -tshift off     \\
            -giant_move                             \\
            -cost lpc -multi_cost lpa lpc+ZZ mi

   That adds -giant_move, and uses the basic lpc cost function along with
   3 additional cost functions (lpa, lpc+ZZ, mi).  The result is 4 new
   anatomies aligned to the EPI, 1 per cost function:

           subj99_anat_al+orig         - cost func lpc      (see -cost opt)
           subj99_anat_al_lpa+orig     - cost func lpa         (additional)
           subj99_anat_al_lpc+ZZ+orig  - cost func lpc+ZZ      (additional)
           subj99_anat_al_mi+orig      - cost func mi          (additional)

   Also, if part of the dataset gets clipped in the case of -giant_move,
   consider the align_epi_anat.py option '-resample off'.

--------------------------------------------------
WARP TO TLRC NOTE: ~2~

afni_proc.py can now apply a +tlrc transformation to the EPI data as part
of the volreg step via the option '-volreg_tlrc_warp'.  Note that it can
also align the EPI and anatomy at the volreg step via '-volreg_align_e2a'.

Manual Talairach transformations can also be applied, but separately, after
volreg.  See '-volreg_tlrc_adwarp'.

This tlrc transformation is recommended for many reasons, though some are
not yet implemented.  Advantages include:

    - single interpolation of the EPI data

        Done separately, volume registration, EPI to anat alignment and/or
        the +tlrc transformation interpolate the EPI data 2 or 3 times.  By
        combining these transformations into a single one, there is no
        resampling penalty for the alignment or the warp to standard space.

        Thanks to D Glen for the steps used in align_epi_anat.py.

    - EPI time series become directly comparable across subjects

        Since the volreg output is now in standard space, there is already
        voxel correspondence across subjects with the EPI data.

    - group masks and/or atlases can be applied to the EPI data without
      additional warping

        It becomes trivial to extract average time series data over ROIs
        from standard atlases, say.

        This could even be done automatically with afni_proc.py, as part
        of the single-subject processing stream (not yet implemented).
        One would have afni_proc.py extract average time series (or maybe
        principal components) from all the ROIs in a dataset and apply
        them as regressors of interest or of no interest.

    - no interpolation of statistics

        If the user wishes to include statistics as part of the group
        analysis (e.g. using 3dMEMA.R), this warping becomes more needed.
        Warping to standard space *after* statistics are generated is not
        terribly valid.

--------------------------------------------------
RETROICOR NOTE: ~2~

** Cardiac and respiratory regressors must be created from an external
   source, such as the RetroTS.py program written by Z Saad, and converted
   to python by J Zosky.  The input to that should be the 2+ signals.  The
   output should be a single file per run, containing 13 or more regressors
   for each slice.  That set of output files would be applied here in
   afni_proc.py.

Removal of cardiac and respiratory regressors can be done using the 'ricor'
processing block.  By default, this would be done after 'despike', but
before any other processing block.

These card/resp signals would be regressed out of the MRI data in the
'ricor' block, after which processing would continue normally. In the final
'regress' block, regressors for slice 0 would be applied (to correctly
account for the degrees of freedom and also to remove residual effects).
    --> This is now only true when using '-regress_apply_ricor yes'.
        The default as of 30 Jan 2012 is to not include them in the final
        regression (since degrees of freedom are really not important for a
        subsequent correlation analysis).

Users have the option of removing the signal "per-run" or "across-runs".

Example R1: 7 runs of data, 13 card/resp regressors, process "per-run"

    Since the 13 regressors are processed per run, the regressors can have
    different magnitudes each run.  So the 'regress' block will actually
    get 91 extra regressors (13 regressors times 7 runs each).

Example R2: process "across-run"

    In this case the regressors are catenated across runs when they are
    removed from the data.  The major difference between this and "per-run"
    is that now only 1 best fit magnitude is applied per regressor (not the
    best for each run).  So there would be only the 13 catenated regressors
    for slice 0 added to the 'regress' block.

Those analyzing resting-state data might prefer the per-run method, as it
would remove more variance and degrees of freedom might not be as valuable.

Those analyzing a normal signal model might prefer doing it across-runs,
giving up only 13 degrees of freedom, and helping not to over-model the
data.

** The minimum options would be specifying the 'ricor' block (preferably
   after despike), along with -ricor_regs and -ricor_regress_method.

Example R3: afni_proc.py option usage:

    Provide additional options to afni_proc.py to apply the despike and
    ricor blocks (which will be the first 2 blocks by default), with each
    regressor named 'slibase*.1D' going across all runs, and where the
    first 3 TRs are removed from each run (matching -tcat_remove_first_trs,
    most likely).

        -do_block despike ricor
        -ricor_regs slibase*.1D
        -ricor_regress_method across-runs
        -ricor_regs_nfirst 3

--------------------------------------------------
MULTI ECHO NOTE: ~2~

    rcr - todo

In the case of multi-echo data, there are many things to consider.
-combine_method
-mask_epi_anat yes
-blocks ... mask combine ...

    see TEDANA NOTE
--------------------------------------------------
TEDANA NOTE: ~2~

This deserves its own section.
-tshift_interp -wsinc9
-mask_epi_anat yes
-volreg_warp_final_interp wsinc5

    see MULTI ECHO NOTE
--------------------------------------------------
RUNS OF DIFFERENT LENGTHS NOTE: ~2~

In the case that the EPI datasets are not all of the same length, here
are some issues that may come up, listed by relevant option:

    -volreg_align_to        OK, as of version 1.49.

    -ricor_regress_method   OK, as of version 3.05.

    -regress_polort         Probably no big deal.
                            If this option is not used, then the degree of
                            polynomial used for the baseline will come from
                            the first run.  Only 1 polort may be applied.

    -regress_est_blur_epits OK, as of version 1.49.

 *  -regress_use_stim_files This may fail, as make_stim_times.py is not
                            currently prepared to handle runs of different
                            lengths.
                          * This option is essentially obsolete.

    -regress_censor_motion  OK, as of version 2.14

 * probably will be fixed (please let me know of interest)

--------------------------------------------------
SCRIPT EXECUTION NOTE: ~2~

The suggested way to run the output processing SCRIPT is via...

    a) if you use tcsh:    tcsh -xef SCRIPT |& tee output.SCRIPT

    b) if you use bash:    tcsh -xef SCRIPT 2>&1 | tee output.SCRIPT

    c) if you use tcsh and the script is executable, maybe use one of:

                        ./SCRIPT |& tee output.SCRIPT
                        ./SCRIPT 2>&1 | tee output.SCRIPT

Consider usage 'a' for example:  tcsh -xef SCRIPT |& tee output.SCRIPT

That command means to invoke a new tcsh with the -xef options (so that
commands echo to the screen before they are executed, exit the script
upon any error, do not process the ~/.cshrc file) and have it process the
SCRIPT file, piping all output to the 'tee' program, which will duplicate
output back to the screen, as well as to the given output file.

parsing the command: tcsh -xef SCRIPT |& tee output.SCRIPT

    a. tcsh

       The script itself is written in tcsh syntax and must be run that way.
       It does not mean the user must use tcsh.  Note uses 'a' and 'b'.
       There tcsh is specified by the user.  The usage in 'c' applies tcsh
       implicitly, because the SCRIPT itself specifies tcsh at the top.

    b. tcsh -xef

       The -xef options are applied to tcsh and have the following effects:

            x : echo commands to screen before executing them
            e : exit (terminate) the processing on any errors
            f : do not process user's ~/.cshrc file

       The -x option is very useful so one see not just output from the
       programs, but the actual commands that produce the output.  It
       makes following the output much easier.

       The -e option tells the shell to terminate on any error.  This is
       useful for multiple reasons.  First, it allows the user to easily
       see the failing command and error message.  Second, it would be
       confusing and useless to have the script try to continue, without
       all of the needed data.

       The -f option tells the shell not to process the user's ~/.cshrc
       (or ~/.tcshrc) file.  The main reason for including this is because
       of the -x option.  If there were any errors in the user's ~/.cshrc
       file and -x option were used, they would terminate the shell before
       the script even started, probably leaving the user confused.

    c. tcsh -xef SCRIPT

       The T-shell is invoked as described above, executing the contents
       of the specified text file (called 'SCRIPT', for example) as if the
       user had typed the included commands in their terminal window.

    d. |&

       These symbols are for piping the output of one program to the input
       of another.  Many people know how to do 'afni_proc.py -help | less'
       (or maybe '| more').  This script will output a lot of text, and we
       want to get a copy of that into a text file (see below).

       Piping with '|' captures only stdout (standard output), and would
       not capture errors and warnings that appear.  Piping with '|&'
       captures both stdout and stderr (standard error).  The user may not
       be able to tell any difference between those file streams on the
       screen, but since programs write to both, we want to capture both.

    e. tee output.SCRIPT

       Where do we want to send this captured stdout and stderr text?  Send
       it to the 'tee' program.  Like a plumber's tee, the 'tee' program
       splits the data (not water) stream off into 2 directions.

       Here, one direction that tee sends the output is back to the screen,
       so the user can still see what is happening.

       The other direction is to the user-specified text file.  In this
       example it would be 'output.SCRIPT'.  With this use of 'tee', all
       screen output will be duplicated in that text file.

"""
g_help_options = """
==================================================
OPTIONS:  ~2~

    Informational options, general options, and block options.
    Block options are ordered by block.

    -----------------------------------------------------------------
    Informational/terminal options  ~3~

    -help                   : show the complete help

    -help_section SECTION   : show help for given SECTION

        The help is divided into sections, an any one of these can be
        displayed individually by providing the given SECTION:

              intro         - introduction
              examples      - afni_proc.py command examples
              notes         - NOTE_* entries
              options       - descriptions of options
              trailer       - final trailer

    -help_tedana_files      : show tedana file names, compare orig vs bids

       The file naming between older and newer tedana versions (or newer
       using "tedana --convention orig") is shown with this option.  For
       example, the denoised time series after being Optimally Combined
       has possible names of:

           orig                 BIDS
           ----                 ----
           dn_ts_OC.nii.gz      desc-optcomDenoised_bold.nii.gz          

        Please see 'tedana --help' for more information.

    -hist                   : show the module history

    -hist_milestones        : show the history of interesting milestones

    -requires_afni_version  : show AFNI date required by processing script

        Many updates to afni_proc.py are accompanied by corresponding
        updates to other AFNI programs.  So if the processing script is
        created on one computer but executed on another (with an older
        version of AFNI), confusing failures could result.

        The required date is adjusted whenever updates are made that rely
        on new features of some other program.  If the processing script
        checks the AFNI version, the AFNI package must be as current as the
        date output via this option.  Checks are controlled by the option
        '-check_afni_version'.

        The checking method compares the output of:
            afni_proc.py -requires_afni_version

        against the most recent date in afni_history:
            afni_history -past_entries 1

        See also '-requires_afni_hist'.

        See also '-check_afni_version'.

    -requires_afni_hist     : show history of -requires_afni_version

        List the history of '-requires_afni_version' dates and reasons.

    -show_valid_opts        : show all valid options (brief format)

    -show_example NAME      : display the given example command

            e.g. afni_proc.py -show_example 'example 6b'
            e.g. afni_proc.py -show_example 'example 6b' -verb 0
            e.g. afni_proc.py -show_example 'example 6b' -verb 2

        Display the given afni_proc.py help example.  Details shown depend
        on the verbose level, as specified with -verb:

            0: no formatting - command can be copied and applied elsewhere
            1: basic         - show header and formatted command
            2: detailed      - include full description, as in -help output

        To list examples that can be shown, use:

            afni_proc.py -show_example_names

        See also '-show_example_names'.

    -show_example_names     : show names of all sample commands
                              (possibly for use with -compare options)

            e.g. afni_proc.py -show_example_names
            e.g. afni_proc.py -show_example_names -verb 3

        Use this command to list the current examples know by afni_proc.py.
        The format of the output is affected by -verb, with -verb 2 format
        being the default.

        Adding -verb 3 will display the most recent modification date.

    -show_example_keywords  : show keywords associated with all examples

            e.g. afni_proc.py -show_example_keywords
            e.g. afni_proc.py -show_example_keywords -verb 2

        Use this command to list the current examples know by afni_proc.py.
        The format of the output is affected by -verb, with -verb 2 format
        being the default.

    -show_merged_opts EG    : merge with example EG and show resulting command

            e.g. -show_merged_opts 'publish 3i'

        This option is intended to help one supply a list of input datasets
        and perhaps some preferable options, and then to fill the command with
        option based on a known example.

           all (non-merge) options are initially included, and then...

           if an example option is already provided
              the option is skipped
           else if the example option is to specify a dataset
              the option is added, with a '-CHECK' prefix to the option name
           else
              the option is added

        This command would show the entire 'publish 3i' example.
        Many output options start with -CHECK.

           afni_proc.py -show_merged_opts 'publish 3i'

        This command would show the entire 'publish 3i' example, but include
        the user-specified options.

           afni_proc.py -copy_anat sub-000_anat.nii.gz              \\
                        -dsets_me_run sub-000_echo{1,2,3}.nii.gz    \\
                        -regress_censor_motion 0.4                  \\
                        -show_merged_opts 'publish 3i'

        See also -compare_merged_opts.

    -show_pretty_command    : output the same command, but in a nice format

            e.g. afni_proc.py -show_pretty_command

        Adding this option to an existing afni_proc.py command will result in
        displaying the command itself in a nicely indented manner, using the
        P Taylor special routines.

    -show_pythonic_command  : output the same command, but as a python list

            e.g. afni_proc.py -show_pythonic_command

        Adding this option to an existing afni_proc.py command will result in
        displaying the command itself, but in a python list format that is
        helpful to me.

    -ver                    : show the version number

    -----------------------------------------------------------------
    Terminal 'compare' options ~3~

    These options are used to help compare one afni_proc.py command with a
    different one.  One can compare a current command to a given example,
    one example to another, or one command to another.

    To see a list of examples one can compare against, consider:

        afni_proc.py -show_example_names

    -compare_example_pair EG1 EG2 : compare options for pair of examples

            e.g. -compare_example_pair 'example 6' 'example 6b'

        more completely:

            afni_proc.py -compare_example_pair 'example 6' 'example 6b'

        This option allows one to compare a pair of pre-defined examples
        (from the list in 'afni_proc.py -show_example_names').  It is like
        using -compare_opts, but for comparing example vs. example.

    -compare_opts EXAMPLE   : compare current options against EXAMPLE

            e.g. -compare_opts 'example 6b'

        more completely:

            afni_proc.py  ... my options ...  -compare_opts 'example 6b'

        Adding this option (and parameter) to an existing afni_proc.py
        command results in comparing the options applied in the current
        command against those of the specified target example.

        The afni_proc.py command terminates after showing the comparison
        output.

        The output from this is controlled by the -verb LEVEL:

            0       : show (python-style) lists of differing options
            1 (def) : include parameter differences
                      (except where expected, e.g. -copy_anat dset)
                      (limit param lists to current text line)
            2       : show parameter diffs, but try to distinguish what might
                      just be a difference in paths to a file
            3       : show complete parameter diffs

        Types of differences shown include:

            missing options         :
                where the current command is missing options that the
                specified target command includes
            extra options           :
                where the current command has extra options that the
                specified target command is missing
            differing options       :
                where the current command and target use the same option,
                but their parameters differ (possibly just in a file path)
            fewer applied options   :
                where the current command and target use multiple copies of
                the same option, but the current command has fewer
                (what is beyond the matching/differing cases)
            more applied options    :
                where the current command and target use multiple copies of
                the same option, but the current command has more
                (what is beyond the matching/differing cases)

        This option is the basis for all of the -compare* options.

        * Note: options with the same option name are compared in order, so
                a different order of such options will appear as differences.
                For example, -ROI_import options all need to be in the same
                relative order, or they will be seen as differing.
                Such is life.  If this fact proves disastrous, let me know.

        See also -show_example_names.

    -compare_opts_vs_opts opts... : compare 2 full commands

        more completely:

            afni_proc.py                            \\
                ... one full set of options ...     \\
                -compare_opts_vs_opts               \\
                ... another full set of options ...

        Like other -compare_* options, but this compares 2 full commands,
        separated by -compare_opts_vs_opts.  This is a comparison method
        for comparing 2 local commands, rather than against any known
        example.

    -compare_merged_opts EG : first merge with example EG, then -compare_opts

            e.g. -compare_merged_opts 'publish 3i'

        This is like running "-show_merged_opts EG", then "-compare_opts EG".

        Instead of showing the merged command with options, the merged command
        is then compared with the original EG, as would be done with
        -compare_opts EG.

        See also -show_merged_opts, -compare_opts.

    -----------------------------------------------------------------
    General execution and setup options ~3~

    -anat_follower LABEL GRID DSET : specify anat follower dataset

            e.g. -anat_follower GM anat FS_GM_MASK.nii

        Use this option to pass any anatomical follower dataset.  Such a
        dataset is warped by any transformations that take the original
        anat to anat_final.

        Anatomical follower datasets are resampled using wsinc5.  The only
        difference with -anat_follower_ROI is that such ROI datasets are
        resampled using nearest neighbor interpolation.

           LABEL    : to name and refer to this dataset
           GRID     : which grid should this be sampled on, anat or epi?
           DSET     : name of input dataset, changed to copy_af_LABEL

        A default anatomical follower (in the case of skull stripping) is
        the original anat.  That is to get a warped version that still has
        a skull, for quality control.

        See also -anat_follower_ROI, anat_follower_erode.

    -anat_follower_erode LABEL LABEL ...: erode masks for given labels

            e.g. -anat_follower_erode WMe

        Perform a single erosion step on the mask dataset for the given
        label.  This is done on the input ROI (anatomical?) grid.

        The erosion step is applied before any transformation, and uses the
        18-neighbor approach (6 face and 12 edge neighbors, not 8 corner
        neighbors) in 3dmask_tool.

      * For more control on the erosion level, see -anat_follower_erode_level.

        See also -anat_follower_erode_level, -regress_ROI_PC, -regress_ROI.
        Please see '3dmask_tool -help' for more information on eroding.

    -anat_follower_erode_level LABEL LEVEL : erode a mask at a specific level

            e.g. -anat_follower_erode_level WMe 2

        Use this option to specify an anatomical erosion level, in voxels.

        The erosion step is applied before any transformation, and uses the
        18-neighbor approach (6 face and 12 edge neighbors, not 8 corner
        neighbors) in 3dmask_tool.

      * For more control on the erosion level, see -anat_follower_erode_level.

        See also -anat_follower_erode_level, -regress_ROI_PC, -regress_ROI.
        Please see '3dmask_tool -help' for more information on eroding.

    -anat_follower_ROI LABEL GRID DSET : specify anat follower ROI dataset

        e.g. -anat_follower_ROI aaseg anat aparc.a2009s+aseg_REN_all.nii.gz
        e.g. -anat_follower_ROI FSvent epi fs_ap_latvent.nii.gz

        Use this option to pass any anatomical follower dataset.  Such a
        dataset is warped by any transformations that take the original
        anat to anat_final.

        Similar to -anat_follower, except that these anatomical follower
        datasets are resampled using nearest neighbor (NN) interpolation,
        to preserve data values (as opposed to -anat_follower, which uses
        wsinc5).  That is the only difference between these options.

           LABEL    : to name and refer to this dataset
           GRID     : which grid should this be sampled on, anat or epi?
           DSET     : name of input dataset, changed to copy_af_LABEL

        Labels defined via this option may be used in -regress_ROI or _PC.

        See also -anat_follower, anat_follower_erode, -regress_ROI
        or -regress_ROI_PC.

    -anat_has_skull yes/no  : specify whether the anatomy has a skull

            e.g. -anat_has_skull no

        Use this option to block any skull-stripping operations, likely
        either in the align or tlrc processing blocks.

    -anat_uniform_method METHOD : specify uniformity correction method

            e.g. -anat_uniform_method unifize

        Specify the method for anatomical intensity uniformity correction.

            none    : do not do uniformity correction at all
            default : use 3dUnifize at whim of auto_warp.py
            unifize : apply 3dUnifize early in processing stream
                      (so it affects more than auto_warp.py)

        Please see '3dUnifize -help' for details.
        See also -anat_opts_unif.

    -anat_opts_unif OPTS ... : specify extra options for unifize command

            e.g. -anat_opts_unif -Urad 14

        Specify options to be applied to the command used for anatomical
        intensity uniformity correction, such as 3dUnifize.

        Please see '3dUnifize -help' for details.
        See also -anat_uniform_method.

    -anat_unif_GM yes/no    : also unifize gray matter (lower intensities)
                              the default is 'no'

            e.g. -anat_unif_GM yes
            default: -anat_unif_GM no

        If this is set to yes, 3dUnifize will not only apply uniformity
        correction across the brain volume, but also to voxels that look
        like gray matter.  That is to say the option adds '-GM' to the
        3dUnifize command.

      * The default was changed from yes to no 2014, May 16.

        Please see '3dUnifize -help' for details.
        See also -anat_uniform_method, -anat_opts_unif.

    -ask_me                 : ask the user about the basic options to apply

        When this option is used, the program will ask the user how they
        wish to set the basic options.  The intention is to give the user
        a feel for what options to apply (without using -ask_me).

    -bash                   : show example execution command in bash form

        After the script file is created, this program suggests how to run
        it (piping stdout/stderr through 'tee').  If the user is running
        the bash shell, this option will suggest the 'bash' form of a
        command to execute the newly created script.

        example of tcsh form for execution:

            tcsh -x proc.ED.8.glt |& tee output.proc.ED.8.glt

        example of bash form for execution:

            tcsh -x proc.ED.8.glt 2>&1 | tee output.proc.ED.8.glt

        Please see "man bash" or "man tee" for more information.

    -bids_deriv BDIR        : request BIDS derivative output

            e.g. -bids_deriv yes
            e.g. -bids_deriv /my/path/to/derivatives/TASK_PICKLES
            default: -bids_deriv no

        Use this option to request a copy of relevant output converted to BIDS
        tree format.  BDIR can be one of:

            no      : (default) do not produce any BIDS tree
            yes     : the BIDS tree will go under the subject results directory
            BDIR    : a path to a derivative directory
                      (must be absolute, i.e. staring with a /)

        The resulting directory will include the directories:

            anat        : anat and template
            func        : EPI BOLD time series, mask, residuals...
            func_stats  : statistical contrasts and stats datasets
            logs        : any copied log files

        Please see 'map_ap_to_deriv.py -help' for more information.  Note that
        map_ap_to_deriv.py can easily be run separately.

    -blocks BLOCK1 ...      : specify the processing blocks to apply

            e.g. -blocks volreg blur scale regress
            e.g. -blocks despike tshift align volreg blur scale regress
            default: tshift volreg blur mask scale regress

        The user may apply this option to specify which processing blocks
        are to be included in the output script.  The order of the blocks
        may be varied, and blocks may be skipped.

        See also '-do_block' (e.g. '-do_block despike').

    -check_afni_version yes/no : check that AFNI is current enough

            e.g. -check_afni_version no
            default: yes

        Check that the version of AFNI is recent enough for processing of
        the afni_proc.py script.

        For the version check, the output of:
            afni_proc.py -requires_afni_version

        is tested against the most recent date in afni_history:
            afni_history -past_entries 1

        In the case that newer features in other programs might not be
        needed by the given afni_proc.py script (depending on the options),
        the user is left with this option to ignore the AFNI version check.

        Please see 'afni_history -help' or 'afni -ver' for more information.
        See also '-requires_afni_version'.

    -check_results_dir yes/no : check whether dir exists before proceeding

            e.g. -check_results_dir no
            default: yes

        By default, if the results directory already exists, the script
        will terminate before doing any processing.  Set this option to
        'no' to remove that check.

    -check_setup_errors yes/no : terminate on setup errors

            e.g. -check_setup_errors yes
            default: no

        Have the script check $status after each command in the setup
        processing block.  It is preferable to run the script using the
        -e option to tcsh (as suggested), but maybe the user does not wish
        to do so.

    -command_comment_style STYLE: set style for final AP command comment

            e.g. -command_comment_style pretty

        This controls the format for the trailing afni_proc.py commented
        command at the end of the proc script.  STYLE can be:

            none        - no trailing command will be included
            compact     - the original compact form will be included
            pretty      - the PT-special pretty form will be included

    -copy_anat ANAT         : copy the ANAT dataset to the results dir

            e.g. -copy_anat Elvis/mprage+orig

        This will apply 3dcopy to copy the anatomical dataset(s) to the
        results directory.  Note that if a +view is not given, 3dcopy will
        attempt to copy +acpc and +tlrc datasets, also.

        See also '3dcopy -help'.

    -copy_files file1 ...   : copy file1, etc. into the results directory

            e.g. -copy_files glt_AvsB.txt glt_BvsC.1D glt_eat_cheese.txt
            e.g. -copy_files contrasts/glt_*.txt

        This option allows the user to copy some list of files into the
        results directory.  This would happen before the tcat block, so
        such files may be used for other commands in the script (such as
        contrast files in 3dDeconvolve, via -regress_opts_3dD).

    -do_block BLOCK_NAME ...: add extra blocks in their default positions

            e.g. -do_block despike ricor
            e.g. -do_block align

        With this option, any 'optional block' can be applied in its
        default position.  This includes the following blocks, along with
        their default positions:

            despike : first (between tcat and tshift)
            ricor   : just after despike (else first)
            align   : before tlrc, before volreg
            tlrc    : after align, before volreg
            empty   : NO DEFAULT, cannot be applied via -do_block

        Any block not included in -blocks can be added via this option
        (except for 'empty').

        See also '-blocks', as well as the "PROCESSING BLOCKS" section of
        the -help output.

    -dsets dset1 dset2 ...  : (REQUIRED) specify EPI run datasets

            e.g. -dsets Elvis_run1+orig Elvis_run2+orig Elvis_run3+orig
            e.g. -dsets Elvis_run*.HEAD

        The user must specify the list of EPI run datasets to analyze.
        When the runs are processed, they will be written to start with
        run 1, regardless of whether the input runs were just 6, 7 and 21.

        Note that when using a wildcard it is essential for the EPI
        datasets to be alphabetical, as that is how the shell will list
        them on the command line.  For instance, epi_run1+orig through
        epi_run11+orig is not alphabetical.  If they were specified via
        wildcard their order would end up as run1 run10 run11 run2 ...

        Note also that when using a wildcard it is essential to specify
        the datasets suffix, so that the shell doesn't put both the .BRIK
        and .HEAD filenames on the command line (which would make it twice
        as many runs of data).

    -dsets_me_echo dset1 dset2 ...  : specify ME datasets for one echo
                                      (all runs with each option)

       These examples might correspond to 3 echoes across 4 runs.

            e.g. -dsets_me_echo epi_run*.echo_1+orig.HEAD
                 -dsets_me_echo epi_run*.echo_2+orig.HEAD
                 -dsets_me_echo epi_run*.echo_3+orig.HEAD

            e.g. -dsets_me_echo r?.e1.nii
                 -dsets_me_echo r?.e2.nii
                 -dsets_me_echo r?.e3.nii

            e.g. -dsets_me_echo r1.e1.nii r2.e1.nii r3.e1.nii r4.e1.nii
                 -dsets_me_echo r1.e2.nii r2.e2.nii r3.e2.nii r4.e2.nii
                 -dsets_me_echo r1.e3.nii r2.e3.nii r3.e3.nii r4.e3.nii

        This option is convenient when there are more runs than echoes.

        When providing multi-echo data to afni_proc.py, doing all echoes
        of all runs at once seems messy and error prone.  So one must
        provide either one echo at a time (easier if there are more runs)
        or one run at a time (easier if there are fewer runs).

        With this option:

           - use one option per echo (as opposed to per run, below)
           - each option use should list all run datasets for that echo

        For example, if there are 7 runs and 3 echoes, use 3 options, one
        per echo, and pass the 7 runs of data for that echo in each.

        See also -dsets_me_run.
        See also -echo_times and -reg_echo.

    -dsets_me_run dset1 dset2 ...   : specify ME datasets for one run
                                      (all echoes with each option)

       These examples might correspond to 4 echoes across 2 runs.

            e.g. -dsets_me_run epi_run1.echo_*+orig.HEAD
                 -dsets_me_run epi_run2.echo_*+orig.HEAD

            e.g. -dsets_me_run r1.e*.nii
                 -dsets_me_run r2.e*.nii

            e.g. -dsets_me_run r1.e1.nii r1.e2.nii r1.e3.nii r1.e4.nii
                 -dsets_me_run r2.e1.nii r2.e2.nii r2.e3.nii r2.e4.nii

        This option is convenient when there are more echoes than runs.

        When providing multi-echo data to afni_proc.py, doing all echoes
        of all runs at once seems messy and error prone.  So one must
        provide either one echo at a time (easier if there are more runs)
        or one run at a time (easier if there are fewer runs).

        With this option:

           - use one option per run (as opposed to per echo, above)
           - each option use should list all echo datasets for that run

        For example, if there are 2 runs and 4 echoes, use 2 options, one
        per run, and pass the 4 echoes of data for that run in each.

        See also -dsets_me_echo.
        See also -echo_times and -reg_echo.

    -echo_times TE1 TE2 TE3 ... : specify echo-times for ME data processing

            e.g. -echo_times 20 30.5 41.2

        Use this option to specify echo times, if they are needed for the
        'combine' processing block (OC/ME-ICA/tedana).

        See also -combine_method.

    -execute                : execute the created processing script

        If this option is applied, not only will the processing script be
        created, but it will then be executed in the "suggested" manner,
        such as via:

            tcsh -xef proc.sb23 |& tee output.proc.sb23

        Note that it will actually use the bash format of the command,
        since the system command (C and therefore python) uses /bin/sh.

            tcsh -xef proc.sb23 2>&1 | tee output.proc.sb23

    -exit_on_error yes/no   : set whether proc script should exit on error

            e.g.     -exit_on_error no
            default: -exit_on_error yes

        This option affects how the program will suggest running any
        created proc script, as well as how one would be run if -execute
        is provided.

        If the choice is 'yes' (the default), the help for how to run the
        proc script (terminal and in script, itself) will show running it
        via "tcsh -xef", where the 'e' parameter says to exit on error.
        For example (using tcsh notation):

            tcsh -xef proc.sb23 |& tee output.proc.sb23

        If the choice is 'no', then it will suggest using simply "tcsh -x".
        For example (using tcsh notation):

            tcsh -x proc.sb23 |& tee output.proc.sb23

        This is also applied when using -execute, where afni_proc.py itself 
        runs the given system command.

        See also -execute.

    -find_var_line_blocks B0 B1 ... : specify blocks for find_variance_lines

            default: -find_var_line_blocks tcat
            e.g. -find_var_line_blocks tcat volreg
            e.g. -find_var_line_blocks NONE

        With this option set, find_variance_lines.tcsh will be run at the end
        of each listed block.  It looks for columns of high temporal variance 
        (looking across slices) in the time series data.

        Valid blocks include:

            tcat, tshift, volreg, blur, scale, NONE

        Since 'tcat' is the default block used, this option is turned off by
        using NONE as a block.

        See 'find_variance_lines.tcsh -help' for details.

    -gen_epi_review SCRIPT_NAME : specify script for EPI review

            e.g. -gen_epi_review review_orig_EPI.txt

        By default, the proc script calls gen_epi_review.py on the original
        EPI data (from the tcat step, so only missing pre-SS TRs).  This
        creates a "drive afni" script that the user can run to quickly scan
        that EPI data for apparent issues.

        Without this option, the script will be called @epi_review.$subj,
        where $subj is the subject ID.

        The script starts afni, loads the first EPI run and starts scanning
        through time (effectively hitting 'v' in the graph window).  The
        user can press <enter> in the prompting terminal window to go to
        each successive run.

        Note that the user has full control over afni, aside from a new run
        being loaded whey they hit <enter>.  Recall that the <space> key
        (applied in the graph window) can terminate the 'v' (video mode).

        See 'gen_epi_review.py -help' for details.
        See also 'no_epi_review', to disable this feature.

    -no_epi_review

        This option is used to prevent writing a gen_epi_review.py command
        in the processing script (i.e. do not create a script to review the
        EPI data).

        The only clear reason to want this option is if gen_epi_review.py
        fails for some reason.  It should not hurt to create that little
        text file (@epi_review.$subj, by default).

        See also '-gen_epi_review'.

    -html_review_opts  ...   : pass extra options to apqc_make_tcsh.py

            e.g. -html_review_opts -mot_grayplot_off
            e.g. -html_review_opts -vstat_list vis aud V-A

        Blindly pass the given options to apqc_make_tcsh.py.

    -html_review_style STYLE : specify generation method for HTML review

            e.g.     -html_review_style pythonic
            default: -html_review_style basic

        At the end of processing, by default, the proc script will generate
        quality control images and other information that is akin to 
        running @ss_review_driver (the minimum QC suggested for every
        subject).  This information will be stored in a static HTML page,
        for an optional, quick review.

        Use this option to specify the STYLE of the pages:

            none     : no HTML review pages
            basic    : static - time graph images generated by 1dplot
            pythonic : static - time graph images generated in python

            more to come?  pester Paul...
            STYLE omnicient : page will explain everything about the image
                 (available by March 17, 3097, or your money back)

        The result of this will be a QC_$subj directory (e.g., QC_FT),
        containing index.html, along with media_dat and media_img dirs.
        One should be able to view the QC information by opening index.html
        in a browser.

        These methods have different software requirements, but 'basic'
        was meant to have almost nothing, and should work on most systems.
        If insufficient software is available, afni_proc.py will
        (hopefully) not include this step.  Use 'none' to opt out.

    -keep_rm_files          : do not have script delete rm.* files at end

            e.g. -keep_rm_files

        The output script may generate temporary files in a block, which
        would be given names with prefix 'rm.'.  By default, those files
        are deleted at the end of the script.  This option blocks that
        deletion.

    -keep_script_on_err     : do not remove proc script if AP command fails

        When there is a fatal error in the afni_proc.py command, it will
        delete any incomplete proc script, unless this option is applied.

    -move_preproc_files     : move preprocessing files to preproc.data dir

        At the end of the output script, create a 'preproc.data' directory,
        and move most of the files there (dfile, outcount, pb*, rm*).

        See also -remove_preproc_files.

    -no_proc_command        : do not print afni_proc.py command in script

            e.g. -no_proc_command

        If this option is applied, the command used to generate the output
        script will be stored at the end of the script.

    -out_dir DIR            : specify the output directory for the script

            e.g. -out_dir ED_results
            default: SUBJ.results

        The AFNI processing script will create this directory and perform
        all processing in it.

    -outlier_count yes/no   : should we count outliers with 3dToutcount?

            e.g. -outlier_count no
            default: yes

        By default, outlier fractions are computed per TR with 3dToutcount.
        To disable outlier counting, apply this option with parameter 'no'.
        This is a yes/no option, meaning those are the only valid inputs.

        Note that -outlier_count must be 'yes' in order to censor outliers
        with -regress_censor_outliers.

        See "3dToutcount -help" for more details.
        See also -regress_censor_outliers.

    -outlier_legendre yes/no : use Legendre polynomials in 3dToutcount?

            e.g. -outlier_legendre no
            default: yes

        By default the -legendre option is passed to 3dToutcount.  Along
        with using better behaved polynomials, it also allows them to be
        higher than 3rd order (if desired).

        See "3dToutcount -help" for more details.

    -outlier_polort POLORT  : specify polynomial baseline for 3dToutcount

            e.g. -outlier_polort 3
            default: same degree that 3dDeconvolve would use:
                     1 + floor(run_length/150)

        Outlier counts come after detrending the data, where the degree
        of the polynomial trend defaults to the same that 3dDeconvolve
        would use.  This option will override the default.

        See "3dToutcount -help" for more details.
        See "3dDeconvolve -help" for more details.
        See also '-regress_polort' and '-outlier_legendre'.

    -radial_correlate yes/no : correlate each voxel with local radius

            e.g. -radial_correlate yes
            default: no

     ** Consider using -radial_correlate_blocks, instead.

        With this option set, @radial_correlate will be run on the
        initial EPI time series datasets.  That creates a 'corr_test'
        directory that one can review, plus potential warnings (in text)
        if large clusters of high correlations are found.

        (very abbreviated) method for @radial_correlate:
            for each voxel
               compute average time series within 20 mm radius sphere
               correlate central voxel time series with spherical average
            look for clusters of high correlations

        This is a useful quality control (QC) dataset that helps one find
        scanner artifacts, particularly including coils going bad.

        To visually check the results, the program text output suggests:

            run command: afni corr_test.results.postdata
            then set:    Underlay  = epi.SOMETHING
                         Overlay   = res.SOMETHING.corr
                         maybe threshold = 0.9, maybe clusterize

        See also -radial_correlate_blocks.
        See "@radial_correlate -help" for details and a list of options.

    -radial_correlate_blocks B0 B1 ... : specify blocks for correlations

            e.g.    -radial_correlate_blocks tcat volreg
            e.g.    -radial_correlate_blocks tcat volreg regress
            default -radial_correlate_blocks regress
            e.g.    -radial_correlate_blocks NONE

        With this option set, @radial_correlate will be run at the end of
        each listed block.  It computes, for each voxel, the correlation
        with a local spherical average (def = 20mm radius).  By default,
        this uses a fast technique to compute an approximate average that
        is slightly Gaussian weighted (relative weight 0.84 at the radius)
        via 3dmerge, but far faster than a flat average via 3dLocalstat.

        Valid blocks include:

            tcat, tshift, volreg, blur, scale, regress, NONE

      * The default is to apply "-radial_correlate_blocks regress".
        To omit all blocks, use "-radial_correlate_blocks NONE".

        The @radial_correlate command will produce an output directory of
        the form radcor.pbAA.BBBB, where 'AA' is the processing block index
        (e.g. 02), and BBBB is the block label (e.g. volreg).

        Those 'radcor.*' directories will contain one epi.ulay.rRUN dataset
        and a corresponding radcor.BLUR.rRUN.corr dataset for that run,
        e.g.,

            radcor.pb02.volreg/epi.ulay.r01+tlrc.BRIK
                               epi.ulay.r01+tlrc.HEAD
                               radcor.20.r01.corr+tlrc.BRIK
                               radcor.20.r01.corr+tlrc.HEAD

        For the regress block, radcor results will be generated for the
        all_runs and errts datasets.

        See also -radial_correlate_opts.
        See '@radial_correlate -help' for more details.

    -radial_correlate_opts OPTS...: specify options for @radial_correlate

            e.g. -radial_correlate_opts -corr_mask yes -merge_frad 0.25

        Use this to pass additional options to all @radial_correlate
        commands in the proc script.

        See also -radial_correlate_blocks.

    -reg_echo ECHO_NUM  : specify 1-based echo for registration

            e.g. -reg_echo 3
            default: 2

        Multi-echo data is registered based on a single echo, with the
        resulting transformations being applied to all echoes.  Use this
        option to specify the 1-based echo used to drive registration.

        Note that the echo used for driving registration should have
        reasonable tissue contrast.

    -remove_preproc_files   : delete pre-processed data

        At the end of the output script, delete the intermediate data (to
        save disk space).  Delete dfile*, outcount*, pb* and rm*.

        See also -move_preproc_files.

    -script SCRIPT_NAME     : specify the name of the resulting script

            e.g. -script ED.process.script
            default: proc_subj

        The output of this program is a script file.  This option can be
        used to specify the name of that file.

        See also -scr_overwrite, -subj_id.

    -scr_overwrite          : overwrite any existing script

            e.g. -scr_overwrite

        If the output script file already exists, it will be overwritten
        only if the user applies this option.

        See also -script.

    -sep_char CHAR          : apply as separation character in filenames

            e.g. -sep_char _
            default: .

        The separation character is used in many output filenames, such as
        the default '.' in:

            pb04.Nancy.r07.scale+orig.BRIK

        If (for some crazy reason) an underscore (_) character would be
        preferable, the result would be:

            pb04_Nancy_r07_scale+orig.BRIK

        If "-sep_char _" is applied, so is -subj_curly.

        See also -subj_curly.

    -subj_curly             : apply $subj as ${subj}

        The subject ID is used in dataset names is typically used without
        curly brackets (i.e. $subj).  If something is done where this would
        result in errors (e.g. "-sep_char _"), the curly brackets might be
        useful to delimit the variable (i.e. ${subj}).

        Note that this option is automatically applied in the case of
        "-sep_char _".

        See also -sep_char.

    -subj_id SUBJECT_ID     : specify the subject ID for the script

            e.g. -subj_id elvis
            default: SUBJ

        The subject ID is used in dataset names and in the output directory
        name (unless -out_dir is used).  This option allows the user to
        apply an appropriate naming convention.

    -test_for_dsets yes/no  : test for existence of input datasets

            e.g. -test_for_dsets no
            default: yes

        This options controls whether afni_proc.py check for the existence
        of input datasets.  In general, they must exist when afni_proc.py
        is run, in order to get run information (TR, #TRs, #runs, etc).

    -test_stim_files yes/no : evaluate stim_files for appropriateness?

            e.g. -test_stim_files no
            default: yes

        This options controls whether afni_proc.py evaluates the stim_files
        for validity.  By default, the program will do so.

        Input files are one of local stim_times, global stim_times or 1D
        formats.  Options -regress_stim_files and -regress_extra_stim_files
        imply 1D format for input files.  Otherwise, -regress_stim_times is
        assumed to imply local stim_times format (-regress_global_times
        implies global stim_times format).

        Checks include:

            1D              : # rows equals total reps
            local times     : # rows equal # runs
                            : times must be >= 0.0
                            : times per run (per row) are unique
                            : times cannot exceed run time
            global times    : file must be either 1 row or 1 column
                            : times must be >= 0.0
                            : times must be unique
                            : times cannot exceed total duration of all runs

        This option provides the ability to disable this test.

        See "1d_tool.py -help" for details on '-look_like_*' options.
        See also -regress_stim_files, -regress_extra_stim_files,
        -regress_stim_times, -regress_local_times, -regress_global_times.

    -uvar UVAR VAL VAL ..   : set a user variable and its values

            e.g. -uvar taskname my.glorious.task
                 -uvar ses ses-003
                 -uvar somelistvar A B C

        Use this option once per uvar.  Each such option will be passed along
        as part of the user variable list, along to APQC, for example.

        These variables will be initialized in out.ap_uvars.json .

    -verb LEVEL             : specify the verbosity of this script

            e.g. -verb 2
            default: 1

        Print out extra information during execution.

    -write_3dD_prefix PREFIX : specify prefix for outputs from 3dd_script

            e.g. -write_3dD_prefix basis.tent.
            default: test.

        If a separate 3dDeconvolve command script is generated via the
        option -write_3dD_script, then the given PREFIX will be used for
        relevant output files. in the script.

        See also -write_3dD_script.

    -write_3dD_script SCRIPT : specify SCRIPT only for 3dDeconvolve command

            e.g. -write_3dD_script run.3dd.tent

        This option is intended to be used with the EXACT same afni_proc.py
        command (aside from any -write_3dD_* options).  The purpose is to
        generate a corresponding 3dDeconvolve command script which could
        be run in the same results directory.

        Alternatively, little things could be changed that would only
        affect the 3dDeconvolve command in the new script, such as the
        basis function(s).

        The new script should include a prefix to distinguish output files
        from those created by the original proc script.

      * This option implies '-test_stim_files no'.

        See also -write_3dD_prefix, -test_stim_files.

    -write_ppi_3dD_scripts  : flag: write 3dD scripts for PPI analysis

            e.g. -write_ppi_3dD_scripts                        \\
                 -regress_ppi_stim_files PPI_*.1D some_seed.1D \\
                 -regress_ppi_stim_labels PPI_A PPI_B PPI_C seed

        Request 3dDeconvolve scripts for pre-PPI filtering (do regression
        without censoring) and post-PPI filtering (include PPI regressors
        and seed).

        This is a convenience method for creating extra 3dDeconvolve
        command scripts without having to run afni_proc.py multiple times
        with different options.

        Using this option, afni_proc.py will create the main proc script,
        plus :

           A. (if censoring was done) an uncensored 3dDeconvolve command
              pre-PPI filter script, to create an uncensored errts time
              series.

              This script is akin to using -write_3dD_* to output a
              regression script, along with adding -regress_skip_censor.
              The regression command should be identical to the original
              one, except for inclusion of 3dDeconvolve's -censor option.

           B. a 3dDeconvolve post-PPI filter script to include the PPI
              and seed regressors.

              This script is akin to using -write_3dD_* to output a
              regression script, along with passing the PPI and seed
              regressors via -regress_extra_stim_files and _labels.

        Use -regress_ppi_stim_files and -regress_ppi_stim_labels to
        specify the PPI (and seed) regressors and their labels.  These
        options are currently required.

        See also -regress_ppi_stim_files, -regress_ppi_stim_labels.

    -----------------------------------------------------------------
    Block options (in default block order) ~3~

    These options pertain to individual processing blocks.  Each option
    starts with the block name.

    -tcat_preSS_warn_limit LIMIT : TR #0 outlier limit to warn of pre-SS

            e.g. -tcat_preSS_warn_limit 0.7
            default: 0.4

        Outlier fractions are computed across TRs in the tcat processing
        block.  If TR #0 has a large fraction, it might suggest that pre-
        steady state TRs have been included in the analysis.  If the
        detected fraction exceeds this limit, a warning will be stored
        (and output by the @ss_review_basic script).

        The special case of limit = 0.0 implies no check will be done.

    -tcat_remove_first_trs NUM : specify how many TRs to remove from runs

            e.g. -tcat_remove_first_trs 3
            e.g. -tcat_remove_first_trs 3 1 0 0 3
            default: 0

        Since it takes several seconds for the magnetization to reach a
        steady state (at the beginning of each run), the initial TRs of
        each run may have values that are significantly greater than the
        later ones.  This option is used to specify how many TRs to
        remove from the beginning of every run.

        If the number needs to vary across runs, then one number should
        be specified per run.

    -tcat_remove_last_trs NUM : specify TRs to remove from run ends

            e.g. -tcat_remove_last_trs 10
            default: 0

        For when the user wants a simple way to shorten each run.

        See also -ricor_regs_rm_nlast.

    -despike_mask           : allow Automasking in 3dDespike

        By default, -nomask is applied to 3dDespike.  Since anatomical
        masks will probably not be contained within the Automask operation
        of 3dDespike (which uses methods akin to '3dAutomask -dilate 4'),
        it is left up to the user to speed up this operation via masking.

        Note that the only case in which this should be done is when
        applying the EPI mask to the regression.

        Please see '3dDespike -help' and '3dAutomask -help' for more
        information.

    -despike_new yes/no/... : set whether to use new version of 3dDespike

            e.g. -despike_new no
            e.g. -despike_new -NEW25
            default: yes

        Valid parameters: yes, no, -NEW, -NEW25

        Use this option to control whether to use one of the new versions.

        There is a '-NEW' option/method in 3dDespike which runs a faster
        method than the previous L1-norm method (Nov 2013).  The results
        are similar but not identical (different fits).  The difference in
        speed is more dramatic for long time series (> 500 time points).

        The -NEW25 option is meant to be more aggressive in despiking.

        Sep 2016: in 3dDespike, -NEW is now the default if the input is
                  longer than 500 time points.

        See also env var AFNI_3dDespike_NEW and '3dDespike -help' for more
        information.

    -despike_opts_3dDes OPTS... : specify additional options for 3dDespike

            e.g. -despike_opts_3dDes -nomask -ignore 2

        By default, 3dDespike is used with only -prefix and -nomask
        (unless -despike_mask is applied).  Any other options must be
        applied via -despike_opts_3dDes.

        Note that the despike block is not applied by default.  To apply
        despike in the processing script, use either '-do_block despike'
        or '-blocks ... despike ...'.

        Please see '3dDespike -help' for more information.
        See also '-do_blocks', '-blocks', '-despike_mask'.

    -ricor_datum DATUM      : specify output data type from ricor block

            e.g. -ricor_datum float

        By default, if the input is unscaled shorts, the output will be
        unscaled shorts.  Otherwise the output will be floats.

        The user may override this default with the -ricor_datum option.
        Currently only 'short' and 'float' are valid parameters.

        Note that 3dREMLfit only outputs floats at the moment.  Recall
        that the down-side of float data is that it takes twice the disk
        space, compared with shorts (scaled or unscaled).

        Please see '3dREMLfit -help' for more information.

    -ricor_polort POLORT    : set the polynomial degree for 3dREMLfit

            e.g. -ricor_polort 4
            default: 1 + floor(run_length / 75.0)

        The default polynomial degree to apply during the 'ricor' block is
        similar to that of the 'regress' block, but is based on twice the
        run length (and so should be almost twice as large).  This is to
        account for motion, since volreg has typically not happened yet.

        Use -ricor_polort to override the default.

    -ricor_regress_method METHOD    : process per-run or across-runs

            e.g. -ricor_regress_method across-runs
            default: NONE: this option is required for a 'ricor' block

        * valid METHOD parameters: per-run, across-runs

        The cardiac and respiratory signals can be regressed out of each
        run separately, or out of all runs at once.  The user must choose
        the method, there is no default.

        See "RETROICOR NOTE" for more details about the methods.

    -ricor_regress_solver METHOD    : regress using OLSQ or REML

            e.g. -ricor_regress_solver REML
            default: OLSQ

        * valid METHOD parameters: OLSQ, REML

        Use this option to specify the regression method for removing the
        cardiac and respiratory signals.  The default method is ordinary
        least squares, removing the "best fit" of the card/resp signals
        from the data (also subject to the polort baseline).

        To apply the REML (REstricted Maximum Likelihood) method, use this
        option.

        Note that 3dREMLfit is used for the regression in either case,
        particularly since the regressors are slice-based (they are
        different for each slice).

        Please see '3dREMLfit -help' for more information.

    -ricor_regs REG1 REG2 ...       : specify ricor regressors (1 per run)

            e.g. -ricor_regs slibase*.1D

        This option is required with a 'ricor' processing block.

        The expected format of the regressor files for RETROICOR processing
        is one file per run, where each file contains a set of regressors
        per slice.  If there are 5 runs and 27 slices, and if there are 13
        regressors per slice, then there should be 5 files input, each with
        351 (=27*13) columns.

        This format is based on the output of RetroTS.py, included in the
        AFNI distribution.

    -ricor_regs_nfirst NFIRST       : ignore the first regressor timepoints

            e.g. -ricor_regs_nfirst 2
            default: 0

        This option is similar to -tcat_remove_first_trs.  It is used to
        remove the first few TRs from the -ricor_regs regressor files.

        Since it is likely that the number of TRs in the ricor regressor
        files matches the number of TRs in the original input dataset (via
        the -dsets option), it is likely that -ricor_regs_nfirst should
        match -tcat_remove_first_trs.

        See also '-tcat_remove_first_trs', '-ricor_regs', '-dsets'.

    -ricor_regs_rm_nlast NUM : remove the last NUM TRs from each regressor

            e.g. -ricor_regs_rm_nlast 10
            default: 0

        For when the user wants a simple way to shorten each run.

        See also -tcat_remove_last_trs.

    -tshift_align_to TSHIFT OP : specify 3dTshift alignment option

            e.g. -tshift_align_to -slice 14
            default: -tzero 0

        By default, each time series is aligned to the beginning of the
        TR.  This option allows the users to change the alignment, and
        applies the option parameters directly to the 3dTshift command
        in the output script.

        It is likely that the user will use either '-slice SLICE_NUM' or
        '-tzero ZERO_TIME'.

        Note that when aligning to an offset other than the beginning of
        the TR, and when applying the -regress_stim_files option, then it
        may be necessary to also apply -regress_stim_times_offset, to
        offset timing for stimuli to later within each TR.

        Please see '3dTshift -help' for more information.
        See also '-regress_stim_times_offset'.

    -tshift_interp METHOD   : specify the interpolation method for tshift

            e.g. -tshift_interp -wsinc9
            e.g. -tshift_interp -Fourier
            e.g. -tshift_interp -cubic
            default -quintic

        Please see '3dTshift -help' for more information.

    -tshift_opts_ts OPTS ... : specify extra options for 3dTshift

            e.g. -tshift_opts_ts -tpattern alt+z

        This option allows the user to add extra options to the 3dTshift
        command.  Note that only one -tshift_opts_ts should be applied,
        which may be used for multiple 3dTshift options.

        Please see '3dTshift -help' for more information.

    -blip_forward_dset      : specify a forward blip dataset

            e.g. -blip_forward_dset epi_forward_blip+orig'[0..9]'

        Without this option, the first TRs of the first input EPI time
        series would be used as the forward blip dataset.

        See also -blip_reverse_dset.

        Please see '3dQwarp -help' for more information, and the -plusminus
        option in particular.

    -blip_reverse_dset      : specify a reverse blip dataset

            e.g. -blip_reverse_dset epi_reverse_blip+orig
            e.g. -blip_reverse_dset epi_reverse_blip+orig'[0..9]'

        EPI distortion correction can be applied via blip up/blip down
        acquisitions.  Unless specified otherwise, the first TRs of the
        first run of typical EPI data specified via -dsets is considered
        to be the forward direction (blip up, say).  So only the reverse
        direction data needs separate input.

        Please see '3dQwarp -help' for more information, and the -plusminus
        option in particular.

    -blip_opts_qw OPTS ...  : specify extra options for 3dQwarp

            e.g. -blip_opts_qw -noXdis -noZdis

        This option allows the user to add extra options to the 3dQwarp
        command specific to the 'blip' processing block.

        There are many options (e.g. for blurring) applied in the 3dQwarp
        command by afni_proc.py by default, so review the resulting script.

        Please see '3dQwarp -help' for more information.

    -blip_warp_dset DSET    : specify extra options for 3dQwarp

            e.g. -blip_warp_dset epi_b0_WARP.nii.gz

        This option allows the user to pass a pre-computed distortion warp
        dataset, to replace the computation of a warp in the blip block.
        The most likely use is to first run epi_b0_correct.py for a b0
        distortion map computation, rather than the reverse phase encoding
        method that would be computed with afni_proc.py.

        When applying this option in afni_proc.py, instead of using options
        like:

            -blip_forward_dset DSET_FORWARD \\
            -blip_reverse_dset DSET_REVERSE \\
            -blip_opts_qw      OPTIONS ...  \\

        use just this one option to pass the warp:

            -blip_warp_dset epi_b0_WARP.nii.gz \\

        Please see 'epi_b0_correct.py -help' for more information.

    -tlrc_anat              : run @auto_tlrc on '-copy_anat' dataset

            e.g. -tlrc_anat

        Run @auto_tlrc on the anatomical dataset provided by '-copy_anat'.
        By default, warp the anat to align with TT_N27+tlrc, unless the
        '-tlrc_base' option is given.

        The -copy_anat option specifies which anatomy to transform.

     ** Note, use of this option has the same effect as application of the
        'tlrc' block.

        Please see '@auto_tlrc -help' for more information.
        See also -copy_anat, -tlrc_base, -tlrc_no_ss and the 'tlrc' block.

    -tlrc_base BASE_DSET    : run "@auto_tlrc -base BASE_DSET"

            e.g. -tlrc_base TT_icbm452+tlrc
            default: -tlrc_base TT_N27+tlrc

        This option is used to supply an alternate -base dataset for
        @auto_tlrc (or auto_warp.py).  Otherwise, TT_N27+tlrc will be used.

        Note that the default operation of @auto_tlrc is to "skull strip"
        the input dataset.  If this is not appropriate, consider also the
        '-tlrc_no_ss' option.

        Please see '@auto_tlrc -help' for more information.
        See also -tlrc_anat, -tlrc_no_ss.

    -tlrc_copy_base yes/no  : copy base/template to results directory

            e.g. -tlrc_copy_base no
            default: -tlrc_copy_base yes

        By default, the template dataset (-tlrc_base) will be copied
        to the local results directory (for QC purposes).
        Use this option to override the default behavior.

        See also -tlrc_base.

    -tlrc_affine_warped_dsets ANAT WARP.1D : import affine warp results

            e.g. -tlrc_affine_warped_dsets anat.nii anat.un.aff.Xat.1D

        If the user has already run an affine of the subject anatomy
        to transform to standard space, those datasets can be input to
        save re-processing time, or if the transformations are preferable
        to what would be computed by @auto_tlrc.

        The warp should be the forward transformation, akin to what would
        be in warp.anat.Xat.1D after running:

            cat_matvec FT_anat_ns+tlrc::WARP_DATA -I > warp.anat.Xat.1D

        When using this option, the 'tlrc' block will be empty of actions.
        See also -tlrc_NL_warped_dsets.

    -tlrc_NL_warp           : use non-linear for template alignment

            e.g. -tlrc_NL_warp

        If this option is applied, then auto_warp.py is applied for the
        transformation to standard space, rather than @auto_tlrc, which in
        turn applies 3dQwarp (rather than 3dWarpDrive in @auto_tlrc).

        The output datasets from this operation are:

            INPUT_ANAT+tlrc         : standard space version of anat
            anat.un.aff.Xat.1D      : affine xform to standard space
            anat.un.aff.qw_WARP.nii : non-linear xform to standard space
                                      (displacement vectors across volume)

        The resulting ANAT dataset is copied out of the awpy directory
        back into AFNI format, and with the original name but new view,
        while the 2 transformation files (one text file of 12 numbers, one
        3-volume dataset vectors) are moved out with the original names.

        If -volreg_tlrc_warp is given, then the non-linear transformation
        will also be applied to the EPI data, sending the 'volreg' output
        directly to standard space.  As usual, all transformations are
        combined so that the EPI is only resampled one time.

        Options can be added to auto_warp.py via -tlrc_opts_at.

        Consider use of -anat_uniform_method along with this option.

        Please see 'auto_warp.py -help' for more information.
        See also -tlrc_opts_at, -anat_uniform_method.

    -tlrc_NL_warped_dsets ANAT WARP.1D NL_WARP: import auto_warp.py output

            e.g. -tlrc_NL_warped_dsets anat.nii           \\
                                       anat.un.aff.Xat.1D \\
                                       anat.un.aff.qw_WARP.nii

        If the user has already run auto_warp.py on the subject anatomy
        to transform (non-linear) to standard space, those datasets can
        be input to save re-processing time.

        They are the same 3 files that would be otherwise created by
        running auto_warp_py from the proc script.

        When using this option, the 'tlrc' block will be empty of actions.
        See also -tlrc_affine_warped_dsets.

    -tlrc_NL_force_view Y/N : force view when copying auto_warp.py result

            e.g.     -tlrc_NL_force_view no
            default: -tlrc_NL_force_view yes

        The auto_warp.py program writes results using NIFTI format.  If the
        alignment template is in a standard space that is not part of the
        NIFTI standard (TLRC and MNI are okay), then currently the only
        sform_code available is 2 ("aligned to something").  But that code
        is ambiguous, so users often set it to mean orig view (by setting
        AFNI_NIFTI_VIEW=orig).  This option (defaulting to yes) forces
        sform_code=2 to mean standard space, using +tlrc view.

    -tlrc_NL_awpy_rm Y/N    : specify whether to remove awpy directory

            e.g.     -tlrc_NL_awpy_rm no
            default: -tlrc_NL_awpy_rm yes

        The auto_warp.py program does all its work in an sub-directory
        called 'awpy', which is removed by default.  Use this option with
        'no' to save the awpy directory.

    -tlrc_no_ss             : add the -no_ss option to @auto_tlrc

            e.g. -tlrc_no_ss

        This option is used to tell @auto_tlrc not to perform the skull
        strip operation.

        Please see '@auto_tlrc -help' for more information.

    -tlrc_opts_at OPTS ...   : add additional options to @auto_tlrc

            e.g. -tlrc_opts_at -OK_maxite

        This option is used to add user-specified options to @auto_tlrc,
        specifically those afni_proc.py is not otherwise set to handle.

        In the case of -tlrc_NL_warp, the options will be passed to
        auto_warp.py, instead.

        Please see '@auto_tlrc -help' for more information.
        Please see 'auto_warp.py -help' for more information.

    -tlrc_rmode RMODE       : apply RMODE resampling in @auto_tlrc

            e.g. -tlrc_rmode NN

        This option is used to apply '-rmode RMODE' in @auto_tlrc.

        Please see '@auto_tlrc -help' for more information.

    -tlrc_suffix SUFFIX     : apply SUFFIX to result of @auto_tlrc

            e.g. -tlrc_suffix auto_tlrc

        This option is used to apply '-suffix SUFFIX' in @auto_tlrc.

        Please see '@auto_tlrc -help' for more information.

    -align_epi_ext_dset DSET : specify dset/brick for align_epi_anat EPI

            e.g. -align_epi_ext_dset subj10/epi_r01+orig'[0]'

        This option allows the user to specify an external volume for the
        EPI base used in align_epi_anat.py in the align block.  The user
        should apply sub-brick selection if the dataset has more than one
        volume.  This volume would be used for both the -epi and the
        -epi_base options in align_epi_anat.py.

        The user might want to align to an EPI volume that is not in the
        processing stream in the case where there is not sufficient EPI
        contrast left after the magnetization has reached a steady state.
        Perhaps volume 0 has sufficient contrast for alignment, but is not
        appropriate for analysis.  In such a case, the user may elect to
        align to volume 0, while excluding it from the analysis as part of
        the first volumes removed in -tcat_remove_first_trs.

        e.g. -dsets subj10/epi_r*_orig.HEAD
             -tcat_remove_first_trs 3
             -align_epi_ext_dset subj10/epi_r01+orig'[0]'
             -volreg_align_to first

        Note that even if the anatomy were acquired after the EPI, the user
        might still want to align the anat to the beginning of some run,
        and align all the EPIs to a time point close to that.  Since the
        anat and EPI are being forcibly aligned, it does not make such a
        big difference whether the EPI base is close in time to the anat
        acquisition.

        Note that this option does not affect the EPI registration base.

        Note that without this option, the volreg base dataset (whether
        one of the processed TRs or not) will be applied for anatomical
        alignment, assuming the align block is applied.

        See also -volreg_base_dset.
        Please see "align_epi_anat.py -help" for more information.

    -align_opts_aea OPTS ... : specify extra options for align_epi_anat.py

            e.g. -align_opts_aea -cost lpc+ZZ
            e.g. -align_opts_aea -cost lpc+ZZ -check_flip
            e.g. -align_opts_aea -Allineate_opts -source_automask+4
            e.g. -align_opts_aea -giant_move -AddEdge
            e.g. -align_opts_aea -skullstrip_opts -blur_fwhm 2

        This option allows the user to add extra options to the alignment
        command, align_epi_anat.py.

        Note that only one -align_opts_aea option should be given, with
        possibly many parameters to be passed on to align_epi_anat.py.

        Note the second example.  In order to pass '-source_automask+4' to
        3dAllineate, one must pass '-Allineate_opts -source_automask+4' to
        align_epi_anat.py.

        Similarly, the fourth example passes '-blur_fwhm 2' down through
        align_epi_anat.py to 3dSkullStrip.

      * The -check_flip option to align_epi_anat.py is good for evaluating
        data from external sources.  Aside from performing the typical
        registration, it will compare the final registration cost to that
        of a left/right flipped version.  If the flipped version is lower,
        one should investigate whether the axes are correctly labeled, or
        even labeled at all.

      * Please do not include -epi_strip with this -align_opts_aea option.
        That option to align_epi_anat.py should be controlled by
        -align_epi_strip_method.

        Please see "align_epi_anat.py -help" for more information.
        Please see "3dAllineate -help" for more information.

    -align_opts_eunif OPTS ... : add options to EPI uniformity command

            e.g. -align_opts_eunif -wdir_name work.epi_unif -no_clean

        This option allows the user to add extra options to the EPI
        uniformity correction command, probably 3dLocalUnifize (possibly
        3dUnifize).

        Please see "3dLocalUnifize -help" for more information.

    -align_epi_strip_method METHOD : specify EPI skull strip method in AEA

            e.g. -align_epi_strip_method 3dSkullStrip
            default: 3dAutomask (changed from 3dSkullStrip, 20 Aug, 2013)

        When align_epi_anat.py is used to align the EPI and anatomy, it
        uses 3dSkullStrip to remove non-brain tissue from the EPI dataset.
        However afni_proc.py changes that to 3dAutomask by default (as of
        August 20, 2013).  This option can be used to specify which method
        to use, one of 3dSkullStrip, 3dAutomask or None.

        This option assumes the 'align' processing block is used.

        Please see "align_epi_anat.py -help" for more information.
        Please see "3dSkullStrip -help" for more information.
        Please see "3dAutomask -help" for more information.

    -align_unifize_epi METHOD: run uniformity correction on EPI base volume

            e.g. -align_unifize_epi local
            default: no

        Use this option to run uniformity correction on the vr_base dataset
        for the purpose of alignment to the anat.

        The older yes/no METHOD choices were based on 3dUnifize.  The
        METHOD choices now include:

            local   : use 3dLocalUnifize ... (aka the "P Taylor special")
            unif    : use 3dUnifize -T2 ...
            yes     : (old choice) equivalent to unif
            no      : do not run EPI uniformity correction

        The uniformity corrected EPI volume is only used for anatomical
        alignment, and possibly visual quality control.

        One can use option -align_opts_eunif to pass extra options to
        either case (3dLocalUnifize or 3dUnifize).

        Please see "3dLocalUnifize -help" for more information.
        Please see "3dUnifize -help" for more information.

    -volreg_align_e2a       : align EPI to anatomy at volreg step

        This option is used to align the EPI data to match the anatomy.
        It is done by applying the inverse of the anatomy to EPI alignment
        matrix to the EPI data at the volreg step.  The 'align' processing
        block is required.

        At the 'align' block, the anatomy is aligned to the EPI data.
        When applying the '-volreg_align_e2a' option, the inverse of that
        a2e transformation (so now e2a) is instead applied to the EPI data.

        Note that this e2a transformation is catenated with the volume
        registration transformations, so that the EPI data is still only
        resampled the one time.  If the user requests -volreg_tlrc_warp,
        the +tlrc transformation will also be applied at that step in a
        single transformation.

        See also the 'align' block and '-volreg_tlrc_warp'.

    -volreg_align_to POSN   : specify the base position for volume reg

            e.g. -volreg_align_to last
            e.g. -volreg_align_to MIN_OUTLIER
            default: third

        This option takes 'first', 'third', 'last' or 'MIN_OUTLIER' as a
        parameter.  It specifies whether the EPI volumes are registered to
        the first or third volume (of the first run), the last volume (of
        the last run), or the volume that is consider a minimum outlier.
        The choice of 'first' or 'third' might correspond with when the
        anatomy was acquired before the EPI data.  The choice of 'last'
        might correspond to when the anatomy was acquired after the EPI
        data.

        The default of 'third' was chosen to go a little farther into the
        steady state data.

        Note that this is done after removing any volumes in the initial
        tcat operation.

      * A special case is if POSN is the string MIN_OUTLIER, in which
        case the volume with the minimum outlier fraction would be used.

        Since anat and EPI alignment tends to work very well, the choice
        of alignment base could even be independent of when the anatomy
        was acquired, making MIN_OUTLIER a good choice.

        Please see '3dvolreg -help' for more information.
        See also -tcat_remove_first_trs, -volreg_base_ind and
        -volreg_base_dset.

    -volreg_allin_auto_stuff OPT ...  : specify 'auto' options for 3dAllin.

            e.g. -volreg_allin_auto_stuff -autoweight

        When using 3dAllineate to do EPI motion correction, the default
        'auto' options applied are:

            -automask -source_automask -autoweight
        
        Use this option to _replace_ them with whatever is preferable.

      * All 3 options will be replaced, so if -autoweight is still wanted,
        for example, please include it with -volreg_allin_auto_stuff.

      * Do not pass -warp through here, but via -volreg_allin_warp.

        Please see '3dAllineate -help' for more details.

    -volreg_allin_warp WARP : specify -warp for 3dAllineate EPI volreg step

            e.g.    -volreg_allin_warp affine_general
            default -volreg_allin_warp shift_rotate

        When using 3dAllineate to do EPI motion correction, the default -warp
        type is shift_rotate (rigid body).  Use this option to specify another.

        The valid WARP options are:

            shift_rotates       : 6-param rigid body
            shift_rotate_scale  : 9-param with scaling
            affine_general      : 12-param full affine

        While 3dAllinate allows shift_rotate, afni_proc.py does not, as it
        would currently require an update to handle the restricted parameter
        list.  Please let rickr know if this is wanted.

        Please see '-warp' from '3dAllineate -help' for more details.

    -volreg_allin_cost COST : specify the cost function used in 3dAllineate

            e.g. -volreg_allin_cost lpa+zz

        When using 3dAllineate to do EPI motion correction, the default
        cost function is lpa.  Use this option to specify another.

        Please see '3dAllineate -help' for more details, including a list
        of cost functions.

    -volreg_post_vr_allin yes/no : do cross-run alignment of reg bases

            e.g. -volreg_post_vr_allin yes

        Using this option, time series registration will be done per run,
        with an additional cross-run registration of each within-run base
        to what would otherwise be the overall EPI registration base.

        3dAllineate is used for cross-run vr_base registration (to the
        global vr_base, say, which may or may not be one of the per-run
        vr_base datasets).

      * Consider use of -volreg_warp_dxyz, for cases when the voxel size
        might vary across runs.  It would ensure that the final grids are
        the same.

        See also -volreg_pvra_base_index, -volreg_warp_dxyz.

    -volreg_pvra_base_index INDEX : specify per run INDEX for post_vr_allin

            e.g.     -volreg_pvra_base_index 3
            e.g.     -volreg_pvra_base_index $
            e.g.     -volreg_pvra_base_index MIN_OUTLIER
            default: -volreg_pvra_base_index 0

        Use this option to specify the within-run volreg base for use with
        '-volreg_post_vr_allin yes'.  INDEX can be one of:

            0             : the default (the first time point per run)
            VAL           : an integer index, between 0 and the last
            $             : AFNI syntax to mean the last volume
            MIN_OUTLIER   : compute the MIN_OUTLIER per run, and use it

        See also -volreg_post_vr_allin.

    -volreg_base_dset DSET  : specify dset/sub-brick for volreg base

            e.g. -volreg_base_dset subj10/vreg_base+orig'[0]'
            e.g. -volreg_base_dset MIN_OUTLIER

        This option allows the user to specify an external dataset for the
        volreg base.  The user should apply sub-brick selection if the
        dataset has more than one volume.

        For example, one might align to a pre-magnetic steady state volume.

        Note that unless -align_epi_ext_dset is also applied, this volume
        will be used for anatomical to EPI alignment (assuming that is
        being done at all).

      * A special case is if DSET is the string MIN_OUTLIER, in which
        case the volume with the minimum outlier fraction would be used.

        See also -align_epi_ext_dset, -volreg_align_to and -volreg_base_ind.

    -volreg_base_ind RUN SUB : specify run/sub-brick indices for base

            e.g. -volreg_base_ind 10 123
            default: 0 0

        This option allows the user to specify exactly which dataset and
        sub-brick to use as the base registration image.  Note that the
        SUB index applies AFTER the removal of pre-steady state images.

      * The RUN number is 1-based, matching the run list in the output
        shell script.  The SUB index is 0-based, matching the sub-brick of
        EPI time series #RUN.  Yes, one is 1-based, the other is 0-based.
        Life is hard.

        The user can apply only one of the -volreg_align_to and
        -volreg_base_ind options.

        See also -volreg_align_to, -tcat_remove_first_trs and
        -volreg_base_dset.

    -volreg_get_allcostX yes/no : compute all anat/EPI costs

            e.g. -volreg_get_allcostX no
            default: yes

        By default, given the final anatomical dataset (anat_final) and
        the the final EPI volreg base (final_epi), this option can be used
        to compute alignment costs between the two volumes across all cost
        functions from 3dAllineate.  Effectively, it will add the following
        to the proc script:

            3dAllineate -base FINAL_EPI -input FINAL_ANAT -allcostX

        The text output is stored in the file out.allcostX.txt.

        This operation is informational only, to help evaluate alignment
        costs across subjects.

        Please see '3dAllineate -help' for more details.

    -volreg_compute_tsnr yes/no : compute TSNR datasets from volreg output

            e.g. -volreg_compute_tsnr yes
            default: no

        Use this option to compute a temporal signal to noise (TSNR)
        dataset at the end of the volreg block.  Both the signal and noise
        datasets are from the run 1 output, where the "signal" is the mean
        and the "noise" is the detrended time series.

        TSNR = average(signal) / stdev(noise)

        See also -regress_compute_tsnr.

    -volreg_interp METHOD   : specify the interpolation method for volreg

            e.g. -volreg_interp -quintic
            e.g. -volreg_interp -Fourier
            default: -cubic

        Please see '3dvolreg -help' for more information.

    -volreg_method METHOD   : specify method for EPI motion correction

            e.g. -volreg_method 3dAllineate
            default: 3dvolreg

        Use this option to specify which program should be run to perform
        EPI to EPI base motion correction over time.

        Please see '3dvolreg -help' for more information.

    -volreg_motsim          : generate motion simulated time series

        Use of this option will result in a 'motsim' (motion simulation)
        time series dataset that is akin to an EPI dataset altered only
        by motion and registration (no BOLD, no signal drift, etc).

        This dataset can be used to generate regressors of no interest to
        be used in the regression block.

        rcr - note relevant options once they are in

        Please see '@simulate_motion -help' for more information.

    -volreg_no_volreg       : omit 3dvolreg operation in the volreg block

            e.g. -volreg_no_volreg
                 -regress_motion_file motion_params.1D

        For EPI data that is already aligned (registered at the scanner?), one
        might still want to align to the anat, to a template, and possibly do
        distortion correction, concatenating the transformations in the volreg
        block.  So process the data as usual, except that the 3dvolreg xform
        will be replaced by an identity xform.

        One would typically also want to pass parameters from motion
        registration to the regress block.  Adding these two options to an
        otherwise typical command would generally be appropriate.

        The B Feige option.

        See also '-regress_motion_file'.

    -volreg_opts_ms OPTS ... : specify extra options for @simulate_motion

            e.g. -volreg_opts_ms -save_workdir

        This option can be used to pass extra options directly to the
        @simulate_motion command.

        See also -volreg_motsim.
        Please see '@simulate_motion -help' for more information.

    -volreg_opts_ewarp OPTS ... : specify extra options for EPI warp steps

            e.g. -volreg_opts_ewarp -short

        This option allows the user to add extra options to the commands
        used to apply combined transformations to EPI data, warping it to
        its final grid space (currently via either 3dAllineate or 
        3dNwarpApply).

        Please see '3dAllineate -help' for more information.
        Please see '3dNwarpApply -help' for more information.

    -volreg_opts_vr OPTS ... : specify extra options for 3dvolreg

            e.g. -volreg_opts_vr -twopass
            e.g. -volreg_opts_vr -noclip -nomaxdisp

        This option allows the user to add extra options to the 3dvolreg
        command.  Note that only one -volreg_opts_vr should be applied,
        which may be used for multiple 3dvolreg options.

        Please see '3dvolreg -help' for more information.

    -volreg_no_extent_mask  : do not create and apply extents mask

            default: apply extents mask

        This option says not to create or apply the extents mask.

        The extents mask:

        When EPI data is transformed to the anatomical grid in either orig
        or tlrc space (i.e. if -volreg_align_e2a or -volreg_tlrc_warp is
        applied), then the complete EPI volume will only cover part of the
        resulting volume space.  Worse than that, the coverage will vary
        over time, as motion will alter the final transformation (remember
        that volreg, EPI->anat and ->tlrc transformations are all combined,
        to prevent multiple resampling steps).  The result is that edge
        voxels will sometimes have valid data and sometimes not.

        The extents mask is made from an all-1 dataset that is warped with
        the same per-TR transformations as the EPI data.  The intersection
        of the result is the extents mask, so that every voxel in the
        extents mask has data at every time point.  Voxels that are not
        are missing data from some or all TRs.

        It is called the extents mask because it defines the 'bounding box'
        of valid EPI data.  It is not quite a tiled box though, as motion
        changes the location slightly, per TR.

        See also -volreg_align_e2a, -volreg_tlrc_warp.
        See also the 'extents' mask, in the "MASKING NOTE" section above.

    -volreg_regress_per_run : regress motion parameters from each run

        === This option has been replaced by -regress_motion_per_run. ===

    -volreg_tlrc_adwarp     : warp EPI to +tlrc space at end of volreg step

            default: stay in +orig space

        With this option, the EPI data will be warped to standard space
        (via adwarp) at the end of the volreg processing block.  Further
        processing through regression will be done in standard space.

        This option is useful for applying a manual Talairach transform,
        which does not work with -volreg_tlrc_warp.  To apply one from
        @auto_tlrc, -volreg_tlrc_warp is recommended.

        The resulting voxel grid is the minimum dimension, truncated to 3
        significant bits.  See -volreg_warp_dxyz for details.

        Note: this step requires a transformed anatomy, which can come from
        the -tlrc_anat option or from -copy_anat importing an existing one.

        Please see 'WARP TO TLRC NOTE' above, for additional details.
        See also -volreg_tlrc_warp, -volreg_warp_dxyz, -tlrc_anat,
        -copy_anat.

    -volreg_tlrc_warp       : warp EPI to +tlrc space at volreg step

            default: stay in +orig space

        With this option, the EPI data will be warped to standard space
        in the volreg processing block.  All further processing through
        regression will be done in standard space.

        Warping is done with volreg to apply both the volreg and tlrc
        transformations in a single step (so a single interpolation of the
        EPI data).  The volreg transformations (for each volume) are stored
        and multiplied by the +tlrc transformation, while the volume
        registered EPI data is promptly ignored.

        The volreg/tlrc (affine or non-linear) transformation is then
        applied as a single concatenated warp to the unregistered data.

        Note that the transformation concatenation is not possible when
        using the 12-piece manual transformation (see -volreg_tlrc_adwarp
        for details).

        The resulting voxel grid is the minimum dimension, truncated to 3
        significant bits.  See -volreg_warp_dxyz for details.

        Note: this step requires a transformed anatomy, which can come from
        the -tlrc_anat option or from -copy_anat importing an existing one.

        Please see 'WARP TO TLRC NOTE' above, for additional details.
        See also -volreg_tlrc_adwarp, -volreg_warp_dxyz, -tlrc_anat,
        -volreg_warp_master, -copy_anat.

    -volreg_warp_dxyz DXYZ  : grid dimensions for _align_e2a or _tlrc_warp

            e.g. -volreg_warp_dxyz 3.5
            default: min dim truncated to 3 significant bits
                     (see description, below)

        This option allows the user to specify the grid size for output
        datasets from the -volreg_tlrc_warp and -volreg_align_e2a options.
        In either case, the output grid will be isotropic voxels (cubes).

        By default, DXYZ is the minimum input dimension, truncated to
        3 significant bits (for integers, starts affecting them at 9, as
        9 requires 4 bits to represent).

        Some examples:
            ----------------------------  (integer range, so >= 4)
            8.00   ...  9.99   --> 8.0
            ...
            4.00   ...  4.99   --> 4.0
            ----------------------------  (3 significant bits)
            2.50   ...  2.99   --> 2.5
            2.00   ...  2.49   --> 2.0
            1.75   ...  1.99   --> 1.75
            1.50   ...  1.74   --> 1.5
            1.25   ...  1.49   --> 1.25
            1.00   ...  1.24   --> 1.0
            0.875  ...  0.99   --> 0.875
            0.75   ...  0.874  --> 0.75
            0.625  ...  0.74   --> 0.625
            0.50   ...  0.624  --> 0.50
            0.4375 ...  0.49   --> 0.4375
            0.375  ...  0.4374 --> 0.375
            ...

        Preferably, one can specify the new dimensions via -volreg_warp_master.

      * As of 2024.04.07: values just under a 3 bit limit will round up.
        The minimum dimension will first be scaled up by a factor of 1.0001
        before the truncation.  For example, 2.9998 will "round" up to 3.0,
        while 2.9997 will truncate down to 2.5.

        For a demonstration, try:

            afni_python_wrapper.py -eval 'test_truncation()'

        See also -volreg_warp_master.

    -volreg_warp_final_interp METHOD : set final interpolation method

            e.g. -volreg_warp_final_interp wsinc5
            default: none (use defaults of called programs)

        This option allows the user to specify the final interpolation
        method used when warping data or concatenating warps.  This applies
        to computation of a final/output volume, after any transformations
        are already known.  Examples include:

            - all combined non-NN warp cases, such as for the main EPI
              datasets from concatenated transformations
              (both affine and non-linear)
              (NN warps are where nearest neighbor is not automatic)
            - final EPI (warped vr base)
            - anatomical followers

        These options are currently applied via:

            3dAllineate -final
            3dNwarpApply -ainterp

        Common choices:

            NN          : nearest neighbor
            linear      : \\
            cubic       : as stated, or "tri" versions, e.g. trilinear
                          (these apply to 3dAllineate and 3dNwarpApply)
            quintic     : /
          * wsinc5      : nice interpolation, less blur, sharper edges
                          ==> the likely use case

        Please see '3dAllineate -help' for more details.
        Please see '3dNwarpApply -help' for more details.

    -volreg_warp_master MASTER : master dataset for volreg warps

            e.g. -volreg_warp_master my_fave_grid+orig
            e.g. -volreg_warp_master my_fave_grid+tlrc
            default: anatomical grid at truncated voxel size
                     (if applicable)

        This option allows the user to specify a dataset grid to warp
        the registered EPI data onto.  The voxels need not be isotropic.

        One can apply -volreg_warp_dxyz in conjunction, to specify the
        master box, along with an isotropic voxel size.
        
        It is up to the user to be sure the MASTER grid is in a suitable
        location for the results.

        See also -volreg_warp_dxyz.

    -volreg_zpad N_SLICES   : specify number of slices for -zpad

            e.g. -volreg_zpad 4
            default: -volreg_zpad 1

        This option allows the user to specify the number of slices applied
        via the -zpad option to 3dvolreg.

    -surf_anat ANAT_DSET    : specify surface volume dataset

            e.g. -surf_anat SUMA/sb23_surf_SurfVol+orig

        This option is required in order to do surface-based analysis.

        This volumetric dataset should be the one used for generation of
        the surface (and therefore should be in perfect alignment).  It may
        be output by the surface generation software.

        Unless specified by the user, the processing script will register
        this anatomy with the current anatomy.

        Use -surf_anat_aligned if the surf_anat is already aligned with the
        current experiment.

        Use '-surf_anat_has_skull no' if the surf_anat has already been
        skull stripped.

        Please see '@SUMA_AlignToExperiment -help' for more details.
        See also -surf_anat_aligned, -surf_anat_has_skull.
        See example #8 for typical usage.

    -surf_spec spec1 [spec2]: specify surface specification file(s)

            e.g. -surf_spec SUMA/sb23_?h_141_std.spec

        Use this option to provide either 1 or 2 spec files for surface
        analysis.  Each file must have lh or rh in the name (to encode
        the hemisphere), and that can be their only difference.  So if
        the files do not have such a naming pattern, they should probably
        be copied to new files that do.  For example, consider the spec
        files included with the AFNI_data4 sample data:

            SUMA/sb23_lh_141_std.spec
            SUMA/sb23_rh_141_std.spec

    -surf_A surface_A       : specify first surface for mapping

            e.g. -surf_A smoothwm
            default: -surf_A smoothwm

        This option allows the user to specify the first (usually inner)
        surface for use when mapping from the volume and for blurring.
        If the option is not given, the smoothwm surface will be assumed.

    -surf_B surface_B       : specify second surface for mapping

            e.g. -surf_B pial
            default: -surf_B pial

        This option allows the user to specify the second (usually outer)
        surface for use when mapping from the volume (not for blurring).
        If the option is not given, the pial surface will be assumed.

    -surf_blur_fwhm FWHM    :  NO LONGER VALID

        Please use -blur_size, instead.

    -blur_filter FILTER     : specify 3dmerge filter option

            e.g. -blur_filter -1blur_rms
            default: -1blur_fwhm

        This option allows the user to specify the filter option from
        3dmerge.  Note that only the filter option is set here, not the
        filter size.  The two parts were separated so that users might
        generally worry only about the filter size.

        Please see '3dmerge -help' for more information.
        See also -blur_size.

    -blur_in_automask       : apply 3dBlurInMask -automask

        This option forces use of 3dBlurInMask -automask, regardless of
        whether other masks exist and are being applied.

        Note that one would not want to apply -automask via -blur_opts_BIM,
        as that might result in failure because of multiple -mask options.

        Note that -blur_in_automask implies '-blur_in_mask yes'.

        Please see '3dBlurInMask -help' for more information.
        See also -blur_in_mask, -blur_opts_BIM.

    -blur_in_mask yes/no    : specify whether to restrict blur to a mask

            e.g. -blur_in_mask yes
            default: no

        This option allows the user to specify whether to use 3dBlurInMask
        instead of 3dmerge for blurring.

        Note that the algorithms are a little different, and 3dmerge comes
        out a little more blurred.

        Note that 3dBlurInMask uses only FWHM kernel size units, so the
        -blur_filter should be either -1blur_fwhm or -FWHM.

        Please see '3dBlurInMask -help' for more information.
        Please see '3dmerge -help' for more information.
        See also -blur_filter.

    -blur_opts_BIM OPTS ...  : specify extra options for 3dBlurInMask

            e.g. -blur_opts_BIM -automask

        This option allows the user to add extra options to the 3dBlurInMask
        command.  Only one -blur_opts_BIM should be applied, which may be
        used for multiple 3dBlurInMask options.

        This option is only useful when '-blur_in_mask yes' is applied.

        Please see '3dBlurInMask -help' for more information.
        See also -blur_in_mask.

    -blur_opts_merge OPTS ... : specify extra options for 3dmerge

            e.g. -blur_opts_merge -2clip -20 50

        This option allows the user to add extra options to the 3dmerge
        command.  Note that only one -blur_opts_merge should be applied,
        which may be used for multiple 3dmerge options.

        Please see '3dmerge -help' for more information.

    -blur_size SIZE_MM      : specify the size, in millimeters

            e.g. -blur_size 6.0
            default: 4

        This option allows the user to specify the size of the blur used
        by 3dmerge (or another applied smoothing program).  It is applied
        as the 'bmm' parameter in the filter option (such as -1blur_fwhm)
        in 3dmerge.

        Note the relationship between blur sizes, as used in 3dmerge:

            sigma = 0.57735027 * rms = 0.42466090 * fwhm
            (implying fwhm = 1.359556 * rms)

        Programs 3dmerge and 3dBlurInMask apply -blur_size as an additional
        gaussian blur.  Therefore smoothing estimates should be computed
        per subject for the correction for multiple comparisons.

        Programs 3dBlurToFWHM and SurfSmooth apply -blur_size as the
        resulting blur, and so do not require blur estimation.

        Please see '3dmerge -help'      for more information.
        Please see '3dBlurInMask -help' for more information.
        Please see '3dBlurToFWHM -help' for more information.
        Please see 'SurfSmooth -help'   for more information.
        See also -blur_filter.

    -blur_to_fwhm           : blur TO the blur size (not add a blur size)

        This option changes the program used to blur the data.  Instead of
        using 3dmerge, this applies 3dBlurToFWHM.  So instead of adding a
        blur of size -blur_size (with 3dmerge), the data is blurred TO the
        FWHM of the -blur_size.

        Note that 3dBlurToFWHM should be run with a mask.  So either:
            o  put the 'mask' block before the 'blur' block, or
            o  use -blur_in_automask
        It is not appropriate to include non-brain in the blur estimate.

        Note that extra options can be added via -blur_opts_B2FW.

        Please see '3dBlurToFWHM -help' for more information.
        See also -blur_size, -blur_in_automask, -blur_opts_B2FW.

    -blur_opts_B2FW OPTS ... : specify extra options for 3dBlurToFWHM

            e.g. -blur_opts_B2FW -rate 0.2 -temper

        This allows the user to add extra options to the 3dBlurToFWHM
        command.  Note that only one -blur_opts_B2FW should be applied,
        which may be used for multiple 3dBlurToFWHM options.

        Please see '3dBlurToFWHM -help' for more information.

    -mask_apply TYPE        : specify which mask to apply in regression

            e.g. -mask_apply group

        If possible, masks will be made for the EPI data, the subject
        anatomy, the group anatomy and EPI warp extents.  This option is
        used to specify which of those masks to apply to the regression.
        One can specify a pre-defined TYPE, or a user-specified one that
        is defined via -anat_follower_ROI or -mask_import, for example.

           Valid pre-defined choices:  epi, anat, group, extents.
           Valid user-defined choices: mask LABELS specified elsewhere.

        A subject 'anat' mask will be created if the EPI anat anatomy are
        aligned, or if the EPI data is warped to standard space via the
        anat transformation.  In any case, a skull-stripped anat will exist.

        A 'group' anat mask will be created if the 'tlrc' block is used
        (via the -blocks or -tlrc_anat options).  In such a case, the anat
        template will be made into a binary mask.

        This option makes -regress_apply_mask obsolete.

        See "MASKING NOTE" and "DEFAULTS" for details.
        See also -blocks.
        See also -mask_import.

    -mask_dilate NUM_VOXELS : specify the automask dilation

            e.g. -mask_dilate 3
            default: 1

        By default, the masks generated from the EPI data are dilated by
        1 step (voxel), via the -dilate option in 3dAutomask.  With this
        option, the user may specify the dilation.  Valid integers must
        be at least zero.

        Note that 3dAutomask dilation is a little different from the
        natural voxel-neighbor dilation.

        Please see '3dAutomask -help' for more information.
        See also -mask_type.

    -mask_epi_anat yes/no : apply epi_anat mask in place of EPI mask

            e.g. -mask_epi_anat yes

        An EPI mask might be applied to the data either for simple
        computations (e.g. global brain correlation, GCOR), or actually
        applied to the EPI data.  The EPI mask $full_mask is used for most
        such computations, by default.

        The mask_epi_anat dataset is an intersection of full_mask and
        mask_anat, and might be better suited to such computations.

        Use this option to apply mask_epi_anat in place of full_mask.

    -mask_import LABEL MSET : import a final grid mask with the given label

            e.g. -mask_import Tvent template_ventricle_3mm+tlrc

      * Note: -ROI_import basically makes -mask_import unnecessary.

        Use this option to import a mask that is aligned with the final
        EPI data _and_ is on the final grid (with -ROI_import, the ROI will
        be resampled onto the final grid).

            o  this might be based on the group template
            o  this should already be resampled appropriately
            o  no warping or resampling will be done to this dataset

        This mask can be applied via LABEL as other masks, using options
        like: -regress_ROI, -regress_ROI_PC, -regress_make_corr_vols,
              -regress_anaticor_label, -mask_intersect, -mask_union.

        For example, one might import a ventricle mask from the template,
        intersect it with the subject specific CSFe (eroded CSF) mask,
        and possibly take the union with WMe (eroded white matter), before
        using the result for principle component regression, as in:

            -mask_import Tvent template_ventricle_3mm+tlrc \\
            -mask_intersect Svent CSFe Tvent               \\
            -mask_union WM_vent Svent WMe                  \\
            -regress_ROI_PC WM_vent 3                      \\

        See also -ROI_import, -regress_ROI, -regress_ROI_PC,
                 -regress_make_corr_vols, -regress_anaticor_label,
                 -mask_intersect, -mask_union.

    -mask_intersect NEW_LABEL MASK_A MASK_B : intersect 2 masks

            e.g. -mask_intersect Svent CSFe Tvent

        Use this option to intersect 2 known masks to create a new mask.
        NEW_LABEL will be the label of the result, while MASK_A and MASK_B
        should be labels for existing masks.

        One could use this to intersect a template ventricle mask with each
        subject's specific CSFe (eroded CSF) mask from 3dSeg, for example.

        See -mask_import for more details.

    -mask_union NEW_LABEL MASK_A MASK_B : take union of 2 masks

            e.g. -mask_union WM_vent Svent WMe

        Use this option to take the union of 2 known masks to create a new
        mask.  NEW_LABEL will be the label of the result, while MASK_A and
        MASK_B should be labels for existing masks.

        One could use this to create union of CSFe and WMe for principle
        component regression, for example.

        See -mask_import for more details.

    -mask_opts_automask ... : specify extra options for 3dAutomask

            e.g. -mask_opts_automask -clfrac 0.2 -dilate 1

        This allows one to add extra options to the 3dAutomask command used
        to create a mask from the EPI data.

        Please see '3dAutomask -help' for more information.

    -mask_rm_segsy Y/N  : choose whether to delete the Segsy directory

            e.g. -mask_rm_segsy no
            default: yes

        This option is a companion to -mask_segment_anat.

        In the case of running 3dSeg to segment the anatomy, a resulting
        Segsy directory is created.  Since the main result is a Classes
        dataset, and to save disk space, the Segsy directory is removed
        by default.  Use this option to preserve it.

        See also -mask_segment_anat.

    -mask_segment_anat Y/N  : choose whether to segment anatomy

            e.g. -mask_segment_anat yes
            default: no (if anat_final is skull-stripped)

        This option controls whether 3dSeg is run to segment the anatomical
        dataset.  Such a segmentation would then be resampled to match the
        grid of the EPI data.

        When this is run, 3dSeg creates the Classes dataset, which is a
        composition mask of the GM/WM/CSF (gray matter, white matter and
        cerebral spinal fluid) regions.  Then 3dresample is used to create
        Classes_resam, the same mask but at the resolution of the EPI.

        Such a dataset might have multiple uses, such as tissue-based
        regression.  Note that for such a use, the ROI time series should
        come from the volreg data, before any blur.

      * Mask labels created by -mask_segment_anat and -mask_segment_erode
        can be applied with -regress_ROI and -regress_ROI_PC.

      * The CSF mask is of ALL CSF (not just in the ventricles), and is
        therefore not very appropriate to use with tissue-based regression.

        Consider use of -anat_uniform_method along with this option.

        Please see '3dSeg -help' for more information.
        Please see '3dUnifize -help' for more information.
        See also -mask_rm_segsy, -anat_uniform_method -mask_segment_erode,
         and -regress_ROI, -regress_ROI_PC.

    -mask_segment_erode Y/N

            e.g. -mask_segment_erode Yes
            default: yes (if -regress_ROI or -regress_anaticor)

        This option is a companion to -mask_segment_anat.

        Anatomical segmentation is used to create GM (gray matter), WM
        (white matter) and CSF masks.  When the _erode option is applied,
        eroded versions of those masks are created via 3dmask_tool.

        See also -mask_segment_anat, -regress_anaticor.
        Please see '3dmask_tool -help' for more information.

    -mask_test_overlap Y/N  : choose whether to test anat/EPI mask overlap

            e.g. -mask_test_overlap No
            default: Yes

        If the subject anatomy and EPI masks are computed, then the default
        operation is to run 3dABoverlap to evaluate the overlap between the
        two masks.  Output is saved in a text file.

        This option allows one to disable such functionality.

        Please see '3dABoverlap -help' for more information.

    -mask_type TYPE         : specify 'union' or 'intersection' mask type

            e.g. -mask_type intersection
            default: union

        This option is used to specify whether the mask applied to the
        analysis is the union of masks from each run, or the intersection.
        The only valid values for TYPE are 'union' and 'intersection'.

        This is not how to specify whether a mask is created, that is
        done via the 'mask' block with the '-blocks' option.

        Please see '3dAutomask -help', '3dMean -help' or '3dcalc -help'.
        See also -mask_dilate, -blocks.

    -combine_method METHOD  : specify method for combining echoes

            e.g. -combine_method OC
            default: OC

        When using the 'combine' block to combine echoes (for each run),
        this option can be used to specify the method used.  There are:

            - basic methods
            - methods using tedana.py (or similar) from Prantik
            - methods using tedana from the MEICA group


        ---- basic combine methods (that do not use any tedana) ----

            methods - AFNI only
            -------
            mean             : simple mean of echoes
            OC               : optimally combined (via @compute_OC_weights)
                               (current default is OC_A)
            OC_A             : original log(mean()) regression method
            OC_B             : newer log() time series regression method
                               (there is little difference between OC_A
                               and OC_B)

        ---- combine methods that use Prantik's "original" tedana.py ----

           Prantik's tedana.py is run using the 'tedana*' combine methods.

              Prantik's tedana.py requires python 2.7.

              By default, tedana.py will be applied from the AFNI
              installation directory.

              Alternatively, one can specify the location of a different
              tedana.py using -combine_tedana_path.  And if it is 
              preferable to run it as an executable (as opposed to running
              it via 'python PATH/TO/tedana.py'), one can tell this to
              tedana_wrapper.py by applying:
                     -combine_opts_tedwrap -tedana_is_exec

            methods - Prantik tedana
            -------
            OC_tedort        : OC, and pass tedana orts to regression
            tedana           : run tedana.py, using output dn_ts_OC.nii
            tedana_OC        : run tedana.py, using output ts_OC.nii
                               (i.e. use tedana.py for optimally combined)
            tedana_OC_tedort : tedana_OC, and include tedana orts


        ---- combine methods that use tedana from the MEICA group ----

           The MEICA group tedana is specified with 'm_tedana*' methods.

              This tedana requires python 3.6+.

              AFNI does not distribute this version of tedana, so it must
              be in the PATH.  For installation details, please see:

                 https://tedana.readthedocs.io/en/stable/installation.html

            methods - MEICA group tedana
            -------
            m_tedana          : tedana from MEICA group (dn_ts_OC.nii.gz)
            m_tedana_OC       : tedana OC from MEICA group (ts_OC.nii.gz)
            m_tedana_m_tedort : tedana from MEICA group (dn_ts_OC.nii.gz)
                                "tedort" from MEICA group
                                (--tedort: "good" projected from "bad")
            m_tedana_OC_tedort: MEICA OC, and tedorts to 3dDeconvolve
            OC_m_tedort       : AFNI  OC, and tedorts to 3dDeconvolve


        The OC/OC_A combine method is from Posse et. al., 1999, and then
        applied by Kundu et. al., 2011 and presented by Javier in a 2017
        summer course.

        The 'tedort' methods for Prantik's tedana.py are applied using
        @extract_meica_ortvec, which projects the 'good' MEICA components
        out of the 'bad' ones, and saves those as regressors to be applied
        later.  Otherwise, some of the 'good' components are removed with
        the 'bad.  The tedort method can be applied with either AFNI OC or
        tedana OC (meaning the respective OC method would be applied to
        combine the echoes, and the tedort components will be passed on to
        the regress block).

        The 'm_tedana_m_tedort' method for the MEICA group's passes
        option --tedort to 'tedana', and tedana does the "good" from "bad"
        projection before projecting the modified "bad" components from the
        time series.  The 'm_tedana_OC_tedort' method gets the MEICA OC data,
        and passes the MEICA tedort regressors on to the regress block.  The
        'OC_m_tedort' methed gets the AFNI OC result, and passes the MEICA
        tedort regressors on to the regress block.

        Please see '@compute_OC_weights -help' for more information.
        Please see '@extract_meica_ortvec -help' for more information.
        See also -combine_tedana_path.

    -combine_opts_tedana OPT OPT ... : specify extra options for tedana.py

            e.g. -combine_opts_tedana --sourceTEs=-1 --kdaw=10 --rdaw=1

        Use this option to pass extra options through to tedana.py.
        This applies to any tedana-based -combine_method.

        See also -combine_method.

    -combine_opts_tedwrap OPT OPT ... : pass options to tedana_wrapper.py

            e.g. -combine_opts_tedwrap -tedana_is_exec

        Use this option to pass extra options to tedana_wrapper.py.
        This applies to any tedana-based -combine_method.

    -combine_tedana_path PATH : specify path to tedana.py

            e.g. -combine_tedana_path ~/testbin/meica.libs/tedana.py
            default: from under afni binaries directory

        If one wishes to use a version of tedana.py other than what comes
        with AFNI, this option allows one to specify that file.

        This applies to any tedana-based -combine_method.

        See also -combine_method.

    -combine_tedort_reject_midk yes/no : reject midk components

            e.g. -combine_tedort_reject_midk no
            default: yes (matching original method)

        Is may not be clear whether the midk (mid-Kappa) components are
        good ones or bad.  If one is not so sure, it might make sense not
        to project them out.  To refrain from projecting them out, use
        this option with 'no' (the default is 'yes' to match the original
        method).

    -combine_tedana_save_all yes/no : save all ted wrapper preproc files

            e.g. -combine_tedana_save_all yes
            default: no (save only 3dZcat stacked dataset)

        Use the option to save all of the preprocessing files created by
        tedana_wrapper.py (when calling tedana.py).  The default is to save
        only the 3dZcat stacked dataset, which is then passed to tedana.py.

        Please see 'tedana_wrapper.py -help' for details.

    -scale_max_val MAX      : specify the maximum value for scaled data

            e.g. -scale_max_val 1000
            default 200

        The scale step multiples the time series for each voxel by a
        scalar so that the mean for that particular run is 100 (allowing
        interpretation of EPI values as a percentage of the mean).

        Values of 200 represent a 100% change above the mean, and so can
        probably be considered garbage (or the voxel can be considered
        non-brain).  The output values are limited so as not to sacrifice
        the precision of the values of short datasets.  Note that in a
        short (2-byte integer) dataset, a large range of values means
        bits of accuracy are lost for the representation.

        No max will be applied if MAX is <= 100.

        Please see 'DATASET TYPES' in the output of '3dcalc -help'.
        See also -scale_no_max.

    -scale_no_max           : do not apply a limit to the scaled values

        The default limit for scaled data is 200.  Use of this option will
        remove any limit from being applied.

        A limit on the scaled data is highly encouraged when working with
        'short' integer data, especially when not applying a mask.

        See also -scale_max_val.

    -regress_3dD_stop       : 3dDeconvolve should stop after X-matrix gen

        Use this option to tell 3dDeconvolve to stop after generating the
        X-matrix (via -x1D_stop).  This is useful if the user only wishes
        to run the regression through 3dREMLfit.

        See also -regress_reml_exec.

    -regress_anaticor       : generate errts using ANATICOR method

        Apply the ANATICOR method of HJ Jo, regressing out the WMeLocal
        time series, which varies across voxels.

        WMeLocal is the average time series from all voxels within 45 mm
        which are in the eroded white matter mask.

        The script will run the standard regression via 3dDeconvolve (or
        stop after setting up the X-matrix, if the user says to), and use
        that X-matrix, possibly censored, in 3dTproject.  The WMeLocal time
        series is applied along with the X-matrix to get the result.

        Note that other 4-D time series might be regressed out via the
        3dTproject step, as well.

        In the case of task-based ANATICOR, -regress_reml_exec is required,
        which uses 3dREMLfit to regress the voxel-wise ANATICOR regressors.

        This option implies -mask_segment_anat and -mask_segment_erode.

      * Consider use of -regress_anaticor_fast, instead.

        Please see "@ANATICOR -help" for more detail, including the paper
        reference for the method.
        See also -mask_segment_anat, -mask_segment_erode, -regress_3dD_stop.
        See also -regress_reml_exec.

    -regress_anaticor_label LABEL : specify LABEL for ANATICOR ROI

        To go with either -regress_anaticor or -regress_anaticor_fast,
        this option is used the specify an alternate label of an ROI
        mask to be used in the ANATICOR step.  The default LABEL is WMe
        (eroded white matter from 3dSeg).

        When this option is included, it is up to the user to make sure
        afni_proc.py has such a label, either by including options:
            -mask_segment_anat (and possibly -mask_segment_erode),
            -regress_ROI_PC, -regress_ROI, or -anat_follower_ROI.

        Any known label made via those options may be used.

        See also -mask_segment_anat, -mask_segment_erode, -regress_ROI_PC,
            -anat_follower_ROI, -ROI_import.

    -regress_anaticor_radius RADIUS : specify RADIUS for local WM average

        To go with -regress_anaticor or -regress_anaticor_fast, use this
        option to specify the radius.  In the non-fast case that applies
        to spheres within which local white matter is averaged.  In the
        fast case, the radius is applied as the HWHM (half width at half
        max).  A small radius means the white matter is more local.

        If no white matter is found within the specified distance of some
        voxel, the effect is that ANATICOR will simply not happen at that
        voxel.  That is a reasonable "failure" case, in that it says there
        is simply no white matter close enough to regress out (again, at
        the given voxel).

        See also -regress_anaticor or -regress_anaticor_fast.

    -regress_anaticor_fast  : generate errts using fast ANATICOR method

        This applies basically the same method as with -regress_anaticor,
        above.  While -regress_anaticor creates WMeLocal dataset by
        getting the average white matter voxel within a fixed radius, the
        'fast' method computes it by instead integrating the white matter
        over a gaussian curve.

        There some basic effects of using the 'fast' method:

            1. Using a Gaussian curve to compute each voxel-wise regressor
               gives more weight to the white matter that is closest to
               each given voxel.  The FWHM of this 3D kernel is specified
               by -regress_anaticor_fwhm, with a default of 30 mm.

            2. If there is no close white matter (e.g. due to a poor
               segmentation), the Gaussian curve will likely find white
               matter far away, instead of creating an empty regressor.

            3. This is quite a bit faster, because it is done by creating
               a time series of all desired white matter voxels, blurring
               it, and then just regressing out that dataset.  The blur
               operation is much faster than a localstat one.

        Please see "@ANATICOR -help" for more detail, including the paper
        reference for the method.
        See also -regress_anaticor_fwhm/
        See also -mask_segment_anat, -mask_segment_erode, -regress_3dD_stop.
        See also -regress_anaticor.

    -regress_anaticor_fwhm FWHM  : specify FWHM for 'fast' ANATICOR, in mm

            e.g.     -regress_anaticor_fwhm 20
            default: -regress_anaticor_fwhm 30


     ** This option is no longer preferable.  The newer application of
        -regress_anaticor_fast "thinks" in terms of a radius, like HWHM.
        So consider -regress_anaticor_radius for all cases.


        This option applies to -regress_anaticor_fast.

        The 'fast' ANATICOR method blurs the time series of desired white
        matter voxels using a Gaussian kernel with the given FWHM (full
        width at half maximum).

        To understand the FWHM, note that it is essentially the diameter of
        a sphere where the contribution from points at that distance
        (FWHM/2) contribute half as much as the center point.  For example,
        if FWHM=10mm, then any voxel at a distance of 5 mm would contribute
        half as much as a voxel at the center of the kernel.

        See also -regress_anaticor_fast.

    -regress_anaticor_term_frac FRAC : specify termination fraction

            e.g.     -regress_anaticor_term_frac .25
            default: -regress_anaticor_term_frac .5

        In the typical case of -regress_anaticor_fast, to make it behave
        very similarly to -regress_anaticor, blurring is applied with a
        Gaussian kernel out to the radius specified by the user, say 30 mm.

        To make this kernel more flat, it is terminated at a fraction of
        the HWHM (half width at half max, say 0.5), while the blur radius
        is extended by the reciprocal (to keep the overall distance fixed).
        So that means blurring with a wider Gaussian kernel, but truncating
        it to stay fixed at the given radius.

        If the fraction were 1.0, the relative contribution at the radius
        would be 0.5 of the central voxel (by definition of FWHM/HWHM).
        At a fraction of 0.5 (default), the relative contribution is 0.84.
        At a fraction of 0.25, the relative contribution is 0.958, seen by:

           afni_util.py -print 'gaussian_at_hwhm_frac(.25)'

        Consider the default fraction of 0.5.  That means we want the
        "radius" of the blur to terminate at 0.5 * HWHM, making it more
        flat, such that the relative contribution at the edge is ~0.84.
        If the specified blur radius is 30 mm, that mean the HWHM should
        actually be 60 mm, and we stop computing at HWHM/2 = 30 mm.  Note
        that the blur in 3dmerge is applied not as a radius (HWHM), but as
        a diameter (FWHM), so these numbers are always then doubled.  In
        this example, it would use FWHM = 120 mm, to achieve a flattened
        30 mm radius Gaussian blur.

        In general, the HWHM widening (by 1/FRAC) makes the inner part of
        the kernel more flat, and then the truncation at FRAC*HWHM makes
        the blur computations still stop at the radius.  Clearly one can
        make a flatter curve with a smaller FRAC.

        To make the curve a "pure Gaussian", with no truncation, consider
        the option -regress_anaticor_full_gaussian.

        Please see "@radial_correlate -help" for more information.
        Please also see:
           afni_util.py -print 'gaussian_at_hwhm_frac.__doc__'
        See also -regress_anaticor_fast, -regress_anaticor_radius,
        -regress_anaticor_full_gaussian.

    -regress_anaticor_full_gaussian yes/no: use full Gaussian blur

            e.g.     -regress_anaticor_full_gaussian yes
            default: -regress_anaticor_full_gaussian no

        When using -regress_anaticor_fast to apply ANATICOR via a Gaussian
        blur, the blur kernel is extended and truncated to stop at the
        -regress_anaticor_radius HWHM of the Gaussian curve, allowing the
        shape to be arbitrarily close to the flat curve applied in the
        original ANATICOR method via -regress_anaticor.

        Use this option to prevent the truncation, so that a full Gaussian
        blur is applied at the specified HWHM radius (FWHM = 2*HWHM).

      * Note that prior to 22 May 2019, the full Gaussian was always
        applied with -regress_anaticor_fast.  This marks an actual change
        in processing.

        See also -regress_anaticor_fast, -regress_anaticor_radius.

    -regress_apply_mask     : apply the mask during scaling and regression

        By default, any created union mask is not applied to the analysis.
        Use this option to apply it.

     ** This option is essentially obsolete.  Please consider -mask_apply
        as a preferable option to choose which mask to apply.

        See "MASKING NOTE" and "DEFAULTS" for details.
        See also -blocks, -mask_apply.

    -regress_apply_mot_types TYPE1 ... : specify motion regressors

            e.g. -regress_apply_mot_types basic
            e.g. -regress_apply_mot_types deriv
            e.g. -regress_apply_mot_types demean deriv
            e.g. -regress_apply_mot_types none
            default: demean

        By default, the motion parameters from 3dvolreg are applied in the
        regression, but after first removing the mean, per run.  This is
        the application of the 'demean' regressors.

        This option gives the ability to choose a combination of:

            basic:  dfile_rall.1D - the parameters straight from 3dvolreg
                    (or an external motion file, see -regress_motion_file)
            demean: 'basic' params with the mean removed, per run
            deriv:  per-run derivative of 'basic' params (de-meaned)
            none:   do not regress any motion parameters
                    (but one can still censor)

     ** Note that basic and demean cannot both be used, as they would cause
        multi-collinearity with the constant drift parameters.

     ** Note also that basic and demean will give the same results, except
        for the betas of the constant drift parameters (and subject to
        computational precision).

     ** A small side effect of de-meaning motion parameters is that the
        constant drift terms should evaluate to the mean baseline.

        See also -regress_motion_file, -regress_no_motion_demean,
        -regress_no_motion_deriv, -regress_no_motion.

    -regress_apply_ricor yes/no : apply ricor regs in final regression

            e.g.     -regress_apply_ricor yes
            default: no

        This is from a change in the default behavior 30 Jan 2012.  Prior
        to then, the 13 (?) ricor regressors from slice 0 would be applied
        in the final regression (mostly accounting for degrees of freedom).
        But since resting state analysis relies on a subsequent correlation
        analysis, it seems cleaner not to regress them (a second time).

    -regress_bandpass lowf highf : bandpass the frequency range

            e.g.  -regress_bandpass 0.01 0.1

        This option is intended for use in resting state analysis.

        Use this option to perform bandpass filtering during the linear
        regression.  While such an operation is slow (much slower than the
        FFT using 3dBandpass), doing it during the regression allows one to
        perform (e.g. motion) censoring at the same time.

        This option has a similar effect to running 3dBandpass, e.g. the
        example of '-regress_bandpass 0.01 0.1' is akin to running:

            3dBandpass -ort motion.1D -band 0.01 0.1

        except that it is done in 3dDeconvolve using linear regression.
        And censoring is easy in the context of regression.

        Note that the Nyquist frequency is 0.5/TR.  That means that if the
        TR were >= 5 seconds, there would be no frequencies within the band
        range of 0.01 to 0.1 to filter.  So there is no point to such an
        operation.

        On the flip side, if the TR is 1.0 second or shorter, the range of
        0.01 to 0.1 would remove about 80% of the degrees of freedom (since
        everything above 0.1 is filtered/removed, up through 0.5).  This
        might result in a model that is overfit, where there are almost as
        many (or worse, more) regressors than time points to fit.

        So a 0.01 to 0.1 bandpass filter might make the most sense for a
        TR in [2.0, 3.0], or so.

        A different filter range would affect this, of course.

        See also -regress_censor_motion.

    -regress_basis BASIS    : specify the regression basis function

            e.g. -regress_basis 'BLOCK(4,1)'
            e.g. -regress_basis 'BLOCK(5)'
            e.g. -regress_basis 'TENT(0,14,8)'
            default: GAM

        This option is used to set the basis function used by 3dDeconvolve
        in the regression step.  This basis function will be applied to
        all user-supplied regressors (please let me know if there is need
        to apply different basis functions to different regressors).

     ** Note that use of dmBLOCK requires -stim_times_AM1 (or AM2).  So
        consider option -regress_stim_types.

     ** If using -regress_stim_types 'file' for a particular regressor,
        the basis function will be ignored.  In such a case, it is safest
        to use 'NONE' for the corresponding basis function.

        Please see '3dDeconvolve -help' for more information, or the link:
            https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
        See also -regress_basis_normall, -regress_stim_times,
                 -regress_stim_types, -regress_basis_multi.

    -regress_basis_multi BASIS BASIS .. : specify multiple basis functions

            e.g. -regress_basis_multi 'BLOCK(30,1)' 'TENT(0,45,16)' \\
                                      'BLOCK(30,1)' dmUBLOCK

        In the case that basis functions vary across stim classes, use
        this option to list a basis function for each class.  The given
        basis functions should correspond to the listed -regress_stim_times
        files, just as the -regress_stim_labels entries do.

        See also -regress_basis.

    -regress_basis_normall NORM : specify the magnitude of basis functions

            e.g. -regress_basis_normall 1.0

        This option is used to set the '-basis_normall' parameter in
        3dDeconvolve.  It specifies the height of each basis function.

        For the example basis functions, -basis_normall is not recommended.

        Please see '3dDeconvolve -help' for more information.
        See also -regress_basis.

    -regress_censor_extern CENSOR.1D : supply an external censor file

            e.g. -regress_censor_extern censor_bad_trs.1D

        This option is used to provide an initial censor file, if there
        is some censoring that is desired beyond the automated motion and
        outlier censoring.

        Any additional censoring (motion or outliers) will be combined.

         See also -regress_censor_motion, -regress_censor_outliers.

    -regress_censor_motion LIMIT : censor TRs with excessive motion

            e.g. -regress_censor_motion 0.3

        This option is used to censor TRs where the subject moved too much.
        "Too much" is decided by taking the derivative of the motion
        parameters (ignoring shifts between runs) and the sqrt(sum squares)
        per TR.  If this Euclidean Norm exceeds the given LIMIT, the TR
        will be censored.

        This option will result in the creation of 3 censor files:

            motion_$subj_censor.1D
            motion_$subj_CENSORTR.txt
            motion_$subj_enorm.1D

        motion_$subj_censor.1D is a 0/1 columnar file to be applied to
        3dDeconvolve via -censor.  A row with a 1 means to include that TR,
        while a 0 means to exclude (censor) it.

        motion_$subj_CENSORTR.txt is a short text file listing censored
        TRs, suitable for use with the -CENSORTR option in 3dDeconvolve.
        The -censor option is the one applied however, so this file is not
        used, but may be preferable for users to have a quick peek at.

        motion_$subj_enorm.1D is the time series that the LIMIT is applied
        to in deciding which TRs to censor.  It is the Euclidean norm of
        the derivatives of the motion parameters.  Plotting this will give
        users a visual indication of why TRs were censored.

        By default, the TR prior to the large motion derivative will also
        be censored.  To turn off that behavior, use -regress_censor_prev
        with parameter 'no'.

        If censoring the first few TRs from each run is also necessary,
        use -regress_censor_first_trs.

        Please see '1d_tool.py -help' for information on censoring motion.
        See also -regress_censor_prev and -regress_censor_first_trs.

    -regress_censor_first_trs N  : censor the first N TRs in each run

            e.g.     -regress_censor_first_trs 3
            default: N = 0

        If, for example, censoring the first 3 TRs per run is desired, a
        user might add "-CENSORTR '*:0-2'" to the -regress_opts_3dD option.
        However, when using -regress_censor_motion, these censoring options
        must be combined into one for 3dDeconvolve.

        The -regress_censor_first_trs censors those TRs along with any with
        large motion.

        See '-censor_first_trs' under '1d_tool.py -help' for details.
        See also '-regress_censor_motion'.

    -regress_censor_prev yes/no  : censor TRs preceding large motion

            default: -regress_censor_prev yes

        Since motion spans two TRs, the derivative is not quite enough
        information to decide whether it is more appropriate to censor
        the earlier or later TR.  To error on the safe side, many users
        choose to censor both.

        Use this option to specify whether to include the previous TR
        when censoring.

        By default this option is applied as 'yes'.  Users may elect not
        not to censor the previous TRs by setting this to 'no'.

        See also -regress_censor_motion.

    -regress_censor_outliers LIMIT : censor TRs with excessive outliers

            e.g. -regress_censor_outliers 0.15

        This option is used to censor TRs where too many voxels are flagged
        as outliers by 3dToutcount.  LIMIT should be in [0.0, 1.0], as it
        is a limit on the fraction of masked voxels.

        '3dToutcount -automask -fraction' is used to output the fraction of
        (auto)masked voxels that are considered outliers at each TR.  If
        the fraction of outlier voxels is greater than LIMIT for some TR,
        that TR is censored out.

        Depending on the scanner settings, early TRs might have somewhat
        higher intensities.  This could lead to the first few TRs of each
        run being censored.  To avoid censoring the first few TRs of each
        run, apply the -regress_skip_first_outliers option.

        Note that if motion is also being censored, the multiple censor
        files will be combined (multiplied) before 3dDeconvolve.

        See '3dToutcount -help' for more details.
        See also -regress_skip_first_outliers, -regress_censor_motion.

    -regress_compute_gcor yes/no : compute GCOR from unit errts

            e.g. -regress_compute_gcor no
            default: yes

        By default, the global correlation (GCOR) is computed from the
        masked residual time series (errts).

        GCOR can be thought of as the result of:
            A1. compute the correlations of each voxel with every other
                --> can be viewed as an NMASK x NMASK correlation matrix
            A2. compute GCOR: the average of the NMASK^2 values

        Since step A1 would take a lot of time and disk space, a more
        efficient computation is desirable:
            B0. compute USET: scale each voxel time series to unit length
            B1. compute GMU: the global mean of this unit dataset
            B2. compute a correlation volume (of each time series with GMU)
            B3. compute the average of this volume

        The actual computation is simplified even further, as steps B2 and
        B3 combine as the L2 norm of GMU.  The result is:
            B2'. length(GMU)^2  (or the sum of squares of GMU)

        The steps B0, B1 and B2' are performed in the proc script.

        Note: This measure of global correlation is a single number in the
              range [0, 1] (not in [-1, 1] as some might expect).

        Note: computation of GCOR requires a residual dataset, an EPI mask,
              and a volume analysis (no surface at the moment).

    -regress_compute_auto_tsnr_stats yes/no : compute auto TSNR stats

            e.g. -regress_compute_auto_tsnr_stats no
            default: yes

        By default, -regress_compute_tsnr_stats is applied with the 'brain'
        mask and the APQC_atlas dataset for the final space, if they exist
        and are appropriate.

        Use this option to prevent automatic computation of those TSNR stats.

        See also -regress_compute_tsnr, -regress_compute_tsnr_stats.

    -regress_compute_tsnr yes/no : compute TSNR dataset from errts

            e.g. -regress_compute_tsnr no
            default: yes

        By default, a temporal signal to noise (TSNR) dataset is created at
        the end of the regress block.  The "signal" is the all_runs dataset
        (input to 3dDeconvolve), and the "noise" is the errts dataset (the
        residuals from 3dDeconvolve).  TSNR is computed (per voxel) as the
        mean signal divided by the standard deviation of the noise.

           TSNR = average(signal) / stdev(noise)

        The main difference between the TSNR datasets from the volreg and
        regress blocks is that the data in the regress block has been
        smoothed and "completely" detrended (detrended according to the
        regression model: including polort, motion and stim responses).

        Use this option to prevent the TSNR dataset computation in the
        'regress' block.

        One can also compute per-ROI statistics over the resulting TSNR
        dataset via -regress_compute_tsnr_stats.

        See also -volreg_compute_tsnr.
        See also -regress_compute_tsnr_stats.

    -regress_compute_tsnr_stats ROI_DSET_LABEL ROI_1 ROI_2 ...
                              : compute TSNR statistics per ROI

            e.g. -regress_compute_tsnr_stats Glasser 4 41 99 999

            e.g. -anat_follower_ROI aeseg epi SUMA/aparc.a2009s+aseg.nii.gz \\
                 -ROI_import Glasser MNI_Glasser_HCP_v1.0.nii.gz            \\
                 -ROI_import faves my.favorite.ROIs.nii.gz                  \\
                 -regress_compute_tsnr_stats aeseg   18 54 11120 12120 2 41 \\
                 -regress_compute_tsnr_stats Glasser 4 41 99 999
                 -regress_compute_tsnr_stats faves   ALL_LT

            default: -regress_compute_tsnr_stats brain 1

        Given:
           - TSNR statistics are being computed in the regress block
           - there is an EPI-grid ROI dataset with label ROI_DSET_LABEL

        Then one can list ROI regions in each ROI dataset to compute TSNR
        statistics over.  Details will be output for each ROI region, such as
        quartiles of the TSNR values, and maximum depth coordinates.  If the
        ROI dataset has a label table, one can use ALL_LT to use all of them.

        This option results in a compute_ROI_stats.tcsh command being run for
        the ROI and TSNR datasets, and the ROI indices of interest.

        ROI datasets (and their respective labels) are made via options like
        -anat_follower_ROI, -ROI_import or even -mask_segment_anat.

      * Is it okay to specify ROI values that do not exist in the ROI dataset.
        That is somewhat expected with subject specific datasets and resampling.

      * This option is currently automatically applied with a 'brain' ROI and
        the relevant APQC_atlas, if appropriate.  To override use of such an
        atlas, specify '-regress_compute_auto_tsnr_stats no'.

        See 'compute_ROI_stats.tcsh -help' for more details.
        See also -anat_follower_ROI, -ROI_import, -regress_compute_tsnr.

    -regress_mask_tsnr yes/no : apply mask to errts TSNR dataset

            e.g. -regress_mask_tsnr yes
            default: no

        By default, a temporal signal to noise (TSNR) dataset is created at
        the end of the regress block.  By default, this dataset is not
        masked (to match what is done in the regression).

        To mask, apply this option with 'yes'.

      * This dataset was originally masked, with the default changing to
        match the regression 22 Feb, 2021.

        See also -regress_compute_tsnr.

    -regress_fout yes/no         : output F-stat sub-bricks

            e.g. -regress_fout no
            default: yes

        This option controls whether to apply -fout in 3dDeconvolve.  The
        default is yes.

    -regress_make_cbucket yes/no : add a -cbucket option to 3dDeconvolve

            default: 'no'

        Recall that the -bucket dataset (no 'c') contains beta weights and
        various statistics, but generally not including baseline terms
        (polort and motion).

        The -cbucket dataset (with a 'c') is a little different in that it
        contains:
            - ONLY betas (no t-stats, no F-stats, no contrasts)
            - ALL betas (including baseline terms)
        So it has one volume (beta) per regressor in the X-matrix.

        The use is generally for 3dSynthesize, to recreate time series
        datasets akin to the fitts, but where the user can request any set
        of parameters to be included (for example, the polort and the main
        2 regressors of interest).

        Setting this to 'yes' will result in the -cbucket option being
        added to the 3dDeconvolve command.

        Please see '3dDeconvolve -help' for more details.

    -regress_make_corr_vols LABEL1 ... : create correlation volume dsets

            e.g. -regress_make_corr_vols aeseg FSvent
            default: one is made against full_mask

        This option is used to specify extra correlation volumes to compute
        based on the residuals (so generally for resting state analysis).

        What is a such a correlation volume?

           Given: errts     : the residuals from the linear regression
                  a mask    : to correlate over, e.g. full_mask == 'brain'

           Compute: for each voxel (in the errts, say), compute the correlation
              against the average over all voxels within the given mask.

         * This is a change (as of Jan, 2020).  This WAS a mean correlation
           (across masked voxels), but now it is a correlation of the mean
           (over masked voxels).

        The labels specified can be from any ROI mask, such as those coming
        via -ROI_import, -anat_follower_ROI, -regress_ROI_PC, or from the
        automatic masks from -mask_segment_anat.

        See also -ROI_import, -anat_follower_ROI, -regress_ROI_PC,
                 -mask_segment_anat.

    -regress_mot_as_ort yes/no : regress motion parameters using -ortvec

            default: yes
            [default changed from 'no' to 'yes' 16 Jan, 2019]

        Applying this option with 'no', motion parameters would be passed
        to 3dDeconvolve using -stim_file and -stim_base, instead of the
        default -ortvec.

        Using -ortvec (the default) produces a "cleaner" 3dDeconvolve
        command, without the many extra -stim_file options.  Otherwise,
        all results should be the same.

    -regress_motion_per_run : regress motion parameters from each run

            default: regress motion parameters catenated across runs

        By default, motion parameters from the volreg block are catenated
        across all runs, providing 6 (assuming 3dvolreg) regressors of no
        interest in the regression block.

        With -regress_motion_per_run, the motion parameters from each run
        are used as separate regressors, providing a total of (6 * nruns)
        regressors.

        This allows for the magnitudes of the regressors to vary over each
        run, rather than using a single (best) magnitude over all runs.
        So more motion-correlated variance can be accounted for, at the
        cost of the extra degrees of freedom (6*(nruns-1)).

        This option will apply to all motion regressors, including
        derivatives (if requested).

        ** This option was previously called -volreg_regress_per_run. **

    -regress_skip_first_outliers NSKIP : ignore the first NSKIP TRs

            e.g. -regress_skip_first_outliers 4
            default: 0

        When using -regress_censor_outliers, any TR with too high of an
        outlier fraction will be censored.  But depending on the scanner
        settings, early TRs might have somewhat higher intensities, leading
        to them possibly being inappropriately censored.

        To avoid censoring any the first few TRs of each run, apply the
        -regress_skip_first_outliers option.

        See also -regress_censor_outliers.

    -regress_compute_fitts       : compute fitts via 3dcalc, not 3dDecon

        This option is to save memory during 3dDeconvolve, in the case
        where the user has requested both the fitts and errts datasets.

        Normally 3dDeconvolve is used to compute both the fitts and errts
        time series.  But if memory gets tight, it is worth noting that
        these datasets are redundant, one can be computed from the other
        (given the all_runs dataset).

            all_runs = fitts + errts

        Using -regress_compute_fitts, -fitts is no longer applied in 3dD
        (though -errts is).  Instead, note that an all_runs dataset is
        created just after 3dDeconvolve.  After that step, the script will
        create fitts as (all_runs-errts) using 3dcalc.

        Note that computation of both errts and fitts datasets is required
        for this option to be applied.

        See also -regress_est_blur_errts, -regress_errts_prefix,
        -regress_fitts_prefix and -regress_no_fitts.

    -regress_cormat_warnings Y/N : specify whether to get cormat warnings

            e.g. -mask_cormat_warnings no
            default: yes

        By default, '1d_tool.py -show_cormat_warnings' is run on the
        regression matrix.  Any large, pairwise correlations are shown
        in text output (which is also saved to a text file).

        This option allows one to disable such functionality.

        Please see '1d_tool.py -help' for more details.

    -regress_est_blur_detrend yes/no : use -detrend in blur estimation

            e.g. -regress_est_blur_detrend no
            default: yes

        This option specifies whether to apply the -detrend option when
        running 3dFWHMx to estimate the blur (auto correlation function)
        size/parameters.  It will apply to both epits and errts estimation.

        See also -regress_est_blur_epits, -regress_est_blur_errts.
        Please see '3dFWHMx -help' for more details.

    -regress_est_blur_epits      : estimate the smoothness of the EPI data

        This option specifies to run 3dFWHMx on each of the EPI datasets
        used for regression, the results of which are averaged.  These blur
        values are saved to the file blur_est.$subj.1D, along with any
        similar output from errts.

        These blur estimates may be input to 3dClustSim, for any multiple
        testing correction done for this subject.  If 3dClustSim is run at
        the group level, it is reasonable to average these estimates
        across all subjects (assuming they were scanned with the same
        protocol and at the same scanner).

        The mask block is required for this operation (without which the
        estimates are not reliable).

        Please see '3dFWHMx -help' for more information.
        See also -regress_est_blur_errts.

    -regress_est_blur_errts      : estimate the smoothness of the errts

        This option specifies to run 3dFWHMx on the errts dataset, output
        from the regression (by 3dDeconvolve).

        These blur estimates may be input to 3dClustSim, for any multiple
        testing correction done for this subject.  If 3dClustSim is run at
        the group level, it is reasonable to average these estimates
        across all subjects (assuming they were scanned with the same
        protocol and at the same scanner).

        Note that the errts blur estimates should be not only slightly
        more accurate than the epits blur estimates, but they should be
        slightly smaller, too (which is beneficial).

        The mask block is required for this operation (without which the
        estimates are not reliable).

        Please see '3dFWHMx -help' for more information.
        See also -regress_est_blur_epits.

    -regress_errts_prefix PREFIX : specify a prefix for the -errts option

            e.g. -regress_fitts_prefix errts

        This option is used to add a -errts option to 3dDeconvolve.  As
        with -regress_fitts_prefix, only the PREFIX is specified, to which
        the subject ID will be added.

        Please see '3dDeconvolve -help' for more information.
        See also -regress_fitts_prefix.

    -regress_fitts_prefix PREFIX : specify a prefix for the -fitts option

            e.g. -regress_fitts_prefix model_fit
            default: fitts

        By default, the 3dDeconvolve command in the script will be given
        a '-fitts fitts' option.  This option allows the user to change
        the prefix applied in the output script.

        The -regress_no_fitts option can be used to eliminate use of -fitts.

        Please see '3dDeconvolve -help' for more information.
        See also -regress_no_fitts.

    -regress_global_times        : specify -stim_times as global times

            default: 3dDeconvolve figures it out, if it can

        By default, the 3dDeconvolve determines whether -stim_times files
        are local or global times by the first line of the file.  If it
        contains at least 2 times (which include '*' characters), it is
        considered as local_times, otherwise as global_times.

        The -regress_global_times option is mostly added to be symmetric
        with -regress_local_times, as the only case where it would be
        needed is when there are other times in the first row, but the
        should still be viewed as global.

        See also -regress_local_times.

    -regress_local_times         : specify -stim_times as local times

            default: 3dDeconvolve figures it out, if it can

        By default, the 3dDeconvolve determines whether -stim_times files
        are local or global times by the first line of the file.  If it
        contains at least 2 times (which include '*' characters), it is
        considered as local_times, otherwise as global_times.

        In the case where the first run has only 1 stimulus (maybe even
        every run), the user would need to put an extra '*' after the
        first stimulus time.  If the first run has no stimuli, then two
        would be needed ('* *'), but only for the first run.

        Since this may get confusing, being explicit by adding this option
        is a reasonable thing to do.

        See also -regress_global_times.

    -regress_iresp_prefix PREFIX : specify a prefix for the -iresp option

            e.g. -regress_iresp_prefix model_fit
            default: iresp

        This option allows the user to change the -iresp prefix applied in
        the 3dDeconvolve command of the output script.

        By default, the 3dDeconvolve command in the script will be given a
        set of '-iresp iresp' options, one per stimulus type, unless the
        regression basis function is GAM.  In the case of GAM, the response
        form is assumed to be known, so there is no need for -iresp.

        The stimulus label will be appended to this prefix so that a sample
        3dDeconvolve option might look one of these 2 examples:

            -iresp 7 iresp_stim07
            -iresp 7 model_fit_donuts

        The -regress_no_iresp option can be used to eliminate use of -iresp.

        Please see '3dDeconvolve -help' for more information.
        See also -regress_no_iresp, -regress_basis.

    -regress_make_ideal_sum IDEAL.1D : create IDEAL.1D file from regressors

            e.g. -regress_make_ideal_sum ideal_all.1D

        By default, afni_proc.py will compute a 'sum_ideal.1D' file that
        is the sum of non-polort and non-motion regressors from the
        X-matrix.  This -regress_make_ideal_sum option is used to specify
        the output file for that sum (if sum_idea.1D is not desired).

        Note that if there is nothing in the X-matrix except for polort and
        motion regressors, or if 1d_tool.py cannot tell what is in there
        (if there is no header information), then all columns will be used.

        Computing the sum means adding a 1d_tool.py command to figure out
        which columns should be used in the sum (since mixing GAM, TENT,
        etc., makes it harder to tell up front), and a 3dTstat command to
        actually sum those columns of the 1D X-matrix (the X-matrix is
        output by 3dDeconvolve).

        Please see '3dDeconvolve -help', '1d_tool.py -help' and
        '3dTstat -help'.
        See also -regress_basis, -regress_no_ideal_sum.

    -regress_motion_file FILE.1D  : use FILE.1D for motion parameters

            e.g. -regress_motion_file motion.1D

        Particularly if the user performs motion correction outside of
        afni_proc.py, they may wish to specify a motion parameter file
        other than dfile_rall.1D (the default generated in the volreg
        block).

        Note: such files no longer need to be copied via -copy_files.

        If the motion file is in a remote directory, include the path,
        e.g. -regress_motion_file ../subject17/data/motion.1D .

    -regress_no_fitts       : do not supply -fitts to 3dDeconvolve

            e.g. -regress_no_fitts

        This option prevents the program from adding a -fitts option to
        the 3dDeconvolve command in the output script.

        See also -regress_fitts_prefix.

    -regress_no_ideal_sum      : do not create sum_ideal.1D from regressors

        By default, afni_proc.py will compute a 'sum_ideal.1D' file that
        is the sum of non-polort and non-motion regressors from the
        X-matrix.  This option prevents that step.

        See also -regress_make_ideal_sum.

    -regress_no_ideals      : do not generate ideal response curves

            e.g. -regress_no_ideals

        By default, if the GAM or BLOCK basis function is used, ideal
        response curve files are generated for each stimulus type (from
        the output X matrix using '3dDeconvolve -x1D').  The names of the
        ideal response function files look like 'ideal_LABEL.1D', for each
        stimulus label, LABEL.

        This option is used to suppress generation of those files.

        See also -regress_basis, -regress_stim_labels.

    -regress_no_iresp       : do not supply -iresp to 3dDeconvolve

            e.g. -regress_no_iresp

        This option prevents the program from adding a set of -iresp
        options to the 3dDeconvolve command in the output script.

        By default -iresp will be used unless the basis function is GAM.

        See also -regress_iresp_prefix, -regress_basis.

    -regress_no_mask        : do not apply the mask in regression

        ** This is now the default, making the option unnecessary.

        This option prevents the program from applying the mask dataset
        in the scaling or regression steps.

        If the user does not want to apply a mask in the regression
        analysis, but wants the full_mask dataset for other reasons
        (such as computing blur estimates), this option can be used.

        See also -regress_est_blur_epits, -regress_est_blur_errts.

    -regress_no_motion      : do not apply motion params in 3dDeconvolve

            e.g. -regress_no_motion

        This option prevents the program from adding the registration
        parameters (from volreg) to the 3dDeconvolve command, computing
        the enorm or censoring.

        ** To omit motion regression but to still compute the enorm and
            possibly censor, use:

                -regress_apply_mot_types none

    -regress_no_motion_demean : do not compute de-meaned motion parameters

            default: do compute them

        Even if they are not applied in the regression, the default is to
        compute de-meaned motion parameters.  These may give the user a
        better idea of motion regressors, since their scale will not be
        affected by jumps across run breaks or multi-run drift.

        This option prevents the program from even computing such motion
        parameters.  The only real reason to not do it is if there is some
        problem with the command.

    -regress_no_motion_deriv  : do not compute motion parameter derivatives

            default: do compute them

        Even if they are not applied in the regression, the default is to
        compute motion parameter derivatives (and de-mean them).  These can
        give the user a different idea about motion regressors, since the
        derivatives are a better indication of per-TR motion.  Note that
        the 'enorm' file that is created (and optionally used for motion
        censoring) is basically made by collapsing (via the Euclidean Norm
        - the square root of the sum of the squares) these 6 derivative
        columns into one.

        This option prevents the program from even computing such motion
        parameters.  The only real reason to not do it is if there is some
        problem with the command.

            See also -regress_censor_motion.

    -regress_no_stim_times  : do not use

        OBSOLETE: please see -regress_use_stim_files (which is also OBSOLETE)
        OBSOLETE: please see -regress_stim_times and -regress_stim_types

    -regress_opts_fwhmx OPTS ... : specify extra options for 3dFWHMx

            e.g. -regress_opts_fwhmx -ShowMeClassicFWHM

        This option allows the user to add extra options to the 3dFWHMx
        commands used to get blur estimates.  Note that only one
        such option should be applied, though multiple parameters
        (3dFWHMx options) can be passed.

        Please see '3dFWHMx -help' for more information.

    -regress_opts_3dD OPTS ...   : specify extra options for 3dDeconvolve

            e.g. -regress_opts_3dD -gltsym ../contr/contrast1.txt  \\
                                   -glt_label 1 FACEvsDONUT        \\
                                   -jobs 6                         \\
                                   -GOFORIT 8

        This option allows the user to add extra options to the 3dDeconvolve
        command.  Note that only one -regress_opts_3dD should be applied,
        which may be used for multiple 3dDeconvolve options.

        Please see '3dDeconvolve -help' for more information, or the link:
            https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004

    -regress_opts_reml OPTS ...  : specify extra options for 3dREMLfit

            e.g. -regress_opts_reml                                 \\
                    -gltsym ../contr/contrast1.txt FACEvsDONUT      \\
                    -MAXa 0.92

        This option allows the user to add extra options to the 3dREMLfit
        command.  Note that only one -regress_opts_reml should be applied,
        which may be used for multiple 3dREMLfit options.

        Please see '3dREMLfit -help' for more information.

    -regress_ppi_stim_files FILE FILE ... : specify PPI (and seed) files

            e.g. -regress_ppi_stim_files PPI.1.A.1D PPI.2.B.1D PPI.3.seed.1D

        Use this option to pass PPI stimulus files for inclusion in
        3dDeconvolve command.  This list is essentially appended to
        (and could be replaced by) -regress_extra_stim_files.

      * These are not timing files, but direct regressors.

        Use -regress_ppi_stim_labels to specify the corresponding labels.

        See also -write_ppi_3dD_scripts, -regress_ppi_stim_labels.

    -regress_ppi_stim_labels LAB1 LAB2 ... : specify PPI (and seed) labels

            e.g. -regress_ppi_stim_files PPI.taskA PPI.taskB PPI.seed

        Use this option to specify labels for the PPI stimulus files
        specified via -regress_ppi_stim_files.  This list is essentially
        appended to (and could be replaced by) -regress_extra_stim_labels.

        Use -regress_ppi_stim_labels to specify the corresponding labels.

        See also -write_ppi_3dD_scripts, -regress_ppi_stim_labels.

    -regress_polort DEGREE  : specify the polynomial degree of baseline

            e.g. -regress_polort 2
            default: 1 + floor(run_length / 150.0)

        3dDeconvolve models the baseline for each run separately, using
        Legendre polynomials (by default).  This option specifies the
        degree of polynomial.  Note that this will create DEGREE * NRUNS
        regressors.

        The default is computed from the length of a run, in seconds, as
        shown above.  For example, if each run were 320 seconds, then the
        default polort would be 3 (cubic).

      * It is also possible to use a high-pass filter to model baseline
        drift (using sinusoids).  Since sinusoids do not model quadratic
        drift well, one could consider using both, as in:

            -regress_polort 2         \\
            -regress_bandpass 0.01 1

        Here, the sinusoids allow every frequency from 0.01 on up to pass
        (assuming the Nyquist frequency is <= 1), modeling the lower
        frequencies as regressors of no interest, along with 3 terms for
        polort 2.

        Please see '3dDeconvolve -help' for more information.

    -regress_reml_exec      : execute 3dREMLfit, matching 3dDeconvolve cmd

        3dDeconvolve automatically creates a 3dREMLfit command script to
        match the regression model of 3dDeconvolve.  Via this option, the
        user can have that command executed.

        Note that the X-matrix used in 3dREMLfit is actually generated by
        3dDeconvolve.  The 3dDeconvolve command generates both the X-matrix
        and the 3dREMLfit command script, and so it must be run regardless
        of whether it actually performs the regression.

        To terminate 3dDeconvolve after creation of the X-matrix and
        3dREMLfit command script, apply -regress_3dD_stop.

        See also -regress_3dD_stop.

    -regress_ROI R1 R2 ... : specify a list of mask averages to regress out

            e.g. -regress_ROI WMe
            e.g. -regress_ROI brain WMe CSF
            e.g. -regress_ROI FSvent FSwhite

        Use this option to regress out one more more known ROI averages.
        In this case, each ROI (dataset) will be used for a single regressor
        (one volume cannot be used for multiple ROIs).

        ROIs that can be generated from -mask_segment_anat/_erode include:

            name    description     source dataset    creation program
            -----   --------------  --------------    ----------------
            brain   EPI brain mask  full_mask         3dAutomask
                       or, if made: mask_epi_anat     3dAutomask/3dSkullStrip
            CSF     CSF             mask_CSF_resam    3dSeg -> Classes
            CSFe    CSF (eroded)    mask_CSFe_resam   3dSeg -> Classes
            GM      gray matter     mask_GM_resam     3dSeg -> Classes
            GMe     gray (eroded)   mask_GMe_resam    3dSeg -> Classes
            WM      white matter    mask_WM_resam     3dSeg -> Classes
            WMe     white (eroded)  mask_WMe_resam    3dSeg -> Classes

        Other ROI labels can come from -anat_follower_ROI or -ROI_import
        options, i.e. imported masks.

      * Use of this option requires either -mask_segment_anat or labels
        defined via -anat_follower_ROI or -ROI_import options.

        See also -mask_segment_anat/_erode, -anat_follower_ROI, -ROI_import.
        Please see '3dSeg -help' for more information on the masks.

    -regress_ROI_PC LABEL NUM_PC    : regress out PCs within mask

            e.g. -regress_ROI_PC vent 3
                 -regress_ROI_PC WMe 3

        Add the top principal components (PCs) over an anatomical mask as
        regressors of no interest.

        As with -regress_ROI, each ROI volume is considered a single mask to
        compute PCs over (for example, here the ventricle and white matter
        masks are passed individually).

          - LABEL   : the class label given to this set of regressors
          - NUM_PC  : the number of principal components to include

        The LABEL can apply to something defined via -mask_segment_anat or
        -anat_follower_ROI (assuming 'epi' grid), and possibly eroded via
        -mask_segment_erode.  LABELs can also come from -ROI_import options,
        or be simply 'brain' (defined as the final EPI mask).
     
        The -mask_segment* options define ROI labels implicitly (see above),
        while the user defines ROI labels in any -anat_follower_ROI or
        -ROI_import options.

        Method (mask alignment, including 'follower' steps):

          The follower steps apply to only -anat_follower* datasets, not to
          -ROI_import, -mask_import or -mask_segment_anat.

          If -ROI_import is used to define the label, then the follower steps
          do not apply, the ROI is merely resampled onto the final EPI grid.

          If ROIs are created 'automatically' via 3dSeg (-mask_segment_anat)
          then the follower steps do not apply.

          If -anat_follower_ROI is used to define the label, then the
          follower ROI steps would first be applied to that dataset:

             F1. if requested (-anat_follower_erode) erode the ROI mask
             F2. apply all anatomical transformations to the ROI mask
                 a. catenate all anatomical transformations
                    i.   anat to EPI?
                    ii.  affine xform of anat to template?
                    iii. subsequent non-linear xform of anat to template?
                 b. sample the transformed mask on the EPI grid
                 c. use nearest neighbor interpolation, NN

        Method (post-mask alignment):

          P1. extract the top NUM_PC principal components from the volume
              registered EPI data, over the mask
              a. detrend the volume registered EPI data at the polort level
                 to be used in the regression, per run
              b. catenate the detrended volreg data across runs
              c. compute the top PCs from the (censored?) time series
              d. if censoring, zero-fill the time series with volumes of
                 zeros at the censored TRs, to maintain TR correspondence
          P2. include those PCs as regressors of no interest
              a. apply with: 3dDeconvolve -ortvec PCs LABEL

        Typical usage might start with the FreeSurfer parcellation of the
        subject's anatomical dataset, followed by ROI extraction using
        3dcalc (to make a new dataset of just the desired regions).  Then
        choose the number of components to extract and a label.

        That ROI dataset, PC count and label are then applied with this
        option.

      * The given MASK must be in register with the anatomical dataset,
        though it does not necessarily need to be on the anatomical grid.

      * Multiple -regress_ROI_PC options can be used.

        See also -anat_follower, -anat_follower_ROI, -regress_ROI_erode,
        and -regress_ROI.

    -regress_ROI_per_run LABEL ... : regress these ROIs per run

            e.g. -regress_ROI_per_run vent
            e.g. -regress_ROI_per_run vent WMe

        Use this option to create the given ROI regressors per run.
        Instead of creating one regressor spanning all runs, this option
        leads to creating one regressor per run, akin to splitting the
        long regressor across runs, and zero-padding to be the same length.

        See also -regress_ROI_PC, -regress_ROI_PC_per_run.

    -regress_ROI_PC_per_run LABEL ... : regress these PCs per run

            e.g. -regress_ROI_PC_per_run vent
            e.g. -regress_ROI_PC_per_run vent WMe

        Use this option to create the given PC regressors per run.  So
        if there are 4 runs and 3 'vent' PCs were requested with the
        option "-regress_ROI_PC vent 3", then applying this option with
        the 'vent' label results in not 3 regressors (one per PC), but
        12 regressors (one per PC per run).

        Note that unlike the -regress_ROI_per_run case, this is not merely
        splitting one signal across runs.  In this case the principle
        components are be computed per run, almost certainly resulting in
        different components than those computed across all runs at once.

        See also -regress_ROI_PC, -regress_ROI_per_run.

    -regress_RSFC           : perform bandpassing via 3dRSFC

        Use this option flag to run 3dRSFC after the linear regression
        step (presumably to clean resting state data).  Along with the
        bandpassed data, 3dRSFC will produce connectivity parameters,
        saved in the RSFC directory by the proc script.

        The -regress_bandpass option is required, and those bands will be
        passed directly to 3dRSFC.  Since bandpassing will be done only
        after the linear regression, censoring is not advisable.

        See also -regress_bandpass, -regress_censor_motion.
        Please see '3dRSFC -help' for more information.

    -regress_RONI IND1 ...  : specify a list of regressors of no interest

            e.g. -regress_RONI 1 17 22

        Use this option flag regressors as ones of no interest, meaning
        they are applied to the baseline (for full-F) and the corresponding
        beta weights are not output (by default at least).

        The indices in the list should match those given to 3dDeconvolve.
        They start at 1 first with the main regressors, and then with any
        extra regressors (given via -regress_extra_stim_files).  Note that
        these do not apply to motion regressors.

        The user is encouraged to check the 3dDeconvolve command in the
        processing script, to be sure they are applied correctly.

    -regress_show_df_info yes/no    : set whether to report DoF information

            e.g.     -regress_show_df_info no
            default: -regress_show_df_info yes

        This option is used to specify whether get QC information about
        degrees of freedom using:

            1d_tool.py -show_df_info

        By default, that will be run, saving output to out.df_info.txt.

        Please see '1d_tool.py -help' for more information.

    -regress_stim_labels LAB1 ...   : specify labels for stimulus classes

            e.g. -regress_stim_labels houses faces donuts
            default: stim01 stim02 stim03 ...

        This option is used to apply a label to each stimulus type.  The
        number of labels should equal the number of files used in the
        -regress_stim_times option, or the total number of columns in the
        files used in the -regress_stim_files option.

        These labels will be applied as '-stim_label' in 3dDeconvolve.

        Please see '3dDeconvolve -help' for more information.
        See also -regress_stim_times, -regress_stim_labels.

    -regress_stim_times FILE1 ... : specify files used for -stim_times

            e.g. -regress_stim_times ED_stim_times*.1D
            e.g. -regress_stim_times times_A.1D times_B.1D times_C.1D

        3dDeconvolve will be run using '-stim_times'.  This option is
        used to specify the stimulus timing files to be applied, one
        file per stimulus type.  The order of the files given on the
        command line will be the order given to 3dDeconvolve.  Each of
        these timing files will be given along with the basis function
        specified by '-regress_basis'.

        The user must specify either -regress_stim_times or
        -regress_stim_files if regression is performed, but not both.
        Note the form of the files is one row per run.  If there is at
        most one stimulus per run, please add a trailing '*'.

        Labels may be specified using the -regress_stim_labels option.

        These two examples of such files are for a 3-run experiment.  In
        the second example, there is only 1 stimulus at all, occurring in
        run #2.

            e.g.            0  12.4  27.3  29
                            *
                            30 40 50

            e.g.            *
                            20 *
                            *

        Please see '3dDeconvolve -help' for more information, or the link:
            https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
        See also -regress_stim_files, -regress_stim_labels, -regress_basis,
                 -regress_basis_normall, -regress_polort.

    -regress_stim_files FILE1 ... : specify TR-locked stim files

            e.g. -regress_stim_files ED_stim_file*.1D
            e.g. -regress_stim_files stim_A.1D stim_B.1D stim_C.1D

        ------------------------------------------------------------
      * Most likely this option should not be used.

        Only use this option with the intention of having afni_proc.py
        convert the given stim-locked binary files to stim_times format
        via make_stim_times.py.

        So preferably, do not apply -regress_use_stim_files with this
        option.  Rather use -regress_stim_times and -regress_stim_types
        (with corresponding parameters of 'file').
        ------------------------------------------------------------

        Without the -regress_use_stim_files option, 3dDeconvolve will be
        run using '-stim_times', not '-stim_file'.  The user can still
        specify the 3dDeconvolve -stim_file files here, but they would
        then be converted to -stim_times files using the script,
        make_stim_times.py .

        It might be more educational for the user to run make_stim_times.py
        outside afni_proc.py (such as was done before example 2, above), or
        to create the timing files directly.

        Each given file can be for multiple stimulus classes, where one
        column is for one stim class, and each row represents a TR.  So
        each file should have NUM_RUNS * NUM_TRS rows.

        The stim_times files will be labeled stim_times.NN.1D, where NN
        is the stimulus index.

        Note that if the stimuli were presented at a fixed time after
        the beginning of a TR, the user should consider the option,
        -regress_stim_times_offset, to apply that offset.

        ---

        If the -regress_use_stim_files option is provided, 3dDeconvolve
        will be run using each stim_file as a regressor.  The order of the
        regressors should match the order of any labels, provided via the
        -regress_stim_labels option.

        Alternately, and preferably, this can be done via -regress_stim_times,
        along with -regress_stim_types 'file'.

        Please see '3dDeconvolve -help' for more information, or the link:
            https://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
        See also -regress_stim_times, -regress_stim_labels, -regress_basis,
                 -regress_basis_normall, -regress_polort,
                 -regress_stim_times_offset, -regress_use_stim_files.

    -regress_extra_stim_files FILE1 ... : specify extra stim files

            e.g. -regress_extra_stim_files resp.1D cardiac.1D
            e.g. -regress_extra_stim_files regs_of_no_int_*.1D

        Use this option to specify extra files to be applied with the
        -stim_file option in 3dDeconvolve (as opposed to the more usual
        option, -stim_times).

        These files will not be converted to stim_times format.

        Corresponding labels can be given with -regress_extra_stim_labels.

        See also -regress_extra_stim_labels, -regress_ROI, -regress_RONI.

    -regress_extra_stim_labels LAB1 ... : specify extra stim file labels

            e.g. -regress_extra_stim_labels resp cardiac

        If -regress_extra_stim_files is given, the user may want to specify
        labels for those extra stimulus files.  This option provides that
        mechanism.  If this option is not given, default labels will be
        assigned (like stim17, for example).

        Note that the number of entries in this list should match the
        number of extra stim files.

        See also -regress_extra_stim_files.

    -regress_stim_times_offset OFFSET : add OFFSET to -stim_times files

            e.g. -regress_stim_times_offset 1.25
            e.g. -regress_stim_times_offset -9.2
            default: 0

        With -regress_stim_times:

           If the -regress_stim_times option is uses, and if ALL stim files
           are timing files, then timing_tool.py will be used to add the
           time offset to each -regress_stim_times file as it is copied into
           the stimuli directory (near the beginning of the script).

        With -regress_stim_files:

           If the -regress_stim_files option is used (so the script would
           convert -stim_files to -stim_times before 3dDeconvolve), the
           user may want to add an offset to the times in the resulting
           timing files.

           For example, if -tshift_align_to is applied and the user chooses
           to align volumes to the middle of the TR, it might be appropriate
           to add TR/2 to the times of the stim_times files.

           This OFFSET will be applied to the make_stim_times.py command in
           the output script.

        Please see 'make_stim_times.py -help' for more information.
        See also -regress_stim_files, -regress_use_stim_files,
                 -regress_stim_times and -tshift_align_to.

    -regress_stim_types TYPE1 TYPE2 ... : specify list of stim types

            e.g. -regress_stim_types times times AM2 AM2 times AM1 file
            e.g. -regress_stim_types AM2
            default: times

        If amplitude, duration or individual modulation is desired with
        any of the stimulus timing files provided via -regress_stim_files,
        then this option should be used to specify one (if all of the types
        are the same) or a list of stimulus timing types.  One can also use
        the type 'file' for the case of -stim_file, where the input is a 1D
        regressor instead of stimulus times.

        The types should be (possibly repeated) elements of the set:
        {times, AM1, AM2, IM}, where they indicate:

            times:  a standard stimulus timing file (not married)
                    ==> use -stim_times in 3dDeconvolve command

            AM1:    have one or more married parameters
                    ==> use -stim_times_AM1 in 3dDeconvolve command

            AM2:    have one or more married parameters
                    ==> use -stim_times_AM2 in 3dDeconvolve command

            IM:     NO married parameters, but get beta for each stim
                    ==> use -stim_times_IM in 3dDeconvolve command

            file:   a 1D regressor, not a stimulus timing file
                    ==> use -stim_file in 3dDeconvolve command

        Please see '3dDeconvolve -help' for more information.
        See also -regress_stim_times.
        See also example 7 (esoteric options).

    -regress_use_stim_files : use -stim_file in regression, not -stim_times

        The default operation of afni_proc.py is to convert TR-locked files
        for the 3dDeconvolve -stim_file option to timing files for the
        3dDeconvolve -stim_times option.

        If the -regress_use_stim_times option is provided, then no such
        conversion will take place.  This assumes the -regress_stim_files
        option is applied to provide such -stim_file files.

        This option has been renamed from '-regress_no_stim_times'.

        Please see '3dDeconvolve -help' for more information.
        See also -regress_stim_files, -regress_stim_times,
                 -regress_stim_labels.

    -regress_extra_ortvec FILE1 ... : specify extra -ortvec files

            e.g. -regress_extra_ortvec ort_resp.1D ort_cardio.1D
            e.g. -regress_extra_ortvec lots_of_orts.1D

        Use this option to specify extra files to be applied with the
        -ortvec option in 3dDeconvolve.  These are applied as regressors
        of no interest, going into the baseline model.

        These files should be in 1D format, columns of regressors in text
        files.  They are not modified by the program, and should match the
        length of the final regression.

        Corresponding labels can be set with -regress_extra_ortvec_labels.

        See also -regress_extra_ortvec_labels.

    -regress_extra_ortvec_labels LAB1 ... : specify label for extra ortvecs

            e.g. -regress_extra_ortvec_labels resp cardio
            e.g. -regress_extra_ortvec_labels EXTERNAL_ORTs

        Use this option to specify labels to correspond with files given
        by -regress_extra_ortvec.  There should be one label per file.

        See also -regress_extra_ortvec.

    -----------------------------------------------------------------
    3dClustSim options ~3~

    -regress_run_clustsim yes/no : add 3dClustSim attrs to stats dset

            e.g. -regress_run_clustsim no
            default: yes

        This option controls whether 3dClustSim will be executed after the
        regression analysis.  Since the default is 'yes', the effective use
        of this option would be to turn off the operation.

        3dClustSim generates a table of cluster sizes/alpha values that can
        then be stored in the stats dataset for a simple multiple
        comparison correction in the cluster interface of the afni GUI, or
        which can be applied via a program like 3dClusterize.

        The blur estimates and mask dataset are required, and so the
        option is only relevant in the context of blur estimation.

        Please see '3dClustSim -help' for more information.
        See also -regress_est_blur_epits, -regress_est_blur_epits and
                 -regress_opts_CS.

    -regress_CS_NN LEVELS   : specify NN levels for 3dClustSim command

            e.g.     -regress_CS_NN 1
            default: -regress_CS_NN 123

        This option allows the user to specify which nearest neighbors to
        consider when clustering.  Cluster results will be generated for
        each included NN level.  Using multiple levels means being able to
        choose between those same levels when looking at the statistical
        results using the afni GUI.

        The LEVELS should be chosen from the set {1,2,3}, where the
        respective levels mean "shares a face", "shares an edge" and
        "shares a corner", respectively.  Any non-empty subset can be used.
        They should be specified as is with 3dClustSim.

        So there are 7 valid subsets: 1, 2, 3, 12, 13, 23, and 123.

        Please see '3dClustSim -help' for details on its '-NN' option.

    -regress_opts_CS OPTS ...    : specify extra options for 3dClustSim

            e.g. -regress_opts_CS -athr 0.05 0.01 0.005 0.001

        This option allows the user to add extra options to the 3dClustSim
        command.  Only 1 such option should be applied, though multiple
        options to 3dClustSim can be included.

        Please see '3dClustSim -help' for more information.
        See also -regress_run_clustsim.

    -ROI_import LABEL RSET       : import a final grid ROI with the given label

            e.g. -ROI_import Glasser MNI_Glasser_HCP_v1.0.nii.gz
            e.g. -ROI_import Benny my_habenula_rois.nii.gz
            e.g. -ROI_import Benny path/to/ROIs/my_habenula_rois.nii.gz

        Use this option to import an ROI dataset that is in the final space of
        the EPI data.  It will merely be resampled onto the final EPI grid
        (not transformed).

            o  this might be based on the group template
            o  no warping will be done to this dataset
            o  this dataset WILL be resampled to match the final EPI

        This option was added to be applied with -regress_compute_tsnr_stats,
        for example:

            -ROI_import Glasser MNI_Glasser_HCP_v1.0.nii.gz  \\
            -regress_compute_tsnr_stats Glasser 4 41 99 999  \\

        This mask can be applied via LABEL as other masks, using options
        like: -regress_ROI, -regress_ROI_PC, -regress_make_corr_vols,
              -regress_anaticor_label, -mask_intersect, -mask_union,
              (and for the current purpose) -regress_compute_tsnr_stats.
"""
g_help_trailer = """
- R Reynolds  Dec, 2006                             thanks to Z Saad
===========================================================================
"""
# end global help string
# ----------------------------------------------------------------------

