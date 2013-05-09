#!/usr/bin/env python
import sys

# whine about execution as a main program
if __name__ == '__main__':
   print '** %s: not a main program' % sys.argv[0].split('/')[-1]
   sys.exit(1)

import math, os
import afni_base as BASE, afni_util as UTIL
import option_list as OL
import lib_afni1D as LD


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

def apply_uopt_to_block(opt_name, user_opts, block):
    """just pass any parameters for opt_name along to the block
       return 0/1, based on whether the option was found
    """
    uopt = user_opts.find_opt(opt_name)
    if uopt:
        bopt = block.opts.find_opt(opt_name)
        if bopt: bopt.parlist = uopt.parlist
        else:    block.opts.add_opt(opt_name, 1, uopt.parlist, setpar=1)

        return 1

    return 0

# --------------- tcat ---------------

# modify the tcat block options according to the user options
def db_mod_tcat(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-tcat_remove_first_trs', 1, [0], setpar=1)

    errs = 0

    uopt = user_opts.find_opt('-tcat_remove_first_trs')
    bopt = block.opts.find_opt('-tcat_remove_first_trs')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print "** ERROR: %s: invalid integer: %s"   \
                  % (uopt.label, uopt.parlist[0])
            errs += 1
        if errs == 0 and bopt.parlist[0] > 0:
          print                                                              \
            '** warning: removing first %d TRs from beginning of each run\n' \
            '   --> the stimulus timing files must reflect the '             \
                    'removal of these TRs' % bopt.parlist[0]

    apply_uopt_to_block('-tcat_remove_last_trs', user_opts, block)
    apply_uopt_to_block('-tcat_preSS_warn_limit', user_opts, block)

    if errs == 0: block.valid = 1
    else        : block.valid = 0

# do not rely on the form of input filenames
# use 3dtcat to copy each file to od_var, then 'cd' into it
def db_cmd_tcat(proc, block):
    block.index = proc.bindex   # save

    cmd = ''
    opt = block.opts.find_opt('-tcat_remove_first_trs')
    first = opt.parlist[0]

    # maybe the user updated our warning limit
    val, err = block.opts.get_type_opt(float, '-tcat_preSS_warn_limit')
    if err: return 1, ''
    if val != None:
       if val < 0.0 or val > 1.0:
          print '** -tcat_preSS_warn_limit: limit %s outside [0,1.0]' % val
          return 1, ''
       proc.out_ss_lim = val

    # remove the last TRs?  set rmlast
    val, err = proc.user_opts.get_type_opt(int, '-tcat_remove_last_trs')
    if err: return 1, ''
    if val == None: rmlast = 0
    else: rmlast = val

    cmd = cmd + "# %s\n"                                                      \
                "# apply 3dTcat to copy input dsets to results dir, while\n"  \
                "# removing the first %d TRs\n"                               \
                % (block_header('auto block: tcat'), first)
    for run in range(0, proc.runs):
        if rmlast == 0: final = '$'
        else:
            reps = proc.reps_all[run]
            final = '%d' % (reps-rmlast-1)
            if reps-rmlast-1 < 0:
                print '** run %d: have %d reps, cannot remove %d!' \
                      % (run+1, reps, rmlast)
        cmd = cmd + "3dTcat -prefix %s/%s %s'[%d..%s]'\n" %              \
                    (proc.od_var, proc.prefix_form(block,run+1),
                     proc.dsets[run].rel_input(), first, final)

    proc.reps   -= first+rmlast # update reps to account for removed TRs
    proc.reps_all = [reps-first-rmlast for reps in proc.reps_all]

    cmd = cmd + '\n'                                                    \
                '# and make note of repetitions (TRs) per run\n'        \
                'set tr_counts = ( %s )\n'%UTIL.int_list_string(proc.reps_all)

    cmd = cmd + '\n'                                                          \
                '# -------------------------------------------------------\n' \
                '# enter the results directory (can begin processing data)\n' \
                'cd %s\n\n\n' % proc.od_var

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    # now that the 'output' index and label are set, maybe get outlier counts
    opt = proc.user_opts.find_opt('-outlier_count')
    if not opt or OL.opt_is_yes(opt):
        rv, oc = make_outlier_commands(proc)
        if rv: return   # failure (error has been printed)
        cmd = cmd + oc

    if proc.verb > 0: print "-- %s: reps is now %d" % (block.label, proc.reps)

    return cmd

# could do this from any block, but expect to do at end of tcat
# return error code (0 = success) and string
def make_outlier_commands(proc):
    # ----------------------------------------
    # check for any censoring
    val, err = proc.user_opts.get_type_opt(float, '-regress_censor_outliers')
    if err: return 1, ''
    elif val == None: censor = 0.0
    elif val < 0.0 or val > 1.0:
        print '** -regress_censor_outliers value %f is not in [0,1.0]' % val
        return 1, ''
    else: censor = val

    # check for ignoring first TRs of each run
    val, err = proc.user_opts.get_type_opt(int, '-regress_skip_first_outliers')
    if err: return 1, ''
    elif val == None: nskip = 0
    elif val < 0:
        print '** -regress_skip_first_outliers: bad value %d' % val
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
    else: polort = UTIL.get_default_polort(proc.tr, proc.reps)

    # use Legendre polynomials?
    opt = proc.user_opts.find_opt('-outlier_legendre')
    if not opt or OL.opt_is_yes(opt): lstr = ' -legendre'
    else:                             lstr = ''

    prev_prefix = proc.prev_prefix_form_run(view=1)
    ofile = 'outcount.r$run.1D'
    warn  = '** TR #0 outliers: possible pre-steady state TRs in run $run'
    proc.out_wfile = 'out.pre_ss_warn.txt'

    cmd  = '# %s\n'                                                       \
           '# data check: compute outlier fraction for each volume\n'     \
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
 
    return 0, cmd

def combine_censor_files(proc, cfile, newfile=''):
    """create a 1deval command to multiply the 2 current censor file
       with the existing one, writing to newfile

       store newfile as censor_file

       return err, cmd_str   (where err=0 implies success)"""

    if not newfile:
        newfile = 'censor_${subj}_combined_%d.1D' % (proc.censor_count+1)
    if not proc.censor_file or not cfile:
        print '** combine_censor_files: missing input'
        return 1, ''
    cstr = '# combine multiple censor files\n'          \
           '1deval -a %s -b %s \\\n'                    \
           '       -expr "a*b" > %s\n'                  \
           % (cfile, proc.censor_file, newfile)
    proc.censor_file = newfile
    proc.censor_count += 1

    return 0, cstr

# --------------- align (anat2epi) ---------------

def db_mod_align(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-align_opts_aea', -1, [])

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
    uopt = user_opts.find_opt('-align_epi_strip_method')
    bopt = block.opts.find_opt('-align_epi_strip_method')
    if uopt and not bopt: 
       block.opts.add_opt('-align_epi_strip_method', 1, uopt.parlist, setpar=1)
    elif uopt and bopt: bopt.parlist = uopt.parlist

    block.valid = 1

# align anat to epi -> anat gets _al suffix to its prefix
#                   -> matrix is ${anat_prefix}_al.mat.aff12.1D
# (adjust prefix of proc.anat and proc.tlrcanat)
# set a2e xform matrix
def db_cmd_align(proc, block):
    block.index = proc.bindex   # save

    if not proc.anat:
        print '** ERROR: missing anat for align block (consider -copy_anat)\n'
        return

    # first note EPI alignment base and sub-brick, as done in volreg block
    # (alignEA EPI and base might be given externally via -align_epi_base_dset)
    if proc.align_ebase != None:
        basevol = "%s%s" % (proc.align_epre,proc.view)
        bind = 0
    elif proc.vr_ext_base != None:
        basevol = "%s%s" % (proc.vr_ext_pre,proc.view)
        bind = 0
    else:
        rind, bind = proc.get_vr_base_indices()
        if rind < 0:
            print '** align base index failure: %d, %d' % (rind, bind)
            return     # error message is printed
        basevol = proc.prev_prefix_form(rind+1, view=1)

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

    # note whether this the aea output is expected to be used
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

    # store alignment matrix file for possible later use
    proc.a2e_mat = "%s%s_mat.aff12.1D" % (proc.anat.prefix, suffix)

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
                   proc.tlrcanat.prefix = "%s%s" % (proc.tlrcanat.prefix, suffix)
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

    # note the alignment in EPIs warp bitmap (2=a2e)
    proc.warp_epi |= WARP_EPI_ALIGN_A2E

    return hdr + cmd

# --------------- despike ---------------

def db_mod_despike(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-despike_opts_3dDes', -1, [])

    uopt = user_opts.find_opt('-despike_opts_3dDes')
    bopt = block.opts.find_opt('-despike_opts_3dDes')
    if uopt and bopt: bopt.parlist = uopt.parlist

    uopt = user_opts.find_opt('-despike_mask')
    bopt = block.opts.find_opt('-despike_mask')
    if uopt and not bopt: block.opts.add_opt('-despike_mask', 0, [])

    block.valid = 1

# apply 3dDespike to each run
def db_cmd_despike(proc, block):
    block.index = proc.bindex   # save

    cmd = ''

    # see if the user has provided other options
    opt = block.opts.find_opt('-despike_opts_3dDes')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = ' %s' %      \
               ' '.join(UTIL.quotize_list(opt.parlist, '', 1))

    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(view=1)

    # maybe the user wants to mask here (to speed this step up)
    if block.opts.find_opt('-despike_opts_mask'): mstr = ''
    else:                                         mstr = ' -nomask'

    # write commands
    cmd = cmd + '# %s\n'                                \
                '# apply 3dDespike to each run\n' % block_header('despike')
    cmd = cmd + 'foreach run ( $runs )\n'               \
                '    3dDespike%s%s -prefix %s %s\n'     \
                'end\n\n' %                             \
                (other_opts, mstr, prefix, prev)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

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
            import lib_afni1D as LAD
            for reg in proc.ricor_regs:
                adata = LAD.Afni1D(reg)
                trs.append(adata.nt)
        except:
            print '** failing to remove last %d TRs' % proc.ricor_nlast
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
        print "** missing ricor option: '-ricor_regs'"
        return
    if len(uopt.parlist) < 1:
        print "** missing '-ricor_regs' regressor list"
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
    block.index = proc.bindex   # save

    #----- check for problems -----
    # check regressors against num runs
    if len(proc.ricor_regs) != proc.runs:
        print '** ERROR: have %d runs but %d slice-base ricor regressors' % \
              (proc.runs, len(proc.ricor_regs))
        return

    # get datum, if set
    rdatum, err = block.opts.get_string_opt('-ricor_datum')
    if err: return
    # if no option and input was unscaled shorts, convert output back to it
    if rdatum == None:
        if proc.datatype == 1 and proc.scaled == 0:
          if proc.verb > 0:
            print '-- ricor: have unscaled short input, will revert back to it'
          rdatum = 'short' # treat as unscaled short
        else: rdatum = 'float'
    # we might want to force the -float option in 3dDeconvolve
    if rdatum == 'float': proc.datatype = 3

    # get regress method (will only currently work as 'per-run')
    rmethod, err = block.opts.get_string_opt('-ricor_regress_method')
    if err or rmethod == None:
        print "** ERROR: option -ricor_regress_method required for ricor block"
        return

    # get nslices
    err, dims = UTIL.get_typed_dset_attr_list(proc.dsets[0].rel_input(),
                                              "DATASET_DIMENSIONS", int)
    if err or len(dims) < 4:
        print '** ERROR: failed to get DIMENSIONS from %s' \
              % proc.dsets[0].rel_input()
        return
    nslices = dims[2]
    if proc.verb > 2: print '-- ricor: found nslices = %d' % nslices

    # check regressors against nslices and nTR (also check reg[0] existence)
    adata = None
    try:
        import lib_afni1D as LAD
        adata = LAD.Afni1D(proc.ricor_regs[0], verb=proc.verb)
    except: pass
    if not adata or not adata.ready:
        print "** ERROR: failed to read '%s' as Afni1D" % proc.ricor_regs[0]
        return
    nsr_labs = adata.labs_matching_str('s0.')
    nsliregs = adata.nvec // nslices
    if nsliregs * nslices != adata.nvec:
        print "** ERROR: ricor nsliregs x nslices != nvec (%d,%d,%d)\n" \
              "   (# slice 0 labels found = %d)"                        \
              % (nsliregs, nslices, adata.nvec, len(nsr_labs))
        return
    if proc.verb > 1: print '-- ricor: nsliregs = %d, # slice 0 labels = %d' \
                            % (nsliregs, len(nsr_labs))
    if proc.verb > 2: print '-- ricor: slice 0 labels: %s' % ' '.join(nsr_labs)

    # check reps against adjusted NT
    nt = adata.nt-proc.ricor_nfirst-proc.ricor_nlast
    if proc.reps != nt:
        print "** ERROR: ricor NT != dset len (%d, %d)"                 \
              "   (check -ricor_regs_nfirst/-tcat_remove_first_trs)"    \
              % (nt, proc.reps)
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
        print "-- ricor: processed '%s' using '%s' on %d sliregs" \
              % (rmethod, sstr, nsliregs)

    #----- everything seems okay, write command string -----

    if rmethod == 'per-run':
        cmd = ricor_process_per_run(proc, block, polort,solver,nsliregs,rdatum)
    else:
        cmd = ricor_process_across_runs(proc, block, polort, solver,
                                        nsliregs, rdatum)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def ricor_process_across_runs(proc, block, polort, solver, nsliregs, rdatum):
    """- for each run: 3dDetrend polort from regressors
       - 1dcat all s0 regressors together for "dummy" in regress process block
       - 3dD -input ALL -polort -x1D -x1D_stop
       - 3dREMLfit -input -matrix -Rerrts -Rbeta? -slibase_sm -verb?
       - 3dSynthesize -matrix -cbucket -select baseline -prefix
       - for each run: 3dcalc -a errts -b baseline -expr a+b -prefix pbXX.ricor
    """

    prev_dsets = proc.prev_dset_form_wild(view=1)
    cur_prefix = proc.prefix_form_run(block)
    prefix     = 'pb%02d.ricor' % proc.bindex
    matrix     = '%s.xmat.1D' % prefix

    # we have a regressor file to pass to the regress processing block
    proc.ricor_reg = 'stimuli/ricor_s0_rall.1D'
    proc.ricor_nreg = nsliregs

    cmd = '# %s\n'                                                      \
          '# RETROICOR - remove cardiac and respiratory signals\n'      \
          '#           - across runs: catenate regressors across runs\n'\
          'foreach run ( $runs )\n' % block_header('ricor')

    cmd = cmd +                                                         \
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
        % (polort, prev_dsets, matrix)

    cmd = cmd +                                                         \
        "# 3dREMLfit does not currently catenate a dataset list\n"      \
        "set dsets = ( %s )\n\n" % prev_dsets

    cmd = cmd +                                                 \
        "# regress out the detrended RETROICOR regressors\n"    \
        "# (matrix from 3dD does not have slibase regressors)\n"\
        '3dREMLfit -input "$dsets" \\\n'                        \
        "    -matrix %s \\\n"                                   \
        "    -%sbeta %s.betas \\\n"                             \
        "    -%serrts %s.errts \\\n"                            \
        "    -slibase_sm stimuli/ricor_det_rall.1D\n\n"         \
        % (matrix, solver, prefix, solver, prefix)

    cmd = cmd +                                                 \
        "# re-create polynomial baseline\n"                     \
        "3dSynthesize -matrix %s \\\n"                          \
        "    -cbucket %s.betas%s'[0..%d]' \\\n"                 \
        "    -select polort -prefix %s.polort\n\n"              \
        % (matrix, prefix, proc.view, (polort+1)*proc.runs-1, prefix)

    # short data is unscaled, float is the default, else convert to short
    if   rdatum == 'short':  dstr = '           -datum short -nscale \\\n'
    elif rdatum == 'float':  dstr = ''
    else:                    dstr = '           -datum %s \\\n' % rdatum

    cmd = cmd +                                                 \
        "# final result: add REML errts to polynomial baseline\n" \
        "# (and separate back into individual runs)\n"          \
        "set startind = 0\n"                                    \
        "foreach rind ( `count -digits 1 1 $#runs` )\n"         \
        "    set run = $runs[$rind]\n"                          \
        "    set runlen = $tr_counts[$rind]\n"                  \
        "    @ endind = $startind + $runlen - 1\n"              \
        "\n"                                                    \
        '    3dcalc -a %s.errts%s"[$startind..$endind]" \\\n'   \
        '           -b %s.polort%s"[$startind..$endind]" \\\n'  \
        '%s'                                                    \
        '           -expr a+b -prefix %s\n'                     \
        "    @ startind = $endind + 1\n"                        \
        'end\n\n'                                               \
        % (prefix, proc.view, prefix, proc.view, dstr, cur_prefix)

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
    

    prev_prefix = proc.prev_prefix_form_run(view=1)
    cur_prefix  = proc.prefix_form_run(block)
    prefix      = 'pb%02d.ricor' % proc.bindex
    matrix      = '%s.r$run.xmat.1D' % prefix

    # we have a regressor file to pass to the regress processing block
    proc.ricor_reg = 'stimuli/ricor_s0_rall.1D'
    proc.ricor_nreg = proc.runs * nsliregs

    cmd = '# %s\n'                                                      \
          '# RETROICOR - remove cardiac and respiratory signals\n'      \
          '#           - per run: each run uses separate regressors\n'  \
          'foreach run ( $runs )\n' % block_header('ricor')

    cmd = cmd +                                                            \
        "    # detrend regressors (make orthogonal to poly baseline)\n"    \
        "    3dDetrend -polort %d -prefix rm.ricor.$run.1D \\\n"           \
        "              stimuli/ricor_orig_r$run.1D\\'\n\n"                 \
        "    1dtranspose rm.ricor.$run.1D stimuli/ricor_det_r$run.1D\n\n"  \
        "    # pad slice0 regressors across all runs (for 'regress' block)\n" \
        "    1d_tool.py -infile stimuli/ricor_det_r$run.1D'[0..%d]' \\\n"  \
        "               -pad_into_many_runs $run $#runs \\\n"              \
        "               -write rm.ricor_s0_r$run.1D\n\n"                 % \
        (polort, nsliregs-1)
    proc.have_rm = 1            # rm.* files exist

    cmd = cmd +                                                 \
        "    # create (polort) X-matrix to apply in 3dREMLfit\n"\
        "    3dDeconvolve -polort %d -input %s \\\n"            \
        "        -x1D_stop -x1D %s\n\n" %                       \
        (polort, prev_prefix, matrix)
    cmd = cmd +                                                 \
        "    # regress out the detrended RETROICOR regressors\n"\
        "    3dREMLfit -input %s \\\n"                          \
        "        -matrix %s \\\n"                               \
        "        -%sbeta %s.betas.r$run \\\n"                   \
        "        -%serrts %s.errts.r$run \\\n"                  \
        "        -slibase_sm stimuli/ricor_det_r$run.1D\n\n"    \
        % (prev_prefix, matrix, solver, prefix, solver, prefix)
    cmd = cmd +                                                 \
        "    # re-create polynomial baseline\n"                 \
        "    3dSynthesize -matrix %s \\\n"                      \
        "        -cbucket %s.betas.r$run%s'[0..%d]' \\\n"       \
        "        -select polort -prefix %s.polort.r$run\n\n"    \
        % (matrix, prefix, proc.view, polort, prefix)

    # short data is unscaled, float is the default, else convert to rdatum
    if   rdatum == 'short':  dstr = '           -datum short -nscale \\\n'
    elif rdatum == 'float':  dstr = ''
    else:                    dstr = '           -datum %s \\\n' % rdatum

    cmd = cmd +                                                 \
        "    # final result: add REML errts to polynomial baseline\n"   \
        "    3dcalc -a %s.errts.r$run%s \\\n"                   \
        "           -b %s.polort.r$run%s \\\n"                  \
        '%s'                                                    \
        "           -expr a+b -prefix %s\n"                     \
        % (prefix, proc.view, prefix, proc.view, dstr, cur_prefix)

    # end of foreach loop encompassing ricor block
    cmd = cmd + "end\n\n"

    cmd = cmd +                                                            \
        "# put ricor regressors into a single file for 'regress' block\n"  \
        "1dcat rm.ricor_s0_r[0-9]*.1D > %s\n\n" % proc.ricor_reg

    return cmd

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
           and not user_opts.find_opt('-regress_no_stim_times'):
          print '-----------------------------------------------------------\n'\
                '** warning: using -tshift_align_to and -regress_stim_files\n' \
                '   --> if temporal alignment is not to the beginning of the\n'\
                '       TR, consider: -regress_stim_times_offset\n'            \
                '-----------------------------------------------------------'
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
    block.index = proc.bindex   # save

    cmd = ''
    # get the base options
    opt = block.opts.find_opt('-tshift_align_to')
    align_to = ' '.join(opt.parlist)  # maybe '-tzero 0'
    opt = block.opts.find_opt('-tshift_interp')
    resam = ' '.join(opt.parlist)     # maybe '-quintic'

    # note cur and prev prefix forms (with $run)
    cur_prefix = proc.prefix_form_run(block)
    prev_prefix = proc.prev_prefix_form_run(view=1)

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-tshift_opts_ts')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '             %s \\\n' % ' '.join(opt.parlist)

    # write commands
    cmd = cmd + '# %s\n'                                                \
                '# time shift data so all slice timing is the same \n'  \
                % block_header('tshift')
    cmd = cmd + 'foreach run ( $runs )\n'                               \
                '    3dTshift %s %s -prefix %s \\\n'                    \
                '%s'                                                    \
                '             %s\n'                                     \
                'end\n\n'                                               \
                % (align_to, resam, cur_prefix, other_opts, prev_prefix)
    
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

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
        print '** option -volreg_regress_per_run is no longer valid\n' \
              '   (please use -regress_motion_per_run, instead)'
        return 1

    # check base_dset (do not allow with selector options)
    if baseopt:
        if uopt or aopt:
            print "** cannot use -volreg_base_ind or _align_to with _base_dset"
            print "   (use sub-brick selection with -volreg_base_dset DSET)"
            return 1
        proc.vr_ext_base = baseopt.parlist[0]

    if uopt and bopt:
        # copy new params as ints
        if aopt:
            print "** cannot use both '-volreg_base_ind' and '-volreg_align_to'"
            return 1
        errs = 0
        try: bopt.parlist[0] = int(uopt.parlist[0]) - 1  # run -> index
        except: errs += 1
        try: bopt.parlist[1] = int(uopt.parlist[1])
        except: errs += 1
        if errs > 0:
            print "** -volreg_base_ind requires integer params (have %s,%s)" % \
                  (uopt.parlist[0], uopt.parlist[1])
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
            # if we don't know runs/reps yet, will have -1, which is okay
            # (if reps_vary is set, we should use reps_all)
            if proc.reps_vary: reps = proc.reps_all[-1]
            else:              reps = proc.reps
            bopt.parlist[0] = proc.runs - 1     # index of last dset
            bopt.parlist[1] = reps - 1          # index of last rep
        else:   
            print "** unknown '%s' param with -volreg_base_ind option" \
                  % aopt.parlist[0]
            return 1

    uopt = user_opts.find_opt('-volreg_interp')
    if uopt:
        bopt = block.opts.find_opt('-volreg_interp')
        bopt.parlist = uopt.parlist

    zopt = user_opts.find_opt('-volreg_zpad')
    if zopt:
        bopt = block.opts.find_opt('-volreg_zpad')
        try: bopt.parlist[0] = int(zopt.parlist[0])
        except:
            print "** -volreg_zpad requires an int (have '%s')"%zopt.parlist[0]
            return 1

    uopt = user_opts.find_opt('-volreg_opts_vr')
    if uopt:
        bopt = block.opts.find_opt('-volreg_opts_vr')
        bopt.parlist = uopt.parlist

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
        print '** cannot use both -volreg_tlrc_adwarp and -volreg_tlrc_warp'
        return 1
    if uopt or u2:
        if proc.origview == '+tlrc':
           print '** already in tlrc space: -volreg_tlrc_* is not allowed'
           return 1
        exists = 0
        # to get from -copy_anat, no view and +tlrc exists
        if proc.anat and proc.tlrcanat and not proc.anat.view:
           if proc.tlrcanat.exist(): exists = 1
        if proc.verb > 1: print '++ -copy_anat +tlrc exists = ', exists
        if not exists and not proc.find_block('tlrc'):
           print "** cannot warp to tlrc space without +tlrc anat via"
           print "   either -copy_anat or the 'tlrc' processing block"
           return 1
        if exists:
           wpieces = UTIL.get_num_warp_pieces(proc.tlrcanat.ppv(),proc.verb)
           if uopt and wpieces == 1:    # warning
              print "** have auto_tlrc anat, consider '-volreg_tlrc_warp'\n" \
                    "   (in place of '-volreg_tlrc_adwarp')"
           elif u2 and wpieces == 12:   # error
              print "** -volreg_tlrc_warp does not work with manual tlrc\n" \
                    "   (consider -volreg_tlrc_adwarp instead)"
              return 1

    uopt = user_opts.find_opt('-volreg_no_extent_mask')
    bopt = block.opts.find_opt('-volreg_no_extent_mask')
    if uopt and not bopt:
        block.opts.add_opt('-volreg_no_extent_mask', 0, [])

    uopt = user_opts.find_opt('-volreg_align_e2a')
    bopt = block.opts.find_opt('-volreg_align_e2a')
    if uopt and not bopt:
        block.opts.add_opt('-volreg_align_e2a', 0, [])

    uopt = user_opts.find_opt('-volreg_warp_dxyz')
    bopt = block.opts.find_opt('-volreg_warp_dxyz')
    if uopt:
        try: dxyz = float(uopt.parlist[0])
        except:
            print "** -volreg_warp_dxyz requires float ('%s')"%uopt.parlist[0]
            return 1
        if bopt: bopt.parlist[0] = dxyz
        else: block.opts.add_opt('-volreg_warp_dxyz', 1, [dxyz], setpar=1)

    # check on tsnr
    uopt = user_opts.find_opt('-volreg_compute_tsnr')
    bopt = block.opts.find_opt('-volreg_compute_tsnr')
    if uopt: bopt.parlist = uopt.parlist

    block.valid = 1

def db_cmd_volreg(proc, block):
    block.index = proc.bindex   # save

    cmd = ''
    # get the base options
    opt = block.opts.find_opt('-volreg_base_ind')
    dset_ind = opt.parlist[0]
    sub      = opt.parlist[1]

    if dset_ind == -1: dset_ind = proc.runs - 1  # may need updates
    if sub      == -1: sub = proc.reps_all[-1] - 1

    # if ext aea base, expect sub to be small
    if proc.align_ebase != None and sub > 3 and sub >= proc.reps_all[-1] - 3:
        print '** have external align EPI volume, but seem to be\n'     \
              '   aligning EPI to end the runs, this looks fishy...'

    # get any base_vol option
    if proc.vr_ext_base != None: basevol = "%s%s" % (proc.vr_ext_pre,proc.view)
    else: basevol = None

    if proc.verb > 0:
        if basevol: print "-- %s: using base dset %s" % (block.label,basevol)
        else:       print "-- %s: base/sub indices are %d, %d" % \
                          (block.label,dset_ind,sub)

    # get base prefix (run is index+1)
    base = proc.prev_prefix_form(dset_ind+1, view=1)

    # get the interpolation value
    opt = block.opts.find_opt('-volreg_interp')
    resam = ' '.join(opt.parlist)     # maybe '-cubic'

    # get the zpad value
    opt = block.opts.find_opt('-volreg_zpad')
    if not opt or not opt.parlist: zpad = 1
    else: zpad = opt.parlist[0]

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-volreg_opts_vr')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '             %s \\\n' % ' '.join(opt.parlist)

    if basevol: bstr = basevol
    else:       bstr = "%s'[%d]'" % (base,sub)

    # ---------------
    # note whether we warp to tlrc, and set the prefix and flags accordingly
    doadwarp = block.opts.find_opt('-volreg_tlrc_adwarp') != None
    dowarp = block.opts.find_opt('-volreg_tlrc_warp') != None
    doe2a = block.opts.find_opt('-volreg_align_e2a') != None

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

    cur_prefix = proc.prefix_form_run(block)
    proc.volreg_prefix = cur_prefix
    cstr   = '' # appended to comment string
    if dowarp or doe2a:
        # verify that we have someplace to warp to
        if dowarp and not proc.tlrcanat:
            print '** cannot warp, need -tlrc_anat or -copy_anat with tlrc'
            return
        if doe2a and not proc.a2e_mat:
            print "** cannot align e2a at volreg, need mat from 'align' block"
            return
        prefix = 'rm.epi.volreg.r$run'
        proc.have_rm = 1            # rm.* files exist
        matstr = '%*s-1Dmatrix_save mat.r$run.vr.aff12.1D \\\n' % (13,' ')
        if doe2a:  cstr = cstr + ', align to anat'
        if dowarp: cstr = cstr + ', warp to tlrc space'
    else:
        if doadwarp: cstr = cstr + ', adwarp to tlrc space'
        prefix = cur_prefix
        matstr = ''
    prev_prefix = proc.prev_prefix_form_run(view=1)

    cmd = cmd + "# %s\n" \
                "# align each dset to base volume%s\n" \
                % (block_header('volreg'), cstr)

    if dowarp or doadwarp or do_extents:
        cmd = cmd + '\n'

        if dowarp or doadwarp:
            cmd = cmd + \
                "# verify that we have a +tlrc warp dataset\n"          \
                "if ( ! -f %s.HEAD ) then\n"                            \
                '    echo "** missing +tlrc warp dataset: %s.HEAD" \n'  \
                '    exit\n'                                            \
                'endif\n\n'                                             \
                % (proc.tlrcanat.pv(), proc.tlrcanat.pv())

        if do_extents:
            cmd = cmd + \
                "# create an all-1 dataset to mask the extents of the warp\n" \
                "3dcalc -a %s -expr 1 -prefix rm.epi.all1\n\n"                \
                % proc.prev_prefix_form(1, view=1)
            all1_input = 'rm.epi.all1' + proc.view

        if dowarp or do_extents: cmd = cmd + '# register and warp\n'

    cmd = cmd + "foreach run ( $runs )\n"                                     \
                "    # register each volume to the base\n"                    \
                "    3dvolreg -verbose -zpad %d -base %s \\\n"                \
                "             -1Dfile dfile.r$run.1D -prefix %s \\\n"         \
                "             %s \\\n"                                        \
                "%s"                                                          \
                "%s"                                                          \
                "             %s\n" %                                         \
                (zpad, bstr, prefix, resam, other_opts, matstr, prev_prefix)

    # if warping, multiply matrices and apply
    if doadwarp or dowarp or doe2a:
        opt = block.opts.find_opt('-volreg_warp_dxyz')
        if opt: dim = opt.parlist[0]
        else:
            dim = UTIL.get_truncated_grid_dim(proc.dsets[0].rel_input())
            if dim <= 0:
                print '** failed to get grid dim from %s' \
                      % proc.dsets[0].rel_input()
                return

    # if warping, multiply matrices and apply
    if dowarp or doe2a:
        # warn the user of output grid change
        print '++ volreg:',
        if doe2a and dowarp:
            print 'warp and align to isotropic %g mm tlrc voxels'%dim
            cstr = 'volreg, epi2anat and tlrc'
        elif doe2a:
            print 'aligning to isotropic %g mm voxels' % dim
            cstr = 'volreg and epi2anat'
        else:
            cstr = 'volreg and tlrc'
            print 'warping to isotropic %g mm tlrc voxels' % dim

        cmd = cmd + '\n'                                \
            '    # catenate %s transformations\n'       \
            '    cat_matvec -ONELINE \\\n' % cstr

        if dowarp: cmd = cmd +                                 \
                   '               %s::WARP_DATA -I \\\n' % proc.tlrcanat.pv()

        if doe2a:  cmd = cmd +                         \
                   '               %s -I \\\n' % proc.a2e_mat

        # if tlrc, use that for 3dAllineate base and change view
        if dowarp:
            allinbase = proc.tlrcanat.pv()
            proc.view = '+tlrc'
        else: allinbase = proc.anat.pv()

        cmd = cmd +                                     \
            '               mat.r$run.vr.aff12.1D > mat.r$run.warp.aff12.1D\n'

        
        if do_extents: wprefix = "rm.epi.nomask.r$run"
        else:          wprefix = cur_prefix
        cmd = cmd + '\n' +                                                 \
            '    # apply catenated xform : %s\n'                           \
            '    3dAllineate -base %s \\\n'                                \
            '                -input %s \\\n'                               \
            '                -1Dmatrix_apply mat.r$run.warp.aff12.1D \\\n' \
            '                -mast_dxyz %g\\\n'                            \
            '                -prefix %s \n'                                \
            % (cstr, allinbase, prev_prefix, dim, wprefix)

        if do_extents:      # then warp the all data and intersect over the run
           cmd = cmd + '\n' +                                                 \
               '    # warp the all-1 dataset for extents masking \n'          \
               '    3dAllineate -base %s \\\n'                                \
               '                -input %s \\\n'                               \
               '                -1Dmatrix_apply mat.r$run.warp.aff12.1D \\\n' \
               '                -mast_dxyz %g -final NN -quiet \\\n'          \
               '                -prefix rm.epi.1.r$run \n'                    \
               % (allinbase, all1_input, dim)

           cmd = cmd + '\n' +                                                 \
               '    # make an extents intersection mask of this run\n'        \
               '    3dTstat -min -prefix rm.epi.min.r$run rm.epi.1.r$run%s\n' \
               % proc.view

    # if there is a base_dset option, check for failure in 3dvolreg
    if basevol:
        cmd = cmd + '\n    # if there was an error, exit so user can see'     \
                    '\n    if ( $status ) exit\n\n'

    proc.mot_default = 'dfile_rall.1D'
    cmd = cmd + "end\n\n"                                                     \
                "# make a single file of registration params\n"               \
                "cat dfile.r*.1D > %s\n\n" % proc.mot_default

    # if not censoring motion, make a generic motion file
    if not proc.user_opts.find_opt('-regress_censor_motion'):
        cmd = cmd +                                                         \
            "# compute motion magnitude time series: the Euclidean norm\n"  \
            "# (sqrt(sum squares)) of the motion parameter derivatives\n"

        if proc.reps_vary :     # use -set_run_lengths aot -set_nruns
           cmd = cmd +                                                      \
               "1d_tool.py -infile %s \\\n"                                 \
               "           -set_run_lengths %s \\\n"                        \
               "           -derivative  -collapse_cols euclidean_norm \\\n" \
               "           -write motion_${subj}_enorm.1D\n\n"              \
               % (proc.mot_file, UTIL.int_list_string(proc.reps_all))
        else:                   # stick with -set_nruns
           cmd = cmd +                                                      \
               "1d_tool.py -infile %s -set_nruns %d \\\n"                   \
               "           -derivative  -collapse_cols euclidean_norm \\\n" \
               "           -write motion_${subj}_enorm.1D\n\n"              \
               % (proc.mot_file, proc.runs)

    if do_extents:
        proc.mask_extents = BASE.afni_name('mask_epi_extents' + proc.view)

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
            "foreach run ( $runs )\n"                           \
            "    3dcalc -a rm.epi.nomask.r$run%s -b %s \\\n"    \
            "           -expr 'a*b' -prefix %s\n"               \
            "end\n\n" % (proc.view, proc.mask_extents.pv(), cur_prefix)

    # ---------------
    # next, see if we want to apply a (manual) warp to tlrc space
    if block.opts.find_opt('-volreg_tlrc_adwarp'):
        if proc.view == '+tlrc':
            print '** cannot apply -volreg_tlrc_adwarp: alread in tlrc space'
            return
        if not proc.tlrcanat:
            print '** need -copy_anat with tlrc for -volreg_tlrc_adwarp'
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
    # make a copy of the "final" anatomy, called "anat_final.$subj"
    if proc.view == '+tlrc': aset = proc.tlrcanat
    else:                    aset = proc.anat
    if aset != None:
       proc.anat_final = aset.new(new_pref='anat_final.%s'%proc.subj_label)
       cmd += "# create an anat_final dataset, aligned with stats\n"    \
              "3dcopy %s %s\n\n"                                        \
              % (aset.pv(), proc.anat_final.prefix)

    if do_extents: emask = proc.mask_extents.prefix
    else:          emask = ''

    # if requested, make TSNR dataset from run 1 of volreg output
    opt = block.opts.find_opt('-volreg_compute_tsnr')
    if opt.parlist[0] == 'yes':
       tcmd = db_cmd_volreg_tsnr(proc, block, emask)
       if tcmd == None: return
       if tcmd != '': cmd += tcmd

    # used 3dvolreg, so have these labels
    proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

# compute temporal signal to noise before the blur (just run 1?)
def db_cmd_volreg_tsnr(proc, block, emask=''):

    # signal and error are both first run of previous output
    signal = proc.prefix_form(block, 1)

    return db_cmd_tsnr(proc,
           "# --------------------------------------\n" \
           "# create a TSNR dataset, just from run 1\n",
           signal, signal, proc.view, mask=emask,
           name_qual='.vreg.r01',detrend=1)

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
          print '\n'                                                         \
                '** error: spec files MUST contain lh or rh, and otherwise\n'\
                '   match (consider making copies, like SUBJ.stdmesh.lh.spec)'
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
          print '** error: failed to make spec var from %s' % snames[0]
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

    val, err = user_opts.get_type_opt(float, '-blur_size')
    if err:
        print '** error: -blur_size requires float argument'
        return 1
    elif val != None and val > 0.0:
        proc.surf_blur_fwhm = val
    else:
        proc.surf_blur_fwhm = 4.0
        print '** applying default -blur_size of %s mm FWHM' \
              % proc.surf_blur_fwhm

    if proc.verb > 2:
        print '-- surf info\n'          \
              '   spec          : %s\n' \
              '   anat          : %s\n' \
              '   anat_aligned  : %s\n' \
              '   anat_has_skull: %s\n' \
              '   surf_A        : %s\n' \
              '   surf_B        : %s\n' \
              '   blur_size     : %s\n' \
              '   spec_dir      : %s\n' \
              '   surf_spd_var  : %s\n' \
              '   spec_var      : %s\n' \
              % (proc.surf_spec, proc.surf_anat, proc.surf_anat_aligned,
                 proc.surf_anat_has_skull, proc.surf_A, proc.surf_B,
                 proc.surf_blur_fwhm, proc.surf_spec_dir, proc.surf_spd_var,
                 proc.surf_spec_var)

    errs = 0
    if not proc.surf_anat.exist(): 
        print '** error: missing -surf_anat dataset: %s' % proc.surf_anat.ppv()
        errs += 1
    if not proc.surf_spec:
        print '** error: missing -surf_spec option'
        return 1
    if not os.path.isfile(proc.surf_spec[0]):
        print '** error: missing -surf_spec file: %s' % proc.surf_spec[0]
        errs += 1
    if proc.surf_spec_dir and not os.path.isdir(proc.surf_spec_dir):
        print '** error: spec file directory not found: %s' % proc.surf_spec_dir
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
       print '-- surf_sv       : %s\n' \
             '   surf_sv_dir   : %s\n' \
             '   surf_svd_var  : %s\n' \
             % (proc.surf_sv.pv(), proc.surf_sv_dir, proc.surf_svd_var)

def db_cmd_surf(proc, block):
    """use 3dVol2Surf to map volume data to surface

         - set initial variables for spec_dir and sv_dir (if different)

         - call update_surf_sv(), depending on local alignment
         - possibly align surface volume to local anat (requires -copy_anat)
         - set variable for sv_dir (maybe spec_dir, maybe nothing)
    """

    block.index = proc.bindex   # save

    if proc.surf_anat == None:
        print '** error: missing surf_anat'
        return None

    if not proc.surf_A or not proc.surf_B:
        print '** error: both surf_A and surf_B are currently required'
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

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

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

    prev   = proc.prev_prefix_form_run(view=1)
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
        print '** missing final anat to align to as experiment base'
        return None

    # current surf_sv is surely remote, so apply dirctory and variables
    if proc.surf_anat_has_skull == 'yes': sstr = ' -strip_skull surf_anat'
    else: sstr = ''

    # the new surf_sv will be the aligned one
    newsv = proc.surf_sv.new(new_pref='${subj}_SurfVol_Alnd_Exp')
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

    block.valid = 1

def db_cmd_blur(proc, block):
    block.index = proc.bindex   # save

    # handle surface data separately
    if proc.surf_anat: return cmd_blur_surf(proc, block)

    opt    = block.opts.find_opt('-blur_filter')
    filter = opt.parlist[0]
    opt    = block.opts.find_opt('-blur_size')
    if opt:
        size = opt.parlist[0]
        havesize = 1
    else:
        size = 4.0
        havesize = 0
        
    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(view=1)

    try: fsize = float(size)
    except:
        print "** -blur_size must be a real number, have '%s'" %(parlist[0])
        return

    other_opts = ''

    # if -blur_in_mask, use 3dBlurInMask (requires 1blur_fwhm)
    bopt = block.opts.find_opt('-blur_to_fwhm')
    if bopt:
       if not havesize:
           print '** warning: using default 4.0 mm FWHM as _resulting_ blur\n'\
                 '            (use -blur_size to adjust)'

       # set any mask option
       if block.opts.find_opt('-blur_in_automask'):
           mopt = ' -automask'
       else:
          mopt = ''
          mask = proc.mask
          if not mask_created(mask): mask = proc.mask_extents
          if mask_created(mask): mopt = ' -mask %s%s'%(mask.prefix, proc.view)
          else:
             print '** error: no mask for -blur_to_fwhm, failing...'
             return

       # any last request?
       opt = block.opts.find_opt('-blur_opts_B2FW')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '                 %s \\\n' % ' '.join(opt.parlist)

       # make command string
       cstr = "    3dBlurToFWHM -FWHM %s%s \\\n"        \
              "%s"                                      \
              "                 -input %s \\\n"         \
              "                 -prefix %s \n"          \
              % (size, mopt, other_opts, prev, prefix)

    # if -blur_in_mask, use 3dBlurInMask (requires 1blur_fwhm)
    elif OL.opt_is_yes(block.opts.find_opt('-blur_in_mask')) or \
        block.opts.find_opt('-blur_in_automask'):

       # verify FWHM filter
       if filter != '-1blur_fwhm' and filter != '-FWHM':
          print "** error: 3dBlurInMask requires FWHM filter, have '%s'\n" \
                "   (consider scale of 1.36 for RMS->FWHM)" % (filter)
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
             print '** warning: no mask for -blur_in_mask, still proceeding...'

       # any last request?
       opt = block.opts.find_opt('-blur_opts_BIM')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '             %s \\\n' % ' '.join(opt.parlist)

       # make command string
       cstr = "    3dBlurInMask -preserve -FWHM %s%s \\\n"      \
              "                 -prefix %s \\\n"                \
              "%s"                                              \
              "                 %s\n"                           \
              % (str(size),mopt,prefix, other_opts, prev)

    else: # default: use 3dmerge for blur
       # maybe there are extra options to append to the command
       opt = block.opts.find_opt('-blur_opts_merge')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '             %s \\\n' % ' '.join(opt.parlist)

       cstr = "    3dmerge %s %s -doall -prefix %s \\\n"        \
              "%s"                                              \
              "            %s\n"                                \
              % (filter, str(size), prefix, other_opts, prev)

    cmd = "# %s\n" % block_header('blur')

    cmd = cmd + "# blur each volume of each run\n"      \
                "foreach run ( $runs )\n"               \
                "%s"                                    \
                "end\n\n" % (cstr)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def mod_blur_surf(block, proc, user_opts):

    # check for option updates
    uopt = user_opts.find_opt('-surf_smooth_niter')
    if uopt: block.opts.add_opt('-surf_smooth_niter', 1, uopt.parlist, setpar=1)

    block.valid = 1

def cmd_blur_surf(proc, block):
    """surface analysis: return a command to blur the data"""

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

    prev   = proc.prev_prefix_form_run()
    prefix = proc.prefix_form_run(block)
    param_file = 'surf.smooth.params.1D'

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
          '                       | tee %s \n'                          \
          '        else\n'                                              \
          '            set params = `1dcat %s`\n'                       \
          '            SurfSmooth -spec %s \\\n'                        \
          '                       -surf_A %s \\\n'                      \
          '                       -input %s \\\n'                       \
          '                       -met HEAT_07 \\\n'                    \
          '                       -Niter $params[1] \\\n'               \
          '                       -sigma $params[2] \\\n'               \
          '                       -output %s \n'                        \
          '        endif\n'                                             \
          '    end\n'                                                   \
          'end\n\n'                                                     \
          % (feh_str,
             param_file, spec_str, proc.surf_A, prev,
             proc.surf_blur_fwhm, prev, prefix, param_file,
             param_file, spec_str, proc.surf_A, prev, prefix)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_mask(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-mask_type', 1, ['union'], setpar=1)
        block.opts.add_opt('-mask_dilate', 1, [1], setpar=1)
        block.opts.add_opt('-mask_rm_segsy', 1, ['yes'], setpar=1)
        block.opts.add_opt('-mask_test_overlap', 1, ['yes'], setpar=1)

    # check for user updates

    uopt = user_opts.find_opt('-mask_dilate')
    bopt = block.opts.find_opt('-mask_dilate')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print "** -mask_dilate requres an int nsteps (have '%s')" % \
                  uopt.parlist[0]
            block.valid = 0
            return 1

    apply_uopt_to_block('-mask_apply',        user_opts, block)
    apply_uopt_to_block('-mask_rm_segsy',     user_opts, block)
    apply_uopt_to_block('-mask_segment_anat', user_opts, block)
    apply_uopt_to_block('-mask_segment_erode',user_opts, block)
    apply_uopt_to_block('-mask_test_overlap', user_opts, block)
    apply_uopt_to_block('-mask_type',         user_opts, block)

    proc.mask_epi = BASE.afni_name('full_mask%s$subj' % proc.sep_char)
    # we have an EPI mask, add it to the roi_dict for optional regress_ROI
    proc.roi_dict['brain'] = proc.mask_epi
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
    block.index = proc.bindex   # save

    cmd = ''
    opt = block.opts.find_opt('-mask_type')
    type = opt.parlist[0]
    if type == 'union': minv = 0           # result must be greater than minv
    else:               minv = 0.999

    # if we have an EPI mask, set the view here
    if proc.mask_epi.view == '': proc.mask_epi.view = proc.view
    if not proc.mask: proc.mask = proc.mask_epi
    if not proc.mask:
        print '** ERROR: no mask dset for mask block'
        return

    opt = block.opts.find_opt('-mask_dilate')
    nsteps = opt.parlist[0]

    prev = proc.prev_prefix_form_run(view=1)
    cmd = cmd + "# %s\n"                                                \
                "# create 'full_mask' dataset (%s mask)\n"              \
                "foreach run ( $runs )\n"                               \
                "    3dAutomask -dilate %d -prefix rm.mask_r$run %s\n"  \
                "end\n\n" % (block_header('mask'), type, nsteps, prev)
    proc.have_rm = 1            # rm.* files exist

    if proc.runs > 1:  # if more than 1 run, create union mask
        cmd = cmd + "# get mean and compare it to %s for taking '%s'\n"      \
                    "3dMean -datum short -prefix rm.mean rm.mask*.HEAD\n"    \
                    "3dcalc -a rm.mean%s -expr 'ispositive(a-%s)' "          \
                    "-prefix %s\n\n" %                                       \
                    (str(minv), type, proc.view, str(minv),
                     proc.mask_epi.prefix)
    else:  # just copy the one
        cmd = cmd + "# only 1 run, so copy this to full_mask\n"              \
                    "3dcopy rm.mask_r01%s %s\n\n"                            \
                    % (proc.view, proc.mask_epi.prefix)
    proc.mask_epi.created = 1  # so this mask 'exists' now

    # if possible make a subject anat mask, resampled to EPI
    if proc.warp_epi:
        mc = anat_mask_command(proc, block)
        if mc == None: return
        cmd = cmd + mc

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
        elif mtype == 'group':   proc.mask = proc.mask_group
        elif mtype == 'extents': proc.mask = proc.mask_extents
        if proc.verb > 1: print "++ applying mask as '%s'" % mtype
        if proc.mask: proc.regmask = 1 # apply, if it seems to exist
        else:
            print "** ERROR: cannot apply %s mask" % mtype
            return

    cmd += mask_segment_anat(proc, block)

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
    """

    # ----------------------------------------------------------------------
    # make any segmentation masks

    opt = block.opts.find_opt('-mask_segment_anat')
    if not OL.opt_is_yes(opt): return ''        # default is now no

    if not proc.anat_final:
        if proc.verb > 1:
           print '-- no Segsy (either no anat_final or -mask_segment_anat no)'
        return ''
    # and proc.anat_has_skull:

    cin  = BASE.afni_name('Segsy/Classes%s' % proc.view)
    cres = BASE.afni_name('Classes_resam%s' % proc.view)
    mset = proc.prev_prefix_form(1, view=1)

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

    # ==== if not doing ROI regression and not eroding, we are done ====
    if not proc.user_opts.find_opt('-regress_ROI') and \
       not OL.opt_is_yes(block.opts.find_opt('-mask_segment_erode')):
       return cmd

    ### else continue and make ROI masks

    # make erosion ROIs?  (default = yes)
    erode = not OL.opt_is_no(block.opts.find_opt('-mask_segment_erode'))

    # list ROI labels for comments
    baseliststr = '%s' % ' '.join(sclasses)
    if erode: liststr = '(%s and %s)' % (baseliststr, 'e '.join(sclasses))
    else:     liststr = '(%s)' % baseliststr

    # make ROIs per class, and erode them by default
    roiprefix = 'mask_${class}'
    cc = '# make individual ROI masks for regression %s\n' \
         'foreach class ( %s )\n' % (liststr, baseliststr)

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

    proc.mask_classes = cres    # store, just in case
    
    for sc in sclasses:
       proc.roi_dict[sc] = BASE.afni_name('mask_%s_resam%s' % (sc, proc.view))
       if erode:
          ec = '%se' % sc
          proc.roi_dict[ec] = BASE.afni_name('mask_%s_resam%s'%(ec,proc.view))

    return cmd


# if possible: make a group anatomical mask (resampled to EPI)
#    - only if tlrc block and -volreg_tlrc_warp
#    - apply from -tlrc_base
# return None on failure
def group_mask_command(proc, block):
    if not proc.warp_epi & WARP_EPI_TLRC_WARP or not proc.tlrc_base:
        if proc.verb>2: print "-- no group mask, warp_epi = %d" % proc.warp_epi
        return ''

    #--- first things first, see if we can locate the tlrc base

    cmd = '@FindAfniDsetPath %s' % proc.tlrc_base.pv()
    if proc.verb > 1: estr = 'echo'
    else            : estr = ''
    com = BASE.shell_com(cmd, estr, capture=1)
    com.run()

    if com.status or not com.so or len(com.so[0]) < 2:
        # call this a non-fatal error for now
        print "** failed to find tlrc_base '%s' for group mask" \
              % proc.tlrc_base.pv()
        if proc.verb > 2:
           print '   status = %s' % com.status
           print '   stdout = %s' % com.so
           print '   stderr = %s' % com.se
        return ''

    proc.tlrc_base.path = com.so[0]
    # nuke any newline character
    newline = proc.tlrc_base.path.find('\n')
    if newline > 1: proc.tlrc_base.path = proc.tlrc_base.path[0:newline]
    print "-- masking: group anat = '%s', exists = %d"   \
          % (proc.tlrc_base.pv(), proc.tlrc_base.exist())

    if not proc.tlrc_base.exist():
        print "** cannot create group mask"
        return ''

    #--- tlrc base exists, now resample and make a mask of it
    proc.mask_group = proc.mask_epi.new('mask_group')
    cmd = "# ---- create group anatomy mask, %s ----\n"  \
          "#      (resampled from tlrc base anat, %s)\n" \
          % (proc.mask_group.pv(), proc.tlrc_base.pv())

    tanat = proc.mask_group.new('rm.resam.group') # temp resampled group dset
    cmd = cmd + "3dresample -master %s -prefix ./%s \\\n" \
                "           -input %s\n\n"                \
                % (proc.mask_epi.pv(), tanat.prefix, proc.tlrc_base.ppv())

    # convert to a binary mask via 3dmask_tool, to fill in a bit
    cmd = cmd + "# convert to binary group mask; fill gaps and holes\n"     \
                "3dmask_tool -dilate_input 5 -5 -fill_holes -input %s \\\n" \
                "            -prefix %s\n\n"                                \
                % (tanat.pv(), proc.mask_group.prefix)

    proc.mask_group.created = 1  # so this mask 'exists' now

    return cmd

# if possible make a subject anatomical mask (resampled to EPI)
#    * if -volreg_tlrc_adwarp, there is no ss anat
#    - if -volreg_tlrc_warp, apply from tlrc anat
#    - if a2e, apply from anat_al
#    - if e2a, apply from anat_ss (intermediate anat)
# return None on failure
def anat_mask_command(proc, block):
    if not proc.warp_epi: return ''

    # adwarp: we cannot rely on skull-stripped anat, so just return
    if proc.warp_epi & WARP_EPI_TLRC_ADWARP: return ''

    proc.mask_anat = proc.mask_epi.new('mask_anat.$subj')
    cmd = "# ---- create subject anatomy mask, %s ----\n" % proc.mask_anat.pv()

    if proc.verb > 2:
        print '-- mask for anat based on warp_epi == %d\n'                \
              '   (1==tlrc_adwarp, 2==tlrc_warp, 4=anat2epi, 8=epi2anat)' \
              % proc.warp_epi

    # set anat, comment string text and temporary anat
    if proc.warp_epi & WARP_EPI_TLRC_WARP:
        anat = proc.tlrcanat
        ss = 'tlrc'
    elif proc.warp_epi & (WARP_EPI_ALIGN_A2E | WARP_EPI_ALIGN_E2A):
        anat = proc.anat
        ss = 'aligned'
    # no longer invert e2a matrix to get ss anat, since the current
    # anat will already be stripped
    else: # should not happen
        print '** anat_mask_command: invalid warp_epi = %d' % proc.warp_epi
        return None
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

    opt = block.opts.find_opt('-mask_test_overlap')
    if not opt or OL.opt_is_yes(opt):  # so default to 'yes'
        if proc.mask_epi and proc.mask_anat:
            rcmd = "# compute overlaps between anat and EPI masks\n"  \
                   "3dABoverlap -no_automask %s %s \\\n"              \
                   "            |& tee out.mask_overlap.txt\n\n"      \
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
            print "** -scale_max_val requres an int param (have '%s')" % \
                  uopt.parlist[0]
            block.valid = 0
            return 1

    # if the user does not want a max, use 0
    if user_opts.find_opt('-scale_no_max') and bopt:
        bopt.parlist[0] = 0

    block.valid = 1

def db_cmd_scale(proc, block):
    block.index = proc.bindex   # save

    cmd = ''
    # check for max scale value 
    opt = block.opts.find_opt('-scale_max_val')
    max = opt.parlist[0]
    if max > 100: valstr = 'min(%d, a/b*100)*step(a)*step(b)' % max
    else:         valstr = 'a/b*100*step(a)'

    # options for surface analysis
    if proc.surf_anat:
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

    prev = proc.prev_prefix_form_run(view=1)
    prefix = proc.prefix_form_run(block)
    cmd += "# %s\n"                                                     \
           "# scale each voxel time series to have a mean of 100\n"     \
           "# (be sure no negatives creep in)\n"                        \
           "%s"                                                         \
           % (block_header('scale'), maxstr)

    cmd += feh_str      # if surf, foreach hemi

    cmd += "%sforeach run ( $runs )\n"                                  \
           "%s    3dTstat -prefix %s_r$run%s %s\n"                      \
           "%s    3dcalc -a %s %s-b %s_r$run%s \\\n"                    \
           "%s"                                                         \
           % (istr, istr, mean_pre, suff, prev,
              istr, prev, bstr, mean_pre, vsuff, mask_str)

    cmd += "%s           -expr '%s' \\\n"                               \
           "%s           -prefix %s\n"                                  \
           "%send\n"                                                    \
           % (istr, expr, istr, prefix, istr)

    cmd += feh_end + '\n'

    proc.have_rm = 1            # rm.* files exist

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_regress(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-regress_basis', 1, ['GAM'], setpar=1)
        block.opts.add_opt('-regress_censor_prev', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_compute_tsnr', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_compute_gcor', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_cormat_warnings', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_fout', 1, ['yes'], setpar=1)
        block.opts.add_opt('-regress_polort', 1, [-1], setpar=1)
        block.opts.add_opt('-regress_stim_files', -1, [])
        block.opts.add_opt('-regress_stim_labels', -1, [])
        block.opts.add_opt('-regress_RONI', -1, [])
        block.opts.add_opt('-regress_stim_times', -1, [])
        block.opts.add_opt('-regress_stim_times_offset', 1, [0], setpar=1)

        block.opts.add_opt('-regress_extra_stim_files', -1, [])
        block.opts.add_opt('-regress_extra_stim_labels', -1, [])

        block.opts.add_opt('-regress_opts_3dD', -1, [])
        block.opts.add_opt('-regress_opts_reml', -1, [])
        block.opts.add_opt('-regress_make_ideal_sum', 1, ['sum_ideal.1D'],
                                                       setpar=1)
        block.opts.add_opt('-regress_errts_prefix', 1, [])
        block.opts.add_opt('-regress_fitts_prefix', 1, ['fitts.$subj'],
                                                       setpar=1)
        block.opts.add_opt('-regress_make_cbucket', 1, ['no'], setpar=1)

    errs = 0  # allow errors to accumulate

    apply_uopt_to_block('-regress_anaticor', user_opts, block)

    # check for user updates
    uopt = user_opts.find_opt('-regress_basis')
    bopt = block.opts.find_opt('-regress_basis')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]
        # rcr - may have many basis functions, if any have unknown response
        #       curve, set iresp prefix for them
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
            print "** -regress_basis_normall requires float param (have '%s')" \
                  % uopt.parlist[0]
            errs += 1
        bopt = block.opts.find_opt('-regress_basis_normall')
        if bopt: bopt.parlist[0] = norm
        else: block.opts.add_opt('-regress_basis_normall', 1, [norm], setpar=1)

    uopt = user_opts.find_opt('-regress_polort')
    bopt = block.opts.find_opt('-regress_polort')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print "** -regress_polort requires int for degree (have '%s')\n" \
                  % uopt.parlist[0]
            errs += 1

    # files can have many stim classes in one file
    uopt = user_opts.find_opt('-regress_stim_files')
    bopt = block.opts.find_opt('-regress_stim_files')
    if uopt and bopt:
        if len(uopt.parlist) <= 0:
            print "** no files for -regress_stim_files?"
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
                print "++ have RONI indices: %s" % bopt.parlist
        except:
            print "** -regress_RONI requires integral parameters, have: %s" % \
                  uopt.parlist
            errs += 1

    apply_uopt_to_block('-regress_ROI', user_opts, block)  # 04 Sept 2012

    # times is one file per class
    uopt = user_opts.find_opt('-regress_stim_times')
    bopt = block.opts.find_opt('-regress_stim_times')
    if uopt and bopt:
        if len(uopt.parlist) <= 0:
            print "** no files for -regress_stim_times?"
            errs += 1
        # verify this doesn't go with no_stim_times
        if user_opts.find_opt('-regress_use_stim_files'):
            print '** have both -regress_use_stim_files and -regress_stim_times'
            errs += 1
        if user_opts.find_opt('-regress_no_stim_times'):
            print '** have both -regress_no_stim_times and -regress_stim_times'
            errs += 1
        bopt.parlist = uopt.parlist
        proc.stims_orig = uopt.parlist   # store for initial copy

    uopt = user_opts.find_opt('-regress_stim_times_offset')
    bopt = block.opts.find_opt('-regress_stim_times_offset')
    if uopt and bopt:
        try: bopt.parlist[0] = float(uopt.parlist[0])
        except:
            print "** stim times offset must be float, have '%s'" \
                  % uopt.parlist[0]

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
        bopt = block.opts.find_opt('-regress_CS_NN')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_CS_NN', 1, uopt.parlist, setpar=1)

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
            print "** have -regress_extra_stim_labels without" + \
                  " -regress_extra_stim_files"
            errs += 1
        elif nxstim != nxlabs:
            print "** have %d extra stims but %d extra labels" % \
                  (nxstim, nxlabs)
            errs += 1

    # --------------------------------------------------
    # if we are here, then we should have stimulus files
    if len(proc.stims_orig) > 0:
        # create local names for stim files
        proc.stims = []
        for file in proc.stims_orig:
            proc.stims.append('stimuli/%s' % os.path.basename(file))

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
        print "** -regress_censor_motion limit must be float, have '%s'" \
              % uopt.parlist[0]
        errs += 1
      if limit < 0.0:
        print '** -regress_censor_motion limit must be positive, have %g'%limit
        errs += 1
      if bopt: bopt.parlist[0] = limit
      else: block.opts.add_opt('-regress_censor_motion', 1, [limit], setpar=1)

    # do we also censor first N TRs per run?
    uopt = user_opts.find_opt('-regress_censor_first_trs')
    bopt = block.opts.find_opt('-regress_censor_first_trs')
    if uopt:
        if not block.opts.find_opt('-regress_censor_motion'):
            print '** -regress_censor_first_trs requires -regress_censor_motion'
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
    uopt = user_opts.find_opt('-regress_cormat_warnigns')
    if uopt:
        bopt = block.opts.find_opt('-regress_cormat_warnigns')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_cormat_warnigns', 1,
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
        print '** -regress_no_mask is now the default'
        proc.regmask = 0

    # maybe the user really does want to apply the mask to regression
    # note: no_mask is now the default    24 Mar 2009
    uopt = user_opts.find_opt('-regress_apply_mask')
    if uopt:
        print "** regress_apply_mask has been deprecated"
        print "   (consider '-mask_apply epi')"
        proc.regmask = 1

    # check for global or local stim_times
    uopt = user_opts.find_opt('-regress_global_times')
    u2   = user_opts.find_opt('-regress_local_times')
    bopt = block.opts.find_opt('-regress_global_times')
    b2   = block.opts.find_opt('-regress_local_times')
    if uopt or u2:
      if uopt and u2:
        print '** error: given -regress_global_times AND -regress_local_times'
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
            print '** failed to parse directory/file from -regress_motion_file'
            print '   (file is %s)' % uopt.parlist[0]
            errs += 1
        proc.mot_extern = uopt.parlist[0]
        proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']
        # -volreg_regress_per_run should be okay  20 May 2011
        # (must still assume TR correspondence)

    # maybe the user wants to specify types of motion parameters to use
    uopt = user_opts.find_opt('-regress_apply_mot_types')
    if uopt:
        if user_opts.find_opt('-regress_no_motion_demean') or \
           user_opts.find_opt('-regress_no_motion_deriv'):
            print '** cannot use -regress_apply_mot_types with either of\n' \
                  '   -regress_no_motion_demean or -regress_no_motion_deriv'
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
        if proc.verb > 0: print '-- will use -stim_files in 3dDeconvolve'
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
        # if explicit 'yes', require blur estimation
        if OL.opt_is_yes(uopt)                                and \
           not block.opts.find_opt('-regress_est_blur_errts') and \
           not block.opts.find_opt('-regress_est_blur_epits'):
            print '** blur estimation is required for ClustSim\n' \
                  '   (consider -regress_est_blur_errts (or _epits))'
            errs += 1

    # check on tsnr and gcor
    apply_uopt_to_block('-regress_compute_tsnr', user_opts, block)
    apply_uopt_to_block('-regress_compute_gcor', user_opts, block)

    # possibly update cbucket option
    apply_uopt_to_block('-regress_make_cbucket', user_opts, block)

    # possibly update cbucket option
    apply_uopt_to_block('-regress_apply_ricor', user_opts, block)

    # maybe do bandpass filtering in the regression
    apply_uopt_to_block('-regress_bandpass', user_opts, block)

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
    block.index = proc.bindex   # save

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

    # maybe we want a special prefix
    if proc.script_3dD: tmp_prefix = "${prefix_3dd}"
    else:               tmp_prefix = ''

    # options for surface analysis
    if proc.surf_anat:
        # string for foreach hemi loop
        feh_str = 'foreach %s ( %s )\n' \
                  % (proc.surf_spec_var_iter, ' '.join(proc.surf_hemilist))
        feh_end = 'end\n'

        # suffix needs the hemisphere iteration variable
        suff = '.%s.niml.dset' % proc.surf_svi_ref
        istr = ' '*4             # extra indent, for foreach hemi loop
        vstr = suff
    else:
        feh_str = ''
        feh_end = ''
        suff = ''
        istr = ''
        vstr = proc.view

    opt = block.opts.find_opt('-regress_polort')
    polort = opt.parlist[0]
    if ( polort < 0 ) :
        polort = UTIL.get_default_polort(proc.tr, proc.reps)
        if proc.verb > 0:
            print "++ updating polort to %d, from run len %.1f s" %  \
                  (polort, proc.tr*proc.reps)

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
            if proc.verb > 2: print '-- duplicating single basis function'
            basis = [basis[0] for ind in range(len(proc.stims))]
        else:
            print '** error: have %d basis functions but %d stim classes' \
                  % (len(basis), len(proc.stims))
            return

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
            print '** error: have %d stim types but %d stim classes' \
                  % (len(stim_types), len(proc.stims))
            return
    if not UTIL.vals_are_constant(stim_types, 'times'):
        print '++ applying %d stim types: %s' % (len(stim_types),stim_types)

    # ----------------------------------------
    # deal with motion (demean, deriv, per-run, censor)
    if block.opts.find_opt('-regress_no_motion'): proc.mot_labs = []
    else:
        err, newcmd = db_cmd_regress_motion_stuff(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # ----------------------------------------
    # bandpass?
    if block.opts.find_opt('-regress_bandpass'):
        err, newcmd = db_cmd_regress_bandpass(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # ----------------------------------------
    # gmean?  change to generic -regress_RONI
    if block.opts.find_opt('-regress_ROI'):
        err, newcmd = db_cmd_regress_ROI(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

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
    if not opt.parlist or len(opt.parlist) == 0:
        # then any original stims are as 1D, whether converting or not
        if not valid_file_types(proc, proc.stims_orig, 1,
                                stypes=stim_types): return
    else:
        # local/global question just answered above
        if not valid_file_types(proc,proc.stims_orig,verify_times_type,
                                stypes=stim_types): return

    # and check any extras against 1D only
    if not valid_file_types(proc, proc.extra_stims_orig, 1): return

    # and check any extras against 1D only
    if not married_types_match(proc, proc.stims_orig, stim_types, basis): return

    nmotion = len(proc.mot_labs) * len(proc.mot_regs)
    if proc.ricor_apply == 'yes': nricor = proc.ricor_nreg
    else:                         nricor = 0
    total_nstim =  len(proc.stims) + len(proc.extra_stims) + \
                   nmotion + nricor
    
    # maybe we will censor
    if proc.censor_file: censor_str = '    -censor %s' % proc.censor_file
    else:                censor_str = ''

    # check for regress_orts lines
    reg_orts = []
    for ort in proc.regress_orts:
       reg_orts.append('    -ortvec %s %s' % (ort[0], ort[1]))

    # make actual 3dDeconvolve command as string c3d:
    #    init c3d, add O3dd elements, finalize c3d
    #    (O3dd = list of 3dd option lines, which may need an extra indent)

    O3dd = ['%s3dDeconvolve -input %s' % (istr, proc.prev_dset_form_wild()),
            mask, censor_str]
    O3dd.extend(reg_orts)
    O3dd.extend([ '    -polort %d%s' % (polort, datum),
                  normall, times_type,
                  '    -num_stimts %d' % total_nstim])

    # verify labels (now that we know the list of stimulus files)
    opt = block.opts.find_opt('-regress_stim_labels')
    if not opt or not opt.parlist:
        labels = []
        for ind in range(len(proc.stims)):
            labels.append('stim%02d' % (ind+1))
        if proc.verb > 0: print ('++ adding labels: %s' % labels)
    elif len(proc.stims) != len(opt.parlist):
        print "** cmd_regress: have %d stims but %d labels" % \
              (len(proc.stims), len(opt.parlist))
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
            if proc.verb > 0: print ('++ adding extra labels: %s' % exlabs)

    # note the total number of regressors (of interest)
    nregs = len(proc.stims) + len(proc.extra_stims)

    # note any RONI (regs of no interest)
    roni_list = []
    bopt = block.opts.find_opt('-regress_RONI')
    if bopt and bopt.parlist:
        roni_list = bopt.parlist
        # check min/max
        if min(roni_list) < 1 or max(roni_list) > nregs:
            print "** regressor indices in RONI list must be in [1,%d]\n" \
                  "   have: %s" % (nregs, roni_list)
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
        #       (so mabye -stim_times can be set from proc.stim_times_opt)
        if sfiles:  # then -stim_file and no basis function
            O3dd.append("    -stim_file %d %s" % (ind+1, proc.stims[ind]))
        elif stim_types[ind] == 'file':
            O3dd.append("    -stim_file %d %s" % (ind+1, proc.stims[ind]))
            if basis[ind] != 'NONE':
               ss = os.path.basename(proc.stims[ind])
               print "** ignoring basis #%d, %s, for -stim_type 'file' = %s" \
                     % (ind+1, basis[ind], ss)
        else:
            if stim_types[ind] == 'times': st_suf = ''
            else:                          st_suf = '_%s' % stim_types[ind]
            O3dd.append("    -stim_times%s %d %s '%s'"  % \
                        (st_suf, ind+1, proc.stims[ind], basis[ind]))
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
    if nregs == 0 or (not opt.parlist and (bluropt or tsnropt)):
        opt.parlist = ['errts.${subj}%s' % suff]

    errts_pre = ''
    if not opt or not opt.parlist: errts = ''
    else:
        errts_pre = opt.parlist[0]
        errts = '    -errts %s%s' % (tmp_prefix, errts_pre)
    # -- end errts --

    # if the user wants to compute fitts, save the prefix
    compute_fitts = 0
    if block.opts.find_opt('-regress_compute_fitts'):
        if fitts_pre == '':
            print '** regress_compute_fitts: no fitts prefix'
            return
        if errts_pre == '':
            print '** regress_compute_fitts: no errts prefix'
            return
        compute_fitts = 1
        fitts = ''  # so nothing in 3dDeconvolve

    # see if the user has provided other options (like GLTs)
    opt = block.opts.find_opt('-regress_opts_3dD')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '    %s' % \
          ' '.join(UTIL.quotize_list(opt.parlist, '\\\n%s    '%istr, 1))

    # are we going to stop with the 1D matrix?
    opt = block.opts.find_opt('-regress_3dD_stop')
    if opt: stop_opt = '    -x1D_stop'
    else  : stop_opt = ''

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

    # possibly run the REML script (only here in the case of surfaces)
    if block.opts.find_opt('-regress_reml_exec') and proc.surf_anat:
        rcmd = db_cmd_reml_exec(proc, block, short=1)
        if not rcmd: return
        rcmd = '\n' + rcmd
    else: rcmd = ''

    # now create full 3dDeconvolve command, connecting every option
    # line with space, backslash, a newline, and possibly another indent,

    jstr = ' \\\n%s' % istr
    c3d  = '# run the regression analysis\n' + feh_str + \
           jstr.join([s for s in O3dd if s])
    c3d += rcmd + feh_end + '\n\n'

    # done creating 3dDeconvolve command c3d, add to cmd string
    cmd += c3d

    # maybe user just wants a 3dDeconvolve command script
    if proc.script_3dD:
       header = '#!/bin/tcsh\n\n'               \
                'set subj = %s\n\n'             \
                'set prefix_3dd = %s\n\n' % (proc.subj_id, proc.prefix_3dD)
       UTIL.write_text_to_file(proc.script_3dD, header+c3d, wrap=1, exe=1)
       print '++ writing 3dDeconvolve script %s ...' % proc.script_3dD
       sys.exit(0)

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
    opt = block.opts.find_opt('-regress_cormat_warnigns')
    if not opt or OL.opt_is_yes(opt):  # so default to 'yes'
        rcmd = "# display any large pariwise correlations from the X-matrix\n"\
               "1d_tool.py -show_cormat_warnings -infile %s"                  \
               " |& tee out.cormat_warn.txt\n\n" % proc.xmat
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

    # possibly run the REML script (run eariler in the case of surfaces)
    if block.opts.find_opt('-regress_reml_exec') and not proc.surf_anat:
        rcmd = db_cmd_reml_exec(proc, block)
        if not rcmd: return
        cmd = cmd + rcmd + '\n\n'

    # if REML, -x1D_stop and errts_pre, append _REML to errts_pre
    if block.opts.find_opt('-regress_reml_exec') and \
        stop_opt and errts_pre: errts_pre = errts_pre + '_REML'

    # create all_runs dataset
    proc.all_runs = 'all_runs%s$subj%s' % (proc.sep_char, suff)
    cmd = cmd + "# create an all_runs dataset to match the fitts, errts, etc.\n"
    cmd = cmd + feh_str + "%s3dTcat -prefix %s %s\n" % \
          (istr, proc.all_runs, proc.prev_dset_form_wild()) + feh_end + '\n'

    # maybe add the 3dTfitter block
    if block.opts.find_opt('-regress_anaticor') or \
       block.opts.find_opt('-regress_sim_motion') :
       rv, tcmd = db_cmd_regress_tfitter(proc, block)
       if rv: return
       cmd += tcmd

    # if errts and scaling, maybe create tsnr volume as mean/stdev(errts)
    # (if scaling, mean should be 100)
    opt = block.opts.find_opt('-regress_compute_tsnr')
    if opt.parlist[0] == 'yes':
       if errts_pre:
          tcmd = db_cmd_regress_tsnr(proc, block, proc.all_runs, errts_pre)
          if tcmd == None: return  # error
          if tcmd != '': cmd += tcmd
       else: print '-- no errts, will not compute final TSNR'

    # if errts and epi mask, maybe compute GCOR as l2norm of maskave of
    # unit errts (leave as rm. dataset)
    opt = block.opts.find_opt('-regress_compute_gcor')
    if opt.parlist[0] == 'yes':
       if errts_pre and proc.mask_epi and not proc.surf_anat:
          tcmd = db_cmd_regress_gcor(proc, block, errts_pre)
          if tcmd == None: return  # error
          if tcmd != '': cmd += tcmd
       elif proc.verb > 1:
          print '-- no errts or EPI mask (or have surf), will not compute GCOR'

    # possibly create computed fitts dataset
    if compute_fitts:
        fstr = feh_str
        if stop_opt == '': # create if no -x1D_stop
            fstr += "%s# create fitts dataset from all_runs and errts\n" % istr
            fstr += "%s3dcalc -a %s%s -b %s%s -expr a-b \\\n"            \
                    "%s       -prefix %s%s\n"                            \
                    % (istr, proc.all_runs, vstr, errts_pre, vstr,
                       istr, fitts_pre, suff)
        elif not block.opts.find_opt('-regress_reml_exec'):
            print '** cannot compute fitts, have 3dD_stop but no reml_exec'
            return

        # if reml_exec, make one for the REML fitts, too
        if block.opts.find_opt('-regress_reml_exec'):
            if stop_opt: fstr += '\n'
            fstr += "%s# create fitts from REML errts\n" % istr
            fstr += "%s3dcalc -a %s%s -b %s\_REML%s -expr a-b \\\n" \
                    "%s       -prefix %s\_REML%s\n"                 \
                    % (istr, proc.all_runs, vstr, errts_pre, vstr,
                       istr, fitts_pre, suff)
        cmd = cmd + fstr + feh_end + '\n'

    # extract ideal regressors, and possibly make a sum
    opt = block.opts.find_opt('-regress_no_ideals')
    if not opt and len(basis) > 0:
        if len(labels) != len(basis):
            print '** internal error: label and basis arrays not equal lengths'
            print '   (%d labels, %d basis functions)'%(len(labels),len(basis))
            return
        # while basis functions have one regressor, make ideals
        # (so no ideal after failure)
        if UTIL.basis_has_one_reg(basis[0]):
            cmd = cmd + "# create ideal files for fixed response stim types\n"
            first = (polort+1) * proc.runs
            for ind in range(len(labels)):
                # once unknown or multiple regs, quit
                if not UTIL.basis_has_one_reg(basis[ind]): break
                cmd = cmd + "1dcat %s'[%d]' > ideal_%s.1D\n" % \
                            (proc.xmat_nocen, first+ind, labels[ind])
            cmd = cmd + '\n'
        else: print '-- classes have multiple regressors, so not making ideals'

    opt = block.opts.find_opt('-regress_make_ideal_sum')
    nopt = block.opts.find_opt('-regress_no_ideal_sum')
    # opt should always be set, so let nopt override
    if opt and opt.parlist and not nopt:
        # get regressors of interest from X-matrix, rather than in python
        # (this requires check_date of 2 Nov 2010)
        cmd = cmd +                                                           \
               "# --------------------------------------------------------\n" \
               "# compute sum of non-baseline regressors from the X-matrix\n" \
               "# (use 1d_tool.py to get list of regressor colums)\n"      \
               "set reg_cols = `1d_tool.py -infile %s -show_%s`\n"         \
               '3dTstat -sum -prefix %s %s"[$reg_cols]"\n\n'               \
               '# also, create a stimulus-only X-matrix, for easy review\n'\
               '1dcat %s"[$reg_cols]" > X.stim.xmat.1D\n\n'                \
                % (proc.xmat_nocen, "indices_interest", opt.parlist[0],
                   proc.xmat_nocen, proc.xmat_nocen)

    # check for blur estimates
    bcmd = db_cmd_blur_est(proc, block)
    if bcmd == None: return  # error
    if bcmd: cmd += bcmd

    proc.pblabel = block.label  # set 'previous' block label

    return cmd

# create a short command to run the REML script
# The script name is currently stats.REML_cmd, based on the 'stats.' -bucket
# prefix in 3dD.
#
# return None on failure
def db_cmd_reml_exec(proc, block, short=0):
    """short version does not have the status check
       - probably used for surface analysis"""

    if proc.verb > 1: print '++ creating reml_exec command string'

    if proc.surf_anat: istr = '    '
    else:              istr = ''

    # see if the user has provided other 3dREMLfit options
    opt = block.opts.find_opt('-regress_opts_reml')
    if not opt or not opt.parlist: reml_opts = ''
    else: reml_opts = ' '.join(UTIL.quotize_list(opt.parlist, '', 1))

    cmd = '%s# -- execute the 3dREMLfit script, written by 3dDeconvolve --\n' \
          '%stcsh -x stats.REML_cmd %s\n' % (istr, istr, reml_opts)

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
def db_cmd_regress_gcor(proc, block, errts_pre):

    if not errts_pre or not proc.mask_epi: return ''
    if proc.surf_anat:
       if proc.verb > 1: print "** no gcor until handle 'both' hemis"
       return ''

    # Do not handle surface until we have both hemispheres at once.

    gcor_file = 'out.gcor.1D'
    gu_mean   = 'gmean.errts.unit.1D'
    uset      = BASE.afni_name('rm.errts.unit%s' % proc.view)

    cmd = '# ---------------------------------------------------\n'     \
          '# compute and store GCOR (global correlation average)\n'     \
          '# (sum of squares of global mean of unit errts)\n'           \
          '3dTnorm -norm2 -prefix %s %s%s\n'                            \
          '3dmaskave -quiet -mask %s %s > %s\n'                         \
          % (uset.prefix, errts_pre, proc.view, proc.mask_epi.pv(),
             uset.pv(), gu_mean)

    cmd += "3dTstat -sos -prefix - %s\\' > %s\n"                        \
           'echo "-- GCOR = `cat %s`"\n\n'                              \
            % (gu_mean, gcor_file, gcor_file)

    return cmd

# run 3dTfitter on the xmatrix and any 4-D dataset needed in regression
def db_cmd_anaticor(proc, block, rset, select=''):
    """return a string for running 3dTfitter

       inputs: 
          volreg datasets       : to compute WMeLocal time series
          mask_WMe_resam        : same
          rset                  : BASE.afni_name for Localstat result

       Generate rm.all_runs.volreg, then WMeLocal_rall.

       return status (0=success) and command string
    """

    volreg_wild = proc.dset_form_wild('volreg')
    if not proc.roi_dict.has_key('WMe'):
       print '** ANATICOR needs WMe mask -->\n' \
             '   (options -mask_segment_anat, -mask_segment_erode)'
       return 1, ''
    mset = proc.roi_dict['WMe']
    vall = 'rm.all_runs.volreg'
    
    cmd = '# catenate volreg dsets in case of censored sub-brick selection\n' \
          '3dTcat -prefix %s %s\n\n' % (vall, volreg_wild)

    cmd += '# generate time series averaged over the closest white matter\n'
    if select and proc.censor_file:
       cmd += '# (exclude censored TRs at this point to save time)\n'

    cmd += "3dLocalstat -stat mean -nbhd 'SPHERE(45)' -prefix %s \\\n" \
           "            -mask %s -use_nonmask \\\n"                    \
           "            %s%s%s\n\n"                                    \
           % (rset.out_prefix(), mset.shortinput(), vall, proc.view, select)

    return 0, cmd

# run 3dTfitter on the xmatrix and any 4-D dataset neede in regression
def db_cmd_regress_tfitter(proc, block):
    """return a string for running 3dTfitter - generate errts dataset

       This currently does only anaticor, but will be extended for motion
       simulation (still don't know what to call that).

       if anaticor: get WMeLocal time series (censoring?)
       if other: generate

          - if censoring: set keep_trs based on proc.xmat
          - get WMeLocal TS from anaticor
          - run 3dTfitter (to create fitts)
          - run 3dcalc to (to create errts)

       return status (0=success) and command string
    """

    if not block.opts.find_opt('-regress_anaticor'): return 0, ''
    if proc.surf_anat:
       print '** -regress_anaticor: not ready for surface analysis'
       return 1, ''

    rlabel = 'anaticor'
    rset = BASE.afni_name('errts.%s.$subj'%rlabel)
    rset.view = proc.view
    lset = BASE.afni_name('WMeLocal_rall')
    lset.view = proc.view

    cmd = '# --------------------------------------------------\n' \
          '# generate ANATICOR result: %s\n\n' % rset.shortinput()
    # rcr - if motion, add a comment...

    # sub-brick selection, in case of censoring
    if proc.censor_file:
       cs = '# 3dTfitter does not take censor file, so note TRs to process\n' \
            'set keep_trs = `1d_tool.py -infile %s %s`\n\n'                   \
            % (proc.xmat, '-show_trs_uncensored encoded')
       cmd += cs
       substr = '"[$keep_trs]"'
    else: substr = ''

    rv, cs = db_cmd_anaticor(proc, block, lset, select=substr)
    if rv: return 1, ''
    cmd += cs

    if proc.mask and proc.regmask:
        maskstr = '-mask %s ' % proc.mask.shortinput()
    else: maskstr = ''

    cmd += '# use 3dTfitter to perform the original regression,\n'      \
           '# plus regress out the voxel-wise WMeLocal time series\n'

    fitts = 'fitts.%s.$subj' % rlabel
    cmd += '3dTfitter -polort -1 %s\\\n'                    \
           '          -RHS %s%s%s \\\n'                     \
           '          -LHS %s %s \\\n'                      \
           '          -prefix stats.%s.$subj -fitts %s\n\n' \
           % (maskstr, proc.all_runs, proc.view, substr,
                       proc.xmat, lset.shortinput(), rlabel, fitts)

    cmd += '# compute the final errts dataset (as all_runs - fitts)\n' \
           '3dcalc -a %s%s%s -b %s%s \\\n'      \
           '       -expr a-b -prefix %s\n\n'  \
           % (proc.all_runs, proc.view, substr, fitts, proc.view, rset.prefix)

    return 0, cmd

# compute temporal signal to noise after the regression
def db_cmd_regress_tsnr(proc, block, all_runs, errts_pre):
    if not all_runs or not errts_pre: return ''

    if proc.mask: mask_pre = proc.mask.prefix
    else:         mask_pre = ''

    return db_cmd_tsnr(proc,
           '# --------------------------------------------------\n' \
           "# create a temporal signal to noise ratio dataset \n"   \
           "#    signal: if 'scale' block, mean should be 100\n"    \
           "#    noise : compute standard deviation of errts\n",
           all_runs, errts_pre, proc.view, mask=mask_pre)

# compute temporal signal to noise after the regression
def db_cmd_tsnr(proc, comment, signal, noise, view,
                        mask='', name_qual='', detrend=0):
    """return a string for computing temporal signal to noise
         comment:   leading comment string
         signal:    prefix for mean dset
         noise:     prefix for stdev dset
         view:      dset view
         mask:      (optional) prefix for mask dset
         name_qual: (optional) qualifier for name, such as '.r01'
         detrend:   (optional) if > 0, 
    """
    if not signal or not noise or not view:
        print '** compute TSNR: missing input'
        return None

    if mask:
       cstr = '\\\n       -c %s%s ' % (mask, view)
       estr = 'c*a/b'
    else:
       cstr = ''
       estr = 'a/b'

    if proc.surf_anat:
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

    if detrend:
        polort=UTIL.get_default_polort(proc.tr, proc.reps)
        detcmd="%s3dDetrend -polort %d -prefix rm.noise.det%s " \
               "-overwrite %s%s\n"\
               % (istr, polort, suff, noise, vsuff)
        noise = 'rm.noise.det%s' % suff
    else: detcmd = ''

    if name_qual == '': suff = '.all%s' % suff
    else:               suff = name_qual + suff

    cmd  = comment + feh_str
    cmd += "%s3dTstat -mean -prefix rm.signal%s %s%s\n"           \
           "%s"                                                   \
           "%s3dTstat -stdev -prefix rm.noise%s %s%s\n"           \
           % (istr, suff, signal, vsuff, detcmd, istr, suff, noise, vsuff)

    cmd += "%s3dcalc -a rm.signal%s%s \\\n"     \
           "%s       -b rm.noise%s%s %s \\\n"   \
           "%s       -expr '%s' -prefix %s \n"  \
           % (istr, suff, vsuff,
              istr, suff, vsuff, cstr,
              istr, estr, dname)

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

    if not aopt and not eopt:
        if proc.verb > 0: print '-- no 3dClustSim (since no blur estimation)'
        return cmd

    # set the mask (if we don't have one, bail)
    if not proc.mask:
        print '** refusing to estimate blur without a mask dataset'
        print '   (masks are not applied without -regress_apply_mask)'
        return

    if proc.verb > 1: print '++ computing blur estimates'
    blur_file = 'blur_est.$subj.1D'
    mask_dset = '%s%s' % (proc.mask.prefix, proc.view)

    # call this a new sub-block
    cmd = cmd + '# %s\n'                                \
                '# compute blur estimates\n'            \
                'touch %s   # start with empty file\n\n'\
                % (block_header('blur estimation'), blur_file)

    if aopt:
        bstr = blur_est_loop_str(proc,
                    'all_runs%s$subj%s' % (proc.sep_char, proc.view), 
                    mask_dset, 'epits', blur_file)
        if not bstr: return
        cmd = cmd + bstr

    opt = block.opts.find_opt('-regress_errts_prefix')
    if opt and opt.parlist: errts_pre = opt.parlist[0]
    else:   errts_pre = 'errts.${subj}'

    if eopt and not sopt: # want errts, but 3dD was not executed
        bstr = blur_est_loop_str(proc, '%s%s' % (errts_pre, proc.view), 
                    mask_dset, 'errts', blur_file)
        if not bstr: return
        cmd = cmd + bstr
    if eopt and ropt: # want errts and reml was executed
        # cannot use ${}, so escape the '_'
        bstr = blur_est_loop_str(proc, '%s_REML%s' % (errts_pre, proc.view), 
                    mask_dset, 'err_reml', blur_file)
        if not bstr: return
        cmd = cmd + bstr
    cmd = cmd + '\n'

    # maybe make string to run and apply 3dClustSim
    opt = block.opts.find_opt('-regress_run_clustsim')
    if not opt or OL.opt_is_yes(opt):
        stats_dset = 'stats.$subj%s' % proc.view
        if block.opts.find_opt('-regress_reml_exec'):
           reml_dset = 'stats.${subj}_REML%s' % proc.view
        else: reml_dset = ''
        rv, bstr = make_clustsim_commands(proc, block, blur_file, 
                                          mask_dset, stats_dset, reml_dset)
        if rv: return   # failure (error has been printed)
        cmd = cmd + bstr + '\n'

    return cmd

def make_clustsim_commands(proc, block, blur_file, mask_dset,
                           stats_dset, reml_dset):
    if proc.verb > 0: print '-- will add 3dClustSim table to stats dset'
    if proc.verb > 1:
        print '-- make_clustsim_commands: blur = %s\n'  \
              '   mask = %s, stats = %s, reml = %s'\
              % (blur_file, mask_dset, stats_dset, reml_dset)

    # track which neighbors to go after
    nnvalid = ['1','2','3']
    nnlist  = ['1','2','3']

    opt = block.opts.find_opt('-regress_CS_NN')
    if opt and len(opt.parlist) > 0:
        # verify and separate as an array
        nnlist = []
        for nn in opt.parlist[0]:
            if nn not in nnvalid:
                print "** CS_NN value %s is not in %s" % (nn,','.join(nnvalid))
                return 1, ''
            nnlist.append(nn)

    nnlist.sort()   # just to be sure we look pretty
    nnstr = ''.join(nnlist)
    if proc.verb > 2: print "++ have CS_NN list string %s" % nnstr

    opt = block.opts.find_opt('-regress_opts_CS')
    optstr = ''
    if opt:
        if len(opt.parlist) > 0 :
           optstr = '           %s \\\n' % ' '.join(opt.parlist)

    cprefix = 'ClustSim'        # prefix for 3dClustSim files
    cstr = '# add 3dClustSim results as attributes to the stats dset\n' \
           'set fxyz = ( `tail -1 %s` )\n'                              \
           '3dClustSim -both -NN %s -mask %s \\\n'                      \
           '%s'                                                         \
           '           -fwhmxyz $fxyz[1-3] -prefix %s\n'                \
           % (blur_file, nnstr, mask_dset, optstr, cprefix)

    # start with the mask attr, add each NNx, and finally the stats input dset
    cstr += '3drefit -atrstring AFNI_CLUSTSIM_MASK file:%s.mask     \\\n' \
             % cprefix
    for nn in nnlist:
        cstr += '        -atrstring AFNI_CLUSTSIM_NN%s  file:%s.NN%s.niml \\\n'\
                % (nn, cprefix, nn)

    # finally, the input
    if reml_dset == '': rstr = ''
    else:               rstr = ' ' + reml_dset

    cstr += '        %s%s\n\n' % (stats_dset, rstr)

    return 0, cstr

def blur_est_loop_str(proc, dname, mname, label, outfile):
    """return tcsh command string to compute blur from this dset
        proc     : afni_proc SubjProcStream (for reps or reps_all)
        dname    : dataset name to estimate blur on
        mname    : mask dataset name
        label    : text label for comments
        outfile  : final output filename
    """
    dset  = BASE.afni_name(dname)
    inset = dset.shortinput()
    inset = dname
    mset  = BASE.afni_name(mname)
    mask  = mset.shortinput()
    tmpfile = 'blur.%s.1D' % label

    if not inset:
        print "** failed to get blur_est input name from '%s'" % dname
        return ''
    if not mask:
        print "** failed to get mask input name from '%s'" % mname
        return ''

    cmd = '# -- estimate blur for each run in %s --\n'          \
          'touch %s\n\n' % (label, tmpfile)

    cmd = cmd +                                                 \
        'set b0 = 0     # first index for current run\n'        \
        'set b1 = -1    # will be last index for current run\n' \
        'foreach reps ( $tr_counts )\n'                         \
        '    @ b1 += $reps  # last index for current run\n'     \
        '    3dFWHMx -detrend -mask %s \\\n'                    \
        '        %s"[$b0..$b1]" >> %s\n'                        \
        % (mask, inset, tmpfile)

    cmd = cmd +                                                 \
        '    @ b0 += $reps  # first index for next run\n'       \
        'end\n\n'

    # how to get the blurs differs if there is only 1 run
    if proc.runs > 1: blur_str = "3dTstat -mean -prefix - %s\\\'" % tmpfile
    else:             blur_str = "cat %s" % tmpfile

    cmd = cmd +                                                 \
        '# compute average blur and append\n'                   \
        'set blurs = ( `%s` )\n'                                \
        'echo average %s blurs: $blurs\n'                       \
        'echo "$blurs   # %s blur estimates" >> %s\n\n'     %   \
        (blur_str, label, label, outfile)

    return cmd

# convert a stim_files list into a stim_times list
def db_cmd_regress_sfiles2times(proc, block):

    # check for a stimulus timing offset
    opt = block.opts.find_opt('-regress_stim_times_offset')
    if opt and opt.parlist and opt.parlist[0] != 0:
        off_cmd = '                   -offset %s \\\n' % str(opt.parlist[0])
    else: off_cmd = ''

    cmd = ''
    if proc.verb > 0: print '-- old stim list: %s' % proc.stims

    cmd = cmd + '\n# create -stim_times files\n'
    cmd = cmd + 'make_stim_times.py -prefix stim_times -tr %s -nruns %d'       \
                ' -nt %d \\\n'                                                 \
                '%s'                                                           \
                '                   -files '    \
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

    if proc.verb > 0: print '++ new stim list: %s' % proc.stims

    return cmd

def db_cmd_regress_ROI(proc, block):
    """remove any regressors of no interest

        ** use orts for now, but change to simple regressors
        ** just do global signal for now

       return an error code (0=success) and command string
    """

    # maybe we shouldn't be here
    oname = '-regress_ROI'
    opt = block.opts.find_opt(oname)
    if not opt: return 0, ''
    rois = opt.parlist
    if len(rois) == 0:
       print '** have -regress_ROI but no ROIs provided'
       return 1, ''

    # report errors for any unknown ROIs (not in roi_dict)
    keystr = ', '.join(proc.roi_dict.keys())
    nerrs = 0
    segstr = 'requires -mask_segment_anat'
    erdstr = 'requires -mask_segment_anat and -mask_segment_erode'
    for roi in rois:
        if not proc.roi_dict.has_key(roi):
            if   roi == 'brain': estr='EPI automask requires mask block'
            elif roi == 'GM'   : estr='gray matter %s'  % segstr
            elif roi == 'WM'   : estr='white matter %s' % segstr
            elif roi == 'CSF'  : estr='CSF %s'          % segstr
            elif roi == 'GMe'  : estr='eroded gray %s'  % erdstr
            elif roi == 'WMe'  : estr='white matter %s' % erdstr
            elif roi == 'CSFe' : estr='CSF %s'          % erdstr
            else               : estr = 'not a known ROI'
            print "** ROI '%s' : %s" % (roi, estr)
            nerrs += 1
    if nerrs:
        if keystr == '': keystr = 'NONE'
        print '-- currently known ROIs include: %s' % keystr
        return 1, ''

    if len(rois) > 1:
          cmd = '# create %d ROI regressors: %s\n' % (len(rois),', '.join(rois))
    else: cmd = '# create ROI regressor: %s\n' % rois[0]

    cmd += 'foreach run ( $runs )\n'
    cmd += '    # get each ROI average time series and remove resulting mean\n'
    for roi in rois:
        mset = proc.roi_dict[roi]
        # -- no more label table, masks are now unit            22 Apr 2013
        # maybe we need a label table value selector
        # if roi in ['GM', 'WM', 'CSF']: substr = '"<%s>"' % roi
        # else:                          substr = ''
        ofile = 'rm.ROI.%s.r$run.1D' % roi
        cmd += '    3dmaskave -quiet -mask %s %s%s \\\n'                   \
               '              | 1d_tool.py -infile - -demean -write %s \n' \
               % (mset.pv(), proc.volreg_prefix, proc.view, ofile)
    cmd += 'end\n'

    cmd += '# and catenate the demeaned ROI averages across runs\n'
    for roi in rois:
        rname = 'ROI.%s' % roi
        rfile = '%s_rall.1D' % rname
        cmd += 'cat rm.%s.r*.1D > %s\n' % (rname, rfile)
        proc.regress_orts.append([rfile, rname])
    cmd += '\n'

    print '++ have %d ROIs to regress: %s' % (len(rois), ', '.join(rois))

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

    freq, err = block.opts.get_type_list(float, opt=opt)
    if len(freq) != 2:
        print '** %s requires 2 parameters, low and high frequencies' % oname
        return 1, ''
    if freq[0] >= freq[1]:
        print '** %s: must have low freq < high freq' % oname
        return 1, ''

    bfile = 'bandpass_rall.1D'
    tfile = 'rm.bpass.1D'

    cmd = '# create bandpass regressors (instead of using 3dBandpass, say)\n'
    if proc.runs != 1:
        cmd += '# (make separate regressors per run, with all in one file)\n'

    if proc.runs == 1: # simple case
        cmd += '1dBport -nodata %s %s -band %g %g -invert -nozero' \
               ' > %s\n\n' % (proc.reps, proc.tr, freq[0], freq[1], bfile)
    else: # loop over 1dBport and 1d_tool.py
        cmd += 'foreach index ( `count -digits 1 1 $#runs` )\n'               \
               '    set nt = $tr_counts[$index]\n'                            \
               '    set run = $runs[$index]\n'                                \
               '    1dBport -nodata $nt %g -band %g %g -invert -nozero > %s\n'\
               % (proc.tr, freq[0], freq[1], tfile)
        cmd += '    1d_tool.py -infile %s -pad_into_many_runs $run $#runs \\\n'\
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
        for mfile in mfiles:
            if   mfile == proc.mot_extern: mtype = 'extern'
            elif mfile == proc.mot_demean: mtype = 'demean'
            elif mfile == proc.mot_deriv : mtype = 'deriv'
            else:                          mtype = 'dfile'
            # note the output prefix and expected output file list
            mprefix = 'mot_%s' % mtype
            outfiles = ['%s.r%02d.1D'%(mprefix,r+1) for r in range(proc.runs)]
            pcmd += '1d_tool.py -infile %s %s \\\n'          \
                    '           -split_into_pad_runs %s\n\n' \
                    % (mfile, runopt, mprefix)
            proc.mot_regs.extend(outfiles)

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
          print "** must allow 'demean' and 'deriv' computation when using\n" \
                "   option -regress_apply_mot_types"
          return 1, ''
       print '-- will apply motion types: %s' % ', '.join(apply_types)
    elif block.opts.find_opt('-regress_no_motion_demean'):
          apply_types = ['basic']
    else: apply_types = ['demean'] # now the default

    # note whether to use run lengths or number of runs for demean and deriv
    if proc.reps_vary :
       ropt = '-set_run_lengths %s' % UTIL.int_list_string(proc.reps_all)
    else: ropt = '-set_nruns %d' % proc.runs

    # handle 3 cases of motion parameters: 'basic', 'demean' and 'deriv'

    # 1. update mot_regs for 'basic' case
    if 'basic' in apply_types: proc.mot_regs.append(proc.mot_file)

    # 2. possibly compute de-meaned motion params
    if not block.opts.find_opt('-regress_no_motion_demean'):
       mtype = 'demean'
       mopt = 'demean'
       motfile = 'motion_%s.1D' % mtype
       # if requested for regression, add this file
       if mtype in apply_types:
          if 'basic' in apply_types:
             print "** cannot apply both 'basic' and 'demean' motion params"
             print "   (would lead to multi-collinearity)"
             return 1, ''
          proc.mot_regs.append(motfile)
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
            print '** regress_censor_motion: have negative limit %g' % limit
            return 1, ''
    else: return 0, ''

    # run lengths may now vary  16 Nov, 2009

    # maybe there is no file to use
    if proc.mot_file == '': return 0, ''

    # check for -regress_censor_first_trs
    val, err = block.opts.get_type_opt(int, '-regress_censor_first_trs')
    if err:
        print '** -regress_censor_first_trs requires integer argument'
        return 1, ''
    elif val != None and val > 0: cfstr = '    -censor_first_trs %d \\\n' % val
    else:                         cfstr = ''

    # check for censor_prev_TR
    opt = block.opts.find_opt('-regress_censor_prev')
    if opt.parlist[0] == 'yes': prev_str = '-censor_prev_TR'
    else:                       prev_str = ''

    if proc.verb > 1:
        print '-- creating motion censor command, file = %s' % proc.mot_file

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
        cmd = cmd + '1d_tool.py -infile %s -set_run_lengths %s \\\n' \
                    % (proc.mot_file, UTIL.int_list_string(proc.reps_all))
    else:
        cmd = cmd + '1d_tool.py -infile %s -set_nruns %d \\\n'       \
                    % (proc.mot_file, proc.runs)

    # remove useless -set_tr option  17 Oct 2012
    cmd = cmd + '    -show_censor_count %s\\\n'                 \
                '%s'                                            \
                '    -censor_motion %g %s\n\n'                  \
                % (prev_str, cfstr, limit, mot_prefix)

    proc.mot_cen_lim = limit

    if cfs: cmd += cfs

    return 0, cmd

# --------------- tlrc (anat) ---------------

def db_mod_tlrc(block, proc, user_opts):
    if len(block.opts.olist) == 0:      # then init to defaults
        block.opts.add_opt('-tlrc_base', 1, ['TT_N27+tlrc'], setpar=1)
        block.opts.add_opt('-tlrc_suffix', 1, ['NONE'], setpar=1)

    # verify that anatomical dataset exists
    opt_anat = user_opts.find_opt('-copy_anat')
    if not opt_anat:
        print '** tlrc (anat) block requires anatomy via -copy_anat'
        return

    dset = BASE.afni_name(opt_anat.parlist[0])
    if not dset.exist():  # allow for no +view
        dset = BASE.afni_name(opt_anat.parlist[0]+'+orig')
        if not dset.exist():
            print "** -tlrc_anat dataset '%s' does not exist" % \
                  opt_anat.parlist[0]
            return

    # add other options

    uopt = user_opts.find_opt('-tlrc_base')
    if uopt:
        bopt = block.opts.find_opt('-tlrc_base')
        bopt.parlist = uopt.parlist

    uopt = user_opts.find_opt('-tlrc_opts_at')
    bopt = block.opts.find_opt('-tlrc_opts_at')
    if uopt :
        if bopt: bopt.parlist = uopt.parlist
        else:    block.opts.add_opt('-tlrc_opts_at', 0, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-tlrc_no_ss')
    bopt = block.opts.find_opt('-tlrc_no_ss')
    if uopt and not bopt:
        block.opts.add_opt('-tlrc_no_ss', 0, [])

    uopt = user_opts.find_opt('-tlrc_rmode')
    if uopt:
        bopt = block.opts.find_opt('-tlrc_rmode')
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-tlrc_rmode', 1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-tlrc_suffix')
    if uopt:
        bopt = block.opts.find_opt('-tlrc_suffix')
        bopt.parlist = uopt.parlist

    block.valid = 1

# create a command to run @auto_tlrc
def db_cmd_tlrc(proc, block):
    """warp proc.anat to standard space"""

    block.index = proc.bindex   # save

    dname = proc.anat.pv()
    if not dname :
        print "** missing dataset name for tlrc operation"
        return None

    # no longer look to add +orig

    opt = block.opts.find_opt('-tlrc_base')
    if opt: base = opt.parlist[0]
    else:   base = 'TT_N27+tlrc'

    proc.tlrc_base = BASE.afni_name(base)       # store for later

    # add any user-specified options
    opt = block.opts.find_opt('-tlrc_opts_at')
    if opt and opt.parlist: extra_opts = " \\\n           %s" % \
                            ' '.join(UTIL.quotize_list(opt.parlist, '', 1))
    else:   extra_opts = ''

    opt = block.opts.find_opt('-tlrc_no_ss')
    if opt or not proc.anat_has_skull or not proc.tlrc_ss: ss = ' -no_ss'
    else:                                                  ss = ''

    opt = block.opts.find_opt('-tlrc_rmode')
    if opt: rmode = ' -rmode %s' % opt.parlist[0]
    else:   rmode = ''

    # note any suffix for the tlrcanat dataset
    suf = ''
    opt = block.opts.find_opt('-tlrc_suffix')
    if opt:
        suffix = ' -suffix %s' % opt.parlist[0]
        suf = opt.parlist[0]
    else: suffix = ' -suffix NONE'     # make NONE the default
    if suf == 'NONE': suf = ''         # clear if we had the default option

    # store what we expect for output
    proc.tlrcanat = proc.anat.new(proc.anat.prefix+suf, '+tlrc')

    # start with block separator
    cmd = "# %s\n" % block_header('tlrc')

    cmd += "# warp anatomy to standard space\n"         \
           "@auto_tlrc -base %s -input %s%s%s%s"        \
           "%s"                                         \
           "\n\n"                                       \
           % (base, dname, ss, rmode, suffix, extra_opts)

    return cmd

def married_types_match(proc, stims, stypes, bases):
    """if any stim files are married, the stype should be AM1 or AM2
       some basis functions require married files
    """

    if len(stims) == 0: return 1
    if not proc.test_stims: return 1
    
    if proc.verb > 2:
        print '-- checking married type match for:'
        print '   stims : %s' % stims
        print '   types : %s' % stypes

    ok_all = 1  # assume good, and look for failure
    mtypes = ['AM1', 'AM2']

    for ind in range(len(stims)):
        fname = stims[ind]
        stype = stypes[ind]
        basis = bases[ind]

        if stype == 'file': continue

        adata = LD.AfniData(fname)
        if adata == None:
            print "** MTM: failed to load stim timing file '%s'" % fname
            ok_all = 0
            continue
        if not adata.ready:
            print "** MTM: failed to load stimulus timing file '%s'" % fname

        if stype in mtypes and not adata.married:
            print '** stim type %d is married (%s), but file (%s) is not' \
                  % (ind+1, stype, fname)
            ok_all = 0

        # just a warning for now, since stim_types are new

        if stype not in mtypes and adata.married:
            print '** stim type %d is not married, but file (%s) is' \
                  % (ind+1, fname)

        if UTIL.basis_is_married(basis) and not stype in mtypes:
            print '** have married basis (#%d = %s), but not married type\n' \
                  '   (consider -regress_stim_types)' % (ind+1,basis)

        if UTIL.basis_is_married(basis) and not adata.married:
            print '** have married basis (#%d = %s), but not married file %s\n'\
                  % (ind+1, basis, fname)

    if not ok_all and proc.verb > 0:
        print '   (use "-test_stim_files no" to ignore such errors)'

    return ok_all

def valid_file_types(proc, stims, file_type, stypes=[]):
    """verify that the files are valid as 1D, local or global times

         1 : 1D
         2 : local
         3 : global
        10 : local or global

       If invalid, print messages.
       If valid, print any warning messages.
       So checks are always run twice.

       return 1 if valid, 0 otherwise
    """

    if len(stims) == 0: return 1

    if not proc.test_stims: return 1

    if proc.verb > 2: print '-- check files of type %d: %s' % (file_type, stims)

    ok_all = 1  # assume good, and look for failure

    for findex in range(len(stims)):
        fname = stims[findex]
        ftype = file_type
        if stypes and stypes[findex] == 'file': ftype = 1

        adata = LD.AfniData(fname, verb=proc.verb)
        if adata == None:
            print "** failed to load stim timing file '%s'" % fname
            ok_all = 0
            continue
        if not adata.ready:
            print "** failed to load stimulus timing file '%s'" % fname
            ok_all = 0
            continue

        if adata.married and proc.verb > 1:
            print '-- have married file %s' % fname

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
            print '** valid_file_types: bad type %d' % ftype
            return 0

        # if empty, warn user (3dD will fail)
        if ok and adata.empty:
            print '** empty stim file %s (consider 3dD -GOFORIT ...)\n' % fname

        # if current file is good, move on
        if ok: continue

        if ok_all: # first time: surround errors with dashed lines
           print '------------------------------------------------------------'

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
        print '------------------------------------------------------------'

    if not ok_all:
        print "-- consider use of '-test_stim_files no' if files are OK"
        print '------------------------------------------------------------'

    return ok_all

# currently nothing to verify for an 'empty' command (placeholder command)
# just return 1
def db_mod_empty(block, proc, user_opts):
    block.valid = 1
    return 1

# create a placeholder command using 3dTcat to copy the EPI data
def db_cmd_empty(proc, block):
    block.index = proc.bindex   # save

    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(view=1)

    cmd = "# %s\n"                                                      \
          "# empty block: use '3dTcat' as a placeholder command\n"      \
          "foreach run ( $runs )\n"                                     \
          "    3dTcat -prefix %s %s\n"                                  \
          "end\n\n" % (block_header('empty'), prefix, prev)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

# create a gen_epi_review.py command
def db_cmd_gen_review(proc):
    if not proc.epi_review: return None

    tblk = proc.find_block('tcat')

    # get dataset names, but be sure not to get the surface form
    dstr = proc.dset_form_wild('tcat', proc.origview, surf_names=0)
    cmd = "# %s\n\n"                                                    \
          "# generate a review script for the unprocessed EPI data\n"   \
          "gen_epi_review.py -script %s \\\n"                           \
          "    -dsets %s\n\n"                                           \
          % (block_header('auto block: generate review scripts'),
             proc.epi_review, dstr)

    lopts = ' '
    if proc.mot_cen_lim > 0.0: lopts += '-mot_limit %s ' % proc.mot_cen_lim
    if proc.out_cen_lim > 0.0: lopts += '-out_limit %s ' % proc.out_cen_lim
    if proc.mot_extern != '' : lopts += '-motion_dset %s ' % proc.mot_file
        
    cmd += '# generate scripts to review single subject results\n'      \
           '# (try with defaults, but do not allow bad exit status)\n'  \
           'gen_ss_review_scripts.py%s-exit0\n\n' % lopts

    return cmd

def block_header(hname, maxlen=74, hchar='=', endchar=''):
    """return a title string of 'hchar's with the middle chars set to 'name'
       if endchar is set, put at both ends of header
       e.g. block_header('volreg', endchar='##') """
    if len(hname) > 0: name = ' %s ' % hname
    else:              name = ''

    if endchar != '': maxlen -= 2*len(endchar)
    rmlen = len(name)
    if rmlen >= maxlen:
        print "** block_header, rmlen=%d exceeds maxlen=%d" % (rmlen, maxlen)
        return name
    prelen  = (maxlen - rmlen) // 2     # basically half the chars
    postlen = maxlen - rmlen - prelen   # other 'half'

    return endchar + prelen*hchar + name + postlen*hchar + endchar

# ----------------------------------------------------------------------
# global help string (see end global help string)
# -- this is long, get it out of the main library

g_help_string = """
    ===========================================================================
    afni_proc.py        - generate a tcsh script for an AFNI process stream

    Purpose:

       This program is meant to create single subject processing scripts for
       task, resting state or surface-based analyses.  The processing scripts
       are written in the tcsh language.

       The typical goal is to create volumes of aligned resopnse magnitudes
       (stimulus beta weights) to use as input for a group analysis.

    Inputs (only EPI is required):

       - anatomical dataset
       - EPI time series datasets
       - stimulus timing files
       - processing and design decisions:
           e.g. TRs to delete, blur size, censoring options, basis functions

    Main outputs (many datasets are created):

       - for task-based analysis: stats dataset (and anat_final)
       - for resting-state analysis: errts datasets ("cleaned up" EPI)

    Basic script outline:

       - copy all inputs to new 'results' directory
       - process data: e.g. despike,tshift/align/tlrc/volreg/blur/scale/regress
       - leave all (well, most) results there, so user can review processing
       - create @ss_review scripts to help user with basic quality control

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
    SECTIONS: order of sections in the "afni_proc.py -help" output

        program introduction    : (above) basic overview of afni_proc.py
        PROCESSING BLOCKS       : list of possible processing blocks
        DEFAULTS                : basic default operations, per block
        EXAMPLES                : various examples of running this program
        NOTE sections           : details on various topics
            RESTING STATE NOTE, TIMING FILE NOTE, MASKING NOTE,
            ANAT/EPI ALIGNMENT CASES NOTE, ANAT/EPI ALIGNMENT CORRECTIONS NOTE,
            WARP TO TLRC NOTE, RETROICOR NOTE, RUNS OF DIFFERENT LENGTHS NOTE,
            SCRIPT EXECUTION NOTE
        OPTIONS                 : desriptions of all program options
            informational       : options to get quick info and quit
            general execution   : options not specific to a processing block
            block options       : specific to blocks, in default block order

    ==================================================
    PROCESSING BLOCKS (of the output script):

    The output script will go through the following steps, unless the user
    specifies otherwise.

    automatic blocks (the tcsh script will always perform these):

        setup       : check subject arg, set run list, create output dir, and
                      copy stim files
        tcat        : copy input datasets and remove unwanted initial TRs

    default blocks (the user may skip these, or alter their order):

        tshift      : slice timing alignment on volumes (default is -time 0)
        volreg      : volume registration (default to third volume)
        blur        : blur each volume (default is 4mm fwhm)
        mask        : create a 'brain' mask from the EPI data (dilate 1 voxel)
        scale       : scale each run mean to 100, for each voxel (max of 200)
        regress     : regression analysis (default is GAM, peak 1, with motion
                      params)

    optional blocks (the default is to _not_ apply these blocks)

        align       : align EPI anat anatomy (via align_epi_anat.py)
        despike     : truncate spikes in each voxel's time series
        empty       : placeholder for some user command (uses 3dTcat as sample)
        ricor       : RETROICOR - removal of cardiac/respiratory regressors
        tlrc        : warp anat to standard space

    ==================================================
    DEFAULTS: basic defaults for each block (blocks listed in default order)

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

    D   volreg:   - align to third volume of first run, -zpad 1
                        (option: -volreg_align_to third)
                        (option: -volreg_zpad 1)
                  - use cubic interpolation for volume resampling
                        (option: -volreg_interp -cubic)
                  - apply motion params as regressors across all runs at once
                  - do not align EPI to anat
                  - do not warp to standard space

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

    ==================================================
    EXAMPLES (options can be provided in any order):

        1. Minimum use.

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

        2. Very simple.  Use all defaults, except remove 3 TRs and use basis
           function BLOCK(30,1).  The default basis function is GAM.

                afni_proc.py -subj_id sb23.e2.simple                       \\
                        -dsets sb23/epi_r??+orig.HEAD                      \\
                        -tcat_remove_first_trs 3                           \\
                        -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                        -regress_basis 'BLOCK(30,1)'

        3. The current class example.  This may change of course.

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

        4. Similar to the class example, but specify the processing blocks,
           adding despike and tlrc, and removing tshift.  Note that the tlrc
           block is to run @auto_tlrc on the anat.  Ignore the GLTs.

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

        5a. RETROICOR example a, resting state data.

           Assuming the class data is for resting-state and that we have the
           appropriate slice-based regressors from RetroTS.m, apply the despike
           and ricor processing blocks.  Note that '-do_block' is used to add
           non-default blocks into their default positions.  Here the 'despike'
           and 'ricor' processing blocks would come before 'tshift'.

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

        5b. RETROICOR example b, while running a normal regression.

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

        5c. RETROICOR example c (modern): censoring and bandpass filtering.

           This is an example of how we might currently suggest analyzing
           resting state data.  If no RICOR regressors exist, see example 9
           (or just remove any ricor options).

           Censoring due to motion has long been considered appropriate in
           BOLD FMRI analysis, but is less common for those doing bandpass
           filtering in RC FMRI because the FFT requires one to either break
           the time axis (evil) or to replace the censored data with something
           probably inapproprate.

           Instead, it is slow (no FFT, but maybe SFT :) but effective to
           regress frequencies within the regression model, where censored is
           simple.

           Note: bandpassing in the face of RETROICOR processing is questionable.
                 There is no strong opinion on it (at least within our group).
                 To skip bandpassing, remove the -regress_bandpass option line.

           Also, align EPI to anat and warp to standard space.

                afni_proc.py -subj_id sb23.e5a.ricor            \\
                        -dsets sb23/epi_r??+orig.HEAD           \\
                        -blocks despike ricor tshift align tlrc \\
                                volreg blur mask regress        \\
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
                        -regress_est_blur_errts
                        
        6. A modern example.  GOOD TO CONSIDER.

           Align the EPI to the anatomy.  Also, process in standard space.

           For alignment in either direction, add the 'align' block, which
           aligns the anatomy to the EPI.  To then align the EPI to the anat,
           apply -volreg_align_e2a, where that transform (inverse) is applied
           along with the motion alignment.

           On top of that, complete the processing in standard space by running
           @auto_tlrc on the anat (via the 'tlrc' block) and applying the same
           transformation to the EPI via -volreg_tlrc_warp.  Again, the EPI
           transformation is applied along with the motion alignment.

           So add the 2 processing blocks and 2 extra volreg warps to #3 via
           '-do_block align tlrc', '-volreg_align_e2a', '-volreg_tlrc_warp'.

           As an added bonus, censor TR pairs where the Euclidean Norm of the
           motion derivative exceeds 1.0.

                afni_proc.py -subj_id sb23.e6.align                        \\
                        -dsets sb23/epi_r??+orig.HEAD                      \\
                        -do_block align tlrc                               \\
                        -copy_anat sb23/sb23_mpra+orig                     \\
                        -tcat_remove_first_trs 3                           \\
                        -volreg_align_to last                              \\
                        -volreg_align_e2a                                  \\
                        -volreg_tlrc_warp                                  \\
                        -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                        -regress_stim_labels tneg tpos tneu eneg epos      \\
                                             eneu fneg fpos fneu           \\
                        -regress_basis 'BLOCK(30,1)'                       \\
                        -regress_censor_motion 0.3                         \\
                        -regress_opts_3dD                                  \\
                            -gltsym 'SYM: +eneg -fneg'                     \\
                            -glt_label 1 eneg_vs_fneg                      \\
                        -regress_est_blur_epits                            \\
                        -regress_est_blur_errts

           To process in orig space, remove -volreg_tlrc_warp.
           To apply manual tlrc transformation, use -volreg_tlrc_adwarp.
           To process as anat aligned to EPI, remove -volreg_align_e2a.

        7. Similar to 6, but get a little more esoteric.

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
                        -do_block align tlrc                               \\
                        -copy_anat sb23/sb23_mpra+orig                     \\
                        -tcat_remove_first_trs 3                           \\
                        -volreg_align_to last                              \\
                        -volreg_align_e2a                                  \\
                        -volreg_tlrc_warp                                  \\
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

        8. Based on subject FT under AFNI_data6.

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
                -surf_anat      : volumed aligned with surface
                -surf_spec      : spec file(s) for surface

           This example is intended to be run from AFNI_data6/FT_analysis.
           It is provided with the class data in file s03.ap.surface.

                afni_proc.py -subj_id FT.surf                            \\
                    -blocks tshift align volreg surf blur scale regress  \\
                    -copy_anat FT/FT_anat+orig                           \\
                    -dsets FT/FT_epi_r?+orig.HEAD                        \\
                    -surf_anat FT/SUMA/FTmb_SurfVol+orig                 \\
                    -surf_spec FT/SUMA/FTmb_?h.spec                      \\
                    -tcat_remove_first_trs 2                             \\
                    -volreg_align_to third                               \\
                    -volreg_align_e2a                                    \\
                    -blur_size 6                                         \\
                    -regress_stim_times FT/AV1_vis.txt FT/AV2_aud.txt    \\
                    -regress_stim_labels vis aud                         \\
                    -regress_basis 'BLOCK(20,1)'                         \\
                    -regress_censor_motion 0.3                           \\
                    -regress_opts_3dD                                    \\
                        -jobs 2                                          \\
                        -gltsym 'SYM: vis -aud' -glt_label 1 V-A

        9. Resting state analysis (modern): censoring and bandpass filtering.

           This is our suggested way to do pre-processing for resting state
           analysis, under the assumption that no cardio/physio recordings
           were made (see example 5 for cardio files).

           Censoring due to motion has long been considered appropriate in
           BOLD FMRI analysis, but is less common for those doing bandpass
           filtering in RC FMRI because the FFT requires one to either break
           the time axis (evil) or to replace the censored data with something
           probably inapproprate.

           Instead, it is slow (no FFT, but maybe SFT :) but effective to
           regress frequencies within the regression model, where censored is
           simple.

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
                 example, and censoring of outliers has also been added.

                 Outliers are typically due to motion, and may capture motion
                 in some cases where the motion parameters do not, because
                 motion is not generally a whole-brain-between-TRs event.

           Note: if regressing out regions of interest, either create the ROI
                 time series before the blur step, or remove blur from the list
                 of blocks (and apply any desired blur after the regression).

                afni_proc.py -subj_id subj123                                \\
                  -dsets epi_run1+orig.HEAD                                  \\
                  -copy_anat anat+orig                                       \\
                  -blocks despike tshift align tlrc volreg blur mask regress \\
                  -tcat_remove_first_trs 3                                   \\
                  -volreg_align_e2a                                          \\
                  -volreg_tlrc_warp                                          \\
                  -regress_censor_motion 0.2                                 \\
                  -regress_censor_outliers 0.1                               \\
                  -regress_bandpass 0.01 0.1                                 \\
                  -regress_apply_mot_types demean deriv                      \\
                  -regress_run_clustsim no                                   \\
                  -regress_est_blur_errts

       9b. Resting state analysis with ANATICOR.

           Like example #9, but also regress out the signal from locally
           averaged white matter.  The only change is adding the option
           -regress_anaticor.

           Note that -regress_anaticor implies options -mask_segment_anat and
           -mask_segment_erode.

                afni_proc.py -subj_id subj123                                \\
                  -dsets epi_run1+orig.HEAD                                  \\
                  -copy_anat anat+orig                                       \\
                  -blocks despike tshift align tlrc volreg blur mask regress \\
                  -tcat_remove_first_trs 3                                   \\
                  -volreg_align_e2a                                          \\
                  -volreg_tlrc_warp                                          \\
                  -regress_anaticor                                          \\
                  -regress_censor_motion 0.2                                 \\
                  -regress_censor_outliers 0.1                               \\
                  -regress_bandpass 0.01 0.1                                 \\
                  -regress_apply_mot_types demean deriv                      \\
                  -regress_run_clustsim no                                   \\
                  -regress_est_blur_errts


       10. Resting state analysis, with tissue-based regressors.

           Like example #9, but also regress eroded white matter and CSF
           averages.  The WMe and CSFe signals come from the Classes dataset,
           created by 3dSeg via the -mask_segment_anat option.

                afni_proc.py -subj_id subj123                               \\
                        -dsets epi_run1+orig.HEAD                           \\
                        -copy_anat anat+orig                                \\
                        -blocks despike align tlrc volreg blur mask regress \\
                        -tcat_remove_first_trs 3                            \\
                        -volreg_align_e2a                                   \\
                        -volreg_tlrc_warp                                   \\
                        -mask_segment_anat yes                              \\
                        -regress_censor_motion 0.2                          \\
                        -regress_censor_outliers 0.1                        \\
                        -regress_bandpass 0.01 0.1                          \\
                        -regress_apply_mot_types demean deriv               \\
                        -regress_ROI WMe CSFe                               \\
                        -regress_run_clustsim no                            \\
                        -regress_est_blur_errts


    --------------------------------------------------
    -ask_me EXAMPLES:

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

    ==================================================
    Many NOTE sections:
    ==================================================

    --------------------------------------------------
    RESTING STATE NOTE:

    Resting state data should be processed with physio recordings (for typical
    single-echo EPI data).  Without such recordings, bandpassing is currently
    considered as the default.

    Comment on bandpassing:

        Bandpassing is the norm right now.  However most TRs may be too long
        for this process to be able to remove the desired components of no
        interest.  Perhaps bandpassing will eventually go away.  But it is the
        norm right now.

        Also, there is a danger with bandpassing and censoring in that subjects
        with a lot of motion may run out of degrees of freedom (for baseline,
        censoring, bandpassing and removal of other signals of no interest).
        Many papers have been published where a lot of censoring was done,
        followed up by bandpassing.  It is likely that many subjects ended up
        with negative degrees of freedom, making the resulting signals useless
        (or worse, misleading garbage).  But without keeping track of it,
        researchers may not even know.

        In afni_proc.py, this is all done in a single regression model (removal
        of noise and baseline signals, bandpassing and censoring).  If some
        subject were to lose too many TRs due to censoring, this step would
        fail, as it should.

    There are 3 main steps (generate ricor regs, pre-process, group analysis):

        step 0: If physio recordings were made, generate slice-based regressors
                using RetroTS.  Such regressors can be used by afni_proc.py via
                the 'ricor' processing block.

                RetroTS is Ziad Saad's MATLAB routine to convert the 2 time 
                series into 13 slice-based regressors.  RetroTS requires the
                signal processing toolkit for MATLAB.

        step 1: analyze with afni_proc.py

                Consider these afni_proc.py -help examples:
                   5b. case of ricor and no bandpassing
                   5c. ricor and bandpassing and full registration
                   9.  no ricor, but with bandpassing
                   10. also with tissue-based regressors
                   soon: with WMeLocal (local white-matter, eroded) - ANATICOR
                         extra motion regs via motion simulated time series
                         (either locally or not)

            processing blocks:

                despike (shrink large spikes in time series)
                ricor   (if applicable, remove the RetroTS regressors)
                tshift  (correct for slice timing)
                volreg  (align anat and EPI together, and to standard template)
                blur    (apply desired FWHM blur to EPI data)
                regress (polort, motion, mot deriv, bandpass, censor)
                        (depending on chosen options)
                        soon: ANATICOR/WMeLocal
                              extra motion regressors (via motion simulation)

                ==> "result" is errts dataset, "cleaned" of known noise sources

        step 2: correlation analysis, hopefully with 3dGroupInCorr

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

                   (afni GUI, 3dclust, or 3dmerge)
            
    --------------------------------------------------
    TIMING FILE NOTE:

    One issue that the user must be sure of is the timing of the stimulus
    files (whether -regress_stim_files or -regress_stim_times is used).

    The 'tcat' step will remove the number of pre-steady-state TRs that the
    user specifies (defaulting to 0).  The stimulus files, provided by the
    user, must match datasets that have had such TRs removed (i.e. the stim
    files should start _after_ steady state has been reached).

    --------------------------------------------------
    MASKING NOTE:

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

   --> To deal with this, a time series of all 1s is created on the original
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

 ** Why has the default been changed?

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

 ** Note that the EPI data can now be warped to standard space at the volreg
    step.  In that case, it might be appropriate to mask the EPI data based
    on the Talairach template, such as what is used for -base in @auto_tlrc.
    This can be done via '-mask_apply group'.

    ---

 ** For those who have processed some of their data with the older method:

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
    ANAT/EPI ALIGNMENT CASES NOTE:

    This outlines the effects of alignment options, to help decide what options
    seem appropriate for various cases.

    1. EPI to EPI alignment (the volreg block)

        Alignment of the EPI data to a single volume is based on the 3 options
        -volreg_align_to, -volreg_base_dset and -volreg_base_ind, where the
        first option is by far the most commonly used.

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
        (such as a pre-steady state volume that will be excluded by the option
        -tcat_remove_first_trs), use -volreg_base_dset.

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
    ANAT/EPI ALIGNMENT CORRECTIONS NOTE:

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
         -align_opts_aea -cost lpc+ZZ -giant_move -resample off

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
    WARP TO TLRC NOTE:

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
            principle components) from all the ROIs in a dataset and apply
            them as regressors of interest or of no interest.

        - with 3dBlurToFWHM, using an AlphaSim look-up table might be possible

            Since the blur and data grid could both be isotropic and integral,
            and since the transformation could depend on a known anatomy (such
            as the N27 Colin brain or icbm_452), it would be easy to create a
            look-up table of AlphaSim results (so users would not actually need
            to run it).

            The known numbers would correspond to a cluster size (each for a
            given, common voxel-wise threshold).  This correction could then
            be applied automatically.  Again, not yet implemented...

        - no interpolation of statistics

            If the user wishes to include statistics as part of the group
            analysis (e.g. using 3dMEMA.R), this warping becomes more needed.
            Warping to standard space *after* statistics are generated is not
            terribly valid.

    --------------------------------------------------
    RETROICOR NOTE:

    ** Cardiac and respiratory regressors must be created from an external
       source, such as the RetroTS.m matlab program written by Z Saad.  The
       input to that would be the 2+ signals.  The output would be a single
       file per run, containing 13 or more regressors for each slice.  That
       set of output files would be applied here in afni_proc.py.

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
    RUNS OF DIFFERENT LENGTHS NOTE:

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

        -regress_censor_motion  OK, as of version 2.14

     * probably will be fixed (please let me know of interest)

    --------------------------------------------------
    SCRIPT EXECUTION NOTE:

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

                -x : echo commands to screen before executing them
                -e : exit (terminate) the processing on any errors
                -f : do not process user's ~/.cshrc file

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

    ==================================================
    OPTIONS: (information options, general options, block options)
             (block options are ordered by block)

        ------------ informational/terminal options ------------

        -help                   : show this help
        -hist                   : show the module history

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

            See also '-check_afni_version'.

        -show_valid_opts        : show all valid options (brief format)
        -ver                    : show the version number

        ------------ general execution and setup options ------------

        -anat_has_skull yes/no  : specify whether the anatomy has a skull

                e.g. -anat_has_skull no

            Use this option to block any skull-stripping operations, likely either
            in the align or tlrc processing blocks.

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

        -execute                : execute the created processing script

            If this option is applied, not only will the processing script be
            created, but it will then be executed in the "suggested" manner,
            such as via:

                tcsh -xef proc.sb23 |& tee output.proc.sb23

            Note that it will actually use the bash format of the command,
            since the system command (C and therefore python) uses /bin/sh.

                tcsh -xef proc.sb23 2>&1 | tee output.proc.sb23

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

        -keep_rm_files          : do not have script delete rm.* files at end

                e.g. -keep_rm_files

            The output script may generate temporary files in a block, which
            would be given names with prefix 'rm.'.  By default, those files
            are deleted at the end of the script.  This option blocks that
            deletion.

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
                default: same degree that 3dDeconvolve would use: 1+time/150

            Outlier counts come after detrending the data, where the degree
            of the polynomial trend defaults to the same that 3dDeconvolve
            would use.  This option will override the default.

            See "3dToutcount -help" for more details.
            See "3dDeconvolve -help" for more details.
            See also '-regress_polort' and '-outlier_legendre'.

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

            See also -write_3dD_prefix.

        ------------ block options (in default block order) ------------

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
                default: 0

            Since it takes several seconds for the magnetization to reach a
            steady state (at the beginning of each run), the initial TRs of
            each run may have values that are significantly greater than the
            later ones.  This option is used to specify how many TRs to
            remove from the beginning of every run.

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

            This format is based on the output of RetroTS.m, included in the
            AFNI distribution (as part of the matlab package), by Z Saad.

        -ricor_regs_nfirst NFIRST       : ignore the first regressor timepoints

                e.g. -ricor_regs_nfirst 2
                default: 0

            This option is similar to -tcat_remove_first_trs.  It is used to
            remove the first few TRs from the -ricor_regs regressor files.

            Since it is likely that the number of TRs in the ricor regressor
            files matches the number of TRs in the original input dataset (via
            -dsets), it is likely that -ricor_regs_nfirst should match
            -tcat_remove_first_trs.

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
            @auto_tlrc.  Otherwise, TT_N27+tlrc will be used.

            Note that the default operation of @auto_tlrc is to "skull strip"
            the input dataset.  If this is not appropriate, consider also the
            '-tlrc_no_ss' option.

            Please see '@auto_tlrc -help' for more information.
            See also -tlrc_anat, -tlrc_no_ss.

        -tlrc_opts_at OPTS ...   : add additional options to @auto_tlrc

                e.g. -tlrc_opts_at -OK_maxite

            This option is used to add user-specified options to @auto_tlrc,
            specifically those afni_proc.py is not otherwise set to handle.

            Please see '@auto_tlrc -help' for more information.

        -tlrc_no_ss             : add the -no_ss option to @auto_tlrc

                e.g. -tlrc_no_ss

            This option is used to tell @auto_tlrc not to perform the skull
            strip operation.

            Please see '@auto_tlrc -help' for more information.

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
                e.g. -align_opts_aea -Allineate_opts -source_automask+4
                e.g. -align_opts_aea -giant_move -AddEdge -epi_strip 3dAutomask

            This option allows the user to add extra options to the alignment
            command, align_epi_anat.py.

            Note that only one -align_opts_aea option should be given, with
            possibly many parameters to be passed on to align_epi_anat.py.

            Note the second example.  In order to pass '-source_automask+4' to
            3dAllineate, one must pass '-Allineate_opts -source_automask+4' to
            align_epi_anat.py.

            Please see "align_epi_anat.py -help" for more information.
            Please see "3dAllineate -help" for more information.

        -align_epi_strip_method METHOD : specify EPI skull strip method in AEA

                e.g. -align_epi_strip_method 3dAutomask
                default: 3dSkullStrip

            When align_epi_anat.py is used to align the EPI and anatomy, it
            uses 3dSkullStrip to remove non-brain tissue from the EPI dataset.
            This option can be used to specify which method to use, one of
            3dSkullStrip, 3dAutomask or None.

            This option assumes the 'align' processing block is used.

            Please see "align_epi_anat.py -help" for more information.
            Please see "3dSkullStrip -help" for more information.
            Please see "3dAutomask -help" for more information.

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
                default: third

            This option takes 'first', 'third' or 'last' as a parameter.
            It specifies whether the EPI volumes are registered to the first
            or third volume (of the first run) or the last volume (of the last
            run).  The choice of 'first' or 'third' should correspond to when
            the anatomy was acquired before the EPI data.  The choice of 'last'
            should correspond to when the anatomy was acquired after the EPI
            data.

            The default of 'third' was chosen to go a little farther into the
            steady state data.

            Note that this is done after removing any volumes in the initial
            tcat operation.

            Please see '3dvolreg -help' for more information.
            See also -tcat_remove_first_trs, -volreg_base_ind and
            -volreg_base_dset.

        -volreg_base_dset DSET  : specify dset/sub-brick for volreg base

                e.g. -volreg_base_dset subj10/vreg_base+orig'[4]'

            This option allows the user to specify an external dataset for the
            volreg base.  The user should apply sub-brick selection if the
            dataset has more than one volume.

            Note that unless -align_epi_ext_dset is also applied, this volume
            will be used for anatomical to EPI alignment (assuming that is
            being done at all).

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
                default -cubic

            Please see '3dvolreg -help' for more information.

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

            The volreg/tlrc transformation is then applied as a single warp to
            the unregistered data.

            Note that this is only possible when using @auto_tlrc, not the 12
            piece manual transformation.  See -volreg_tlrc_adwarp for applying
            a manual transformation.

            The resulting voxel grid is the minimum dimension, truncated to 3
            significant bits.  See -volreg_warp_dxyz for details. 

            Note: this step requires a transformed anatomy, which can come from
            the -tlrc_anat option or from -copy_anat importing an existing one.

            Please see 'WARP TO TLRC NOTE' above, for additional details.
            See also -volreg_tlrc_adwarp, -volreg_warp_dxyz, -tlrc_anat,
            -copy_anat.

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

        -surf_spec spec1 [spec2]: specify surface specificatin file(s)

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
            resulting blur, and so do not requre blur estimation.

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

            Valid choices: epi, anat, group, extents.

            A subject 'anat' mask will be created if the EPI anat anatomy are
            aligned, or if the EPI data is warped to standard space via the
            anat transformation.  In any case, a skull-stripped anat will exist.

            A 'group' anat mask will be created if the 'tlrc' block is used
            (via the -block or -tlrc_anat options).  In such a case, the anat
            template will be made into a binary mask.

            This option makes -regress_apply_mask obsolete.

            See "MASKING NOTE" and "DEFAULTS" for details.
            See also -blocks.

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

            Please see '3dSeg -help' for more information
            See also -mask_rm_segsy.

        -mask_segment_erode Y/N

                e.g. -mask_segment_erode Yes
                default: yes (if -regress_ROI or -regress_anaticor)

            This option is a companion to -mask_segment_anat.

            Anatomical segmentation is used to create GM (gray matter), WM
            (white matter) and CSF masks.

            See also -mask_segment_anat, -regress_anaticor.

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
            that X-matrix, possibly censored, in 3dTfitter.  The WMeLocal time
            series is applied along with the X-matrix to get the result.

            Note that other 4-D time series might be regressed out via the
            3dTfitter step, as well.

            This option implies -mask_segment_anat and -mask_segment_erode.

            See also -mask_segment_anat, -mask_segment_erode, -regress_3dD_stop.

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
                default: demean

            By default, the motion parameters from 3dvolreg are applied in the
            regression, but after first removing the mean, per run.  This is
            the application of the 'demean' regressors.

            This option gives the ability to choose a combination of:

                basic:  dfile_rall.1D - the parameters straight from 3dvolreg
                        (or an external motion file, see -regress_motion_file)
                demean: 'basic' params with the mean removed, per run
                deriv:  per-run derivative of 'basic' params (de-meaned)

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
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_basis_normall, -regress_stim_times,
                     -regress_stim_types.

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

        -regress_compute_tsnr yes/no : compute TSNR datasets from errts

                e.g. -regress_compute_tsnr no
                default: yes

            By default, a temporal signal to noise (TSNR) dataset is created at
            the end of the regress block.  The "signal" is the mean of the
            all_runs dataset (input to 3dDeconvolve), and the "noise" is the
            errts dataset (residuals from 3dDeconvolve).

            The main difference between the TSNR datasets from the volreg and
            regress blocks is that the data in the regress block has been
            smoothed (plus it has been "completely" detrended, according to
            the regression model - this includes polort, motion and even stim
            responses).

            Use this option to prevent the TSNR dataset computation in the
            'regress' block.

            TSNR = average(signal) / stdev(noise)

            See also -volreg_compute_tsnr.

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

                e.g. -mask_cormat_warnings No
                default: Yes

            By default, '1d_tool.py -show_cormat_warnings' is run on the 
            regression matrix.  Any large, pairwise correlations are shown
            in text output (which is also saved to a text file).

            This option allows one to disable such functionality.

            Please see '1d_tool.py -help' for more details.

        -regress_est_blur_epits      : estimate the smoothness of the EPI data

            This option specifies to run 3dFWHMx on each of the EPI datasets
            used for regression, the results of which are averaged.  These blur
            values are saved to the file blur_est.$subj.1D, along with any
            similar output from errts.

            These blur estimates may be input to AlphaSim, for any multiple
            testing correction done for this subject.  If AlphaSim is run at
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

            These blur estimates may be input to AlphaSim, for any multiple
            testing correction done for this subject.  If AlphaSim is run at
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
            parameters (from volreg) to the 3dDeconvolve command.

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

        -regress_opts_3dD OPTS ...   : specify extra options for 3dDeconvolve

                e.g. -regress_opts_3dD -gltsym ../contr/contrast1.txt  \\
                                       -glt_label 1 FACEvsDONUT        \\
                                       -jobs 6                         \\
                                       -GOFORIT 8

            This option allows the user to add extra options to the 3dDeconvolve
            command.  Note that only one -regress_opts_3dD should be applied,
            which may be used for multiple 3dDeconvolve options.

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004

        -regress_opts_reml OPTS ...  : specify extra options for 3dREMLfit

                e.g. -regress_opts_reml                                 \\
                        -gltsym ../contr/contrast1.txt FACEvsDONUT      \\
                        -MAXa 0.92

            This option allows the user to add extra options to the 3dREMLfit
            command.  Note that only one -regress_opts_reml should be applied,
            which may be used for multiple 3dREMLfit options.

            Please see '3dREMLfit -help' for more information.

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

            Use this option to regress out one more more known ROI averages.
            Currently known ROIs include:

                name    description     source dataset    creation program
                -----   --------------  --------------    ----------------
                brain   EPI brain mask  full_mask         3dAutomask
                CSF     CSF             mask_CSF_resam    3dSeg -> Classes
                CSFe    CSF (eroded)    mask_CSFe_resam   3dSeg -> Classes
                GM      gray matter     mask_GM_resam     3dSeg -> Classes
                GMe     gray (eroded)   mask_GMe_resam    3dSeg -> Classes
                WM      white matter    mask_WM_resam     3dSeg -> Classes
                WMe     white (eroded)  mask_WMe_resam    3dSeg -> Classes

            Note: use of this option requires the 'mask' processing block
            Note: use of any non-brain cases requires -mask_segment_anat.

            See also -mask_segment_anat.
            Please see '3dSeg -help' for motion information on the masks.

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
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_stim_files, -regress_stim_labels, -regress_basis,
                     -regress_basis_normall, -regress_polort.

        -regress_stim_files FILE1 ... : specify TR-locked stim files

                e.g. -regress_stim_times ED_stim_file*.1D
                e.g. -regress_stim_times stim_A.1D stim_B.1D stim_C.1D

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

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_stim_times, -regress_stim_labels, -regress_basis,
                     -regress_basis_normall, -regress_polort,
                     -regress_stim_times_offset, -regress_use_stim_files.

        -regress_extra_stim_files FILE1 ... : specify extra stim files

                e.g. -regress_extra_stim_files resp.1D cardiac.1D
                e.g. -regress_extra_stim_files regs_of_no_int_*.1D

            Use this option to specify extra files to be applied with the
            -stim_file option in 3dDeconvolve (as opposed to the more usual
            -stim_times).  These files will not be converted to stim_times.

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
                default: 0

            If the -regress_stim_files option is used (so the script converts
            -stim_files to -stim_times before 3dDeconvolve), the user may want
            to add an offset to the times in the output timing files.

            For example, if -tshift_align_to is applied, and the user chooses
            to align volumes to the middle of the TR, it would be appropriate
            to add TR/2 to the times of the stim_times files.

            This OFFSET will be applied to the make_stim_times.py command in
            the output script.

            Please see 'make_stim_times.py -help' for more information.
            See also -regress_stim_files, -regress_use_stim_files,
                     -tshift_align_to.

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

        --------------- 3dClustSim options ------------------

        -regress_run_clustsim yes/no : add 3dClustSim attrs to stats dset

                e.g. -regress_run_clustsim no
                default: yes

            This option controls whether 3dClustSim will be executed after the
            regression analysis.  Since the default is 'yes', the effective use
            of this option would be to turn off the operation.

            3dClustSim is a more advanced version of AlphaSim, and generates a
            table of cluster sizes/alpha values that can be then stored in the
            stats dataset for a simple multiple comparison correction in the
            cluster interface of the afni GUI.

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

    - R Reynolds  Dec, 2006                             thanks to Z Saad
    ===========================================================================
"""
# end global help string
# ----------------------------------------------------------------------

