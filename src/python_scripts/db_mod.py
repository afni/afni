#!/usr/bin/env python

# whine about execution as a main program
if __name__ == '__main__':
   import sys
   print '** %s: not a main program' % sys.argv[0].split('/')[-1]
   sys.exit(1)

import math, os
import afni_base as BASE, afni_util as UTIL
import option_list as OL


WARP_EPI_TLRC_ADWARP    = 1
WARP_EPI_TLRC_WARP      = 2
WARP_EPI_ALIGN_A2E      = 4
WARP_EPI_ALIGN_E2A      = 8

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

    if errs == 0: block.valid = 1
    else        : block.valid = 0

# do not rely on the form of input filenames
# use 3dtcat to copy each file to od_var, then 'cd' into it
def db_cmd_tcat(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-tcat_remove_first_trs')
    first = opt.parlist[0]

    cmd = cmd + "# %s\n"                                                      \
                "# apply 3dTcat to copy input dsets to results dir, while\n"  \
                "# removing the first %d TRs\n" % (block_header('tcat'), first)
    for run in range(0, proc.runs):
        cmd = cmd + "3dTcat -prefix %s/%s %s'[%d..$]'\n" %              \
                    (proc.od_var, proc.prefix_form(block,run+1),
                     proc.dsets[run].rpv(), first)

    cmd = cmd + '\n'                            + \
                '# and enter the results directory\n' \
                'cd %s\n\n' % proc.od_var

    proc.reps   -= first        # update reps to account for removed TRs
    proc.reps_all = [reps-first for reps in proc.reps_all]

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    if proc.verb > 0: print "-- %s: reps is now %d" % (block.label, proc.reps)

    return cmd

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

    block.valid = 1

# align anat to epi -> anat gets _al suffix to its prefix
#                   -> matrix is ${anat_prefix}_al.mat.aff12.1D
# (adjust prefix of self.anat and self.tlrcanat)
# set a2e xform matrix
def db_cmd_align(proc, block):

    if not proc.anat:
        print '** ERROR: missing anat for align block (consider -copy_anat)\n'
        return

    # first note EPI alignment base and sub-brick, as done in volreg block
    # ## rcr: alignEA EPI and base might be given externally
    #         via -align_epi_base_dset
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

    # add any user-specified options
    opt = block.opts.find_opt('-align_opts_aea')
    if opt and opt.parlist: extra_opts = "       %s \\\n" % \
                            ' '.join(UTIL.quotize_list(opt.parlist, '', 1))
    else:   extra_opts = ''

    # write commands
    cmd =       '# %s\n'                                        \
                '# align anatomy to EPI registration base\n'    \
                % block_header('align')
    cmd = cmd + 'align_epi_anat.py -anat2epi \\\n'                      \
                '       -anat %s \\\n'                                  \
                '       -epi %s \\\n'                                   \
                '%s'                                                    \
                '       -epi_base %d -volreg off -tshift off\n\n'       \
                % (proc.anat.pv(), basevol, extra_opts, bind)

    # store alignment matrix file for possible later use
    proc.a2e_mat = "%s_al_mat.aff12.1D" % proc.anat.prefix

    # update anat and tlrc to aligned one, unless going e2a at volreg
    opt = None
    vblock = proc.find_block('volreg')
    if vblock: opt = vblock.opts.find_opt('-volreg_align_e2a')
    if not opt:
        proc.anat.prefix = "%s_al" % proc.anat.prefix
        if proc.tlrcanat: proc.tlrcanat.prefix = "%s_al" % proc.tlrcanat.prefix
        proc.tlrc_ss = 0        # default to no skull-strip

    # note the alignment in EPIs warp bitmap (2=a2e)
    proc.warp_epi |= WARP_EPI_ALIGN_A2E

    return cmd

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

    str = '# copy slice-based regressors for RETROICOR (rm first %d TRs)\n' \
          % proc.ricor_nfirst

    if proc.ricor_nfirst > 0: offstr = "'{%d..$}'" % proc.ricor_nfirst
    else:                     offstr = ''
    
    for ind in range(len(proc.ricor_regs)):
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
    err, dims = UTIL.get_typed_dset_attr_list(proc.dsets[0].rpv(),
                                              "DATASET_DIMENSIONS", int)
    if err or len(dims) < 4:
        print '** ERROR: failed to get DIMENSIONS from %s' % proc.dsets[0].rpv()
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
    nt = adata.nt-proc.ricor_nfirst
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
        "set volsperrun = %d\n"                                 \
        "set startind = 0\n"                                    \
        "@ endind = $volsperrun - 1\n"                          \
        "foreach run ( $runs )\n"                               \
        '    3dcalc -a %s.errts%s"[$startind..$endind]" \\\n'   \
        '           -b %s.polort%s"[$startind..$endind]" \\\n'  \
        '%s'                                                    \
        '           -expr a+b -prefix %s\n'                     \
        '    @ startind += $volsperrun    # add nreps\n'        \
        '    @ endind += $volsperrun\n'                         \
        'end\n\n'                                               \
        % (proc.reps, prefix, proc.view, prefix, proc.view, dstr, cur_prefix)

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

# run 3dToutcount and 3dTshift for each run
def db_cmd_tshift(proc, block):
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
    cmd = cmd + '# %s\n'                                        \
                '# run 3dToutcount and 3dTshift for each run\n' \
                % block_header('tshift')
    cmd = cmd + 'foreach run ( $runs )\n'                                     \
                '    3dToutcount -automask %s > outcount_r$run.1D\n'          \
                '\n'                                                          \
                '    3dTshift %s %s -prefix %s \\\n'                          \
                '%s'                                                          \
                '             %s\n'                                           \
                'end\n\n' %                                                   \
                (prev_prefix, align_to, resam,cur_prefix,other_opts,prev_prefix)
    
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_volreg(block, proc, user_opts):
    if len(block.opts.olist) == 0:   # init dset/brick indices to defaults
        block.opts.add_opt('-volreg_base_ind', 2, [0, 2], setpar=1)
        block.opts.add_opt('-volreg_interp', 1, ['-cubic'], setpar=1)
        block.opts.add_opt('-volreg_opts_vr', -1, [])
        block.opts.add_opt('-volreg_zpad', 1, [1], setpar=1)

    # check for updates to -volreg_base option
    uopt = user_opts.find_opt('-volreg_base_ind')
    bopt = block.opts.find_opt('-volreg_base_ind')
    aopt = user_opts.find_opt('-volreg_align_to')
    baseopt = user_opts.find_opt('-volreg_base_dset')

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

    uopt = user_opts.find_opt('-volreg_regress_per_run')
    bopt = block.opts.find_opt('-volreg_regress_per_run')
    if uopt and not bopt:
        block.opts.add_opt('-volreg_regress_per_run', 0, [])

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
                    "   in place of '-volreg_tlrc_adwarp'"
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

    block.valid = 1

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
        print '** have external align EPI volume, but seem to be\n'     \
              '   aligning EPI to end the runs, this looks fishy...'

    # get any base_vol option
    if proc.vr_ext_base != None: basevol = "%s%s" % (proc.vr_ext_pre,proc.view)
    else: basevol = None

    if proc.verb > 0:
        if basevol:
            print "-- %s: using base dset %s" % (block.label,basevol)
        else:
            print "-- %s: base/sub indices are %d, %d" % \
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
            dim = UTIL.get_truncated_grid_dim(proc.dsets[0].rpv())
            if dim <= 0:
                print '** failed to get grid dim from %s' % proc.dsets[0].rpv()
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

    # if we want to regress motion files per run, create them and add to list
    if block.opts.find_opt('-volreg_regress_per_run'):
        cmd = cmd + '\n' +                                                  \
            "    # pad motion parameters from each run to span all runs \n" \
            "    1d_tool.py -infile dfile.r$run.1D "                        \
            "-pad_into_many_runs $run %d \\\n"                              \
            "               -write dfile.r$run.pad.1D\n" % proc.runs
        proc.mot_files = ['dfile.r%02d.pad.1D'%(r+1) for r in range(proc.runs)]

    # if there is a base_dset option, check for failure in 3dvolreg
    if basevol:
        cmd = cmd + '\n    # if there was an error, exit so user can see'     \
                    '\n    if ( $status ) exit\n\n'

    cmd = cmd + "end\n\n"                                                     \
                "# make a single file of registration params\n"               \
                "cat dfile.r??.1D > dfile.rall.1D\n\n"

    # if not censoring motion, make a generic motion file
    if not proc.user_opts.find_opt('-regress_censor_motion'):
        cmd = cmd +                                                         \
            "# compute motion magnitude time series: the Euclidean norm\n"  \
            "# (sqrt(sum squares)) of the motion parameter derivatives\n"

        if proc.reps_vary :     # use -set_run_lengths aot -set_nruns
           cmd = cmd +                                                      \
               "1d_tool.py -infile dfile.rall.1D \\\n"                      \
               "           -set_run_lengths %s \\\n"                        \
               "           -derivative  -collapse_cols euclidean_norm \\\n" \
               "           -write motion_${subj}_enorm.1D\n\n"              \
               % UTIL.int_list_string(proc.reps_all)
        else:                   # stick with -set_nruns
           cmd = cmd +                                                      \
               "1d_tool.py -infile dfile.rall.1D -set_nruns %d \\\n"        \
               "           -derivative  -collapse_cols euclidean_norm \\\n" \
               "           -write motion_${subj}_enorm.1D\n\n" % proc.runs

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
            "3dcopy rm.epi.min.r01 %s\n\n" % (proc.mask_extents.prefix)

        proc.mask_extents.created = 1  # so this mask 'exists' now

        cmd = cmd +                                             \
            "# and apply the extents mask to the EPI data \n"   \
            "# (delete any time series with missing data)\n"    \
            "foreach run ( $runs )\n"                           \
            "    3dcalc -a rm.epi.nomask.r$run%s -b %s \\\n"    \
            "           -expr 'a*b' -prefix %s\n"               \
            "end\n\n" % (proc.view, proc.mask_extents.pv(), cur_prefix)

    # ---------------
    # lastly, see if we want to apply a (manual) warp to tlrc space
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

    # used 3dvolreg, so have these labels
    proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_blur(block, proc, user_opts):
    if len(block.opts.olist) == 0: # init blur option
        block.opts.add_opt('-blur_filter', 1, ['-1blur_fwhm'], setpar=1)
        block.opts.add_opt('-blur_size', 1, [4.0], setpar=1)
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
        if bopt: bopt.parlist[0] = uopt.parlist[0]
        else: block.opts.add_opt('-blur_in_mask', 1, uopt.parlist, setpar=1)

    uopt = user_opts.find_opt('-blur_in_automask')
    bopt = block.opts.find_opt('-blur_in_automask')
    if uopt and not bopt: block.opts.add_opt('-blur_in_automask', 0, [])

    uopt = user_opts.find_opt('-blur_size')
    bopt = block.opts.find_opt('-blur_size')
    if uopt and bopt:
        try: bopt.parlist[0] = float(uopt.parlist[0])
        except:
            print "** -blur_size must be a real number, have '%s'" %(parlist[0])
            block.valid = 0
            return 1

    uopt = user_opts.find_opt('-blur_opts_merge')
    bopt = block.opts.find_opt('-blur_opts_merge')
    if uopt and bopt: bopt.parlist = uopt.parlist

    block.valid = 1

def db_cmd_blur(proc, block):
    cmd = ''
    opt    = block.opts.find_opt('-blur_filter')
    filter = opt.parlist[0]
    opt    = block.opts.find_opt('-blur_size')
    size   = opt.parlist[0]
    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run(view=1)

    other_opts = ''

    # if -blur_in_mask, use 3dBlurInMask (requires 1blur_fwhm)
    if OL.opt_is_yes(block.opts.find_opt('-blur_in_mask')) or \
        block.opts.find_opt('-blur_in_automask'):

       # verify FWHM filter
       if filter != '-1blur_fwhm' and filter != '-FWHM':
          print "** error: 3dBlurInMask requires FWHM filter, have '%s'\n" \
                "   (consider scale of 1.36 for RMS->FWHM)" % (filter)
          return 1

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
       cstr = "    3dBlurInMask -preserve -FWHM %s%s \\\n"    \
              "                 -prefix %s" % (str(size), mopt, prefix)
       sstr = '                 '


    else: # default: use 3dmerge for blur
       # maybe there are extra options to append to the command
       opt = block.opts.find_opt('-blur_opts_merge')
       if not opt or not opt.parlist: other_opts = ''
       else: other_opts = '             %s \\\n' % ' '.join(opt.parlist)

       cstr = "    3dmerge %s %s -doall -prefix %s" % (filter,str(size),prefix)
       sstr = '            '

    cmd = cmd + "# %s\n"                                \
                "# blur each volume of each run\n"      \
                "foreach run ( $runs )\n"               \
                "%s \\\n"                               \
                "%s"                                    \
                "%s%s\n"                                \
                "end\n\n" % (block_header('blur'), cstr, sstr, other_opts, prev)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_mask(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-mask_type', 1, ['union'], setpar=1)
        block.opts.add_opt('-mask_dilate', 1, [1], setpar=1)

    # check for user updates
    uopt = user_opts.find_opt('-mask_type')
    bopt = block.opts.find_opt('-mask_type')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]   # no worries, using acplist

    uopt = user_opts.find_opt('-mask_dilate')
    bopt = block.opts.find_opt('-mask_dilate')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print "** -mask_dilate requres an int nsteps (have '%s')" % \
                  uopt.parlist[0]
            block.valid = 0
            return 1

    uopt = user_opts.find_opt('-mask_apply')
    bopt = block.opts.find_opt('-mask_apply')
    if uopt:
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-mask_apply', 1, uopt.parlist, setpar=1)

    proc.mask_epi = BASE.afni_name('full_mask%s$subj' % proc.sep_char)
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
def db_cmd_mask(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-mask_type')
    type = opt.parlist[0]
    if type == 'union': min = 0            # result must be greater than min
    else:               min = 0.999

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
                    (str(min), type, proc.view, str(min), proc.mask_epi.prefix)
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

    # lastly, see if the user wants to choose which mask to apply
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

    # do not increment block index or set 'previous' block label,
    # as there are no datasets created here

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
        print "** failed to find tlrc_base '%s' for group mask"%proc.tlrc_base
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

    # and finally, convert to the binary mask of choice
    cmd = cmd + "# convert resampled group brain to binary mask\n"  \
                "3dcalc -a %s -expr 'ispositive(a)' -prefix %s\n\n" \
                % (tanat.pv(), proc.mask_group.prefix)

    proc.mask_group.created = 1  # so this mask 'exists' now

    return cmd

# if possible make a subject anatomical mask (resampled to EPI)
#    - if -volreg_tlrc_warp, apply from tlrc anat
#    - if a2e, apply from anat_al
#    - if e2a, apply from anat_al with inverted transform
# return None on failure
def anat_mask_command(proc, block):
    if not proc.warp_epi: return ''

    proc.mask_anat = proc.mask_epi.new('mask_anat.$subj')
    cmd = "# ---- create subject anatomy mask, %s ----\n" % proc.mask_anat.pv()

    if proc.verb > 2:
        print '-- mask for anat based on warp_epi == %d\n'                \
              '   (1==tlrc_adwarp, 2==tlrc_warp, 4=anat2epi, 8=epi2anat)' \
              % proc.warp_epi

    # set anat, comment string text and temporary anat
    if proc.warp_epi & (WARP_EPI_TLRC_WARP | WARP_EPI_TLRC_ADWARP):
        anat = proc.tlrcanat
        ss = 'tlrc'
    elif proc.warp_epi & WARP_EPI_ALIGN_A2E:
        anat = proc.anat
        ss = 'aligned'
    elif proc.warp_epi & WARP_EPI_ALIGN_E2A:
        anat = proc.anat.new(proc.anat.prefix+'_al')
        ss = 'aligned'
    else: # should not happen
        print '** anat_mask_command: invalid warp_epi = %d' % proc.warp_epi
        return None
    cmd = cmd + "#      (resampled from %s anat)\n" % ss
    tanat = anat.new('rm.resam.anat')   # temporary resampled anat dset

    if proc.warp_epi == WARP_EPI_ALIGN_E2A:
        cmd = cmd + '\n'                                                     \
              "# invert a2e matrix, and warp/resample skull-stripped anat\n" \
              "cat_matvec -ONELINE %s -I > mat.a2e.inv.aff12.1D\n"           \
              "3dAllineate -input %s -master %s \\\n"                        \
              "            -1Dmatrix_apply mat.a2e.inv.aff12.1D \\\n"        \
              "            -prefix %s\n\n"                                   \
               % (proc.a2e_mat, anat.pv(), proc.mask_epi.pv(), tanat.prefix)
    else:
        # resample masked anat to epi grid, output is temp anat
        cmd = cmd + "3dresample -master %s -prefix %s \\\n"             \
                    "           -input %s\n\n"                          \
                    % (proc.mask_epi.pv(), tanat.prefix, anat.pv())

    # and finally, convert to the binary mask of choice
    cmd = cmd + "# convert resampled anat brain to binary mask\n"   \
                "3dcalc -a %s -expr 'ispositive(a)' -prefix %s\n\n" \
                % (tanat.pv(), proc.mask_anat.prefix)

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
    cmd = ''
    # check for max scale value 
    opt = block.opts.find_opt('-scale_max_val')
    max = opt.parlist[0]
    if max > 100: valstr = 'max(0, min(%d, a/b*100))' % max
    else:         valstr = 'a/b*100*step(a)'

    if proc.mask and proc.regmask:
        mask_dset = '           -c %s%s \\\n' % (proc.mask.prefix, proc.view)
        expr      = 'c * %s' % valstr
    else:
        mask_dset = ''
        expr      = valstr

    if max > 100: maxstr = '# (subject to a range of [0,%d])\n' % max
    else        : maxstr = ''

    prev = proc.prev_prefix_form_run(view=1)
    prefix = proc.prefix_form_run(block)
    cmd = cmd + "# %s\n"                                                \
                "# scale each voxel time series to have a mean of 100\n"\
                "# (be sure no negatives creep in)\n"                   \
                "%s"                                                    \
                "foreach run ( $runs )\n"                               \
                "    3dTstat -prefix rm.mean_r$run %s\n"                \
                "    3dcalc -a %s -b rm.mean_r$run%s \\\n"              \
                "%s"                                                    \
                "           -expr '%s' \\\n"                            \
                "           -prefix %s\n"                               \
                "end\n\n" %                                             \
                (block_header('scale'), maxstr, prev, prev, proc.view,
                 mask_dset, expr, prefix)
    proc.have_rm = 1            # rm.* files exist

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_regress(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-regress_basis', 1, ['GAM'], setpar=1)
        block.opts.add_opt('-regress_censor_prev', 1, ['yes'], setpar=1)
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
        block.opts.add_opt('-regress_make_ideal_sum', 1, [])
        block.opts.add_opt('-regress_errts_prefix', 1, [])
        block.opts.add_opt('-regress_fitts_prefix', 1, ['fitts.$subj'],
                                                       setpar=1)

    errs = 0  # allow errors to accumulate

    # check for user updates
    uopt = user_opts.find_opt('-regress_basis')
    bopt = block.opts.find_opt('-regress_basis')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]
        if not UTIL.basis_has_known_response(bopt.parlist[0], warn=1):
            if not user_opts.find_opt('-regress_iresp_prefix'):
                block.opts.add_opt('-regress_iresp_prefix',1,['iresp'],setpar=1)
        uopt = user_opts.find_opt('-regress_make_ideal_sum')
        if uopt and not UTIL.basis_has_known_response(bopt.parlist[0]):
            print '** -regress_make_ideal_sum is inappropriate for basis %s'\
                  % bopt.parlist[0]
            errs += 1

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

    uopt = user_opts.find_opt('-regress_stim_labels')
    bopt = block.opts.find_opt('-regress_stim_labels')
    if uopt and bopt:  # check length later, when we know num stim types
        bopt.parlist = uopt.parlist

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

    uopt = user_opts.find_opt('-regress_opts_3dD')
    bopt = block.opts.find_opt('-regress_opts_3dD')
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

    # check for censoring of large motion
    uopt = user_opts.find_opt('-regress_censor_motion')
    bopt = block.opts.find_opt('-regress_censor_motion')
    if uopt:
      try: limit = float(uopt.parlist[0])
      except:
        print "** -regress_censor_motion limit must be float, have '%s'" \
              % uopt.parlist[0]
        return 1
      if limit < 0.0:
        print '** -regress_censor_motion limit must be positive, have %g'%limit
        errs += 1
      if bopt: bopt.parlist[0] = limit
      else: block.opts.add_opt('-regress_censor_motion', 1, [limit], setpar=1)

    # do we also censor the previous TR?
    uopt = user_opts.find_opt('-regress_censor_prev')
    bopt = block.opts.find_opt('-regress_censor_prev')
    if uopt:
        if bopt: bopt.parlist = uopt.parlist
        else: block.opts.add_opt('-regress_censor_prev', 1,
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
        bopt.parlist = [uopt.parlist[0] + '.$subj']    # add $subj to prefix

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
    if uopt: proc.regmask = 1

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
        proc.mot_files = uopt.parlist
        proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']
        # do not allow -volreg_regress_per_run with this
        blk = proc.find_block('volreg')
        if blk:
           vopt = blk.opts.find_opt('-volreg_regress_per_run')
           if vopt:
              print '** -volreg_regress_per_run is illegal with' \
                    ' -regress_motion_file'
              errs += 1

    # maybe the user does not want to convert stim_files to stim_times
    uopt = user_opts.find_opt('-regress_use_stim_files')
    if not uopt: uopt = user_opts.find_opt('-regress_no_stim_times')
    bopt = block.opts.find_opt('-regress_no_stim_times')
    if uopt and not bopt:
        if proc.verb > 0: print '-- will use -stim_files in 3dDeconvolve'
        block.opts.add_opt('-regress_no_stim_times',0,[],setpar=1)

    # just pass along regress_3dD_stop
    uopt = user_opts.find_opt('-regress_3dD_stop')
    bopt = block.opts.find_opt('-regress_3dD_stop')
    if uopt and not bopt: block.opts.add_opt('-regress_3dD_stop',0,[])

    # just pass along regress_reml_exec
    uopt = user_opts.find_opt('-regress_reml_exec')
    bopt = block.opts.find_opt('-regress_reml_exec')
    if uopt and not bopt: block.opts.add_opt('-regress_reml_exec',0,[])

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
    basis = opt.parlist[0]

    opt = block.opts.find_opt('-regress_basis_normall')
    if opt: normall = '    -basis_normall %s \\\n' % opt.parlist[0]
    else:   normall = ''

    opt = block.opts.find_opt('-regress_no_motion')
    if opt: proc.mot_labs = []   # then clear any motion labels

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

    cmd = cmd + "# %s\n" \
                "# run the regression analysis\n" % block_header('regress')

    # possibly add a make_stim_times.py command
    opt = block.opts.find_opt('-regress_stim_times')
    convert = (block.opts.find_opt('-regress_no_stim_times') == None) and \
                len(proc.stims) > 0
    if convert and (not opt.parlist or len(opt.parlist) == 0):
        newcmd = db_cmd_regress_sfiles2times(proc, block)
        if not newcmd: return
        cmd = cmd + newcmd

    # if the user wants to censor large motion, create a censor.1D file
    if block.opts.find_opt('-regress_censor_motion'):
        err, newcmd = db_cmd_regress_censor_motion(proc, block)
        if err: return
        if newcmd: cmd = cmd + newcmd

    # possibly use a mask
    if proc.mask and proc.regmask:
        mask = '    -mask %s%s \\\n' % (proc.mask.prefix, proc.view)
    else:
        mask = ''

    # maybe the user has specified global or local times
    if block.opts.find_opt('-regress_global_times'):
        times_type = '    -global_times \\\n'
    elif block.opts.find_opt('-regress_local_times'):
        times_type = '    -local_times \\\n' 
    else: times_type=''

    # if the input datatype is float, force such output from 3dDeconvolve
    if proc.datatype == 3: datum = '-float '
    else:                  datum = ''

    nmotion = len(proc.mot_labs) * len(proc.mot_files)
    total_nstim =  len(proc.stims) + len(proc.extra_stims) + \
                   nmotion + proc.ricor_nreg
    if proc.regress_censor_file:
        censor_str = '    -censor %s \\\n' % proc.regress_censor_file
    else: censor_str = ''
    cmd = cmd + '3dDeconvolve -input %s \\\n'           \
                '%s'                                    \
                '    -polort %d %s\\\n'                 \
                '%s%s%s'                                \
                '    -num_stimts %d \\\n'               \
                % ( proc.prev_dset_form_wild(), censor_str, polort, datum,
                    mask, normall, times_type, total_nstim )

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

    # we need labels for iresp
    opt = block.opts.find_opt('-regress_iresp_prefix')
    if not opt or not opt.parlist: iresp = ''
    else:
        iresp = ''
        for index in range(len(labels)):
            iresp = iresp + "    -iresp %d %s_%s.$subj \\\n" % \
                            (index+1, opt.parlist[0], labels[index])

    # write out stim lines (add -stim_base to any RONI)

    sfiles = block.opts.find_opt('-regress_no_stim_times')
    for ind in range(len(proc.stims)):
        if sfiles:  # then -stim_file and no basis function
            cmd = cmd + "    -stim_file %d %s \\\n" % (ind+1,proc.stims[ind])
        else:
            cmd = cmd + "    -stim_times %d %s '%s' \\\n"  % \
                        (ind+1, proc.stims[ind], basis)
        # and add the label
        if ind+1 in roni_list: rstr = '-stim_base %d ' % (ind+1)
        else:                  rstr = ''
        cmd = cmd + "    -stim_label %d %s %s\\\n" % (ind+1, labels[ind], rstr)

    # accumulate offset for current regressor list (3dD input is 1-based)
    regindex = len(proc.stims) + 1

    # maybe add extra_stims (add -stim_base to any RONI)
    if len(proc.extra_stims) > 0:
        for ind in range(len(proc.extra_stims)):
            sind = ind+regindex
            if sind in roni_list: rstr = '-stim_base %d ' % sind
            else:                 rstr = ''
            cmd = cmd + "    -stim_file %d %s \\\n"    \
                        "    -stim_label %d %s %s\\\n" %  \
                        (sind,proc.extra_stims[ind],sind,exlabs[ind],rstr)
        regindex += len(proc.extra_stims)

    # write out registration param lines
    if nmotion > 0:
        nlabs = len(proc.mot_labs)
        nmf = len(proc.mot_files)
        for findex in range(nmf):
            mfile = proc.mot_files[findex]
            for ind in range(nlabs):
                if nmf > 1: mlab = '%s_%02d' % (proc.mot_labs[ind], findex+1)
                else:       mlab = '%s'      % (proc.mot_labs[ind])
                sind = regindex + nlabs*findex + ind
                cmd = cmd + "    -stim_file %d %s'[%d]' "       \
                        "-stim_base %d -stim_label %d %s \\\n"  \
                        % (sind, proc.mot_files[findex], ind, sind, sind, mlab)
        regindex += nmotion

    # write out ricor param lines (put labels afterwards)
    if proc.ricor_reg and proc.ricor_nreg > 0:
        for ind in range(proc.ricor_nreg):
            cmd = cmd + "    -stim_file %02d %s'[%02d]' "       \
                        "-stim_base %02d \\\n"                  \
                        % (ind+regindex, proc.ricor_reg, ind, ind+regindex)
        cmd = cmd + '    '
        for ind in range(proc.ricor_nreg):
            cmd = cmd + "-stim_label %02d ricor%02d " % (ind+regindex, ind)
        cmd = cmd + '\\\n'
        regindex += proc.ricor_nreg

    # see if the user wants the fit time series
    opt = block.opts.find_opt('-regress_fitts_prefix')
    if not opt or not opt.parlist: fitts = ''
    else: fitts = '    -fitts %s \\\n' % opt.parlist[0]

    # -- see if the user wants the error time series --
    opt = block.opts.find_opt('-regress_errts_prefix')
    bluropt = block.opts.find_opt('-regress_est_blur_errts')
    # if there is no errts prefix, but the user wants to measure blur, add one
    # (or if there are no normal regressors)
    if nregs == 0 or (not opt.parlist and bluropt):
        opt.parlist = ['errts.$subj']

    if not opt or not opt.parlist: errts = ''
    else: errts = '    -errts %s \\\n' % opt.parlist[0]
    # -- end errts --

    # see if the user has provided other options (like GLTs)
    opt = block.opts.find_opt('-regress_opts_3dD')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '    %s \\\n' %         \
               ' '.join(UTIL.quotize_list(opt.parlist, '\\\n    ', 1))

    # are we going to stop with the 1D matrix?
    opt = block.opts.find_opt('-regress_3dD_stop')
    if opt: stop_opt = '    -x1D_stop \\\n'
    else  : stop_opt = ''

    # do we want F-stats
    opt = block.opts.find_opt('-regress_fout')
    if opt.parlist[0] == 'yes': fout_str = '-fout '
    else:                       fout_str = ''

    # add misc options
    cmd = cmd + iresp
    cmd = cmd + other_opts
    cmd = cmd + "    %s-tout -x1D X.xmat.1D -xjpeg X.jpg \\\n" % fout_str
    cmd = cmd + fitts + errts + stop_opt
    cmd = cmd + "    -bucket stats.$subj\n\n\n"

    # if 3dDeconvolve fails, terminate the script
    cmd = cmd + "# if 3dDeconvolve fails, terminate the script\n"       \
                "if ( $status != 0 ) then\n"                            \
                "    echo '---------------------------------------'\n"  \
                "    echo '** 3dDeconvolve error, failing...'\n"        \
                "    echo '   (consider the file 3dDeconvolve.err)'\n"  \
                "    exit\n"                                            \
                "endif\n\n\n"

    # possibly run the REML script
    if block.opts.find_opt('-regress_reml_exec'):
        rcmd = db_cmd_reml_exec(proc, block)
        if not rcmd: return
        cmd = cmd + rcmd

    # create all_runs dataset
    all_runs = 'all_runs%s$subj' % proc.sep_char
    cmd = cmd + "# create an all_runs dataset to match the fitts, errts, etc.\n"
    cmd = cmd + "3dTcat -prefix %s %s\n\n" % \
                (all_runs, proc.prev_dset_form_wild())

    # extract ideal regressors, and possibly make a sum
    opt = block.opts.find_opt('-regress_no_ideals')
    if not opt and UTIL.basis_has_known_response(basis):
        # then we compute individual ideal files for each stim
        cmd = cmd + "# create ideal files for each stim type\n"
        first = (polort+1) * proc.runs
        for ind in range(len(labels)):
            cmd = cmd + "1dcat X.xmat.1D'[%d]' > ideal_%s.1D\n" % \
                        (first+ind, labels[ind])
        cmd = cmd + '\n'

    opt = block.opts.find_opt('-regress_make_ideal_sum')
    if opt and opt.parlist:
        first = (polort+1) * proc.runs
        last = first + len(proc.stims) - 1
        cmd = cmd + "# create ideal file by adding ideal regressors\n"
        cmd = cmd + "3dTstat -sum -prefix %s X.xmat.1D'[%d..%d]'\n\n" % \
                    (opt.parlist[0], first, last)

    # check for blur estimates
    bcmd = db_cmd_blur_est(proc, block)
    if bcmd == None: return  # error
    cmd = cmd + bcmd

    proc.pblabel = block.label  # set 'previous' block label

    return cmd

# create a short command to run the REML script
# The script name is currently stats.REML_cmd, based on the 'stats.' -bucket
# prefix in 3dD.
#
# return None on failure
def db_cmd_reml_exec(proc, block):
    if proc.verb > 1: print '++ creating reml_exec command string'

    cmd = '# -- execute the REML command script and check the status --\n'
    cmd = cmd + 'tcsh -x stats.REML_cmd\n\n'

    # if 3dDeconvolve fails, terminate the script
    cmd = cmd + "# if 3dREMLfit fails, terminate the script\n"          \
                "if ( $status != 0 ) then\n"                            \
                "    echo '---------------------------------------'\n"  \
                "    echo '** 3dREMLfit error, failing...'\n"           \
                "    exit\n"                                            \
                "endif\n\n\n"

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
        if proc.verb > 2: print '-- no blur estimation'
        return cmd

    # set the mask (if we don't have one, bail)
    if not proc.mask:
        print '** refusing to estimate blur without a mask dataset'
        print '   (masks are not applied without -regress_apply_mask)'
        return

    if proc.verb > 1: print '++ computing blur estimates'
    blur_file = 'blur_est.$subj.1D'

    # call this a new sub-block
    cmd = cmd + '# %s\n'                                \
                '# compute blur estimates\n'            \
                'touch %s   # start with empty file\n\n'\
                % (block_header('blur estimation'), blur_file)

    if aopt:
        bstr = blur_est_loop_str(proc,
                    'all_runs%s$subj%s' % (proc.sep_char, proc.view), 
                    '%s%s' % (proc.mask.prefix, proc.view), 'epits', blur_file)
        if not bstr: return
        cmd = cmd + bstr

    opt = block.opts.find_opt('-regress_errts_prefix')
    if opt and opt.parlist: errts_pre = opt.parlist[0]
    else:   errts_pre = 'errts.$subj'

    if eopt and not sopt: # want errts, but 3dD was not executed
        bstr = blur_est_loop_str(proc, '%s%s' % (errts_pre, proc.view), 
                    '%s%s' % (proc.mask.prefix, proc.view), 'errts', blur_file)
        if not bstr: return
        cmd = cmd + bstr
    if eopt and ropt: # want errts and reml was executed
        # cannot use ${}, so escape the '_'
        bstr = blur_est_loop_str(proc, '%s\_REML%s' % (errts_pre, proc.view), 
                    '%s%s'%(proc.mask.prefix,proc.view), 'err_reml', blur_file)
        if not bstr: return
        cmd = cmd + bstr
    cmd = cmd + '\n'

    return cmd

def blur_est_loop_str(proc, dname, mname, label, outfile):
    """return tcsh command string to compute blur from this dset
        proc     : afni_proc SubjProcStream (for reps or reps_all)
        dname    : dataset name to estimate blur on
        mname    : mask dataset name
        label    : text label for comments
        outfile  : final output filename
    """
    dset  = BASE.afni_name(dname)
    input = dset.shortinput()
    mset  = BASE.afni_name(mname)
    mask  = mset.shortinput()
    tmpfile = 'blur.%s.1D' % label

    if not input:
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
        'foreach reps ( %s )\n'                                 \
        '    @ b1 += $reps  # last index for current run\n'     \
        '    3dFWHMx -detrend -mask %s \\\n'                    \
        '        %s"[$b0..$b1]" >> %s\n'                        \
        % (UTIL.int_list_string(proc.reps_all), mask, input, tmpfile)

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

def db_cmd_regress_censor_motion(proc, block):
    """return a command to create a censor.1D file

       Require a non-negative LIMIT, consistent run length and a single
       motion file.

       As a side effect, set proc.regress_censor_file.

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

    if len(proc.mot_files) == 0: return 0, ''
    elif len(proc.mot_files) > 1:       # okay if -volreg_regress_per_run
        vblk = proc.find_block('volreg')
        opt = None
        mot_file = ''
        if vblk: opt = vblk.find_opt('-volreg_regress_per_run')
        if opt: mot_file = 'dfile.rall.1D'
        if not mot_file:
            print '** bad motion files for -regress_censor_motion'
            return 1, ''
    else: mot_file = proc.mot_files[0]  # there is only 1

    # check for censor_prev_TR
    opt = block.opts.find_opt('-regress_censor_prev')
    if opt.parlist[0] == 'yes': prev_str = '-censor_prev_TR'
    else:                       prev_str = ''

    if proc.verb > 1:
        print '-- creating motion censor command, file = %s' % mot_file

    # save string to apply in 3dDeconvolve
    mot_prefix = 'motion_${subj}'
    proc.regress_censor_file = '%s_censor.1D' % mot_prefix
    cmd = '\n# create censor file %s, for censoring motion \n' \
          % proc.regress_censor_file
                
    # make command string to create censor file
    if proc.reps_vary :     # use -set_run_lengths aot -set_nruns
        cmd = cmd + '1d_tool.py -infile %s -set_run_lengths %s \\\n' \
                    % (mot_file, UTIL.int_list_string(proc.reps_all))
    else:
        cmd = cmd + '1d_tool.py -infile %s -set_nruns %d \\\n'       \
                    % (mot_file, proc.runs)

    cmd = cmd + '    -set_tr %g -show_censor_count %s\\\n'     \
                '    -censor_motion %g %s\n\n'                 \
                % (proc.tr, prev_str, limit, mot_prefix)

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
    """warp self.anat to standard space"""

    dname = proc.anat.pv()
    if not dname :
        print "** missing dataset name for tlrc operation"
        return None

    # no longer look to add +orig

    opt = block.opts.find_opt('-tlrc_base')
    if opt: base = opt.parlist[0]
    else:   base = 'TT_N27+tlrc'

    proc.tlrc_base = BASE.afni_name(base)       # store for later

    opt = block.opts.find_opt('-tlrc_no_ss')
    if opt or not proc.tlrc_ss: ss = ' -no_ss'
    else:                       ss = ''

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

    cmd += "# warp anatomy to standard space\n"                          \
           "@auto_tlrc -base %s -input %s%s%s%s\n\n"                     \
           % (base, dname, ss, rmode, suffix)

    return cmd

# currently nothing to verify for an 'empty' command (placeholder command)
# just return 1
def db_mod_empty(block, proc, user_opts):
    block.valid = 1
    return 1

# create a placeholder command using 3dTcat to copy the EPI data
def db_cmd_empty(proc, block):
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
    if not proc.gen_review: return None

    tblk = proc.find_block('tcat')

    cmd = "# %s\n"                                                      \
          "# generate a review script for the unprocessed EPI data\n"   \
          "gen_epi_review.py -script %s \\\n"                           \
          "    -dsets %s\n\n" % (block_header('gen_epi_review.py'),
               proc.gen_review, proc.dset_form_wild('tcat',proc.origview))

    return cmd

def block_header(hname, maxlen=74, hchar='='):
    """return a title string of 'hchar's with the middle chars set to 'name'"""
    if len(hname) > 0: name = ' %s ' % hname
    else:              name = ''

    rmlen = len(name)
    if rmlen >= maxlen:
        print "** block_header, rmlen=%d exceeds maxlen=%d" % (rmlen, maxlen)
        return name
    prelen  = (maxlen - rmlen) // 2     # basically half the chars
    postlen = maxlen - rmlen - prelen   # other 'half'

    return prelen*hchar + name + postlen*hchar

# ----------------------------------------------------------------------
# global help string (see end global help string)
# -- this is long, get it out of the main library

g_help_string = """
    ===========================================================================
    afni_proc.py        - generate a tcsh script for an AFNI process stream

    This python script can generate a processing script via a command-line
    interface, with an optional question/answer session (-ask_me), or by a tk
    GUI (eventually).

    The user should provide at least the input datasets (-dsets) and stimulus
    files (-regress_stim_*), in order to create an output script.  See the
    'DEFAULTS' section for a description of the default options for each block.

    The output script, when executed will create a results directory, copy
    input files into it, and perform all processing there.  So the user can
    delete the results directory and re-run the script at their whim.

    Note that the user need not actually run the output script.  The user
    should feel free to modify the script for their own evil purposes, before
    running it.

    The text interface can be accessed via the -ask_me option.  It invokes a
    question & answer session, during which this program sets user options on
    the fly.  The user may elect to enter some of the options on the command
    line, even if using -ask_me.  See "-ask_me EXAMPLES", below.

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
    DEFAULTS: basic defaults for each block (not all defaults)

        setup:    - use 'SUBJ' for the subject id
                        (option: -subj_id SUBJ)
                  - create a t-shell script called 'proc_subj'
                        (option: -script proc_subj)
                  - use results directory 'SUBJ.results'
                        (option: -out_dir SUBJ.results)

        tcat:     - do not remove any of the first TRs

        empty:    - do nothing (just copy the data using 3dTcat)

        despike:  - NOTE: by default, this block is _not_ used
                  - automasking is not done (requires -despike_mask)

        ricor:    - NOTE: by default, this block is _not_ used
                  - polort based on twice the actual run length
                  - solver is OLSQ, not REML
                  - do not remove any first TRs from the regressors

        tshift:   - align slices to the beginning of the TR
                  - use quintic interpolation for time series resampling
                        (option: -tshift_interp -quintic)

        align:    - align the anatomy to match the EPI
                    (also required for the option of aligning EPI to anat)

        volreg:   - align to third volume of first run, -zpad 1
                        (option: -volreg_align_to third)
                        (option: -volreg_zpad 1)
                  - use cubic interpolation for volume resampling
                        (option: -volreg_interp -cubic)
                  - apply motion params as regressors across all runs at once
                  - do not align EPI to anat
                  - do not warp to standard space

        blur:     - blur data using a 4 mm FWHM filter with 3dmerge
                        (option: -blur_filter -1blur_fwhm)
                        (option: -blur_size 4)
                        (option: -blur_in_mask no)

        mask:     - create a union of masks from 3dAutomask on each run
                  - not applied in regression without -regress_apply_mask
                  - if possible, create a subject anatomy mask
                  - if possible, create a group anatomy mask (tlrc base)

        scale:    - scale each voxel to mean of 100, clip values at 200

        regress:  - use GAM regressor for each stim
                        (option: -regress_basis)
                  - compute the baseline polynomial degree, based on run length
                        (e.g. option: -regress_polort 2)
                  - do not censor large motion
                  - output fit time series
                  - output ideal curves for GAM/BLOCK regressors
                  - output iresp curves for non-GAM/non-BLOCK regressors

        tlrc:     - use TT_N27+tlrc as the base (-tlrc_base TT_N27+tlrc)
                  - no additional suffix (-tlrc_suffix NONE)

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

     ***********************************************************
     *  New and improved!  Examples that apply to AFNI_data4.  *
     ***********************************************************

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
                        -regress_make_ideal_sum sum_ideal.1D               \\
                        -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                        -regress_stim_labels tneg tpos tneu eneg epos      \\
                                             eneu fneg fpos fneu           \\
                        -regress_basis 'BLOCK(30,1)'                       \\
                        -regress_est_blur_epits                            \\
                        -regress_est_blur_errts                            \\
                        -regress_opts_3dD                                  \\
                            -gltsym 'SYM: +eneg -fneg'                     \\
                            -glt_label 1 eneg_vs_fneg                      \\
                            -gltsym 'SYM: 0.5*fneg 0.5*fpos -1.0*fneu'     \\
                            -glt_label 2 face_contrast                     \\
                            -gltsym 'SYM: tpos epos fpos -tneg -eneg -fneg'\\
                            -glt_label 3 pos_vs_neg

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

           The regression will use 198 regressors (all of "no interest"):

                 27 baseline  regressors ( 3 per run * 9 runs)
                 54 motion    regressors ( 6 per run * 9 runs)
                117 RETROICOR regressors (13 per run * 9 runs)

           To example #3, add -do_block, -ricor_* and -volreg_regress_per_run.

                afni_proc.py -subj_id sb23.e5a.ricor            \\
                        -dsets sb23/epi_r??+orig.HEAD           \\
                        -do_block despike ricor                 \\
                        -tcat_remove_first_trs 3                \\
                        -ricor_regs_nfirst 3                    \\
                        -ricor_regs sb23/RICOR/r*.slibase.1D    \\
                        -ricor_regress_method 'per-run'         \\
                        -volreg_regress_per_run

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
                        -regress_make_ideal_sum sum_ideal.1D               \\
                        -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                        -regress_stim_labels tneg tpos tneu eneg epos      \\
                                             eneu fneg fpos fneu           \\
                        -regress_basis 'BLOCK(30,1)'                       \\
                        -regress_est_blur_epits                            \\
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
                        -regress_make_ideal_sum sum_ideal.1D               \\
                        -regress_stim_times sb23/stim_files/blk_times.*.1D \\
                        -regress_stim_labels tneg tpos tneu eneg epos      \\
                                             eneu fneg fpos fneu           \\
                        -regress_basis 'BLOCK(30,1)'                       \\
                        -regress_censor_motion 1.0                         \\
                        -regress_est_blur_epits                            \\
                        -regress_est_blur_errts                            \\
                        -regress_opts_3dD                                  \\
                            -gltsym 'SYM: +eneg -fneg'                     \\
                            -glt_label 1 eneg_vs_fneg                      \\

           To process in orig space, remove -volreg_tlrc_warp.
           To apply manual tlrc transformation, use -volreg_tlrc_adwarp.
           To process as anat aligned to EPI, remove -volreg_align_e2a.

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

       This mask is considered necessary because the align/warp transformation
       that is applied on top of the volreg alignment transformation (applied
       at once), meaning the transformation from the EPI grid to the anatomy
       grid will vary per TR.

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

    --- masking, continued...

    Note that it may still not be a good idea to apply any of the masks to the
    regression, as it would then be necessary to intersect the masks across all
    subjects, though applying the 'group' mask might be reasonable.

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

     *  -ricor_regress_method   If across-runs, $volsperrun is not appropriate.

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

        ------------ information options ------------

        -help                   : show this help
        -hist                   : show the module history
        -show_valid_opts        : show all valid options (brief format)
        -ver                    : show the version number

        ------------ general execution and setup options ------------

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

                align   : after tlrc, before volreg
                despike : first (between tcat and tshift)
                empty   : NO DEFAULT, cannot be applied via -do_block
                ricor   : just after despike (else first)
                tlrc    : before volreg, after align

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

        -verb LEVEL             : specify the verbosity of this script

                e.g. -verb 2
                default: 1

            Print out extra information during execution.

        ------------ block options (in default block order) ------------

        These options pertain to individual processing blocks.  Each option
        starts with the block name.

        -tcat_remove_first_trs NUM : specify how many TRs to remove from runs

                e.g. -tcat_remove_first_trs 3
                default: 0

            Since it takes several seconds for the magnetization to reach a
            steady state (at the beginning of each run), the initial TRs of
            each run may have values that are significantly greater than the
            later ones.  This option is used to specify how many TRs to
            remove from the beginning of every run.

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

            After the regression block, run @auto_tlrc on the anatomical
            dataset provided by '-copy_anat'.  By default, warp the anat to
            align with TT_N27+tlrc, unless the '-tlrc_base' option is given.

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
            EPI and base used in align_epi_anat.py in the align block.  The
            user should apply sub-brick selection if the dataset has more than
            one volume.  This volume would be used for both the -epi and the
            -epi_base options in align_epi_anat.py.

            This user might want to align to an EPI volume that is not in the
            processing stream for the case where there is not sufficient EPI
            contrast left after the magnetization has reached a steady state.
            Perhaps volume 0 has sufficient contrast for alignment, but is not
            appropriate for analysis.  In such a case, the user may elect to
            align to volume 0, while excluding it from the analysis as part of
            the first volumes removed in -tcat_remove_first_trs.

            e.g. -dsets subj10/epi_r*_orig.HEAD
                 -tcat_remove_first_trs 3
                 -align_epi_ext_dset subj10/epi_r01+orig'[0]'
                 -volreg_align_to first

            Note that if the anatomy were acquired after the EPI, the user may
            want to still align it to the beginning of some run, and all the
            EPIs to a time point close to that.

            Please see "align_epi_anat.py -help" for more information.

        -align_opts_aea OPTS ... : specify extra options for align_epi_anat.py

                e.g. -align_opts_aea -big_move 
                e.g. -align_opts_aea -giant_move -AddEdge -epi_strip 3dAutomask

            This option allows the user to add extra options to the alignment
            command, align_epi_anat.py.

            Note that only one -align_opts_aea option should be given, with
            possibly many parameters to be passed on to align_epi_anat.py.

            Please see "align_epi_anat.py -help" for more information.

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

        -volreg_interp METHOD   : specify the interpolation method for volreg

                e.g. -volreg_interp -quintic
                e.g. -volreg_interp -Fourier
                default -cubic

            Please see '3dvolreg -help' for more information.

        -volreg_opts_vr OPTS ... : specify extra options for 3dvolreg

                e.g. -volreg_opts_vr -noclip -nomaxdisp

            This option allows the user to add extra options to the 3dvolreg
            command.  Note that only one -volreg_opts_vr should be applied,
            which may be used for multiple 3dvolreg options.

            Please see '3dvolreg -help' for more information.

        -volreg_regress_per_run : regress motion parameters from each run

                default: regress motion parameters catenated across runs

            By default, motion parameters from the volreg block are catenated
            across all runs, providing 6 (assuming 3dvolreg) regressors of no
            interest in the regression block.

            With -volreg_regress_per_run, the motion parameters from each run
            are used as separate regressors, providing a total of (6 * nruns)
            regressors.

            This allows for the magnitudes of the regressors to vary over each
            run, rather than using a single (best) magnitude over all runs.
            So more motion-correlated variance can be accounted for, at the
            cost of the extra degrees of freedom (6*(nruns-1)).

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
            by 3dmerge.  It is applied as the 'bmm' parameter in the filter
            option (such as -1blur_fwhm).

            Note the relationship between blur sizes, as used in 3dmerge:

                sigma = 0.57735027 * rms = 0.42466090 * fwhm
                (implying rms = 1.359556 * fwhm)

            Please see '3dmerge -help' for more information.
            See also -blur_filter.

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

        -regress_apply_mask     : apply the mask during scaling and regression

            By default, any created union mask is not applied to the analysis.
            Use this option to apply it.

         ** This option is essentially obsolete.  Please consider -mask_apply
            as a preferable option to choose which mask to apply.

            See "MASKING NOTE" and "DEFAULTS" for details.
            See also -blocks, -mask_apply.

        -regress_basis BASIS    : specify the regression basis function

                e.g. -regress_basis 'BLOCK(4,1)'
                e.g. -regress_basis 'BLOCK(5)'
                e.g. -regress_basis 'TENT(0,14,8)'
                default: GAM

            This option is used to set the basis function used by 3dDeconvolve
            in the regression step.  This basis function will be applied to
            all user-supplied regressors (please let me know if there is need
            to apply different basis functions to different regressors).

         ** Note that use of dmBLOCK requires -stim_times_AM1 (or AM2).  Until
            that is handled properly by afni_proc.py, users will need to edit
            the processing script, changing -stim_times to the appropriate
            _AM1 or _AM2.
        
            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004
            See also -regress_basis_normall, -regress_stim_times.

        -regress_basis_normall NORM : specify the magnitude of basis functions

                e.g. -regress_basis_normall 1.0

            This option is used to set the '-basis_normall' parameter in
            3dDeconvolve.  It specifies the height of each basis function.

            For the example basis functions, -basis_normall is not recommended.

            Please see '3dDeconvolve -help' for more information.
            See also -regress_basis.

        -regress_censor_motion LIMIT : censor TRs with excessive motion

                e.g. -regress_censor_motion 1.0

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

            Please see '1d_tool.py -help' for information on censoring motion.
            See also -regress_censor_prev.

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

            If the -regress_basis function is a single parameter function
            (either GAM or some form of BLOCK), then this option can be
            applied to create an ideal response curve which is the sum of
            the individual stimulus response curves.

            Use of this option will add a 3dTstat command to sum the regressor
            (of interest) columns of the 1D X-matrix, output by 3dDeconvolve.

            This is similar to the default behavior of creating ideal_STIM.1D
            files for each stimulus label, STIM.

            Please see '3dDeconvolve -help' and '3dTstat -help'.
            See also -regress_basis, -regress_no_ideals.

        -regress_motion_file FILE.1D  : use FILE.1D for motion parameters

                e.g. -regress_motion_file motion.1D

            Particularly if the user performs motion correction outside of
            afni_proc.py, they may wish to specify a motion parameter file
            other than dfile.rall.1D (the default generated in the volreg
            block).

            If the motion parameter file is in an external directory, the
            user should copy it via the -copy_files option.

            See also -copy_files.

        -regress_no_fitts       : do not supply -fitts to 3dDeconvolve

                e.g. -regress_no_fitts

            This option prevents the program from adding a -fitts option to
            the 3dDeconvolve command in the output script.

            See also -regress_fitts_prefix.

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

        -regress_opts_3dD OPTS ...   : specify extra options for 3dDeconvolve

                e.g. -regress_opts_3dD -gltsym ../contr/contrast1.txt  \\
                                       -glt_label 1 FACEvsDONUT        \\
                                       -xjpeg Xmat

            This option allows the user to add extra options to the 3dDeconvolve
            command.  Note that only one -regress_opts_3dD should be applied,
            which may be used for multiple 3dDeconvolve options.

            Please see '3dDeconvolve -help' for more information, or the link:
                http://afni.nimh.nih.gov/afni/doc/misc/3dDeconvolveSummer2004

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

        -regress_stim_labels LAB1 ...   : specify labels for stimulus types

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

            See also -regress_extra_stim_labels, -regress_RONI.

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

                e.g. -stim_times_offset 1.25
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

    - R Reynolds  Dec, 2006                             thanks to Z Saad
    ===========================================================================
"""
# end global help string
# ----------------------------------------------------------------------

