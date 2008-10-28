#!/usr/bin/env python

import math, os
import afni_base as BASE, afni_util as UTIL

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
            print "** %s: invalid integer: %s" % (uopt.label, uopt.parlist[0])
            errs += 1
        if errs == 0 and bopt.parlist[0] > 0:
          print                                                              \
            '-----------------------------------------------------------\n'  \
            'warning: removing first %d TRs from beginning of each run\n'    \
            '   --> it is essential that stimulus timing files match the\n'  \
            '       removal of these TRs\n'                                  \
            '-----------------------------------------------------------'    \
            % bopt.parlist[0]

    if errs == 0: block.valid = 1
    else        : block.valid = 0

# do not rely on the form of input filenames
# use 3dtcat to copy each file to od_var, then 'cd' into it
def db_cmd_tcat(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-tcat_remove_first_trs')
    first = opt.parlist[0]

    cmd = cmd + "# -------------------------------------------------------\n" \
              + "# apply 3dTcat to copy input dsets to results dir, while\n"  \
              + "# removing the first %d TRs\n" % first
    for run in range(0, proc.runs):
        cmd = cmd + "3dTcat -prefix %s/%s %s'[%d..$]'\n" %              \
                    (proc.od_var, proc.prefix_form(block,run+1),
                     proc.dsets[run].rpv(), first)

    cmd = cmd + '\n# and enter the results directory\ncd %s\n\n' % proc.od_var

    proc.reps   -= first        # update reps to account for removed TRs
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    if proc.verb > 0: print "-d %s: reps is now %d" % (block.label, proc.reps)

    return cmd

# --------------- despike ---------------

def db_mod_despike(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-despike_opts_3dDes', -1, [])

    uopt = user_opts.find_opt('-despike_opts_3dDes')
    bopt = block.opts.find_opt('-despike_opts_3dDes')
    bopt = block.opts.find_opt('-despike_opts_3dDes')
    if uopt and bopt: bopt.parlist = uopt.parlist

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
    prev   = proc.prev_prefix_form_run()

    # write commands
    cmd = cmd + '# -------------------------------------------------------\n' \
              + '# apply 3dDespike to each run\n'
    cmd = cmd + 'foreach run ( $runs )\n'                                     \
                '    3dDespike%s -prefix %s %s+orig\n'                        \
                'end\n\n' %                                                   \
                (other_opts, prefix, prev)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

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
    prev_prefix = proc.prev_prefix_form_run()

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-tshift_opts_ts')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '             %s  \\\n' % ' '.join(opt.parlist)

    # write commands
    cmd = cmd + '# -------------------------------------------------------\n' \
              + '# run 3dToutcount and 3dTshift for each run\n'
    cmd = cmd + 'foreach run ( $runs )\n'                                     \
                '    3dToutcount -automask %s+orig > outcount_r$run.1D\n'     \
                '\n'                                                          \
                '    3dTshift %s %s -prefix %s      \\\n'                     \
                '%s'                                                          \
                '             %s+orig\n'                                      \
                'end\n\n' %                                                   \
                (prev_prefix, align_to, resam,cur_prefix,other_opts,prev_prefix)
    
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_volreg(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init dset/brick indices to defaults
        block.opts.add_opt('-volreg_base_ind', 2, [0, 2], setpar=1)
        block.opts.add_opt('-volreg_interp', 1, ['-cubic'], setpar=1)
        block.opts.add_opt('-volreg_opts_vr', -1, [])
        block.opts.add_opt('-volreg_zpad', 1, [1], setpar=1)

    # check for updates to -volreg_base option
    uopt = user_opts.find_opt('-volreg_base_ind')
    bopt = block.opts.find_opt('-volreg_base_ind')
    aopt = user_opts.find_opt('-volreg_align_to')
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
            # (if we don't know runs/reps yet, will have -1, which is okay)
            bopt.parlist[0] = proc.runs - 1     # index of last dset
            bopt.parlist[1] = proc.reps - 1     # index of last rep
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

    block.valid = 1

def db_cmd_volreg(proc, block):
    cmd = ''
    # get the base options
    opt = block.opts.find_opt('-volreg_base_ind')
    dset_ind = opt.parlist[0]
    sub      = opt.parlist[1]

    if dset_ind == -1: dset_ind = proc.runs - 1  # may need updates
    if sub      == -1: sub      = proc.reps - 1

    if proc.verb > 0:
        print "-d %s: base/sub indices are %d, %d" % (block.label,dset_ind,sub)

    # get base prefix (run is index+1)
    base = proc.prev_prefix_form(dset_ind+1)

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
    else: other_opts = '             %s  \\\n' % ' '.join(opt.parlist)

    cmd = cmd + "# -------------------------------------------------------\n" \
                "# align each dset to the base volume\n"                      \
                "foreach run ( $runs )\n"                                     \
                "    3dvolreg -verbose -zpad %d -base %s+orig'[%d]'  \\\n"    \
                "             -1Dfile dfile.r$run.1D -prefix %s  \\\n"        \
                "             %s \\\n"                                        \
                "%s"                                                          \
                "             %s+orig\n"                                      \
                "end\n\n"                                                     \
                "# make a single file of registration params\n"               \
                "cat dfile.r??.1D > dfile.rall.1D\n\n" %                      \
                    (zpad, proc.prev_prefix_form(dset_ind+1), sub, 
                     proc.prefix_form_run(block), resam, other_opts,
                     proc.prev_prefix_form_run())

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
    prev   = proc.prev_prefix_form_run()

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-blur_opts_merge')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '             %s  \\\n' % ' '.join(opt.parlist)

    cmd = cmd + "# -------------------------------------------------------\n" \
                "# blur each volume\n"                                        \
                "foreach run ( $runs )\n"                                     \
                "    3dmerge %s %s -doall -prefix %s   \\\n"                  \
                "%s"                                                          \
                "            %s+orig\n"                                       \
                "end\n\n" % (filter, str(size), prefix, other_opts, prev)

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

    block.valid = 1

def db_cmd_mask(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-mask_type')
    type = opt.parlist[0]
    if type == 'union': min = 0            # result must be greater than min
    else:               min = 0.999

    opt = block.opts.find_opt('-mask_dilate')
    nsteps = opt.parlist[0]

    prev = proc.prev_prefix_form_run()
    cmd = cmd + "# -------------------------------------------------------\n" \
                "# create 'full_mask' dataset (%s mask)\n"                    \
                "foreach run ( $runs )\n"                                     \
                "    3dAutomask -dilate %d -prefix rm.mask_r$run %s+orig\n"   \
                "end\n\n" % (type, nsteps, prev)

    if proc.runs > 1:  # if more than 1 run, create union mask
        cmd = cmd + "# get mean and compare it to %s for taking '%s'\n"      \
                    "3dMean -datum short -prefix rm.mean rm.mask*.HEAD\n"    \
                    "3dcalc -a rm.mean+orig -expr 'ispositive(a-%s)' "       \
                    "-prefix full_mask.$subj\n\n" % (str(min), type, str(min))
    else:  # just copy the one
        cmd = cmd + "# only 1 run, so copy this to full_mask\n"              \
                    "3dcopy rm.mask_r01+orig full_mask.$subj\n\n" 

    proc.mask = 'full_mask.$subj'  # note that we have a mask to apply

    # do not increment block index or set 'previous' block label,
    # as there are no datasets created here

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
    if max > 100: valstr = 'min(%d, a/b*100)' % max
    else:         valstr = 'a/b*100'

    if proc.mask and proc.regmask:
        mask_dset = '           -c %s+orig \\\n' % proc.mask
        expr      = 'c * %s' % valstr
    else:
        mask_dset = ''
        expr      = valstr

    if max > 100: maxstr = '# (subject to maximum value of %d)\n' % max
    else        : maxstr = ''

    prev = proc.prev_prefix_form_run()
    prefix = proc.prefix_form_run(block)
    cmd = cmd + "# -------------------------------------------------------\n" \
                "# scale each voxel time series to have a mean of 100\n"      \
                "%s"                                                          \
                "foreach run ( $runs )\n"                                     \
                "    3dTstat -prefix rm.mean_r$run %s+orig\n"                 \
                "    3dcalc -a %s+orig -b rm.mean_r$run+orig  \\\n"           \
                "%s"                                                          \
                "           -expr '%s'  \\\n"                                 \
                "           -prefix %s\n"                                     \
                "end\n\n" % (maxstr, prev, prev, mask_dset, expr, prefix)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_regress(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-regress_basis', 1, ['GAM'], setpar=1)
        block.opts.add_opt('-regress_polort', 1, [-1], setpar=1)
        block.opts.add_opt('-regress_stim_files', -1, [])
        block.opts.add_opt('-regress_stim_labels', -1, [])
        block.opts.add_opt('-regress_stim_times', -1, [])
        block.opts.add_opt('-regress_stim_times_offset', 1, [0], setpar=1)

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
        if not UTIL.basis_has_known_response(bopt.parlist[0]):
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
    # if we are here, then we should have stimulus files
    if len(proc.stims_orig) > 0:
        # create local names for stim files
        proc.stims = []
        for file in proc.stims_orig:
            proc.stims.append('stimuli/%s' % os.path.basename(file))

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
    elif not bopt: # maybe it was deleted previously (not currently possible)
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
    if uopt: proc.regmask = 0

    # maybe the user does not want to regress the motion parameters
    # apply uopt to bopt
    uopt = user_opts.find_opt('-regress_no_motion')
    bopt = block.opts.find_opt('-regress_no_motion')
    if uopt and not bopt: block.opts.add_opt('-regress_no_motion',0,[])
    elif not uopt and bopt: block.opts.del_opt('-regress_no_motion',0,[])

    # maybe the user wants to specify a motion file
    uopt = user_opts.find_opt('-regress_motion_file')
    if uopt:  # and make sure we have labels
        proc.mot_file = uopt.parlist[0]
        proc.mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']

    # maybe the user does not want to convert stim_files to stim_times
    uopt = user_opts.find_opt('-regress_use_stim_files')
    if not uopt: uopt = user_opts.find_opt('-regress_no_stim_times')
    bopt = block.opts.find_opt('-regress_no_stim_times')
    if uopt and not bopt:
        if proc.verb > 0: print '-d will use -stim_files in 3dDeconvolve'
        block.opts.add_opt('-regress_no_stim_times',0,[],setpar=1)

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
    if opt: normall = '    -basis_normall %s  \\\n' % opt.parlist[0]
    else:   normall = ''

    opt = block.opts.find_opt('-regress_no_motion')
    if opt: proc.mot_labs = []   # then clear any motion labels

    opt = block.opts.find_opt('-regress_polort')
    polort = opt.parlist[0]
    if ( polort < 0 ) :
        polort = UTIL.get_default_polort(proc.tr, proc.reps)
        if proc.verb > 0:
            print "+d updating polort to %d, from run len %.1f s" %  \
                  (polort, proc.tr*proc.reps)

    if len(proc.stims) <= 0:   # be sure we have some stim files
        print "** missing stim files (-regress_stim_times/-regress_stim_files)"
        block.valid = 0
        return

    cmd = cmd + "# -------------------------------------------------------\n" \
                "# run the regression analysis\n"

    # possibly add a make_stim_times.py command
    opt = block.opts.find_opt('-regress_stim_times')
    use_times = (block.opts.find_opt('-regress_no_stim_times') == None)
    if use_times and (not opt.parlist or len(opt.parlist) == 0):
        newcmd = db_cmd_regress_sfiles2times(proc, block)
        if not newcmd: return
        cmd = cmd + newcmd

    if proc.mask and proc.regmask: mask = '    -mask %s+orig  \\\n' % proc.mask
    else                         : mask = ''

    cmd = cmd + '3dDeconvolve -input %s+orig.HEAD    \\\n'      \
                '    -polort %d  \\\n'                          \
                '%s%s'                                          \
                '    -num_stimts %d  \\\n'                      \
                % ( proc.prev_prefix_form_rwild(), polort,
                    mask, normall,
                    len(proc.stims)+len(proc.mot_labs) )

    # verify labels (now that we know the list of stimulus files)
    opt = block.opts.find_opt('-regress_stim_labels')
    if not opt or not opt.parlist:
        labels = []
        for ind in range(len(proc.stims)):
            labels.append('stim%02d' % (ind+1))
        if proc.verb > 0: print ('+d adding labels: %s' % labels)
    elif len(proc.stims) != len(opt.parlist):
        print "** cmd_regress: have %d stims but %d labels" % \
              (len(proc.stims), len(opt.parlist))
        return
    else:  # we have the same number of labels as stims
        labels = opt.parlist

    # we need labels for iresp
    opt = block.opts.find_opt('-regress_iresp_prefix')
    if not opt or not opt.parlist: iresp = ''
    else:
        iresp = ''
        for index in range(len(labels)):
            iresp = iresp + "    -iresp %d %s_%s.$subj  \\\n" % \
                            (index+1, opt.parlist[0], labels[index])

    # write out stim lines
    sfiles = block.opts.find_opt('-regress_no_stim_times')
    for ind in range(len(proc.stims)):
        if sfiles:  # then -stim_file and no basis function
            cmd = cmd + "    -stim_file %d %s  \\\n" % (ind+1,proc.stims[ind])
        else:
            cmd = cmd + "    -stim_times %d %s '%s'  \\\n"  % \
                        (ind+1, proc.stims[ind], basis)
        # and add the label
        cmd = cmd +  "    -stim_label %d %s  \\\n" % (ind+1, labels[ind])

    # write out registration param lines
    if len(proc.mot_labs) > 0:
        first = len(proc.stims) + 1 # first stim index
        for ind in range(len(proc.mot_labs)):
            cmd = cmd + "    -stim_file %d %s'[%d]' "           \
                        "-stim_base %d -stim_label %d %s  \\\n" \
                % (ind+first, proc.mot_file, ind, ind+first, ind+first,
                   proc.mot_labs[ind])

    # see if the user wants the fit time series
    opt = block.opts.find_opt('-regress_fitts_prefix')
    if not opt or not opt.parlist: fitts = ''
    else: fitts = '    -fitts %s  \\\n' % opt.parlist[0]

    # -- see if the user wants the error time series --
    opt = block.opts.find_opt('-regress_errts_prefix')
    bluropt = block.opts.find_opt('-regress_est_blur_errts')
    # if there is no errts prefix, but the user wants to measure blur, add one
    if not opt.parlist and bluropt:
        opt.parlist = ['errts.$subj']

    if not opt or not opt.parlist: errts = ''
    else: errts = '    -errts %s  \\\n' % opt.parlist[0]
    # -- end errts --

    # see if the user has provided other options (like GLTs)
    opt = block.opts.find_opt('-regress_opts_3dD')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '    %s  \\\n' %         \
               ' '.join(UTIL.quotize_list(opt.parlist, '\\\n    ', 1))

    # add misc options
    cmd = cmd + iresp
    cmd = cmd + other_opts
    cmd = cmd + "    -fout -tout -x1D X.xmat.1D -xjpeg X.jpg \\\n"
    cmd = cmd + fitts + errts
    cmd = cmd + "    -bucket stats.$subj\n\n\n"

    # if 3dDeconvolve fails, terminate the script
    cmd = cmd + "# if 3dDeconvolve fails, terminate the script\n"       \
                "if ( $status != 0 ) then\n"                            \
                "    echo '---------------------------------------'\n"  \
                "    echo '** 3dDeconvolve error, failing...'\n"        \
                "    echo '   (consider the file 3dDeconvolve.err)'\n"  \
                "    exit\n"                                            \
                "endif\n\n\n"

    # create all_runs, and store name for blur_est
    all_runs = 'all_runs.$subj'
    cmd = cmd + "# create an all_runs dataset to match the fitts, errts, etc.\n"
    cmd = cmd + "3dTcat -prefix %s %s+orig.HEAD\n\n" % \
                (all_runs, proc.prev_prefix_form_rwild())

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

# might estimate blur from either all_runs or errts
# need mask (from mask, or automask from all_runs)
# requires 'all_runs' dataset, in case a mask is needed
#
# return None to fail out
def db_cmd_blur_est(proc, block):
    cmd = ''
    aopt = block.opts.find_opt('-regress_est_blur_epits')
    eopt = block.opts.find_opt('-regress_est_blur_errts')
    if not aopt and not eopt:
        if proc.verb > 2: print '-d no blur estimation'
        return cmd

    # set the mask (if we don't have one, bail)
    if proc.mask: mask = '-mask %s+orig' % proc.mask
    else:
        print '** refusing to estimate blur without a mask dataset'
        print '   (perhaps keep the mask block and apply -regress_no_mask)'
        return

    if proc.verb > 1: print 'computing blur_estimates'
    blur_file = 'blur_est.$subj.1D'


    # call this a new sub-block
    cmd = cmd + '# -------------------------\n'                 \
                '# compute blur estimates\n'                    \
                'touch %s   # start with empty file\n\n' % blur_file


    if aopt:    # get average across all runs
        prev = proc.prev_prefix_form_run()
        cmd = cmd + '# estimate blur for each run\n'                         \
                    'touch blur.EPI.1D \n'                                   \
                    'foreach run ( $runs )\n'                                \
                    '    3dFWHMx -detrend %s %s+orig >> blur.EPI.1D\n'       \
                    'end\n\n' % (mask, prev)
        cmd = cmd +                                                          \
                '# compute average blur, and append\n'                       \
                'set blurs = ( `3dTstat -mean -prefix - blur.EPI.1D\\\'` )\n'\
                'echo average EPI blurs: $blurs\n'                           \
                'echo "$blurs   # EPI blur estimates" >> %s\n\n'         %   \
                blur_file

    if eopt:    # get errts blur estimate
        etsopt = block.opts.find_opt('-regress_errts_prefix')
        errts = etsopt.parlist[0] + '+orig'
        if not etsopt or not etsopt.parlist:
            print '** want est_blur_errts, but have no errts_prefix'
            return

        cmd = cmd + '# estimate blur for each run in errts\n'           \
                    'touch blur.errts.1D\n\n'
        cmd = cmd + 'set b0 = 0\n'                                      \
                    'set b1 = %d    # nreps-1\n' % (proc.reps-1)
        cmd = cmd + 'foreach run ( $runs )\n'                           \
                    '    3dFWHMx -detrend %s %s"[$b0..$b1]" \\\n'       \
                    '        >> blur.errts.1D\n'                        \
                    '    @ b0 += %d   # add nreps\n'                    \
                    '    @ b1 += %d\n'                                  \
                    'end\n\n' % (mask, errts, proc.reps, proc.reps)

        cmd = cmd +                                                          \
                '\n# compute average blur, and append\n'                     \
                'set blurs = ( `3dTstat -mean -prefix - blur.errts.1D\\\'` )\n'\
                'echo average errts blurs: $blurs\n'                         \
                'echo "$blurs   # errts blur estimates" >> %s\n\n'       %   \
                blur_file

    return cmd

# convert a stim_files list into a stim_times list
def db_cmd_regress_sfiles2times(proc, block):

    # check for a stimulus timing offset
    opt = block.opts.find_opt('-regress_stim_times_offset')
    if opt and opt.parlist and opt.parlist[0] != 0:
        off_cmd = '                   -offset %s  \\\n' % str(opt.parlist[0])
    else: off_cmd = ''

    cmd = ''
    if proc.verb > 0: print '-d old stim list: %s' % proc.stims

    cmd = cmd + '\n# create -stim_times files\n'
    cmd = cmd + 'make_stim_times.py -prefix stim_times -tr %s -nruns %d'       \
                ' -nt %d  \\\n'                                                \
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

    if proc.verb > 0: print '+d new stim list: %s' % proc.stims

    return cmd

# verify consistency of -tlrc_* options
# return 1 or 0
def db_tlrc_opts_okay(opts):
    opta = opts.find_opt('-tlrc_anat')

    if not opta:
        if opts.find_opt('-tlrc_base'):
            print '** -tlrc_base requires dataset via -tlrc_anat'
            return 0
        if opts.find_opt('-tlrc_no_ss'):
            print '** -tlrc_no_ss requires dataset via -tlrc_anat'
            return 0
        if opts.find_opt('-tlrc_rmode'):
            print '** -tlrc_rmode requires dataset via -tlrc_anat'
            return 0
        if opts.find_opt('-tlrc_suffix'):
            print '** -tlrc_rmode requires dataset via -tlrc_anat'
            return 0

        return 1  # okay, no options

    opt_anat = opts.find_opt('-copy_anat')
    if not opt_anat:
        print '** -tlrc_anat option requires anatomy via -copy_anat'
        return 0

    dset = BASE.afni_name(opt_anat.parlist[0])
    if not dset.exist():  # allow for no +view
        dset = BASE.afni_name(opt_anat.parlist[0]+'+orig')
        if not dset.exist():
            print "** -tlrc_anat dataset '%s' does not exist" % \
                  opt_anat.parlist[0]
            return 0

    # base image does not need to exist (might be in abin)

    return 1

# create a command to run @auto_tlrc
def db_cmd_tlrc(dname, options):
    if not dname : # should include +orig
        print "** missing dataset name for tlrc operation"
        return None

    dset = BASE.afni_name(dname)   # allow for no +view
    if not dset.exist():
        dname = dname + '+orig'

    opt = options.find_opt('-tlrc_base')
    if opt: base = opt.parlist[0]
    else:   base = 'TT_N27+tlrc'

    opt = options.find_opt('-tlrc_no_ss')
    if opt: ss = ' -no_ss'
    else:   ss = ''

    opt = options.find_opt('-tlrc_rmode')
    if opt: rmode = ' -rmode %s' % opt.parlist[0]
    else:   rmode = ''

    opt = options.find_opt('-tlrc_suffix')
    if opt: suffix = ' -suffix %s' % opt.parlist[0]
    else:   suffix = ' -suffix NONE'     # make NONE the default

    cmd = "# -------------------------------------------------------\n" \
          "# run @auto_tlrc to warp '%s' to match template '%s'\n"      \
          "@auto_tlrc -base %s -input %s%s%s%s\n\n"                     \
          % (dname, base,base, dname, ss, rmode, suffix)

    return cmd

# currently nothing to verify for an 'empty' command (placeholder command)
# just return 1
def db_mod_empty(block, proc, user_opts):
    block.valid = 1
    return 1

# create a placeholder command using 3dTcat to copy the EPI data
def db_cmd_empty(proc, block):
    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run()

    cmd = "# -------------------------------------------------------\n" \
          "# empty block: use '3dTcat' as a placeholder command\n"      \
          "foreach run ( $runs )\n"                                     \
          "    3dTcat -prefix %s %s+orig\n"                             \
          "end\n\n" % (prefix, prev)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

# create a gen_epi_review.py command
def db_cmd_gen_review(proc):
    if not proc.gen_review: return None

    tblk = proc.find_block('tcat')

    cmd = "# -------------------------------------------------------\n" \
          "# generate a review script for the unprocessed EPI data\n"   \
          "gen_epi_review.py -script %s \\\n"                           \
          "    -dsets %s\n\n" % (proc.gen_review, proc.dset_form_wild('tcat'))

    return cmd

# dummy main - should not be used
if __name__ == '__main__':

    print '** this file is not to be used as a main program **'

