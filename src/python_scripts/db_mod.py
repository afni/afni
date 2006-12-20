import os, afni_util

# modify the tcat block options according to the user options
def db_mod_tcat(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-tcat_remove_first_trs', 1, [0], setpar=True)

    errs = 0

    uopt = user_opts.find_opt('-tcat_remove_first_trs')
    bopt = block.opts.find_opt('-tcat_remove_first_trs')
    if uopt and bopt:
        try: bopt.parlist[0] = int(uopt.parlist[0])
        except:
            print "** %s: invalid integer: %s" % (uopt.label, uopt.parlist[0])
            errs += 1

    if errs == 0: block.valid = 1
    else        : block.valid = 0

# do not rely on the form of input filenames
# use 3dtcat to copy each file to out_dir, then 'cd' into it
def db_cmd_tcat(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-tcat_remove_first_trs')
    first = opt.parlist[0]

    cmd = cmd + "# -------------------------------------------------------\n" \
              + "# apply 3dTcat to copy input dsets to results dir, while\n"  \
              + "# removing the first %d TRs\n" % first
    for run in range(0, proc.runs):
        cmd = cmd + "3dTcat -prefix %s/%s %s'[%d..$]'\n" %              \
                    (proc.out_dir, proc.prefix_form(block,run+1),
                     proc.dsets[run].rpv(), first)

    cmd = cmd + '\n# and enter the results directory\ncd %s\n\n' % proc.out_dir

    proc.reps   -= first        # update reps to account for removed TRs
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    if proc.verb > 0: print "-d %s: reps is now %d" % (block.label, proc.reps)

    return cmd

def db_mod_tshift(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-tshift_align_to', -1, ['-tzero', '0'], setpar=True)
        block.opts.add_opt('-tshift_interp', 1, ['-quintic'], setpar=True)
        block.opts.add_opt('-tshift_opts_ts', -1, [])

    # check for updates to -tshift_align_to option
    uopt = user_opts.find_opt('-tshift_align_to')
    bopt = block.opts.find_opt('-tshift_align_to')
    if uopt and bopt:
        bopt.parlist = uopt.parlist     # copy new params
        # warn the user about -regress_stim_times_offset
        if user_opts.find_opt('-regress_stim_files') and proc.verb > 0   \
           and not user_opts.find_opt('-regress_stim_times_offset'):
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
        block.opts.add_opt('-volreg_base_ind', 2, [0, 0], setpar=True)
        block.opts.add_opt('-volreg_opts_vr', -1, [])

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
        else:   # 'last' (if we don't know runs/reps yet, will have -1)
            bopt.parlist[0] = proc.runs - 1     # index of last dset
            bopt.parlist[1] = proc.reps - 1     # index of last rep

    uopt = user_opts.find_opt('-volreg_opts_vr')
    bopt = block.opts.find_opt('-volreg_opts_vr')
    if uopt and bopt: bopt.parlist = uopt.parlist

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

    # maybe there are extra options to append to the command
    opt = block.opts.find_opt('-volreg_opts_vr')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '             %s  \\\n' % ' '.join(opt.parlist)

    cmd = cmd + "# -------------------------------------------------------\n" \
                "# align each dset to the base volume\n"                      \
                "foreach run ( $runs )\n"                                     \
                "    3dvolreg -verbose -zpad 1 -base %s+orig'[%d]'  \\\n"     \
                "             -1Dfile dfile.r$run.1D -prefix %s  \\\n"        \
                "%s"                                                          \
                "             %s+orig\n"                                      \
                "end\n\n"                                                     \
                "# make a single file of registration params\n"               \
                "cat dfile.r??.1D > dfile.rall.1D\n\n" %                      \
                    (proc.prev_prefix_form(dset_ind+1), sub, 
                     proc.prefix_form_run(block), other_opts,
                     proc.prev_prefix_form_run())

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_blur(block, proc, user_opts):
    if len(block.opts.olist) == 0: # init blur option
        block.opts.add_opt('-blur_filter', 1, ['-1blur_fwhm'], setpar=True)
        block.opts.add_opt('-blur_size', 1, [4.0], setpar=True)
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
                "    3dmerge %s %d -doall -prefix %s   \\\n"                  \
                "%s"                                                          \
                "            %s+orig\n"                                       \
                "end\n\n" % (filter, size, prefix, other_opts, prev)

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    return cmd

def db_mod_mask(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-mask_type', 1, ['union'], setpar=True)
        block.opts.add_opt('-mask_dilate', 1, [1], setpar=True)

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

    cmd = cmd + "# get mean and compare it to %s for taking '%s'\n"      \
                "3dMean -datum short -prefix rm.mean rm.mask*.HEAD\n"    \
                "3dcalc -a rm.mean+orig -expr 'ispositive(a-%s)' "       \
                "-prefix full_mask\n\n" % (str(min), type, str(min))

    proc.mask = 'full_mask'     # note that we have a mask dataset to apply

    # do not increment block index or set 'previous' block label,
    # as there are no datasets created here

    return cmd

def db_mod_scale(block, proc, user_opts):     # no options at this time
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-scale_max_val', 1, [200], setpar=True)

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

    block.valid = 1

def db_cmd_scale(proc, block):
    cmd = ''
    # check for max scale value 
    opt = block.opts.find_opt('-scale_max_val')
    max = opt.parlist[0]
    valstr = 'a/b*100'
    if max > 100: maxstr = ' * step(%d-%s) + %d*step(%s-%d)' \
                           % (max,valstr,max,valstr,max-1)
    else:         maxstr = ''

    if proc.mask:
        mask_dset = '           -c %s+orig \\\n' % proc.mask
        expr      = 'c*(a/b*100%s)' % maxstr
    else:
        mask_dset = ''
        expr      = 'a/b*100%s' % maxstr

    if max > 100: maxstr = ', subject to maximum value of %d' % max
    else        : maxstr = ''

    prev = proc.prev_prefix_form_run()
    prefix = proc.prefix_form_run(block)
    cmd = cmd + "# -------------------------------------------------------\n" \
                "# create a scaled dataset for each run%s\n"                  \
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
        block.opts.add_opt('-regress_basis', 1, ['GAM'], setpar=True)
        block.opts.add_opt('-regress_basis_normall', 1, [1], setpar=True)
        block.opts.add_opt('-regress_polort', 1, [2], setpar=True)
        block.opts.add_opt('-regress_stim_files', -1, [])
        block.opts.add_opt('-regress_stim_labels', -1, [])
        block.opts.add_opt('-regress_stim_times', -1, [])
        block.opts.add_opt('-regress_stim_times_offset', 1, [0], setpar=True)

        block.opts.add_opt('-regress_opts_3dD', -1, [])
        block.opts.add_opt('-regress_make_ideal_sum', 1, [])
        block.opts.add_opt('-regress_fitts_prefix', 1, ['fitts'], setpar=True)

    errs = 0  # allow errors to accumulate

    # check for user updates
    uopt = user_opts.find_opt('-regress_basis')
    bopt = block.opts.find_opt('-regress_basis')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]
        if bopt.parlist[0] != 'GAM': # then default to -iresp
            block.opts.add_opt('-regress_iresp_prefix',1,['iresp'],setpar=True)
        # check on GAM/BLOCK for -regress_make_ideal_sum
        uopt = user_opts.find_opt('-regress_make_ideal_sum')
        if uopt and not afni_util.basis_has_known_response(bopt.parlist[0]):
            print '** -regress_make_ideal_sum is inappropriate for basis %s'\
                  % bopt.parlist[0]
            errs += 1

    uopt = user_opts.find_opt('-regress_basis_normall')
    bopt = block.opts.find_opt('-regress_basis_normall')
    if uopt and bopt:
        try: bopt.parlist[0] = float(uopt.parlist[0])
        except:
            print "** -regress_basis_normall requires float param (have '%s')" \
                  % uopt.parlist[0]
            errs += 1

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
    if uopt and bopt:  # check the length once we know the runs
        bopt.parlist = uopt.parlist

    # times is one file per class
    uopt = user_opts.find_opt('-regress_stim_times')
    bopt = block.opts.find_opt('-regress_stim_times')
    if uopt and bopt:
        if len(uopt.parlist) <= 0:
            print "** no files for -regress_stim_times?"
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
    if len(proc.stims_orig) == 0:
        print "** missing stim files (-regress_stim_times/-regress_stim_files)"
        errs += 1
    # create local names for stim files
    proc.stims = []
    for file in proc.stims_orig:
        proc.stims.append('stimuli/%s' % os.path.basename(file))

    # check for fitts prefix
    uopt = user_opts.find_opt('-regress_fitts_prefix')
    bopt = block.opts.find_opt('-regress_fitts_prefix')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]
    elif not bopt: # maybe it was deleted previously (not currently possible)
        block.opts.add_opt('-regress_fitts_prefix', 1, uopt.parlist,setpar=True)

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
        block.opts.add_opt('-regress_iresp_prefix', 1, uopt.parlist,setpar=True)

    # maybe the user does not want default ideals
    uopt = user_opts.find_opt('-regress_no_ideals')
    bopt = block.opts.find_opt('-regress_no_ideals')
    if uopt and not bopt: block.opts.add_opt('-regress_no_ideals',0,[])

    # maybe the user wants to delete it
    uopt = user_opts.find_opt('-regress_no_iresp')
    bopt = block.opts.find_opt('-regress_iresp_prefix')
    if uopt and bopt: block.opts.del_opt('-regress_iresp_prefix')

    # prepare to return
    if errs > 0:
        block.valid = 0
        return 1

    block.valid = 1

# here we need to concatenate the dfiles, and possible create stim_times files
#
# without stim_times, use stim_files to generate stim_times
# without stim_labels, use stim_times to create labels
def db_cmd_regress(proc, block):
    cmd = ''
    opt = block.opts.find_opt('-regress_basis')
    basis = opt.parlist[0]

    opt = block.opts.find_opt('-regress_basis_normall')
    normall = opt.parlist[0]

    opt = block.opts.find_opt('-regress_polort')
    polort = opt.parlist[0]

    if len(proc.stims) <= 0:   # be sure we have some stim files
        print "** cmd_regress: no stim files"
        return

    cmd = cmd + "# -------------------------------------------------------\n" \
                "# run the regression analysis\n"

    opt = block.opts.find_opt('-regress_stim_times')
    if not opt.parlist or len(opt.parlist) == 0:
        newcmd = db_cmd_regress_sfiles2times(proc, block)
        if not newcmd: return
        cmd = cmd + newcmd
    else: stim_times = opt.parlist

    if proc.mask: mask = '    -mask %s+orig  \\\n' % proc.mask
    else        : mask = ''

    cmd = cmd + '3dDeconvolve -input %s+orig.HEAD    \\\n'      \
                '    -polort %d  \\\n'                          \
                '%s'                                            \
                '    -basis_normall %s  \\\n'                   \
                '    -num_stimts %d  \\\n'                      \
                % ( proc.prev_prefix_form_rwild(), polort, mask, str(normall),
                    len(proc.stims)+6 )  # +6 for motion params

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
            iresp = iresp + "    -iresp %d %s_%s  \\\n" % \
                            (index+1, opt.parlist[0], labels[index])

    # write out stim lines
    for ind in range(len(proc.stims)):
        cmd = cmd + "    -stim_times %d %s '%s'  \\\n"  \
                    "    -stim_label %d %s  \\\n"     \
                    % (ind+1, proc.stims[ind], basis, ind+1, labels[ind])

    # write out registration param lines
    mot_labs = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']
    first = len(proc.stims) + 1 # first stim index
    for ind in range(6):
        cmd = cmd + "    -stim_file %d dfile.rall.1D'[%d]' "  \
                    "-stim_base %d -stim_label %d %s  \\\n"   \
                    % (ind+first, ind, ind+first, ind+first, mot_labs[ind])

    # see if the user wants the fit time series
    opt = block.opts.find_opt('-regress_fitts_prefix')
    if not opt or not opt.parlist: fitts = ''
    else: fitts = '    -fitts %s  \\\n' % opt.parlist[0]

    # see if the user has provided other options (like GLTs)
    opt = block.opts.find_opt('-regress_opts_3dD')
    if not opt or not opt.parlist: other_opts = ''
    else: other_opts = '    %s  \\\n' %         \
                       ' '.join(afni_util.quotize_list(opt.parlist))

    # add misc options
    cmd = cmd + iresp
    cmd = cmd + other_opts
    cmd = cmd + "    -fout -tout -full_first -x1D Xmat.1D  \\\n"
    cmd = cmd + fitts
    cmd = cmd + "    -bucket stats.$subj\n\n"

    if fitts != '':
        cmd = cmd + "\n# create an all_runs dataset to match the fitts\n"
        cmd = cmd + "3dTcat -prefix all_runs %s+orig.HEAD\n\n" % \
                    proc.prev_prefix_form_rwild()

    opt = block.opts.find_opt('-regress_no_ideals')
    if not opt: # then we compute individual ideal files for each stim
        cmd = cmd + "\n# create ideal files for each stim type\n"
        first = (polort+1) * proc.runs
        for ind in range(len(labels)):
            cmd = cmd + "1dcat Xmat.1D'[%d]' > ideal_%s.1D\n" % \
                        (first+ind, labels[ind])

    opt = block.opts.find_opt('-regress_make_ideal_sum')
    if opt and opt.parlist:
        first = (polort+1) * proc.runs
        last = first + len(proc.stims) - 1
        cmd = cmd + "\n# create ideal file by adding ideal regressors\n"
        cmd = cmd + "3dTstat -sum -prefix %s Xmat.1D'[%d..%d]'\n\n" % \
                    (opt.parlist[0], first, last)

    proc.pblabel = block.label  # set 'previous' block label

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
        cols += afni_util.num_cols_1D(file)         # tally the number of cols
    cmd = cmd + '\n\n'

    # and reset proc.stims to the new file list (which is 1-based)
    proc.stims = []
    for ind in range(1,cols+1):
        proc.stims.append('stimuli/stim_times.%02d.1D' % ind)

    if proc.verb > 0: print '+d new stim list: %s' % proc.stims

    return cmd

