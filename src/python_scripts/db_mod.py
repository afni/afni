import glob, afni_util

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
    opt = block.opts.find_opt('-tcat_remove_first_trs')
    first = opt.parlist[0]

    if proc.verb > 2: block.show('+d db_cmd_tcat: ')

    proc.fp.write("# -------------------------------------------------------\n"
                  "# apply 3dTcat to copy input dsets to results dir, while\n"
                  "# removing the first %d TRs\n" % first)
    for run in range(0, proc.runs):
        proc.fp.write("3dTcat -prefix %s/%s %s'[%d..$]'\n" %    \
           (proc.out_dir, proc.prefix_form(block,run+1),        \
            proc.dsets[run].rpv(), first))

    proc.fp.write('\n# and enter the results directory\n'
                  'cd %s\n\n' % proc.out_dir)

    proc.reps   -= first        # update reps to account for removed TRs
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

    if proc.verb > 1: print "+d %s: reps is now %d" % (block.label, proc.reps)
        
    return None

def db_mod_tshift(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init to defaults
        block.opts.add_opt('-tshift_align_to', -1, ['-tzero', '0'], setpar=True)
        block.opts.add_opt('-tshift_resam', 1, ['-quintic'], setpar=True)

    # check for updates to -tshift_align_to option
    uopt = user_opts.find_opt('-tshift_align_to')
    bopt = block.opts.find_opt('-tshift_align_to')
    if uopt and bopt:
        bopt.parlist = uopt.parlist     # copy new params

    # check for updates to -tshift_resam option
    uopt = user_opts.find_opt('-tshift_resam')
    bopt = block.opts.find_opt('-tshift_resam')
    if uopt and bopt:
        bopt.parlist = uopt.parlist     # copy new params

    block.valid = 1

# run 3dToutcount and 3dTshift for each run
def db_cmd_tshift(proc, block):
    # get the base options
    opt = block.opts.find_opt('-tshift_align_to')
    align_to = ' '.join(opt.parlist)  # maybe '-tzero 0'
    opt = block.opts.find_opt('-tshift_resam')
    resam = ' '.join(opt.parlist)     # maybe '-quintic'

    # note cur and prev prefix forms (with $run)
    cur_prefix = proc.prefix_form_run(block)
    prev_prefix = proc.prev_prefix_form_run()

    # write commands
    proc.fp.write('# -------------------------------------------------------\n'
                  '# run 3dToutcount and 3dTshift for each run\n')
    proc.fp.write('foreach run ( $runs )\n'
                  '    3dToutcount -automask %s+orig > toutcount_r$run.1D\n'
                  '\n'
                  '    3dTshift %s %s -prefix %s      \\\n'
                  '             %s+orig\n'
                  'end\n\n' %     \
                  (prev_prefix, align_to, resam, cur_prefix, prev_prefix))
    
    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

def db_mod_volreg(block, proc, user_opts):
    if len(block.opts.olist) == 0:    # then init dset/brick indices to defaults
        block.opts.add_opt('-volreg_base_ind', 2, [0, 0], setpar=True)

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
        try: bopt.parlist[0] = int(uopt.parlist[0])
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

    block.valid = 1

def db_cmd_volreg(proc, block):
    # get the base options
    opt = block.opts.find_opt('-volreg_base_ind')
    dset_ind = opt.parlist[0]
    sub      = opt.parlist[1]

    if dset_ind == -1: dset_ind = proc.runs - 1  # may need updates
    if sub      == -1: sub      = proc.reps - 1

    if proc.verb > 1:
        print "+d %s: base/sub indices are %d, %d" % (block.label,dset_ind,sub)

    # get base prefix (run is index+1)
    base = proc.prev_prefix_form(dset_ind+1)

    proc.fp.write("# -------------------------------------------------------\n"
                  "# align each dset to the base volume\n"
                  "foreach run ( $runs )\n"
                  "    3dvolreg -verbose -zpad 1 -base %s+orig'[%d]'  \\\n"
                  "             -1Dfile dfile.r$run.1D -prefix %s  \\\n"
                  "             %s+orig\n"
                  "end\n\n"
                  "# make a single file of registration params\n"
                  "cat dfile.r??.1D > dfile.rall.1D\n\n" %
                    (proc.prev_prefix_form(dset_ind+1), sub, 
                     proc.prefix_form_run(block),
                     proc.prev_prefix_form_run())
                )

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

def db_mod_blur(block, proc, user_opts):
    if len(block.opts.olist) == 0: # init blur option
        block.opts.add_opt('-blur_filter', 1, ['-1blur_fwhm'], setpar=True)
        block.opts.add_opt('-blur_size', 1, [4.0], setpar=True)

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

    block.valid = 1

def db_cmd_blur(proc, block):
    opt    = block.opts.find_opt('-blur_filter')
    filter = opt.parlist[0]
    opt    = block.opts.find_opt('-blur_size')
    size   = opt.parlist[0]
    prefix = proc.prefix_form_run(block)
    prev   = proc.prev_prefix_form_run()

    proc.fp.write("# -------------------------------------------------------\n"
                  "# blur each volume\n"
                  "foreach run ( $runs )\n"
                  "    3dmerge %s %d -doall -prefix %s   \\\n"
                  "            %s+orig\n"
                  "end\n\n" % (filter, size, prefix, prev))

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

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
    opt = block.opts.find_opt('-mask_type')
    type = opt.parlist[0]
    if type == 'union': min = 1
    else:               min = proc.runs

    opt = block.opts.find_opt('-mask_dilate')
    nsteps = opt.parlist[0]

    prev = proc.prev_prefix_form_run()
    proc.fp.write("# -------------------------------------------------------\n"
                  "# create 'full_mask' dataset (%s mask)\n"
                  "foreach run ( $runs )\n"
                  "    3dAutomask -dilate %d -prefix rm.mask_r$run %s+orig\n"
                  "end\n\n" % (type, nsteps, prev))

    proc.fp.write("# get sum and compare it to %d for taking '%s'\n"
                  "3dMean -sum -datum short -prefix rm.sum rm.mask*.HEAD\n"
                  "3dcalc -a rm.sum+orig -expr 'ispositive(a-%d)' "
                      "-prefix full_mask\n\n" % (min, type, min))

    proc.mask = 'full_mask'     # note that we have a mask dataset to apply

    # do not increment block index or set 'previous' block label,
    # as there are no datasets created here

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
    # check for max scale value 
    opt = block.opts.find_opt('-scale_max_val')
    max = opt.parlist[0]
    if max > 100: maxstr = ' * step(%d-a) + %d*step(a-%d)' % (max,max,max-1)
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
    proc.fp.write("# -------------------------------------------------------\n"
                  "# create a scaled dataset for each run%s\n"
                  "foreach run ( $runs )\n"
                  "    3dTstat -prefix rm.mean_r$run %s+orig\n"
                  "    3dcalc -a %s+orig -b rm.mean_r$run+orig  \\\n"
                  "%s"
                  "           -expr '%s'  \\\n"
                  "           -prefix %s\n"
                  "end\n\n" % (maxstr, prev, prev, mask_dset, expr, prefix))

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

def db_mod_regress(block, proc, user_opts):
    if len(block.opts.olist) == 0: # then init
        block.opts.add_opt('-regress_basis', 1, ['GAM'], setpar=True)
        block.opts.add_opt('-regress_basis_normall', 1, [1], setpar=True)
        block.opts.add_opt('-regress_polort', 1, [2], setpar=True)
        block.opts.add_opt('-regress_stim_files', -1, [])
        block.opts.add_opt('-regress_stim_labels', -1, [])
        block.opts.add_opt('-regress_stim_times', -1, [])

    # check for user updates
    uopt = user_opts.find_opt('-regress_basis')
    bopt = block.opts.find_opt('-regress_basis')
    if uopt and bopt:
        bopt.parlist[0] = uopt.parlist[0]

    errs = 0
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
            print "** -regress_polort requires int for poly degree (have %s)\n" \
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
        proc.stims = uopt.parlist   # store for initial copy

    uopt = user_opts.find_opt('-regress_stim_labels')
    bopt = block.opts.find_opt('-regress_stim_labels')
    if uopt and bopt:
        if len(uopt.parlist) != proc.runs:
            print "** num -regress_stim_labels (%s) != num runs (%d)" \
                  % (len(uopt.parlist), proc.runs)
            errs += 1
        bopt.parlist = uopt.parlist

    # times is one file per class
    uopt = user_opts.find_opt('-regress_stim_times')
    bopt = block.opts.find_opt('-regress_stim_times')
    if uopt and bopt:
        if len(uopt.parlist) <= 0:
            print "** no files for -regress_stim_times?"
            errs += 1
        bopt.parlist = uopt.parlist
        proc.stims = uopt.parlist   # store for initial copy

    # if we are here, then we should have stimulus files
    if len(proc.stims) == 0:
        print "** missing stim files (-regress_stim_times/-regress_stim_files)"
        errs += 1

    if errs > 0:
        block.valid = 0
        return 1

    block.valid = 1

# here we need to concatenate the dfiles, and possible create stim_times files
#
# without stim_times, use stim_files to generate stim_times
# without stim_labels, use stim_times to create labels
def db_cmd_regress(proc, block):
    opt = block.opts.find_opt('-regress_basis')
    basis = opt.parlist[0]

    opt = block.opts.find_opt('-regress_basis_normall')
    normall = opt.parlist[0]

    opt = block.opts.find_opt('-regress_polort')
    polort = opt.parlist[0]

    if len(proc.stims) <= 0:   # be sure we have some stim files
        print "** cmd_regress: no stim files"
        return 1

    proc.fp.write("# -------------------------------------------------------\n"
                  "# run the regression analysis\n")

    opt = block.opts.find_opt('-regress_stim_times')
    if not opt.parlist or len(opt.parlist) == 0:
        if db_cmd_regress_sfiles2times(proc, block): return 1
    else: stim_times = opt.parlist

    if proc.mask: mask = '    -mask %s+orig  \\\n' % proc.mask
    else        : mask = ''

    proc.fp.write('3dDeconvolve -input %s+orig.HEAD    \\\n'
                  '    -polort %d  \\\n'
                  '%s'
                  '    -basis_normall %s  \\\n'
                  '    -num_stimts %d  \\\n'
                  % ( proc.prev_prefix_form_rwild(), polort, mask, str(normall),
                      len(proc.stims)+6 ) )  # +6 for motion params

    # verify labels (now that we know the list of stimulus files)
    opt = block.opts.find_opt('-regress_stim_labels')
    if not opt or not opt.parlist:
        labels = []
        for ind in range(len(proc.stims)):
            labels.append('stim%02d' % (ind+1))
        if proc.verb > 1: print ('+d adding labels: %s' % labels)
    elif len(proc.stims) != len(opt.parlist):
        print "** cmd_regress: have %d stims but %d labels"
        return 1
    else:  # we have the same number of labels as stims
        labels = opt.parlist

    # write out stim lines
    print ('** %d stims, %d labels' % (len(proc.stims), len(labels)))
    print ('labels = %s' % labels)
    for ind in range(len(proc.stims)):
        proc.fp.write('    -stim_times %d %s %s  \\\n'
                      '    -stim_label %d %s  \\\n'
                      % (ind+1, proc.stims[ind], basis, ind+1, labels[ind]))

    # write out registration param lines
    labels = ['roll', 'pitch', 'yaw', 'dS', 'dL', 'dP']
    first = len(proc.stims) + 1 # first stim index
    for ind in range(6):
        proc.fp.write("    -stim_file %d dfile.rall.1D'[%d]' "
                      "-stim_base %d -stim_label %d %s  \\\n"
                      % (ind+first, ind, ind+first, ind+first, labels[ind]))

    # add misc options
    proc.fp.write("    -fout -tout -full_first -x1D Xmat.1D  \\\n"
                  "    -bucket stats.$subj\n\n")

    proc.bindex += 1            # increment block index
    proc.pblabel = block.label  # set 'previous' block label

# convert a stim_files list into a stim_times list
def db_cmd_regress_sfiles2times(proc, block):
    if proc.verb > 1: print '-d old stim list: %s' % proc.stims

    proc.fp.write('\n# create -stim_times files\n')
    proc.fp.write('make.stim.times.py -prefix stim_times -tr %s -nruns %d  \\\n'
                  '                   -files '
                  % (str(proc.tr), proc.runs))
    cols = 0
    for file in proc.stims:
        proc.fp.write('%s ' % file)             # put name on cmd line
        cols += afni_util.num_cols_1D(file)     # and tally the number of cols
    proc.fp.write('\n\n')

    # and reset proc.stims to the new file list (which is 1-based)
    proc.stims = []
    for ind in range(1,cols+1):
        proc.stims.append('stimuli/stim_times.%02d.1D' % ind)

    if proc.verb > 1: print '-d new stim list: %s' % proc.stims
