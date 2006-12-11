#!/usr/bin/env python

# todo: - verify whether dsets contain subj_id as sub-string
#
#
# For now, do not consider re-running old process steps from
# script dir.  Since the purpose of this python script is to
# create a tcsh script, then that script would need to be
# responsible for creation of the python state.  Maybe we can
# create a state file with the subject ID, and allow the user
# to specify it along with an execution directory.
#
# dset name form is index.user_prefix.run.operation
# e.g. p02.SUBJ.r03.volreg (+orig)
#
# note: in the script, runs are 1-based (probably expected)

import sys
from time import asctime

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *
from db_mod import *

# globals

# ----------------------------------------------------------------------
g_help_string = """

    ---------------------------------------------------------------------------
    afni_proc.py        - generate a tcsh script for an AFNI process stream

    This python script can generate a processing script via a command-line
    interface by a tk GUI (eventually).  The user should minimally provide
    the input datasets (-dsets) and stimulus files (-regress_stim_*).

    The output script will create a results directory, copy input files into
    it, and perform all processing there.  So the user can delete the results
    directory and re-run the script at their leisure.

    --------------------------------------------------
    The output script will go through the following steps, unless the user
    specifies otherwise.

    automatic steps:

        setup       : check subject arg, set run list, create output dir, and
                      copy stim files
        tcat        : copy input datasets and remove unwanted initial TRs

    user controllable steps:

        tshift      : slice timing alignment on volumes (default is -time 0)
        volreg      : volume registration (default to first volume)
        blur        : blur each volume (default is 4mm fwhm)
        mask        : create a 'brain' mask from the EPI data (dilate 1 voxel)
        scale       : scale each run mean to 100, for each voxel (max of 200)
        regress     : regression analysis (default is GAM, peak 1, with motion
                      params)

    --------------------------------------------------
    example usage (options can be provided in any order):

        1. Minimum use, provide datasets and stim files.  Note that a dataset
           suffix (e.g. HEAD) must be used with wildcards, so datasets are not
           applied twice.  In this case, a stim_file with many columns is
           given, allowing the script to change it to stim_times files.

                afni_proc.py -dsets epiRT*.HEAD -regress_stim_files stims.1D

        2. In addition, specify the output script name, the subject ID, to
           remove the first 3 TRs of each run (before steady state), and to
           align volumes to the end of the runs (anat acquired after EPI).

                afni_proc.py -dsets epiRT*.HEAD -regress_stim_files stims.1D \\
                             -script process_ED -subj_id ED                  \\
                             -tcat_remove_first_trs 3 -volreg_align_to last

        3. Similar to #2, but skip tshift and mask steps (so the user must
           specify the others), apply 4 second BLOCK response function, and
           specify stim_times files, instead of stim_files.  Also, provide
           options given that ED's input files are sitting in directory
           subjects/ED.

                afni_proc.py -dsets subjects/ED/epiRT*.HEAD                  \\
                         -blocks volreg blur scale regress                   \\
                         -regress_stim_times subjects/ED/ED_stim_times*.1D   \\
                         -script process_ED -subj_id ED                      \\
                         -tcat_remove_first_trs 3 -volreg_align_to last      \\
                         -regress_basis 'BLOCK(4,1)'

    --------------------------------------------------
    options:

    -- general execution options --

        -blocks BLOCK1 ...      : specify the processing blocks to apply

                e.g. -blocks volreg blur scale regress
                default: tshift volreg blur mask scale regress

            The user may apply this option to specify which processing blocks
            are to be included in the output script.  The order of the blocks
            may be varied, and blocks may be skipped.

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
            wildward their order would end up as run1 run10 run11 run2 ...

            Note also that when using a wildcard it is essential to specify
            the datasets suffix, so that the shell doesn't put both the .BRIK
            and .HEAD filenames on the command line (which would make it twice
            as many runs of data).

        -out_dir DIR            : specify the output directory for the script

                e.g. -out_dir ED_results
                default: SUBJ.results

            The AFNI processing script will create this directory and perform
            all processing in it.

        -script SCRIPT_NAME     : specify the name of the resulting script

                e.g. -script @ED.process.script
                default: @proc_subj

            The output of this program is a script file.  This option can be
            used to specify the name of that file.

        -scr_overwrite          : overwrite any existing script

                e.g. -scr_overwrite

            If the output script file already exists, it will be overwritten
            only if the user applies this option.

        -subj_id SUBJECT_ID     : specify the subject ID for the script

                e.g. -subj_id elvis
                default: SUBJ

            The suject ID is used in dataset names and in the output directory
            name (unless -out_dir is used).  This option allows the user to
            apply an appropriate naming convention.

    -- block options --

        These options pertain to individual processing blocks.  Each option
        starts with the block name.

        -tcat_remove_first_trs NUM : specify how many TRs to remove from runs

                e.g. -tcat_remove_first_trs 3
                default: 0

            Since the scanner takes several seconds to reach a steady state,
            the initial TRs of each run may have values that are significantly
            greater than the later ones.  This option is used to specify how
            many TRs to remove from the beginning of every run.

        -tshift_align_to TSHIFT OP : specify 3dTshift alignment option

                e.g. -tshift_align_to -slice 14
                default: -tzero 0

            This option applies directly to the 3dTshift command, and is used
            specify the time that slices are aligned to.

            It is likely that the user will use either '-slice SLICE_NUM' or
            '-tzero ZERO_TIME'.  Please see '3dTshift -help' for more
            information.
            
        -tshift_interp METHOD   : specify the interpolation method for tshift

                e.g. -tshift_interp -Fourier
                e.g. -tshift_interp -cubic
                default -quintic

        -volreg_align_to POSN   : specify the base position for volume reg

                e.g. -volreg_align_to last
                default: first

            This option takes either 'first' or 'last' as a parameter.  It
            specifies whether the EPI volumes are registered to the first
            volume (of the first run) or the last volume (of the last run).

            Note that this is done after removing any volumes in the initial
            tcat operation.  See -tcat_remove_first_trs.

    ---------------------------------------------------------------------------
"""

# ----------------------------------------------------------------------
# dictionary of block types and modification functions
BlockLabels  = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
BlockModFunc  = {'tcat'  : db_mod_tcat,     'tshift' :db_mod_tshift,
                 'volreg': db_mod_volreg,   'blur'   :db_mod_blur,
                 'mask'  : db_mod_mask,
                 'scale' : db_mod_scale,    'regress':db_mod_regress}
BlockCmdFunc  = {'tcat'  : db_cmd_tcat,     'tshift' :db_cmd_tshift,
                 'volreg': db_cmd_volreg,   'blur'   :db_cmd_blur,
                 'mask'  : db_cmd_mask,
                 'scale' : db_cmd_scale,    'regress':db_cmd_regress}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# ----------------------------------------------------------------------
# data processing stream class
class SubjProcSream:
    def __init__(self, label):
        self.label      = label         # name for stream
        self.valid_opts = None          # list of possible user options
        self.user_opts  = None          # list of given user options

        self.blocks     = []            # list of ProcessBlock elements
        self.dsets      = []            # list of afni_name elements
        self.stims_orig = []            # orig list of stim files to apply
        self.stims      = []            # list of stim files to apply
        self.opt_src    = 'cmd'         # option source
        self.subj_id    = 'SUBJ'        # hopefully user will replace this
        self.subj_label = '$subj'       # replace this for execution
        self.out_dir    = ''            # output directory for use by script
        self.script     = '@proc_subj'
        self.overwrite  = False         # overwrite script file?
        self.fp         = None          # file object

        self.verb       = 1             # verbosity level

        self.tr         = 0.0           # TR, in seconds
        self.reps       = 0             # TRs per run
        self.runs       = 0             # number of runs
        self.mask       = None          # mask dataset

        self.bindex     = 0             # current block index
        self.pblabel    = ''            # previous block label

        return
        
    def show(self, mesg):       # rcr - update
        print '%sSubjProcSream: %s' % (mesg, self.label)
        for block in self.blocks:
            block.show('    Block %d: ' % self.blocks.index(block))
        print '    Dsets : ',
        if self.verb > 2:
            print
            for dset in self.dsets: dset.show()
        else:
            for dset in self.dsets: print dset.pv(),
            print
        if self.verb > 2: self.valid_opts.show('valid_opts: ')
        if self.verb > 1: self.user_opts.show('user_opts: ')

    def init_opts(self):
        self.valid_opts = OptionList('init_opts')

        # input style options  rcr - update
        # self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

        # general execution options
        self.valid_opts.add_opt('-blocks', -1, [])
        self.valid_opts.add_opt('-dsets', -1, [])

        self.valid_opts.add_opt('-out_dir', 1, [])
        self.valid_opts.add_opt('-scr_overwrite', 0, [])
        self.valid_opts.add_opt('-script', 1, [])
        self.valid_opts.add_opt('-subj_id', -1, [])

        # self.valid_opts.add_opt('-remove_rm_files', 0, [])
        # self.valid_opts.add_opt('-remove_pXX_files', 0, [])

        # block options
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [])

        self.valid_opts.add_opt('-tshift_align_to', -1, [])
        self.valid_opts.add_opt('-tshift_interp', 1, [])

        self.valid_opts.add_opt('-volreg_align_to', 1, [], ['first','last'])
        self.valid_opts.add_opt('-volreg_base_ind', 2, [])

        self.valid_opts.add_opt('-blur_filter', 1, [])
        self.valid_opts.add_opt('-blur_size', 1, [])

        self.valid_opts.add_opt('-mask_type', 1, [], ['union','intersection'])
        self.valid_opts.add_opt('-mask_dilate', 1, [])

        self.valid_opts.add_opt('-scale_max_val', 1, [])

        self.valid_opts.add_opt('-regress_basis', 1, [])
        self.valid_opts.add_opt('-regress_basis_normall', 1, [])
        self.valid_opts.add_opt('-regress_polort', 1, [])
        self.valid_opts.add_opt('-regress_stim_files', -1, [])
        self.valid_opts.add_opt('-regress_stim_labels', -1, [])
        self.valid_opts.add_opt('-regress_stim_times', -1, [])


        # other options
        self.valid_opts.add_opt('-help', 0, [1])
        self.valid_opts.add_opt('-verb', 1, [1])

        self.valid_opts.trailers = False   # do not allow unknown options
        
    def get_user_opts(self):
        self.user_opts = read_options(sys.argv, self.valid_opts)
        if self.user_opts == None: return 1
        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt: print "** seem to have trailers, but cannot find them!"
            else: print "** have invalid trailing args: %s", opt.show()
            return 1  # failure

        opt = self.user_opts.find_opt('-verb')    # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        opt = self.user_opts.find_opt('-help')    # does the user want help?
        if opt != None:
            self.disp_opt_help()
            return 1  # terminate
        
        opt = self.user_opts.find_opt('-subj_id')
        if opt != None: self.subj_id = opt.parlist[0]

        # get datasets
        opt = self.user_opts.find_opt('-dsets')
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))

        opt = self.user_opts.find_opt('-out_dir')
        if opt != None: self.out_dir = opt.parlist[0]

        opt = self.user_opts.find_opt('-script')
        if opt != None: self.script = opt.parlist[0]

        opt = self.user_opts.find_opt('-scr_overwrite')
        if opt != None: self.overwrite = True

        # done checking options

        # update out_dir now (may combine option results)
        if self.out_dir == '': self.out_dir = '%s.results' % self.subj_label

        if self.verb > 1: self.show('end get_user_opts ')

    # init blocks from command line options, then check for an
    # alternate source       rcr - will we use 'file' as source?
    def create_blocks(self):
        blocklist = BlockLabels
        opt = self.user_opts.find_opt('-blocks')
        if opt:  # then create blocklist from user opts (but prepend tcat)
            if opt.parlist[0] != 'tcat':
                blocklist = ['tcat'] + opt.parlist
            else: blocklist = opt.parlist
        for label in blocklist:
            rv = self.add_block(label)
            if rv != None: return rv

        # if there is another place to update options from, do it
        uopt = self.user_opts.find_opt('-opt_source')
        if uopt != None:
            print '** not ready to update options from %s' % str(uopt.parlist)
            return 1

        # do some final error checking
        if len(self.dsets) == 0:
            print 'error: dsets have not been specified (consider -dsets)'
            return 1

    # create script from blocks and options
    def create_script(self):
        rv = self.get_run_info()
        if rv != None: return rv

        rv = self.init_script()         # create the script file and header
        if rv != None: return rv

        errs = 0
        for block in self.blocks:
            cmd_str = BlockCmdFunc[block.label](self, block)
            if cmd_str == None:
                print "** script creation failure for block '%s'" % block.label
                errs += 1
            else: self.fp.write(cmd_str)

        rv = self.finalize_script()     # finish the script
        if rv != None: return rv

        if self.fp: self.fp.close()

        if errs > 0: return 1    # so we print all erros before leaving

        # verify whether subj_id is sub-string of each dset

        return

    def get_run_info(self):
        self.runs = len(self.dsets)
        if self.runs < 1:
            print "** have no dsets to analyze"
            return 1

        # updated by 'tcat' opteration (and -remove_trs option)
        dset = self.dsets[0].rpv()
        list = read_attribute(dset, 'TAXIS_NUMS')
        if list == None: return 1
        try: self.reps = int(list[0])
        except:
            print "** reps '%s' is not an int?" % list[0]
            return 1
        if self.reps < 1:
            print "** invalid nreps (%d) for dset %s" % (self.reps, dset)
            return 1

        list = read_attribute(dset, 'TAXIS_FLOATS')
        if list == None: return 1
        try: self.tr = float(list[1])
        except:
            print "** TR '%s' is not an int?" % list[0]
            return 1

        if self.verb > 1: print '(reps, runs, tr) = (%d, %d, %f)' %  \
                                 (self.reps, self.runs, self.tr)

    def disp_opt_help(self):    # rcr
        print g_help_string

    # create a new block for the given label, and append it to 'blocks'
    def add_block(self, label):
        block = ProcessBlock(label, self)
        if not block.valid:
            print '** invalid block type: %s' % block.label
            return 1
        self.blocks.append(block)

    def find_block(self, label):
        for block in self.blocks:
            if block.label == label: return block
        return None

    def find_block_index(self, label):
        block = find_block(label)
        if block: return self.blocks.index(block)
        return None

    # set subj shell variable, check output dir, create and cd
    def init_script(self):
        if not self.overwrite and os.path.isfile(self.script):
            print "error: script file '%s' already exists, exiting..." % \
                  self.script
            return 1
        try: self.fp = open(self.script,'w')
        except:
            print "cannot open script file '%s' for writing" % self.script
            return 1

        self.fp.write('#!/usr/bin/env tcsh\n\n')
        self.fp.write('# auto-generated by afni_proc.py, %s\n\n' % asctime())
        self.fp.write('# --------------------------------------------------\n'
                      '# script setup\n\n'
                      '# the user may specify a single subject to run with\n')
        self.fp.write('if ( $#argv > 0 ) then  \n'
                      '    set subj = $argv[0] \n'
                      'else                    \n'
                      '    set subj = %s       \n'
                      'endif\n\n' % self.subj_id )
        self.fp.write('# verify that the results directory does not yet exist\n'
                      'if ( -d %s ) then \n'
                      '    echo output dir "$sub.results" already exists\n'
                      '    exit                     \n'
                      'endif\n\n' % self.out_dir)
        self.fp.write('# set list of runs\n')
        self.fp.write('set runs = (`count -digits 2 1 %d`)\n\n' % self.runs)

        self.fp.write('# create results directory\n')
        self.fp.write('mkdir %s\n\n' % self.out_dir)
        
        if len(self.stims_orig) > 0: # copy stim files into script's stim dir
            self.fp.write('# create stimuli directory, and copy stim files\n')
            self.fp.write('mkdir %s/stimuli\ncp ' % self.out_dir)
            for ind in range(len(self.stims)):
                self.fp.write('%s ' % self.stims_orig[ind])
            self.fp.write('%s/stimuli\n\n' % self.out_dir)

        self.fp.flush()

    # and last steps
    def finalize_script(self):
        self.fp.write('# return to parent directory\n'
                      'cd ..\n\n')

    # given a block, run, return a prefix of the form: pNN.SUBJ.rMM.BLABEL
    #    NN = block index, SUBJ = subj label, MM = run, BLABEL = block label
    def prefix_form(self, block, run):
        return 'p%02d.%s.r%02d.%s' %    \
                (self.bindex, self.subj_label, run, block.label)

    # same, but leave run as a variable
    def prefix_form_run(self, block):
        return 'p%02d.%s.r$run.%s' % (self.bindex, self.subj_label, block.label)

    # same as prefix_form, but use previous block values (index and label)
    # (so we don't need the block)
    def prev_prefix_form(self, run):
        return 'p%02d.%s.r%02d.%s' %    \
                (self.bindex-1, self.subj_label, run, self.pblabel)

    # same, but leave run as a variable
    def prev_prefix_form_run(self):
        return 'p%02d.%s.r$run.%s' %    \
                (self.bindex-1, self.subj_label, self.pblabel)

    # same, but leave run wild
    def prev_prefix_form_rwild(self):
        return 'p%02d.%s.r??.%s' %    \
                (self.bindex-1, self.subj_label, self.pblabel)

class ProcessBlock:
    def __init__(self, label, proc):
        self.label = label      # label is block type
        self.valid = 0          # block is not yet valid
        self.verb  = proc.verb  # verbose level
        if not label in BlockModFunc: return

        self.opts = OptionList(label)                   # init option list
        BlockModFunc[label](self, proc, proc.user_opts) # add block

        if self.verb > 2:
            if self.valid: self.show('+d successful add ')
            else:          print '+d invalid ProcessBlock: %s' % label

    def show(self, mesg):
        print '------- %sProcessBlock: %s -------' % (mesg, self.label)
        self.opts.show('new options: ')

def test_proc():

    ps = SubjProcSream('subject regression')
    ps.init_opts()

    rv = ps.get_user_opts()
    if rv != None: return rv

    rv = ps.create_blocks()
    if rv != None: return rv

    rv = ps.create_script()
    if rv != None: return rv

    if ps.verb > 1: ps.show('testing... ')

# main, for testing
if __name__ == '__main__':

    test_proc()
