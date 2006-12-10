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

BlockLabels  = ['tcat', 'tshift', 'volreg', 'blur', 'mask', 'scale', 'regress']
# dictionary of block types and modification functions
BlockModFunc  = {'tcat'  : db_mod_tcat,     'tshift' :db_mod_tshift,
                 'volreg': db_mod_volreg,   'blur'   :db_mod_blur,
                 'mask'  : db_mod_mask,
                 'scale' : db_mod_scale,    'regress':db_mod_regress}
BlockCmdFunc  = {'tcat'  : db_cmd_tcat,     'tshift' :db_cmd_tshift,
                 'volreg': db_cmd_volreg,   'blur'   :db_cmd_blur,
                 'mask'  : db_cmd_mask,
                 'scale' : db_cmd_scale,    'regress':db_cmd_regress}
AllOptionStyles = ['cmd', 'file', 'gui', 'sdir']

# data processing stream class
class SubjProcSream:
    def __init__(self, label):
        self.label      = label         # name for stream
        self.valid_opts = None          # list of possible user options
        self.user_opts  = None          # list of given user options

        self.blocks     = []            # list of ProcessBlock elements
        self.dsets      = []            # list of afni_name elements
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
        self.valid_opts.add_opt('-opt_source', 1, [], AllOptionStyles)

        # general execution options
        self.valid_opts.add_opt('-blocks', -1, [])
        self.valid_opts.add_opt('-dsets', -1, [])

        self.valid_opts.add_opt('-out_dir', 1, [])
        self.valid_opts.add_opt('-scr_overwrite', 0, [])
        self.valid_opts.add_opt('-script', 1, [])
        self.valid_opts.add_opt('-subj_id', -1, [])

        self.valid_opts.add_opt('-remove_rm_files', 0, [])
        self.valid_opts.add_opt('-remove_pXX_files', 0, [])

        # block options
        self.valid_opts.add_opt('-tcat_remove_first_trs', 1, [])

        self.valid_opts.add_opt('-tshift_align_to', -1, [])
        self.valid_opts.add_opt('-tshift_resam', 1, [])

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
        for label in BlockLabels:
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
            if str == None:
                print "** script creation failure for block '%s'" % block.label
                errs += 1
            # else: append_string_to_file(self.script, cmd_str)

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
        print '''
        afni_proc - create/run an AFNI processing script
        '''

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
        
        if len(self.stims) > 0: # then copy stim files into script's stim dir
            if self.verb > 1: print "+d init_s: copy stims: %s" % self.stims
            self.fp.write('# create stimuli directory, and copy stim files\n')
            self.fp.write('mkdir %s/stimuli\ncp ' % self.out_dir)
            for ind in range(len(self.stims)):
                self.fp.write('%s ' % self.stims[ind])
                # after copying, strip pathname from filenames
                self.stims[ind] = 'stimuli/%s'%os.path.basename(self.stims[ind])
            self.fp.write('%s/stimuli\n\n' % self.out_dir)
            if self.verb > 1: print "+d init_s: new stims: %s" % self.stims

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
