#!/usr/bin/env python

import sys

# AFNI modules
from afni_base import *
from afni_util import *
from option_list import *

# globals
AllBlockLabels = ["tcat", "tshift", "reg_vr", "blur", "scale", "decon"]

# data processing stream class
class ProcessStream:
    def __init__(self, label):
        self.label = label      # string
        self.blocks = []        # list of ProcessBlock elements

        self.valid_opts = None  # list of possible user options
        self.user_opts = None   # list of given user options

        self.sid  = 'subj'
        self.dsets  = []        # list of afni_name elements

        self.sdir = os.getcwd()
        self.verb = 1           # verbosity level

        return
        
    def show(self, mesg):
        print "%sProcessStream: %s" % (mesg, self.label)
        print "source dir: %s" % self.sdir
        for block in self.blocks:
            block.show("    Block %d: " % self.blocks.index(block))
        print "    Dsets : ",
        if self.verb > 2:
            print
            for dset in self.dsets: dset.show()
        else:
            for dset in self.dsets: print dset.pv(),
            print
        if self.verb > 2: self.valid_opts.show('valid_opts')
        if self.verb > 1: self.user_opts.show('user_opts')

    def init_opts(self):
        self.valid_opts = OptionList('init_opts')

        # execution options
        self.valid_opts.add_opt('-subj_id', -1, [])
        self.valid_opts.add_opt('-dsets', -1, [])
        self.valid_opts.add_opt('-remove_trs', 2, [])
        self.valid_opts.add_opt('-base_reg_vol', 1, [])
        self.valid_opts.add_opt('-stim_files', -1, [])
        self.valid_opts.add_opt('-stim_times', -1, [])
        self.valid_opts.add_opt('-stim_labels', -1, [])
        self.valid_opts.add_opt('-new_dir', 1, [])
        self.valid_opts.add_opt('-blur_rad', 1, [])
        self.valid_opts.add_opt('-3dd_basis', 1, [])

        # input style options
        self.valid_opts.add_opt('-gui', 0, [])
        self.valid_opts.add_opt('-opt_file', 1, [])

        # other options
        self.valid_opts.add_opt('-help', 0, [1])
        self.valid_opts.add_opt('-verb', 1, [1])
        
    def get_user_opts(self):
        self.user_opts = read_options(sys.argv, self.valid_opts)

        opt = self.user_opts.find_opt("-verb")    # set and use verb
        if opt != None: self.verb = int(opt.parlist[0])

        opt = self.user_opts.find_opt("-help")    # does the user want help?
        if opt != None: return self.disp_opt_help()
        
        opt = self.user_opts.find_opt("-dsets")
        if opt != None:
            for dset in opt.parlist:
                self.dsets.append(afni_name(dset))
        if self.verb > 1: self.show('post read_options ')

    def disp_opt_help(self):
        print """
        afni_proc - create/run an AFNI processing script
        """

    def add_block(self, block):
        if block.valid: self.blocks.append(block)
        else: print "** invalid block: %s" % block.label

    def find_block(self, label):
        for block in self.blocks:
            if block.label == label: return block
        return None

    def find_block_index(self, label):
        block = find_block(label)
        if block: return self.blocks.index(block)
        return None

class ProcessBlock:
    def __init__(self, label):  # label is block name
        self.label = label
        self.valid = 0          # block is not yet valid
        if not OKBlockLabel(label): return

        self.valid = 1
        self.verb  = 1
        if self.verb > 1: print "+d init new ProcessBlock: %s" % label

    def show(self, mesg):
        print "%sProcessBlock: %s" % (mesg, self.label)

def OKBlockLabel(label): return label in AllBlockLabels


def test_proc():

    ps = ProcessStream("subject regression")
    ps.init_opts()
    if ps.get_user_opts() == None: return


    ps.dsets.append(afni_name("epi_r1+orig"))
    ps.dsets.append(afni_name("epi_r2+orig"))

    ps.add_block(ProcessBlock("tcat"))
    ps.add_block(ProcessBlock("pickle"))
    ps.add_block(ProcessBlock("tshift"))

    if ps.verb > 1: ps.show("testing... ")

# main, for testing
if __name__ == "__main__":

    test_proc()
