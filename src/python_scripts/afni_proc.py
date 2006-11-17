#!/usr/bin/env python

import sys, glob, string        # standard

from afni_base import *         # AFNI modules
from afni_util import *         # AFNI modules
from option_list import *

# globals
AllBlockLabels = ["tcat", "tshift", "reg_vr", "blur", "scale", "decon"]
global debug

# data processing stream class
class ProcessStream:
    def __init__(self, label):
        self.label = label      # string
        self.blocks = []        # list of ProcessBlock elements
        self.dsets  = []        # list of afni_name elements

        if debug > 1: print "+d init new ProcessStream: %s" % label

        return
        
    def show(self, mesg):
        print "%sProcessStream: %s" % (mesg, self.label)
        for block in self.blocks:
            block.show("    Block %d: " % self.blocks.index(block))
        print "    Dsets : ",
        if debug > 1:
            print
            for dset in self.dsets: dset.show()
        else:
            for dset in self.dsets: print dset.pv(),
            print
        return

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
        if debug > 1: print "+d init new ProcessBlock: %s" % label

    def show(self, mesg):
        print "%sProcessBlock: %s" % (mesg, self.label)

def OKBlockLabel(label): return label in AllBlockLabels

# main, for testing
if __name__ == "__main__":
    debug = 0

    test_comopts()

else:    

    ps = ProcessStream("subject regression")
    opts = process_options()

    ps.dsets.append(afni_name("epi_r1+orig"))
    ps.dsets.append(afni_name("epi_r2+orig"))

    ps.add_block(ProcessBlock("tcat"))
    ps.add_block(ProcessBlock("pickle"))
    ps.add_block(ProcessBlock("tshift"))

    if debug > 0: ps.show("testing... ")

    print "arguments were: %s" % sys.argv

    # testing getopts2()


