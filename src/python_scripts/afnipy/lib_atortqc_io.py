#!/usr/bin/env python


ver = '1.0' ; date = 'Nov 3, 2022' ; auth = 'PA Taylor'
# + [PT] birth: for TORTOISE QC HTML
#
#
#########################################################################

# Supplementary stuff and I/O functions for the ATORTQC tcsh script.
# Modeled heavily on parallel progs for APQC

import sys, copy, os
from   afnipy import lib_apqc_io as laio

# ========================================================================
# ======================== for atortqc_make_tcsh =========================

help_string_atortqc_make_tcsh = '''

This program creates the single subject (ss) HTML review script
'run_atortqc_', which itself generates images and text that form
the TORTOISE quality control (ATORTQC) HTML.

It is typically run by TORTOISE itself.

Options:

-tvar_json  TJ    :(req) TJ is a text file of tvars ("tortoise variables")
                   created by TORTOISE, which catalogues important
                   files in the results directory, for the ATORTQC.

-subj_dir   SD    :(req) location of TORTOISE results directory (often '.',
                   as this program is often run from within the TORTOISE
                   results directory).

'''

# -------------------------------------------------------------------

class atortqc_tcsh_opts:

    def __init__(self):
        # Each of these is required.  Might have other optional
        # options in the future
        self.json     = ""
        self.subjdir  = ""
        self.revstyle = "pythonic"
        self.pythonic2basic = 0

    def set_json(self, json):
        self.json = json

    def set_subjdir(self, subjdir):
        self.subjdir = subjdir

    # check requirements
    def check_req(self):
        MISS = 0
        if self.json == "":
            print("missing: json")
            MISS+=1
        if self.subjdir == "":
            print("missing: subjdir")
            MISS+=1
        # to start, 'pythonic' is hardwired in
        if self.revstyle == 'pythonic' :
            # if user asks for Pythonic but sys is not set up for it,
            # downgrade back to 'basic' --- same as for APQC
            checked_style = laio.check_apqc_pythonic_ok()
            if checked_style != self.revstyle :
                print("+* WARNING: matplotlib does not exist or ver is low")
                self.pythonic2basic = 1
            #self.set_revstyle(checked_style)

        return MISS

# -------------------------------------------------------------------

def parse_atortqc_tcsh_args(argv):
    '''Parse arguments for ATORTQC tcsh scripter.

    Input
    -----
    argv : list of args (not including prog name)

    Return
    ------

    iopts : an object with the argument values stored, including a
        self-"check_req()" method, as well.
    '''

    Narg = len(argv)

    if not(Narg):
        print(help_string_atortqc_make_tcsh)
        sys.exit(0)

    # initialize argument array
    iopts = atortqc_tcsh_opts()

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_atortqc_make_tcsh)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "-tvar_json":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_json(argv[i])

        elif argv[i] == "-subj_dir":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_subjdir(argv[i])

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: with input arguments (missing or incorrect)")
        sys.exit(1)
        
    return iopts

# ========================================================================
# ======================== for atortqc_make_html =========================

help_string_atortqc_make_html = '''

Help is here.

-qc_dir

'''

class atortqc_html_opts:

    def __init__(self):
        # Each of these is required.  Might have other optional
        # options in the future
        self.qcdir = ""

    def set_qcdir(self, qcdir):
        self.qcdir = qcdir

    # check requirements
    def check_req(self):
        MISS = 0
        if self.qcdir == "":
            print("missing: qcdir")
            MISS+=1
        return MISS

# -------------------------------------------------------------------

def parse_atortqc_html_args(argv):
    '''Parse arguments for ATORTQC html generator.

    Input
    -----
    argv : list of args (not including prog name)

    Return
    ------

    iopts : an object with the argument values stored, including a
        self-"check_req()" method, as well.
    '''

    Narg = len(argv)

    if not(Narg):
        print(help_string_atortqc_make_html)
        sys.exit(0)

    # initialize argument array
    iopts = atortqc_atortqc_html_opts()

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h":
            print(help_string_atortqc_make_html)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "-qc_dir":
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_qcdir(argv[i])

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("** ERROR: Missing input arguments")
        sys.exit(1)
        
    return iopts

# ----------------------------------------------------------------------------
