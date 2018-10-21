#ver = '1.00' ; date = 'Oct 17, 2018' ; auth = 'PA Taylor'
# + birth
#
#########################################################################

# Supplementary stuff and I/O functions for the AP QC tcsh script

import sys

# -------------------------------------------------------------------

def ARG_missing_arg(arg):
    print("** ERROR: missing argument after option flag: {}".format(arg))
    sys.exit(1)

# ========================================================================
# ======================== for apqc_make_tcsh ============================

help_string_apqc_make_tcsh = '''

Help is here.

-uvar_json
-subj_dir

'''

# -------------------------------------------------------------------

class apqc_tcsh_opts:

    # Each of these is required.  Might have other optional options in
    # the future
    json    = ""
    subjdir = ""

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
        return MISS

# -------------------------------------------------------------------

def parse_tcsh_args(argv):
    '''Parse arguments for tcsh scripter.

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
        print(help_string_apqc_make_tcsh)
        sys.exit(0)

    # initialize argument array
    iopts = apqc_tcsh_opts()

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h" or argv[i] == "-hview":
            print(help_string_apqc_make_tcsh)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "-uvar_json":
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
        print("** ERROR: Missing input arguments")
        sys.exit(1)
        
    return iopts

# ========================================================================
# ======================== for apqc_make_html ============================

help_string_apqc_make_html = '''

Help is here.

-qc_dir

'''

class apqc_html_opts:

    # Each of these is required.  Might have other optional options in
    # the future
    qcdir = ""

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

def parse_html_args(argv):
    '''Parse arguments for html generator.

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
        print(help_string_apqc_make_html)
        sys.exit(0)

    # initialize argument array
    iopts = apqc_html_opts()

    # check through inputs
    i = 0
    while i < Narg:
        if argv[i] == "-ver":
            print(ver)
            sys.exit(0)

        elif argv[i] == "-help" or argv[i] == "-h" or argv[i] == "-hview":
            print(help_string_apqc_make_html)
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
