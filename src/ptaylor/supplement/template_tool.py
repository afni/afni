#!/usr/bin/env python

# python3 status: compatible

# system libraries
import sys, os

# AFNI libraries
from afnipy import option_list   as OL
from afnipy import afni_util     as UTIL
from afnipy import afni_base     as BASE
from afnipy import template_library as TL

# ----------------------------------------------------------------------
# globals

g_help_string = """Overview ~1~

This program is for *** templating the start of a python program

auth = PA Taylor (SSCC, NIMH, NIH, USA)

------------------------------------------------------------------------

Usage ~1~

-inset INSET   :(req) name of the input dataset

-prefix PREFIX :(req) name of output dset


****

-workdir WD    : working directory name, without path; the working dir
                will be subdirectory of the output location
                (def: name with random chars)

-do_clean DC   :state whether to clean up any intermediate files;
                allowed values are:  Yes, 1, No, 0
                (def: '{do_clean}')

-do_log        :add this opt to turn on making a text log of all the
                shell commands that are run when this program is
                executed.  Mainly for debugging purposes.

-help, -h      :display program help file

-hist          :display program history

-ver           :display program version number

-verb  VVV     :control verbosity (def: {verb})

-show_valid_opts :show valid options for this program

------------------------------------------------------------------------

Notes ~1~

***

------------------------------------------------------------------------

Examples ~1~

 ****

           

""".format(**TL.DOPTS)

g_history = """
  **TEMPLATE_tool.py** history:

  0.1   Sep 25, 2025 :: started this command line interface 
"""

g_prog    = g_history.split()[0]
g_ver     = g_history.split("\n")[-2].split("::")[0].strip()
g_version = g_prog + " version " + g_ver

class InOpts:
    """Object for storing any/all command line inputs, and just checking
that any input files do, in fact, exist.  Option parsing and other
checks happen in a subsequent object.

    """

    def __init__(self):
        # main variables
        self.status          = 0                       # exit value
        self.valid_opts      = None
        self.user_opts       = None

        # general variables
        self.verb            = TL.DOPTS['verb']
        self.do_clean        = None
        self.overwrite       = None
        self.do_log          = None

        # main data variables
        self.inset           = None
        self.prefix          = None

        # control variables
        self.workdir         = None
        self.outdir          = None

        # ----- take action(s)

        # prelim stuff
        tmp1 = self.init_options()

    # ----- methods

    def init_options(self):
        """
        Prepare the set of all options, with very short help descriptions
        for each.
        """

        self.valid_opts = OL.OptionList('valid opts')

        # short, terminal arguments

        self.valid_opts.add_opt('-help', 0, [],           \
                        helpstr='display program help')
        self.valid_opts.add_opt('-hist', 0, [],           \
                        helpstr='display the modification history')
        self.valid_opts.add_opt('-show_valid_opts', 0, [],\
                        helpstr='display all valid options')
        self.valid_opts.add_opt('-ver', 0, [],            \
                        helpstr='display the current version number')

        # required parameters

        self.valid_opts.add_opt('-inset', 1, [], 
                        helpstr='name of input dataset')

        self.valid_opts.add_opt('-prefix', 1, [], 
                        helpstr='name of output dataset')

        # optional parameters

        self.valid_opts.add_opt('-workdir', 1, [], 
                        helpstr='name of workdir (no path)')

        # may derive from -prefix by default
        #self.valid_opts.add_opt('-outdir', 1, [], 
        #                helpstr='name of output dir')

        # general options

        self.valid_opts.add_opt('-do_clean', 1, [], 
                        helpstr="turn on/off removal of intermediate files")

        self.valid_opts.add_opt('-do_log', 0, [], 
                        helpstr="turn on/off logging shell cmd execution")

        self.valid_opts.add_opt('-overwrite', 0, [], 
                        helpstr='overwrite preexisting outputs')

        self.valid_opts.add_opt('-verb', 1, [], 
                        helpstr='set the verbose level (default is 0)')

        return 0

    def process_options(self):
        """return  1 on valid and exit        (e.g. -help)
           return  0 on valid and continue    (e.g. do main processing)
           return -1 on invalid               (bad things, panic, abort)
        """

        # process any optlist_ options
        self.valid_opts.check_special_opts(sys.argv)

        # process terminal options without the option_list interface
        # (so that errors are not reported)
        # return 1 (valid, but terminal)

        # if no arguments are given, apply -help
        if len(sys.argv) <= 1 or '-help' in sys.argv:
           print(g_help_string)
           return 1

        if '-hist' in sys.argv:
           print(g_history)
           return 1

        if '-show_valid_opts' in sys.argv:
           self.valid_opts.show('', 1)
           return 1

        if '-ver' in sys.argv:
           print(g_version)
           return 1

        # ============================================================
        # read options specified by the user
        self.user_opts = OL.read_options(sys.argv, self.valid_opts)
        uopts = self.user_opts            # convenience variable
        if not uopts: return -1           # error condition

        # ------------------------------------------------------------
        # process non-chronological options, verb comes first

        val, err = uopts.get_type_opt(int, '-verb')
        if val != None and not err: self.verb = val

        # ------------------------------------------------------------
        # process options sequentially, to make them like a script

        err_base = "Problem interpreting use of opt: "

        for opt in uopts.olist:

            # main options

            if opt.name == '-inset':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.inset = val

            elif opt.name == '-prefix':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.prefix = val

            # general options

            elif opt.name == '-workdir':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.workdir = val

            # general options

            elif opt.name == '-do_clean':
                val, err = uopts.get_string_opt('', opt=opt)
                if val is None or err:
                    BASE.EP(err_base + opt.name)
                self.do_clean = val

            elif opt.name == '-do_log':
                self.do_log = True

            elif opt.name == '-overwrite':
                self.overwrite = '-overwrite'

            # ... verb has already been checked above

        return 0

    def check_options(self):
        """perform any final tests before execution"""

        if self.verb > 1:
            BASE.IP("Begin processing options")

        # required opt
        if self.inset is None :
            BASE.EP1("missing -inset option")
            return -1

        if self.prefix is None:
            BASE.EP1("missing -prefix option")
            return -1

        return 0

    def test(self, verb=3):
        """one might want to be able to run internal tests,
           alternatively, test from the shell
        """
        print('------------------------ initial tests -----------------------')
        self.verb = verb
        
        print('------------------------ reset files -----------------------')

        print('------------------------ should fail -----------------------')

        print('------------------------ more tests ------------------------')

        return None

    # ----- decorators

    @property
    def ninset(self):
        """number of insets"""
        return len(self.inset)


# ----------------------------------------------------------------------------

def main():

    # init option-reading obj
    inobj = InOpts()
    if not(inobj) :  
        return 1, None

    # process (= read) options
    rv = inobj.process_options()
    if rv > 0: 
        # exit with success (e.g. -help)
        return 0, None
    if rv < 0:
        # exit with error status
        BASE.EP1('failed to process options')
        return 1, None

    # check the options
    rv2 = inobj.check_options()
    if rv2 :
        # exit with error status
        BASE.EP1('failed whilst checking options')
        return rv2, None

    # use options to create main object
    mainobj = TL.MainObj( user_inobj=inobj )
    if not mainobj :  
        return 1

    # write out log/history of what has been done (not done by default, to
    # save some time, bc this takes a mini-while)
    if inobj.do_log :
        olog = 'log_obliquity_tool.txt'
        UTIL.write_afni_com_log(olog)

    return 0, mainobj

# ============================================================================

if __name__ == '__main__':

    stat, mainobj = main()
    sys.exit(stat)


