#!/usr/bin/env python

"""AFNI option parsing and input controls for align_epi_anat.py."""

import copy
import os
import sys

from afnipy import afni_base    as ab
from afnipy import afni_util    as au
from afnipy import option_list  as OL
from afnipy import lib_aea_defs as DEF


class InOpts:
    """Read AFNI options and retain the original program's control
    precedence."""

    def __init__(self, argv=None):
        """Initialize independent defaults and the original argument list.

        argv includes the program name, as for AFNI's read_options().
        Register all existing options without probing datasets or running
        processing commands.
        """

        self.status        = 0
        self.label         = 'align_epi_anat.py'
        self.argv          = list(sys.argv if argv is None else argv)
        self.args_orig     = self.argv[:]

        for key, value in DEF.DOPTS.items():
            setattr(self, key, copy.deepcopy(value))

        self.odir          = os.getcwd()
        self.user_opts     = None
        self.input_options = {}

        # --------------

        self.init_options()

    def init_options(self):
        """Register the unchanged options, defaults, aliases and help text.

        Retain their registration order and disallow trailing arguments.
        Return 0 after building a fresh AFNI OptionList.
        """

        self.valid_opts = OL.OptionList('init_opts')
        for args, kwargs in copy.deepcopy(DEF.LIST_opts):
            self.valid_opts.add_opt(*args, **kwargs)
        self.valid_opts.trailers = 0
        return 0

    def process_options(self):
        """Read inputs and expose the first occurrence of each parsed option.

        Keep AFNI-inserted defaults and repeated-option precedence.  Return
        0 to continue, 1 after a successful terminal request, or -1 after
        a parser/control error.  Preserve legacy ciao() output and history.
        """

        try:
            rv = self.get_user_opts()
            if rv is not None:
                self.ciao(1)
        except SystemExit as exc:
            self.status = exc.code
            return 1 if self.status == 0 else -1

        for opt in self.user_opts.olist:
            self.input_options.setdefault(opt.name, opt)
        return 0

    def check_options(self):
        """Run the original per-option tests after initial controls are read.

        Return 0 on success or -1 on failure.  Dataset checks and derived
        alignment/grid settings remain in MainObj.basic_setup(), preserving
        their timing relative to filesystem and AFNI commands.
        """

        try:
            for opt in self.user_opts.olist:
                if opt.test() is None:
                    self.ciao(1)
        except SystemExit as exc:
            self.status = exc.code
            return -1
        return 0

    def get_user_opts(self):
        """Read AFNI options and apply the initial control settings.

        Use this object's argv, including the program name.  Return None on
        success or 1 on a parser/trailer error.  Help and version requests
        terminate through ciao(), as in the original command-line program.
        """

        self.valid_opts.check_special_opts(self.argv) #ZSS March 2014
        self.user_opts = OL.read_options(self.argv, self.valid_opts)
        if self.user_opts == None: return 1 #bad
        # no options: apply -help
        if ( len(self.user_opts.olist) == 0 or \
              len(self.argv) <= 1 ) :
            self.self_help()
            self.ciao(0)  # terminate

        if self.user_opts.trailers:
            opt = self.user_opts.find_opt('trailers')
            if not opt:
                ab.EP1("seem to have trailers, but cannot find them!")
            else:
                msg = "have invalid trailing args: {}".format(opt.show())
                ab.EP1(msg)
            return 1  # failure

        # apply the user options
        if self.apply_initial_opts(self.user_opts): return 1

        if self.verb > 3:
            self.show('------ found options ------ ')

        return


    def apply_initial_opts(self, opt_list):
        """Apply the initial controls in their historical precedence order.

        Set execution, alignment direction, preprocessing and save
        flags from opt_list.  Terminal options print their output and
        terminate through ciao(); preserve these exits and the
        existing option interactions."""

        # user only wants version
        opt1 = opt_list.find_opt('-version')
        opt2 = opt_list.find_opt('-ver')

        if ((opt1 != None) or (opt2 != None)):
            # ps.version()
            self.ciao(0)   # terminate

        # set and use verb
        opt = opt_list.find_opt('-verb')
        if opt != None: self.verb = int(opt.parlist[0])

        # save executed script
        opt = opt_list.find_opt('-save_script')
        if opt != None: self.save_script = opt.parlist[0]

        # set execute mode
        opt = opt_list.find_opt('-ex_mode')
        if opt != None: self.oexec = opt.parlist[0]

        # keep temp files
        opt = opt_list.find_opt('-keep_rm_files')
        if opt != None: self.rmrm = 0

        # preprocessing only
        opt = opt_list.find_opt('-prep_only')
        if opt != None: self.prep_only = 1

        # does the user want help?
        opt = opt_list.find_opt('-help')
        if opt != None:
            # give full help now by def
            self.self_help(2)
            self.ciao(0)  # terminate

        # less help?
        opt = opt_list.find_opt('-limited_help')
        if opt != None:
            self.self_help()
            self.ciao(0)  # terminate

        # help for options only
        opt = opt_list.find_opt('-option_help')
        if opt != None:
            self.self_help(1)
            self.ciao(0)  # terminate

        # set and use percentile for weight
        opt = opt_list.find_opt('-perc')
        if opt != None: self.perc = float(opt.parlist[0])

        opt = opt_list.find_opt('-suffix')
        if opt != None:
            self.suffix = opt.parlist[0]
            if self.suffix == "" or self.suffix == " " :
                self.error_msg("Cannot have blank suffix")
                self.ciao(1);

        opt = opt_list.find_opt('-cost')
        if opt != None: self.cost = opt.parlist[0]
        else: self.cost = ''

        opt = opt_list.find_opt('-pow_mask')
        if opt != None: self.sqmask = opt.parlist[0]

        opt = opt_list.find_opt('-box_mask')
        if opt != None: self.boxmask = opt.parlist[0]

        opt = opt_list.find_opt('-bin_mask')
        if opt != None: self.binmask = opt.parlist[0]

        if (opt_list.find_opt('-epi2anat') or
            opt_list.find_opt('-dset2to1') ):    # align epi to anat
            self.epi2anat    = 1
            self.anat2epi    = 0     # turn off anat to epi unless requested
            self.volreg_flag = 1     # turn on motion correction/volreg
            if(opt_list.find_opt('-anat2epi') or
                opt_list.find_opt('-dset1to2')): # align anat to epi
                self.anat2epi = 1

        opt = opt_list.find_opt('-prep_off')     # turn off all preproc steps
        if opt != None:
            self.deoblique_flag = 0
            self.tshift_flag    = 0
            self.volreg_flag    = 0
            self.resample_flag  = 0
            self.info_msg( \
            "turning off deobliquing tshift, volume registration, resampling")
            # note - individual flags can be turned on, if desired

        opt = opt_list.find_opt('-deoblique')    # deoblique data
        if opt != None:
            if(opt.parlist[0]=='on'):
                self.deoblique_flag = 1
            elif(opt.parlist[0]=='off'):
                self.deoblique_flag = 0
            else:
                self.error_msg("deoblique option not on/off")
                self.ciao(1)

        opt = opt_list.find_opt('-tshift')    # do time shifting
        if opt != None:
            if(opt.parlist[0]=='on'):
                self.tshift_flag = 1
            elif(opt.parlist[0]=='off'):
                self.tshift_flag = 0
            else:
                self.error_msg("tshift option not on/off")
                self.ciao(1)

        opt = opt_list.find_opt('-volreg')    # do volume registration
        if opt != None:
            if(opt.parlist[0]=='on'):
                self.volreg_flag = 1
                self.info_msg("turning on volume registration")
            elif(opt.parlist[0]=='off'):
                self.volreg_flag = 0
                self.info_msg("turning off volume registration")
            else:
                self.error_msg("volreg option not on/off");
                self.ciao(1)

        opt = opt_list.find_opt('-resample')    # resample epi to anat
        if opt != None:
            if(opt.parlist[0]=='on'):
                self.resample_flag = 1
            elif(opt.parlist[0]=='off'):
                self.resample_flag = 0
                self.info_msg("turning off resampling")
            else:
                self.error_msg("resample option not on/off");
                self.ciao(1)

        opt = opt_list.find_opt('-check_flip')       # check for LR flip
        if opt is not None:
            self.flip        = 1
            self.save_epi_ns = 1

        # optional data to save
        # save 3dAllineate input datasets
        opt = opt_list.find_opt('-save_Al_in')
        if opt != None: 
            self.save_Al_in = 1

        opt = opt_list.find_opt('-save_tsh')         # save timeshifted data
        if opt != None: 
            self.save_tsh = 1

        # save volume registered epi data
        opt = opt_list.find_opt('-save_vr')
        if opt != None:
            self.save_vr = 1
            opt = opt_list.find_opt('-volreg')       # if volreg not set
            if opt == None:  
                self.volreg_flag = 1    # turn on volreg proc

        opt = opt_list.find_opt('-save_skullstrip')  # save unaligned SS'ed
        if opt != None: 
            self.save_skullstrip = 1

        # save unaligned, unobliqued dataset
        opt = opt_list.find_opt('-save_orig_skullstrip')
        if opt != None:
            val, err = opt_list.get_string_opt('', opt=opt)
            if val == None or err:
                self.self_help()
                self.ciao(0)  # terminate
            self.save_origstrip = val

        opt = opt_list.find_opt('-save_epi_ns')   # save unaligned SS'ed epi
        if opt != None: 
            self.save_epi_ns = 1

        # save unaligned represent. epi
        opt = opt_list.find_opt('-save_rep')
        if opt != None: 
            self.save_rep = 1

        opt = opt_list.find_opt('-save_resample') # save unaligned res epi
        if opt != None: 
            self.save_resample = 1

        opt = opt_list.find_opt('-save_all')      # save all opt output dsets
        if ((opt != None) or self.prep_only):
            self.save_Al_in = 1      # save 3dAllineate input files
            self.save_tsh = 1        # save tshifted epi
            self.save_vr = 1         # save volume registered epi
            self.save_skullstrip = 1 # save skullstripped (not aligned)
            self.save_rep = 1        # save representative tstat epi
            self.save_resample = 1   # save resampled epi
            self.save_epi_ns = 1     # save skull-stripped epi


    def show(self, mesg=""):
        """Print the parsed options with an optional message.

        Include the valid option table when verbosity exceeds two."""

        print("{!s}: {!s}".format(mesg, self.label))
        if self.verb > 2: self.valid_opts.show('valid_opts: ')
        self.user_opts.show('user_opts: ')


    def info_msg(self, mesg=""):
        """Print mesg with the informational prefix when verbosity permits."""

        if(self.verb >= 1) :
            print("#++ {!s}".format(mesg))


    def error_msg(self, mesg=""):
        """Print mesg with the existing error prefix without terminating."""

        print("#**ERROR {!s}".format(mesg))


    def ciao(self, i):
        """Finish with status i after restoring the original working
        directory.

        Print the historical completion/error message and, when requested,
        write the accumulated AFNI command history.  Raise SystemExit(i);
        the entry-point/processing boundary records that status."""

        if i > 0:
            print("** ERROR - script failed")
        elif i==0:
            print("")

        os.chdir(self.odir)

        if self.save_script:
            au.write_afni_com_history(self.save_script)

        # return status code
        sys.exit(i)


    def self_help(self, help_level=0):
        """Print the requested help level using the unchanged option table.

        Level 0 prints main help, 1 option help, and 2 both.  Return 1.
        """

        if(help_level!=1) :
            print(DEF.g_help_string)
        if(help_level):
            print("A full list of options for {!s}: ~1~\n".format(self.label))
            for opt in self.valid_opts.olist:
                print("{!s:<20}".format(opt.name))
                if (opt.helpstr != ''):
                    print("{!s:<20}   {!s}".format(
                        "   use:",
                        opt.helpstr.replace("\n","\n   {!s:<20}   ".format(
                            ' '
                        ))
                    ))
                if (opt.acceptlist):
                    print("{!s:<20}   {!s}".format(
                        "   allowed:",
                        str.join(', ', opt.acceptlist)
                    ))
                if (opt.deflist):
                    print("{!s:<20}   {!s}".format(
                        "   default:",
                        str.join(' ', opt.deflist)
                    ))
        return 1


    def version(self):
        """Print the unchanged alignment version at the current verbosity."""

        self.info_msg("align_epi_anat version: {!s}".format(
            self.align_version
        ))
