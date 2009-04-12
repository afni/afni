#!/usr/bin/env python

import sys, os
import option_list, afni_util as UTIL, afni_base as BASE

g_help_string = """
===========================================================================
gen_epi_review.py:

    This program will generate an AFNI processing script that can be used
    to review EPI data (possibly called @review_epi_data).

    The @review_epi_data script is meant to provide an easy way to quickly
    review the (preferably un-altered) EPI data.  It runs afni and then a
    looping set of drive_afni commands.

    Note that there should not be another instance of 'afni' running on
    the system when the script is run, as 'drive_afni' will communicate
    with only the first invoked 'afni' program.

    The most simple usage comes with the -dsets option, along with the
    necessary pieces of the gen_epi_review.py command.

--------------------------------------------------
examples:

    These examples assume the EPI dataset names produced as a result of
    the afni_proc.py processing script proc.sb23.blk, produced by the
    command in AFNI_data4/s1.afni_proc.block, provided with the class data.

    Yes, that means running the s1.afni_proc.block (tcsh) script to call
    the afni_proc.py (python) script to produce the proc.sb23.blk (tcsh)
    script, which calls the gen_epi_review.py (python) script to produce
    the @review_epi_data (tcsh) script, which can be run to review your EPI 
    data.  Ahhhhhhh...  :)

    Note that when using wildcards, the datasets must exist in the current
    directory.  But when using the {1,2,..} format, the files do not yet
    need to exist.  So command #2 could be run anywhere and still create the
    same script, no data needed.

    1. simple usage, just providing datasets (and general options)

        gen_epi_review.py -dsets pb00.sb23.blk.r??.tcat+orig.HEAD

    2. expand 5 runs with shell notation, rather than wildcards, and
       specify an alternate script name

        gen_epi_review.py -dsets pb00.sb23.blk.r{1,2,3,4,5}.tcat        \\
                -script @review_epi_5runs

    3. choose to see all three image windows

        gen_epi_review.py -dsets pb00.sb23.blk.r*.tcat+orig.HEAD        \\
                -windows sagittal axial coronal                         \\
                -script @review_epi_windows

    4. specify the graph size and position (can do the same for image windows)

        gen_epi_review.py -dsets pb00.sb23.blk.r*.tcat+orig.HEAD        \\
                -gr_size 600 450 -gr_xoff 100 -gr_yoff 200              \\
                -script @review_epi_posn

----------------------------------------------------------------------
OPTIONS:
----------------------------------------------------------------------
informational arguments:

    -help                       : display this help
    -hist                       : display the modification history
    -show_valid_opts            : display all valid options (short format)
    -ver                        : display the version number

----------------------------------------
required argument:

    -dsets dset1 dset2 ...      : specify input datasets for processing

        e.g. -dsets epi_r*+orig.HEAD

        This option is used to provide a list of datasets to be processed
        in the resulting script.

----------------------------------------
optional arguments:

    -script SCRIPT_NAME         : specify the name of the generated script

        e.g. -script review.epi.subj23

        By default, the script name will be '@' followed by the name used
        for the '-generate' option.  So when using '-generate review_epi_data',
        the default script name will be '@review_epi_data'.

        This '-script' option can be used to override the default.

    -verb LEVEL                 : specify a verbosity level

        e.g. -verb 3

        Use this option to print extra information to the screen

    -windows WIN1 WIN2 ...      : specify the image windows to open

        e.g. -windows sagittal axial

        By default, the script will open 2 image windows (sagittal and axial).
        This option can be used to specify exactly which windows get opened,
        and in which order.

        Acceptable window names are: sagittal, axial, coronal

----------------------------------------
geometry arguments (optional):

    -im_size dimX dimY          : set image dimensions, in pixels

        e.g. -im_size 300 300

        Use this option to alter the size of the image windows.  This
        option takes 2 parameters, the pixels in the X and Y directions.

    -im_xoff XOFFSET            : set the X-offset for the image, in pixels

        e.g. -im_xoff 420

        Use this option to alter the placement of images along the x-axis.
        Note that the x-axis is across the screen, from left to right.

    -im_yoff YOFFSET            : set the Y-offset for the image, in pixels

        e.g. -im_xoff 400

        Use this option to alter the placement of images along the y-axis.
        Note that the y-axis is down the screen, from top to bottom.

    -gr_size dimX dimY          : set graph dimensions, in pixels

        e.g. -gr_size 400 300

        Use this option to alter the size of the graph window.  This option
        takes 2 parameters, the pixels in the X and Y directions.

    -gr_xoff XOFFSET            : set the X-offset for the graph, in pixels

        e.g. -gr_xoff 0

        Use this option to alter the placement of the graph along the x-axis.
        Note that the x-axis is across the screen, from left to right.

    -gr_yoff YOFFSET            : set the Y-offset for the graph, in pixels

        e.g. -gr_xoff 400

        Use this option to alter the placement of the graph along the y-axis.
        Note that the y-axis is down the screen, from top to bottom.


- R Reynolds  June 27, 2008
===========================================================================
"""

g_history = """
    gen_epi_review.py history:

    0.1  Jun 27, 2008: initial version
    0.2  Jun 30, 2008:
         - make script executable, decrease sleep time and add usage comment
    0.3  Sep 23, 2008: 
         - in script, check for existence of given datasets
"""

g_version = "version 0.3, Sep 23, 2008"

gDEF_VERB       = 1             # default verbose level
gDEF_IM_SIZE    = [300,300]     # image size, in pixels
gDEF_IM_XOFF    = 420           # image offset in the x direction
gDEF_IM_YOFF    = 400           # (initial) image offset in y direction
gDEF_GR_SIZE    = [400,300]     # graph size, in pixels
gDEF_GR_XOFF    = 0             # graph offset in the x direction
gDEF_GR_YOFF    = 400           # (initial) graph offset in y direction

gDEF_WINDOWS    = ['sagittal', 'axial'] # default windows to open

class GenEPIReview:
    def __init__(self, label):
        # general parameters
        self.label      = label
        self.verb       = gDEF_VERB
        self.valid_opts = None                  # OptionList - valid options
        self.user_opts  = None                  # OptionList - user supplied

        # required user parameters
        self.dsets      = []                    # list of input datasets
        self.adsets     = []                    # list of afni_name datasets

        # optional parameters
        self.script     = None                  # script name
        self.windows    = gDEF_WINDOWS          # list of windows to open

        # coordinate parameters
        self.im_size    = None                  # image width, in pixels
        self.im_xoff    = None                  # x-offset of image windows
        self.im_yoff    = None                  # initial y-offset if images
        self.gr_size    = None                  # graph width, in pixels
        self.gr_xoff    = None                  # x-offset of image windows
        self.gr_yoff    = None                  # initial y-offset if images

    def init_opts(self):
        global g_help_string
        self.valid_opts = option_list.OptionList('for input')

        # short, terminal arguments
        self.valid_opts.add_opt('-help', 0, [],      \
                        helpstr='display program help')
        self.valid_opts.add_opt('-hist', 0, [],      \
                        helpstr='display the modification history')
        self.valid_opts.add_opt('-show_valid_opts', 0, [], \
                        helpstr='display all valid options')
        self.valid_opts.add_opt('-ver', 0, [],       \
                        helpstr='display the current version number')

        # required arguments
        self.valid_opts.add_opt('-dsets', -1, [], req=1,
                        helpstr='list of input datasets')

        # optional arguments
        self.valid_opts.add_opt('-script', 1, [],
                        helpstr='name for output script')
        self.valid_opts.add_opt('-verb', 1, [],
                        helpstr='verbose level (0=quiet, 1=default, ...)')
        self.valid_opts.add_opt('-windows', -1, [],
                        acplist=['axial','coronal','sagittal'],
                        helpstr='choose afni image windows to display')

        # corrdinate arguments
        self.valid_opts.add_opt('-im_size', 2, [],
                        helpstr='image size, in pixels (2 integers)')
        self.valid_opts.add_opt('-im_xoff', 1, [],
                        helpstr='x-offset for image, in pixels')
        self.valid_opts.add_opt('-im_yoff', 1, [],
                        helpstr='y-offset for image, in pixels')

        self.valid_opts.add_opt('-gr_size', 2, [],
                        helpstr='graph size, in pixels (2 integers)')
        self.valid_opts.add_opt('-gr_xoff', 1, [],
                        helpstr='x-offset for graph, in pixels')
        self.valid_opts.add_opt('-gr_yoff', 1, [],
                        helpstr='y-offset for graph, in pixels')


    def read_opts(self):
        """check for terminal arguments, then read the user options"""

        # ------------------------------------------------------------
        # process any optlist_ options
        self.valid_opts.check_special_opts(sys.argv)

        # ------------------------------------------------------------
        # check for terminal args in argv (to ignore required args)

        # if argv has only the program name, or user requests help, show it
        if len(sys.argv) <= 1 or '-help' in sys.argv:
            print g_help_string
            return 0

        if '-hist' in sys.argv:
            print g_history
            return 0

        if '-show_valid_opts' in sys.argv:      # show all valid options
            self.valid_opts.show('', 1)
            return 0

        if '-ver' in sys.argv:
            print g_version
            return 0

        # ------------------------------------------------------------
        # now read user options

        self.user_opts = option_list.read_options(sys.argv, self.valid_opts)
        if not self.user_opts: return 1         # error condition

        return None     # normal completion

    def process_opts(self):
        """apply each option"""

        # ----------------------------------------
        # set verb first
        self.verb,err = self.user_opts.get_type_opt(int, '-verb')
        if err: return 1
        if self.verb == None: self.verb = gDEF_VERB

        # ----------------------------------------
        # required args

        opt = self.user_opts.find_opt('-dsets')
        if opt and opt.parlist:
            self.dsets = opt.parlist
            self.adsets = [BASE.afni_name(s) for s in opt.parlist]
        if len(self.dsets) < 1:
            print '** missing input dataets'
            return 1

        # ----------------------------------------
        # optional arguments

        self.script, err = self.user_opts.get_string_opt('-script')
        if err: return 1
        if self.script == None: self.script = '@review_epi_data'
        if os.path.isfile(self.script):
            print "** script file '%s' already exists, failing..."      \
                  % self.script
            return 1

        opt = self.user_opts.find_opt('-windows')
        if opt and opt.parlist:
            self.windows = opt.parlist
        if len(self.windows) < 1:
            print '** missing window list'
            return 1

        # ----------------------------------------
        # image coordinates
        self.im_size, err = self.user_opts.get_type_list(int, '-im_size',
                                2, 'two', verb=self.verb)
        if err: return 1
        if not self.im_size: self.im_size = gDEF_IM_SIZE

        self.im_xoff, err = self.user_opts.get_type_opt(int, '-im_xoff')
        if err: return 1
        if not self.im_xoff: self.im_xoff = gDEF_IM_XOFF

        self.im_yoff, err = self.user_opts.get_type_opt(int, '-im_yoff')
        if err: return 1
        if not self.im_yoff: self.im_yoff = gDEF_IM_YOFF


        # ----------------------------------------
        # graph coordinates
        self.gr_size, err = self.user_opts.get_type_list(int, '-gr_size',
                                2, 'two', verb=self.verb)
        if err: return 1
        if not self.gr_size: self.gr_size = gDEF_GR_SIZE

        self.gr_xoff, err = self.user_opts.get_type_opt(int, '-gr_xoff')
        if err: return 1
        if not self.gr_xoff: self.gr_xoff = gDEF_GR_XOFF

        self.gr_yoff, err = self.user_opts.get_type_opt(int, '-gr_yoff')
        if err: return 1
        if not self.gr_yoff: self.gr_yoff = gDEF_GR_YOFF

        # ----------------------------------------
        # check over the inputs

    def make_script(self):
        """create the review EPI (plugout_drive) script"""

        c2  = "#!/bin/tcsh\n\n"

        c2 += "# ------------------------------------------------------\n"  \
              "# review EPI data via 'afni' and 'plugout_drive'\n\n"        \
              "# note that when running this script, prompts to change\n"   \
              "# datasets will appear in the terminal window\n\n"

        c2 += "# ------------------------------------------------------\n"  \
              "# set the list of datasets\n"                                \
              "set dsets = ( %s )\n\n" %                                    \
                 ' '.join([dset.prefix for dset in self.adsets])

        c2 += '# ------------------------------------------------------\n'  \
              '# verify that the input data exists\n'                       \
              'if ( ! -f $dsets[1]+orig.HEAD ) then\n'                      \
              '    echo "** missing data to review (e.g. $dsets[1])"\n'     \
              '    exit\n'                                                  \
              'endif\n\n'

        c2 += '# ------------------------------------------------------\n'  \
              '# start afni is listening mode, and take a brief nap\n\n'    \
              'afni -yesplugouts &\n\n'                                     \
              'sleep 5\n\n'

        cmd = UTIL.add_line_wrappers(c2)

        c2  = '# ------------------------------------------------------\n'  \
              '# tell afni to load the first dataset and open windows\n\n'  \
              'plugout_drive \\\n'                                          \
              '    -com "SWITCH_UNDERLAY %s" \\\n'                          \
               % self.adsets[0].prefix

        # open windows in the list
        if self.verb>1: print '++ opening windows: %s' % ', '.join(self.windows)

        for ind in range(len(self.windows)):
            c2 += '    -com "OPEN_WINDOW %simage  \\\n'                  \
                  '                      geom=%dx%d+%d+%d" \\\n' %          \
                  (self.windows[ind], self.im_size[0], self.im_size[1], 
                   self.im_xoff+ind*self.im_size[0], self.im_yoff)

        c2 += '    -com "OPEN_WINDOW sagittalgraph  \\\n'                   \
              '                      geom=%dx%d+%d+%d" \\\n' %              \
              (self.gr_size[0], self.gr_size[1], self.gr_xoff, self.gr_yoff)

        # terminate the initial plugout_drive command
        c2 += '    -quit\n\n'

        c2  = UTIL.add_line_wrappers(c2)  # do it early, for verb

        if self.verb > 2: print 'initial drive command:\n%s' % c2

        cmd += c2

        cmd += 'sleep 2    # give afni time to open the windows\n\n\n'

        c2  = '# ------------------------------------------------------\n'  \
              '# process each dataset using video mode\n\n'                 \
              'foreach dset ( $dsets )\n'                                   \
              '    plugout_drive \\\n'                                      \
              '        -com "SWITCH_UNDERLAY $dset" \\\n'                   \
              '        -com "OPEN_WINDOW sagittalgraph  \\\n'               \
              '                          keypress=a\\\n'                    \
              '                          keypress=v"\\\n'                   \
              '        -quit\n\n'                                           \
              '    sleep 2    # wait for plugout_drive output\n\n'          \
              '    echo ""\n'                                               \
              '    echo "++ now viewing $dset, hit enter to continue"\n'    \
              '    set ret = $<    # wait for user to hit enter\n'          \
              'end\n\n\n'

        cmd += UTIL.add_line_wrappers(c2)

        c2  = '# ------------------------------------------------------\n'  \
              '# stop video mode when the user is done\n\n'                 \
              'plugout_drive -com "OPEN_WINDOW sagittalgraph keypress=s"'   \
                           ' -quit\n\n\n'                                   \
              'sleep 2    # wait for plugout_drive output\n\n'              \
              'echo ""\n'                                                   \
              'echo "data review complete"\n\n\n'

        cmd += UTIL.add_line_wrappers(c2)

        c2  = '# ----------------------------------------------------------'\
              "------\n# auto-generated by gen_epi_review.py, %s\n" % g_version
        c2 += "#\n# %s %s\n" % (os.path.basename(sys.argv[0]),
                                ' '.join(UTIL.quotize_list(sys.argv[1:],'')))

        cmd += UTIL.add_line_wrappers(c2)

        fp = open(self.script, "w")
        if not fp:
            print '** failed to open %s for writing decon script' % self.script
            return 1

        fp.write(cmd)
        fp.close()
        os.chmod(self.script, 0755)

        return

def process():
    review = GenEPIReview('EPI review script')
    review.init_opts()
    rv = review.read_opts()
    if rv != None:      # 0 is okay, else error code
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command (RO):")
        return rv

    rv = review.process_opts()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command (PO):")
        return rv

    rv = review.make_script()
    if rv != None:
        if rv: UTIL.show_args_as_command(sys.argv,"** failed command (MS):")
        return rv

    return 0

if __name__ == "__main__":
    rv = process()
    sys.exit(rv)

