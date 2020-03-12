#
#
#
#ver='1.0' ; date='Oct 22, 2019'
# + [PT] start
#
# ...
#
ver='3.0' ; date='March 12, 2020'
# + [PT] Deemed unnecessary after discussions and useful feedback from
#        the FS folks---thanks, D. Greve!
#
###############################################################################

import sys, os
import afni_base as BASE

ddefs = {}

all_opts = {
    'ver'                : '-ver',
    'date'               : '-date',
    'h'                  : '-h',
    'help'               : '-help',
    'hview'              : '-hview',
}

all_opts_vals = list(all_opts.values())

# ----------------------------------------------------------------------------

# this should make a single dict in py23; 
# preferred {**all_opts, **ddefs} is only in py3
help_dict = all_opts.copy()
help_dict.update(ddefs)     

help_string_this_prog = '''

  This program has actually been deemed to *not* be necessary, after
  all.

'''.format( **help_dict )

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

def parse_args_this_prog(full_argv):

    argv = full_argv[1:]
    Narg = len(argv)

    if not(Narg):
        print(help_string_this_prog)
        sys.exit(0)
    
    i = 0
    while i < Narg:
        if argv[i] == "{ver}".format(**all_opts) :
            print(ver)
            sys.exit(0)

        elif argv[i] == "{date}".format(**all_opts) :
            print(date)
            sys.exit(0)

        elif argv[i] == "{help}".format(**all_opts) or \
             argv[i] == "{h}".format(**all_opts) :
            print(help_string_this_prog)
            sys.exit(0)

        elif argv[i] == "{hview}".format(**all_opts) :
            prog = os.path.basename(full_argv[0])
            cmd = 'apsearch -view_prog_help {}'.format( prog )
            BASE.simple_shell_exec(cmd)
            sys.exit(0)

        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    # --------------------------------------------------------------------

    return 0


# --------------------------------------------------------------------------


