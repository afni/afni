#
#
#
ver='1.0' ; date='July 18, 2019'
# + [PT] start
#
#
###############################################################################

import sys, os
import json
import afni_base as BASE
import afni_util as UTIL

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

ddefs = {
    'DEF_ver'        : ver,
    'DEF_date'       : date,
}

# ----------------------------------------------------------------------------

all_opts = {
    'prefix'             : '-prefix',
    'overwrite'          : '-overwrite',
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

help_string_b0_corr = '''
  PURPOSE ~1~

  This program does SOMETHING.

  Ver  : {DEF_ver}
  Date : {DEF_date}


  INPUTS ~1~

  OUTPUTS ~1~

  RUNNING ~1~


  {help}                : display program help in terminal (consider
                         '-hview' to open help in a separate text editor)
  {ver}                 : display program version number in terminal 
  {date}                : display date of program's last update in terminal 


  NOTES ~1~

  EXAMPLES ~1~



'''.format( **help_dict )

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

def check_for_shell_com_failure( com, cmd, 
                                 exit_on_fail=True,
                                 jump_home_on_exit='',
                                 disp_so=True, disp_se=True ):

    '''Wrapper for check_for_shell_exec_failure()

    '''

    so = '\n'.join(com.so)
    se = '\n'.join(com.se)

    check_for_shell_exec_failure( com.status, so, se, cmd, 
                                  exit_on_fail,
                                  jump_home_on_exit,
                                  disp_so, disp_se )

# ----------------------------------------------------------------------------

def check_for_shell_exec_failure( status, so, se, cmd, 
                                  exit_on_fail=True,
                                  jump_home_on_exit='',
                                  disp_so=True, disp_se=True ):
    '''When using afni_base.py's simple_shell_exec(), parse the output
    status/so/se and decide what to do.  

    By default, this will exit the program on failure.

    '''

    if status:
        print("** ERROR executing this command in the shell:\n"
              "   {}\n"
              "".format(cmd))
        print("** Exit status    : {}".format(status))
        print("** Standard error : {}".format(se))

        if exit_on_fail :

            if jump_home_on_exit :
                os.chdir( jump_home_on_exit )
            sys.exit(22)
    else:
        if disp_so :
            print(so)
        if disp_se :
            print(se)

# ==========================================================================

# read in inputs for this program, and store info in this obj
class iopts_this_prog:

    def __init__(self):

        self.full_cmd       = ''           # existential...

        self.code_ver       = ddefs['DEF_ver']
        self.afni_ver       = ''


        self.overwrite      = ''           # pass along overwrite flag (or not)

        # This is here basically to just initialize the 'comm' obj
        cmd = 'pwd'
        self.comm = BASE.shell_com( cmd, capture=1 )
        self.comm.run()
        check_for_shell_com_failure( self.comm, cmd,
                                     disp_so=False, disp_se=False )


    # -------------- methods to populate -----------------

    def set_overwrite( self, ll ):
        # 'll' is just true or false here
        if ll :
            self.overwrite = '-overwrite'
        else:
            self.overwrite = ''

    def set_afni_ver( self, full_ver_info ):
        ver_list = full_ver_info.split()
        indV     = ver_list.index('(Version')

        ver_str  = ver_list[indV+1]
        ver_str += ' (' + ' '.join(ver_list[:3]) + ')'

        # refine+shorten
        aa = ver_str.replace(':', '')
        bb = aa.replace('Precompiled', 'Precomp')
        cc = bb.replace('binary', 'bin')
        self.afni_ver = cc

    def set_full_cmd( self, dd ):
        self.full_cmd = dd



    # ---------- fill in vals ----------------

    def finish_defs(self):

        # get some basic versions that matter
        cmd = '''afni -ver'''
        com = BASE.shell_com(cmd, capture=1, save_hist=0)
        com.run()
        check_for_shell_com_failure(com, cmd, disp_so=False, disp_se=False )
        self.set_afni_ver(com.so[0])


    # ---------- check ----------------

    def check_req(self):
        ''' Check for and point out any missing inputs.'''
        MISS = 0

        #if not(self.prefix) :
        #    print("** ERROR: missing '{prefix}' info"
        #          "".format(**all_opts))
        #    MISS+=1

        return MISS

    # ---------------------------------------------------------------------
    # ---------------------------------------------------------------------



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

def parse_args_this_prog(full_argv):

    argv = full_argv[1:]
    Narg = len(argv)

    if not(Narg):
        print(help_string_this_prog)
        sys.exit(0)
    
    # initialize objs
    iopts  = iopts_this_prog()

    iopts.set_full_cmd(full_argv)

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
            print(help_string_b0_corr)
            sys.exit(0)

        elif argv[i] == "{hview}".format(**all_opts) :
            prog = os.path.basename(full_argv[0])
            cmd = 'apsearch -view_prog_help {}'.format( prog )
            BASE.simple_shell_exec(cmd)
            sys.exit(0)

        # ---------- req ---------------

        elif argv[i] == "{prefix}".format(**all_opts) :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_prefix(argv[i])

        elif argv[i] == "{overwrite}".format(**all_opts) :
            iopts.set_overwrite(True)


        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("   -------------------------------")
        print("** ERROR with input arguments (see detailed whining above).")
        sys.exit(1)

    iopts.finish_defs()  # make some paths we need later

    return iopts


# --------------------------------------------------------------------------


