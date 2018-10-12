#!/usr/bin/env python

########################################################################
## 03/2018 Justin Rajendra
## prep data from afni_proc.py for tedana.py
## 05/07/2018: removed the mean addition
## 05/15/2018: added local check_output [rickr]

## system libraries
import sys, os, glob, subprocess, re, argparse, signal, textwrap, shutil

# sys.path.insert(0,afni_dir)
import afni_base, afni_util


########################################################################
## functions


# Local check_output(), since subprocess.check_output did
# not exist prior to p2.7.            15 May 2018 [rickr]
def check_output(cmd, showerr=1):
    """return text output from the given command, executed via 'tcsh -c cmd'

       if showerr and there is a command error, show it
    """
    status, output = afni_util.exec_tcsh_command(cmd)
    if status and showerr:
        print(cmd)
        print(output)
    return output

# p = subprocess.Popen("ls -a -l", stdout=subprocess.PIPE,shell=True)
# output = p.communicate()[0]
# print(output)
# print(p.returncode)
# sys.exit(1)

def exec_or_error(cmd_str,error_msg="ERROR!!!"):
    """Execute subprocess.Popen(cmd_str,stdout=subprocess.PIPE,shell=True)
       Return status and shell output.
       On non 0 status print error_msg and exit.
    """
    print("")
    print(cmd_str)
    sys.stdout.flush()

    # use local library, which returns strings instead of bytes
    retcode, cmd_output = afni_util.exec_tcsh_command(cmd_str)
    if retcode != 0:
       print("\n%s\n\n%s" % (error_msg, cmd_output))
       sys.exit(1)
    return retcode, cmd_output

    #p = subprocess.Popen(cmd_str,stdout=subprocess.PIPE,shell=True)
    #cmd_output = p.communicate()[0]
    #print("")
    #if p.returncode != 0:
    #    print("\n"+error_msg+"\n\n"+cmd_output)
    #    sys.exit(1)
    #return p.returncode,cmd_output


########################################################################
## locations of stuff

# use local check_output, subprocess might not have it
# afni_bin = subprocess.check_output("which afni",shell=True)
afni_bin = check_output("which afni")
# afni_dir = os.path.dirname(afni_bin)
afni_dir = sys.path[0]


########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
------------------------------------------
Overview ~1~

    Internal wrapper to run tedana.py.
    Usually only run from within afni_proc.py.

Caveats ~1~

    Nearly all of the tedana.py options will be the defaults unless the user
    specifies them with the -tedana_prog argument. See the help from tedana.py
    for valid options.

Example ~1~

    tedana_wrapper.py -TE 11 22.72 34.44 \\
    -mask masked_bandit+tlrc \\
    -input echo_01+tlrc echo_02+tlrc echo_03+tlrc \\
    -tedana_opts "--initcost=tanh --conv=2.5e-5 --kdaw=10"

------------------------------------------

Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 02/2018
I hope this will be useful for someone...
Keep on keeping on!
------------------------------------------
                                 '''))

parser._optionals.title = 'Optional arguments'
tedana = parser.add_argument_group('tedana arguments')
required = parser.add_argument_group('Required arguments')

parser._action_groups.reverse()

## required
required.add_argument('-input',type=str,help='4D dataset for each echo.',
                      required=True,metavar='DSETS',nargs='+')
required.add_argument('-TE',type=float,help=('Echo time (ms) for each echo.'),
                      required=True,metavar='ms',nargs='+')
required.add_argument('-mask',type=str,required=True,
                      help=('Mask in same space/grid as the input datasets.'))

## optional
parser.add_argument('-help',action='help',help='Show this help.')
parser.add_argument('-results_dir',type=str,default="./Bunnymen",metavar='DIR',
                    help=("Folder to be created for all outputs. "+
                          "Default [./Bunnymen]."))
parser.add_argument('-prefix',type=str,default="Bunnymen",
                    help="Prefix for dataset names. Default [Bunnymen].")
parser.add_argument('-save_all',action='store_true',default=False,
                    help=("Save intermediate datasets. Default is to save "+
                          "only the 3dZcat stacked dataset (and tedana stuff)"))

## tedana
tedana.add_argument('-prep_only',action='store_true',default=False,
                    help="Do not run tedana.py, stop at 3dZcat.")
tedana.add_argument("-tedana_prog",type=str,
                    help=("Path and name of the version of tedana.py that "+
                          "will be run."+"Default is meica.libs/tedana.py "+
                          "in the afni binaries directory."))
tedana.add_argument('-tedana_is_exec',action='store_true',default=False,
                    help="Run 'tedana.py' rather than 'python tedana.py'.")
# tedana.add_argument('-sourceTEs',type=int,default="-1",nargs='+',metavar="STEs",
#                       help=("Source TEs for models. Examples: -sourceTEs 2 3; "+
#                             "-sourceTEs 0 for all; -sourceTEs -1 for "+
#                             "optimally combined. Default [-1]."))
# tedana.add_argument("-kdaw",type=float,default=10,
#                     help=("Dimensionality augmentation weight (Kappa). "+
#                           "-1 for low-dimensional ICA. Default [10]."))
# tedana.add_argument("-rdaw",type=float,default=1,
#                     help=("Dimensionality augmentation weight (Rho). "+
#                           "Use -1 for low-dimensional ICA. Default [1]."))
tedana.add_argument('-ted_label',type=str,default="TED",metavar="LABEL",
                    help=("Suffix for output folder. "+
                          "Adds suffix like TED.LABEL (NOT A PATH)"))
tedana.add_argument("-tedana_opts",type=str,default="",metavar="'OPTS'",
                    help=("Additional options to pass to tedana.py. "+
                    "(In quotes) Example: '--initcost=tanh --conv=2.5e-5'"))

## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

## do the parsing
args = parser.parse_args()

########################################################################
## collect the arguments
echos = args.input
mask = args.mask
TED_label = args.ted_label
overwrite = ""
OutFolder = args.results_dir
OutName = args.prefix
save_all = args.save_all
prep_only = args.prep_only
ted_bin = args.tedana_prog
tedana_is_exec = args.tedana_is_exec
# kdaw = str(args.kdaw)
# rdaw = str(args.rdaw)

# this argument is actually a list of options, so it might
# have newline characters inside        4 Apr 2018 [rickr]
tedana_opts = args.tedana_opts.replace('\n', ' ')

## combine stuff with commas for tedana.py input
TEs = ','.join(map(str,args.TE))
# sourceTEs = ','.join(map(str,args.sourceTEs))

########################################################################
## validate

## make sure there are matching TEs, echos, and sourceTEs
if len(args.TE) != len(echos):
    print("\nERROR: The number of TEs ("+str(len(args.TE))+") do not match "+
          "the number of input datasets ("+str(len(echos))+")!!\n")
    sys.exit(1)

# ## check to see if the specified source TEs is among the possible
# possible_source_TEs = [-1,0]+range(1,len(args.TE)+1)
# possible_source_TEs = set(possible_source_TEs)
# if not set(args.sourceTEs).issubset(possible_source_TEs):
#     print("\nERROR: The specified source TEs ("+str(args.sourceTEs)+
#           ") are not valid!!\n")
#     sys.exit(1)

## make sure the output folder is not there
if os.path.isdir(OutFolder):
    print("\nERROR: -results_dir folder ("+OutFolder+") exists!!\n")
    sys.exit(1)
if OutFolder[-1] != "/":
    OutFolder = OutFolder+"/"

## check for tedana.py
if not prep_only:
    if ted_bin is None:
        ted_bin = afni_dir+"/meica.libs/tedana.py"
    if not os.path.exists(ted_bin):
        print("\nERROR: tedana.py version: "+ted_bin+" not found!!\n")
        sys.exit(1)
    else:
        print("\nUsing: "+ted_bin+" for tedana.py.\n")

########################################################################
## proc loop

## place holder
p50 = ""

## create the mask object here
mask_dset = afni_base.afni_name(mask)

for e in range(0,len(echos)):

    ## make names for the intermediate datasets
    cur_echo = afni_base.afni_name(echos[e])

    mask_echo = afni_base.afni_name(echos[e])
    mask_echo.new_prefix(OutName+"_echo_"+str(e+1)+"_masked")
    mask_echo.new_path(OutFolder)

    # mask_scaled_echo = afni_base.afni_name(echos[e])
    # mask_scaled_echo.new_prefix(OutName+"_echo_"+str(e+1)+"_masked_scaled")
    # mask_scaled_echo.new_path(OutFolder)

    # mean_echo = afni_base.afni_name(echos[e])
    # mean_echo.new_prefix(OutName+"_echo_"+str(e+1)+"_mean")
    # mean_echo.new_path(OutFolder)

    out_echo = afni_base.afni_name(echos[e])
    out_echo.new_prefix(OutName+"_echo_"+str(e+1)+"_preZcat")
    out_echo.new_path(OutFolder)

    ## mask
    cmd_str = ("3dcalc "+overwrite+" -a "+mask_dset.real_input()+
               " -b "+cur_echo.real_input()+" -expr 'a*b' "+
               "-prefix "+mask_echo.ppv())
    exec_or_error(cmd_str,"ERROR: Masking failed!!!")

    ## get percentiles if the first echo
    if e == 0:
        cmd_str = ("3dBrickStat -mask "+mask_dset.real_input()+" "+
                    "-percentile 50 1 50 "+mask_echo.ppv()+"'[0]'")
        error_msg = "ERROR: 3dBrickStat failed!!"
        cmd_status,cmd_output = exec_or_error(cmd_str,error_msg)
        p50 = cmd_output.split()[1]

    ## scale
    #  for python3, use %s for string conversion, rather than + 
    #  (might be a moot point, now that exec_or_error() uses afni_util)
    cmd_str = "3dcalc -float %s -a %s -expr 'a*10000/%s' -prefix %s" \
              % (overwrite , mask_echo.ppv(), p50, out_echo.pp())
    exec_or_error(cmd_str,"ERROR: Scaling failed!!!")

    # ## mean
    # cmd_str = ("3dTstat "+overwrite+" -prefix "+mean_echo.pp()+" "+
    #            mask_scaled_echo.ppv())
    # exec_or_error(cmd_str,"ERROR: Mean failed!!!")
    #
    # ## output strange addition
    # cmd_str = ("3dcalc -float "+overwrite+" -a "+mask_scaled_echo.ppv()+
    #            " -b "+mean_echo.ppv()+" -expr 'a+b' "+
    #            "-prefix "+out_echo.pp())
    # exec_or_error(cmd_str,"ERROR: Mean addition failed!!!")

    ## clean up
    if not save_all:
        mask_echo.delete()
        # mask_scaled_echo.delete()
        # mean_echo.delete()

###################################
## stack up

## make name and get view string
Zcat_all = afni_base.afni_name(echos[0])
out_view = Zcat_all.initial_view()
Zcat_all.new_prefix(OutName+"_Zcat_all.nii.gz")
Zcat_all.new_path(OutFolder)
Zcat_all.new_view()

## stack up into NIFTI
afni_cmd = ("3dZcat "+overwrite+" -prefix "+Zcat_all.pp()+" "+
            OutFolder+OutName+"_echo_*_preZcat"+out_view+".HEAD")
# subprocess.check_output(afni_cmd,shell=True)
check_output(afni_cmd)

## clean up
if not save_all:
    for f in glob.glob(OutFolder+OutName+"_echo_*_preZcat"+out_view+".*"):
        os.remove(f)
else:
    ## compress the intermediates
    print("\nCompressing intermediate datasets....\n")
    afni_cmd = ("gzip -v "+OutFolder+"*.BRIK")
    # subprocess.check_output(afni_cmd,shell=True)
    check_output(afni_cmd)
    print("\nCompression complete!!\n")

## stop here?
print("Output Zcat dataset is "+Zcat_all.pp()+"\n")

###################################
## tedana
if not prep_only:

    ## generate the script
    if tedana_is_exec:
        tedana_script = ("\n"+ted_bin+" \\\n"+
                         "--label "+TED_label+" \\\n"+
                         "--orig_data "+Zcat_all.pp()+" \\\n"+
                         "--TEs "+TEs+" \\\n"+
                         tedana_opts+"\n")
    else:
        tedana_script = ("\npython "+ted_bin+" \\\n"+
                         "--label "+TED_label+" \\\n"+
                         "--orig_data "+Zcat_all.pp()+" \\\n"+
                         "--TEs "+TEs+" \\\n"+
                         tedana_opts+"\n")

    ## change directory
    os.chdir(OutFolder)

    ## save the script out to a file
    ted_out = open(OutName+"_tedana_command.txt","w")
    ted_out.write(tedana_script)
    ted_out.close()
    print(tedana_script)
    sys.stdout.flush()    # force output before system call

    ## and run it
    os.system(tedana_script)

sys.exit(0)
