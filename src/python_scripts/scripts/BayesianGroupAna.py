#!/usr/bin/env python

########################################################################
## 05/2018 Justin Rajendra
## wrapper for Gang's Bayesian Group Analysis

## system libraries
import sys, os, glob, subprocess, re, argparse, csv, signal, textwrap, shutil

## location and name of the R program that is run
R_script = sys.path[0]+'/BayesianGroupAna.R'

## check for valid integers
def int_gt_0(v):
    try:
        vi = int(v)
        if vi >= 1:
            return v
        else:
            raise argparse.ArgumentTypeError("%r is not an integer >= 1."%(v,))
    except:
        raise argparse.ArgumentTypeError("%r is not an integer >= 1."%(v,))

def zero2one(p):
    try:
        float(p)
    except:
        raise argparse.ArgumentTypeError("%r is not positive value < 1."%(p,))
    if float(p) < 0.0 or float(p) > 1.0:
        raise argparse.ArgumentTypeError("%r is not positive value < 1."%(p,))
    return p

## check for rectiness of mvm table
def data_is_rect(mdata):
    if mdata == None: return 1
    if len(mdata) == 0: return 1
    rlen = len(mdata[0])
    for row in mdata:
        if len(row) != rlen: return 0
    return 1

########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
------------------------------------------
Overview ~1~

    This program conducts Bayesian Group Analysis (BGA) on a list
    (e.g., 6 or more) of regions of interest (ROIs) as laid out in Chen et al.
    (2018, https://www.biorxiv.org/content/early/2018/02/20/238998).
    Compared to the conventional univariate GLM in which each voxel or ROI is
    considered autonomous and analyzed independently, BGA pools and shares the
    information across the ROIs in a multilevel system. It is the
    probability of incorrect sign, instead of false positive rate that is
    controlled. In other words, there is only one BGA model that incorporates
    the data from all ROIs.

    This will explore the effect of X on Y at each ROI. The computation may
    take a few minutes or more depending on the amount of input data and
    model complexity. The final inferences are conducted through the
    posterior distribution or quantile intervals for each effect that are
    provided in a table in the output. A boxplot can also be generated if
    requested with -plot.

    The computation requires that the R package "brms" be installed
    (e.g., through rPkgsInstall).
    More info on the brms package can be found here:
        https://CRAN.R-project.org/package=brms
    And the brms reference manual is here:
        https://cran.r-project.org/web/packages/brms/brms.pdf

Details ~1~

    Similar to 3dMVM and 3dLME, a data table  should be created containing
    the input data and relevant variables (with at least 3 columns: subject
    labels, ROI labels, response variable values).

    The -dataTable should be formated as follows:

        Subj  ROI   some_y  some_x other_x
        S001  roi1  0.12    0.056  0.356
        S001  roi2  0.65    0.232  0.231
        S002  roi1  0.14    0.456  0.856
        S002  roi2  0.64    0.432  0.431
        ...

    The Subj and ROI columns must be included with the exact spelling!!
    If there are no x variables, only the intercept will be calculated.

Outputs ~1~

    Given -prefix is "gangBGA" and -x is "some_x", the default outputs are the
    following files:

        gangBGA_summary.txt:
            Summary of the brmsfit object from R.

        gangBGA_rhats.csv:
            rhats for each effect and x variable combination.

        gangBGA_Intercept_table.csv:
            Table with the MedianEst, StdDev, 2.50%, 5%, 50%, 95%, and 97.50%
            of each ROI for the Intercept term.

        gangBGA_some_x_table.csv:
            The same table as the Intercept but for the some_x variable.

Caveats ~1~

    All x variables are centered by default.

    The boxplot with -plot is not a standard boxplot.
    It is a plot of the 2.50%, 5%, 50%, 95%, 97.50% percentiles.
    The coloring of the boxes is determined by where the zero line crosses the
    box and whiskers.
        White:  The zero line crosses the main box (between 5% and 95%).
        Purple: The zero line crosses between the whiskers and the main box.
                (2.50% to 5%) OR (95% to 97.50%)
        Red:    The zero line does not cross the box or the whiskers.

    Additional plot types for -more_plots include (not sure all of these work):
        hist dens hist_by_chain dens_overlay violin intervalsareas
        acf acf_bar trace trace_highlight rhat rhat_hist neff neff_hist
        nuts_acceptance nuts_divergence nuts_stepsize nuts_treedepth
        nuts_energy

    Tables and plots will be created for the intercept and all specified x
    variables separately. So there may be a lot of output.

Examples ~1~

    Minimum requirement only calculates the intercept (may not be useful).
        BayesianGroupAna.py -dataTable my_roi_data.txt -y zscore

    More useful. Calculates 2 x variables and saves out some plots.
        BayesianGroupAna.py -dataTable my_roi_data.txt  \\
                            -prefix dock_of_the_bayes   \\
                            -y zscore -x some_x other_x \\
                            -chains 4 -iterations 1000  \\
                            -plot -more_plots rhat violin

------------------------------------------

Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 05/2018
4 Gang Box...
Keep on keeping on!
------------------------------------------
                                 '''))

parser._optionals.title = 'Optional arguments'
required = parser.add_argument_group('Required arguments')

parser._action_groups.reverse()

## required
required.add_argument('-dataTable',type=str,help='Input text file.',
                      required=True)
required.add_argument('-y',type=str,help='Column name for the y variable.',
                      required=True,metavar='VAR')

## optional
parser.add_argument('-help',action='help',help='Show this help.')
parser.add_argument('-prefix',type=str,default="BayesOut",
                    help="Name of the output file.")
parser.add_argument('-x',type=str,default='1',nargs='+',metavar='VAR',
                      help=('Column name for the x variables. '+
                            'If not specified, only the intercept will be '+
                            'added.'))
parser.add_argument('-no_center',action='store_true',default=False,
                    help=("Disable centering on the x variables. "+
                          "Maybe useful if you centered manually."))
parser.add_argument('-iterations',type=int_gt_0,default=1000,metavar='ITER',
                    help=("Number of total iterations per chain including"+
                          " warmup. Default [1000]"))
parser.add_argument('-chains',type=int_gt_0,default=4,
                    help="Number of Markov chains. Default [4]")
parser.add_argument('-control_list',type=str,default="",metavar="LIST",
                    help=("Comma separated list of control parameters to"+
                          " pass to the brm function. (example:"+
                          " 'adapt_delta=0.99,max_treedepth=20')."+
                          " Default is the brm function defaults"))
parser.add_argument('-plot',action='store_true',default=False,
                    help="Output box, fit, and posterior prediction plots.")
parser.add_argument('-more_plots',type=str,default='',nargs='+',
                    metavar='TYPE',help=('Output "stanplots" given different'+
                                         ' types of plot names.'))
parser.add_argument('-RData',action='store_true',default=False,
                    help="Save the R session workspace and data.")
parser.add_argument('-seed',type=int_gt_0,default=1234,
                    help="Seed to generate random number. Default [1234]")
parser.add_argument('-overwrite',action='store_true',default=False,
                    help="Overwrites the output files.")

## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

## do the parsing
args = parser.parse_args()

########################################################################
## collect the arguments (need strings) and validate
InFile = args.dataTable
y_var = args.y
x_var = args.x
OutFile = args.prefix
NumIter = str(args.iterations)
NumChains = str(args.chains)
control_list = args.control_list
InSeed = str(args.seed)
StanPlots = args.more_plots
overwrite = args.overwrite

## make caps for R code (check for plots)
plot = "TRUE" if args.plot or len(StanPlots) > 0 else "FALSE"
NoCenter = "TRUE" if args.no_center else "FALSE"
RData = "TRUE" if args.RData else "FALSE"

########################################################################
## validate

## check for output rhats
if os.path.isfile(OutFile+"_rhats.csv") and not overwrite:
    print("\nERROR: Output file ("+OutFile+"_rhats.csv) exists!!\n")
    sys.exit(1)

## check for input file
if not os.path.isfile(InFile):
    print("\nERROR: Input file ("+InFile+") not found!!\n")
    sys.exit(1)

## make sure the table is valid
test_table = []
with open(InFile) as fin:
    rows = (re.split('[ \t,;]+', line) for line in fin)  ## any delimiter?
    for row in rows:
        test_table.append(row)
if not data_is_rect(test_table):
    print("\nERROR: Input file ("+InFile+") is not a rectangular table!!\n")
    sys.exit(1)

## check plotting options
## standard plot choices
stanplot_types = ['hist','dens','hist_by_chain','dens_overlay','violin',
                  'intervals','areas','acf','acf_bar','trace',
                  'trace_highlight','rhat','rhat_hist','neff',
                  'neff_hist','nuts_acceptance','nuts_divergence',
                  'nuts_stepsize','nuts_treedepth',' nuts_energy']

## do not work yet: 'scatter',

## check the plotting types
if plot == "TRUE":
    print("\nPlotting: \nboxes\nfit\nPostPred")
    if set(StanPlots) < set(stanplot_types):
        for s in StanPlots: print(s)
        print("")
        StanPlots = " ".join(StanPlots)
    else:
        print("")
        for p in StanPlots:
            if p not in stanplot_types:
                print("ERROR: "+p+" is not a valid plot type!!!")
        print("")
        sys.exit(1)

########################################################################
## check the variable names

print("\nVariables in the table are:\n")
var_names = [item.strip() for item in test_table[0]]
for var in var_names: print(var)
print("")

if "Subj" not in var_names:
    print("\nERROR: Missing 'Subj' column!! Spelling counts!!\n")
    sys.exit(1)
if "ROI" not in var_names:
    print("\nERROR: Missing 'ROI' column!! Spelling counts!!\n")
    sys.exit(1)
if y_var not in var_names:
    print("\nERROR: "+y_var+" is not in the list of variables!! "+
          "Spelling counts!!\n")
    sys.exit(1)
if x_var != '1':
    for x in x_var:
        if x not in var_names:
            print("\nERROR: "+x+" is not in the list of variables!!"+
                  " Spelling counts!!\n")
            sys.exit(1)

## convert x_vars to space separated string
x_var = " ".join(x_var)

## add intercept if necessary
if x_var is not "1":
    x_var = "1 "+x_var

########################################################################
## run the R script with arguments

subprocess.call(['Rscript',R_script,InFile,y_var,x_var,OutFile,
                 NumIter,NumChains,control_list,InSeed,os.getcwd(),plot,
                 StanPlots,RData,NoCenter])

## check output
if os.path.isfile(OutFile+"_rhats.csv"):
    print("\nProcessing complete! Output is "+OutFile+"_rhats.csv\n")
    sys.exit(0)
else:
    print("\nERROR: Something failed in R!! There is no output file!\n")
    sys.exit(1)


