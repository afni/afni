#!/usr/bin/env python

########################################################################
## 07/31/2017 Justin Rajendra
## parse data and make shiny app

## system libraries
import sys, os, glob, subprocess, csv, re, shutil, argparse, signal, textwrap
import random

# AFNI libraries
# import afni_util as UTIL
# import option_list as OL
## to do
## add MinVox to output

## locations of stuff
afni_bin = subprocess.check_output("which afni",shell=True)
afni_dir = os.path.dirname(afni_bin)
HistProg = afni_dir+"/ClustExp_HistTable.py"
ShinyFolder = afni_dir+"/shiny/ClustExp_ShinyTemplate"

########################################################################
## definitions

## check for rectiness of mvm table
def data_is_rect(mdata):
    if mdata == None: return 1
    if len(mdata) == 0: return 1
    rlen = len(mdata[0])
    for row in mdata:
        if len(row) != rlen: return 0
    return 1

## to check for valid input options with numbers
def need_pos_float(x):
    x = float(x)
    if x < 0.0:
        raise argparse.ArgumentTypeError("%r is not positive"%(x,))
    return x
def non_neg_int(x):
    ivalue = int(x)
    if ivalue < 0:
         raise argparse.ArgumentTypeError("%s is not >= 0" % x)
    return ivalue
def min_vox_two(x):
    ivalue = int(x)
    if ivalue <= 2:
         raise argparse.ArgumentTypeError("%s is not >= 2" % x)
    return ivalue

## random prefix for temp files
def randomDigits(digits):
    lower = 10**(digits-1)
    upper = 10**digits - 1
    return random.randint(lower, upper)
TempPrefix = "__"+str(randomDigits(5))

## need a cleanup function
def cleanUp(message):
    try:
        os.remove(TempPrefix+"_val_delete_me00.dat")
    except OSError:
        pass
    try:
        os.remove(TempPrefix+"_whereami_delete_me00.txt")
    except OSError:
        pass
    try:
        os.remove(TempPrefix+"_peak_delete_me00.txt")
    except OSError:
        pass
    try:
        os.remove(CleanTab)
    except OSError:
        pass
    print("")
    print(message)
    print("")

## 3dMVM and 3dLME table extraction function
def mvm_lme_extract(hist_in):

    ## split the column headers from the table
    mvm_hist = hist_in.split("-dataTable")[1]
    mvm_hdr = mvm_hist.split("InputFile")[0]+"InputFile"
    mvm_hdr = mvm_hdr.split()
    mvm_tab = mvm_hist.split("InputFile")[1]
    mvm_tab = mvm_tab.splitlines()[0]
    mvm_tab = mvm_tab.split()

    ## turn into table
    mvm_tab = [mvm_tab[n:n+len(mvm_hdr)] for n in range(0,len(mvm_tab),
                                                        len(mvm_hdr))]
    ## check for rectiness and return
    if data_is_rect(mvm_tab):
        return (mvm_tab, mvm_hdr)
    else:
        cleanUp("Data table is not rectangular")
        sys.exit(1)

########################################################################
## parse command line arguments / build help

## make parser with help
parser = argparse.ArgumentParser(prog=str(sys.argv[0]),
                                 formatter_class=argparse.RawDescriptionHelpFormatter,
                                 description=textwrap.dedent('''\
------------------------------------------
## Overview ~1~

## Input datasets ~2~
All data must be in the same space and aligned to the same template.
And must be +tlrc or .nii or .nii.gz, +orig should fail.
For the master, you need the full path.
It does not have to be the same voxel size as the subject and stats data sets.
This will resample the grid of the master to match the other data sets.
I will add a lookup for the built ins later.

## Subject table ~2~
The -SubjTable needs to be 3 columns.
1: Subject ID
2: Data set and current location path.
3: Data set and path at the time of running the analysis (to match the history).
The input files to your 3dttest++ or 3dMVM must be included in your input
subjects table -SubjTable and match EXACTLY!
If you put ./subjects/subj1.nii.gz[0] in the analysis, the -SubjTable
must have the same exact string.
This is to take care of paths like: ./subjects/subj1/data.nii.gz[0].

## Caveats ~2~
Statistics image must be DIRECTLY from 3dttest++ or 3dMVM.
3dttest++ must have been run with no covariates.

For now only some simple models will work with the shiny app.
GLTs included in the 3dMVM command will be ignored in the shiny app.
But the data table from the output should still be useful.

If you did 3dcopy or something else to your data set after analysis,
you may not have the history information necessary for this process.

Only outputs NIfTI images, as they are easier for the shiny app.
------------------------------------------

## Outputs ~1~

Outputs files named with your -prefix and some with the -p
(as example -prefix disco -p 0.01):

disco_p_uncor_0.01_mean.csv:
    Table with all data extracted from all of your subjects.
    The column headers are the coordinates of the center of mass of the cluster.
    The values are means of each cluster for that subject.

disco_GroupTable.csv:
    Table with information parsed from the statistics data set history.
    Includes subject ID, any grouping variables, and input data sets.

disco_p_uncor_0.01_3dclust.1D:
    Output directly from 3dclust with orientation of LPI.

disco_p_uncor_0.01_clusters.csv:
    Cleaned up version of the whereami output. Includes labels the FIRST entry
    of your search atlas. The default atlas is TT_Daemon. If nothing is found,
    there is an NA, but this gets replaced by the coordinate in the shiny app.

disco_StatInfo.csv:
    Some summary info for the shiny app. Includes most of the command line
    arguments and things parsed from the statistics data set history.

disco_p_uncor_0.01.nii.gz:
    A new data set from your input statistics data set, thresholded at your
    uncorrected p value using the selected subbriks.

disco_p_uncor_0.01_mask.nii.gz:
    An integer labeled mask of the above image with cluster sizes at least
    as big as the -MinVox (default 100 may be too much for larger voxel sizes).

disco_master.nii.gz:
    A NIfTI copy of the master file provided that may have been resampled.
    This is for the shiny app.

------------------------------------------

## Options ~1~

                                 '''),epilog=textwrap.dedent('''\
------------------------------------------
Justin Rajendra circa 09/2017
I hope this will be useful for someone...
Keep on keeping on!
------------------------------------------
                                 '''))

parser._action_groups.pop()
required = parser.add_argument_group('required')
optional = parser.add_argument_group('optional')

## required
required.add_argument('-StatDSET',type=str,help='Statistics dataset.',
                      required=True)
required.add_argument('-MeanBrik',type=non_neg_int,required=True,
                      metavar='MEANBK',
                      help='Mean subbrik (integer >= 0).')
required.add_argument('-ThreshBrik',type=non_neg_int,required=True,
                      metavar='THRESHBK',
                      help=("Threshold subbrik. Might be the same "+
                            "as MeanBrik (integer >= 0)."))
required.add_argument('-SubjDSET',type=str,required=True,
                      help=("Labeled dataset with all subjects "+
                            "(from @ClustExp_CatLab)."))
required.add_argument('-SubjTable',type=str,required=True,
                      help='Table with subject labels and input datasets.')
required.add_argument('-master',type=str,required=True,
                      help='Master data set for underlay.')

## optional
optional.add_argument('-prefix',type=str,default="MyOutput",
                      help="Name for output (no path). [MyOutput]")
optional.add_argument('-p',type=need_pos_float,default=0.005,metavar='PVAL',
                      help="Uncorrected p value for thresholding. [0.005]")
optional.add_argument('-MinVox',type=min_vox_two,default=20,
                      help="Minimum voxels in cluster. [20]")
optional.add_argument('-atlas',type=str,default="TT_Daemon",
                      help="Atlas name for lookup. (list at: whereami -help) [TT_Daemon]")
optional.add_argument('-session',type=str,default="./",
                      help=("Output parent folder if you don't want the "+
                            "current working directory. [./]"))
optional.add_argument('-NoShiny',action="store_true", default=False,
                      help="Do not create shiny app.")
optional.add_argument('-overwrite',action="store_true", default=False,
                      help="Remove previous folder with same PREFIX")
parser.add_argument('-help',action='help',help='Show this help.')

## if nothing, show help
if len(sys.argv) == 1:
    parser.print_help()
    sys.exit(1)

## do the parsing
args = parser.parse_args()

########################################################################
## collect the arguments (need strings)
StatFile = args.StatDSET
MeanBrik = str(args.MeanBrik)
ThreshBrik = str(args.ThreshBrik)
SubjDSET = args.SubjDSET
SubjTable = args.SubjTable   ## add fix to clean
master = args.master
prefix = args.prefix
p_val = str(args.p)
MinVox = str(args.MinVox)
atlas = str(args.atlas)
session = os.path.abspath(args.session)
NoShiny = args.NoShiny

########################################################################
## verify some stuff before continuing
if "/" in prefix:
    print("")
    print("Error: "+prefix+" contains a path!")
    print("-prefix must not include a path.")
    print("Use -session to add a path.")
    print("")
    sys.exit(1)

## make sure the input data sets are good
afni_cmd = ("3dinfo -exists "+StatFile)
if subprocess.check_output(afni_cmd,shell=True)[0] == "0":
    print("")
    print("Error: "+StatFile+" is not loadable!")
    print("")
    sys.exit(1)
afni_cmd = ("3dinfo -exists "+SubjDSET)
if subprocess.check_output(afni_cmd,shell=True)[0] == "0":
    print("")
    print("Error: "+SubjDSET+" is not loadable!")
    print("")
    sys.exit(1)
afni_cmd = ("3dinfo -exists "+master)
if subprocess.check_output(afni_cmd,shell=True)[0] == "0":
    print("")
    print("Error: "+master+" is not loadable!")
    print("")
    sys.exit(1)

## make sure the subjects and stat image are in the same grid
grid_cmd = "3dinfo -same_grid "+StatFile+" "+SubjDSET
grid_match = subprocess.check_output(grid_cmd,shell=True)
if int(grid_match[0]) + int(grid_match[2]) != 2:
    print("")
    print("Error: Grid mismatch!")
    print(StatFile+" and "+SubjDSET+" are not on the same grid!")
    print("")
    sys.exit(1)

## make sure there is enough history in the stat file to parse
## and store the history to variable
hist_cmd = "3dinfo -history "+StatFile
hist_all = subprocess.check_output(hist_cmd,shell=True)
if not ("3dttest++" in hist_all or "3dMVM" in hist_all):
    print("")
    print("Error: "+StatFile+" does not have an intact history!")
    print("Are you sure it is DIRECTLY from 3dttest++, 3dMVM, or 3dLME?")
    print("")
    sys.exit(1)

## check for covariates file 3dttest++
if "3dttest++" in hist_all:

    ## the string after -setA should be an image
    if "-covariates" in hist_all and not NoShiny:
        print("")
        print("Error: "+StatFile+" includes a covariates file.")
        print("Since I don't have the covariates, the shiny app will not work.")
        print("Try -NoShiny if you just want the cluster data extracted.")
        print("")
        sys.exit(1)

## check for 3dMVM below
## end verification of stuff

########################################################################
## make output file names and template location (may change these)
# ShinyFolder = "/Users/discoraj/research/ClusterExplorer/ClustExp_ShinyTemplate"
OutDir = session+"/"+prefix+"_ClustExp_shiny/data"
ParOutDir = session+"/"+prefix+"_ClustExp_shiny"

if NoShiny:
    OutDir = session+"/"+prefix+"_ClustExp"
    ParOutDir = session+"/"+prefix+"_ClustExp"

## remove previous if asked
if args.overwrite:
    if os.path.isdir(ParOutDir):
        shutil.rmtree(ParOutDir)

ClustTable = OutDir+"/"+prefix+"_p_uncor_"+p_val+"_3dClust.1D"
OutDSET    = OutDir+"/"+prefix+"_p_uncor_"+p_val+".nii.gz"
OutMask    = OutDir+"/"+prefix+"_p_uncor_"+p_val+"_mask.nii.gz"
OutROI     = OutDir+"/"+prefix+"_p_uncor_"+p_val+"_clusters.csv"
OutPeak    = OutDir+"/"+prefix+"_p_uncor_"+p_val+"_peak_mask.nii.gz"
OutMaster  = OutDir+"/"+prefix+"_master.nii.gz"
OutTableMean = OutDir+"/"+prefix+"_p_uncor_"+p_val+"_mean.csv"
OutTablePeak = OutDir+"/"+prefix+"_p_uncor_"+p_val+"_peak.csv"
OutStatInfo  = OutDir+"/"+prefix+"_StatInfo.csv"
OutGroupTab  = OutDir+"/"+prefix+"_GroupTable.csv"
CleanTab     = OutDir+"/"+prefix+"_CleanSubjTab.1D"

########################################################################
## make output folder and copy shiny templates etc
if not os.path.exists(OutDir):
    os.makedirs(OutDir)
    if not NoShiny:
        for file in glob.glob(ShinyFolder+'/*.R'):
            shutil.copy(file,ParOutDir)

        ## "sed" the shiny templates
        with open(session+"/"+prefix+"_ClustExp_shiny/global.R", "r") as sources:
            lines = sources.readlines()
        with open(session+"/"+prefix+"_ClustExp_shiny/global.R", "w") as sources:
            for line in lines:
                sources.write(re.sub(r'prefix_replace',prefix,line))
else:
    print("")
    print("Error: Output folder exists!")
    print(session+"/"+prefix+"_group_shiny")
    print("")
    sys.exit(1)

###########################################################################
## make sure the subjects, stat images and master are in the same grid
## and make copy of the master
grid_cmd = "3dinfo -same_grid "+StatFile+" "+master
grid_match = subprocess.check_output(grid_cmd,shell=True)
if int(grid_match[0]) + int(grid_match[2]) != 2:
    print("")
    print("Resampling master to subject data.")
    print("")
    afni_cmd = ("3dresample -rmode Cu -master "+SubjDSET+
                " -prefix "+OutMaster+" -inset "+master)
    subprocess.check_output(afni_cmd,shell=True)
else:
    afni_cmd = "3dcopy "+master+" "+OutMaster
    subprocess.check_output(afni_cmd,shell=True)

###########################################################################
## get all the subjects from the @Group_CatLab data set
SubjList_cmd = "3dinfo -label "+SubjDSET
SubjListT = subprocess.check_output(SubjList_cmd,shell=True)
SubjListT = SubjListT.rstrip()
SubjListT = SubjListT.split("|")

## fix funky text files
FT_cmd = ("file_tool -show_file_type -show_bad_char -infiles "+SubjTable+
          " -prefix "+CleanTab)
subprocess.check_output(FT_cmd,shell=True)

## add header for output to files
SubjList = ["Subj"]
SubjList.extend(SubjListT)

## subject table from the input table
subj_dset_tab = [["Subj","InputFile"]]      ## for labeling extracted data
SubjDict = [["InputFile","Subj"]]           ## for lookup subject id
SubjInFile = []                             ## for matching history inputs
with open(CleanTab,'rb') as f:
    for line in f:
        # fields = line.split()
        fields = re.split('[ \t,]',line)
        fields[-1] = fields[-1].strip()
        subj_dset_tab.append([fields[0],fields[2]])
        SubjDict.append([fields[2],fields[0]])
        SubjInFile.append(fields[2])

## make a dictionary to look up the subj based on file name
SubjDict = {x[0]: x[1] for x in SubjDict}

## make sure the table and DSET match
afni_cmd = "3dinfo -nv "+SubjDSET
nv = subprocess.check_output(afni_cmd,shell=True)
nv = int(nv.split()[0])
if nv != len(subj_dset_tab)-1:
    cleanUp(("The number of entries in "+SubjTable+" does not match "+
             "the number of volumes in "+SubjDSET))
    sys.exit(1)

## check all ttest/mvm/lme inputs are in the subj dset/table
if "3dMVM" in hist_all:
    ## load mvm/lm table
    mvm_tab, mvm_hdr = mvm_lme_extract(hist_all)
    ## make sure that the InputFiles from the history
    ## are in the SubjDSET/Table
    InCol = len(mvm_tab[0])-1
    InFileList_dirty = [row[InCol] for row in mvm_tab]
    InFileList = []
    for row in InFileList_dirty:
        if row.startswith(('"',"'")):
             InFileList.append(row[1:-1])
        else:
            InFileList.append(row)

    if not set(InFileList).issubset(SubjInFile):
        ## get the missing ones and print them out
        missInFile = set(InFileList).difference(SubjInFile)
        print("")
        for i in missInFile:
            print(i)
        cleanUp(("ERROR: The above data sets are in your 3dMVM/3dLME analysis,"
                 " but not in your -StatDSET or -StatTable !!!"))
        sys.exit(1)

###########################################################################
## get the degrees of freedom and convert the p to T
dof_cmd = "3dAttribute BRICK_STATAUX "+StatFile+"["+ThreshBrik+"]"
dof = subprocess.check_output(dof_cmd,shell=True)
dof = dof.rstrip()
dof = dof.split()

StatType_cmd = "3dAttribute BRICK_STATSYM "+StatFile+"["+ThreshBrik+"]"
StatType = subprocess.check_output(StatType_cmd,shell=True)
StatType = StatType.split("(")[0]

## check for type and convert p (only F and T now)
if StatType == "Ttest":
    ThrVal_cmd = "ccalc -expr 'fitt_p2t("+p_val+","+dof[3]+")'"
    ThrVal = subprocess.check_output(ThrVal_cmd,shell=True)
    ThrVal = ThrVal.rstrip()
elif StatType == "Ftest":
    ThrVal_cmd = "ccalc -expr 'fift_p2t("+p_val+","+dof[3]+","+dof[4]+")'"
    ThrVal = subprocess.check_output(ThrVal_cmd,shell=True)
    ThrVal = ThrVal.rstrip()
else:
    print("ERROR: only Tstats and FStats for now...Sorry!")
    sys.exit(1)

###########################################################################
## make some output images and table

## label brik
StatLab_cmd = "3dAttribute BRICK_LABS "+StatFile+"["+ThreshBrik+"]"
StatLab = subprocess.check_output(StatLab_cmd,shell=True)
StatLab = StatLab.split("~")[0]
StatLab = StatLab+"_p_uncor_"+p_val
StatLab = re.sub(r'\s+','_',StatLab)    ## replace white space

## write out thresholded image
afni_cmd = ("3dmerge -1thresh "+ThrVal+" -1dindex "+MeanBrik+" -1tindex "
            +ThreshBrik+" -prefix "+OutDSET+" "+StatFile)
subprocess.check_output(afni_cmd,shell=True)

## write out cluster mask and table
afni_cmd = ("3dclust -DAFNI_ORIENT=LPI -savemask "+OutMask+" -nosum -1thresh "+ThrVal+
            " -1dindex "+MeanBrik+" -1tindex "+ThreshBrik+" -dxyz=1 1.01 "+
            MinVox+" "+StatFile+" > "+ClustTable)
subprocess.check_output(afni_cmd,shell=True)

## add the brik label
afni_cmd = "3drefit -relabel_all_str "+StatLab+" "+OutDSET
subprocess.check_output(afni_cmd,shell=True)
afni_cmd = "3drefit -relabel_all_str "+StatLab+" "+OutMask
subprocess.check_output(afni_cmd,shell=True)

###########################################################################
## get the ROI label from whereami

# ## find the orientation set by users variable as output by 3dclust
# with open(ClustTable) as f:
#     for line in f:
#         if re.findall(r'Coordinates Order', line):
#             orient = line.split()[3]
#             orient_short = orient
#             orient = "-"+orient.lower()

## peak
afni_cmd = ("whereami -coord_file "+ClustTable+"'[13,14,15]' -tab -lpi"+
            " -max_areas 1 -atlas "+atlas+" -atlas CA_N27_LR "+
            "> "+TempPrefix+"_whereami_delete_me00.txt")
subprocess.check_output(afni_cmd,shell=True)

peak_atlas = []
with open(TempPrefix+"_whereami_delete_me00.txt") as f:
    a = " "
    while(a):
        a = f.readline()
        l = a.find("Within") #Gives a non-negative value when there is a match
        if ( l >= 0 ):
            atlas_line = f.readline()
            atlas_line = atlas_line.split("\t")
            line_1 = atlas_line[0].split()
            if ( line_1 == [atlas] ):
                atlas_line = atlas_line[2].rstrip()
                peak_atlas.append(atlas_line)
            else:
                peak_atlas.append("NA")
## end peak

## center of mass
afni_cmd = ("whereami -coord_file "+ClustTable+"'[1,2,3]' -tab -lpi"+
            " -max_areas 1 -atlas "+atlas+" -atlas CA_N27_LR "+
            "> "+TempPrefix+"_whereami_delete_me00.txt")
subprocess.check_output(afni_cmd,shell=True)

cm_atlas = []
with open(TempPrefix+"_whereami_delete_me00.txt") as f:
    a = " "
    while(a):
        a = f.readline()
        l = a.find("Within") #Gives a non-negative value when there is a match
        if ( l >= 0 ):
            atlas_line = f.readline()
            atlas_line = atlas_line.split("\t")
            line_1 = atlas_line[0].split()
            if ( line_1 == [atlas] ):
                atlas_line = atlas_line[2].rstrip()
                cm_atlas.append(atlas_line)
            else:
                cm_atlas.append("NA")
## end center of mass
## end atlas label lookup

###########################################################################
## output region coords, labels and LUT

## set the header as a list
clust_hdr = (["Voxels","x_cm","y_cm","z_cm","x_y_z_cm","label_cm",
              "x_peak","y_peak","z_peak","x_y_z_peak","label_peak"])

## get the cluster table as a list, skip headers,
clust_tab = []
with open(ClustTable,'rb') as file_clust:
    for line in file_clust:
        fields = line.split()
        clust_tab.append(fields)
clust_tab = clust_tab[12:]

## for the headers of the extracted data and the peak voxels for extraction
cm_hdr = [] ; pk_hdr = [] ; pk_x_out = [] ; pk_y_out = [] ; pk_z_out = []

## make combined coords
atlas_index = 0
clust_short = []
peak_out = []
peak_index = 1
for row in clust_tab:
    cm_x = float(row[1])+0      ## (+0 for the -0.0)
    cm_y = float(row[2])+0
    cm_z = float(row[3])+0
    pk_x = float(row[13])+0
    pk_y = float(row[14])+0
    pk_z = float(row[15])+0
    cm_lab = str(cm_x)+"_"+str(cm_y)+"_"+str(cm_z)
    pk_lab = str(pk_x)+"_"+str(pk_y)+"_"+str(pk_z)
    new_row = ([int(row[0]),
                cm_x,cm_y,cm_z,cm_lab,cm_atlas[atlas_index],
                pk_x,pk_y,pk_z,pk_lab,peak_atlas[atlas_index]])
    clust_short.append(new_row)
    cm_hdr.append(cm_lab)
    pk_hdr.append(pk_lab)
    pk_x_out.append(pk_x)
    pk_y_out.append(pk_y)
    pk_z_out.append(pk_z)
    peak_out.append([pk_x,pk_y,pk_z,peak_index])
    peak_index += 1
    atlas_index += 1

## write out as csv
wtr = csv.writer(open(OutROI,'w'),delimiter=',',
                 lineterminator='\n')
wtr.writerow(clust_hdr)
for row in clust_short: wtr.writerow(row)

## write out the peak coords for 3dUndump later
wtr = csv.writer(open(TempPrefix+"_peak_delete_me00.txt",'w'),delimiter=' ',
                 lineterminator='\n',quoting=csv.QUOTE_NONE)
for row in peak_out: wtr.writerow(row)

## may do this later if I find a reason to
# ## add labels to mask
# @MakeLabelTable -labeltable label_table_delete \
#                 -lab_file clust_lab_delete_me00.txt 1 0 -dset ${OutMask}

########################################################################
## extract all subjects from the thresholded mask as mean

## get info for time warning
afni_cmd = "3dinfo -nv "+SubjDSET
nv = subprocess.check_output(afni_cmd,shell=True)
nv = nv.split()[0]
afni_cmd = "3dinfo -max "+OutMask
clust_num = subprocess.check_output(afni_cmd,shell=True)
clust_num = clust_num.split()[0]
print("")
print("Extracting "+clust_num+" clusters from "+nv+" data sets.")
print("Please wait....")
print("")

## extract the data from all subjects
afni_cmd = ("3dROIstats -quiet -mask "+OutMask+" -1Dformat "+SubjDSET+
            " > "+TempPrefix+"_val_delete_me00.dat")
subprocess.check_output(afni_cmd,shell=True)

## read in the values
with open(TempPrefix+"_val_delete_me00.dat") as f:
    reader = csv.reader(f, delimiter="\t")
    subj_dat_temp = list(reader)

## remove the first empty column
subj_dat = []
for d in subj_dat_temp:
    dat_temp = [x for x in d if x]
    subj_dat.append(dat_temp)

## add the header
subj_dat.insert(0,cm_hdr)

## combine with the subject list and input file
sDataOut = []
i = 0
for row in subj_dat:
    temp_row = [subj_dset_tab[i][0],subj_dset_tab[i][1]]
    temp_row.extend(row)
    sDataOut.append(temp_row)
    i += 1

## write out as csv
wtr = csv.writer(open(OutTableMean,'w'),delimiter=',',
                 lineterminator='\n')
for row in sDataOut: wtr.writerow(row)

## end mean extraction and output

########################################################################
## extract all subjects from the peak voxels

## make a peak voxel mask from table file
afni_cmd = ("3dUndump -prefix "+OutPeak+" -master "+OutMaster+" -xyz "+
            "-orient LPI "+TempPrefix+"_peak_delete_me00.txt")
subprocess.check_output(afni_cmd,shell=True)

print("")
print("Extracting "+clust_num+" peaks from "+nv+" data sets.")
print("Please wait....")
print("")

## extract the data from all subjects
afni_cmd = ("3dROIstats -quiet -mask "+OutPeak+" -1Dformat "+SubjDSET+
            " > "+TempPrefix+"_val_delete_me00.dat")
subprocess.check_output(afni_cmd,shell=True)

## read in the values
with open(TempPrefix+"_val_delete_me00.dat") as f:
    reader = csv.reader(f, delimiter="\t")
    subj_dat_temp = list(reader)

## remove the first empty column
subj_dat = []
for d in subj_dat_temp:
    dat_temp = [x for x in d if x]
    subj_dat.append(dat_temp)

## add the header
subj_dat.insert(0,pk_hdr)

## combine with the subject list and input file
sDataOut = []
i = 0
for row in subj_dat:
    temp_row = [subj_dset_tab[i][0],subj_dset_tab[i][1]]
    temp_row.extend(row)
    sDataOut.append(temp_row)
    i += 1

## write out as csv
wtr = csv.writer(open(OutTablePeak,'w'),delimiter=',',
                 lineterminator='\n')
for row in sDataOut: wtr.writerow(row)

## end peak extraction and output

########################################################################
## parse history from stats image

## get the whole history
hist_cmd = "3dinfo -history "+StatFile
hist_all = subprocess.check_output(hist_cmd,shell=True)

########################################################################
## parse the history and output as a table
afni_cmd = (HistProg+" -StatDSET "+StatFile+" -prefix "+prefix+"_GroupTable "+
            "-session "+OutDir)
subprocess.check_output(afni_cmd,shell=True)

## get the stat info from the history
if "3dttest++" in hist_all:

    ## assumptions of defaults
    paired_T = "FALSE"
    model_T = "OneSample"
    labelB = "NA"

    ## setA
    setA = hist_all.split("-setA")[1]
    setA = setA.split("-")[0]
    setA = setA.split()

    ## check for long form
    if ("+tlrc" not in setA[0]) and (".nii" not in setA[0]):

        ## chop off the label
        labelA = setA[0]

        ## setB
        if "-setB" in hist_all:
            setB = hist_all.split("-setB")[1]
            setB = setB.split("-")[0]
            setB = setB.split()

            ## chop off the label
            labelB = setB[0]

            ## check for other stuff
            if "-BminusA" in hist_all:
                model_T = "BA"
            else:
                model_T = "AB"
            if "-paired" in hist_all:
                paired_T = "TRUE"

    #################################
    else:   ## short form needs the lookup table for the Subj
        ## labels
        if "-labelA" in hist_all:
            labelA = hist_all.split("-labelA")[1]
            labelA = labelA.split()[0]
        else:
            labelA = "SetA"

        ## make array to match the number of subjects
        labA_arr = [labelA] * len(setA)

        ## setB if there is one
        if "-setB" in hist_all:
            setB = hist_all.split("-setB")[1]
            setB = setB.split("-")[0]
            setB = setB.split()

            ## label
            if "-labelB" in hist_all:
                labelB = hist_all.split("-labelB")[1]
                labelB = labelB.split()[0]
            else:
                labelB = "SetB"

            labB_arr = [labelB] * len(setB)
            labels_out = labA_arr+labB_arr
            subj_out = setA+setB
            tab_out = [labels_out,subj_out]

            ## check for other stuff
            if "-BminusA" in hist_all:
                model_T = "BA"
            else:
                model_T = "AB"
            if "-paired" in hist_all:
                paired_T = "TRUE"

        else:   ## no -setB
            labels_out = labA_arr
            subj_out = setA
            model_T = labelA
            tab_out = [labels_out,subj_out]
        ## end -setB check

        ## transpose array
        tab_out = [list(x) for x in zip(*tab_out)]

        ## get the subject label from the dictionary
        subj_table = [["Subj","Group","InputFile"]]
        for dset in tab_out:
            if dset[-1].startswith(('"',"'")):   ## remove quotes
                dset[-1] = dset[-1][1:-1]
            subj_id = SubjDict[dset[1]]
            if subj_id:
                subj_table.append([subj_id,dset[0],dset[1]])
            else:
                subj_table.append(["NA",dset[0],dset[1]])

        ## write out data table with headers
        wtr = csv.writer(open(OutGroupTab,'w'),delimiter=',',
                         lineterminator='\n')
        for row in subj_table : wtr.writerow(row)
    ## end long vs short

    #################################
    ## write out data table of the stat info
    stat_hdr = (["test","p_val","setA","setB","model","paired",
                 "MeanBrik","ThreshBrik","master","atlas",
                 "StatFile","SubjDSET","SubjTable"])
    stat_nfo = ([StatType,p_val,labelA,labelB,model_T,paired_T,
                 MeanBrik,ThreshBrik,os.path.basename(master),atlas,
                 os.path.basename(StatFile),os.path.basename(SubjDSET),
                 os.path.basename(SubjTable)])
    wtr = csv.writer(open(OutStatInfo,'w'),delimiter=',',lineterminator='\n')
    wtr.writerow(stat_hdr)
    wtr.writerow(stat_nfo)
##  end 3dttest++ check

########################################################################
## 3dMVM
elif "3dMVM" in hist_all:

    ## get the -model or -bsVars
    if "-bsVars" in hist_all:
        mvm_model = hist_all.split("-bsVars")[1]
        mvm_model = mvm_model.split()[0]
    elif "-model" in hist_all:
        mvm_model = hist_all.split("-model")[1]
        mvm_model = mvm_model.split()[0]
    else:
        cleanUp("Model not found")
        sys.exit(1)

    ## other variables (may need to edit here)
    if "-wsVars" in hist_all:
        mvm_wsVars = hist_all.split("-wsVars")[1]
        mvm_wsVars = mvm_wsVars.split()[0]
    else:
        mvm_wsVars = "NA"
    if "-qVars" in hist_all:
        mvm_qVars = hist_all.split("-qVars")[1]
        mvm_qVars = mvm_qVars.split()[0]
    else:
        mvm_qVars = "NA"
    if "-qVarCenters" in hist_all:
        mvm_qVarCenters = hist_all.split("-qVarCenters")[1]
        mvm_qVarCenters = mvm_qVarCenters.split()[0]
    else:
        mvm_qVarCenters = "NA"
    if "-SS_type" in hist_all:
        mvm_SS_type = hist_all.split("-SS_type")[1]
        mvm_SS_type = mvm_SS_type.split()[0]
    else:
        mvm_SS_type = "3"

    ## write out data table of the stat info
    stat_hdr = (["test","p_val","model","qVars","qVarCenters","wsVars",
                 "MeanBrik","ThreshBrik","SS_type","master","atlas",
                 "StatFile","SubjDSET","SubjTable"])
    stat_nfo = (["3dMVM",p_val,mvm_model,mvm_qVars,mvm_qVarCenters,
                 mvm_wsVars,MeanBrik,ThreshBrik,mvm_SS_type,
                 os.path.basename(master),atlas,os.path.basename(StatFile),
                 os.path.basename(SubjDSET),os.path.basename(SubjTable)])
    wtr = csv.writer(open(OutStatInfo,'w'),delimiter=',',
                     lineterminator='\n')
    wtr.writerow(stat_hdr)
    wtr.writerow(stat_nfo)

##  end 3dMVM check
else:
    cleanUp("No model information!!")
    sys.exit(1)
## end 3dttest++ and 3dMVM check

########################################################################
## clean up and end

cleanUp("Complete!")
print("Thanks!")
print("")
if NoShiny:
    print("View results at "+ParOutDir)
else:
    print("To view results run:")
    print("@ClustExp_run_shiny "+ParOutDir)
print("")

sys.exit(0)
