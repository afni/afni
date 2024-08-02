#!/usr/bin/env AFNI_Batch_R

##!/usr/bin/env afni_run_R

# Command line to run this script: RBA.R dataStr.txt diary.txt &
# (Output is a file in which the running progress including 
# error messages will be stored)

first.in.path <- function(file) {
ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
ff<-ff[lapply(ff,file.exists)==TRUE];
#cat('Using ', ff[1],'\n');
return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
ExecName <- 'RBA'

# Global variables

#################################################################################
##################### Begin RBA Input functions ################################
#################################################################################

#The help function for RBA batch (AFNI-style script mode)
help.RBA.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

intro <- 
'
	      Welcome to RBA ~1~
Region-Based Analysis Program through Bayesian Multilevel Modeling 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Version 1.1.6, July 31, 2024 
Author: Gang Chen (gangchen@mail.nih.gov)
Website - https://afni.nimh.nih.gov/gangchen_homepage
SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Usage: ~1~
------ 
RBA performs region-based analysis (RBA) as theoretically elaborated in the
manuscript: https://rdcu.be/bhhJp and is conducted with a shell script (as
shown in the examples below). The input data should be formulated in a
pure-text table that codes the regions and variables. The response variable
is some effect at the individual subject level.

Thanks to Paul-Christian Bürkner and the Stan/R communities for the strong support.

Citation: ~1~
If you want to cite the approach for RBA, consider the following:~2~

Chen G, Xiao Y, Taylor PA, Riggins T, Geng F, Redcay E, 2019. Handling Multiplicity
in Neuroimaging through Bayesian Lenses with Multilevel Modeling. Neuroinformatics.
https://rdcu.be/bhhJp

Chen, G., Taylor, P.A., Cox, R.W., Pessoa, L., 2020. Fighting or embracing 
multiplicity in neuroimaging? neighborhood leverage versus global calibration. 
NeuroImage 206, 116320. https://doi.org/10.1016/j.neuroimage.2019.116320

Chen, G., Taylor, P.A., Stoddard, J., Cox, R.W., Bandettini, P.A., Pessoa, L., 
2022. Sources of Information Waste in Neuroimaging: Mishandling Structures, 
Thinking Dichotomously, and Over-Reducing Data. Aperture Neuro 2021, 46. 
https://doi.org/10.52294/2e179dbf-5e37-4338-a639-9ceb92b055ea

=============================== 
Read the following carefully!
===============================
A data table in pure text format is needed as input for an RBA script. The
data table should contain at least 3 columns that specify the information
about subjects, regions and the response variable values with the following
fixed header. The header labels are case-sensitive, and their order does not
matter.

Subj   ROI        Y      Age
S1     Amyg    0.2643    11
S2     BNST    0.3762    16
...

0) You are performing Bayesian analysis. So, you will directly obtain the
probability of the respective effect being positive or negative with your 
data and adopted model, instead of looking for the p-value (weirdness of 
your data under the modeling assumptions when pretending that absolutely 
no effect exists).

1) Avoid using pure numbers to code the labels for categorical variables. The
column order does not matter. You can specify those column names as you
prefer, but it saves a little bit scripting if you adopt the default naming
for subjects (\'Subj\'), regions (\'ROI\') and response variable (\'Y\').

2) Add more columns if explanatory variables are considered in the model. Currently
only between-subjects variables (e.g., sex, patients vs. controls, age) are
allowed. Capability of modeling within-subject or repeated-measures variables
may be added in the future. Each label in a between-subjects factor (categorical
variable) should be coded with at least 1 character (labeling with pure numbers
is fine but not recommended). If preferred, you can quantitatively code the
levels of a factor yourself by creating k-1 columns for a factor with k levels.
However, be careful with your coding strategy because it would impact how to
interpret the results. Here is a good reference about factor coding strategies:
https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/

3) It is strongly suggested that a quantitative explanatory variable be
standardized with option -stdz; that is, remove the mean and scale by
the standard deviation. This will improve the chance of convergence
with each Markov chain. If a between-subjects factor (e.g., sex) is
involved, it may be better to standardize a quantitative variable
within each group in terms of interpretability if the mean value differs
substantially. However, do not standardize a between-subjects factor if
you quantitatively code it. And do not standardize the response variable
if the intercept is of interest!

4) For within-subject variables, try to formulate the data as a contrast
between two factor levels or as a linear combination of multiple levels.

5) The results from RBA are effect estimates for each region. They can be
slightly different across different runs or different computers and R
package versions due to the nature of randomness involved in Monte Carlo
simulations.

6) The evidence for region effects in the output can be assessed through P+,
the probability that the effect is positive conditional on the current
model and dataset. Unlike the NHST convention, we emphasize that a decision 
about the strength of statistical evidence should not be purely based on an
artificial threshold. Instead, we encourage full results reporting:
highlight some results with strong evidence and literature support (if
available) without hiding the rest.

7) WARNING: If the results are unexpectedly homogenized across regions, it is an
indication that presumably partial pooling becomes full pooling. Most
likely the cross-region variability is so negligible that the model
renders the overall average as individual effects for all regions. When
this occurs, you may need much more data for the model to differentiate
the subtle effects.

=========================

Installation requirements: ~1~
In addition to R installation, the R package "brms" is required for RBA. Make
sure that you have the most recent version of R. To install "brms", run the following
command at the terminal:

rPkgsInstall -pkgs "brms" -site http://cran.us.r-project.org"

Alternatively, you may install them in R:

install.packages("brms")

*** To take full advantage of parallelization, install both \'cmdstan\' and 
\'cmdstanr\' and use the option -WCP in MBA. However, extra stpes are required: 
both \'cmdstan\' and \'cmdstanr\' have to be installed. To install \'cmdstanir\',
execute the following command in R:

install.packages(\'cmdstanr\', repos = c(\'https://mc-stan.org/r-packages/\', getOption(\'repos\')))

Then install \'cmdstan\' using the following command in R:

cmdstanr::install_cmdstan(cores = 2)
# Follow the instruction here for the installation of \'cmdstan\': 
#    https://mc-stan.org/cmdstanr/articles/cmdstanr.html
# If \'cmdstan\' is installed in a directory other than home, use option -StanPath 
# to specify the path (e.g., -StanPath \'~/my/stan/path\').

In addition, if you want to show the ridge plots of the posterior distributions
through option -ridgePlot, make sure that the following R packages are installed:

data.table
ggplot2
ggridges
dplyr
tidyr
scales

Running: ~1~
Once the RBA command script is constructed, it can be run by copying and
pasting to the terminal. Alternatively (and probably better) you save the 
script as a text file, for example, called myRBA.txt, and execute it with the 
following  (assuming on tcsh shell),

nohup tcsh -x myRBA.txt > diary.txt &
nohup tcsh -x myRBA.txt |& tee diary.txt &

The advantage of the commands above is that the progression is saved into
the text file diary.txt and, if anything goes awry, can be examined later.
The \'nohup\' command allows the analysis running in the background even if
the terminal is killed.'

ex1 <- 
"\n--------------------------------
Examples: ~1~

Example 1 --- Simplest scenario. Values from regions are the input from
  each subject. No explanatory variables are considered. Research
  interest is about the population effect at each region.

RBA -prefix output -dataTable myData.txt  \\

The above script is equivalent to

RBA -prefix myResult -chains 4 -iterations 1000 -model 1 -EOI 'Intercept' \\
-dataTable myData.txt  \\

The 2nd version above is recommended because of its explicit specifications.

If the data are skewed or have outliers, use Student's t-distribution:

RBA -prefix myResult -chains 4 -iterations 1000 -model 1 -EOI 'Intercept' \\
-distY 'student' -dataTable myData.txt  \\

If a computer is equipped with as many CPUs as a factor 4 (e.g., 8, 16, 24,
...), a speedup feature can be adopted through within-chain parallelization
with the option -WCP. For example, the script assumes a computer with 24 CPUs
(6 CPUs per chain):

RBA -prefix myResult -chains 4 -WCP 6 \\
-iterations 1000 -model 1 -EOI 'Intercept' -distY 'student' \\
-dataTable myData.txt  \\

The input file 'myData.txt' is a data table in pure text format as below: 
						     
Subj  ROI          Y
S01   lFFA       0.162
S02   lAmygdala -0.598
S03   DMNLAG     0.249
S04   DMNPCC     0.568
...

If t-statistic (or standard error) values corresponding to the response variable
Y are available, add the t-statistic (or standard error) values as a column in the input
data table so that they can be incorporated into the BML model using the option -tstat
or -se with the following script (assuming the tstat column is named as 'tvalue'),

RBA -prefix myResult -chains 4 -WCP 6 \\
-iterations 1000 -model 1 -EOI 'Intercept' -distY 'student' -tstat tvalue \\
-dataTable myData.txt  \\

or (assuming the se column is named as 'SE'),

RBA -prefix myResult -chains 4 -WCP 6 \\
-iterations 1000 -model 1 -EOI 'Intercept' -distY 'student' -se SE \\
-dataTable myData.txt  \\

\n"         

ex2 <-
"--------------------------------
Example 2 --- 2 between-subjects factors (sex and group): ~2~

RBA -prefix output -Subj subject -ROI region -Y zscore -ridgePlot 10 8 \\
-chains 4 -iterations 1000 -model '1+sex+group' \\
-cVars 'sex,group' -EOI 'Intercept,sex,group' \\
-dataTable myData.txt

If a computer is equipped with as many CPUs as a factor 4 (e.g., 8, 16, 24,
...), a speedup feature can be adopted through within-chain parallelization
with the option -WCP. For example, consider adding 
'-WCP 6' on a computer with 24 CPUs.

The input file 'myData.txt' is formatted as below:

subject region  zscore  sex group
S1      DMNLAG  0.274    F  patient
S1      DMNLHC  0.443    F  patient
S2      DMNRAG  0.455    M  control
S2      DMNRHC  0.265    M  control
...

Notice that the interaction between 'sex' and 'group' is not modeled in
this case. The option -ridgePlot generates a stacked list of posterior
distributions in a sequential order among the regions for each effect of
interest specified through -EOI. The two numbers of 10 and 8 associated
with the option -ridgePlot specifies the figure window size with 10\" wide
and 8\" high.
\n"

ex3 <-
"---------------------------------
Example 3 --- one between-subjects factor (sex), one within-subject factor (two
conditions), one between-subjects covariate (age), and one quantitative
variable: ~2~

RBA -prefix result -ridgePlot 8 6 -Subj Subj -ROI region -Y value \\
-chains 4 -iterations 1000 -model '1+sex+age+SA' -qVars 'sex,age,SA' \\
-EOI 'Intercept,sex,age,SA' -dataTable myData.txt

If a computer is equipped with as many CPUs as a factor 4 (e.g., 8, 16, 24,
...), a speedup feature can be adopted through within-chain parallelization
with the option -WCP. For example, consider adding '-WCP 6' to the script
on a computer with 24 CPUs.

The input file 'myData.txt' is formatted as below:

Subj   region   value sex  age   SA
S1    DMNLAG    0.274  1  1.73  1.73
S1    DMNLHC    0.443  1  1.73  1.73
S2    DMNRAG    0.455 -1 -0.52  0.52
S2    DMNRHC    0.265 -1 -0.52  0.52
...

Notice

1) The 'Y' column is the contrast between the two conditions.
2) Since we want to model the interaction between 'sex' and 'age', 'sex' is
   coded through deviation coding.
3) 'age' has already been standardized within each sex due to large age
   difference between the two sexes.
4) The 'SA' column codes for the interaction between 'sex' and 'age', which
   is the product of the two respective columns.
\n"   

ex4 <-
"---------------------------------
Example 4 --- a more flexible way to specify a model. 

RBA -prefix test -chains 4 -iterations 1000 -mean 'score~1+(1|roi)+(1|subj)' \\
-sigma '1+(1|roi)+(1|subj)' -ROI 'roi' -EOI 'Intercept' -WCP 8 \
-dataTable test.tbl

The input file 'test.tbl' is formatted as below:

subj    roi     score 
S1    DMNLAG    0.274 
S1    DMNLHC    0.443
...
S2    DMNLAG    0.455 
S2    DMNLHC    0.265 
...

Notice

1) The -mean option specifies the formulation for the mean of the likelihood (Gaussian
   in this case).
2) The -sigma option specifies the formulation for the standard deviation of likelihood
   (Gaussian in this case).
3) It is important to identify the pivotal variable as 'roi' since the label is different
   from the default ('ROI').
\n"

parnames <- names(params)
ss <- vector('character')
if(alpha) {
parnames <- sort(parnames)   
ss <- paste('Options in alphabetical order:\n',
	  '------------------------------\n', sep='')
} else ss <- paste('Options:\n', '--------\n', sep='')
for(ii in 1:length(parnames)) {
op <- params[parnames[ii]][[1]]
if(!is.null(op$help)) ss <- c(ss , paste(itspace, op$help, sep='')) else
 ss <- c(ss, paste(itspace, parnames[ii], '(no help available)\n', sep='')) 
}
ss <- paste(ss, sep='\n')
cat(intro, ex1, ex2, ex3, ex4, ss, sep='\n')

if (adieu) exit.AFNI();
}

# options list 
read.RBA.opts.batch <- function (args=NULL, verb = 0) {
params <- list (
'-prefix' = apl(n = 1, d = NA,  h = paste(
"-prefix PREFIX: Prefix is used to specify output file names. The main output is",
"        a text with prefix appended with .txt and stores inference information ",
"        for effects of interest in a tabulated format depending on selected ",
"        options. The prefix will also be used for other output files such as ",
"        visualization plots and for saved R data in binary format. The .RData can",
"        be used for post hoc processing such as customized processing and plotting.",
"        Remove the .RData file to save disk space once you deem such a file is no",
"        longer useful.\n", sep = '\n'
	     ) ),

'-chains' = apl(n = 1, d = 1, h = paste(
"-chains N: Specify the number of Markov chains. Make sure there are enough",
"         processors available on the computer. Most of the time 4 cores are good",
"         enough. However, a larger number of chains (e.g., 8, 12) may help achieve",
"         higher accuracy for posterior distribution. Choose 1 for a single-processor",
"         computer, which is only practical only for simple models.\n", sep = '\n'
	     ) ),

'-iterations' = apl(n = 1, d = 1, h = paste(
"-iterations N: Specify the number of iterations per Markov chain. Choose 1000 (default)",
"         for simple models (e.g., one or no explanatory variables). If convergence",
"         problem occurs as indicated by Rhat being great than 1.1, increase the number of",
"         iterations (e.g., 2000) for complex models, which will lengthen the runtime.",
"         Unfortunately, there is no way to predict the optimum iterations ahead of time.\n", sep = '\n'
	     ) ),

'-verb' = apl(n = 1, d = 1, h = paste(
"-verb VERB: Specify verbose level.\n", sep = '\n'
	     ) ),

'-model' = apl(n = 1, d = 1, h = paste(
"-model FORMULA: This option specifies the effects associated with explanatory",
"         variables. By default, (without user input) the model is specified as",
"         1 (Intercept). Currently only between-subjects factors (e.g., sex, ",
"         patients vs. controls) and quantitative variables (e.g., age) are",
"         allowed. When no between-subject factors are present, simply put 1",
"         (default) for FORMULA. The expression FORMULA with more than one",
"         variable has to be surrounded within (single or double) quotes (e.g.,",
"         '1+sex', '1+sex+age'. Variable names in the formula should be consistent",
"         with the ones used in the header of data table. A+B represents the",
"         additive effects of A and B, A:B is the interaction between A",
"         and B, and A*B = A+B+A:B. Subject as a variable should not occur in",
"         the model specification here.\n", sep = '\n'
     ) ),

'-mean' = apl(n = 1, d = 1, h = paste(
"-mean FORMULA: Specify the formulation for the mean of the likelihood (sampling",
"          distribution).\n", sep = '\n') ),

'-sigma' = apl(n = 1, d = 1, h = paste(
"-sigma FORMULA: Specify the formulation for the standard deviation (sigma) of the",
"          likelihood (sampling distribution). When this option is absent in the",
"          script, it is assumed to be 1, meaning a single parameter for the variance",
"          (homogeneity).\n", sep = '\n') ),

'-dbgArgs' = apl(n=0, h = paste(
"-dbgArgs: This option will enable R to save the parameters in a file called",
"         .RBA.dbg.AFNI.args in the current directory so that debugging can be",
"         performed.\n", sep='\n')),

#'-MD' = apl(n=0, h = paste(
#"-MD: This option indicates that there are missing data in the input. With n",
#"         regions, at least n(n-1)/2 values are assumed from each subject in the",
#"         input with no missing data (default). When missing data are present,",
#"         invoke this option so that the program will handle it properly.\n", sep='\n')),

'-r2z' = apl(n=0, h = paste(
"-r2z: This option performs Fisher transformation on the response variable",
"         (column Y) if it is correlation coefficient.\n", sep='\n')),

'-cVars' = apl(n=c(1,100), d=NA, h = paste(
"-cVars variable_list: Identify categorical (qualitive) variables (or",
"         factors) with this option. The list with more than one variable",
"         has to be separated with comma (,) without any other characters such",
"         as spaces and should be surrounded within (single or double) quotes.",
"         For example, -cVars \"sex,site\"\n",
     sep = '\n'
     ) ),

'-qVars' = apl(n=c(1,100), d=NA, h = paste(
"-qVars variable_list: Identify quantitative variables (or covariates) with",
"         this option. The list with more than one variable has to be",
"         separated with comma (,) without any other characters such as",
"         spaces and should be surrounded within (single or double) quotes.",
"         For example, -qVars \"Age,IQ\"\n",
     sep = '\n'
     ) ),

'-stdz' = apl(n=c(1,100), d=NA, h = paste(
"-stdz variable_list: Identify quantitative variables (or covariates) to be",
"         standardized. To obtain meaningful and interpretable results and to",
"         achieve better convergence of Markov chains with reasonable iterations,",
"         it is recommended that all quantitative variables be standardized",
"         except for the response variable and indicator variables that code for",
"         factors. For example, -stdz \"Age,IQ\". If the mean of a quantitative",
"         variable varies substantially between groups, it may make sense to",
"         standardize the variable within each group before plugging the values",
"         into the data table. Currently RBA does not offer the option to perform",
"         within-group standardization.\n",
     sep = '\n'
     ) ),

 '-scale' = apl(n = 1, d = 1, h = paste(
"-scale d: Specify a multiplier for the Y values. When the values for response",
"         are too small or large, it may create a convergence problem for MCMC. To",
"         avoid the problem, set a scaling factor so that the range of value is",
"         around 1-10. The results will be adjusted back to the original scale.\n", sep = '\n'
	     ) ),

'-EOI' = apl(n=c(1,100), d=NA, h = paste(
"-EOI variable_list: Identify effects of interest in the output by specifying the",
"         variable names separated with comma (,). For example, -EOI \"sex,age\".",
"         By default, the Intercept is considered to be an effect of interest.",
"         Currently only variables, not their interactions, can be directly",
"         requested for output. However, most interaction effects can be obtained by",
"         either properly coding the variables (see example 3) or post processing.\n",
     sep = '\n'
     ) ),

'-qContr' = apl(n=c(1,100), d=NA, h = paste(
"-qContr contrast_list: Identify comparisons of interest between quantitative",
"         variables in the output separated with comma (,). It only allows for",
"         pair-wise comparisons between two quantitative variables. For example,",
"         -qContr \"age vs IQ, age vs weight, IQ vs weight\", where V1, V2, and V3 are three",
"         quantitative variables and three comparisons, V1 - V2, V1 - V3 and V2 - V3",
"         will be provided in the output. Make sure that such comparisons are",
"         meaningful (e.g., with the same scale and unit. This can be used to",
"         formulate comparisons among factor levels if the user quantitatively",
"         codes the factor levels.\n",
     sep = '\n'
     ) ),

'-Y' = apl(n = 1, d = NA,  h = paste(
"-Y var_name: var_name is used to specify the column name that is designated as",
"        as the response/outcome variable. The default (when this option is not",
"        invoked) is 'Y'.\n", sep = '\n'
	     ) ),


'-tstat' = apl(n = 1, d = NA,  h = paste(
"-tstat var_name: var_name is used to specify the column name that lists",
"        the t-statistic values, if available, for the response variable 'Y'.", 
"        In the case where standard errors are available for the effect", 
"        estimates of 'Y', use the option -se.\n", sep = '\n'
	     ) ),

'-se'  = apl(n = 1, d = 0, h = paste(
"-se: This option indicates that standard error for the response variable is",
"         available as input, and a column is designated for the standard error",
"         in the data table. If effect estimates and their t-statistics are the",
"         output from preceding analysis, standard errors can be obtained by",
"         dividing the effect estimates ('betas') by their t-statistics. The",
"         default assumes that standard error is not part of the input.\n", sep='\n')),

'-distY' = apl(n = 1, d = NA,  h = paste(
"-distY distr_name: Use this option to specify the distribution for the response",
"        variable. The default is Gaussian when this option is not invoked. When",
"        skewness or outliers occur in the data, consider adopting the Student's",
"        t-distribution or exGaussian by using this option with 'student' or",
"        'exgaussian'.\n", sep = '\n'
	     ) ),

'-distROI' = apl(n = 1, d = NA,  h = paste(
"-distROI distr_name: Use this option to specify the distribution for the ROIs.",
"        The default is Gaussian when this option is not invoked. When the number of",
"         regions is small (e.g., less than 20), consider adopting the Student's",
"        t-distribution by using this option with 'student'.\n", sep = '\n'
	     ) ),

'-distSubj' = apl(n = 1, d = NA,  h = paste(
"-distSubj distr_name: Use this option to specify the distribution for the subjects.",
"        The default is Gaussian when this option is not invoked. When the number of",
"         regions is small (e.g., less than 20), consider adopting the Student's",
"        t-distribution by using this option with 'student'.\n", sep = '\n'
	     ) ),

'-Subj' = apl(n = 1, d = NA,  h = paste(
"-Subj var_name: var_name is used to specify the column name that is designated as",
"        as the measuring unit variable (usually subject). The default (when this",
"        option is not invoked) is 'Subj'.\n", sep = '\n'
	     ) ),

'-ROI' = apl(n = 1, d = NA,  h = paste(
"-ROI var_name: var_name is used to specify the column name that is designated as",
"        as the region variable. The default (when this option is not invoked) is",
"        'ROI'.\n", sep = '\n'
	     ) ),

'-WCP' = apl(n = 1, d = 1, h = paste(
"-WCP k: This option will invoke within-chain parallelization to speed up runtime.",
"         To take advantage of this feature, you need the following: 1) at least 8",
"         or more CPUs; 2) install 'cmdstan'; 3) install 'cmdstanr'. The value 'k'",
"         is the number of threads per chain that is requested. For example, with 4",
"         chains on a computer with 24 CPUs, you can set 'k' to 6 so that each",
"         chain will be assigned with 6 threads.\n", sep='\n')),

#   '-StanPath' = apl(n = 1, d = 1, h = paste(
#   "-StanPath dir: Use this option to specify the path (directory) where 'cmdstan' is",
#   "         is installed on the computer. Together with option '-WCP', within-chain",
#   "         parallelization can be used to speed up runtime. To take advantage of",
#   "         this feature, you need the following: 1) at least 8 or more CPUs; 2)",
#   "         install 'cmdstan'; 3) install 'cmdstanr'. The default (the absence of the",
#   "         option '-StanPath') means that 'cmdstan' is under the home directory:",
#   "         '~/'; otherwise, explicictly indicate the path as, for example, ",
#   "         '-StanPath \"~/here/is/myStanPath\"'.\n", sep='\n')),

'-PDP' = apl(n = 2, d = NA, h = paste(
"-PDP nr nc: Specify the layout of posterior distribution plot (PDP) with nr rows",
"         and nc columns among the number of plots. For example, with 16 regions,",
"         you can set nr = 4 and nc = 4. The region names will be shown in each plot.",
"         So, label the regions concisely.\n", sep = '\n'
	     ) ),

'-ridgePlot' = apl(n=2, d = NA,  h = paste(
"-ridgePlot width height: This option will plot the posterior distributions stacked",
"         together in a sequential order, likely preferable to the one generated",
"         with option -PDP. The size of the figure window is specified through the",
"         two parameters of width and height in inches. You can fine-tune the plot",
"         yourself by loading up the *.RData file if you know the tricks.\n", sep='\n')),


'-dataTable' = apl(n=c(1, 1000000), d=NA, h = paste(
"-dataTable TABLE: List the data structure in a table of long format (cf. wide",
"         format) in R with a header as the first line. \n",
"         NOTE:\n",
"         1) There should have at least three columns in the table. These minimum",
"         three columns can be in any order but with fixed and reserved with labels:",
"         'Subj', 'ROI', and 'Y'. The column 'ROI' is meant to code the regions",
"         that are associated with each value under the column Y. More columns can",
"         be added in the table for explanatory variables (e.g., groups, age, site)",
"         if applicable. Only subject-level (or between-subjects) explanatory variables",
"         are allowed now. The labels for the columns of 'Subj' and 'ROI'",
"         can be any identifiable characters including numbers.",
"         2) Each row is associated with one and only one 'Y' value, which is the",
"         response variable in the table of long format (cf. wide format) as",
"         defined in R. With n subjects and m regions, there should have totally mn",
"         rows, assuming no missing data. ",
"         3) It is fine to have variables (or columns) in the table that are not used",
"         in the current analysis.",
"         4) The context of the table can be saved as a separate file, e.g., called",
"         table.txt. In the script specify the data with '-dataTable table.txt'.",
"         This option is useful when: (a) there are many rows in the table so that",
"         the program complains with an 'Arg list too long' error; (b) you want to",
"         try different models with the same dataset.\n",
     sep = '\n'
	     ) ),

'-help' = apl(n=0, h = '-help: this help message\n'),
'-show_allowed_options' = apl(n=0, h=
"-show_allowed_options: list of allowed options\n" )
    )
	     
ops <- parse.AFNI.args(args, params, other_ok=FALSE)
if (verb) show.AFNI.args(ops, verb=0, hstr='')
if (is.null(ops)) 
errex.AFNI('Error parsing arguments. See RBA -help for details.')

#Parse dems options
#initialize with defaults
lop <- AFNI.new.options.list(history = '', parsed_args = ops)
lop$chains <- 1
lop$WCP    <- NA
#lop$StanPath   <- NULL
lop$iterations <- 1000
lop$model  <- 1
lop$mean     <- NA
lop$sigma     <- NA
lop$cVars  <- NULL
lop$qVars  <- 'Intercept'
lop$stdz   <- NA
lop$EOI    <- 'Intercept'
lop$qContr <- NA
lop$se     <- NULL
lop$tstat  <- NULL
lop$Y      <- 'Y'
lop$distY  <- 'gaussian'
lop$distROI  <- NA
lop$distSubj <- NA
lop$Subj   <- 'Subj'
lop$ROI    <- 'ROI'
lop$PDP    <- NA
lop$ridgePlot <- NULL

lop$dbgArgs <- FALSE # for debugging purpose
#lop$MD      <- FALSE # for missing data 
lop$student <- FALSE
lop$r2z     <- FALSE # Fisher transformation
lop$verb    <- 0
lop$scale   <- 1
#Get user's input
for (i in 1:length(ops)) {
opname <- strsplit(names(ops)[i],'^-')[[1]];
opname <- opname[length(opname)];
switch(opname,
     prefix = lop$outFN  <- pprefix.AFNI.name(ops[[i]]),
     chains   = lop$chains <- ops[[i]],
     WCP        = lop$WCP    <- ops[[i]],
     #StanPath   = lop$StanPath   <- ops[[i]],
     iterations = lop$iterations <- ops[[i]],
     verb   = lop$verb   <- ops[[i]],
     model  = lop$model  <- ops[[i]],
     mean     = lop$mean     <- ops[[i]],
     sigma     = lop$sigma     <- ops[[i]],
     cVars  = lop$cVars  <- ops[[i]],
     qVars  = lop$qVars  <- ops[[i]],
     stdz   = lop$stdz   <- ops[[i]],
     EOI    = lop$EOI    <- ops[[i]],
     qContr = lop$qContr <- ops[[i]],
     Y      = lop$Y      <- ops[[i]],
     distY  = lop$distY  <- ops[[i]],
     se         = lop$se     <- ops[[i]],
     tstat      = lop$tstat  <- ops[[i]],
     distROI   = lop$distROI   <- ops[[i]],
     distSubj  = lop$distSubj  <- ops[[i]],
     Subj   = lop$Subj   <- ops[[i]],
     ROI    = lop$ROI    <- ops[[i]],
     PDP    = lop$PDP    <- ops[[i]],
     ridgePlot = lop$ridgePlot <- ops[[i]],
     help    = help.RBA.opts(params, adieu=TRUE),
     dbgArgs = lop$dbgArgs <- TRUE,
#     MD      = lop$MD      <- TRUE,
     
     scale   = lop$scale   <- ops[[i]],
     r2z     = lop$r2z     <- TRUE,
     dataTable  = lop$dataTable <- read.table(ops[[i]], header=T),
     )
}

return(lop)
}# end of read.RBA.opts.batch
				       
				       
#Change options list to RBA variable list 
process.RBA.opts <- function (lop, verb = 0) {
if(is.null(lop$outFN)) errex.AFNI(c("Output filename not specified! Add filename with -prefix.\n"))
an <- parse.AFNI.name(lop$outFN)
if(!lop$overwrite && (
    file.exists(paste0(lop$outFN,".txt")) ||
    file.exists(paste0(lop$outFN,".RData")) ||
    file.exists(paste0(lop$outFN,".pdf"))) ) {
 errex.AFNI(c("File ", lop$outFN, " exists! Try a different name.\n"))
 return(NULL)
}      

if(!is.null(lop$cVars[1])) lop$CV <- strsplit(lop$cVars, '\\,')[[1]]   
if(!is.na(lop$qVars[1])) lop$QV <- strsplit(lop$qVars, '\\,')[[1]]


if(lop$chains < 1) lop$chains <- 1

return(lop)
}
# process.RBA.opts(lop, verb = lop$verb)                                               

#################################################################################
########################## Begin RBA main ######################################
#################################################################################

if(!exists('.DBG_args')) { 
args = (commandArgs(TRUE))  
rfile <- first.in.path(sprintf('%s.R',ExecName))
# save only on -dbgArgs          28 Apr 2016 [rickr]
if ('-dbgArgs' %in% args) try(save(args, rfile, file=".RBA.dbg.AFNI.args", ascii = TRUE), silent=TRUE) 
} else {
note.AFNI("Using .DBG_args resident in workspace")
args <- .DBG_args
}
if(!length(args)) {
BATCH_MODE <<- 0
cat(greeting.RBA(),
"Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.\n", 
sep='\n')
#browser()
if(length(args)<6) modFile <- "model.txt" else modFile <- args[6]
if (is.null(lop <- read.RBA.opts.from.file(modFile, verb=0))) {
 stop('Error parsing input from file!');
}

if(0) str(lop)

} else {
if(!exists('.DBG_args')) {
 BATCH_MODE <<- 1
} else {
 BATCH_MODE <<- 0
}
if(is.null(lop <- read.RBA.opts.batch(args, verb = 0)))
 stop('Error parsing input')                

#str(lop);
if(is.null(lop <- process.RBA.opts(lop, verb = lop$verb))) 
 stop('Error processing input')

}
#if(lop$verb > 1) { 
#Too much output, big dump of header structs of input dsets..
#   str(lop)
#}


########################################################################

library("brms")

# write data.frame to a file
outDF <- function(DF, fl) cat(capture.output(DF), file = paste0(fl, '.txt'), sep = '\n', append=TRUE)

# standardize the names for Y, ROI and subject
if(is.na(lop$mean)) {
   names(lop$dataTable)[names(lop$dataTable)==lop$Subj] <- 'Subj'
   names(lop$dataTable)[names(lop$dataTable)==lop$Y] <- 'Y'
   #names(lop$dataTable)[names(lop$dataTable)==lop$ROI] <- 'ROI'

   # make sure ROI and Subj are treated as factors
   if(!is.factor(lop$dataTable[[lop$ROI]])) lop$dataTable[[lop$ROI]] <- as.factor(lop$dataTable[[lop$ROI]])
   if(!is.factor(lop$dataTable$Subj)) lop$dataTable$Subj <- as.factor(lop$dataTable$Subj)
   
   # verify variable types
   if(lop$model==1) terms <- 1 else terms <- strsplit(lop$model, '\\+')[[1]]
   if(length(terms) > 1) {
   #terms <- terms[terms!='1']
   for(ii in 1:length(terms)) {
      if(!is.null(lop$cVars[1])) if(terms[ii] %in% strsplit(lop$cVars, '\\,')[[1]] & !is.factor(lop$dataTable[[terms[ii]]])) # declared factor with quantitative levels
         lop$dataTable[[terms[ii]]] <- as.factor(lop$dataTable[[terms[ii]]])
         if(terms[ii] %in% strsplit(lop$qVars, '\\,')[[1]] & is.factor(lop$dataTable[[terms[ii]]])) # declared numerical variable contains characters
       stop(sprintf('Column %s in the data table is declared as numerical, but contains characters!', terms[ii]))
      }
   }
   
   # number of ROIs
   lop$dataTable[[lop$ROI]] <- as.factor(lop$dataTable[[lop$ROI]])
   nR <- nlevels(lop$dataTable[[lop$ROI]])
   
   cat('===== Summary of variable information =====', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('Total number of ROIs: %i', nR), 
   file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat(sprintf('Response variable Y - mean: %f; SD: %f', mean(lop$dataTable$Y), sd(lop$dataTable$Y)), 
   file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   outDF(summary(lop$dataTable$Y), lop$outFN)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('Data structure:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   outDF(str(lop$dataTable), lop$outFN)
   cat('Subjects:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   outDF(summary(lop$dataTable$Subj), lop$outFN)
   cat('ROIs:', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   outDF(summary(lop$dataTable[[lop$ROI]]), lop$outFN)
   
#   if(!lop$MD) if(nlevels(lop$dataTable$Subj)*nR < nrow(lop$dataTable))
#stop(sprintf('Error: with %d regions and %d subjects, it is expected to have %d rows per subject, leading to totally %d rows in the input data table. However, there are only %d rows. If you have missing data, use option -MD', nR, nlevels(lop$dataTable$Subj), nR, nlevels(lop$dataTable$Subj)*nR, nrow(lop$dataTable)))
}

cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

lop$EOIq <- strsplit(lop$qVars, '\\,')[[1]]
if(!('Intercept' %in% lop$EOIq)) lop$EOIq <- c('Intercept', lop$EOIq)
lop$EOIq <- intersect(strsplit(lop$EOI, '\\,')[[1]], lop$EOIq)
if(is.null(lop$cVars)) lop$EOIc <- NA else 
lop$EOIc <- intersect(strsplit(lop$EOI, '\\,')[[1]], strsplit(lop$cVars, '\\,')[[1]])

if(!is.na(lop$qContr)) {
qContrL <- unlist(strsplit(lop$qContr, '\\,'))
# verify 'vs' in alternating location
ll <- which(qContrL %in% 'vs')
if(!all(ll == seq(2,300,3)[1:length(ll)]))
stop(sprintf('Quantitative contrast specification -qContr is incorrect!'))
lop$qContrL <- qContrL[!qContrL %in% 'vs']
# verify that variable names are correct
if(!all(lop$qContrL %in% c(lop$QV, 'Intercept'))) 
stop(sprintf('At least one of the variable labels in quantitative contrast specification -qContr is incorrect!'))
}

# change the se column name when se is provided as input
if(!is.null(lop$se)) names(lop$dataTable)[which(names(lop$dataTable)==lop$se)] <- 'se'

# convert tstat to se when tstat is provided as input
if(!is.null(lop$tstat)) {
lop$se <- TRUE
lop$dataTable$se <- lop$dataTable$Y/lop$dataTable[[lop$tstat]]
}

# deviation coding: -1/0/1 - the intercept is associated with the mean across the levels of the factor
# each coding variable corresponds to the level relative to the mean: alphabetically last level is
# is baseline or reference level
options(contrasts = c("contr.sum", "contr.poly"))
options(mc.cores = parallel::detectCores())

# within-chain parallelization?
if(!is.na(lop$WCP)) {
require('cmdstanr')
#if(!grepl('\\/$', lop$StanPath)) lop$StanPath <- paste0(lop$StanPath, '/') # make sure / is added to the path
#path <- ifelse(is.null(lop$StanPath), '~/cmdstan', paste0(lop$StanPath, 'cmdstan'))
#set_cmdstan_path(path)
#set_cmdstan_path('~/cmdstan') # where is this located for the user?
}

# Fisher transformation
fisher <- function(r) ifelse(abs(r) < .995, 0.5*(log(1+r)-log(1-r)), stop('Are you sure that you have correlation values so close to 1 or -1?'))
if(lop$r2z) lop$dataTable$Y <- fisher(lop$dataTable$Y)

# standardization
if(!is.na(lop$stdz)) {
sl <- strsplit(lop$stdz, '\\,')[[1]]
for(ii in 1:length(sl)) if(is.numeric(lop$dataTable[[sl[ii]]])) {
lop$dataTable[[sl[ii]]] <- scale(lop$dataTable[[sl[ii]]], center = TRUE, scale = TRUE) 
#if(sl[[ii]] == 'Y') {
#   mmm <- mean(lop$dataTable[[sl[ii]]]) 
#   sss <- sd(lop$dataTable[[sl[ii]]])
#}
#lop$dataTable[[sl[ii]]] <- scale(lop$dataTable[[sl[ii]]], center = TRUE, scale = TRUE)
} else
stop(sprintf('The column %s is categorical, not numerical! Why are you asking me to standardize it?', sl[ii]))
}

set.seed(1234)
lop$dataTable$w <- 1

# Start the clock!
ptm <- proc.time()

## for testing only: remove this soon ####
#lop$dataTable$V1 <- rnorm(nrow(lop$dataTable))
#lop$dataTable$V2 <- rnorm(nrow(lop$dataTable), mean=0.5, sd=1)
#lop$model <- '1+V1+V2'

##################### MCMC ####################

##### model formulation #####

#if(is.na(lop)[1]) { # specifically for ROI-type modeling
if(is.na(lop$mean)) {
  if(is.null(lop$se))  { # model without standard errors
    if(!is.na(lop$distROI) & lop$distROI == 'student') {
       if(!is.na(lop$distSubj) & lop$distSubj == 'student') {
          if(lop$model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|gr(Subj, dist=\'student\')) + (1|gr(', lop$ROI, ',dist=\'student\'))')) else
          modelForm <- as.formula(paste('Y~', lop$model, '+(1|gr(Subj, dist=\'student\'))+(', lop$model, '|gr(', lop$ROI, ',dist=\'student\'))'))  
       } else { # if(!is.na(lop$distSubj) & lop$distSubj == 'student')
          if(lop$model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|Subj) + (1|gr(', lop$ROI, ',dist=\'student\'))')) else
          modelForm <- as.formula(paste('Y~', lop$model, '+(1|Subj)+(', lop$model, '|gr(', lop$ROI, ',dist=\'student\'))'))
       } #if(!is.na(lop$distSubj) & lop$distSubj == 'student')
    } else { # if(!is.na(lop$distROI) & lop$distROI == 'student')
       if(!is.na(lop$distSubj) & lop$distSubj == 'student') {
          if(lop$model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|gr(Subj, dist=\'student\')) + (1|', lop$ROI, ')')) else
          modelForm <- as.formula(paste('Y~', lop$model, '+(1|gr(Subj, dist=\'student\'))+(', lop$model, '|', lop$ROI, ')'))  
       } else { # if(!is.na(lop$distSubj) & lop$distSubj == 'student')
          if(lop$model==1) modelForm <- as.formula(paste('Y ~ 1 + (1|Subj) + (1|', lop$ROI, ')')) else
             modelForm <- as.formula(paste('Y~', lop$model, '+(1|Subj)+(', lop$model, '|', lop$ROI, ')'))
       } # if(!is.na(lop$distSubj) & lop$distSubj == 'student')
    } # if(!is.na(lop$distROI) & lop$distROI == 'student')
  
  } else { # if(is.null(lop$se)): model with standard errors
    if(!is.na(lop$distROI) & lop$distROI == 'student') {
      if(!is.na(lop$distSubj) & lop$distSubj == 'student') {
         if(lop$model==1) modelForm <- as.formula(paste('Y|se(se, sigma = TRUE) ~ 1 + (1|gr(Subj, dist=\'student\')) + (1|gr(', lop$ROI, ',dist=\'student\'))')) else
         modelForm <- as.formula(paste('Y|se(se, sigma = TRUE)~', lop$model, '+(1|gr(Subj, dist=\'student\'))+(', lop$model, '|gr(', lop$ROI, ',dist=\'student\'))'))
      } else {
         if(lop$model==1) modelForm <- as.formula(paste('Y|se(se, sigma = TRUE) ~ 1 + (1|Subj) + (1|gr(', lop$ROI, ',dist=\'student\'))')) else
         modelForm <- as.formula(paste('Y|se(se, sigma = TRUE)~', lop$model, '+(1|Subj)+(', lop$model, '|gr(', lop$ROI, ',dist=\'student\'))'))
      } #if(!is.na(lop$distSubj) & lop$distSubj == 'student')
    } else {
      if(!is.na(lop$distSubj) & lop$distSubj == 'student') {
         if(lop$model==1) modelForm <- as.formula(paste('Y|se(se, sigma = TRUE) ~ 1 + (1|gr(Subj, dist=\'student\')) + (1|', lop$ROI, ')')) else
         modelForm <- as.formula(paste('Y|se(se, sigma = TRUE)~', lop$model, '+(1|gr(Subj, dist=\'student\'))+(', lop$model, '|', lop$ROI, ')'))
      } else { # if(!is.na(lop$distSubj) & lop$distSubj == 'student')
        if(lop$model==1) modelForm <- as.formula(paste('Y|se(se, sigma = TRUE) ~ 1 + (1|Subj) + (1|', lop$ROI, ')')) else
           modelForm <- as.formula(paste('Y|se(se, sigma = TRUE)~', lop$model, '+(1|Subj)+(', lop$model, '|', lop$ROI, ')'))
      } # if(!is.na(lop$distSubj) & lop$distSubj == 'student') 
    } # if(!is.na(lop$distROI) & lop$distROI == 'student') 
  } # if(is.null(lop$se))
} else { # if(is.na(lop$mean))
  m1 <- as.formula(lop$mean)
  if(!is.na(lop$sigma)) m2 <- as.formula(paste0('sigma~',lop$sigma)) # no user-specified standard deviation
} # if(is.na(lop$mean))

if(lop$scale!=1) lop$dataTable$Y <- (lop$dataTable$Y)*lop$scale

#if(lop$model==1) fm <- brm(modelForm, data=lop$dataTable, chains = lop$chains, 
#      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15)) else
#   fm <- brm(modelForm, data=lop$dataTable, 
#      prior=c(prior(normal(0, 1), class = "Intercept"), prior(normal(0, 0.5), class = "sd")),
#      chains = lop$chains, iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))

if(!is.na(lop$WCP)) {
   if(is.na(lop$mean))
      fm <- brm(modelForm, data=lop$dataTable, chains = lop$chains, family=lop$distY, init=0, 
         iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15),
         backend = "cmdstanr", threads = threading(lop$WCP))
   else if(is.na(lop$sigma))
      fm <- brm(m1, data=lop$dataTable, chains = lop$chains, family=lop$distY, init=0,
         iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15),
         backend = "cmdstanr", threads = threading(lop$WCP))
   else fm <- brm(bf(m1,m2), data=lop$dataTable, chains = lop$chains, family=lop$distY, init=0,
         iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15),
         backend = "cmdstanr", threads = threading(lop$WCP))
} else {
   if(is.na(lop$mean))
      fm <- brm(modelForm, data=lop$dataTable, chains = lop$chains, family=lop$distY, init=0,
      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))
   else if(is.na(lop$sigma))
      fm <- brm(m1, data=lop$dataTable, chains = lop$chains, family=lop$distY, init=0,
      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))
   else
      fm <- brm(bf(m1,m2), data=lop$dataTable, chains = lop$chains, family=lop$distY, init=0,
      iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))
} # if(!is.na(lop$WCP))
#   fm <- brm(modelForm, data=lop$dataTable,
#      prior=c(prior(normal(0, 1), class = "Intercept"), prior(gamma(2, 0.5), class = "sd")),
#      chains = lop$chains, iter=lop$iterations, control = list(adapt_delta = 0.99, max_treedepth = 15))

print(format(Sys.time(), "%D %H:%M:%OS3"))

# Stop the clock
proc.time() - ptm

rs <- summary(fm)
ppc <- pp_check(fm, ndraws = 100)
pe <- fixef(fm, summary = FALSE) # Population-Level Estimates
ge <- ranef(fm, summary = FALSE) # Extract Group-Level (or random-effect) Estimate
save.image(file=paste0(lop$outFN, ".RData"))

cat(format(Sys.time(), "%D %H:%M:%OS3"), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat(capture.output(proc.time() - ptm), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

##################### Post Processing ####################

options(width=300)
cat('\n++++++++++++++++++++++++++++++++++++++++++++++++++++\n')
cat('***** Summary information of model information *****\n')
rs
cat('\n***** End of model information *****\n')   
cat('++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n')

cat('\n***** Summary information of model results *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)

#  Rhat checking #
#dd <- function(ll) any(ll[,'Rhat'] > 1.1)
#if(any(sapply(c(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars), rs$random), dd))) {
#   cat('\n***** Warning: convergence issue!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   cat('Consider increasing the number of iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#}

#d1 <- function(ll, mm, nn) any(ll[,mm] > nn)
## Rhat check
#if(any(sapply(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars, rs$random$ROI, rs$random$Subj), d1, 'Rhat', 1.1))) {
#   cat('\n***** WARNING: convergence issue - Rhat > 1.1!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   cat('Consider increasing the number of iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#}
#d2 <- function(ll, mm, nn) any(ll[,mm] < nn)
## effective size check
#if(is.null(attr(rs$cor_pars, 'dimnames')[[1]])) {
#   if(any(sapply(list(fixed=rs$fixed, spec_pars=rs$spec_pars, rs$random$ROI, rs$random$Subj), d2, 'Eff.Sample', 75))) {
#      cat('\n***** WARNING: effective size too small!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#      cat('Consider increasing the number of chains or iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   } 
#} else if(any(sapply(list(fixed=rs$fixed, spec_pars=rs$spec_pars, cor_pars=rs$cor_pars, rs$random$ROI, rs$random$Subj), dd, 'Eff.Sample', 75))) {
#   cat('\n***** WARNING: effective size too small!!! *****\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#   cat('Consider increasing the number of chains or iterations for Markov chains!\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
#}

cat(capture.output(rs), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)   

ns <- lop$iterations*lop$chains/2
aa <- fixef(fm, summary = FALSE)/lop$scale # Population-Level Estimates
#bb <- lapply(ranef(fm, summary = FALSE), `*`, sss) # Extract Group-Level (or random-effect) Estimates
bb <- lapply(ranef(fm, summary = FALSE), `/`, lop$scale)

# compute P+
cnt <- function(x, ns) return(sum(x>0)/ns)

########## region effects #############
# posterior samples at ROIs for a term
psROI <- function(aa, bb, tm) {
  ps <- apply(bb[[lop$ROI]][,,tm], 2, '+', aa[,tm])
  return(ps)
}
# ps <- ww(aa, bb, 'Intercept', nR)

# summary for ROIs: nd - number of digits to output
sumROI <- function(R0, ns, nd) {
  hubs <- data.frame(cbind(apply(R0, 2, mean), apply(R0, 2, sd), apply(R0, 2, cnt, ns), t(apply(R0, 2, quantile, 
      probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))))
  names(hubs) <- c('mean', 'SD', 'P+', '2.5%', '5%', '50%', '95%', '97.5%')
  return(round(hubs,nd))
}
#gg <- sumROI(gg, ns, 3)

#is.even <- function(x) x %% 2 == 0

addTrans <- function(color,trans)
{
  # This function adds transparency to a color.
  # Define transparency with an integer between 0 and 255
  # 0 being fully transparent and 255 being fully visible
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.

  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

plotPDP <- function(fn, ps, nR, nr, nc, w=8) {
   h <- ceiling(8*nr/(nc*2))  # plot window height
   pdf(paste0(fn, "_PDF.pdf"), width=w, height=h)
   #dev.new(width=w, height=h)
   par(mfrow=c(lop$PDP[1], nc), mar=c(2.5,0,0.0,0.8), oma=c(0,0,0,0))
   qq <- apply(ps, 2, quantile, c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)) # 95% central interval
   kk <- 0
   for(ii in 1:nR) {
      kk <- kk+1
      #x <- quantile(ps[,ii], probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))
      dens <- density(ps[,ii])
      #par(mar=c(1.85,0.2,0.0,0.8))
      plot(dens, main='', axes=F, bty="n", xlab='', ylab='')
      axis(side = 1)
      abline(v=0, col='blue')
      #if(is.even(kk)) mtext(dimnames(ps)[[2]][ii], side = 1, line=-7, las=0) else
      mtext(dimnames(ps)[[2]][ii], side = 3, line=-2, las=0)
      x1 <- min(which(dens$x >= qq[6,ii]))  # 97.5% 
      x2 <- max(which(dens$x <  4e10))         # infinity
      x3 <- min(which(dens$x >= -4e10))        # -infinity
      x4 <- max(which(dens$x <  qq[1,ii]))  # 2.5%
      x5 <- min(which(dens$x >= qq[5,ii]))  # 95%
      x6 <- max(which(dens$x <  qq[2,ii]))  # 5%
      x7 <- min(which(dens$x >= qq[4,ii]))  # 90%
      x8 <- max(which(dens$x <  qq[3,ii]))  # 10%
      with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=addTrans('green',175))) # right tail
      with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col=addTrans('green',175))) # left tail
      with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col=addTrans('orange',150)))
      with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col=addTrans('gray',125)))
      with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col=addTrans('gray',125)))
      #with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="green")) # right tail
      #with(dens, polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, y[x3:x4], 0), col="green")) # left tail
      #with(dens, polygon(x=c(x[c(x5,x5:x1,x1)]), y= c(0, y[x5:x1], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x4,x4:x6,x6)]), y= c(0, y[x4:x6], 0), col="orange"))
      #with(dens, polygon(x=c(x[c(x7,x7:x5,x5)]), y= c(0, y[x7:x5], 0), col="gray"))
      #with(dens, polygon(x=c(x[c(x6,x6:x8,x8)]), y= c(0, y[x6:x8], 0), col="gray"))
      if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('green',200), lwd=3)
      if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('orange',150), lwd=3)
      if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = 'solid', border = addTrans('gray',100), lwd=3)
      #if(qq[1,ii] > 0 | qq[6,ii] < 0) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'green', lwd=3)
      #if((qq[1,ii] < 0 & qq[2,ii] > 0) | (qq[5,ii] < 0 &  qq[6,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'orange', lwd=3)
      #if((qq[2,ii] < 0 & qq[3,ii] > 0) | (qq[4,ii] < 0 &  qq[5,ii] > 0)) rect(range(dens$x)[1], range(dens$y)[1], range(dens$x)[2], range(dens$y)[2], lty = '1373', border = 'gray', lwd=3)
   }
   dev.off()
}

ridge <- function(dat, xlim, labx, wi, hi) {
   # required libraries
   library(data.table)
   library(ggplot2)
   library(ggridges)
   library(dplyr)
   library(tidyr)
   library(scales)

   data <- data.frame(dat)
   data$X <- NULL
   nobj=dim(data)[1]

   # rename columns with ROI list
   rois <- dimnames(dat)[[2]]
   colnames(data) <- rois
   data_stats <- data.frame(1:length(rois))

   # create ROI column instead of numerics to match threat table above
   data_stats$ROI <- rois
   data_stats$mean <- colMeans(data)  # median: quantile(x, probs=.5)
   data_stats$P <- colSums(data > 0)/nobj
   data_stats$Pn <- ifelse(data_stats$P < .5, 1-data_stats$P, data_stats$P)
   # this will order the distributions correctly
   data_stats <- data_stats[order(data_stats$mean),]

   data_trans <- as.data.frame(t(as.matrix(data)))
   # add two more columns
   data_trans <- tibble::rownames_to_column(data_trans, "ROI")
   data_trans$X <- 1:nrow(data_trans)

   # merge values & stats into one table by ROI
   data_merge <- merge(data_stats, data_trans, by = "ROI")
   data_merge <- data_merge[order(data_merge$X),]

   # Transform data into long form: Melt dataframe by ROI
   data_long <- reshape2::melt(data_trans, id=c("ROI","X"))
   data_long <- data_long[order(data_long$X),]

   #clunky, but for now stats by ensuring orders are all the same and repeating each value nobj times. no success for alternatives.
   data_long$mean <- rep(data_merge$mean, each = nobj)
   data_long$P <- rep(data_merge$P, each =nobj)
   data_long$Pn <- rep(data_merge$Pn, each =nobj)
   data_long$gray.vals <- rep(data_merge$gray.vals, each =nobj)

   ########################  G R A P H I N G  #########################################################
   #######################     V A R I A B L E S    ######################################################
   # set your labels here so you don't have to change within the plot below:
   y.axis.labs <- data_stats$ROI                              # y axis labels
   sec.y.axis.labs <- round(data_stats$P,2)                   # second y axis labels (probabilities) - Rounded to 2 decimals

   ################# X AXIS LABELS ###########################################################
   # X AXIS LABELS NEED TO CHANGE TO CORRESPOND TO DATA SET! UNCOMMENT WHICHEVER MATCHES
   x.axis.labs <- NULL                                # x axis labels  INTERACTION, not sure what to put.
   x.labs.pos <- NULL                                 # a axis position INTERACTION, change when labels decided

   ######## T I T L E S #############################################################
   #data.name <- tl
   graph.title <- "Interaction (% signal change)"                                   # graph title
   legend.title <- "P+"                              # legend title
   y.axis.title <- NULL                                       # for now ...
   x.axis.title <- NULL                                       # for now...

   ########################## D A T A  ##############################################################
   # GRAPH DATA
   dataset <- data_long
   x.values <- data_long$value                               # x values
   y.values <- data_long$ROI                                 # y values
   y.values.RO <- data_long$value                            # values to reorder Y by
   distrib.fill <- data_long$P                       # fill graph with probabilities
   group <- data_long$ROI

   ######################### S A V E  ################################################
   # SAVE SETTINGS -- Currently low res and jpeg to make it easier to share
   # adjusting height + width will throw font sizes out of wack: need change (see other aspects below)

   dpi <- 300
   units <- "in"                                           # "in", "cm", or "mm"
   height <- 5
   width <- 9
   file.type <- ".jpeg"                   # can be ".jpeg",".pdf",".png",".bmp",".tiff",etc

   ############################### O T H E R  #################################################
   #gradient.colors<-c("#41245C","yellow","gray","gray","blue","#C9182B") # change gradient colors
   gradient.colors <- c("blue","cyan","gray","gray","yellow","#C9182B")  # change gradient colors here
   ROI.label.size <- 15                 # adjust ROI and probability y-axis font size
   P.label.size <- 15
   title.size <- 20                         # adjust graph title size
   x.axis.size <- 15                                        # adjust x-axis label sizes

   ##################  G R A P H  ########################################
   # change information about the graph and add other characteristics using ggplot and ggridges
   ggplot(dataset, aes(x = x.values,
                       y = as.numeric(reorder(y.values, y.values.RO)),
                       fill = distrib.fill,
                       group = group))   +
     guides(fill = guide_colorbar(barwidth = 1,             #legend characteristics
                                  barheight = 20,
                                  nbin = 100, # can change # bins to play around with color gradient
                                  frame.colour = "black",
                                  frame.linewidth = 1.5,
                                  ticks.colour = "black",
                                  title.position = "top",
                                  title.hjust = 0.5)) +
     #geom_density_ridges() +                            # scale = spacing, alpha = transparency
     stat_density_ridges(quantile_lines = TRUE,         # divide into two quantiles (show mean)
                         quantiles = 2,
                         size = .6,
                         alpha = .8,
                         scale = 2,
                         color = "black") +
     geom_vline(xintercept = 0,                        #create line at X = 0
                linetype="solid",
                alpha = 1,
                size = 1,
                color = "green") +
     scale_fill_gradientn(
                          colors = gradient.colors,                       # set gradient
                          limits = c(0,1),                                # scale size
                          name = legend.title,
                         breaks = c(0,0.05,0.1,0.9,0.95,1),
                         expand = expansion(0),
                         labels = c("0","0.05","0.1","0.9", "0.95","1")
                         ) +
     scale_y_continuous(breaks = 1:length(rois), # A VERY HACK-Y WAY TO HAVE TWO Y AXES W DISCRETE DATA
                        labels = y.axis.labs,   # Trick ggplot into thinking data is continuous...
                        sec.axis = sec_axis(~.,  # Second axis to show probabilities
                                            breaks = 1:length(rois),
                                            labels = sec.y.axis.labs)) +
     theme_ridges(font_size = ROI.label.size, grid = TRUE, center_axis_labels = TRUE) +  # theme info
     #ggtitle(graph.title)+                                                   # graph title
     annotate("text", x=0.06, y=1.5, label=labx, size=5.5)+
     theme(
       plot.title = element_text(vjust = -0.5, size = title.size),   # plot title size and position
       #axis.title.x=element_text(vjust=0, hjust=0.5),
       axis.text.y.left = element_text(size=ROI.label.size),        # y-axis text size
       axis.text.y.right = element_text(size = P.label.size),       # y-axis info for right axis
       axis.text.x = element_text(size = x.axis.size),   # x-axis text size/face
       #axis.text.x = element_text(size = x.axis.size, face = "bold"),   # x-axis text size/face
       legend.title.align = 5,
       #legend.text = element_text(face = "bold"),
       legend.title = element_text(size = 15))+
       #legend.title = element_text(face = "bold", size = 15))+
       labs(
       #x = 'interaction (% signal change)',                 # Add or not add X and Y labels
       x = NULL,
       y = NULL) +
     scale_x_continuous(limits = xlim)+xlab(labx)
     ggsave(file = paste0(labx, "_ridge.pdf"), width=wi, height=hi, dpi = 120)
}

# for Intercept and quantitative variables
if(any(!is.na(lop$EOIq) == TRUE)) for(ii in 1:length(lop$EOIq)) {
   cat(sprintf('===== Summary of region effects for %s (RBA results) =====', lop$EOIq[ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   ps0 <- psROI(aa, bb, lop$EOIq[ii])
   gg <- sumROI(ps0, ns, 8)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   if(any(!is.na(lop$PDP) == TRUE)) plotPDP(lop$EOIq[ii], ps0, nR, lop$PDP[1], lop$PDP[2], 8)
   if(!is.null(lop$ridgePlot)) ridge(ps0, range(ps0), lop$EOIq[ii], lop$ridgePlot[1], lop$ridgePlot[2])
}

# for contrasts among quantitative variables
if(any(!is.na(lop$qContr) == TRUE)) for(ii in 1:(length(lop$qContrL)/2)) {
   cat(sprintf('===== Summary of region effects for %s vs %s (RBA results) =====', lop$qContrL[2*ii-1], lop$qContrL[2*ii]), 
      file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   ps0 <- psROI(aa, bb, lop$qContrL[2*ii-1]) - psROI(aa, bb, lop$qContrL[2*ii])
   gg <- sumROI(ps0, ns, 8)
   cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   if(any(!is.na(lop$PDP) == TRUE)) plotPDP(paste0(lop$qContrL[2*ii-1], '-', lop$qContrL[2*ii]), ps0, nR, lop$PDP[1], lop$PDP[2], 8)
   if(!is.null(lop$ridgePlot)) ridge(ps0, range(ps0), lop$qContrL[2*ii-1], lop$ridgePlot[1], lop$ridgePlot[2])
}

# for factor
if(any(!is.na(lop$EOIc) == TRUE)) for(ii in 1:length(lop$EOIc)) {
   lvl <- levels(lop$dataTable[[lop$EOIc[ii]]])  # levels
   nl <- nlevels(lop$dataTable[[lop$EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
   ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
   for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(lop$EOIc[ii],jj))
   ps[nl,,] <- psROI(aa, bb, 'Intercept') # Intercept: averge effect 
   psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
   for(jj in 1:(nl-1)) {
      psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      psa[nl,,] <- psa[nl,,] + ps[jj,,] 
   }
   psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level
   #dimnames(psa) <- list(NULL, NULL, dimnames(bb$ROI)[[2]]) # add ROI names for plotting
   dimnames(psa) <- list(NULL, NULL, dimnames(bb[[lop$ROI]])[[2]]) # add ROI names for plotting
   
   oo <- apply(psa, 1, sumROI, ns, 8)
   oo <-lapply(oo, 'rownames<-', dimnames(bb[[lop$ROI]])[[2]])
   cat(sprintf('===== Summary of region effects for %s (RBA results) =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:nl) {
      cat(sprintf('----- %s level: %s', lop$EOIc[ii], lvl[jj]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(oo[[jj]]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      if(!is.null(lop$ridgePlot)) ridge(psa[jj,,], range(psa[jj,,]), paste0(lop$EOIc[ii],'-',lvl[jj]), lop$ridgePlot[1], lop$ridgePlot[2])
   }

   if(any(!is.na(lop$PDP) == TRUE)) for(jj in 1:nl)
      plotPDP(paste0(lop$EOIc[ii], '_', lvl[jj]), psa[jj,,], nR, lop$PDP[1], lop$PDP[2], 8)
   
   cat(sprintf('===== Summary of region effects for %s comparisons (RBA results) =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 8)
      rownames(oo) <- dimnames(bb[[lop$ROI]])[[2]]
      cat(capture.output(oo), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      if(any(!is.na(lop$PDP) == TRUE))  plotPDP(paste0(lvl[jj], '-', lvl[kk]), psa[jj,,] - psa[kk,,], nR, lop$PDP[1], lop$PDP[2], 8)
      if(!is.null(lop$ridgePlot)) ridge(psa[jj,,] - psa[kk,,], range(psa[jj,,] - psa[kk,,]), paste0(lop$EOIc[ii], '-', lvl[jj], 'vs', lvl[kk]), lop$ridgePlot[1], lop$ridgePlot[2])
   }
}

##################### conventional GLM #####################
if(is.na(lop$mean)) {
   mm <- list()
   GLM <- as.formula(paste('Y ~', lop$model))
   if(lop$scale!=1) lop$dataTable$Y <- (lop$dataTable$Y)/lop$scale  # scale back for GLM
   for(ii in levels(lop$dat[[lop$ROI]])) mm[[ii]] = lm(GLM, data=lop$dat[lop$dat[[lop$ROI]]==ii,])
   nn <- lapply(mm, summary)
   ll <- lapply(nn, `[`, 'coefficients')
   
   sumGLM <- function(ll, tm, nR, DF, nd) {
      th <- qt(c(0.025, 0.05, 0.5, 0.95, 0.975), DF)
      rr <- matrix(0, nrow = nR, ncol = 8, dimnames=list(levels(lop$dat[[lop$ROI]]), c('mean', 'SD', '2-sided-p', '2.5%', '5%', '50%', '95%', '97.5%')))
      rownames(rr) <- levels(lop$dat[[lop$ROI]])
      if(tm == 'Intercept') tm <- '(Intercept)'
      for(ii in 1:nR) {
        u1 <- ll[[ii]]$coefficients[tm,1] # mean
        u2 <- ll[[ii]]$coefficients[tm,2] # sd
        u3 <- ll[[ii]]$coefficients[tm,4] # 2-sided p
        rr[ii,] <- round(c(u1, u2, u3, u1+u2*th),nd)
      } 
      return(rr)
   }
   
   # for Intercept and quantitative variables
   if(any(!is.na(lop$EOIq) == TRUE)) for(ii in 1:length(lop$EOIq)) {
      cat(sprintf('===== Summary of region effects under GLM for %s (for reference only): no adjustment for multiplicity =====', lop$EOIq[ii]), 
         file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      gg <- sumGLM(ll, lop$EOIq[ii], nR, nn[[ii]]$df[2], 8)
      cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }
   
   # for contrasts among quantitative variables
   if(any(!is.na(lop$qContr) == TRUE)) for(ii in 1:(length(lop$qContrL)/2)) {
      cat(sprintf('===== Summary of region effects under GLM for %s vs %s (for reference only) =====', lop$qContrL[2*ii-1], lop$qContrL[2*ii]), 
         file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat(capture.output(gg), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
   }
   
   # for factor
   if(any(!is.na(lop$EOIc) == TRUE)) for(ii in 1:length(lop$EOIc)) {
      #ss <- do.call(rbind,lapply(lapply(nn, `[[`, 'coefficients'), `[`,2,,drop=FALSE))
      lvl <- levels(lop$dataTable[[lop$EOIc[ii]]])  # levels
      nl <- nlevels(lop$dataTable[[lop$EOIc[ii]]])  # number of levels: last level is the reference in deviation coding
      ss <- vector("list", length = nl-1) 
      for(jj in 1:(nl-1)) {
         ss[[jj]] <- do.call(rbind,lapply(lapply(nn, `[[`, 'coefficients'), `[`,2,,drop=FALSE))
         rownames(ss[[jj]]) <- levels(lop$dat[[lop$ROI]])
      }
      #ss[[nl]] <- sumGLM(ll, '(Intercept)', nR, nn[[ii]]$df, 8)
      #ssa <- vector("list", length = nR)
      #for(jj in 1:(nl-1)) {
      #   ssa[[jj]] <- ss[[nl]]  + ss[[jj]]
      #   ssa[[nl]] <- ssa[[nl]] + ss[[jj]] 
      #}
      #ssa[[nl]] <- ss[[nl]] - ssa[[nl]]  # reference level
      #
      #
      #ps <- array(0, dim=c(nl, ns, nR)) # posterior samples
      #for(jj in 1:(nl-1)) ps[jj,,] <- psROI(aa, bb, paste0(lop$EOIc[ii],jj))
      #ps[nl,,] <- psROI(aa, bb, 'Intercept') # Intercept: averge effect 
      #psa <- array(0, dim=c(nl, ns, nR)) # posterior samples adjusted
      #for(jj in 1:(nl-1)) {
      #   psa[jj,,] <- ps[nl,,]  + ps[jj,,]
      #   psa[nl,,] <- psa[nl,,] + ps[jj,,] 
      #}
      #psa[nl,,] <- ps[nl,,] - psa[nl,,]  # reference level
      #
      #oo <- apply(psa, 1, sumROI, ns, 8)
      
      #cat(sprintf('===== Summary of region effects under GLM for %s =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #for(jj in 1:nl) {
      #   cat(sprintf('----- %s level: %s', lop$EOIc[ii], lvl[jj]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #   cat(capture.output(oo[[jj]]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #}
      
      cat(sprintf('===== Summary of region effects under GLM for %s comparisons (for reference only) =====', lop$EOIc[ii]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      for(jj in 1:(nl-1)) {
         cat(sprintf('----- level comparison: %i vs reference level', jj), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
         cat(capture.output(ss[[jj]]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
         cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #for(jj in 1:(nl-1)) for(kk in (jj+1):nl) {
      #   cat(sprintf('----- level comparison: %s vs %s', lvl[jj], lvl[kk]), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #   oo <- sumROI(psa[jj,,] - psa[kk,,], ns, 8)
      #   cat(capture.output(oo), file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      #   cat('\n', file = paste0(lop$outFN, '.txt'), sep = '\n', append=TRUE)
      }
   }
}
################

# save it again
save.image(file=paste0(lop$outFN, ".RData"))
cat("\nCongratulations! The above results are saved in file ", lop$outFN, "\n\n", sep='')
