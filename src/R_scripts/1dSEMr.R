print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("          ================== Welcome to 1dSEMr.R ==================          ")
print("AFNI Path Analysis (or Structural Equation Modeling) Package!")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print("Version 0.0.2,  March. 18, 2009")
print("Author: Gang Chen (gangchen@mail.nih.gov)")
print("Website: TBD")
print("SSCC/NIMH, National Institutes of Health, Bethesda MD 20892")
print("#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

libLoad <- function(myLib) {
   sucLoad <- FALSE
   sucCheck <- FALSE
   try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
   if (sucLoad) {print(sprintf("Package %s successfully loaded!", myLib)); sucCheck <- TRUE} else {
	  	try(install.packages(myLib))
      try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
      if (sucLoad) print(sprintf("Package %s successfully loaded...", myLib)) 
   	}
}

libLoad("sem")  # load sem by John Fox

print("~~~~~~~~~~~~~~~~~")
print("Use CNTL-C on Unix or ESC on GUI version of R to stop at any moment.")
print("~~~~~~~~~~~~~~~~~")

nROIs <- as.integer(readline("Number of regions/nodes (e.g., 5)? "))     # number of ROIs
labelROIs <- vector('list', nROIs)
# Get ROI labels
for (ii in 1:nROIs) labelROIs[[ii]] <- readline(sprintf("No. %i ROI label: ", ii))

#Get covariance matrix
print("~~~~~~~~~~~~~~~~~")
print("Since the variance-covariance matrix is symmetric, you don't have to provide the full")
print("matrix, and instead only provide the lower-triangular part as many lines as the number")
print("of ROIs, omitting the above-diagonal elements. For example, a variance-covariance for 5")
print("5 ROIs would look like (diagnals would be 1s if correlation matrix is provided):")
print(".828                             ")
print(".776   .779                      ")
print(".439   .493    .46               ")
print(".432   .464    .425   .674       ")
print(".447   .489    .443   .59    .541")
print("~~~~~~~~~~~~~~~~~")
print(sprintf("Hit RETURN when seeing line No. %i.", (nROIs+1)*nROIs/2+1))
covMat <- read.moments(diag=TRUE, names=labelROIs)

#Acquire user-defined network
print("~~~~~~~~~~~~~~~~~")
print("Provide the network to be validated. List each connection on one line with 3 ")
print("components: 1st the directional path, then path parameter name, and lastly the start")
print("value. Use a single-headed or directional arrow to indicate a path, for example,")
print("A->B. Start value for a path coefficient can be given as NA if you have no idea")
print("about its approximate range. NOTE: Don't forget to use bi-directional arrows to")
print("specify a residural term for each ROI!!! For example,")
print("IPL -> VEC, th1, NA   #this line and 5 below are path specifications among the 5 ROIs")
print("VEC -> PFC, th2, NA")
print("PFC -> SMA, th3, NA")
print("SMA -> IFG, th4, NA")
print("IFG -> IPL, th5, NA")
print("VEC -> IPL, th6, NA")
print("VEC <-> VEC, b1, NA   #this line and 4 below are for estimating the residuals")
print("PFC <-> PFC, b2, NA")
print("SMA <-> SMA, b3, NA")
print("IFG <-> IFG, b4, NA")
print("IPL <-> IPL, b5, NA")
print("~~~~~~~~~~~~~~~~~")
print("Same parameter name for two or more paths indicates an equality constraint.")
print("~~~~~~~~~~~~~~~~~")

print("Hit RETURN when finished.")
pathModel <- specify.model()
print("~~~~~~~~~~~~~~~~~")
# degrees of freedom
DFs <- as.integer(readline("Degrees of freedom (e.g., 127)? ")) 
print("~~~~~~~~~~~~~~~~~")
print("If one of the following methods fails, try the other one.")
method <- as.logical(as.integer(readline("Analysis method (0: numeric; 1: analytic)? ")))
# fitted model
fm <- sem(pathModel, covMat, DFs, analytic.gradient = method)

# display the result
print("Satistical result for the specified network:")
print(summary(fm))
print("~~~~~~~~~~~~~~~~~")
print("Confidence interval for a path can be obtained with its standard error from the table above.")
