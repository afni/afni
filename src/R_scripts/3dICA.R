#!/usr/bin/env afni_run_R
#Welcome to 3dICA.R, an AFNI IVA Package!
#-----------------------------------------------------------
#Version 0.0.1, Feb. 29, 2007
#Author: Gang Chen (gangchen@mail.nih.gov)
#Website: http://afni.nimh.nih.gov/sscc/gangc/ica.html
#SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
#-----------------------------------------------------------

# Commannd line to run this script: 3dIVA.R Output (Output is a file
# in which the running progress including error message will be stored)

system("rm -f .RData")
source(file.path(Sys.getenv("AFNI_R_DIR"), "AFNIio.R"))
#source(file.path(Sys.getenv("LME"), "AFNIio.R"))
library(fastICA)

# C is much faster than R!
meth <- "C"         # or "R"

# Line 1: data type - volume or surface
parFile <- commandArgs()[6]
#paste(commandArgs())
InFile <- unlist(strsplit(unlist(scan(file=parFile, what= list(""), 
   skip=0, nline=1)), "\\:"))[2]
	
View <- unlist(strsplit(unlist(strsplit(InFile, "\\+"))[2], "\\."))[1]

#  Line 2: Output filename for the components in 3D
#how to check output filename?
Out <- unlist(strsplit(unlist(scan(file=parFile, what= list(""), 
   skip=1, nline=1)), "\\:"))[2]
OutFile <- paste(Out, "+orig", sep="")

#  Line 3: Output filename for the mixing matrix in 1D. Transformed for 
# easier handling when plotting with 1dplot
OutTemp <- unlist(strsplit(unlist(scan(file=parFile, what= list(""), 
   skip=2, nline=1)), "\\:"))[2]
OutTempFile <- paste(OutTemp, ".1D", sep="")

#  Line 4: Number of components
NoComp <- as.integer(unlist(strsplit(unlist(scan(file=parFile, 
   what= list(""), skip=3, nline=1)), "\\:"))[2])
	
#  Line 5: function for approximation to neg-antropy
Func <- unlist(strsplit(unlist(scan(file=parFile, 
   what= list(""), skip=4, nline=1)), "\\:"))[2]
	
#  Line 6: extraction method
Type <- unlist(strsplit(unlist(scan(file=parFile, 
   what= list(""), skip=5, nline=1)), "\\:"))[2]
		

Data <- read.AFNI(InFile)
dimx <- Data$dim[1]
dimy <- Data$dim[2]
dimz <- Data$dim[3]
tp   <- Data$dim[4]

NData <- array(data=NA, dim=c(dimx, dimy, dimz, tp))
NData <- Data$ttt
dim(NData) <- c(prod(dimx, dimy, dimz), tp)

#ww <- apply(NData, 4, rbind)


# If the number of components are estimated to be very high (like 100 components for 200 images) 
# then around 20 to 30 components should give a reasonable answer.

# 10500 voxels, 38 times points
ica <- fastICA(NData, NoComp, alg.typ = Type, fun = "logcosh", alpha = 1,
                  method = meth, row.norm = FALSE, maxit = 200, 
                  tol = 0.0001, verbose = TRUE)

MData <- ica$S 
dim(MData) <- c(dimx, dimy, dimz, NoComp)

MyLabel <- rep("component", NoComp)

write.AFNI(OutFile, MData, MyLabel, note=Data$header$HISTORY_NOTE, origin=Data$origin, delta=Data$delta, idcode="whatever")
statpar <- "3drefit"

if (View == "tlrc") statpar <- paste(statpar, " -view tlrc -newid ", OutFile) else
   statpar <- paste(statpar, " -newid ", OutFile)
system(statpar)

# dump out the mixing matrix 
write(ica$A, file = OutTempFile, ncolumns = NoComp)

PlotComm <- "1dplot -ynames"
for (ii in 1:NoComp) PlotComm <- paste(PlotComm, sprintf(" Comp%i", ii)) 
PlotComm <- paste(PlotComm, " -xlabel TR -plabel TimeCoursesOfComponents ", OutTempFile, " &")
system(PlotComm)

paste("The time courses of the components are plotted with the following command:")
paste(PlotComm)
print(sprintf("Congratulations! You've got output %s+tlrc.* and %s", Out, OutTempFile))

# set save defaults using option:
options(save.defaults=list(ascii=TRUE, safe=FALSE))
save.image()
unlink(".RData")
