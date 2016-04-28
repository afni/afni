#!/usr/bin/env AFNI_Batch_R


#Clean up
rm(list = ls())

first.in.path <- function(file) {
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff<-ff[lapply(ff,file.exists)==TRUE];
   #cat('Using ', ff[1],'\n');
   return(gsub('//','/',ff[1], fixed=TRUE)) 
}
source(first.in.path('AFNIio.R'))
source(first.in.path('AFNIplot.R'))

ExecName <- '1dRplot'


greeting.1dRplot <- function ()
   return( "#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          ================== Welcome to 1dRplot.R ==================          
             An R beta version of 1dplot
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      )
      
reference.1dRplot <- function ()
   return(
""
   )
examples.1dRplot.getdata <- function () {
   return("
To download demo data from AFNI's website run this command:
-----------------------------------------------------------
curl -o demo.X.xmat.1D afni.nimh.nih.gov/pub/dist/edu/data/samples/X.xmat.1D
curl -o demo.motion.1D afni.nimh.nih.gov/pub/dist/edu/data/samples/motion.1D
")
}

run.1dRplot.examples <- function () {
   ii <- 1
   while (!is.null(s <- examples.1dRplot(ii))) {
      s <- strsplit(s,'1dRplot')[[1]][2]
      #str(s)
      sys.AFNI(paste('1dRplot ', s, '&'), echo=TRUE)
      if (prompt.AFNI("Continue ?", c('y','n'))==1) {
         ii <- ii + 1
      } else {
         break;
      }      
   }
   return()
}

examples.1dRplot <- function (demo=0) {
   s <- vector()
   ii <- 0

   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot -input demo.X.xmat.1D'[5..10]'",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input demo.X.xmat.1D'[5..10]' \\
         -input_type XMAT",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input demo.motion.1D \\
         -input_type VOLREG",
      sep = '')
   )

   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot -input 'R:plot.1D.testmat(100, 10)'",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input 'R:plot.1D.testmat(100, 5)' \\
         -one ",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot -input 'R:plot.1D.testmat(100, 10)' \\
         -one \\
         -col.ystack",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot -input 'R:plot.1D.testmat(100, 10)' \\
         -one \\
         -col.ystack \\
         -col.grp '1D:1 1 1 2 2 2 3 3 3 3'  \\
         -grp.label slow medium fast \\
         -prefix ta.jpg \\
         -yax.lim 0 18 \\
         -leg.show \\
         -leg.position top ",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot -input 'R:plot.1D.testmat(100, 10)' \\
         -one \\
         -col.ystack \\
         -col.grp '1D:1 1 1 2 2 2 3 3 3 3'  \\
         -grp.label slow medium fast \\
         -prefix tb.jpg \\
         -yax.lim 0 18 \\
         -leg.show \\
         -leg.position top \\
         -nozeros \\
         -addavg ",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot -input 'R:plot.1D.testmat(100, 10)' \\
         -one \\
         -col.ystack \\
         -col.grp '1D:1 1 1 2 2 2 3 3 3 3'  \\
         -grp.label slow medium fast \\
         -prefix tb.jpg \\
         -yax.lim 0 18 \\
         -leg.show \\
         -leg.position top \\
         -nozeros \\
         -addavg \\
         -col.text.lym Tutti mi chiedono tutti mi vogliono \\
                       Donne ragazzi vecchi fanciulle \\
         -col.text.rym \"R:paste('Col',seq(1,10), sep='')\" ",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s,paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input 'R:plot.1D.testmat(100, 2)' \\
         -one \\
         -col.plot.char 2 \\
         -col.plot.type p  ",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s, paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input 'R:plot.1D.testmat(100, 2)' \\
         -one \\
         -col.line.type 3 \\
         -col.plot.type l ",
      sep = '')
   )
   
   ii <- ii + 1; s <- c(s, paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input 'R:plot.1D.testmat(100, 2)' \\
         -one \\
         -col.plot.char 2 \\
         -col.line.type 3 \\
         -col.plot.type b ",
      sep = '')
   )  
     
   ii <- ii + 1; s <- c(s, paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input 'R:plot.1D.testmat(100, 2)' \\
         -one \\
         -col.plot.char 2 5\\
         -col.line.type 3 4\\
         -col.plot.type b \\
         -TR 2 ",
      sep = '')
   )   

   ii <- ii + 1; s <- c(s, paste(
"
Example ", ii," --- :
-------------------------------- 
1dRplot  -input 'R:plot.1D.testmat(100, 2)' \\
         -one -col.plot.char 2 -col.line.type 3 \\
         -col.plot.type b -TR 2 \\
         -yax.tic.text 'numa numa numa numaei' \\
         -xax.tic.text 'Alo'  'Salut' 'sunt eu' 'un haiduc'",
      sep = '')
   )   
   
   if (demo==0) demo=1:length(s)
   if (max(demo) > length(s)) return(NULL)
   else return (s[demo])
}

#The help function for 1dRplot batch (command line mode)
help.1dRplot.opts <- function (params, alpha = TRUE, itspace='   ', adieu=FALSE) {

   intro <- 
'
Usage:
------ 
 1dRplot is a program for plotting a 1D file'
   

   parnames <- names(params)
   ss <- vector('character')
   if (alpha) {
       parnames <- sort(parnames)   
      ss <- paste('Options in alphabetical order:\n',
                  '------------------------------\n', sep='')
   } else {
      ss <- paste('Options:\n',
                  '--------\n', sep='')
   }
   for (ii in 1:length(parnames)) {
      op <- params[parnames[ii]][[1]]
      if (!is.null(op$help)) {
         ss <- c(ss , paste(itspace, op$help, sep=''));
      } else {
         ss <- c(ss, paste(itspace, parnames[ii], 
                           '(no help available)\n', sep='')); 
      }
   }
   ss <- paste(ss, sep='\n');
   cat(intro,  
         ss,
         eval.AFNI.string.help(),
         examples.1dRplot.getdata(), 
         paste(examples.1dRplot(), '\n', sep='\n'),
         reference.1dRplot(), sep='\n');
   
   if (adieu) exit.AFNI();
}

parse.1dRplot.colinput <- function(op) {
   #If multiple entries, return them
   if (length(op) > 1 || 
       is.numeric(op) ) return(op)
   
   if (is.character(op)) {
      if (op == "ENUM") return(op)
   }
   #If filename, or 1D or R expression
   if (is.AFNI.R.string(op) || is.AFNI.1D.string(op) ||
      file.exists(op)) {
      mm <- read.AFNI.matrix(op)
      return(mm)
   } 
   return(op)
}

parse.1dRplot.other <- function(op, ll) {
   if (length(op)) {
      if (is.null(ll$dmat) || is.na(ll$dmat)) {
         ll$dmat <- op
      } else {
         ll$dmat <- c(ll$dmat, op)
      }
   }
   return(ll)
}

init.1DRplot.lop <- function () {
   lop <- plot.1D.optlist()
   return(lop)
}

#Change command line arguments into an options list
read.1dRplot.opts.batch <- function (args=NULL, verb = 0) {
   
   params <- list (
      '-i' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-i 1D_INPUT: file to plot. This field can have multiple\n",
   "                 formats. See Data Strings section below.\n",
   "             1dRplot will automatically detect certain\n",
   "             1D files ouput by some programs such as 3dhistog\n",
   "             or 3ddot and adjust parameters accordingly.\n"
                     ) ),
                     
      '-input' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input 1D_INPUT: Same as -i\n"
                     ) ),
                     
      '-input_type' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input_type 1D_TYPE: Type of data in 1D file.\n",
   "            Choose from 'VOLREG', or 'XMAT'\n" 
                     ) ),
                     
      '-input_delta' = apl(n = c(1, Inf), d = NA,  h = paste(
   "-input_delta 1D_INPUT: file containing value for error bars\n"
                     ) ),
                     
      '-x' = apl(n = c(1), h = paste (
   "-x 1D_INPUT: x axis. You can also use the string 'ENUM'\n",
   "             to indicate that the x axis should go from\n",
   "             1 to N, the number of samples in -input\n"
                  ) ), 

      '-save' = apl(n = 1, d = NA,  h = paste(
   "-save PREFIX: Save plot and quit\n",
   "                   No need for -prefix with this option\n"
                     ) ),
                     
      '-save.Rdat' = apl(n = 0, d = NA,  h = paste(
   "-save.Rdat : Save data list for reproducing plot in R.\n",
   "             You need to specify -prefix or -save\n",
   "             along with this option to set the prefix.\n",
   "             See also -load.Rdat\n"
                     ) ),
      
      '-load.Rdat' = apl(n = 1, d = NA,  h = paste(
   "-load.Rdat RDAT: load data list from save.Rdat for reproducing plot.\n",
   "                 Note that you cannot override the settings in RDAT,\n",
   "                 unless you run in the interactive R mode. For example,\n",
   "                 say you have dice.Rdat saved from a previous command\n",
   "                 and you want to change P$nodisp to TRUE:\n",
   "              load('dice.Rdat'); P$nodisp <- TRUE; plot.1D.eng(P)\n"
                     ) ),
      
      '-save.size' = apl(n = 2, d = c(2000,2000),  h = paste(
   "-save.size width height: Save figure size in pixels\n",
   "                   Default is 2000 2000\n"
                     ) ),
                     
      '-prefix' = apl(n = 1, d = NA,  h = paste(
   "-prefix PREFIX: Output prefix. See also -save. \n"
                     ) ),

      '-title' = apl(n = c(1, Inf), d = NULL,  h = paste(
   "-title TITLE: Graph title. File name is used by default.\n",
   "              Use NONE to be sure no title is used.\n"
                     ) ),
            
      '-grp.label' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-grp.label GROUP1 [GROUP2 ...]: Labels assigned to each group.\n",
   "                         Default is no labeling\n"
                     ) ),
      '-col.plot.char' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.plot.char CHAR1 [CHAR2 ...] : Symbols for each column in -input.\n",
   "                            CHAR? are integers (usually 0-127), or\n",
   "                            characters + - I etc.\n",
   "     See the following link for what CHAR? values you can use:\n",
   "http://stat.ethz.ch/R-manual/R-patched/library/graphics/html/points.html\n"
                     ) ),
                     
      '-col.color' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.color COL1 [COL2 ...]: Colors for each column in -input.\n",
   "                            COL? are integers for now.\n"
                     ) ),
                     
      '-col.line.type' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.line.type LT1 [LT2 ...]: Line type for each column in -input.\n",
   "                            LT? are integers for now.\n"
                     ) ),
                     
      '-col.name' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-col.name NAME1 [NAME2 ...]: Name of each column in -input. \n",
   "       Special flags:\n",
   "            VOLREG: --> 'Roll Pitch Yaw I-S R-L A-P'\n"
                     ) ),

      '-row.name' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-row.name NAME1 [NAME2 ...]: Name of each row in -input. \n",
   "       For the moment, this is only used with -matplot\n"
                     ) ),

      '-rowcol.name' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-rowcol.name NAME1 [NAME2 ...]: Names of rows, same as name of columns.\n",
   "       For the moment, this is only used with -matplot.\n"
                     ) ),

      '-col.name.show' = apl(n = 0, d = FALSE, h = paste(
   "-col.name.show : Show names of column in -input.\n"
                     ) ),

      '-leg.names' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.names : Names to use for items in legend.\n",
   "             Default is taken from column names.\n"
                     ) ),

      '-leg.line.type' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.line.type : Line type to use for items in legend.\n",
   "             Default is taken from column line types.\n",
   "             If you want no line, set -leg.line.type = 0\n"
                     ) ),

      '-leg.line.color' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.line.color : Color to use for items in legend.\n",
   "             Default is taken from column line color.\n"
                     ) ),

      '-leg.plot.char' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.plot.char : plot characters to use for items in legend.\n",
   "             Default is taken from column plot character (-col.plot.char).\n"
                     ) ),

      '-leg.position' = apl(n = c(1,Inf), d = NULL, h = paste(
   "-leg.position : Legend position. Choose from:\n",
   "                bottomright, bottom, bottomleft\n",
   "                left, topleft, top, topright, right,\n", 
   "                and center\n"
                     ) ),
      
      '-leg.fontsize' = apl(n = 1, d = 1.0, h = paste(
   "-leg.fontsize : fontsize for legend text.\n"
                     ) ),

      '-leg.ncol' = apl(n = 1, d = 4, h = paste(
   "-leg.ncol : Number of columns in legend.\n"
                     ) ),
                     
      '-leg.show' = apl(n = 0, d = FALSE, h = paste(
   "-leg.show : Show legend.\n"
                     ) ),
                     
      '-nozeros' = apl (n = 0, d = FALSE, h = paste (
   "-nozeros:  Do not plot all zeros time series\n"
                        ) ),
      '-col.nozeros' = apl (n = 0, d = FALSE, h = paste (
   "-col.nozeros:  Do not plot all zeros columns\n"
                        ) ),
      '-zeros' = apl (n = 0, d = TRUE, h = paste (
   "-zeros:  Do  plot all zeros time series\n"
                        ) ),
                        
      '-col.ystack' = apl (n = 0, d = TRUE, h = paste (
   "-col.ystack:  Scale each column and offset it based on its\n",
   "               column index. This is useful for stacking\n",
   "               a large number of columns on one plot.\n",
   "              It is only carried out when graphing more\n",
   "              than one series with the -one option.\n"
                        ) ),
                              
      '-oneplot' = apl (n = 0, d = FALSE, h = paste (
   "-oneplot:  Put all columns on one graph\n"
                        ) ),   
                        
      '-multiplot' = apl (n = 0, d = FALSE, h = paste (
   "-multiplot:  Put columns in separate graphs\n"
                        ) ),   
                        
      '-matplot' = apl (n = 0, d = FALSE, h = paste (
   "-matplot:  Display as matrix\n"
                        ) ),   
                        
      '-one' = apl (n = 0, d = FALSE, h = paste (
   "-one:  Put all columns on one graph\n"
                        ) ),  
                                                           
      '-multi' = apl (n = 0, d = FALSE, h = paste (
   "-multi:  Put columns in separate graphs\n"
                        ) ),  

      '-mat' = apl (n = 0, d = FALSE, h = paste (
   "-mat:  Display as matrix\n"
                        ) ),  
                                                           
      '-col.grp' = apl(n = c(1, Inf), h = paste (
   "-col.grp 1Dfile or Rexp: integer labels defining column grouping\n"
                  ) ), 
                  
      '-col.text.lym' = apl(n = c(1, Inf), h = paste (
   "-col.text.lym LYM_TEXT: Text to be placed at left Y margin.\n",
   "                        You need one string per column.\n",
   "        Special Flags: You can also use COL.NAME to use column\n",
   "                        names for the margin text, or you can use\n",
   "                        COL.IND to use the colum's index in the file\n"
                  ) ), 
                  
      '-col.text.rym' = apl(n = c(1, Inf), h = paste (
   "-col.text.rym RYM_TEXT: Text to be placed at right Y margin.\n",
   "                        You need one string per column.\n",
   "       See also Special Flags section under -col.text.lym\n"     
                  ) ), 
                  
      '-col.plot.type' = apl(n = c(1, Inf), h = paste (
   "-col.plot.type PLOT_TYPE: Column plot type. \n",
   "                         'l' for line, 'p' for points, 'b' for both\n"   
                  ) ), 
                  
      '-xax.lim' = apl(n = c(2,3), h = paste (
   "-xax.lim MIN MAX [STEP]: Range of X axis, STEP is optional\n"
                  ) ), 
                  
      '-yax.lim' = apl(n = c(2,3), h = paste (
   "-yax.lim MIN MAX [STEP]: Range of X axis, STEP is optional\n"
                  ) ),  
                  
      '-addavg' = apl (n = 0, d = FALSE, h = paste (
   "-addavg:  Add line at average of column\n"
                        ) ),              
      
      '-xax.label' = apl(n = c(1, Inf), h = paste (
   "-xax.label XLABEL: Label of X axis \n"
                  ) ), 
                  
      '-xax.tic.text' = apl(n = c(1, Inf), h = paste (
   "-xax.tic.text XTTEXT: X tics text\n"
                  ) ), 
                  
      '-yax.label' = apl(n = c(1, Inf), h = paste (
   "-yax.label YLABEL: Label of Y axis\n"
                  ) ), 
                  
      '-yax.tic.text' = apl(n = c(1, Inf), h = paste (
   "-yax.tic.text YTTEXT: Y tics text \n"
                  ) ), 
                  
      '-TR' = apl(n=1, d=0, h=paste(
   "-TR TR: Sampling period, in seconds. \n"
                  ) ),
            
      '-grid.show' = apl(n = 0, d = FALSE, h = paste(
   "-grid.show : Show grid.\n"
                     ) ),
                    
      '-verb' = apl(n=1, d = 0, h = paste(
   "-verb VERB: VERB is an integer specifying verbosity level.\n",
   "            0 for quiet (Default). 1 or more: talkative.\n"
                        ) ),
      '-help' = apl(n=0, h = '-help: this help message\n'),
      
      '-run_examples' = apl(n=0, h = 
   '-run_examples: Run all examples, one after the other.\n'),
   
      '-show_allowed_options' = apl(n=0, h=
   "-show_allowed_options: list of allowed options\n" ),
   
      '-msg.trace' = apl(n=0, h=
   "-msg.trace: Output trace information along with errors and notices\n" )

         );
                     
   ops <- parse.AFNI.args(args,  params,
                          other_ok=TRUE, verb=verb
                          )
   if (verb) show.AFNI.args(ops, verb=verb-1, hstr='');
   if (is.null(ops)) {
      errex.AFNI('Error parsing arguments. See 1dRplot -help for details.');
   }
   
   #Parse dems options
   #initialize with defaults
   lop <- init.1DRplot.lop()

   #Get user's input
   for (i in 1:length(ops)) {
      opname <- strsplit(names(ops)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      
      switch(opname,
             i = lop$dmat <- ops[[i]],
             input = lop$dmat <- ops[[i]],
             input_type = lop$dmat.type <- ops[[i]],
             input_delta = lop$dmat.err <- ops[[i]],
             x = lop$dmat.xval <- parse.1dRplot.colinput(ops[[i]]),
             TR = lop$dmat.TR <- ops[[i]],
             prefix = lop$prefix  <- ops[[i]],
             save = {lop$prefix <- ops[[i]]; lop$nodisp=TRUE;} ,
             save.Rdat = {lop$save.Rdat=TRUE;} ,
             load.Rdat = { reload_mode(ops[[i]]); } ,
             save.size = lop$save.size <- ops[[i]],
             nozeros = lop$col.nozeros <- TRUE,
             col.nozeros = lop$col.nozeros <- TRUE,
             zeros = lop$col.nozeros <- FALSE,
             oneplot = lop$plotmode <- 1,
             one = lop$plotmode <- 1,
             multiplot = lop$plotmode <- 2,
             multi = lop$plotmode <- 2,
             matplot = lop$plotmode <- 3,
             mat = lop$plotmode <- 3,
             col.grp = 
               lop$col.grp <- parse.1dRplot.colinput(ops[[i]]),
             col.ystack = lop$col.ystack <- TRUE,
             grp.label = lop$grp.label <- ops[[i]],
             xax.lim = lop$xax.lim <- ops[[i]],
             yax.lim = lop$yax.lim <- ops[[i]],
             addavg = lop$col.mean.line <- TRUE,
             title  = lop$ttl.main <- paste(ops[[i]], collapse=' '),
             xax.label  = lop$xax.label <- paste(ops[[i]], collapse=' '),
             xax.tic.text = lop$xax.tic.text <- ops[[i]],
             yax.label  = lop$yax.label <- paste(ops[[i]], collapse=' '),
             yax.tic.text = lop$yax.tic.text <- ops[[i]],
             verb = lop$verb <- ops[[i]],
             col.plot.char = 
                        lop$col.plot.char <- parse.1dRplot.colinput(ops[[i]]),
             col.color = lop$col.color <- parse.1dRplot.colinput(ops[[i]]),
             col.line.type = lop$col.line.type <- 
                                    parse.1dRplot.colinput(ops[[i]]),
             col.plot.type = lop$col.plot.type <- 
                                    parse.1dRplot.colinput(ops[[i]]),
             col.name = lop$col.name <- ops[[i]],
             col.name.show = lop$col.name.show <- TRUE,
             row.name = lop$row.name <- ops[[i]],
             rowcol.name = {lop$row.name <- ops[[i]]; lop$col.name <- ops[[i]];},
             col.text.lym = lop$col.text.lym <- parse.1dRplot.colinput(ops[[i]]),
             col.text.rym = lop$col.text.rym <- parse.1dRplot.colinput(ops[[i]]),
             leg.show = lop$leg.show <- TRUE,
             leg.names = lop$leg.names <- ops[[i]],
             leg.line.type = lop$leg.line.type <- ops[[i]],
             leg.line.color = lop$leg.line.color <- ops[[i]],
             leg.plot.char = lop$leg.plot.char <- ops[[i]],
             leg.position = lop$leg.position <- ops[[i]],
             leg.fontsize = lop$leg.fontsize <- ops[[i]],
             leg.ncol = lop$leg.ncol <- ops[[i]],
             grid.show = lop$grid.show <- TRUE,
             help = help.1dRplot.opts(params, adieu=TRUE),
             msg.trace = set.AFNI.msg.trace(TRUE),
             show_allowed_options = show.AFNI.args(ops, verb=0, 
                                              hstr="1dRplot's",adieu=TRUE),
             run_examples = run.1dRplot.examples(),
             other = lop <- parse.1dRplot.other(ops[[i]], lop),
             allowed_options = {},
             errex.AFNI(paste("Option '", opname,"' not recognized", sep=''))  
             )
   }


   return(lop)
}# end of read.1dRplot.opts.batch

#Change options list to 1dRplot variable list 
process.1dRplot.opts <- function (lop, verb = 0) {
   return(lop)
}


reload_mode <- function (rdat) {
   load(rdat); 
   thisplot = plot.1D.eng(P);
   if (BATCH_MODE) { #do not quit until device is closed
      while (length(which(dev.list()==thisplot))) Sys.sleep(0.25);
   }
   exit.AFNI();
}  


#################################################################################
########################## Begin 1dRplot main ###################################
#################################################################################


   if (!exists('.DBG_args')) { 
      args = (commandArgs(TRUE))  
      rfile <- first.in.path(sprintf('%s.R',ExecName))  
      # save only on -dbg_args          28 Apr 2016 [rickr]
      if ( '-dbg_args' %in% args ) {
         save(args, rfile, file="1dRplot.dbg.AFNI.args", ascii = TRUE) 
      }
   } else {
      note.AFNI("Using .DBG_args resident in workspace");
      args <- .DBG_args
   }
   if (!length(args)) {
      BATCH_MODE <<- 0
      err.AFNI("No parameters");
   } else {
      if (!exists('.DBG_args')) {
         BATCH_MODE <<- 1
      } else {
         BATCH_MODE <<- 0
      }  
      if (is.null(lop <- read.1dRplot.opts.batch(args, verb = 0))) {
         stop('Error parsing input');
      }
      if (is.null(lop$verb) || is.na(lop$verb)) lop$verb <- 0
      #str(lop);
      if (is.null(lop <- process.1dRplot.opts(lop, verb = lop$verb))) {
         stop('Error processing input');
      }
   }
   
   if (lop$verb) { 
      str(lop);
   }      

   thisplot <- plot.1D( dmat = lop$dmat, dmat.err=lop$dmat.err,
            dmat.colsel = lop$dmat.colsel, dmat.xval = lop$dmat.xval,
            dmat.TR = lop$dmat.TR, dmat.type = lop$dmat.type,
            col.nozeros = lop$col.nozeros, 
            col.grp = lop$col.grp,
            col.ystack = lop$col.ystack,
            grp.label = lop$grp.label,
            ttl.main = lop$ttl.main, 
            prefix = lop$prefix,
            save.Rdat = lop$save.Rdat, 
            img.width = lop$save.size[1],
            img.height = lop$save.size[2],
            nodisp = lop$nodisp,
            plotmode = lop$plotmode,
            col.mean.line = lop$col.mean.line,
            xax.lim=lop$xax.lim, xax.tic.text = lop$xax.tic.text,
            yax.lim=lop$yax.lim, yax.tic.text = lop$yax.tic.text,
            xax.label=lop$xax.label,
            yax.label=lop$yax.label, 
            col.plot.char=lop$col.plot.char,
            col.color = lop$col.color,
            col.line.type = lop$col.line.type,
            col.plot.type = lop$col.plot.type,
            col.name = lop$col.name,
            row.name = lop$row.name,
            col.name.show = lop$col.name.show,
            col.text.lym = lop$col.text.lym,
            col.text.rym = lop$col.text.rym,
            leg.show = lop$leg.show,
            leg.names = lop$leg.names,
            leg.line.type = lop$leg.line.type,
            leg.line.color = lop$leg.line.color,
            leg.plot.char = lop$leg.plot.char,
            leg.position = lop$leg.position,
            leg.fontsize = lop$leg.fontsize,
            leg.ncol = lop$leg.ncol,
            grid.show = lop$grid.show,
            verb=lop$verb)

   if (BATCH_MODE) { #do not quit until device is closed
      while (length(which(dev.list()==thisplot))) Sys.sleep(0.25);
   }
   
