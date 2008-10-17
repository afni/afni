#!/usr/bin/env AFNI_Batch_R
#iplots are quite cool, 
#  but when it comes to plotting multiple time series (say 576 x 42) 
#  it gets reaaaaal slow. JAVA SUCKS.
#tkrplot has the stupid feature of not allowing autoresize of the plot.
#  you can do it manually by setting hscale and vscale, but I'd rather
#  play with a copperhead instead

require('graphics')
require('tcltk')

expand_1D_string <- function (t) {
   vvf = vector(length = 0, mode="numeric")
   #replace .. with : and split at ,
   s = strsplit(sub("..",":",t, fixed=TRUE), ",")[[1]]

   #Now loop and form vector of components
   for (si in s) {
      #cat ("working ", si, "\n")
      if (length(grep(":",si))) {
         vv = eval(parse(text=si))
      } else if (length(grep("@",si))) {
         ssi = as.numeric(strsplit(si,"@")[[1]])
         #cat ("ssi = ", ssi, "\n")
         vv = rep(ssi[2], ssi[1])
      } else {
         vv = as.numeric(si)
      }
      #cat(si," = ",vv, "\n")
      vvf <- c(vvf, vv)
      #cat("vvnow = ",vvf, "\n")
   }
   return(vvf)
}


show_xmat <- function (ff, isel=1:1:ncol(ff), descr="") {
   #cat ('isel in show_xmat')
   #show(isel)
   #Set colors based on group and offset by 2 so that -1 and 0 get colored
   colvec <- cg <- attr(ff,'ColumnGroups')
   #first color regressors of no interest
   colvec[cg == -1] = 1
   colvec[cg == 0 ] = 2
   N_basegroup = 2 #2 types of baseline (RONI)
   
   #now color regressors of interest, skip yellow
   collist = c(3,4,5,6,8)  #all but 1st two and yellow
   N_collist = length(collist)
   for (i in min(colvec[colvec > 0]):max(colvec[colvec > 0])) {
      colvec[cg == i] <- collist[(i-N_basegroup) %% N_collist + 1] 
   }
   
   if (length(isel) > 10) {
      tp = 'single'
      #Calculate normalized version for full display
      ffr <- ff[,isel]
      ra = matrix(nrow=ncol(ffr), ncol=3)
      for (i in 1:ncol(ffr)) {
         ra[i,] <- as.vector(quantile(ffr[,i], c(0,1,0.5)))
      }
      offs <-vector(length = ncol(ffr), mode="numeric")
      for (i in 1:ncol(ffr)) {
         ffr[,i] <- (ffr[,i]-ra[i,1])/(ra[i,2]-ra[i,1])+1.4*i
         offs[i] = mean(ffr[,i])
      }
      if (is.null(dev.list())) x11()
      plot(ffr, plot.type = tp,
           xy.labels = colnames(ff[,isel]), xy.lines = TRUE, 
           panel = lines, nc = 3, yax.flip = FALSE,
           axes = TRUE, col = colvec[isel], main = '')
      xoff = ((1:length(isel))%%5)*(length(ff[,1])/frequency(ff)/5)
      text (xoff, offs, colnames(ff[,isel]), col = colvec[isel], adj=c(0,0))
   } else {
      tp = 'multiple'
      if (is.null(dev.list())) x11()
      if (length(isel) == 1) {
         colm <- colvec[isel]
      } else {
         colm <- 1   #ARRRRGH only one col is used for multiple line drawings!
      }
      plot(ff[,isel], plot.type = tp,
           xy.labels = colnames(ff[,isel]), xy.lines = TRUE, 
           panel = lines, nc = 1, yax.flip = FALSE,
           axes = TRUE, col = colm, main = '') 
   }
   
   cat           ( 'Xmat ', attr(ff,'FileName'), '\n',
                  'Baseline indices: ', xmat_base_index(ff)-1, '\n',
                  'Motion indices: ', xmat_motion_index(ff)-1, '\n',
                  'Task indices: ', xmat_alltasks_index(ff)-1, '\n');
   if (length(isel)) {
      view_cond <- kappa(ff[,isel], exact=TRUE)
   }else{
      view_cond <- 0
   }
   if (length(xmat_alltasks_index(ff))) {
      stim_cond <- kappa(ff[,xmat_alltasks_index(ff)], exact=TRUE)
   } else {
      stim_cond <- 0
   }  
   all_cond <- kappa(ff, exact=TRUE)
   if (length(xmat_motion_index(ff))) {
      all_cond_no_mot <- kappa(ff[,-xmat_motion_index(ff)], exact=TRUE)
   } else {
      all_cond_no_mot <- 0
   }
   stit = paste ( 'Rall          : ', sprintf('%.2f', all_cond), '\n',
                  'Rall-motion   : ', sprintf('%.2f', all_cond_no_mot), '\n',
                  'Rall-roni     : ', sprintf('%.2f', stim_cond),'\n',
                  'Rviewed       : ', sprintf('%.2f', view_cond),'\n');
   
   #title (paste('Xmat: ', attr(ff,'FileName'),'\n'));
   title ('');
   
   #update report window
   ttc <<- condition_report(ttc, ff, isel, descr=descr)

   return(0)
}

text_window_kill <- function() {
   tkdestroy(ttc)
   ttc <<-NA
}

condition_report <- function(tt=NA, ff, isel=1:1:ncol(ff), descr="") {
   if (is.na(tt)[1]) {
      #cat ('Have no widget\n')
      tt  <- tktoplevel()
      #Catch the destroy button to reset ttc to NA 
      tcl(  "wm", "protocol", tt, 
            "WM_DELETE_WINDOW", function()text_window_kill())
      scr <- tkscrollbar(tt, repeatinterval=5,
                         command=function(...)tkyview(txt,...))
      #make sure txt is being kept for next call
      txt <<- tktext(  tt,bg="white",font="courier",
                      yscrollcommand=function(...)tkset(scr,...))
      tkgrid(txt,scr)
      tkgrid.configure(scr,sticky="ns")
      
      #Numbers that will stay constant
      if (length(xmat_alltasks_index(ff))) {
         stim_cond <- kappa(ff[,xmat_alltasks_index(ff)], exact=TRUE)
      } else {
         stim_cond <-  0 
      }
      all_cond <- kappa(ff, exact=TRUE)
      if (length(xmat_motion_index(ff))) {
         all_cond_no_mot <- kappa(ff[,-xmat_motion_index(ff)], exact=TRUE)
      } else {
         all_cond_no_mot <- all_cond
      }
      stitcom = paste ( 
                  'Matrix file: ', attr(ff,'FileName'), '\n',
                  '* Baseline indices: ', paste(xmat_base_index(ff)-1,
                                             collapse=", "), '\n',
                  '* Motion indices: ', paste(xmat_motion_index(ff)-1,
                                             collapse=", "), '\n',
                  '* Task indices: ', paste(xmat_alltasks_index(ff)-1,
                                             collapse=", "), '\n',
                  '* Task Labels: ', paste( attr(ff,'TaskNames'),
                                          collapse = ", "),'\n',
                  '* Standard condition numbers:', '\n',
                  '   Rall          : ', sprintf('%.2f', all_cond), '\n',
                  '   Rall-motion   : ', sprintf('%.2f', all_cond_no_mot), '\n',
                  '   Rall-roni     : ', sprintf('%.2f', stim_cond),'\n',
                  '=============================================\n');
   } else {
      #cat ('reusing widget\n')
      stitcom = paste ('User Selection:', paste(descr, collapse=" ") , '\n')
   }
   
   #cat ('Calculating conditions\n')
   if (length(isel)) {
      view_cond <- kappa(ff[,isel], exact=TRUE)
   } else {
      view_cond  = 0
   }
   stit = paste ( stitcom,
                  '   Rviewed       : ', sprintf('%.2f', view_cond),'\n',
                  '----------------------------------------------\n');
   #cat ('inserting string\n')
   tkconfigure(txt, state="normal") #allow editing
   tkinsert(txt,"end",stit)
   tkconfigure(txt, state="disabled") #no more editing 
   tkfocus(txt)
   return(tt)
}

xmat_base_index <- function (ff) {
   cg <- attr(ff,'ColumnGroups')
   return(which(cg == -1))
}
xmat_motion_index <- function (ff) {
   cg <- attr(ff,'ColumnGroups')
   return(which(cg == 0))
}
xmat_roni_index <- function (ff) {
   cg <- attr(ff,'ColumnGroups')
   return(which(cg <= 0))
}
xmat_alltasks_index <- function (ff) {
   cg <- attr(ff,'ColumnGroups')
   return(which(cg > 0))
}


 
sel_gui <- function (tt, ff) {
   require(tcltk)
   Name <- tclVar("")
   entry.Name <-tkentry(tt,width="20",textvariable=Name)
   tkgrid(tklabel(tt,
                  text=paste(  "Enter indices or label identifiers\n",
                               "example: 0:3, ", attr(ff,'TaskNames')[1],", 7 9") ) )
   tkgrid(entry.Name)
   OnOK <- function()
   {
	   #cat ('OnOK\n')
      NameVal <- gsub("[,;\'\"]", " ",tclvalue(Name))
      show_sel(strsplit(NameVal, " ")[[1]],ff)
      tkfocus(tt)
   }
   OnDone <- function()
   {
      tkdestroy(tt)
      exit_flag <<- TRUE
   }
   OnHelp <- function()
   {
      ExamineXmat.help()
   }

   OK.but <-tkbutton(tt,text=" OK ",command=OnOK)
   Done.but <- tkbutton(tt,text=" Done ",command=OnDone)
   Help.but <- tkbutton(tt,text=" Help ",command=OnHelp)
   tkbind(entry.Name, "<Return>", OnOK)
   tkgrid(OK.but, Done.but, Help.but)
   tkfocus(tt)
   exit_flag <<- FALSE
}

ExamineXmat.help <- function() {
   sh <- paste (
'===============================================================\n',
'ExamineXmat Help:\n',
'-----------------\n',
'ExamineXmat.R is an interactive tool to examine a design matrix.\n',   
'The interface consists of 3 windows: Graph, Control, Results.\n',
'\n',
'The Graph window displays the entire design matrix or a subset \n',
'designated by the user.\n',
'The Control window allows users to designate which regressors\n',
'to display.\n',
'The Results window displays properties of the design matrix in \n',
'its entirety and the condition number of the matrix formed \n',
'by the designated (or viewed in the graph) regressors only.\n',
'\n',
'To select regressors, you can use regressor indices or regressor \n',
'labels. For example, say your tasks (and therefore regressors) \n',
'are labeled "house", "face", "random"\n',
'Then to view house and face regressors only, select:\n',
'"house, face" or "house face" or "h fa" etc.\n',
'You can also use regressor indices (start at 0)\n',
'So you can select "0, 5, 10:12" for regressors 0, 5, 10, 11, and 12\n',
'You can also combine strings and integers in the same selection.\n',
'Commas, semicolons, and quotes are ignored.\n',
'\n',
'When viewing more than 10 regressors, the regressors get automatically\n',
'rescaled and displayed on the same axis. In this case, the amplitude\n',
'of the Y axis is of no relevance. In this display mode, baseline \n',
'regressors are black, motion are red, and each task group gets a distinct\n',
'color.\n',
'When 10 or less regressors are displayed simultaneously, each one\n',
'gets its own subplot and the y axis represents the regressor amplitude\n',
'as it is in the design matrix. Also, in this display mode, all regressors\n',
'are drawn in black.\n',
'===============================================================\n',
'\n')
   cat (sh)
   #tkmessageBox(message=sh)     #ugly...
   #use reults output instead
   tkconfigure(txt, state="normal") #allow editing
   tkinsert(txt,"0.0",sh)
   tkconfigure(txt, state="disabled") #no more editing 
   tkfocus(txt)
   #tkmessageBox(message="See text in Results window") 
}  
show_sel <- function(sel, ff) {
   #cat(sel, 'length:' , length(sel), '\n')
   ilst = vector('integer');
   if (length(sel)) {
      for (isel in sel) {
         #cat("processing: ",isel,'\n')
         if (!inherits( e <- try(as.integer(eval(parse(text=isel))),
                                 silent=TRUE),
                        "try-error")) {
            ilst <- append(ilst, 
                           e+1,       
                           after=length(ilst))
         } else {
            #cat("processing as string: ",isel,'\n')
            ilst <- append(ilst, grep(isel, colnames(ff)), after=length(ilst))
         }
      }
   } else {
      ilst = 0:(ncol(ff)-1)
   }

   if (length(ilst)) {
      ilst = unique(ilst[ilst >0 & ilst <ncol(ff)])
      if (length(ilst)) {
         cat ('selection is: ', ilst-1, '\n')
         show_xmat(ff, ilst, descr=sel)
      }
   }
   
}

read_xmat <- function (xmatfile, nheadmax=10) {
   #Get first few lines
   ff <- scan(xmatfile, what = 'character', nmax = nheadmax, sep = '\n')

   #Search for ColumnLabels and split string
   ffs <- strsplit(ff[grep('ColumnLabels', ff)], '=')[[1]][2]
      #remove bad chars and split into labels
      ffs <- gsub("[;\"]","", ffs)
      ffsv <- strsplit(ffs," ")[[1]]
      #some components are blanks to be killed
      labels <- ffsv[which(nchar(ffsv)!=0)]
   #Get the TR
   ffs <- strsplit(ff[grep('RowTR', ff)], '=')[[1]][2]
      #remove bad chars and change to number
      TR = as.double(gsub("[;\" *]","", ffs))

   #Get the ColumnGroups
   ffs <- strsplit(ff[grep('ColumnGroups', ff)], '=')[[1]][2]
   colg = expand_1D_string (gsub("[;\" *]","", ffs))

   #Now load the whole deal, comments are skipped by defaults
   ff <- ts(as.matrix(read.table(xmatfile)), names = labels, deltat = TR)
   
   #Put the column groups in
   #cat ("insterting colg: ", colg, "\n")
   attr(ff,'ColumnGroups') <- colg
   
   #add filename for the record
   attr(ff,'FileName') <- xmatfile
   
   #Get the task names
   llv <- vector(length=0,mode="numeric")
   tcolg <- unique(colg)
   for (i in tcolg) {
      if (i>0) {
         ll <- which(colg == i)
         llv <- c(llv,ll[1])
      }
   }
   tnames <-paste(strsplit(labels[llv],"#0"))
   attr(ff,'TaskNames') <- tnames
   
   return(ff)
}

main_loop <- function (fn) {
   #read file
   ff <- read_xmat(fn)

   #top level widget 
   ttc <<- NA
   
   #Show all of ffr
   
   show_xmat(ff)

   #begin the selection toy, this GUI controls the quitting etc.
   sel_gui(tktoplevel(), ff)

   #dunno what to do for event loop, all I can do is sleep
   #Without this, the batch mode exits immediately
   
   while (!exit_flag) {
      Sys.sleep(0.5)
   }
   return(0)
}

#main   
   #begin windgetting
   fn <- tclvalue(   tkgetOpenFile( filetypes = 
                                    "{{Xmat Files} {.xmat.1D}} {{All files} *}",
                                    title = 'Choose design matrix file'))
   if (nchar(fn)) {
      if (main_loop(fn)) cat ('error')
   } else {
      #tkmessageBox(message="So long.")  
   }
