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
      ra = matrix(nrow=ncol(ff), ncol=3)
      for (i in 1:ncol(ff)) {
         ra[i,] <- as.vector(quantile(ff[,i], c(0,1,0.5)))
      }
      ffr <- ff
      offs <-vector(length = ncol(ff), mode="numeric")
      for (i in 1:ncol(ff)) {
         ffr[,i] <- (ff[,i]-ra[i,1])/(ra[i,2]-ra[i,1])+1.4*i
         offs[i] = mean(ffr[,i])
      }
      if (is.null(dev.list())) x11()
      plot(ffr[,isel], plot.type = tp,
           xy.labels = colnames(ff[,isel]), xy.lines = TRUE, 
           panel = lines, nc = 3, yax.flip = FALSE,
           axes = TRUE, col = colvec[isel])
      text (0, offs, colnames(ff), col = colvec[isel], adj=c(1,0))
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
           axes = TRUE, col = colm) 
   }
   
   cat           ( 'Xmat ', attr(ff,'FileName'), '\n',
                  'Baseline indices: ', xmat_base_index(ff), '\n',
                  'Motion indices: ', xmat_motion_index(ff), '\n',
                  'Task indices: ', xmat_alltasks_index(ff), '\n');
   
   view_cond <- kappa(ff[,isel], exact=TRUE)
   stim_cond <- kappa(ff[,xmat_alltasks_index(ff)], exact=TRUE)
   all_cond <- kappa(ff, exact=TRUE)
   all_cond_no_mot <- kappa(ff[,-xmat_motion_index(ff)], exact=TRUE)
   stit = paste ( 'Rall          : ', sprintf('%.2f', all_cond), '\n',
                  'Rall-motion   : ', sprintf('%.2f', all_cond_no_mot), '\n',
                  'Rall-roni     : ', sprintf('%.2f', stim_cond),'\n',
                  'Rviewed       : ', sprintf('%.2f', view_cond),'\n');
      
   title (paste('Xmat: ', attr(ff,'FileName'),'\n'));
   
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
      stim_cond <- kappa(ff[,xmat_alltasks_index(ff)], exact=TRUE)
      all_cond <- kappa(ff, exact=TRUE)
      all_cond_no_mot <- kappa(ff[,-xmat_motion_index(ff)], exact=TRUE)
      stitcom = paste ( 
                  'Matrix file: ', attr(ff,'FileName'), '\n',
                  '* Baseline indices: ', paste(xmat_base_index(ff),
                                             collapse=", "), '\n',
                  '* Motion indices: ', paste(xmat_motion_index(ff),
                                             collapse=", "), '\n',
                  '* Task indices: ', paste(xmat_alltasks_index(ff),
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
   view_cond <- kappa(ff[,isel], exact=TRUE)
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
                  text=paste(  "Enter indices (start at 1) \n",
                           "or label identifiers\n",
                           "example: 1:4 pos 7 9") ) )
   tkgrid(entry.Name)
   OnOK <- function()
   {
	   #cat ('OnOK\n')
      NameVal <- tclvalue(Name)
      show_sel(strsplit(NameVal, " ")[[1]],ff)
   }
   OnDone <- function()
   {
      tkdestroy(tt)
      exit_flag <<- TRUE
   }

   OK.but <-tkbutton(tt,text=" OK ",command=OnOK)
   Done.but <- tkbutton(tt,text=" Done ",command=OnDone)
   tkbind(entry.Name, "<Return>", OnOK)
   tkgrid(OK.but, Done.but)
   tkfocus(tt)
   exit_flag <<- FALSE
}

show_sel <- function(sel, ff) {
   #cat(sel, 'length:' , length(sel), '\n')
   if (!is.na(sel) && length(sel)) {
      ilst = vector('integer');
      for (isel in sel) {
         #cat("processing: ",isel,'\n')
         if (!inherits( e <- try(as.integer(eval(parse(text=isel))),
                                 silent=TRUE),
                        "try-error")) {
            ilst <- append(ilst, 
                           e,       
                           after=length(ilst))
         } else {
            #cat("processing as string: ",isel,'\n')
            ilst <- append(ilst, grep(isel, colnames(ff)), after=length(ilst))
         }
      }
      if (length(ilst)) {
         ilst = unique(ilst)
         cat ('selection is: ', ilst, '\n')
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
   fn <- tclvalue(tkgetOpenFile(filetypes = "{{Xmat Files} {.xmat.1D}} {{All files} *}"))
   if (nchar(fn)) {
      if (main_loop(fn)) cat ('error')
   } else {
      tkmessageBox(message="So long.")  
   }
