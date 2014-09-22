PLO <<- NULL

set.plot.1D.global.P <- function(P) { PLO <<- P }
get.plot.1D.global.P <- function() { return(PLO) }

#You can't pass P to plot.1D.multifunc, so I resort to
# a temp copy into PLO 
plot.1D.multifunc <- function(x, col, bg, pch, type, ...) {
   
   lines(x=x,col=col[PLO$iplt],bg=bg,
         pch=pch[PLO$iplt], type=type[PLO$iplt],
         ...)

   if (0 && PLO$col.name.show) { 
         #autoffset computations currently used are bad for multiplots.
         #Fix them before enabling
         text (PLO$mat2plt.xoffnames[PLO$iplt], PLO$mat2plt.yoffnames[PLO$iplt], 
               PLO$col.name[PLO$dmat.colsel[PLO$iplt]], 
               col = PLO$col.color[PLO$dmat.colsel[PLO$iplt]], adj=c(0,0))
   }
   
   if (!is.null(PLO$col.text.lym)) {
      if (PLO$col.text.lym.at == 'YOFF') {
         ym.at <- PLO$mat2plt.colmeans
      } else {
         err.AFNI(paste("Bad PLO$col.text.lym.pos"));
         return(0)
      }
      opar <- par();
      par(ps = PLO$col.text.lym.fontsize)  
      mtext(PLO$col.text.lym[PLO$dmat.colsel[PLO$iplt]], 
            side=2, at=ym.at[PLO$iplt], 
            las=2, adj=1)
      par(ps = opar$ps)
   }
   
   if (!is.null(PLO$col.text.rym)) {
      if (PLO$col.text.rym.at == 'YOFF') {
         ym.at <- PLO$mat2plt.colmeans
      } else {
         err.AFNI(paste("Bad PLO$col.text.rym.pos"));
         return(0)
      }
      opar <- par();
      par(ps = PLO$col.text.rym.fontsize)  
      mtext(PLO$col.text.rym[PLO$dmat.colsel[PLO$iplt]], 
            side=4, at=ym.at[PLO$iplt], 
            las=2, adj=0)
      par(ps = opar$ps)
   }
   
   plot.1D.drawmeanlines(PLO, thissel = PLO$iplt)
   
   PLO$iplt <<- PLO$iplt + 1
}

plot1D.colindex <- function (ivec, 
                  collist=c(1:10)   
                  ) {
   N_collist = length(collist)
   return(collist[(ivec) %% N_collist])
}

plot.1D.testmat <- function(nrow = 100, ncol=10) {
   mm <- matrix(nrow=nrow, ncol= ncol)
   for (i in 1:ncol) {
      if (i != 10) mm[,i] <- sin(i*seq(from=0, by=0.1, length.out=nrow))
      else mm[,i] <- rep(0, nrow)
   }
   return(mm)
}


plot.1D.mapcolors <- function (colvec, collist=c(3,4,5,6,8,1,2,7,9,seq(10:20))) {
   cg <- colvec
   ugr <- unique(colvec)
   if (length(ugr) > 1) {
      cnt <- 1
      for (i in ugr) {
         colvec[cg == i] <- cnt
         cnt <- cnt+1
      }
   }
   #now color regressors of interest
   for (i in min(colvec[colvec > 0]):max(colvec[colvec > 0])) {
      colvec[cg == i] <- plot1D.colindex(i, collist)
   }
   return(colvec)
}

plot.1D.setmat2plt <- function (dmat, isel, stack) {
   mat2plt <- as.matrix(dmat[,isel])
   #Calculate column range
   ra = matrix(nrow=ncol(mat2plt), ncol=3)
   for (i in 1:ncol(mat2plt)) {
      ra[i,] <- as.vector(quantile(mat2plt[,i], c(0,1,0.5), na.rm='TRUE'))
   }
   for (i in 1:ncol(mat2plt)) {
      if (stack[i]) {
         mat2plt[,i] <- (mat2plt[,i]-ra[i,1])/(ra[i,2]-ra[i,1])+1.4*i
      } 
      mat2plt[is.nan(mat2plt[,i]),i] <- 0
   }
   return(mat2plt)
}

plot.1D.colmean <- function (mat2plt) {
   colmeans <- vector(length = ncol(mat2plt), mode="numeric")
   for (i in 1:ncol(mat2plt)) colmeans[i] = mean(mat2plt[,i])
   return(colmeans) 
}

plot.1D.colorOFgroups <- function(col.grp, col.color) {
   grp <- unique(col.grp);
   col <- vector(length = length(grp));
   for (gg in 1:length(col)) {
      kk <- which(col.grp==grp[gg])
      if (length(kk) < 1) {
         err.AFNI("COuld not find color of group");
         return(0)
      }
      col[gg] <- col.color[kk[1]]
   }
   return(col)
}

#When you add to this one, add to
#examples.1dRplot
plot.1D.demo.str <- function(demo=0) {
   if (demo == 0) {
      err.AFNI("demo not set")
      return('')
   }  
   if (demo==1) {
      s <- "plot.1D( dmat = plot.1D.testmat(100, 10), 
               col.nozeros=TRUE, 
               col.grp=c(rep(1,3), rep(2,3), rep(3,4)), 
               col.ystack=FALSE, grp.label=c('CSF', 'GM','WM'),
               prefix = 't1.jpg', verb = 1)"
   } else if (demo==2){
      s <- "plot.1D( dmat = plot.1D.testmat(100, 10), 
               col.nozeros=TRUE, 
               col.grp=c(rep(1,3), rep(2,3), rep(3,4)), 
               col.ystack=FALSE, multi.ncol = 2, 
               grp.label=c('CSF', 'GM','WM'),
               prefix = 't2.jpg', verb = 1)"
   } else if (demo==3) {
      s <- "plot.1D( dmat = plot.1D.testmat(100, 3), 
               col.nozeros=TRUE, 
               col.grp=c(rep(1,2), rep(2,1)), 
               col.ystack=FALSE, grp.label=c('CSF', 'GM'),
               plotmode=1,
               prefix = 't3.jpg', verb = 1)"
   }else if (demo==4) {
      s <- "plot.1D( dmat = plot.1D.testmat(100, 3), 
               col.nozeros=TRUE, 
               col.grp=c(rep(1,2), rep(2,1)), 
               col.ystack=FALSE, grp.label=c('CSF', 'GM'),
               plotmode=1, leg.show = TRUE,
               prefix = 't4.jpg', verb = 1)"
   }else {
      err.AFNI("No such demo")
      return('')
   }
   
   
   return(s)
   
}

plot.1D.demo <- function(demo=0) {
   if (demo == 0) demo <- seq(1,4)
   for (d in demo) {
      s <- plot.1D.demo.str(demo=d)
      eval(parse(text=s))
      if (d!=demo[length(demo)]) {
         if (prompt.AFNI("Waiting for your desire",c('g','s')) == 2) return()
      }
   }
   return()
}



is.good.dev <- function (dd=NULL) {
   if (is.null(dd) || dd == 0) return(FALSE)
   if (length(which(dev.list()==dd))) return(TRUE)
   return(FALSE)
}

plot.1D.setupdevice <- function (P) {
   if (is.null(P$prefix) && P$nodisp) {
      err.AFNI("Nothing to do");
      return(0)
   }
   if (is.null(P$img.width)) { P$img.width<- 2000 }
   if (is.null(P$img.height)) { P$img.height<- 2000 }
   if (is.null(P$udev)) { 
      P$udev <- NULL
      if (is.null(P$udev)) {
         ccpp <- capabilities(what='aqua')
         if (length(ccpp) == 1 && ccpp) P$udev <- 'quartz'
      }
      if (is.null(P$udev)) {
         ccpp <- capabilities(what='X11')
         if (length(ccpp) == 1 && ccpp) P$udev <- 'X11'
      }
      if (is.null(P$udev)) {
         warning.AFNI("Setting device to pdf, no interactive ones found.");
         if (is.null(P$prefix)) {
            P$prefix <- 'AFNI_plot_bailing_out.pdf'
         }
         P$nodisp <- TRUE
      }
   }

   #Note, there are other ways to control graph size, see 
   #?par for pin and fin
   if (!is.null(P$prefix) && P$nodisp) { #render to device directly
      pp <- parse.name(P$prefix)
      if (tolower(pp$ext) == '.jpg') 
         jpeg(P$prefix, width=P$img.width, height=P$img.height, 
                  quality=P$img.qual, 
                  res=P$img.dpi, pointsize=P$img.def.fontsize)
      else if (tolower(pp$ext) == '.png') 
         png(P$prefix, width=P$img.width, height=P$img.height,
                  res=P$img.dpi, pointsize=P$img.def.fontsize)
      else if (tolower(pp$ext) == '.pdf') {
         pdf(P$prefix, pointsize=P$img.def.fontsize)
      } else {
        if (0) {
         pdf(paste(P$prefix,'.pdf',sep='')) #best, but perhaps not always present       
        } else {
         jpeg(  paste(P$prefix,'.jpg',sep=''), 
               width=P$img.width, height=P$img.height, quality=P$img.qual, 
               res=P$img.dpi, pointsize=P$img.def.fontsize)   
        }
      }
   } else {
      if (is.good.dev(P$dev.this)) {
         dev.set(P$dev.this)
      } else if (P$dev.new) {
         #give me new one
         if (P$udev == 'X11') x11()
         else if (P$udev == 'quartz') quartz()
         else {
            warn.AFNI("No proper device selected");
            x11()
         }
      } else {
         #Only get new if list is empty
         if (is.null(dev.list())) {
            if (P$udev == 'X11') x11()
            else if (P$udev == 'quartz') quartz()
            else {
               warn.AFNI("No proper device selected");
               x11()
            }   
         }
      }
   }
   return(dev.cur())
}   

plot.1D.unsetupdevice <- function(P) {
   #Might want to save and restore current dev...
   if (is.good.dev(P$dev.this)) dev.set(P$dev.this)
   thisplot <- dev.cur()
   if (!is.null(P$prefix)) {
      if (!P$nodisp) {  #Need a copy
         pp <- parse.name(P$prefix)
         if (tolower(pp$ext) == '.jpg') dev.copy(jpeg,P$prefix)
         else if (tolower(pp$ext) == '.png') dev.copy(png,P$prefix)
         else if (tolower(pp$ext) == '.pdf') dev.copy(pdf,P$prefix)
         else dev.copy(pdf,paste(P$prefix,'.pdf',sep=''))
         dev.off()
      } else { #rendered directly to image
         dev.off()
         thisplot <- 0
      }
   }
   return(thisplot)
}

plot.1D.save <- function (prefix='plot.pdf', dev=NULL) {
   if (!is.good.dev(dev)) {
      dev = dev.cur()
   }
   if (!is.good.dev(dev)) {
      err.AFNI("NO good device");
      return(0)
   }
   #cat('Working dev ', dev, '\n');
   pp <- parse.name(prefix)
   if (tolower(pp$ext) == '.jpg') dev.copy(jpeg,prefix)
   else if (tolower(pp$ext) == '.png') dev.copy(png,prefix)
   else if (tolower(pp$ext) == '.pdf') dev.copy(pdf,prefix)
   else {
      prefix <- paste(prefix,'.pdf',sep='')
      dev.copy(pdf,prefix)
   }
   dev.off()
   if (!file.exists(prefix)) {
      err.AFNI(paste("Failed to write", prefix))
      return(0)
   }
   return(1)
}

plot.1D.puttitle <- function (P) {   
   if (!(is.null(P$ttl.main) || P$ttl.main == 'NONE') || 
       !(is.null(P$ttl.sub) || P$ttl.sub == 'NONE')) {
      opar <- par();
      par(font.main = P$ttl.main.fontsize)
      par(font.sub = P$ttl.sub.fontsize) 
      title(main=P$ttl.main, sub=P$ttl.sub)
      par(font.main = opar$font.main)
      par(font.sub = opar$font.sub)
   }
}   

plot.1D.drawmeanlines <- function (P, thissel=NULL, addval=TRUE) {
      if (is.null(thissel)) selv <- 1:length(P$col.mean.line[P$dmat.colsel])
      else selv <- thissel
      for (i in selv) {
         if (P$col.mean.line[P$dmat.colsel[i]]) {
               #note.AFNI(paste ("Called ", P$mat2plt.colmeans[i]));
               vv <- rep(P$mat2plt.colmeans[i], dim(P$mat2plt)[1])
               lines(x=P$dmat.xval, y=vv, 
                  col = P$col.color[P$dmat.colsel[i]],  lty=3)
               if (addval) {
                  it <- max(1,length(P$dmat.xval)-2)
                  text( x=P$dmat.xval[it],
                        y=vv[it], sprintf('%.2f',vv[it]),
                        adj=c(1,0))
               }
         }
      }
      return()
}
 
#If you pass no parameters to this function
#You get a list that has all the elements of lldef
#but with NA values everywhere
#
#If you pass parameters, they replace their namesakes in
#the returned list and the uninitialized ones get the defaults
#from lldef
plot.1D.optlist <- function(...) {
   #list of all variables in default value
   #
   lldef <- list(dmat=NULL, dmat.err=NULL, dmat.colsel=NULL, 
            dmat.xval=NULL, dmat.TR = NULL, dmat.type = NULL,
            col.grp = NULL, col.ystack=FALSE, col.nozeros=FALSE, 
            col.name=NULL, col.name.show=FALSE, 
            col.name.x=NULL, col.name.y=NULL,
            row.name=NULL,
            col.color = NULL, col.plot.char=NULL,
            col.plot.type ='l', col.line.type = 1, col.line.width=3,
            col.mean.line=FALSE,
            grp.label=NULL,
            ttl.main=NULL, ttl.main.fontsize = 10,
            ttl.sub=NULL, ttl.sub.fontsize = 10,
            prefix = NULL, showval=FALSE, save.Rdat=FALSE,
            nodisp = FALSE, plotmode = 2, boxtype = 'n', multi.ncol=2,
            NAval = 0, NANval = 0,
            colorset = seq(1,20),
            xax.lim=NULL, xax.step = NULL, xax.label=NULL, xax.tic.text = NULL, 
            yax.lim=NULL, yax.step = NULL, yax.label=NULL, yax.tic.text = NULL,
            leg.show=FALSE, leg.ncol = 4, leg.names=NULL, leg.line.type = NULL,
            leg.line.color = NULL, leg.plot.char = NULL, 
            leg.position="topright", leg.fontsize = 12,
            grid.show=FALSE, 
            col.text.lym = NULL, col.text.lym.at = 'YOFF', 
            col.text.lym.fontsize = 10, 
            col.text.rym = NULL, col.text.rym.at = 'YOFF', 
            col.text.rym.fontsize = 10, 
            img.width=2000, img.height=2000, img.qual = 98, 
            img.dpi = 300, img.def.fontsize=12,
            dev.this=NULL, dev.new=FALSE,
            showcond = FALSE, udev = NULL,
            verb = 0);
   
   #Same list, but all flagged with NA as not user initialized
   ll <- lldef; ll[1:length(ll)] <- NA
   
  if( length(list(...)) ){
      #Add user specifics
      up <- list(...)
      for (iu in 1:length(up)) {
         wiu <- which(names(ll) == names(up)[iu])
         if (length(wiu)) ll[wiu] <- up[iu]
         else {
            warn.AFNI(paste("User variable '", names(up)[iu] ,
                            "' not good for plot variables.\n",
                            "All variables must be named.",
                            sep = ''))
         }
      }
      #If the user specified dmat, read it now, it won't get read again
      if (!is.null(ll$dmat) && is.character(ll$dmat)) {
         dmatv <- ll$dmat
         if (!is.null(ll$ttl.main) && is.na(ll$ttl.main)) 
                     ll$ttl.main<-paste(ll$dmat, collapse='\n')
         for (i in 1:length(dmatv)) { #Won't work for different row numbers...
            if (is.null(dmatc <- read.AFNI.matrix(dmatv[i]))) {
               err.AFNI(sprintf("Failed to read file %s", dmatv[i]))
               return(0) 
            }
            #str(dmatc)
            if (i==1) { 
               ll$dmat<-dmatc; 
            }else {
               if (dim(dmatc)[1] < dim(ll$dmat)[1]) {
                  note.AFNI("Padding dmatc with NA to match nrows in dmat")
                  dmatna <- matrix(NA, dim(ll$dmat)[1], dim(dmatc)[2])
                  dmatna[1:dim(dmatc)[1],1:dim(dmatc)[2]] <- dmatc
                  dmatc <- dmatna
               }
               if (nrow(ll$dmat) !=nrow(dmatc)) {
                  err.AFNI(
                     paste("Don't know what to do about catting matrices.\n",
                           "Currently have ",nrow(ll$dmat), 
                           " rows and trying to",
                           "append ", nrow(dmatc), 
                           " rows from", dmatv[i]))
                  return(0)
               }
               ll$dmat <- cbind(ll$dmat, dmatc)
            }
         }
      }
      
      #Do we need to load xval ? 
      if (!is.null(ll$dmat.xval) && 
           is.character(ll$dmat.xval) && ll$dmat.xval != "ENUM") {
         ff <- ll$dmat.xval
         if (is.null(ll$dmat.xval <- read.AFNI.matrix(ff))) {
            err.AFNI(sprintf("Failed to read X file %s", ff))
            return(0) 
         }
      }
      
      #Now, based on dmat.type, do some setup without overririding user's whishes
      if (!is.na(ll$dmat.type)) {
         if (ll$dmat.type == 'VOLREG') {
            if (!is.null(ll$col.name) && is.na(ll$col.name)) 
               ll$col.name <- c('Roll', 'Pitch', 'Yaw', 'I-S', 'R-L', 'A-P')
            if (!is.null(ll$multi.ncol) && is.na(ll$multi.ncol)) 
               ll$multi.ncol <- 1
         }
         if (ll$dmat.type == 'XMAT') {
            if (!is.null(ll$multi.ncol) && is.na(ll$multi.ncol)) 
               ll$multi.ncol <- 1
            if (!is.null(ll$col.ystack) && is.na(ll$col.ystack)) 
               ll$col.ystack <- TRUE
            if (!is.null(ll$plotmode) && is.na(ll$plotmode)) 
               ll$plotmode <- 1
            if (!is.null(ll$col.text.lym) && is.na(ll$col.text.lym)) 
               ll$col.text.lym <- 'COL.NAME'
            if (!is.null(ll$col.text.rym) && is.na(ll$col.text.rym)) 
               ll$col.text.rym <- 'COL.IND'
         }
      } else { #Try information from dmat's attributes
        if (!is.null(nm <- attr(ll$dmat,"name"))) {
           if (attr(ll$dmat,"name") == '3dhistog' ) {
              if (dim(ll$dmat)[2] == 3) { 
                  #Don't go here if users send in a partial file
                  #like hist.1D[1,2]
                 if (!is.null(ll$dmat.xval) && is.na(ll$dmat.xval)) 
                     ll$dmat.xval <- ll$dmat[,1]
                 if (!is.null(ll$dmat.colsel) && is.na(ll$dmat.colsel)) 
                     ll$dmat.colsel <- c(2,3)
                 if (!is.null(ll$xax.label) && is.na(ll$xax.label)) 
                     ll$xax.label <- colnames(ll$dmat)[1]
              }
           }
        }
        if (!is.null(nm <- attr(ll$dmat,"name"))) {
           if (attr(ll$dmat,"name") == 'seg_histogram' ) {
              if (dim(ll$dmat)[2] == 3) { 
                  #Don't go here if users send in a partial file
                  #like hist.1D[1,2]
                 if (!is.null(ll$dmat.xval) && is.na(ll$dmat.xval)) 
                     ll$dmat.xval <- ll$dmat[,1]
                 if (!is.null(ll$dmat.colsel) && is.na(ll$dmat.colsel)) 
                     ll$dmat.colsel <- c(3) #Don't bother with non-normalized
                 if (!is.null(ll$xax.label) && is.na(ll$xax.label)) 
                     bw <- attr(ll$dmat,"BinWidth")
                     if (is.null(bw)) bw <- 0 
                    if (!is.null(attr(ll$dmat,"xlabel"))) {
                        ll$xax.label <- sprintf("%s (K=%d,W=%.3f)",
                                          attr(ll$dmat,"xlabel"),
                                          dim(ll$dmat)[1], bw)
                    } else {
                       ll$xax.label <- sprintf("%s (K=%d,W=%.3f)",
                                               colnames(ll$dmat)[1],
                                                dim(ll$dmat)[1], bw)
                    }
              }
           }
        } 
        if (!is.null(nm <- attr(ll$dmat,"name"))) {
           if (attr(ll$dmat,"name") == '3ddot' ||
               length(grep('CorrMat',attr(ll$dmat,"name"))) ) {
              if (dim(ll$dmat)[2] == dim(ll$dmat)[1] ) { 
                 if (!is.null(ll$plotmode) && is.na(ll$plotmode)) 
                    ll$plotmode <- 3
                 if (!is.null(ll$row.name) && is.na(ll$row.name))
                    ll$row.name <- colnames(ll$dmat)
              }
           }
        }
         
           
      }
      #Now, apply all defaults for what remains uninitialized
      for (i in 1:length(ll)) 
         if (is.na(ll[i])) ll[i] <- lldef[i]
      
   } else {
      #Returning with all set as NA
   }
   
   
   return(ll)
}

plot.1D.freq <- function(P) {
   freq <- 1
   if (!is.null(P$dmat.TR) && P$dmat.TR) freq <- 1/P$dmat.TR
   else if (!is.null(nn <- attr(P$dmat,"name"))) {
      if (nn == '3dhistog') {
         if (!is.null(bw <-attr(P$dmat,"BinWidth"))) {
            freq <- 1/bw
         }
      }
   }
   return(freq)
}      

#see plot.1D.optlist for allowed options
plot.1D <- function (...) {
   if (length(list(...)) == 0) {
      note.AFNI("Plot.1D in hardwired test mode")
      plot.1D.demo()
      return(1)
   }
   P <- plot.1D.optlist(...)
   #Set some more variables
   P <- c(P, iplt=1, mat2plt=NULL, mat2plt.colmeans=NULL,  
                  mat2plt.minus=NULL, mat2plt.plus=NULL, 
                  mat2plt.xoffnames=NULL, mat2plt.yoffnames=NULL )
   
   if (P$verb) {
      note.AFNI("P list before calling plot.1D.eng");
      str(P)
   }
   return(plot.1D.eng(P))
}

plot.1D.eng <- function (P) {
   thisplot <- NULL
   #Load DATA
   if (is.null(P$dmat)) {
      err.AFNI("NULL dmat");
      return(0)  
   } else if (is.character(P$dmat)) {
      dmatv <- P$dmat
      if (is.null(P$ttl.main)) P$ttl.main<-paste(P$dmat, collapse='\n')
      for (i in 1:length(dmatv)) { #Won't work for different row numbers...
         if (is.null(dmatc <- read.AFNI.matrix(dmatv[i]))) {
            err.AFNI("Failed to read test file")
            return(0) 
         }
         #str(dmatc)
         if (i==1) { 
            P$dmat<-dmatc; 
         }else {
            if (dim(dmatc)[1] < dim(P$dmat)[1]) {
               note.AFNI("Padding dmatc with NA to match nrows in dmat")
               dmatna <- matrix(NA, dim(P$dmat)[1], dim(dmatc)[2])
               dmatna[1:dim(dmatc)[1],1:dim(dmatc)[2]] <- dmatc
               dmatc <- dmatna
            }
            if (nrow(P$dmat) !=nrow(dmatc)) {
               err.AFNI(paste("Don't know what to do about catting matrices.\n",
                        "Currently have ",nrow(P$dmat), " rows and trying to",
                        "append ", nrow(dmatc), 
                        " rows from", dmatv[i]))
               return(0)
            }
            P$dmat <- cbind(P$dmat, dmatc)
         }
      }
   }
   
   if (is.null(P$dmat)) {
      err.AFNI("Null input");
      return(0)
   }
   
   #SET TR 
   if (is.null(P$dmat.TR)) {
      P$dmat.TR <- attr(P$dmat,"TR")
   }
   if (is.null(P$dmat.TR)) {
      P$dmat.TR <- 0 
   }
    
   #Load the error matrix
   if (!is.null(P$dmat.err) && is.character(P$dmat.err)) {
      dmatv <- P$dmat.err
      for (i in 1:length(dmatv)) { #Won't work for different row numbers...
         if (is.null(ffc <- read.AFNI.matrix(dmatv[i]))) {
            err.AFNI("Failed to read test file")
            return(0) 
         }
         #str(ffc)
         if (i==1) { 
            P$dmat.err<-ffc; 
         }else {
            if (dim(ffc)[1] < dim(P$dmat.err)[1]) {
               ffna <- matrix(NA, dim(P$dmat.err)[1], dim(ffc)[2])
               ffna[1:dim(ffc)[1],1:dim(ffc)[2]] <- ffc
               ffc <- ffna
            }
            P$dmat.err <- cbind(P$dmat.err, ffc)
         }
      }
   }
   
   if (!is.null(P$dmat.err)) {
      if (!is.null(P$dmat.err) && P$col.ystack) {
         err.AFNI("Error bars with P$col.ystack?");
         return(0);
      }
      if (dim(P$dmat.err)[1] != dim(P$dmat)[1] ||
          dim(P$dmat.err)[2] != dim(P$dmat)[2]) {
         err.AFNI("Dimensions mismatch between P$dmat.err and dmat");
         return(0)   
      }
   }   
   
   #Check for proper input
   if (!is.null(P$col.grp)) {
      if (length(P$col.grp) != ncol(P$dmat)) {
         if (length(P$col.grp) == 1) P$col.grp <- rep(P$col.grp, ncol(P$dmat))
         else {
            err.AFNI(
               paste("P$col.grp must either have one or",ncol(P$dmat),"values",
                     "Have ", length(P$col.grp)));
            return(0);
         }
      }
   } else {
      #Try from the attributes, else all is 1
      if (is.null(P$col.grp <- attr(P$dmat,'ColumnGroups'))) 
         P$col.grp <- rep(1,ncol(P$dmat))
      
   }
   
   #Check for P$grp.label
   if (!is.null(P$grp.label)) {
      if (is.null(P$col.grp)) {
         err.AFNI("Have P$grp.label, but no P$col.grp");
         return(0)
      }
      if (length(P$grp.label) < max(unique(P$col.grp))) {
         err.AFNI(paste("Have ", length(P$grp.label), 
                        "group labels and a reference to grp ",
                        max(unique(P$col.grp))));
         return(0)
      }
   } else {
      if (!is.null(P$col.grp)) {
         P$grp.label <- paste('Grp', unique(P$col.grp), sep='')
      }
   }
   
      
   #Set colors 
   if (is.null(P$col.color)) {
      if (is.null(P$col.grp)) {
         P$col.color <- (1:ncol(P$dmat))%%5+1 
      } else {
         gu <- unique(P$col.grp)
         P$col.color <- vector(length=length(P$col.grp))
         for (i in 1:length(gu))
            P$col.color[which(P$col.grp==gu[i])] <- i
      }
   } else {
      if (length(P$col.color) != ncol(P$dmat)) {
         if (length(P$col.color) == 1) 
            P$col.color <- rep(P$col.color, ncol(P$dmat))
         else {
            err.AFNI(
               paste("P$col.color must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.color)));
            return(0);
         }
      }
   }
   
   
   #remap colors to a unique set of decent colors
   P$col.color <- plot.1D.mapcolors(P$col.color, P$colorset) 
     
   #Set plot characters
   if(is.null(P$col.plot.char)) {
      P$col.plot.char <- 20+(P$col.color%%6)
   } else if (length(P$col.plot.char) != ncol(P$dmat)) {
      if (length(P$col.plot.char) == 1) {
         if (P$col.plot.char == -1) {
            P$col.plot.char <- rep(NULL,ncol(P$dmat))
         } else {
            P$col.plot.char <- rep(P$col.plot.char,ncol(P$dmat))
         }
      } else {
         P$col.plot.char <- rep(P$col.plot.char,ncol(P$dmat))[1:ncol(P$dmat)]
         
         warn.AFNI(
            paste("P$col.plot.char is recycled to fit number of columns. Need ",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.plot.char)));
         #   return(0);
      }
   }
   #Set plot types
   if(is.null(P$col.plot.type)) {
      P$col.plot.type <- rep('l',ncol(P$dmat))
   } else if (length(P$col.plot.type) != ncol(P$dmat)) {
      if (length(P$col.plot.type) == 1) {
         P$col.plot.type <- rep(P$col.plot.type,ncol(P$dmat))
      } else {
         err.AFNI(
             paste("P$col.plot.type must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.plot.type)));
            return(0);
      }
   } 
   #Set line types
   if(is.null(P$col.line.type)) {
      P$col.line.type <- rep(1,ncol(P$dmat))
   } else if (length(P$col.line.type) != ncol(P$dmat)) {
      if (length(P$col.line.type) == 1) {
         P$col.line.type <- rep(P$col.line.type,ncol(P$dmat))
      } else {
         err.AFNI(
            paste("P$col.line.type must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.line.type)));
            return(0);
      }
   } 
   #Set line widths
   if(is.null(P$col.line.width)) {
      P$col.line.width <- rep(1,ncol(P$dmat))
   } else if (length(P$col.line.width) != ncol(P$dmat)) {
      if (length(P$col.line.width) == 1) {
         P$col.line.width <- rep(P$col.line.width,ncol(P$dmat))
      } else {
         err.AFNI(
               paste("P$col.line.width must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.line.width)));
            return(0);
      }
   } 
   
   #Some processing of col.name
   if (!is.null(P$col.name)) {
      if (length(P$col.name) == 1) {
         if (P$col.name == 'VOLREG') {
            if (ncol(P$dmat) != 6) {
               err.AFNI(paste("Have VOLREG for col.name, but ", 
                           ncol(P$dmat), 
                           "columns in dmat. 6 columns are required."))
            }
            P$col.name <- c('Roll', 'Pitch', 'Yaw', 'I-S', 'R-L', 'A-P')
         }
      }
   } else {
      #Try colnames
      P$col.name <- colnames(P$dmat)
   }
   
   if (is.null(P$row.name)) {
      P$row.name <- rownames(P$dmat)
   }
   
   if (P$col.name.show && is.null(P$col.name)) {
      err.AFNI("Want to show col.name but cannot find column names")
      return(0);
   }

   if(is.null(P$col.mean.line)) {
      P$col.mean.line <- rep(FALSE,ncol(P$dmat))
   } else if (length(P$col.mean.line) != ncol(P$dmat)) {
      if (length(P$col.mean.line) == 1) {
         P$col.mean.line <- rep(P$col.mean.line,ncol(P$dmat))
      } else {
         err.AFNI(
               paste("P$col.mean.line must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.mean.line)));
            return(0);
      }
   } 
   if (!is.null(P$col.text.lym)) {
      if (length(P$col.text.lym) == 1) {
         if (P$col.text.lym == 'COL.NAME' && !is.null(P$col.name)) 
            P$col.text.lym <- P$col.name
         else if (P$col.text.lym == 'COL.IND') 
            P$col.text.lym <- paste('c', 
                                    seq(0,ncol(P$dmat)-1), sep='')
         else {
            P$col.text.lym <- paste(P$col.text.lym, 
                                    seq(0,ncol(P$dmat)-1), sep='')
         }
      }
      if (length(P$col.text.lym) != ncol(P$dmat)) {
         err.AFNI(
               paste("P$col.text.lym", P$col.text.lym, "must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.text.lym)));
         return(0);
      }
   }
   if (!is.null(P$col.text.rym)) {
      if (length(P$col.text.rym) == 1) {
         if (P$col.text.rym == 'COL.NAME' && !is.null(P$col.name)) 
            P$col.text.rym <- P$col.name
         else if (P$col.text.rym == 'COL.IND') 
            P$col.text.rym <- paste('c', 
                                    seq(0,ncol(P$dmat)-1), sep='')
         else {
            P$col.text.rym <- paste(P$col.text.rym, 
                                    seq(0,ncol(P$dmat)-1), sep='')
         }
      }
      if (length(P$col.text.rym) != ncol(P$dmat)) {
         err.AFNI(
               paste("P$col.text.rym", P$col.text.rym, "must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.text.rym)));
         return(0);
      }
   }
   #Set *.step, if lim is a three vector
   if (!is.null(P$xax.lim) && length(P$xax.lim) == 3 && is.null(P$xax.step)) 
         P$xax.step=P$xax.lim[3]
   if (!is.null(P$yax.lim) && length(P$yax.lim) == 3 && is.null(P$yax.step)) 
         P$yax.step=P$yax.lim[3]
   
   #Setup default labels
   if (is.null(P$xax.label)) {
      if (P$dmat.TR==0) P$xax.label <- 'sample'
      else P$xax.label <- 'time'
   }
   if (is.null(P$yax.label)) P$yax.label <- ''
   
   #Column selectors 
   if (is.null(P$dmat.colsel)) {
      P$dmat.colsel=1:1:ncol(P$dmat) 
   }
   
   #Set all NA, NAN, to P$NAval, and P$NANval
   P$dmat[is.na(P$dmat)] = P$NAval
   P$dmat[is.nan(P$dmat)] = P$NANval
   
   #Remove all zeros
   if (P$col.nozeros) {
      i0 <- vector()
      for (i in P$dmat.colsel) {
         if (sum(abs(P$dmat[,P$dmat.colsel[i]])) == 0) 
               i0 <- c(i0,P$dmat.colsel[i]) 
      }
      P$dmat.colsel <- setdiff(P$dmat.colsel, i0)
   }
   
   #Check if possible to do multi graph
   if (P$plotmode == 2 && length(P$dmat.colsel) > 10) {
      warn.AFNI(paste("Too many columns for multi plots.\n",
               "Maximum allowed is 10, have ",length(P$dmat.colsel),"\n",
               "Reverting to one plot mode. Consider matrix mode."));
      P$plotmode <- 1
   }
   
   #Also, multiplot does not behave well, meaning it does not call the 
   #panel function if it is plotting just one colum, so force plotmode
   #for single column selections
   if (length(P$dmat.colsel)==1) P$plotmode <- 1
   
   #Set Offset flag
   if (!is.null(P$col.ystack)) {
      if (length(P$col.ystack) != ncol(P$dmat)) {
         if (length(P$col.ystack) == 1) 
            P$col.ystack <- rep(P$col.ystack, ncol(P$dmat))
         else {
            err.AFNI(
               paste("P$col.ystack must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.ystack)));
            return(0);
         }
      }
   } else {
      P$col.ystack <- rep(FALSE, ncol(P$dmat))
   }

   #Make sure col.ystack is allowed
   if (P$plotmode != 1 || length(P$dmat.colsel) == 1) {
      if (P$col.ystack[1]) {
         note.AFNI("Stacking ignored, either multiplot or single column", 
                     callstr='');
      }
      #No stack allowed if multiplot, or just one column
      P$col.ystack <- rep(FALSE, ncol(P$dmat))
   }
   
   #setup rendering device
   P$dev.this <- plot.1D.setupdevice(P) 

   #Create matrix to be plotted
   P$mat2plt <- plot.1D.setmat2plt (P$dmat, P$dmat.colsel, P$col.ystack)
   #Get the mean of each column
   P$mat2plt.colmeans <- plot.1D.colmean (P$mat2plt)
   #Set plus or minus values
   P$mat2plt.minus <- NULL; P$mat2plt.plus <-NULL;
   if (!is.null(P$dmat.err)) {
      P$mat2plt.minus <- as.matrix(P$dmat.err[,P$dmat.colsel])
      P$mat2plt.plus <- as.matrix(P$dmat.err[,P$dmat.colsel])
   }
   
   #Axes
   if (is.null(P$dmat.xval) || length(P$dmat.xval) != dim(P$mat2plt)[1]) {
      
      P$dmat.xval=seq(from=0,
                      to = dim(P$mat2plt)[1]-1)/plot.1D.freq(P);
   } else if (is.character(P$dmat.xval) && P$dmat.xval == "ENUM") {
      P$dmat.xval=seq(from=1,
                      to = dim(P$mat2plt)[1]);
   }

   #Tick locations
   xat = NULL; yat = NULL;
   if (!is.null(P$xax.step)) {
      xat <- seq(from=P$xax.lim[1],to=P$xax.lim[2],by=P$xax.step);
   }
   if (!is.null(P$yax.step)) {
      yat <- seq(from=P$yax.lim[1],to=P$yax.lim[2],by=P$yax.step);
   } 
     
   if (is.null(P$xax.tic.text)) {
      xaxtinit <- "s" #Plot axis at first pass
   } else {
      xaxtinit <- "n" #Plot it later
   }
   
   if (is.null(P$yax.tic.text) && !prod(P$col.ystack)) {
      yaxtinit <- "s" #Plot axis at first pass
                  #unless all columns are offset. 
                  #YAXIS becomes meaningless then
   } else {
      
      yaxtinit <- "n"
   }
   
   
   #Automatic positioning of column names?
   if (P$col.name.show) {
      if (is.null(P$col.name.x)) {
         xofi <- ((1:length(P$dmat.colsel))%%5)*(length(P$dmat[,1])/5) +
                     (1:length(P$dmat.colsel))%/%5
         P$mat2plt.xoffnames <- xofi/plot.1D.freq(P)
      } else {
         P$mat2plt.xoffnames <- P$col.name.x[P$dmat.colsel] 
         xofi <- round(P$mat2plt.xoffnames*plot.1D.freq(P))
         xofi[xofi < 1] <- 1
         xofi[xofi > nrow(P$mat2plt)] <- nrow(P$mat2plt)  
      }
      if (is.null(P$col.name.y)) {
         P$mat2plt.yoffnames <- vector('numeric', length(P$dmat.colsel))
         for (i in 1:length(P$dmat.colsel)) 
            P$mat2plt.yoffnames[i] <- P$mat2plt[xofi[i], i]
      } else {
         P$mat2plt.yoffnames <- P$col.name.y[P$dmat.colsel]
      }
   }
   
   if (P$plotmode == 1) {
      if (P$verb) note.AFNI("Singleplotmode");
      tp = 'single'
      par(bty=P$boxtype)
      #You can control the margins here if you like someday with:
      #   par(omi = c(Bot, Left, Top, Right))
      #   but you cannot use it to autocrop
      matplot(x=P$dmat.xval, P$mat2plt,             
           col = P$col.color[P$dmat.colsel], main = '',
           xlim=P$xax.lim[1:2], ylim=P$yax.lim[1:2], 
           xlab=P$xax.label, ylab=P$yax.label,
           type= P$col.plot.type[P$dmat.colsel], 
           pch=P$col.plot.char[P$dmat.colsel],
           lty=P$col.line.type[P$dmat.colsel], 
           lwd =P$col.line.width[P$dmat.colsel], xaxt=xaxtinit, yaxt=yaxtinit)
      thisplot <- dev.cur()
      
      if (P$verb>1) {
         note.AFNI("Post matplot browser");
         #browser() does nothing, or so we think, when running 
         #in batch mode. However, If you uncomment the next line, 
         #and you run from CSH prompt with -verb 2, you get this output
         #from this section:
         # ++ Note:  @ 16:36:28
         #Post matplot browser
         #Called from: plot.1D.eng(P)
         #
         #The last line is produced by note.AFNI, when SHOW_TRC is set
         #to TRUE. But here it was not, so that is puzzling.
         #
         #Should not leave uncommented browser commands.
         #They should be manually inserted when running from R prompt
          
         #browser()
      }

      if (!is.null(P$xax.step) && is.null(P$xax.tic.text)) {
         axis(1,seq(from=P$xax.lim[1],to=P$xax.lim[2],by=P$xax.step));
      }
      if (!is.null(P$yax.step)) { 
         axis(2,seq(from=P$yax.lim[1],to=P$yax.lim[2],by=P$yax.step));
      }
      if (!is.null(P$xax.tic.text)) {
         #setup axis ticks based on text, override existing tick locations
         if (!is.null(P$xax.lim)) {
            xat <- seq(from=P$xax.lim[1], to=P$xax.lim[2], 
                    by=(P$xax.lim[2]-P$xax.lim[1])/(length(P$xax.tic.text)-1)); 
         } else {
            xt <- axTicks(1)
            if (length(P$xax.tic.text) == length(xt)) {
               xat <- xt
            } else if (length(P$xax.tic.text) == length(P$dmat.xval)) {
               xat <- P$dmat.xval
            } else {
               xat <- seq(from=xt[1], to=xt[length(xt)], 
                           length.out=length(P$xax.tic.text));
            }
         }
         if (length(xat) != length(P$xax.tic.text)) {
            
            err.AFNI(paste(length(xat), "X ticks", 
                     length(P$xax.tic.text),"X tick text"));
            return(0)
         }
         axis(1,at=xat,labels=P$xax.tic.text);
      }
      if (!is.null(P$yax.tic.text)) { 
         if (!is.null(P$yax.lim)) {
            yat <- seq(from=P$yax.lim[1], to=P$yax.lim[2],
                 by=(P$yax.lim[2]-P$yax.lim[1])/(length(P$yax.tic.text)-1));
         } else {
            yt <- axTicks(2)
            if (length(P$yax.tic.text) == length(yt)) {
               yat <- yt
            } else {
               yat <- seq(from=yt[1], to=yt[length(yt)], 
                           length.out=length(P$yax.tic.text));
            }
         }
         if (length(yat) != length(P$yax.tic.text)) {
            note.AFNI(paste(length(yat), "Y ticks", 
                     length(P$yax.tic.text),"Y tick text."));
            return(0)
         }
         axis(2,at=yat,labels=P$yax.tic.text);
      }

      if (P$verb>1) {
         note.AFNI("Drawing meanlines"); 
      }
      plot.1D.drawmeanlines(P)
      
      if (P$col.name.show) {
         text (P$mat2plt.xoffnames, P$mat2plt.yoffnames, 
               P$col.name[P$dmat.colsel], 
               col = P$col.color[P$dmat.colsel], adj=c(0,0))
      }
       
      if (!is.null(P$mat2plt.minus) && !is.null(P$mat2plt.plus)) {
         for (i in 1:length(P$dmat.colsel)) {
            arrows(x0=P$dmat.xval, y0=P$mat2plt[, i]-P$mat2plt.minus[,i], 
                   x1=P$dmat.xval, y1=P$mat2plt[, i]+P$mat2plt.plus[,i],
                   length=.05, angle=90, code = 3,
                   col = P$col.color[P$dmat.colsel[i]] ) 
         } 
      }  
      #Margin text handled here for single plot, 
      #in plot.1D.multifunc for multiplot
      if (!is.null(P$col.text.lym)) {
         if (P$col.text.lym.at == 'YOFF') {
            ym.at <- P$mat2plt.colmeans
         } else {
            err.AFNI(paste("Bad P$col.text.lym.pos"));
            return(0)
         }
         opar <- par();
         par(ps = P$col.text.lym.fontsize)  
         mtext(P$col.text.lym[P$dmat.colsel], side=2, at=ym.at, 
               las=2, adj=1)
         par(ps = opar$ps)
      }

      if (!is.null(P$col.text.rym)) {
         if (P$col.text.rym.at == 'YOFF') {
            ym.at <- P$mat2plt.colmeans
         } else {
            err.AFNI(paste("Bad P$col.text.rym.pos"));
            return(0)
         }
         opar <- par();
         par(ps = P$col.text.rym.fontsize)  
         mtext(P$col.text.rym[P$dmat.colsel], side=4, at=ym.at, 
               las=2, adj=0)
         par(ps = opar$ps)
      }
   } else if (P$plotmode == 3) { # An image 
      if (P$verb) note.AFNI("Matrix Mode");
      if (is.null(P$col.name)) 
         multinames <- paste('Series', P$dmat.colsel-1, sep='')       
      else multinames <- P$col.name[P$dmat.colsel]
      if (!is.null(P$row.name)) {
         multirownames <- P$row.name 
      } else multirownames <- paste('Row', P$dmat.colsel-1, sep='')    
      
      set.plot.1D.global.P(P)
      colnames(P$mat2plt) <- multinames
      rownames(P$mat2plt) <- multirownames
      #Someday should compute margin width as a function of 
      #row and col name lengths. For now, using oma which is
      #in unit of lines. See also omi and omd
      matrix.AFNI.show(P$mat2plt);
      thisplot <- dev.cur()
      P <- get.plot.1D.global.P()
   } else if (P$plotmode == 2){
      if (P$verb) note.AFNI("Multiplotmode");
      tp = 'multiple'
      
      if (is.null(P$col.name)) 
         multinames <- paste('Series', P$dmat.colsel-1, sep='')       
      else multinames <- P$col.name[P$dmat.colsel]

      set.plot.1D.global.P(P)
      #ylab does not work here. Plot insists on using colnames() of
      #what is being plotted for ylabel
      colnames(P$mat2plt) <- multinames
      plot( ts(P$mat2plt, start=P$dmat.xval[1], frequency= plot.1D.freq(P)), 
         plot.type = tp, 
         type= P$col.plot.type[P$dmat.colsel],
         xy.labels = FALSE, xy.lines = TRUE, 
         panel = plot.1D.multifunc, xlab = P$xax.label, ylab = P$yax.label,
         nc = P$multi.ncol, yax.flip = FALSE,
         axes = TRUE, col = P$col.color[P$dmat.colsel], main = '',
         pch=P$col.plot.char[P$dmat.colsel]) 
      thisplot <- dev.cur()
      P <- get.plot.1D.global.P()
   }    
   if (P$grid.show) {
      grid()
   }
 
   if (P$verb>1) {
      note.AFNI("Legend under consideration"); 
   }
   
   if (P$leg.show) {
      if (is.null(P$col.name) && is.null(P$leg.names)) {
         if (!is.null(P$grp.label)) {
            col = plot.1D.colorOFgroups(P$col.grp, P$col.color) 
            legend(P$leg.position, legend=P$grp.label, 
                     text.col=col, lwd=2, col=col)
         } else {
            err.AFNI(paste("Can't show legend.",
                     "Have no col.name, no leg.names, and no grp.label"));
            
         }
      } else {
         if (is.null(P$leg.names)) P$leg.names <- P$col.name
         if (is.null(P$leg.line.type)) {
            P$leg.line.type <- P$col.line.type[P$dmat.colsel]
            P$leg.line.type[which(P$col.plot.type[P$dmat.colsel]=='p')] = 0;
         } 
         if (is.null(P$leg.line.color)) 
                           P$leg.line.color <- P$col.color[P$dmat.colsel]
         if (is.null(P$leg.plot.char)) 
                           P$leg.plot.char <- P$col.plot.char[P$dmat.colsel]
         opar <- par();
         par(ps = P$leg.fontsize)  
         legend(P$leg.position, legend=P$leg.names, 
                  text.col=P$leg.line.color,
                  col=P$leg.line.color, 
                  pch=P$leg.plot.char[P$dmat.colsel], lwd=2, lty=P$leg.line.type,
                  ncol=P$leg.ncol, bty='n')
         par(ps = opar$ps)
      }
   }
   
   if (P$showcond) {
      view_cond <- 0
      all_cond <- 0
      if (length(P$dmat.colsel)) {
         if (length(which(is.na(P$dmat[,P$dmat.colsel])))==0) {
            view_cond <- kappa(P$dmat[,P$dmat.colsel], exact=TRUE)
         } else {
            view_cond <- -1
         }  
         if (length(which(is.na(P$dmat)))==0) {
            all_cond <- kappa(P$dmat, exact=TRUE)
         } else {
           all_cond  <- -1
         } 
      }
      stit = paste ( 'Condition Numbers:\n',
                     'All          : ', sprintf('%.2f', all_cond), '\n',
                     'Viewed       : ', sprintf('%.2f', view_cond),'\n', sep='');

      cat(stit)
   }
   
   plot.1D.puttitle(P)

   P$dev.this <- plot.1D.unsetupdevice(P)
      
   if (P$save.Rdat) {
      if (is.null(P$prefix)) {
         save(P, file="plot.1D.eng.Rdat", ascii=TRUE);
      } else {
         save(P, file=sprintf('%s.Rdat', P$prefix), ascii=TRUE);
      } 
   }  
   return(thisplot)
}

image.corr.1D <- function(...) {
   p <- plot.1D.optlist(...)
   return(image.corr.1D.eng(p))
}

image.corr.1D.eng <- function(P) {
   
   if (is.null(P$dmat)) return(NULL)
   if (is.null(P$dmat.colsel) || length(P$dmat.colsel)==0) 
      P$dmat.colsel <- seq(1,ncol(P$dmat))
   if (length(P$dmat.colsel) < 2) {
      #warn.AFNI("Have less than 2 columns, nothing to correlate")
      #return(NULL)  
      cc <- as.matrix(1)
   } else {
      cc <- cor(P$dmat[,P$dmat.colsel])
      for (i in 1:length(P$dmat.colsel)) cc[i,i]=0
   }
   if (is.null(P$showval)) {
      if (length(P$dmat.colsel) < 20) ShowVal <- TRUE
      else ShowVal <- FALSE
   } else ShowVal <- P$showval
   
   P$dev.this <- plot.1D.setupdevice(P) 
   
   matrix.AFNI.show(cc, zlim=c(-1,1), ShowVal=ShowVal)
   
   plot.1D.puttitle(P)
   
   P$dev.this <- plot.1D.unsetupdevice(P) 
   return(P$dev.this)
}

make.col.map <- function (fids=NULL, ncols=32, hex=FALSE, stdmap=NULL) {
   if (!is.null(stdmap)) {
      sys.AFNI(com=paste('MakeColorMap -std ', stdmap),
               fout=paste(stdmap,'.1D.cmap.R', sep=''), echo=FALSE);
      m <- read.AFNI.matrix(paste(stdmap,'.1D.cmap.R', sep=''));
      sys.AFNI(com=paste('\\rm -f ',paste(stdmap,'.1D.cmap.R', sep='')));
   } else if (is.null(fids)) {
      fids <- matrix(0,3,3)
      for(i in 1:3) fids[i,i]<-1
      rr <- ncols%%(nrow(fids)-1)
      nc <- ncols/(nrow(fids)-1)
      if (rr) {
         err.AFNI(paste("Can't create colormap of ", ncols, "colors from ",
                           nrow(fids), "fiducials. I suggest you use ", 
                           ncols-rr, "or " , ceiling(nc)*(nrow(fids)-1),
                           "colors instead"))
         return(NULL);
      }
      m <- matrix(0,ncols,3)
      for (j in 1:3) {
         r <- vector()
         for (i in 1:(nrow(fids)-1)) {
            nc <- ncols/(nrow(fids)-1)
            fr <- fids[i,j]
            ft <- fids[i+1,j]
            bb <- (ft-fr)/nc
            if (bb != 0) {
               r <- c(r,seq(from=fr,to=ft, by = bb))
            } else {
               r <- c(r,rep(fr, nc)) 
            }
         }
         #browser()
         m[,j] <- r[ncols:1]
      }
   }
   if (hex) m <- rgb(m)
   return(m)
}

ROIcmap <- function(nc=32, state=0, avoid=c(1,1,1), hex=FALSE) {
   set.seed(state);
   M <- matrix(0,nc,3)

   alldiff_lim <- 0.5; #between 0 and 1, controls how different all colors 
                       #in map are. 
               #The first few colors can be quite different, high alldiff_lim 
               #The difference is adjusted as more colors are demanded.
   g_lim = 0.2; #limit for too gray (0-1)
   d_lim <- 0.40; #limit for too dim (0-3)
   b_lim <- 2.2; #limit for too bright  (0-3)              
   for (i in 1:nc) {
      M[i,] <- runif(3);
      cnt <- 0;
      #reject if too gray or too close to previous color
      while (  toogray(M[i,], g_lim, d_lim, b_lim) ||  
               tooclose(M,i, 0.6, alldiff_lim) ||
               (!is.null(avoid) && (sum(abs(M[i,]-avoid)) < 0.6))) {
         M[i,] <- runif(3);
         cnt <- cnt + 1;
         if (cnt > 2000 && d_lim != 0.01 && alldiff_lim != 0.02 && b_lim != 8) {
                     #too tight, relax
            #cat('relaxing\n')
            alldiff_lim <- 0.9*alldiff_lim
            if (alldiff_lim < 0.02) alldiff_lim <- 0.02
            d_lim <- 0.9*d_lim
            if (d_lim < 0.01) d_lim <- 0.01
            b_lim <- 1.1*b_lim;
            if (b_lim > 8) b_lim <- 8              
            cnt <- 0;
         }
      }
   }
   
   if (hex) M <- rgb(M)
   return(M)
}

toogray <- function (ccol, g_lim, d_lim, b_lim) {
   dc <- abs(ccol - mean(ccol));
   cs <- sum(ccol);
   if (dc[1] < g_lim && dc[2] < g_lim && dc[3] < g_lim) { return(TRUE); }
   if (cs < d_lim || cs > b_lim) { return(TRUE); }
   return(FALSE);
}

tooclose <- function (M,i,prev_lim, alldiff_lim) {

   if (i==1) { return(FALSE); }
   
   
   #too close to previous ?
   dc <- abs(M[i,]-M[i-1,]);
   if (sum(dc) < prev_lim) { return(TRUE); }
   
   #too close to one before?
   if (i > 2) {
      for (j in 1:(i-2)) {
         dc = abs(M[i,]-M[j,]);
         if (dc[1] < alldiff_lim && dc[2] < alldiff_lim && dc[3] < alldiff_lim) {
          return(TRUE); 
         }  
      }
   }
   
   return(FALSE);
}   

#This function is based on
#www.phaget4.org/R/mymatrix.AFNI.show.R
# possibly written by Chris Seidel
matrix.AFNI.show <- function(x, ...){
     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
     title <-c()
     tt <- NULL
     xt <- NULL
     yt <- NULL
     oma <- NULL
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
       min <- Lst$zlim[1]
       max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
       yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
       xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
       title <- Lst$title
    }
    if( !is.null(Lst$text) ) {
      tt <- Lst$text
      if (is.null(Lst$text.x) || is.null(Lst$text.y)) {
         err.AFNI("Can't pass text without text.x|y");
         return(NULL)
      }
      if (length(Lst$text.x) != length(Lst$text.y) ||
          length(Lst$text.x) != length(Lst$text)) {
         err.AFNI("Length mismatch between text and text.x|y");
         return(NULL)   
      }
      xt <- Lst$text.x
      yt <- Lst$text.y
    }
    if( !is.null(Lst$ShowVal)) {
      if (Lst$ShowVal) {
         tt <- vector(length=ncol(x)*nrow(x))
         xt <- vector(length=ncol(x)*nrow(x))
         yt <- vector(length=ncol(x)*nrow(x))
         cn <- 1
         for (i in 1:ncol(x)) {
            for (j in 1:nrow(x)) {
               xt[cn] <- i
               yt[cn] <- j
               tt[cn] <- sprintf('%.2f',x[i,j])
               cn <- cn+1
            }
         }
      }
    }
  }
  
# check for null values
if( is.null(xLabels) ){
   xLabels <- c(1:ncol(x))
}
if( is.null(yLabels) ){
   yLabels <- c(1:nrow(x))
}

layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))

 # Red and green range from 0 to 1 while Blue ranges from 1 to 0
 #ColorRamp <- rgb( seq(0,1,length=256),  # Red
 #                  seq(0,1,length=256),  # Green
 #                  seq(1,0,length=256))  # Blue
 ColorRamp <- make.col.map(ncols=256,hex=TRUE)
 ColorLevels <- seq(min, max, length=length(ColorRamp))

 # Reverse Y axis
 reverse <- nrow(x) : 1
 yLabels <- yLabels[reverse]
 x <- x[reverse,]

 # Data Map
 par(mar = c(3,5,2.5,2))
   
 #Adjust margins to accommodate width of strings
   wwwc <- max(c(strwidth(colnames(x), 'inches')-0.8,0))
   wwwr <- max(c(strwidth(rownames(x), 'inches')-1.2,0))
   par(omi = c(wwwc, wwwr,0,0))
   
 image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
 ylab="", axes=FALSE, zlim=c(min,max))
 if( !is.null(title) ){
    title(main=title)
 }
 
 axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7, las=2)
 axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
      cex.axis=0.7)

 if (!is.null(tt)) {
      opar <- par();
      par(ps = 12)  
   text(xt,yt,tt)
      par(ps=opar$ps)
 }

 # Color Scale
 par(mar = c(3,2.5,2.5,2))
 image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col=ColorRamp,
      xlab="",ylab="",
      xaxt="n")
 layout(1)
}
