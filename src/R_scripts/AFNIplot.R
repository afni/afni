
plot1D.colindex <- function (ivec, 
                  collist=c(1:10)   
                  ) {
   N_collist = length(collist)
   return(collist[(ivec) %% N_collist + 1])
}

plot.1D.testmat <- function(nrow = 100, ncol=10) {
   mm <- matrix(nrow=nrow, ncol= ncol)
   for (i in 1:ncol) {
      mm[,i] <- sin(i*seq(from=0, by=0.1, length.out=nrow))
   }
   return(mm)
}


plot.1D.mapcolors <- function (colvec, collist=c(3,4,5,6,8,1,2,7,9)) {
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
   collist <- seq(1,20) 
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

plot.1D.colorOFgroups <- function(col.grp, col.colors) {
   grp <- unique(col.grp);
   col <- vector(length = length(grp));
   for (gg in 1:length(col)) {
      kk <- which(col.grp==grp[gg])
      if (length(kk) < 1) {
         err.AFNI("COuld not find color of group");
         return(0)
      }
      col[gg] <- col.colors[kk[1]]
   }
   return(col)
}

plot.1D.demo <- function(demo=0) {
   if (demo == 0) demo <- seq(1,4)
   for (d in demo) {
      if (d==1) {
         plot.1D( dmat = plot.1D.testmat(100, 10), 
               col.nozeros=TRUE, 
               col.grp=c(rep(1,3), rep(2,3), rep(3,4)), 
               col.ystack=FALSE, grp.labels=c('CSF', 'GM','WM'),
               prefix = 't1.jpg', verb = 1)
      } else if (d==2){
         plot.1D( dmat = plot.1D.testmat(100, 10), 
               col.nozeros=TRUE, 
               col.grp=c(rep(1,3), rep(2,3), rep(3,4)), 
               col.ystack=FALSE, multi.ncol = 2, 
               grp.labels=c('CSF', 'GM','WM'),
               prefix = 't2.jpg', verb = 1)
      } else if (d==3) {
         plot.1D( dmat = plot.1D.testmat(100, 3), 
                  col.nozeros=TRUE, 
                  col.grp=c(rep(1,2), rep(2,1)), 
                  col.ystack=FALSE, grp.labels=c('CSF', 'GM'),
                  oneplot=TRUE,
                  prefix = 't3.jpg', verb = 1)
      }else if (d==4) {
         plot.1D( dmat = plot.1D.testmat(100, 3), 
                  col.nozeros=TRUE, 
                  col.grp=c(rep(1,2), rep(2,1)), 
                  col.ystack=FALSE, grp.labels=c('CSF', 'GM'),
                  oneplot=TRUE, leg.show = TRUE,
                  prefix = 't4.jpg', verb = 1)
      }
      if (d!=demo[length(demo)]) {
         if (prompt.AFNI("Waiting for your desire",c('g','s')) == 2) return()
      }
   }
   return()
}

PLO <<- list(iplt = 0)

plot.1D.multifunc <- function(x, col, bg, pch, type, ...) {
   lines(x=x,col=col[PLO$iplt],bg=bg,
         pch=pch[PLO$iplt], type=type[PLO$iplt],
         ...)
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
   PLO$iplt <<- PLO$iplt + 1
}

is.good.dev <- function (dd=NULL) {
   if (is.null(dd) || dd == 0) return(FALSE)
   if (length(which(dev.list()==dd))) return(TRUE)
}

plot.1D.setupdevice <- function (P) {
   if (!is.null(P$prefix) && P$nodisp) { #render to device directly
      pp <- parse.name(P$prefix)
      if (tolower(pp$ext) == 'jpg') 
         jpeg(P$prefix, width=P$img.width, height=P$img.height, 
                  quality=P$img.qual, 
                  res=P$img.dpi, pointsize=P$img.def.fontsize)
      else if (tolower(pp$ext) == 'png') 
         png(P$prefix, width=P$img.width, height=P$img.height,
                  res=P$img.dpi, pointsize=P$img.def.fontsize)
      else if (tolower(pp$ext) == 'pdf') 
         pdf(P$prefix, width=P$img.width, height=P$img.height,
                  res=P$img.dpi, pointsize=P$img.def.fontsize)
      else {
        if (0) {
         pdf(paste(P$prefix,'.pdf',sep=''))   #best, but not always present       
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
         x11()
      } else {
         #Only get new if list is empty
         if (is.null(dev.list())) x11()
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
         if (tolower(pp$ext) == 'jpg') dev.copy(jpeg,P$prefix)
         else if (tolower(pp$ext) == 'png') dev.copy(png,P$prefix)
         else if (tolower(pp$ext) == 'pdf') dev.copy(pdf,P$prefix)
         else dev.copy(pdf,paste(P$prefix,'.pdf',sep=''))
         dev.off()
      } else { #rendered directly to image
         dev.off()
         thisplot <- 0
      }
   }
   return(thisplot)
}

plot.1D.puttitle <- function (P) {   
   if (!is.null(P$ttl.main) || !is.null(P$ttl.sub)) {
      opar <- par();
      par(font.main = P$ttl.main.fontsize)
      par(font.sub = P$ttl.sub.fontsize) 
      title(main=P$ttl.main, sub=P$ttl.sub)
      par(font.main = opar$font.main)
      par(font.sub = opar$font.sub)
   }
}   

plot.1D.optlist <- function(...) {
   ll <- list(dmat=NULL, dmat.err=NULL, dmat.colsel=NULL, 
            dmat.xval=NULL,
            col.grp = NULL, col.ystack=FALSE, col.nozeros=FALSE, 
            col.names=NULL, col.names.x=NULL, col.names.y=NULL,
            col.colors = NULL, col.plot.char=NULL,
            col.plot.type ='l', col.line.type = 1, col.line.width=3,
            col.mean.line=FALSE,
            grp.labels=NULL,
            ttl.main=NULL, ttl.main.fontsize = 10,
            ttl.sub=NULL, ttl.sub.fontsize = 10,
            prefix = NULL, showval=FALSE,
            nodisp = FALSE, oneplot = FALSE, boxtype = 'n', multi.ncol=2,
            colorset = seq(1,20),
            xax.lim=NULL, xax.step = NULL, xax.label=NULL, xax.tic.text = NULL, 
            yax.lim=NULL, yax.step = NULL, yax.label=NULL, yax.tic.text = NULL,
            leg.show=FALSE, leg.ncol = 4, leg.names=NULL, 
            leg.position="topright", leg.fontsize = 12,
            col.text.lym = NULL, col.text.lym.at = 'YOFF', 
            col.text.lym.fontsize = 10, 
            col.text.rym = NULL, col.text.rym.at = 'YOFF', 
            col.text.rym.fontsize = 10, 
            img.width=2000, img.height=2000, img.qual = 98, 
            img.dpi = 300, img.def.fontsize=12,
            dev.this=NULL, dev.new=FALSE,
            verb = 0);
  #Add user specifics
  if( length(list(...)) ){
      up <- list(...)
      for (iu in 1:length(up)) {
         wiu <- which(names(ll) == names(up)[iu])
         if (length(wiu)) ll[wiu] <- up[iu]
         else {
            warn.AFNI(sprintf("User variable %s not good for plot variables",
                              names(up)[iu]))
         }
      }
   }

   return(ll)
}

#see plot.1D.optlist for allowed options
plot.1D <- function (...) {
   if (length(list(...)) == 0) {
      note.AFNI("Plot.1D in hardwired test mode")
      plot.1D.demo()
      return(1)
   }
   PLO <<- plot.1D.optlist(...)
   #Set some more variables
   PLO <<- c(PLO, iplt=1, mat2plt=NULL, mat2plt.colmeans=NULL,  
                  mat2plt.minus=NULL, mat2plt.plus=NULL)
   
   return(plot.1D.eng(PLO))
}

plot.1D.eng <- function (P) {
   thisplot <- NULL
   #Load DATA
   if (is.null(P$dmat)) {
      err.AFNI("NULL dmat");
      return(0)  
   } else if (is.character(P$dmat)) {
      dmatv <- P$dmat
      if (is.null(P$ttl.main)) ttl.main<-paste(P$dmat, collapse='\n')
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
      return(FALSE)
   }
   
   #Load the error matrix
   if (!is.null(P$dmat.err) && is.character(P$dmat.err)) {
      dmatv <- P$dmat.err
      for (i in 1:length(dmatv)) { #Won't work for different row numbers...
         if (is.null(ffc <- read.AFNI.matrix(ffv[i]))) {
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
   } 
   
   #Check for P$grp.labels
   if (!is.null(P$grp.labels)) {
      if (is.null(P$col.grp)) {
         err.AFNI("Have P$grp.labels, but no P$col.grp");
         return(0)
      }
      if (length(P$grp.labels) != length(unique(P$col.grp))) {
         err.AFNI(paste("Have ", length(P$grp.labels), "group labels and",
               length(unique(P$col.grp)),"unique group flags"));
         return(0)
      }
   } else {
      if (!is.null(P$col.grp)) {
         P$grp.labels <- paste('Grp', P$col.grp, sep='')
      }
   }
   
      
   #Set colors 
   if (is.null(P$col.colors)) {
      if (is.null(P$col.grp)) {
         if (is.null(P$col.colors <- attr(dmat,'ColumnGroups'))) {
            P$col.colors <- (1:ncol(P$dmat))%%5+1 
         }
      } else {
         P$col.colors <- P$col.grp
      }
   } else {
      if (length(P$col.colors) != ncol(P$dmat)) {
         if (length(P$col.colors) == 1) 
            P$col.colors <- rep(P$col.colors, ncol(P$dmat))
         else {
            err.AFNI(
               paste("P$col.colors must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.colors)));
            return(0);
         }
      }
   }
   
   
   #remap colors to a unique set of decent colors
   P$col.colors <- plot.1D.mapcolors(P$col.colors, P$colorset) 
     
   #Set plot characters
   if(is.null(P$col.plot.char)) {
      P$col.plot.char <- 19+P$col.colors
   } else if (length(P$col.plot.char) != ncol(P$dmat)) {
      if (length(P$col.plot.char) == 1) {
         if (P$col.plot.char == -1) {
            P$col.plot.char <- rep(NULL,ncol(P$dmat))
         } else {
            P$col.plot.char <- rep(P$col.plot.char,ncol(P$dmat))
         }
      } else {
         err.AFNI(
            paste("P$col.plot.char must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.plot.char)));
            return(0);
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
      if (length(P$col.text.lym) != ncol(P$dmat)) {
         err.AFNI(
               paste("P$col.text.lym", P$col.text.lym, "must either have one or",
                     ncol(P$dmat),"values",
                     "Have ", length(P$col.text.lym)));
         return(0);
      }
   }
   if (!is.null(P$col.text.rym)) {
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
   if (is.null(P$xax.label)) P$xax.label <- 'sample'
   if (is.null(P$yax.label)) P$yax.label <- ''
   
   #Column selectors 
   if (is.null(P$dmat.colsel)) {
      P$dmat.colsel=1:1:ncol(P$dmat) 
   }
   
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
   if (!P$oneplot && length(P$dmat.colsel) > 10) {
      warn.AFNI(paste("Too many columns for multi plots.\n",
               "Maximum allowed is 10, have ",length(P$dmat.colsel),"\n",
               "Reverting to one plot mode"));
      P$oneplot <- TRUE
   }
   
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
   if (!P$oneplot || length(P$dmat.colsel) == 1) {
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
   #Stick these matrices in PLO, the global structure
   PLO$mat2plt <<- P$mat2plt
   PLO$mat2plt.colmeans <<- P$mat2plt.colmeans
   PLO$mat2plt.minus <<- P$mat2plt.minus
   PLO$mat2plt.plus <<- P$mat2plt.plus
      
   #Axes
   if (is.null(P$dmat.xval) || length(P$dmat.xval) != dim(P$mat2plt)[1]) {
      P$dmat.xval=seq(0,dim(P$mat2plt)[1]-1)/frequency(P$dmat);
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
      #setup axis ticks based on text, override existing tick locations
      if (!is.null(P$xax.lim)) {
         xat <- seq(from=P$xax.lim[1], to=P$xax.lim[2], 
                     by=(P$xax.lim[2]-P$xax.lim[1])/length(P$xax.tic.text)); 
      } else {
         xat <- seq(from=0, to=length(P$xax.tic.text)-1, by=1); 
      }
      if (length(xat) != length(P$xax.tic.text)) {
         err.AFNI(paste(length(xat), "X ticks", 
                  length(P$xax.tic.text),"X tick text"));
         return(0)
      }
      xaxtinit <- "n" #Plot it later
   }
   if (is.null(P$yax.tic.text) && !prod(P$col.ystack)) {
      yaxtinit <- "s" #Plot axis at first pass
                  #unless all columns are offset. 
                  #YAXIS becomes meaningless then
   } else {
      if (!is.null(P$yax.tic.text)) {
         if (!is.null(P$yax.lim)) {
            yat <- seq(from=P$yax.lim[1], to=P$yax.lim[2],
                 by=(P$yax.lim[2]-P$yax.lim[1])/length(P$yax.tic.text));
         } else {
            yat <- seq(from=0, to=length(P$yax.tic.text)-1, by=1); 
         }
         if (length(yat) != length(P$yax.tic.text)) {
            err.AFNI(paste(length(yat), "Y ticks", 
                     length(P$yax.tic.text),"Y tick text"));
            return(0)
         }
      }
      yaxtinit <- "n"
   }
   
   #Automatic positioning of column names?
   if (!is.null(P$col.names)) {
      if (is.null(P$col.names.x)) {
         xofi <- ((1:length(P$dmat.colsel))%%5)*(length(P$dmat[,1])/5) +
                     (1:length(P$dmat.colsel))%/%5
         xoffnames <- xofi/frequency(P$dmat)
      } else {
         xoffnames <- P$col.names.x[P$dmat.colsel] 
         xofi <- round(xoffnames*frequency(P$dmat))
         xofi[xofi < 1] <- 1
         xofi[xofi > nrow(P$mat2plt)] <- nrow(P$mat2plt)  
      }
      if (is.null(P$col.names.y)) {
         yoffnames <- vector('numeric', length(P$dmat.colsel))
         for (i in 1:length(P$dmat.colsel)) yoffnames[i] <- P$mat2plt[xofi[i], i]
      } else {
         yoffnames <- P$col.names.y[P$dmat.colsel]
      }
   }
   
   if (P$oneplot) {
      if (P$verb) note.AFNI("Singleplotmode");
      tp = 'single'
      par(bty=P$boxtype)
      matplot(x=P$dmat.xval, P$mat2plt, 
           col = P$col.colors[P$dmat.colsel], main = '',
           xlim=P$xax.lim[1:2], ylim=P$yax.lim[1:2], 
           xlab=P$xax.label, ylab=P$yax.label,
           type= P$col.plot.type[P$dmat.colsel], 
           pch=P$col.plot.char[P$dmat.colsel],
           lty=P$col.line.type[P$dmat.colsel], 
           lwd =P$col.line.width[P$dmat.colsel], xaxt=xaxtinit, yaxt=yaxtinit)
      thisplot <- dev.cur()
      if (P$verb>1) {
         note.AFNI("Post matplot browser"); browser()
      }
      if (!is.null(P$xax.step)) {
         axis(1,seq(from=P$xax.lim[1],to=P$xax.lim[2],by=P$xax.step));
      }
      if (!is.null(P$yax.step)) { 
         axis(2,seq(from=P$yax.lim[1],to=P$yax.lim[2],by=P$yax.step));
      }
      if (!is.null(P$xax.tic.text)) {
         axis(1,at=xat,labels=P$xax.tic.text);
      }
      if (!is.null(P$yax.tic.text)) { 
         axis(2,at=yat,labels=P$yax.tic.text);
      }
      for (i in 1:length(P$col.mean.line[P$dmat.colsel])) {
         if (P$col.mean.line[P$dmat.colsel[i]]) {
            ravg <- matrix(nrow=nrow(P$mat2plt), ncol=ncol(P$mat2plt))
            for (i in 1:ncol(P$mat2plt)) {
               ravg[,i] <- rep(P$mat2plt.colmeans[i], dim(P$mat2plt)[1])
               lines(ravg[,i], col = P$col.colors[P$dmat.colsel[i]],  lty=3)
            }
         }
      }
      
      if (!is.null(P$col.names)) {
         text (xoffnames, yoffnames, 
               P$col.names[P$dmat.colsel], 
               col = P$col.colors[P$dmat.colsel], adj=c(0,0))
      }
       
      if (!is.null(P$mat2plt.minus) && !is.null(P$mat2plt.plus)) {
         for (i in 1:length(P$dmat.colsel)) {
            arrows(x0=x, y0=P$mat2plt[, i]-P$mat2plt.minus[,i], 
                   x1=x, y1=P$mat2plt[, i]+P$mat2plt.plus[,i],
                   length=.05, angle=90, code = 3,
                   col = P$col.colors[P$dmat.colsel[i]] ) 
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
   } else {
      if (P$verb) note.AFNI("Multiplotmode");
      tp = 'multiple'
      
      if (is.null(P$col.names)) multinames <- colnames(P$dmat[,P$dmat.colsel])
      else multinames <- P$col.names[P$dmat.colsel]
      plot( as.ts(P$mat2plt), plot.type = tp, 
            type= P$col.plot.type[P$dmat.colsel],
            xy.labels = multinames, xy.lines = TRUE, 
            panel = plot.1D.multifunc, nc = P$multi.ncol, yax.flip = FALSE,
            axes = TRUE, col = P$col.colors[P$dmat.colsel], main = '',
            pch=P$col.plot.char[P$dmat.colsel]) 
      thisplot <- dev.cur()
   }
   
   
   if (P$leg.show) {
      if (is.null(P$col.names) && is.null(P$leg.names)) {
         if (!is.null(P$grp.labels)) {
            col = plot.1D.colorOFgroups(P$col.grp, P$col.colors) 
            legend("topright", legend=P$grp.labels, 
                     text.col=col,lwd=2, col=col)
         } else {
            err.AFNI(paste("Can't show legend.",
                     "Have no col.names, no leg.names, and no grp.labels"));
            
         }
      } else {
         if (is.null(P$leg.names)) P$leg.names <- P$col.names
         opar <- par();
         par(ps = P$leg.fontsize)  
         legend(P$leg.position, legend=P$leg.names, 
                  text.col=P$col.colors[P$dmat.colsel],
                  col=P$col.colors[P$dmat.colsel], 
                  pch=P$col.plot.char[P$dmat.colsel], lwd=2, lty=P$col.line.type,
                  ncol=P$leg.ncol, bty='n')
         par(ps = opar$ps)
      }
   }
   
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
   
   plot.1D.puttitle(P)

   P$dev.this <- plot.1D.unsetupdevice(P)
      
  
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

make.col.map <- function (fids=NULL, ncols=32, hex=FALSE) {
   if (is.null(fids)) {
      fids <- matrix(0,3,3)
      for(i in 1:3) fids[i,i]<-1
   }
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
      m[,j] <- r[1:ncols]
   }
   if (hex) m <- rgb(m)
   return(m)
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
      if (is.null(Lst$test.x) || is.null(Lst$text.y)) {
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
 ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
 ColorRamp <- make.col.map(ncols=256,hex=TRUE)
 ColorLevels <- seq(min, max, length=length(ColorRamp))

 # Reverse Y axis
 reverse <- nrow(x) : 1
 yLabels <- yLabels[reverse]
 x <- x[reverse,]

 # Data Map
 par(mar = c(3,5,2.5,2))
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
