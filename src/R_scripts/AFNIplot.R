
plot1D.colindex <- function (ivec, 
                  collist=c(1:10)   
                  ) {
   N_collist = length(collist)
   return(collist[(ivec) %% N_collist + 1])
}

plot.1D <- function (ff=NULL, ffd=NULL, isel=NULL, 
                     descr="", NoZeros=FALSE,
                     ColumnGroups = NULL,
                     OffsetBase=TRUE,
                     GroupLabels=NULL,
                     Title=NULL,
                     prefix = NULL,
                     CloseAfterSave = FALSE,
                     OneSubplot = FALSE,
                     addmean=TRUE,
                     xlim=NULL,
                     ylim=NULL,
                     xlabel=NULL,
                     ylabel=NULL,
                     symbs=NULL, cols=NULL, cnames=NULL, 
                     xstep=NULL, ystep=NULL, ltypes = NULL,
                     leg.ncol = 4, leg.names=NULL, leg.position="topright",
                     x=NULL) {
   if (!is.null(xlim) && length(xlim) == 3 && is.null(xstep)) xstep=xlim[3]
   if (!is.null(ylim) && length(ylim) == 3 && is.null(ystep)) ystep=ylim[3]
   
   if (is.null(ff)) {
      note.AFNI("Plot.1D in hardwired test mode")
      return(
         plot.1D( 'IBSR_01.SubClasses.skew.1D.repsig', 
                  NoZeros=TRUE, 
                  ColumnGroups=c(rep(1,10), rep(2,10), rep(3,10)), 
                  OffsetBase=FALSE, GroupLabels=c('CSF', 'GM','WM') ) )
   } else if (is.character(ff)) {
      ffv <- ff
      if (is.null(Title)) Title<-paste(ff, collapse='\n')
      for (i in 1:length(ffv)) { #Won't work for different row numbers...
         if (is.null(ffc <- read.AFNI.matrix(ffv[i]))) {
            err.AFNI("Failed to read test file")
            return(0) 
         }
         #str(ffc)
         if (i==1) { 
            ff<-ffc; 
         }else {
            if (dim(ffc)[1] < dim(ff)[1]) {
               ffna <- matrix(NA, dim(ff)[1], dim(ffc)[2])
               ffna[1:dim(ffc)[1],1:dim(ffc)[2]] <- ffc
               ffc <- ffna
            }
            ff <- cbind(ff, ffc)
         }
      }
   }
    
   if (is.null(ff)) {
      err.AFNI("Null input");
      return(FALSE)
   }
   
   if (!is.null(ffd) && is.character(ffd)) {
      ffv <- ffd
      for (i in 1:length(ffv)) { #Won't work for different row numbers...
         if (is.null(ffc <- read.AFNI.matrix(ffv[i]))) {
            err.AFNI("Failed to read test file")
            return(0) 
         }
         #str(ffc)
         if (i==1) { 
            ffd<-ffc; 
         }else {
            if (dim(ffc)[1] < dim(ffd)[1]) {
               ffna <- matrix(NA, dim(ffd)[1], dim(ffc)[2])
               ffna[1:dim(ffc)[1],1:dim(ffc)[2]] <- ffc
               ffc <- ffna
            }
            ffd <- cbind(ffd, ffc)
         }
      }
   }
   
   if (!is.null(ffd)) {
      if (!is.null(ffd) && OffsetBase) {
         err.AFNI("Error bars with OffsetBase?");
         return(0);
      }
      if (dim(ffd)[1] != dim(ff)[1] ||
          dim(ffd)[2] != dim(ff)[2]) {
         err.AFNI("Dimensions mismatch between ffd and ff");
         return(0)   
      }
   } 
   if (is.null(isel)) {
      isel=1:1:ncol(ff) 
   }
   #cat ('isel in show_xmat')
   #show(isel)
   #Set colors based on groups 
   if (is.null(cols)) {
      if (is.null(ColumnGroups)) {
         if (is.null(colvec <- attr(ff,'ColumnGroups'))) {
            colvec <- (1:ncol(ff))%%5+1 
         }
      } else {
         colvec <- ColumnGroups
         if (length(colvec) != ncol(ff)) {
            err.AFNI("Mismatch between length of colvec and ff")
            return(0)
         }
      }
   } else {
      colvec <- cols
   }
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
   #collist <- c(3,4,5,6,8,1,2,7,9) 
   collist <- seq(1,20) 
   for (i in min(colvec[colvec > 0]):max(colvec[colvec > 0])) {
      colvec[cg == i] <- plot1D.colindex(i, collist)
   }
   
   #Remove all zeros
   if (NoZeros) {
      i0 <- vector()
      for (i in isel) {
         if (sum(abs(ff[,isel[i]])) == 0) i0 <- c(i0,isel[i]) 
      }
      isel <- setdiff(isel, i0)
   }
   
   if (!is.null(prefix) && CloseAfterSave) { #render to device directly
      pp <- parse.name(prefix)
      if (tolower(pp$ext) == 'jpg') 
         jpeg(prefix, width=2000, height=2000, quality=95, res=300, pointsize=14)
      else if (tolower(pp$ext) == 'png') png(prefix)
      else if (tolower(pp$ext) == 'pdf') pdf(prefix)
      else pdf(paste(prefix,'.pdf',sep='')) 
   } else {
      if (is.null(dev.list())) x11()
   }
   if (length(isel) > 10 || OneSubplot) {
      tp = 'single'
      #Calculate normalized version for full display
      ffr <- as.matrix(ff[,isel])
      ra = matrix(nrow=ncol(ffr), ncol=3)
      for (i in 1:ncol(ffr)) {
         ra[i,] <- as.vector(quantile(ffr[,i], c(0,1,0.5), na.rm='TRUE'))
      }
         offs <-vector(length = ncol(ffr), mode="numeric")
         for (i in 1:ncol(ffr)) {
            if (OffsetBase) {
               ffr[,i] <- (ffr[,i]-ra[i,1])/(ra[i,2]-ra[i,1])+1.4*i
               ffr[is.nan(ffr[,i]),i] <- 0
               if (!is.null(ffd)) {
                  warn.AFNI("Error bars Not scaled as timeseries were");
               }
            } 
            offs[i] = mean(ffr[,i])
         }
      
      ffdrm = NULL; ffdrp=NULL;
      if (!is.null(ffd)) {
         ffdrm <- as.matrix(ffd[,isel])
         ffdrp <- as.matrix(ffd[,isel])
      } 
      if(is.null(symbs)) symbs<-19+seq(1,ncol(ffr))
      #xy.labels = colnames(ff[,isel])
      if (is.null(x) || length(x) != dim(ffr)[1]) {
         x=seq(0,dim(ffr)[1]-1);
      }
      matplot(x=x,ffr, col = colvec[isel], main = '',
           xlim=xlim[1:2], ylim=ylim[1:2], xlab=xlabel, ylab=ylabel,
           type= 'b', pch=symbs, lty=ltypes)
      if (!is.null(xstep)) axis(1,seq(from=xlim[1],to=xlim[2],by=xstep));
      if (!is.null(ystep)) axis(2,seq(from=ylim[1],to=ylim[2],by=ystep));
      if (addmean) {
         ravg <- matrix(nrow=nrow(ffr), ncol=ncol(ffr))
         for (i in 1:ncol(ffr)) {
            ravg[,i] <- rep(mean(ffr[,i]), dim(ffr)[1])
            lines(ravg[,i], col = colvec[isel[i]],  lty=3)
         }
      }
      thisplot <- dev.cur()
      xofi = ((1:length(isel))%%5)*(length(ff[,1])/5)+(1:length(isel))%/%5
      xoff = xofi/frequency(ff)
      yoff <- vector('numeric', length(isel))
      for (i in 1:length(isel)) yoff[i] <- ffr[xofi[i], i]
      if (!is.null(cnames)) {
         text (xoff, yoff, 
               cnames[isel], col = colvec[isel], adj=c(0,0))
      } else {
         if (0) {
            text (xoff, yoff, 
               colnames(ff[,isel]), col = colvec[isel], adj=c(0,0))
         }
      }
      if (!is.null(ffdrm) && !is.null(ffdrp)) {
         for (i in 1:length(isel)) {
            arrows(x0=x, y0=ffr[, i]-ffdrm[,i], 
                   x1=x, y1=ffr[, i]+ffdrp[,i],
                   length=.05, angle=90, code = 3,
                   col = colvec[isel[i]] ) 
            #errbar(x=x,y=ffr[, i],ffr[, i]-ffdrm[,i], ffr[, i]+ffdrp[,i],
             #        col = colvec[isel[i]] )
         } 
      }  
   } else {
      tp = 'multiple'
      if (length(isel) == 1) {
         colm <- colvec[isel]
      } else {
         colm <- 1   #ARRRRGH only one col is used for multiple line drawings!
      }
      plot(as.ts(ff[,isel]), plot.type = tp,
           xy.labels = colnames(ff[,isel]), xy.lines = TRUE, 
           panel.first = lines, nc = 1, yax.flip = FALSE,
           axes = TRUE, col = colm, main = '') 
      thisplot <- dev.cur()
   }
   
   if (is.null(cnames) && is.null(leg.names)) {
      if (!is.null(GroupLabels)) {
         if (length(GroupLabels) == length(ugr)) {
            col = plot1D.colindex(1:length(GroupLabels), collist)
         } else if (length(GroupLabels) == ncol(ff)) {
            col = plot1D.colindex(isel, collist)
         } else {
            col = vector()
         }
         legend("topright", legend=GroupLabels, text.col=col,lwd=2)
      }
   
   } else {
      if (is.null(leg.names)) leg.names <- cnames
      legend(leg.position, legend=leg.names, text.col=colvec[isel],
                  col=colvec[isel], pch=symbs, lwd=2, lty=ltypes,
                  ncol=leg.ncol, bty='n')
   }
   view_cond <- 0
   all_cond <- 0
   if (length(isel)) {
      if (length(which(is.na(ff[,isel])))==0) {
         view_cond <- kappa(ff[,isel], exact=TRUE)
      } else {
         view_cond <- -1
      }  
      if (length(which(is.na(ff)))==0) {
         all_cond <- kappa(ff, exact=TRUE)
      } else {
        all_cond  <- -1
      } 
   }
   stit = paste ( 'Condition Numbers:\n',
                  'All          : ', sprintf('%.2f', all_cond), '\n',
                  'Viewed       : ', sprintf('%.2f', view_cond),'\n', sep='');
   
   cat(stit)
   
   
   if (!is.null(Title)) {
      if (view_cond > 0) {
         Title <- paste(Title,'\n', 
                     'cn=', sprintf('%.2f', view_cond), sep='');
      } else {
         Title <- paste(Title, sep='');
      }
      title(Title)
   }
      
   if (!is.null(prefix)) {
      if (!CloseAfterSave) {  #Need a copy
         pp <- parse.name(prefix)
         if (tolower(pp$ext) == 'jpg') dev.copy(jpeg,prefix)
         else if (tolower(pp$ext) == 'png') dev.copy(png,prefix)
         else if (tolower(pp$ext) == 'pdf') dev.copy(pdf,prefix)
         else dev.copy(pdf,paste(prefix,'.pdf',sep=''))
         dev.off()
      } else { #rendered directly to image
         dev.off()
         thisplot <- 0
      }
   }
   
   return(thisplot)
}
