#------------------------------------------------------------------
# Global Variables
#------------------------------------------------------------------
BATCH_MODE <<- 0  #Initialize batch mode flag to 0

#------------------------------------------------------------------
# Functions to deal with AFNI file names
#------------------------------------------------------------------
strip.extension <- function (filename, extvec=NULL, verb=0) {
   n <- list()
   if (is.null(extvec)) {
      ff <- strsplit(filename, '\\.')[[1]]
      if (length(ff) > 1) {
         n$ext <- ff[length(ff)]
         n$name_noext <- paste(ff[1:length(ff)-1],collapse='.')
      } else {
         n$ext <- ''
         n$name_noext <- filename
      } 
   } else {
      n$ext <- ''
      n$name_noext <- filename
      for (ex in extvec) {
         patt <- paste('\\',ex,'$',collapse='', sep='')
         if (length(grep(patt, filename))) {
            n$ext <- ex
            n$name_noext <- sub(patt,'',filename)
            return(n)
         }
      }
   }   
   return(n)
}

parse.name <- function (filename, extvec=NULL, verb=0) {
   n <- list()
   ff <- strsplit(filename, .Platform$file.sep)[[1]]
   n$name <- ff[length(ff)]
   n$path <- '.'
   if (length(ff)>1) 
      n$path <- paste(ff[1:length(ff)-1],collapse=.Platform$file.sep)
   n$path <- paste(n$path, .Platform$file.sep, sep="")
   
   n2 <- strip.extension(n$name, extvec, verb)
   n$ext <- n2$ext
   n$name_noext <- n2$name_noext   
   
   return(n)
}

date.stamp <- function (fancy=FALSE) {
   if (fancy) {
      return(gsub(' ','_',date()))
   } else {
      return(format(Sys.time(), "%d%m%y-%H%M%S"))
   }
}

parse.AFNI.name.selectors <- function(filename,verb=0) {
   n <- list()
   n$brsel<- NULL;
   n$rosel<- NULL;
   n$rasel<- NULL;
   n$insel<- NULL;
   
   selecs <- strsplit(filename,"\\[|\\{|<|#")[[1]];
   n$name <- selecs[1]
   for (ss in selecs[2:length(selecs)]) {
      if (length(grep("]",ss))) {
         n$brsel <- strsplit(ss,"\\]")[[1]][1];
      } else if (length(grep("}",ss))) {
         n$rosel <- strsplit(ss,"\\}")[[1]][1];
      } else if (length(grep(">",ss))) {
         n$rasel <- strsplit(ss,">")[[1]][1];
      } 
   }
   selecs <- strsplit(filename,"#")[[1]];
   if (length(selecs) > 1) {
      n$insel <- selecs[2]
   }
   
   return(n)    
}

parse.AFNI.name <- function(filename, verb = 0) {
  if (filename == '-self_test') { #Secret testing flag
      note.AFNI('Function running in test mode');
      show.AFNI.name(parse.AFNI.name('DePath/hello.DePrefix', verb))
      show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc', verb))
      show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.', verb))
      show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.HEAD', verb))
      show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.BRIK.gz', verb))
      show.AFNI.name(parse.AFNI.name('DePath/DePrefix+acpc.HEAD[23]', verb))
      show.AFNI.name(
         parse.AFNI.name('DePath/DePrefix+acpc.HEAD[DeLabel]{DeRow}', verb))
      show.AFNI.name(
         parse.AFNI.name('DePath/DePrefix+acpc[DeLabel]{DeRow}', verb))
      show.AFNI.name(
         parse.AFNI.name('DePath/DePrefix+acpc.[DeLabel]{DeRow}', verb))
      return(NULL)
   }
   an <- list()
   an$view <- NULL
   an$prefix <- NULL
   an$brsel <- NULL;
   an$rosel <- NULL;
   an$rasel <- NULL;
   an$insel <- NULL;
   an$type <- NULL;
   an$path <- NULL;
   an$orig_name <- filename;

   if (verb) { cat ('Parsing >>',filename,'<<\n', sep=''); }
   if (!is.character(filename)) {
      warning(paste('filename >>', 
                     filename, '<< not a character string\n', sep=''),
                 immediate. = TRUE);
      traceback();
      return(NULL);
   }

   #Deal with selectors
   n <- parse.AFNI.name.selectors(filename, verb)
   filename <- n$name
   an$brsel <- n$brsel;
   an$rosel <- n$rosel;
   an$rasel <- n$rasel; 
   an$insel <- n$insel;

   #Remove last dot if there
   filename <- sub('\\.$','',filename)

   #NIFTI?
   n <- strip.extension(filename, c('.nii', '.nii.gz'), verb)
   if (n$ext != '') {
      an$ext <- n$ext
      an$type <- 'NIFTI'
      an$prefix <- n$name_noext
   } else {
      #remove other extensions
      n <- strip.extension(filename, c('.HEAD','.BRIK','.BRIK.gz',
                                       '.BRIK.bz2','.BRIK.Z',
                                       '.1D', '.1D.dset', 
                                       '.niml.dset',
                                       '.'  ),
                           verb)
      if (n$ext == '.1D' || n$ext == '.1D.dset') {
         an$type <- '1D'
      } else if (n$ext == '.niml.dset') {
         an$type <- 'NIML'
      } else {
         an$type <- 'BRIK'
      } 
      
      if (n$ext == '.') {
         n$ext <- ''
      }
      an$ext <- n$ext
      filename <- n$name_noext
      
      n <- strip.extension(filename, c('+orig','+tlrc','+acpc'), verb)
      if (n$ext != '') {
         an$view <- n$ext
      } else {
         an$view <- NA
      }
      an$prefix <- n$name_noext
   }
 
  if (verb > 2) {
   browser()
  }
  
  return(an)
}

exists.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   
   ans <- 0
   if (file.exists(head.AFNI.name(an))) ans <- ans + 1;
   
   if (file.exists(brik.AFNI.name(an)) ||
       file.exists(paste(brik.AFNI.name(an),'.gz', sep='')) ||
       file.exists(paste(brik.AFNI.name(an),'.Z', sep=''))) ans <- ans + 2;
   return(ans);
}

prefix.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   return(an$prefix);
}

view.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   return(an$view);
}

pv.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   return(paste(an$prefix,an$view,sep=''));
}

head.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   if (an$type == 'BRIK' && !is.na(an$view)) {
      return(paste(an$prefix,an$view,".HEAD",sep=''));
   } else {
      return((an$orig_name));
   }
}

brik.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   if (an$type == 'BRIK' && !is.na(an$view)) {
      return(paste(an$prefix,an$view,".BRIK",sep=''));
   } else {
      return((an$orig_name));
   }
}

compressed.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   if (length(grep('\\.gz$', an$ext))) {
      return('gz')
   } else if (length(grep('\\.bz2$', an$ext))) {
      return('bz2')
   } else if (length(grep('\\.Z$', an$ext))) {
      return('Z')
   } else {
      return('')
   }

}
uncompress.AFNI <- function(an, verb = 1) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   
   ans <- 0
   
   zz <- paste(brik.AFNI.name(an),'.Z', sep='');
   if (file.exists(zz)) {
      if (verb) {
         cat ('Uncompressing', zz, '\n');
      }
      system(paste('uncompress', zz));
   }

   zz <- paste(brik.AFNI.name(an),'.gz', sep='');
   if (file.exists(zz)) {
      if (verb) {
         cat ('gzip -d-ing', zz, '\n');
      }
      system(paste('gzip -d', zz));
   }
   
   
   return(an);
}
  
show.AFNI.name <- function(an) {
   cat ('\n',
        'uname=', an$orig_name, '\n',
        'type =', an$type,'\n',
        'pref.=', an$prefix, '\n',
        'view =', an$view, '\n',
        'head =', head.AFNI.name(an), '\n',
        'brik =', brik.AFNI.name(an), '\n',
        'brsel=', an$brsel, '\n',
        'rosel=', an$rosel, '\n',
        'rasel=', an$rasel, '\n',
        'insel=', an$insel, '\n',
        'compr=', compressed.AFNI.name(an), '\n',
        'exist=', exists.AFNI.name(an), '\n');
}

#------------------------------------------------------------------
# Functions to parse command-line arguments
#------------------------------------------------------------------
value.AFNI.args <- function(name, ops) {
   ifnd <- which(name == names(ops));
   if (length(ifnd)) {
      vv <- vector(typeof(ops[ifnd][[1]]));
      for (i in 1:length(ifnd)) {
         vv <- c (vv,ops[i][[1]])
      }
      return(vv);
   } 
   return(NULL);
}

show.AFNI.args <- function (ops, verb=0, adieu=FALSE, hstr='') {
   if (is.null(ops)) {
      cat ('NULL options\n');
   } else {
      cat (hstr,'Allowed Options:\n');
      if (length(ops[['allowed_options']])) {
         for (i in 1:length(ops[['allowed_options']])) {
            cat (' ', ops[['allowed_options']][i], '\n');
         }
      } else {
         cat ('whatever grinds your beans');
      }
      if (verb) {
         cat (hstr, 'User options:\n');
         for (i in 1:length(ops)) {
            if ((names(ops)[i] != 'allowed_options')) {
               cat (' ', names(ops)[i], ': ', ops[[i]],'\n');
            }
         }
      }
   }
   if (adieu) exit.AFNI(0)
}

check.AFNI.args <- function ( ops, params = NULL) {
   if (!is.null(params) && !is.null(ops)) {
      for (i in 1:length(ops)) {
         #str(names(ops)[i])
         #str(params[names(ops)[i]][[1]])
         #cat('\nChecking on ', paste(ops[[i]],collapse=','),'\n');
         ipar <- which(names(ops)[i] == names(params));
         if (length(ipar)) {
            pp <- params[ipar[1]][[1]]['count'][[1]];
            opsvec <- ops[[i]];
            if (length(pp) == 1) { #exact number 
               if (length(opsvec) !=  pp) {
                  msg <- paste( 'Expecting ',pp, ' parameters for option "',
                                 names(ops)[i], '".\n  Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = '') 
                  if (length(opsvec) > 0 && grep('^-[a-z,A-Z]', opsvec[1])) {
                     msg <- paste( msg, '\n Also note that ', opsvec[1], 
                                        ' is not a recognized option.',
                                        collapse = '', sep = '' );  
                  }
                  err.AFNI(msg);
                  return(0);                  
               }
            } else if (length(pp) == 2) { #range
               if (length(opsvec) <  pp[1] || length(opsvec) >  pp[2]) {
                  if (pp[2] == Inf) {
                     msg <- paste( 'Expecting more than ',pp[1],  
                                 ' parameters for option "',
                                 names(ops)[i], '".\n  Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = '') 
                     if (length(opsvec) > 0 && grep('^-[a-z,A-Z]', opsvec[1])) {
                        msg <- paste( msg, '\n Also note that ', opsvec[1], 
                                          ' is not a recognized option.',
                                          collapse = '', sep = '' );  
                     }
                     err.AFNI(msg);
                  } else {
                     msg <- paste( 'Expecting ',pp[1], ' to ', pp[2], 
                                 ' parameters for option "',
                                 names(ops)[i], '".\n  Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = '');
                     if (length(opsvec) > 0 && grep('^-[a-z,A-Z]', opsvec[1])) {
                        msg <- paste( msg, '\n Also note that ', opsvec[1], 
                                          ' is not a recognized option.',
                                          collapse = '', sep = '' );  
                     }
                     err.AFNI(msg);
                  }
                  return(0);                  
               }
               
            } else {
               warning(paste( 'I do not know what to do here'),
                        immediate. = TRUE);
               return(0);   
            }  
         } else {
            #ok
         } 
      }
   }
   return(1); #all ok
}

apl <- function ( n = 0, d=NA, h=NULL, dup=FALSE ) {
   return(list('count'=n, 'default'=d, help=h, duplicate_ok=dup));
}

load.debug.AFNI.args <- function ( fnm = NULL) {
   if (is.null(fnm)) {
      fnm <- system('ls .*.dbg.AFNI.args', ignore.stderr = TRUE, intern=TRUE)
   }
   if (length(fnm) > 1) {
      err.AFNI(paste("More than one argument file found:", 
                     paste(fnm, collapse=' ', sep=' ')));
      return(FALSE);
   }else if (length(fnm) == 0) {
      err.AFNI("No files to load");
      return(FASLE);
   }
   load(fnm)
   if (exists('args')) {
      note.AFNI(sprintf("Setting .DBG_args from args in %s", fnm));
      .DBG_args <<- args 
   } else {
      err.AFNI(sprintf("Variable args not in %s", fnm));
      return(FALSE)
   }
   return(TRUE)
}

parse.AFNI.args <- function ( args, params = NULL, 
                              other_ok=TRUE,
                              verb = 0) {
   #for (i in 1:length(args)) {
   #   cat (i, args[[i]],'\n');
   #}
   if (!is.null(params)) {
      allowed_options <- sort(names(params));
      duplicate_okvec <- vector('character');
      for (i in 1:1:length(params)) {
         pl <- params[i][[1]];
         if (pl['duplicate_ok'][[1]]) {
            duplicate_okvec <- c(duplicate_okvec, names(params)[i])
         }
      }
   } else {
      allowed_options <- vector('character');
   }
   
   #find locations of -*
   ii <- grep ('^-.*', args);
   iflg <- vector('numeric')
   for (i in 1:length(ii)) {
      if (!is.num.string(args[[ii[i]]])) {
         if (!length(allowed_options)) {
            iflg <- append(iflg, ii[i]);
         } else { #Make sure it is an acceptable name
            if (length(which(args[[ii[i]]] == allowed_options))) {
               iflg <- append(iflg, ii[i]);
            }
         }
      }     
   }
   
   if (verb) paste(args[iflg])
   
   ops = list()
   used <- vector('logical', length(args));
   if (length(iflg)) {
      iflg <- append(iflg,length(args)+1)
      #store results
      nm <- vector('character');
      for (i in 1:(length(iflg)-1)) {
         if (0) { # Why remove the -?, makes things inconsistent elsewhere
            #newnm <- strsplit(args[[iflg[i]]],'-')[[1]][2] 
         } else {
            newnm <- args[[iflg[i]]]
         }
         if (length(nm) && length(which(newnm == nm)) &&
             (!length(duplicate_okvec) || 
               length(which(iflg[i] == duplicate_okvec))) ){
            warning(paste('option ', newnm, 'already specified.\n'),
                     immediate. = TRUE);
            show.AFNI.args(ops)
            return(NULL); 
         }
         nm <- append(nm, newnm)
         
         used[iflg[i]] <- TRUE;
         istrt = iflg[i]+1;
         pp <- vector('character');
         if (istrt <= length(args) && istrt != iflg[i+1]) {
            iend <- max(c(iflg[i+1]-1, istrt));
            for (ii in istrt:iend) {
               pp <- append(pp, args[[ii]]);
               used[ii] <- TRUE;
            }
         }
         #create a cleaned up string
         pp <- paste(pp, collapse = ' ')
         pp <- strsplit(clean.args.string(pp), ' ')
         ops <- c(ops, (pp));
         names(ops)[length(ops)] <- newnm;
      }
   }
   
   #cleanup
   if (length(ops)) {
      for (i in 1:length(ops)) {
         #ops[[i]] <- clean.args.string(ops[[i]])
      }
   }
   
   #numeric changes 
   if (length(ops)) {
      for (i in 1:length(ops)) {
         if (is.num.string(ops[[i]])) {
            ops[[i]] <- as.numeric(ops[[i]]);
         }
      }
   }
   
   #defaults
   pp <- c(args[used == FALSE])
   ops <- c (ops, list("other"=pp));
   
   #add allowed options
   ops <- c (ops, list("allowed_options"=allowed_options));
   
   if (!other_ok) {
      if (length(ops[['other']])) {
         err.AFNI(paste('Illegal parameters on command line:\n',
                        '      ', ops['other'],
                        '\nTry -allowed_options, or -help for details\n',
                        '\n'));
         exit.AFNI(1); 
      }
   }
   
   #check 
   if (!check.AFNI.args(ops, params)) {
      return(NULL);
   } else {
      return(ops);
   }
}

#------------------------------------------------------------------
#   Some utilities
#------------------------------------------------------------------
#history and grep
hgrep <- function (pattern=NULL){
   if (is.null(pattern)) {
      history()
   } else {
      history(max.show=Inf, pattern=pattern)
   }
} 

#Report objects using the most memory

R.bit.version <- function () {
  if (.Machine$sizeof.pointer == 4) return(32)
  else if (.Machine$sizeof.pointer == 8) return(64)
  else return(0)
}

memory.hogs <- function (n=10, test=FALSE) {
   if (test) {
      toy <- function() { g <- array(0,c(128,128,128,10)); 
                           memory.hogs(test=FALSE) }
      moy <- function() {k <- array(0,c(128, 128, 128, 3)); toy() }
      moy()
      return(NULL)
   }
   us <- 1024*1024
   nfr <- sys.nframe()
   z<- NULL
   pp <- 0
   while (nfr > 0) {
      nfr <- nfr -1
      pp <- pp+1
      envir <- parent.frame(pp)
      envfunc <- (as.character(sys.call(-(pp))))
      if (!length(envfunc)) envfunc <- 'GLOBAL'
      zs <- sapply(ls(envir), function(x)
                    object.size(get(x, envir = envir)))
      names(zs) <- paste(names(zs)," (in ",envfunc,")")
      z <- c(z, zs)
   }
   m <- as.matrix(rev(sort(z))[1:n]/us)
   colnames(m) <- c("Size(Mb)")
   m <- rbind(m,'        total' = sum(m));
   m <- rbind(m,'  Grand total' = sum(z)/us);
   
   cat( sprintf("Memory hogs check from function: %s\nR-%d bit version\n", 
                     as.character(paste(sys.call(-1), collapse=' ')), 
                     R.bit.version()) );
   print(m, digits=2)
   invisible(m)
}

who.called.me <- function () {
   caller <- as.character(sys.call(-2))
   callstr <- paste( caller[1],'(',
                     paste(caller[2:length(caller)], collapse=','),
                     ')', sep='')
   return(callstr)
}

#print warnings a la AFNI
warn.AFNI <- function (str='Consider yourself warned',callstr=NULL, 
                       newline=TRUE) {
   if (is.null(callstr)) callstr <- who.called.me()
   nnn<-''
   if (newline) nnn <- '\n'
   if (BATCH_MODE) ff <- stderr()
   else ff <- ''
   cat(  '\n', 'oo Warning from: ',  callstr,':\n   ', 
         paste(str, collapse=''), nnn, 
       sep='', file = ff);
}

err.AFNI <- function (str='Danger Danger Will Robinson',callstr=NULL, 
                      newline=TRUE) {
   if (is.null(callstr)) callstr <- who.called.me()
   nnn<-''
   if (newline) nnn <- '\n'
   if (BATCH_MODE) ff <- stderr()
   else ff <- ''
   cat(  '\n', '** Error from: ',  callstr,':\n   ', 
         paste(str, collapse=''), nnn, 
       sep='', file = ff);
}

note.AFNI <- function (str='May I speak frankly?',callstr=NULL, newline=TRUE) {
   if (is.null(callstr)) callstr <- who.called.me()
   nnn<-''
   if (newline) nnn <- '\n'
   if (BATCH_MODE) ff <- stderr()
   else ff <- ''
   cat(  '\n', '** Note from: ',  callstr,':\n   ', 
         paste(str, collapse=''),nnn, 
       sep='', file = ff);
}

errex.AFNI <- function (str='Alas this must end',callstr=NULL, newline=TRUE) {
   err.AFNI(str,callstr=who.called.me(), newline)
   exit.AFNI(stat=1)
}

exit.AFNI <- function(str='The piano has been drinking.', stat=0) {
   quit(save='no', status = stat);
}

#return 1 if all strings in vector ss can be changed to numbers
is.num.string <- function(ss) {
   if (is.null(ss) || !length(ss) || ss == '' ||
       is.null(tryCatch(as.numeric(ss), 
                           warning=function(ex) {}))) {
      return(0);
   } else {
      return(1);
   }
}

clean.args.string <- function(ss) {
   if (is.list(ss) || length(ss) > 1) {
      warning(paste('Function only works on single strings', 
                    str(ss),'\n', sep=''),
                  immediate.=TRUE);
      return(NULL);
   }
   #remove trailing whites
   ss <- sub('^[[:space:]]*','',ss);
   ss <- sub('[[:space:]]*$','',ss);  
   #remove multiple whites
   ss <- gsub('[[:space:]]+',' ',ss);  
   #treat = nicely
   ss <- gsub('[[:space:]]*=[[:space:]]*','=',ss)
   return(ss)
}
 
as.num.vec <- function(ss, addcount=TRUE, sepstr='.', reset=FALSE) {
   if (is.list(ss) || length(ss) > 1) {
      warning(paste('Function only works on single strings', 
                    str(ss),'\n', sep=''),
                  immediate.=TRUE);
      return(NULL);
   }
   ss <- clean.args.string(ss)
   dd <- strsplit(ss,' ')[[1]];
   nn <- vector('numeric');
   ww <- vector('character');
   lastname <- '.v'
   valnum <- 0
   for (ii in 1:length(dd)) {
      vv <- strsplit(dd[ii],'=')[[1]];
      if (length(vv) > 1) {
         valnum <- valnum+1
         ll <- vv[1] 
         vv <- as.numeric(vv[length(vv)]);
         if (is.na(vv)) { return(NULL); }
         lastname <- ll
      } else {
         valnum <- valnum+1
         vv <- as.numeric(vv[1]);
         if (is.na(vv)) { return(NULL); }
         if (addcount) {
            sfnd <- paste(lastname, sepstr,'[[:digit:]]*$', sep='', collapse='')
            if (!reset) {
               ifnd <- grep(sfnd,ww);
            } else {
               ifnd <- grep(sfnd,ww[length(ww)]);
               if (length(ifnd)) {
                  ifnd <- length(ww);
               } else {
                  valnum <- 1
               }
            }
            if (length(ifnd)) {
               lastval <- strsplit(ww[ifnd[length(ifnd)]],
                                    paste(lastname, sepstr,sep=''))[[1]];
               if (lastval[length(lastval)] == '') {
                  valnum <- 1
               } else {
                  valnum <- as.numeric(lastval[length(lastval)]) + 1
               }
            }  
            ll <- paste(lastname,sepstr, as.numeric(valnum), sep='')
         } else {
            ll <- paste(lastname, sep='')
         }
      }
      
      nn <- c(nn,vv)
      ww <- c(ww,ll)
   }
   names(nn) <- ww
   return(nn)
}
as.char.vec <- function(ss) {
   if (is.list(ss) || length(ss) > 1) {
      warning(paste('Function only works on single strings', 
                    str(ss),'\n', sep=''),
                  immediate.=TRUE);
      return(NULL);
   }
   ss <- clean.args.string(ss)
   dd <- strsplit(ss,' ')[[1]];
   nn <- vector('character');
   ww <- vector('character');
   for (ii in 1:length(dd)) {
      vv <- strsplit(dd[ii],'=')[[1]];
      if (length(vv) > 1) ll <- vv[1] 
      else ll <- paste('v',as.character(vv[1]), sep='')
      
      vv <- as.character(vv[length(vv)]);
      if (is.na(vv)) { return(NULL); }
      
      nn <- c(nn,vv)
      ww <- c(ww,ll)
   }
   names(nn) <- ww
   return(nn)
}

#------------------------------------------------------------------
#   Functions to read 1D and other tables
#------------------------------------------------------------------
read.AFNI.matrix.test <- function(verb=1) {
      cat ( 'Running read.AFNI.matrix in test mode\n',
            'See read.AFNI.matrix.test for details' )
      i <- 0
         mm <- paste ( 'subj age weight height\n',
                       'joe   13  299  123   \n',
                       'jane  22  600   234   \n',
                       'jim   2   188   23\n',
                      sep='', collapse='');
         fouts <- sprintf('___fout%02d.1D', i);
         cat (mm,file=fouts);
         
         comm <- 'read.AFNI.matrix(fouts, verb=verb)'
         note.AFNI(sprintf("Working with fouts=%s:\n   %s", fouts, comm))
         print(eval(parse(text=comm)))
         
         comm <- paste("read.AFNI.matrix(fouts, verb=verb,", 
                                         "userrownames=c('jim','jane'))",
                        sep='', collapse='')
         note.AFNI(sprintf("Working with fouts=%s:\n   %s", fouts, comm))
         print(eval(parse(text=comm)))
         
         comm <- paste("read.AFNI.matrix(fouts, verb=verb, ", 
                                         "userrownames=c('jim','jane'),", 
                                         "usercolnames=c('weight','age'))",
                        sep='', collapse='')
         print(eval(parse(text=comm)))
         
      i<- i+1
         mm <- paste ( ' age weight height\n',
                       ' 13  299  123   \n',
                       ' 22  600   234   \n',
                       ' 2   188   23\n',
                      sep='', collapse='');
         fouts <- sprintf('___fout%02d.1D', i);
         cat (mm,file=fouts);
         note.AFNI(sprintf("Working with %s", fouts))
         
         comm <- 'read.AFNI.matrix(fouts, verb=verb)'
         note.AFNI(sprintf("Working with fouts=%s:\n   %s", fouts, comm))
         print(eval(parse(text=comm)))
         
         comm <- paste("read.AFNI.matrix(fouts, verb=verb,", 
                                   "usercolnames=c('height','weight,','age'))",
                        sep='', collapse='')
         note.AFNI(sprintf("Working with fouts=%s:\n   %s", fouts, comm))
         print(eval(parse(text=comm)))
         
      i<- i+1
         mm <- paste ( ' 13  299  123   \n',
                       ' 22  600   234   \n',
                       ' 2   188   23\n',
                      sep='', collapse='');
      
         fouts <- sprintf('___fout%02d.1D', i);
         cat (mm,file=fouts);
         note.AFNI(sprintf("Working with %s", fouts))
         print(read.AFNI.matrix(fouts, verb=verb))
         print(read.AFNI.matrix(fouts, verb=verb, usercolnames=c('col02'), 
                                 userrownames=c('row03','row01','row01')))
                                 
      i<- i+1
         mm <- paste ( '  heft   \n',
                       '  299    \n',
                       '  600     \n',
                       '  188     \n',
                      sep='', collapse='');
         fouts <- sprintf('___fout%02d.1D', i);
         cat (mm,file=fouts);
         note.AFNI(sprintf("Working with %s", fouts))
         print(read.AFNI.matrix(fouts, verb=verb))
         print(read.AFNI.matrix(fouts, verb=verb, usercolnames=c('heft')))
         
      i<- i+1
         mm <- paste ( '  299    \n',
                       '  600     \n',
                       '  188     \n',
                      sep='', collapse='');
         fouts <- sprintf('___fout%02d.1D', i);
         note.AFNI(sprintf("Working with %s", fouts))
         cat (mm,file=fouts);
         print(read.AFNI.matrix(fouts, verb=verb))
         
      for (j in 0:i) {   
         system(sprintf('\\rm -f ___fout%02d.1D', i));
      }
}

read.AFNI.matrix <- function (fname, 
                              usercolnames=NULL, 
                              userrownames=NULL,
                              verb = 0) {
   if (fname == '-self_test') {
      read.AFNI.matrix.test()
      return(NULL);
   }
   if (verb) print(who.called.me())
   
   ttt <- read.table(fname, colClasses='character');
   if ( tolower(ttt$V1[1]) == 'name' || 
        tolower(ttt$V1[1]) == 'subj' ||
        tolower(ttt$V1[1]) == '#file') {
      subjCol <- ttt$V1[2:dim(ttt)[1]]; 
      covNames <- paste(ttt[1,2:dim(ttt)[2]]);
      if (dim(ttt)[2]-1 == 1) {
         covMatrix <- as.matrix(as.numeric(ttt[2:dim(ttt)[1],2]))
      } else {
         for (ii in 1:(dim(ttt)[2]-1)) { #Add one column at a time
            if (ii==1) {
               covMatrix <- cbind(
                  as.numeric(ttt[2:dim(ttt)[1],2:dim(ttt)[2]][[ii]]));
            } else {
               covMatrix <- cbind(covMatrix,
                  as.numeric(ttt[2:dim(ttt)[1],2:dim(ttt)[2]][[ii]]));
            }
         }
      }
   }  else {
      flg <- tryCatch({as.numeric(ttt$V1[1])}, warning=function(aa) {});
      if (is.null(flg)) { #Just labels
         covNames <- paste(ttt[1,1:dim(ttt)[2]]);
         subjCol <- paste('row',sprintf('%02d',c(1:(dim(ttt)[1]-1))), sep='')
         istrt<- 2
      }else {  #completely naked
         covNames <- paste('col',sprintf('%02d',c(1:dim(ttt)[2])),sep='');
         subjCol <- paste('row',sprintf('%02d',c(1:dim(ttt)[1])), sep='')
         istrt<- 1
      }
      if (dim(ttt)[2] == 1) {
         covMatrix <- as.matrix(as.numeric(ttt[istrt:dim(ttt)[1],1]))
      } else {
         for (ii in 1:(dim(ttt)[2])) { #Add one column at a time
            if (ii==1) {
               covMatrix <- cbind(
                  as.numeric(ttt[istrt:dim(ttt)[1],1:dim(ttt)[2]][[ii]]));
            } else {
               covMatrix <- cbind(covMatrix,
                  as.numeric(ttt[istrt:dim(ttt)[1],1:dim(ttt)[2]][[ii]]));
            }
         }
      }
   } 


   if (verb>2) {
      browser()
   }
   rownames(covMatrix) <- subjCol;
   colnames(covMatrix) <- covNames;
   
   #Now, reorder per user*names 
   if (!is.null(userrownames)) {
      dd <- userrownames[!(userrownames %in% subjCol)]
      if (length(dd)) {
         warning (paste('Row(s) "', paste(dd,collapse=' '),
                        '" do(es) not have an entry.\n'),
                  immediate.=TRUE);
         return(NULL);
      }
      
      for (ii in 1:1:length(userrownames)) {
         if (ii==1) {
            mm <- rbind(covMatrix[userrownames[ii],]);
         } else {
            mm <- rbind(mm, covMatrix[userrownames[ii],]);
         }
      }
      rownames(mm) <- userrownames
      covMatrix <- mm
   }
   if (!is.null(usercolnames)) {
      dd <- usercolnames[!(usercolnames %in% covNames)]
      if (length(dd)) {
         warning (paste('Column(s) "', paste(dd,collapse=' '),
                        '" do(es) not have an entry.\n'),
                  immediate.=TRUE);
         return(NULL);
      }
      
      for (ii in 1:1:length(usercolnames)) {
         if (ii==1) {
            mm <- cbind(covMatrix[,usercolnames[ii]]);
         } else {
            mm <- cbind(mm, covMatrix[,usercolnames[ii]]);
         }
      }
      colnames(mm) <- usercolnames
      covMatrix <- mm
   }

   return(covMatrix)
}   

#------------------------------------------------------------------
# Extracted (and modified) from fmri library by Karsten Tabelow, 
# tabelow@wias-berlin.de and Joerg Polzehl (polzehl@wias-berlin.de)
#
# Updates by ZSS & GC
#------------------------------------------------------------------
orcode.AFNI <- function(orstr) {
   if (is.character(orstr)) {
      orcode <- c (-1,-1,-1)
      orstr <- strsplit(orstr,'')[[1]]
      for (i in 1:3) {
         switch (tolower(orstr[i]),
                  r = orcode[i] <- 0,
                  l = orcode[i] <- 1,          
                  p = orcode[i] <- 2,          
                  a = orcode[i] <- 3,          
                  i = orcode[i] <- 4,          
                  s = orcode[i] <- 5)
         if (orcode[i] < 0) {
            err.AFNI(paste('Bad orientation code ', orstr)); 
            return(NULL)
         }           
      }  
   } else {
      for (i in 1:3) {
         orcode <- orstr
         if (length(orcode) != 3 || orcode[i] < 0 || orcode[i] > 5) {
            err.AFNI(paste('Bad orientation code ', orstr)); 
            return(NULL)
         }
      }
   }
   return(orcode);
}
read.AFNI <- function(filename, verb = 0, ApplyScale = 1, PercMask=0.0) {
  an <- parse.AFNI.name(filename);
  
  if (verb) {
   show.AFNI.name(an);
  }
  
  #If you have any selectors, use 3dbucket to get what you want, then read
  #temp dset. This is an ugly fix for now, but will change it later if
  #I/O is issue
  
  if (verb) { cat ('Need tmp?\n'); }
  rmtmp <- 0;
  if (!is.null(an$brsel) || !is.null(an$rosel) ||  !is.null(an$rasel)) {
    rmtmp <- 1;
    com <- paste ('3dcalc -overwrite -prefix ___R.read.AFNI.' ,
               basename(an$prefix), 
               ' -a "', filename,'" -expr "a" >& /dev/null', 
               sep = '');
    if (try(system(com)) != 0) {
      warning(paste("Failed to execute:\n   ", com),
              immediate. = TRUE);
      return(NULL);
    }
    an$prefix <- paste('___R.read.AFNI.',basename(an$prefix), sep = '');
    if (!(exists.AFNI.name(head.AFNI.name(an)))) {
      warning(paste("Failed to create:   ", 
                     head.AFNI.name(an), brik.AFNI.name(an), '\n'),
              immediate. = TRUE);
      return(NULL);
    }
  }
  
  if (verb) { cat ('Checking existence\n'); }
  if (!(exists.AFNI.name(head.AFNI.name(an)))) {
    err.AFNI(paste("Failed to read:   ", head.AFNI.name(an), 
                                         brik.AFNI.name(an)));
    return(NULL);
  }
  
  #Cannot read compressed stuff (see size usage below)
  if (verb) { cat ('Uncompressing\n'); }
  uncompress.AFNI(head.AFNI.name(an));
  
  conhead <- file(head.AFNI.name(an),"r")
  header <- readLines(conhead)
  close(conhead)

  types <- NULL
  args <- NULL
  counts <- NULL
  values <- NULL
  
  for (i in 1:length(header)) {
    if (regexpr("^type *= *", header[i]) != -1) {
      tmptype <- strsplit(header[i]," *= *")[[1]][2]
      types <- c(types,tmptype)
      args <- c(args,strsplit(header[i+1]," *= *")[[1]][2])
      tmpcounts <- as.numeric(strsplit(header[i+2]," *= *")[[1]][2])
      counts <- c(counts,tmpcounts)
      i <- i+3
      tmpvalue <- ""
      while ((regexpr("^$", header[i]) == -1) && (i <= length(header))) {
        tmpvalue <- paste(tmpvalue,header[i])
        i <- i+1
      }
      tmpvalue <- sub("^ +","",tmpvalue)
      if ((tmptype == "integer-attribute") || (tmptype == "float-attribute")) {
        tmpvalue <- as.numeric(strsplit(tmpvalue," +")[[1]])
      }
      values <- c(values,list(value=tmpvalue))
    }        
  }

  names(values) <- args

  dx <- values$DATASET_DIMENSIONS[1]
  dy <- values$DATASET_DIMENSIONS[2]
  dz <- values$DATASET_DIMENSIONS[3]
  dt <- values$DATASET_RANK[2]
  scale <- values$BRICK_FLOAT_FACS

  size <- file.info(brik.AFNI.name(an))$size/(dx*dy*dz*dt)

  if (regexpr("MSB",values$BYTEORDER_STRING[1]) != -1) {
    endian <- "big"
  } else {
    endian <- "little"
  }

  if (min(abs(values$DELTA)) != 0) {
    weights <-
      abs(values$DELTA/min(abs(values$DELTA)))
  } else {
    weights <- NULL
  }

#  browser()
   if (verb) { cat ('Reading Bin\n'); }
   if (as.integer(size) == size) {
      conbrik <- file(brik.AFNI.name(an),"rb")
      # modified below by GC 12/2/2008
      if (all(values$BRICK_TYPES==0) | all(values$BRICK_TYPES==1)) {
         myttt<- readBin( conbrik, "int", n=dx*dy*dz*dt, size=size, 
                          signed=TRUE, endian=endian) 
      } else if (all(values$BRICK_TYPES==3)) {
         myttt<- readBin(conbrik, "numeric", n=dx*dy*dz*dt, size=size, 
                         signed=TRUE, endian=endian) # float        
      } else {
         err.AFNI("Cannot read datasets of multiple data types");
         close(conbrik)
         return(NULL);
      }  
      close(conbrik)
      dim(myttt) <- c(dx,dy,dz,dt)

      if (ApplyScale) {
         if (verb) { cat ('Scaling\n'); }
         #After this operation, size of mytt doubles if initially read as int
         for (k in 1:dt) if (scale[k] != 0) myttt[,,,k] <- scale[k] * myttt[,,,k]
      }

      mask=NULL;
      if (PercMask > 0.0) { #ZSS: Dunno what that is for. 
                            #     0.75 was default for PercMask
         mask <- array(TRUE,c(dx,dy,dz))
         mask[myttt[,,,1] < quantile(myttt[,,,1],PercMask)] <- FALSE
      } 
      z <- list(ttt=myttt,format="HEAD/BRIK",delta=values$DELTA,
            origin=values$ORIGIN,
            orient=values$ORIENT_SPECIFIC,
            dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)
  } else {
    warning("Error reading file: Could not detect size per voxel\n")
    z <- list(ttt=NULL,format="HEAD/BRIK",delta=NULL,
              origin=NULL,orient=NULL,dim=NULL,weights=NULL,
              header=values,mask=NULL)    
  }

  class(z) <- "fmridata"
  attr(z,"file") <- paste(filename,".HEAD/BRIK",sep="")
  
  if (rmtmp == 1) {
   if (verb) {
      cat ('ZSS: Will remove tmp files\n');
   }
   system('\\rm -f ___R.read.AFNI.* >& /dev/null');
  }else{
   if (verb) {
      cat ('ZSS: No temps to remove\n');
   }
  }

  invisible(z);
}

#A funtion to create an AFNI header string 
AFNIheaderpart <- function(type, name, value) {
 a <- "\n"
 a <- paste(a, "type = ", type, "\n", sep="")
 a <- paste(a, "name = ", name, "\n", sep="")
 if (regexpr("string",type) == 1) {
   value <- paste("'", value, "~", sep="")
   a <- paste(a, "count = ", nchar(value) - 1, "\n", sep ="")
   a <- paste(a, value, "\n", sep="")
 } else {
   a <- paste(a, "count = ", length(value), "\n", sep ="")
   j <- 0
   while (j<length(value)) {
     left <- length(value) - j
     if (left>4) left <- 5
     a <- paste(a, paste(value[(j+1):(j+left)],collapse="  "), "\n", sep="  ")
     j <- j+5
   }
 }
 return(a);
}

#Calculate the min, and max of BRIK data y
minmax <- function(y) {
   r <- NULL;
   for (k in 1:dim(y)[4]) {
      r <- c(r,min(y[,,,k]),max(y[,,,k]))
   }; 
   return(r);
}


newid.AFNI <- function(ext=0) {
   if (ext) { #in house
      return(
         paste('GCR_',paste(
                  sample(c(rep(0:9,each=5),LETTERS,letters),22,replace=TRUE),
                  collapse=''),
               sep='') )
   } else { #Call AFNI program
      return(system('3dnewid -fun', intern=TRUE) );
   }
}

write.AFNI <- function( filename, ttt=NULL, label=NULL, 
                        note=NULL, origin=NULL, delta=NULL,
                        orient=NULL, 
                        idcode=NULL, defhead=NULL,
                        verb = 0,
                        maskinf=0) {
  
  #Set the defaults. 
  if (is.null(note)) note <- '';
  if (is.null(idcode)) idcode <- newid.AFNI(0);
  
  if (is.null(defhead)) { # No default header
     if (is.null(label)) label <- paste(c(1:dim(ttt)[4]),collapse='~');
     if (is.null(origin)) origin <- c(0,0,0)
     if (is.null(delta)) delta <- c(4,4,4)
     if (is.null(orient)) orient <- 'RAI'
  } else {  #When possible, call on default header
     if (is.null(label)) {
      if (!is.null(defhead$BRICK_LABS)) {
         label <- gsub("^'", '', defhead$BRICK_LABS);
      } else {
         label <- paste(c(1:dim(ttt)[4]),collapse='~');
      }
     }
     if (is.null(origin)) {
      if (!is.null(defhead$ORIGIN)) {
         origin <- defhead$ORIGIN;
      } else {
         origin <- c(0,0,0);
      }
     }
     if (is.null(delta)) {
      if (!is.null(defhead$DELTA)) {
         delta <- defhead$DELTA;
      } else {
         delta <- c(4,4,4);
      }
     }
     if (is.null(orient)) {
      if (!is.null(defhead$ORIENT_SPECIFIC)) {
         orient <- defhead$ORIENT_SPECIFIC;
      } else {
         orient <- 'RAI';
      }
     }
  }
  if (is.null(ttt) || !is.numeric(ttt)) {
   err.AFNI("data array improperly formatted");
   return(0);
  }      

  if (verb) {
   note.AFNI("Writing header")
  }
  an <- parse.AFNI.name(filename);
  if (is.na(an$view)) {
   err.AFNI('Bad filename');
   return(0)
  }
  conhead <- file(head.AFNI.name(an), "w")
  writeChar(AFNIheaderpart("string-attribute","HISTORY_NOTE",note),
            conhead,eos=NULL)
  writeChar(AFNIheaderpart("string-attribute","TYPESTRING","3DIM_HEAD_FUNC"),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","IDCODE_STRING",''),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","IDCODE_DATE",date()),
            conhead,eos=NULL)  
  if (an$view == '+orig') { sv <- 0 }
  else if (an$view == '+acpc') { sv <- 1 }
  else if (an$view == '+tlrc') { sv <- 2 }
  else { sv <- 0 }
  
  writeChar(AFNIheaderpart("integer-attribute","SCENE_DATA",
                            c(sv,11,1,-999,-999,-999,-999,-999)),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","ORIENT_SPECIFIC", 
            orcode.AFNI(orient)),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("float-attribute","ORIGIN",origin),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("float-attribute","DELTA",delta),
            conhead,eos=NULL)  
  
  if (maskinf) {
   if (verb) note.AFNI("Masking infs");
   ttt[!is.finite(ttt)]=0
  }
  
  mm <- minmax(ttt)
  writeChar(AFNIheaderpart("float-attribute","BRICK_STATS",mm),
            conhead,eos=NULL)
  writeChar(AFNIheaderpart("integer-attribute","DATASET_RANK",
                           c(3,dim(ttt)[4],0,0,0,0,0,0)),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","DATASET_DIMENSIONS",
                           c(dim(ttt)[1:3],0,0)),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","BRICK_TYPES",rep(1,dim(ttt)[4])),
            conhead,eos=NULL)  

  if (verb) {
   note.AFNI("Computing scaling factors");
  }
  scale <- rep(0,dim(ttt)[4])
  for (k in 1:dim(ttt)[4]) {
    scale[k] <- max(abs(mm[2*k-1]),abs(mm[2*k]))/32767
  }

  writeChar(AFNIheaderpart("float-attribute","BRICK_FLOAT_FACS",scale),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BRICK_LABS",
                           paste(label,collapse="~")),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BYTEORDER_STRING","MSB_FIRST"),
            conhead,eos=NULL)  
  close(conhead)

  # Write BRIK
  conbrik <- file(brik.AFNI.name(an), "wb")
  if (0) { #ZSS: old method, 
           # runs out of memory for large dsets
           # when scaling. Code kept here for testing
   if (verb) {
      note.AFNI("Applying scaling ")
     }
   for (k in 1:dim(ttt)[4]) {
      ttt[,,,k] <- ttt[,,,k] / scale[k]
   }
   dim(ttt) <- NULL  #Don't know why this was done here ....
     if (verb) {
      note.AFNI("Writing brik")
     }
     writeBin(as.integer(ttt), conbrik,size=2, endian="big")
   } else {
      if (verb) {
      note.AFNI("Writing /Scaling, meth 2")
     }
     #Write on sub-brick at a time to reduce memory use. 
     #      as.integer will allocate a new copy
     for (k in 1:dim(ttt)[4]) {
      writeBin(as.integer(ttt[,,,k] / scale[k]), conbrik,size=2, endian="big") 
     }
   }
   close(conbrik)
  
  return(1);
}

read.NIFTI <- function(filename) {
  fileparts <- strsplit(filename,"\\.")[[1]]
  ext <- tolower(fileparts[length(fileparts)])

  if (ext == "nii") {
    filename.nii <- filename
    filename.hdr <- paste(c(fileparts[-length(fileparts)],"hdr"),collapse=".")
    filename.img <- paste(c(fileparts[-length(fileparts)],"img"),collapse=".")
  } else if (ext == "hdr") {
    filename.hdr <- filename
    filename.img <- paste(c(fileparts[-length(fileparts)],"img"),collapse=".")
  } else if (ext == "img") {
    filename.hdr <- paste(c(fileparts[-length(fileparts)],"hdr"),collapse=".")
    filename.img <- filename
  } else {
    filename.nii <- paste(filename,".nii",sep="")
    filename.hdr <- paste(filename,".hdr",sep="")
    filename.img <- paste(filename,".img",sep="")
  }

  if ((ext != "hdr") && (ext != "img") && (!is.na(file.info(filename.nii)$size))) {
    con <- file(filename.nii,"rb")
    header <- read.NIFTI.header(con)
    if (!(header$magic == "n+1") && !(header$magic == "ni1")) 
      warning("Hmmm! Dont see the magic NIFTI string! Try to proceed, but maybe some weird results will occur!");
    bytes <- header$voxoffset - 348
    header$extension <- readBin(con,"raw",bytes)
  } else {
    if (is.na(file.info(filename.hdr)$size) | (file.info(filename.hdr)$size < 348))
      stop("Hmmm! This does not seem to be a NIFTI header (hdr/img-pair)! Wrong size or does not exist!");
    con <- file(filename.hdr,"rb")
    header <- read.NIFTI.header(con)
    header$extension <- NULL  
    close(con)
    if (is.na(file.info(filename.img)$size))     
      stop("Hmmm! This does not seem to be a NIFTI header (hdr/img-pair)! img-file not found!");
    con <- file(filename.img,"rb")
  }
    
  dx <- header$dimension[2]
  dy <- header$dimension[3]
  dz <- header$dimension[4]
  dt <- header$dimension[5]
  endian <- header$endian
  if (header$datatype == 1) { # logical
    what <- "raw"
    signed <- TRUE
    size <- 1
  } else if (header$datatype == 2) { # unsigned char????
    what <- "int"
    signed <- FALSE
    size <- if (header$bitpix) header$bitpix/8 else 2
  } else if (header$datatype == 4) { # signed short
    what <- "int"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 2
  } else if (header$datatype == 8) { # signed integer
    what <- "int"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 4
  } else if (header$datatype == 16) { # float
    what <- "double"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 4
  } else if (header$datatype == 32) { # complex
    what <- "complex"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 8
  } else if (header$datatype == 64) { # double
    what <- "double"
    signed <- TRUE
    size <- if (header$bitpix) header$bitpix/8 else 8
  } else { # all other
    what <- "raw"
    signed <- TRUE
    size <- 1
  }
  ttt <- readBin(con, what, n=dx*dy*dz*dt, size=size, signed=signed, endian=endian) 
  close(con)

  if (min(abs(header$pixdim[2:4])) != 0) {
    weights <-
      abs(header$pixdim[2:4]/min(abs(header$pixdim[2:4])))
  } else {
    weights <- NULL
  }
  dim(ttt) <- c(dx,dy,dz,dt)

  mask <- array(TRUE,c(dx,dy,dz))
  mask[ttt[,,,1] < quantile(ttt[,,,1],0.75)] <- FALSE

  z <- list(ttt=writeBin(as.numeric(ttt),raw(),4),format="NIFTI",delta=header$pixdim[2:4],
                origin=NULL,orient=NULL,dim=header$dimension[2:5],weights=weights,header=header,mask=mask)

  class(z) <- "fmridata"

  invisible(z)
}

extract.data <- function(z,what="data") {
  if (!("fmridata"%in%class(z))) {
    warning("extract.data: data not of class <fmridata>. Try to proceed but strange things may happen")
  }
  if (what=="residuals") {  
      if(!is.null(z$resscale)){
          ttt <- readBin(z$res,"integer",prod(z$dim),2)*z$resscale 
          dim(ttt) <- z$dim
          } else {
          warning("extract.data: No residuals available, returning NULL")
          ttt <- NULL
      }
      } else { 
      ttt <- readBin(z$ttt,"numeric",prod(z$dim),4)
      dim(ttt) <- z$dim
      }
 
  invisible(ttt)
}
