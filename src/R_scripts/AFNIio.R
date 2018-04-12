#------------------------------------------------------------------
# Global Variables
#------------------------------------------------------------------
BATCH_MODE <<- 0  #Initialize batch mode flag to 0
R_io <<- -1
SHOW_TRC <<- FALSE

#------------------------------------------------------------------
# Functions for library loading
#------------------------------------------------------------------
find.in.path <- function(file) { #Pretty much same as first.in.path
   ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
   ff <- ff[lapply(ff,file.exists)==TRUE];
   aa <- gsub('//','/',ff[1], fixed=TRUE)
   if (is.na(aa)) aa <- NULL
   return(aa) 
}

#Return cat's file option to format the help output
#for TXT or sphinx purposes
help.cat.file.AFNI <- function (pname=NULL, targ='TXT') {
   if (is.null(pname)) {
      err.AFNI("NULL name for help function");
      return("");
   }
        if (targ == 'TXT') {
         dopt = '-hdoc_2_txt';
   } else if (targ == 'SPX') {
      dopt = '-hdoc_2_spx';
   } else if (targ == 'ASPX') {
      dopt = '-hdoc_2_aspx';
   } else if (targ == 'RAW') {
      return("");
   } else {
      warn.AFNI(paste("targ", targ,"unknown. Assuming 'TXT'"));
      dopt = '-hdoc_2_txt';
   }
   return(paste("|apsearch ", dopt ,pname,"-"));
}

#print warnings a la AFNI
prompt.AFNI <- function (str='I await', choices=c('y','n'), vals=NULL) {
   if (!is.null(vals) && length(vals) != length(choices)) {
      err.AFNI(paste("Have ", length(choices), "options, but",
                     length(vals), "values to return"));
      return(0)
   }
   choices[1]<-toupper(choices[1])
   kk<-vector(length=0);
   spr <- paste(str," [",paste(choices, collapse='|'),"]:", sep='')
   if (BATCH_MODE) { #Only two choices available for this one!
      if (length(choices)!=2) {
         err.AFNI("This one can't run in batch mode for more than 2 choices")
         return(0)
      }
      ss <- sys.AFNI(paste(
         "prompt_user -pause '", spr,"'", sep='' ))
      if (ss$out == "0") return(2) #The not default
      else return(1) #The default
   } else {
      while (length(kk) == 0) {
         cat(spr)
         bb <- readLines(n=1)
         if (bb == '') {
            kk <- 1;
         } else {
            kk <-which(tolower(choices) == tolower(bb))
         }
      }
      if (!is.null(vals)) {
         return(vals[kk])
      } else {
         return(kk)
      }
   }
   return(0)
}

set.AFNI.msg.trace <- function (vv=FALSE) { SHOW_TRC <<- vv }

warn.AFNI <- function (str='Consider yourself warned',
                       callstr=NULL, 
                       newline=TRUE) {
   if (is.null(callstr)) {
      if (SHOW_TRC) callstr <- who.called.me(TRUE)
      else callstr <- ''
   }
   nnn<-''
   if (newline) nnn <- '\n'
   if (BATCH_MODE) ff <- stderr()
   else ff <- ''
   cat(  '\n', 'oo Warning: ',  callstr,'\n   ', 
         paste(str, collapse=''), nnn, 
       sep='', file = ff);
}

err.AFNI <- function (str='Danger Danger Will Robinson',
                        callstr=NULL, 
                      newline=TRUE) {
   if (is.null(callstr)) {
      if (SHOW_TRC) callstr <- who.called.me(TRUE)
      else callstr <- ''
   }
   nnn<-''
   if (newline) nnn <- '\n'
   if (BATCH_MODE) ff <- stderr()
   else ff <- ''
   cat(  '\n', '** Error: ',  callstr,'\n   ', 
         paste(str, collapse=''), nnn, 
       sep='', file = ff);
}

errex.AFNI <- function (str='Alas this must end',
                        callstr=NULL, newline=TRUE) {
   if (is.null(callstr)) {
      if (SHOW_TRC) callstr <- who.called.me(TRUE)
      else callstr <- ''
   }
   err.AFNI(str,callstr, newline)
   exit.AFNI(str='\n   Execution halted',stat=1)
}

note.AFNI <- function (str='May I speak frankly?',
                       callstr=NULL, newline=TRUE, tic=1,
                       trimtrace=30) {
   if (is.null(callstr)) {
      if (SHOW_TRC)  callstr <- who.called.me(TRUE, trim=trimtrace)
      else callstr <- ''
   }
   nnn<-''
   if (newline) nnn <- '\n'
   if (BATCH_MODE) ff <- stderr()
   else ff <- ''
   if (tic == 1) {
      tm <- format(Sys.time(), " @ %H:%M:%S")
   } else if (tic==2) {
      tm <- format(Sys.time(), " @ %a %b %d %H:%M:%S %Y")
   } else tm <- ''
   
   cat(  '\n', '++ Note: ',  callstr,
         sprintf('%s\n   ', tm), 
         paste(str, collapse=''),nnn, 
       sep='', file = ff);
}

exit.AFNI <- function(str='The piano has been drinking.', stat=0) {
   if (BATCH_MODE) {
      quit(save='no', status = stat);
   } else {
      #note.AFNI(str)
      stop(str)
   }
}

#Locate and load R_io.so
set_R_io <- function() {
   rio <- 0
   ll <- find.in.path('R_io.so')
   if (!is.null(ll)) {
      dd <- try(dyn.load(ll), silent=TRUE)
      # newer versions might return R_io.so   8 Dec 2017 [rickr]
      if (dd[[1]]!="R_io" && dd[[1]]!="R_io.so") {
         warn.AFNI(paste("Failed to load R_io.so with this error message:\n"));
         dyn.load(ll)
      } else {
         rio <- 1
      }
   }
   return(rio) 
}

if (R_io == -1) {
   R_io <<- set_R_io()
}

have_R_io <- function() {
   if (R_io == 1) return(TRUE) else return(FALSE)
}

libLoad <- function(myLib) {
   sucLoad <- FALSE
   sucCheck <- FALSE
   try(sucLoad <- library(myLib, character.only = TRUE, logical.return = TRUE))
   if (sucLoad) {
      print(sprintf("Package %s successfully loaded!", myLib)); sucCheck <- TRUE
   } else {
      if (BATCH_MODE == 1) {
         err.AFNI(paste(   "Need to install package ",myLib,
            "\n   Start an interactive R session then run:\n",
            "\n      update.packages(checkBuilt=TRUE, ask=FALSE) ",
            "#Not mandatory, but recommended",
            "\n      install.packages('",myLib,"')\n",
            "\n   Then quit R and rerun your command.\n",
                        sep=''))
      } else {
         try(install.packages(myLib))
         try(sucLoad <- library(myLib, character.only = TRUE, 
                               logical.return = TRUE))
         if (sucLoad) print(sprintf("Package %s successfully loaded...", myLib)) 
   	}
   }
   return(sucLoad)
}

pkgLoad <- function(pkgs) { 	
   # install packages not already loaded:
   pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
   if (length(pkgs_miss) > 0) {
      install.packages(pkgs_miss, dep=TRUE, repos='http://watson.nci.nih.gov/cran_mirror/')
   }
    
   # load packages not already loaded:
   attached <- search()
   attached_pkgs <- attached[grepl("package", attached)]
   need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    
   if (length(need_to_attach) > 0) {
      for (i in 1:length(need_to_attach)) if(require(need_to_attach[i], character.only = TRUE))
         cat('Package ', need_to_attach[i], ' loaded successfully!\n\n', sep='')
   }
	
   if (length(need_to_attach) == 0) {
      message("\n ...All required packages were already loaded!\n")
   }
}

#------------------------------------------------------------------
# Functions to deal with AFNI file names
#------------------------------------------------------------------

strip.extension <- function (filename, extvec=NULL, verb=0) {
   n <- list()
   if (is.null(extvec)) {
      ff <- strsplit(filename, '\\.')[[1]]
      if (length(ff) > 1) {
         n$ext <- paste('.',ff[length(ff)], sep='')
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

is.AFNI.1D.string <- function(t) {
   if (length(grep('^1D:',t))) return(TRUE)
   return(FALSE)
}

eval.AFNI.string.help <- function() {
   return("
Data Strings:
-------------
You can specify input matrices and vectors in a variety of
ways. The simplest is by specifying a .1D file with all 
the trimmings of column and row selectors. You can also
specify a string that gets evaluated on the fly. 
For example: '1D: 1 4 8' evaluates to a vector of values 1 4 and 8.
Also, you can use R expressions such as: 'R: seq(0,10,3)'   
")
}

eval.AFNI.1D.string <- function (t, verb=0, nmax=0) {
   #remove 1D:
   t <- sub("^1D:","",t)
   #any transpose?
   doTr = FALSE
   if (length(grep("'$",t))) {
      t<-sub("'$",'',t)
      doTr = TRUE
   }
   #replace commas with space
   t<-gsub(",",' ',t)
   
   #remove multiple blanks
   t <- deblank.string(t, middle=TRUE)

   vvf <- vector(length = 0, mode="numeric")
   #replace .. with : and split at 'space'
   s <- strsplit(sub("..",":",t, fixed=TRUE), " ")[[1]]

   #replace $ with nmax if possible
   s <- gsub("[$]",as.character(nmax),s)
    
   #Now loop and form vector of components
   for (si in s) {
      if (verb) cat ("working ", si, "\n")
      if (length(grep(":",si))) {
         sss <- strsplit(si,'[(*)]')[[1]]
         if (length(sss)>1) {
            se <- sss[2]
         
            si <- strsplit(sss[1],":")[[1]]
            vv <- eval(parse(text=sprintf('seq(from=%s,to=%s,by=%s)',
                                          si[1], si[2], se) ))
         }  else {
            vv <- eval(parse(text=si))
         }
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
   if (doTr) return(t(vvf))
   else return(vvf)
}

is.AFNI.R.string <- function(t) {
   if (length(grep('^R:',t))) return(TRUE)
   return(FALSE)
}

eval.AFNI.R.string <- function (t) {
   t <- sub("^R:","",t)
   return(eval(parse(text=t)))
}

eval.AFNI.string <- function (t) {
   if (is.AFNI.1D.string(t)) return(eval.AFNI.1D.string(t))
   else if (is.AFNI.R.string(t)) return(eval.AFNI.R.string(t))
   else return(NULL)
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
   an$pprefix <- NULL
   an$brsel <- NULL;
   an$rosel <- NULL;
   an$rasel <- NULL;
   an$insel <- NULL;
   an$type <- NULL;
   an$path <- NULL;
   an$orig_name <- filename;
   an$file <- NULL;

   if (verb) { cat ('Parsing >>',filename,'<<\n', sep=''); }
   if (!is.character(filename)) {
      warning(paste('filename >>', 
                     filename, '<< not a character string\n', sep=''),
                 immediate. = TRUE);
      traceback();
      return(NULL);
   }
   #Deal with special names:
   if (length(grep("^1D:.*$",filename))) {
      an$type = '1Ds'
      return(an)
   } else if (length(grep("^R:.*$",filename))) {
      an$type = 'Rs'
      return(an)
   }
   
   #Deal with selectors
   n <- parse.AFNI.name.selectors(filename, verb)
   filename <- n$name
   an$file  <- n$name
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
      an$pprefix <- n$name_noext
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
      an$pprefix <- n$name_noext
   }
   
   #a prefix with no path
   an$prefix <- basename(an$pprefix)
   
   #and the path
   an$path <- dirname(an$orig_name)
   
   if (verb > 2) {
      note.AFNI("Browser not active");
      # browser()
   }
   if (  an$type != '1D' && (
         !is.null(an$brsel) || !is.null(an$rosel) || 
         !is.null(an$rasel) || !is.null(an$insel))) {
       #Remove trailing quote if any
       an$prefix <- gsub("'$", '', an$prefix); 
       an$prefix <- gsub('"$', '', an$prefix);       
       an$pprefix <- gsub("'$",'', an$pprefix); 
       an$pprefix <- gsub('"$','', an$pprefix);       
   } 

   if ( an$type != 'BRIK' ) {
      #Put the extension back on
      an$pprefix <- paste(an$pprefix,an$ext, sep='');
      an$prefix <- paste(an$prefix,an$ext, sep='');
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

#Read parameters for -dataTable options (e.g. 3dLME)
#If opts is just one string starting with '@' such as
#@PPP, the parameters are read from text file PPP and
#returned as if they were found on the command line  
dataTable.AFNI.parse <- function(opts) {
   if (is.null(opts) || length(opts) ==  0) {
      return(NULL);
   }
   if (length(opts) == 1 && length(grep('^@.*$',opts))) {
      ff <- strsplit(opts, '')[[1]]
      ff <- paste(ff[2:length(ff)], sep='', collapse='')
      if (!file.exists(ff)) {
         err.AFNI(paste("table file", ff, "does not exist"));
         return(NULL)
      }
      #Can't read as table because users might have EOL \ from shell command
      #opts <- scan(ff, what='character')
      opts <- scan(ff, what='character', na.strings="") # GC 08/15/2014: modified to allow NA in dataTable
      opts <- opts[grep('[^\\\\]',opts)]
   }

   return(opts);
}

used.AFNI.prefix <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   
   ans <- 0
   ans <- exists.AFNI.name(an)
   if (ans > 0) return(1)
   
   if (exists.AFNI.name(sprintf('%s+orig', an$pprefix))) return(1)
   if (exists.AFNI.name(sprintf('%s+tlrc', an$pprefix))) return(1)
   if (exists.AFNI.name(sprintf('%s+acpc', an$pprefix))) return(1)
   
   return(0)
}

pprefix.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   return(an$pprefix);
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
   return(paste(an$pprefix,an$view,sep=''));
}

head.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   if (an$type == 'BRIK' && !is.na(an$view)) {
      return(paste(an$pprefix,an$view,".HEAD",sep=''));
   } else {
      return((an$orig_name));
   }
}

brik.AFNI.name <- function(an) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   if (an$type == 'BRIK' && !is.na(an$view)) {
      return(paste(an$pprefix,an$view,".BRIK",sep=''));
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

modify.AFNI.name <- function (name, what="append", val="_new", cwd=NULL) {
   if (!is.loaded('R_SUMA_ParseModifyName')) {
      err.AFNI("Missing R_io.so");
      return(NULL);
   }
   an <- .Call("R_SUMA_ParseModifyName", 
               name = name, 
               what = what,
               val = val,
               cwd = cwd)
   return(an)
}

AFNI.command.history <- function(ExecName=NULL, args=NULL, ohist=NULL) {
   if (!is.loaded('R_SUMA_HistString')) {
      return(NULL);
   }
   an <- .Call("R_SUMA_HistString", ExecName, args, ohist)
   return(an)
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
   
   zz <- paste(brik.AFNI.name(an),'.bz2', sep='');
   if (file.exists(zz)) {
      if (verb) {
         cat ('bzip2 -d-ing', zz, '\n');
      }
      system(paste('bzip2 -d', zz));
   }
   
   
   return(an);
}

copy.AFNI.dset <- function(an, ancp, overwrite=FALSE, verb = 1) {
   if (is.character(an)) an <- parse.AFNI.name(an);
   if (is.character(ancp)) ancp <- parse.AFNI.name(ancp);
   
   ans <- 0
   
   if (!exists.AFNI.name(an)) {
      err.AFNI(sprintf('Dset %s not found', an$file));
      return(ans)
   }
   if (exists.AFNI.name(ancp) && !overwrite) {
      err.AFNI(sprintf('New dset %s already exists', ancp$file));
      return(ans)
   }
   opt <- ''
   if (overwrite) opt <- sprintf('%s %s', opt, -overwrite);
   com = sprintf('3dcopy %s %s', an$file, ancp$file);
   system(com, ignore.stderr = TRUE, intern=TRUE)
   if (!exists.AFNI.name(ancp)) {
      err.AFNI(sprintf('New dset %s not created', ancp$file));
      return(ans)
   }
   return(1)
}  

show.AFNI.name <- function(an) {
   cat ('\n',
        'uname=', an$orig_name, '\n',
        'file=', an$file,'\n',
        'type =', an$type,'\n',
        'pref.=', an$pprefix, '\n',
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

check.AFNI.args <- function ( ops, params = NULL, verb=0) {
   if (!is.null(params) && !is.null(ops)) {
      for (i in 1:length(ops)) {
         if (verb) {
            str(names(ops)[i])
            str(params[names(ops)[i]][[1]])
            cat('\nChecking on ', paste(ops[[i]],collapse=','),'\n');
         }
         ipar <- which(names(ops)[i] == names(params));
         if (length(ipar)) {
            pp <- params[ipar[1]][[1]]['count'][[1]];
            opsvec <- ops[[i]];
            if (length(pp) == 1) { #exact number 
               if (length(opsvec) !=  pp) {
                  #browser()
                  msg <- paste( 'Expecting ',pp, ' parameters for option "',
                                 names(ops)[i], '".\n   Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = '') 
                  if (length(opsvec) > pp && 
                      length(grep('^-[a-z,A-Z]', opsvec[1+pp]))) {
                     msg <- paste( msg, '\n   NOTE that ', opsvec[1+pp], 
                              ' in bad option above is not a recognized option.',
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
                                 names(ops)[i], '".\n   Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = '') 
                     if (length(opsvec) > 0 && 
                         length(grep('^-[a-z,A-Z]', opsvec[1]))) {
                        msg <- paste( msg, '\n   NOTE that ', opsvec[1], 
                              ' in bad option above is not a recognized option.',
                                          collapse = '', sep = '' );  
                     }
                     err.AFNI(msg);
                  } else {
                     msg <- paste( 'Expecting ',pp[1], ' to ', pp[2], 
                                 ' parameters for option "',
                                 names(ops)[i], '".\n   Have ', 
                                 length(opsvec), ' parameter(s) in string "', 
                                 paste(opsvec, collapse = ' '),
                                 '" instead.', sep = '');
                     if (length(opsvec) > pp[2] && 
                         length(grep('^-[a-z,A-Z]', opsvec[1+pp[2]]))) {
                        msg <- paste( msg, '\n   NOTE that ', opsvec[1+pp[2]], 
                              ' in bad option above is not a recognized option.',
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
      fnm <- system('ls *.dbg.AFNI.args .*.dbg.AFNI.args', ignore.stderr = TRUE, intern=TRUE)
   }
   if (length(fnm) > 1) {
      err.AFNI(paste("More than one argument file found:", 
                     paste(fnm, collapse=' ', sep=' '), ".\n",
                     "Try one of:\n",
                     paste("load.debug.AFNI.args('",fnm,"')", 
                           collapse='\n', sep=''),
                     sep = ''));
      return(FALSE);
   }else if (length(fnm) == 0) {
      err.AFNI("No files to load");
      return(FASLE);
   }
   load(fnm)
   ok_auto <- FALSE
   if (exists('rfile')) {
      ss <- sprintf("\n   Start interactive code with:\n\nsource('%s')\n",
                     rfile);
      ok_auto <- TRUE
   } else {
      ss <- 
         sprintf("\n   Start interactive code with:\n\nsource('yourRfile')\n");
   }
   if (exists('args')) {
      note.AFNI(sprintf("Setting .DBG_args from args in %s%s", fnm, ss));
      .DBG_args <<- args 
   } else {
      err.AFNI(sprintf("Variable args not in %s", fnm));
      return(FALSE)
   }
   
   if (ok_auto) {
      if (prompt.AFNI("Execute source command?", c('y','n')) == 1) {
         source(rfile)
      }
   }
   return(TRUE)
}

parse.AFNI.args <- function ( args, params = NULL, 
                              other_ok=TRUE,
                              verb = 0) {
   #for (i in 1:length(args)) {
   #   cat (i, args[[i]],'\n');
   #}
   if (is.null(args)) return(NULL)
   
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
   
   #Add global args like -overwrite
   allowed_options <- c(allowed_options, "-overwrite")
   
   #find locations of -*
   ii <- grep ('^-.*', args);
   iflg <- vector('numeric')
   if (length(ii) > 0) {
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
   }
   
   if (verb) note.AFNI(paste(args[iflg]))
   
   ops = list()
   used <- vector('logical', length(args));
   if (length(iflg)) {
      iflg <- append(iflg,length(args)+1)
      #store results
      nm <- vector('character');
      for (i in 1:(length(iflg)-1)) {
         if (0) { # Why remove the -?, makes things inconsistent elsewhere
            #newnm <- strsplit(args[[iflg[i]]],'-')[[1]][2] 
         } else newnm <- args[[iflg[i]]]

         if (length(nm) && length(which(newnm == nm)) &&
             (newnm != '-gltLabel') && (newnm != '-gltCode') &&  # 10/18/2012 GC: added this line for 3dMVM
             (newnm != '-glfLabel') && (newnm != '-glfCode') &&  # 12/22/2014 GC: added this line for 3dMVM
             (!length(duplicate_okvec) || 
               length(which(iflg[i] == duplicate_okvec))) ){
            warning(paste('option ', newnm, 'already specified.\n'),
                     immediate. = TRUE);
            show.AFNI.args(ops)
            return(NULL); 
         }
         #nm <- append(nm, newnm)
         
         used[iflg[i]] <- TRUE;
         istrt = iflg[i]+1;
         pp <- vector('character');
         if (istrt <= length(args) && istrt != iflg[i+1]) {
            iend <- max(c(iflg[i+1]-1, istrt))
            for (ii in istrt:iend) {
               pp <- append(pp, args[[ii]]);
               used[ii] <- TRUE;
            }
         }
         #create a cleaned up string
         pp <- paste(pp, collapse = ' ')
         if (  length(grep('^".*"$',pp)) || #Quoted string, do not split
               length(grep("^'.*'$",pp)) || #Quoted string, do not split 
               length(grep("^1D:.*$",pp)) || #1D: string, do not split
               length(grep("^R:.*$",pp)) ) { 
         } else {
            if (verb) {
               note.AFNI(sprintf("Splitting >>>%s<<<", pp)) 
            }
            pp <- strsplit(clean.args.string(pp), ' ')
         }
         if((newnm != '-gltLabel') && (newnm != '-glfLabel') && (newnm != '-gltCode') && (newnm != '-glfCode')) { # 12/22/2014 GC: added this line for 3dMVM
            ops <- c(ops, (pp))
            names(ops)[length(ops)] <- newnm
         } else if(length(which(newnm == nm))) 
            ops[[newnm]] <- c(ops[[newnm]], pp) else {
            ops <- c(ops, list(pp))
            names(ops)[length(ops)] <- newnm
         }
         nm <- append(nm, newnm)  
      }
   }
   #cleanup
   if (length(ops)) {
      for (i in 1:length(ops)) {
         #ops[[i]] <- clean.args.string(ops[[i]])
      }
   }
   
   #numeric changes 
   if (length(ops))
      for (i in 1:length(ops))
         if(!is.list(ops[[i]])) if (is.num.string(ops[[i]]))
            ops[[i]] <- as.numeric(ops[[i]])
   
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

AFNI.new.options.list <- function(history = '', parsed_args = NULL) {
   lop <- list (com_history = history);
   #Look for defaults
   lop$overwrite <- FALSE
   for (i in 1:length(parsed_args)) {
      opname <- strsplit(names(parsed_args)[i],'^-')[[1]];
      opname <- opname[length(opname)];
      switch(opname,
             overwrite = lop$overwrite <- TRUE )
   }
   return(lop)
}

#------------------------------------------------------------------
#   Some utilities
#------------------------------------------------------------------
#history.AFNI (is almost identical to history() but without
#the interactive mode and grep
history.AFNI <- function (max.show = 25, reverse = FALSE, pattern, ...) 
{
    file1 <- tempfile("Rrawhist")
    savehistory(file1)
    rawhist <- readLines(file1)
    unlink(file1)
    if (!missing(pattern)) 
        rawhist <- unique(grep(pattern, rawhist, value = TRUE, 
            ...))
    nlines <- length(rawhist)
    if (nlines) {
        inds <- max(1, nlines - max.show):nlines
        if (reverse) 
            inds <- rev(inds)
    }
    else inds <- integer(0L)
    file2 <- tempfile("hist")
    writeLines(rawhist[inds], file2)
    #file.show(file2, title = "R History", delete.file = TRUE)
    mm <- readLines(file2)
    file.remove(file2)
    cat(mm, sep='\n')
    invisible(mm)
}

hgrep <- function (pattern=NULL){
   if (is.null(pattern)) {
      history.AFNI(max.show=200)
   } else {
      history.AFNI(max.show=Inf, pattern=pattern)
   }
} 
hsgrep <- function (pattern='source'){
   hgrep(pattern)
} 

#Report objects using the most memory

R.bit.version <- function () {
  if (.Machine$sizeof.pointer == 4) return(32)
  else if (.Machine$sizeof.pointer == 8) return(64)
  else return(0)
}

memory.hogs <- function (n=10, top_frac=0.9, test=FALSE, msg=NULL) {
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
   
   #Stop at the top_frac
   if (top_frac > 0.0) {
      m <- rev(sort(z))[1:n]
      mtot <- sum(m)
      ms <- m[1]
      nf <- 1
      while (ms /mtot < top_frac && nf <= n) {
         nf <- nf+1
         ms <- ms+m[nf]
      }
   }
   m <- as.matrix(rev(sort(z))[1:nf]/us)
   colnames(m) <- c("Size(Mb)")
   m <- rbind(m,'        total' = sum(m));
   m <- rbind(m,'  Grand total' = sum(z)/us);
   
   cat( sprintf("Memory hogs check from function: %s\nR-%d bit version\n", 
                     as.character(paste(sys.call(-1), collapse=' ')), 
                     R.bit.version()) );
   if (!is.null(msg)) cat(msg)
   print(m, digits=2)
   invisible(m)
}

newid.AFNI <-function(len=26) {  
   return(paste("XYZ_",
            paste(sample(c(rep(0:9,each=5),LETTERS, letters),
            len-4, replace=TRUE), collapse='', sep=''), sep='',collapse=''))
}

sys.AFNI <- function(com=NULL, fout=NULL, ferr=NULL, echo=FALSE,
                     CoS=errex.AFNI ) {
   ss <- list(stat=1, err='', out='')
   if (is.null(com)) return(ss)
   rmfout <- FALSE
   if (is.null(fout)) {
      fout <- sprintf('/tmp/fout.%s', newid.AFNI())
      rmfout <- TRUE
   }
   rmferr <- FALSE
   if (is.null(ferr)) {
      ferr <- sprintf('/tmp/ferr.%s', newid.AFNI())
      rmferr <- TRUE
   }
   if (echo) note.AFNI(paste("Executing shell command",com))
   ss$stat<-try(system(paste(com,' 1>', fout,' 2>', ferr, collapse='')))
   ss$out <- readLines(fout, n=-1, ok=TRUE, warn=FALSE, encoding='unknown')
   if (rmfout) unlink(fout); 
   ss$err <- readLines(ferr, n=-1, ok=TRUE, warn=FALSE, encoding='unknown')
   if (rmfout) unlink(ferr); 
   if (ss$stat && !is.null(CoS)) CoS(paste("Error status executing:\n",com))  
   invisible(ss)
}

who.called.me <- function (quiet_inquisitor=FALSE, trim = 0) {
   mm <- (as.list(sys.calls()))
   #str(mm)
   N_mm <- length(mm)
   callstr <- NULL
   if (quiet_inquisitor) skp <- 2
   else skp <- 1
   callstr <- ''
   for (i in (N_mm-skp):1 ) {
      caller <- as.character(mm[i])
      if (length(caller) == 0) {
         caller <- 'R_prompt'
      }
      if (trim==-1) { #function only
         caller <-  strsplit(caller[1],'(', fixed=TRUE)[[1]][1]
      } else if (trim>0) {
         fun <- strsplit(caller[1],'(', fixed=TRUE)[[1]][1]
         par <- strsplit(caller[1],'(', fixed=TRUE)[[1]][2]
         par <- strsplit(par,'')[[1]]
         n_char <- min(length(par),trim)
         if (n_char < length(par)) ell <- '...'
         else {
            ell <- ''
            #and remove last parentheses
            par <- sub(')$','',par)
         }
         if (is.na(par[1])) {
            caller <-  paste(fun, collapse='', sep='')
         } else {
            caller <-  paste(fun, '(', paste(par[1:n_char],collapse='', sep=''),
                          ell, ')', collapse='', sep='')
         }
      }
      spc = '    '
      if (i == N_mm-skp) {
         callstr <- paste(callstr, caller[1])
      } else {
         spc <- paste(spc, '   ', sep='')
         callstr <-paste(callstr, '\n',spc,  '-->', 
                         caller,  
                         sep= '') 
      }
   }
   #if (BATCH_MODE) caller <- as.character(sys.call(-1))
   #else caller <- as.character(sys.call(-2))
   return(callstr)
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

deblank.string <- function(s, start=TRUE, end=TRUE, middle=FALSE) {
   if (end) {
      s = sub('[[:space:]]+$','',s);
   }
   if (start) {
      s = sub('^[[:space:]]+','',s);
   }
   if (middle) {
      s = gsub('[[:space:]]+',' ',s); 
   }
   return(s);
}

pad.string.lines <- function(s, pre='   ', post=NULL) {
   if (!is.null(pre)) {
      s = sub('^',pre,s);
      s = gsub('\n',sprintf('\n%s',pre),s);
   }
   if (!is.null(post)) {
      s = sub('$',post,s);
      s = gsub('\n',sprintf('%s\n',post),s);
   }
   
   return(s);
}

trim.string <- function (s, nchar=32, left=TRUE, strim='...')
{
   ss <- strsplit(s,'')[[1]]
   if (length(ss)>nchar) {
      #try deblanking
      s <- deblank.string(s)
      ss <- strsplit(s,'')[[1]]
      nc <- length(ss)
      if (nc>nchar) {
         #browser()
         nstrim = length(strsplit(strim,'')[[1]])
         if (left) {
            ns <- nc - nchar - nstrim
            if (ns > nstrim) {
               ss <- ss[ns:nc] 
               s<-paste(strim,paste(ss,collapse=''), sep='')
            }
            return(s)
         }else {
            ns <- nchar - nstrim
            ss <- ss[1:ns]
            s<-paste(paste(ss,collapse=''), strim, sep='') 
         }
      } else return(s)
   } else return(s)
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
         wrn <- getOption('warn'); options(warn=-1);
         vv <- as.numeric(vv[1]); options(warn=wrn);
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
expand_1D_string <- function (t) {
   vvf = vector(length = 0, mode="numeric")
   #replace .. with : and split at ,
   s = strsplit(sub("..",":",t, fixed=TRUE), ",")[[1]]

   #Now loop and form vector of components
   for (si in s) {
      #cat ("working ", si, "\n")
      if (length(grep(":",si))) {
         vv <- eval(parse(text=si))
      } else if (length(grep("@",si))) {
         ssi = as.numeric(strsplit(si,"@")[[1]])
         #cat ("ssi = ", ssi, "\n")
         vv <- rep(ssi[2], ssi[1])
      } else {
         vv <- as.numeric(si)
      }
      #cat(si," = ",vv, "\n")
      vvf <- c(vvf, vv)
      #cat("vvnow = ",vvf, "\n")
   }
   return(vvf)
}

is.wholenumber.AFNI <- function(x, tol = .Machine$double.eps^0.5, all=TRUE)  {
      if (is.null(x)) return(FALSE)
      if (!all) {
         return(abs(x - round(x)) < tol)
      } else {
         return(prod(abs(x - round(x)) < tol)==1)
      }
      return(FALSE)
}

r.NI_new_element <- function (name, dat=NULL, atlist=NULL, tp=NULL){
   nel <- list (name=name, atlist=atlist, dat=NULL)
   if (is.null(tp)) {
      if (is.character(dat)) {
         tp <- "String"
      } else if (is.numeric(dat)) {
         if (is.wholenumber.AFNI(dat)) tp<-"int"
         else tp <- "float"
      }
   }
   
   if (!is.null(dat)) {
      if (is.vector(dat)) {
         nel <- r.NI_set_attribute(nel, "ni_dimen", length(dat))
         nel <- r.NI_set_attribute(nel, "ni_type", tp)
         nel$dat <- matrix(as.character(dat), length(dat),1)
      } else if (is.matrix(dat)){
         nel <- r.NI_set_attribute(nel, "ni_dimen", paste(dim(dat),collapse=','))
         nel <- r.NI_set_attribute(nel, "ni_type", 
                                    paste(rep(tp,dim(dat)[2]), collapse=','))
         nel$dat <- matrix(as.character(dat), dim(dat)[1], dim(dat)[2])
      } else {
         err.AFNI("Bad dat");
      }
      if (tp == "String") {
         nel$dat <- apply(nel$dat, c(1,2), paste)
      }
   }
   
   return(nel) 
}

r.NI_get_attribute <- function (nel,name, brsel=NULL, 
                                 colwise=FALSE, is1Dstr=FALSE,
                                 sep=' ; ', num=FALSE) {
   ffs <- NULL
   for (i in 1:length(nel$atlist)) {
      if (!is.na(nel$atlist[[i]]$lhs) && 
            nel$atlist[[i]]$lhs == name) {
         ffs <- nel$atlist[[i]]$rhs
         break
      }
   }
   
   if (is.null(ffs)) return(NULL)
   
   #ffs <- gsub("[;\"]","", ffs)
   if (colwise) { #a per column deal, process it
      #remove bad chars and split into one for each column
      if (is1Dstr) {
         ffs = expand_1D_string (ffs)
      } else {
         ffsv <- strsplit(ffs,sep)[[1]]
         #some components are blanks to be killed
         ffs <- ffsv[which(nchar(ffsv)!=0)]
      }
      if (!is.null(brsel)) {
         if (max(brsel+1) > length(ffs)) {
            err.AFNI(paste("Have ", length(ffs), 
               "attribute elements in ", paste(ffs,collapse=' '), 
               ".\nBrick selection calls for max. of ",
               max(brsel+1)," columns\n"
                       ));
         }
         ffs <- ffs[brsel+1]
         if (num) return(as.numeric(ffs))
         else return(ffs)
      } else {
         #No selection, return all
         if (num) return(as.numeric(ffs))
         else return(ffs)
      }
   } else {
      if (num) return(as.numeric(ffs))
      else return(ffs)
   }
   return(NULL)
}

r.NI_set_attribute <- function (nel,name, val) {
   ig <- -1
   
   if (length(nel$atlist)) {
      for (i in 1:length(nel$atlist)) {
         if (!is.na(nel$atlist[[i]]$lhs) && 
               nel$atlist[[i]]$lhs == name) {
            ig<-i
            break
         }
      }
   }
   if (ig > 0) {
      nel$atlist[[ig]] <- list(lhs=deblank.string(name), 
                   rhs=r.NI_dequotestring(val, db=TRUE))
   } else {
      nel$atlist <- c (nel$atlist,
         list(list(lhs=deblank.string(name), 
                   rhs=r.NI_dequotestring(val, db=TRUE))))
   }
   return(nel)
}

r.NI_dequotestring <- function(val, db=FALSE) {
   if (db) val <- deblank.string(val)
   val <- sub('^\"','', val)
   val <- sub('\"$','', val)
   val <- sub("^\'",'', val)
   val <- sub("\'$",'', val)
   return((val))
}

# A very basic parser, works only on simple elements 
#of ascii headersharp niml files . Needs lots of work!
r.NI_read_element <- function (fname, HeadOnly = TRUE) {
      fnp <- parse.AFNI.name(fname)
   
   if (!file.exists(fnp$file)) {
      return(NULL)
   }
   
   ff <- scan(fnp$file, what = 'character', sep = '\n', quiet=TRUE)
   
   return(r.NI_read_str_element(ff, HeadOnly))
}

r.NI_read_str_element <- function (ff, HeadOnly = TRUE) {
   nel <- list(atlist=NULL, dat=NULL)
   #Remove #
   ff <- gsub('^[[:space:]]*#[[:space:]]*', '',ff)
   if (!length(grep('^<', ff))) {
      return(NULL)
   }
   #strt markers
   strv <- grep ('<', ff)
   #stp markers
   stpv <- grep ('>', ff)
   #check
   if (length(strv) != length(stpv)) {
      err.AFNI("Have extra brackets");
      return(NULL)
   }
   if (length(strv) < 1) {
      err.AFNI("Have nothing");
      return(NULL)
   }
   
   #If you have a group, jump to next element.
   shead <- ff[strv[1]:stpv[1]]
   if (length(grep('ni_form[[:space:]]*=[[:space:]]*\"ni_group\"', shead))
      && stpv[1]+1<length(ff)) {
      #try again
      return(r.NI_read_str_element(ff[stpv[1]+1:length(ff)], HeadOnly))
   }
   
   #Get only the first element, for now
   for (i in 1:1) {
      shead <- ff[strv[i]:stpv[i]]
      #get the name
      nel$name <- strsplit(shead[1],'<')[[1]][2]
      nel$atlist <- vector()
      #remove last > from shead
      shead[length(shead)] <- sub('>$','',shead[length(shead)]) 
      for (j in 2:length(shead)) {
         attr <- strsplit(shead[j],'=')[[1]]
         if (length(attr) == 1) {
            nel <- r.NI_set_attribute(nel, attr[1], "")
         } else if (length(attr) == 2) {
            nel <- r.NI_set_attribute(nel, attr[1], attr[2])
         } else if (length(attr) > 2) {
            err.AFNI(paste("Parse error for ", shead[j]));
         }
      }
   }
   
   if (HeadOnly) return(nel)
   
   #Get the data part, the lazy way 
   #All the content here is text. Not sure what to do with 
   #this next. Wait till context arises
   tp <- r.NI_get_attribute(nel,"ni_type");
   if (tp == 'String') {
      for (i in (stpv[1]+1):(strv[2]-1)) {
         if (is.null(nel$dat)) 
            nel$dat <- ff[i]
         else nel$dat <- c(nel$dat, ff[i])
      }
   } else {
      for (i in (stpv[1]+1):(strv[2]-1)) {
         #browser()
         if (is.null(nel$dat)) 
            nel$dat <- rbind(r.NI_dequotestring(
                     strsplit(deblank.string(ff[i]),split=' ')[[1]]))
         else nel$dat <- rbind(nel$dat,
                              r.NI_dequotestring(strsplit(
                                 deblank.string(ff[i]),split=' ')[[1]]))
      }
   }
   return(nel)
}

r.NI_write_str_element<- function(nel, HeadOnly = TRUE) {
   ff <- paste("<",nel$name,'\n', sep='')
   for (i in 1:length(nel$atlist)) {
      ff <- paste(ff, 
               ' ',nel$atlist[[i]]$lhs,'="', nel$atlist[[i]]$rhs,'"\n', sep='')
   }
   ff <- paste(ff,'>');
   if (!HeadOnly) {
      if (!is.null(nel$dat)) {
         ff <- paste(ff,'\n')
         if (is.matrix(nel$dat)) {
            for (j in 1:dim(nel$dat)[1]) {
               ff <- paste(ff,paste(nel$dat[j,], collapse= ' ', sep=''),
                           '\n', sep='')
            }
         } else {
            ff <- paste(ff,paste(nel$dat, collapse= '', sep=''),'\n',sep='')
         }
      }
   }
   ff <- paste(ff,'</',nel$name,'>', sep='')
   return(ff) 
} 

is.NI.file <- function (fname, asc=TRUE, hs=TRUE) {
   fnp <- parse.AFNI.name(fname)
   
   if (!file.exists(fnp$file)) {
      return(FALSE)
   }
   
   ff <- scan(fnp$file, what = 'character', nmax = 2, sep = '\n', quiet=TRUE)
   
   if (asc && hs) {
      if (!length(grep('^[[:space:]]*#[[:space:]]*<', ff))) {
         return(FALSE)
      } else {
         return(TRUE)
      }
   }
   return(FALSE)
}

apply.AFNI.matrix.header <- function (fname, mat, 
                              brsel=NULL, rosel=NULL,rasel=NULL, 
                              nheadmax = 10, checkNI=TRUE) {
   attr(mat,'name') <- 'noname'
   attr(mat,'FileName') <- fname
   
   fnp <- parse.AFNI.name(fname)
   
   if (!file.exists(fnp$file)) {
      return(mat)
   }
  
   #Does this look 1D nimly?
   if (checkNI && !is.NI.file(fnp$file, asc=TRUE, hs=TRUE)) {
      return(mat)
   }
   
   nel <- r.NI_read_element(fnp$file, HeadOnly = TRUE)

   # Common attributes
   attr(mat,'name') <- nel$name
   if (!is.null(labels <- 
            r.NI_get_attribute(nel, 'ColumnLabels', brsel, colwise=TRUE))){
         if (length(labels) == ncol(mat)) colnames(mat) <- labels
   }
   
   if (nel$name == 'matrix') {
      if (!is.null(colg <- 
            r.NI_get_attribute(nel, 'ColumnGroups', brsel, 
                                 colwise=TRUE, is1Dstr=TRUE))){
         if (length(colg) == ncol(mat)) attr(mat,'ColumnGroups') <- colg
         if (!is.null(labels)) {
            llv <- vector(length=0,mode="numeric")
            tcolg <- unique(colg)
            for (i in tcolg) {
               if (i>0) {
                  ll <- which(colg == i)
                  llv <- c(llv,ll[1])
               }
            }
            tnames <-paste(strsplit(labels[llv],"#0"))
            attr(mat,'TaskNames') <- tnames
         }
      }

      if (!is.null(TR <- r.NI_get_attribute(nel, 'RowTR'))){
         attr(mat, 'TR') <- as.double(TR)
      }
   } else if (nel$name == 'DICE') {
   } else if (nel$name == '3dhistog' || nel$name == 'seg_histogram') {
      if (!is.null(bw <- r.NI_get_attribute(nel, 'BinWidth'))){
         attr(mat, 'BinWidth') <- as.double(bw)
      }
      if (!is.null(bw <- r.NI_get_attribute(nel, 'window'))){
         attr(mat, 'BinWidth') <- as.double(bw)
      }
      if (!is.null(bw <- r.NI_get_attribute(nel, 'xlabel'))){
         attr(mat, 'xlabel') <- bw
      }
   } else {
      if (0) { #No need to whine 
         warn.AFNI(paste(
            "Don't know what to do with attribute of element ", nel$name));
      }
   }
   
   
   return(mat)
}

read.AFNI.xmat <- function (xmatfile, nheadmax=10) {
   if (!file.exists(xmatfile)) {
      err.AFNI(paste("xmatfile ",xmatfile,"not found"));
      return(NULL);
   }

   #Now load the whole deal, comments are skipped by defaults
   ff <- as.matrix(read.table(xmatfile))
   
   #Check for attributes
   ff <- apply.AFNI.matrix.header(xmatfile, ff)
   
   #Make matrix be ts
   ff <- ts(ff, names=colnames(ff), deltat=attr(ff,"TR"))
   
   return(ff)
}

xmat.base.index <- function (xmat, AFNIindex = FALSE) {
   cg <- attr(xmat,'ColumnGroups')
   an <- which(cg == -1)
   if (AFNIindex) an <- an -1
   return(an)
}
xmat.motion.index <- function (xmat, AFNIindex = FALSE) {
   cg <- attr(xmat,'ColumnGroups')
   an <- which(cg == 0)
   if (AFNIindex) an <- an -1
   return(an)
}
xmat.roni.index <- function (xmat, AFNIindex = FALSE) {
   cg <- attr(xmat,'ColumnGroups')
   an <- which(cg <= 0)
   if (AFNIindex) an <- an -1
   return(an)
}
xmat.alltasks.index <- function (xmat, AFNIindex = FALSE) {
   cg <- attr(xmat,'ColumnGroups')
   an <- which(cg > 0)
   if (AFNIindex) an <- an -1
   return(an)
}

xmat.select.indices <- function(selstr, xmat, AFNIindex = FALSE) {
   #paste(selstr, collapse=" ") is used because at times selstr is a vector
   sel <- strsplit(paste(selstr, collapse=" "), 
                     " ")[[1]]   
   #str(sel)
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
            if (isel == 'ALL') {
               ilst <- append(ilst, seq(1,ncol(xmat)))
            } else if (isel == 'ALL_TASKS') {
               ilst <- append(ilst, 
                  xmat.alltasks.index(xmat, AFNIindex=FALSE))
            } else if (isel == 'RONI') {
               ilst <- append(ilst, xmat.roni.index(xmat, AFNIindex=FALSE))
            } else if (isel == 'MOTION') {
               ilst <- append(ilst, xmat.motion.index(xmat, AFNIindex=FALSE))
            } else if (isel == 'BASE') {
               ilst <- append(ilst, xmat.base.index(xmat, AFNIindex=FALSE))
            } else {
               if (0) { #Too lose 
                  ilst <- append(ilst, grep(isel, colnames(xmat)), 
                              after=length(ilst))
               } else {
                  ilst <- append(ilst, 
                              grep(sprintf('^%s',isel), colnames(xmat)), 
                              after=length(ilst))
               }
            }
         }
      }
   } else {
      ilst = 0:(ncol(xmat)-1)
   }
   
   if (length(ilst) && AFNIindex) ilst <- ilst -1
   
   return(ilst)
}



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
      
      i <- i+1
         mm <- paste ( 'subj age \n',
                       'joe   13  \n',
                       'jane  22 \n',
                       'jim   2  \n',
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
         
       
      for (j in 0:i) {   
         system(sprintf('\\rm -f ___fout%02d.1D', i));
      }
}

AFNI.matrix.input.help.string <- function() {
   s<-
'You specify 1D input in a variety of ways:
FILE.1D : Name of 1D file
1D_EXPR: A 1D expression a la AFNI "1D: 1,2,5"
R_EXPR: An R expression, "R: rep(seq(1,3),2)"
'
   return(s)
}

read.AFNI.matrix <- function (fname, 
                              usercolnames=NULL, 
                              userrownames=NULL,
                              checkNI = TRUE, verb = 0) {
   if (length(fname)>1) {
      err.AFNI(paste("Cannot handle more than one name.\nHave ",
                     paste(fname,collapse=' '), sep=''))
      return(NULL);
   }
   if (fname == '-self_test') {
      read.AFNI.matrix.test()
      return(NULL);
   }
   
   if (!is.null(mm <- eval.AFNI.string(fname))) {
      #Might need to add some names someday...
      attr(mm,'FileName') <- paste('STR<',fname,'>',sep='')
      return(as.matrix(mm))
   }
   
   if (verb) cat(who.called.me())
   
   if (is.character(fname)) {
      fname <- parse.AFNI.name(fname)
      fnameattr <- fname$orig_name
   }else{
      fnameattr <- 'NONAME'
   }
   
   if (fname$type == 'NIML') {
      nmout <- sprintf('/tmp/fout.%s.1D.dset', newid.AFNI())
      if (verb) warn.AFNI("Clumsy handling of NIML files via ConvertDset ...");
      com <- paste(
         'ConvertDset -overwrite -o_1D -prefix ', nmout, ' -i', 
         fname$orig_name);
      ss <- sys.AFNI(com, echo = FALSE);
      if (ss$stat == 1) {
         err.AFNI(paste("Failed to get keys from labeltable",ltfile, ".\n",
                        "Command ",com,"Failed"));
         return(NULL);
      }
      mm<-read.AFNI.matrix(nmout);
      sys.AFNI(sprintf('\\rm -f %s',nmout));
      return(mm);
   } else {
      #str(fname)
      #fname$file
      brk <- NULL
      brk <- tryCatch({read.table(fname$file, colClasses='character')}, 
                      error=function(a){})
      if (is.null(brk)) { #try as niml, just in case 
         if (verb) note.AFNI(paste("Attempting read as NIML."), callstr='');
         fff <- r.NI_read_element(fname$file, FALSE)
         if (!is.null(fff$dat)) {
            brk <- as.data.frame(fff$dat, stringsAsFactors=FALSE)
            checkNI = FALSE;
         } else {
            err.AFNI(paste("Failed to read matrix from ", fname$file,".\n"));
            return(NULL);
         }
      }
   }
   if ( tolower(brk$V1[1]) == 'name' || 
        tolower(brk$V1[1]) == 'subj' ||
        tolower(brk$V1[1]) == '#file') {
      subjCol <- brk$V1[2:dim(brk)[1]]; 
      covNames <- paste(brk[1,2:dim(brk)[2]]);
      if (dim(brk)[2]-1 == 1) {
         covMatrix <- as.matrix(as.numeric(brk[2:dim(brk)[1],2]))
      } else {
         for (ii in 1:(dim(brk)[2]-1)) { #Add one column at a time
            if (ii==1) {
               covMatrix <- cbind(
                  as.numeric(brk[2:dim(brk)[1],2:dim(brk)[2]][[ii]]));
            } else {
               covMatrix <- cbind(covMatrix,
                  as.numeric(brk[2:dim(brk)[1],2:dim(brk)[2]][[ii]]));
            }
         }
      }
   }  else {
      flg <- tryCatch({as.numeric(brk$V1[1])}, warning=function(aa) {});
      if (is.null(flg)) { #Just labels
         covNames <- paste(brk[1,1:dim(brk)[2]]);
         subjCol <- paste('row',sprintf('%02d',c(1:(dim(brk)[1]-1))), sep='')
         istrt<- 2
      }else {  #completely naked
         ccc <- trim.string(fname$prefix, nchar=10, left=FALSE, strim='..');
         covNames <- paste(ccc,sprintf(' c%02d',c(1:dim(brk)[2])),sep='');
         subjCol <- paste(ccc,sprintf(' r%02d',c(1:dim(brk)[1])), sep='')
         istrt<- 1
      }
      if (dim(brk)[2] == 1) {
         covMatrix <- as.matrix(as.numeric(brk[istrt:dim(brk)[1],1]))
      } else {
         for (ii in 1:(dim(brk)[2])) { #Add one column at a time
            ccc <- tryCatch(
               {as.numeric(brk[istrt:dim(brk)[1],1:dim(brk)[2]][[ii]])},
                        warning=function(aa) {} ) 
            if (is.null(ccc)) {
               warn.AFNI(paste("Failed to process column ",
                                ii-1, " in ", fnameattr,
                                ". Using NA instead"))
               ccc <- NA*vector(length=dim(brk)[1]-istrt+1)
            }
            if (ii==1) {
               covMatrix <- cbind(ccc);
            } else {
               covMatrix <- cbind(covMatrix,ccc);
            }
         }
      }
   } 


   if (verb>2) {
      note.AFNI("Browser here, not active");
      #browser()
   }
   rownames(covMatrix) <- subjCol;
   colnames(covMatrix) <- covNames;
   
   #And now apply selectors
   if (!is.null(fname$brsel)) {
      sbsel <- eval.AFNI.1D.string(fname$brsel, nmax=dim(covMatrix)[2]-1)
      if (min(sbsel) < 0 || max(sbsel)>=dim(covMatrix)[2]) {
         err.AFNI(
            sprintf('column selection outside possible range of <0,%d> in %s',
                    dim(covMatrix)[2]-1, fname$file));
         return(NULL); 
      } 
      covMatrix <- covMatrix[,sbsel+1, drop=FALSE]
   } else sbsel <- NULL
   
   if (!is.null(fname$rosel)) {
      rosel <- eval.AFNI.1D.string(fname$rosel, nmax=dim(covMatrix)[1]-1)
      if (min(rosel) < 0 || max(rosel)>=dim(covMatrix)[1]) {
         err.AFNI(
            sprintf('row selection outside possible range of <0,%d> in %s',
                    dim(covMatrix)[1]-1, fname$file));
         return(NULL); 
      } 
      covMatrix <- covMatrix[rosel+1,, drop=FALSE]
   } else rosel <- NULL
   
   if (!is.null(fname$rasel)) {
      err.AFNI('Not ready to deal with range selection');
      return(NULL); 
   } else rasel <- NULL
   
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
      if(length(covNames)) colnames(mm) <- covNames
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
   
   covMatrix <- apply.AFNI.matrix.header(fnameattr, covMatrix, 
                            brsel=sbsel, rosel=rosel, 
                            rasel=rasel,checkNI = checkNI)

   return(covMatrix)
}   

read.AFNI.labeltable <- function (ltfile=NULL,
      def.grp.label = c('CSF','GM','WM','InSk','OutSk','Out')) {
   if (!file.exists(ltfile)) {
      err.AFNI(paste(ltfile, "does not exist"));
      return(NULL)
   }
   if (is.null(lt <- r.NI_read_element(ltfile, HeadOnly=FALSE))) {
      err.AFNI("Failed to read label table");
      return(NULL)
   }  
   if (lt$name != "VALUE_LABEL_DTABLE") {
      err.AFNI("lt file not VALUE_LABEL_DTABLE") 
      return(NULL)
   }
   lt$labels <- lt$dat[,2]
   lt$keys <- as.numeric(lt$dat[,1])
   if (is.null(gl <- r.NI_get_attribute(lt, "grp.label", colwise=TRUE))) {
      gl <- def.grp.label
   } 
   if (!is.null(gl)) {
      lt$grp.label <- gl
   }
   
   grps <- r.NI_get_attribute(lt, "grp.label", colwise=TRUE, num=TRUE) 
   if (is.null(grps)) {
      for (lab in lt$labels) {
         nn <- 0
         ig <- 0
         for (i in 1:length(gl)) {
            if (length(grep(gl[i],lab))) {
               if (ig == 0) {
                  nn <- 1
                  ig <- i
               } else {
                  if (  length(gl[ig]) != length(lab) ) {
                     if (length(gl[i]) == length(lab) ) {
                        # A better match
                        nn <- 1
                        ig <- i
                     } else {
                        #Another partial
                        nn <- nn + 1
                        ig <- i
                     }
                  } 
               }
            }  
            
         }
         if (nn != 1) {
            warn.AFNI(paste(
               "Failed to find 1 group for ",lab, ". Found ",
               nn ,"candidates. Setting grp to",
               length(gl)+1))
            ig <- length(gl)+1
         }
         grps <- c(grps, ig) 
      }
   }
   if (!is.null(grps)) {
      lt$groups <- grps
   }
   return(lt)
}


write.AFNI.matrix <- function (m, fname='test.1D') {
   if (class(m) == "AFNI_c_dataset") {
      m <- m$brk
   }
   if (is.vector(m)) {
      write(m, fname, ncolumns=1)
   } else if (is.matrix(m)) { #Need to transpose matrix
      write(t(m), fname, ncolumns=dim(m)[2])
   } else {
      err.AFNI("Don't know how to write this thing");
   }
   return(fname)
}

dset.nonzero.indices <- function (dset) {
   nn <- dset.ndim(dset)
   if (nn == 3 || nn==1) {
      return(which(rowSums(abs(dset$brk), dims=nn)!=0,arr.ind=TRUE))
   } else {
      err.AFNI("Bad dim");
      return(NULL);
   }
}

dset.zero.indices <- function (dset) {
   nn <- dset.ndim(dset)
   if (nn == 3 || nn==1) {
      return(which(rowSums(abs(dset$brk), dims=nn)==0,arr.ind=TRUE))
   } else {
      err.AFNI("Bad dim");
      return(NULL);
   }
}

dset.nvox <- function (dset) {
   nvox = -1
   if (length(dd <- dim(dset$brk)) <= 2) {
      nvox = dd[1]
   } else if (length(dd) > 2) {
      nvox = prod(dd[1:3])
   } 
   return(nvox)
}

dset.nval <- function (dset) {
   nval = -1
   if (length(dd <- dim(dset$brk)) == 1) {
      nval = 1
   } else if (length(dd) == 2) {
      nval = dd[2]
   } else if (length(dd) == 3) {
      nval = 1
   } else if (length(dd) == 4) {
      nval = dd[4]
   } 
   return(nval)
}

dset.nx <- function(dset) {
   return(dset$dim[1])
}
dset.ny <- function(dset) {
   return(dset$dim[2])
}
dset.nz <- function(dset) {
   return(dset$dim[3])
}
dset.nslice <- function(dset) {
   return(dset$dim[3])
}

dset.ndim <- function (dset) {
   ndims <- -1
   if (length(dd <- dim(dset$brk)) == 1 ||
       length(dd) == 2) {
      ndims <- 1
   } else if (length(dd) == 3 || length(dd) == 4)  {
      ndims <- 3
   }
   return(ndims)
}

get.c.AFNI.attribute<- function (atlist, attribute, asis = FALSE, verb=0) {
   if (!is.null(atlist$NI_head)) atlist <- atlist$NI_head
   nel <- atlist[[attribute]]
   if (!is.null(nel)) {
      if (verb > 1) note.AFNI(c("Found", attribute, '\n'))
      if (asis) { return(nel$dat) }
      else {
         tp <- r.NI_get_attribute(nel,"ni_type");
         if (tp == 'String') { 
            dd <- sub("^ *\"",'', nel$dat)
            dd <- sub(" *\"$",'', dd)
            return(strsplit(dd,'~')[[1]]) 
         } else if (tp == 'float') { 
            return(as.numeric(nel$dat));
         } else if (tp == 'int') { 
            return(as.integer(nel$dat));
         } else { 
            cat ("What of ", tp)
            #browser()
            nel
            return(NULL);
         }
      }
   }
   if (verb) {
      err.AFNI(paste("Attribute:",attribute,"Not found"))
      show.dset.attr(atlist)
   }
   return(NULL);
}

set.c.AFNI.attribute<- function (atlist, attribute, val=NULL, verb=0, tp=NULL,
                                 strsep="~") {
   if (!is.null(atlist$NI_head)) atlist <- atlist$NI_head
   nel <- atlist[[attribute]]
   if (!is.null(nel)) {
      if (verb) cat ("Found", attribute, '\n')
      tp <- r.NI_get_attribute(nel,"ni_type");
      if (tp == 'String') { 
            nel$dat <- paste("\"",paste(val, collapse=strsep), "\"", sep='')
            nel <- r.NI_set_attribute(nel, "ni_dimen", 1);
      } else if (tp == 'float' || tp == 'int') { 
         ff <- NULL
         if (is.matrix(val)) {
            nel <- r.NI_set_attribute(nel,
                        "ni_dimen",paste(dim(val),collapse=','))
            nel$dat <- matrix(as.character(val), dim(val)[1], dim(val)[2])
         } else if (is.vector(val)) {
            nel <- r.NI_set_attribute(nel,"ni_dimen",paste(length(val)))
            nel$dat <- matrix(as.character(val), length(val),1)
         } else {
            nel$dat <- val
         }
      } else { 
         cat ("What of ", tp)
         #browser()
         nel
      }
   } else {
      nel <- r.NI_new_element("AFNI_atr", dat=val, atlist= NULL, tp=tp)
      nel <- r.NI_set_attribute(nel, "atr_name", attribute)
      atlist[[attribute]] <- nel
            #Now that name and type are set, call again because
            #you want the multi-col strings to be turned into one
            #part with the '~' delimiters....
      tp <- r.NI_get_attribute(nel,"ni_type");
      if (tp == 'String') { 
         atlist <- set.c.AFNI.attribute(atlist, attribute, val=val, 
                                        verb=verb,strsep=strsep)
         return(atlist);
      }
   }
   atlist[[attribute]] <- nel
   if (verb && is.null(nel)) {
      err.AFNI(paste("Attribute:",attribute,"Not found."))
      show.dset.attr(atlist)
   }
   return(atlist);
}

#See README.attributes's SCENE_DATA and niml_stat.c's distname[]
statsym.distold2new <- function(dist) {
   if (dist == 'fico') return("Correl")
   if (dist == 'fitt') return("Ttest")
   if (dist == 'fift') return("Ftest")
   if (dist == 'fizt') return("Zscore")
   if (dist == 'fict') return("Chisq")
   if (dist == 'fibt') return("Beta")
   if (dist == 'fibn') return("Binom")
   if (dist == 'figt') return("Gamma")
   if (dist == 'fipt') return("Poisson")
   return(dist) 
}

statsym.list2code <- function(statsym) {
   if (is.null(statsym) || length(statsym)==0) return("")
   imx <- statsym[[1]]$sb
   if(length(statsym)>1) for (i in 2:length(statsym)) {
      if (imx < statsym[[i]]$sb) imx <- statsym[[i]]$sb
   }
   code <- NULL
   for (i in 1:imx+1) {
      code <- c(code,"none");
   }
   for (i in 1:length(statsym)) {
      code[statsym[[i]]$sb+1] <- 
         paste(statsym.distold2new(statsym[[i]]$typ),"(",
               paste(statsym[[i]]$par,collapse=','), ")",
               sep = '');
   }
   return(code)
}

header.version <- function(hh, defmeth='UNKNOWN') {
   if (defmeth == 'AUTO') {
      if (have_R_io()) defmeth <- 'clib' else defmeth <- 'Rlib'
   }
   if (!is.null(hh) && length(hh)>0) {
      if (is.null(names(hh[[1]]))) return('Rlib');
      if (length(which("atlist" == names(hh[[1]]))) > 0) return('clib');
      #Cannot tell, return default
      return(defmeth)
   }
   return(defmeth)
}

#This function is to be used in different ways depending on whether you
#are setting, or retrieving attributes
#Example:
#rs <- read.c.AFNI('littleone.niml.dset')
#to retrieve attributes:
#   dset.attr(rs,"IJK_TO_DICOM")
# or 
#   dset.attr(rs,"BRICK_LABS")
#To set attributes:
#   rs <- dset.attr(rs,"BRICK_LABS",val=c('one', 'two', 'three'))
#To set an attribute to a default if none exists
#   rs <- dset.attr(rs,"BRICK_LABS",default=c('set if none existed'))
#In query mode only, one can also pass the old '$header' list 
#generated by the old read.AFNI
dset.attr <- function (dset, name=NULL, colwise=FALSE, num=FALSE, 
                       val=NULL, default=NULL, tp=NULL) {
   attr <- NULL
   if (class(dset) != 'AFNI_R_dataset' && class(dset) != 'AFNI_c_dataset') {
      if (header.version(dset) == 'Rlib') {
         #OK to change nature of dset, only queries allowed for old style
         dset <- list(header=dset);
      }
   }
   if (!is.null(dset$header)) { #olde way
      if (!is.null(val)) {
         err.AFNI("Cannot set values with old header format");
         return(attr)
      }
      iattr <- which(name == names(dset$header))
      if (!length(iattr)) {
         return(attr)
      } else {
         attr <- dset$header[[iattr]]
      }
      if (colwise) {
         attr <- sub("^'",'', attr)
         attr <- strsplit(attr,'~')[[1]]
      }
      if (num) attr <- as.numeric(attr)
   } else {
      if (!is.null(dset$NI_head)) hatr <- dset$NI_head
      else hatr <- dset
      if (is.null(name)) {
         return(names(hatr))
      }
      if (is.null(val) && is.null(default)) { #Retrieval
         if (name == 'dim') {
            dd <- get.c.AFNI.attribute(hatr, "DATASET_DIMENSIONS")
            ee <- get.c.AFNI.attribute(hatr, "DATASET_RANK")
            return(c(dd[1], dd[2], dd[3], ee[2]))
         } else if (name == 'orient') {
            return(orcode.AFNI(get.c.AFNI.attribute(hatr, "ORIENT_SPECIFIC")))
         } else if (name == 'origin') {
            return(get.c.AFNI.attribute(hatr, "ORIGIN"))
         } else if (name == 'delta') {
            return(get.c.AFNI.attribute(hatr, "DELTA"))
         } else if (name == 'hist') {
            return(get.c.AFNI.attribute(hatr, "HISTORY_NOTE"))
         } else if (name == "TR") {
            dd <- get.c.AFNI.attribute(hatr, "TAXIS_NUMS")
            ee <- get.c.AFNI.attribute(hatr, "TAXIS_FLOATS")
            if (dd[3] == 77001) ee[2] = ee[2]/1000; #From msec to sec
            return(ee[2])
         } else {
            return(get.c.AFNI.attribute(hatr, name))
         }
      } else if (!is.null(val)) { #Set the attribute 
         if (name == 'note') {
            i <- get.c.AFNI.attribute(hatr, "NOTES_COUNT")
            if (is.null(i)) i<-0
            i<-i+1
            hatr <- set.c.AFNI.attribute(hatr, "NOTES_COUNT", val = i)
            name <- sprintf('NOTE_NUMBER_%03d', i)
            hatr <- set.c.AFNI.attribute(hatr, name, val)
         } else if (name == 'dim') {
            if (is.null(val)) {
               if (is.null(dset$brk)) {
                  err.AFNI("Cannot set dims from nothing");
                  return(NULL);
               }
               val <- dset.dimBRKarray(dset)
            }
               
            if (length(val) != 4) {
               err.AFNI("Bad val for dim");
               return(NULL);
            } 
            hatr <- set.c.AFNI.attribute(hatr, "DATASET_DIMENSIONS", 
                                         val=c(val[1:3], 0, 0))
            hatr <- set.c.AFNI.attribute(hatr, "DATASET_RANK", 
                                         val=c(3, val[4], rep(0,6)))            
         } else if (name == "statsym") {
            hatr <- set.c.AFNI.attribute(hatr, "BRICK_STATSYM",
                                 statsym.list2code(statsym=val), strsep=';')
         } else if (name == "TR") {
            dd <- get.c.AFNI.attribute(hatr, "TAXIS_NUMS");
            if (is.null(dd)) {
               rr <- get.c.AFNI.attribute(hatr, "DATASET_RANK")
               if (is.null(rr)) {
                  err.AFNI("Cannot set TAXIS_NUMS with DATASET_RANK missing");
                  return(NULL);
               }
               dd <- c(rr[2], 0, 77002, -999, -999, -999, -999, -999);
            }
            ee <- get.c.AFNI.attribute(hatr, "TAXIS_FLOATS");
            if (is.null(ee)) {
               ee <- c(0, 0, 0, 0, 0, -999999.000, -999999.000, -999999.0000);
            }
            dd[2] <- 77002; #seconds
            ee[2] <- val;
            hatr <- set.c.AFNI.attribute(hatr, "TAXIS_NUMS", val=dd);
            hatr <- set.c.AFNI.attribute(hatr, "TAXIS_FLOATS", val = ee);
         } else{
            hatr <- set.c.AFNI.attribute(hatr, name, val, tp=tp)
         }
         if (!is.null(dset$NI_head)) {
            dset$NI_head <- hatr;
            return(dset);
         } else return(hatr);
      } else if (!is.null(default)) {
         if (is.null(get.c.AFNI.attribute(hatr,name))) {
            hatr <- set.c.AFNI.attribute(hatr, name, val=default, tp=tp)
         }
         if (!is.null(dset$NI_head)) {
            dset$NI_head <- hatr;
            return(dset);
         } else return(hatr);
      } 
   }
   return(attr)
}



show.dset.attr <- function (atlist, val=FALSE, sel=NULL ) {
   if (!is.null(atlist$NI_head)) atlist <- atlist$NI_head
   j = 1;
   while (j<=length(atlist)) {
      nm <- r.NI_get_attribute(atlist[[j]],"atr_name")
      if (is.null(sel) || length(grep(sel,nm, ignore.case=TRUE))) {
         cat(nm)
         if (val) {
            cat('=', atlist[[j]]$dat,'\n')
         } else {
            cat('\n');
         }    
      }
      j <- j + 1;
   }
   invisible(NULL);
}

dset.labels <- function (dset) {
   return(dset.attr(dset, 'BRICK_LABS', colwise=TRUE))
}

dimBRKarray <- function(brk=NULL) {
   d <- c(1,1,1,1)
   if (is.array(brk)) dd <- dim(brk)
   else if (is.vector(brk)) dd <- length(brk)
   else {
      note.AFNI("Don't know what to do with this brk")
      return(NULL)
   } 
   d[1:length(dd)]<-dd
   return(d)
}

subBRKarray <- function(brk=NULL, sel=NULL) {
   d <- dimBRKarray(brk)
   if (is.null(d)) {
      note.AFNI("Failed to get dims")
      return(NULL)
   }
   if (is.null(sel) || d[4] == 1) {
      v <- brk
   } else {
      if (max(sel) > d[4] || min(sel) < 1) {
         note.AFNI(printf("sel outside of range 1:%d\n", d[4]))
         return(NULL)
      }
      v <- brk[,,,sel]
      d[4] <- length(sel)
      dim(v) <- d
   }
   return(v)
}

dset.dimBRKarray <- function(dset) {
   return(dimBRKarray(dset$brk))
}

index3Dto1D <- function(ii, dd) {
   dd12 <- dd[1]*dd[2]
   return(  (ii[,1]-1)+
            (ii[,2]-1)*dd[1]+
            (ii[,3]-1)*dd12 + 1)          
}

index1Dto3D <- function(ii, dd) {
   dd12 <- dd[1]*dd[2]
   ii <- ii-1
   k <- ii %/% dd12
   j <- ii %% dd12
   i <- j %% dd[1]
   j <- j %/% dd[1]
   return(cbind(i+1,j+1,k+1))          
}

dset.index3Dto1D <- function(ii, dset) {
   dd <- dset$dim
   return( index3Dto1D(ii,dd ))          
}

dset.index1Dto3D <- function(ii, dset) {
   dd <- dset$dim
   return( index1Dto3D(ii,dd ))          
}

dset.3DBRKarrayto1D <- function(dset) {
   if (is.null(dset$brk)) {
      err.AFNI("NULL or non existent dset$brk")
      return(NULL);
   }
   if (dset.ndim(dset) == 1) return(dset)
   dd <- dset.dimBRKarray(dset)
   dim(dset$brk) <- c(prod(dd[1:3]),dd[4])
   return(dset)
}

dset.1DBRKarrayto3D <- function(dset, dd=NULL) {
   if (is.null(dset$brk)) {
      err.AFNI("NULL or non existent dset$brk")
      return(NULL);
   }
   if (dset.ndim(dset) == 3) return(dset)
   ddi <- dset.dimBRKarray(dset)
   if (is.null(dd)) { #get it from voxel header
      dd <- dset$dim
   }
   if (prod(dd[1:3]) != prod(ddi[1])) {
      err.AFNI("Dimension mismatch");
      return(NULL)
   }
   dim(dset$brk) <- c(dd[1:3],ddi[2])
   return(dset)
}

array2dset <- function (brk=NULL, format='BRIK', meth='AUTO') {
   if (meth == 'AUTO') {
      if (have_R_io()) meth <- 'clib' else meth <- 'Rlib'
   }
   if (is.vector(brk)) {
      brk <- as.array(brk, dim=c(length(brk),1,1,1))
   }
   dd <- dimBRKarray(brk)
   if (meth=='Rlib') {
      z <- list(brk=brk,format=format, dim=dd, 
                 delta=NULL, origin=NULL, orient=NULL)
      class(z) <- "AFNI_R_dataset"
   } else if (meth == 'clib') {
      z <- list(brk=brk, NI_head<- list())
      z$NI_head <- dset.attr(z$NI_head, 
                             "IDCODE_STRING", val="SET_AT_WRITE_RANDOM")
      z$NI_head <- dset.attr(z$NI_head, "ORIGIN", val=c(0,0,0), tp="float")
      z$NI_head <- dset.attr(z$NI_head, "DELTA", val=c(1,1,1), tp="float")
      z$NI_head <- dset.attr(z$NI_head, "ORIENT_SPECIFIC",
                                    val = orcode.AFNI('RAI'))
      if (!is.null(names(brk))) 
         z$NI_head <- dset.attr(z$NI_head, "BRICK_LABS",
                                    val = names(brk));
      
      
      class(z) <- "AFNI_c_dataset"
   } else {
      err.AFNI('bad meth');
      return(NULL)
   }
   return(z)   
}

typecode.AFNI <- function(typestr, def='MRI_short') {
   if (is.null(typestr)) return(typecode.AFNI(def))
   if (is.character(typestr)) {
      if (typestr == 'MRI_byte' || typestr == 'byte') return(0)
      if (typestr == 'MRI_short' || typestr == 'short') return(1)
      if (typestr == 'MRI_int' || typestr == 'int') return(2)
      if (typestr == 'MRI_float' || typestr == 'float') return(3)
      if (typestr == 'MRI_double' || typestr == 'double') return(4)
      if (typestr == 'MRI_complex' || typestr == 'complex') return(5)
      if (typestr == 'MRI_rgb' || typestr == 'rgb') return(6)
      err.AFNI(paste('Bad typecode ', typestr));
      return(NULL);
   } else {
      if (typestr < 0  || typestr >6) {
         err.AFNI(paste('Bad typecode ', typestr));
         return(NULL);
      }
      return(typestr);
   } 
   err.AFNI(paste('Should not be here ', typestr));
   return(NULL);
}

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

#------------------------------------------------------------------
# Extracted (and modified) from fmri library by Karsten Tabelow, 
# tabelow@wias-berlin.de and Joerg Polzehl (polzehl@wias-berlin.de)
#
# Updates by ZSS & GC
#------------------------------------------------------------------

read.AFNI <- function(filename, verb = 0, ApplyScale = 1, PercMask=0.0,
                      meth = 'AUTO', forcedset = FALSE) {
  
  if (meth == 'AUTO') {
   if (have_R_io()) meth <- 'clib' else meth <- 'Rlib'
  }

  an <- parse.AFNI.name(filename);
  
  
  if (verb > 1) {
   cat('-- read.AFNI via', meth, an$type, '\n');
   show.AFNI.name(an);
  }
  
  if ( !forcedset && ((an$type == "1D"  && an$ext != ".1D.dset") 
         || an$type == "Rs" || an$type == "1Ds")) {
    brk <- read.AFNI.matrix(an$orig_name)
    z <- array2dset(brk, format=an$type)
    return(z)
  }
  
  if (meth == 'clib' || an$type == 'NIML' || 
      (an$type == "1D"  && an$ext == ".1D.dset")) {
    return(read.c.AFNI(filename, verb = verb, ApplyScale = 1, PercMask=0.0))
  }
  #If you have any selectors, use 3dbucket to get what you want, then read
  #temp dset. This is an ugly fix for now, but will change it later if
  #I/O is issue
  
  if (verb) { cat ('Need tmp?\n'); }
  rmtmp <- 0;
  if (!is.null(an$brsel) || !is.null(an$rosel) ||  !is.null(an$rasel)) {
    rmtmp <- 1;
    #Changed below from >& /dev/null to > /dev/null 2>&1
    #because system uses sh not tcsh
    #Tx to G. Pagnoni & Co.
    com <- paste ('3dcalc -overwrite -prefix ___R.read.AFNI.' ,
               basename(an$pprefix), 
               ' -a "', filename,'" -expr "a" > /dev/null 2>&1', 
               sep = '');
    if (try(system(com)) != 0) {
      warning(paste("Failed to execute:\n   ", com),
              immediate. = TRUE);
      return(NULL);
    }
    an$pprefix <- paste('___R.read.AFNI.',basename(an$pprefix), sep = '');
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
  if (is.na(size)) {
   err.AFNI("Failed to determine file size");
   return(NULL);
  }
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
         mybrk<- readBin( conbrik, "int", n=dx*dy*dz*dt, size=size, 
                          signed=TRUE, endian=endian) 
      } else if (all(values$BRICK_TYPES==3)) {
         mybrk<- readBin(conbrik, "numeric", n=dx*dy*dz*dt, size=size, 
                         signed=TRUE, endian=endian) # float        
      } else {
         err.AFNI("Cannot read datasets of multiple data types");
         close(conbrik)
         return(NULL);
      }  
      close(conbrik)
      dim(mybrk) <- c(dx,dy,dz,dt)

      if (ApplyScale) {
         if (verb) { cat ('Scaling\n'); }
         #After this operation, size of mytt doubles if initially read as int
         for (k in 1:dt) if (scale[k] != 0) mybrk[,,,k] <- scale[k] * mybrk[,,,k]
      }

      mask=NULL;
      if (PercMask > 0.0) { #ZSS: Dunno what that is for. 
                            #     0.75 was default for PercMask
         mask <- array(TRUE,c(dx,dy,dz))
         mask[mybrk[,,,1] < quantile(mybrk[,,,1],PercMask)] <- FALSE
      } 
      z <- list(brk=mybrk,format=an$type,delta=values$DELTA,
            origin=values$ORIGIN,
            orient=values$ORIENT_SPECIFIC,
            dim=c(dx,dy,dz,dt),weights=weights, header=values,mask=mask)
  } else {
    warning("Error reading file: Could not detect size per voxel\n")
    z <- list(brk=NULL,format=an$type,delta=NULL,
              origin=NULL,orient=NULL,dim=NULL,weights=NULL,
              header=values,mask=NULL)    
  }

  class(z) <- "AFNI_R_dataset"
  attr(z,"file") <- paste(filename,"BRIK",sep="")
  
  if (rmtmp == 1) {
   if (verb) {
      cat ('ZSS: Will remove tmp files\n');
   }
   #Changed below from >& to 2&>1 because system uses sh not tcsh
   #Tx to G. Pagnoni & Co.
   system('\\rm -f ___R.read.AFNI.* > /dev/null 2>&1');
  }else{
   if (verb) {
      cat ('ZSS: No temps to remove\n');
   }
  }

  invisible(z);
}

read.c.AFNI <- function(filename, verb = 0, ApplyScale = 1, PercMask=0.0) {
   if (!is.loaded('R_THD_load_dset')) {
      err.AFNI("Missing R_io.so");
      return(NULL);
   }
   if (ApplyScale != 1 || PercMask != 0.0) {
      err.AFNI(paste("This function is not ready to abide by ApplyScale != 1 or",
                     "PercMask != 0.0"));
      return(NULL);
   }
   uopts = list( debug=verb );
   rs2 <- .Call("R_THD_load_dset", name = as.character(filename), opts = uopts )
   if (is.null(rs2)) return(NULL);
   
   hatr <- parse.c.AFNI.head(rs2$head);
   rs2$head <- NULL
   ddb <- dset.attr(hatr, "dim")
   dim(rs2$brk) <- ddb
   
   rs2[['format']] <- 1
   rs2[['delta']] <- dset.attr(hatr, "delta")
   rs2[['origin']] <- dset.attr(hatr, "origin")
   rs2[['orient']] <- dset.attr(hatr, "orient")
   rs2[['dim']] <- dset.attr(hatr, "dim")
   rs2[['NI_head']] <- hatr
   rs2[['header']] <- NULL
   rs2[['mask']] <- NULL
   rs2[['weights']] <- NULL
    
   class(rs2) <- "AFNI_c_dataset"

   invisible(rs2)   
}

parse.c.AFNI.head <- function (head) {
   j <- 1
   nel <- NULL;
   nms <- NULL;
   while (j <= length(head)) {
      nelc <- r.NI_read_str_element(strsplit(head[j], "\n")[[1]], FALSE)
      if (!is.null(nelc)) {
         nn <- r.NI_get_attribute(nelc, "atr_name"); 
         if (!is.null(nn)) {
            nms <- c(nms, nn)
            nel <- c(nel, list(nelc));
         } else {
            warn.AFNI("Have element that has no atr_name");
         }
      }
      j <- j + 1;
   }
   if (!is.null(nel)) {
      names(nel) <- nms
   }
   return(nel)
}

unparse.c.AFNI.head <- function (nel) {
   j <- 1
   head <- NULL;
   nms <- NULL;
   while (j <= length(nel)) {
      #r.NI_write_str_element(nel[[j]],FALSE)
      nm <- r.NI_get_attribute(nel[[j]],"atr_name")
      if (!is.null(nm)) nms <- c(nms,nm)
      else nms <- c(nms, 'noname');
      head <- c(head, r.NI_write_str_element(nel[[j]],FALSE));
      j <- j + 1;
   }
   if (length(nms)) names(head)<-nms
   return(head)
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

newid.AFNI.old <- function(ext=0) {
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

AFNI.view2viewtype <- function(view="+orig"){
   if (is.null(view)) return(0)
   if (length(grep("orig", view, ignore.case=TRUE))) return(0)
   if (length(grep("acpc", view, ignore.case=TRUE))) return(1)
   if (length(grep("tlrc", view, ignore.case=TRUE))) return(2)
   return(0)
}

AFNI.viewtype2view <- function(view=0){
   if (is.null(view)) return("+orig")
   if (view == 0) return("+orig")
   if (view == 1) return("+acpc")
   if (view == 2) return("+tlrc")
   return("+orig")
}

dset.view <- function(dset) {
   dd <- dset.attr(dset$NI_head, "SCENE_DATA")
   return( AFNI.viewtype2view(dd[1]))          
}

dset.gridmatch <- function(mdset, idset) {
   if (is.null(mdset) || is.null(idset)) return(FALSE)
   if (dset.nvox(mdset) != dset.nvox(idset)) return(FALSE)
   return(TRUE);
}

write.c.AFNI <- function( filename, dset=NULL, label=NULL, space=NULL,
                        note=NULL, origin=NULL, delta=NULL,
                        orient=NULL, 
                        idcode=NULL, defhead=NULL,
                        verb = 1,
                        maskinf=0, scale = TRUE, 
                        overwrite=FALSE, addFDR=0,
                        statsym=NULL, view=NULL,
                        com_hist=NULL, TR=NULL, type=NULL) {
  
   an <- parse.AFNI.name(filename);
   if (verb > 1) {
      show.AFNI.name(an);
   }

   if (class(dset) == "AFNI_c_dataset") {
      #Assume all is in dset already
      if (is.null(dset$NI_head)) {
         err.AFNI("Old dset?");
         return(NULL);
      }
   } else if (class(dset) == "AFNI_R_dataset") {
      err.AFNI("Should not be with R dataset here");
      return(NULL);
   } else {
      #Assume user sent in an array, not a dset
      dset <- array2dset(dset,meth='clib')
   }
   
   if (!is.loaded('R_THD_write_dset')) {
      err.AFNI("Missing R_io.so.");
      return(NULL);
   } 
   
   if (is.null(view)) {
      if (is.null(defhead)) {
         view <- "+tlrc"
      }else {
         view <- dset.view(defhead)
      }
   }
   
      #Revert to old default if view is still not set
   if (is.null(view)) view <- "+tlrc"
         #Setup the basics (might be repetitive, oh well)
   dset$NI_head <- dset.attr(dset$NI_head, "dim", val = dset.dimBRKarray(dset))
                              #Make sure TYPESTRING and SCENE_DATA are coherent
   dset$NI_head <- dset.attr( dset$NI_head, "TYPESTRING", 
                              default = '3DIM_GEN_FUNC')
   dset$NI_head <- dset.attr( dset$NI_head, "SCENE_DATA", 
                        default = c(AFNI.view2viewtype(view),11,3, rep(0,5))) 
   
   taxnums <- NULL;
   taxfloats <- NULL;
   taxoff <- NULL;
   if (!is.null(defhead)) {
      taxnums <- dset.attr( defhead, "TAXIS_NUMS");
      taxfloats <- dset.attr( defhead, "TAXIS_FLOATS");
      taxoff <- dset.attr( defhead, "TAXIS_OFFSETS");
   }
   

         #The user options
   if (is.null(idcode)) idcode <- newid.AFNI();
 
   if (!is.null(idcode)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "IDCODE_STRING", val=idcode)
   
   if (!is.null(TR)) {
      if (is.null(taxnums)) {
                     #77002 for seconds
         taxnums <- c(dset.nval(dset), 0, 77002, -999, -999, -999, -999, -999); 
      }
      if (is.null(taxfloats)) {
         taxfloats <- c(0, TR, 0, 0, 0, -999999.000, -999999.000, -999999.0000);
      }
      if (is.null(taxoff)) {
         taxoff <- rep(0, dset.nslize(dset));
      }
      dset$NI_head <- dset.attr( dset$NI_head, "TAXIS_NUMS", val =  taxnums);
      dset$NI_head <- dset.attr( dset$NI_head, "TAXIS_FLOATS", val =  taxfloats);
      dset$NI_head <- dset.attr( dset$NI_head, "TAXIS_OFFSETS", val =  taxoff);

      dset$NI_head <- dset.attr(dset$NI_head, "TR", val = TR);
   } else { #use defhead values, setup from above 
      if (!is.null(taxnums)) {
         dset$NI_head <- dset.attr( dset$NI_head, "TAXIS_NUMS", val = taxnums);
      }
      if (!is.null(taxfloats)) {
         dset$NI_head <- dset.attr( dset$NI_head, "TAXIS_FLOATS",
                                    val =  taxfloats);
      }
      if (!is.null(taxoff)) {
         dset$NI_head <- dset.attr( dset$NI_head, "TAXIS_OFFSETS", 
                                    val =  taxoff);
      }
   }  

   if (is.null(origin) && !is.null(defhead)) 
      origin <- dset.attr(defhead,"ORIGIN")
   if (!is.null(origin)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "ORIGIN", val=origin)
   
   if (is.null(delta) && !is.null(defhead)) 
      delta <- dset.attr(defhead,"DELTA")
   if (!is.null(delta)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "DELTA", val=delta)
   
   IJK_TO_DICOM_REAL <- NULL
   if (!is.null(defhead)) 
      IJK_TO_DICOM_REAL <- dset.attr(defhead$NI_head,"IJK_TO_DICOM_REAL")
   if (!is.null(IJK_TO_DICOM_REAL)) dset$NI_head <- 
      dset.attr(dset$NI_head, "IJK_TO_DICOM_REAL", val=IJK_TO_DICOM_REAL)
   
   if (is.null(orient) && !is.null(defhead)) 
      orient <- dset.attr(defhead,"ORIENT_SPECIFIC")
   if (!is.null(orient)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "ORIENT_SPECIFIC",
                                    val = orcode.AFNI(orient))
   
   if (is.null(label) && !is.null(defhead)) 
      label <- dset.attr(defhead,"BRICK_LABS")
   if (!is.null(label)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "BRICK_LABS",
                                    val = label);

   if (is.null(space) && !is.null(defhead)) 
      space <- dset.attr(defhead,"TEMPLATE_SPACE")
   if (!is.null(space)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "TEMPLATE_SPACE",
                                    val = space);  
   
   if (is.null(type) && !is.null(defhead)) {
      tps = dset.attr(defhead,"BRICK_TYPES");
      if (!is.null(tps) && length(tps)>0) type <- tps[1]
   }   
   if (!is.null(type)) {
      if (!is.null(type <- typecode.AFNI(type))) {
         type <- rep(type,  dset.dimBRKarray(dset)[4])
         dset$NI_head <- dset.attr(dset$NI_head, "BRICK_TYPES", val=type)
      }
   }
   
   if (is.null(note) && !is.null(defhead)) 
      note <- dset.attr(defhead,"note")
   if (!is.null(note)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "note",
                                    val = note);
   
   if (is.null(statsym) && !is.null(defhead)) 
      statsym <- dset.attr(defhead,"statsym")
   if (!is.null(statsym)) dset$NI_head <- 
                           dset.attr(dset$NI_head, "statsym",
                                    val = statsym);
                                    
   if (maskinf) {
     if (verb) note.AFNI("Masking infs");
     dset$brk[!is.finite(dset$brk)]=0
   } 
   
   uopts = list( scale = scale, overwrite=overwrite, debug=verb,
                 addFDR=addFDR, hist=com_hist);
   dset$head <- unparse.c.AFNI.head(dset$NI_head)
   rso <- .Call("R_THD_write_dset", name = as.character(filename), 
                dset = as.list(dset), opts = uopts)
   invisible(dset);
}

write.AFNI <- function( filename, brk=NULL, label=NULL, space=NULL,
                        note=NULL, origin=NULL, delta=NULL,
                        orient=NULL, 
                        idcode=NULL, defhead=NULL,
                        verb = 0,
                        maskinf=0, scale = TRUE, 
                        meth='AUTO', addFDR=FALSE, 
                        statsym=NULL, view=NULL,
                        com_hist=NULL, type=NULL, TR = NULL,
                        overwrite=FALSE) {

  if (meth == 'AUTO') {
   if (class(brk) == 'AFNI_R_dataset') {
      meth = 'Rlib'
   } else if (class(brk) == 'AFNI_c_dataset') {
      meth = 'clib'
   } else if (have_R_io()) {
      meth <- 'clib' 
   } else {
      meth <- 'Rlib'
   }
  }

  an <- parse.AFNI.name(filename);
  if (verb>1) {
   show.AFNI.name(an);
  }
  
  if (an$type == "1D" || an$type == "Rs" || an$type == "1Ds") {
    return(write.AFNI.matrix(drop(brk), an$orig_name))
  }
  
  if (meth == 'clib' || an$type == 'NIML' || class(brk) == "AFNI_c_dataset") {
    return(write.c.AFNI(filename, dset=brk, label=label, space=space,
                        note=note, origin=origin, delta=delta,
                        orient=orient, 
                        idcode=idcode, defhead=defhead,
                        verb = verb,
                        maskinf=maskinf, scale = scale, addFDR=addFDR,
                        statsym=statsym, view=view, com_hist=com_hist,
                        type=type, TR=TR, overwrite=overwrite))
  }
   #Switch to old default for view if not using write.c.AFNI
  if (is.null(view)) view <- "+tlrc"
  
  if (is.na(an$view)) {
   err.AFNI('Bad filename for old writing method. Need view');
   show.AFNI.name(an);
   return(0)
  }
  
  if (addFDR && verb) {
   warn.AFNI("addFDR is not supported for AFNI_R_dataset structs");
  }
  if (!is.null(statsym) && verb) {
   warn.AFNI("statsym is not supported for AFNI_R_dataset structs");
  }
  #Call with brk if dset is sent in
  if (class(brk) == "AFNI_R_dataset") {
     return(write.AFNI( filename, brk$brk, label, space, note, origin, delta, 
                        orient, idcode, defhead, verb, maskinf, scale)) 
  }
  
  #Set the defaults. 
  if (is.null(note)) note <- '';
  if (is.null(idcode)) idcode <- newid.AFNI();
  if (is.null(brk)) {
   err.AFNI("NULL input");
   return(0)
  }
  #make sure we don't have 3 dims array, need always 4d
  ddb <- dimBRKarray(brk)
  dim(brk) <- ddb
  
  if (is.null(defhead)) { # No default header
     if (verb) note.AFNI("Have no default header");
     if (is.null(label)) label <- paste(c(1:ddb[4]),collapse='~');
     if (is.null(space)) space <-'TLRC';
     if (is.null(origin)) origin <- c(0,0,0)
     if (is.null(delta)) delta <- c(4,4,4)
     if (is.null(orient)) orient <- 'RAI'
  } else {  #When possible, call on default header
     if (verb) note.AFNI("Have default header");
     if (is.null(label)) {
      if (!is.null(defhead$BRICK_LABS)) {
         label <- gsub("^'", '', defhead$BRICK_LABS);
      } else {
         label <- paste(c(1:ddb[4]),collapse='~');
      }
     }
     if (is.null(space)) {
      if (!is.null(defhead$NI_head$TEMPLATE_SPACE$dat)) {
         space <- defheadTEMPLATE_SPACE$dat;
      } else {
         space <- 'TLRC';
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
  if (is.null(brk) || !is.numeric(brk)) {
   err.AFNI("data array improperly formatted");
   return(0);
  }      

  if (verb) {
   note.AFNI("Writing header")
  }
  conhead <- file(head.AFNI.name(an), "w")
  writeChar(AFNIheaderpart("string-attribute","TEMPLATE_SPACE",space),
            conhead,eos=NULL)
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
   brk[!is.finite(brk)]=0
  }
  
  mm <- minmax(brk)
  if (verb) {
   note.AFNI("minmax");
   str(mm)
  }
  writeChar(AFNIheaderpart("float-attribute","BRICK_STATS",mm),
            conhead,eos=NULL)
  writeChar(AFNIheaderpart("integer-attribute","DATASET_RANK",
                           c(3,ddb[4],0,0,0,0,0,0)),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("integer-attribute","DATASET_DIMENSIONS",
                           c(ddb[1:3],0,0)),
            conhead,eos=NULL)  
  if (is.null(type)) type <- 1
  writeChar(AFNIheaderpart("integer-attribute","BRICK_TYPES",
                  rep(typecode.AFNI(type),ddb[4])),
            conhead,eos=NULL)  

  if (scale) {
     if (verb) {
      note.AFNI("Computing scaling factors");
     }
     scale_fac <- rep(0,ddb[4])
     for (k in 1:ddb[4]) {
       scale_fac[k] <- max(abs(mm[2*k-1]),abs(mm[2*k]))/32767
     }
   } else {
      scale_fac <- rep(0,ddb[4])
   }
   if (verb) {
      note.AFNI("scaling factors:");
      str(scale_fac);
     }
  writeChar(AFNIheaderpart("float-attribute","BRICK_FLOAT_FACS",scale_fac),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BRICK_LABS",
                           paste(label,collapse="~")),
            conhead,eos=NULL)  
  writeChar(AFNIheaderpart("string-attribute","BYTEORDER_STRING","MSB_FIRST"),
            conhead,eos=NULL)  
  
  #The tross logger
  trlog <- Sys.getenv("AFNI_HISTDB_SCRIPT")
  if ( trlog != '') {
    writeChar(AFNIheaderpart("string-attribute","HISTDB_SCRIPT", trlog ),
               conhead,eos=NULL)  ;
  }
  
  close(conhead)

  # Write BRIK
  conbrik <- file(brik.AFNI.name(an), "wb")
  if (0) { #ZSS: old method, 
           # runs out of memory for large dsets
           # when scaling. Code kept here for testing
   if (scale) {
      if (verb) {
         note.AFNI("Applying scaling ")
        }
      for (k in 1:ddb[4]) {
         brk[,,,k] <- brk[,,,k] / scale_fac[k]
      }
   }
   dim(brk) <- NULL  #Don't know why this was done here ....
     if (verb) {
      note.AFNI("Writing brik")
     }
     writeBin(as.integer(brk), conbrik,size=2, endian="big")
   } else {
      if (verb) {
      note.AFNI("Writing /Scaling, meth 2")
     }
     #Write on sub-brick at a time to reduce memory use. 
     #      as.integer will allocate a new copy
     if (scale) {
        for (k in 1:ddb[4]) {
         writeBin(as.integer(brk[,,,k] / scale_fac[k]), 
                  conbrik,size=2, endian="big") 
        }
     } else {
        for (k in 1:ddb[4]) {
         writeBin(as.integer(brk[,,,k]), 
                  conbrik,size=2, endian="big") 
        }
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
  brk <- readBin(con, what, n=dx*dy*dz*dt, size=size, signed=signed, endian=endian) 
  close(con)

  if (min(abs(header$pixdim[2:4])) != 0) {
    weights <-
      abs(header$pixdim[2:4]/min(abs(header$pixdim[2:4])))
  } else {
    weights <- NULL
  }
  dim(brk) <- c(dx,dy,dz,dt)

  mask <- array(TRUE,c(dx,dy,dz))
  mask[brk[,,,1] < quantile(brk[,,,1],0.75)] <- FALSE

  z <- list(brk=writeBin(as.numeric(brk),raw(),4),format="NIFTI",delta=header$pixdim[2:4],
                origin=NULL,orient=NULL,dim=header$dimension[2:5],weights=weights,header=header,mask=mask)

  class(z) <- "AFNI_R_dataset"

  invisible(z)
}

extract.data <- function(z,what="data") {
  if (!("AFNI_R_dataset"%in%class(z)) || !("AFNI_c_dataset"%in%class(z))) {
    warning("extract.data: data not of class <AFNI_[cR]_dataset>. Try to proceed but strange things may happen")
  }
  if (what=="residuals") {  
      if(!is.null(z$resscale)){
          brk <- readBin(z$res,"integer",prod(z$dim),2)*z$resscale 
          dim(brk) <- z$dim
          } else {
          warning("extract.data: No residuals available, returning NULL")
          brk <- NULL
      }
      } else { 
      brk <- readBin(z$brk,"numeric",prod(z$dim),4)
      dim(brk) <- z$dim
      }
 
  invisible(brk)
}

# GC 01/12/2015: Used in 3dMVM.T and 3dLME.R to construct GLT and GLF stuff
#gl_Constr2 <- function(n_gl, code, lop) {  # n_gl: number of tests: lop$num_glt or lop$num_glf; code: lop$glfCode
#   if (n_gl > 0) {
#      outList <- vector('list', 3)
#      outList[[1]]    <- vector('list', n_gl)
#      outList[[2]]    <- vector('list', n_gl)
#      outList[[3]] <- vector('list', n_gl)
#      for (n in 1:n_gl) { # assuming each GLT has one slope involved and placed last
#         # if(length(lop$QV)==0) outList[[1]][[n]] <- glfConstr(code[[n]], lop$dataStr) else {
#         if((length(lop$QV)==0) & is.na(lop$vVars)) outList[[1]][[n]] <- glfConstr(code[[n]], lop$dataStr) else {
#         if((length(lop$QV)>0) & any(lop$QV %in% code[[n]])) {
#            QVpos <- which(code[[n]] %in% lop$QV)
#            if(is.na(code[[n]][QVpos+2])) { # test for covariate effect
#               if(QVpos==1) outList[[1]][[n]] <- NA else
#                  outList[[1]][[n]] <-glfConstr(code[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
#               outList[[2]][[n]]    <- code[[n]][QVpos]
#            } else { # effect at a specific covariate value
#              if(QVpos==1) outList[[1]][[n]] <- NA else
#                  outList[[1]][[n]] <-glfConstr(code[[n]][-(QVpos:(QVpos+2))], lop$dataStr)
#               outList[[3]][[n]] <- as.numeric(code[[n]][QVpos+2])
#               names(outList[[3]][[n]]) <- code[[n]][QVpos]
#            } # if(is.na(lop$gltCode[[n]][QVpos+2]))
#         } else if(!is.na(lop$vVars) & any(lop$vQV %in% code[[n]])) { # voxel-wise covariate
#            vQVpos <- which(code[[n]] %in% lop$vQV)
#            if(is.na(code[[n]][vQVpos+2])) { # test for covariate effect
#               if(vQVpos==1) outList[[1]][[n]] <- NA else
#                  outList[[1]][[n]] <-glfConstr(code[[n]][-c(vQVpos, vQVpos+1)], lop$dataStr)
#               outList[[2]][[n]]    <- code[[n]][vQVpos]
#            } else { # effect at a specific covariate value
#              if(vQVpos==1) outList[[1]][[n]] <- NA else
#                  outList[[1]][[n]] <-glfConstr(code[[n]][-(vQVpos:(vQVpos+2))], lop$dataStr)
#               outList[[3]][[n]] <- as.numeric(code[[n]][vQVpos+2])
#               names(outList[[3]][[n]]) <- code[[n]][vQVpos]
#           } # if(is.na(lop$gltCode[[n]][vQVpos+2]))
#         } else outList[[1]][[n]] <- glfConstr(code[[n]], lop$dataStr) # if((length(lop$QV)>0) & any(lop$QV %in% lop$gltCode[[n]]))
#      }
#      }
#   }
#   return(outList)
#}


gl_Constr <- function(n_gl, code, lop) {  # n_gl: number of tests: lop$num_glt or lop$num_glf; code: lop$glfCode
   if (n_gl > 0) {
      outList <- vector('list', 3)
      outList[[1]]    <- vector('list', n_gl)
      outList[[2]]    <- vector('list', n_gl)
      outList[[3]] <- vector('list', n_gl)
      comQV <- c(lop$QV, lop$vVars)
      for (n in 1:n_gl) { # assuming each GLT has one slope involved and placed last
         if(length(comQV)==0) outList[[1]][[n]] <- glfConstr(code[[n]], lop$dataStr) else {
         if((length(comQV)>0) & any(comQV %in% code[[n]])) {
            QVpos <- which(code[[n]] %in% comQV)
            if(is.na(code[[n]][QVpos+2])) { # test for covariate effect
               if(QVpos==1) outList[[1]][[n]] <- NA else
                  outList[[1]][[n]] <-glfConstr(code[[n]][-c(QVpos, QVpos+1)], lop$dataStr)
               outList[[2]][[n]]    <- code[[n]][QVpos]
            } else { # effect at a specific covariate value
              if(QVpos==1) outList[[1]][[n]] <- NA else
                  outList[[1]][[n]] <-glfConstr(code[[n]][-(QVpos:(QVpos+2))], lop$dataStr)
               outList[[3]][[n]] <- as.numeric(code[[n]][QVpos+2])
               names(outList[[3]][[n]]) <- code[[n]][QVpos]
            } # if(is.na(lop$gltCode[[n]][QVpos+2]))
         } else outList[[1]][[n]] <- glfConstr(code[[n]], lop$dataStr) # if((length(lop$QV)>0) & any(lop$QV %in% lop$gltCode[[n]]))
         }
         if(is.null(outList[[1]][[n]])) errex.AFNI(paste("Inappropriate coding in test No.", n, "! \n   ", sep = ""))
      }
   }
   return(outList)
}
