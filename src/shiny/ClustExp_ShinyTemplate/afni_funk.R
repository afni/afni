#####################################
## 07/27/2017 Justin Rajendra
## Group extract test
## afni functions


#########################################################
## launch afni functions

afni_launch <- function(under.in,thresh.in,mask.in,peak.in){

  ## get the file name without the path to switch
  under.img  <- basename(under.in)
  thresh.img <- basename(thresh.in)
  mask.img   <- basename(mask.in)
  peak.img   <- basename(peak.in)

  system(paste0('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
                afni.path,
                '/afni -q -tbar shiny -yesplugouts -skip_afnirc ',
                '-dset ',under.in,' ',thresh.in,' ',mask.in,' ',peak.in,' ',
                '-DAFNI_ENFORCE_ASPECT=NO -DAFNI_SPLASH_ANIMATE=NO ',
                '-DAFNI_SPLASH_MELT=NO -DAFNI_STARTUP_SCRIPT=dNULL; sleep 3; ',
                afni.path,'/plugout_drive ',
                '-com "SWITCH_UNDERLAY ',under.img,'" ',
                '-com "SWITCH_OVERLAY ',thresh.img,'" ',
                '-com "SEE_OVERLAY+" ',
                '-com "OPEN_WINDOW A.axialimage geom=250x325+1+1" ',
                '-com "OPEN_WINDOW A.coronalimage geom=300x300+1+328" ',
                '-com "OPEN_WINDOW A.sagittalimage geom=325x250+1+651" ',
                '-com "SET_PBAR_ALL -99 1.0 Spectrum:red_to_blue+gap" ',
                '-quit'))
}   ## end launch afni

afni_overlay <- function(over.in,brik0,brik1,thresh){

  ## get the file name without the path to switch
  over.img <- basename(over.in)

  system(paste('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
               afni.path,'/plugout_drive ',
               '-com "SWITCH_OVERLAY ',over.img,'"',
               '-com "SET_SUBBRICKS A 0 ',brik0,brik1,'" ',
               '-com "OPEN_WINDOW A.axialimage geom=250x325+1+1" ',
               '-com "OPEN_WINDOW A.coronalimage geom=300x300+1+328" ',
               '-com "OPEN_WINDOW A.sagittalimage geom=325x250+1+651" ',
               '-quit'))
}   ## end switch overlay

afni_move <- function(x.in,y.in,z.in){

  system(paste0('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
                afni.path,'/plugout_drive ',
                '-com "SET_SPM_XYZ A. ',x.in,' ',y.in,' ',z.in,'" ',
                '-quit '))
}   ## end move

afni_close <- function(){
  system(paste0('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
                afni.path,'/plugout_drive -com "QUITT" -quit '))
}

# ## get tlrc base
# get_tlrc_base <- function(out.folder,sid.in) {
#
#   ## get the chosen base from the uber subject no gui script
#   script.folder <- unlist(tstrsplit(out.folder,split='/'))
#   script.folder <- paste(script.folder[-length(script.folder)],collapse='/')
#   var.file <- paste0(script.folder,'/',sid.in,'.afni_proc_vars.txt')
#   script.text <- readLines(var.file)
#   tlrc.base <- script.text[grepl('-svar tlrc_base',script.text)]
#   tlrc.base <- unlist(tstrsplit(tlrc.base,' '))
#   tlrc.base <- tlrc.base[length(tlrc.base)-1]
#
#   ## see if it was one of the defaults, if not use the whole path
#   if(tlrc.base %in% c('TT_N27+tlrc','TT_avg152T1+tlrc',
#                       'TT_icbm452+tlrc','MNI_avg152T1+tlrc')){
#     base.out <- paste0(afni.path,'/',tlrc.base,'.HEAD')
#     if(file.exists(base.out)){
#       return(base.out)
#     }
#   } else {
#     if(file.exists(tlrc.base)){
#       return(tlrc.base)
#     } else { return(NULL) }
#   }
# }   ## end get tlrc base





