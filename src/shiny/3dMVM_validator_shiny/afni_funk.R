#####################################
## 11/2017 Justin Rajendra
## 3dMVM validator
## afni functions

## mask ave function
voxel_extract <- function(dset.list,x.in,y.in,z.in,vox.file,vox_roi,srad){

  if(vox_roi == "Voxel"){
    system(paste0('cd ',cur.dir,' ; ',
                  afni.path,'/3dmaskave -ibox ',
                  x.in,' ',y.in,' ',z.in,' -quiet "',
                  dset.list,'" > ',vox.file) )

  } else if(vox_roi == "ROI"){   ## if ROI use an image
    mask.img <- paste0(out.dir,"/seed_mask_",x.in,"_",y.in,"_",z.in,
                       "_",srad,".nii.gz")
    system(paste0('cd ',cur.dir,' ; ',
                  afni.path,'/3dmaskave -mask ',mask.img,' -quiet "',
                  dset.list,'" > ',vox.file) )
  }
  system("echo ; echo EXTRACTION COMPLETE! ; echo ")
}   ## end voxel_extract


#####################################
## get ijk
get_ijk_xyz <- function(){

  ## make temp file
  coord.file <- paste0(out.dir,"/temp_coord.txt")

  ## get ijk for 3dmaskave
  system(paste0(afni.path,'/plugout_drive ',
                '-com "SET_OUTPLUG ',coord.file,'" -com "GET_IJK" -quit'))
  ijk.str <- readLines(coord.file)
  ijk <- as.numeric(unlist(tstrsplit(ijk.str, ' '))[2:4])

  ## remove file
  file.remove(coord.file,showWarnings=FALSE)

  ## get xyz for display
  system(paste0(afni.path,'/plugout_drive ',
                '-com "SET_OUTPLUG ',coord.file,'" -com "GET_DICOM_XYZ" -quit'))
  RAI.str <- readLines(coord.file)
  RAI <- as.numeric(unlist(tstrsplit(RAI.str, ' '))[3:5])
  RAI <- as.numeric(RAI)

  ## remove file
  file.remove(coord.file,showWarnings=FALSE)

  ## get the LPI
  LPI <- c(RAI[1]*-1,RAI[2]*-1,RAI[3])

  ## combine and return
  coord.out <- rbind(RAI,LPI,ijk)
  return(coord.out)

}   ## end voxel_extract

#########################################################
## launch afni functions

afni_launch <- function(under.in,dir.in){

  ## set the current dir for any relative paths
  # setwd(cur.dir)

  ## get the file name without the path to switch
  under.img  <- basename(under.in)

  system(paste0('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
                afni.path,
                '/afni -q -tbar shiny -yesplugouts ',
                '-dset ',under.in,' ',
                '-DAFNI_ENFORCE_ASPECT=YES -DAFNI_SPLASH_ANIMATE=NO ',
                '-DAFNI_SPLASH_MELT=NO; sleep 3; ',
                afni.path,'/plugout_drive ',
                '-com "SWITCH_UNDERLAY ',under.img,'" ',
                '-com "SEE_OVERLAY-" ',
                '-com "OPEN_WINDOW A.axialimage geom=250x325+1+1" ',
                '-com "OPEN_WINDOW A.coronalimage geom=300x300+1+328" ',
                '-com "OPEN_WINDOW A.sagittalimage geom=325x250+1+651" ',
                '-com "CHDIR ',dir.in,'" ',
                '-com "RESCAN_THIS" ',
                '-quit'))
}   ## end launch afni

afni_add_mask <- function(ijk.x,ijk.y,ijk.z,srad){

  ## get the file name without the path to switch
  mask.img <- paste0("seed_mask_",ijk.x,"_",ijk.y,"_",ijk.z,
                     "_",srad,".nii.gz")

  system(paste0('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
                'sleep 1; ',
                afni.path,'/plugout_drive ',
                '-com "RESCAN_THIS" ',
                '-com "SWITCH_OVERLAY ',mask.img,'" ',
                '-com "SET_PBAR_ALL A.+99 1.0 Spectrum:red_to_blue" ',
                '-com "SEE_OVERLAY+" ',
                '-com "SET_SUBBRICKS A 0 0 0" ',
                '-com "SET_IJK A. ',ijk.x,' ',ijk.y,' ',ijk.z,'" ',
                '-quit'))
}   ## end add mask

afni_hide_OLAY <- function(){
  system(paste0('export DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace; ',
                afni.path,'/plugout_drive ',
                '-com "SEE_OVERLAY-" ',
                '-quit'))
}   ## end afni_hide_OLAY

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

#########################################################
## make seed
afni_roi <- function(master,srad,ijk.x,ijk.y,ijk.z){

  ## make temp files
  mask.img <- paste0(out.dir,"/seed_mask_",ijk.x,"_",ijk.y,"_",ijk.z,
                     "_",srad,".nii.gz")
  coord.file <- paste0(out.dir,"/temp_mask.txt")

  ## make coordinate file
  system(paste0('echo ',ijk.x,' ',ijk.y,' ',ijk.z,
                ' > ',coord.file))

  system(paste0(afni.path,'/3dUndump -ijk -prefix ',mask.img,
                ' -master ',out.dir,'/',master,' -srad ',srad,' ',coord.file))

  ## remove file
  file.remove(coord.file,showWarnings=FALSE)

}   ## end afni seed









