#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dsepi=NULL , *dsant=NULL ;
   int nopt=1 ;
   char *prefix=NULL , str[1024] ;
   int nx=1 , ny=5 , nz=0 , ii ;
   float step=1.0 ;
   int verb=0 ;
   THD_fvec3 fv ;

WARNING_message("This program (3dAnatNudge) is old, obsolete, and not maintained!") ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAnatNudge [options]\n"
             "Moves the anat dataset around to best overlap the epi dataset.\n"
             "\n"
             "OPTIONS:\n"
             " -anat aaa   = aaa is an 'scalped' (3dIntracranial) high-resolution\n"
             "                anatomical dataset [a mandatory option]\n"
             " -epi eee    = eee is an EPI dataset [a mandatory option]\n"
             "                The first [0] sub-brick from each dataset is used,\n"
             "                unless otherwise specified on the command line.\n"
             " -prefix ppp = ppp is the prefix of the output dataset;\n"
             "                this dataset will differ from the input only\n"
             "                in its name and its xyz-axes origin\n"
             "                [default=don't write new dataset]\n"
             " -step sss   = set the step size to be sss times the voxel size\n"
             "                in the anat dataset [default=1.0]\n"
             " -x nx       = search plus and minus nx steps along the EPI\n"
             " -y ny          dataset's x-axis; similarly for ny and the\n"
             " -z nz          y-axis, and for nz and the z-axis\n"
             "                [default: nx=1 ny=5 nz=0]\n"
             " -verb       = print progress reports (this is a slow program)\n"
             "\n"
             "NOTES\n"
             "*Systematically moves the anat dataset around and find the shift\n"
             "  that maximizes overlap between the anat dataset and the EPI\n"
             "  dataset.  No rotations are done.\n"
             "*Note that if you use -prefix, a new dataset will be created that\n"
             "  is a copy of the anat, except that it's origin will be shifted\n"
             "  and it will have a different ID code than the anat.  If you want\n"
             "  to use this new dataset as the anatomy parent for the EPI\n"
             "  datasets, you'll have to use\n"
             "    3drefit -apar ppp+orig eee1+orig eee2+orig ...\n"
             "*If no new dataset is written (no -prefix option), then you\n"
             "  can use the 3drefit command emitted at the end to modify\n"
             "  the origin of the anat dataset.  (Assuming you trust the\n"
             "  results - visual inspection is recommended!)\n"
             "*The reason the default search grid is mostly along the EPI y-axis\n"
             "  is that axis is usually the phase-encoding direction, which is\n"
             "  most subject to displacement due to off-resonance effects.\n"
             "*Note that the time this program takes will be proportional to\n"
             "  (2*nx+1)*(2*ny+1)*(2*nz+1), so using a very large search grid\n"
             "  will result in a very large usage of CPU time.\n"
             "*Recommended usage:\n"
             " + Make a 1-brick function volume from a typical EPI dataset:\n"
             "     3dbucket -fbuc -prefix epi_fb epi+orig\n"
             " + Use 3dIntracranial to scalp a T1-weighted volume:\n"
             "     3dIntracranial -anat spgr+orig -prefix spgr_st\n"
             " + Use 3dAnatNudge to produce a shifted anat dataset\n"
             "     3dAnatNudge -anat spgr_st+orig -epi epi_fb+orig -prefix spgr_nudge\n"
             " + Start AFNI and look at epi_fb overlaid in color on the\n"
             "    anat datasets spgr_st+orig and spgr_nudge+orig, to see if the\n"
             "    nudged dataset seems like a better fit.\n"
             " + Delete the nudged dataset spgr_nudge.\n"
             " + If the nudged dataset DOES look better, then apply the\n"
             "    3drefit command output by 3dAnatNudge to spgr+orig.\n"
             "*Note that the x-, y-, and z-axes for the epi and anat datasets\n"
             "  may point in different directions (e.g., axial SPGR and\n"
             "  coronal EPI).  The 3drefit command applies to the anat\n"
             "  dataset, NOT to the EPI dataset.\n"
             "*If the program runs successfully, the only thing set to stdout\n"
             "  will be the 3drefit command string; all other messages go to\n"
             "  stderr.  This can be useful if you want to capture the command\n"
             "  to a shell variable and then execute it, as in the following\n"
             "  csh fragment:\n"
             "     set cvar = `3dAnatNudge ...`\n"
             "     if( $cvar[1] == \"3drefit\" ) $cvar\n"
             "  The test on the first sub-string in cvar allows for the\n"
             "  possibility that the program fails, or that the optimal\n"
             "  nudge is zero.\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dAnatNudge"); machdep(); AFNI_logger("3dAnatNudge",argc,argv);
   PRINT_VERSION("3dAnatNudge");

   while( nopt < argc ){

      /*-- -anat --*/

      if( strcmp(argv[nopt],"-anat") == 0 ){
         if( dsant != NULL ){
           fprintf(stderr,"** Can't have 2 -anat options!\n"); exit(1);
         }
         dsant = THD_open_dataset( argv[++nopt] ) ;
         CHECK_OPEN_ERROR(dsant,argv[nopt]) ;
         nopt++ ; continue ;
      }

      /*-- -epi --*/

      if( strcmp(argv[nopt],"-epi") == 0 ){
         if( dsepi != NULL ){
           fprintf(stderr,"** Can't have 2 -epi options!\n"); exit(1);
         }
         dsepi = THD_open_dataset( argv[++nopt] ) ;
         CHECK_OPEN_ERROR(dsepi,argv[nopt]) ;
         nopt++ ; continue ;
      }

      /*-- -prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         prefix = argv[++nopt] ;
         if( !THD_filename_ok(prefix) ){
           fprintf(stderr,"** Illegal prefix\n"); exit(1);
         }
         nopt++ ; continue ;
      }

      /*-- -verb --*/

      if( strcmp(argv[nopt],"-verb") == 0 ){
        verb = 1 ; nopt++ ; continue ;
      }

      /*-- -step --*/

      if( strcmp(argv[nopt],"-step") == 0 ){
         step = strtod( argv[++nopt] , NULL ) ;
         if( step <= 0.0 || step > 2.0 ){
           fprintf(stderr,"** Illegal value of -step\n"); exit(1);
         }
         nopt++ ; continue ;
      }

      /*-- -x or -y or -z --*/

      if( strcmp(argv[nopt],"-x") == 0 ){
         nx = strtol( argv[++nopt] , NULL , 10 ) ;
         if( nx < 0 || nx > 30 ){
           fprintf(stderr,"** Illegal value of -x (range 0 30)\n"); exit(1);
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-y") == 0 ){
         ny = strtol( argv[++nopt] , NULL , 10 ) ;
         if( ny < 0 || ny > 30 ){
           fprintf(stderr,"** Illegal value of -y (range 0 30)\n"); exit(1);
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-z") == 0 ){
         nz = strtol( argv[++nopt] , NULL , 10 ) ;
         if( nz < 0 || nz > 30 ){
           fprintf(stderr,"** Illegal value of -z (range 0 30)\n"); exit(1);
         }
         nopt++ ; continue ;
      }

      /*-- bad news --*/

      fprintf(stderr,"** Illegal option: %s\n",argv[nopt]); exit(1);
   }

   if( dsant == NULL || dsepi == NULL ){
      fprintf(stderr,"** Must use both -anat and -epi options!\n"); exit(1);
   }

   /* do the actual work! */

   fv = THD_autonudge( dsepi,0 , dsant,0 , step , nx,ny,nz , verb ) ;

   /* see if we got anything back */

   if( fv.xyz[0] == 0.0 && fv.xyz[1] == 0.0 && fv.xyz[2] == 0.0 ){
      fprintf(stderr,"# Best nudge is nothing.\n") ; /* don't exit - 05/13 */
   }
   else {
      fprintf(stderr,"++ Best nudge is %.4f%c %.4f%c %.4f%c\n" ,
              -fv.xyz[0] , ORIENT_typestr[dsant->daxes->xxorient][0] ,
              -fv.xyz[1] , ORIENT_typestr[dsant->daxes->yyorient][0] ,
              -fv.xyz[2] , ORIENT_typestr[dsant->daxes->zzorient][0]  ) ;

      /* print 3drefit command */

      strcpy(str,"3drefit") ; ii = strlen(str) ;
      if( fv.xyz[0] != 0.0 ){
         sprintf(str+ii," -dxorigin %.4f",fv.xyz[0]); ii = strlen(str);
      }
      if( fv.xyz[1] != 0.0 ){
         sprintf(str+ii," -dyorigin %.4f",fv.xyz[1]); ii = strlen(str);
      }
      if( fv.xyz[2] != 0.0 ){
         sprintf(str+ii," -dzorigin %.4f",fv.xyz[2]); ii = strlen(str);
      }
      sprintf(str+ii," %s",DSET_HEADNAME(dsant)) ;
      printf("%s\n",str) ;
   }
   /* produce new dataset */

   if( prefix != NULL ){
      THD_fvec3 ov , nv ;
      LOAD_FVEC3(ov,DSET_XORG(dsant),DSET_YORG(dsant),DSET_ZORG(dsant)) ;
      nv = ADD_FVEC3(ov,fv) ;
      DSET_load(dsant) ;
      dsant->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ; /* 14 Jan 2004 */
      dsant->idcode = MCW_new_idcode() ;
      EDIT_dset_items( dsant ,
                         ADN_prefix , prefix ,
                         ADN_xyzorg , nv ,
                       ADN_none ) ;
      if( THD_is_file(DSET_HEADNAME(dsant)) ){
         fprintf(stderr,"** Can't overwrite existing dataset %s\n",
                 DSET_HEADNAME(dsant) ) ;
         exit(1) ;
      }
      tross_Make_History( "3dAnatNudge" , argc,argv , dsant ) ;
      DSET_write(dsant) ;
      fprintf(stderr,"++ Wrote new dataset %s\n",DSET_BRIKNAME(dsant)) ;
   }

   exit(0) ;
}
