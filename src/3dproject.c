/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#if 0
#  define DB(sss,str) printf("%s %s\n",sss,str)
#else
#  define DB(sss,str)
#endif

static EDIT_options PRED_edopt ;

/*------------------------------ prototypes ------------------------------*/

typedef struct {
   float xbot , xtop , ybot , ytop , zbot , ztop ;
} dset_range ;

dset_range PR_get_range( THD_3dim_dataset * ) ;
void PR_one_slice( int , MRI_IMAGE * , MRI_IMAGE * ) ;
float PR_type_scale( int , MRI_IMAGE * ) ;

#define PRED_err(str) \
  do{ fprintf(stderr,"\n*** %s\n",str) ; exit(-1) ; } while(1)

/*-------------------------------------------------------------------------*/

void Syntax(void)
{
   printf(
     "Projection along cardinal axes from a 3D dataset\n"
     "Usage: 3dproject [editing options]\n"
     "        [-sum|-max|-amax|-smax] [-output root] [-nsize] [-mirror]\n"
     "        [-RL {all | x1 x2}] [-AP {all | y1 y2}] [-IS {all | z1 z2}]\n"
     "        [-ALL] dataset\n"
     "\n"
     "Program to produce orthogonal projections from a 3D dataset.\n"
     "  -sum     ==> Add the dataset voxels along the projection direction\n"
     "  -max     ==> Take the maximum of the voxels [the default is -sum]\n"
     "  -amax    ==> Take the absolute maximum of the voxels\n"
     "  -smax    ==> Take the signed maximum of the voxels; for example,\n"
     "                -max  ==> -7 and 2 go to  2 as the projected value\n"
     "                -amax ==> -7 and 2 go to  7 as the projected value\n"
     "                -smax ==> -7 and 2 go to -7 as the projected value\n"
     "  -first x ==> Take the first value greater than x\n"
     "  -nsize   ==> Scale the output images up to 'normal' sizes\n"
     "               (e.g., 64x64, 128x128, or 256x256)\n"
     "               This option only applies to byte or short datasets.\n"
     "  -mirror  ==> The radiologists' and AFNI convention is to display\n"
     "               axial and coronal images with the subject's left on\n"
     "               the right of the image; the use of this option will\n"
     "               mirror the axial and coronal projections so that\n"
     "               left is left and right is right.\n"
     "\n"
     "  -output root ==> Output projections will named\n"
     "                   root.sag, root.cor, and root.axi\n"
     "                   [the default root is 'proj']\n"
     "\n"
     "  -RL all      ==> Project in the Right-to-Left direction along\n"
     "                   all the data (produces root.sag)\n"
     "  -RL x1 x2    ==> Project in the Right-to-Left direction from\n"
     "                   x-coordinate x1 to x2 (mm)\n"
     "                   [negative x is Right, positive x is Left]\n"
     "                   [OR, you may use something like -RL 10R 20L\n"
     "                        to project from x=-10 mm to x=+20 mm  ]\n"
     "\n"
     "  -AP all      ==> Project in the Anterior-to-Posterior direction along\n"
     "                   all the data (produces root.cor)\n"
     "  -AP y1 y2    ==> Project in the Anterior-to-Posterior direction from\n"
     "                   y-coordinate y1 to y2 (mm)\n"
     "                   [negative y is Anterior, positive y is Posterior]\n"
     "                   [OR, you may use something like -AP 10A 20P\n"
     "                        to project from y=-10 mm to y=+20 mm  ]\n"
     "\n"
     "  -IS all      ==> Project in the Inferior-to-Superior direction along\n"
     "                   all the data (produces root.axi)\n"
     "  -IS y1 y2    ==> Project in the Inferior-to-Superior direction from\n"
     "                   z-coordinate z1 to z2 (mm)\n"
     "                   [negative z is Inferior, positive z is Superior]\n"
     "                   [OR, you may use something like -IS 10I 20S\n"
     "                        to project from z=-10 mm to z=+20 mm  ]\n"
     "\n"
     "  -ALL         ==> Equivalent to '-RL all -AP all -IS all'\n"
     "\n"
     "* NOTE that a projection direction will not be used if the bounds aren't\n"
     "   given for that direction; thus, at least one of -RL, -AP, or -IS must\n"
     "   be used, or nothing will be computed!\n"
     "* NOTE that in the directions transverse to the projection direction,\n"
     "   all the data is used; that is, '-RL -5 5' will produce a full sagittal\n"
     "   image summed over a 10 mm slice, irrespective of the -IS or -AP extents.\n"
     "* NOTE that the [editing options] are the same as in 3dmerge.\n"
     "   In particular, the '-1thtoin' option can be used to project the\n"
     "   threshold data (if available).\n"
   ) ;

   printf("\n" MASTER_SHORTHELP_STRING ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

#define PROJ_SUM   0
#define PROJ_MMAX  1
#define PROJ_AMAX  2
#define PROJ_SMAX  3
#define PROJ_FIRST 4 /* 02 Nov 2000 */

#define MIRR_NO  0
#define MIRR_YES 1

#define BIGG 9999999.99

static float first_thresh=0.0 ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   THD_dataxes      * daxes ;
   FD_brick        ** brarr , * baxi , * bsag , * bcor ;

   int iarg , ii ;
   Boolean ok ;
   MRI_IMAGE * pim , * flim , * slim ;
   float * flar ;
   dset_range dr ;
   float val , fimfac ;
   int  ival,ityp , kk ;

   float xbot=BIGG,xtop=BIGG , ybot=BIGG,ytop=BIGG , zbot=BIGG,ztop=BIGG ;
   int   xgood=0 , ygood=0 , zgood=0 ,
         proj_code=PROJ_SUM , mirror_code=MIRR_NO , nsize=0 ;
   char  root[THD_MAX_NAME] = "proj." ;
   char  fname[THD_MAX_NAME] ;

   int ixbot=0,ixtop=0 , jybot=0,jytop=0 , kzbot=0,kztop=0 ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

   /*--- read command line arguments ---*/

WARNING_message("This program (3dproject) is old, not maintained, and probably useless!") ;

   if( argc < 2 || strncmp(argv[1],"-help",6) == 0 ) Syntax() ;

   INIT_EDOPT( &PRED_edopt ) ;
   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

DB("new arg:",argv[iarg]) ;

      /**** check for editing option ****/

      ii = EDIT_check_argv( argc , argv , iarg , &PRED_edopt ) ;
      if( ii > 0 ){
         iarg += ii ;
         continue ;
      }

      /**** -sum or -max ****/

      if( strncmp(argv[iarg],"-sum",6) == 0 ){
         proj_code = PROJ_SUM ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-max",6) == 0 ){
         proj_code = PROJ_MMAX ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-amax",6) == 0 ){
         proj_code = PROJ_AMAX ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-smax",6) == 0 ){
         proj_code = PROJ_SMAX ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-first") == 0 ){  /* 02 Nov 2000 */
         proj_code = PROJ_FIRST ;
         first_thresh = strtod( argv[++iarg] , NULL ) ;
         iarg++ ; continue ;
      }

      /**** -mirror ****/

      if( strncmp(argv[iarg],"-mirror",6) == 0 ){
         mirror_code = MIRR_YES ;
         iarg++ ; continue ;
      }

      /**** -nsize ****/

      if( strncmp(argv[iarg],"-nsize",6) == 0 ){
         nsize = 1 ;
         iarg++ ; continue ;
      }

      /**** -output root ****/

      if( strncmp(argv[iarg],"-output",6) == 0 ||
          strncmp(argv[iarg],"-root",6)   == 0   ){

         if( iarg+1 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         MCW_strncpy( root , argv[++iarg] , THD_MAX_NAME-1 ) ;
         ii = strlen(root) ;
         if( ii == 0 ){
            fprintf(stderr,"\n*** illegal rootname!\n") ; exit(-1) ;
         }
         if( root[ii-1] != '.' ){ root[ii] = '.' ; root[ii+1] = '\0' ; }
         iarg++ ; continue ;
      }

      /**** -ALL ****/

      if( strncmp(argv[iarg],"-ALL",6) == 0 ||
          strncmp(argv[iarg],"-all",6) == 0   ){

         xgood = ygood = zgood = 1 ;
         xbot  = ybot  = zbot  = -BIGG ;
         xtop  = ytop  = ztop  =  BIGG ;
         iarg++ ; continue ;
      }

      /**** -RL {all | x1 x2} ****/

      if( strncmp(argv[iarg],"-RL",6) == 0 ||
          strncmp(argv[iarg],"-LR",6) == 0 ||
          strncmp(argv[iarg],"-rl",6) == 0 ||
          strncmp(argv[iarg],"-lr",6) == 0 ||
          strncmp(argv[iarg],"-sag",6)== 0  ){

         char * cerr ; float tf ;

         xgood = 1 ;  /* mark for x projection */

         if( iarg+1 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         if( strncmp(argv[iarg+1],"all",6) == 0 ||
             strncmp(argv[iarg+1],"ALL",6) == 0   ){

            xbot = -BIGG;
            xtop =  BIGG ;
            iarg += 2 ; continue ;
         }

         if( iarg+2 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         xbot = strtod( argv[iarg+1] , &cerr ) ;
         if( cerr == argv[iarg+1] ){
            fprintf(stderr,"\n*** illegal argument after %s: %s\n",
                    argv[iarg],argv[iarg+1] ) ; exit(-1) ;
         }
         if( *cerr == 'R' && xbot > 0.0 ) xbot = -xbot ;

         xtop = strtod( argv[iarg+2] , &cerr ) ;
         if( cerr == argv[iarg+2] ){
            fprintf(stderr,"\n*** illegal argument after %s: %s\n",
                    argv[iarg],argv[iarg+2] ) ; exit(-1) ;
         }
         if( *cerr == 'R' && xtop > 0.0 ) xtop = -xtop ;

         if( xbot > xtop ){ tf = xbot ; xbot = xtop ; xtop = tf ; }
         iarg +=3 ; continue ;
      }

      /**** -AP {all | y1 y2} ****/

      if( strncmp(argv[iarg],"-AP",6) == 0 ||
          strncmp(argv[iarg],"-PA",6) == 0 ||
          strncmp(argv[iarg],"-ap",6) == 0 ||
          strncmp(argv[iarg],"-pa",6) == 0 ||
          strncmp(argv[iarg],"-cor",6)== 0  ){

         char * cerr ; float tf ;

         ygood = 1 ;  /* mark for y projection */

         if( iarg+1 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         if( strncmp(argv[iarg+1],"all",6) == 0 ||
             strncmp(argv[iarg+1],"ALL",6) == 0   ){

            ybot = -BIGG ;
            ytop =  BIGG ;
            iarg += 2 ; continue ;
         }

         if( iarg+2 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         ybot = strtod( argv[iarg+1] , &cerr ) ;
         if( cerr == argv[iarg+1] ){
            fprintf(stderr,"\n*** illegal argument after %s: %s\n",
                    argv[iarg],argv[iarg+1] ) ; exit(-1) ;
         }
         if( *cerr == 'A' && ybot > 0.0 ) ybot = -ybot ;

         ytop = strtod( argv[iarg+2] , &cerr ) ;
         if( cerr == argv[iarg+2] ){
            fprintf(stderr,"\n*** illegal argument after %s: %s\n",
                    argv[iarg],argv[iarg+2] ) ; exit(-1) ;
         }
         if( *cerr == 'A' && ytop > 0.0 ) ytop = -ytop ;

         if( ybot > ytop ){ tf = ybot ; ybot = ytop ; ytop = tf ; }
         iarg +=3 ; continue ;
      }

      /**** -IS {all | z1 z2} ****/

      if( strncmp(argv[iarg],"-IS",6) == 0 ||
          strncmp(argv[iarg],"-SI",6) == 0 ||
          strncmp(argv[iarg],"-is",6) == 0 ||
          strncmp(argv[iarg],"-si",6) == 0 ||
          strncmp(argv[iarg],"-axi",6)== 0   ){

         char * cerr ; float tf ;

         zgood = 1 ;  /* mark for y projection */

         if( iarg+1 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         if( strncmp(argv[iarg+1],"all",6) == 0 ||
             strncmp(argv[iarg+1],"ALL",6) == 0   ){

            zbot = -BIGG ;
            ztop =  BIGG ;
            iarg += 2 ; continue ;
         }

         if( iarg+2 >= argc ){
            fprintf(stderr,"\n*** no argument after option %s\n",argv[iarg]) ;
            exit(-1) ;
         }

         zbot = strtod( argv[iarg+1] , &cerr ) ;
         if( cerr == argv[iarg+1] ){
            fprintf(stderr,"\n*** illegal argument after %s: %s\n",
                    argv[iarg],argv[iarg+1] ) ; exit(-1) ;
         }
         if( *cerr == 'I' && zbot > 0.0 ) zbot = -zbot ;

         ztop = strtod( argv[iarg+2] , &cerr ) ;
         if( cerr == argv[iarg+2] ){
            fprintf(stderr,"\n*** illegal argument after %s: %s\n",
                    argv[iarg],argv[iarg+2] ) ; exit(-1) ;
         }
         if( *cerr == 'I' && ztop > 0.0 ) ztop = -ztop ;

         if( zbot > ztop ){ tf = zbot ; zbot = ztop ; ztop = tf ; }
         iarg +=3 ; continue ;
      }

      /**** unknown option ****/

      fprintf(stderr,"\n*** Unknown option: %s\n",argv[iarg]) ;
      exit(-1) ;
   }  /* end of loop over input options */

   if( ! xgood && ! ygood && ! zgood ){
      fprintf(stderr,"\n*** No projections ordered!?\n") ; exit(-1) ;
   }

   /*--- open dataset and set up to extract data slices ---*/

   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){
      fprintf(stderr,"\n*** Can't open dataset file %s\n",argv[iarg]) ;
      exit(-1) ;
   }
   if( DSET_NUM_TIMES(dset) > 1 ){
      fprintf(stderr,"\n*** Can't project time-dependent dataset!\n") ;
      exit(1) ;
   }
   EDIT_one_dataset( dset, &PRED_edopt ) ;
   daxes = dset->daxes ;
   brarr = THD_setup_bricks( dset ) ;
   baxi  = brarr[0] ; bsag = brarr[1] ; bcor = brarr[2] ;

   /*--- determine index range for each direction ---*/

   dr = PR_get_range( dset ) ;

   if( xgood ){
      if( xbot < dr.xbot ) xbot = dr.xbot ;
      if( xtop > dr.xtop ) xtop = dr.xtop ;

      fv = THD_dicomm_to_3dmm( dset , TEMP_FVEC3(xbot,0,0) ) ;
      iv = THD_3dmm_to_3dind ( dset , fv ) ;
      iv = THD_3dind_to_fdind( bsag , iv ) ; ixbot = iv.ijk[2] ;

      fv = THD_dicomm_to_3dmm( dset , TEMP_FVEC3(xtop,0,0) ) ;
      iv = THD_3dmm_to_3dind ( dset , fv ) ;
      iv = THD_3dind_to_fdind( bsag , iv ) ; ixtop = iv.ijk[2] ;

      if( ixbot > ixtop ) { ii = ixbot ; ixbot = ixtop ; ixtop = ii ; }

      if( ixbot <  0        ) ixbot = 0 ;
      if( ixtop >= bsag->n3 ) ixtop = bsag->n3 - 1 ;
   }

   if( ygood ){
      if( ybot < dr.ybot ) ybot = dr.ybot ;
      if( ytop > dr.ytop ) ytop = dr.ytop ;

      fv = THD_dicomm_to_3dmm( dset , TEMP_FVEC3(0,ybot,0) ) ;
      iv = THD_3dmm_to_3dind ( dset , fv ) ;
      iv = THD_3dind_to_fdind( bcor , iv ) ; jybot = iv.ijk[2] ;

      fv = THD_dicomm_to_3dmm( dset , TEMP_FVEC3(0,ytop,0) ) ;
      iv = THD_3dmm_to_3dind ( dset , fv ) ;
      iv = THD_3dind_to_fdind( bcor , iv ) ; jytop = iv.ijk[2] ;

      if( jybot > jytop ) { ii = jybot ; jybot = jytop ; jytop = ii ; }

      if( jybot <  0        ) jybot = 0 ;
      if( jytop >= bcor->n3 ) jytop = bcor->n3 - 1 ;
   }

   if( zgood ){
      if( zbot < dr.zbot ) zbot = dr.zbot ;
      if( ztop > dr.ztop ) ztop = dr.ztop ;

      fv = THD_dicomm_to_3dmm( dset , TEMP_FVEC3(0,0,zbot) ) ;
      iv = THD_3dmm_to_3dind ( dset , fv ) ;
      iv = THD_3dind_to_fdind( baxi , iv ) ; kzbot = iv.ijk[2] ;

      fv = THD_dicomm_to_3dmm( dset , TEMP_FVEC3(0,0,ztop) ) ;
      iv = THD_3dmm_to_3dind ( dset , fv ) ;
      iv = THD_3dind_to_fdind( baxi , iv ) ; kztop = iv.ijk[2] ;

      if( kzbot > kztop ) { ii = kzbot ; kzbot = kztop ; kztop = ii ; }

      if( kzbot <  0        ) kzbot = 0 ;
      if( kztop >= baxi->n3 ) kztop = baxi->n3 - 1 ;
   }

   ival   = DSET_PRINCIPAL_VALUE(dset) ;    /* index to project */
   ityp   = DSET_BRICK_TYPE(dset,ival) ;    /* type of this data */
   fimfac = DSET_BRICK_FACTOR(dset,ival) ;  /* scale factor of this data */

   /*--- project the x direction, if desired ---*/

   if( xgood ){
      int n1 = bsag->n1 , n2 = bsag->n2 ;
      int ss , npix ;
      float fmax , fmin , scl ;

      /*-- set up --*/

      npix = n1*n2 ;
      flim = mri_new( n1 , n2 , MRI_float ) ;
      flar = mri_data_pointer( flim ) ;
      for( ii=0 ; ii < npix ; ii++ ) flar[ii] = 0.0 ;

      /*-- actually project --*/

      for( ss=ixbot ; ss <= ixtop ; ss++ ){
         slim = FD_brick_to_mri( ss , ival , bsag ) ;
         if( slim->kind != MRI_float ){
            pim = mri_to_float( slim ) ;
            mri_free( slim ) ; slim = pim ;
         }
         PR_one_slice( proj_code , slim , flim ) ;
         mri_free( slim ) ;
      }

      /*-- form output --*/

      if( fimfac != 0.0 && fimfac != 1.0 ){
         slim = mri_scale_to_float( 1.0/fimfac , flim ) ;
         mri_free(flim) ; flim = slim ;
      }

      scl = PR_type_scale( ityp , flim ) ;
      pim = mri_to_mri_scl( ityp , scl , flim ) ; mri_free( flim ) ;
      if( nsize ){
         slim = mri_nsize( pim ) ;
         if( slim != NULL && slim != pim ) { mri_free(pim) ; pim = slim ; }
      }

      if( scl != 1.0 )
         printf("Sagittal projection pixels scaled by %g to avoid overflow!\n",
                scl ) ;

      fmax = mri_max(pim) ; fmin = mri_min(pim) ;
      printf("Sagittal projection min = %g  max = %g\n",fmin,fmax) ;

      strcpy(fname,root) ; strcat(fname,"sag") ;
      mri_write( fname, pim ) ;
      mri_free(pim) ;
   }

   /*--- project the y direction, if desired ---*/

   if( ygood ){
      int n1 = bcor->n1 , n2 = bcor->n2 ;
      int ss , npix ;
      float fmax , fmin , scl ;

      /*-- set up --*/

      npix = n1*n2 ;
      flim = mri_new( n1 , n2 , MRI_float ) ;
      flar = mri_data_pointer( flim ) ;
      for( ii=0 ; ii < npix ; ii++ ) flar[ii] = 0.0 ;

      /*-- actually project --*/

      for( ss=jybot ; ss <= jytop ; ss++ ){
         slim = FD_brick_to_mri( ss , ival , bcor ) ;
         if( slim->kind != MRI_float ){
            pim = mri_to_float( slim ) ;
            mri_free( slim ) ; slim = pim ;
         }
         PR_one_slice( proj_code , slim , flim ) ;
         mri_free( slim ) ;
      }

      /*-- form output --*/

      if( fimfac != 0.0 && fimfac != 1.0 ){
         slim = mri_scale_to_float( 1.0/fimfac , flim ) ;
         mri_free(flim) ; flim = slim ;
      }

      scl = PR_type_scale( ityp , flim ) ;
      pim = mri_to_mri_scl( ityp , scl , flim ) ; mri_free( flim ) ;
      if( nsize ){
         slim = mri_nsize( pim ) ;
         if( slim != NULL && slim != pim ) { mri_free(pim) ; pim = slim ; }
      }

      if( scl != 1.0 )
         printf("Coronal projection pixels scaled by %g to avoid overflow!\n",
                scl ) ;

      fmax = mri_max(pim) ; fmin = mri_min(pim) ;
      printf("Coronal projection min = %g  max = %g\n",fmin,fmax) ;

      if( mirror_code == MIRR_YES ){
         slim = mri_flippo( MRI_ROT_0 , TRUE , pim ) ;
         mri_free(pim) ; pim = slim ;
      }
      strcpy(fname,root) ; strcat(fname,"cor") ;
      mri_write( fname, pim ) ;
      mri_free(pim) ;
   }

   /*--- project the z direction, if desired ---*/

   if( zgood ){
      int n1 = baxi->n1 , n2 = baxi->n2 ;
      int ss , npix ;
      float fmax , fmin , scl ;

      /*-- set up --*/

      npix = n1*n2 ;
      flim = mri_new( n1 , n2 , MRI_float ) ;
      flar = mri_data_pointer( flim ) ;
      for( ii=0 ; ii < npix ; ii++ ) flar[ii] = 0.0 ;

      /*-- actually project --*/

      for( ss=kzbot ; ss <= kztop ; ss++ ){
         slim = FD_brick_to_mri( ss , ival , baxi ) ;
         if( slim->kind != MRI_float ){
            pim = mri_to_float( slim ) ;
            mri_free( slim ) ; slim = pim ;
         }
         PR_one_slice( proj_code , slim , flim ) ;
         mri_free( slim ) ;
      }

      /*-- form output --*/

      if( fimfac != 0.0 && fimfac != 1.0 ){
         slim = mri_scale_to_float( 1.0/fimfac , flim ) ;
         mri_free(flim) ; flim = slim ;
      }

      scl = PR_type_scale( ityp , flim ) ;
      pim = mri_to_mri_scl( ityp , scl , flim ) ; mri_free( flim ) ;
      if( nsize ){
         slim = mri_nsize( pim ) ;
         if( slim != NULL && slim != pim ) { mri_free(pim) ; pim = slim ; }
      }

      if( scl != 1.0 )
         printf("Axial projection pixels scaled by %g to avoid overflow!\n",
                scl ) ;

      fmax = mri_max(pim) ; fmin = mri_min(pim) ;
      printf("Axial projection min = %g  max = %g\n",fmin,fmax) ;

      if( mirror_code == MIRR_YES ){
         slim = mri_flippo( MRI_ROT_0 , TRUE , pim ) ;
         mri_free(pim) ; pim = slim ;
      }
      strcpy(fname,root) ; strcat(fname,"axi") ;
      mri_write( fname, pim ) ;
      mri_free(pim) ;
   }

   exit(0) ;
}

/*------------------------------------------------------------------*/

dset_range PR_get_range( THD_3dim_dataset * dset )
{
   THD_dataxes      * daxes ;
   THD_fvec3 fv1 , fv2 , fv3 ;
   THD_ivec3 iv ;
   float tf ;
   dset_range dr ;

#define FSWAP(x,y) (tf=(x),(x)=(y),(y)=tf)

   daxes = dset->daxes ;

   LOAD_FVEC3(fv1 , daxes->xxorg , daxes->yyorg , daxes->zzorg) ;
   fv1 = THD_3dmm_to_dicomm( dset , fv1 ) ;

   LOAD_FVEC3(fv2 , daxes->xxorg + (daxes->nxx-1)*daxes->xxdel ,
                    daxes->yyorg + (daxes->nyy-1)*daxes->yydel ,
                    daxes->zzorg + (daxes->nzz-1)*daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( dset , fv2 ) ;

   if( fv1.xyz[0] > fv2.xyz[0] ) FSWAP( fv1.xyz[0] , fv2.xyz[0] ) ;
   if( fv1.xyz[1] > fv2.xyz[1] ) FSWAP( fv1.xyz[1] , fv2.xyz[1] ) ;
   if( fv1.xyz[2] > fv2.xyz[2] ) FSWAP( fv1.xyz[2] , fv2.xyz[2] ) ;

   dr.xbot = fv1.xyz[0] ; dr.xtop = fv2.xyz[0] ;
   dr.ybot = fv1.xyz[1] ; dr.ytop = fv2.xyz[1] ;
   dr.zbot = fv1.xyz[2] ; dr.ztop = fv2.xyz[2] ;

   return dr ;
}

/**********************************************************************
  Add one slice to the projection.
    slim      = input image from brick (both images are MRI_float)
    prim      = accumulating projection image
    proj_code = one of the PROJ_* values
**********************************************************************/

void PR_one_slice( int proj_code , MRI_IMAGE * slim , MRI_IMAGE * prim )
{
   float * slar = MRI_FLOAT_PTR(slim) ;
   float * flar = MRI_FLOAT_PTR(prim) ;
   int ii , npix = slim->nvox ;

   switch( proj_code ){
      default:
      case PROJ_SUM:
         for( ii=0 ; ii < npix ; ii++ ) flar[ii] += slar[ii] ;
      break ;

      case PROJ_FIRST:                    /* 02 Nov 2000 */
         for( ii=0 ; ii < npix ; ii++ )
            if( flar[ii] == 0.0 && slar[ii] > first_thresh )
               flar[ii] = slar[ii] ;
      break ;

      case PROJ_MMAX:
         for( ii=0 ; ii < npix ; ii++ )
            if( flar[ii] < slar[ii] ) flar[ii] = slar[ii] ;
      break ;

      case PROJ_AMAX:{
         int ss ;
         for( ii=0 ; ii < npix ; ii++ ){
            ss = abs(slar[ii]) ;
            if( flar[ii] < ss ) flar[ii] = ss ;
         }
      }
      break ;

      case PROJ_SMAX:{
         int ss ;
         for( ii=0 ; ii < npix ; ii++ ){
            ss = abs(slar[ii]) ;
            if( fabs(flar[ii]) < ss ) flar[ii] = slar[ii] ;
         }
      }
      break ;
   }
   return ;
}

/*--------------------------------------------------------------
   Find the scale factor needed to convert the MRI_float input
   image to the desired output type.
----------------------------------------------------------------*/

float PR_type_scale( int itype , MRI_IMAGE * prim )
{
   float ptop , scl ;

   if( ! MRI_IS_INT_TYPE(itype) ) return 1.0 ;

   ptop = mri_maxabs( prim ) ;
   scl  = 1.0 ;

   switch( itype ){

      default: return scl ;

      case MRI_short:
         while( ptop > 32767.0 ){
            scl /= 10.0 ; ptop /= 10.0 ;
         }
      return scl ;

      case MRI_byte:
         while( ptop > 255.0 ){
            scl /= 10.0 ; ptop /= 10.0 ;
         }
      return scl ;

      case MRI_int:
         while( ptop > 2147483647.0 ){
            scl /= 10.0 ; ptop /= 10.0 ;
         }
      return scl ;
   }
}
