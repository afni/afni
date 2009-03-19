#include "mrilib.h"
#include "thd.h"
#define MAIN

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*--- This program will read in a dataset and write it out in axial order ---*/

/*** 06 Mar 2000: allow sagittal and coronal as well ***/

static Boolean write_output = False;  /* 08 Aug 2006 [dg] -force rewrite as in 3drefit by rickr */
static Boolean NIFTI_mode = False;    /* saving NIFTI output */
static int cmode = COMPRESS_NOFILE;   /* check compression mode for NIFTI separately */
static int AXIAL_frugal = 0 ;         /* Saving sub-bricks one at a time is optional 18 Mar 2009 [dg] */


int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;
   FD_brick ** fbr , * brax ;
   int iarg , a1,a2,a3 , aa1,aa2,aa3 , ival,kk,npix,dsiz,code ;
   THD_ivec3 iv_nxyz   , iv_xyzorient ;
   THD_fvec3 fv_xyzorg , fv_xyzdel ;
   float xyz_org[4] , xyz_del[4] , brfac_save ;
   MRI_IMAGE * im ;
   void * imar ;
   FILE * data_file ;
   THD_datablock * old_dblk , * new_dblk ;
   char new_prefix[THD_MAX_PREFIX] = "axialize" ;
   int verbose = 0 , nim , pim=2 ;
   int native_order , save_order ;  /* 23 Nov 1999 */

   int axord=0 ;          /* 06 Mar 2000 */
   char orients[4]="\0" ; /* 07 Dec 2001 */

   MRI_IMARR *im_array;             /* 12 Mar 2009 */
   MRI_IMAGE *svol_im;
   void  * svol ;

   /*- sanity check -*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3daxialize [options] dataset\n"
             "Purpose: Read in a dataset and write it out as a new dataset\n"
             "         with the data brick oriented as axial slices.\n"
             "         The input dataset must have a .BRIK file.\n"
             "         One application is to create a dataset that can\n"
             "         be used with the AFNI volume rendering plugin.\n"
             "\n"
             "Options:\n"
             " -prefix ppp  = Use 'ppp' as the prefix for the new dataset.\n"
             "               [default = 'axialize']\n"
             " -verb        = Print out a progress report.\n"
             "\n"
             "The following options determine the order/orientation\n"
             "in which the slices will be written to the dataset:\n"
             " -sagittal    = Do sagittal slice order [-orient ASL]\n"
             " -coronal     = Do coronal slice order  [-orient RSA]\n"
             " -axial       = Do axial slice order    [-orient RAI]\n"
             "                 This is the default AFNI axial order, and\n"
             "                 is the one currently required by the\n"
             "                 volume rendering plugin; this is also\n"
             "                 the default orientation output by this\n"
             "                 program (hence the program's name).\n"
             "\n"
             " -orient code = Orientation code for output.\n"
             "                The code must be 3 letters, one each from the\n"
             "                pairs {R,L} {A,P} {I,S}.  The first letter gives\n"
             "                the orientation of the x-axis, the second the\n"
             "                orientation of the y-axis, the third the z-axis:\n"
             "                 R = Right-to-left         L = Left-to-right\n"
             "                 A = Anterior-to-posterior P = Posterior-to-anterior\n"
             "                 I = Inferior-to-superior  S = Superior-to-inferior\n"
             "                If you give an illegal code (e.g., 'LPR'), then\n"
             "                the program will print a message and stop.\n"
             "          N.B.: 'Neurological order' is -orient LPI\n"
             " -frugal      = Write out data as it is rotated, a sub-brick at\n"
             "                a time. This saves a little memory and was the\n"
             "                previous behavior.\n"
             "                Note the frugal option is not available with NIFTI\n"
             "                datasets\n"
            ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3daxialize main"); machdep(); AFNI_logger("3daxialize",argc,argv);
   PRINT_VERSION("3daxialize") ;

   /*- scan options -*/

   iarg = 1 ;
   while( argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-orient") == 0 ){    /* 07 Dec 2001 */
         int xx,yy,zz ;
         MCW_strncpy(orients,argv[++iarg],4) ;
         if( strlen(orients) != 3 ){
           ERROR_exit("Bad code after -orient: not 3 characters long");
         }
         xx = ORCODE(orients[0]) ;
         yy = ORCODE(orients[1]) ; zz = ORCODE(orients[2]) ;
         if( xx < 0 || yy < 0 || zz < 0 ){
           ERROR_exit("Bad code after -orient: illegal characters");
         }
         if( !OR3OK(xx,yy,zz) ){
           ERROR_exit("Bad code after -orient: dependent axes");
         }
         axord = -1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-axial") == 0 ){     /* 06 Mar 2000 */
         axord = 0 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-sagittal") == 0 ){  /* 06 Mar 2000 */
         axord = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-coronal") == 0 ){   /* 06 Mar 2000 */
         axord = 2 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        iarg++ ;
        if( iarg >= argc ){
          ERROR_exit("Need argument after -prefix!") ;
        }
        MCW_strncpy( new_prefix , argv[iarg++] , THD_MAX_PREFIX ) ;
	 
        if( strstr(new_prefix,".nii") != NULL ) {  /* check for NIFTI mod - drg 08 Aug 2006 */
            write_output = True;
	    NIFTI_mode = True;
            if( strstr(new_prefix,".nii.gz") != NULL ) {
	       cmode = 0; /* force gzip compression  (actually zlib from nifti library)*/
	    }   
	}       
        else if( !THD_filename_ok(new_prefix) ){
            ERROR_exit("Illegal new prefix: %s",new_prefix);
         }
        continue ;
      }

      if( strncmp(argv[iarg],"-verbose",5) == 0 ){
         verbose++ ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-frugal",4) == 0 ){
         AXIAL_frugal = 1; iarg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[iarg]);
   }

   if(NIFTI_mode && AXIAL_frugal){
      ERROR_message("Frugality and NIFTI output do not mix.\n");
      ERROR_exit("Try without -frugal or with different output type.\n");
   }
   
   /*- get input dataset -*/

   old_dset = THD_open_dataset( argv[iarg] ) ;
   if( old_dset == NULL ){
      ERROR_exit("Can't open input dataset: %s",argv[iarg]) ;
   }

   if( verbose ) INFO_message("Loading input dataset %s",argv[iarg]) ;

   DSET_load(old_dset) ;  CHECK_LOAD_ERROR(old_dset) ;

   /*- setup output dataset -*/

   /* use FD bricks for axial, sagittal, coronal displays as basis */

   if( axord >= 0 ){
      fbr = THD_setup_bricks( old_dset ) ; brax = fbr[axord] ;
   } else {
      brax = THD_oriented_brick( old_dset , orients ) ;
      if( brax == NULL ){
         ERROR_exit("Can't use -orient code: %s",orients);
      }
   }

   new_dset = EDIT_empty_copy( old_dset ) ;

   tross_Copy_History( old_dset , new_dset ) ;
   tross_Make_History( "3daxialize" , argc,argv , new_dset ) ;

   /* number of points along each axis */

   LOAD_IVEC3( iv_nxyz , brax->n1 , brax->n2 , brax->n3 ) ;

   /* orientation codes for each axis */

   switch( axord ){  /* 06 Mar 2000 */

      case -1:{
         int xx=ORCODE(orients[0]) ,
             yy=ORCODE(orients[1]) , zz=ORCODE(orients[2]) ;

         LOAD_IVEC3( iv_xyzorient , xx,yy,zz ) ;
      }
      break ;

      case 0:
       LOAD_IVEC3( iv_xyzorient, ORI_R2L_TYPE, ORI_A2P_TYPE, ORI_I2S_TYPE ) ;
      break ;

      case 1:
       LOAD_IVEC3( iv_xyzorient, ORI_A2P_TYPE, ORI_S2I_TYPE, ORI_L2R_TYPE ) ;
      break ;

      case 2:
       LOAD_IVEC3( iv_xyzorient, ORI_R2L_TYPE, ORI_S2I_TYPE, ORI_A2P_TYPE ) ;
      break ;
   }

   /* grid spacing for each axis */

   LOAD_FVEC3( fv_xyzdel ,
               ORIENT_sign[iv_xyzorient.ijk[0]]=='+' ? brax->del1 : -brax->del1,
               ORIENT_sign[iv_xyzorient.ijk[1]]=='+' ? brax->del2 : -brax->del2,
               ORIENT_sign[iv_xyzorient.ijk[2]]=='+' ? brax->del3 : -brax->del3);

   UNLOAD_IVEC3( brax->a123 , a1,a2,a3 ) ;
   aa1 = abs(a1) ; aa2 = abs(a2) ; aa3 = abs(a3) ;
   xyz_org[1] = new_dset->daxes->xxorg ; xyz_del[1] = new_dset->daxes->xxdel ;
   xyz_org[2] = new_dset->daxes->yyorg ; xyz_del[2] = new_dset->daxes->yydel ;
   xyz_org[3] = new_dset->daxes->zzorg ; xyz_del[3] = new_dset->daxes->zzdel ;
   LOAD_FVEC3( fv_xyzorg ,
               (a1 > 0) ? xyz_org[aa1] : xyz_org[aa1]+(brax->n1-1)*xyz_del[aa1],
               (a2 > 0) ? xyz_org[aa2] : xyz_org[aa2]+(brax->n2-1)*xyz_del[aa2],
               (a3 > 0) ? xyz_org[aa3] : xyz_org[aa3]+(brax->n3-1)*xyz_del[aa3] );

   /*-- open output BRIK file --*/
   if ((cmode == COMPRESS_NOFILE)) { /* ignore compression for NIFTI - do in write
   automatically later */
      cmode = THD_get_write_compression() ; /* check env. variable for compression*/
      }

   EDIT_dset_items( new_dset ,
                       ADN_nxyz      , iv_nxyz ,
                       ADN_xyzdel    , fv_xyzdel ,
                       ADN_xyzorg    , fv_xyzorg ,
                       ADN_xyzorient , iv_xyzorient ,
                       ADN_prefix    , new_prefix ,
                    ADN_none ) ;

   if( DSET_NUM_TTOFF(new_dset) > 0 )
      EDIT_dset_items( new_dset , ADN_nsl , 0 , ADN_none ) ;  /* 28 Apr 1999 */

   /*- prepare to write new dataset -*/

   new_dblk = new_dset->dblk ;
   old_dblk = old_dset->dblk ;

   if (AXIAL_frugal)
      data_file   = COMPRESS_fopen_write( new_dblk->diskptr->brick_name, cmode ) ;
   else
      INIT_IMARR(im_array);  

   npix  = brax->n1 * brax->n2 ;

   /*- get slices from input, write to disk -*/

   if( verbose ){
      INFO_message("Writing new dataset .BRIK"); fflush(stdout);
      pim = brax->n3 / 5 ; if( pim <= 1 ) pim = 2 ;
   }

   native_order = mri_short_order() ;                           /* 23 Nov 1999 */
   save_order   = (DSET_BYTEORDER(new_dset) > 0) ? DSET_BYTEORDER(new_dset)
                                                 : THD_get_write_order() ;

   for( nim=ival=0 ; ival < DSET_NVALS(old_dset) ; ival++ ){
      brfac_save = DBLK_BRICK_FACTOR(new_dblk,ival) ;
      DBLK_BRICK_FACTOR(new_dblk,ival) = 0.0 ;
      DBLK_BRICK_FACTOR(old_dblk,ival) = 0.0 ;
      dsiz = mri_datum_size( DSET_BRICK_TYPE(new_dset,ival) ) ;

      if(!AXIAL_frugal) {
           /* copy the sub-brick volume, svol, into an MRI_IMAGE structure  and 
               then append that to the image array */
           svol_im = mri_new_vol( brax->n1 , brax->n2 , brax->n3, DSET_BRICK_TYPE(new_dset,ival)) ;
           svol = mri_data_pointer(svol_im);
      }


      for( kk=0 ; kk < brax->n3 ; kk++ ){
         im   = FD_warp_to_mri( kk , ival , brax ) ;
         imar = mri_data_pointer(im) ;
         if( save_order != native_order ){                   /* 23 Nov 1999 */
            switch( im->kind ){
               case MRI_short:   mri_swap2(  npix,imar) ; break ;
               case MRI_float:
               case MRI_int:     mri_swap4(  npix,imar) ; break ;
               case MRI_complex: mri_swap4(2*npix,imar) ; break ;

               default:
                  ERROR_exit(
                    "** Illegal input brick type=%s\n",MRI_TYPE_name[im->kind]) ;
            }
         }

         if(AXIAL_frugal) {
            code = fwrite( imar , dsiz , npix , data_file ) ;
            mri_free(im) ;
         }
         else {
            memcpy((char*)svol+(kk*npix*dsiz), imar, npix*dsiz );
         }

         if( verbose && (++nim)%pim == 1 ){ printf("."); fflush(stdout); }
      }


      DBLK_BRICK_FACTOR(new_dblk,ival) = brfac_save ;
      DSET_unload_one(old_dset,ival) ;

      if(!AXIAL_frugal) {
           /* copy the sub-brick volume, svol, into an MRI_IMAGE structure  and 
               then append that to the image array */
           ADDTO_IMARR(im_array, svol_im);
      }

      if( verbose ){ printf("!"); fflush(stdout); }
   }
   if(AXIAL_frugal)
      COMPRESS_fclose(data_file) ;
   else
      new_dset->dblk->brick = im_array;   /* update pointer to data */

   if( verbose ){ printf("\nUnloading old dataset\n") ; fflush(stdout); }

   DSET_unload( old_dset ) ;

   /*- do the output header -*/
   if(AXIAL_frugal) {
      DSET_load( new_dset ) ;
   }
      else write_output = 1;      /* have to write everything out now*/
   THD_load_statistics( new_dset ) ;

   if( verbose ){ printf("\nWriting new dataset\n") ; fflush(stdout); }

   code = THD_write_3dim_dataset(NULL,NULL,new_dset,write_output); /* (re)write output file */
   if( !code )
       ERROR_exit("Could not write dataset");

   if( verbose )
      INFO_message("Wrote new dataset: %s",DSET_BRIKNAME(new_dset)) ;
   DSET_unload( new_dset ) ;

    exit(0) ;
}
