

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994-7 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/
/* Modified 1/24/97 by BDW to combine the previous modifications */
/* Modified 4/19/96 by MSB to give cluster intensity and extent information */
/* Modified 11/1/96 by MSB to give cluster intensity standard error of the mean (SEM)
and average intensity and SEM for all voxels (globally)
Modified source files were
3dclust.c
Modified variables were:
sem (added) contains calculated SEM
sqsum (added) contains cumulative sum of squares
glmm (added)   global mean
glsqsum (added) global sum of squares
Major modifications to code are noted with  "MSB 11/1/96" and comments
Testing and Verification
Program was run on data file JKt3iravfm0.5cymc+orig
3dclust -1noneg -1thresh 0.50 15 100 gives a cluster of
 138  15525.0    -32.8     46.2    -26.7    -16.9     39.4     10.0    338.7     12.4    984.0
Voxel values in this cluster were downloaded to Excel;
the avg, SE, and max were calculated and identical values were found.

information and summary information
Important User Notes:
- center of mass calculations makes use of the intensity at each point; this
should perhaps be selectable
- cluster calculations are all done on the absolute value of the intensity;
  hence, positive and negative voxels can be grouped together into the same
  cluster, which skews results; to prevent this, use the -1noneg option
- SEM values are not realistic for interpolated data sets (because comparisons
are not independent) a ROUGH correction is to multiply the interpolated SEM
of the interpolated data set by the square root of the number of interpolated
voxels per original voxel

*/
/*----------------------------------------------------------------------------
  Modified 12/27/96 by BDW
  Corrections to the SEM calculations.
----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>

#include "mrilib.h"

static EDIT_options CL_edopt ;

static int CL_nopt ;

static int CL_summarize = 0 ;

/**-- RWCox: July 1997
      Report directions based on AFNI_ORIENT environment --**/

static THD_coorder CL_cord ;

int compare_cluster( void * , void * ) ;
void CL_read_opts( int , char ** ) ;
#define CL_syntax(str) \
  do{ fprintf(stderr,"\n*** %s\a\n",str) ; exit(1) ; } while(0)

void MCW_fc7( float qval , char * buf ) ;

int main( int argc , char * argv[] )
{
   float rmm , vmul ;
   int iarg ;
   THD_3dim_dataset * dset ;
   void * vfim ;
   int nx,ny,nz , nxy,nxyz , ivfim ,
       iclu , ptmin , ipt , ii,jj,kk , ndet , nopt,noneg=0 ;
   float dx,dy,dz , xx,yy,zz,mm , xxsum,yysum,zzsum,mmsum , volsum , fimfac ,
                                  xxmax,yymax,zzmax,mmmax , mssum,msmax ,
   		RLmax, RLmin, APmax, APmin, ISmax, ISmin;
   double sem, sqsum, glmmsum, glsqsum;
   MCW_cluster_array * clar , * clbig ;
   MCW_cluster       * cl ;
   THD_fvec3 fv ;
   int nvox_total ;
   float vol_total ;
   char buf1[16],buf2[16],buf3[16] ;

   if( argc < 4 || strncmp(argv[1],"-help",4) == 0 ){
      fprintf(stderr,
             "Copyright 1994-7 Medical College of Wisconsin\n\n"
             "Simple-minded Cluster Detection in 3D Datasets\n"
             "Usage: 3dclust [editing options] [-summarize] rmm vmul dset ... \n"
             "  where rmm  = cluster connection radius (mm);\n"
             "        vmul = minimum cluster volume (micro-liters)\n"
             "               (both rmm and vmul must be positive);\n"
             "        dset = input dataset (more than one allowed).\n"
             "  The report is sent to stdout.\n"
             "  N.B.: The -summarize option will write out only the total\n"
             "        nonzero voxel count and volume for each dataset.\n"
             "  N.B.: The editing options are as in 3dmerge.\n"
             "  N.B.: The program does not work on complex-valued datasets!\n"
             "  N.B.: Using the -1noneg option is strongly recommended!	\n "
             " N.B.: SEM values are not realistic for interpolated data sets! \n"
             "       a ROUGH correction is to multiply the SEM of the interpolated \n"
             "       data set by the square root of the number of interpolated \n"
	     "       voxels per original voxel. \n" ) ;
      exit(0) ;
   }

   THD_coorder_fill( getenv("AFNI_ORIENT") , &CL_cord ) ; /* July 1997 */
   CL_read_opts( argc , argv ) ;
   nopt = CL_nopt ;

   if( nopt+3 >  argc ){
      fprintf(stderr,"\n*** No rmm or vmul arguments?\a\n") ;
      exit(1) ;
   }

   rmm  = strtod( argv[nopt++] , NULL ) ;
   vmul = strtod( argv[nopt++] , NULL ) ;
   if( rmm <= 0.0 || vmul <= 0.0 ){
      fprintf(stderr,"\n*** Illegal rmm=%f and/or vmul=%f\a\n",rmm,vmul) ;
      exit(1) ;
   }

   /**-- loop over datasets --**/

   dset = NULL ;
   for( iarg=nopt ; iarg < argc ; iarg++ ){
      if( dset != NULL ) THD_delete_3dim_dataset( dset , False ) ; /* flush old   */
      dset = THD_open_one_dataset( argv[iarg] ) ;                  /* open new    */
      if( dset == NULL ) continue ;                                /* failed?     */
      if( DSET_NUM_TIMES(dset) > 1 ){                              /* no time     */
         fprintf(stderr,                                           /* dependence! */
                 "*** cannot use time-dependent dataset %s\n",argv[iarg]) ;
         continue ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ; /* don't mmap  */
      THD_load_datablock( dset->dblk , NULL ) ;                    /* read in     */
      EDIT_one_dataset( dset , &CL_edopt ) ;                       /* editing?    */
      ivfim  = DSET_PRINCIPAL_VALUE(dset) ;                        /* useful data */
      vfim   = DSET_ARRAY(dset,ivfim) ;                            /* ptr to data */
      fimfac = DSET_BRICK_FACTOR(dset,ivfim) ;                     /* scl factor  */
      if( vfim == NULL ){
         fprintf(stderr,"*** cannot access data in dataset %s\a\n",argv[iarg]) ;
         continue ;
      }

      if( ! AFNI_GOOD_FUNC_DTYPE( DSET_BRICK_TYPE(dset,ivfim) ) ){
         fprintf(stderr,"*** Illegal datum type in dataset %s\a\n",argv[iarg]) ;
         continue ;
      }

      nx    = dset->daxes->nxx ; dx = fabs(dset->daxes->xxdel) ;
      ny    = dset->daxes->nyy ; dy = fabs(dset->daxes->yydel) ;
      nz    = dset->daxes->nzz ; dz = fabs(dset->daxes->zzdel) ;
      nxy   = nx * ny ; nxyz = nxy * nz ;
      ptmin = vmul / (dx*dy*dz) + 0.99 ;

#if 0
      if( rmm < dx && rmm < dy && rmm < dz ){
         fprintf(stderr,
            "*** file %s: cluster rmm %f smaller than voxels dx=%f dy=%f dz=%f\n",
            argv[iarg],rmm,dx,dy,dz ) ;
         continue ;
      }
#endif

      /*-- print report header --*/

      if( !CL_summarize ){
         printf( "\n"
          "Cluster report for file %s\n"
#if 0
          "[3D Dataset Name: %s ]\n"
          "[    Short Label: %s ]\n"
#endif
          "[Connectivity radius = %.2f mm  Volume threshold = %.2f ]\n"
          "[Single voxel volume = %.1f (microliters) ]\n"
          "[Voxel datum type    = %s ]\n"
          "[Voxel dimensions    = %.3f mm X %.3f mm X %.3f mm ]\n"
"Volume  CM %s  CM %s  CM %s  min%s  max%s  min%s  max%s  min%s  max%s  Mean|*|    SEM    Max Int  MI %s  MI %s  MI %s\n"
"------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -------  -------  -------  -----  -----  -----\n",
           argv[iarg] ,
#if 0
           dset->self_name , dset->label1 ,
#endif
           rmm , vmul , dx*dy*dz ,
           MRI_TYPE_name[ DSET_BRICK_TYPE(dset,ivfim) ] ,
           dx,dy,dz ,
           ORIENT_tinystr[ CL_cord.xxor ] ,
           ORIENT_tinystr[ CL_cord.yyor ] ,
           ORIENT_tinystr[ CL_cord.zzor ] ,
           ORIENT_tinystr[ CL_cord.xxor ] , ORIENT_tinystr[ CL_cord.xxor ] ,
           ORIENT_tinystr[ CL_cord.yyor ] , ORIENT_tinystr[ CL_cord.yyor ] ,
           ORIENT_tinystr[ CL_cord.zzor ] , ORIENT_tinystr[ CL_cord.zzor ] ,
           ORIENT_tinystr[ CL_cord.xxor ] ,
           ORIENT_tinystr[ CL_cord.yyor ] ,
           ORIENT_tinystr[ CL_cord.zzor ]
          ) ;

       } else {
          printf("\nCluster summary for file %s\n# Vox  Volume    Mean |*|  SEM     \n",argv[iarg]) ;
       }

      clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz ,
                                DSET_BRICK_TYPE(dset,ivfim) , vfim , rmm ) ;
      PURGE_DSET( dset ) ;

#ifdef CLDEBUG
if( clar != NULL ){
printf("3DCLUST: found %d clusters\n",clar->num_clu) ;
for( iclu=0 ; iclu < clar->num_clu ; iclu++)
  printf(" cluster %d has %d voxels\n",iclu,clar->clar[iclu]->num_pt) ;
}
#endif

      if( clar == NULL || clar->num_clu == 0 ){
         printf("*** NO CLUSTERS FOUND ***\n") ;
         if( clar != NULL ) DESTROY_CLARR(clar) ;
         continue ;
      }

    /** edit for volume (June 1995) **/
      INIT_CLARR(clbig) ;
      for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
         cl = clar->clar[iclu] ;
         if( cl->num_pt >= ptmin ){ /* big enough */
            ADDTO_CLARR(clbig,cl) ;    /* copy pointer */
            clar->clar[iclu] = NULL ;  /* null out original */
         }
      }
      DESTROY_CLARR(clar) ;
      clar = clbig ;
      if( clar == NULL || clar->num_clu == 0 ){
         printf("*** NO CLUSTERS FOUND ***\n") ;
         if( clar != NULL ) DESTROY_CLARR(clar) ;
         continue ;
      }

    /** end of June 1995 addition **/

      if( clar->num_clu < 1000 ){
         SORT_CLARR(clar) ;
      } else if( !CL_summarize ){
         printf("*** TOO MANY CLUSTERS TO SORT BY VOLUME ***\n") ;
      }
      ndet = 0 ;

      vol_total = nvox_total = 0 ;
      glmmsum = glsqsum = 0;
      	
      for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
         cl = clar->clar[iclu] ;
         if( cl == NULL || cl->num_pt < ptmin ) continue ;  /* no good */

         volsum = cl->num_pt * dx*dy*dz ;
         xxsum = yysum = zzsum = mmsum = mssum = 0.0 ;
         xxmax = yymax = zzmax = mmmax = msmax = 0.0 ;
         sqsum = sem = 0;

         /* These should be pegged at whatever actual max/min values are */
         RLmax = APmax = ISmax = -1000;
         RLmin = APmin = ISmin = 1000;

         for( ipt=0 ; ipt < cl->num_pt ; ipt++ ){

#if 0
/** this is obsolete and nonfunctional code **/
            IJK_TO_THREE( cl->ijk[ipt] , ii,jj,kk , nx,nxy ) ;
#endif
            ii = cl->i[ipt] ; jj = cl->j[ipt] ; kk = cl->k[ipt] ;

            fv = THD_3dind_to_3dmm( dset , TEMP_IVEC3(ii,jj,kk) ) ;
            fv = THD_3dmm_to_dicomm( dset , fv ) ;
            xx = fv.xyz[0] ; yy = fv.xyz[1] ; zz = fv.xyz[2] ;
            THD_dicom_to_coorder( &CL_cord , &xx,&yy,&zz ) ;  /* July 1997 */
            mm = fabs(cl->mag[ipt]) ; mmsum += mm ; mssum += cl->mag[ipt] ;
            sqsum += mm * mm;
            xxsum += mm * xx ; yysum += mm * yy ; zzsum += mm * zz ;
            if( mm > mmmax ){
               xxmax = xx ; yymax = yy ; zzmax = zz ;
               mmmax = mm ; msmax = cl->mag[ipt] ;
            }

	    /* Dimensions: */
            if ( xx > RLmax )
            	RLmax = xx;
            if ( xx < RLmin )
            	RLmin = xx;	
            if ( yy > APmax )
            	APmax = yy;
            if ( yy < APmin )
            	APmin = yy;		
            if ( zz > ISmax )
            	ISmax = zz;
            if ( zz < ISmin )
            	ISmin = zz;

         }
         if( mmsum == 0.0 ) continue ;

	 glmmsum += mmsum;
	 glsqsum += sqsum ;

         ndet++ ;
         xxsum /= mmsum ; yysum /= mmsum ; zzsum /= mmsum ;
         mssum /= cl->num_pt ; mmsum /= cl->num_pt ;
         if( fimfac != 0.0 ){ mssum *= fimfac ; mmsum *= fimfac ; msmax *= fimfac ; }

	 /* MSB 11/1/96  Calculate SEM using SEM^2=s^2/N, where s^2 = (SUM Y^2)/N - (Ymean)^2
	 where sqsum = (SUM Y^2 )*/

	 /* BDW 12/27/96 */
	 if( fimfac != 0.0 ){ sqsum *= fimfac*fimfac ; }
	 if (cl->num_pt > 1)
	   sem = (sqsum - (cl->num_pt * mmsum * mmsum)) / (cl->num_pt - 1);
	 else
	   sem = 0.0;
	 sem = sqrt( sem / cl->num_pt );

         if( !CL_summarize ){
           MCW_fc7(mmsum,buf1) ;
           MCW_fc7(msmax,buf2) ;
           MCW_fc7(sem  ,buf3) ;

	   printf("%6.0f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %7s  %7s  %7s  %5.1f  %5.1f  %5.1f \n",
		  volsum, xxsum, yysum, zzsum, RLmin, RLmax, APmin, APmax, ISmin, ISmax, buf1, buf3, buf2, xxmax, yymax, zzmax ) ;
         }

         nvox_total += cl->num_pt ;
         vol_total  += volsum ;

      }

      DESTROY_CLARR(clar) ;
      if( ndet == 0 )
         printf("*** NO CLUSTERS FOUND ABOVE THRESHOLD VOLUME ***\n") ;

      /* MSB 11/1/96  Calculate global SEM */
      glmmsum /= nvox_total;

      /* BDW 12/27/96 */
      if( fimfac != 0.0 ){ glsqsum *= fimfac*fimfac ; glmmsum *= fimfac ; }
      if (nvox_total > 1)
	sem = (glsqsum - (nvox_total * glmmsum * glmmsum)) / (nvox_total - 1);
      else
	sem = 0.0;
      sem = sqrt( sem / nvox_total );

      /* MSB 11/1/96 Modified so that mean and SEM would print in correct column */
      if( CL_summarize )
         printf( "----- --------- -------- -------- \n"
                 "%5d %9.1f %8.1f %6.3f\n" , nvox_total , vol_total, glmmsum, sem ) ;
      else if( ndet > 1  )
	{
          MCW_fc7(glmmsum,buf1) ;
          MCW_fc7(sem    ,buf3) ;
	  printf ("------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -------  -------  -------  -----  -----  -----\n");
	   printf("%6.0f                                                                 %7s  %7s                             \n",
		  vol_total, buf1, buf3 ) ;
	}

   }

   exit(0) ;
}

/*--------------------------------------------------------------------
   read the arguments, and load the global variables
----------------------------------------------------------------------*/

#ifdef CLDEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

void CL_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  ival , kk ;

   INIT_EDOPT( &CL_edopt ) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** check for editing options ****/

      ival = EDIT_check_argv( argc , argv , nopt , &CL_edopt ) ;
      if( ival > 0 ){
         nopt += ival ;
         continue ;
      }

      /**** -summarize ****/

      if( strncmp(argv[nopt],"-summarize",5) == 0 ){
         CL_summarize = 1 ;
         nopt++ ;
         continue ;
      }

      /**** -orient code ****/

      if( strncmp(argv[nopt],"-orient",5) == 0 ){
         THD_coorder_fill( argv[++nopt] , &CL_cord ) ; /* July 1997 */
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\a\n",argv[nopt]) ;
      exit(-1) ;

   }  /* end of loop over options */

#ifdef CLDEBUG
printf("*** finished with options\n") ;
#endif

   CL_nopt = nopt ;
   return ;
}

/*-----------------------------------------------*/

void MCW_fc7( float qval , char * buf )
{
   float aval = fabs(qval) ;
   int lv , il ;
   char lbuf[16] ;

   /* special case if the value is an integer */

   lv = (int) qval ;

   if( qval == lv && abs(lv) < 100000 ){
      if( lv >= 0 ) sprintf( buf , " %d" , lv ) ;
      else          sprintf( buf , "%d"  , lv ) ;
      return ;
   }

/* macro to strip trailing zeros from output */

#define BSTRIP \
   for( il=6 ; il>1 && lbuf[il]=='0' ; il-- ) lbuf[il] = '\0'

   /* noninteger: choose floating format based on magnitude */

   lv = (int) (10.0001 + log10(aval)) ;

   switch( lv ){

      default:
         if( qval > 0.0 ) sprintf( lbuf , "%7.1e" , qval ) ;
         else             sprintf( lbuf , "%7.0e" , qval ) ;
      break ;

      case  7:  /* 0.001 -0.01  */
      case  8:  /* 0.01  -0.1   */
      case  9:  /* 0.1   -1     */
      case 10:  /* 1     -9.99  */
         sprintf( lbuf , "%7.4f" , qval ) ; BSTRIP ; break ;

      case 11:  /* 10-99.9 */
         sprintf( lbuf , "%7.3f" , qval ) ; BSTRIP ; break ;

      case 12:  /* 100-999.9 */
         sprintf( lbuf , "%7.2f" , qval ) ; BSTRIP ; break ;

      case 13:  /* 1000-9999.9 */
         sprintf( lbuf , "%7.1f" , qval ) ; BSTRIP ; break ;

      case 14:  /* 10000-99999.9 */
         sprintf( lbuf , "%7.0f" , qval ) ; break ;

   }
   strcpy(buf,lbuf) ;
   return ;
}
