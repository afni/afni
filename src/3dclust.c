/*---------------------------------------------------------------------------*/
/*
  This program performs cluster detection in 3D datasets.
*/

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dclust"                       /* name of this program */
#define PROGRAM_AUTHOR "R. W. Cox et al."                  /* program author */
#define PROGRAM_DATE "26 March 1999"             /* date of last program mod */

/*---------------------------------------------------------------------------*/

/* Modified 3/26/99 by BDW to enable -1erode and -1dilate options. */
/* Modified 1/19/99 by BDW to allow use of signed intensities in calculation
     of cluster averages, etc. (-noabs option), as requested by H. Garavan */
/* Modified 1/24/97 by BDW to combine the previous modifications  */
/* Modified 12/27/96 by BDW  Corrections to the SEM calculations. */
/* Modified 4/19/96 by MSB to give cluster intensity and extent information */
/* Modified 11/1/96 by MSB to give cluster intensity standard error of the mean
   (SEM) and average intensity and SEM for all voxels (globally)  

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

/*---------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>

#include "mrilib.h"

static EDIT_options CL_edopt ;
static int CL_ivfim=-1 , CL_ivthr=-1 ;

static int CL_nopt ;

static int CL_summarize = 0 ;

static int CL_noabs = 0;   /* BDW  19 Jan 1999 */

static int CL_verbose = 0 ; /* RWC 01 Nov 1999 */

static int CL_quiet = 0;   /* MSB 02 Dec 1999 */

/**-- RWCox: July 1997
      Report directions based on AFNI_ORIENT environment --**/

static THD_coorder CL_cord ;

int compare_cluster( void * , void * ) ;
void CL_read_opts( int , char ** ) ;
#define CL_syntax(str) \
  do{ fprintf(stderr,"\n*** %s\a\n",str) ; exit(1) ; } while(0)

void MCW_fc7( float qval , char * buf ) ;

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float rmm , vmul ;
   int iarg ;
   THD_3dim_dataset * dset ;
   void * vfim ;
   int nx,ny,nz , nxy,nxyz , ivfim ,
       iclu , ptmin , ipt , ii,jj,kk , ndet , nopt,noneg=0 ;
   float dx,dy,dz , xx,yy,zz,mm , xxsum,yysum,zzsum,mmsum , volsum , fimfac ,
                                  xxmax,yymax,zzmax,mmmax , ms, mssum , msmax ,
         RLmax, RLmin, APmax, APmin, ISmax, ISmin;
   double mean, sem, sqsum, glmmsum, glsqsum, glmssum, glmean, glxxsum, glyysum, glzzsum;
   MCW_cluster_array * clar , * clbig ;
   MCW_cluster       * cl ;
   THD_fvec3 fv ;
   int nvox_total ;
   float vol_total ;
   char buf1[16],buf2[16],buf3[16] ;

   if( argc < 4 || strncmp(argv[1],"-help",4) == 0 ){
      printf ("\n\n");
      printf ("Program: %s \n", PROGRAM_NAME);
      printf ("Author:  %s \n", PROGRAM_AUTHOR); 
      printf ("Date:    %s \n", PROGRAM_DATE);
      printf ("\n");
      fprintf(stderr,
          "Copyright 1994-9 Medical College of Wisconsin\n\n"
          "Simple-minded Cluster Detection in 3D Datasets\n"
          "Usage: 3dclust [editing options] [-summarize] [-verbose] rmm vmul dset ...\n"
          "  where rmm  = cluster connection radius (mm);\n"
          "        vmul = minimum cluster volume (micro-liters)\n"
          "               (both rmm and vmul must be positive);\n"
          "        dset = input dataset (more than one allowed).\n"
          "  The report is sent to stdout.\n"
          "  "
          "  The -noabs option uses the signed voxel intensities (not the \n"
          "     abs. value) for calculation of the mean and SEM \n"
          "  The -summarize option will write out only the total\n"
          "     nonzero voxel count and volume for each dataset.\n"
          "  The -verbose option prints out a progress report (to stderr)\n"
          "     as the computations proceed.\n"
          "  The -quiet option suppresses all non-essential output. \n"
          "  The editing options are as in 3dmerge\n"
          "     (including use of the -1dindex and -1tindex options).\n"
          "  The program does not work on complex-valued datasets!\n"
          "  Using the -1noneg option is strongly recommended!\n "
          "  SEM values are not realistic for interpolated data sets! \n"
          "  A ROUGH correction is to multiply the SEM of the interpolated\n"
          "     data set by the square root of the number of interpolated \n"
          "     voxels per original voxel. \n" ) ;
      exit(0) ;
   }

   THD_coorder_fill( my_getenv("AFNI_ORIENT") , &CL_cord ) ; /* July 1997 */
   CL_read_opts( argc , argv ) ;
   nopt = CL_nopt ;

 /*----- Identify software -----*/
  if( !CL_quiet ){
     printf ("\n\n");
     printf ("Program: %s \n", PROGRAM_NAME);
     printf ("Author:  %s \n", PROGRAM_AUTHOR); 
     printf ("Date:    %s \n", PROGRAM_DATE);
     printf ("\n");
  }

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

   /* BDW  26 March 1999  */
   else
     {
       if( CL_edopt.clust_rmm > 0.0 )  /* 01 Nov 1999 */
          fprintf(stderr,"*** Warning: replacing -1clust values with later inputs!\n") ;

       CL_edopt.clust_rmm = rmm;
       CL_edopt.clust_vmul = vmul;
     }

   /**-- loop over datasets --**/

   dset = NULL ;
   for( iarg=nopt ; iarg < argc ; iarg++ ){
      if( dset != NULL ) THD_delete_3dim_dataset( dset , False ) ; /* flush old   */
      dset = THD_open_one_dataset( argv[iarg] ) ;                  /* open new    */
      if( dset == NULL ){                                          /* failed?     */
         fprintf(stderr,"*** Warning: skipping dataset %s\n",argv[iarg]) ;
         continue ;
      }
      if( DSET_NUM_TIMES(dset) > 1 &&
          ( CL_edopt.iv_fim < 0 || CL_edopt.iv_thr < 0 ) ){      /* no time     */

         fprintf(stderr,                                           /* dependence! */
                 "*** cannot use time-dependent dataset %s\n",argv[iarg]) ;
         continue ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ; /* don't mmap  */
      if( CL_verbose )
         fprintf(stderr,"+++ Loading dataset %s\n",argv[iarg]) ;
      THD_load_datablock( dset->dblk , NULL ) ;                    /* read in     */
      EDIT_one_dataset( dset , &CL_edopt ) ;                       /* editing?    */

      if( CL_ivfim < 0 )
         ivfim  = DSET_PRINCIPAL_VALUE(dset) ;                     /* useful data */
      else
         ivfim  = CL_ivfim ;                                       /* 16 Sep 1999 */

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
  if( !CL_quiet ){

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
          "[Voxel dimensions    = %.3f mm X %.3f mm X %.3f mm ]\n",
           argv[iarg] ,
#if 0
           dset->self_name , dset->label1 ,
#endif
           rmm , vmul , dx*dy*dz ,
           MRI_TYPE_name[ DSET_BRICK_TYPE(dset,ivfim) ] ,
           dx,dy,dz );

         if (CL_noabs)                                   /* BDW  19 Jan 1999 */
           printf ("Mean and SEM based on Signed voxel intensities: \n\n");
         else
           printf ("Mean and SEM based on Absolute Value "
                   "of voxel intensities: \n\n");

         printf (
"Volume  CM %s  CM %s  CM %s  min%s  max%s  min%s  max%s  min%s  max%s    Mean     SEM    Max Int  MI %s  MI %s  MI %s\n"
"------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -------  -------  -------  -----  -----  -----\n",

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
         if (CL_noabs)                                   /* BDW  19 Jan 1999 */
           printf ("Mean and SEM based on Signed voxel intensities: \n");
         else
           printf ("Mean and SEM based on Absolute Value "
                   "of voxel intensities: \n");
         printf("Cluster summary for file %s\n",argv[iarg]);
         printf("Volume  CM %s  CM %s  CM %s  Mean    SEM    \n",ORIENT_tinystr[ CL_cord.xxor ],ORIENT_tinystr[ CL_cord.yyor ] ,ORIENT_tinystr[ CL_cord.zzor ]);



       }
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
      glmmsum = glmssum = glsqsum = glxxsum = glyysum = glzzsum = 0;

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

            ms = cl->mag[ipt];                         /* BDW  18 Jan 1999 */
            mm = fabs(ms);

	    mssum += ms;
	    mmsum += mm;
            sqsum += mm * mm;
            xxsum += mm * xx ; yysum += mm * yy ; zzsum += mm * zz ;
            if( mm > mmmax ){
               xxmax = xx ; yymax = yy ; zzmax = zz ;
               mmmax = mm ; msmax = ms ;
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

	 glmssum += mssum;
	 glmmsum += mmsum;
	 glsqsum += sqsum ;
	 glxxsum += xxsum;
	 glyysum += yysum;
	 glzzsum += zzsum;
	 
         ndet++ ;
         xxsum /= mmsum ; yysum /= mmsum ; zzsum /= mmsum ;

	 if (CL_noabs)   mean = mssum / cl->num_pt;     /* BDW  19 Jan 1999 */ 
         else            mean = mmsum / cl->num_pt; 

         if( fimfac != 0.0 )
	   { mean  *= fimfac;  msmax *= fimfac; 
	     sqsum *= fimfac*fimfac; }                      /* BDW 12/27/96 */

	 /* MSB 11/1/96  Calculate SEM using SEM^2=s^2/N, 
	    where s^2 = (SUM Y^2)/N - (Ymean)^2
	    where sqsum = (SUM Y^2 ) */

	 if (cl->num_pt > 1)
	   {
	     sem = (sqsum - (cl->num_pt * mean * mean)) / (cl->num_pt - 1);
	     if (sem > 0.0) sem = sqrt( sem / cl->num_pt );  else sem = 0.0;
	   }
	 else
	   sem = 0.0;

         if( !CL_summarize ){
           MCW_fc7(mean, buf1) ;
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

      if (CL_noabs)   glmean = glmssum / nvox_total;    /* BDW  19 Jan 1999 */
      else            glmean = glmmsum / nvox_total; 

      /* BDW 12/27/96 */
      if( fimfac != 0.0 ){ glsqsum *= fimfac*fimfac ; glmean *= fimfac ; }
      if (nvox_total > 1)
	{
	  sem = (glsqsum - (nvox_total*glmean*glmean)) / (nvox_total - 1);
	  if (sem > 0.0) sem = sqrt( sem / nvox_total );  else sem = 0.0;
	}
      else
	sem = 0.0;

     glxxsum /= glmmsum ; glyysum /= glmmsum ; glzzsum /= glmmsum ;

      /* MSB 11/1/96 Modified so that mean and SEM would print in correct column */
      if( CL_summarize )
	 {   if( !CL_quiet )
	         printf( "------  -----  -----  ----- -------- -------- \n");
		printf("%6.0f  %5.1f  %5.1f  %5.1f %8.1f %6.3f\n" , vol_total, glxxsum, glyysum, glzzsum, glmean, sem ) ;
      }
	 else if( ndet > 1  )
	{
          MCW_fc7(glmean ,buf1) ;
          MCW_fc7(sem    ,buf3) ;
	  if( !CL_quiet )
	  	printf ("------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -------  -------  -------  -----  -----  -----\n");
	     printf ("%6.0f  %5.1f  %5.1f  %5.1f                                            %7s  %7s                             \n",
		  vol_total, glxxsum, glyysum, glzzsum, buf1, buf3 ) ;
	}

   }

   exit(0) ;
}


/*---------------------------------------------------------------------------*/
/*
   read the arguments, and load the global variables
*/

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

      /**** Sep 16 1999: -1tindex and -1dindex ****/

      if( strncmp(argv[nopt],"-1dindex",5) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -1dindex!\n") ; exit(1) ;
         }
         CL_ivfim = CL_edopt.iv_fim = (int) strtod( argv[nopt++] , NULL ) ;
         continue ;
      }

      if( strncmp(argv[nopt],"-1tindex",5) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -1tindex!\n") ; exit(1) ;
         }
         CL_ivthr = CL_edopt.iv_thr = (int) strtod( argv[nopt++] , NULL ) ;
         continue ;
      }

      /**** -summarize ****/

      if( strncmp(argv[nopt],"-summarize",5) == 0 ){
         CL_summarize = 1 ;
         nopt++ ;
         continue ;
      }

      /**** -verbose ****/

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         CL_verbose = CL_edopt.verbose = 1 ;
         nopt++ ;
         continue ;
      }

      /**** -quiet ****/

      if( strncmp(argv[nopt],"-quiet",5) == 0 ){
         CL_quiet = 1 ;
	    nopt++ ;
         continue ;
      }

      /**** -orient code ****/

      if( strncmp(argv[nopt],"-orient",5) == 0 ){
         THD_coorder_fill( argv[++nopt] , &CL_cord ) ; /* July 1997 */
         nopt++ ; continue ;
      }

      /**** -noabs ****/                                /* BDW  19 Jan 1999 */

      if( strncmp(argv[nopt],"-noabs",6) == 0 ){
         CL_noabs = 1 ;
         nopt++ ;
         continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\a\n",argv[nopt]) ;
      exit(1) ;

   }  /* end of loop over options */

#ifdef CLDEBUG
printf("*** finished with options\n") ;
#endif

   CL_nopt = nopt ;
   return ;
}

/*---------------------------------------------------------------------------*/

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
/*---------------------------------------------------------------------------*/
