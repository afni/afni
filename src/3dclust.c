/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program performs cluster detection in 3D datasets.
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME   "3dclust"                     /* name of this program */
#define PROGRAM_AUTHOR "RW Cox et al"                      /* program author */
#define PROGRAM_DATE   "29 Nov 2001"             /* date of last program mod */

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

/*-- 29 Nov 2001: RWCox adds the -prefix option --*/

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

static char * CL_prefix = NULL ; /* 29 Nov 2001 -- RWCox */

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
   THD_3dim_dataset *dset=NULL ;
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
   float dxf,dyf,dzf ;                  /* 24 Jan 2001: for -dxyz=1 option */

   if( argc < 4 || strncmp(argv[1],"-help",4) == 0 ){
      printf ("\n\n");
      printf ("Program: %s \n", PROGRAM_NAME);
      printf ("Author:  %s \n", PROGRAM_AUTHOR); 
      printf ("Date:    %s \n", PROGRAM_DATE);
      printf ("\n");
      printf(
          "Simple-minded Cluster Detection in 3D Datasets\n"
          "Usage: 3dclust [editing options] [other options] rmm vmul dset ...\n"
          "  where rmm  = cluster connection radius (mm);\n"
          "        vmul = minimum cluster volume (micro-liters)\n"
          "               (both rmm and vmul must be positive);\n"
          "        dset = input dataset (more than one allowed).\n"
          "  The report is sent to stdout.\n"
          "\n"
          "Options\n"
          "-------\n"
          "* Editing options are as in 3dmerge\n"
          "  (including -1thresh, -1dindex, -1tindex, -dxyz=1 options)\n"
          "\n"
          "* -noabs      ==> use the signed voxel intensities (not the abs\n"
          "                  value) for calculation of the mean and SEM\n"
          "\n"
          "* -summarize  ==> write out only the total nonzero voxel\n"
          "                  count and volume for each dataset\n"
          "\n"
          "* -nosum      ==> suppress printout of the totals\n"
          "\n"
          "* -verb       ==> print out a progress report (to stderr)\n"
          "                  as the computations proceed\n"
          "\n"
          "* -quiet      ==> suppress all non-essential output\n"
          "\n"
          "* -prefix ppp ==> write a new dataset that is a copy of the\n"
          "                  input, but with all voxels not in a cluster\n"
          "                  set to zero; the new dataset's prefix is 'ppp'\n"
          "            N.B.: use of the -prefix option only affects the\n"
          "                  first input dataset\n"
          "\n"
          "Nota Bene\n"
          "---------\n"
          "* The program does not work on complex-valued datasets!\n"
          "* Using the -1noneg option is strongly recommended!\n "
          "* 3D+time datasets are allowed, but only if you use the\n"
          "   -1tindex and -1dindex options.\n"
          "* Bucket datasets are allowed, but you will almost certainly\n"
          "   want to use the -1tindex and -1dindex options with these.\n"
          "* SEM values are not realistic for interpolated data sets! \n"
          "   A ROUGH correction is to multiply the SEM of the interpolated\n"
          "   data set by the square root of the number of interpolated \n"
          "   voxels per original voxel. \n"
          "* If you use -dxyz=1, then rmm should be given in terms of\n"
          "   voxel edges (not mm) and vmul should be given in terms of\n"
          "   voxel counts (not microliters).  Thus, to connect to only\n"
          "   3D nearest neighbors and keep clusters of 10 voxels or more,\n"
          "   use something like '3dclust -dxyz=1 1.1 10 dset+orig'.\n"
          "   In the report, 'Volume' will be voxel count, but the rest of\n"
          "   the coordinate dependent information will be in actual xyz\n"
          "   millimeters.\n"
        ) ;
      exit(0) ;
   }

   mainENTRY("3dclust main"); machdep(); AFNI_logger("3dclust",argc,argv);

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
          fprintf(stderr,"** Warning: replacing -1clust values with later inputs!\n") ;

       CL_edopt.clust_rmm = rmm;
       CL_edopt.clust_vmul = vmul;
     }

   /**-- loop over datasets --**/

   dset = NULL ;
   for( iarg=nopt ; iarg < argc ; iarg++ ){
      if( dset != NULL ) THD_delete_3dim_dataset( dset , False ) ; /* flush old   */
      dset = THD_open_dataset( argv[iarg] ) ;                      /* open new    */
      if( dset == NULL ){                                          /* failed?     */
         fprintf(stderr,"** Warning: skipping dataset %s\n",argv[iarg]) ;
         continue ;
      }
      if( DSET_NUM_TIMES(dset) > 1 &&
          ( CL_edopt.iv_fim < 0 || CL_edopt.iv_thr < 0 ) ){      /* no time     */

         fprintf(stderr,                                           /* dependence! */
                 "** Cannot use time-dependent dataset %s\n",argv[iarg]) ;
         continue ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ; /* don't mmap  */
      if( CL_verbose )
         fprintf(stderr,"+++ Loading dataset %s\n",argv[iarg]) ;
      THD_load_datablock( dset->dblk  ) ;                          /* read in     */
      EDIT_one_dataset( dset , &CL_edopt ) ;                       /* editing?    */

      if( CL_ivfim < 0 )
         ivfim  = DSET_PRINCIPAL_VALUE(dset) ;                     /* useful data */
      else
         ivfim  = CL_ivfim ;                                       /* 16 Sep 1999 */

      vfim   = DSET_ARRAY(dset,ivfim) ;                            /* ptr to data */
      fimfac = DSET_BRICK_FACTOR(dset,ivfim) ;                     /* scl factor  */
      if( vfim == NULL ){
         fprintf(stderr,"** Cannot access data in dataset %s\a\n",argv[iarg]) ;
         continue ;
      }

      if( ! AFNI_GOOD_FUNC_DTYPE( DSET_BRICK_TYPE(dset,ivfim) ) ){
         fprintf(stderr,"** Illegal datum type in dataset %s\a\n",argv[iarg]) ;
         continue ;
      }

      nx    = dset->daxes->nxx ; dx = fabs(dset->daxes->xxdel) ;
      ny    = dset->daxes->nyy ; dy = fabs(dset->daxes->yydel) ;
      nz    = dset->daxes->nzz ; dz = fabs(dset->daxes->zzdel) ;
      nxy   = nx * ny ; nxyz = nxy * nz ;

      if( CL_edopt.fake_dxyz ){ dxf = dyf = dzf = 1.0 ; }         /* 24 Jan 2001 */
      else                    { dxf = dx ; dyf = dy ; dzf = dz ; }

      ptmin = (int) (vmul / (dxf*dyf*dzf) + 0.99) ;

#if 0
      if( rmm < dxf && rmm < dyf && rmm < dzf ){
         fprintf(stderr,
            "** File %s: cluster rmm %f smaller than voxels dx=%f dy=%f dz=%f\n",
            argv[iarg],rmm,dxf,dyf,dzf ) ;
         continue ;
      }
#endif

      /*-- print report header --*/
     if( !CL_quiet ){

         if( CL_summarize != 1 ){
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

             if( CL_edopt.fake_dxyz )  /* 24 Jan 2001 */
               printf("[Fake voxel dimen    = %.3f mm X %.3f mm X %.3f mm ]\n",
                      dxf,dyf,dzf) ;

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
      } /* end of report header */

      /*-- actually find the clusters in the dataset */

      clar = MCW_find_clusters( nx,ny,nz , dxf,dyf,dzf ,
                                DSET_BRICK_TYPE(dset,ivfim) , vfim , rmm ) ;

      /*-- don't need dataset data any more --*/

      PURGE_DSET( dset ) ;

      if( clar == NULL || clar->num_clu == 0 ){
         printf("** NO CLUSTERS FOUND ***\n") ;
         if( clar != NULL ) DESTROY_CLARR(clar) ;
         continue ;                               /* next dataset */
      }

      /** edit for volume (June 1995) **/

      INIT_CLARR(clbig) ;
      for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
         cl = clar->clar[iclu] ;
         if( cl != NULL && cl->num_pt >= ptmin ){ /* big enough */
            ADDTO_CLARR(clbig,cl) ;               /* copy pointer */
            clar->clar[iclu] = NULL ;             /* null out original */
         }
      }
      DESTROY_CLARR(clar) ;
      clar = clbig ;
      if( clar == NULL || clar->num_clu == 0 ){
         printf("** NO CLUSTERS FOUND ***\n") ;
         if( clar != NULL ) DESTROY_CLARR(clar) ;
         continue ;
      }

      /** end of June 1995 addition **/

      /*-- 29 Nov 2001: write out an edited dataset? --*/

      if( iarg == nopt && CL_prefix != NULL ){
        int qv ; byte *mmm ;

        /* make a mask of voxels to keep */

        mmm = (byte *) calloc(sizeof(byte),nxyz) ;
        for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
          cl = clar->clar[iclu] ; if( cl == NULL ) continue ;
          for( ipt=0 ; ipt < cl->num_pt ; ipt++ ){
            ii = cl->i[ipt] ; jj = cl->j[ipt] ; kk = cl->k[ipt] ;
            mmm[ii+jj*nx+kk*nxy] = 1 ;
          }
        }

        DSET_load( dset ) ;             /* reload data from disk */

        EDIT_dset_items( dset ,         /* rename dataset internally */
                           ADN_prefix , CL_prefix ,
                         ADN_none ) ;

        tross_Make_History( "3dclust" , argc , argv , dset ) ;

        /* mask out each sub-brick */

        for( qv=0 ; qv < DSET_NVALS(dset) ; qv++ ){

           switch( DSET_BRICK_TYPE(dset,qv) ){

             case MRI_short:{
               short *bar = (short *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[ii] = 0 ;
             }
             break ;

             case MRI_byte:{
               byte *bar = (byte *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[ii] = 0 ;
             }
             break ;

             case MRI_int:{
               int *bar = (int *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[ii] = 0 ;
             }
             break ;

             case MRI_float:{
               float *bar = (float *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[ii] = 0.0 ;
             }
             break ;

             case MRI_double:{
               double *bar = (double *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[ii] = 0.0 ;
             }
             break ;

             case MRI_complex:{
               complex *bar = (complex *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[ii].r = bar[ii].i = 0.0 ;
             }
             break ;

             case MRI_rgb:{
               byte *bar = (byte *) DSET_ARRAY(dset,qv) ;
               for( ii=0 ; ii < nxyz ; ii++ )
                 if( mmm[ii] == 0 ) bar[3*ii] = bar[3*ii+1] = bar[3*ii+2] = 0 ;
             }
             break ;
          } /* end of switch over sub-brick type */
        } /* end of loop over sub-bricks */

        /* write dataset out */

        fprintf(stderr,"++ Writing dataset %s\n",DSET_HEADNAME(dset)) ;
        DSET_write(dset) ; PURGE_DSET(dset) ; free(mmm) ;
      }

      /** sort clusters by size, to make a nice report **/

      if( clar->num_clu < 1000 ){
         SORT_CLARR(clar) ;
      } else if( CL_summarize != 1 ){
         printf("** TOO MANY CLUSTERS TO SORT BY VOLUME ***\n") ;
      }
      ndet = 0 ;

      vol_total = nvox_total = 0 ;
      glmmsum = glmssum = glsqsum = glxxsum = glyysum = glzzsum = 0;

      for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
         cl = clar->clar[iclu] ;
         if( cl == NULL || cl->num_pt < ptmin ) continue ;  /* no good */

         volsum = cl->num_pt * dxf*dyf*dzf ;
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

         if( CL_summarize != 1 ){
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
         printf("** NO CLUSTERS FOUND ABOVE THRESHOLD VOLUME ***\n") ;


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
      if( CL_summarize == 1 )
	 {   if( !CL_quiet )
	         printf( "------  -----  -----  ----- -------- -------- \n");
		printf("%6.0f  %5.1f  %5.1f  %5.1f %8.1f %6.3f\n" , vol_total, glxxsum, glyysum, glzzsum, glmean, sem ) ;
      }
	 else if( ndet > 1 && CL_summarize != -1 )
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

      /**** 29 Nov 2001: -prefix ****/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -prefix!\n") ; exit(1) ;
         }
         CL_prefix = argv[nopt] ;
         if( !THD_filename_ok(CL_prefix) ){
            fprintf(stderr,"-prefix string is illegal: %s\n",CL_prefix); exit(1);
         }
         nopt++ ; continue ;
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
         nopt++ ; continue ;
      }

      /**** -nosum [05 Jul 2001] ****/

      if( strncmp(argv[nopt],"-nosum",6) == 0 ){
         CL_summarize = -1 ;
         nopt++ ; continue ;
      }

      /**** -verbose ****/

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         CL_verbose = CL_edopt.verbose = 1 ;
         nopt++ ; continue ;
      }

      /**** -quiet ****/

      if( strncmp(argv[nopt],"-quiet",5) == 0 ){
         CL_quiet = 1 ;
         nopt++ ; continue ;
      }

      /**** -orient code ****/

      if( strncmp(argv[nopt],"-orient",5) == 0 ){
         THD_coorder_fill( argv[++nopt] , &CL_cord ) ; /* July 1997 */
         nopt++ ; continue ;
      }

      /**** -noabs ****/                                /* BDW  19 Jan 1999 */

      if( strncmp(argv[nopt],"-noabs",6) == 0 ){
         CL_noabs = 1 ;
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"** Unrecognized option %s\a\n",argv[nopt]) ;
      exit(1) ;

   }  /* end of loop over options */

#ifdef CLDEBUG
printf("** Finished with options\n") ;
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
