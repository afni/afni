/*
  This program generates a histogram for the input AFNI dataset.

  Mod:   2 June 1997
         Added option to generate histogram for only those voxels which are
         above the operator specified threshold value.

*/


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
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mrilib.h"

static EDIT_options HI_edopt ;

static int   HI_nopt ;
static int   HI_nbin = 100 ;
static int   HI_log  = 0 ;
static float HI_thr = 0.0;


void HI_read_opts( int , char ** ) ;
#define HI_syntax(str) \
  do{ fprintf(stderr,"\n*** %s\a\n",str) ; exit(-1) ; } while(1)

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset * dset ;

   int nx,ny,nz , nxyz , ii , kk , nopt , nbin ;
   float fbot , ftop ;
   int * fbin , * tbin;
   float df , dfi ;

   int iv_thr, thr_type;
   float thrfac, fimfac;
   int iv_fim, fim_type;
   byte    * bfim = NULL , * bthr = NULL ;
   short   * sfim = NULL , * sthr = NULL ;
   float   * ffim = NULL , * fthr = NULL ;
   void    * vfim = NULL , * vthr = NULL ;
   long cumfbin, cumtbin;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
      fprintf(stderr,
             "Compute histogram of 3D Dataset\n"
             "Usage: 3dhistog [editing options] [-nbin #] [-log10] [-thr r] dset \n"
             "  where -nbin # means to use '#' bins (default = 100).\n"
	     "        -thr r  means to count only voxels with threshold above r \n"
             "  The report is sent to stdout.\n"
             "  N.B.: The editing options are as in 3dmerge.\n" ) ;
      exit(0) ;
   }

   HI_read_opts( argc , argv ) ;
   nopt = HI_nopt ;

   if( nopt >=  argc ) HI_syntax("no dset argument?") ;

   fbin = (int *) malloc( sizeof(int) * HI_nbin ) ;
   if( fbin == 0 ) HI_syntax("can't allocate histogram array!") ;

   if (HI_thr > 0.0)
     {
       tbin = (int *) malloc( sizeof(int) * HI_nbin ) ;
       if( tbin == 0 ) HI_syntax("can't allocate histogram array!") ;
     }

   dset = NULL ;
   for( iarg=nopt ; iarg < argc ; iarg++ ){
      if( dset != NULL ) THD_delete_3dim_dataset( dset , False ) ;
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ) continue ;
      if( DSET_NUM_TIMES(dset) > 1 ){                              /* no time     */
         fprintf(stderr,                                           /* dependence! */
                 "*** cannot use time-dependent dataset %s\n",argv[iarg]) ;
         continue ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;
      THD_load_datablock( dset->dblk , NULL ) ;
      EDIT_one_dataset( dset , &HI_edopt ) ;
      iv_fim = DSET_PRINCIPAL_VALUE(dset) ;                         /* useful data */
      fimfac   = DSET_BRICK_FACTOR(dset,iv_fim) ;
      if (fimfac == 0.0)  fimfac = 1.0;
      vfim  = DSET_ARRAY(dset,iv_fim) ;                             /* ptr to data */
      if( vfim == NULL ){
         fprintf(stderr,"*** cannot access data in dataset %s\n",argv[iarg]) ;
         continue ;
      }


      iv_thr   = FUNC_ival_thr[dset->func_type] ;
      
      if( iv_thr < 0 )
	{
	  thr_type = ILLEGAL_TYPE ;
	  thrfac   = 0.0 ;
	} 
      else 
	{
	  thr_type = DSET_BRICK_TYPE(dset,iv_thr) ;
	  thrfac   = DSET_BRICK_FACTOR(dset,iv_thr) ;
	  if( thrfac == 0.0 )
	    {
	      switch( thr_type )
		{
		case MRI_short: 
		  thrfac = 1.0/FUNC_scale_short[dset->func_type]; 
		  break;
		case MRI_byte : 
		  thrfac = 1.0/FUNC_scale_byte [dset->func_type]; 
		  break;
		}
	    }
	}

      
      /** load the pointers to the sub-bricks **/
      
      vfim = DSET_ARRAY(dset,iv_fim) ;
      fim_type = DSET_BRICK_TYPE(dset,iv_fim) ;

      switch( fim_type )
	{
	default:
	  fprintf(stderr,"\n*** Illegal data type in dataset %s\a\n",
		  dset->dblk->diskptr->brick_name ) ;
	  exit(1) ;
	  
	case MRI_short:   sfim = (short *)   vfim ; break ;
	case MRI_float:   ffim = (float *)   vfim ; break ;
	case MRI_byte:    bfim = (byte *)    vfim ; break ;
	}
      
      if( iv_thr >= 0 )
	{
	  vthr = DSET_ARRAY(dset,iv_thr) ;
	  switch( thr_type )
	    {
	    default:
	      fprintf (stderr,
		       "\n*** Illegal thresh data type in dataset %s\a\n",
		       dset->dblk->diskptr->brick_name ) ;
	      exit(1) ;
	      
	    case MRI_short:   sthr = (short *) vthr ; break ;
	    case MRI_float:   fthr = (float *) vthr ; break ;
	    case MRI_byte:    bthr = (byte *)  vthr ; break ;
	    }
	}
      
      nx = dset->daxes->nxx ;
      ny = dset->daxes->nyy ;
      nz = dset->daxes->nzz ; nxyz = nx * ny * nz ;
      
      for( kk=0 ; kk < HI_nbin ; kk++ ) fbin[kk] = 0 ;

      fim_type = DSET_BRICK_TYPE(dset,iv_fim) ;
      switch( fim_type ){

         default:
            fprintf(stderr,
                    "*** data of type %s in dataset %s -- not yet implemented!\n" ,
                    MRI_TYPE_name[fim_type] , argv[iarg] ) ;
            fbot = 1.0 ; ftop = 0.0 ;
         break ;

         case MRI_short:{
            short * fim = (short *) vfim ;

            fbot = ftop = fim[0] ;
            for( ii=1 ; ii < nxyz ; ii++ ){
                    if( fim[ii] < fbot ) fbot = fim[ii] ;
               else if( fim[ii] > ftop ) ftop = fim[ii] ;
            }
            if( fbot == ftop ) break ;
            nbin = ftop-fbot+1 ;
            if( nbin > HI_nbin ) nbin = HI_nbin ;
            if( nbin < 2       ) nbin = 2 ;
            df  = (ftop-fbot) / ((float)(nbin-1)) ;
            dfi = 1.0 / df ;
            for( ii=0 ; ii < nxyz ; ii++ ){
               kk = (int)( (fim[ii]-fbot)*dfi ) ;
               fbin[kk]++ ;
            }
         }
         break ;

         case MRI_byte:{
            byte * fim = (byte *) vfim ;

            fbot = ftop = fim[0] ;
            for( ii=1 ; ii < nxyz ; ii++ ){
                    if( fim[ii] < fbot ) fbot = fim[ii] ;
               else if( fim[ii] > ftop ) ftop = fim[ii] ;
            }
            if( fbot == ftop ) break ;
            nbin = ftop-fbot+1 ;
            if( nbin > HI_nbin ) nbin = HI_nbin ;
            if( nbin < 2       ) nbin = 2 ;
            df  = (ftop-fbot) / ((float)(nbin-1)) ;
            dfi = 1.0 / df ;
            for( ii=0 ; ii < nxyz ; ii++ ){
               kk = (int)( (fim[ii]-fbot)*dfi ) ;
               fbin[kk]++ ;
            }
         }
         break ;

         case MRI_float:{
            float * fim = (float *) vfim ;

            fbot = ftop = fim[0] ;
            for( ii=1 ; ii < nxyz ; ii++ ){
                    if( fim[ii] < fbot ) fbot = fim[ii] ;
               else if( fim[ii] > ftop ) ftop = fim[ii] ;
            }
            if( fbot == ftop ) break ;
            nbin = HI_nbin ;
            df  = (ftop-fbot) / ((float)(nbin-1)) ;
            dfi = 1.0 / df ;
            for( ii=0 ; ii < nxyz ; ii++ ){
               kk = (int)( (fim[ii]-fbot)*dfi ) ;
               fbin[kk]++ ;
            }
         }
         break ;

      }  /* end of switch on data brick type */
      

      
      /*----- start of logic for histogram of thresholded values -----*/

      /*----- apply threshold? -----*/
      if( HI_thr > 0.0 && iv_thr >= 0 )
	{
	  for (kk=0;  kk < HI_nbin;  kk++)  tbin[kk] = 0;
	
	  switch( thr_type )
	    {
	    
	    /*--- threshold datum is shorts ---*/
	    case MRI_short:
	      {
		short thrplu , thrmin ;
		float fplu = HI_thr / thrfac ;
		if( fplu > 32767.0 ){
		  fprintf(stderr,"\n*** -thr out of range: reset to %g\n",
			  32767.0 * thrfac ) ;
		  fplu = 32767.0 ;
		}
		thrplu = (short) fplu ;
		thrmin = -thrplu ;
		
		switch( fim_type )
		  {
		  case MRI_short:   /* fim datum is shorts */
		    for( ii=0 ; ii < nxyz ; ii++ )
		      if( sthr[ii] > thrplu || sthr[ii] < thrmin )
			{ 
			  kk = (int)( (sfim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
			}
		    break ;
		    
		  case MRI_byte:    /* fim datum is bytes */
		    for( ii=0 ; ii < nxyz ; ii++ )
		      if( sthr[ii] > thrplu || sthr[ii] < thrmin )
			{ 
			  kk = (int)( (bfim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
			}
		    break ;
		    
		  case MRI_float:   /* fim datum is floats */
		    for( ii=0 ; ii < nxyz ; ii++ )
		      if( sthr[ii] > thrplu || sthr[ii] < thrmin )
			{
 			  kk = (int)( (ffim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
			}
		    break ;
		  }
	      }
	      break ;
	      
	    /** threshold datum is bytes **/
	    case MRI_byte:
	      {
		byte thrplu ;
		float fplu = HI_thr / thrfac ;
		if( fplu > 255.0 )
		  {
		    fprintf(stderr,
			    "\n*** -thr out of range: reset to %g\n",
			    255.0 * thrfac ) ;
		    fplu = 255.0 ;
		  }
		thrplu = (byte) fplu ;
		
		switch( fim_type )
		  {
		  case MRI_short:   /* fim datum is shorts */
		    for( ii=0 ; ii < nxyz ; ii++ ) 
		      if( bthr[ii] > thrplu )
			{ 
			  kk = (int)( (sfim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
			}
		    break ;
		    
		  case MRI_byte:    /* fim datum is bytes */
		    for( ii=0 ; ii < nxyz ; ii++ ) 
		      if( bthr[ii] > thrplu )
			{
			  kk = (int)( (bfim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
			}
		    break ;
		    
		  case MRI_float:   /* fim datum is floats */
		    for( ii=0 ; ii < nxyz ; ii++ ) 
		      if( bthr[ii] > thrplu )
			{
			  kk = (int)( (ffim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
			}
		    break ;
		  }
	      }
	      break ;
	      
	    /** threshold datum is floats **/
	    case MRI_float:
	      {
		float thrplu , thrmin ;
		thrplu = HI_thr ; 
		if( thrfac > 0.0 ) thrplu /= thrfac ;
		thrmin = -thrplu ;
		
		switch( fim_type ){
		case MRI_short:   /* fim datum is shorts */
                  for( ii=0 ; ii < nxyz ; ii++ )
		    if( fthr[ii] > thrplu || fthr[ii] < thrmin )
		      { 
			  kk = (int)( (sfim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
		      }
		  break ;
		  
		case MRI_byte:    /* fim datum is bytes */
                  for( ii=0 ; ii < nxyz ; ii++ )
		    if( fthr[ii] > thrplu || fthr[ii] < thrmin )
		      { 
			  kk = (int)( (bfim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
		      }
		  break ;
		  
		case MRI_float:   /* fim datum is floats */
                  for( ii=0 ; ii < nxyz ; ii++ )
		    if( fthr[ii] > thrplu || fthr[ii] < thrmin )
		      {
			  kk = (int)( (ffim[ii]-fbot)*dfi ) ;
			  tbin[kk]++ ;
		      }
		  break ;
		}
	      }
	      break ;
	    }
	}
      
      
      /*** print something, maybe ***/

      if( fbot > ftop ) continue ;   /* something bad happened */
      if( fbot == ftop ){
         printf("*** all data = %f in dataset %s\n",
                fbot , argv[iarg] ) ;
         continue ;
      }

      cumfbin = 0; 
      cumtbin = 0;

      if( HI_log )
	{
	  printf ("%12s %13s %13s ",  
		  "Magnitude", "Log_Freq", "Log_Cum_Freq");
	  if (HI_thr > 0.0)
	    printf ("%13s %13s\n",  "Log_Thr_Freq", "Log_Cum_Thr_Frq");
	  else
	    printf (" \n");
      
         for( kk=0 ; kk < nbin ; kk++ )
	   {
	     cumfbin += fbin[kk];
	     printf ("%12.6f %13.6f %13.6f ", (fbot+kk*df)*fimfac, 
		     log10((double)fbin[kk]+1.0), log10((double)cumfbin+1.0));
	     if (HI_thr > 0.0)
	       {
		 cumtbin += tbin[kk];
		 printf ("%13.6f %13.6f \n", log10((double)tbin[kk]+1.0), 
			 log10((double)cumtbin+1.0));
	       }
	     else
	       printf (" \n");
	   }
	} 
      else 
	{
	  printf ("%12s %13s %13s ",  "Magnitude", "Freq", "Cum_Freq");
	  if (HI_thr > 0.0)
	    printf ("%13s %13s \n",  "Thr_Freq", "Cum_Thr_Freq");
	  else
	    printf (" \n");

	  for( kk=0 ; kk < nbin ; kk++ )
	    {
	      cumfbin += fbin[kk];
	      printf ("%12.6f %13d %13ld ", (fbot+kk*df)*fimfac, 
		     fbin[kk], cumfbin);
	      if (HI_thr > 0.0)
		{
		  cumtbin += tbin[kk];
		  printf ("%13d %13ld \n", tbin[kk], cumtbin);
		}
	      else
		printf (" \n");
	      
	    }
	}
   }
   exit(0) ;
}


/*--------------------------------------------------------------------
   read the arguments, and load the global variables
----------------------------------------------------------------------*/

#ifdef HIDEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

void HI_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  ival , kk ;

   INIT_EDOPT( &HI_edopt ) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** check for editing options ****/

      ival = EDIT_check_argv( argc , argv , nopt , &HI_edopt ) ;
      if( ival > 0 ){
         nopt += ival ;
         continue ;
      }

      if( strncmp(argv[nopt],"-nbin",5) == 0 ){
        HI_nbin = strtol( argv[++nopt] , NULL , 10 ) ;
        if( HI_nbin < 10 ) HI_syntax("illegal value of -nbin!") ;
        nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-log10",5) == 0 ){
         HI_log = 1 ;
         nopt++ ; continue ;
      }

      /*-----   -thr   -----*/
      if (strncmp(argv[nopt], "-thr", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  HI_syntax ("need argument after -thr ");
	  sscanf (argv[nopt], "%f", &val); 
	  if (val < 0.0)
	    HI_syntax ("illegal argument after -thr ");
	  HI_thr = val;
	  nopt++;
	  continue;
	}
      
      /**** unknown switch ****/

      fprintf(stderr,"*** unrecognized option %s\a\n",argv[nopt]) ;
      exit(-1) ;

   }  /* end of loop over options */

#ifdef HIDEBUG
printf("*** finished with options\n") ;
#endif

   HI_nopt = nopt ;
   return ;
}
