#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *yset=NULL , *aset=NULL , *mset=NULL ;
   MRI_IMAGE *fim=NULL , *qim ; float *flar , *qar ;
   MRI_IMARR *fimar=NULL ;
   int nt=0 , nxyz=0 , nvox=0 , nparam=0 , nqbase , polort=0 , ii,jj,kk,bb ;
   byte *mask=NULL ; int nmask=0 , iarg ;
   char *fname_out="-" ;

   /**--- help the pitiful user? ---**/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dInvFMRI [options]\n"
      "Program to compute stimulus time series, given a 3D+time dataset\n"
      "and an activation map (the inverse of the usual FMRI analysis problem).\n"
      "\n"
      "OPTIONS:\n"
      "\n"
      " -data yyy  =\n"
      "   *OR*     = Defines input 3D+time dataset [a non-optional option].\n"
      " -input yyy =\n"
      "\n"
      " -map  aaa  = Defines activation map; 'aaa' should be a bucket dataset,\n"
      "                each sub-brick of which defines the beta weight map for\n"
      "                an unknown stimulus time series [also non-optional].\n"
      "\n"
      " -mask mmm  = Defines a mask dataset, to restrict input voxels from\n"
      "                -data and -map.\n"
      "\n"
      " -base fff  = Each column of the 1D file 'fff' defines a baseline time\n"
      "                series; these columns should be the same length as\n"
      "                number of time points in 'yyy'.  Multiple -base options\n"
      "                can be given.\n"
      " -polort pp = Adds polynomials of order 'pp' to the baseline collection.\n"
      "                The default baseline model is '-polort 0' (constant).\n"
      "                To specify no baseline model, use '-polort -1'.\n"
      "\n"
      " -out vvv   = Name of 1D output file will be 'vvv'.  Default is stdout.\n"
     ) ;\n"
     exit(0) ;
   }

   /**--- bureaucracy ---**/

   mainENTRY("3dInvFMRI main"); machdep();
   PRINT_VERSION("3dInvFMRI"); AUTHOR("Zhark");
   AFNI_logger("3dInvFMRI",argc,argv) ;

   /**--- scan command line ---**/

   iarg = 1 ;
   while( iarg < argc ){

     if( strcmp(argv[iarg],"-data") == 0 || strcmp(argv[iarg],"-input") == 0 ){
       if( yset != NULL ) ERROR_exit("Can't input 2 3D+time datasets") ;
       yset = THD_open_dataset(argv[++iarg]) ;
       if( yset == NULL ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       nt = DSET_NVALS(yset) ;
       if( nt < 2 ) ERROR_exit("Only 1 sub-brick in dataset %s",argv[iarg]) ;
       nxyz = DSET_NVOX(yset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-map") == 0 ){
       if( aset != NULL ) ERROR_exit("Can't input 2 -map datasets") ;
       aset = THD_open_dataset(argv[++iarg]) ;
       if( aset == NULL ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       nparam = DSET_NVALS(aset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( mset != NULL ) ERROR_exit("Can't input 2 -mask datasets") ;
       mset = THD_open_dataset(argv[++iarg]) ;
       if( mset == NULL ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       polort = (int)strtod(argv[++iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-out") == 0 ){
       fname_out = strdup(argv[++iarg]) ;
       if( !THD_filename_ok(fname_out) )
         ERROR_exit("Bad -out parameter '%s'",fname_out) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-base") == 0 ){
       if( fimar == NULL ) INIT_IMARR(fimar) ;
       qim = mri_read_1D( argv[++iarg] ) ;
       if( qim == NULL ) ERROR_exit("Can't read 1D file %s",argv[iarg]) ;
       ADDTO_IMARR(fimar,qim) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unrecognized option '%s'",argv[iarg]) ;
   }

   /**--- finish up processing options ---**/

   if( yset == NULL ) ERROR_exit("No input 3D+time dataset?!") ;
   if( aset == NULL ) ERROR_exit("No input FMRI -map dataset?!") ;

   if( DSET_NVOX(aset) != nxyz )
     ERROR_exit("Grid mismatch between -data and -map") ;

   if( mset != NULL ){
     if( DSET_NVOX(mset) != nxyz )
       ERROR_exit("Grid mismatch between -data and -mask") ;
     mask = THD_makemask( mset , 0 , 1.0f,-1.0f ); DSET_delete(mset);
     nmask = THD_countmask( nxyz , mask ) ;
     if( nmask < 2 ){
       ERROR_message("Mask has %d voxels -- ignoring!",nmask) ;
       free(mask) ; mask = NULL ; nmask = 0 ;
     } else {
       INFO_message("Mask has %d voxels",nmask) ;
     }
   }

   /**--- set up baseline funcs in one array ---*/

   nqbase = (polort >= 0 ) ? polort+1 : 0 ;
   if( fimar != NULL ){
     for( kk=0 ; kk < IMARR_COUNT(fimar) ; kk++ ){
       qim = IMARR_SUBIMAGE(fimar,kk) ;
       if( qim->nx != nt )
         WARNING_message("-base '%s' length=%d; data length=%d",qim->nx,nt) ;
       nqbase += qim->ny ;
     }
   }

#undef  F
#define F(i,j) flar[(i)+(j)*nt]
   if( nqbase > 0 ){
     fim  = mri_new( nt , nqbase , MRI_float ) ;
     flar = MRI_FLOAT_PTR(fim) ;
     bb = 0 ;
     if( polort >= 0 ){
       double a = 2.0/(nt-1.0) ;
       for( jj=0 ; jj <= polort ; jj++ ){
         for( ii=0 ; ii < nt ; ii++ )
           F(ii,jj) = (float)Plegendre( a*ii-1.0 , jj ) ;
       }
       bb = polort+1 ;
     }

#undef  Q
#define Q(i,j) qar[(i)+(j)*qim->nx]
     if( fimar != NULL ){
       for( kk=0 ; kk < IMARR_COUNT(fimar) ; kk++ ){
         qim = IMARR_SUBIMAGE(fimar,kk) ; qar = MRI_FLOAT_PTR(qim) ;
         for( jj=0 ; jj < qim->ny ; jj++ ){
           for( ii=0 ; ii < nt ; ii++ )
             F(ii,bb+jj) = (ii < qim->nx) ? Q(ii,jj) : 0.0f
         }
         bb += qim->ny ;
       }
       DESTROY_IMARR(fimar) ; fimar=NULL ;
     }
   }
