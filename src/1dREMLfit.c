#include "mrilib.h"
#include "remla.c"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *inim , *outim ; float *iv ;
   int iarg , ii,jj , nreg , ntime , *tau=NULL , rnum ;
   NI_element *nelmat=NULL ; char *matname=NULL ;
   MTYPE rhomax=0.7 , bmax=0.7 ; int rhonum=7 , bnum=14 ;
   char *cgl , *rst ;
   matrix X ; vector y ;
   float cput ;
   reml_collection *rrcol ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 1dREMLfit [option] file.1D\n"
      "Least squares fit with REML estimation of the ARMA(1,1) noise.\n"
      "\n"
      "Options (the first one is mandatory)\n"
      "------------------------------------\n"
      " -matrix mmm = Read the matrix 'mmm', which should have been\n"
      "                 output from 3dDeconvolve via the '-x1D' option.\n"
      " -MAXrho rm  = Set the max allowed rho parameter to 'rm' (default=0.7).\n"
      " -Nrho nr    = Use 'nr' values for the rho parameter (default=7).\n"
      " -MAXb bm    = Set max allow MA b parameter to 'bm' (default=0.7).\n"
      " -Nb nb      = Use 'nb' values for the b parameter (default=7).\n"
     ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /** rho and del params **/

      if( strcmp(argv[iarg],"-MAXrho") == 0 ){
        rhomax = (MTYPE)strtod(argv[++iarg],NULL) ;
             if( rhomax < 0.3 ) rhomax = 0.3 ;
        else if( rhomax > 0.9 ) rhomax = 0.9 ;
        iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-Nrho") == 0 ){
        rhonum = (int)strtod(argv[++iarg],NULL) ;
             if( rhonum <  2 ) rhonum =  2 ;
        else if( rhonum > 20 ) rhonum = 20 ;
        iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-MAXb") == 0 ){
        bmax = (MTYPE)strtod(argv[++iarg],NULL) ;
             if( bmax < 0.3 ) bmax = 0.3 ;
        else if( bmax > 0.9 ) bmax = 0.9 ;
        iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-Nb") == 0 ){
        bnum = (int)strtod(argv[++iarg],NULL) ;
             if( bnum <  2 ) bnum =  2 ;
        else if( bnum > 20 ) bnum = 20 ;
        iarg++ ; continue ;
      }

      /** -matrix **/

      if( strcmp(argv[iarg],"-matrix") == 0 ){
        if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!");
        nelmat = NI_read_element_fromfile( argv[++iarg] ) ; /* read NIML file */
        matname = argv[iarg];
        if( nelmat == NULL ){                     /* try to read as a 1D file */
          MRI_IMAGE *nim ; float *nar ;
          nim = mri_read_1D(argv[iarg]) ;
          if( nim != NULL ){              /* construct a minimal NIML element */
            nelmat = NI_new_data_element( "matrix" , nim->nx ) ;
            nar    = MRI_FLOAT_PTR(nim) ;
            for( jj=0 ; jj < nim->ny ; jj++ )
              NI_add_column( nelmat , NI_FLOAT , nar + nim->nx*jj ) ;
            mri_free(nim) ;
          }
        }
        if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
          ERROR_exit("Can't process -matrix file!");
        iarg++ ; continue ;
      }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( iarg >= argc ) ERROR_exit("No 1D file on command line?!") ;

   inim = mri_read_1D( argv[iarg] ) ;
   if( inim == NULL ) ERROR_exit("Can't read 1D file %s",argv[iarg]) ;

   nreg  = nelmat->vec_num ;
   ntime = nelmat->vec_len ;
   if( ntime != inim->nx )
     ERROR_exit("matrix vectors are %d long but input 1D file is %d long",
                ntime,inim->nx) ;

   cgl = NI_get_attribute( nelmat , "GoodList" ) ;
   if( cgl != NULL ){
     int Ngoodlist,*goodlist , Nruns,*runs ;
     NI_int_array *giar ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < ntime )
       ERROR_exit("-matrix 'GoodList' badly formatted?") ;
     Ngoodlist = giar->num ; goodlist = giar->ar ;
     rst = NI_get_attribute( nelmat , "RunStart" ) ;
     if( rst != NULL ){
       NI_int_array *riar = NI_decode_int_list( rst , ";,") ;
       if( riar == NULL ) ERROR_exit("-matrix 'RunStart' badly formatted?") ;
       Nruns = riar->num ; runs = riar->ar ;
     } else {
       Nruns = 1 ; runs = calloc(sizeof(int),1) ;
     }
     rnum = 0 ; tau = (int *)malloc(sizeof(int)*ntime) ;
     for( ii=0 ; ii < ntime ; ii++ ){
       jj = goodlist[ii] ;
       for( ; rnum+1 < Nruns && jj >= runs[rnum+1] ; rnum++ ) ; /*nada*/
       tau[ii] = jj + 10000*rnum ;
     }
   }

   matrix_initialize( &X ) ;
   matrix_create( ntime , nreg , &X ) ;
   if( nelmat->vec_typ[0] == NI_FLOAT ){
     float *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (float *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){
     double *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (double *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else {
     ERROR_exit("-matrix file stored will illegal data type!?") ;
   }

   cput = COX_cpu_time() ;
   rrcol = REML_setup( &X , tau , rhonum,rhomax,bnum,bmax ) ;
   if( rrcol == NULL ) ERROR_exit("REML setup fails?" ) ;
   cput = COX_cpu_time() - cput ;
   INFO_message("REML setup: rows=%d cols=%d %d cases CPU=%.2f",
                ntime,nreg,rrcol->nset,cput) ;

   cput = COX_cpu_time() ;
   vector_initialize( &y ) ; vector_create_noinit( ntime , &y ) ;
   for( jj=0 ; jj < inim->ny ; jj++ ){
     iv = MRI_FLOAT_PTR(inim) + ntime*jj ;
     for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[ii] ;
     (void)REML_find_best_case( &y , rrcol ) ;
     INFO_message(
       "Vector #%d: best_rho=%.2f best_b=%.2f best_lam=%.2f best_ssq=%g  olsq_ssq=%g",
       jj, REML_best_rho, REML_best_bb, REML_best_lam, REML_best_ssq, REML_olsq_ssq ) ;
   }
   cput = COX_cpu_time() - cput ;
   INFO_message("REML fitting: CPU=%.2f",cput) ;
   exit(0) ;
}
