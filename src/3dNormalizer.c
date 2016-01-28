#include "mrilib.h"
#include "parser.h"

floatvecvec * symmetric_semi_rCDF( THD_3dim_dataset *dset ,
                                   byte *mask , float top , int nbin ) ;

int main( int argc , char *argv[] )
{
   int nopt=1 , nvox ;
   char *prefix="Normalizer" ;
   THD_3dim_dataset *samset=NULL, *inset=NULL, *outset=NULL ;
   byte *mask=NULL ; int nmask,nmask_hits ;
   float apar,bpar,cpar,dpar ;
   floatvec *rfv , *pfv ;

   /*-----------------------------------------------------------------------*/

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
      "Usage: 3dNormalizer [options]\n"
      "\n"
      "This program builds the CDF of the '-sample' dataset, then\n"
      "uses that to normalize the distribution of the '-input' dataset.\n"
      "\n"
      "Options\n"
      "-------\n"
      " -sample sss  = Read dataset 'sss' as the sample.  MANDATORY\n"
      " -input  iii  = Read dataset 'iii' as the input.   MANDATORY\n"
      " -mask   mmm  = Read dataset 'iii' as the mask.\n"
      " -prefix ppp  = Use 'ppp' for the output datset prefix.\n"
      "\n"
      "Currently highly experimental! -- Zhark the nonGaussian\n"
      "\n"
     ) ;
     exit(0) ;
   }

   /*-----------------------------------------------------------------------*/

   while( nopt < argc && argv[nopt][0] == '-' ){

     /*---*/

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Prefix '%s' is not acceptable",prefix) ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-sample") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       if( samset != NULL )
         ERROR_exit("Can't use '-sample' more than once!") ;
       samset = THD_open_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(samset,argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-input") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       if( inset != NULL )
         ERROR_exit("Can't use '-input' more than once!") ;
       inset = THD_open_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(inset,argv[nopt]) ;
       nopt++ ; continue ;
     }

     /*---*/

     if( strcasecmp(argv[nopt],"-mask") == 0 ){
       bytevec *bvec ;
       if( mask != NULL )
         ERROR_exit("Can't use '-mask' twice!") ;
       if( ++nopt >= argc )
         ERROR_exit("Need argument after '%s'",argv[nopt-1]) ;
       bvec = THD_create_mask_from_string(argv[nopt]) ;
       if( bvec == NULL )
         ERROR_exit("Can't create mask from '-mask' option") ;
       mask = bvec->ar ; nmask = bvec->nar ;
       nmask_hits = THD_countmask( nmask , mask ) ;
       if( nmask_hits > 0 )
         INFO_message("%d voxels in -mask dataset",nmask_hits) ;
       else
         ERROR_exit("no nonzero voxels in -mask dataset") ;
       nopt++ ; continue ;
     }

     /*---*/

     ERROR_exit("Unknown option '%s'",argv[nopt]) ;

   }

   /*-----------------------------------------------------------------------*/

   if( samset == NULL ) ERROR_exit("'-sample' is mandatory") ;
   if( inset  == NULL ) ERROR_exit("'-input' is mandatory") ;

   nvox = DSET_NVOX(samset) ;

   if( DSET_NVOX(samset) != DSET_NVOX(inset) )
     ERROR_exit("'-sample' and '-inset' datasets don't match in voxel count!") ;

   if( mask != NULL && DSET_NVOX(samset) != nmask )
     ERROR_exit("'-sample' and '-mask' datasets don't match in voxel count!") ;

   if( mask == NULL ){
     mask = (byte *)malloc(sizeof(byte)*nvox) ;
     memset( mask , 1 , sizeof(byte)*nvox ) ;
     nmask = nmask_hits = nvox ;
   }

   /*-----------------------------------------------------------------------*/

   { floatvecvec *ovv ;
     ovv = symmetric_semi_rCDF( samset , mask , 5.0f , 100 ) ;
     rfv = ovv->fvar + 0 ;
     pfv = ovv->fvar + 1 ;
#if 1
     mri_write_floatvec( modify_afni_prefix(prefix,NULL,".cdf.1D") , rfv ) ;
     mri_write_floatvec( modify_afni_prefix(prefix,NULL,".pdf.1D") , pfv ) ;
#endif
   }

#undef  HFUNC
#define HFUNC(x) (bpar*(x)+apar*logf(coshf(dpar*(x)-cpar)/coshf(cpar)))

   { float *qv, *wv, *xv , *fitv, parbot[26],partop[26],parout[26] ;
     int ii, nval=rfv->nar ; float dx=rfv->dx ;
     qv = (float *)malloc(sizeof(float)*nval) ;
     wv = (float *)malloc(sizeof(float)*nval) ;
     xv = (float *)malloc(sizeof(float)*nval) ;
     for( ii=0 ; ii < nval ; ii++ ){
       qv[ii] = qginv(0.5*rfv->ar[ii]) ;
#if 0
       wv[ii] = (qv[ii] <= 4.0f ) ? (1.0f-0.03f*xv[ii]*xv[ii]) : 0.0f ;
#else
       wv[ii] = (qv[ii] <= 4.0f ) ? 1.0f : 0.01f ;
#endif
       xv[ii] = ii*dx ;
       qv[ii] = qv[ii] - xv[ii] ;
     }
     parbot[0] =  0.0f ; partop[0] = 2.0f ; /* limits on a */
     parbot[1] = -0.5f ; partop[1] = 0.5f ; /* limits on b */
     parbot[2] =  0.1f ; partop[2] = 2.9f ; /* limits on c */
     parbot[3] =  0.2f ; partop[3] = 2.2f ; /* limits on c */
#if 0
     powell_set_verbose(2) ;
#endif
     fitv = PARSER_fitter( nval , xv , qv ,
                           "b*x+a*logcosh(d*x-c)-logcosh(c)" , "x" ,
                           parbot , partop , parout , 1 , wv ) ;
     if( fitv == NULL )
       ERROR_exit("PARSER_fitter() fails :-(") ;
     apar = parout[0] ;
     bpar = parout[1] ;
     cpar = parout[2] ;
     dpar = parout[3] ;
#if 1
     INFO_message("apar=%g  bpar=%g  cpar=%g  dpar=%g",apar,bpar,cpar,dpar) ;
     { MRI_IMAGE *qim = mri_new(nval,2,MRI_float) ;
       float *qar = MRI_FLOAT_PTR(qim) ;
       for( ii=0 ; ii < nval ; ii++ ){
         qar[ii] = qv[ii] ; qar[ii+nval] = fitv[ii] ;
       }
       mri_write_1D( modify_afni_prefix(prefix,NULL,".qfit.1D") , qim ) ;
       mri_free(qim) ;
     }
#endif
   }

   /*-----------------------------------------------------------------------*/

   exit(0) ;
}


/*--------------------------------------------------------------------------*/

floatvecvec * symmetric_semi_rCDF( THD_3dim_dataset *dset ,
                                   byte *mask , float top , int nbin )
{
   floatvec *rfv=NULL , *pfv=NULL ; floatvecvec *ovv=NULL ;
   int64vec *riv=NULL ;
   float dx=top/nbin , dxinv=nbin/top , val , scl ;
   int ival , ii,jj , nvox,nval ; int64_t ntot , nsum ;
   MRI_IMAGE *bim ; float *bar ;

ENTRY("symmetric_semi_rCDF") ;

   MAKE_int64vec( riv , nbin+1 ) ;

   nvox = DSET_NVOX(dset) ; nval = DSET_NVALS(dset) ;

   ntot=0 ;
   for( ival=0 ; ival < nval ; ival++ ){
     bim = THD_extract_float_brick( ival , dset ) ;
     if( bim == NULL ) continue ;
     bar = MRI_FLOAT_PTR(bim) ;
     for( ii=0 ; ii < nvox ; ii++ ){
       val = fabsf(bar[ii]) ;
       if( val > 0.0f && val < top && mask[ii] ){
         jj = (int)(val*dxinv) ; riv->ar[jj]++ ; ntot++ ;
       }
     }
   }

   if( ntot == 0 ){
     KILL_int64vec(riv) ; RETURN(NULL) ;
   }

   scl = 1.0f / ntot ;

   ovv = (floatvecvec *)malloc(sizeof(floatvecvec)) ;
   ovv->nvec = 2 ;
   ovv->fvar = (floatvec *)malloc(sizeof(floatvec)*2) ;
   ovv->fvar[0].nar = nbin+1 ;
   ovv->fvar[0].ar  = (float *)calloc(sizeof(float),(nbin+1)) ;
   ovv->fvar[1].nar = nbin+1 ;
   ovv->fvar[1].ar  = (float *)calloc(sizeof(float),(nbin+1)) ;
   rfv = ovv->fvar + 0 ; rfv->dx = dx ; rfv->x0 = 0.0f ;
   pfv = ovv->fvar + 1 ; pfv->dx = dx ; pfv->x0 = 0.0f ;

   nsum = 0 ;
   for( jj=nbin ; jj >=0 ; jj-- ){
     nsum += riv->ar[jj] ;
     rfv->ar[jj] = scl * nsum ;
     pfv->ar[jj] = riv->ar[jj] * scl ;
   }

   KILL_int64vec(riv) ;
   RETURN(ovv) ;
}
