static int verb = 1 ;

static void NCO_help(void) ;  /* prototype */

/*---------------------------------------------------------------------------*/
#ifndef FLOATIZE
# include "matrix.h"          /* double precision */
# define MTYPE    double
# define NI_MTYPE NI_DOUBLE
# define QEPS 1.e-6
#else
# include "matrix_f.h"        /* single precision */
# define MTYPE    float
# define NI_MTYPE NI_FLOAT
# define QEPS 1.e-4
#endif

#include "mrilib.h"        /* Keep after decision about matrix.h inclusion 
                                                      ZSS  Nov. 21 2014*/

/*---------------------------------------------------------------------------*/
/* Everything needed to solve one linear problem (baseline and full models) */

typedef struct {
  matrix x_all ;  /* the whole matrix (before censoring): nall X p */
  matrix x_full ; /* after censoring: nfull X p */

  matrix xtxinv_full ;    /* p X p */
  matrix xtxinvxt_full ;  /* p X nfull */

  matrix x_base ;         /* nfull X q */
  matrix xtxinvxt_base ;  /* q X nfull */
} linear_setup ;

#define INIT_linear_setup(ls)                    \
 do{ matrix_initialize(&((ls).x_all)) ;          \
     matrix_initialize(&((ls).x_full)) ;         \
     matrix_initialize(&((ls).xtxinv_full)) ;    \
     matrix_initialize(&((ls).xtxinvxt_full)) ;  \
     matrix_initialize(&((ls).x_base) ;          \
     matrix_initialize(&((ls).xtxinvxt_base) ;   \
 } while(0)

/*---------------------------------------------------------------------------*/
/* Specify one HRF model function */

typedef struct {
  int parnum ;                          /* number of parameters */
  float tbot , ttop ;                   /* start and stop times */
  float *parval , *parbot , *partop ;   /* parameter values and ranges */
  void *qpar ;                          /* other information */
  float (*f)(float,int,float *,void *); /* function to evaluate HRF */
} hrf_model ;

/*---------------------------------------------------------------------------*/
/* Everything needed to solve the nonlinear problem. */

typedef struct {
  int polort, /* number of polynomial parameters PER run */
      qp,     /* total number of polynomial baseline parameters */
      q ,     /* total number of baseline parameters */
      p ,     /* total number of linear parameters (matrix columns) */
      nall ,  /* number of input data rows (before censoring) */
      nfull ; /* number of input data rows (after censoring) */

  int ngood ;     /* 0 if no censoring, or nfull if censoring */
  int *goodlist ; /* goodlist[i] = index of i-th good row in all data */
  int  run_num ;  /* number of runs in total data */
  int *run_start; /* run_start[i] = data index of start of run #i */
  float tr ;      /* time step of data */

  int          nprob ;      /* number of linear setups (slices) */
  linear_setup *prob ;      /* one per slice */
  float        *prob_toff ; /* time shift for each prob */

                /* Data time series from input dataset,
                   separated into slices and in time-then-space order: */
  int   *nvec ; /* nvec[i] = # vectors (length=nfull) in prob #i */
  float **vec ; /* vec[i]  = ptr to nvec[i] data vectors */
  int  **ivec ; /* ivec[i][k] = 3D index of where k-th data vector is from */
  int nx,ny,nz; /* spatial dimensions of input grid */

  int    num_hrf ;  /* number of HRF model functions we're finding */
  hrf_model *hrf ;  /* description of HRF model function */

  int     num_times ;        /* number of -stimtime options */
  char      **times_label ;  /* label for each option */
  MRI_IMAGE **times ;        /* times for each option */
  int        *times_hrfind ; /* which HRF model function to use */
} nonlinear_setup ;

/*---------------------------------------------------------------------------*/
/* Given the x_all matrix, finish setting up the rest of the stuff
   (i.e., extract the full and base matrices, compute pseudo-inverses).
-----------------------------------------------------------------------------*/

void NCO_finish_linear_setup( linear_setup *ls, int ngd, int *gdlist, int qb )
{
   matrix *xf ;  /* stand-in for full matrix */

ENTRY("NCO_finish_linear_setup") ;

   if( !ISVALID_MATRIX(ls->x_all) ){
     WARNING_message("Unprepared call to NCO_finish_linear_setup"); EXRETURN;
   }

   /* on each entry, x_all was changed in the HRF part,
      so need to re-extract and pseudo-invert the x_full sub-matrix */

   if( ISVALID_MATRIX(ls->x_full) ) matrix_destroy( &(ls->x_full) ) ;

   if( ngd > 0 && gdlist != NULL ){      /* if any censoring was done */
     xf = &(ls->x_full) ;
     matrix_extract_rows( ls->x_all , ngd,gdlist , xf ) ;
   } else {                              /* no censoring was done */
     xf = &(ls->x_all) ;
   }
   matrix_psinv( *xf , &(ls->xtxinv_full) , &(ls->xtxinvxt_full) ) ;

   /* only on first entry should baseline part of x_all change */

   if( qb > 0 && !ISVALID_MATRIX(ls->x_base) ){
     int *qlist=malloc(sizeof(int)*qb) , ii ;
     for( ii=0 ; ii < qb ; ii++ ) qlist[ii] = ii ;
     matrix_extract_cols( *xf , qb,qlist , &(ls->x_base) ) ;
     free((void *)qlist) ;
     matrix_psinv( ls->x_base , NULL , &(ls->xtxinvxt_base) ) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Erase the forgettable parts of a linear_setup struct. */

void NCO_clear_linear_setup( linear_setup *ls , int dobase )
{
   matrix_destroy( &(ls->x_full) ) ;
   matrix_destroy( &(ls->xtxinv_full) ) ;
   matrix_destroy( &(ls->xtxinvxt_full) ) ;
   if( dobase ){
     matrix_destroy( &(ls->x_base) ) ;
     matrix_destroy( &(ls->xtxinvxt_base) ) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/
/* Extract all slice data in the mask from the given dataset.
   Output is an image of time series vectors, and an image of their
   3D indexes in the dataset array.
-----------------------------------------------------------------------------*/

MRI_IMARR * NCO_extract_slice( THD_3dim_dataset *dset, int ss,
                               byte *mask, int ngood, int *goodlist )
{
   int ii,jj,kk,vv,uu,ww , nx,ny,nz , nt,ng , nin;
   MRI_IMAGE *sim ; float *sar , *tar , *qar ;
   MRI_IMAGE *iim ; int *iar ;
   MRI_IMARR *imar ;

ENTRY("NCO_extract_slice") ;

   nx = DSET_NX(dset); ny = DSET_NY(dset);
   nz = DSET_NZ(dset); nt = DSET_NVALS(dset);
   if( ss < 0 || ss >= nz ) ERROR_exit("illegal slice index %d!?",ss) ;

   /* number of output time points */
   ng = (ngood > 0 && goodlist != NULL) ? ngood : nt ;

   vv = ss*nx*ny ;     /* slice index offset into 3D arrays */
   if( mask != NULL ){ /* count number of voxels in mask & in this slice */
     for( nin=jj=0 ; jj < ny ; jj++ ){
       uu = vv + jj*nx ;
       for( ii=0 ; ii < nx ; ii++ ) if( mask[ii+uu] ) nin++ ;
     }
     if( nin == 0 ) RETURN(NULL) ;  /* nothing in the mask */
   } else {
     nin = nx*ny ;     /* use all voxels in slice */
   }

   /* create output image */

   sim = mri_new(ng,nin,MRI_float) ; sar = MRI_FLOAT_PTR(sim) ;
   iim = mri_new(nin,1 ,MRI_int)   ; iar = MRI_INT_PTR  (iim) ;

   /* perhaps need temp staging area for array from dataset? */

   if( ng < nt ) tar = (float *)malloc(sizeof(float)*nt) ;
   else          tar = NULL ;

   /* loop over voxels in this slice */

   for( kk=jj=0 ; jj < ny ; jj++ ){  /* kk=output voxel index */
     uu = vv + jj*nx ;               /* uu=index to jj-th row in slice */
     for( ii=0 ; ii < nx ; ii++ ){
       if( mask != NULL && mask[ii+uu] == 0 ) continue ; /* skip this'n */
       qar = sar + kk*ng ;           /* where to store output */
       if( tar != NULL ){            /* get temp array, then sub-sample */
         THD_extract_array( ii+uu , dset , 0 , tar ) ;
         for( ww=0 ; ww < ng ; ww++ ) qar[ww] = tar[goodlist[ww]] ;
       } else {                      /* get directly into output */
         THD_extract_array( ii+uu , dset , 0 , qar ) ;
       }
       iar[kk] = ii+uu ; /* save index of where this data vector came from */
       kk++ ;            /* increment count of saved voxel vectors */
     }
   }

   if( tar != NULL ) free((void *)tar) ;  /* toss some trash */

   INIT_IMARR(imar); ADDTO_IMARR(imar,sim); ADDTO_IMARR(imar,iim); RETURN(imar);
}

/*===========================================================================*/
/*---------------------- Main program (not much here yet) -------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 ;

   THD_3dim_dataset *inset=NULL , *maskset=NULL ;
   int automask=0 ;
   float tr=0.0f ;
   nonlinear_setup *nls ;
   MRI_IMARR *baseim ;
   MRI_IMAGE *concatim=NULL ;
   int         num_CENSOR=0 ;
   int_triple *abc_CENSOR=NULL ;
   char *tpat=NULL ;

   int ii,jj,kk , nslice , nt , nvectot ;
   int nmask=0 ; byte *mask=NULL ;
   MRI_IMAGE *sim,*iim ; MRI_IMARR *imar ; float *sar ; int *iar ;

   /*----- help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){ NCO_help(); exit(0); }

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif
   PRINT_VERSION("3dNeocon") ; AUTHOR("RW Cox");
   mainENTRY("3dNeocon main") ; machdep() ;
   AFNI_logger("3dNeocon",argc,argv) ;

   /*----- read arguments -----*/

   nls = (nonlinear_setup *)calloc(1,sizeof(nonlinear_setup)) ;
   nls->polort = -666 ;  /* automatic */
   INIT_IMARR(baseim) ;

   while( iarg < argc ){

     /*----------*/

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]) ;
       if( inset != NULL )
         ERROR_exit("Can't have two -input arguments!") ;
       inset = THD_open_dataset( argv[++iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       if( DSET_NVALS(inset) < 9 )
         ERROR_exit("Input dataset has less than 9 time points!") ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]) ;
       if( maskset != NULL )
         ERROR_exit("Can't have two -mask arguments!") ;
       if( automask )
         ERROR_exit("Can't combine -mask and -automask!") ;
       maskset = THD_open_dataset( argv[++iarg] ) ;
       CHECK_OPEN_ERROR(maskset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( maskset != NULL ) ERROR_exit("Can't combine -automask and -mask!");
       automask = 1 ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-TR") == 0 ){
       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]) ;
       tr = (float)strtod(argv[++iarg],NULL) ;
       if( tr <= 0.0f ) ERROR_exit("Can't read valid value after -TR") ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-polort") == 0 ){
       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]) ;
       iarg++ ;
       if( argv[iarg][0] == 'A' ) nls->polort = -1 ;
       else                       nls->polort = (int)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-baseline") == 0 ){
       MRI_IMAGE *bim ;
       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]) ;
       bim = mri_read_1D( argv[++iarg] ) ;
       if( bim == NULL ) ERROR_exit("Can't read -baseline '%s'",argv[iarg]) ;
       ADDTO_IMARR(baseim,bim) ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-concat") == 0 ){
       if( iarg == argc-1   ) ERROR_exit("Need argument after '%s'",argv[iarg]);
       if( concatim != NULL ) ERROR_exit("Can't have 2 -concat arguments!");
       concatim = mri_read_1D( argv[++iarg] ) ;
       if( concatim == NULL ) ERROR_exit("Can't read -concat '%s'",argv[iarg]);
       iarg++ ; continue ;
     }

     /*----------*/

     if( strncmp(argv[iarg],"-CENSOR",7)   == 0 ||   /* adapted from */
         strncmp(argv[iarg],"-censorTR",9) == 0   ){ /* 3dDeconvolve */

       NI_str_array *nsar ;
       char *src=malloc(1), *cpt, *dpt ;
       int ns, r,a,b, nerr=0 ; int_triple rab ;

       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]);

       *src = '\0' ;   /* cat all following options until starts with '-' */
       for( iarg++ ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         ns = strlen(argv[iarg]) ; if( ns == 0 ) continue ;
         src = realloc(src,strlen(src)+ns+2) ;
         strcat(src," ") ; strcat(src,argv[iarg]) ;
       }
       if( *src == '\0' ) ERROR_exit("Bad argument after -CENSORTR") ;
       nsar = NI_decode_string_list( src , "," ) ; /* break into substrings */
       for( ns=0 ; ns < nsar->num ; ns++ ){ /* loop over substrings */
         cpt = nsar->str[ns] ; dpt = strchr(cpt,':') ; r = 0 ;
         if( *cpt == '\0' ) continue ;   /* skip an empty string */
         if( dpt != NULL ){              /* found 'run:' */
           if( *cpt == '*' ){ /* wildcard = all runs */
             r = -666 ;
           } else {
             r = (int)strtol(cpt,NULL,10) ;
             if( r <= 0 ){  /* skip out */
               ERROR_message("-CENSORTR %s -- run index '%d' is bad!",nsar->str[ns],r);
               nerr++ ; continue ;
             }
           }
           cpt = dpt+1 ;  /* skip to character after ':' */
           if( *cpt == '\0' ){  /* skip out */
             ERROR_message("-CENSORTR %s -- no data after run index!",nsar->str[ns]);
             nerr++ ; continue ;
           }
         }
         a = (int)strtol(cpt,&dpt,10) ;    /* get first index number */
         if( a < 0 ){  /* skip out */
           ERROR_message("-CENSORTR %s -- time index '%d' is bad!",nsar->str[ns],a);
           nerr++ ; continue ;
         }
         if( *dpt == '\0' ){  /* no second number */
           b = a ;
         } else {             /* get second number */
           for( dpt++ ; *dpt != '\0' && !isdigit(*dpt) ; dpt++ ) ; /*nada*/
           b = (int)strtol(dpt,NULL,10) ;
           if( b < a || b < 0 ){  /* skip out */
             ERROR_message("-CENSORTR %s -- time indexes '%d' to '%d' is bad!",
                           nsar->str[ns],a,b);
             nerr++ ; continue ;
           }
         }
         abc_CENSOR = (int_triple *)realloc( abc_CENSOR ,
                                             sizeof(int_triple)*(num_CENSOR+1) );
         rab.i = r; rab.j = a; rab.k = b; abc_CENSOR[num_CENSOR++] = rab ;
       } /* end of loop over -CENSORTR strings */
       if( nerr > 0 ) ERROR_exit("Can't proceed after -CENSORTR errors!") ;
       NI_delete_str_array(nsar) ; free(src) ;
       continue ;  /* next option */
     }

     /*----------*/

     if( strcasecmp(argv[iarg],"-verb") == 0 ){ verb++ ; iarg++ ; continue ; }
     if( strcasecmp(argv[iarg],"-quiet")== 0 ){ verb=0 ; iarg++ ; continue ; }

     /*----------*/

     if( strcmp(argv[iarg],"-tpattern") == 0 ){
       if( iarg == argc-1 ) ERROR_exit("Need argument after '%s'",argv[iarg]);
       tpat = argv[++iarg] ;
       iarg++ ; continue ;
     }

     /*----------*/

     if( strcmp(argv[iarg],"-stimtime") == 0 ){ iarg++ ; continue ; }
     if( strcmp(argv[iarg],"-threshtype") == 0 ){ iarg++ ; continue ; }
     if( strcmp(argv[iarg],"-fitts") == 0 ){ iarg++ ; continue ; }
     if( strcmp(argv[iarg],"-errts") == 0 ){ iarg++ ; continue ; }
     if( strcmp(argv[iarg],"-cbucket") == 0 ){ iarg++ ; continue ; }

     /*==========*/

     ERROR_exit("Unknown argument: '%s'",argv[iarg]) ;

   } /*----- end of scanning args -----*/

   /*--- check for input errors or inconsistencies ---*/

   if( iarg < argc ) WARNING_message("arguments after last option???") ;

   if( inset == NULL ) ERROR_exit("No -input dataset given?") ;

   /* check CENSOR command for run indexes: should all have them, or none */

   if( abc_CENSOR != NULL ){
     int ic , rr , nzr=0 ;
     for( ic=0 ; ic < num_CENSOR ; ic++ ) /* count number with run != 0 */
       if( abc_CENSOR[ic].i ) nzr++ ;
     if( nzr > 0 && nzr < num_CENSOR )
       WARNING_message("%d -CENSORTR commands have run: numbers and %d do not!" ,
                       nzr , num_CENSOR-nzr ) ;
   }

   /*--- setup timing information ---*/

   if( tr <= 0.0f ){
     tr = DSET_TR(inset) ;
     if( tr <= 0.0f ){
       WARNING_message("no TR in dataset and no -TR option ==> TR = 1 s") ;
       tr = 1.0f ;
     }
   } else {
     float dd = DSET_TR(inset) ;
     if( dd > 0.0f && fabsf(dd-tr) > 0.001f )
       WARNING_message("-TR %g overrides dataset TR=%g",tr,dd) ;
   }
   nls->tr = tr ;

   if( tpat != NULL ){
     nls->prob_toff = TS_parse_tpattern( DSET_NZ(inset), nls->tr , tpat ) ;
     if( nls->prob_toff == NULL )
       nls->prob_toff = (float *)calloc(sizeof(float),DSET_NZ(inset)) ;
   } else {
     nls->prob_toff = (float *)calloc(sizeof(float),DSET_NZ(inset)) ;
     if( inset->taxis != NULL && inset->taxis->toff_sl != NULL )
       memcpy( nls->prob_toff, inset->taxis->toff_sl,
                               sizeof(float)*DSET_NZ(inset) ) ;
   }

   /*----- setup run start indexes -----*/

   nt = DSET_NVALS(inset) ;

   if( DSET_IS_TCAT(inset) ){  /* from dataset catenated on command line */

     if( concatim != NULL )
       WARNING_message("-concat ignored since -input catenated on command line");
     nls->run_num   = inset->tcat_num ;
     nls->run_start = (int *)malloc( sizeof(int)*nls->run_num ) ;
     nls->run_start[0] = 0 ;
     for( ii=0 ; ii < nls->run_num-1 ; ii++ )
       nls->run_start[ii+1] = nls->run_start[ii] + inset->tcat_len[ii] ;

   } else if( concatim != NULL ){  /* from -concat */

     float *car = MRI_FLOAT_PTR(concatim) ;
     nls->run_num   = concatim->nvox ;
     nls->run_start = (int *)malloc( sizeof(int)*nls->run_num ) ;
     for( ii=0 ; ii < nls->run_num ; ii++ ) nls->run_start[ii] = (int)car[ii] ;
     mri_free(concatim); concatim = NULL;
     jj = 0 ;
     if( nls->run_start[0] != 0 ) jj++ ;
     for( ii=1 ; ii < nls->run_num ; ii++ )
       if( nls->run_start[ii] <= nls->run_start[ii-1] ) jj++ ;
     if( jj > 0 ) ERROR_exit("Disordered run start indexes in -concat") ;
     if( nls->run_start[nls->run_num-1] >= nt )
       ERROR_exit("last -concat value %d is after end of input dataset (%d)?!",
                  nls->run_start[nls->run_num-1] , nt-1 ) ;

   } else {  /* default = just 1 big happy run */

     nls->run_num      = 1 ;
     nls->run_start    = (int *)malloc(sizeof(int)) ;
     nls->run_start[0] = 0 ;
   }

   /*--- estimate desirable polort from max block duration ---*/

   { int ibot, itop, ilen, lmax=0 ; float dmax ;
     for( ii=0 ; ii < nls->run_num ; ii++ ){
       ibot = nls->run_start[ii] ;
       itop = (ii < nls->run_num-1) ? nls->run_start[ii+1] : nt ;
       ilen = itop - ibot ; lmax = MAX(lmax,ilen) ;
     }
     dmax = tr * lmax ; ilen = 1 + (int)floor(dmax/150.0) ;
     if( nls->polort < -1 ){
       nls->polort = ilen ;
       INFO_message("longest run=%.1f s; automatic polort=%d",dmax,ilen) ;
     } else if( nls->polort >= 0 && ilen > nls->polort ){
       WARNING_message(
           "input polort=%d; longest run=%.1fs; recommended polort=%d",
           nls->polort , dmax , ilen ) ;
     }
   }

   /*----- mask-ification -----*/

   if( automask ){
     if( verb > 1 ) INFO_message("Making automask from input dataset") ;
     mask = THD_automask( inset ) ;
     if( mask == NULL ) ERROR_exit("automask-ing fails?!") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("%d voxels in automask",nmask) ;
   } else if( maskset != NULL ){
     if( DSET_NVOX(maskset) != DSET_NVOX(inset) )
       ERROR_exit("-mask and -input datasets not the same size!") ;
     if( !EQUIV_GRIDS(maskset,inset) )
       WARNING_message("-mask and -input datasets have differing grids!") ;
     if( verb > 1 ) INFO_message("reading mask dataset") ;
     mask = THD_makemask( maskset , 0 , 1.0f,-1.0f ) ;
     DSET_delete(maskset) ;
     if( mask == NULL ) ERROR_exit("mask is empty?!") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("%d voxels in mask",nmask) ;
   } else {
     nmask = DSET_NVOX(inset) ; mask = NULL ;
     if( verb ) INFO_message("no masking ==> will process all %d voxels",nmask) ;
   }

   /*----- construct the goodlist (from the censoring data) -----*/

   nls->nall = nt ;
   if( num_CENSOR == 0 || abc_CENSOR == NULL ){
     nls->ngood = 0 ; nls->goodlist = NULL ; nls->nfull = nt ;
     if( verb > 1 )
       INFO_message("no censoring ==> all %d time points used",nt) ;
   } else {
     int ic,it , rr , aa,bb , nerr=0 , bbot,btop , nblk=nls->run_num ;
     byte *cenar=(byte *)malloc(sizeof(byte)*nt) ;
     for( it=0 ; it < nt ; it++ ) cenar[it] = 1 ;
     for( ic=0 ; ic < num_CENSOR ; ic++ ){  /* loop over CENSOR commands */
       rr = abc_CENSOR[ic].i ;
       aa = abc_CENSOR[ic].j ; if( aa < 0  ) continue ;  /* shouldn't happen */
       bb = abc_CENSOR[ic].k ; if( bb < aa ) continue ;  /* shouldn't happen */
       if( rr == -666 ){  /* run = wildcard ==> expand to nblk new triples */
         int_triple rab ;
         abc_CENSOR = (int_triple *)realloc( abc_CENSOR ,
                                             sizeof(int_triple)*(num_CENSOR+nblk) );
        for( rr=1 ; rr <= nblk ; rr++ ){
           rab.i = rr; rab.j = aa; rab.k = bb; abc_CENSOR[num_CENSOR++] = rab;
         }
         continue ;  /* skip to next one */
       }
       if( rr > 0 ){       /* convert local indexes to global */
         if( rr > nblk ){  /* stupid user */
           ERROR_message("-CENSORTR %d:%d-%d has run index out of range 1..%d",
                         rr,aa,bb , nblk ) ;
           nerr++ ; aa = -66666666 ;
         } else {
           bbot = nls->run_start[rr-1] ;        /* start index of block #rr */
           btop = (rr < nblk) ? nls->run_start[rr]-1 : nt-1 ; /* last index */
           if( aa+bbot > btop ){  /* WTF? */
             WARNING_message(
              "-CENSORTR %d:%d-%d has start index past end of run (%d) - IGNORING",
              rr,aa,bb,btop-bbot ) ; aa = -66666666 ;
           } else if( bb+bbot > btop ){  /* oopsie */
             WARNING_message(
              "-CENSORTR %d:%d-%d has stop index past end of run (%d) - STOPPING THERE",
              rr,aa,bb,btop-bbot ) ;
           }
           aa += bbot ; bb += bbot ; if( bb > btop ) bb = btop ;
         }
       } else {           /* global indexes: check for stupidities */
         if( aa >= nt ){
           WARNING_message(
            "-CENSORTR %d..%d has start index past end of data (%d) - IGNORING",
            rr,aa,bb,nt-1 ) ; aa = -66666666 ;
         } else if( bb > nt ){
           WARNING_message(
            "-CENSORTR %d..%d has stop index past end of data (%d) - STOPPING THERE",
            rr,aa,bb,nt-1 ) ; bb = nt-1 ;
         }
       }
       if( aa < 0  || aa >= nt ) continue ;  /* nothing to do */
       if( bb < aa || bb >= nt ) continue ;
       if( verb > 1 )
         ININFO_message("applying -CENSORTR global time indexes %d..%d",aa,bb) ;
       for( it=aa ; it <= bb ; it++ ) cenar[it] = 0 ;
     } /* end of loop over CENSOR commands */
     if( nerr > 0 ) ERROR_exit("Can't continue! Fix the -CENSORTR error%s",
                               (nerr==1) ? "." : "s." ) ;
     free((void *)abc_CENSOR) ; abc_CENSOR = NULL ; num_CENSOR = 0 ;
     nls->goodlist = (int *)malloc(sizeof(int)*nt) ;
     for( ic=it=0 ; it < nt ; it++ ){
       if( cenar[it] ) nls->goodlist[ic++] = it ;
     }
     nls->ngood = nls->nfull = ic ;
     if( ic < 3 ) ERROR_exit("Number of points surviving censoring = %d",ic) ;
     free((void *)cenar) ;
     if( verb > 1 )
       INFO_message("censoring ==> %d time points used (out of %d total)",
                    nls->ngood , nt ) ;
   }

   /*----- extract time series from input dataset -----*/

   if( verb > 1 ) INFO_message("loading time series vectors") ;

   DSET_load(inset); CHECK_LOAD_ERROR(inset);

   nls->nz = DSET_NZ(inset); nls->ny = DSET_NY(inset); nls->nx = DSET_NX(inset);
   nls->nprob = nls->nz ;
   nls->nvec  = (int *)   calloc(nls->nprob,sizeof(int)    ) ;
   nls->vec   = (float **)calloc(nls->nprob,sizeof(float *)) ;
   nls->ivec  = (int **)  calloc(nls->nprob,sizeof(int *)  ) ;
   for( nvectot=kk=0 ; kk < nls->nz ; kk++ ){
     imar = NCO_extract_slice( inset, kk, mask, nls->ngood,nls->goodlist ) ;
     if( imar == NULL || IMARR_COUNT(imar) != 2 ){
       WARNING_message("No data extracted in slice #%d",kk) ;
       nls->vec[kk] = NULL ; nls->ivec[kk] = NULL ; nls->nvec[kk] = 0 ;
     } else {
       sim = IMARR_SUBIM(imar,0) ; sar = MRI_FLOAT_PTR(sim) ;
       iim = IMARR_SUBIM(imar,1) ; iar = MRI_INT_PTR  (iim) ;
       nls->vec[kk]  = MRI_FLOAT_PTR(sim) ; mri_clear_data_pointer(sim) ;
       nls->ivec[kk] = MRI_INT_PTR(iim)   ; mri_clear_data_pointer(iim) ;
       nls->nvec[kk] = iim->nx ; nvectot += iim->nx ;
       DESTROY_IMARR(imar) ;
     }
   }
   DSET_unload(inset) ;
   if( nvectot == 0 ) ERROR_exit("no time series to analyze?!") ;
   else if( verb )    INFO_message("analyzing %d time series",nvectot) ;

   /************/
   exit(0) ;
}

/*---------------------------------------------------------------------------*/

static void NCO_help(void)
{
   printf("\n"
    "This program fits a mixed linear/nonlinear deconvolution model to FMRI\n"
    "time series.  At present, it is experimental, so be careful out there.\n"
    "\n"
    "Usage: 3dNeocon ...\n"
    "\n"
    "Data Arguments\n"
    "--------------\n"
    "These arguments specify the input data and things about it.\n"
    "\n"
    "  -input dname      = read 3D+time dataset 'dname'\n"
    "  -mask mname       = read dataset 'mname' as a mask\n"
    "  -automask         = compute a mask from the 3D+time dataset\n"
    "  -TR tt            = use 'tt' as the TR (in seconds)\n"
    "  -CENSORTR clist   = like in 3dDeconvolve\n"
    "  -concat rname     = like in 3dDeconvolve\n"
    "  -tpattern ppp     = set the slice timing pattern to 'ppp', as in\n"
    "                      3dTshift, to override whatever slice timing\n"
    "                      information is in the '-input' dataset header;\n"
    "                      in particular, use 'zero' or 'equal' to specify\n"
    "                      that all slices are to be treated as acquired\n"
    "                      simultaneously (e.g., 3D imaging or 2D imaging\n"
    "                      with slice timing correction already performed)\n"
    "\n"
    "Baseline Model Arguments\n"
    "------------------------\n"
    "These arguments set up the baseline model, which is linear and estimated\n"
    "separately in each voxel (much as in 3dDeconvolve).\n"
    "\n"
    "  -polort pnum      = like in 3dDeconvolve [default is 'pnum' == 'A']\n"
    "\n"
    "  -baseline bb      = read 'bb' (a 1D file) for the baseline model;\n"
    "                      this file can have 1 or more columns:\n"
    "                      * if 1 column, then it is used in all slices\n"
    "                      * if more than 1 column, then 'bb' must have\n"
    "                        the same number of columns that the '-input'\n"
    "                        dataset has slices, and each column in 'bb'\n"
    "                        becomes part of the baseline model for only\n"
    "                        the corresponding slice\n"
    "\n"
    "Response Model Arguments\n"
    "------------------------\n"
    "These arguments specify the response model.  The Hemodynamic Response\n"
    "Function (HRF) model is nonlinear.  Given the HRF, the response to each\n"
    "stimulus is modeled additively.  Important notes:\n"
    " * Each slice might have a different time offset, in which case the HRF\n"
    "    will be applied slightly differently in each slice.\n"
    " * At least one '-stimtime' option must be given (or what would the\n"
    "    program be doing?).\n"
    " * The same HRF applies to all voxels -- this is one distinction\n"
    "    between 3dNeocon and 3dDeconvolve, where the HRF is different\n"
    "    in each voxel.  Only the amplitudes of the HRF fit vary between\n"
    "    voxels.\n"
    " * The HRF itself has amplitude 1 (maximum absolute value).  It is\n"
    "    the fit coefficients in each voxel that make the response model\n"
    "    vary in magnitude.\n"
    " * Each time in the '-stimtime' inputs gets a separate amplitude estimate.\n"
    "    These need to be combined and/or contrasted in some other program,\n"
    "    such as 3dttest, to get a statistical map.\n"
    "\n"
    "  -stimtime tname label HRFmodel\n"
    "\n"
    "   'tname' is the same format as 3dDeconvolve's '-stim_times' option\n"
    "   'label' is a character string to identify the output coefficients\n"
    "   'HRFmodel' specifies the form of the nonlinear response model; the\n"
    "    possibilities are\n"
    "    * 'GAMVAR' == the HRF has two parameters: 'tpeak' and 'fwhm';\n"
    "        HRF(t) = (t/bc)^b exp(b-t/c) * Heaviside(t)\n"
    "        b      = (2.3*tpeak/fwhm)^2\n"
    "        c      = tpeak/b\n"
    "        tpeak is allowed to range from 5 to 8 s;\n"
    "        fwhm is allowed to range from 4 to 8 s.\n"
    "        This HRF choice is appropriate for brief (under 2 s)\n"
    "        activations in response to each stimulus.\n"
    "    * 'IGAMVAR(d)' == similar to 'GAMVAR' but integrated over a duration\n"
    "        of 'd' seconds.  This HRF choice is designed for block-design\n"
    "        FMRI experiments.\n"
    "    * 'CSPLIN(b,t,n)' == same as in 3dDeconvolve, with the caveat that\n"
    "        the maximum amplitude of the HRF will be one.\n"
    "    * 'DITTO' or 'ditto' == use the same model AND the same parameter set\n"
    "        as the previous '-stimtime' argument.  In this way, the nonlinear\n"
    "        parameters for the HRFs for the two sets of time will be collapsed\n"
    "        into one set (e.g., both use the same value of 'tpeak' and 'fwhm').\n"
    "        Of course, the linear amplitudes for the two sets of times will be\n"
    "        separated.\n"
    "\n"
    "  -threshtype hhh \n"
    "\n"
    "   'hhh' specifies the thresholding model used in the HRF analysis.\n"
    "   Given a set of HRF parameters, linear regression is used to fit\n"
    "   the baseline model and the baseline+response model in all voxels.\n"
    "   Those voxels that pass the threshold model (i.e., their response\n"
    "   fit is 'significant' in some sense) are used to judge the quality\n"
    "   of the overall fit.  The choices for 'hhh' are\n"
    "   * 'RCLU(p,c)' == the nominal R^2-statistic (assuming white noise)\n"
    "      is computed in each voxel, and those voxels with a p-value at\n"
    "      or below 'p' are kept, if they are in a cluster of at least 'c'\n"
    "      such contiguous voxels.  Subsequent to this thresholding, voxels\n"
    "      that fit better (have a larger R^2) are weighted more highly\n"
    "      in the HRF fitting objective function.\n"
    "   * At this time, there is no other thresholding model available.\n"
    "   * The default is currently 'RCLU(0.01,5)' which was just picked out\n"
    "      of thin air with no justification.  This default is subject to\n"
    "      change!\n"
    "\n"
    "Output Arguments\n"
    "----------------\n"
    "  -fitts fprefix    = Output a fitted time series model dataset.\n"
    "  -errts eprefix    = Output the residuals into a dataset.\n"
    "  -cbucket cprefix  = Output the fit coefficients into a dataset.\n"
    "\n"
    "  -verb   = Increase the verbosity of the messages as the program runs.\n"
    "  -quiet  = Turn off informational progress messages.\n"
   ) ;

   printf("\n*** AUTHOR = Zhark the Experimental, October 2007 ***\n\n" ) ;
   return ;
}
