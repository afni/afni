#include "mrilib.h"

#undef  PREP_SPEARMAN
#undef  PREP_QUANTILE

#define PREP_SPEARMAN  1
#define PREP_QUANTILE  3

static int num_quantile = 9 ;

/*--------------------------------------------------------------------------*/

static void rank_orderize( int n , float *a )
{
   register int ii , ns , n1 , ib ;
   int   *b ;  /* workspaces */
   float *c ;
   float cs ;

   /*- handle special cases -*/

   if( a == NULL || n < 1 ) return ;        /* meaningless input */
   if( n == 1 ){ a[0] = 0.0f ; return ; }    /* only one point!? */

   /*- make workspaces -*/

   b = (int   *) malloc(sizeof(int  )*n) ;
   c = (float *) malloc(sizeof(float)*n) ;

   for( ii=0 ; ii < n ; ii++ ) c[ii] = b[ii] = ii ;

   /*- sort input, carrying b=original index along -*/

   qsort_floatint( n , a , b ) ;  /* see cs_sort_fi.c */

   /* compute ranks into c[] */

   n1 = n-1 ;
   for( ii=0 ; ii < n1 ; ii++ ){
     if( a[ii] == a[ii+1] ){                  /* handle ties */
       cs = 2*ii+1 ; ns = 2 ; ib = ii ; ii++ ;
       while( ii < n1 && a[ii] == a[ii+1] ){ ii++ ; ns++ ; cs += ii ; }
       for( cs/=ns ; ib <= ii ; ib++ ) c[ib] = cs ;
     }
   }

   /* put ranks into original vector at original index locations */

   for( ii=0 ; ii < n ; ii++ ) a[b[ii]] = c[ii] ;

   free(c) ; free(b) ; return ;
}

/*--------------------------------------------------------------------------*/
/* These PREP function process the data in-place. */

void PREP_spearman( int n , float *ar )
{
   int ii ; float rb ;

   rank_orderize(n,ar) ;        /*  ranks are 0..n-1 (cf. thd_correlate.c) */
   rb = 0.5f*(n-1) ;                                          /* mean rank */
   for( ii=0 ; ii < n ; ii++ ) ar[ii] -= rb ;          /* remove mean rank */
   return ;
}

/*--------------------------------------------------------------------------*/

void PREP_quantile( int n , float *a )
{
   int ii ; float rb , jf ;

   jf = 0.001f + 1.00001f * (n-0.5f) / (float)num_quantile ;
   if( jf <= 2.0f ){ PREP_spearman(n,a) ; return ; }
   jf = 1.0f / jf ;

   rank_orderize(n,a) ;        /* convert to ranks */

   for( rb=0.0f,ii=0 ; ii < n ; ii++ ){
     a[ii] = (int)( (a[ii]+0.333f)*jf ) ; rb += a[ii] ;
   }
   rb /= n ;
   for( ii=0 ; ii < n ; ii++ ) a[ii] -= rb ;
   return ;
}

/*--------------------------------------------------------------------------*/

void usage_3dTransformGroupInCorr(int detail)
{
   printf(
     "Usage: 3dTransformGroupInCorr [options] AAA.grpincorr.niml\n"
     "\n"
     "This program reads in a collection of AFNI 3D+time datasets\n"
     "that were processed by 3dSetupGroupInCorr and then applies\n"
     "a transform to each time series in the file AAA.grpincorr.data.\n"
     "\n"
     "** WARNING **\n"
     " The .data and .niml files are transformed IN-PLACE -- that is, they\n"
     " are re-written. If you want to keep the original, then you must use this\n"
     " program on a COPY of the original files, or they will be lost forever.\n"
     "\n"
     "OPTIONS:\n"
     "--------\n"
     " -prep XXX    = Defines the transformation.  The current possibilities are:\n"
     "                  SPEARMAN   ==> convert the data to ranks, so that the\n"
     "                                 resulting individual subject correlations\n"
     "                                 in 3dGroupInCorr are Spearman correlations\n"
     "                  QUANTILE:n ==> convert the data to n-levels of quantiles,\n"
     "                                 so that the individual subject correlations\n"
     "                                 become quantile correlations.  If ':n' is\n"
     "                                 not given, then n=9 will be used.\n"
     "\n"
     "NOTA BENE:\n"
     "----------\n"
     "Normally, you would probably do something like\n"
     "  cp XXX.grpincorr.niml SP_XXX.grpincorr.niml\n"
     "  cp XXX.grpincorr.data SP_XXX.grpincorr.data\n"
     "  3dTransformGroupInCorr -prep SPEARMAN SP_XXX.grpincorr.niml\n"
     "In this way, you would keep the original files for comparison purposes.\n"
     "\n"
     "Author -- RWCox -- May 2012\n"
   ) ;
   PRINT_COMPILE_DATE ;
   return;
}

/*--------------------------------------------------------------------------*/

typedef struct {

  int nvec  ;     /* number of vectors in a dataset */
  int ndset ;     /* number of datasets */
  int *nvals ;    /* nvals[i] = number of values in a vector in i-th dataset */
  int nvals_max ; /* largest nvals[i] value */
  int nvals_tot ; /* sum of nvals[i] values */
  int datum ;     /* 1 for sbyte, 2 for short */

  int nuse , *use ;  /* 07 Apr 2010: subset of datasets to use */

  char *geometry_string ;
  THD_3dim_dataset *tdset ; /* template dataset */
  int nx,ny,nz,nvox ;       /* number of nodes in template */

  char *dfname ;  /* data file name */
  int  *ivec   ;  /* ivec[i] = spatial index of i-th vector, i=0..nvec-1 */
  float *fac   ;  /* fac[i] = scale factor for i-th dataset, i=0..ndset-1 */
  short **sv   ;  /* sv[i] = short array [nvals[i]*nvec] for i-th dataset */
  sbyte **bv   ;  /* bv[i] = sbyte array [nvals[i]*nvec] for i-th dataset */

  char **dslab ;  /* dslab[i] = label string for i-th dataset [23 May 2010] */

  long long nbytes ;  /* number of bytes in the data array */

  /* Surface stuff  ZSS Jan 09 2010 */

  int nnode[2]   ;
  int ninmask[2] ;

} MRI_shindss ;  /* short/sbyte indexed datasets */

/*--------------------------------------------------------------------------*/

#undef  GQUIT
#define GQUIT(sss)                                                     \
 do{ if( tdset != NULL ) DSET_delete(tdset) ;                          \
     if( dfname != NULL ) free(dfname) ;                               \
     if( geometry_string != NULL ) free(geometry_string) ;             \
     NI_free_element(nel) ; nelshd = NULL ;                            \
     if( sss != NULL ) ERROR_message("TIC: file %s: %s",fname,(sss)) ; \
     return(NULL) ;                                                    \
 } while(0)

/*--------------------------------------------------------------------------*/

static NI_element *nelshd = NULL ;

static const long long twogig = 2ll * 1024ll * 1024ll * 1024ll ;  /* 2 GB */

/*----- read a PREFIX.grpincorr.niml file into a struct -----*/

MRI_shindss * GRINCOR_read_input( char *fname )
{
   NI_element *nel=NULL ;
   char *dfname=NULL , *atr ;
   NI_float_array *facar ; NI_int_array *nvar, *nnode=NULL, *ninmask=NULL;
   MRI_shindss *shd ;
   long long nbytes_needed , nbytes_dfname=0 ; int fdes ;
   void *var ; int ids , nvmax , nvtot ;
   int datum , datum_size ;

   char *geometry_string=NULL ;
   THD_3dim_dataset *tdset=NULL; int nvox;
   int no_ivec=0 , *ivec=NULL , *nvals=NULL , nvec,ndset ; float *fac=NULL ;
   NI_str_array *slabar=NULL ;

   if( fname == NULL || *fname == '\0' ) GQUIT(NULL) ;

   /* get data element */

   if (!THD_is_ondisk(fname))
     GQUIT("not on disk") ;

   nelshd = nel = NI_read_element_fromfile(fname) ;

   if( nel == NULL || nel->type != NI_ELEMENT_TYPE )
     GQUIT("not properly formatted") ;
   if( strcmp(nel->name,"3dGroupInCorr") != 0 )
     GQUIT("data element name is not '3dGroupInCorr'") ;

   /* no data vector ==> using all voxels */

   no_ivec = ( nel->vec_num < 1 ||
               nel->vec_len < 1 || nel->vec_typ[0] != NI_INT ) ;

   /* number of vectors in each dataset */

   atr = NI_get_attribute(nel,"nvec");
   if( atr == NULL ) GQUIT("nvec attribute missing?") ;
   nvec = (int)strtod(atr,NULL) ;
   if( nvec < 2 || (!no_ivec && nel->vec_len != nvec) )
     GQUIT("nvec attribute has illegal value") ;

   /* number of datasets */

   atr = NI_get_attribute(nel,"ndset");
   if( atr == NULL ) GQUIT("ndset attribute missing") ;
   ndset = (int)strtod(atr,NULL) ;
   if( ndset < 1 ) GQUIT("ndset attribute has illegal value") ;

   /* number of time points in each dataset (varies with dataset) */

   atr = NI_get_attribute(nel,"nvals");
   if( atr == NULL ) GQUIT("nvals attribute missing") ;
   nvar = NI_decode_int_list(atr,",") ;
   if( nvar == NULL || nvar->num < ndset )
     GQUIT("nvals attribute doesn't match ndset") ;
   nvals = nvar->ar ; nvar->ar = NULL ; NI_delete_int_array(nvar) ;

   nvmax = nvtot = nvals[0] ;
   for( ids=1 ; ids < ndset ; ids++ ){             /* Feb 2011 */
     nvtot += nvals[ids] ;
     if( nvals[ids] > nvmax ) nvmax = nvals[ids] ;
   }

   /* dataset labels [23 May 2010] */

   atr = NI_get_attribute(nel,"dset_labels") ;
   if( atr != NULL ){
     slabar = NI_decode_string_list(atr,";,") ;
     if( slabar == NULL || slabar->num < ndset )
       GQUIT("dset_labels attribute invalid") ;
   }

   /* datum of datasets */

   atr = NI_get_attribute(nel,"datum") ;
   if( atr != NULL && strcasecmp(atr,"byte") == 0 ){
     datum = 1 ; datum_size = sizeof(sbyte) ;
   } else {
     datum = 2 ; datum_size = sizeof(short) ;
   }

   /* number of bytes needed:
        sizeof(datum) * number of vectors per dataset
                      * number of datasets
                      * sum of per dataset vector lengths */

   nbytes_needed = 0 ;
   for( ids=0 ; ids < ndset ; ids++ ) nbytes_needed += nvals[ids] ;
   nbytes_needed *= ((long long)nvec) * datum_size ;

   if( nbytes_needed >= twogig &&
       ( sizeof(void *) < 8 || sizeof(size_t) < 8 ) ) /* too much for 32-bit */
     GQUIT("datafile size exceeds 2 GB -- you need a 64-bit computer!") ;

   /* scale factor for each dataset */

   atr = NI_get_attribute(nel,"fac") ;
   if( atr == NULL ) GQUIT("fac attribute missing") ;
   facar = NI_decode_float_list(atr,",") ;
   if( facar == NULL || facar->num < ndset )
     GQUIT("can't decode fac attribute") ;
   fac = facar->ar ; facar->ar = NULL ; NI_delete_float_array(facar) ;

   for( ids=0 ; ids < ndset ; ids++ ) if( fac[ids] <= 0.0f ) fac[ids] = 1.0f ;

   /* grid definition */

   atr = NI_get_attribute(nel,"geometry") ;
   if( atr == NULL ) GQUIT("geometry attribute missing") ;
   geometry_string = strdup(atr) ;
   tdset = EDIT_geometry_constructor( geometry_string , "GrpInCorr" ) ;
   if( tdset == NULL ) GQUIT("can't decode geometry attribute") ;
   nvox = DSET_NVOX(tdset) ;
   if(  no_ivec && nvox != nvec )
     GQUIT("geometry attribute doesn't match nvec attribute") ;
   if( !no_ivec && nvox <  nvec )
     GQUIT("geometry attribute specifies too few voxels") ;

   /* name of data file: check its size against what's needed */

#if 0
   atr = NI_get_attribute(nel,"datafile") ;
   if( atr != NULL ){
     dfname = strdup(atr) ; nbytes_dfname = THD_filesize(dfname) ;
     if( nbytes_dfname <= 0 && strstr(dfname,"/") != NULL ){
       char *tnam = THD_trailname(atr,0) ;
       nbytes_dfname = THD_filesize(tnam) ;
       if( nbytes_dfname > 0 ){ free(dfname); dfname = strdup(tnam); }
   }
#endif
   if( nbytes_dfname <= 0 && strstr(fname,".niml") != NULL ){
     if( dfname != NULL ) free(dfname) ;
     dfname = strdup(fname) ; strcpy(dfname+strlen(dfname)-5,".data") ;
     nbytes_dfname = THD_filesize(dfname) ;
   }
   if( nbytes_dfname <= 0 ){
     char mess[THD_MAX_NAME+256] ;
     sprintf(mess,"datafile is missing (%s)",dfname) ; GQUIT(mess) ;
   } else if( nbytes_dfname < nbytes_needed ){
     char mess[THD_MAX_NAME+1024] ;
     sprintf(mess,"datafile %s has %s bytes but needs at least %s",
              dfname ,
              commaized_integer_string(nbytes_dfname) ,
              commaized_integer_string(nbytes_needed) ) ;
     GQUIT(mess) ;
   } else {
     INFO_message("TIC: data file %s found with %s bytes of data",
                  dfname , commaized_integer_string(nbytes_dfname) ) ;
   }
   fdes = open( dfname , O_RDWR ) ;
   if( fdes < 0 ){
     char mess[THD_MAX_NAME+256] ;
     sprintf(mess,"can't open datafile (%s)",dfname) ; GQUIT(mess) ;
   }
   NI_set_attribute( nelshd , "datafile" , dfname ) ;

   /* ivec[i] is the voxel spatial index of the i-th vector */

   if( no_ivec ){
     ivec = NULL ;  /* means all voxels: ivec[i] == i */
   } else {
     ivec = (int *)nel->vec[0] ; /* copy pointer */
   }

   /* And stuff for LR surface pairs      ZSS Jan 09*/
   if ((atr=NI_get_attribute(nel,"LRpair_nnode"))) {
      nnode = NI_decode_int_list(atr,",") ;
   }
   if ((atr=NI_get_attribute(nel,"LRpair_ninmask"))) {
      ninmask = NI_decode_int_list(atr,",") ;
   }

   /* create output struct */

   shd = (MRI_shindss *)malloc(sizeof(MRI_shindss)) ;

   shd->nvals = nvals ; shd->nvals_max = nvmax ; shd->nvals_tot = nvtot ;
   shd->nvec  = nvec  ;
   shd->ndset = ndset ;

   shd->geometry_string = geometry_string ;
   shd->tdset           = tdset ;
   shd->dfname          = dfname ;
   shd->nvox            = nvox ;
   shd->nx = DSET_NX(tdset); shd->ny = DSET_NY(tdset); shd->nz = DSET_NZ(tdset);

   shd->ivec = ivec ;
   shd->fac  = fac  ;

   /* and surface fields...      ZSS      Jan 09 */
   if (nnode) {
      if (nnode->num != 2) GQUIT("LRpair_nnode must have 2 values");
      shd->nnode[0] = nnode->ar[0];
      shd->nnode[1] = nnode->ar[1];
      NI_delete_int_array(nnode); nnode=NULL;
   } else {
      shd->nnode[0] = shd->nnode[1] = -1 ;
   }
   if (ninmask) {
      if (ninmask->num != 2) GQUIT("LRpair_ninmask must have 2 values");
      shd->ninmask[0] = ninmask->ar[0];
      shd->ninmask[1] = ninmask->ar[1];
      NI_delete_int_array(ninmask); ninmask=NULL;
   } else {
      shd->ninmask[0] = shd->ninmask[1] = -1 ;
   }

   /*--- 07 Apr 2010: setup default use list (all of them) ---*/

   shd->nuse = ndset ;
   shd->use  = (int *)malloc(sizeof(int)*ndset) ;
   for( ids=0 ; ids < ndset ; ids++ ) shd->use[ids] = ids ;

   shd->dslab = (slabar != NULL) ? slabar->str : NULL ;  /* 23 May 2010 */

   /*--- now have to map data from disk ---*/

   var = mmap( 0 , (size_t)nbytes_needed ,
                   PROT_WRITE , THD_MMAP_FLAG , fdes , 0 ) ;
   close(fdes) ;  /* close file descriptor does not unmap data */

   if( var == (void *)(-1) ){ /* this is bad */
     ERROR_message(
       "TIC: file %s: can't mmap() datafile -- memory space exhausted?" , dfname ) ;
     free(shd) ; return NULL ;
   }

   /*-- create array of pointers to each dataset's data array --*/

   shd->datum = datum ;

   if( datum == 2 ){  /* shorts */
     shd->sv    = (short **)malloc(sizeof(short *)*ndset) ;
     shd->bv    = NULL ;
     shd->sv[0] = (short *)var ;
     for( ids=1 ; ids < ndset ; ids++ )
       shd->sv[ids] = shd->sv[ids-1] + nvals[ids-1]*nvec ;
   } else {           /* sbytes */
     shd->sv    = NULL ;
     shd->bv    = (sbyte **)malloc(sizeof(sbyte *)*ndset) ;
     shd->bv[0] = (sbyte *)var ;
     for( ids=1 ; ids < ndset ; ids++ )
       shd->bv[ids] = shd->bv[ids-1] + nvals[ids-1]*nvec ;
   }

   shd->nbytes = nbytes_needed ;
   return shd ;
}

#undef GQUIT

/*----------------------------------------------------------------------------*/
/* Create a vectim struct from 1 dataset inside the collection */

MRI_vectim * GRINCOR_extract_vectim_short( MRI_shindss *shd , int ids )
{
   MRI_vectim *mv ;
   long long nvec=shd->nvec , nvals=shd->nvals[ids] , ii,nvv ;
   float fac=shd->fac[ids] , *fv ;
   short *sv = shd->sv[ids] ;

   MAKE_VECTIM( mv , nvec , nvals ) ;
   fv  = mv->fvec ;
   nvv = nvec * nvals ;
   for( ii=0 ; ii < nvv ; ii++ ) fv[ii] = fac * sv[ii] ;
   return mv ;
}

MRI_vectim * GRINCOR_extract_vectim_sbyte( MRI_shindss *shd , int ids )
{
   MRI_vectim *mv ;
   long long nvec=shd->nvec , nvals=shd->nvals[ids] , ii,nvv ;
   float fac=shd->fac[ids] , *fv ;
   sbyte *sv = shd->bv[ids] ;

   MAKE_VECTIM( mv , nvec , nvals ) ;
   fv  = mv->fvec ;
   nvv = nvec * nvals ;
   for( ii=0 ; ii < nvv ; ii++ ) fv[ii] = fac * sv[ii] ;
   return mv ;
}

MRI_vectim * GRINCOR_extract_vectim( MRI_shindss *shd , int ids )
{
   MRI_vectim *mv ;
   if( shd->datum == 1 ) mv = GRINCOR_extract_vectim_sbyte(shd,ids) ;
   else                  mv = GRINCOR_extract_vectim_short(shd,ids) ;
   return mv ;
}

/*----------------------------------------------------------------------------*/
/* Put a vectim back into its dataset, scaling as needed. */

float GRINCOR_scale_vectim( MRI_vectim *mv )
{
   long long nvec , nvals , kk , nvv ; float *fv , val, top ;

   THD_vectim_normalize( mv ) ;   /* L2 normalize */

   /* find largest absolute value over all vectors */

   nvec = mv->nvec ; nvals = mv->nvals ; nvv = nvec * nvals ;
   top = 0.0f ; fv = mv->fvec ;
   for( kk=0 ; kk < nvv ; kk++ ){
     val = fabsf(fv[kk]) ; if( val > top ) top = val ;
   }
   if( top == 0.0f ) top = 1.0f ;

   return top ;
}

void GRINCOR_insert_vectim_short( MRI_shindss *shd , int ids , MRI_vectim *mv )
{
   long long nvec=shd->nvec , nvals=shd->nvals[ids] , ii , kk,nvv ;
   float *fv , val,top ;
   short *sv = shd->sv[ids] ;

   top = 32766.0f / GRINCOR_scale_vectim(mv) ;
   shd->fac[ids] = 1.0f / top ;
   nvv = nvec * nvals ;
   fv  = mv->fvec ;
   for( kk=0 ; kk < nvv ; kk++ ) sv[kk] = (short)rintf(top*fv[kk]) ;
   return ;
}

void GRINCOR_insert_vectim_sbyte( MRI_shindss *shd , int ids , MRI_vectim *mv )
{
   long long nvec=shd->nvec , nvals=shd->nvals[ids] , ii , kk,nvv ;
   float *fv , val,top ;
   sbyte *sv = shd->bv[ids] ;

   top = 127.4f / GRINCOR_scale_vectim(mv) ;
   shd->fac[ids] = 1.0f / top ;
   nvv = nvec * nvals ;
   fv  = mv->fvec ;
   for( kk=0 ; kk < nvv ; kk++ ) sv[kk] = (sbyte)rintf(top*fv[kk]) ;
   return ;
}

void GRINCOR_insert_vectim( MRI_shindss *shd , int ids , MRI_vectim *mv )

{
   if( shd->datum == 1 ) GRINCOR_insert_vectim_sbyte(shd,ids,mv) ;
   else                  GRINCOR_insert_vectim_short(shd,ids,mv) ;
   return ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   MRI_shindss *shd ;
   int   prepcode=0 , ids , nopt , kk ;
   void *prepfunc=NULL ;
   char prepname[128] = "\0" , *fname=NULL , *buf ;
   MRI_vectim *mv ; NI_float_array *facar ;

   /*--- official AFNI startup stuff ---*/

   mainENTRY("3dTransformGroupInCorr"); machdep();
   AFNI_logger("3dTransformGroupInCorr",argc,argv);
   PRINT_VERSION("3dTransformGroupInCorr"); AUTHOR("RW Cox");

   /*-- process options --*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-help") == 0 || strcmp(argv[nopt],"-h") == 0 ){
       usage_3dTransformGroupInCorr(strlen(argv[nopt]) > 3 ? 2:1);
       exit(0) ;
     }

     if( strcasecmp(argv[nopt],"-prep") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after %s",argv[nopt-1]) ;
       if( strcasecmp(argv[nopt],"SPEARMAN") == 0 ){
         prepcode = PREP_SPEARMAN ;
         prepfunc = (void *)PREP_spearman ;
         strcpy(prepname,"SPEARMAN") ;
       } else if( strncasecmp(argv[nopt],"QUANTILE",8) == 0 ){
         char *cpt ;
                           cpt = strchr(argv[nopt],':') ;
         if( cpt == NULL ) cpt = strchr(argv[nopt],'#') ;
         if( cpt == NULL ) cpt = strchr(argv[nopt],'@') ;
         if( cpt == NULL ) cpt = strchr(argv[nopt],'_') ;
         if( cpt == NULL ) cpt = argv[nopt]+8 ;
         if( isdigit(*(cpt+1)) ){
           kk = (int)strtod(cpt+1,NULL) ;
           if( kk > 1 && kk < 100 ) num_quantile = kk ;
         }
         prepcode = PREP_QUANTILE ;
         prepfunc = (void *)PREP_quantile ;
         sprintf(prepname,"QUANTILE:%d",num_quantile) ;
       } else {
         ERROR_exit("Illegal string '%s' after option %s",argv[nopt],argv[nopt-1]) ;
       }
       nopt++ ; continue ;
     }

     ERROR_message("Unknown option: '%s'",argv[nopt]) ;
     suggest_best_prog_option(argv[0], argv[nopt]);
     exit(1);
   }

   if( argc < 2 ){ usage_3dTransformGroupInCorr(2) ; exit(0) ; }

   /* check for errors */

   if( prepfunc == NULL ) ERROR_exit("no -prep option given? :-(") ;

   if( nopt >= argc ) ERROR_exit("No input filename on command line?!") ;

   /*-- read input file --*/

   fname = strdup(argv[nopt]) ;
   if( STRING_HAS_SUFFIX(fname,".data") ){
     strcpy(fname+strlen(fname)-5,".niml") ;
     WARNING_message("TIC: Replaced '.data' with '.niml' in filename") ;
   } else if( STRING_HAS_SUFFIX(fname,".grpincorr") ){
     fname = (char *)realloc(fname,strlen(fname)+16) ;
     strcat(fname,".niml") ;
     INFO_message("TIC: Added '.niml' to end of filename") ;
   } else if( STRING_HAS_SUFFIX(fname,".grpincorr.") ){
     fname = (char *)realloc(fname,strlen(fname)+16) ;
     strcat(fname,"niml") ;
     INFO_message("TIC: Added 'niml' to end of filename") ;
   }
   shd = GRINCOR_read_input( fname ) ;
   if( shd == NULL || nelshd == NULL ) ERROR_exit("TIC: Cannot continue after input error") ;
   INFO_message("TIC: file opened, contains %d datasets, %d time series, %s bytes",
                shd->ndset , shd->nvec , commaized_integer_string(shd->nbytes) ) ;

   /*-- process input file --*/

   fprintf(stderr,"++ %s %d datasets: ",prepname,shd->ndset) ;
   for( ids=0 ; ids < shd->ndset ; ids++ ){
     fprintf(stderr,"%d",ids+1) ;
     mv = GRINCOR_extract_vectim( shd , ids ) ; fprintf(stderr,".") ;
     THD_vectim_applyfunc( mv , prepfunc ) ;    fprintf(stderr,".") ;
     GRINCOR_insert_vectim( shd , ids , mv ) ;  fprintf(stderr,".") ;
     VECTIM_destroy( mv ) ;
   }
   sync() ; fprintf(stderr,"\n") ;

   facar = (NI_float_array *)malloc(sizeof(NI_float_array)) ;
   facar->num = shd->ndset ; facar->ar = shd->fac ;
   buf = NI_encode_float_list( facar , "," ) ;
   NI_set_attribute( nelshd , "fac"  , buf ) ;       /* scale factor per dataset */
   NI_set_attribute( nelshd , "prep" , prepname ) ;

   kk = NI_write_element_tofile( fname , nelshd , NI_BINARY_MODE ) ;
   if( kk < 0 )
     ERROR_exit("TIC: Error while writing header file %s",fname) ;

   INFO_message("TIC: Re-wrote header file %s",fname) ; exit(0) ;
}
