#ifdef USE_OMP
#include <omp.h>
#endif

#include "mrilib.h"

/***** 3dREMLfit.c *****/

#ifdef USE_OMP
   int maxthr = 1 ;  /* max number of threads [16 Jun 2009] */
#else
# define maxthr 1    /* no OpenMP ==> 1 thread by definition */
#endif

static int verb=1 ;
static int goforit=0 ;

#undef FLOATIZE      /* we will use double precision for matrices */
#include "remla.c"   /* do NOT change this to FLOATIZE !!! */

#undef  INMASK
#define INMASK(i) ( mask[i] != 0 )

#undef MEMORY_CHECK
#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
# define MEMORY_CHECK                                                  \
   do{ long long nb = mcw_malloc_total() ;                             \
       if( nb > 0 && verb > 1 )                                        \
         ININFO_message("Memory usage now = %lld (%s)" ,               \
                        nb , approximate_number_string((double)nb) ) ; \
   } while(0)
#else
# define MEMORY_CHECK /*nada*/
#endif

#undef PATCHX
#undef REML_DEBUG

/*---------------------------------------------------------------------------*/
/*! Check matrix condition number.  Return value is the
    number of bad things that were detected.  Hopefully, zero.
-----------------------------------------------------------------------------*/

int check_matrix_condition( matrix xdata , char *xname )
{
    double *ev, ebot,emin,emax ;
    int i,nsmall=0, ssing=(verb>2), bad=0 ;

#undef  PSINV_EPS
#define PSINV_EPS      1.e-14  /* double precision */
#define CN_VERYGOOD   20.0
#define CN_GOOD      400.0
#define CN_OK       8000.0
#define CN_BEWARE 160000.0

    if( xname == NULL ) xname = "\0" ;
    ev   = matrix_singvals( xdata ) ;
    emax = ev[0] ;
    for( i=1 ; i < xdata.cols ; i++ ) if( ev[i] > emax ) emax = ev[i] ;
    ebot = sqrt(PSINV_EPS)*emax ; emin = 1.e+38 ;
    for( i=0 ; i < xdata.cols ; i++ ){
      if( ev[i] >= ebot && ev[i] < emin ) emin = ev[i] ;
      if( ev[i] <  ebot ) nsmall++ ;
    }

    if( emin <= 0.0 || emax <= 0.0 ){
      ERROR_message("----- !! %s matrix condition:  UNDEFINED ** VERY BAD **\n" ,
                     xname ) ;
      ssing = 1 ; bad++ ;
    } else {
      double cond=sqrt(emax/emin) ; char *rating ;
           if( cond < CN_VERYGOOD )  rating = "++ VERY GOOD ++"    ;
      else if( cond < CN_GOOD     )  rating = "++ GOOD ++"         ;
      else if( cond < CN_OK       )  rating = "++ OK ++"           ;
      else if( cond < CN_BEWARE   )  rating = "++ A LITTLE BIG ++" ;
      else                         { rating = "** BEWARE **" ; bad++; ssing = 1; }
      if( strstr(rating,"*") == NULL ){
        if( verb > 1 ){
          INFO_message("----- %s matrix condition (%dx%d):  %g  %s\n",
                  xname,
                  xdata.rows,xdata.cols , cond , rating ) ;
        }
      } else {
        WARNING_message("----- !! %s matrix condition (%dx%d):  %g  %s\n",
                xname,
                xdata.rows,xdata.cols , cond , rating ) ;
      }
    }

    if( nsmall > 0 ){
      WARNING_message(
        "!! in %s matrix:\n"
        " * Largest singular value=%g\n"
        " * %d singular value%s less than cutoff=%g\n"
        " * Implies strong collinearity in the matrix columns! \n",
        xname , emax , nsmall , (nsmall==1)?" is":"s are" , ebot ) ;
      ssing = 1 ; bad++ ;
    }

    if( ssing ){
      INFO_message("%s matrix singular values:\n",xname) ;
      for( i=0; i < xdata.cols ; i++ ){
        fprintf(stderr," %13g",ev[i]) ;
        if( i < xdata.cols-1 && i%5 == 4 ) fprintf(stderr,"\n") ;
      }
      fprintf(stderr,"\n") ;
    }

    free((void *)ev) ; return bad ;
}

/*---------------------------------------------------------------------------*/
/*! For voxel loop progress report. */

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*--------------------------------------------------------------------------*/

static int    Argc ;
static char **Argv ;
static char *commandline = NULL ;  /* from 3dDeconvolve */

static int usetemp = 0 ;

#undef  MTHRESH
#define MTHRESH (int64_t)7654321  /* 7.6+ million bytes */

/*--------------------------------------------------------------------------*/
/*! To create an empty dataset, with zero-filled sub-bricks,
    or with the data to be saved on disk temporarily (if fnam, fp != NULL). */

THD_3dim_dataset * create_float_dataset( THD_3dim_dataset *tset ,
                                         int nvol , char *prefix, int func ,
                                         char **fnam , FILE **fp            )
{
   THD_3dim_dataset *nset ; int kk ;

ENTRY("create_float_dataset") ;

   if( tset == NULL || nvol < 1 || prefix == NULL ) RETURN(NULL) ;

   nset = EDIT_empty_copy( tset ) ;
   EDIT_dset_items( nset ,
                      ADN_prefix    , prefix    ,
                      ADN_datum_all , MRI_float ,
                      ADN_brick_fac , NULL      ,
                      ADN_nvals     , nvol      ,
                      ADN_ntt       , nvol      ,
                    ADN_none ) ;
  if( func ) EDIT_TO_FUNC_BUCKET(nset) ;

  tross_Copy_History( tset , nset ) ;
  if( commandline != NULL )
    tross_multi_Append_History( nset , "Matrix source:" , commandline , NULL ) ;
  tross_Make_History( "3dREMLfit" , Argc,Argv , nset ) ;

  if( usetemp && fnam != NULL && fp != NULL &&
      DSET_TOTALBYTES(nset) > MTHRESH         ){
    reml_setup_savfilnam( fnam ) ;
    *fp = fopen( *fnam , "w+b" ) ;
    if( *fp == NULL ) ERROR_exit("3dREMLfit can't open -usetemp file %s",*fnam);
    add_purge(*fnam) ;
  } else {
    for( kk=0 ; kk < nvol ; kk++ )
      EDIT_substitute_brick( nset , kk , MRI_float , NULL ) ;
  }

  RETURN(nset) ;
}

/*----------------------------------------------------------------------------*/
/*! Save a series of floats into a dataset,
    either directly into the bricks, or to a temp file to be restored later. */

void save_series( int vv, THD_3dim_dataset *dset, int npt, float *var, FILE *fp )
{
   if( fp == NULL )
     THD_insert_series( vv , dset , npt , MRI_float , var , 0 ) ;
   else
     my_fwrite( var , sizeof(float) , npt , fp ) ;
}

/*--------------------------------------------------------------------------*/
/*! Check a dataset for float errors. */

void check_dataset( THD_3dim_dataset *dset )
{
   int nn ; static int first=1 ;
   nn = dset_floatscan( dset ) ;
   if( nn > 0 ){
     WARNING_message("Zero-ed %d float errors in dataset %s",DSET_BRIKNAME(dset));
     if( first ){
       ININFO_message(" Float errors include NaN and Infinity values") ;
       ININFO_message(" ==> Check matrix setup! Check inputs! Check results!") ;
       first = 0 ;
     }
   }
   return ;
}

/*----------------------------------------------------------------------------*/
/*! If fp!=NULL, load the dataset's data from that file (-usetemp).
    Otherwise, there's nothing to do, so vamoose the ranch.
    [19 Dec 2008] Also, check the dataset for float errors. */

void populate_dataset( THD_3dim_dataset *dset, byte *mask,
                                               char *fnam, FILE *fp  )
{
   int kk,vv=-1 , nvox , nvals , bb ;
   float *var ;

ENTRY("populate_dataset") ;

   if( !ISVALID_DSET(dset) || fp == NULL ){  /* fp != NULL means to */
     check_dataset(dset) ; EXRETURN ;        /* read data from file */
   }

   /* create empty bricks to receive data from file */

   nvals = DSET_NVALS(dset) ;
   for( kk=0 ; kk < nvals ; kk++ )
     EDIT_substitute_brick( dset , kk , MRI_float , NULL ) ;

   if( mask == NULL || fp == NULL ) EXRETURN ;  /* should not happen */

   if( verb > 1 && fnam != NULL ){
     INFO_message("Populating dataset %s from file %s",
                  DSET_FILECODE(dset) , fnam ) ;
     MEMORY_CHECK ;
   }

   var  = (float *)calloc(sizeof(float),nvals) ; /* space for 1 voxel's data */
   nvox = DSET_NVOX(dset) ;

   kk = fseek(fp,0,SEEK_SET) ;
   if( kk != 0 ) perror("rewind") ;

   /* load data from file */

   for( kk=0 ; kk < nvox ; kk++ ){
     if( !INMASK(kk) ) continue ;
     (void)fread( var , sizeof(float) , nvals , fp ) ;
     THD_insert_series( kk , dset , nvals , MRI_float , var , 0 ) ;
   }

   /* done */

   free(var) ; fclose(fp) ;
   if( fnam != NULL ){ kill_purge(fnam) ; remove(fnam) ; }
   check_dataset(dset) ; EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! This function creates a G matrix (for GLTs) that selects a subset
    of the full model;
     - mpar = number of parameters in the full model
            = number of columns in the output matrix
     - nrow = number of parameters of the subset model
            = number of rows in the output matrix
     - set  = array of indexes to select (nrow of them)
*//*------------------------------------------------------------------------*/

matrix * create_subset_matrix( int mpar , int nrow , int *set )
{
   matrix *gm ; int ii,jj ;

ENTRY("create_subset_matrix") ;

   if( mpar < 1 || nrow < 1 || nrow > mpar || set == NULL ) RETURN(NULL) ;

   gm = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(gm) ;
   matrix_create( nrow , mpar , gm ) ;
   for( ii=0 ; ii < nrow ; ii++ ){    /* zero out entire matrix */
     for( jj=0 ; jj < mpar ; jj++ ) gm->elts[ii][jj] = 0.0 ;
   }
   for( ii=0 ; ii < nrow ; ii++ ){
     if( set[ii] < 0 || set[ii] >= mpar ){ free(gm); RETURN(NULL); }
     for( jj=0 ; jj < ii ; jj++ )
       if( set[ii] == set[jj] ){ free(gm); RETURN(NULL); }
     gm->elts[ii][set[ii]] = 1.0 ;  /* ii-th row selects set[ii] param */
   }
   RETURN(gm) ;
}

/*--------------------------------------------------------------------------*/
/* struct for putting values from a GLT into a bucket dataset:
     nrow        = number of rows = number of beta and ttst values (each)
     ivbot       = first index in dataset to get a value from this GLT
     ivtop       = last index in dataset to get a value from this GLT
     beta_ind[i] = index in dataset for i-th beta value (i=0..nrow-1)
                   N.B.: beta_ind == NULL if beta values not to be saved
     beta_lab[i] = label for i-th beta value
     ttst_ind[i] = index in dataset for i-th ttst value
                   N.B.: ttst_ind == NULL if ttst values not to be saved
     ttst_lab[i] = label for i-th beta value
     ftst_ind    = index for ftst value (-1 if not to be saved)
     ftst_lab    = label for ftst value
     rtst_ind    = index for R^2 value (-1 if not to be saved)
     rtst_lab    = label for R^2 value
  Note that the 'built-in' GLTs for each stimulus (group of regressors) have
  as their matrices 0-1 callouts of the appropriate regression coefficients.
  So their GLT coefficients returned will just be the regression
  coefficients.
----------------------------------------------------------------------------*/

typedef struct {
  int nrow , ivbot , ivtop ;
  int   *beta_ind ,  *ttst_ind ,  ftst_ind ,  rtst_ind ;
  char **beta_lab , **ttst_lab , *ftst_lab , *rtst_lab ;
} GLT_index ;

/*-------------------------------------------------------------------------*/
/*! Create a struct for putting values from a GLT into a bucket dataset:
     * ivfirst = index of first sub-brick to get a value
     * nrow    = number of rows in the GLT
     * do_beta = whether to include beta values in the output
     * do_ttst = whether to include t-statistics in the output
     * do_ftst = whether to include F-statistic in the output
     * do_rtst = whether to include R^2 statistic in the output
     * name    = prefix name for this GLT (for sub-brick labels)
*//*-----------------------------------------------------------------------*/

GLT_index * create_GLT_index( int ivfirst , int nrow ,
                              int do_beta , int do_ttst ,
                              int do_ftst , int do_rtst , char *name )
{
   GLT_index *gin ; char lll[256] ; int ii,iv=ivfirst ;

ENTRY("create_GLT_index") ;

   if( !do_beta && !do_ttst && !do_ftst && !do_rtst ) RETURN(NULL) ; /* bad */
   if( ivfirst < 0 || nrow < 1 || name == NULL      ) RETURN(NULL) ;

   gin = (GLT_index *)calloc(1,sizeof(GLT_index)) ;

   gin->nrow = nrow ;

   if( do_beta ){
     gin->beta_ind = (int *  )calloc( sizeof(int)    , nrow ) ;
     gin->beta_lab = (char **)calloc( sizeof(char *) , nrow ) ;
   }
   if( do_ttst ){
     gin->ttst_ind = (int *  )calloc( sizeof(int)    , nrow ) ;
     gin->ttst_lab = (char **)calloc( sizeof(char *) , nrow ) ;
   }

   /* add Coef and Tstat entries, alternating if both are used */

   if( do_beta || do_ttst ){
     for( ii=0 ; ii < nrow ; ii++ ){
       if( do_beta ){
         gin->beta_ind[ii] = iv++ ;
         sprintf( lll , "%.32s#%d_Coef" , name , ii ) ;
         gin->beta_lab[ii] = strdup(lll) ;
       }
       if( do_ttst ){
         gin->ttst_ind[ii] = iv++ ;
         sprintf( lll , "%.32s#%d_Tstat" , name , ii ) ;
         gin->ttst_lab[ii] = strdup(lll) ;
       }
     }
   }

   if( do_rtst ){                  /* 23 Oct 2008 */
     gin->rtst_ind = iv++ ;
     sprintf( lll , "%.32s_R^2" , name ) ;
     gin->rtst_lab = strdup(lll) ;
   } else {
     gin->rtst_ind = -1 ;
     gin->rtst_lab = NULL ;
   }

   if( do_ftst ){
     gin->ftst_ind = iv++ ;
     sprintf( lll , "%.32s_Fstat" , name ) ;
     gin->ftst_lab = strdup(lll) ;
   } else {
     gin->ftst_ind = -1 ;
     gin->ftst_lab = NULL ;
   }

   gin->ivbot = ivfirst ; gin->ivtop = iv-1 ; RETURN(gin) ;
}

/*-------------------------------------------------------------------------*/

static int        nSymStim = 0    ;
static SYM_irange *SymStim = NULL ;

/*! Create a GLT matrix from the symbolic string. */

matrix * create_gltsym( char *sym , int ncol )
{
   int ii,jj , nr=0 , iv ;
   floatvecvec *fvv ;
   float **far=NULL ;
   char *str_echo=NULL ;
   matrix *cmat ;

ENTRY("create_gltsym") ;

   if( sym == NULL || ncol < 1 || nSymStim < 1 )
     ERROR_exit("Bad call to create_gltsym!") ;

   if( strncmp(sym,"SYM:",4) == 0 ){  /* read directly from sym string */

     char *fdup=strdup(sym+4) , *fpt , *buf ;
     int ss , ns ;
     buf = fdup ;
STATUS("SYM: gltsym") ;
     while(*buf != '\0'){
                         fpt = strchr(buf,'\\'); /* find end of 'line' */
       if( fpt == NULL ) fpt = strchr(buf,'|') ;
       if( fpt != NULL ) *fpt = '\0' ;           /* mark end of line with NUL */
STATUS(buf) ;
       fvv = SYM_expand_ranges( ncol-1 , nSymStim,SymStim , buf ) ;
       if( fvv == NULL || fvv->nvec < 1 ){ buf=fpt+1; continue; }  /* bad?  blank? */
       far = (float **)realloc((void *)far , sizeof(float *)*(nr+fvv->nvec)) ;
       for( iv=0 ; iv < fvv->nvec ; iv++ ) far[nr++] = fvv->fvar[iv].ar ;
       free((void *)fvv->fvar) ; free((void *)fvv) ;
       if( fpt == NULL ) break ;   /* reached end of string? */
       buf = fpt+1 ;               /* no, so loop back for next 'line' */
     }
     free((void *)fdup) ;

   } else {                             /* read from file */

     char buf[8192] , *cpt ;
     FILE *fp = fopen( sym , "r" ) ;
     if( fp == NULL ) ERROR_exit("Can't open GLT matrix file '%.33s'",sym) ;
     while(1){
       cpt = fgets( buf , 8192 , fp ) ;   /* read next line */
       if( cpt == NULL ) break ;          /* end of input? */
       str_echo = THD_zzprintf(str_echo," : %s",cpt) ;
       fvv = SYM_expand_ranges( ncol-1 , nSymStim,SymStim , buf ) ;
       if( fvv == NULL || fvv->nvec < 1 ) continue ;
       far = (float **)realloc((void *)far , sizeof(float *)*(nr+fvv->nvec)) ;
       for( iv=0 ; iv < fvv->nvec ; iv++ ) far[nr++] = fvv->fvar[iv].ar ;
       free((void *)fvv->fvar) ; free((void *)fvv) ;
     }
     fclose(fp) ;

   }

   if( nr == 0 ) ERROR_exit("Can't read GLT matrix from '%.33s'",sym) ;
   cmat = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(cmat) ;
   array_to_matrix( nr , ncol , far , cmat ) ;

   for( ii=0 ; ii < nr ; ii++ ) free((void *)far[ii]) ;
   free((void *)far) ;

   if( !AFNI_noenv("AFNI_GLTSYM_PRINT") ){
     printf("------------------------------------------------------------\n");
     printf("GLT matrix from '%.33s':\n",sym) ;
     if( str_echo != NULL ){ printf("%s",str_echo); free(str_echo); }
     matrix_print( *cmat ) ;
   }

   /** check for all zero rows, which will cause trouble later **/

   for( ii=0 ; ii < nr ; ii++ ){
     for( jj=0 ; jj < ncol && cmat->elts[ii][jj] == 0.0 ; jj++ ) ; /*nada*/
     if( jj == ncol )
       ERROR_message("Row #%d of GLT matrix '%.33s' is all zero!", ii+1 , sym ) ;
   }

   RETURN(cmat) ;
}

/*-------------------------------------------------------------------------*/


/*==========================================================================*/
/********************************** Main program ****************************/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   MRI_vectim   *inset_mrv=NULL ; /* 05 Nov 2008 */
   THD_3dim_dataset *abset=NULL ; int abfixed=0 ; float afix,bfix ;
   MRI_IMAGE *aim=NULL, *bim=NULL ; float *aar=NULL, *bar=NULL ;
   MRI_IMAGE *rim=NULL ;            float *rar=NULL ;
   byte *mask=NULL,*gmask=NULL ;
   int mask_nx=0,mask_ny=0,mask_nz=0, automask=0, nmask=0;
   float *iv , *jv ; int niv ;
   int iarg, ii,jj,kk, ntime,ddof, *tau=NULL, rnum, nfull, nvals,nvox,vv,rv ;
   NI_element *nelmat=NULL ; char *matname=NULL ;
   MTYPE rhomax=0.8 , bmax  =0.8 ; int nlevab=3 ;
   MTYPE rm_set=0.0 , bm_set=0.0 ;
   char *cgl , *rst ;
   matrix X ; vector y ;
   reml_collection *rrcol ;
   int nprefixO=0 , nprefixR=0 , vstep=0 ;

   char *Rbeta_prefix =NULL; THD_3dim_dataset *Rbeta_dset =NULL; FILE *Rbeta_fp =NULL; char *Rbeta_fn =NULL;
   char *Rvar_prefix  =NULL; THD_3dim_dataset *Rvar_dset  =NULL; FILE *Rvar_fp  =NULL; char *Rvar_fn  =NULL;
   char *Rfitts_prefix=NULL; THD_3dim_dataset *Rfitts_dset=NULL; FILE *Rfitts_fp=NULL; char *Rfitts_fn=NULL;
   char *Obeta_prefix =NULL; THD_3dim_dataset *Obeta_dset =NULL; FILE *Obeta_fp =NULL; char *Obeta_fn =NULL;
   char *Ovar_prefix  =NULL; THD_3dim_dataset *Ovar_dset  =NULL; FILE *Ovar_fp  =NULL; char *Ovar_fn  =NULL;
   char *Ofitts_prefix=NULL; THD_3dim_dataset *Ofitts_dset=NULL; FILE *Ofitts_fp=NULL; char *Ofitts_fn=NULL;

   char *Rbuckt_prefix=NULL; THD_3dim_dataset *Rbuckt_dset=NULL; FILE *Rbuckt_fp=NULL; char *Rbuckt_fn=NULL;
   char *Obuckt_prefix=NULL; THD_3dim_dataset *Obuckt_dset=NULL; FILE *Obuckt_fp=NULL; char *Obuckt_fn=NULL;
   int nbuckt=0 , do_buckt=0 ;
   int *betaset , nbetaset , nobout=0 ;  /* 25 Mar 2009 */

   char *Rerrts_prefix=NULL; THD_3dim_dataset *Rerrts_dset=NULL; FILE *Rerrts_fp=NULL; char *Rerrts_fn=NULL;
   char *Oerrts_prefix=NULL; THD_3dim_dataset *Oerrts_dset=NULL; FILE *Oerrts_fp=NULL; char *Oerrts_fn=NULL;
   char *Rwherr_prefix=NULL; THD_3dim_dataset *Rwherr_dset=NULL; FILE *Rwherr_fp=NULL; char *Rwherr_fn=NULL;

   char *Rglt_prefix  =NULL; THD_3dim_dataset *Rglt_dset  =NULL; FILE *Rglt_fp  =NULL; char *Rglt_fn  =NULL;
   char *Oglt_prefix  =NULL; THD_3dim_dataset *Oglt_dset  =NULL; FILE *Oglt_fp  =NULL; char *Oglt_fn  =NULL;
   int neglt=0 , do_eglt=0 ;

   int Ngoodlist,*goodlist=NULL , Nruns,*runs=NULL , izero ;
   NI_int_array *giar ; NI_str_array *gsar ; NI_float_array *gfar ;
   float mfilt_radius=0.0 , dx,dy,dz ; int do_mfilt=0 , do_dxyz , nx,ny,nz,nxy ;
   int do_Ostuff=0 , do_Rstuff=0 ;

   int glt_num=0, glt_rtot=0 , do_glt=1 ; matrix **glt_mat=NULL ; char **glt_lab=NULL ;
   sparmat **glt_smat=NULL ;
   GLT_index **glt_ind=NULL ;
   int stim_num=0; int *stim_bot=NULL , *stim_top=NULL ; char **stim_lab=NULL ;
   int do_fstat=0 , do_tstat=0 , do_stat=0 , do_rstat=0 ;
   int num_allstim=0, *allstim=NULL , num_basetim=0, *basetim=NULL ;

   char **beta_lab=NULL ;
   int do_FDR = 1 ;

   int usetemp_rcol=0 ;

   MRI_IMARR *imar_addbase=NULL ; int ncol_addbase=0 ;
   MRI_IMARR *imar_slibase=NULL ; int ncol_slibase=0 , nfile_slibase=0 ;
   int nrega,nrego , nbad , ss,ssold , dmbase=1 ;
   int               nsli , nsliper ;
   matrix          **Xsli =NULL ;
   reml_collection **RCsli=NULL ;

   int    eglt_num = 0    ;   /* extra GLTs from the command line */
   char **eglt_lab = NULL ;
   char **eglt_sym = NULL ;
   int    oglt_num = 0    ;   /* number of 'original' GLTs */

   /**------- Get by with a little help from your friends? -------**/

   Argc = argc ; Argv = argv ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dREMLfit [options]\n"
      "\n"
      "Least squares time series fit, with REML estimation of the\n"
      "temporal auto-correlation structure.\n"
      "\n"
      "* This program provides a generalization of 3dDeconvolve:\n"
      "    it allows for serial correlation in the time series noise.\n"
      "* It solves the linear equations for each voxel in the generalized\n"
      "    (prewhitened) least squares sense, using the REML estimation method\n"
      "    to find a best-fit ARMA(1,1) model for the time series noise\n"
      "    correlation matrix in each voxel.\n"
      "* You MUST run 3dDeconvolve first to generate the input matrix\n"
      "    (.xmat.1D) file, which contains the hemodynamic regression\n"
      "    model, censoring and catenation information, the GLTs, etc.\n"
      "    See the output of '3dDeconvolve -help' for information on\n"
      "    using that program to setup the analysis.\n"
      "* If you don't want the 3dDeconvolve analysis to run, you can\n"
      "    prevent that by using 3dDeconvolve's '-x1D_stop' option.\n"
      "* 3dDeconvolve also prints out a cognate command line for running\n"
      "    3dREMLfit, which should get you going with relative ease.\n"
      "* The output datasets from 3dREMLfit are structured to resemble\n"
      "    the corresponding results from 3dDeconvolve, to make it\n"
      "    easy to adapt your scripts for further processing.\n"
      "* Is this type of analysis important?\n"
      "    That depends on your point of view, your data, and your goals.\n"
      "    If you really want to know the answer, you should run\n"
      "    your analyses both ways (with 3dDeconvolve and 3dREMLfit),\n"
      "    through to the final step (e.g., group analysis), and then\n"
      "    decide if your neuroscience/brain conclusions depend strongly\n"
      "    on the type of linear regression that was used.\n"
      "\n"
      "-------------------------------------------\n"
      "Input Options (the first two are mandatory)\n"
      "-------------------------------------------\n"
      " -input ddd  = Read time series dataset 'ddd'.\n"
      "\n"
      " -matrix mmm = Read the matrix 'mmm', which should have been\n"
      "                 output from 3dDeconvolve via the '-x1D' option.\n"
      "            *** N.B.: 3dREMLfit will NOT work with all zero columns,\n"
      "                       unlike 3dDeconvolve!!!\n"
      "             ** N.B.: Actually, you can omit the '-matrix' option, but\n"
      "                       then the program will fabricate a matrix consisting\n"
      "                       of a single column with all 1s.  This option is\n"
      "                       mostly for the convenience of the author; for\n"
      "                       example, to have some fun with an AR(1) time series:\n"
      "                 1deval -num 1001 -expr 'gran(0,1)+(i-i)+0.7*z' > g07.1D\n"
      "                 3dREMLfit -input g07.1D'{1..$}'\' -Rvar -.1D -grid 5 -MAXa 0.9\n"
      "\n"
      " -mask kkk   = Read dataset 'kkk' as a mask for the input.\n"
      " -automask   = If you don't know what this does by now, I'm not telling.\n"
      "            *** If you don't specify ANY mask, the program will\n"
      "                 build one automatically (from each voxel's RMS)\n"
      "                 and use this mask solely for the purpose of\n"
      "                 computing the FDR curves in the bucket dataset's header.\n"
      "              * If you DON'T want this to happen, then use '-noFDR'\n"
      "                 and later run '3drefit -addFDR' on the bucket dataset.\n"
      "              * To be precise, the FDR automask is only built if\n"
      "                 the input dataset has at least 5 voxels along each of\n"
      "                 the x and y axes, to avoid applying it when you run\n"
      "                 3dREMLfit on 1D timeseries inputs.\n"
      "\n"
      "--------------------------------------------------------------------------\n"
      "Options to Add Baseline (Null Hypothesis) Columns to the Regression Matrix\n"
      "--------------------------------------------------------------------------\n"
      " -addbase bb = You can add baseline model columns to the matrix with\n"
      "                 this option.  Each column in the .1D file 'bb' will\n"
      "                 be appended to the matrix.  This file must have at\n"
      "                 least as many rows as the matrix does.\n"
      "              * Multiple -addbase options can be used, if needed.\n"
      "              * More than 1 file can be specified, as in\n"
      "                  -addbase fred.1D ethel.1D elvis.1D\n"
      "              * No .1D filename can start with the '-' character.\n"
      "              * If the matrix from 3dDeconvolve was censored, then\n"
      "                  this file (and '-slibase' files) can either be\n"
      "                  censored to match, OR 3dREMLfit will censor these\n"
      "                  .1D files for you.\n"
      "               + If the column length (number of rows) of the .1D file\n"
      "                   is the same as the column length of the censored\n"
      "                   matrix, then the .1D file WILL NOT be censored.\n"
      "               + If the column length of the .1D file is the same\n"
      "                   as the column length of the uncensored matrix,\n"
      "                   then the .1D file WILL be censored -- the same\n"
      "                   rows excised from the matrix in 3dDeconvolve will\n"
      "                   be resected from the .1D file before the .1D file's\n"
      "                   columns are appended to the matrix.\n"
      "               + The censoring information from 3dDeconvolve is stored\n"
      "                   in the matrix file header, and you don't have to\n"
      "                   provide it again on the 3dREMLfit command line!\n"
      "\n"
      " -slibase bb = Similar to -addbase in concept, BUT each .1D file 'bb'\n"
      "                 must have an integer multiple of the number of slices\n"
      "                 in the input dataset; then, separate regression\n"
      "                 matrices are generated for each slice, with the\n"
      "                 [0] column of 'bb' appended to the matrix for\n"
      "                 the #0 slice of the dataset, the [1] column of 'bb'\n"
      "                 appended to the matrix for the #1 slice of the dataset,\n"
      "                 and so on.  For example, if the dataset has 3 slices\n"
      "                 and file 'bb' has 6 columns, then the order of use is\n"
      "                     bb[0] --> slice #0 matrix\n"
      "                     bb[1] --> slice #1 matrix\n"
      "                     bb[2] --> slice #2 matrix\n"
      "                     bb[3] --> slice #0 matrix\n"
      "                     bb[4] --> slice #1 matrix\n"
      "                     bb[5] --> slice #2 matrix\n"
      "\n"
      "              * If this order is not correct, consider -slibase_sm.\n"
      "\n"
      "              * Intended to help model physiological noise in FMRI,\n"
      "                 or other effects you want to regress out that might\n"
      "                 change significantly in the inter-slice time intervals.\n"
      "              * Slices are the 3rd dimension in the dataset storage\n"
      "                 order -- 3dinfo can tell you what that direction is:\n"
      "                   Data Axes Orientation:\n"
      "                     first  (x) = Right-to-Left\n"
      "                     second (y) = Anterior-to-Posterior\n"
      "                     third  (z) = Inferior-to-Superior   [-orient RAI]\n"
      "                 In the above example, the slice direction is from\n"
      "                 Inferior to Superior, so the columns in the '-slibase'\n"
      "                 input file should be ordered in that direction as well.\n"
      "              * Will slow the program down, and make it use a\n"
      "                  lot more memory (to hold all the matrix stuff).\n"
      "            *** At this time, 3dSynthesize has no way of incorporating\n"
      "                  the extra baseline timeseries from -addbase or -slibase.\n"
      "\n"
      " -slibase_sm bb = Similar to -slibase above, BUT each .1D file 'bb'\n"
      "                 must be in slice major order (i.e. all slice0 columns\n"
      "                 come first, then all slice1 columns, etc).\n"
      "                 For example, if the dataset has 3 slices and file\n"
      "                 'bb' has 6 columns, then the order of use is\n"
      "                     bb[0] --> slice #0 matrix, regressor 0\n"
      "                     bb[1] --> slice #0 matrix, regressor 1\n"
      "                     bb[2] --> slice #1 matrix, regressor 0\n"
      "                     bb[3] --> slice #1 matrix, regressor 1\n"
      "                     bb[4] --> slice #2 matrix, regressor 0\n"
      "                     bb[5] --> slice #2 matrix, regressor 1\n"
      "\n"
      "              * If this order is not correct, consider -slibase.\n"
      "\n"
      " -usetemp    = Write intermediate stuff to disk, to economize on RAM.\n"
      "                 Using this option might be necessary to run with\n"
      "                 '-slibase' and with '-Grid' values above the default,\n"
      "                 since the program has to store a large number of\n"
      "                 matrices for such a problem: two for every slice and\n"
      "                 for every (a,b) pair in the ARMA parameter grid.\n"
      "              * '-usetemp' can actually speed the program up, interestingly,\n"
      "                   even if you have enough RAM to hold all the intermediate\n"
      "                   matrices needed with '-slibase'.  YMMV.\n"
      "              * '-usetemp' also writes temporary files to store dataset\n"
      "                   results, which can help if you are creating multiple large\n"
      "                   dataset (e.g., -Rfitts and -Rerrts in the same program run).\n"
      "              * Temporary files are written to the directory given\n"
      "                  in environment variable TMPDIR, or in /tmp, or in ./\n"
      "                  (preference is in that order).\n"
      "                 + If the program crashes, these files are named\n"
      "                     REML_somethingrandom, and you might have to\n"
      "                     delete them manually.\n"
      "                 + If the program ends normally, it will delete\n"
      "                     these temporary files before it exits.\n"
      "                 + Several gigabytes of disk space might be used\n"
      "                     for this temporary storage!\n"
      "              * If the program crashes with a 'malloc failure' type of\n"
      "                  message, then try '-usetemp' (malloc=memory allocator).\n"
#ifdef USING_MCW_MALLOC
      "              * If you use '-verb', then memory usage is printed out\n"
      "                  at various points along the way.\n"
#endif
#ifdef USE_OMP
      "              * '-usetemp' disables OpenMP multi-CPU usage.\n"
      "                  Only use this option if you need to, since OpenMP should\n"
      "                  speed the program up significantly on multi-CPU computers.\n"
#endif
      "\n"
      " -nodmbase   = By default, baseline columns added to the matrix\n"
      "                 via '-addbase' or '-slibase' will each have their\n"
      "                 mean removed (as is done in 3dDeconvolve).  If you\n"
      "                 do NOT want this operation performed, use '-nodmbase'.\n"
      "              * Using '-nodmbase' would make sense if you used\n"
      "                 '-polort -1' to set up the matrix in 3dDeconvolve, and/or\n"
      "                 you actually care about the fit coefficients of the extra\n"
      "                 baseline columns (in which case, don't use '-nobout').\n"
      "\n"
      "------------------------------------------------------------------------\n"
      "Output Options (at least one must be given; 'ppp' = dataset prefix name)\n"
      "------------------------------------------------------------------------\n"
      " -Rvar  ppp  = dataset for REML variance parameters\n"
      " -Rbeta ppp  = dataset for beta weights from the REML estimation\n"
      "                 [similar to the -cbucket output from 3dDeconvolve]\n"
      " -Rbuck ppp  = dataset for beta + statistics from the REML estimation;\n"
      "                 also contains the results of any GLT analysis requested\n"
      "                 in the 3dDeconvolve setup.\n"
      "                 [similar to the -bucket output from 3dDeconvolve]\n"
      "               * If the matrix file from 3dDeconvolve does not contain\n"
      "                 'Stim attributes' (which will happen if all inputs\n"
      "                 to 3dDeconvolve were labeled as '-stim_base'), then\n"
      "                 -Rbuck won't work, since it is designed to give the\n"
      "                 statistics for the 'stimuli' and there aren't any matrix\n"
      "                 columns labeled as being 'stimuli'.\n"
      "               * In such a case, to get statistics on the coefficients,\n"
      "                 you'll have to use '-gltsym' and '-Rglt'; for example,\n"
      "                 to get t-statistics for all coefficients from #0 to #37:\n"
      "                    -tout -Rglt Colstats -gltsym 'Col[[0..37]]' Col\n"
      " -Rglt  ppp  = dataset for beta + statistics from the REML estimation,\n"
      "                 but ONLY for the GLTs added on the 3dREMLfit command\n"
      "                 line itself via '-gltsym'; GLTs from 3dDeconvolve's\n"
      "                 command line will NOT be included.\n"
      "               * Intended to give an easy way to get extra contrasts\n"
      "                   after an earlier 3dREMLfit run.\n"
      "               * Use with '-ABfile vvv' to read the (a,b) parameters\n"
      "                   from the earlier run, where 'vvv' is the '-Rvar'\n"
      "                   dataset output from that run.\n"
      "                   [If you didn't save the '-Rvar' file, then it will]\n"
      "                   [be necessary to redo the REML loop, which is slow]\n"
      "\n"
      " -fout       = put F-statistics into the bucket dataset\n"
      " -rout       = put R^2 statistics into the bucket dataset\n"
      " -tout       = put t-statistics into the bucket dataset\n"
      "                 [if you use -Rbuck and do not give any of -fout, -tout,]\n"
      "                 [or -rout, then the program assumes -fout is activated.]\n"
      " -noFDR      = do NOT add FDR curve data to bucket datasets\n"
      "                 [FDR curves can take a long time if -tout is used]\n"
      "\n"
      " -nobout     = do NOT add baseline (null hypothesis) regressor betas\n"
      "                 to the -Rbeta and/or -Obeta output datasets.\n"
      "                 ['stimulus' columns are marked in the .xmat.1D matrix ]\n"
      "                 [file; all other matrix columns are 'baseline' columns]\n"
      "\n"
      " -Rfitts ppp = dataset for REML fitted model\n"
      "                 [like 3dDeconvolve, a censored time point gets]\n"
      "                 [the actual data values from that time index!!]\n"
      "\n"
      " -Rerrts ppp = dataset for REML residuals = data - fitted model\n"
      "                 [like 3dDeconvolve,  a censored time]\n"
      "                 [point gets its residual set to zero]\n"
      " -Rwherr ppp = dataset for REML residual, whitened using the\n"
      "                 estimated ARMA(1,1) correlation matrix of the noise\n"
      "                 [Note that the whitening matrix used is the inverse  ]\n"
      "                 [of the Choleski factor of the correlation matrix C; ]\n"
      "                 [however, the whitening matrix isn't uniquely defined]\n"
      "                 [(any matrix W with C=inv(W'W) will work), so other  ]\n"
      "                 [whitening schemes could be used and these would give]\n"
      "                 [different whitened residual time series datasets.   ]\n"
      "\n"
      " -gltsym g h = read a symbolic GLT from file 'g' and label it with\n"
      "                 string 'h'\n"
      "                * As in 3dDeconvolve, you can also use the 'SYM:' method\n"
      "                    to put the definition of the GLT directly on the\n"
      "                    command line.\n"
      "                * The symbolic labels for the stimuli are as provided\n"
      "                    in the matrix file, from 3dDeconvolve.\n"
      "              *** Unlike 3dDeconvolve, you supply the label 'h' for\n"
      "                    the output coefficients and statistics directly\n"
      "                    after the matrix specification 'g'.\n"
      "                * Like 3dDeconvolve, the matrix generated by the\n"
      "                    symbolic expression will be printed to the screen\n"
      "                    unless environment variable AFNI_GLTSYM_PRINT is NO.\n"
      "                * These GLTs are in addition to those stored in the\n"
      "                    matrix file, from 3dDeconvolve.\n"
      "                * If you don't create a bucket dataset using one of\n"
      "                    -Rbuck or -Rglt (or -Obuck / -Oglt), using\n"
      "                    -gltsym is completely pointless!\n"
      "               ** Besides the stimulus labels read from the matrix\n"
      "                    file (put there by 3dDeconvolve), you can refer\n"
      "                    to regressor columns in the matrix using the\n"
      "                    symbolic name 'Col', which collectively means\n"
      "                    all the columns in the matrix.  'Col' is a way\n"
      "                    to test '-addbase' and/or '-slibase' regressors\n"
      "                    for significance; for example, if you have a\n"
      "                    matrix with 10 columns from 3dDeconvolve and\n"
      "                    add 2 extra columns to it, then you could use\n"
      "                      -gltsym 'SYM: Col[[10..11]]' Addons -tout -fout\n"
      "                    to create a GLT to include both of the added\n"
      "                    columns (numbers 10 and 11).\n"
      "\n"
      "The options below let you get the Ordinary Least SQuares outputs\n"
      "(without adjustment for serial correlation), for comparisons.\n"
      "These datasets should be essentially identical to the results\n"
      "you would get by running 3dDeconvolve (with the '-float' option!):\n"
      "\n"
      " -Ovar   ppp = dataset for OLSQ st.dev. parameter (kind of boring)\n"
      " -Obeta  ppp = dataset for beta weights from the OLSQ estimation\n"
      " -Obuck  ppp = dataset for beta + statistics from the OLSQ estimation\n"
      " -Oglt   ppp = dataset for beta + statistics from '-gltsym' options\n"
      " -Ofitts ppp = dataset for OLSQ fitted model\n"
      " -Oerrts ppp = dataset for OLSQ residuals (data - fitted model)\n"
      "                 [there is no -Owherr option; if you don't]\n"
      "                 [see why, then think about it for a while]\n"
      "\n"
      "Note that you don't have to use any of the '-R' options; you could\n"
      "use 3dREMLfit just for the '-O' options if you want.  In that case,\n"
      "the program will skip the time consuming ARMA(1,1) estimation for\n"
      "each voxel, by pretending you used the option '-ABfile =0,0'.\n"
      "\n"
      "-------------------------------------------------------------------\n"
      "The following options control the ARMA(1,1) parameter estimation\n"
      "for each voxel time series; normally, you do not need these options\n"
      "-------------------------------------------------------------------\n"
      " -MAXa am   = Set max allowed AR a parameter to 'am' (default=0.8).\n"
      "                The range of a values scanned is   0 .. +am (-POScor)\n"
      "                                           or is -am .. +am (-NEGcor).\n"
      "\n"
      " -MAXb bm   = Set max allow MA b parameter to 'bm' (default=0.8).\n"
      "                The range of b values scanned is -bm .. +bm.\n"
      "               * The largest value allowed for am and bm is 0.9.\n"
      "               * The smallest value allowed for am and bm is 0.1.\n"
      "               * For a nearly pure AR(1) model, use '-MAXb 0.1'\n"
      "               * For a nearly pure MA(1) model, use '-MAXa 0.1'\n"
      "\n"
      " -Grid pp   = Set the number of grid divisions in the (a,b) grid\n"
      "                to be 2^pp in each direction over the range 0..MAX.\n"
      "                The default (and minimum) value for 'pp' is 3.\n"
      "                Larger values will provide a finer resolution\n"
      "                in a and b, but at the cost of some CPU time.\n"
      "               * To be clear, the default settings use a grid\n"
      "                   with 8 divisions in the a direction and 16 in\n"
      "                   the b direction (since a is non-negative but\n"
      "                   b can be either sign).\n"
      "               * If -NEGcor is used, then '-Grid 3' means 16 divisions\n"
      "                   in each direction, so that the grid spacing is 0.1\n"
      "                   if MAX=0.8.  Similarly, '-Grid 4' means 32 divisions\n"
      "                   in each direction, '-Grid 5' means 64 divisions, etc.\n"
      "               * I see no reason why you would ever use a -Grid size\n"
      "                   greater than 5 (==> parameter resolution = 0.025).\n"
      "               * In my limited experiments, there was little appreciable\n"
      "                   difference in activation maps between '-Grid 3' and\n"
      "                   '-Grid 5', especially at the group analysis level.\n"
      "               * The program is somewhat slower as the -Grid size expands.\n"
      "                   And uses more memory, to hold various matrices for\n"
      "                   each (a,b) case.\n"
      "\n"
      " -NEGcor    = Allows negative correlations to be used; the default\n"
      "                is that only positive correlations are searched.\n"
      "                When this option is used, the range of a scanned\n"
      "                is -am .. +am; otherwise, it is 0 .. +am.\n"
      "               * Note that when -NEGcor is used, the number of grid\n"
      "                   points in the a direction doubles to cover the\n"
      "                   range -am .. 0; this will slow the program down.\n"
      " -POScor    = Do not allow negative correlations.  Since this is\n"
      "                the default, you don't actually need this option.\n"
      "                [FMRI data doesn't seem to need the modeling  ]\n"
      "                [of negative correlations, but you never know.]\n"
      "\n"
      " -Mfilt mr  = After finding the best fit parameters for each voxel\n"
      "                in the mask, do a 3D median filter to smooth these\n"
      "                parameters over a ball with radius 'mr' mm, and then\n"
      "                use THOSE parameters to compute the final output.\n"
      "               * If mr < 0, -mr is the ball radius in voxels,\n"
      "                   instead of millimeters.\n"
      "                [No median filtering is done unless -Mfilt is used.]\n"
      "\n"
      " -CORcut cc = The exact ARMA(1,1) correlation matrix (for a != 0)\n"
      "                has no non-zero entries.  The calculations in this\n"
      "                program set correlations below a cutoff to zero.\n"
      "                The default cutoff is %.3f, but can be altered with\n"
      "                this option.  The usual reason to use this option is\n"
      "                to test the sensitivity of the results to the cutoff.\n"
      "\n"
      " -ABfile ff = Instead of estimating the ARMA(a,b) parameters from the\n"
      "                data, read them from dataset 'ff', which should have\n"
      "                2 float-valued sub-bricks.\n"
      "               * Note that the (a,b) values read from this file will\n"
      "                   be mapped to the nearest ones on the (a,b) grid\n"
      "                   before being used to solve the generalized least\n"
      "                   squares problem.  For this reason, you may want\n"
      "                   to use '-Grid 5' to make the (a,b) grid finer, if\n"
      "                   you are not using (a,b) values from a -Rvar file.\n"
      "               * Using this option will skip the slowest part of\n"
      "                   the program, which is the scan for each voxel\n"
      "                   to find its optimal (a,b) parameters.\n"
      "               * One possible application of -ABfile:\n"
      "                  + save (a,b) using -Rvar in 3dREMLfit\n"
      "                  + process them in some way (spatial smoothing?)\n"
      "                  + use these modified values for fitting in 3dREMLfit\n"
      "                      [you should use '-Grid 5' for such a case]\n"
      "               * Another possible application of -ABfile:\n"
      "                  + use (a,b) from -Rvar to speed up a run with -Rglt\n"
      "                      when you want to run some more contrast tests.\n"
      "               * Special case:\n"
      "                     -ABfile =0.7,-0.3\n"
      "                   e.g., means to use a=0.7 and b=-0.3 for all voxels.\n"
      "                   The program detects this special case by looking for\n"
      "                   '=' as the first character of the string 'ff' and\n"
      "                   looking for a comma in the middle of the string.\n"
      "                   The values of a and b must be in the range -0.9..+0.9.\n"
      "\n"
      " -GOFORIT   = 3dREMLfit checks the regression matrix for tiny singular\n"
      "                values (as 3dDeconvolve does).  If the matrix is too\n"
      "                close to being rank-deficient, then the program will\n"
      "                not proceed.  You can use this option to force the\n"
      "                program to continue past such a failed collinearity\n"
      "                check, but you must check your results to see if they\n"
      "                make sense!\n"
      "\n"
      "---------------------\n"
      "Miscellaneous Options\n"
      "---------------------\n"
      " -quiet = turn off most progress messages\n"
      " -verb  = turn on more progress messages\n"
#ifdef USING_MCW_MALLOC
      "            [including memory usage reports at various stages]\n"
#endif
      "\n"
      "==========================================================================\n"
      "===========  Various Notes (as if this help weren't long enough) =========\n"
      "==========================================================================\n"
      "\n"
      "------------------\n"
      "What is ARMA(1,1)?\n"
      "------------------\n"
      "* The correlation coefficient r(k) of noise samples k units apart in time,\n"
      "    for k >= 1, is given by r(k) = lam * a^(k-1)\n"
      "    where                   lam  = (b+a)(1+a*b)/(1+2*a*b+b*b)\n"
      "    (N.B.: lam=a when b=0 -- AR(1) noise has r(k)=a^k for k >= 0)\n"
      "    (N.B.: lam=b when a=0 -- MA(1) noise has r(k)=b for k=1, r(k)=0 for k>1)\n"
      "* lam can be bigger or smaller than a, depending on the sign of b:\n"
      "    b > 0 means lam > a;  b < 0 means lam < a.\n"
      "* What I call (a,b) here is sometimes called (p,q) in the ARMA literature.\n"
      "* For a noise model which is the sum of AR(1) and white noise, 0 < lam < a\n"
      "    (i.e., a > 0  and  -a < b < 0 ).\n"
      "* The natural range of a and b is -1..+1.  However, unless -NEGcor is\n"
      "    given, only non-negative values of a will be used, and only values\n"
      "    of b that give lam > 0 will be allowed.  Also, the program doesn't\n"
      "    allow values of a or b to be outside the range -0.9..+0.9.\n"
      "* The program sets up the correlation matrix using the censoring and run\n"
      "    start information saved in the header of the .xmat.1D matrix file, so\n"
      "    that the actual correlation matrix used will not always be Toeplitz.\n"
      "* The 'Rvar' dataset has 4 sub-bricks with variance parameter estimates:\n"
      "    #0 = a = factor by which correlations decay from lag k to lag k+1\n"
      "    #1 = b parameter\n"
      "    #2 = lam (see the formula above) = correlation at lag 1\n"
      "    #3 = standard deviation of ARMA(1,1) noise in that voxel\n"
      "* The 'Rbeta' dataset has the beta (model fit) parameters estimates\n"
      "    computed from the pre-whitened time series data in each voxel,\n"
      "    as in 3dDeconvolve's '-cbucket' output, in the order in which\n"
      "    they occur in the matrix.  -addbase and -slibase beta values\n"
      "    come last in this file.\n"
       "   [The '-nobout' option will disable output of baseline parameters.]\n"
      "* The 'Rbuck' dataset has the beta parameters and their statistics\n"
      "    mixed together, as in 3dDeconvolve's '-bucket' output.\n"
      "\n"
      "-------------------------------------------------------------------\n"
      "What is REML = REsidual (or REstricted) Maximum Likelihood, anyway?\n"
      "-------------------------------------------------------------------\n"
      "* Ordinary Least SQuares (which assumes the noise correlation matrix is\n"
      "    the identity) is consistent for estimating regression parameters,\n"
      "    but is not consistent for estimating the noise variance if the\n"
      "    noise is significantly correlated in time ('serial correlation').\n"
      "* Maximum likelihood estimation (ML) of the regression parameters and\n"
      "    variance/correlation together is asymptotically consistent as the\n"
      "    number of samples goes to infinity, but the variance estimates\n"
      "    might still have significant bias at a 'reasonable' number of\n"
      "    data points.\n"
      "* REML estimates the variance/correlation parameters in a space\n"
      "    of residuals -- the part of the data left after the model fit\n"
      "    is subtracted.  The amusing/cunning part is that the model fit\n"
      "    used to define the residuals is itself the generalized least\n"
      "    squares fit where the variance/correlation matrix is the one found\n"
      "    by the REML fit itself.  This feature makes REML estimation nonlinear,\n"
      "    and the REML equations are usually solved iteratively, to maximize\n"
      "    the log-likelihood in the restricted space.  In this program, the\n"
      "    REML function is instead simply optimized over a finite grid of\n"
      "    the correlation matrix parameters a and b.  The matrices for each\n"
      "    (a,b) pair are pre-calculated in the setup phase, and then are\n"
      "    re-used in the voxel loop.  The purpose of this grid-based method\n"
      "    is speed -- optimizing iteratively to a highly accurate (a,b)\n"
      "    estimation for each voxel would be very time consuming, and pretty\n"
      "    pointless.  If you are concerned about the sensitivity of the\n"
      "    results to the resolution of the (a,b) grid, you can use the\n"
      "    '-Grid 5' option to increase this resolution and see if your\n"
      "    activation maps change significantly.  In test cases, the resulting\n"
      "    betas and statistics have not changed appreciably between '-Grid 3'\n"
      "    and '-Grid 5'; however, you might want to test this on your own data\n"
      "    (just for fun).\n"
      "* REML estimates of the variance/correlation parameters are still\n"
      "    biased, but are generally significantly less biased than ML estimates.\n"
      "    Also, the regression parameters (betas) should be estimated somewhat\n"
      "    more accurately (i.e., with smaller variance than OLSQ).  However,\n"
      "    this effect is generally small in FMRI data, and probably won't affect\n"
      "    your group results noticeably (if you don't carry parameter variance\n"
      "    estimates to the inter-subject analysis).\n"
      "* After the (a,b) parameters are estimated, then the solution to the\n"
      "    linear system is available via Generalized Least SQuares; that is,\n"
      "    via pre-whitening using the Choleski factor of the estimated\n"
      "    variance/covariance matrix.\n"
      "* In the case with b=0 (that is, AR(1) correlations), and if there are\n"
      "    no time gaps (no censoring, no run breaks), then it is possible to\n"
      "    directly estimate the a parameter without using REML.  This program\n"
      "    does not implement such a method (e.g., the Yule-Walker equation).\n"
      "    The reason why should be obvious.\n"
      "\n"
      "----------------\n"
      "Other Commentary\n"
      "----------------\n"
      "* Again: the ARMA(1,1) parameters 'a' (AR) and 'b' (MA) are estimated\n"
      "    only on a discrete grid, for the sake of CPU time.\n"
      "* Each voxel gets a separate pair of 'a' and 'b' parameters.\n"
      "    There is no option to estimate global values for 'a' and 'b'\n"
      "    and use those for all voxels.  Such an approach might be called\n"
      "    'elementary school statistics' by some people.\n"
      "* OLSQ = Ordinary Least SQuares; these outputs can be used to compare\n"
      "         the REML/GLSQ estimations with the simpler OLSQ results\n"
      "         (and to test this program vs. 3dDeconvolve).\n"
      "* GLSQ = Generalized Least SQuares = estimated linear system solution\n"
      "         taking into account the variance/covariance matrix of the noise.\n"
      "* The '-matrix' file must be from 3dDeconvolve; besides the regression\n"
      "    matrix itself, the header contains the stimulus labels, the GLTs,\n"
      "    the censoring information, etc.\n"
      "* If you don't actually want the OLSQ results from 3dDeconvolve, you can\n"
      "    make that program stop after the X matrix file is written out by using\n"
      "    the '-x1D_stop' option, and then running 3dREMLfit; something like this:\n"
      "      3dDeconvolve -bucket Fred -nodata 800 2.5 -x1D_stop ...\n"
      "      3dREMLfit -matrix Fred.xmat.1D -input ...\n"
      "    In the above example, no 3D dataset is input to 3dDeconvolve, so as to\n"
      "    avoid the overhead of having to read it in for no reason.  Instead,\n"
      "    the '-nodata 800 2.5' option is used to setup the time series of the\n"
      "    desired length (corresponding to the real data's length, here 800 points),\n"
      "    and the appropriate TR (here, 2.5 seconds).  This will properly establish\n"
      "    the size and timing of the matrix file.\n"
      "* The bucket output datasets are structured to mirror the output\n"
      "    from 3dDeconvolve with the default options below:\n"
      "      -nobout -full_first\n"
      "    Note that you CANNOT use options like '-bout', '-nocout', and\n"
      "    '-nofull_first' with 3dREMLfit -- the bucket datasets are ordered\n"
      "    the way they are and you'll just have to live with it.\n"
      "* If the 3dDeconvolve matrix generation step did NOT have any non-base\n"
      "    stimuli (i.e., everything was '-stim_base'), then there are no 'stimuli'\n"
      "    in the matrix file.  In that case, since by default 3dREMLfit doesn't\n"
      "    compute statistics of baseline parameters, to get statistics you will\n"
      "    have to use the '-gltsym' option here, specifying the desired column\n"
      "    indexes with the 'Col[]' notation, and then use '-Rglt' to get these\n"
      "    values saved somewhere (since '-Rbuck' won't work if there are no\n"
      "    'Stim attributes').\n"
      " ++ Example to get t-statistics for all coefficients from #0 to #37:\n"
      "      -tout -Rglt Colstats -gltsym 'Col[[0..37]]' Col\n"
      "* All output datasets are in float format [i.e., no '-short' option].\n"
      "    Internal calculations are done in double precision.\n"
      "* If the regression matrix (including any added columns from '-addbase'\n"
      "    or '-slibase') is rank-deficient (e.g., has collinear columns),\n"
      "    then the program will print a message something like\n"
      "      ** ERROR: X matrix has 1 tiny singular value -- collinearity\n"
#ifndef PATCHX
      "    At this time, the program will NOT continue past this type of error.\n"
#else
      "    The program will still produce a solution.  However, since\n"
      "    3dREMLfit uses a different linear algebra approach than\n"
      "    3dDeconvolve for solving the equations, the solution for the\n"
      "    beta-weights involved in the collinearity will probably differ\n"
      "    from what 3dDeconvolve would produce in such a situation.\n"
      "    If you receive such a warning, you should examine your results\n"
      "    carefully to make sure they are reasonable (e.g., look at\n"
      "    the fitted model overlay on the input time series).\n"
#endif
      "* Despite my best efforts, this program is somewhat slow.\n"
      "    Partly because it solves many linear systems for each voxel,\n"
      "    trying to find the 'best' ARMA(1,1) pre-whitening matrix.\n"
      "    However, a careful choice of algorithms for solving the linear\n"
      "    systems (QR method, sparse matrix operations, etc.) and some\n"
      "    other code optimizations should make running 3dREMLfit tolerable.\n"
      "    Depending on the matrix and the options, you might expect CPU time\n"
      "    to be about 1..3 times that of the corresponding 3dDeconvolve run.\n"
      "    (Slower than that if you use '-slibase' and/or '-Grid 5', however.)\n"
      "\n"
      "-----------------------------------------------------------\n"
      "To Dream the Impossible Dream, to Write the Uncodeable Code\n"
      "-----------------------------------------------------------\n"
      "* Add a -jobs option to use multiple CPUs (or multiple Steves?).\n"
      "* Add options for -iresp/-sresp for -stim_times?\n"
      "* Output variance estimates for the betas, to be carried to the\n"
      "    inter-subject (group) analysis level?\n"
      "* Prevent Daniel Glen from referring to this program as 3dARMAgeddon.\n"
      "* Establish incontrovertibly the nature of quantum mechanical observation!\n"
      "\n"
      "----------------------------------------------------------\n"
      "* For more information, see the contents of\n"
      "    http://afni.nimh.nih.gov/pub/dist/doc/misc/3dREMLfit/\n"
      "  which includes comparisons of 3dDeconvolve and 3dREMLfit\n"
      "  activations (individual subject and group maps), and an\n"
      "  outline of the mathematics implemented in this program.\n"
      "----------------------------------------------------------\n"
      "\n"
      "============================\n"
      "== RWCox - July-Sept 2008 ==\n"
      "============================\n"

      , corcut
     ) ;
     PRINT_AFNI_OMP_USAGE("3dREMLfit",
         "* The REML matrix setup and REML voxel ARMA(1,1) estimation loops are\n"
         "   parallelized.\n"
         "* The GLSQ and OLSQ loops are not parallelized. They are usually much\n"
         "   faster than the REML voxel loop, and so I made no effort to speed\n"
         "   these up (yet).\n"
         "* '-usetemp' disables OpenMP multi-CPU usage, since the file I/O for\n"
         "   saving and restoring various matrices and results is not easily\n"
         "   parallelized.\n"
       ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------- official startup ------*/

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   PRINT_VERSION("3dREMLfit"); mainENTRY("3dREMLfit main"); machdep();
   AFNI_logger("3dREMLfit",argc,argv); AUTHOR("RWCox");
   (void)COX_clock_time() ;

   /**------- scan command line --------**/

   if( AFNI_yesenv("AFNI_3dDeconvolve_GOFORIT") ) goforit++ ;

   iarg = 1 ;
   while( iarg < argc ){

     /**==========   -gltsym  ==========**/

     if( strcasecmp(argv[iarg],"-gltsym") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("Need 2 arguments after '%s'",argv[iarg-1]) ;
       eglt_sym = (char **)realloc( eglt_sym , sizeof(char *)*(eglt_num+1) ) ;
       eglt_lab = (char **)realloc( eglt_lab , sizeof(char *)*(eglt_num+1) ) ;
       eglt_sym[eglt_num] = argv[iarg++] ;
       if( argv[iarg][0] != '-' ){
         eglt_lab[eglt_num] = argv[iarg++] ;
       } else {
         eglt_lab[eglt_num] = (char *)malloc(sizeof(char)*16) ;
         sprintf( eglt_lab[eglt_num] , "GLTsym%02d" , eglt_num+1 ) ;
       }
       eglt_num++ ; continue ;
     }

     /**==========   verbosity  ==========**/

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     /**==========  -GOFORIT babee!  =========**/

     if( strcasecmp(argv[iarg],"-GOFORIT") == 0 ){  /* 19 Dec 2008 */
       iarg++ ;
       if( iarg < argc && isdigit(argv[iarg][0]) )
         goforit += (int)strtod(argv[iarg++],NULL) ;
       else
         goforit++ ;
       continue ;
     }

     /**==========   -nodmbase and -usetemp  ==========**/

     if( strcasecmp(argv[iarg],"-nodmbase") == 0 ){
       dmbase = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-usetemp") == 0 ){
       usetemp = 1 ; iarg++ ; continue ;
     }

     /**==========   -addbase  ==========**/

     if( strcasecmp(argv[iarg],"-addbase") == 0 ){
       MRI_IMAGE *im ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       do{
         im = mri_read_1D( argv[iarg] ) ;
         if( im == NULL ) ERROR_exit("Can't read -addbase file '%.33s'",argv[iarg]) ;
         if( imar_addbase == NULL ) INIT_IMARR(imar_addbase) ;
         mri_add_name( THD_trailname(argv[iarg],0) , im ) ;
         ADDTO_IMARR( imar_addbase , im ) ;
         ncol_addbase += im->ny ;  /* number of columns to add to the matrix */
         iarg++ ;
       } while( iarg < argc && argv[iarg][0] != '-' ) ;
       continue ;
     }

     /**==========   -slibase  ==========**/

     if( strcasecmp(argv[iarg],"-slibase") == 0 ){
       MRI_IMAGE *im ;
       int label_order ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       do{
         im = mri_read_1D( argv[iarg] ) ;
         if( im == NULL ) ERROR_exit("Can't read -slibase file '%.33s'",argv[iarg]) ;
         /* if known, require slice-minor order of regressors 28 Jul 2009 [r] */
         label_order = niml_get_major_label_order(argv[iarg]);
         if( label_order != 2 ) {  /* not slice-minor order */
           if( label_order == 1 ) {
             ERROR_exit("Label ordering of -slibase dataset is slice-major,\n"
                 "   for which -slibase_sm is more appropriate.  If this is\n"
                 "   not clear, search for it on the AFNI Message Board:\n"
                 "        http://afni.nimh.nih.gov/afni/community/board");
           } else { /* order is unknown */
             WARNING_message("If your regressors are made via 'RetroTS',\n"
                 "   perhaps -slibase_sm is more appropriate than -slibase.\n"
                 "   -> consider: 1d_tool.py -show_label_ordering"
                 " -infile %s", argv[iarg]);
           }
         } /* end label_order check */
         if( imar_slibase == NULL ) INIT_IMARR(imar_slibase) ;
         mri_add_name( THD_trailname(argv[iarg],0) , im ) ;
         ADDTO_IMARR( imar_slibase , im ) ;
         nfile_slibase++ ;
         iarg++ ;
       } while( iarg < argc && argv[iarg][0] != '-' ) ;
       continue ;
     }

     /**==========   -slibase_sm (slice-major order) ==========**/
     /**                                   27 Jul 2009 [rickr] **/

     if( strcasecmp(argv[iarg],"-slibase_sm") == 0 ){
       MRI_IMAGE *im, *newim;
       int       nz, label_order ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( !inset ) ERROR_exit("-slibase_sm must follow -input");
       nz = DSET_NZ(inset);
       do{
         im = mri_read_1D( argv[iarg] ) ;
         if( im == NULL )
            ERROR_exit("Can't read -slibase_sm file '%.33s'", argv[iarg]) ;
         /* if known, require slice-major order of regressors 28 Jul 2009 [r] */
         label_order = niml_get_major_label_order(argv[iarg]);
         if( label_order != 1 ) {  /* not slice-major order */
           if( label_order == 2 ) {
             ERROR_exit("Label order of -slibase_sm dataset is slice-minor,\n"
                 "   for which -slibase is more appropriate.  If this is\n"
                 "   not clear, search for it on the AFNI Message Board:\n"
                 "        http://afni.nimh.nih.gov/afni/community/board");
           } else { /* order is unknown */
             WARNING_message("If your regressors are made via 'RetroTS',\n"
                 "   perhaps -slibase is more appropriate than -slibase_sm.\n"
                 "   -> consider: 1d_tool.py -show_label_ordering"
                 " -infile %s", argv[iarg]);
           }
         } /* end label_order check */
         if( imar_slibase == NULL ) INIT_IMARR(imar_slibase) ;
         /* conver the slice-major column order to slice-minor */
         if( (newim = mri_interleave_columns(im, nz) ) == NULL ) exit(1) ;
         mri_free(im) ; im = newim ;
         mri_add_name( THD_trailname(argv[iarg],0) , im ) ;
         ADDTO_IMARR( imar_slibase , im ) ;
         nfile_slibase++ ;
         iarg++ ;
       } while( iarg < argc && argv[iarg][0] != '-' ) ;
       continue ;
     }

     /**==========   -noFDR and -FDR  ==========**/

     if( strcasecmp(argv[iarg],"-noFDR") == 0 ){
       do_FDR = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-FDR") == 0 ){
       do_FDR = 1 ; iarg++ ; continue ;
     }

     /**==========   -ABfile  ==========**/

     if( strcasecmp(argv[iarg],"-ABfile") == 0 ){
       if( abset != NULL || abfixed )
         ERROR_exit("Can't have 2 '%s' options",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( argv[iarg][0] == '=' && strchr(argv[iarg],',') != NULL ){
         afix = bfix = -66.0f ;
         sscanf( argv[iarg] , "=%f,%f" , &afix , &bfix ) ;
         if( afix < -0.9f || afix > 0.9f ||
             bfix < -0.9f || bfix > 0.9f   )
           ERROR_exit("%s %s has illegal fixed values of a and/or b!",
                      argv[iarg-1] , argv[iarg] ) ;
         abfixed = 1 ;
       } else {
         abset = THD_open_dataset( argv[iarg] ) ;
         CHECK_OPEN_ERROR(abset,argv[iarg]) ;
       }
       iarg++ ; continue ;
     }

      /**==========   ARMA params  ==========**/

     if( strcasecmp(argv[iarg],"-MAXrho") == 0 || strcasecmp(argv[iarg],"-MAXa") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       rhomax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( rhomax < 0.1 ){ rhomax = 0.1; WARNING_message("-MAXa re-set to 0.1"); }
       else if( rhomax > 0.9 ){ rhomax = 0.9; WARNING_message("-MAXa re-set to 0.9"); }
       rm_set = rhomax ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-Grid") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       nlevab = (int)strtod(argv[iarg],NULL) ;
            if( nlevab < 3 ){ nlevab = 3; WARNING_message("-Grid reset to 3"); }
       else if( nlevab > 7 ){ nlevab = 7; WARNING_message("-Grid reset to 7"); }
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-MAXb") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bmax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( bmax < 0.1 ){ bmax = 0.1; WARNING_message("-MAXb re-set to 0.1"); }
       else if( bmax > 0.9 ){ bmax = 0.9; WARNING_message("-MAXb re-set to 0.9"); }
       bm_set = bmax ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-NEGcor") == 0 ){
       REML_allow_negative_correlations(1) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-POScor") == 0 ){
       REML_allow_negative_correlations(0) ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-Mfilt") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       mfilt_radius = (float)strtod(argv[iarg],NULL) ;
       do_mfilt = (mfilt_radius != 0.0f) ;
       do_dxyz  = (mfilt_radius > 0.0f) ;
       mri_medianfilter_usedxyz( do_dxyz ) ;
       if( mfilt_radius < 0.0f ) mfilt_radius = -mfilt_radius ;
       iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-CORcut") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       dx = (float)strtod(argv[iarg],NULL) ;
       if( dx > 0.0f && dx <= 0.1f ) corcut = (MTYPE)dx ;
       else WARNING_message("Illegal value after -CORcut -- ignoring it!") ;
       iarg++ ; continue ;
     }

     /**==========   -matrix  ==========**/

     if( strcasecmp(argv[iarg],"-matrix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!?");
       nelmat = NI_read_element_fromfile( argv[iarg] ) ; /* read NIML file */
       matname = argv[iarg];
       if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
         ERROR_exit("Can't process -matrix file!?") ;
       iarg++ ; continue ;
     }

      /**==========   -input  ==========**/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options!?") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /**==========   -mask  ==========**/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%.33s'",argv[iarg]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb || nmask < 1 ) INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 1 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     /**==========   statistics options  ==========**/

     if( strcasecmp(argv[iarg],"-fout") == 0 ){
       do_fstat = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-tout") == 0 ){
       do_tstat = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-rout") == 0 ){  /* 23 Oct 2008 */
       do_rstat = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-nobout") == 0 ){ /* 25 Mar 2009 */
       nobout = 1 ; iarg++ ; continue ;
     }

     /**==========   prefix options  ==========**/

#undef  PREFIX_OPTION
#define PREFIX_OPTION(vnam)                                      \
     if( strncasecmp(argv[iarg], "-" #vnam ,5) == 0 ){           \
       if( ++iarg >= argc )                                      \
         ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;   \
       vnam = strdup(argv[iarg]) ;                               \
       if( (#vnam)[0] == 'R' ) nprefixR++ ; else nprefixO++ ;    \
       if( !THD_filename_ok(vnam) )                              \
         ERROR_exit("Illegal string after %s",argv[iarg-1]) ;    \
       iarg++ ; continue ;                                       \
     }

     PREFIX_OPTION(Rbeta_prefix)  ;
     PREFIX_OPTION(Rvar_prefix)   ;
     PREFIX_OPTION(Rbuckt_prefix) ;

     PREFIX_OPTION(Obeta_prefix)  ;
     PREFIX_OPTION(Ovar_prefix)   ;
     PREFIX_OPTION(Obuckt_prefix) ;
     PREFIX_OPTION(Rfitts_prefix) ;
     PREFIX_OPTION(Ofitts_prefix) ;

     PREFIX_OPTION(Rerrts_prefix) ;
     PREFIX_OPTION(Oerrts_prefix) ;
     PREFIX_OPTION(Rwherr_prefix) ;

     PREFIX_OPTION(Rglt_prefix) ;
     PREFIX_OPTION(Oglt_prefix) ;

     /**==========   bad users must be punished  ==========**/

     ERROR_exit("Unknown option '%.33s'",argv[iarg]) ;
   }

STATUS("options done") ;

   /**--------------- sanity checks, dataset input, maskifying --------------**/

   if( inset             == NULL ) ERROR_exit("No -input dataset?!") ;
   if( nprefixR+nprefixO == 0    ) ERROR_exit("No output datasets at all?!") ;
   if( nprefixR          == 0    ){
     WARNING_message("No -R* output datasets? Skipping REML(a,b) estimation!") ;
     afix = bfix = 0.0f ; abfixed = 1 ;
     if( abset != NULL ){ DSET_delete(abset) ; abset = NULL ; }
   }

   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;
   dx = fabsf(DSET_DX(inset)) ; nx = DSET_NX(inset) ;
   dy = fabsf(DSET_DY(inset)) ; ny = DSET_NY(inset) ; nxy = nx*ny ;
   dz = fabsf(DSET_DZ(inset)) ; nz = DSET_NZ(inset) ;

#ifdef USE_OMP
  omp_set_nested(0) ;
  maxthr = omp_get_max_threads() ;
   if( maxthr > 1 ){  /* disable threads for some options [16 Jun 2009] */
     if( usetemp ){
       maxthr = 1 ;
       WARNING_message("-usetemp disables OpenMP multi-CPU usage") ;
     } else if( nvox < 999 ){
       maxthr = 1 ;
       WARNING_message("only %d voxels: disables OpenMP multi-CPU usage",nvox) ;
     }
   }
#if 0
   if( maxthr > 1 ){
     WARNING_message("OpenMP threads disabled at this time -- sorry") ;
     maxthr = 1 ;
   }
#else
   if( maxthr > 1 ) INFO_message("Number of OpenMP threads = %d",maxthr) ;
#endif
#endif

#if 0
   if( nelmat == NULL ) ERROR_exit("No -matrix file?!") ;
#else
   if( nelmat == NULL ){  /* make up a matrix [14 Apr 2009] */
     char *str = NULL ; NI_stream ns ;
     WARNING_message(
       "No -matrix file! Making up a matrix with one column of %d 1s",nvals) ;
     str = THD_zzprintf(str,
                        "str:<matrix ni_type='double' ni_dimen='%d'\n"
                        "            NRowFull='%d' GoodList='0..%d' >\n",
                        nvals , nvals , nvals-1 ) ;
     for( ii=0 ; ii < nvals ; ii++ ) str = THD_zzprintf(str," 1\n") ;
     str = THD_zzprintf(str,"</matrix>\n") ;
     ns  = NI_stream_open( str , "r" ) ;
     if( ns == (NI_stream)NULL ) ERROR_exit("Can't fabricate matrix!?") ;
     nelmat = NI_read_element( ns , 9 ) ;
     NI_stream_close(ns) ; free(str) ;
     if( nelmat == NULL ) ERROR_exit("Can't fabricate matrix?!") ;
   }
#endif

   usetemp_rcol = usetemp ;
   if( usetemp ){
     if( abfixed || nfile_slibase == 0 || nz == 1 ) usetemp_rcol = 0 ;
     INFO_message(
         "-usetemp filenames will be of the form\n"
         "       %s/REML_%s*\n"
         "   If 3dREMLfit crashes, you may have to 'rm' these manually" ,
         mri_purge_get_tmpdir() , mri_purge_get_tsuf() ) ;
   } else if( nfile_slibase > 0 && nz > 1 ){
     INFO_message(
       "If program runs out of memory, try re-running with -usetemp option");
   }

   if( eglt_num == 0 && (Rglt_prefix != NULL || Oglt_prefix != NULL) ){
     WARNING_message("-Rglt/-Oglt disabled since no GLTs on 3dREMLfit command line") ;
     Rglt_prefix = Oglt_prefix = NULL ;
   }

   do_buckt = (Rbuckt_prefix != NULL) || (Obuckt_prefix != NULL) ;
   do_eglt  = (Rglt_prefix   != NULL) || (Oglt_prefix   != NULL) ;

   if( !do_buckt && !do_eglt ){
     if( do_fstat ){
       WARNING_message("-fout disabled because no bucket dataset will be output");
       do_fstat = 0 ;
     }
     if( do_tstat ){
       WARNING_message("-tout disabled because no bucket dataset will be output");
       do_tstat = 0 ;
     }
     if( do_rstat ){
       WARNING_message("-rout disabled because no bucket dataset will be output");
       do_rstat = 0 ;
     }
   }

   if( (do_buckt || do_eglt) && !do_fstat && !do_tstat && !do_rstat ){
     do_fstat = 1 ;
     if( verb )
       INFO_message("assuming -fout since you asked for a bucket dataset") ;
   }

   do_stat = (do_fstat || do_tstat || do_rstat) ;

   /*--------- check out and read the -ABfile, if there is one ---------*/

   if( abset != NULL ){
     float abot,atop , bbot,btop ; ATR_float *atr ; MTYPE atm ;

     if( DSET_NX(abset) != nx || DSET_NY(abset) != ny || DSET_NZ(abset) != nz )
       ERROR_exit("-input and -ABfile datasets don't match grid sizes!") ;
     if( DSET_NVALS(abset) < 2 )
       ERROR_exit("-ABfile must have (at least) 2 sub-bricks!") ;
     else if( DSET_NVALS(abset) > 2 )
       WARNING_message("-ABfile has %d sub-bricks: only using first 2" ,
                       DSET_NVALS(abset) ) ;
     if( DSET_BRICK_TYPE(abset,0) != MRI_float ||
         DSET_BRICK_TYPE(abset,1) != MRI_float   )
       ERROR_exit("-ABfile sub-bricks are not stored as floats!?") ;

     DSET_load(abset) ; CHECK_LOAD_ERROR(abset) ;
     aim = DSET_BRICK(abset,0); abot = mri_min(aim); atop = mri_max(aim);
     bim = DSET_BRICK(abset,1); bbot = mri_min(bim); btop = mri_max(bim);
     if( abot < -0.9f || atop > 0.9f || bbot < -0.9f || btop > 0.9f )
       ERROR_exit("-ABfile (a,b) values out of range -0.9..+0.9 ?!") ;

     REML_allow_negative_correlations(1) ;
     if( do_mfilt ){
       do_mfilt = 0 ; WARNING_message("-ABfile disables -Mfilt !!") ;
     }

     atop = fabsf(atop) ; abot = fabsf(abot) ; rhomax = MAX(abot,atop) ;
     btop = fabsf(btop) ; bbot = fabsf(bbot) ; bmax   = MAX(bbot,btop) ;
     if( rhomax < 0.1    ) rhomax = 0.1 ;
     if( bmax   < 0.1    ) bmax   = 0.1 ;
     if( rhomax < rm_set ) rhomax = rm_set ;
     if( bmax   < bm_set ) bmax   = bm_set ;
     atr = THD_find_float_atr( abset->dblk , "REMLFIT_abmax" ) ;
     if( atr != NULL && atr->nfl >= 3 ){
       atm = (MTYPE)atr->fl[0] ; if( atm > rhomax ) rhomax = atm ;
       atm = (MTYPE)atr->fl[1] ; if( atm > bmax   ) bmax   = atm ;
       ii  = (int)  atr->fl[2] ; if( ii  > nlevab ) nlevab = ii  ;
     }
     rhomax *= 1.00001 ; bmax *= 1.00001 ;
     if( verb )
       INFO_message(
         "-ABfile: (a,b) Grid = %+.3f..%+.3f x %+.3f..%+.3f Levels = %d",
         -rhomax,rhomax , -bmax,bmax , nlevab ) ;

     aim->dx = dx ; aim->dy = dy ; aim->dz = dz ; aar = MRI_FLOAT_PTR(aim) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ; bar = MRI_FLOAT_PTR(bim) ;

     for( ii=2 ; ii < DSET_NVALS(abset) ; ii++ ) DSET_unload_one(abset,ii) ;

   } else if( abfixed ){

     REML_allow_negative_correlations(1) ;
     if( do_mfilt ){
       do_mfilt = 0 ; WARNING_message("-ABfile disables -Mfilt") ;
     }

   }

   /*-------------- process the mask somehow --------------*/

   if( mask != NULL ){     /* check -mask option for compatibility */
     gmask = mask ;
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){  /* create a mask from input dataset */
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( nvox , mask ) ;
     if( verb || nmask < 1 )
       INFO_message("Number of voxels in automask = %d (out of %d = %.1f%%)",
                    nmask, nvox, (100.0f*nmask)/nvox ) ;
     if( nmask < 1 ) ERROR_exit("Automask is too small to process") ;
     gmask = mask ;

   } else {                /* create a 'mask' for all voxels */
     if( verb )
       INFO_message("No mask ==> computing for all %d voxels",nvox) ;
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset( mask , 1 , sizeof(byte)*nvox ) ;

     if( do_FDR && DSET_NX(inset) > 4 && DSET_NY(inset) > 4 ){  /* 27 Mar 2009 */
       MRI_IMAGE *qim ; int mc ;
       qim   = THD_rms_brick(inset) ;
       gmask = mri_automask_image( qim ) ;
       mri_free( qim ) ;
       mc = THD_countmask( nvox , gmask ) ;
       if( mc <= 99 && gmask != NULL ){ free(gmask) ; gmask = NULL ; }
       else if( verb )
         INFO_message("FDR automask has %d voxels (out of %d = %.1f%%)",
                      mc, nvox, (100.0f*mc)/nvox ) ;
     }
   }
   mri_fdr_setmask(gmask) ;  /* 27 Mar 2009 */

   /**-------------------- process the matrix --------------------**/

STATUS("process matrix") ;

   nrego = nelmat->vec_num ;  /* number of matrix columns */
   nrega = nrego ;            /* 'o'=original 'a'=after -addbase */
   ntime = nelmat->vec_len ;  /* number of matrix rows */
   ddof  = ntime - nrego ;
   if( ddof < 1 )             /* should not happen! */
     ERROR_exit("matrix has more columns than rows!") ;

   cgl = NI_get_attribute( nelmat , "CommandLine" ) ;
   if( cgl != NULL ) commandline = strdup(cgl) ;

   /*--- number of rows in the full matrix (without censoring) ---*/

   cgl = NI_get_attribute( nelmat , "NRowFull" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'NRowFull' attribute!") ;
   nfull = (int)strtod(cgl,NULL) ;
   if( nvals != nfull )
     ERROR_exit("-input dataset has %d time points, but matrix indicates %d",
                nvals , nfull ) ;

   /*--- the goodlist = mapping from matrix row index to time index
                        (which allows for possible time point censoring) ---*/

   cgl = NI_get_attribute( nelmat , "GoodList" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'GoodList' attribute!") ;
   giar = NI_decode_int_list( cgl , ";," ) ;
   if( giar == NULL || giar->num < ntime )
     ERROR_exit("Matrix 'GoodList' badly formatted?!") ;
   Ngoodlist = giar->num ; goodlist = giar->ar ;
   if( Ngoodlist != ntime )
     ERROR_exit("Matrix 'GoodList' incorrect length?!") ;

   /*----- run starting points in time indexes -----*/

   rst = NI_get_attribute( nelmat , "RunStart" ) ;
   if( rst != NULL ){
     NI_int_array *riar = NI_decode_int_list( rst , ";,") ;
     if( riar == NULL ) ERROR_exit("Matrix 'RunStart' badly formatted?") ;
     Nruns = riar->num ; runs = riar->ar ;
   } else {
     if( verb )
       INFO_message("Matrix missing 'RunStart' attribute ==> assuming 1 run");
     Nruns = 1 ; runs = calloc(sizeof(int),1) ;
   }

   /*----- set up pseudo-time tau[] vector for R matrix formation -----*/

   rnum = 0 ; tau = (int *)malloc(sizeof(int)*ntime) ;
   for( ii=0 ; ii < ntime ; ii++ ){
     jj = goodlist[ii] ;        /* time index of the ii-th matrix row */
                              /* then find which run this point is in */
     for( ; rnum+1 < Nruns && jj >= runs[rnum+1] ; rnum++ ) ;   /*nada*/
     tau[ii] = jj + 66666*rnum ;  /* the 66666 means 'very far apart' */
   }

   /*--- re-create the regression matrix X, from the NIML data element ---*/

STATUS("re-create matrix") ;

   matrix_initialize( &X ) ;
   matrix_create( ntime , nrego , &X ) ;
   if( nelmat->vec_typ[0] == NI_FLOAT ){        /* from 3dDeconvolve_f */
     float *cd ;
     for( jj=0 ; jj < nrego ; jj++ ){
       cd = (float *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){  /* from 3dDeconvolve */
     double *cd ;
     for( jj=0 ; jj < nrego ; jj++ ){
       cd = (double *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else {
     ERROR_exit("Matrix file stored with illegal data type!?") ;
   }

   /*--------------- get column labels for the betas ---------------*/

   cgl = NI_get_attribute( nelmat , "ColumnLabels" ) ;
   if( cgl == NULL ){
     WARNING_message("ColumnLabels attribute in matrix is missing!?") ;
   } else {
     gsar = NI_decode_string_list( cgl , ";" ) ;
     if( gsar == NULL || gsar->num < nrego )
       ERROR_exit("ColumnLabels attribute in matrix is malformed!?") ;
     beta_lab = gsar->str ;
   }

   /****------------------ process -addbase images ------------------****/

   if( imar_addbase != NULL ){
     MRI_IMAGE *im ; int pp ; float *iar ;

STATUS("process -addbase images") ;

     nrega += ncol_addbase ;  /* new number of regressors */

     if( verb )
       INFO_message("Adding %d column%s to X matrix via '-addbase'" ,
                    ncol_addbase , (ncol_addbase==1) ? "" : "s"      ) ;

     /*--- check each image for OK-ness == right length in time ---*/

     for( nbad=ii=0 ; ii < IMARR_COUNT(imar_addbase) ; ii++ ){
       im = IMARR_SUBIM( imar_addbase , ii ) ;
       if( im->nx == ntime ) continue ; /* OK */
       if( im->nx == nfull ){           /* must be censored */
         MRI_IMAGE *imb = mri_subset_x2D( ntime , goodlist , im ) ;
         mri_free(im) ; IMARR_SUBIM(imar_addbase,ii) = imb ;
         if( verb )
           INFO_message("Censored -addbase file '%.33s' from %d down to %d rows" ,
                        imb->name , nfull , ntime ) ;
         continue ;
       }
       ERROR_message("-addbase file '%.33s' has %d rows, but matrix has %d !?" ,
                     im->name , im->nx , ntime ) ;
       nbad++ ;
     }

     ddof = ntime - nrega ;
     if( ddof < 1 ){
       ERROR_exit("matrix has more columns (%d) than rows (%d) after -addbase!" ,
                  nrega , ntime ) ;
       nbad++ ;
     }

     if( nbad > 0 ) ERROR_exit("Cannot continue after -addbase errors!") ;

     /*------ make up extra labels for these columns ------*/

     if( beta_lab != NULL ){
       char lll[32] ;
       beta_lab = NI_realloc( beta_lab , char * , sizeof(char *)*nrega ) ;
       for( kk=nrego,ii=0 ; ii < IMARR_COUNT(imar_addbase) ; ii++ ){
         im = IMARR_SUBIM( imar_addbase , ii ) ;
         for( jj=0 ; jj < im->ny ; jj++ ){
           if( im->ny > 1 ) sprintf(lll,"%.16s[%d]",im->name,jj) ;
           else             sprintf(lll,"%.16s"    ,im->name   ) ;
           beta_lab[kk++] = NI_strdup(lll) ;
         }
       }
     }

     /*--- enlarge the matrix, add the new columns to it, and proceed ---*/

     matrix_enlarge( 0 , ncol_addbase , &X ) ;
     for( kk=nrego,ii=0 ; ii < IMARR_COUNT(imar_addbase) ; ii++ ){
       im = IMARR_SUBIM(imar_addbase,ii) ; iar = MRI_FLOAT_PTR(im) ;
       for( jj=0 ; jj < im->ny ; jj++,kk++,iar+=im->nx ){  /* kk = matrix col */
         if( dmbase ){       /* demean the column? */      /* jj = input col */
           float csum=0.0f ;
           for( pp=0 ; pp < ntime ; pp++ ) csum += iar[pp] ;
           csum /= ntime ;
           for( pp=0 ; pp < ntime ; pp++ ) iar[pp] -= csum ;
         }
         for( pp=0 ; pp < ntime ; pp++ ) X.elts[pp][kk] = (MTYPE)iar[pp] ;
       }
     }

     DESTROY_IMARR(imar_addbase) ;

   } /**** end of -addbase stuff ****/

#if 0
{ double *sv = matrix_singvals(X) ;
  fprintf(stderr,"X singular values:") ;
  for( ii=0 ; ii < X.cols ; ii++ ) fprintf(stderr," %g",sv[ii]) ;
  fprintf(stderr,"\n") ; free(sv) ;
}
#endif

   /**--------------- check matrix for all zero columns ---------------**/

   for( nbad=jj=0 ; jj < nrega ; jj++ ){
     for( ii=0 ; ii < ntime && X.elts[ii][jj]==0.0 ; ii++ ) ; /*nada*/
     if( ii==ntime ){
       ERROR_message("matrix column #%d is all zero!?",jj) ; nbad++ ;
     }
   }
   if( nbad > 0 ) ERROR_exit("Cannot continue with all zero columns!") ;

   /****------------------ process -slibase images ------------------****/

   if( imar_slibase == NULL ){  /** no per-slice regressors */

     Xsli    = (matrix **)malloc(sizeof(matrix *)) ;
     Xsli[0] = &X ;
     nsli    = 1 ;
     nsliper = (1<<30) ;  /* number of voxels per slice = 1 beellion */

   } else {                     /** have per-slice regressors **/

     MRI_IMAGE *im ; int pp,nregq,nc,cc ; float *iar ; matrix *Xs ;

STATUS("process -slibase images") ;

     /* figure out how many columns will be added */

     nsli = nz ; ncol_slibase = 0 ;

     for( nbad=ii=0 ; ii < IMARR_COUNT(imar_slibase) ; ii++ ){
       im = IMARR_SUBIM( imar_slibase , ii ) ;
       nc = im->ny / nsli ;  /* how many slice sets are in this image */
       kk = im->ny % nsli ;  /* how many left over (should be zero) */
       if( kk != 0 ){
         ERROR_message(
           "-slibase file '%.33s' has %d columns but dataset has %d slices",
           im->name , im->ny , nsli ) ;
         nbad++ ; continue ;
       }
       ncol_slibase += nc ;  /* total number of columns to add to matrix */
       if( im->nx == ntime ) continue ; /* OK */
       if( im->nx == nfull ){
         MRI_IMAGE *imb = mri_subset_x2D( ntime , goodlist , im ) ;
         mri_free(im) ; IMARR_SUBIM(imar_slibase,ii) = imb ;
         if( verb )
           INFO_message("Censored -slibase file '%.33s' from %d down to %d rows" ,
                        imb->name , nfull , ntime ) ;
         continue ;
       }
       ERROR_message("-slibase file '%.33s' has %d rows, but matrix has %d" ,
                     im->name , im->nx , ntime ) ;
       nbad++ ;
     }

     nregq   = nrega ;         /* save this for a little bit */
     nrega  += ncol_slibase ;  /* new number of regressors */
     nsliper = nxy ;           /** number of voxels per slice **/

     if( verb )
       INFO_message("Adding %d column%s to X matrix via '-slibase'" ,
                    ncol_slibase , (ncol_slibase==1) ? "" : "s"      ) ;

     ddof = ntime - nrega ;
     if( ddof < 1 ){
       ERROR_message("matrix has more columns (%d) than rows (%d) after -slibase!" ,
                     nrega , ntime ) ;
       nbad++ ;
     }

     if( nbad > 0 ) ERROR_exit("Cannot continue after -slibase errors!") ;

     /*--------- make up extra labels for these columns ---------*/

     if( beta_lab != NULL ){
       char lll[32] ;
       beta_lab = NI_realloc( beta_lab , char * , sizeof(char *)*nrega ) ;
       for( kk=nregq,ii=0 ; ii < ncol_slibase ; ii++,kk++ ){
         sprintf(lll,"slibase#%d",ii+1) ;
         beta_lab[kk] = NI_strdup(lll) ;
       }
     }

     /*------ for each slice, make up a new matrix, add columns to it ------*/

     Xsli = (matrix **)malloc(sizeof(matrix *)*nsli) ;  /* array of matrices */

     /* loop over slice indexes */

     for( nbad=ss=0 ; ss < nsli ; ss++ ){       /* ss = slice index */

       Xs = (matrix *)malloc(sizeof(matrix)) ;  /* new matrix */
       matrix_initialize(Xs) ;
       matrix_equate( X , Xs ) ;                /* copy in existing matrix */
       matrix_enlarge( 0,ncol_slibase , Xs ) ;  /* make new one bigger */

       /* loop over slibase image files:
           ii = image file index
           kk = column index in matrix to get new data
           nc = number of slices sets in ii-th image (nsli columns per set)
           cc = loop index over slice sets (skipping nsli columns in image) */

       for( kk=nregq,ii=0 ; ii < IMARR_COUNT(imar_slibase) ; ii++ ){

         im  = IMARR_SUBIM(imar_slibase,ii) ;   /* ii-th slibase image */
         nc  = im->ny / nsli ;
         iar = MRI_FLOAT_PTR(im) ;  /* pointer to data */
         iar += ss * im->nx ;       /* pointer to ss-th column */

         /* loop over the ss-th slice set in this file:
             skip head nsli columns in iar at each iteration */

         for( cc=0 ; cc < nc ; cc++,kk++,iar+=nsli*im->nx ){
           if( dmbase ){        /* de-mean the column */
             float csum=0.0f ;
             for( pp=0 ; pp < ntime ; pp++ ) csum += iar[pp] ;
             csum /= ntime ;
             for( pp=0 ; pp < ntime ; pp++ ) iar[pp] -= csum ;
           }
           for( pp=0 ; pp < ntime ; pp++ ) Xs->elts[pp][kk] = (MTYPE)iar[pp] ;

           for( pp=0 ; pp < ntime && iar[pp]==0.0f ; pp++ ) ; /*nada*/
           if( pp == ntime ){
             ERROR_message("-slibase file %.33s col #%d is all zero",im->name,ss) ;
             nbad++ ;
           }
         } /* end of loop over cc = slice set */

       } /* end of loop over input files, extracting for the ss-th slice */

       Xsli[ss] = Xs ;  /* save this completed matrix in the array */

     } /* end of loop over slices */

     if( nbad > 0 ) ERROR_exit("Cannot continue after -slibase errors!") ;

     DESTROY_IMARR(imar_slibase) ;

   } /**** end of -slibase stuff ****/

   /**---- check X matrices for collinearity ----**/

   { char lab[32]="\0" ;
     for( nbad=ss=0 ; ss < nsli ; ss++ ){
       if( nsli > 1 ) sprintf(lab,"slice #%d",ss) ;
       nbad += check_matrix_condition( *(Xsli[ss]) , lab ) ;
     }
     if( nbad > 0 ) {
       if( !goforit ){
         ERROR_exit("Can't continue after matrix condition errors!\n"
                    "** you might try -GOFORIT, but be careful! (cf. '-help')");
       } else {
         WARNING_message("-GOFORIT ==> Continuing on despite my grave misgivings!") ;
         ININFO_message (" Check results carefully!") ;
       }
     }
   }

   /***---------- extract stim information from matrix header ----------***/

   cgl = NI_get_attribute( nelmat , "Nstim" ) ;
   if( cgl != NULL ){

     stim_num = (int)strtod(cgl,NULL) ;
     if( stim_num <= 0 ) ERROR_exit("Nstim attribute in matrix is not positive!");

     cgl = NI_get_attribute( nelmat , "StimBots" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'StimBots' attribute!") ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < stim_num )
       ERROR_exit("Matrix 'StimBots' badly formatted?!") ;
     stim_bot = giar->ar ;

     cgl = NI_get_attribute( nelmat , "StimTops" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'StimTops' attribute!") ;
     giar = NI_decode_int_list( cgl , ";," ) ;
     if( giar == NULL || giar->num < stim_num )
       ERROR_exit("Matrix 'StimTops' badly formatted?!") ;
     stim_top = giar->ar ;

     cgl = NI_get_attribute( nelmat , "StimLabels" ) ;
     if( cgl == NULL ) ERROR_exit("Matrix is missing 'StimLabels' attribute!");
     gsar = NI_decode_string_list( cgl , ";" ) ;
     if( gsar == NULL || gsar->num < stim_num )
       ERROR_exit("Matrix 'StimLabels' badly formatted?!") ;
     stim_lab = gsar->str ;

     nSymStim = stim_num+2 ;
     SymStim  = (SYM_irange *)calloc(sizeof(SYM_irange),nSymStim) ;

     strcpy( SymStim[stim_num].name , "Ort" ) ;    /* pre-stim baselines */
     SymStim[stim_num].nbot = 0 ;
     SymStim[stim_num].ntop = stim_bot[0] - 1 ;
     SymStim[stim_num].gbot = 0 ;

     strcpy( SymStim[stim_num+1].name , "Col" ) ;  /* all regressors */
     SymStim[stim_num+1].nbot = 0 ;
     SymStim[stim_num+1].ntop = nrega-1 ;
     SymStim[stim_num+1].gbot = 0 ;

     for( ss=0 ; ss < stim_num ; ss++ ){
       SymStim[ss].nbot = 0 ;
       SymStim[ss].ntop = stim_top[ss]-stim_bot[ss] ;
       SymStim[ss].gbot = stim_bot[ss] ;
       MCW_strncpy( SymStim[ss].name , stim_lab[ss] , 64 ) ;
     }

   } else {  /* don't have stim info in matrix header?! */

     WARNING_message("-matrix file is missing Stim attributes:") ;
     ININFO_message ("can only use symbolic name 'Col' in GLTs (cf. '-help')" ) ;
     if( do_buckt )
       WARNING_message(" ==> bucket dataset output is disabled") ;
     if( nobout )
       WARNING_message(" ==> -nobout is disabled") ;
     nobout = do_buckt = do_glt = 0 ; Rbuckt_prefix = Obuckt_prefix = NULL ;

     /* a fake name */

     nSymStim = 1 ;
     SymStim  = (SYM_irange *)calloc(sizeof(SYM_irange),nSymStim) ;
     strcpy( SymStim[0].name , "Col" ) ;
     SymStim[0].nbot = 0 ;
     SymStim[0].ntop = nrega-1 ;
     SymStim[0].gbot = 0 ;

   }

   /***--- set the betas to keep in the Rbeta/Obeta outputs ---***/

   betaset = (int *)  malloc(sizeof(int)*nrega) ;  /* beta indexes to keep */

   if( !nobout ){
     for( ii=0 ; ii < nrega ; ii++ ) betaset[ii] = ii ;
     nbetaset = nrega ;
   } else {
     for( kk=jj=0 ; jj < stim_num ; jj++ ){
       for( ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) betaset[kk++] = ii ;
     }
     nbetaset = kk ;
   }

   /***--- setup to do statistics on the stimuli betas, if desired ---***/

#define SKIP_SMAT 1
#undef  ADD_GLT
#define ADD_GLT(lb,gg)                                                              \
 do{ glt_lab = (char **   )realloc((void *)glt_lab, sizeof(char *   )*(glt_num+1)); \
     glt_mat = (matrix ** )realloc((void *)glt_mat, sizeof(matrix * )*(glt_num+1)); \
     glt_smat= (sparmat **)realloc((void *)glt_smat,sizeof(sparmat *)*(glt_num+1)); \
     glt_lab[glt_num] = strdup(lb) ; glt_mat[glt_num] = (gg) ;                      \
     glt_smat[glt_num]= (SKIP_SMAT) ? NULL : matrix_to_sparmat(*gg) ;               \
     glt_num++ ; glt_rtot += gg->rows ;                                             \
 } while(0)

#undef  KILL_ALL_GLTS
#define KILL_ALL_GLTS                                      \
 do{ for( ii=0 ; ii < glt_num ; ii++ ){                    \
       free(glt_lab[ii]) ; sparmat_destroy(glt_smat[ii]) ; \
       matrix_destroy(glt_mat[ii]) ; free(glt_mat[ii]) ;   \
     }                                                     \
     free(glt_lab); free(glt_mat); free(glt_smat);         \
 } while(0)

   if( do_stat && do_glt ){
     char lnam[32] ; int nn,mm ; matrix *gm ;
     int *set = (int *)malloc(sizeof(int)*nrega) ;  /* list of columns to keep */

STATUS("make stim GLTs") ;

     /*------ first set of indexes is all stimuli ------*/

     for( kk=jj=0 ; jj < stim_num ; jj++ ){
       for( ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) set[kk++] = ii ;
     }
     gm = create_subset_matrix( nrega , kk , set ) ;
     if( gm == NULL ) ERROR_exit("Can't create G matrix for FullModel?!") ;
     ADD_GLT( "Full" , gm ) ;

     allstim = (int *)malloc(sizeof(int)*kk) ; num_allstim = kk ;
     memcpy( allstim , set , sizeof(int)*num_allstim ) ;
     qsort_int( num_allstim , allstim ) ;

     /*--- now do each labeled stimulus separately ---*/

     for( jj=0 ; jj < stim_num ; jj++ ){
       for( kk=0,ii=stim_bot[jj] ; ii <= stim_top[jj] ; ii++ ) set[kk++] = ii ;
       gm = create_subset_matrix( nrega , kk , set ) ;
       if( gm == NULL ) ERROR_exit("Can't create G matrix for %.33s?!",stim_lab[jj]);
       ADD_GLT( stim_lab[jj] , gm ) ;
     }

     free((void *)set) ;  /* not needed no more, no how, no way */
   }

   /*------ extract other GLT information from matrix header ------*/

   cgl = NI_get_attribute( nelmat , "Nglt" ) ;
   if( do_stat && (cgl != NULL || eglt_num > 0) ){
     char lnam[32],*lll ; int nn,mm,ngl ; matrix *gm ; float *far ;

     /*----- process GLTs from the matrix header -----*/

     if( cgl != NULL && do_glt ){
STATUS("make GLTs from matrix file") ;
       ngl = (int)strtod(cgl,NULL) ;
       if( ngl <= 0 ) ERROR_exit("Nglt attribute in matrix is not positive!");

       cgl = NI_get_attribute( nelmat , "GltLabels" ) ;
       if( cgl == NULL ) ERROR_exit("Matrix is missing 'GltLabels' attribute!");
       gsar = NI_decode_string_list( cgl , ";" ) ;
       if( gsar == NULL || gsar->num < ngl )
         ERROR_exit("Matrix 'GltLabels' badly formatted?!") ;

       for( kk=0 ; kk < ngl ; kk++ ){
         sprintf(lnam,"GltMatrix_%06d",kk) ;
         cgl = NI_get_attribute( nelmat , lnam ) ;
         if( cgl == NULL ) ERROR_exit("Matrix is missing '%s' attribute!",lnam) ;
         gfar = NI_decode_float_list( cgl , "," ) ;
         if( gfar == NULL || gfar->num < 3 )
           ERROR_exit("Matrix attribute '%s' is badly formatted?!",lnam) ;
         far = gfar->ar ; nn = (int)far[0] ; mm = (int)far[1] ;
         if( nn <= 0 ) ERROR_exit("GLT '%.33s' has %d rows?",lnam,nn) ;
         if( mm != nrego )
           ERROR_exit("GLT '%.33s' has %d columns (should be %d)?",lnam,mm,nrego) ;
         gm = (matrix *)malloc(sizeof(matrix)) ; matrix_initialize(gm) ;
         matrix_create( nn, nrega, gm ) ;
         for( ii=0 ; ii < nn ; ii++ ){
           for( jj=0 ; jj < mm ; jj++ ) gm->elts[ii][jj] = far[jj+2+ii*mm] ;
         }
         lll = gsar->str[kk] ; if( lll == NULL || *lll == '\0' ) lll = lnam ;
         ADD_GLT( lll , gm ) ;
       }
     } /* end of GLTs from the matrix file */

     /**------ now process any extra GLTs on our local command line ------**/

     oglt_num = glt_num ;  /* number of 'original' GLTs */

     if( eglt_num > 0 ) STATUS("make GLTs from command line") ;

     for( nbad=kk=0 ; kk < eglt_num ; kk++ ){
       gm = create_gltsym( eglt_sym[kk] , nrega ) ;
       if( gm != NULL ) ADD_GLT( eglt_lab[kk] , gm ) ;
       else             nbad++ ;
     }
     if( nbad > 0 || SYM_expand_errcount() > 0 )
       ERROR_exit("Can't continue after -gltsym errors!") ;

   } /* end of GLT setup */

   /***--------- done with nelmat ---------***/

   NI_free_element(nelmat) ;  /* save some memory */

   /**------ load time series dataset, check for all zero voxels ------**/

   if( !DSET_LOADED(inset) ){
     if( verb ) INFO_message("Loading input dataset into memory") ;
     DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
     MEMORY_CHECK ;
   }

   vector_initialize( &y ) ; vector_create_noinit( ntime , &y ) ;
   niv = (nvals+nrega+glt_num+9)*2 ;
   iv  = (float *)malloc(sizeof(float)*(niv+1)) ;
   jv  = (float *)malloc(sizeof(float)*(niv+1)) ;

   for( nbad=vv=0 ; vv < nvox ; vv++ ){
     if( !INMASK(vv) ) continue ;
     (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
     for( ii=0 ; ii < ntime && iv[goodlist[ii]]==0.0f ; ii++ ) ; /*nada*/
     if( ii == ntime ){
       mask[vv] = 0 ; nbad++ ;
       if( gmask != NULL && gmask != mask ) gmask[vv] = 0 ;
     }
   }
   if( nbad > 0 ){
     nmask = THD_countmask( nvox , mask ) ;
     if( verb || nmask < 1 )
       ININFO_message("masked off %d voxel%s for being all zero; %d left in mask" ,
                      nbad , (nbad==1) ? "" : "s" , nmask ) ;
     if( nmask < 1 ) ERROR_exit("Can't continue after mask shrinks to nothing!") ;
   }

#ifdef USE_OMP
   if( maxthr > 1 && nmask < 99*maxthr ){
     maxthr = 1 ;
     WARNING_message("only %d voxels in mask: disables OpenMP multi-CPU usage",nmask) ;
   }
#endif

   /* 05 Nov 2008: convert input to a vector image struct, if it saves memory */

   if( !AFNI_noenv("AFNI_REML_ALLOW_VECTIM") ){
     double vsiz = (double)THD_vectim_size( inset , mask ) ;
     double dsiz = (double)DSET_TOTALBYTES( inset ) ;
     if( vsiz < 0.9*dsiz && vsiz > 10.0e+6 ){
       if( verb ){
         INFO_message("Converting dataset to vector image to save memory") ;
         ININFO_message(" dataset = %s bytes",approximate_number_string(dsiz)) ;
         ININFO_message(" vectim  = %s bytes",approximate_number_string(vsiz)) ;
       }
       inset_mrv = THD_dset_to_vectim( inset , mask , 0 ) ;
       if( inset_mrv != NULL ) DSET_unload(inset) ;
       else                    ERROR_message("Can't create vector image!?") ;
       MEMORY_CHECK ;
     }
   }

   /**---------------------- set up for REML estimation ---------------------**/

   if( verb ){
     INFO_message("starting REML setup calculations; total CPU=%.2f Elapsed=%.2f",
                  COX_cpu_time(),COX_clock_time() ) ;
     if( abfixed ) ININFO_message(" using fixed a=%.4f b=%.4f lam=%.4f",
                                  afix,bfix,LAMBDA(afix,bfix) ) ;
   }

   RCsli = (reml_collection **)calloc(sizeof(reml_collection *),nsli) ;

   if( !usetemp_rcol ){  /* set up them all */

     for( ss=0 ; ss < nsli ; ss++ ){  /* might take a while */
       if( abfixed )
         rrcol = REML_setup_all( Xsli[ss] , tau , 0     , afix  ,bfix ) ;
       else {
         if( verb && nsli > 1 && ntime*nrega > 9999 )
           ININFO_message(" start setup for slice #%d",ss) ;
         rrcol = REML_setup_all( Xsli[ss] , tau , nlevab, rhomax,bmax ) ;
       }
       if( rrcol == NULL ) ERROR_exit("REML setup fails at ss=%d?!",ss ) ;
       RCsli[ss] = rrcol ;
     }

     if( verb > 1 ){
       float avg=0.0f ;
       ININFO_message(
        "REML setup finished: matrix rows=%d cols=%d; %d*%d cases; total CPU=%.2f Elapsed=%.2f",
        ntime,nrega,RCsli[0]->nset,nsli,COX_cpu_time(),COX_clock_time()) ;
       for( ss=0 ; ss < nsli ; ss++ ) avg += RCsli[ss]->avglen ;
       avg /= nsli ; ININFO_message(" average case bandwidth = %.2f",avg) ;
     }
     MEMORY_CHECK ;

   } else {  /* just set up the first one (slice #0) */

     rrcol = REML_setup_all( Xsli[0] , tau , nlevab, rhomax,bmax ) ;
     if( rrcol == NULL ) ERROR_exit("REML setup fails?!" ) ;
     RCsli[0] = rrcol ;

     if( verb > 1 )
       ININFO_message(
        "REML setup #0 finished: matrix rows=%d cols=%d; %d cases; total CPU=%.2f Elapsed=%.2f",
        ntime,nrega,RCsli[0]->nset,COX_cpu_time(),COX_clock_time()) ;
     MEMORY_CHECK ;

   }

   izero = RCsli[0]->izero ;  /* index of (a=0,b=0) case */

   /***------------- loop over voxels, find best REML values ------------***/

   vstep = (verb && nvox > 999) ? nvox/50 : 0 ;

   if( aim == NULL && !abfixed ){ /*--- if don't have (a,b) via -ABfile ---*/

     aim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     aim->dx = dx ; aim->dy = dy ; aim->dz = dz ; aar = MRI_FLOAT_PTR(aim) ;
     bim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     bim->dx = dx ; bim->dy = dy ; bim->dz = dz ; bar = MRI_FLOAT_PTR(bim) ;
     rim = mri_new_vol( nx,ny,nz , MRI_float ) ;
     rim->dx = dx ; rim->dy = dy ; rim->dz = dz ; rar = MRI_FLOAT_PTR(rim) ;

     if( vstep ) fprintf(stderr,"++ REML voxel loop: ") ;

  if( maxthr <= 1 ){   /**--------- serial computation (no threads) ---------**/
    int ss,rv,vv,ssold,ii,kbest ;
    int   na = RCsli[0]->na , nb = RCsli[0]->nb , nab = (na+1)*(nb+1) ;
    int   nws = nab + 7*(2*niv+32) + 32 ;
    MTYPE *ws = (MTYPE *)malloc(sizeof(MTYPE)*nws) ;
#ifdef REML_DEBUG
    char *fff ; FILE *fpp=NULL ;
    fff = getenv("REML_DEBUG") ;
    if( fff != NULL ) fpp = fopen( fff , "w" ) ;
#endif

     for( ss=-1,rv=vv=0 ; vv < nvox ; vv++ ){    /* this will take a long time */
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       if( inset_mrv != NULL ){ /* 05 Nov 2008 */
         VECTIM_extract( inset_mrv , rv , iv ) ; rv++ ;
       } else
         (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
       ssold = ss ; ss = vv / nsliper ;  /* slice index in Xsli and RCsli */
       if( usetemp_rcol && ss > ssold && ssold >= 0 )  /* purge to disk */
         reml_collection_save( RCsli[ssold] ) ;
       if( RCsli[ss] == NULL ){                   /* create this now */
         if( verb > 1 && vstep ) fprintf(stderr,"+") ;
         RCsli[ss] = REML_setup_all( Xsli[ss] , tau , nlevab, rhomax,bmax ) ;
         if( RCsli[ss] == NULL ) ERROR_exit("REML setup fails for ss=%d",ss) ;
       }
       kbest = REML_find_best_case( &y , RCsli[ss] , nws,ws ) ;
       aar[vv] = RCsli[ss]->rs[kbest]->rho ;
       bar[vv] = RCsli[ss]->rs[kbest]->barm ;
       rar[vv] = ws[0] ;

#ifdef REML_DEBUG
       if( fpp != NULL ){
         int qq ;
         fprintf(fpp,"%d ",vv) ;
         fprintf(fpp," y=") ;
         for( qq=0 ; qq < ntime ; qq++ ) fprintf(fpp," %g",y.elts[qq]) ;
         fprintf(fpp,"  R=") ;
         for( qq=1 ; qq <= nab ; qq++ ) fprintf(fpp," %g",ws[qq]) ;
         fprintf(fpp,"\n") ;
       }
#endif

     } /* end of REML loop over voxels */

     free(ws) ;
#ifdef REML_DEBUG
     if( fpp != NULL ) fclose(fpp) ;
#endif

  } else {  /**---------------- Parallelized (not paralyzed) ----------------**/

    int *vvar ;
    int   na = RCsli[0]->na , nb = RCsli[0]->nb , nab = (na+1)*(nb+1) ;
    int   nws = nab + 7*(2*niv+32) + 32 ;
#ifdef REML_DEBUG
    char *fff ; FILE *fpp=NULL ;
    fff = getenv("REML_DEBUG") ;
    if( fff != NULL ) fpp = fopen( fff , "w" ) ;
#endif

    vvar = (int *)malloc(sizeof(int)*nmask) ;
    for( vv=ii=0 ; vv < nvox ; vv++ ) if( INMASK(vv) ) vvar[ii++] = vv ;
    if( vstep ) fprintf(stderr,"start %d OpenMP threads",maxthr) ;

#ifdef USE_OMP
#pragma omp parallel
  {
    int ss,vv,rv,ii,kbest , ithr ;
    float *iv ; vector y ;  /* private arrays for each thread */
    MTYPE *ws ;

  AFNI_OMP_START ;

#pragma omp critical (MALLOC)
 {
   iv   = (float *)malloc(sizeof(float)*(niv+1)) ;
   vector_initialize(&y) ; vector_create_noinit(ntime,&y) ;
   ws = (MTYPE *)malloc(sizeof(MTYPE)*nws) ;
 }
   ithr = omp_get_thread_num() ;

#pragma omp for
     for( rv=0 ; rv < nmask ; rv++ ){
       vv = vvar[rv] ;
#pragma omp critical (MEMCPY)
 {
       if( inset_mrv != NULL ){
         VECTIM_extract( inset_mrv , rv , iv ) ;
       } else
         (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
 }
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
       ss = vv / nsliper ;  /* slice index in Xsli and RCsli */
       if( RCsli[ss] == NULL )
         ERROR_exit("NULL slice setup inside OpenMP loop!!!") ;
       kbest = REML_find_best_case( &y , RCsli[ss] , nws,ws ) ;  /* the work */
       aar[vv] = RCsli[ss]->rs[kbest]->rho ;
       bar[vv] = RCsli[ss]->rs[kbest]->barm ;
       rar[vv] = ws[0] ;

#ifdef REML_DEBUG
#pragma omp critical (FPP)
     { if( fpp != NULL ){
         int qq ;
         fprintf(fpp,"%d ",vv) ;
         fprintf(fpp," y=") ;
         for( qq=0 ; qq < ntime ; qq++ ) fprintf(fpp," %g",y.elts[qq]) ;
         fprintf(fpp,"  R=") ;
         for( qq=1 ; qq <= nab ; qq++ ) fprintf(fpp," %g",ws[qq]) ;
         fprintf(fpp,"\n") ;
       } }
#endif

     } /* end of REML loop over voxels */

#pragma omp critical (MALLOC)
   { free(ws) ; free(iv) ; vector_destroy(&y) ; } /* destroy private copies */

  AFNI_OMP_END ;
  } /* end OpenMP */
#else
  ERROR_exit("This code should never be executed!!!") ;
#endif
    free(vvar) ;
#ifdef REML_DEBUG
    if( fpp != NULL ) fclose(fpp) ;
#endif
  }

     if( vstep ) fprintf(stderr,"\n") ;
     if( usetemp_rcol ) reml_collection_save( RCsli[nsli-1] ) ;  /* purge to disk */

     /*----- median filter (a,b)? -----*/

     if( do_mfilt ){
       MRI_IMAGE *afilt , *bfilt ;
       if( verb > 1 )
         ININFO_message("Median filtering best fit ARMA parameters") ;
       afilt = mri_medianfilter( aim , mfilt_radius , mask , 0 ) ;
       bfilt = mri_medianfilter( bim , mfilt_radius , mask , 0 ) ;
       if( afilt == NULL || bfilt == NULL ){
         WARNING_message("Median filter failed?! This is weird.") ;
         mri_free(afilt) ; mri_free(bfilt) ;
       } else {
         mri_free(aim) ; aim = afilt ; aar = MRI_FLOAT_PTR(aim) ;
         mri_free(bim) ; bim = bfilt ; bar = MRI_FLOAT_PTR(bim) ;
       }
     }

     if( verb )
       ININFO_message(
         "ARMA voxel parameters estimated: total CPU=%.2f Elapsed=%.2f",
         COX_cpu_time(),COX_clock_time() ) ;
     MEMORY_CHECK ;

   } /***** end of REML estimation *****/

   /*------- at this point, aim and bim contain the (a,b) parameters -------*/
   /*------- (either from -ABfile or from REML loop just done above) -------*/

   /*-- set up indexing and labels needed for bucket dataset creation --*/

   if( (do_buckt || do_eglt) && glt_num > 0 ){
     int ibot ;
STATUS("setting up glt_ind") ;
     glt_ind = (GLT_index **)calloc( sizeof(GLT_index *) , glt_num ) ;

     if( do_glt ){
       glt_ind[0] = create_GLT_index( 0, glt_mat[0]->rows,
                                      0, 0, 1 , do_rstat , glt_lab[0] ) ;
       kk = glt_ind[0]->ivtop + 1 ; ibot = 1 ;
     } else {
       kk = 0 ; ibot = 0 ;
     }
     for( ii=ibot ; ii < glt_num ; ii++ ){
STATUS(" creating glt_ind") ;
       glt_ind[ii] = create_GLT_index( kk, glt_mat[ii]->rows ,
                                       1 , do_tstat ,
                                           do_fstat , do_rstat , glt_lab[ii] ) ;
       if( glt_ind[ii] == NULL ) ERROR_exit("Can't create GLT_index[%d]!?",ii) ;
       kk = glt_ind[ii]->ivtop + 1 ;
     }
     nbuckt = glt_ind[glt_num-1]->ivtop + 1 ;  /* number of sub-bricks */
     if( do_eglt ){
       if( do_glt )
         neglt = glt_ind[glt_num-1]->ivtop - glt_ind[oglt_num-1]->ivtop ;
       else
         neglt = glt_ind[glt_num-1]->ivtop + 1 ;
     }
   }

   /*----- now, use these values to compute the Generalized Least  -----*/
   /*----- Squares (GLSQ) solution at each voxel, and save results -----*/

   Rbeta_dset = create_float_dataset( inset , nbetaset , Rbeta_prefix,1 , &Rbeta_fn,&Rbeta_fp ) ;
   if( Rbeta_dset != NULL && beta_lab != NULL ){
STATUS("labelizing Rbeta") ;
     for( ii=0 ; ii < nbetaset ; ii++ )
       EDIT_BRICK_LABEL( Rbeta_dset , ii , beta_lab[betaset[ii]] ) ;
   }

   Rvar_dset  = create_float_dataset( inset , 5    , Rvar_prefix,1 , NULL,NULL ) ;
   if( Rvar_dset != NULL ){
     float abar[3] ;
STATUS("labelizing Rvar") ;
     EDIT_BRICK_LABEL( Rvar_dset , 0 , "a" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 1 , "b" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 2 , "lam" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 3 , "StDev" ) ;
     EDIT_BRICK_LABEL( Rvar_dset , 4 , "-LogLik") ;
     abar[0] = rhomax ; abar[1] = bmax ; abar[2] = (float)nlevab ;
     THD_set_float_atr( Rvar_dset->dblk , "REMLFIT_abmax" , 3 , abar ) ;
   }

   Rfitts_dset = create_float_dataset( inset , nfull, Rfitts_prefix,0 , &Rfitts_fn,&Rfitts_fp ) ;
   Rerrts_dset = create_float_dataset( inset , nfull, Rerrts_prefix,0 , &Rerrts_fn,&Rerrts_fp ) ;
   Rwherr_dset = create_float_dataset( inset , nfull, Rwherr_prefix,0 , &Rwherr_fn,&Rwherr_fp ) ;

   Rbuckt_dset = create_float_dataset( inset , nbuckt,Rbuckt_prefix,1 , &Rbuckt_fn,&Rbuckt_fp ) ;
   if( Rbuckt_dset != NULL ){
     int nr ;
STATUS("setting up Rbuckt") ;
     for( ii=0 ; ii < glt_num ; ii++ ){
       if( glt_ind[ii] == NULL ) continue ;
       nr = glt_ind[ii]->nrow ;
       if( glt_ind[ii]->beta_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->beta_ind[jj] ,
                                           glt_ind[ii]->beta_lab[jj]  ) ;
           EDIT_BRICK_TO_NOSTAT( Rbuckt_dset , glt_ind[ii]->beta_ind[jj] ) ;
         }
       }
       if( glt_ind[ii]->ttst_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->ttst_ind[jj] ,
                                           glt_ind[ii]->ttst_lab[jj]  ) ;
           EDIT_BRICK_TO_FITT( Rbuckt_dset , glt_ind[ii]->ttst_ind[jj] , ddof ) ;
         }
       }
       if( glt_ind[ii]->ftst_ind >= 0 ){
         EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->ftst_ind ,
                                         glt_ind[ii]->ftst_lab  ) ;
         EDIT_BRICK_TO_FIFT( Rbuckt_dset , glt_ind[ii]->ftst_ind , nr , ddof ) ;
       }
       if( glt_ind[ii]->rtst_ind >= 0 ){
         EDIT_BRICK_LABEL( Rbuckt_dset , glt_ind[ii]->rtst_ind ,
                                         glt_ind[ii]->rtst_lab  ) ;
         EDIT_BRICK_TO_FIBT( Rbuckt_dset , glt_ind[ii]->rtst_ind, 0.5*nr,0.5*ddof );
       }
     }
   }

   Rglt_dset = create_float_dataset( inset , neglt , Rglt_prefix,1 , &Rglt_fn,&Rglt_fp ) ;
   if( Rglt_dset != NULL ){
     int nr , isub = glt_ind[oglt_num]->ivbot ;
STATUS("setting up Rglt") ;
     for( ii=oglt_num ; ii < glt_num ; ii++ ){
       if( glt_ind[ii] == NULL ) continue ;  /* should not happen! */
       nr = glt_ind[ii]->nrow ;
       if( glt_ind[ii]->beta_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Rglt_dset , glt_ind[ii]->beta_ind[jj]-isub ,
                                           glt_ind[ii]->beta_lab[jj]  ) ;
           EDIT_BRICK_TO_NOSTAT( Rglt_dset , glt_ind[ii]->beta_ind[jj]-isub ) ;
         }
       }
       if( glt_ind[ii]->ttst_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Rglt_dset , glt_ind[ii]->ttst_ind[jj]-isub ,
                                           glt_ind[ii]->ttst_lab[jj]  ) ;
           EDIT_BRICK_TO_FITT( Rglt_dset , glt_ind[ii]->ttst_ind[jj]-isub , ddof ) ;
         }
       }
       if( glt_ind[ii]->ftst_ind >= 0 ){
         EDIT_BRICK_LABEL( Rglt_dset , glt_ind[ii]->ftst_ind-isub ,
                                         glt_ind[ii]->ftst_lab  ) ;
         EDIT_BRICK_TO_FIFT( Rglt_dset , glt_ind[ii]->ftst_ind-isub , nr , ddof ) ;
       }
       if( glt_ind[ii]->rtst_ind >= 0 ){
         EDIT_BRICK_LABEL( Rglt_dset , glt_ind[ii]->rtst_ind-isub ,
                                         glt_ind[ii]->rtst_lab  ) ;
         EDIT_BRICK_TO_FIBT( Rglt_dset , glt_ind[ii]->rtst_ind-isub, 0.5*nr,0.5*ddof );
       }
     }
   }

   do_Rstuff = (Rbeta_dset  != NULL) || (Rvar_dset   != NULL) ||
               (Rfitts_dset != NULL) || (Rbuckt_dset != NULL) ||
               (Rerrts_dset != NULL) || (Rwherr_dset != NULL) ||
               (Rglt_dset   != NULL)                             ;

   /*---------------- and do the second (GLSQ) voxel loop ----------------*/

   if( do_Rstuff ){
     MTYPE *bbar[7] , bbsumq ;  /* workspace for REML_func() [24 Jun 2009] */
     MTYPE *bb1 , *bb2 , *bb3 , *bb4 , *bb5 , *bb6 , *bb7 ;
     vector qq5 ;

     for( ii=0 ; ii < 7 ; ii++ )
       bbar[ii] = (MTYPE *)malloc(sizeof(MTYPE)*(2*ntime+66)) ;
     bb1 = bbar[0] ; bb2 = bbar[1] ; bb3 = bbar[2] ;
     bb4 = bbar[3] ; bb5 = bbar[4] ; bb6 = bbar[5] ; bb7 = bbar[6] ;
     vector_initialize(&qq5) ; vector_create_noinit(nrega,&qq5) ;

     if( vstep ) fprintf(stderr,"++ GLSQ voxel loop: ") ;
     for( ss=-1,rv=vv=0 ; vv < nvox ; vv++ ){
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       if( inset_mrv != NULL ){ /* 05 Nov 2008 */
         VECTIM_extract( inset_mrv , rv , iv ) ; rv++ ;
       } else
         (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
       memcpy( jv , iv , sizeof(float)*nfull ) ;         /* copy of data */
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = iv[goodlist[ii]] ;

       ssold = ss ; ss = vv / nsliper ;  /* slice index in Xsli and RCsli */

       /*=== If at a new slice:
               remove REML setups (except a=b=0 case) for previous slice;
               create new slice matrices, if they don't exist already, OR
               get new slice matrices back from disk, if they were purged
               earlier; add GLT setups to the new slice.
             The purpose of doing it this way is to save memory space.  ======*/

       if( ss > ssold ){
         if( ssold >= 0 ) reml_collection_destroy( RCsli[ssold] , 1 ) ;
         if( RCsli[ss] == NULL ){                   /* create this now */
           if( verb > 1 && vstep ) fprintf(stderr,"+") ;
           RCsli[ss] = REML_setup_all( Xsli[ss] , tau , nlevab, rhomax,bmax ) ;
           if( RCsli[ss] == NULL ) ERROR_exit("REML setup fails for ss=%d",ss) ;
         } else if( RC_SAVED(RCsli[ss]) ){          /* restore from disk */
           if( verb > 1 && vstep ) fprintf(stderr,"+") ;
           reml_collection_restore( RCsli[ss] ) ;
         }
         for( kk=0 ; kk < glt_num ; kk++ )
           REML_add_glt_to_all( RCsli[ss] , glt_mat[kk] ) ;
       }

       if( abfixed )  /* special case */
         jj = 1 ;
       else
         jj = IAB(RCsli[ss],aar[vv],bar[vv]) ;  /* closest point in the (a,b) grid */

       if( RCsli[ss]->rs[jj] == NULL ){       /* try to fix up this oversight */
         int   ia  = jj % (1+RCsli[ss]->na);
         int   ib  = jj / (1+RCsli[ss]->na);
         MTYPE aaa = RCsli[ss]->abot + ia * RCsli[ss]->da;
         MTYPE bbb = RCsli[ss]->bbot + ib * RCsli[ss]->db;

         RCsli[ss]->rs[jj] = REML_setup_one( Xsli[ss] , tau , aaa,bbb ) ;
         for( kk=0 ; kk < glt_num ; kk++ )
           REML_add_glt_to_one( RCsli[ss]->rs[jj] , glt_mat[kk] ) ;
       }
       if( RCsli[ss]->rs[jj] == NULL ){ /* should never happen */
         ERROR_message("bad REML! voxel #%d (%d,%d,%d) has a=%.3f b=%.3f lam=%.3f jj=%d",
                         vv, DSET_index_to_ix(inset,vv) ,
                             DSET_index_to_jy(inset,vv) ,
                             DSET_index_to_kz(inset,vv) ,
                         aar[vv],bar[vv],LAMBDA(aar[vv],bar[vv]),jj) ;
       } else {

         /*--- do the fitting; various results are in the bb? vectors:
                bb5 = estimated betas       (nrega)
                bb6 = fitted model          (ntime)
                bb7 = whitened fitted model (ntime) [not used below]
                bb1 = whitened data         (ntime) [not used below]
                bb2 = whitened residuals    (ntime)
                      (sum of squares of bb2 = bbsumq = noise variance) ------*/

         (void)REML_func( &y , RCsli[ss]->rs[jj] , RCsli[ss]->X , RCsli[ss]->Xs ,
                          bbar , &bbsumq ) ;

         /*--------- scatter the results to various datasets ---------*/

         if( Rfitts_dset != NULL ){  /* note that iv still contains original data */
           for( ii=0 ; ii < ntime ; ii++ ) iv[goodlist[ii]] = bb6[ii] ;
           save_series( vv , Rfitts_dset , nfull , iv , Rfitts_fp ) ;
         }

         if( Rerrts_dset != NULL ){  /* jv contains copy of original data */
           if( ntime < nfull ) for( ii=0 ; ii < nfull ; ii++ ) iv[ii] = 0.0f ;
           for( ii=0 ; ii < ntime ; ii++ )
             iv[goodlist[ii]] = jv[goodlist[ii]] - bb6[ii] ;
           save_series( vv , Rerrts_dset , nfull , iv , Rerrts_fp ) ;
         }

         if( Rwherr_dset != NULL ){  /* note there is no Owherr dataset! */
           if( ntime < nfull ) for( ii=0 ; ii < nfull ; ii++ ) iv[ii] = 0.0f ;
           for( ii=0 ; ii < ntime ; ii++ )
             iv[goodlist[ii]] = bb2[ii] ;
           save_series( vv , Rwherr_dset , nfull , iv , Rwherr_fp ) ;
         }

         if( Rbeta_dset != NULL ){
           for( ii=0 ; ii < nbetaset ; ii++ ) iv[ii] = bb5[betaset[ii]] ;
           save_series( vv , Rbeta_dset , nbetaset , iv , Rbeta_fp ) ;
         }

         if( Rvar_dset != NULL ){
           iv[0] = RCsli[ss]->rs[jj]->rho ; iv[1] = RCsli[ss]->rs[jj]->barm ;
           iv[2] = RCsli[ss]->rs[jj]->lam ; iv[3] = sqrt( bbsumq / ddof ) ;
           iv[4] = (rar != NULL) ? rar[vv] : 0.0f ;
           save_series( vv , Rvar_dset , 5 , iv , Rvar_fp ) ;
         }

         memcpy( qq5.elts , bb5 , sizeof(MTYPE)*nrega ) ; /* 24 Jun 2009 */

         if( glt_num > 0 && Rbuckt_dset != NULL ){
           MTYPE gv ; GLT_index *gin ; int nr ;
           memset( iv , 0 , sizeof(float)*niv ) ;
           for( kk=0 ; kk < glt_num ; kk++ ){
             gin = glt_ind[kk] ; if( gin == NULL ) continue ; /* skip this'n */
             nr = gin->nrow ;
             gv = REML_compute_gltstat( &y , &qq5 , bbsumq ,
                                        RCsli[ss]->rs[jj], RCsli[ss]->rs[jj]->glt[kk],
                                        glt_mat[kk] , glt_smat[kk] ,
                                        RCsli[ss]->X , RCsli[ss]->Xs        ) ;
             if( gin->ftst_ind >= 0 ) iv[gin->ftst_ind] = gv ;
             if( gin->rtst_ind >= 0 ) iv[gin->rtst_ind] = betaR ;
             if( gin->beta_ind != NULL && betaG->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->beta_ind[ii]] = betaG->elts[ii] ;
               }
             }
             if( gin->ttst_ind != NULL && betaT->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->ttst_ind[ii]] = betaT->elts[ii] ;
               }
             }
           }
           save_series( vv , Rbuckt_dset , nbuckt , iv , Rbuckt_fp ) ;
         }

         if( Rglt_dset != NULL ){
           MTYPE gv ; GLT_index *gin ; int nr , isub ;
           memset( iv , 0 , sizeof(float)*niv ) ;
           isub = glt_ind[oglt_num]->ivbot ;  /* first index in first extra GLT */
           for( kk=oglt_num ; kk < glt_num ; kk++ ){
             gin = glt_ind[kk] ; if( gin == NULL ) continue ; /* skip this'n */
             nr = gin->nrow ;
             gv = REML_compute_gltstat( &y , &qq5 , bbsumq ,
                                        RCsli[ss]->rs[jj], RCsli[ss]->rs[jj]->glt[kk],
                                        glt_mat[kk] , glt_smat[kk] ,
                                        RCsli[ss]->X , RCsli[ss]->Xs        ) ;
             if( gin->ftst_ind >= 0 ) iv[gin->ftst_ind-isub] = gv ;
             if( gin->rtst_ind >= 0 ) iv[gin->rtst_ind-isub] = betaR ;
             if( gin->beta_ind != NULL && betaG->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->beta_ind[ii]-isub] = betaG->elts[ii] ;
               }
             }
             if( gin->ttst_ind != NULL && betaT->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->ttst_ind[ii]-isub] = betaT->elts[ii] ;
               }
             }
           }
           save_series( vv , Rglt_dset , neglt , iv , Rglt_fp ) ;
         }

       }
     } /* end of voxel loop */

     reml_collection_destroy( RCsli[nsli-1] , 1 ) ;
     for( ii=0 ; ii < 7 ; ii++ ) free(bbar[ii]) ;
     vector_destroy(&qq5) ;

     if( vstep ) fprintf(stderr,"\n") ;
     if( verb )
       ININFO_message("GLSQ regression done: total CPU=%.2f Elapsed=%.2f",
                      COX_cpu_time(),COX_clock_time() ) ;
     MEMORY_CHECK ;
   }

   /* macro to add FDR curves to a dataset and print a message */

#undef  FDRIZE
#define FDRIZE(ds)                                                            \
 do{ if( do_FDR || !AFNI_noenv("AFNI_AUTOMATIC_FDR") ){                       \
       ii = THD_count_fdrwork(ds) ;                                           \
       if( verb > 1 && ii*nmask > 666666 )                                    \
         INFO_message("creating FDR curves in dataset %s",DSET_FILECODE(ds)); \
       ii = THD_create_all_fdrcurves(ds) ;                                    \
     } else {                                                                 \
       ii = 0 ;                                                               \
     }                                                                        \
     if( ii > 0 && verb > 1 )                                                 \
       ININFO_message("Added %d FDR curve%s to dataset %s",                   \
                      ii , (ii==1)?"":"s" , DSET_FILECODE(ds) ) ;             \
 } while(0)

   /*----------------- write output REML datasets to disk -----------------*/

   if( Rbeta_dset != NULL ){
     populate_dataset( Rbeta_dset , mask , Rbeta_fn,Rbeta_fp ) ;
     DSET_write(Rbeta_dset); WROTE_DSET(Rbeta_dset); DSET_deletepp(Rbeta_dset);
     MEMORY_CHECK ;
   }
   if( Rvar_dset  != NULL ){
     populate_dataset( Rvar_dset , mask , Rvar_fn,Rvar_fp ) ;
     DSET_write(Rvar_dset); WROTE_DSET(Rvar_dset); DSET_deletepp(Rvar_dset);
     MEMORY_CHECK ;
   }
   if( Rfitts_dset != NULL ){
     populate_dataset( Rfitts_dset , mask , Rfitts_fn,Rfitts_fp ) ;
     DSET_write(Rfitts_dset); WROTE_DSET(Rfitts_dset); DSET_deletepp(Rfitts_dset);
     MEMORY_CHECK ;
   }
   if( Rerrts_dset != NULL ){
     populate_dataset( Rerrts_dset , mask , Rerrts_fn,Rerrts_fp ) ;
     DSET_write(Rerrts_dset); WROTE_DSET(Rerrts_dset); DSET_deletepp(Rerrts_dset);
     MEMORY_CHECK ;
   }
   if( Rwherr_dset != NULL ){
     populate_dataset( Rwherr_dset , mask , Rwherr_fn,Rwherr_fp ) ;
     DSET_write(Rwherr_dset); WROTE_DSET(Rwherr_dset); DSET_deletepp(Rwherr_dset);
     MEMORY_CHECK ;
   }
   if( Rbuckt_dset != NULL ){
     populate_dataset( Rbuckt_dset , mask , Rbuckt_fn,Rbuckt_fp ) ;
     FDRIZE(Rbuckt_dset) ;
     DSET_write(Rbuckt_dset); WROTE_DSET(Rbuckt_dset); DSET_deletepp(Rbuckt_dset);
     MEMORY_CHECK ;
   }
   if( Rglt_dset != NULL ){
     populate_dataset( Rglt_dset , mask , Rglt_fn,Rglt_fp ) ;
     FDRIZE(Rglt_dset) ;
     DSET_write(Rglt_dset); WROTE_DSET(Rglt_dset); DSET_deletepp(Rglt_dset);
     MEMORY_CHECK ;
   }

   /*---------------------- create OLSQ outputs, if any ----------------------*/

   Obeta_dset = create_float_dataset( inset , nbetaset , Obeta_prefix,1 , &Obeta_fn,&Obeta_fp ) ;
   if( Obeta_dset != NULL && beta_lab != NULL ){
     for( ii=0 ; ii < nbetaset ; ii++ )
       EDIT_BRICK_LABEL( Obeta_dset , ii , beta_lab[betaset[ii]] ) ;
   }

   Ovar_dset  = create_float_dataset( inset , 1 , Ovar_prefix,1  , NULL,NULL ) ;
   if( Ovar_dset != NULL ){
     EDIT_BRICK_LABEL( Ovar_dset , 0 , "StDev" ) ;
   }

   Ofitts_dset = create_float_dataset( inset , nfull, Ofitts_prefix,0 , &Ofitts_fn,&Ofitts_fp ) ;
   Oerrts_dset = create_float_dataset( inset , nfull, Oerrts_prefix,0 , &Oerrts_fn,&Oerrts_fp ) ;

   Obuckt_dset = create_float_dataset( inset , nbuckt,Obuckt_prefix,1 , &Obuckt_fn,&Obuckt_fp ) ;
   if( Obuckt_dset != NULL ){
     int nr ;
     for( ii=0 ; ii < glt_num ; ii++ ){
       if( glt_ind[ii] == NULL ) continue ;
       nr = glt_ind[ii]->nrow ;
       if( glt_ind[ii]->beta_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->beta_ind[jj] ,
                                           glt_ind[ii]->beta_lab[jj]  ) ;
           EDIT_BRICK_TO_NOSTAT( Obuckt_dset , glt_ind[ii]->beta_ind[jj] ) ;
         }
       }
       if( glt_ind[ii]->ttst_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->ttst_ind[jj] ,
                                           glt_ind[ii]->ttst_lab[jj]  ) ;
           EDIT_BRICK_TO_FITT( Obuckt_dset , glt_ind[ii]->ttst_ind[jj] , ddof ) ;
         }
       }
       if( glt_ind[ii]->ftst_ind >= 0 ){
         EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->ftst_ind ,
                                         glt_ind[ii]->ftst_lab  ) ;
         EDIT_BRICK_TO_FIFT( Obuckt_dset , glt_ind[ii]->ftst_ind , nr , ddof ) ;
       }
       if( glt_ind[ii]->rtst_ind >= 0 ){
         EDIT_BRICK_LABEL( Obuckt_dset , glt_ind[ii]->rtst_ind ,
                                         glt_ind[ii]->rtst_lab  ) ;
         EDIT_BRICK_TO_FIBT( Obuckt_dset , glt_ind[ii]->rtst_ind, 0.5*nr,0.5*ddof );
       }
     }
   }

   Oglt_dset = create_float_dataset( inset , neglt , Oglt_prefix,1 , &Oglt_fn,&Oglt_fp ) ;
   if( Oglt_dset != NULL ){
     int nr , isub = glt_ind[oglt_num]->ivbot ;
     for( ii=oglt_num ; ii < glt_num ; ii++ ){
       if( glt_ind[ii] == NULL ) continue ;  /* should not happen! */
       nr = glt_ind[ii]->nrow ;
       if( glt_ind[ii]->beta_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Oglt_dset , glt_ind[ii]->beta_ind[jj]-isub ,
                                           glt_ind[ii]->beta_lab[jj]  ) ;
           EDIT_BRICK_TO_NOSTAT( Oglt_dset , glt_ind[ii]->beta_ind[jj]-isub ) ;
         }
       }
       if( glt_ind[ii]->ttst_ind != NULL ){
         for( jj=0 ; jj < nr ; jj++ ){
           EDIT_BRICK_LABEL( Oglt_dset , glt_ind[ii]->ttst_ind[jj]-isub ,
                                           glt_ind[ii]->ttst_lab[jj]  ) ;
           EDIT_BRICK_TO_FITT( Oglt_dset , glt_ind[ii]->ttst_ind[jj]-isub , ddof ) ;
         }
       }
       if( glt_ind[ii]->ftst_ind >= 0 ){
         EDIT_BRICK_LABEL( Oglt_dset , glt_ind[ii]->ftst_ind-isub ,
                                         glt_ind[ii]->ftst_lab  ) ;
         EDIT_BRICK_TO_FIFT( Oglt_dset , glt_ind[ii]->ftst_ind-isub , nr , ddof ) ;
       }
       if( glt_ind[ii]->rtst_ind >= 0 ){
         EDIT_BRICK_LABEL( Oglt_dset , glt_ind[ii]->rtst_ind-isub ,
                                         glt_ind[ii]->rtst_lab  ) ;
         EDIT_BRICK_TO_FIBT( Oglt_dset , glt_ind[ii]->rtst_ind-isub, 0.5*nr,0.5*ddof );
       }
     }
   }

   do_Ostuff = (Obeta_dset  != NULL) || (Ovar_dset   != NULL) ||
               (Ofitts_dset != NULL) || (Obuckt_dset != NULL) ||
               (Oerrts_dset != NULL) || (Oglt_dset   != NULL)   ;

   /*---------------- and do the third (OLSQ) voxel loop ----------------*/

   if( do_Ostuff ){
     MTYPE *bbar[7] , bbsumq ;  /* workspace for REML_func() [24 Jun 2009] */
     MTYPE *bb1 , *bb2 , *bb3 , *bb4 , *bb5 , *bb6 , *bb7 ;
     vector qq5 ;

     for( ii=0 ; ii < 7 ; ii++ )
       bbar[ii] = (MTYPE *)malloc(sizeof(MTYPE)*(2*ntime+66)) ;
     bb1 = bbar[0] ; bb2 = bbar[1] ; bb3 = bbar[2] ;
     bb4 = bbar[3] ; bb5 = bbar[4] ; bb6 = bbar[5] ; bb7 = bbar[6] ;
     vector_initialize(&qq5) ; vector_create_noinit(nrega,&qq5) ;

     if( vstep ) fprintf(stderr,"++ OLSQ voxel loop: ") ;
     for( rv=vv=0 ; vv < nvox ; vv++ ){
       if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
       if( !INMASK(vv) ) continue ;
       if( inset_mrv != NULL ){ /* 05 Nov 2008 */
         VECTIM_extract( inset_mrv , rv , iv ) ; rv++ ;
       } else
         (void)THD_extract_array( vv , inset , 0 , iv ) ;  /* data vector */
       memcpy( jv , iv , sizeof(float)*nfull ) ;
       for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;

       jj = izero ;                       /* OLSQ (a,b)=(0,0) case */
       ss = vv / nsliper ;

       if( RCsli[ss]->rs[jj] == NULL ){       /* try to fix up this oversight */
         int   ia  = jj % (1+RCsli[ss]->na);
         int   ib  = jj / (1+RCsli[ss]->na);
         MTYPE aaa = RCsli[ss]->abot + ia * RCsli[ss]->da;
         MTYPE bbb = RCsli[ss]->bbot + ib * RCsli[ss]->db;

         RCsli[ss]->rs[jj] = REML_setup_one( Xsli[ss] , tau , aaa,bbb ) ;
         for( kk=0 ; kk < glt_num ; kk++ )
           REML_add_glt_to_one( RCsli[ss]->rs[jj] , glt_mat[kk]) ;
       }
       if( RCsli[ss]->rs[jj] == NULL ){ /* should never happen */
         ERROR_message("bad OLSQ! voxel #%d (%d,%d,%d) jj=%d",
                         vv, DSET_index_to_ix(inset,vv) ,
                             DSET_index_to_jy(inset,vv) ,
                             DSET_index_to_kz(inset,vv) , jj ) ;
       } else {

         (void)REML_func( &y , RCsli[ss]->rs[jj] , RCsli[ss]->X , RCsli[ss]->Xs ,
                          bbar , &bbsumq ) ;

         if( Ofitts_dset != NULL ){
           for( ii=0 ; ii < ntime ; ii++ ) iv[goodlist[ii]] = bb6[ii] ;
           save_series( vv , Ofitts_dset , nfull , iv , Ofitts_fp ) ;
         }

         if( Oerrts_dset != NULL ){  /* jv contains copy of original data */
           if( ntime < nfull ) for( ii=0 ; ii < nfull ; ii++ ) iv[ii] = 0.0f ;
           for( ii=0 ; ii < ntime ; ii++ )
             iv[goodlist[ii]] = jv[goodlist[ii]] - bb6[ii] ;
           save_series( vv , Oerrts_dset , nfull , iv , Oerrts_fp ) ;
         }

         if( Obeta_dset != NULL ){
           for( ii=0 ; ii < nbetaset ; ii++ ) iv[ii] = bb5[betaset[ii]] ;
           save_series( vv , Obeta_dset , nbetaset , iv , Obeta_fp ) ;
         }

         if( Ovar_dset != NULL ){
           iv[0] = sqrt( bbsumq / ddof ) ;
           save_series( vv , Ovar_dset , 1 , iv , Ovar_fp ) ;
         }

         memcpy( qq5.elts , bb5 , sizeof(MTYPE)*nrega ) ; /* 24 Jun 2009 */

         if( glt_num > 0 && Obuckt_dset != NULL ){
           MTYPE gv ; GLT_index *gin ; int nr ;
           memset( iv , 0 , sizeof(float)*niv ) ;
           for( kk=0 ; kk < glt_num ; kk++ ){
             gin = glt_ind[kk] ; if( gin == NULL ) continue ; /* skip this'n */
             nr = gin->nrow ;
             gv = REML_compute_gltstat( &y , &qq5 , bbsumq ,
                                        RCsli[ss]->rs[jj], RCsli[ss]->rs[jj]->glt[kk],
                                        glt_mat[kk] , glt_smat[kk] ,
                                        RCsli[ss]->X , RCsli[ss]->Xs        ) ;
             if( gin->ftst_ind >= 0 ) iv[gin->ftst_ind] = gv ;
             if( gin->rtst_ind >= 0 ) iv[gin->rtst_ind] = betaR ;
             if( gin->beta_ind != NULL && betaG->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->beta_ind[ii]] = betaG->elts[ii] ;
               }
             }
             if( gin->ttst_ind != NULL && betaT->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->ttst_ind[ii]] = betaT->elts[ii] ;
               }
             }
           }
           save_series( vv , Obuckt_dset , nbuckt , iv , Obuckt_fp ) ;
         }

         if( Oglt_dset != NULL ){
           MTYPE gv ; GLT_index *gin ; int nr , isub ;
           memset( iv , 0 , sizeof(float)*niv ) ;
           isub = glt_ind[oglt_num]->ivbot ;  /* first index in first extra GLT */
           for( kk=oglt_num ; kk < glt_num ; kk++ ){
             gin = glt_ind[kk] ; if( gin == NULL ) continue ; /* skip this'n */
             nr = gin->nrow ;
             gv = REML_compute_gltstat( &y , &qq5 , bbsumq ,
                                        RCsli[ss]->rs[jj], RCsli[ss]->rs[jj]->glt[kk],
                                        glt_mat[kk] , glt_smat[kk] ,
                                        RCsli[ss]->X , RCsli[ss]->Xs        ) ;
             if( gin->ftst_ind >= 0 ) iv[gin->ftst_ind-isub] = gv ;
             if( gin->rtst_ind >= 0 ) iv[gin->rtst_ind-isub] = betaR ;
             if( gin->beta_ind != NULL && betaG->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->beta_ind[ii]-isub] = betaG->elts[ii] ;
               }
             }
             if( gin->ttst_ind != NULL && betaT->dim >= nr ){
               for( ii=0 ; ii < nr ; ii++ ){
                 iv[gin->ttst_ind[ii]-isub] = betaT->elts[ii] ;
               }
             }
           }
           save_series( vv , Oglt_dset , neglt , iv , Oglt_fp ) ;
         }

       }
     } /* end of voxel loop */

     for( ii=0 ; ii < 7 ; ii++ ) free(bbar[ii]) ;
     vector_destroy(&qq5) ;

     if( vstep ) fprintf(stderr,"\n") ;
     if( verb > 1 )
       ININFO_message("OLSQ regression done: total CPU=%.2f Elapsed=%.2f",
                      COX_cpu_time(),COX_clock_time() ) ;
   }

   /*----------------- done with the input dataset -----------------*/

   /* delete lots of memory that's no longer needed */

   MEMORY_CHECK ;
   if( verb > 1 ) ININFO_message("unloading input dataset and REML matrices");
   if( inset_mrv != NULL ) VECTIM_destroy(inset_mrv) ; /* 05 Nov 2008 */
   DSET_delete(inset) ; free(jv) ; free(iv) ; free(tau) ;
   for( ss=0 ; ss < nsli ; ss++ ){
     reml_collection_destroy(RCsli[ss],0) ; matrix_destroy(Xsli[ss]) ;
   }
   free(RCsli) ; free(Xsli) ;
        if( abset != NULL ) DSET_delete(abset) ;
   else if( aim   != NULL ) { mri_free(aim) ; mri_free(bim) ; }
   KILL_ALL_GLTS ;
   MEMORY_CHECK ;

   /*-------------- write output OLSQ datasets to disk --------------*/

   if( Obeta_dset != NULL ){
     populate_dataset( Obeta_dset , mask , Obeta_fn,Obeta_fp ) ;
     DSET_write(Obeta_dset); WROTE_DSET(Obeta_dset); DSET_deletepp(Obeta_dset);
     MEMORY_CHECK ;
   }
   if( Ovar_dset  != NULL ){
     populate_dataset( Ovar_dset , mask , Ovar_fn,Ovar_fp ) ;
     DSET_write(Ovar_dset); WROTE_DSET(Ovar_dset); DSET_deletepp(Ovar_dset);
     MEMORY_CHECK ;
   }
   if( Ofitts_dset != NULL ){
     populate_dataset( Ofitts_dset , mask , Ofitts_fn,Ofitts_fp ) ;
     DSET_write(Ofitts_dset); WROTE_DSET(Ofitts_dset); DSET_deletepp(Ofitts_dset);
     MEMORY_CHECK ;
   }
   if( Oerrts_dset != NULL ){
     populate_dataset( Oerrts_dset , mask , Oerrts_fn,Oerrts_fp ) ;
     DSET_write(Oerrts_dset); WROTE_DSET(Oerrts_dset); DSET_deletepp(Oerrts_dset);
     MEMORY_CHECK ;
   }
   if( Obuckt_dset != NULL ){
     populate_dataset( Obuckt_dset , mask , Obuckt_fn,Obuckt_fp ) ;
     FDRIZE(Obuckt_dset) ;
     DSET_write(Obuckt_dset); WROTE_DSET(Obuckt_dset); DSET_deletepp(Obuckt_dset);
     MEMORY_CHECK ;
   }
   if( Oglt_dset != NULL ){
     populate_dataset( Oglt_dset , mask , Oglt_fn,Oglt_fp ) ;
     FDRIZE(Oglt_dset) ;
     DSET_write(Oglt_dset); WROTE_DSET(Oglt_dset); DSET_deletepp(Oglt_dset);
     MEMORY_CHECK ;
   }

   /*----------------------------- Free at last ----------------------------*/

   INFO_message("3dREMLfit is all done! total CPU=%.2f Elapsed=%.2f",
                COX_cpu_time() , COX_clock_time() ) ;
   if( gmask != NULL && gmask != mask ) free(gmask) ;  /* 27 Mar 2009 */
   free(mask) ; MEMORY_CHECK ;
#if 0
#ifdef USING_MCW_MALLOC
   if( verb > 4 ) mcw_malloc_dump() ;
#endif
#endif
   exit(0) ;
}
