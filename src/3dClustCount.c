/*** Adapted from 3dClustSim.c ***/

#include "mrilib.h"

#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_NAME_LENGTH  THD_MAX_NAME /* max. string length for file names */
#define MAX_CLUSTER_SIZE 99999        /* max. size of cluster for freq. table */

/*---------------------------------------------------------------------------*/
/* Global data */

static int max_cluster_size = MAX_CLUSTER_SIZE ;

static int nx ;
static int ny ;
static int nz ;
static int nxy ;
static int nxyz ;

static int do_niml = 1 ;
static int do_1D   = 1 ;
static int do_2sid = 1 ;

#define PMAX 0.2

static int   npthr   = 29 ;
static double pthr[] = { 0.10,    0.09,    0.08,    0.07,    0.06,
                                0.05,    0.04,    0.03,    0.02,    0.015,    0.01,
                                0.007,   0.005,   0.003,   0.002,   0.0015,   0.001,
                                0.0007,  0.0005,  0.0003,  0.0002,  0.00015,  0.0001,
                                0.00007, 0.00005, 0.00003, 0.00002, 0.000015, 0.00001 } ;

static int   nathr   = 10 ;
static double athr[] = { 0.10, 0.09, .08, .07, .06, .05, .04, .03, .02, .01 } ;

static int verb = 1 ;
static int nthr = 1 ;

static char *prefix = NULL ;
static int do_final = 0 ;

static int    ndset = 0 ;
static char **fdset = NULL ;

#define do_lohi 0
#define nodec   0

/*---------------------------------------------------------------------------*/

void display_help_menu()
{
  printf(
   "Usage: 3dClustCount [options] dataset1 ... \n"
   "\n"
   "This program takes as input 1 or more datasets, thresholds them at various\n"
   "levels, and counts up the number of clusters of various sizes.  It is\n"
   "adapted from 3dClustSim, but only does the cluster counting functions --\n"
   "where the datasets come from is the user's business.  It is intended for\n"
   "use in a simulation script.\n"
#if 0
   "\n"
   "The input datasets can be in one of two forms.\n"
   "(1) A single sub-brick, which should be coded as a statistic so that\n"
   "    a voxel-wise (AKA uncorrected) 2-sided p-value threshold can be\n"
   "    applied.\n"
   "(2) Two sub-bricks, the first of which is some voxel-wise measurement\n"
   "    of the effect (e.g., percent signal change), and the second of which\n"
   "    is the statistical threshold sub-brick.\n"
   "In case (1), only statistics of the cluster sizes are kept.\n"
   "In case (2), statistics of the effect sub-brick are also kept for the\n"
   "clusters of each size.\n"
#endif
   "\n"
   "-------\n"
   "OPTIONS\n"
   "-------\n"
   " -prefix sss = Use string 'sss' as the prefix of the filename into which\n"
   "               results will be summed.  The actual filename will be\n"
   "               'sss.clustcount.niml'.  If this file already exists, then\n"
   "               the results from the current run will be summed into the\n"
   "               existing results, and the file then re-written.\n"
#if 0
   "\n"
   " -addin aaa  = Also add in the results from file 'aaa.clustcount.niml'\n"
   "               to the cumulating counts.  This option lets you run\n"
   "               multiple 3dClustCount jobs in parallel scripts, then\n"
   "               merge the results.\n"
#endif
   "\n"
   " -final      = If this option is given, then the results will be output\n"
   "               in a format like that used from 3dClustSim -- as 1D and\n"
   "               NIML formatted files with probabilities of various\n"
   "               cluster sizes.\n"
   "               ++ You can use '-final' without any input datasets if\n"
   "                  you want to create the final output files from the\n"
   "                  saved '.clustcount.niml' output file from earlier runs.\n"
   "\n"
   " -quiet      = Don't print out the progress reports, etc.\n"
   "               ++ Put this option first to quiet most informational messages.\n"
   "\n"
   "--------\n"
   "EXAMPLE:\n"
   "-------\n"
   "The steps here are\n"
   "  (a) Create a set of 250 3dGroupInCorr results from a set of 190 subjects,\n"
   "      using 250 randomly located seed locations.  Note the use of '-sendall'\n"
   "      to get the individual subject results -- these are used in the next\n"
   "      step, and are in sub-bricks 2..191 -- the collective 3dGroupInCorr\n"
   "      results (in sub-bricks 0..1) are not actually used here.\n"
   "  (b) For each of these 250 output datasets, create 80 random splittings\n"
   "      into 2 groups of 95 subjects each, and carry out a 2-sample t-test\n"
   "      between these groups.\n"
   "      ++ Note the use of program 2perm to create the random splittings into\n"
   "         files QQ_A and QQ_B, drawn from sub-bricks 2..191 of the ${fred}\n"
   "         datasets.\n"
   "      ++ Note the use of the '[1dcat filename]' construction to specify\n"
   "         which sub-bricks of the ${fred} dataset are used for input to\n"
   "         the '-setX' options of 3dttest++.\n"
   "  (c) Count clusters from the '[1]' sub-brick of the 80 t-test outputs --\n"
   "      the t-statistic sub-brick.\n"
   "      ++  Note the use of a wildcard filename with a sub-brick selector:\n"
   "          'QQ*.HEAD[1]' -- 3dClustCount will do the wildcard expansion\n"
   "          internally, then add the sub-brick selector '[1]' to each expanded\n"
   "          dataset filename.\n"
   "  (d) Produce the final report files for empirical cluster-size thresholds\n"
   "      for 3dGroupInCorr analyses -- rather than rely on 3dClustSim's assumption\n"
   "      of Gaussian-shaped spatial correlation structure.\n"
   "The syntax is C-shell (tcsh), naturally.\n"
   "\n"
   "    \\rm -f ABscat*\n"
   "    3dGroupInCorr -setA A.errts.grpincorr.niml                 \\\n"
   "                  -setB B.errts.grpincorr.niml                  \\\n"
   "                  -labelA A -labelB B -seedrad 5 -nosix -sendall \\\n"
   "                  -batchRAND 250 ABscat\n"
   "    foreach fred ( ABscat*.HEAD )\n"
   "      foreach nnn ( `count -dig 2 0 79` )\n"
   "        2perm -prefix QQ 2 191\n"
   "        3dttest++ -setA ${fred}'[1dcat QQ_A]' \\\n"
   "                  -setB ${fred}'[1dcat QQ_B]' \\\n"
   "                  -no1sam -prefix QQ${nnn}\n"
   "      end\n"
   "      3dClustCount -prefix ABcount 'QQ*.HEAD[1]'\n"
   "      \\rm -f QQ*\n"
   "    end\n"
   "    3dClustCount -final -prefix ABcount\n"
   "    \\rm -f ABscat*\n"
   "\n"
   "--------------------------------\n"
   "---- RW Cox -- August 2012 -----\n"
   "--------------------------------\n"
  ) ;

  PRINT_COMPILE_DATE ;
  exit(0);
}

/*---------------------------------------------------------------------------*/
/* Routine to initialize the input options (values are in global variables). */

void get_options( int argc , char **argv )
{
  int nopt=1 , ii ;
  int nexp,iex,didex ; char **fexp ;

  PUTENV("AFNI_GLOB_SELECTORS","YES") ;

  while( nopt < argc && argv[nopt][0] == '-' ){

    /*-----   -input   -----*/

    if( strcasecmp(argv[nopt],"-input") == 0 ){

      if( ++nopt >= argc ) ERROR_exit("need argument after '%s'",argv[nopt-1]) ;

      for( ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){
        if( HAS_WILDCARD(argv[nopt]) ){
          MCW_file_expand( 1,argv+nopt, &nexp,&fexp ) ; didex = 1 ;
          if( nexp < 1 ){
            WARNING_message("wildcard name '%s' doesn't match any files",argv[nopt]) ;
            continue ;
          }
        } else {
          nexp = 1 ; fexp = argv+nopt ; didex = 0 ;
        }
        fdset = (char **)realloc(fdset,sizeof(char *)*(ndset+nexp)) ;
        for( iex=0 ; iex < nexp ; iex++ ) fdset[ndset++] = strdup(fexp[iex]) ;
        if( didex ) MCW_free_expand(nexp,fexp) ;
      }
      continue ;
    }

#if 0
    /*-----   -2sided   ------*/

    if( strcasecmp(argv[nopt],"-2sided") == 0 ){
      do_2sid = 1 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-1sided") == 0 ){
      do_2sid = 0 ; nopt++ ; continue ;
    }
#endif

    /*-----  -prefix -----*/

    if( strcmp(argv[nopt],"-prefix") == 0 ){
      nopt++ ; if( nopt >= argc ) ERROR_exit("need argument after -prefix!") ;
      prefix = strdup(argv[nopt]) ;
      if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
      nopt++ ; continue ;
    }

    /*----   -quiet   ----*/

    if( strcasecmp(argv[nopt],"-quiet") == 0 ){
      verb = 0 ; nopt++ ; continue ;
    }

    /*-----  -final   -----*/

    if( strcasecmp(argv[nopt],"-final") == 0 ){
      do_final = 1 ; nopt++ ; continue ;
    }

    /*----- unknown option -----*/

    ERROR_exit("3dClustCount -- unknown option '%s'",argv[nopt]) ;
  }

  /*----- anything left is a dataset filename -----*/

  for( ; nopt < argc ; nopt++ ){
    if( HAS_WILDCARD(argv[nopt]) ){
      MCW_file_expand( 1,argv+nopt, &nexp,&fexp ) ; didex = 1 ;
      if( nexp < 1 ){
        WARNING_message("wildcard name '%s' doesn't match any files",argv[nopt]) ;
        continue ;
      }
    } else {
      nexp = 1 ; fexp = argv+nopt ; didex = 0 ;
    }
    fdset = (char **)realloc(fdset,sizeof(char *)*(ndset+nexp)) ;
    for( iex=0 ; iex < nexp ; iex++ ) fdset[ndset++] = strdup(fexp[iex]) ;
    if( didex ) MCW_free_expand(nexp,fexp) ;
  }

  if( ndset == 0 && !do_final ) ERROR_exit("No input datasets?!") ;

  if( prefix == NULL ){
    prefix = "ClustCount" ;
    INFO_message("No -prefix option ==> using prefix = '%s'",prefix) ;
  }

  return ;
}

/*---------------------------------------------------------------------------*/

#define DALL MAX_CLUSTER_SIZE

/*! Put (i,j,k) into the current cluster, if it is nonzero. */

#define CPUT(i,j,k)                                             \
  do{ ijk = THREE_TO_IJK(i,j,k,nx,nxy) ;                        \
      if( mmm[ijk] ){                                           \
        if( nnow == nall ){ /* increase array lengths */        \
          nall += DALL + nall/2 ;                               \
          inow = (short *)realloc(inow,sizeof(short)*nall) ;    \
          jnow = (short *)realloc(jnow,sizeof(short)*nall) ;    \
          know = (short *)realloc(know,sizeof(short)*nall) ;    \
        }                                                       \
        inow[nnow] = i ; jnow[nnow] = j ; know[nnow] = k ;      \
        nnow++ ; mmm[ijk] = 0 ;                                 \
      } } while(0)

#define USE_MEMCHR    /* faster, but trickier to understand */

static int    nall = 0    ;  /* arrays for clusterizing */
static short *inow = NULL ;
static short *jnow = NULL ;
static short *know = NULL ;

/*----------------------------------------------------------------------------*/

int find_largest_cluster_NN1( byte *mmm )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km , nnow ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   ijk_last = 0 ;  /* start scanning at the start of voxel-land */

   while(1) {
     /* find next nonzero point in mmm array */

#ifndef USE_MEMCHR
     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
#else
     mch = memchr( mmm+ijk_last , 1 , nxyz-ijk_last ) ;  /* quicker search */
     if( mch == NULL ) ijk = nxyz ;
     else              ijk = mch - mmm ;
#endif
     if( ijk >= nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     mmm[ijk] = 0 ;                                /* clear found point */

     nnow = 1 ;                                    /* # pts in cluster */
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1 ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1 ;

       if( im >= 0 ) CPUT(im,jj,kk) ;  /* NN1 */
       if( ip < nx ) CPUT(ip,jj,kk) ;
       if( jm >= 0 ) CPUT(ii,jm,kk) ;
       if( jp < ny ) CPUT(ii,jp,kk) ;
       if( km >= 0 ) CPUT(ii,jj,km) ;
       if( kp < nz ) CPUT(ii,jj,kp) ;
     }

     if( nnow > max_size ) max_size = nnow ;
   }

   return max_size ;
}

/*----------------------------------------------------------------------------*/

int find_largest_cluster_NN2( byte *mmm )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km , nnow ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

#ifndef USE_MEMCHR
     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
#else
     mch = memchr( mmm+ijk_last , 1 , nxyz-ijk_last ) ;  /* quicker search */
     if( mch == NULL ) ijk = nxyz ;
     else              ijk = mch - mmm ;
#endif
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     mmm[ijk] = 0 ;                                /* clear found point */

     nnow = 1 ;                                    /* # pts in cluster */
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1 ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1 ;

       if( im >= 0 ){  CPUT(im,jj,kk) ;  /* NN1 */
         if( jm >= 0 ) CPUT(im,jm,kk) ;  /* NN2 */
         if( jp < ny ) CPUT(im,jp,kk) ;  /* NN2 */
         if( km >= 0 ) CPUT(im,jj,km) ;  /* NN2 */
         if( kp < nz ) CPUT(im,jj,kp) ;  /* NN2 */
       }
       if( ip < nx ){  CPUT(ip,jj,kk) ;  /* NN1 */
         if( jm >= 0 ) CPUT(ip,jm,kk) ;  /* NN2 */
         if( jp < ny ) CPUT(ip,jp,kk) ;  /* NN2 */
         if( km >= 0 ) CPUT(ip,jj,km) ;  /* NN2 */
         if( kp < nz ) CPUT(ip,jj,kp) ;  /* NN2 */
       }
       if( jm >= 0 ){  CPUT(ii,jm,kk) ;  /* NN1 */
         if( km >= 0 ) CPUT(ii,jm,km) ;  /* NN2 */
         if( kp < nz ) CPUT(ii,jm,kp) ;  /* NN2 */
       }
       if( jp < ny ){  CPUT(ii,jp,kk) ;  /* NN1 */
         if( km >= 0 ) CPUT(ii,jp,km) ;  /* NN2 */
         if( kp < nz ) CPUT(ii,jp,kp) ;  /* NN2 */
       }
       if( km >= 0 )   CPUT(ii,jj,km) ;  /* NN1 */
       if( kp < nz )   CPUT(ii,jj,kp) ;  /* NN1 */
     }

     if( nnow > max_size ) max_size = nnow ;
   }

   return max_size ;
}

/*----------------------------------------------------------------------------*/

int find_largest_cluster_NN3( byte *mmm )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km , nnow ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

#ifndef USE_MEMCHR
     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
#else
     mch = memchr( mmm+ijk_last , 1 , nxyz-ijk_last ) ;  /* quicker search */
     if( mch == NULL ) ijk = nxyz ;
     else              ijk = mch - mmm ;
#endif
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     mmm[ijk] = 0 ;                                /* clear found point */

     nnow = 1 ;                                    /* # pts in cluster */
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1 ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1 ;

       if( im >= 0 ){  CPUT(im,jj,kk) ;             /* NN1 */
         if( jm >= 0 ) CPUT(im,jm,kk) ;             /* NN2 */
         if( jp < ny ) CPUT(im,jp,kk) ;             /* NN2 */
         if( km >= 0 ) CPUT(im,jj,km) ;             /* NN2 */
         if( kp < nz ) CPUT(im,jj,kp) ;             /* NN2 */
         if( jm >= 0 && km >= 0 ) CPUT(im,jm,km) ;  /* NN3 */
         if( jm >= 0 && kp < nz ) CPUT(im,jm,kp) ;  /* NN3 */
         if( jp < ny && km >= 0 ) CPUT(im,jp,km) ;  /* NN3 */
         if( jp < ny && kp < nz ) CPUT(im,jp,kp) ;  /* NN3 */
       }
       if( ip < nx ){  CPUT(ip,jj,kk) ;             /* NN1 */
         if( jm >= 0 ) CPUT(ip,jm,kk) ;             /* NN2 */
         if( jp < ny ) CPUT(ip,jp,kk) ;             /* NN2 */
         if( km >= 0 ) CPUT(ip,jj,km) ;             /* NN2 */
         if( kp < nz ) CPUT(ip,jj,kp) ;             /* NN2 */
         if( jm >= 0 && km >= 0 ) CPUT(ip,jm,km) ;  /* NN3 */
         if( jm >= 0 && kp < nz ) CPUT(ip,jm,kp) ;  /* NN3 */
         if( jp < ny && km >= 0 ) CPUT(ip,jp,km) ;  /* NN3 */
         if( jp < ny && kp < nz ) CPUT(ip,jp,kp) ;  /* NN3 */
       }
       if( jm >= 0 ){  CPUT(ii,jm,kk) ;             /* NN1 */
         if( km >= 0 ) CPUT(ii,jm,km) ;             /* NN2 */
         if( kp < nz ) CPUT(ii,jm,kp) ;             /* NN2 */
       }
       if( jp < ny ){  CPUT(ii,jp,kk) ;             /* NN1 */
         if( km >= 0 ) CPUT(ii,jp,km) ;             /* NN2 */
         if( kp < nz ) CPUT(ii,jp,kp) ;             /* NN2 */
       }
       if( km >= 0 )   CPUT(ii,jj,km) ;             /* NN1 */
       if( kp < nz )   CPUT(ii,jj,kp) ;             /* NN1 */
     }

     if( nnow > max_size ) max_size = nnow ;
   }

   return max_size ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN1( float thr , float *fim , byte *bfim , int *mtab )
{
  register int ii ; int siz ;

  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] >= thr) ;
  siz = find_largest_cluster_NN1(bfim) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  if( siz > 0 ) mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN2( float thr , float *fim , byte *bfim , int *mtab )
{
  register int ii ; int siz ;

  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] >= thr) ;
  siz = find_largest_cluster_NN2(bfim) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  if( siz > 0 ) mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN3( float thr , float *fim , byte *bfim , int *mtab )
{
  register int ii ; int siz ;

  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] >= thr) ;
  siz = find_largest_cluster_NN3(bfim) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  if( siz > 0 ) mtab[siz]++ ;

  return ;
}

/*===========================================================================*/

int main( int argc , char **argv )
{
  int **max_table[4] ; int nnn , ipthr , first_mask=1 , niter ;
  char *fnam ;
  NI_element *neldat=NULL ;

  /*----- does user request help menu? -----*/

  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

  /*----- get the list of things to do -----*/

  mainENTRY("3dClustCount"); machdep();
  AFNI_logger("3dClustCount",argc,argv);
  PRINT_VERSION("3dClustCount"); AUTHOR("Zhark the Enumerator");

  get_options( argc , argv ) ;

  /*----- create some space for the results -----*/

  for( nnn=1 ; nnn <= 3 ; nnn++ ){
    max_table[nnn] = (int **)malloc(sizeof(int *)*npthr) ;  /* array of tables */
    for( ipthr=0 ; ipthr < npthr ; ipthr++ )               /* create tables */
      max_table[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
  }

 /* in 3dClustSim, the corresponding {...} code is inside OpenMP */
 {
   int ipthr, **mt[4] , nnn , ids,nds,ival , statcode , ii ;
   MRI_IMAGE *fim = NULL; float *far, *stataux , thr ; byte *bfar ;
   THD_3dim_dataset *dset ;

   nall = DALL ;
   inow = (short *)malloc(sizeof(short)*DALL) ;
   jnow = (short *)malloc(sizeof(short)*DALL) ;
   know = (short *)malloc(sizeof(short)*DALL) ;
   for( nnn=1 ; nnn <= 3 ; nnn++ ) mt[nnn] = max_table[nnn] ;

   /*--- loop over datasets ---*/

   for( niter=ids=0 ; ids < ndset ; ids++ ){

     /* get the next dataset */

     dset = THD_open_dataset( fdset[ids] ) ;
     if( !ISVALID_DSET(dset) ){
       ERROR_message("Can't open dataset %s -- skipping it :-(",fdset[ids]) ;
       continue ;
     }
     DSET_load(dset) ;
     if( !DSET_LOADED(dset) ){
       ERROR_message("Can't load dataset %s -- skipping it :-(",fdset[ids]) ;
       DSET_delete(dset) ;
     }
     nx = DSET_NX(dset) ;
     ny = DSET_NY(dset) ;
     nz = DSET_NZ(dset) ; nxy = nx*ny ; nxyz = nx*ny*nz ;
     bfar = (byte * )malloc(sizeof(byte)*nxyz) ;

     if( verb )
       fprintf(stderr,"++ Dataset %s",fdset[ids]) ;

     /* process each statistical sub-brick */

     for( nds=ival=0 ; ival < DSET_NVALS(dset) ; ival++ ){
       statcode = DSET_BRICK_STATCODE(dset,ival) ; /* type of data */
       stataux  = DSET_BRICK_STATAUX (dset,ival) ;
       if( !FUNC_IS_STAT(statcode) ) continue ;    /* bad */
       fim = THD_extract_float_brick(ival,dset) ;
       if( fim == NULL ) continue ;                /* bad */
       far = MRI_FLOAT_PTR(fim) ;
       for( ii=0 ; ii < nxyz ; ii++ ) far[ii] = fabsf(far[ii]) ;
       for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
         thr = THD_pval_to_stat( pthr[ipthr] , statcode , stataux ) ;
         gather_stats_NN1( thr , far , bfar , mt[1][ipthr] ) ;
         gather_stats_NN2( thr , far , bfar , mt[2][ipthr] ) ;
         gather_stats_NN3( thr , far , bfar , mt[3][ipthr] ) ;
       }
       niter++ ; nds++ ;  /* one more iteration in the bag */
       if( verb && nds == 1 ) fprintf(stderr," -- ") ;
     } /* end of loop over sub-bricks */

     if( verb && nds > 0 )
       fprintf(stderr,"%d / %d sub-bricks processed -- %d total now\n",
               nds,DSET_NVALS(dset),niter) ;
     else if( verb && nds == 0 )
       fprintf(stderr," -- 0 sub-bricks processed\n") ;

     mri_free(fim) ; free(bfar) ; DSET_delete(dset) ;

  } /* end loop over datasets */

  free(know) ; free(jnow) ; free(inow) ;

 } /* end of processing the inputs */

  if( niter == 0 && !do_final )
    ERROR_exit("No data was processed :-(") ;

  if( verb && ndset > 1 && niter > 1 )
    INFO_message("Processed a total of %d sub-bricks from %d datasets",niter,ndset) ;

  /*---------- now read the existing file, if any, and merge it ----------*/

  fnam = (char *)malloc(sizeof(char)*(strlen(prefix)+128)) ;
  strcpy(fnam,prefix) ;
  if( strstr(fnam,".clustcount.niml") == NULL ) strcat(fnam,".clustcount.niml") ;
  neldat = NI_read_element_fromfile(fnam) ;
  if( neldat != NULL ){

#define NELERR(sf)                                      \
 do{ ERROR_message("File %s has bad data [%s]",fnam,sf) ;  \
     NI_free_element(neldat) ; neldat = NULL ; goto NelDone ; } while(0)

    char *atr ;
    int ii,jj, nelite, nelmax, nelnum, **vel[4]={NULL,NULL,NULL,NULL} ;

    if( neldat->type != NI_ELEMENT_TYPE ) NELERR("type") ;

    nelmax = neldat->vec_len ;
    if( nelmax > max_cluster_size ) nelmax = max_cluster_size+1 ;
    if( nelmax < 2 ) NELERR("length") ;

    nelnum = neldat->vec_num ;
    if( nelnum < 3*npthr ) NELERR("number") ;
    for( nnn=1 ; nnn <= 3 ; nnn++ ){
      vel[nnn] = (int **)malloc(sizeof(int *)*npthr) ;
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
        jj = (nnn-1)*npthr + ipthr ;
        if( neldat->vec_typ[jj] != NI_INT ) NELERR("datum") ;
        vel[nnn][ipthr] = (int *)neldat->vec[jj] ;
      }
    }

    atr = NI_get_attribute( neldat , "niter" ) ;
    if( atr == NULL ) NELERR("niter") ;

    nelite = (int)strtod(atr,NULL) ;
    if( nelite <= 0 ) NELERR("niter") ;

    for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
      for( ii=0 ; ii < nelmax ; ii++ ){
        max_table[1][ipthr][ii] += vel[1][ipthr][ii] ;
        max_table[2][ipthr][ii] += vel[2][ipthr][ii] ;
        max_table[3][ipthr][ii] += vel[3][ipthr][ii] ;
      }
    }

    niter += nelite ;
    if( verb ) INFO_message("merged %d sub-brick counts from file %s",nelite,fnam) ;

#undef NELERR
NelDone:
    if( vel[1] != NULL ){ free(vel[1]); free(vel[2]); free(vel[3]); }
    NI_free_element(neldat) ; neldat = NULL ;
  }

  /*---------- save counting data to file ----------*/

  if( niter > 0 ){
    int itop=0 , ii ; char buf[128] ;

    for( ii=max_cluster_size ; ii > 0 && itop == 0 ; ii-- ){
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
        if( max_table[1][ipthr][ii] > 0 ||
            max_table[2][ipthr][ii] > 0 ||
            max_table[3][ipthr][ii] > 0   ){ itop=ii; break; }
      }
    }
    if( itop == 0 ) ERROR_exit("No clusters found at all!") ;

    neldat = NI_new_data_element( "ClustCounts" , itop+1 ) ;
    for( nnn=1 ; nnn <= 3 ; nnn++ ){
      for( ii=0 ; ii < npthr ; ii++ ){
        NI_add_column( neldat , NI_INT , max_table[nnn][ipthr] ) ;
    }}

    sprintf(buf,"%d",niter) ;
    NI_set_attribute( neldat , "niter" , buf ) ;

    NI_write_element_tofile( fnam , neldat , NI_TEXT_MODE ) ;

    NI_free_element(neldat) ; neldat = NULL ;

  } /* end of save */

  /*---------- compute and print the output table ----------*/

  if( do_final ){
    double *alpha , aval , ahi,alo ;
    float **clust_thresh , cmax=0.0f ;
    int ii , itop , iathr ;
    char *commandline = tross_commandline("3dClustCount",argc,argv) ;
    char fname[THD_MAX_NAME] , pname[THD_MAX_NAME] ;
    char *amesg = NULL ;  /* 20 Jan 2011 */

    alpha        = (double *)malloc(sizeof(double)*(max_cluster_size+1)) ;
    clust_thresh = (float **)malloc(sizeof(float *)*npthr) ;
    for( ipthr=0 ; ipthr < npthr ; ipthr++ )
      clust_thresh[ipthr] = (float *)malloc(sizeof(float)*nathr) ;

    for( nnn=1 ; nnn <= 3 ; nnn++ ){
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
        for( itop=ii=1 ; ii <= max_cluster_size ; ii++ ){
          alpha[ii] = max_table[nnn][ipthr][ii] / (double)niter ;
          if( alpha[ii] > 0.0 ) itop = ii ;
        }
        for( ii=itop-1 ; ii >= 1 ; ii-- ) alpha[ii] += alpha[ii+1] ;
#if 0
INFO_message("pthr[%d]=%g itop=%d",ipthr,pthr[ipthr],itop) ;
for( ii=1 ; ii <= itop ; ii++ )
  fprintf(stderr," %d=%g",ii,alpha[ii]) ;
fprintf(stderr,"\n") ;
#endif
        for( iathr=0 ; iathr < nathr ; iathr++ ){
          aval = athr[iathr] ;
          if( aval > alpha[1] ){  /* unpleasant situation */
            ii = 1 ;
            amesg = THD_zzprintf(
                     amesg ,
                     "   NN=%d  pthr=%9.6f  alpha=%6.3f [max simulated alpha=%6.3f]\n" ,
                     nnn , pthr[ipthr] , aval , alpha[1] ) ;
          } else {
            for( ii=1 ; ii < itop ; ii++ ){
              if( alpha[ii] >= aval && alpha[ii+1] <= aval ) break ;
            }
          }

          alo=alpha[ii] ; ahi=alpha[ii+1] ;
          if( do_lohi ){
            aval = ii ;    /* for debugging */
          } else {
            if( alo >= 1.0 ) alo = 1.0 - 0.1/niter ;
            if( ahi <= 0.0 ) ahi = 0.1/niter ;
            if( ahi >= alo ) ahi = 0.1*alo ;
            aval = log(-log(1.0-aval)) ;
            alo  = log(-log(1.0-alo)) ;
            ahi  = log(-log(1.0-ahi)) ;
            aval = ii + (aval-alo)/(ahi-alo) ;
                 if( aval < 1.0 ) aval = 1.0 ;
            else if( nodec      ) aval = (int)(aval+0.951) ;
          }
          clust_thresh[ipthr][iathr] = aval ;

          if( clust_thresh[ipthr][iathr] > cmax ) cmax = clust_thresh[ipthr][iathr] ;
        }
      }

      if( do_lohi == 0 ){
        /* edit each column to increase as pthr increases [shouldn't be needed] */

        for( iathr=0 ; iathr < nathr ; iathr++ ){
          for( ipthr=npthr-2 ; ipthr >= 0 ; ipthr-- ){
            if( clust_thresh[ipthr][iathr] < clust_thresh[ipthr+1][iathr] )
              clust_thresh[ipthr][iathr] = clust_thresh[ipthr+1][iathr] ;
          }
        }

        /* edit each row to increase as athr decreases [shouldn't be needed] */

        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
          for( iathr=1 ; iathr < nathr ; iathr++ ){
            if( clust_thresh[ipthr][iathr] < clust_thresh[ipthr][iathr-1] )
              clust_thresh[ipthr][iathr] = clust_thresh[ipthr][iathr-1] ;
          }
        }
      }

#if 0
      if( !nodec && !do_niml && cmax > 9999.9f ){  /* if largest is way big, */
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){  /* then truncate to ints. */
          for( iathr=0 ; iathr < nathr ; iathr++ ){
            aval = clust_thresh[ipthr][iathr] ;
            aval = (int)(aval+0.951) ;
            clust_thresh[ipthr][iathr] = aval ;
          }
        }
        nodec = 1 ;
      }
#endif

MPROBE ;

      if( prefix != NULL ){
        sprintf(pname,"%s.NN%d.",prefix,nnn) ;
      } else {
        fflush(stderr) ; fflush(stdout) ;
      }

      if( do_1D ){  /* output in 1D format */
        FILE *fp = stdout ;
        if( prefix != NULL ){
          strcpy(fname,pname) ; strcat(fname,"1D") ; fp = fopen(fname,"w") ;
          if( fp == NULL ){
            ERROR_message("Can't open file %s -- using stdout",fname) ;
            fp = stdout ;
          }
        }
        fprintf(fp,
         "# %s\n"
         "#\n"
         "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
         "# -NN %d  | alpha = Prob(Cluster >= given size)\n"
         "#  pthr  |" ,
         commandline , nnn ) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," %6.3f",athr[iathr]) ;
        fprintf(fp,"\n"
         "# ------ |" ) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," ------") ;
        fprintf(fp,"\n") ;
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
          fprintf(fp,"%9.6f ",pthr[ipthr]) ;
          for( iathr=0 ; iathr < nathr ; iathr++ ){
            if( nodec )
              fprintf(fp,"%7d"  ,(int)clust_thresh[ipthr][iathr]) ;
            else if( clust_thresh[ipthr][iathr] <= 9999.9f )
              fprintf(fp,"%7.1f",     clust_thresh[ipthr][iathr]) ;
            else
              fprintf(fp,"%7.0f",     clust_thresh[ipthr][iathr]) ;
          }
          fprintf(fp,"\n") ;
        }
      }

      if( do_niml ){ /* output in NIML format */
        NI_element *nel ; float *vec ; char buf[1024] , *bbb ; NI_float_array nfar ;
        sprintf(buf,"3dClustCount_NN%d",nnn) ;
        nel = NI_new_data_element( buf , npthr ) ;
        vec = (float *)malloc(sizeof(float)*MAX(npthr,nathr)) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ){
          for( ipthr=0 ; ipthr < npthr ; ipthr++ )
            vec[ipthr] = clust_thresh[ipthr][iathr] ;
          NI_add_column( nel , NI_FLOAT , vec ) ;
        }
          NI_set_attribute( nel , "commandline" , commandline ) ;
        sprintf(buf,"%d,%d,%d",nx,ny,nz) ;
          NI_set_attribute(nel,"nxyz",buf) ;
#if 0
        sprintf(buf,"%.3f,%.3f,%.3f",dx,dy,dz) ;
          NI_set_attribute(nel,"dxyz",buf) ;
        sprintf(buf,"%.2f,%.2f,%.2f",fwhm_x,fwhm_y,fwhm_z) ;
          NI_set_attribute(nel,"fwhmxyz",buf) ;
#endif
        sprintf(buf,"%d",niter) ;
          NI_set_attribute(nel,"iter",buf) ;
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ) vec[ipthr] = pthr[ipthr] ;
        nfar.num = npthr ; nfar.ar = vec ; bbb = NI_encode_float_list(&nfar,",") ;
          NI_set_attribute(nel,"pthr",bbb) ; NI_free(bbb) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ) vec[iathr] = athr[iathr] ;
        nfar.num = nathr ; nfar.ar = vec ; bbb = NI_encode_float_list(&nfar,",") ;
          NI_set_attribute(nel,"athr",bbb) ; NI_free(bbb) ;
#if 0
        if( mask_dset != NULL ){
            NI_set_attribute(nel,"mask_dset_idcode",DSET_IDCODE_STR(mask_dset)) ;
            NI_set_attribute(nel,"mask_dset_name"  ,DSET_HEADNAME(mask_dset)) ;
          sprintf(buf,"%d",mask_ngood) ;
            NI_set_attribute(nel,"mask_count",buf) ;
        }
#endif
        if( prefix != NULL ){ strcpy(fname,pname) ; strcat(fname,"niml") ; }
        else                  strcpy(fname,"stdout:") ;
        NI_write_element_tofile( fname , nel , NI_TEXT_MODE ) ;
        NI_free_element( nel ) ;
      } /* end of NIML output */

    } /* end of loop over nnn = NN degree */

    if( amesg != NULL ){
      WARNING_message("Simulation not effective for these cases:\n\n"
                      "%s\n"
                      "*+ This means that not enough clusters, of any size, +*\n"
                      "     of voxels at or below each pthr threshold, were +*\n"
                      "     found to estimate at each alpha level.          +*\n"
                      "*+ In other words, the probability that noise-only   +*\n"
                      "     data (of the given smoothness) will cause       +*\n"
                      "     above-threshold (at the given pthr) clusters is +*\n"
                      "     smaller than the desired alpha levels.          +*\n"
                      "*+ This problem can arise when the masked region     +*\n"
                      "     being simulated is small and at the same time   +*\n"
                      "     the smoothness (FWHM) is large.                 +*\n"
                      "*+ Read the 'CAUTION and CAVEAT' section at the end  +*\n"
                      "   of the '-help' output for a longer explanation.   +*\n\n"
                    , amesg ) ;
      free(amesg) ;
    }

  } /* end of outputizationing */

  /*-------- run away screaming into the night ----- AAUUGGGHHH!!! --------*/

  exit(0);
}
