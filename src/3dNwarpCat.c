#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#endif
#include "mri_genalign.c"
#include "mri_genalign_util.c"
#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/

void CNW_help(void)
{
   printf(
    "Usage: cat_Nwarp [options] warp1 warp2 ...\n"
    "------\n"
    " * This program catenates (composes) 3D warps defined on a grid,\n"
    "   or via a matrix.\n"
    "\n"
    " * At least one of the warps input must be defined on a grid, via\n"
    "   a 3D dataset (e.g., output from 3dQwarp).\n"
    "\n"
    " * The order of operations in the final (output) warp is, for the\n"
    "   case of 3 input warps:\n"
    "\n"
    "     OUTPUT(x) = warp3( warp2( warp1(x) ) )\n"
    "\n"
    " * For example:\n"
    "\n"
    "     warp1 is a matrix from @auto_tlrc\n"
    "     warp2 is the output of 3dQwarp\n"
    "\n"
    "   cat_Nwarp -prefix Fred_total_WARP Fred.Xat.1D Fred_WARP+tlrc.HEAD\n"
    "\n"
    " * If you wish to invert a warp before it is used here, supply its\n"
    "   input name in the form of\n"
    "     INV(warpfilename)\n"
    "   Or you could use 3dNwarpCalc, of course.\n"
    "\n"
    "OPTIONS\n"
    "-------\n"
    " -interp iii   == 'iii' is the interpolation mode:\n"
    "                  ++ Modes allowed are a subset of those in 3dAllineate:\n"
    "                       linear  quintic  wsinc5\n"
    "                  ++ The default interpolation mode is 'wsinc5'.\n"
    "                  ++ 'linear' is much faster but less accurate.\n"
    "                  ++ 'quintic' is between 'linear' and 'wsinc5'.\n"
    "\n"
    " -verb         == print (to stderr) various fun messages along the road\n"
    "\n"
    " -prefix ppp   == prefix name for the output dataset that holds the warp.\n"
    "\n"
    " -warp1 ww1    == alternative way to specify warp#1\n"
    " -warp2 ww2    == alternative way to specify warp#2 (etc.)\n"
    "                  ++ If you use any '-warpX' option for X=1..99, then\n"
    "                     any addition warps specified after all command\n"
    "                     line options appear AFTER these enumerated warps.\n"
    "                     That is, '-warp1 A+tlrc -warp2 B+tlrc C+tlrc'\n"
    "                     is like using '-warp3 C+tlrc'.\n"
    "                  ++ At most 99 warps can be used.  If you need more,\n"
    "                     please step away from the computer slowly, and\n"
    "                     get professional help.\n"
   ) ;

   printf(
    "\n"
    "AUTHOR -- RWCox -- February 2013\n"
   ) ;

   PRINT_AFNI_OMP_USAGE("cat_Nwarp",NULL) ; PRINT_COMPILE_DATE ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

#define NWMAX 99

static int          nwtop=0 ;
static IndexWarp3D *iwarp[NWMAX] ;
static mat44       *awarp[NWMAX] ;
static int nx=0,ny=0,nz=0 ; char *geomstring=NULL ;
static mat44 cmat , imat ;

static THD_3dim_dataset *inset=NULL ;

static int verb=0 , interp_code=MRI_WSINC5 ;

/*----------------------------------------------------------------------------*/

void CNW_load_warp( int nn , char *cp )
{
   char *wp ; int do_inv=0 , ii ;

   if( nn <= 0 || nn > NWMAX || cp == NULL || *cp == '\0' )
     ERROR_exit("bad inputs to CNW_load_warp") ;

   if( strncasecmp(cp,"INV(",4) == 0 ){
     cp += 4 ; do_inv == 1 ;
   } else if( strncasecmp(cp,"INVERSE(",8) == 0 ){
     cp += 8 ; do_inv = 1 ;
   }
   wp = strdup(cp) ; ii = strlen(wp) ;
   if( ii < 4 ) ERROR_exit("too-short input string to CNW_load_warp") ;
   if( wp[ii-1] == ')' ) wp[ii-1] = '\0' ;

   if( nn > nwtop ) nwtop = nn ;  /* nwtop = largest index thus far */

   if( verb )
     INFO_message("reading warp#%d from file %s",nn,wp) ;

   if( STRING_HAS_SUFFIX_CASE(wp,".1D")  ||
       STRING_HAS_SUFFIX_CASE(wp,".txt")   ){      /* affine warp */

     mat44 mmm ; MRI_IMAGE *qim ; float *qar ;
     qim = mri_read_1D(wp) ;
     if( qim == NULL || qim->nvox < 9 )
       ERROR_exit("cannot read matrix from file '%s'",wp);
     qar = MRI_FLOAT_PTR(qim) ;
     if( qim->nvox < 12 )
       LOAD_MAT44(mmm,qar[0],qar[1],qar[2],0,
                      qar[3],qar[4],qar[5],0,
                      qar[6],qar[7],qar[8],0) ;
     else
       LOAD_MAT44(mmm,qar[0],qar[1],qar[2],qar[3],
                      qar[4],qar[5],qar[6],qar[7],
                      qar[8],qar[9],qar[10],qar[11]) ;
     mri_free(qim) ;

     if( do_inv ){
       mat44 imm ;
       if( verb ) ININFO_message("--- inverting matrix") ;
       imm = MAT44_INV(mmm) ; mmm = imm ;
     }

     awarp[nn-1] = (mat44 *)malloc(sizeof(mat44)) ;
     AAmemcpy(awarp[nn-1],&mmm,sizeof(mat44)) ;
     free(wp) ; return ;

   } else {                                        /* dataset warp */

     THD_3dim_dataset *dset ; IndexWarp3D *AA ;
     dset = THD_open_dataset(wp) ;
     if( dset == NULL )
       ERROR_exit("can't open dataset from file '%s'",wp);
     if( verb ) ININFO_message("--- reading dataset") ;
     AA = IW3D_from_dataset(dset,0,0) ;
     if( AA == NULL )
       ERROR_exit("can't make warp from dataset '%s'",wp);
     if( geomstring == NULL ){       /* first dataset => set geometry globals */
       geomstring = strdup(AA->geomstring) ;
       nx = AA->nx; ny = AA->ny; nz = AA->nz; cmat = AA->cmat; imat = AA->imat;
     } else if( AA->nx != nx || AA->ny != ny || AA->nz != nz ){ /* check them */
       ERROR_exit("warp from dataset '%s' doesn't match earlier inputs in grid size",wp) ;
     }
     if( inset == NULL ){ DSET_unload(dset) ; inset = dset ; }  /* save as template */
     else               { DSET_delete(dset) ; }

     if( do_inv ){
       IndexWarp3D *BB ;
       if( verb ) ININFO_message("--- inverting warp") ;
       BB = IW3D_invert(AA,NULL,interp_code); IW3D_destroy(AA); AA = BB;
     }

     iwarp[nn-1] = AA ; free(wp) ; return ;
   }

   /* unreachable */

   return ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii ;
   char *prefix = "catNwarp" ;
   mat44        wmat      , tmat , smat , qmat ;
   IndexWarp3D *warp=NULL , *tarp=NULL ;
   THD_3dim_dataset *oset ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) CNW_help() ;

   /*-- bureaucracy --*/

   mainENTRY("cat_Nwarp"); machdep();
   AFNI_logger("cat_Nwarp",argc,argv);
   PRINT_VERSION("cat_Nwarp"); AUTHOR("Zhark the Warper");
   (void)COX_clock_time() ;

   ZERO_MAT44(imat) ; ZERO_MAT44(cmat) ;
   for( ii=0 ; ii < NWMAX ; ii++ ){ iwarp[ii] = NULL ; awarp[ii] = NULL ; }

   /*-- scan args --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---------------*/

     if( strcasecmp(argv[iarg],"-NN") == 0 || strncasecmp(argv[iarg],"-nearest",6) == 0 ){
       WARNING_message("NN interpolation not legal here -- switched to linear") ;
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-linear",4)==0 || strncasecmp(argv[iarg],"-trilinear",6)==0 ){
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-cubic",4)==0 || strncasecmp(argv[iarg],"-tricubic",6)==0 ){
       WARNING_message("cubic interplation not legal here -- switched to quintic") ;
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-quintic",4)==0 || strncasecmp(argv[iarg],"-triquintic",6)==0 ){
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-wsinc",5) == 0 ){
       interp_code = MRI_WSINC5 ; iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-interp",5)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 ){
         WARNING_message("NN interpolation not legal here -- changed to linear") ;
         interp_code = MRI_LINEAR ;
       } else if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 ){
         interp_code = MRI_LINEAR ;
       } else if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 ){
         WARNING_message("cubic interplation not legal here -- changed to quintic") ;
         interp_code = MRI_QUINTIC ;
       } else if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 ){
         interp_code = MRI_QUINTIC ;
       } else if( strncasecmp(inam,"wsinc",4)==0 ){
         interp_code = MRI_WSINC5 ;
       } else {
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       }
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; NwarpCalcRPN_verb(verb) ; iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal name after '%s'",argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-warp",5) == 0 ){
       int nn ;
       if( iarg >= argc-1 ) ERROR_exit("no argument after '%s' :-(",argv[iarg]) ;
       if( !isdigit(argv[iarg][5]) ) ERROR_exit("illegal format for '%s' :-(",argv[iarg]) ;
       nn = (int)strtod(argv[iarg]+5,NULL) ;
       if( nn <= 0 || nn > NWMAX )
         ERROR_exit("illegal warp index in '%s' :-(",argv[iarg]) ;
       if( iwarp[nn] != NULL || awarp[nn] != NULL )
         ERROR_exit("'%s': you can't specify warp #%d more than once :-(",argv[iarg],nn) ;

       CNW_load_warp( nn , argv[++iarg] ) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_exit("Unknown, Illegal, and Fattening option '%s' :-(",argv[iarg]) ;
   }

   /*-- load any warps left on the command line, after options --*/

   for( ; iarg < argc ; iarg++ )
     CNW_load_warp( nwtop+1 , argv[iarg] ) ;

   if( geomstring == NULL )
     ERROR_exit("you must have at least one dataset-defined nonlinear warp") ;

   /*-- cat them --*/

   LOAD_IDENT_MAT44(wmat) ;

   for( ii=0 ; ii < nwtop ; ii++ ){

     if( awarp[ii] != NULL ){

       qmat = *(awarp[ii]) ;             /* convert from xyz warp to ijk warp */
       tmat = MAT44_MUL(qmat,cmat) ;
       smat = MAT44_MUL(imat,tmat) ;

       if( warp == NULL ){
         qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;
       } else {
         tarp = IW3D_compose_w1m2(warp,smat,interp_code) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       free(awarp[ii]) ; awarp[ii] = NULL ;

     } else if( iwarp[ii] != NULL ){

       if( warp == NULL ){
         warp = IW3D_compose_m1w2(wmat,iwarp[ii],interp_code) ;
       } else {
         tarp = IW3D_compose(warp,iwarp[ii],interp_code) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       IW3D_destroy(iwarp[ii]) ; iwarp[ii] = NULL ;

     }

   }

   /*--- write result to disk for future fun fun fun in the sun sun sun ---*/

   if( warp == NULL ) ERROR_exit("This message should never appear!") ;

   /*--- run away screaming into the night, never to be seen again ---*/

   INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                COX_cpu_time() , COX_clock_time() ) ;

   exit(0) ;
}
