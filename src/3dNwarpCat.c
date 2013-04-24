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
    "Usage: 3dNwarpCat [options] warp1 warp2 ...\n"
    "------\n"
    " * This program catenates (composes) 3D warps defined on a grid,\n"
    "   OR via a matrix.\n"
    " ++ All transformations are from DICOM xyz (in mm) to DICOM xyz.\n"
    "\n"
    " * Matrix warps are in files that end in '.1D' or in '.txt'.  A matrix\n"
    "   warp file should have 12 numbers in it, as output (for example), by\n"
    "   '3dAllineate -1Dmatrix_save'.\n"
    "  ++ The matrix (affine) warp can have either 12 numbers on one row,\n"
    "     or be in the 3x4 format.\n"
    "\n"
    " * Nonlinear warps are in dataset files (AFNI .HEAD/.BRIK or NIfTI .nii)\n"
    "   with 3 sub-bricks giving the DICOM order xyz grid displacements in mm.\n"
    "\n"
    " * If all the input warps are matrices, then the output is a matrix\n"
    "   and will be written to the file 'prefix.aff12.1D'.\n"
    " ++ Unless the prefix already contains the string '.1D', in which case\n"
    "    the filename is just the prefix.\n"
    " ++ If 'prefix' is just 'stdout', then the output matrix is written\n"
    "    to standard output.\n"
    " ++ In any case, the output format is 12 numbers in one row.\n"
    "\n"
    " * If any of the input warps are datasets, they must all be defined on\n"
    "   the same 3D grid!\n"
    " ++ And of course, then the output will be a dataset on the same grid.\n"
    "\n"
    " * The order of operations in the final (output) warp is, for the\n"
    "   case of 3 input warps:\n"
    "\n"
    "     OUTPUT(x) = warp3( warp2( warp1(x) ) )\n"
    "\n"
    "   That is, warp1 is applied first, then warp2, et cetera.\n"
    "   The 3D x coordinates are taken from each grid location in the\n"
    "   first dataset defined on a grid.\n"
    "\n"
    " * For example, if you aligned a dataset to a template with @auto_tlrc,\n"
    "   then further refined the alignment with 3dQwarp, you would do something\n"
    "   like this:\n"
    "       warp1 is the output of 3dQwarp\n"
    "       warp2 is the matrix from @auto_tlrc\n"
    "       This is the proper order, since the desired warp takes template xyz\n"
    "       to original dataset xyz, and we have\n"
    "         3dQwarp warp:      takes template xyz to affinely aligned xyz, and\n"
    "         @auto_tlrc matrix: takes affinely aligned xyz to original xyz\n"
    "\n"
    "   3dNwarpCat -prefix Fred_total_WARP -warp1 Fred_WARP+tlrc.HEAD -warp2 Fred.Xat.1D \n"
    "\n"
    "   The dataset Fred_total_WARP+tlrc.HEAD could then be used to transform original\n"
    "   datasets directly to the final template space, as in\n"
    "\n"
    "   3dNwarpApply -prefix Wilma_warped        \\\n"
    "                -nwarp Fred_total_WARP+tlrc \\\n"
    "                -source Wilma+orig          \\\n"
    "                -master Fred_total_WARP+tlrc\n"
    "\n"
    " * If you wish to invert a warp before it is used here, supply its\n"
    "   input name in the form of\n"
    "     INV(warpfilename)\n"
    "   To produce the inverse of the warp in the example above:\n"
    "\n"
    "   3dNwarpCat -prefix Fred_total_WARPINV        \\\n"
    "              -warp2 'INV(Fred_WARP+tlrc.HEAD)' \\\n"
    "              -warp1 'INV(Fred.Xat.1D)' \n"
    "\n"
    "   Note the order of the warps is reversed, in addition to the use of 'INV()'.\n"
    "\n"
    " * Other functions you can apply to modify a 3D dataset warp are:\n"
    "    SQRT(datasetname) to get the square root of a warp\n"
    "    SQRTINV(datasetname) to get the inverse square root of a warp\n"
    "   However, you can't do more complex expressions, such as 'SQRT(SQRT(warp))'.\n"
    "   If you need something so rococo, use 3dNwarpCalc.\n"
    "\n"
    " * You can also manufacture a 3D warp from a 1-brick dataset with displacments\n"
    "   in a single direction.  For example:\n"
    "      AP:0.44:disp+tlrc.HEAD  (note there are no blanks here!)\n"
    "   means to take the 1-brick dataset disp+tlrc.HEAD, scale the values inside\n"
    "   by 0.44, then load them into the y-direction displacements of a 3-brick 3D\n"
    "   warp, and fill the other 2 directions with zeros.  The prefixes you can use\n"
    "   here for the 1-brick to 3-brick displacment trick are\n"
    "     RL: for x-displacements (Right-to-Left)\n"
    "     AP: for y-displacements (Anterior-to-Posterior)\n"
    "     IS: for z-displacements (Inferior-to-Superior)\n"
    "     VEC:a,b,c: for displacements in the vector direction (a,b,c),\n"
    "                which vector will be scaled to be unit length.\n"
    "     Following the prefix's colon, you can put in a scale factor followed\n"
    "     by another colon (as in '0.44:' in the example above).  Then the name\n"
    "     of the dataset with the 1D displacments follows.\n"
    " * You might reasonably ask of what possible value is this peculiar format?\n"
    "   This was implemented to use Bz fieldmaps for correction of EPI datasets,\n"
    "   which are distorted only along the phase-encoding direction.  This format\n"
    "   for specifying the input dataset (the fieldmap) is built to make the\n"
    "   scripting a little easier.  Its principal use is in the program 3dNwarpApply.\n"
    "\n"
    "\n"
    "OPTIONS\n"
    "-------\n"
    " -interp iii == 'iii' is the interpolation mode:\n"
    "                ++ Modes allowed are a subset of those in 3dAllineate:\n"
    "                     linear  quintic  wsinc5\n"
    "                ++ The default interpolation mode is 'wsinc5'.\n"
    "                ++ 'linear' is much faster but less accurate.\n"
    "                ++ 'quintic' is between 'linear' and 'wsinc5',\n"
    "                   in both accuracy and speed.\n"
    "\n"
    " -verb       == print (to stderr) various fun messages along the road.\n"
    "\n"
    " -prefix ppp == prefix name for the output dataset that holds the warp.\n"
    "\n"
    " -warp1 ww1  == alternative way to specify warp#1\n"
    " -warp2 ww2  == alternative way to specify warp#2 (etc.)\n"
    "                ++ If you use any '-warpX' option for X=1..99, then\n"
    "                   any addition warps specified after all command\n"
    "                   line options appear AFTER these enumerated warps.\n"
    "                   That is, '-warp1 A+tlrc -warp2 B+tlrc C+tlrc'\n"
    "                   is like using '-warp3 C+tlrc'.\n"
    "                ++ At most 99 warps can be used.  If you need more,\n"
    "                   PLEASE back away from the computer slowly, and\n"
    "                   get professional therapy.\n"
   ) ;

   printf(
    "\n"
    "AUTHOR -- RWCox -- March 2013\n"
   ) ;

   PRINT_AFNI_OMP_USAGE("3dNwarpCat",NULL) ; PRINT_COMPILE_DATE ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

#define NWMAX 99

static int          nwtop=0 ;
static IndexWarp3D *iwarp[NWMAX] ;
static float        iwfac[NWMAX] ;
static mat44       *awarp[NWMAX] ;
static int nx=0,ny=0,nz=0 ; char *geomstring=NULL , *sname=NULL ;
static mat44 cmat , imat ;

static THD_3dim_dataset *inset=NULL ;

static int verb=0 , interp_code=MRI_WSINC5 ;

/*----------------------------------------------------------------------------*/

void CNW_load_warp( int nn , char *cp )
{
   char *wp ; int do_inv=0 , do_sqrt=0 , do_empty=0 , ii ;

   if( nn <= 0 || nn > NWMAX || cp == NULL || *cp == '\0' )
     ERROR_exit("bad inputs to CNW_load_warp") ;

   if( strncasecmp(cp,"INV(",4) == 0 ){
     cp += 4 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERSE(",8) == 0 ){
     cp += 8 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"SQRT(",5) == 0 ){
     cp += 5 ; do_sqrt = 1 ;
   } else if( strncasecmp(cp,"SQRTINV(",8) == 0 || strncasecmp(cp,"INVSQRT(",8) == 0 ){
     cp += 8 ; do_inv = do_sqrt = 1 ;
   } else if( strncasecmp(cp,"IDENT(",6) == 0 ){
     cp += 6 ; do_empty = 1 ;
   }

   wp = strdup(cp) ; ii = strlen(wp) ;
   if( ii < 4 ) ERROR_exit("input string to CNW_load_warp is too short :-((") ;
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
     if( qim->nx < 12 && qim->ny > 1 ){
       MRI_IMAGE *tim = mri_transpose(qim) ; mri_free(qim) ; qim = tim ;
     }
     qar = MRI_FLOAT_PTR(qim) ;
     if( qim->nvox < 12 )                           /* presumably a rotation */
       LOAD_MAT44(mmm,qar[0],qar[1],qar[2],0,
                      qar[3],qar[4],qar[5],0,
                      qar[6],qar[7],qar[8],0) ;
     else                                           /* a full matrix */
       LOAD_MAT44(mmm,qar[0],qar[1],qar[2],qar[3],
                      qar[4],qar[5],qar[6],qar[7],
                      qar[8],qar[9],qar[10],qar[11]) ;
     mri_free(qim) ;

     if( do_inv ){
       mat44 imm ;
       if( verb ) ININFO_message("--- inverting matrix") ;
       imm = MAT44_INV(mmm) ; mmm = imm ;
     }
     if( do_sqrt ){
       if( verb ) ININFO_message("--- square-rooting matrix") ;
       mat44 smm = THD_mat44_sqrt(mmm) ; mmm = smm ;
     }

     awarp[nn-1] = (mat44 *)malloc(sizeof(mat44)) ;
     AAmemcpy(awarp[nn-1],&mmm,sizeof(mat44)) ;
     free(wp) ; return ;

   } else {                                        /* dataset warp */

     THD_3dim_dataset *dset , *eset=NULL ; IndexWarp3D *AA , *BB ;

     /* check for special case of uni-directional warp from 1 sub-brick [19 Mar 2013] */

     if( strncasecmp(wp,"RL:",3) == 0 || strncasecmp(wp,"LR:",3) == 0 ||
         strncasecmp(wp,"AP:",3) == 0 || strncasecmp(wp,"PA:",3) == 0 ||
         strncasecmp(wp,"IS:",3) == 0 || strncasecmp(wp,"SI:",3) == 0 ||
         strncasecmp(wp,"VEC:",4)== 0 || strncasecmp(wp,"UNI:",4)== 0   ){

       float vx=0.0f,vy=0.0f,vz=0.0f,vm=0.0f ;
       char *up=strchr(wp,':')+1 , *vp ;
       MRI_IMAGE *dim ; float *dar , *xar,*yar,*zar ; int nvox ;

       /* set unit vector for direction of warp displacements in 3D */

       switch( toupper(*wp) ){
         case 'R': case 'L':  vx = 1.0f ; vy = vz = 0.0f ; break ;
         case 'A': case 'P':  vy = 1.0f ; vx = vz = 0.0f ; break ;
         case 'I': case 'S':  vz = 1.0f ; vx = vy = 0.0f ; break ;
         default:
           sscanf(up,"%f,%f,%f",&vx,&vy,&vz) ;
           vm = sqrtf(vx*vx+vy*vy+vz*vz) ;
           if( vm < 1.e-9f ){
             ERROR_message("uni-directional warp '%s' :-) direction is unclear",wp) ;
             free(wp) ; EXRETURN ;
           }
           vx /= vm ; vy /= vm ; vz /= vm ;
           vp = strchr(up,':') ;
           if( vp == NULL ){
             ERROR_message("uni-directional warp '%s' :-) no dataset?",wp) ;
             free(wp) ; EXRETURN ;
           }
           up = vp+1 ;
       }

       /* check if there is a scale factor */

       vp = strchr(up,':') ;
       if( vp != NULL && isnumeric(*up) ){
         float wfac = (float)strtod(up,NULL) ;
         if( wfac == 0.0f ){
           ERROR_message("uni-directional warp '%s' :-) scale factor = 0?",wp) ;
           free(wp) ; EXRETURN ;
         }
         up = vp+1 ;
         vx *= wfac ; vy *= wfac ; vz *= wfac ;
       }

       /* now read dataset and do surgery on it */

       eset = THD_open_dataset(up) ;
       if( eset == NULL ){
         ERROR_message("Can't open dataset from file '%s'",up); free(wp); EXRETURN;
       }
       DSET_load(eset) ;
       if( !DSET_LOADED(eset) ){
         ERROR_message("Can't load dataset from file '%s'",up); free(wp); DSET_delete(eset); EXRETURN;
       }
       dim = THD_extract_float_brick(0,eset); dar = MRI_FLOAT_PTR(dim); DSET_unload(eset);
       nvox = dim->nvox ;
       xar = (float *)calloc(sizeof(float),nvox) ; /* bricks for output dataset */
       yar = (float *)calloc(sizeof(float),nvox) ;
       zar = (float *)calloc(sizeof(float),nvox) ;
       dset = EDIT_empty_copy(eset) ;
       EDIT_dset_items( dset ,
                          ADN_nvals , 3 ,
                          ADN_ntt   , 0 ,
                          ADN_datum_all , MRI_float ,
                        ADN_none ) ;
       EDIT_BRICK_FACTOR(dset,0,0.0) ; EDIT_substitute_brick(dset,0,MRI_float,xar) ;
       EDIT_BRICK_FACTOR(dset,1,0.0) ; EDIT_substitute_brick(dset,1,MRI_float,yar) ;
       EDIT_BRICK_FACTOR(dset,2,0.0) ; EDIT_substitute_brick(dset,2,MRI_float,zar) ;
       for( ii=0 ; ii < nvox ; ii++ ){
         xar[ii] = vx * dar[ii]; yar[ii] = vy * dar[ii]; zar[ii] = vz * dar[ii];
       }
       mri_free(dim) ;

     } else {  /*--- standard 3-brick warp ---*/

       dset = THD_open_dataset(wp) ;
       if( dset == NULL )
         ERROR_exit("can't open dataset from file '%s'",wp);
     }

     /*-- convert dataset to warp --*/

     if( verb ) ININFO_message("--- reading dataset") ;
     AA = IW3D_from_dataset(dset,do_empty,0) ;
     if( AA == NULL )
       ERROR_exit("can't make warp from dataset '%s'",wp);

     if( geomstring == NULL ){       /* first dataset => set geometry globals */
       geomstring = strdup(AA->geomstring) ;
       sname      = strdup(dset->atlas_space) ;
       nx = AA->nx; ny = AA->ny; nz = AA->nz; cmat = AA->cmat; imat = AA->imat;
     } else if( AA->nx != nx || AA->ny != ny || AA->nz != nz ){ /* check them */
       ERROR_exit("warp from dataset '%s' doesn't match earlier inputs in grid size",wp) ;
     }

     if( inset == NULL ){ DSET_unload(dset) ; inset = dset ; }  /* save as template */
     else               { DSET_delete(dset) ; }

     if( do_sqrt ){
#ifndef USE_SQRTPAIR
       BB = IW3D_sqrtinv(AA,NULL,MRI_LINEAR) ;  /* inverse AND sqrt */
       if( do_inv ){
         IW3D_destroy(AA) ; AA = BB ;
       } else {                                 /* must re-invert */
         AA = IW3D_invert(BB,NULL,MRI_LINEAR) ; IW3D_destroy(BB) ;
       }
#else
       IndexWarp3D_pair *YZ = IW3D_sqrtpair(AA,MRI_LINEAR) ;
       if( do_inv ){ AA = YZ->iwarp ; IW3D_destroy(YZ->fwarp) ; }
       else        { AA = YZ->fwarp ; IW3D_destroy(YZ->iwarp) ; }
       free(YZ) ;
#endif
     } else if( do_inv ){
       BB = IW3D_invert(AA,NULL,MRI_WSINC5); IW3D_destroy(AA); AA = BB;
     }

     AA->use_emat = 0 ;

     iwarp[nn-1] = AA ; free(wp) ; return ;
   }

   /* unreachable */

   return ;
}

/*----------------------------------------------------------------------------*/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii ;
   char *prefix = "NwarpCat" ;
   mat44        wmat      , tmat , smat , qmat ;
   IndexWarp3D *warp=NULL , *tarp=NULL ;
   THD_3dim_dataset *oset ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) CNW_help() ;

   /*-- bureaucracy --*/

   mainENTRY("3dNwarpCat"); machdep();
   AFNI_logger("3dNwarpCat",argc,argv);
   PRINT_VERSION("3dNwarpCat"); AUTHOR("Zhark the Warper");
   (void)COX_clock_time() ;
   putenv("AFNI_WSINC5_SILENT=YES") ;

   ZERO_MAT44(imat) ; ZERO_MAT44(cmat) ;
   for( ii=0 ; ii < NWMAX ; ii++ ){
     iwarp[ii] = NULL ; awarp[ii] = NULL ; iwfac[ii] = 1.0f ;
   }

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

     if( strncasecmp(argv[iarg],"-wfac",5) == 0 ){
       int nn ;
       if( iarg >= argc-1 ) ERROR_exit("no argument after '%s' :-(",argv[iarg]) ;
       if( !isdigit(argv[iarg][5]) ) ERROR_exit("illegal format for '%s' :-(",argv[iarg]) ;
       nn = (int)strtod(argv[iarg]+5,NULL) ;
       if( nn <= 0 || nn > NWMAX )
         ERROR_exit("illegal warp index in '%s' :-(",argv[iarg]) ;

       iwfac[nn-1] = (float)strtod(argv[++iarg],NULL) ;
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
       if( iwarp[nn-1] != NULL || awarp[nn-1] != NULL )
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

   /*-- cat them --*/

   LOAD_IDENT_MAT44(wmat) ;

   if( verb ) INFO_message("Initialize to Identity matrix") ;

   for( ii=0 ; ii < nwtop ; ii++ ){

     if( awarp[ii] != NULL ){  /* matrix to apply */

       qmat = *(awarp[ii]) ;
       if( geomstring != NULL ){             /* convert from xyz warp to ijk warp */
         tmat = MAT44_MUL(qmat,cmat) ;
         smat = MAT44_MUL(imat,tmat) ;
       } else {
         smat = qmat ;                       /* no conversion */
       }

       if( warp == NULL ){               /* thus far, only matrices */
         ININFO_message("warp #%d = Matrix-Matrix multiply",ii+1) ;
         if( iwfac[ii] != 1.0f )
           WARNING_message(" -- wfac%d=%g ==> ignoring this factor",ii+1,iwfac[ii]) ;
         qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;
       } else {                          /* apply matrix to nonlinear warp */
         ININFO_message("warp #%d = Matrix(Nwarp) compose",ii+1) ;
         if( iwfac[ii] != 1.0f )
           WARNING_message(" -- wfac%d=%g ==> ignoring this factor",ii+1,iwfac[ii]) ;
         tarp = IW3D_compose_w1m2(warp,smat,interp_code) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       free(awarp[ii]) ; awarp[ii] = NULL ;

     } else if( iwarp[ii] != NULL ){   /* nonlinear warp to apply */

       if( iwfac[ii] != 1.0f ) IW3D_scale( iwarp[ii] , iwfac[ii] ) ;

       if( warp == NULL ){            /* create nonlinear warp at this point */
         if( ii == 0 ){  /* first one ==> don't compose with identity matrix */
           ININFO_message("warp #%d = input Nwarp",ii+1) ;
           warp = IW3D_copy(iwarp[ii],1.0f) ;
         } else {                            /* compose with previous matrix */
           ININFO_message("warp #%d = Nwarp(Matrix) compose",ii+1) ;
           warp = IW3D_compose_m1w2(wmat,iwarp[ii],interp_code) ;
         }
         if( iwfac[ii] != 1.0f )
           ININFO_message(" -- Nwarp scaled by wfac=%g",ii+1,iwfac[ii]) ;
       } else {          /* already have nonlinear warp, apply new one to it */
         ININFO_message("warp #%d = Nwarp(Nwarp) compose",ii+1) ;
         if( iwfac[ii] != 1.0f )
           ININFO_message(" -- Nwarp scaled by wfac=%g",ii+1,iwfac[ii]) ;
         tarp = IW3D_compose(warp,iwarp[ii],interp_code) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       IW3D_destroy(iwarp[ii]) ; iwarp[ii] = NULL ;

     } else {

       if( verb ) ININFO_message("warp #%d = skipping",ii+1) ;

     }

   }

   /*--- write result to disk for future fun fun fun in the sun sun sun ---*/

   if( warp == NULL ){                      /* just write a matrix [for Ziad] */
     char *fname = malloc(sizeof(char)*(strlen(prefix)+16)) ; FILE *fp ;
     float a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ;
     if( strcmp(prefix,"-") == 0 || strncmp(prefix,"stdout",6) == 0 ){
       fp = stdout ; strcpy(fname,"stdout") ;
     } else {
       strcpy(fname,prefix) ;
       if( strstr(fname,".1D") == NULL ) strcat(fname,".aff12.1D") ;
       fp = fopen(fname,"w") ;
       if( fp == NULL ) ERROR_exit("Can't open output file %s",fname) ;
     }
     UNLOAD_MAT44(wmat,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34) ;
     fprintf(fp,
             " %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g\n",
             a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;
     if( verb && fp != stdout ) INFO_message("Wrote matrix to %s",fname) ;
     if( fp != stdout ) fclose(fp) ;
     exit(0) ;
   }

   /** write a nonlinear warp dataset **/

   IW3D_adopt_dataset( warp , inset ) ;
   oset = IW3D_to_dataset( warp , prefix ) ;
   tross_Copy_History( inset , oset ) ;
   tross_Make_History( "3dNwarpCat" , argc,argv , oset ) ;
   if( sname != NULL ) MCW_strncpy( oset->atlas_space , sname , THD_MAX_NAME ) ;
   DSET_write(oset) ; WROTE_DSET(oset) ;

   /*--- run away screaming into the night, never to be seen again ---*/

   INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                COX_cpu_time() , COX_clock_time() ) ;

   exit(0) ;
}
