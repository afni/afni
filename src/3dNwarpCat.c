#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_genalign.c"
#include "mri_genalign_util.c"
#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/

void NWC_help(void)
{
   printf(
    "Usage: 3dNwarpCat [options] warp1 warp2 ...\n"
    "------\n"
    " * This program catenates (composes) 3D warps defined on a grid,\n"
    "   OR via a matrix.\n"
    "  ++ All transformations are from DICOM xyz (in mm) to DICOM xyz.\n"
    "\n"
    " * Matrix warps are in files that end in '.1D' or in '.txt'.  A matrix\n"
    "   warp file should have 12 numbers in it, as output (for example), by\n"
    "   '3dAllineate -1Dmatrix_save'.\n"
    "  ++ The matrix (affine) warp can have either 12 numbers on one row,\n"
    "     or be in the 3x4 format.\n"
    "  ++ The 12-numbers-on-one-row format is preferred, and is the format\n"
    "     output by the '-1Dmatrix_save' option in 3dvolreg and 3dAllineate.\n"
    "  ++ The matrix warp is a transformation of coordinates, not voxels,\n"
    "     and its use presumes the correctness of the voxel-to-coordinate\n"
    "     transformation stored in the header of the datasets involved.\n"
    "\n"
    " * Nonlinear warps are in dataset files (AFNI .HEAD/.BRIK or NIfTI .nii)\n"
    "   with 3 sub-bricks giving the DICOM order xyz grid displacements in mm.\n"
    "  ++ Note that it is not required that the xyz order of voxel storage be in\n"
    "     DICOM order, just that the displacements be in DICOM order (and sign).\n"
    "  ++ However, it is important that the warp dataset coordinate order be\n"
    "     properly specified in the dataset header, since warps are applied\n"
    "     based on coordinates, not on voxels.\n"
    "  ++ Also note again that displacements are in mm, NOT in voxel.\n"
    "  ++ You can 'edit' the warp on the command line by using the 'FAC:'\n"
    "     scaling prefix, described later. This input editing could be used\n"
    "     to change the sign of the xyz displacments, if needed.\n"
    "\n"
    " * If all the input warps are matrices, then the output is a matrix\n"
    "   and will be written to the file 'prefix.aff12.1D'.\n"
    "  ++ Unless the prefix already contains the string '.1D', in which case\n"
    "     the filename is just the prefix.\n"
    "  ++ If 'prefix' is just 'stdout', then the output matrix is written\n"
    "     to standard output.\n"
    "  ++ In any of these cases, the output format is 12 numbers in one row.\n"
    "\n"
    " * If any of the input warps are datasets, they must all be defined on\n"
    "   the same 3D grid!\n"
    "  ++ And of course, then the output will be a dataset on the same grid.\n"
    "  ++ However, you can expand the grid using the '-expad' option.\n"
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
    " * The final warp may also be inverted simply by adding the '-iwarp' option, as in\n"
    "\n"
    "   3dNwarpCat -prefix Fred_total_WARPINV -iwarp -warp1 Fred_WARP+tlrc.HEAD -warp2 Fred.Xat.1D \n"
    "\n"
    " * Other functions you can apply to modify a 3D dataset warp are:\n"
    "    SQRT(datasetname) to get the square root of a warp\n"
    "    SQRTINV(datasetname) to get the inverse square root of a warp\n"
    "   However, you can't do more complex expressions, such as 'SQRT(SQRT(warp))'.\n"
    "   If you think you need something so rococo, use 3dNwarpCalc.  Or think again.\n"
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
    " * You can scale the displacements in a 3D warp file via the 'FAC:' prefix, as in\n"
    "     FAC:0.6,0.4,-0.2:fred_WARP.nii\n"
    "   which will scale the x-displacements by 0.6, the y-displacements by 0.4, and\n"
    "   the z-displacments by -0.2.\n"
    "\n"
    " * Finally, you can input a warp catenation string directly as in the '-nwarp'\n"
    "   option of 3dNwarpApply, as in\n"
    "\n"
    "   3dNwarpCat -prefix Fred_total_WARP 'Fred_WARP+tlrc.HEAD Fred.Xat.1D' \n"
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
    " -space sss  == attach string 'sss' to the output dataset as its atlas\n"
    "                space marker.\n"
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
    "                   get professional counseling.\n"
    "\n"
    " -iwarp      == Invert the final warp before output.\n"
    "\n"
    " -expad PP   == Pad the nonlinear warps by 'PP' voxels in all directions.\n"
    "                The warp displacements are extended by linear extrapolation\n"
    "                from the faces of the input grid.\n"
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

static int  nwtop=0 ;
static char *cwarp[NWMAX] ;
static char *sname=NULL ;

static int verb=0 , interp_code=MRI_WSINC5 ;

/*----------------------------------------------------------------------------*/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ii , do_iwarp=0 ;
   char *prefix = "NwarpCat" ;
   mat44 wmat , smat , qmat ;
   THD_3dim_dataset *oset=NULL ;
   char *cwarp_all=NULL ; int ntot=0 ;

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) NWC_help() ;

   /*-- bureaucracy --*/

   mainENTRY("3dNwarpCat"); machdep();
   AFNI_logger("3dNwarpCat",argc,argv);
   PRINT_VERSION("3dNwarpCat"); AUTHOR("Zhark the Warper");
   (void)COX_clock_time() ;
   putenv("AFNI_WSINC5_SILENT=YES") ;

   /*-- initialization --*/

   CW_no_expad = 1 ;  /* don't allow automatic padding of input warp */
   Hverb = 0 ;        /* don't be verbose inside mri_nwarp.c */
   for( ii=0 ; ii < NWMAX ; ii++ ) cwarp[ii] = NULL ;

   /*-- scan args --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---------------*/

     if( strcasecmp(argv[iarg],"-iwarp") == 0 ){
       do_iwarp = 1 ; iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-space") == 0 ){
       sname = strdup(argv[++iarg]) ; iarg++ ; continue ;
     }

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

     if( strcasecmp(argv[iarg],"-expad") == 0 ){
       int expad ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       expad = (int)strtod(argv[iarg],NULL) ;
       if( expad < 0 ){
         WARNING_message("-expad %d is illegal and is set to zero",expad) ;
         expad = 0 ;
       }
       CW_extra_pad = expad ;  /* this is how we force extra padding */
       iarg++ ; continue ;
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
       if( cwarp[nn-1] != NULL )
         ERROR_exit("'%s': you can't specify warp #%d more than once :-(",argv[iarg],nn) ;
       cwarp[nn-1] = strdup(argv[++iarg]) ;
       if( nn > nwtop ) nwtop = nn ;
       iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_message("Confusingly Unknown option '%s' :-(",argv[iarg]) ;
     suggest_best_prog_option(argv[0],argv[iarg]) ;
     exit(1) ;

   }

   /*-- load any warps left on the command line, after options --*/

   for( ; iarg < argc && nwtop < NWMAX-1 ; iarg++ )
     cwarp[nwtop++] = strdup(argv[iarg]) ;

   /*-- check if all warp strings are affine matrices --*/

#undef  AFFINE_WARP_STRING
#define AFFINE_WARP_STRING(ss)     \
  ( strstr((ss)," ") == NULL &&    \
    ( strcasestr((ss),".1D") != NULL || strcasestr((ss),".txt") != NULL ) )

   for( ntot=ii=0 ; ii < nwtop ; ii++ ){
     if( cwarp[ii] == NULL ) continue ;
     ntot += strlen(cwarp[ii]) ;
     if( ! AFFINE_WARP_STRING(cwarp[ii]) ) break ;  /* not affine */
   }
   if( ntot == 0 ) ERROR_exit("No warps on command line?!") ;

   if( ii == nwtop ){  /* all are affine (this is for Ziad) */
     char *fname = malloc(sizeof(char)*(strlen(prefix)+16)) ; FILE *fp ;
     float a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ;

     LOAD_IDENT_MAT44(wmat) ;
     for( ii=0 ; ii < nwtop ; ii++ ){
       if( cwarp[ii] == NULL ) continue ;
       smat = CW_read_affine_warp_OLD(cwarp[ii]) ;
       qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;
     }

     if( strcmp(prefix,"-") == 0 || strncmp(prefix,"stdout",6) == 0 ){
       fp = stdout ; strcpy(fname,"stdout") ;
     } else {
       strcpy(fname,prefix) ;
       if( strstr(fname,".1D") == NULL ) strcat(fname,".aff12.1D") ;
       fp = fopen(fname,"w") ;
       if( fp == NULL ) ERROR_exit("Can't open output matrix file %s",fname) ;
     }
     if( do_iwarp ){
       qmat = MAT44_INV(wmat) ; wmat = qmat ;
     }
     UNLOAD_MAT44(wmat,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34) ;
     fprintf(fp,
             " %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g %13.6g\n",
             a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;
     if( verb && fp != stdout ) INFO_message("Wrote matrix to %s",fname) ;
     if( fp != stdout ) fclose(fp) ;
     exit(0) ;
   }

   /*** at least one nonlinear warp ==> cat all strings, use library function to read ***/

   cwarp_all = (char *)calloc(sizeof(char),(ntot+NWMAX)*2) ;
   for( ii=0 ; ii < nwtop ; ii++ ){
     if( cwarp[ii] != NULL ){ strcat(cwarp_all,cwarp[ii]) ; strcat(cwarp_all," ") ; }
   }

   oset = IW3D_read_catenated_warp( cwarp_all ) ;  /* process all of them at once */

   if( do_iwarp ){            /* 18 Jul 2014 */
     THD_3dim_dataset *qwarp ;
     if( verb ) fprintf(stderr,"Applying -iwarp option") ;
     qwarp = THD_nwarp_invert(oset) ;
     DSET_delete(oset) ;
     oset = qwarp ;
     if( verb ) fprintf(stderr,"\n") ;
   }
   tross_Make_History( "3dNwarpCat" , argc,argv , oset ) ;
   if( sname != NULL ) MCW_strncpy( oset->atlas_space , sname , THD_MAX_NAME ) ;
   EDIT_dset_items( oset , ADN_prefix,prefix , ADN_none ) ;
   DSET_write(oset) ; WROTE_DSET(oset) ;

   /*--- run away screaming into the night, never to be seen again ---*/

   INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                COX_cpu_time() , COX_clock_time() ) ;

   exit(0) ;
}
