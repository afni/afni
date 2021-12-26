#include "mrilib.h"

/*--------------------------- internal prototypes ----------------------------*/
static mat44_vec * read_affine_warp_vector( char *cp ) ;
static void        glue_affine_warp_vector( mat44_vec ** , mat44_vec ** ) ;

static THD_3dim_dataset *default_MNI152_mask_dataset(void) ;
static void hollow_out_mask( int nx, int ny, int nz , byte *mask ) ;

float_pair compare_matrix_pair( THD_3dim_dataset *mset , byte *mask ,
                                mat44 amat , mat44 bmat ) ;
/*----------------------------------------------------------------------------*/

static void show_help(void)
{
   printf("\n"
    "Usage: 3dCompareAffine [options]   ~1\n"
    "\n"
    "This program compares two (or more) affine spatial transformations\n"
    "on a dataset, and outputs various measurements of how much these\n"
    "transformations differ in spatial displacements.\n"
    "\n"
    "One use for this program is to compare affine alignment matrices\n"
    "from different methods for aligning 3D brain images.\n"
    "\n"
    "Transformation matrices are specified in a few different ways:\n"
    "  * ASCII filename containing 12 numbers arranged in 3 lines:\n"
    "     u11 u12 u13 v1\n"
    "     u21 u22 u23 v2\n"
    "     u31 u32 u33 v3\n"
    "  * ASCII filename containing with 12 numbers in a single line:\n"
    "     u11 u12 u13 v1 u21 u22 u23 v2 u31 u32 u33 v3\n"
    "    This is the '.aff12.1D' format output by 3dAllineate,\n"
    "    and this is the only format that can contain more than\n"
    "    one matrix in one file.\n"
    "  * Directly on the command line:\n"
    "     'MATRIX(u11,u12,u13,v1,u21,u22,u23,v2,u31,u32,u33,v3)'\n"
    "\n"
    "-------\n"
    "Options\n"
    "-------\n"
    "-mask mmm    = Read in dataset 'mmm' and use non-zero voxels\n"
    "               as the region over which to compare the two\n"
    "               affine transformations.\n"
    "              * You can specify the use of the MNI152 built-in template\n"
    "                mask by '-mask MNI152'.\n"
    "              * In the future, perhaps other built-in masks will be created?\n"
    " *OR*\n"
    "-dset ddd    = Read in dataset 'mmm', compute an automask from\n"
    "               it (via program 3dAutomask), and use that mask\n"
    "               as the spatial region for comparison.\n"
    "              * If you don't give EITHER '-mask' or '-dset', then\n"
    "                this program will use an internal mask derived from\n"
    "                the MNI152 template (skull off).\n"
    "\n"
    "-affine aaa  = Input an affine transformation (file or 'MATRIX').\n"
    " *OR*         * You can give more than one '-affine' option to\n"
    "-matrix aaa     input multiple files.\n"
    "              * You can also put multiple filenames after the\n"
    "                '-affine' option, as in '-affine aaa.aff12.1D bbb.aff12.1D'\n"
    "              * The first matrix found in the first '-affine' option\n"
    "                is the base transformation to which all following\n"
    "                transformations will be compared.\n"
    "------\n"
    "Method\n"
    "------\n"
    "1) The input mask is hollowed out -- that is, all nonzero mask voxels that\n"
    "    do NOT neighbor a zero voxel are turned to zero. Thus, only the 'edge'\n"
    "    voxels are used in the computations below. For example, the default\n"
    "    MNI152 mask has 1818562 nonzero voxels before hollowing out, and\n"
    "    has 74668 after hollowing out. The hollowing out algorithm is described\n"
    "    in the help for program 3dAutomask.\n"
    "2) For each surviving voxel, the xyz coordinates are calculated and then\n"
    "   transformed by the pair of matrices being compared. Then the Euclidean\n"
    "   distance between these two sets of transformed xyz vectors is calculated.\n"
    "   The outputs for each comparison are the maximum distance and the\n"
    "   root-mean-square (RMS) distance, over the set of hollowed out mask voxels.\n"
    "\n"
    "The purpose of this program is to compare the results from 3dAllineate\n"
    "and other registration programs, run under different conditions.\n"
#ifdef USE_FLEV
    "In particular, for runs with and without AFNI_RANDOMIZE_ROUNDING enabled.\n"
#endif
    "\n"
    "-- Author: RWCox - Mar 2020 at the Tulsa bootcamp\n"
    "\n"
   ) ;

   exit(0) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nopt=1 ;
   mat44_vec *amat=NULL ;  /* vector of matrices */
   THD_3dim_dataset *mset=NULL ;
   byte *mask=NULL ;
   char *internal_mask_name = "MNI152" ;
   int nmask_org , nmask_hol ;
   float_pair pval ; int ii ; float abar,bbar ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) show_help() ;

   /*---------------------------*/

   while( nopt < argc ){  /* loop over options */

     /*--- -matrix / -affine ---*/

     if( strcasecmp(argv[nopt],"-matrix") == 0 ||
         strcasecmp(argv[nopt],"-affine") == 0   ){
       mat44_vec *bmat ;
       if( ++nopt >= argc )
         ERROR_exit("%s - need an argument after this option :(",argv[nopt-1]) ;
       for( ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){
         bmat = read_affine_warp_vector( argv[nopt] ) ;
         if( bmat == NULL )
           ERROR_exit("%s - cannot read matrix from %s :(",argv[nopt-1],argv[nopt]) ;
         if( amat != NULL ){
           glue_affine_warp_vector( &amat , &bmat ) ;
           DESTROY_mat44_vec(bmat) ;
         } else {
           amat = bmat ;
         }
       }
       continue ;
     }

     /*--- -mask ---*/

     if( strcasecmp(argv[nopt],"-mask") == 0 ){
       /* no duplication */
       if( mset != NULL )
         ERROR_exit("%s - You can't input two datasets for masks :(",argv[nopt]) ;
       /* no input? */
       if( ++nopt >= argc )
         ERROR_exit("%s - need an argument after this option :(",argv[nopt-1]) ;
       if( strcasecmp(argv[nopt],"MNI152") == 0 ){  /* special case */
         internal_mask_name = "MNI152" ;
         nopt++ ; continue ;
       }
       mset = THD_open_dataset( argv[nopt] ) ;
       /* can't open input? */
       if( !ISVALID_DSET(mset) )
         ERROR_exit("%s - can't open dataset %s",argv[nopt-1],argv[nopt]) ;
       DSET_load(mset) ;
       /* can't load input into memory? */
       if( !DSET_LOADED(mset) )
         ERROR_exit("%s - can't load dataset %s",argv[nopt-1],argv[nopt]) ;
       /* too much data? */
       if( DSET_NVALS(mset) > 1 )
         WARNING_message("%s - dataset %s has %d volumes - using [0]",
                         argv[nopt-1],argv[nopt],DSET_NVALS(mset)     ) ;
       /* actually make the byte mask */
       mask = THD_makemask( mset , 0 , 1.0f , -1.0f ) ;
       DSET_unload(mset) ;
       nopt++ ; continue ;
     }

     /*--- -dset ---*/

     if( strcasecmp(argv[nopt],"-dset") == 0 ){
       THD_3dim_dataset *dset ; char cmd[2048] , fnam[32] , *uuu ;
       /* no duplication */
       if( mset != NULL )
         ERROR_exit("%s - You can't input two datasets for masks :(",argv[nopt]) ;
       /* no input? */
       if( ++nopt >= argc )
         ERROR_exit("%s - need an argument after this option :(",argv[nopt-1]) ;
       dset = THD_open_dataset( argv[nopt] ) ;
       /* can't open input? */
       if( !ISVALID_DSET(dset) )
         ERROR_exit("%s - can't open dataset %s",argv[nopt-1],argv[nopt]) ;
       DSET_delete(dset) ;  /* just testing for existence */
       /* brute force: run 3dAutomask via system call */
       uuu = UNIQ_idcode_11() ; strcpy(fnam,uuu) ; strcat(fnam,".nii") ;
       sprintf( cmd , "3dAutomask -q -clfrac 0.3 -prefix %s %s",
                      fnam , argv[nopt] ) ;
       system( cmd ) ;
       /* load the results */
       mset = THD_open_dataset( fnam ) ;
       if( !ISVALID_DSET(mset) )
         ERROR_exit("%s - can't open dataset %s",argv[nopt-1],fnam) ;
       DSET_load(mset) ;
       if( !DSET_LOADED(mset) )
         ERROR_exit("%s - can't load dataset %s",argv[nopt-1],fnam) ;
       /* actually make the byte mask */
       mask = THD_makemask( mset , 0 , 1.0f , -1.0f ) ;
       DSET_unload(mset) ;
       nopt++ ; continue ;
     }

     /*---   WTF?!   ---*/

     ERROR_exit("Don't understand option '%s'",argv[nopt]) ;

   } /* end of loop over options */

   /*---------------------------*/

   /* check for valid inputs */

   if( amat == NULL || amat->nmar < 2 )
     ERROR_exit("Need at least 2 input matrices to compare!") ;

   /* create the mask if nothing was given earlier */
   /* [in the future, this might depend on internal_mask_name] */

   if( mset == NULL || mask == NULL ){
     INFO_message("Using default internal mask: %s",internal_mask_name) ;
     mset = default_MNI152_mask_dataset() ;
     mask = (byte *)DSET_BRICK_ARRAY(mset,0) ;
   }

   /* output some info about the mask */

   nmask_org = THD_countmask( DSET_NVOX(mset) , mask ) ;
   hollow_out_mask( DSET_NX(mset), DSET_NY(mset), DSET_NZ(mset), mask ) ;
   nmask_hol = THD_countmask( DSET_NVOX(mset) , mask ) ;
   ININFO_message("Voxel counts: input mask = %d   hollowed mask = %d",
                  nmask_org , nmask_hol ) ;
   if( nmask_hol < 1 )
     ERROR_exit("Can't process with 0 mask voxels :(") ;

   /*---------- I guess I've put this off as long as possible.
                Have to compare amat#0 vs amat#i for i=1, 2, .... ----------*/

   abar = bbar = 0.0f ;
   printf("#            Max      RMS   (mm)\n") ;
   for( ii=1 ; ii < amat->nmar ; ii++ ){
     pval = compare_matrix_pair( mset , mask ,
                               amat->mar[0] , amat->mar[ii] ) ;
     printf("[0]-[%d] = %g  %g\n",ii,pval.a,pval.b) ;
     abar += pval.a ; bbar += pval.b ;
   }
   abar /= (amat->nmar-1) ;
   bbar /= (amat->nmar-1) ;
   printf("mean     = %.5g  %.5g\n",abar,bbar) ;

   /* run away screaming in fear of the coronavirus! */

   exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* Compare 2 matrices */

#define LOAD_MAT44_FROM_ARRAY(MM,zz)                        \
  LOAD_MAT44( MM , zz[0],zz[1],zz[2],zz[3],zz[4] ,zz[5] ,   \
                   zz[6],zz[7],zz[8],zz[9],zz[10],zz[11] )

float_pair compare_matrix_pair( THD_3dim_dataset *mset , byte *mask ,
                                mat44 amat , mat44 bmat              )
{
   int nx,ny,nz,nxy,nxyz , qq,ii,jj,kk , nsum=0 ;
   THD_fvec3 vxyz ; THD_ivec3 vijk ;
   float ax,ay,az , bx,by,bz , siz=0.0f,sum=0.0f,dif ;
   float_pair rval ;

   nx = DSET_NX(mset) ; ny = DSET_NY(mset) ; nz = DSET_NZ(mset) ;
   nxy = nx*ny ; nxyz = nxy*nz ;

   for( qq=0 ; qq < nxyz ; qq++ ){
     if( mask[qq] == 0 ) continue ;
     AFNI_1D_to_3D_index( qq , ii,jj,kk , nx,nxy ) ;
     LOAD_IVEC3( vijk , ii,jj,kk ) ;
     vxyz = THD_3dind_to_dicomm_no_wod( mset , vijk ) ;
     MAT44_VEC(amat,vxyz.xyz[0],vxyz.xyz[1],vxyz.xyz[2],ax,ay,az) ;
     MAT44_VEC(bmat,vxyz.xyz[0],vxyz.xyz[1],vxyz.xyz[2],bx,by,bz) ;
     dif = (ax-bx)*(ax-bx) + (ay-by)*(ay-by) + (az-bz)*(az-bz) ;
     if( dif > siz ) siz = dif ;
     sum += dif ; nsum++ ;
   }

   rval.a = sqrtf(siz) ;
   rval.b = sqrtf(sum/nsum) ;
   return rval ;
}

/*----------------------------------------------------------------------------*/
/* Adapted from mri_nwarp.c */

static mat44_vec * read_affine_warp_vector( char *cp )
{
   mat44 mmm ; mat44_vec *mvv=NULL ;
   MRI_IMAGE *qim ; float *qar, *tar ; char *wp , *ocp ;
   int do_inv=0 , do_sqrt=0 , ii , nmat ;

ENTRY("read_affine_warp_vector") ;

   if( cp == NULL || *cp == '\0' ) RETURN(mvv) ;
   ocp = cp ;

   if( strncasecmp(cp,"INV(",4) == 0 ){                 /* set inversion flag */
     cp += 4 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERT(",7) == 0 ){
     cp += 7 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERSE(",8) == 0 ){
     cp += 8 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"SQRT(",5) == 0 ){        /* set squareroot flag */
     cp += 5 ; do_sqrt = 1 ;
   } else if( strncasecmp(cp,"SQRTINV(",8) == 0 || strncasecmp(cp,"INVSQRT(",8) == 0 ){
     cp += 8 ; do_inv = do_sqrt = 1 ;                       /* set both flags */
   }
   wp = strdup(cp) ; ii = strlen(wp) ;
   if( ii < 4 ){
     ERROR_message("input filename '%s' to read_affine_warp is too short :(",wp) ;
     free(wp) ; RETURN(mvv) ;
   }
   if( wp[ii-1] == ')' ) wp[ii-1] = '\0' ;

   if( strncmp(wp,"MATRIX(",7) == 0 ){  /* special case */
     int nn ;
     qim = mri_new( 1 , 12 , MRI_float ) ;
     qar = MRI_FLOAT_PTR(qim) ;
     nn = sscanf(wp,"MATRIX(%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f",
                    qar+0 , qar+1 , qar+2 , qar+3 ,
                    qar+4 , qar+5 , qar+6 , qar+7 ,
                    qar+8 , qar+9 , qar+10, qar+11 ) ;
     if( nn < 12 )
       WARNING_message("'%s' - found only %d (<12) numeric values",wp,nn) ;
   } else {
     qim = mri_read_1D(wp) ;
   }

   /* bad data? */

   if( qim == NULL || qim->nvox < 12 ){
     ERROR_message("Cannot read affine warp from file '%s'",wp); free(wp); RETURN(mvv);
   }

   if( qim->nx == 3 && qim->ny == 4 ){        /* single matrix in 3x4 'Xat.1D' format? */
     MRI_IMAGE *tim = mri_rowmajorize_1D(qim); mri_free(qim); qim = tim; /* make it 12x1 */
   } else {
     MRI_IMAGE *tim ;
     if( qim->ny != 12 ){
       ERROR_message("Affine warp file '%s': have %d, not 12, values per row",wp,qim->ny) ;
       free(wp) ; RETURN(mvv) ;
     }
     tim = mri_transpose(qim) ; mri_free(qim) ; qim = tim ;  /* flip to column major order */
   }

   if( qim->nx != 12 ){
     ERROR_message("read_affine_warp: nx == %d (!= 12) (this message should never happen!)",qim->nx) ;
     free(wp); RETURN(mvv);
   }

   /* at this point, qim->nx = 12, and qim->ny = number of matrices */

   nmat = qim->ny ;
   mvv  = (mat44_vec *)malloc(sizeof(mat44_vec)) ;
   mvv->nmar = nmat ;
   mvv->mar  = (mat44 *)malloc(sizeof(mat44)*nmat) ;

   qar = MRI_FLOAT_PTR(qim) ;
   for( ii=0 ; ii < nmat ; ii++ ){
     tar = qar + ii*12 ;
     LOAD_MAT44(mmm,tar[0],tar[1],tar[2] ,tar[3],
                    tar[4],tar[5],tar[6] ,tar[7],
                    tar[8],tar[9],tar[10],tar[11]) ;

     if( do_inv  ){ mat44 imm=MAT44_INV(mmm)     ; mmm=imm; } /* invert */
     if( do_sqrt ){ mat44 smm=THD_mat44_sqrt(mmm); mmm=smm; } /* sqrt */

     mvv->mar[ii] = mmm ;
   }

   mri_free(qim) ; free(wp) ;

   NI_strncpy(mvv->fname,ocp,128) ; RETURN(mvv) ;
}

/*----------------------------------------------------------------------------*/

static void glue_affine_warp_vector( mat44_vec **avec , mat44_vec **bvec )
{
   int na , nb , ii ;
   mat44_vec *aa , *bb ;

   if( avec == NULL || bvec == NULL ) return ;  /* no input? */

   aa = *avec ;
   bb = *bvec ;
   if( bb == NULL || bb->nmar <= 0 || bb->mar == NULL ) return ; /* nothing? */
   nb = bb->nmar ;

   if( aa == NULL ){                       /* create something in the output */
     aa = (mat44_vec *)malloc(sizeof(mat44_vec)) ;
     aa->nmar = 0 ;
     aa->mar  = NULL ;
   }
   na = aa->nmar ;

   if( na == 0 ){                 /* just copy bvec contents into empty avec */
     aa->nmar = nb ;
     aa->mar  = (mat44 *)calloc(nb,sizeof(mat44)) ;
     memcpy( aa->mar , bb->mar , nb*sizeof(mat44) ) ;
     NI_strncpy( aa->fname , bb->fname, 128 ) ;
   } else {                                 /* copy onto end of exiting avec */
     aa->nmar = na + nb ;
     aa->mar  = (mat44 *)realloc( aa->mar , aa->nmar * sizeof(mat44) ) ;
     memcpy( aa->mar + na , bb->mar , nb * sizeof(mat44) ) ;
   }

   *avec = aa ; return ;
}

/*----------------------------------------------------------------------------*/
/* Make an MNI152 mask template */

/* String below (887 lines, 64697 char) was created by these 2 commands:
     3dAutomask -prefix q.nii ~/abin/MNI152_2009_template.nii.gz'[0]'
     3dMaskToASCII q.nii > q.txt
   The quotes were added manually to q.txt when it was included
   into this file.
*/

#if 1
static char *textmask =
 "eNrsnU2StDqWpiEpK3pQ1vSwBm2XXEIPe5BW9FJyCTmsQVqipWkpWgJDBhh0uOMOLuk8geSO"
 "fxHx3XMGN/OeG+EID9DPo1fvKQoNDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0N"
 "DQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0N"
 "DQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ2Nd0cL+Q7y"
 "C+TnUs5PlZwfIG9L/ZtoaGhoaGhoaGhoaGhoaGhoaGho/JyYID8DA50b+ByAsmMv5weAtQ7y"
 "dgH4SvlJYa2GhoaGhoaGhoaGhoaGhoaGhobG14UFluqAmTpgnXapIC9fwCygrKV8LtxliAvf"
 "xKwPg4aGhoaGhoaGhoaGhoaGhoaGhsb7YyQ9aa7wdaG8fIEBIK5bajFvAO4yxIX8CHlHNgv/"
 "qQ+JhoaGhoaGhoaGhoaGhoaGhoaGxnkBKNIASzXATJGNLnCBGQSuUybcdajQBZsFuoEJ8iPk"
 "i7/ow6OhoaGhoaGhoaGhoaGhoaGhoaGRHQ6Q4wAslR1f84SvFiCuIYhL+Qng7mmuBmpNq6Gh"
 "oaGhoaGhoaGhoaGhoaGh8XvGH5D/F+SJCY4gEB1kODqBK8AM0JTz0ExgmiNA3AEgroN22lxX"
 "g5lcDYhCQ96S28H/0WdZQ0NDQ0NDQ0NDQ0NDQ0NDQ0PjK2KkPDDTUWSLNTDBBgSffxRWhKD/"
 "VRgRgv63WUQIOiyLCDvdR16EmhbyBvLFAkLWs+AuuxqQZS1Ihmfy1oX80OjDr6GhoaGhoaGh"
 "oaGhoaGhoaGhcUJYYKlORI5D5UTmONVOZItTI7PCsZXLW42dfJp/6GWrVrfIVq12+QihQeaS"
 "l+74kpcaOgPEHSF/ob5i3mW7GmTCXapHNqBlLXnrkmUtUfdK3yINDQ0NDQ0NDQ0NDQ0NDQ0N"
 "jT9J/Cfk/ylm/yXaArRmKYcYqtm+WBoT//xFTyoxxEt+obzA7Cww0GtegJdXlipd+JKX4OUM"
 "DZogPwDE/fQGcuDuBPlizHY1OMmyluqRjeitW8CF4Tn8u76iGhoaGhoaGhoaGhoaGhoaGhq/"
 "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"
 "/H+hB7dJ===8530021"
;
#endif

static THD_3dim_dataset *default_MNI152_mask_dataset(void)
{
   THD_3dim_dataset *mset ;
   bytevec *bvmask ;

   /* create an empty shell of a dataset:
      geometry string below is from output of
        3dinfo ~/abin/MNI152_2009_template.nii.gz */

   mset = EDIT_geometry_constructor(
            "MATRIX(-1,0,0,96,0,-1,0,132,0,0,1,-78):193,229,193" ,
            "JeepRubicon" ) ;

   /* create binary mask from the big string
      (64697 char expands to 8530021 voxels) */

   bvmask = THD_create_mask_from_string(textmask) ;

   if( bvmask->nar != DSET_NVOX(mset) )  /* should not happen */
     ERROR_exit("Internal error creating MNI mask dataset :(\n"
                "nar = %d  nvals = %d" ,
                bvmask->nar , DSET_NVOX(mset) ) ;

   /* glue the mask onto the empty dataset shell */

   EDIT_substitute_brick( mset , 0 , MRI_byte , bvmask->ar ) ;

   return mset ;
}

/*----------------------------------------------------------------------------*/
/* Hollow out a mask */

static void hollow_out_mask( int nx, int ny, int nz , byte *mask )
{
   int ii,jj,kk,qq=0 , nx1=nx-1 , ny1=ny-1 , nz1=nz-1 , nxy=nx*ny ;
   byte *qmask=NULL ;

   if( nx < 5 || ny < 5 || nz < 5 || mask == NULL ) return ;

   qmask = (byte *)malloc(nx*ny*nz*sizeof(byte)) ;
   memcpy(qmask,mask,nx*ny*nz*sizeof(byte)) ;
   for( kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,qq++ ){
       if( mask[qq] == 0 ) continue ;  /* already zero */
       /* if any neighbors are zero, this is at the edge, so keep it */
       if( ii > 0   && mask[qq-1]   == 0 ) continue ;
       if( ii < nx1 && mask[qq+1]   == 0 ) continue ;
       if( jj > 0   && mask[qq-nx]  == 0 ) continue ;
       if( jj < ny1 && mask[qq+nx]  == 0 ) continue ;
       if( kk > 0   && mask[qq-nxy] == 0 ) continue ;
       if( kk < nz1 && mask[qq+nxy] == 0 ) continue ;
       qmask[qq] = 0 ;      /* all neighbors are inside --> erase it */
   }}}
   memcpy(mask,qmask,nx*ny*nz*sizeof(byte)) ; free(qmask) ; return ;
}
