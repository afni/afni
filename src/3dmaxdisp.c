#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL ;
   int nx,ny,nz,nxy,nxyz ;
   int ii,jj,kk, mm, ip,jp,kp, im,jm,km, nmsk=0 , qq , verb=0 , iarg ;
   byte *msk=NULL , *dsk=NULL ;
   THD_ivec3 iv ;
   THD_fvec3 *dispvec ; int num_dispvec=0 ;
   MRI_IMAGE *matim=NULL; float *matim_far=NULL; int matim_nx=0,matim_ny=0;
   mat44 qmat; float xx,yy,zz , dd,dmax,dbar ;

   mainENTRY("3dmaxdisp") ; machdep() ;
   PRINT_VERSION("3dmaxdisp"); AUTHOR("Zhark the Displacer");

   /*-----------------------------------------------------------------------*/
   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
      "Program 3dmaxdisp!\n"
      "\n"
      " * Reads in a 3D dataset and a DICOM-based affine matrix\n"
      " * Outputs the average and maximum displacement that the matrix produces\n"
      "   when applied to the edge voxels of the 3D dataset's automask.\n"
      " * The motivation for this program was to check if two\n"
      "   affine transformation matrices are 'close' -- but of course,\n"
      "   you can use this program for anything else you like.\n"
      " * Suppose you have two affine transformation matrices that\n"
      "   transform a dataset XX.nii to MNI space, stored in files\n"
      "    AA.aff12.1D and BB.aff12.1D\n"
      "   and they aren't identical but they are close. How close?\n"
      " * If these matrices are from 3dAllineate (-1Dmatrix_save),\n"
      "   then each matrix transforms DICOM-order coordinates\n"
      "   in XX.nii to MNI-space.\n"
      " * So Inverse(AA) transforms MNI-space to XX-space\n"
      " * So Inverse(AA)*BB transforms MNI-space to MNI-space,\n"
      "   going back to XX-space via matrix Inverse(AA) and then forward\n"
      "   to MNI-space via BB.\n"
      " * This program (3dmaxdisp) can compute the average and maximum\n"
      "   displacement of Inverse(AA)*BB over the edges of the MNI template,\n"
      "   which will give you the answer to 'How close?' are the matrices.\n"
      "   If these displacements are on the order of a voxel size\n"
      "   (e.g., 1 mm), then the two matrices are for practical purposes\n"
      "   'close enough' (in Zhark's opinion).\n"
      " * How to do this?\n"
      "     cat_matvec AA.aff12.1D -I BB.aff12.1D > AinvB.aff12.1D\n"
      "     3dmaxdisp -dset ~/abin/MNI152_2009_template_SSW.nii.gz'[0]' -matrix AinvB.aff12.1D\n"
      " * Results are sent to stdout, two numbers per row (average and maximum),\n"
      "   one row of output for each matrix row given. Usually you will want to\n"
      "   capture stdout to a file with '>' or '| tee', depending on your further plans.\n"
      "\n"
      "-------\n"
      "OPTIONS:\n"
      "-------\n"
      "  -inset ddd  }= The input dataset is 'ddd', which is used only to form\n"
      "     *OR*     }= the mask over which the displacements will be computed.\n"
      "  -dset  ddd  }=\n"
      "\n"
      "  -matrix mmm  = File 'mmm' has 12 numbers per row, which are assembled\n"
      "                 into the 3x4 affine transformation matrix to be applied\n"
      "                 to the coordinates of the voxels in the dataset mask.\n"
      "                * As a special case, you can use the word 'IDENTITY'\n"
      "                  for the matrix filename, which should result in\n"
      "                  a max displacement of 0 mm.\n"
      "                * If there is more than 1 row in 'mmm', then each row\n"
      "                  is treated as a separate matrix, and the max displacement\n"
      "                  will be computed separately for each matrix.\n"
      "\n"
      "  -verb        = Print a few progress reports (to stderr).\n"
      "\n"
      "------\n"
      "Author: Zhark the Displacer (AKA Bob the Inverted) -- June 2021\n"
      "------\n"
      "\n"
     ) ;
     exit(0) ;
   }
   /*-----------------------------------------------------------------------*/

   /*---------- scan input options ----------*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-dset") == 0 || strcmp(argv[iarg],"-inset") == 0 ){
       if( dset != NULL )   ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dset = THD_open_dataset( argv[iarg] ) ;
       if( dset == NULL ) ERROR_exit("can't open input dataset '%s' :-(",argv[iarg]);
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-matrix") == 0 ){
       char *fname ; MRI_IMAGE *qim ;
       if( matim != NULL )  ERROR_exit("Can't use '%s' more than once :(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       fname = argv[iarg] ;
       if( strcasecmp(fname,"IDENTITY")==0 )          /* special case */
         fname = "1D: 1 0 0 0   0 1 0 0   0 0 1 0" ;
       qim = mri_read_1D(fname) ;
       if( qim == NULL ) ERROR_exit("Can't read -matrix '%s' :-(",fname) ;
       matim     = mri_transpose(qim); mri_free(qim);
       matim_far = MRI_FLOAT_PTR(matim) ;
       matim_nx  = matim->nx ;  /* # of values per row */
       matim_ny  = matim->ny ;  /* number of rows */
       if( matim_nx < 12 && matim->nvox == 12 ){  /* special case of a 3x4 array */
         matim_nx = 12 ; matim_ny = 1 ;
         if(verb)INFO_message("-matrix: converting input 3x4 array to 1 row of 12 numbers") ;
       }
       if( matim_nx < 12 )
         ERROR_exit("%d = Less than 12 numbers per row in -matrix '%s' :-(" ,matim_nx,fname) ;
       else if( matim_nx > 12 )
         WARNING_message("%d = More than 12 numbers per row in -matrix '%s'",matim_ny,fname) ;
       iarg++ ; continue ;
     }

     /*-----*/

     if( strcmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*-----*/

     ERROR_message("Unknown and Illegal option '%s' :-( :-( :-(",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);

   }

   /*----- inputs without formal options? -----*/

   if( matim == NULL )
     ERROR_exit("No -matrix option ???") ;

   if( dset == NULL && iarg >= argc )
     ERROR_exit("No input dataset ???") ;

   if( dset == NULL ){
     dset = THD_open_dataset( argv[iarg] ) ;
     if( dset == NULL ) ERROR_exit("can't open input dataset '%s' :-(",argv[iarg]);
     iarg++ ;
   }

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxy = nx*ny ; nxyz = nxy*nz ;

   /*---------- create the xyz coords for automask edge voxels ----------*/

#undef  DSK
#define DSK(i,j,k) dsk[(i)+(j)*nx+(k)*nxy]
   if( verb ) INFO_message("creating automask") ;
   THD_automask_set_clipfrac(0.333f) ;
   DSET_load(dset) ;
   dsk = THD_automask( dset ) ;  /* volume automask */
   if( dsk == NULL )
     ERROR_exit("Cannot create automask for input dataset :(") ;

   if( verb ) ININFO_message("  %s voxels in automask",
                             commaized_integer_string(mask_count(nxyz,dsk)) ) ;

   msk = (byte *)calloc(1,nxyz) ;
   for( nmsk=mm=0 ; mm < nxyz ; mm++ ){    /* create edges of the automask */
     if( dsk[mm] == 0 ) continue ;                      /* not in the mask */
     ii = mm % nx ; kk = mm / nxy ; jj = (mm%nxy) / nx ;  /* voxel indexes */
     ip=ii+1; im=ii-1; if(ip>=nx || im<0){ msk[mm]=1; nmsk++; continue; }
     jp=jj+1; jm=jj-1; if(jp>=ny || jm<0){ msk[mm]=1; nmsk++; continue; }
     kp=kk+1; km=kk-1; if(kp>=nz || km<0){ msk[mm]=1; nmsk++; continue; }
     if( DSK(ip,jj,kk) && DSK(im,jj,kk) &&              /* if all 6 nbhrs  */
         DSK(ii,jp,kk) && DSK(ii,jm,kk) &&              /* are in automask */
         DSK(ii,jj,kp) && DSK(ii,jj,km)   ) continue ;  /* skip this voxel */
     msk[mm] = 1 ; nmsk++ ;
   }
   free(dsk) ;
   if( nmsk == 0 )
     ERROR_exit("Cannot find edge voxels for input dataset automask :(") ;
   dispvec = (THD_fvec3 *)malloc(sizeof(THD_fvec3)*nmsk) ;
   for( qq=mm=0 ; mm < nxyz ; mm++ ){
     if( msk[mm] == 0 ) continue ;
     ii = mm % nx ; kk = mm / nxy ; jj = (mm%nxy) / nx ;
     iv.ijk[0] = ii ; iv.ijk[1] = jj ; iv.ijk[2] = kk ;        /* convert 3D */
     dispvec[qq++] = THD_3dind_to_dicomm_no_wod( dset , iv ) ; /* index to xyz */
   }
   free(msk) ;
   num_dispvec = qq ;
   if( verb ) ININFO_message("  %s edge voxels in automask",commaized_integer_string(num_dispvec)) ;

   /*--------- Loop over matrix rows and compute displacments ----------*/

   for( qq=0 ; qq < matim_ny ; qq++ ){
     LOAD_MAT44_AR( qmat , matim_far+(12*qq) ) ;  /* get matrix */
     qmat.m[0][0] -= 1.0f ;                       /* subtract identity */
     qmat.m[1][1] -= 1.0f ;                       /* matrix */
     qmat.m[2][2] -= 1.0f ;

     for( dbar=dmax=0.0f,mm=0 ; mm < num_dispvec ; mm++ ){  /* loop over voxels */
       MAT44_VEC( qmat,
                  dispvec[mm].xyz[0], dispvec[mm].xyz[1], dispvec[mm].xyz[2],
                  xx                , yy                , zz                 ) ;
       dd = sqrtf(xx*xx + yy*yy + zz*zz) ;  /* length of this displacement */
       if( dd > dmax ) dmax = dd ;
       dbar += dd ;
     }
     dbar /= num_dispvec ;
     printf("  %.6g  %.6g\n",dbar,dmax) ;
   }

   exit(0) ;
}
