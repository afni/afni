#define FLOAT_TYPE double  /* as in thd_rot3d.c */
#include "vecmat.h"

#include "mrilib.h"
#include <string.h>
#include <stdlib.h>

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*-------------------------------------------------------------------------------*/

#include <ctype.h>

int axcode( THD_3dim_dataset * dset , char ori )
{
   ori = toupper(ori) ;
   if( ori == ORIENT_tinystr[dset->daxes->xxorient][0] ) return  1 ;
   if( ori == ORIENT_tinystr[dset->daxes->xxorient][1] ) return -1 ;
   if( ori == ORIENT_tinystr[dset->daxes->yyorient][0] ) return  2 ;
   if( ori == ORIENT_tinystr[dset->daxes->yyorient][1] ) return -2 ;
   if( ori == ORIENT_tinystr[dset->daxes->zzorient][0] ) return  3 ;
   if( ori == ORIENT_tinystr[dset->daxes->zzorient][1] ) return -3 ;
   return -99 ;
}

/*------------------------------------------------------------------------------*/

int handedness( THD_3dim_dataset * dset )
{
   THD_dataxes * dax = dset->daxes ;
   THD_mat33 q ;
   int col ;
   float val ;

   LOAD_ZERO_MAT(q) ;

   col = 0 ;
   switch( dax->xxorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   col = 1 ;
   switch( dax->yyorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   col = 2 ;
   switch( dax->zzorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   val = MAT_DET(q) ;
   if( val > 0.0 ) return  1 ;   /* right handed */
   else            return -1 ;   /* left handed */
}

/*-----------------------------------------------------------------------------------*/

THD_mat33 DBLE_mat_to_dicomm ( THD_3dim_dataset * ) ;    /* at end of file */

#define MATVEC_DICOM 1
#define MATVEC_ORDER 2

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   char * new_prefix = "rota" , * cpt ;
   float dx=0 , dy=0 , dz=0 ;
   int   ax1=0,ax2=1,ax3=2 , adx,ady,adz ;
   char  cdx,cdy,cdz ;
   float th1=0.0,th2=0.0,th3=0.0 ;
   int iopt , nvox , rotarg=-1 , dcode=-1 , ival,nval , verb=0 , ihand ;
   float * fvol ;
   double cputim ;
   int clipit=0 ;  /* 11 Apr 2000 */
   float cbot,ctop ;

   int matvec=0 ;    /* 19 July 2000 */
   THD_mat33 rmat , pp,ppt ;
   THD_fvec3 tvec ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
      printf(
         "Usage: 3drotate [options] dataset\n"
         "Rotates and/or translates all bricks from an AFNI dataset.\n"
         "'dataset' may contain a sub-brick selector list.\n"
         "The options are:\n"
         "  -prefix fname    = Sets the output dataset prefix name to be 'fname'\n"
         "  -verbose         = Prints out progress reports\n"
         "\n"
         "At most one of these shift options can be used:\n"
         "  -ashift dx dy dz = Shifts the dataset 'dx' mm in the x-direction, etc.,\n"
         "                       AFTER rotation.\n"
         "  -bshift dx dy dz = Shifts the dataset 'dx' mm in the x-direction, etc.,\n"
         "                       BEFORE rotation.\n"
         "    The shift distances by default are along the (x,y,z) axes of the dataset\n"
         "    storage directions (see the output of '3dinfo dataset').  To specify them\n"
         "    anatomically, you can suffix a distance with one of the symbols\n"
         "    'R', 'L', 'A', 'P', 'I', and 'S', meaning 'Right', 'Left', 'Anterior',\n"
         "    'Posterior', 'Inferior', and 'Superior', respectively.\n"
         "\n"
         "  -rotate th1 th2 th3\n"
         "    Specifies the 3D rotation to be composed of 3 planar rotations:\n"
         "       1) 'th1' degrees about the 1st axis,           followed by\n"
         "       2) 'th2' degrees about the (rotated) 2nd axis, followed by\n"
         "       3) 'th3' degrees about the (doubly rotated) 3rd axis.\n"
         "    Which axes are used for these rotations is specified by placing\n"
         "    one of the symbols 'R', 'L', 'A', 'P', 'I', and 'S' at the end\n"
         "    of each angle (e.g., '10.7A').  These symbols denote rotation\n"
         "    about the 'Right-to-Left', 'Left-to-Right', 'Anterior-to-Posterior',\n"
         "    'Posterior-to-Anterior', 'Inferior-to-Superior', and\n"
         "    'Superior-to-Inferior' axes, respectively.  A positive rotation is\n"
         "    defined by the right-hand rule.\n"
         "\n"
         "  -matvec_dicom mfile\n"
         "  -matvec_order mfile\n"
         "    Specifies that the rotation and translation should be read from file\n"
         "    'mfile', which should be in the format\n"
         "           u11 u12 u13 v1\n"
         "           u21 u22 u23 v2\n"
         "           u31 u32 u33 u3\n"
         "    where each 'uij' and 'vi' is a number.  The 3x3 matrix [uij] is the\n"
         "    orthogonal matrix of the rotation, and the 3-vector [vi] is the -ashift\n"
         "    vector of the translation.\n"
         "  -matvec_dset mset\n"
         "    Specifies that the rotation and translation should be read from\n"
         "    the .HEAD file of dataset 'mset', which was created by program\n"
         "    3dTagalign.\n"
         "\n"
         "  * If -matvec_dicom is used, the matrix and vector are given in Dicom\n"
         "     coordinate order (+x=L, +y=P, +z=S).  This is the option to use\n"
         "     if mfile is generated using 3dTagalign -matvec mfile.\n"
         "  * If -matvec_order is used, the the matrix and vector are given in the\n"
         "     coordinate order of the dataset axes, whatever they may be.\n"
         "  * You can't mix -matvec_* options with -rotate and -*shift.\n"
         "\n"
         "Example: 3drotate -prefix Elvis -bshift 10S 0 0 -rotate 30R 0 0 Sinatra+orig\n"
         "\n"
         "This will shift the input 10 mm in the superior direction, followed by a 30\n"
         "degree rotation about the Right-to-Left axis (i.e., nod the head forward).\n"
         "\n"
         "Algorithm: The rotation+shift is decomposed into 4 1D shearing operations\n"
         "           (a 3D generalization of Paeth's algorithm).  The interpolation\n"
         "           (i.e., resampling) method used for these shears can be controlled\n"
         "           by the following options:\n"
         " -Fourier = Use a Fourier method (the default: most accurate; slowest).\n"
         " -linear  = Use linear (1st order polynomial) interpolation (least accurate).\n"
         " -cubic   = Use the cubic (3rd order) Lagrange polynomial method.\n"
         " -quintic = Use the quintic (5th order) Lagrange polynomial method.\n"
         " -heptic  = Use the heptic (7th order) Lagrange polynomial method.\n"
         "\n"
         " -clipit  = Clip results to input brick range\n"
      ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;

      exit(0) ;
   }

#define ERREX(str) (fprintf(stderr,"*** %s\n",str),exit(1))

   iopt = 1 ;
   while( iopt < argc && argv[iopt][0] == '-' ){

      if( strncmp(argv[iopt],"-matvec_",8) == 0 ){  /* 19 Jul 2000 */

         MRI_IMAGE * matim ; float * matar , sum ;

         if( matvec )
            ERREX("*** Can't use 2 -matvec options!") ;
         if( dcode > 0 || rotarg > 0 )
            ERREX("*** Can't use -matvec with -shift or -rotate options!") ;

         if( strcmp(argv[iopt],"-matvec_order") == 0 ) matvec = MATVEC_ORDER ;
         else                                          matvec = MATVEC_DICOM ;

         if( strcmp(argv[iopt],"-matvec_dset") == 0){   /* 20 July 2000 */
            THD_3dim_dataset * mvset ; ATR_float * atr ;

            mvset = THD_open_dataset( argv[++iopt] ) ;
            if( mvset == NULL ) ERREX("*** Can't read -matvec_dset dataset!") ;
            atr = THD_find_float_atr( mvset->dblk , "TAGALIGN_MATVEC" ) ;
            if( atr == NULL || atr->nfl < 12 )
               ERREX("*** -matvec_dset doesn't have matrix+vector in .HEAD!") ;
            matar = atr->fl ;
            LOAD_MAT(rmat,matar[0],matar[1],matar[2],
                          matar[4],matar[5],matar[6],
                          matar[8],matar[9],matar[10] ) ;
            LOAD_FVEC3(tvec,matar[3],matar[7],matar[11]) ;
            DSET_delete(mvset) ;
         } else {
            matim = mri_read_ascii( argv[++iopt] ) ;
            if( matim == NULL ) ERREX("Can't read -matvec file!") ;
            if( matim->nx != 4 || matim->ny != 3 ) ERREX("-matvec file not 4x3!") ;

            matar = MRI_FLOAT_PTR(matim) ;
            LOAD_MAT(rmat,matar[0],matar[1],matar[2],
                          matar[4],matar[5],matar[6],
                          matar[8],matar[9],matar[10] ) ;
            LOAD_FVEC3(tvec,matar[3],matar[7],matar[11]) ;

            mri_free(matim) ;
         }

         /* check if matrix is orthogonal */

         pp = TRANSPOSE_MAT(rmat) ; pp = MAT_MUL(pp,rmat) ;
         sum = fabs(pp.mat[0][0]-1.0)+fabs(pp.mat[1][0])    +fabs(pp.mat[2][0])
              +fabs(pp.mat[0][1])    +fabs(pp.mat[1][1]-1.0)+fabs(pp.mat[2][1])
              +fabs(pp.mat[0][2])    +fabs(pp.mat[1][2])    +fabs(pp.mat[2][2]-1.0);
         if( sum > 0.01 ) ERREX("-matvec matrix not orthogonal!") ;

         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-clipit",4) == 0 ){  /* 11 Apr 2000 */
         clipit = 1 ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-prefix",4) == 0 ){
         new_prefix = argv[++iopt] ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-verbose",4) == 0 ){
         verb = 1 ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-Fourier",4) == 0 || strncmp(argv[iopt],"-fourier",4) == 0 ){
         THD_rota_method( MRI_FOURIER ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-cubic",4) == 0 || strncmp(argv[iopt],"-Cubic",4) == 0 ){
         THD_rota_method( MRI_CUBIC ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-quintic",4) == 0 || strncmp(argv[iopt],"-Quintic",4) == 0 ){
         THD_rota_method( MRI_QUINTIC ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-heptic",4) == 0 || strncmp(argv[iopt],"-Heptic",4) == 0 ){
         THD_rota_method( MRI_HEPTIC ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-linear",4) == 0 || strncmp(argv[iopt],"-Linear",4) == 0 ){
         THD_rota_method( MRI_LINEAR ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-nn",3) == 0 || strncmp(argv[iopt],"-NN",4) == 0 ){
         THD_rota_method( MRI_NN ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-tsshift",6) == 0 ){  /* 12 Dec 1999 */
         THD_rota_method( MRI_TSSHIFT ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-ashift",4) == 0 ){
         if( dcode > 0 ){fprintf(stderr,"*** Can't use 2 shift options!\n");exit(1);}
         if( matvec    ) ERREX("*** Can't use -ashift with -matvec!") ;
         dx = strtod( argv[++iopt] , &cpt ) ; cdx = *cpt ;
         dy = strtod( argv[++iopt] , &cpt ) ; cdy = *cpt ;
         dz = strtod( argv[++iopt] , &cpt ) ; cdz = *cpt ;
         dcode = DELTA_AFTER ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-bshift",4) == 0 ){
         if( dcode > 0 ){fprintf(stderr,"*** Can't use 2 shift options!\n");exit(1);}
         if( matvec    ) ERREX("*** Can't use -bshift with -matvec!") ;
         dx = strtod( argv[++iopt] , &cpt ) ; cdx = *cpt ;
         dy = strtod( argv[++iopt] , &cpt ) ; cdy = *cpt ;
         dz = strtod( argv[++iopt] , &cpt ) ; cdz = *cpt ;
         dcode = DELTA_BEFORE ;
         iopt++ ; continue ;
      }

#if 0
      if( strncmp(argv[iopt],"-cshift",4) == 0 ){
         if( dcode > 0 ){fprintf(stderr,"*** Can't use 2 shift options!\n");exit(1);}
         dx = strtod( argv[++iopt] , &cpt ) ; cdx = *cpt ;
         dy = strtod( argv[++iopt] , &cpt ) ; cdy = *cpt ;
         dz = strtod( argv[++iopt] , &cpt ) ; cdz = *cpt ;
         dcode = DELTA_FIXED ;
         iopt++ ; continue ;
      }
#endif

      if( strncmp(argv[iopt],"-rotate",4) == 0 ){
         if( matvec    ) ERREX("*** Can't use -rotate with -matvec!") ;
         rotarg = iopt ;  /* save and process later */
         iopt += 4 ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[iopt]) ; exit(1) ;
   }

   if( matvec == 0 && dcode < 0 && rotarg < 0 ){
      fprintf(stderr,"*** Don't you want to do anything?\n") ; exit(1) ;
   }

   /** read input dataset */

   if( iopt >= argc ){ fprintf(stderr,"*** No input dataset?\n") ; exit(1) ; }

   dset = THD_open_dataset( argv[iopt] ) ;
   if( dset == NULL ){
      fprintf(stderr,"*** Cannot open dataset %s!\n",argv[iopt]) ; exit(1) ;
   }

   /* now can process rotation arguments */

   ihand = handedness(dset) ;

   if( rotarg > 0 ){
      int neg ;
      iopt = rotarg ;

      th1 = (PI/180.0) * strtod( argv[++iopt] , &cpt ) ;
      switch( *cpt ){
         default: fprintf(stderr,"*** Illegal code after th1 in -rotate\n");exit(1);
         case '\0': case 'x': case 'X': ax1 = 0 ; neg = 0 ; break ;
                    case 'y': case 'Y': ax1 = 1 ; neg = 0 ; break ;
                    case 'z': case 'Z': ax1 = 2 ; neg = 0 ; break ;

         case 'A': case 'P':
         case 'R': case 'L':
         case 'I': case 'S': ax1 = axcode(dset,*cpt) ;
                             neg = (ax1 < 0) ;
                             ax1 = abs(ax1) - 1 ; break ;
      }
      if( neg ) th1 = -th1 ;

      th2 = (PI/180.0) * strtod( argv[++iopt] , &cpt ) ;
      switch( *cpt ){
         default: fprintf(stderr,"*** Illegal code after th2 in -rotate\n");exit(1);
                    case 'x': case 'X': ax2 = 0 ; break ;
         case '\0': case 'y': case 'Y': ax2 = 1 ; break ;
                    case 'z': case 'Z': ax2 = 2 ; break ;

         case 'A': case 'P':
         case 'R': case 'L':
         case 'I': case 'S': ax2 = axcode(dset,*cpt) ;
                             neg = (ax2 < 0) ;
                             ax2 = abs(ax2) - 1 ; break ;
      }
      if( neg ) th2 = -th2 ;

      th3 = (PI/180.0) * strtod( argv[++iopt] , &cpt ) ;
      switch( *cpt ){
         default: fprintf(stderr,"*** Illegal code after th3 in -rotate\n");exit(1);
                    case 'x': case 'X': ax3 = 0 ; break ;
                    case 'y': case 'Y': ax3 = 1 ; break ;
         case '\0': case 'z': case 'Z': ax3 = 2 ; break ;

         case 'A': case 'P':
         case 'R': case 'L':
         case 'I': case 'S': ax3 = axcode(dset,*cpt) ;
                             neg = (ax3 < 0) ;
                             ax3 = abs(ax3) - 1 ; break ;
      }
      if( neg ) th3 = -th3 ;

      if( th1 == 0.0 && th2 == 0.0 && th3 == 0.0 ){
         fprintf(stderr,"*** Why are all the angles after -rotate equal to 0?\n") ;
         exit(1) ;
      }

      if( ax1 < 0 || ax1 > 2 || ax2 < 0 || ax2 > 2 || ax3 < 0 || ax3 > 2 ){
         fprintf(stderr,"*** Some error occured: can't understand axes codes!\n") ;
         exit(1) ;
      }

      if( ihand < 0 ){ th1 = -th1 ; th2 = -th2 ; th3 = -th3 ; }

#if 0
fprintf(stderr,"ihand=%d th1=%g th2=%g th3=%g\n",ihand,th1,th2,th3);
fprintf(stderr,"ax1=%d ax2=%d ax3=%d\n",ax1,ax2,ax3) ;
#endif
   }

   /* may need to process shift arguments as well */

   if( dcode > 0 && (cdx != '\0' || cdy != '\0' || cdz != '\0') ){
      float qdx=0 , qdy=0 , qdz=0 ;

      adx = axcode(dset,cdx) ;
      switch( adx ){
         case  1: qdx = -dx ; break ;
         default:
         case -1: qdx =  dx ; break ;
         case  2: qdy = -dx ; break ;
         case -2: qdy =  dx ; break ;
         case  3: qdz = -dx ; break ;
         case -3: qdz =  dx ; break ;
      }

      ady = axcode(dset,cdy) ;
      switch( ady ){
         case  1: qdx = -dy ; break ;
         default:
         case -1: qdx =  dy ; break ;
         case  2: qdy = -dy ; break ;
         case -2: qdy =  dy ; break ;
         case  3: qdz = -dy ; break ;
         case -3: qdz =  dy ; break ;
      }

      adz = axcode(dset,cdz) ;
      switch( adz ){
         case  1: qdx = -dz ; break ;
         default:
         case -1: qdx =  dz ; break ;
         case  2: qdy = -dz ; break ;
         case -2: qdy =  dz ; break ;
         case  3: qdz = -dz ; break ;
         case -3: qdz =  dz ; break ;
      }

#if 0
fprintf(stderr,"adx=%d dx=%g qdx=%g  ady=%d dy=%g qdy=%g  adz=%d dz=%g qdz=%g\n",
        adx,dx,qdx , ady,dy,qdy , adz,dz,qdz ) ;
#endif

      dx = qdx ; dy = qdy ; dz = qdz ;
   }

   /*- 19 July 2000: deal with -matvec_dicom case -*/

   if( matvec == MATVEC_DICOM ){
      pp   = DBLE_mat_to_dicomm( dset ) ;
      ppt  = TRANSPOSE_MAT(pp) ;
      rmat = MAT_MUL(ppt,rmat) ; rmat = MAT_MUL(rmat,pp) ; tvec = MATVEC(ppt,tvec) ;
   }

   DSET_mallocize(dset) ;
   if( verb ) fprintf(stderr,"+++ Loading dataset %s from disk",dset->dblk->diskptr->header_name);
   DSET_load(dset) ;
   if( verb ) fprintf(stderr,"\n") ;

   dset->idcode = MCW_new_idcode() ;  /* 08 Jun 1999 */
   EDIT_dset_items( dset ,
                       ADN_prefix , new_prefix ,
                       ADN_label1 , new_prefix ,
                    ADN_none ) ;
   if( THD_is_file(dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "*** Output file %s already exists -- cannot continue!\n",
              dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

   /* old history is already in the dataset */

   tross_Make_History( "3drotate" , argc,argv , dset ) ;

   nvox = DSET_NVOX(dset) ;
   fvol = (float *) malloc( sizeof(float) * nvox ) ;

   nval = DSET_NVALS(dset) ;
   if( verb ){
       fprintf(stderr,"+++ %d sub-bricks: ",nval) ;
       cputim = COX_cpu_time() ;
   }
   for( ival=0 ; ival < nval ; ival++ ){

      if( verb ) fprintf(stderr,"%d",ival) ;

      EDIT_coerce_type( nvox ,
                        DSET_BRICK_TYPE(dset,ival),DSET_ARRAY(dset,ival) ,
                        MRI_float,fvol ) ;

      if( verb ) fprintf(stderr,".") ;

      if( clipit ){                                 /* 11 Apr 2000 */
         register int ii ; register float bb,tt ;
         bb = tt = fvol[0] ;
         for( ii=1 ; ii < nvox ; ii++ ){
                 if( fvol[ii] < bb ) bb = fvol[ii] ;
            else if( fvol[ii] > tt ) tt = fvol[ii] ;
         }
         cbot = bb ; ctop = tt ;
      }

      if( matvec )
        THD_rota_vol_matvec( DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ,
                             fabs(DSET_DX(dset)) , fabs(DSET_DY(dset)) ,
                                                   fabs(DSET_DZ(dset)) ,
                             fvol , rmat , tvec ) ;
      else
        THD_rota_vol( DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ,
                      fabs(DSET_DX(dset)), fabs(DSET_DY(dset)), fabs(DSET_DZ(dset)), fvol ,
                      ax1,th1 , ax2,th2 , ax3,th3 , dcode,dx,dy,dz ) ;

      if( verb ) fprintf(stderr,".") ;

      if( clipit ){                                 /* 11 Apr 2000 */
         register int ii ; register float bb,tt ;
         bb = cbot ; tt = ctop ;
         for( ii=0 ; ii < nvox ; ii++ ){
                 if( fvol[ii] < bb ) fvol[ii] = bb ;
            else if( fvol[ii] > tt ) fvol[ii] = tt ;
         }
      }

      EDIT_coerce_type( nvox , MRI_float,fvol ,
                               DSET_BRICK_TYPE(dset,ival),DSET_ARRAY(dset,ival) ) ;
   }
   if( verb ){
      cputim = COX_cpu_time() - cputim ;
      fprintf(stderr,"\n+++ CPU time=%10.3g s" , cputim) ;
      if( nval > 1 ) fprintf(stderr,"  [= %10.3g s/sub-brick]" , cputim/nval) ;
      fprintf(stderr,"\n+++ Writing dataset to disk in %s",dset->dblk->diskptr->header_name) ;
   }

   dset->dblk->master_nvals = 0 ;  /* 11 Apr 2000 hack */
   DSET_write(dset) ;
   if( verb ) fprintf(stderr,"\n") ;
   exit(0) ;
}

/***********************************************************************/

#include "thd.h"

/*---------------------------------------------------------------------
  This produces a permutation-like matrix that transforms from
  brick axis coordinates to Dicom order coordinates.
-----------------------------------------------------------------------*/

THD_mat33 DBLE_mat_to_dicomm( THD_3dim_dataset * dset )
{
   THD_mat33 tod ;

   LOAD_ZERO_MAT(tod) ;

   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE: tod.mat[0][0] =  1.0 ; break ;
      case ORI_L2R_TYPE: tod.mat[0][0] = -1.0 ; break ;
      case ORI_P2A_TYPE: tod.mat[1][0] = -1.0 ; break ;
      case ORI_A2P_TYPE: tod.mat[1][0] =  1.0 ; break ;
      case ORI_I2S_TYPE: tod.mat[2][0] =  1.0 ; break ;
      case ORI_S2I_TYPE: tod.mat[2][0] = -1.0 ; break ;

      default: THD_FATAL_ERROR("illegal xxorient code") ;
   }

   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE: tod.mat[0][1] =  1.0 ; break ;
      case ORI_L2R_TYPE: tod.mat[0][1] = -1.0 ; break ;
      case ORI_P2A_TYPE: tod.mat[1][1] = -1.0 ; break ;
      case ORI_A2P_TYPE: tod.mat[1][1] =  1.0 ; break ;
      case ORI_I2S_TYPE: tod.mat[2][1] =  1.0 ; break ;
      case ORI_S2I_TYPE: tod.mat[2][1] = -1.0 ; break ;

      default: THD_FATAL_ERROR("illegal yyorient code") ;
   }

   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE: tod.mat[0][2] =  1.0 ; break ;
      case ORI_L2R_TYPE: tod.mat[0][2] = -1.0 ; break ;
      case ORI_P2A_TYPE: tod.mat[1][2] = -1.0 ; break ;
      case ORI_A2P_TYPE: tod.mat[1][2] =  1.0 ; break ;
      case ORI_I2S_TYPE: tod.mat[2][2] =  1.0 ; break ;
      case ORI_S2I_TYPE: tod.mat[2][2] = -1.0 ; break ;

      default: THD_FATAL_ERROR("illegal zxorient code") ;
   }

   return tod ;
}
