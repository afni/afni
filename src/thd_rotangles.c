/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd_shear3d.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/*--------- Internal Prototype -----------*/

static void mangle_angle( THD_3dim_dataset *, float, char, float *, int *) ;

/*----------------------------------------------------------------------------
   Routine to take user-input angles (about I,S,R,L,A,P) and return
   dataset-oriented angles (about axes 0,1,2).  Adapted from 3drotate.c.

   dset     = dataset defining axes orientations
   thI_in   = input angle I (I=1,2,3)
   axI_code = axis code for angle I, which is one of
               'I' 'S' 'R' 'L' 'A' 'P' 'x' 'y' 'z'

  *thI_out  = output angle I (will be thI_in or -thI_in)
  *axI_out  = 0,1,2 indicating rotation about dataset x,y,z axis

  If the input dataset is NULL, then it is treated as in Dicom axes order.
------------------------------------------------------------------------------*/

void THD_rotangle_user_to_dset( THD_3dim_dataset * dset ,
                                float th1_in   , char ax1_code ,
                                float th2_in   , char ax2_code ,
                                float th3_in   , char ax3_code ,
                                float *th1_out , int *ax1_out  ,
                                float *th2_out , int *ax2_out  ,
                                float *th3_out , int *ax3_out   )
{
ENTRY("THD_rotangle_user_to_dset") ;

   mangle_angle( dset, th1_in, ax1_code, th1_out, ax1_out ) ;
   mangle_angle( dset, th2_in, ax2_code, th2_out, ax2_out ) ;
   mangle_angle( dset, th3_in, ax3_code, th3_out, ax3_out ) ;

   if( THD_handedness(dset) < 0 ){
      *th1_out = -(*th1_out) ;
      *th2_out = -(*th2_out) ;
      *th3_out = -(*th3_out) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static void mangle_angle( THD_3dim_dataset * dset ,
                          float thin, char ax , float *thout, int *axout )
{
   int neg=0 , ax1 ;
   float th1=thin ;

   switch( ax ){
      default:
         if( th1 == 0.0 ){ *thout = 0.0 ; *axout = 0 ; }
         else { fprintf(stderr,"*** Illegal ax in mangle_angle\n") ; }
      return ;

      case '\0': case 'x': case 'X': ax1 = 0 ; break ;
                 case 'y': case 'Y': ax1 = 1 ; break ;
                 case 'z': case 'Z': ax1 = 2 ; break ;

      case 'A': case 'P':
      case 'R': case 'L':
      case 'I': case 'S': ax1 = THD_axcode(dset,ax) ;
                          neg = (ax1 < 0) ;
                          ax1 = abs(ax1) - 1 ; break ;
   }
   if( neg ) th1 = -th1 ;

   *thout = th1 ; *axout = ax1 ; return ;
}

/*---------------------------------------------------------------------------
   Input:  ori = orientation code from ISAPLR
   Output: +k if ori is along the positive k-axis of the dataset (k=1,2,3)
           -k if ori is along the negative k-axis of the dataset

   06 Feb 2001: Promoted to externally visible.
   13 Feb 2001: bad dataset -> use Dicom axes order
-----------------------------------------------------------------------------*/

int THD_axcode( THD_3dim_dataset * dset , char ori )
{
   int xxor , yyor , zzor ;

ENTRY("THD_axcode") ;

   if( ISVALID_DSET(dset) ){
      xxor = dset->daxes->xxorient ;
      yyor = dset->daxes->yyorient ;
      zzor = dset->daxes->zzorient ;
   } else {
      xxor = ORI_R2L_TYPE ;   /* 13 Feb 2001: Dicom order */
      yyor = ORI_A2P_TYPE ;
      zzor = ORI_I2S_TYPE ;
   }
   ori = toupper(ori) ;
   if( ori == ORIENT_tinystr[xxor][0] ) RETURN( 1) ;
   if( ori == ORIENT_tinystr[xxor][1] ) RETURN(-1) ;
   if( ori == ORIENT_tinystr[yyor][0] ) RETURN( 2) ;
   if( ori == ORIENT_tinystr[yyor][1] ) RETURN(-2) ;
   if( ori == ORIENT_tinystr[zzor][0] ) RETURN( 3) ;
   if( ori == ORIENT_tinystr[zzor][1] ) RETURN(-3) ;
   RETURN(-99) ;
}

/*---------------------------------------------------------------------------
   Output: +1 if dataset xyz axes are right handed
           -1 if dataset xyz axes are left handed
   06 Feb 2001: Promoted to externally visible.
-----------------------------------------------------------------------------*/

int THD_handedness( THD_3dim_dataset * dset )
{
   THD_dataxes * dax ;
   THD_mat33 q ;
   int col ;
   float val ;

ENTRY("THD_handedness") ;

   if( !ISVALID_DSET(dset) ) RETURN(1) ;

   LOAD_ZERO_MAT(q) ;
   dax = dset->daxes ;

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
   if( val > 0.0 ) RETURN( 1) ;  /* right handed */
   else            RETURN(-1) ;  /* left handed */
}

/*-------------------------------------------------------------------------
  13 Feb 2001: convert a command of the form
                 "-rotate 10A 20I 30R -ashift 5I 10A 15L"
               to a matrix/vector pair for the given dataset.
---------------------------------------------------------------------------*/

THD_dvecmat THD_rotcom_to_matvec( THD_3dim_dataset * dset , char * rotcom )
{
   THD_dvecmat out ;
   int nn , rpos=0 , nrc ;
   char * buf ;

ENTRY("THD_rotcom_to_matvec") ;

   LOAD_DIAG_DMAT(out.mm,1,1,1) ;  /* identity matrix */
   LOAD_DFVEC3(out.vv,0,0,0) ;     /* and zero shift */

   if( rotcom == NULL || rotcom[0] == '\0' ) RETURN(out) ;

   /*-- compute rotation matrix --*/

   nrc = strlen(rotcom) ;
   buf = strstr(rotcom,"-rotate") ;
   if( buf != NULL && (buf-rotcom)+10 < nrc ){
      float th1,th2,th3 , dh1,dh2,dh3 ;
      char  cp1,cp2,cp3 ;
      int   ax1,ax2,ax3 ;

      nn = sscanf( buf+7, "%f%c %f%c %f%c",
                   &th1,&cp1, &th2,&cp2, &th3,&cp3 );
      if( nn < 6 ) RETURN(out) ;
      if( isspace(cp1) ) cp1 = 'x' ;  /* should not happen */
      if( isspace(cp2) ) cp2 = 'y' ;
      if( isspace(cp3) ) cp3 = 'z' ;

      THD_rotangle_user_to_dset( dset , th1,cp1 , th2,cp2 , th3,cp3 ,
                                 &dh1,&ax1 , &dh2,&ax2 , &dh3,&ax3   ) ;

      out.mm = rot_to_matrix( ax1 , -(PI/180.0)*dh1,
                              ax2 , -(PI/180.0)*dh2,
                              ax3 , -(PI/180.0)*dh3 ) ;
   }

   /*-- compute shift --*/

                     buf = strstr(rotcom,"-ashift") ;
   if( buf == NULL ) buf = strstr(rotcom,"-bshift") ;

   if( buf != NULL && (buf-rotcom)+10 < nrc ){
      int bs = (buf[1] == 'b') ;  /* save the BS for later */
      float dx,dy,dz , qdx=0,qdy=0,qdz=0 ;
      char  cdx,cdy,cdz ;
      int   adx,ady,adz ;

      nn = sscanf( buf+7, "%f%c %f%c %f%c",
                   &dx,&cdx, &dy,&cdy, &dz,&cdz );
      if( nn < 6 ) RETURN(out) ;

      adx = THD_axcode(dset,cdx) ;
      if( adx > -99 || dx != 0.0 ){
         switch( adx ){
            case  1: qdx = -dx ; break ;
            default:
            case -1: qdx =  dx ; break ;
            case  2: qdy = -dx ; break ;
            case -2: qdy =  dx ; break ;
            case  3: qdz = -dx ; break ;
            case -3: qdz =  dx ; break ;
         }
      }

      ady = THD_axcode(dset,cdy) ;
      if( ady > -99 || dy != 0.0 ){
         switch( ady ){
            case  1: qdx = -dy ; break ;
            case -1: qdx =  dy ; break ;
            case  2: qdy = -dy ; break ;
            default:
            case -2: qdy =  dy ; break ;
            case  3: qdz = -dy ; break ;
            case -3: qdz =  dy ; break ;
         }
      }

      adz = THD_axcode(dset,cdz) ;
      if( adz > -99 || dz != 0.0 ){
         switch( adz ){
            case  1: qdx = -dz ; break ;
            case -1: qdx =  dz ; break ;
            case  2: qdy = -dz ; break ;
            case -2: qdy =  dz ; break ;
            case  3: qdz = -dz ; break ;
            default:
            case -3: qdz =  dz ; break ;
         }
      }

      LOAD_DFVEC3( out.vv , qdx,qdy,qdz ) ;
      if( bs ){
         THD_dfvec3 qv = DMATVEC( out.mm , out.vv ) ;
         out.vv = qv ;
      }
   }

   RETURN(out) ;
}

#if 0
/******************************************
   Sample program for above routine
*******************************************/
#include "mrilib.h"
int main( int argc , char * argv[] )
{
   THD_3dim_dataset * qset ;
   THD_dvecmat vm ;
   if( argc < 3 || strcmp(argv[1],"-help")==0 ){
     printf("Usage: %s dset \"-rotate a b c -ashift a b c\"\n",argv[0]);exit(0);
   }
   qset = THD_open_dataset(argv[1]) ;
   vm   = THD_rotcom_to_matvec( qset , argv[2] ) ;
   DUMP_DVECMAT("r2m output",vm) ; exit(0) ;
}
#endif

/**************************************************************************/

#include "thd.h"

/*---------------------------------------------------------------------
  This produces a permutation-like matrix that transforms from
  brick axis coordinates to Dicom order coordinates.
  [14 Feb 2001 - moved here from 3drotate.c]
-----------------------------------------------------------------------*/

THD_dmat33 DBLE_mat_to_dicomm( THD_3dim_dataset * dset )
{
   THD_dmat33 tod ;

   LOAD_ZERO_DMAT(tod) ;

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

/*---------------------------------------------------------------------
  This produces a permutation-like matrix that transforms from
  brick axis coordinates to Dicom order coordinates.
-----------------------------------------------------------------------*/

THD_mat33 SNGL_mat_to_dicomm( THD_3dim_dataset * dset )
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
