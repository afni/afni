#include "mrilib.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/*--------- Prototypes -----------*/

static int axcode       ( THD_3dim_dataset * , char ) ;
static int handedness   ( THD_3dim_dataset * ) ;
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
------------------------------------------------------------------------------*/

void THD_rotangle_user_to_dset( THD_3dim_dataset * dset ,
                                float th1_in   , char ax1_code ,
                                float th2_in   , char ax2_code ,
                                float th3_in   , char ax3_code ,
                                float *th1_out , int *ax1_out  ,
                                float *th2_out , int *ax2_out  ,
                                float *th3_out , int *ax3_out   )
{
   mangle_angle( dset, th1_in, ax1_code, th1_out, ax1_out ) ;
   mangle_angle( dset, th2_in, ax2_code, th2_out, ax2_out ) ;
   mangle_angle( dset, th3_in, ax3_code, th3_out, ax3_out ) ;

   if( handedness(dset) < 0 ){
      *th1_out = -(*th1_out) ;
      *th2_out = -(*th2_out) ;
      *th3_out = -(*th3_out) ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/

static void mangle_angle( THD_3dim_dataset * dset ,
                          float thin, char ax , float *thout, int *axout )
{
   int neg=0 , ax1 ;
   float th1=thin ;

   switch( ax ){
      default: fprintf(stderr,"*** Illegal ax in mangle_angle\n") ; return ;

      case '\0': case 'x': case 'X': ax1 = 0 ; break ;
                 case 'y': case 'Y': ax1 = 1 ; break ;
                 case 'z': case 'Z': ax1 = 2 ; break ;

      case 'A': case 'P':
      case 'R': case 'L':
      case 'I': case 'S': ax1 = axcode(dset,ax) ;
                          neg = (ax1 < 0) ;
                          ax1 = abs(ax1) - 1 ; break ;
   }
   if( neg ) th1 = -th1 ;

   *thout = th1 ; *axout = ax1 ; return ;
}

/*----------------------------------------------------------------------------*/


static int axcode( THD_3dim_dataset * dset , char ori )
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

/*----------------------------------------------------------------------------*/

static int handedness( THD_3dim_dataset * dset )
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
