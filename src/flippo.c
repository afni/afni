
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
   int npt , nall ;
   float * x , * y ;
   int * typ ;
} mri_cohar ;

#define DEL 0.001

void merge_cohar( mri_cohar * coh )
{
   int ii , jj;

   if( coh == NULL || coh->npt < 2 ) return ;

   /* mark duplicates for destruction */

   for( ii=1 ; ii < coh->npt ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ){

         if( typ[jj] > 0             && typ[jj] == typ[ii]      &&
             fabs(x[jj]-x[ii]) < DEL && fabs(y[jj]-y[ii]) < DEL   ){

            typ[ii] = -1 ;
            break ;
         }
      }
   }
