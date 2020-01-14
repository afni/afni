#include "mrilib.h"

/*---------- These functions used to be in afni.c,
             but are moved away to assuage John Lee [13 Jan 2020]   -------*/

/*---------- 18 May 2000: save/get dataset index for function calls -------*/

static int dset_ijk=-1 , dset_tin=-1 ;

void AFNI_store_dset_index( int ijk , int tin )
{
   dset_ijk = ijk ; dset_tin = tin ; return ;
}

int AFNI_needs_dset_ijk(void){ return dset_ijk ; }
int AFNI_needs_dset_tin(void){ return dset_tin ; }
