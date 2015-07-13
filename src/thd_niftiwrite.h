#ifndef _THD_NIFTIWRITE_H_
#define _THD_NIFTIWRITE_H_

#include "nifti2_io.h"

#define VARIOUS_CONSTANTS             2

/* Various function macros
#define CHECK_NULL_STR(str)  ( str ? str : "<NULL>" )
#define CHECK_EMPTY_STR(str) ( str[0] ? str : "<empty>" )
*/

typedef struct {
  int debug_level ;  /* 0 - 3 */
  char *infile_name ;
} niftiwr_opts_t ;


/* ---- export function prototypes ---- */

nifti_image * populate_nifti_image(THD_3dim_dataset *dset, niftiwr_opts_t options) ;
int THD_write_nifti( THD_3dim_dataset *dset, niftiwr_opts_t options ) ;


#endif   /* _THD_NIFTIWRITE_H_ */
