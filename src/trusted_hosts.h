/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _AFNI_TRUSTHOST_HEADER_
#define _AFNI_TRUSTHOST_HEADER_

/*******************************************************************
   Define which external hosts are allowed to connect to AFNI
********************************************************************/

/** list of strings with beginning components of IP addresses **/

#include "mcw_malloc.h"

static char * trusted_hosts[] = {
    "141.106.106.2" ,                 /* MCW Biophysics computers */
    "127.0.0.1"     ,                 /* localhost */
    "192.168.0."                      /* private class B networks */
} ;

/** if you change the number of elements in trusted_hosts,
    then you must also change this macro accordingly.      **/

#define OKHOST(hh) ( strstr((hh),trusted_hosts[0]) == (hh) ||  \
                     strstr((hh),trusted_hosts[1]) == (hh) ||  \
                     strstr((hh),trusted_hosts[2]) == (hh)     \
                   )

#endif /* _AFNI_TRUSTHOST_HEADER_ */
