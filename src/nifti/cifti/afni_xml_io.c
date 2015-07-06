
/* This library depends on:

        afni_xml.c
        NIFTI-2
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <nifti2_io.h>
#include "afni_xml.h"

/* local protos */
static afni_xml_t * xlist_to_ax1(afni_xml_list * xlist);


/* read file into a single, allocated afni_xml_t struct */
afni_xml_t * axio_read_buf(const char * buf, int64_t blen)
{
   afni_xml_list   xlist;

   /* try to read file */
   xlist = axml_read_buf(buf, blen);
   if( xlist.len <= 0 || ! xlist.xlist ) return NULL;

   /* return the first afni_xml_t tree */
   return xlist_to_ax1(&xlist);
}

/* read file into a single, allocated afni_xml_t struct */
afni_xml_t * axio_read_file(const char * fname)
{
   afni_xml_list   xlist;

   /* try to read file */
   xlist = axml_read_file(fname);
   if( xlist.len <= 0 || ! xlist.xlist ) return NULL;

   /* return the first afni_xml_t tree */
   return xlist_to_ax1(&xlist);
}

/* rcr */
int axio_text_to_binary(afni_xml_t * ax)
{
   if( ! ax ) return;
}


/* ====================================================================== */
/* ====               local functions, not for export                ==== */
/* ====================================================================== */

/* convert xlist to single tree (so destroy xlist) */
static afni_xml_t * xlist_to_ax1(afni_xml_list * xlist)
{
   afni_xml_t * newax;
   int          c;

   /* steal first struct pointer */
   newax = xlist->xlist[0];
   xlist->xlist[0] = NULL;

   /* whine if we get too many structures */
   if( xlist->len > 1 ) {
      fprintf(stdrr,"** axio_read_file: not ready for multiple afni_xml_t\n");
      for(c = 0; c < xlist->len; c++) axml_free_xml_t(xlist->xlist[c]);
   }

   /* and free pointer array */
   free(xlist->xlist);

   return newax;
}
