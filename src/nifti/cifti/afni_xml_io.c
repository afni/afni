
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

static int axio_alloc_known_data(FILE * fp, afni_xml_t * ax, int depth);
static int can_process_dtype(int dtype);
static int dalloc_as_nifti_type(FILE * fp, afni_xml_t * ax, int64_t nvals,
                                int dtype);

static int64_t text_to_i64(int64_t *result, const char * text, int64_t nvals);
static int64_t text_to_f64(double * result, const char * text, int64_t nvals);


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
   xlist = axml_read_file(fname, 1);
   if( xlist.len <= 0 || ! xlist.xlist ) return NULL;

   /* return the first afni_xml_t tree */
   return xlist_to_ax1(&xlist);
}

/* rcr */
int axio_text_to_binary(afni_xml_t * ax)
{
   if( ! ax ) return 0;

   return axml_recur(axio_alloc_known_data, ax);
}

int axio_num_tokens(const char * str, int64_t maxlen)
{
   char    * sp = (char *)str;
   int64_t   ind, len, ntok;
   int       intok;     /* flag: are we inside a token? */

   if( maxlen == 0 )      return 0;
   if( ! str || ! * str ) return 0;

   if( maxlen > 0 ) len = maxlen;
   else             len = strlen(str);

   ntok = 0;
   intok = 0;
   for( ind = 0, sp = (char *)str; ind < len; ind++, sp++ ) {
      /* just look for state switches */
      if( intok ) {
         if( isspace(*sp) || (*sp == ',') )
            intok = 0;   /* no longer within token */
      } else {
         if( ! (isspace(*sp) || (*sp == ',')) ) {
            intok = 1;   /* have new token */
            ntok++;
         }
      }
   }

   return ntok;
}



/* ====================================================================== */
/* ====               local functions, not for export                ==== */
/* ====================================================================== */

/* allocate data for afni_xml_t struct, for known types
   (prototype matches first argument of axml_recur) */
static int axio_alloc_known_data(FILE * fp, afni_xml_t * ax, int depth)
{
   int64_t   ival;
   char    * cp;

   if( ! ax ) return 1;
   if( ! ax->xtext || ax->xlen <= 0 ) return 0;  /* nothing to allocate */

   if( ! ax->name ) {
      fprintf(stderr,"** missing ax name for data alloc\n");
      return 1;
   }

   /* Vertices, TransformationMatrixVoxelIndicesIJKtoXYZ, 
      VertexIndices, VoxelIndicesIJK (convert to straight index?) */

   if( ! strcmp(ax->name, "TransformationMatrixVoxelIndicesIJKtoXYZ") )
      return dalloc_as_nifti_type(fp, ax, 16, NIFTI_TYPE_FLOAT64);

   if( ! strcmp(ax->name, "Vertices") )
      /* we do not know how many there will be, grrrr */
      return dalloc_as_nifti_type(fp, ax, -1, NIFTI_TYPE_INT64);

   if( ! strcmp(ax->name, "VertexIndices") ) {
      cp = axml_attr_value(ax->xparent, "IndexCount");
      if( ! cp ) { fprintf(fp, "** AXAKD: no IndexCount\n");  return 1; }
      text_to_i64(&ival, cp, 1);
      return dalloc_as_nifti_type(fp, ax, ival, NIFTI_TYPE_INT64);
   }

   return 0;
}

static int dalloc_as_nifti_type(FILE * fp, afni_xml_t * ax, int64_t nvals,
                                                            int dtype)
{
   int     nbyper = 0;
   int64_t nread, ntok;

   if( ! ax->xtext || ax->xlen <= 0 ) return 0;  /* nothing to allocate */
   if( ! can_process_dtype(dtype) ) {
      fprintf(stderr,"** DaNT, cannot process dtype %d\n", dtype);
      return 1;
   }

   /* if nvals is not known, count tokens */
   if( nvals >= 0 ) ntok = nvals;
   else             ntok = axio_num_tokens(ax->xtext, ax->xlen);

   if( ntok == 0 ) return 0;  /* nothing to do */

   /* -- we know what to do, get to work -- */

   ax->blen = ntok;
   ax->btype = dtype;

   /* note number of bytes per value and number of values to allocate */
   nifti_datatype_sizes(ax->btype, &nbyper, NULL);

   ax->bdata = malloc(nbyper * ntok);
   if( ! ax->bdata ) {
      fprintf(fp, "** axio_alloc: failed to allocate %ld vals of size %d\n",
              ntok, nbyper);
      ax->blen = 0;
      return 1;
   }

   /* handle all types here */
   if ( ax->btype == NIFTI_TYPE_FLOAT64 )
      nread = text_to_f64((double *)ax->bdata, ax->xtext, ntok);
   else if ( ax->btype == NIFTI_TYPE_INT64 )
      nread = text_to_i64((int64_t *)ax->bdata, ax->xtext, ntok);
   else {
      fprintf(stderr,"** DaNT: rcr - check bad dtype %d\n", ax->btype);
      nread = 0;
   }

   if( nread < ntok ) {
      if( nread == 0 ) { free(ax->bdata); ax->bdata = NULL; }

      ax->blen = nread;
      fprintf(fp, "** axio_alloc: read only %ld of %ld f64\n", nread, ntok);
      return 1;
   }

   return 0;
}

/* list all cases for which text_to_CASE is written */
static int can_process_dtype(int dtype)
{
   if( dtype == NIFTI_TYPE_INT64 )      return 1;
   if( dtype == NIFTI_TYPE_FLOAT64 )    return 1;

   /* warn if type is not even nifti */
   if( ! is_valid_nifti_type(dtype) ) 
      fprintf(stderr,"** DNT, %d is invalid as NIFTI type\n", dtype);

   return 0;
}

/* this is currently processed as "long long" */
static int64_t text_to_i64(int64_t * result, const char * text, int64_t nvals)
{
   char    * eptr, * sptr;
   int64_t * rptr, val;
   int64_t   nread;

   if( ! text || ! result) return 1;
   if( nvals <= 0 )        return 0;

   sptr = (char *)text;

   nread = 0;
   rptr = result;
   while( sptr ) {
      val = (int64_t)strtoll(sptr, &eptr, 10);
      if( sptr == eptr) break;          /* nothing to read */

      /* get data and increment pointer and counter */
      *rptr = val;  rptr++;  nread++;

      if( nread == nvals ) break;

      sptr = eptr;
   }

   return nread;
}

static int64_t text_to_f64(double * result, const char * text, int64_t nvals)
{
   char    * eptr, * sptr;
   double  * rptr, val;
   int64_t   nread;

   if( ! text || ! result) return 1;
   if( nvals <= 0 )        return 0;

   sptr = (char *)text;

   nread = 0;
   rptr = result;
   while( sptr ) {
      val = strtod(sptr, &eptr);
      if( sptr == eptr) break;          /* nothing to read */

      /* get data and increment pointer and counter */
      *rptr = val;  rptr++;  nread++;

      if( nread == nvals ) break;

      sptr = eptr;
   }

   return nread;
}

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
      fprintf(stderr,"** axio_read_file: not ready for multiple afni_xml_t\n");
      for(c = 0; c < xlist->len; c++) axml_free_xml_t(xlist->xlist[c]);
   }

   /* and free pointer array */
   free(xlist->xlist);

   return newax;
}
