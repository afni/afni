
/* This library depends on:

        afni_xml.c
        NIFTI-2
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <nifti2_io.h>
#include <inttypes.h>
#include "afni_xml_io.h"


/* local protos */
static afni_xml_t * xlist_to_ax1(afni_xml_list * xlist);

static int  axio_alloc_known_data(FILE * fp, afni_xml_t * ax, int depth);
static int  can_process_dtype(int dtype);
static int  dalloc_as_nifti_type(FILE * fp, afni_xml_t * ax, int64_t nvals,int);

static void disp_name_n_desc(FILE * fp, afni_xml_t * ax, int indent, int verb);
static void disp_brainmodel_child(FILE * fp, afni_xml_t * ax, int verb);
static void disp_namedmap_child(FILE * fp, afni_xml_t * ax, int verb);
static void disp_parcel_child(FILE * fp, afni_xml_t * ax, int verb);
static void disp_surface_child(FILE * fp, afni_xml_t * ax, int verb);
static void disp_volume_child(FILE * fp, afni_xml_t * ax, int verb);

static int64_t text_to_i64(int64_t *result, const char * text, int64_t nvals);
static int64_t text_to_f64(double * result, const char * text, int64_t nvals);


/* read a complete CIFTI dataset, returning nifti and and xml pieces

   text data is converted to binary, when known
 */
int axio_read_cifti_file(const char * fname, int get_ndata,
                         nifti_image ** nim_out, afni_xml_t ** ax_out)
{
   nifti_image      * nim = NULL;
   afni_xml_t       * ax = NULL;

   if( !fname || !nim_out || !ax_out ) {
      fprintf(stderr,"** axio_CIFTI: NULL inputs %p, %p, %p\n",
              fname, (void *)nim_out, (void *)ax_out);
      return 1;
   }

   /* init */
   *ax_out = NULL;

   /* set nim_out */
   nim = nifti_image_read(fname, get_ndata);
   *nim_out = nim;

   if( ! nim ) {
      fprintf(stderr,"** axio: failed to read NIFTI part of %s\n", fname);
      return 1;
   }

   /* set ax_out */
   ax = axio_cifti_from_ext(nim);
   *ax_out = ax;

   if( ! ax ) {
      fprintf(stderr,"** axio: no CIFTI extension found in %s\n", fname);
      return 1;
   }

   /* convert known data from text to binary */
   return axml_recur(axio_alloc_known_data, ax);
}


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

/* convert any known text to binary */
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


afni_xml_t * axio_cifti_from_ext(nifti_image * nim)
{
   nifti1_extension * ext;
   int                ind;

   if( !nim ) return NULL;

   /* just read until we have a CIFTI extension to process */
   ext = nim->ext_list;
   for( ind = 0; ind < nim->num_ext; ind++, ext++ ) {
      if( ext->ecode != NIFTI_ECODE_CIFTI ) continue;
      return axio_read_buf(ext->edata, ext->esize-8);
   }

   return NULL;
}

int axio_show_attrs(FILE * fp, afni_xml_t * ax, int indent)
{
   FILE * ofp = fp ? fp : stderr;
   int    ind, maxl, slen;

   if( !ax ) return 1;

   for( ind = 0, maxl = 1; ind < ax->attrs.length; ind++ ) {
      slen = strlen(ax->attrs.name[ind]);
      if( slen > maxl ) maxl = slen;
   }

   for( ind = 0; ind < ax->attrs.length; ind++ )
      fprintf(ofp, "%*s%-*s = %s\n", indent, "", maxl, ax->attrs.name[ind],
                                                       ax->attrs.value[ind]);
   return 0;
}

/* assume single Matrix for now */
int axio_show_cifti_summary(FILE * fp, char * mesg, afni_xml_t * ax, int verb)
{
   FILE       * ofp = fp ? fp : stderr;
   afni_xml_t * ac, * am;
   int          ind;

   if( ! ax ) {
      fprintf(stderr,"** AX_SCS: missing ax pointer\n");
      return 1;
   }

   if( mesg ) fputs(mesg, ofp);

   if( strcmp(ax->name, "CIFTI") ) {
      fprintf(ofp, "** missing CIFTI element, have %s\n", ax->name);
      return 1;
   }

   ac = axio_find_map_name(ax, "Matrix", 2);
   if( !ac ) {
      fprintf(ofp, "** missing CIFTI Matrix element\n");
      return 1;
   }

   if( verb > 1 ) fprintf(ofp, "-- have %d MIMap/MD elements\n", ac->nchild);

   for( ind = 0; ind < ac->nchild; ind++ ) {
      am = ac->xchild[ind];
      if( strcmp(am->name, "MatrixIndicesMap") ) continue;
      axio_show_mim_summary(ofp, NULL, am, verb);
   }

   return 0;
}

typedef void(*gen_disp_func_t)(FILE *, afni_xml_t *, int);
#define AXIO_NMIM_KIDS 5
static const char * MIM_kids[AXIO_NMIM_KIDS+1] =
   { "NamedMap", "Surface", "Parcel", "Volume", "BrainModel", "INVALID" };
static gen_disp_func_t MIM_disp_funcs[AXIO_NMIM_KIDS] = {
   disp_namedmap_child, disp_surface_child, disp_parcel_child,
   disp_volume_child, disp_brainmodel_child
};

static int get_map_index(afni_xml_t * ax)
{
   int kid;

   if( ! ax->name || ! *ax->name ) return -1;

   for( kid=0; kid<AXIO_NMIM_KIDS; kid++ )
      if( ! strcmp(ax->name, MIM_kids[kid]) )
         return kid;

   return -1;
}


int axio_show_mim_summary(FILE * fp, const char * mesg, afni_xml_t * ax, int verb)
{
   afni_xml_t * xm, * xt;
   FILE       * ofp = fp ? fp : stderr;
   int          kid, matkid, mind;

   if( ! ax ) {
      fprintf(stderr,"** AX_SMS: missing struct pointer\n");
      return 1;
   }
   if( mesg ) fputs(mesg, ofp);

   xm = axio_find_map_name(ax, "Matrix", 2);

   if( !xm || strcmp(xm->name, "Matrix") ) {
      fprintf(ofp, "** missing Matrix element under %s\n", ax->name);
      return 1;
   }

   if( verb > 1 ) fprintf(ofp, "-- have %d Matrix children\n", xm->nchild);

   for( matkid = 0; matkid < xm->nchild; matkid++ ) {
      xt = xm->xchild[matkid];
      if( strcmp(xt->name, "MatrixIndicesMap") ) continue;

      if( verb > 1 ) fprintf(ofp, "-- have %d MIMap children\n", xt->nchild);

      for( kid=0; kid<xt->nchild; kid++ ) {
         mind = get_map_index(xt->xchild[kid]);
         if( kid >= 0 ) MIM_disp_funcs[mind](ofp, xt->xchild[kid], verb);
      }
   }

   return 0;
}



/* depth first search for struct with given name
   if maxd >= 0, impose depth restriction
 */
afni_xml_t * axio_find_map_name(afni_xml_t * ax, const char * name, int maxd)
{
   afni_xml_t * rv;
   int          ind;

   if( !ax || !name || !*name ) return NULL;

   /* are we looking at it? */
   if( ax->name && !strcmp(ax->name, name) ) return ax;

   /* are we done looking? */
   if( maxd == 0 ) return NULL;

   for( ind=0; ind < ax->nchild; ind++ ) {
      rv = axio_find_map_name(ax->xchild[ind], name, maxd-1);
      if( rv ) return rv;
   }

   return NULL;
}


/* ====================================================================== */
/* ====               local functions, not for export                ==== */
/* ====================================================================== */

static void disp_name_n_desc(FILE * fp, afni_xml_t * ax, int indent, int verb)
{
   int max=50;

   if( !fp || !ax ) return;

   fprintf(fp, "%*s%s : ", indent, "", ax->name);

   if( ax && ax->xtext && ax->xlen > 0 ) {
      if( ax->xlen <= max ) fprintf(fp, "%.*s\n", ax->xlen, ax->xtext);
      else
         fprintf(fp, "\n%*s: %.*s ...\n", indent+3, "", max, ax->xtext);
      if( verb > 1 && ax->blen > 0 )
         fprintf(fp, "%*s: %" PRId64 " values of type %s\n", indent+3, "",
                 ax->blen, nifti_datatype_string(ax->btype));
   } else
      fputc('\n', fp);

   if( verb > 1 )axio_show_attrs(fp, ax, indent+6);
}

static void disp_namedmap_child(FILE * fp, afni_xml_t * ax, int verb)
{
   afni_xml_t * xt = axio_find_map_name(ax, "NamedMap", 1);
   afni_xml_t * xc = axio_find_map_name(ax, "MapName", 2);
   afni_xml_t * xl = axio_find_map_name(ax, "LabelTable", 2);

   /* NamedMap */
   disp_name_n_desc(fp, xt, 6, verb);
   if( !xt ) return;

   if( xl ) fprintf(fp, "         with length %d LabelTable\n", xl->nchild);
   disp_name_n_desc(fp, xc, 9, verb);
   disp_name_n_desc(fp, xl, 9, verb);

   fputc('\n', fp);
}

static void disp_surface_child(FILE * fp, afni_xml_t * ax, int verb)
{
   afni_xml_t * xc = axio_find_map_name(ax, "Surface", 1);

   if( !xc ) return;

   disp_name_n_desc(fp, xc, 6, verb);

   fputc('\n', fp);
}

static void disp_parcel_child(FILE * fp, afni_xml_t * ax, int verb)
{
   afni_xml_t * xc = axio_find_map_name(ax, "Parcel", 1);
   afni_xml_t * xc1, * xc2;

   if( !xc ) return;

   xc1 = axio_find_map_name(xc, "Vertices", 1);
   xc2 = axio_find_map_name(xc, "VoxelIndicesIJK", 1);

   disp_name_n_desc(fp, xc,  6, verb);
   disp_name_n_desc(fp, xc1, 9, verb);
   disp_name_n_desc(fp, xc2, 9, verb);


   fputc('\n', fp);
}

static void disp_volume_child(FILE * fp, afni_xml_t * ax, int verb)
{
   afni_xml_t * xc = axio_find_map_name(ax, "Volume", 1);
   afni_xml_t * xc1;

   if( !xc ) return;

   xc1 = axio_find_map_name(xc, "TransformationMatrixVoxelIndicesIJKtoXYZ", 1);

   disp_name_n_desc(fp, xc,  6, verb);
   disp_name_n_desc(fp, xc1, 9, verb);

   fputc('\n', fp);
}

static void disp_brainmodel_child(FILE * fp, afni_xml_t * ax, int verb)
{
   afni_xml_t * xc = axio_find_map_name(ax, "BrainModel", 1);
   afni_xml_t * xc1, * xc2;

   if( !xc ) return;

   xc1 = axio_find_map_name(xc, "VoxelIndicesIJK", 1);
   xc2 = axio_find_map_name(xc, "VertexIndices", 1);

   disp_name_n_desc(fp, xc,  6, verb);
   disp_name_n_desc(fp, xc1, 9, verb);
   disp_name_n_desc(fp, xc2, 9, verb);

   fputc('\n', fp);
}


/* allocate data for afni_xml_t struct, for known types
   (prototype matches first argument of axml_recur) */
static int axio_alloc_known_data(FILE * fp, afni_xml_t * ax, int depth)
{
   int64_t   ival = 0 ;
   char    * cp;

   (void)(depth); /* avoid warnings, depth is not used for this variant */
   if( ! ax ) return 1;
   if( ! ax->xtext || ax->xlen <= 0 ) return 0;  /* nothing to allocate */

   if( ax->bdata ) return 0;  /* already allocated? */

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
      if( ! cp ) { fprintf(fp, "** axAKD: no IndexCount\n");  return 1; }
      text_to_i64(&ival, cp, 1);
      return dalloc_as_nifti_type(fp, ax, ival, NIFTI_TYPE_INT64);
   }

   if( ! strcmp(ax->name, "VoxelIndicesIJK") ) {
      cp = axml_attr_value(ax->xparent, "IndexCount");
      if( ! cp ) { fprintf(fp, "** axAKD: no ijk IndexCount\n");  return 1; }
      text_to_i64(&ival, cp, 1);  /* allocate for this many IJK triples */
      return dalloc_as_nifti_type(fp, ax, 3*ival, NIFTI_TYPE_INT64);
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
      fprintf(fp, "** axio_alloc: failed to allocate %" PRId64 " vals of size %d\n",
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
      fprintf(fp, "** axio_alloc: read only %" PRId64 " of %" PRId64 " f64\n", nread, ntok);
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

/* this is currently processed as "long long", the meaning of which
 * varies, unfortunately */
static int64_t text_to_i64(int64_t * result, const char * text, int64_t nvals)
{
   char    * eptr, * sptr;
   int64_t * rptr, val;
   int64_t   nread;

   *result = 0; /* Initialize to zero incase of failure */
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
