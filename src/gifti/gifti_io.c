#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <zlib.h>
#include "gifti_io.h"
#include "gifti_xml.h"  /* not for general consumption */

/*! global history and version strings, for printing */
static char * gifti_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of gifti library changes):\n"
  "\n",
  "0.0  18 July, 2007\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     - initial version\n"
  "0.1  31 July, 2007\n",
  "     - changed dim0..dim5 to dims[]\n"
  "     - changed nvals to size_t\n"
  "     - added gifti_init_darray_from_attrs and some validation functions\n"
  "0.2  29 October, 2007\n",
  "     - renamed gifti.[ch] to gifti_io.[ch]\n"
  "     - main data structures all start with gii (or gifti_)\n"
  "     - added user indenting\n"
  "0.3  21 November, 2007\n",
  "     - added base64 encoding/decoding, via b64_en/decode_table\n"
  "     - added gifti_list_index2string, gifti_disp_hex_data, \n"
  "             gifti_check_swap, gifti_swap_Nbytes, etc.\n"
  "     - pop_darray: check for b64 errors and byte swapping\n"
  "     - dind is size_t\n",
  "0.4  29 November, 2007\n"
  "     - added more checks and fixed nvpair value allocation\n"
  "0.5  03 December, 2007: applied changes for GIFTI Format 1.0 (11/21)\n",
  "     - replaced Category with Intent\n"
  "     - replaced Location attribute with ExternalFileName/Offset\n"
  "     - added NumberOfDataArrays attribute to GIFTI element\n"
  "     - applied new index_order strings\n"
  "0.6  10 December, 2007:\n",
  "     - can read/write Base64Binary datasets (can set compress level)\n"
  "     - removed datatype lists (have gifti_type_list)\n"
  "     - added gifti_read_da_list(), with only partial ability\n"
  "     - added GIFTI numDA attribute\n"
  "     - change size_t to long long\n"
  "0.7  11 December, 2007:\n",
  "     - added GIFTI_B64_CHECK defines\n"
  "     - set b64_check default to SKIPNCOUNT\n"
  "     - added disp_gxml_data\n"
  "0.8  12 December, 2007:\n",
  "     - added sub-surface selection, via dalist in gifti_read_da_list()\n"
  "     - added gifti_copy_DataArray, and other structures\n"
};

static char gifti_version[] = "gifti library version 0.8, 12 December, 2007";

/* ---------------------------------------------------------------------- */
/* global lists of XML strings */

/* this should match GIFTI_IND_ORD_* */
char * gifti_index_order_list[] = {"Undefined", "RowMajorOrder",
                                                "ColumnMajorOrder"};
/* {"Undefined", "HighestFirst", "LowestFirst"}; */

/* char * gifti_dataloc_list[] = {"Undefined", "Internal", "External"}; */

static gifti_type_ele gifti_type_list[] = {
    /* type  nbyper  swapsize   name  */
    {   0,      0,      0,      "Undefined"             },
    {   2,      1,      0,      "NIFTI_TYPE_UINT8"      },
    {   4,      2,      2,      "NIFTI_TYPE_INT16"      },
    {   8,      4,      4,      "NIFTI_TYPE_INT32"      },
    {  16,      4,      4,      "NIFTI_TYPE_FLOAT32"    },
    {  32,      8,      4,      "NIFTI_TYPE_COMPLEX64"  },
    {  64,      8,      8,      "NIFTI_TYPE_FLOAT64"    },
    { 128,      3,      0,      "NIFTI_TYPE_RGB24"      },
    { 256,      1,      0,      "NIFTI_TYPE_INT8"       },
    { 512,      2,      2,      "NIFTI_TYPE_UINT16"     },
    { 768,      4,      4,      "NIFTI_TYPE_UINT32"     },
    {1024,      8,      8,      "NIFTI_TYPE_INT64"      },
    {1280,      8,      8,      "NIFTI_TYPE_UINT64"     },
    {1536,     16,     16,      "NIFTI_TYPE_FLOAT128"   },
    {1792,     16,      8,      "NIFTI_TYPE_COMPLEX128" },
    {2048,     32,     16,      "NIFTI_TYPE_COMPLEX256" },
};

/* treat intent codes internally, in case we eventually link to nifti */
typedef struct { char * name; int code; } gifti_intent_ele;
static gifti_intent_ele gifti_intent_list[] = {
    { "NIFTI_INTENT_NONE",               0 },
    { "NIFTI_INTENT_CORREL",             2 },
    { "NIFTI_INTENT_TTEST",              3 },
    { "NIFTI_INTENT_FTEST",              4 },
    { "NIFTI_INTENT_ZSCORE",             5 },
    { "NIFTI_INTENT_CHISQ",              6 },
    { "NIFTI_INTENT_BETA",               7 },
    { "NIFTI_INTENT_BINOM",              8 },
    { "NIFTI_INTENT_GAMMA",              9 },
    { "NIFTI_INTENT_POISSON",           10 },
    { "NIFTI_INTENT_NORMAL",            11 },
    { "NIFTI_INTENT_FTEST_NONC",        12 },
    { "NIFTI_INTENT_CHISQ_NONC",        13 },
    { "NIFTI_INTENT_LOGISTIC",          14 },
    { "NIFTI_INTENT_LAPLACE",           15 },
    { "NIFTI_INTENT_UNIFORM",           16 },
    { "NIFTI_INTENT_TTEST_NONC",        17 },
    { "NIFTI_INTENT_WEIBULL",           18 },
    { "NIFTI_INTENT_CHI",               19 },
    { "NIFTI_INTENT_INVGAUSS",          20 },
    { "NIFTI_INTENT_EXTVAL",            21 },
    { "NIFTI_INTENT_PVAL",              22 },
    { "NIFTI_INTENT_LOGPVAL",           23 },
    { "NIFTI_INTENT_LOG10PVAL",         24 },
    { "NIFTI_INTENT_ESTIMATE",        1001 },
    { "NIFTI_INTENT_LABEL",           1002 },
    { "NIFTI_INTENT_NEURONAME",       1003 },
    { "NIFTI_INTENT_GENMATRIX",       1004 },
    { "NIFTI_INTENT_SYMMATRIX",       1005 },
    { "NIFTI_INTENT_DISPVECT",        1006 },
    { "NIFTI_INTENT_VECTOR",          1007 },
    { "NIFTI_INTENT_POINTSET",        1008 },
    { "NIFTI_INTENT_TRIANGLE",        1009 },
    { "NIFTI_INTENT_QUATERNION",      1010 },
    { "NIFTI_INTENT_DIMLESS",         1011 },
    { "NIFTI_INTENT_TIME_SERIES",     2001 },
    { "NIFTI_INTENT_NODE_INDEX",      2002 },
    { "NIFTI_INTENT_RGB_VECTOR",      2003 },
    { "NIFTI_INTENT_RGBA_VECTOR",     2004 },
    { "NIFTI_INTENT_SHAPE",           2005 }
};

/* this should match GIFTI_ENCODING_* */
char * gifti_encoding_list[] = {
    "Undefined", "ASCII", "Base64Binary", "GZipBase64Binary",
    "ExternalFileBinary"
};

/* this should match GIFTI_ENDIAN_* */
char * gifti_endian_list[] = {"Undefined", "BigEndian", "LittleEndian"};
/* ---------------------------------------------------------------------- */

/* giftilib globals */
static gifti_globals G = { 1 };
int gifti_get_verb( void )     { return G.verb; }
int gifti_set_verb( int level ){ G.verb = level;  return 1; }
int gifti_get_b64_check( void )     { return gxml_get_b64_check(); }
int gifti_set_b64_check( int level ){ return gxml_set_b64_check(level); }
int gifti_get_zlevel( void )        { return gxml_get_zlevel(); }
int gifti_set_zlevel( int level )
{
    /* note that the default currently results in 6 */
    if( level != Z_DEFAULT_COMPRESSION && (level < 0 || level > 9 ) ) {
        fprintf(stderr,"** invalid zlevel, must be %d (default) or {0..9}\n",
                Z_DEFAULT_COMPRESSION);
        return 1;
    }
    return gxml_set_zlevel(level);
}

/* ---------------------------------------------------------------------- */

#undef G_CHECK_NULL_STR
#define G_CHECK_NULL_STR(s) (s ? s : "NULL")

/* ---------------------------------------------------------------------- */
/* static prototypes */
static int str2list_index(char *list[], int max, const char *str);

/* return 0 on success, 1 on error */
int gifti_str2attr_gifti(gifti_image * gim, const char *attr, const char *val)
{
    if( !gim || !attr || !val ) {
        fprintf(stderr,"** GS2AG: bad params (%p,%p,%p)\n",
                (void *)gim, (void *)attr, (void *)val);
        return 1;
    }

    if( G.verb > 2 )
        fprintf(stderr,"++ setting GIFTI attr '%s' from '%s'\n", attr, val);

    if( !strcmp(attr, "Version") ) {
        if( gim->version ) free( gim->version );  /* lose any old copy */
        gim->version = gifti_strdup(val);
    } else if( !strcmp(attr, "NumberOfDataArrays") ) {
        gim->numDA = atol(val);
        if( gim->numDA < 0 ) {
            fprintf(stderr,"** invalid NumberOfDataArrays attribute: %s\n",val);
            gim->numDA = 0;
            return 1;
        }
    } else {
        if( G.verb > 0 )
            fprintf(stderr,"** unknown GIFTI attrib, '%s'='%s'\n",attr,val);
        return 1;
    }

    return 0;
}

gifti_image * gifti_read_image( const char * fname, int read_data )
{
    if( !fname ) {
        fprintf(stderr,"** gifti_read_image: missing filename\n");
        return NULL;
    }

    gxml_set_verb(G.verb);

    return gxml_read_image(fname, read_data, NULL, 0);
}

/* store only list of giiDataArray indices
 * (must CURRENTLY be a sorted subset of unique indices in {0..numDA-1})
 */
gifti_image * gifti_read_da_list( const char * fname, int read_data,
                                  const int * dalist, int len )
{
    if( !fname ) {
        fprintf(stderr,"** gifti_read_image: missing filename\n");
        return NULL;
    }

    gxml_set_verb(G.verb);

    return gxml_read_image(fname, read_data, dalist, len);
}

int gifti_write_image(gifti_image *gim, const char *fname, int write_data)
{
    int errs = 0;

    if( !gim ) {
        fprintf(stderr,"** gifti_write_image, missing gifti_image\n");
        errs++;
    } else if( !fname ) {
        fprintf(stderr,"** gifti_read_image: missing filename\n");
        errs++;
    }

    if( errs ) return 1;

    gxml_set_verb(G.verb);

    return gxml_write_image(gim, fname, write_data);
}

int gifti_set_xml_buf_size( int buf_size )
{
    return gxml_set_buf_size(buf_size);
}


int gifti_free_image( gifti_image * gim )
{
    if( !gim ) {
        if( G.verb > 2 ) fprintf(stderr,"** free w/NULL gifti_image ptr\n");
        return 1;
    }
    
    if( G.verb > 2 ) fprintf(stderr,"-- freeing gifti_image\n");

    if( gim->version ) { free(gim->version);  gim->version = NULL; }

    (void)gifti_free_nvpairs(&gim->meta);
    (void)gifti_free_LabelTable(&gim->labeltable);
    (void)gifti_free_DataArray_list(gim->darray, gim->numDA);
    (void)gifti_free_nvpairs(&gim->ex_atrs);
    free(gim);

    return 0;
}

int gifti_free_nvpairs( nvpairs * p )
{
    int c;

    if( !p ) {
        if( G.verb > 3 ) fprintf(stderr,"** free w/NULL nvpairs ptr\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- freeing %d nvpairs\n", p->length);

    if( p->name && p->value ) {
        for( c = 0; c < p->length; c++ ) {
            if( p->name[c] ) free(p->name[c]);
            if( p->value[c] ) free(p->value[c]);
        }
        free(p->name);
        free(p->value);
        p->name = NULL;
        p->value = NULL;
    }
    p->length = 0;

    return 0;
}

int gifti_free_LabelTable( giiLabelTable * t )
{
    int c;

    if( !t ) {
        if(G.verb > 3) fprintf(stderr,"** free w/NULL giiLabelTable ptr\n");
        return 1;
    }

    if(G.verb > 3)
        fprintf(stderr,"-- freeing %d giiLabelTables\n", t->length);

    if( t->index && t->label ) {
        for( c = 0; c < t->length; c++ )
            if( t->label[c] ) free(t->label[c]);
        free(t->index);
        free(t->label);
        t->index = NULL;
        t->label = NULL;
    }
    t->length = 0;

    return 0;
}

int gifti_free_DataArray_list(giiDataArray ** darray, int numDA)
{
    int c;

    if( !darray ) {
        if( G.verb > 3 ) fprintf(stderr,"** tried to free NULL darray list\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- freeing %d giiDataArrays\n", numDA);

    if( !darray || numDA < 0 ) return 1;
    for( c = 0; c < numDA; c++ )
        if( gifti_free_DataArray(darray[c]) ) return 1;

    free(darray);

    return 0;
}

int gifti_free_DataArray( giiDataArray * darray )
{
    if( !darray ) {
        if( G.verb > 3 ) fprintf(stderr,"** tried to free NULL darray ptr\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-- freeing giiDataArray\n");

    (void)gifti_free_nvpairs(&darray->meta);
    if( darray->coordsys) {
        gifti_free_CoordSystem(darray->coordsys);
        darray->coordsys = NULL;
    }
    if( darray->data ) { free(darray->data); darray->data = NULL; }
    (void)gifti_free_nvpairs(&darray->ex_atrs);
    free(darray);

    return 0;
}

int gifti_free_CoordSystem( giiCoordSystem * cs )
{
    if( !cs ) return 0;  /* this is okay */

    if( G.verb > 3 ) fprintf(stderr,"-- freeing giiCoordSystem\n");

    if( cs->dataspace ) { free(cs->dataspace); cs->dataspace = NULL; }
    if( cs->xformspace ) { free(cs->xformspace); cs->xformspace = NULL; }

    free(cs);

    return 0;
}

int gifti_init_darray_from_attrs(giiDataArray * da, const char ** attr)
{
    int c;

    if( !da || !attr ) {
        if(G.verb>0) fprintf(stderr,"** G_IDFA: bad params (%p,%p)\n",
                             (void *)da,(void *)attr);
        return 1;
    }

    /* init to something kinder and gentler */
    for(c = 0; c < GIFTI_DARRAY_DIM_LEN; c++ ) da->dims[c] = 1;

    /* insert attributes - if unknown, store with extras */
    clear_nvpairs(&da->ex_atrs); /* prepare for unknow attributes */
    for(c = 0; attr[c]; c += 2 ) {
        if( gifti_str2attr_darray(da, attr[c],attr[c+1]) )
            if( gifti_add_to_nvpairs(&da->ex_atrs,attr[c],attr[c+1]) )
                return 1;
    }

    /* clear elements */
    clear_nvpairs(&da->meta);
    da->coordsys = NULL;
    da->data = NULL;

    /* and init extras */

    da->nvals = gifti_darray_nvals(da);
    gifti_datatype_sizes(da->datatype, &da->nbyper, NULL); /* set nbyper */

    (void)gifti_valid_darray(da, G.verb>0);              /* just a check */

    return 0;
}

/* check for consistency - rcr - write */
int gifti_valid_darray(giiDataArray * da, int whine)
{
    int errs = 0;

    if( !da ) {
        if( whine || G.verb > 1 ) fprintf(stderr,"** invalid darray pointer\n");
        return 0;
    }

    if( ! gifti_intent_is_valid(da->intent) ) {
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray intent code = %d\n", da->intent);
        errs++;
    }

    if( ! gifti_valid_datatype(da->datatype, whine) ) /* message printed */
        errs++;

    /* no checks for ext_fname and ext_offset (until reading) */

    if( da->ind_ord<=GIFTI_IND_ORD_UNDEF || da->ind_ord>GIFTI_IND_ORD_MAX ) {
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray ind_ord = %d\n", da->ind_ord);
        errs++;
    }

    if( ! gifti_valid_num_dim(da->num_dim, whine) ) /* message printed */
        errs++;

    if( ! gifti_valid_dims(da, whine) ) /* message printed */
        errs++;

    if( da->encoding<=GIFTI_ENCODING_UNDEF || da->encoding>GIFTI_ENCODING_MAX ){
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray encoding = %d\n", da->encoding);
        errs++;
    }

    if( da->endian<=GIFTI_ENDIAN_UNDEF || da->endian>GIFTI_ENDIAN_MAX ) {
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray endian = %d\n", da->endian);
        errs++;
    }

    /* of sub-element, only verify giiMetaData */
    if( ! gifti_valid_nvpairs(&da->meta, whine) ) /* message printed */
        errs++;

    if( da->nvals <= 0 ) {
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray nvals = %u\n",
                    (unsigned)da->nvals );
        errs++;
    }

    if( ! gifti_valid_nbyper(da->nbyper, whine) ) /* message printed */
        errs++;

    if( ! gifti_valid_nvpairs(&da->ex_atrs, whine) ) /* message printed */
        errs++;

    if( errs ) return 0;

    return 1;
}

/* check for set pointers */
int gifti_valid_nvpairs(nvpairs * nvp, int whine)
{
    int c;

    if( !nvp ) {
        if( G.verb>0 || whine ) fprintf(stderr,"** invalid nvpairs pointer\n");
        return 0;
    }

    if( nvp->length < 0 ) {
        if( G.verb > 1 || whine )
            fprintf(stderr,"** invalid nvpair length = %d\n", nvp->length);
        return 0;
    }

    if( nvp->length == 0 ) return 1;    /* quick case: valid */

    if( !nvp->name || !nvp->value ){
        if( G.verb > 1 || whine )
            fprintf(stderr,"** invalid nvpair name, value = %p, %p\n",
                    (void *)nvp->name, (void *)nvp->value);
        return 0;
    }

    /* quit on first error */
    for( c = 0; c < nvp->length; c++ ) {
        if( ! nvp->name[c] ) {
            if( G.verb > 1 || whine )
                fprintf(stderr,"** invalid nvpair name[%d]\n", c);
            return 0;
        }
        if( ! nvp->value[c] ) {
            if( G.verb > 1 || whine )
                fprintf(stderr,"** invalid nvpair value[%d]\n", c);
            return 0;
        }
    }

    return 1;
}

/* just check bounds */
int gifti_valid_num_dim(int num_dim, int whine)
{
    if( num_dim <= 0 || num_dim > GIFTI_DARRAY_DIM_LEN ) {
        if( G.verb > 1 || whine )
            fprintf(stderr,"** invalid num_dim = %d\n", num_dim);
        return 0;
    }
    return 1;
}

/* check list */
int gifti_valid_datatype(int dtype, int whine)
{
    int c;

    /* check for valid */
    for( c = sizeof(gifti_type_list) / sizeof(gifti_type_ele) - 1; c > 0; c-- )
        if( dtype == gifti_type_list[c].type ) return 1;

    if( whine || G.verb > 1 )
        fprintf(stderr,"** invalid datatype value %d\n", dtype);

    return 0;
}

/* check list */
int gifti_valid_nbyper(int nbyper, int whine)
{
    int c;

    /* check for valid */
    for( c = sizeof(gifti_type_list) / sizeof(gifti_type_ele) - 1; c > 0; c-- )
        if( nbyper == gifti_type_list[c].nbyper ) return 1;

    if( whine || G.verb > 1 )
        fprintf(stderr,"** invalid nbyper value %d\n", nbyper);

    return 0;
}

int gifti_valid_dims(giiDataArray * da, int whine)
{
    int c;

    if( !da ) {
        if( G.verb > 0 ) fprintf(stderr,"** GVD: no giiDataArray\n");
        return 0;
    }

    if( ! gifti_valid_num_dim( da->num_dim, whine ) )
        return 0;

    for( c = 0; c < da->num_dim; c++ )
        if( da->dims[c] <= 0 ) {
            if( G.verb > 1 || whine )
                fprintf(stderr,"** invalid dims[%d] = %d\n", c, da->dims[c]);
            return 0;
        }

    return 1;
}

int gifti_str2attr_darray(giiDataArray * DA, const char *attr,
                                             const char *value)
{
    if( !DA || !attr || !value ) {
        if( G.verb > 0 )
            fprintf(stderr,"** G_S2A_D: bad params (%p,%p,%p)\n",
                    (void *)DA, (void *)attr, (void *)value);
        return 1;
    }

    if(G.verb > 3) fprintf(stderr,"++ setting DA attr '%s'='%s'\n",attr,value);

    if( !strcmp(attr, "Intent") )
        DA->intent = gifti_intent_from_string(value);
    else if( !strcmp(attr, "DataType") )
        DA->datatype = gifti_str2datatype(value);
    else if( !strcmp(attr, "ArrayIndexingOrder") )
        DA->ind_ord = gifti_str2ind_ord(value);
    else if( !strcmp(attr, "Dimensionality") ) DA->num_dim = atoi(value);
    else if( !strcmp(attr, "Dim0") )           DA->dims[0] = atoi(value);
    else if( !strcmp(attr, "Dim1") )           DA->dims[1] = atoi(value);
    else if( !strcmp(attr, "Dim2") )           DA->dims[2] = atoi(value);
    else if( !strcmp(attr, "Dim3") )           DA->dims[3] = atoi(value);
    else if( !strcmp(attr, "Dim4") )           DA->dims[4] = atoi(value);
    else if( !strcmp(attr, "Dim5") )           DA->dims[5] = atoi(value);
    else if( !strcmp(attr, "Encoding") ) 
        DA->encoding = gifti_str2encoding(value);
    else if( !strcmp(attr, "Endian") )
        DA->endian = gifti_str2endian(value);
    else if( !strcmp(attr, "ExternalFileName") )
        DA->ext_fname = gifti_strdup(value);
    else if( !strcmp(attr, "ExternalFileOffset") )
        DA->ext_offset = atoll(value);  /* assumes C99 */
    else {
        if( G.verb > 1 )        /* might go into ex_atrs */
            fprintf(stderr,"** unknown giiDataArray attr, '%s'='%s'\n",
                    G_CHECK_NULL_STR(attr),G_CHECK_NULL_STR(value));
        return 1;
    }

    return 0;
}

/* return 0 (UNDEF) on failure */
static int str2list_index( char * list[], int max, const char *str )
{
    int index;
    if( !list || !str ) {
        if( G.verb > 0 ) fprintf(stderr,"** str2list: bad params (%p,%p)\n",
                                 (void *)list, (void *)str);
        return 0;  /* should be *_UNDEFINED */
    }

    for( index = max; index > 0; index-- )
        if( !strcmp(str, list[index]) ) return index;

    return 0;    /* failure */
}

int gifti_str2ind_ord( const char * str )
{
    int rv = str2list_index(gifti_index_order_list,GIFTI_IND_ORD_MAX,str);
    if( rv <= GIFTI_IND_ORD_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad index order, '%s'\n", str);
    return rv;
}


int gifti_str2encoding( const char * str )
{
    int rv = str2list_index(gifti_encoding_list, GIFTI_ENCODING_MAX, str);
    if( rv <= GIFTI_ENCODING_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad data encoding, '%s'\n", str);
    return rv;
}

/* This function is to index into one of the gifti_*_list arrays, 
 * while being certain the index is not out of range.
 */
char * gifti_list_index2string(char * list[], int index)
{
    int lsize;    /* list size cannot be computed from the passed pointer */

    if     ( list == gifti_index_order_list )
        lsize = sizeof(gifti_index_order_list)/sizeof(char *);

    else if( list == gifti_encoding_list )
        lsize = sizeof(gifti_encoding_list)/sizeof(char *);

    else if( list == gifti_endian_list )
        lsize = sizeof(gifti_endian_list)/sizeof(char *);

    else {
        fprintf(stderr,"** GLI2S: invalid list\n");
        return "UNKNOWN LIST";
    }

    if( index < 0 || index >= lsize ) {
        fprintf(stderr,"** GLI2S: index %d out of range {0..%d}\n",
                index,lsize-1);
        return "INDEX OUT OF RANGE";
    }

    /* all that work, just for the expected indexing...  */

    return list[index];
}

int gifti_str2datatype(const char * str)
{
    int len = sizeof(gifti_type_list)/sizeof(gifti_type_ele);
    int c;

    for( c = len - 1; c > 0; c-- )
        if( !strcmp(str, gifti_type_list[c].name) )
            break;

    return gifti_type_list[c].type;
}


int gifti_str2endian( const char * str )
{
    int rv = str2list_index(gifti_endian_list, GIFTI_ENDIAN_MAX, str);
    if( rv <= GIFTI_ENCODING_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad endian, '%s'\n", str);
    return rv;
}


char * gifti_datatype2str(int type)
{
    int len = sizeof(gifti_type_list)/sizeof(gifti_type_ele);
    int c;

    for( c = len - 1; c > 0; c-- )
        if( type == gifti_type_list[c].type)
            break;

    return gifti_type_list[c].name;
}


int clear_nvpairs(nvpairs * p)
{
    if( !p ) return 1;

    p->length = 0;
    p->name = NULL;
    p->value = NULL;

    return 0;
}

int clear_LabelTable(giiLabelTable * p)
{
    if( !p ) return 1;

    p->length = 0;
    p->index = NULL;
    p->label = NULL;

    return 0;
}

int clear_CoordSystem(giiCoordSystem * p)
{
    if( !p ) return 1;

    p->dataspace = NULL;
    p->xformspace = NULL;
    memset(p->xform,0,sizeof(p->xform));

    return 0;
}

int gifti_add_empty_darray(gifti_image * gim)
{
    giiDataArray * dptr;

    if( !gim ) return 1;

    if( G.verb > 3 ) fprintf(stderr,"++ alloc darray[%d]\n",gim->numDA);

    /* allocate for an additional pointer */
    gim->numDA++;
    gim->darray = (giiDataArray **)realloc(gim->darray,
                                        gim->numDA*sizeof(giiDataArray *));

    if( !gim->darray ) {
        fprintf(stderr,"** failed realloc darray, len %d\n", gim->numDA);
        gim->numDA = 0;
        return 1;
    }

    /* allocate the actual giiDataArray struct */
    dptr = (giiDataArray *)calloc(1, sizeof(giiDataArray));
    if( !dptr ) {
        fprintf(stderr,"** failed to alloc DA element #%d\n",gim->numDA);
        gim->numDA = 0;
        return 1;
    }

    gim->darray[gim->numDA-1] = dptr;

    return 0;
}

/* return 0 on success */
int gifti_add_to_nvpairs(nvpairs * p, const char * name, const char * value)
{
    int index;

    if( !p || !name || !value ) {
        if(G.verb > 1) fprintf(stderr,"** GATN: bad params(%p,%p,%p)\n",
                               (void *)p, (void *)name, (void *)value);
        return 1;
    }

    p->length++;
    p->name = (char **)realloc(p->name, p->length * sizeof(char *));
    p->value = (char **)realloc(p->value, p->length * sizeof(char *));

    if( !p->name || !p->value ) {
        fprintf(stderr,"** GATN: failed to realloc %d pointers\n",p->length);
        return 1;
    } else if ( G.verb > 3 )
        fprintf(stderr,"++ add_nvp [%d]: '%s', '%s'\n", p->length,
                name ? name : "NULL", value ? value : "NULL");

    index = p->length - 1;
    p->name[index] = gifti_strdup(name);
    p->value[index] = gifti_strdup(value);

    if( !p->name[index] || !p->value[index] ) {
        fprintf(stderr,"** GATN: failed to copy pair '%s'='%s'\n",name,value);
        return 1;
    }

    return 0;
}

int gifti_disp_nvpairs(const char * mesg, const nvpairs * p)
{
    int c;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: nvpairs = NULL\n", stderr); return 1; }

    fprintf(stderr,"nvpairs struct, len = %d :\n", p->length);

    for(c = 0; c < p->length; c++ )
        fprintf(stderr,"    nvpair: '%s' = '%s'\n",
                G_CHECK_NULL_STR(p->name[c]), G_CHECK_NULL_STR(p->value[c]));
    if( p->length > 0 ) fputc('\n', stderr);

    return 0;
}


int gifti_disp_LabelTable(const char * mesg, const giiLabelTable * p)
{
    int c;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: giiLabelTable = NULL\n", stderr); return 1; }

    fprintf(stderr,"giiLabelTable struct, len = %d :\n", p->length);

    for(c = 0; c < p->length; c++ )
        fprintf(stderr,"    index %d, label '%s'\n", p->index[c], p->label[c]);
    if( p->length > 0 ) fputc('\n', stderr);

    return 0;
}


int gifti_disp_CoordSystem(const char * mesg, const giiCoordSystem * p)
{
    int c1, c2;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: giiCoordSystem = NULL\n", stderr); return 1; }

    fprintf(stderr,"giiCoordSystem struct\n"
                   "    dataspace  = %s\n"
                   "    xformspace = %s\n", p->dataspace, p->xformspace);
    for( c1 = 0; c1 < 4; c1++ )
    {
        fprintf(stderr,"    xform[%d] :", c1);
        for( c2 = 0; c2 < 4; c2++ )
            fprintf(stderr,"  %f", p->xform[c1][c2]);
        fputc('\n', stderr);
    }

    return 0;
}



int gifti_disp_DataArray(const char * mesg, const giiDataArray * p, int subs)
{
    fprintf(stderr,"--------------------------------------------------\n");

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: giiDataArray = NULL\n", stderr); return 1; }

    fprintf(stderr,"giiDataArray struct\n"
                   "    intent   %4d = %s\n"
                   "    datatype   %2d = %s\n"
                   "    ind_ord    %2d = %s\n"
                   "    num_dim       = %d\n"
                   "    dims          = %d, %d, %d, %d, %d, %d\n"
                   "    encoding   %2d = %s\n"
                   "    endian     %2d = %s\n"
                   "    ext_fname     = %s\n"
                   "    ext_offset    = %lld\n"
                , p->intent,
                gifti_intent_to_string(p->intent),
                p->datatype, gifti_datatype2str(p->datatype),
                p->ind_ord,
                gifti_list_index2string(gifti_index_order_list, p->ind_ord),
                p->num_dim,
                p->dims[0], p->dims[1], p->dims[2],
                p->dims[3], p->dims[4], p->dims[5],
                p->encoding,
                gifti_list_index2string(gifti_encoding_list, p->encoding),
                p->endian,
                gifti_list_index2string(gifti_endian_list, p->endian),
                G_CHECK_NULL_STR(p->ext_fname), p->ext_offset
           );

    if( subs ) gifti_disp_nvpairs("darray->meta", &p->meta);
    if( subs ) gifti_disp_CoordSystem("darray->coordsys", p->coordsys);
                
    fprintf(stderr,"    data       = %p\n"
                   "    nvals      = %u\n"
                   "    nbyper     = %d\n",
                (void *)p->data, (unsigned)p->nvals, p->nbyper);

    if( subs ) gifti_disp_nvpairs("darray->ex_atrs", &p->ex_atrs);
    fprintf(stderr,"--------------------------------------------------\n");

    return 0;
}



int gifti_disp_gifti_image(const char * mesg, const gifti_image * p, int subs)
{
    fprintf(stderr,"==================================================\n");

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: gifti_image = NULL\n", stderr); return 1; }

    fprintf(stderr,"gifti_image struct\n"
                   "    version    = %s\n"
                   "    numDA      = %d\n", p->version, p->numDA);

    if( subs ) {
        char buf[32];
        int  c;

        gifti_disp_nvpairs("gim->meta", &p->meta);
        gifti_disp_LabelTable("gim->labeltable", &p->labeltable);
        for( c = 0; c < p->numDA; c++ ) {
            sprintf(buf, "gim->darray[%d]", c);
            gifti_disp_DataArray(buf, p->darray[c], subs);
        }
    }

    fprintf(stderr,"gifti_image struct\n"
                   "    swapped    = %d\n"
                   "    compressed = %d\n", p->swapped, p->compressed);

    if( subs ) gifti_disp_nvpairs("gim->ex_atrs" , &p->ex_atrs);
    else fprintf(stderr, " -- darray totals: %d MB\n", gifti_gim_DA_size(p,1));

    fprintf(stderr,"==================================================\n");

    return 0;
}


int gifti_gim_DA_size(const gifti_image * p, int in_mb)
{
    int c, bytes = 0;

    if( !p ) return -1;
    if( !p->darray || p->numDA <= 0 ) return 0;

    for( c = 0, bytes = 0; c < p->numDA; c++ )
        bytes += p->darray[c]->nvals * p->darray[c]->nbyper;

    if( in_mb ) bytes = (bytes + (1<<19) ) >> 20;  /* round to nearest MB */

    return bytes;
}


void gifti_datatype_sizes(int datatype, int *nbyper, int *swapsize)
{
    int c;

    for( c = sizeof(gifti_type_list) / sizeof(gifti_type_ele) - 1; c > 0; c-- )
        if( datatype == gifti_type_list[c].type ) {
            if( nbyper ) *nbyper = gifti_type_list[c].nbyper;
            if( swapsize ) *swapsize = gifti_type_list[c].swapsize;
            return;
        }

    if( G.verb > 0 ) fprintf(stderr,"** GDS with bad datatype %d\n", datatype);
    if( nbyper ) *nbyper = 0;
    if( swapsize ) *swapsize = 0;
}

long long gifti_darray_nvals(giiDataArray * da)
{
    long long ndim = 1;
    int       c;

    if(!da){ fprintf(stderr,"** GDND, no ptr\n"); return 0; }

    if( ! gifti_valid_num_dim(da->num_dim, 0) ) {
        fprintf(stderr,"** giiDataArray has illegal num_dim = %d\n",
                da->num_dim);
        return 0;
    }

    for( c = 0; c < da->num_dim; c++ ) ndim *= da->dims[c];

    if( ndim <= 0 ) {
        gifti_disp_DataArray("** bad Dim list in ", da, 0);
        return 0;
    }

    return ndim;
}


/* find giiDataArray element #index of the given intent */
giiDataArray * gifti_find_DA(gifti_image * gim, int intent, int index)
{
    int c, nfound;

    if( !gim || !gifti_intent_is_valid(intent) || index < 0 ) {
        fprintf(stderr,"** find_DA: bad inputs (%p, %d, %d)\n",
                (void *)gim, intent, index);
        return NULL;
    }

    if ( !gim-> darray ) return NULL;

    for( c = 0, nfound = 0; c < gim->numDA; c++ )
        if( gim->darray[c] && gim->darray[c]->intent == intent ) {
            if( nfound == index )
                return gim->darray[c];  /* success */
            nfound++;   /* else, increment counter and keep looking */
        }

    return NULL;
}


/* return an allocated list of giiDataArray pointers of the given intent */
int gifti_find_DA_list(gifti_image * gim, int intent,
                       giiDataArray *** list, int * len)
{
    int c, nfound;

    if( !gim || !gifti_intent_is_valid(intent) || !list || !len ) {
        fprintf(stderr,"** find_DA: bad inputs (%p, %d, %p, %p)\n",
                (void *)gim, intent, (void *)list, (void *)len);
        return 1;
    }

    if ( !gim->darray ) return 1;

    /* create one big enough to hold everything */
    *len = gim->numDA;
    *list = (giiDataArray **)calloc(*len, sizeof(giiDataArray *));
    if( !*list ) {
        fprintf(stderr,"** find_DA_list: failed to alloc %d ptrs\n",*len);
        *len = 0;
        return 1;
    }

    for( c = 0, nfound = 0; c < gim->numDA; c++ )
        if( gim->darray[c] && gim->darray[c]->intent == intent )
            (*list)[nfound++] = gim->darray[c];

    /* if we didn't find any, nuke list, but do not return an error */
    if( nfound == 0 ) { free(*list); *list = NULL; *len = 0; return 0; }

    /* otherwise, reallocate a smaller list */
    if( nfound < *len ) {
        *len  = nfound;
        *list = (giiDataArray**)realloc(*list,*len*sizeof(giiDataArray*));
        if( !*list ) {
            fprintf(stderr,"** find_DA_list: failed realloc of %d ptrs\n",*len);
            *len = 0;
            return 1;
        }
    }

    return 0;
}


int gifti_DA_rows_cols(giiDataArray * da, int * rows, int * cols)
{
    *rows = da->dims[0];  /* init */
    *cols = 1;
                                                                                
    if( da->num_dim == 1 ) return 0;  /* use default */
                                                                                
    if( da->ind_ord == GIFTI_IND_ORD_ROW_MAJOR ) {
        /* treat Dim[0] as nodes (they change most slowly) */
        *rows = da->dims[0];
        *cols = (*rows) ? da->nvals / *rows : 1;    /* be safe */
    } else {
        if( ! gifti_valid_num_dim(da->num_dim, 1) ){
            fprintf(stderr,"** cannot assign DA_rows_cols");
            return 1;
        }

        *rows = da->dims[da->num_dim-1];  /* take highest index */
        *cols = (*rows > 0) ? da->nvals / *rows : 1;
    }
                                                                                
    return 0;
}


void gifti_disp_lib_hist(void)
{
   int c, len = sizeof(gifti_history)/sizeof(char *);
   for( c = 0; c < len; c++ )
       fputs(gifti_history[c], stdout);
}

void gifti_disp_lib_version(void)
{
    printf("%s, compiled %s\n", gifti_version, __DATE__);
}

int gifti_disp_hex_data(const char *mesg, const void *data, int len, FILE *fp)
{
    const char * dp = (const char *)data;
    FILE       * stream;
    int          c;
    
    stream = fp ? fp : stdout;

    if( !data || len < 1 ) return -1;

    if( mesg ) fputs(mesg, stream);

    for( c = 0; c < len; c++ )
        fprintf(stream, " %02x", dp[c]);

    return 0;
}

int gifti_swap_2bytes(void * data, long long nsets)
{
    char    * cp1 = (char *)data, * cp2;
    char      tval;
    long long c;
    
    for( c = 0; c < nsets; c++ ) {
        cp2 = cp1 + 1;
        tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
        cp1 += 2;
    }

    return 0;
}


int gifti_swap_4bytes(void * data, long long nsets)
{
    char    * cp0 = (char *)data, * cp1, * cp2;
    char      tval;
    long long c;
    
    for( c = 0; c < nsets; c++ ) {
        cp1 = cp0;
        cp2 = cp0 + 3;
        tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
        cp1++;  cp2--;
        tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
        cp0 += 4;
    }

    return 0;
}


int gifti_swap_Nbytes(void * data, long long nsets, int swapsize)
{
    char    * cp0, * cp1, * cp2;
    char      tval;
    long long c;
    int       offset;      /* swapsize - 1, for speed */

    if( ! data || nsets < 0 || swapsize < 0 ) {
        fprintf(stderr,"** swap_Nbytes: bad params (%p,%lld,%d)\n",
                (void *)data, nsets, swapsize);
        return 1;
    }

    if     ( swapsize == 2 ) return gifti_swap_2bytes(data, nsets);
    else if( swapsize == 4 ) return gifti_swap_4bytes(data, nsets);
    
    /* peform a swap */
    cp0 = (char *)data;
    offset = swapsize-1;  /* just for speed */

    for( c = 0; c < nsets; c++ ) {
        cp1 = cp0;
        cp2 = cp0 + offset;
        while( cp2 > cp1 ) {
            tval = *cp1;  *cp1 = *cp2;  *cp2 = tval;
            cp1++;  cp2--;
        }
        cp0 += swapsize;
    }

    return 0;
}

/* return the current CPU endian: GIFTI_ENDIAN_BIG or _LITTLE */
int gifti_get_this_endian(void)
{
   int    one = 1;
   char * cp = (char *)&one;

   if( *cp ) return GIFTI_ENDIAN_LITTLE;

   return GIFTI_ENDIAN_BIG;
}

int gifti_check_swap(void * data, int endian, long long nsets, int swapsize)
{
    if( !data || nsets < 0 || swapsize < 0 ) {
        fprintf(stderr,"** check_swap: bad params (%p,%lld, %d)\n",
                (void *)data, nsets, swapsize);
        return 1;
    } else if ( endian <= GIFTI_ENDIAN_UNDEF || endian > GIFTI_ENDIAN_MAX ) {
        fprintf(stderr, "** check_swap: invalid endian %d\n", endian);
        return 1;
    }

    /* if endian is the same as this one, just return */
    if( endian == gifti_get_this_endian() ) {
        if( G.verb > 2 )
            fprintf(stderr,"-- darray no swap needed : %lld sets of %d bytes\n",
                    nsets, swapsize);
        return 0;
    }

    if( G.verb > 1 )
        fprintf(stderr,"++ darray swap: %lld sets of %d bytes\n",
                nsets, swapsize);

    /* do the swap */
    return gifti_swap_Nbytes(data, nsets, swapsize);
}

/*---------------------------------------------------------------------*/
/*! Given a NIFTI_INTENT string, such as "NIFTI_INTENT_NODE_INDEX",
 *  return the corresponding integral intent code.  The intent code is
 *  the macro value defined in nifti1.h.
 *
 *  return 0 on failure (NIFTI_INTENT_NONE)
*//*-------------------------------------------------------------------*/
int gifti_intent_from_string( const char * name )
{
    int tablen = sizeof(gifti_intent_list)/sizeof(gifti_intent_ele);
    int c;

    if( !name ) return 0;

    for( c = tablen-1; c > 0; c-- )
        if( !strcmp(name, gifti_intent_list[c].name) )
            break;

    return gifti_intent_list[c].code;
}


/*---------------------------------------------------------------------*/
/*! Given a NIFTI_TYPE value, such as NIFTI_TYPE_INT16, return the
 *  corresponding macro label as a string.  The dtype code is the
 *  macro value defined in nifti1.h.
*//*-------------------------------------------------------------------*/
char * gifti_intent_to_string( int code )
{
    int tablen = sizeof(gifti_intent_list)/sizeof(gifti_intent_ele);
    int c;

    for( c = tablen-1; c > 0; c-- )
        if( gifti_intent_list[c].code == code )
            break;

    return gifti_intent_list[c].name;
}


/*---------------------------------------------------------------------*/
/*! Return whether the given code is a valid NIFTI_INTENT code.
*//*-------------------------------------------------------------------*/
int gifti_intent_is_valid( int code )
{
    int tablen = sizeof(gifti_intent_list)/sizeof(gifti_intent_ele);
    int c;

    for( c = tablen-1; c > 0; c-- )
        if( gifti_intent_list[c].code == code )
            break;

    return( c > 0 );
}


/*---------------------------------------------------------------------*/
/*! duplicate the given string
*//*-------------------------------------------------------------------*/
char * gifti_strdup( const char * src )
{
    char * newstr;
    int    len;

    if( !src ) return NULL;  /* easy case */

    len = strlen(src) + 1;

    newstr = (char *)malloc(len * sizeof(char));
    if( !newstr ) {
        fprintf(stderr,"** failed gifti_strdup, len = %d\n", len);
        return NULL;
    }

    strcpy(newstr, src);

    return newstr;
}

/*---------------------------------------------------------------------*/
/*! duplicate the given giiDataArray struct, optionally including data
 *
 *  Allocate for a new struct, and fill it so that the contents are
 *  identical (sans pointers) to orig.  Sub-structure arrays will need
 *  to be allocated, also.
 *
 *  If get_data is not set, gnew->data will be left as NULL.
 *  
 *  return the address of the newly allocated structure
*//*-------------------------------------------------------------------*/
giiDataArray * gifti_copy_DataArray(const giiDataArray * orig, int get_data)
{
    giiDataArray * gnew;

    if( ! orig ){ fprintf(stderr,"** copy_DA: input is NULL\n"); return NULL; }

    if(G.verb > 5) fprintf(stderr,"++ copying giiDataArray...\n");

    gnew = (giiDataArray *)calloc(1, sizeof(giiDataArray));
    if(!gnew){fprintf(stderr,"** copy_DA, failed to alloc DA\n"); return NULL;}

    /* cheat - start by copying the entire struct contents */
    *gnew = *orig;

    /* copy any pointer data or structures */
    gnew->ext_fname = gifti_strdup(orig->ext_fname);
    gifti_copy_nvpairs(&gnew->meta, &orig->meta);
    gnew->coordsys = gifti_copy_CoordSystem(orig->coordsys);

    /* maybe the needy user wants data, too */
    if(orig->data && get_data) {
        if(G.verb > 5) fprintf(stderr,"++ copy_DA, adding data\n");
        gnew->data = malloc(gnew->nvals * gnew->nbyper);
        if(!gnew->data)       /* continue? */
            fprintf(stderr,"** copy DA, failed to alloc %lld bytes for data\n",
                    gnew->nvals * gnew->nbyper);
    } else
        gnew->data = NULL;

    /* last and certainly least, ex_atrs */
    gifti_copy_nvpairs(&gnew->ex_atrs, &orig->ex_atrs);

    return gnew;
}

/*---------------------------------------------------------------------*/
/*! dupliate the giiCoordSystem struct (passing NULL is okay)
*//*-------------------------------------------------------------------*/
giiCoordSystem * gifti_copy_CoordSystem(const giiCoordSystem * src)
{
    giiCoordSystem * csnew;
    int              r, c;

    if( !src ) return NULL;     /* this may be okay */

    if( G.verb > 6 ) fprintf(stderr,"++ copy_CS\n");

    csnew = (giiCoordSystem *)malloc(sizeof(giiCoordSystem));
    if( !csnew ){ fprintf(stderr,"** copy_CS: failed alloc\n"); return NULL; }

    csnew->dataspace  = gifti_strdup(src->dataspace);
    csnew->xformspace = gifti_strdup(src->xformspace);

    for( r = 0; r < 4; r++ )
        for( c = 0; c < 4; c++ )
            csnew->xform[r][c] = src->xform[r][c];

    return csnew;
}

/*---------------------------------------------------------------------*/
/*! dupliate the contents of one nvpairs structure into an empty one
 *
 *  return 0 on success
*//*-------------------------------------------------------------------*/
int gifti_copy_nvpairs(nvpairs * dest, const nvpairs * src)
{
    if( !dest || !src ){
        fprintf(stderr,"** copy_NVP, bad params (%p,%p)\n",
                (void*)dest,(void*)src);
        return 1;
    }

    if( G.verb > 6 ) fprintf(stderr,"++ copy_nvp, length %d\n", src->length);

    /* check for a simple case */
    if( src->length <= 0 || !src->name || !src->value ) {
        dest->length = 0;
        dest->name = dest->value = NULL;
        return 0;
    }

    /* else, copy the lists */
    dest->length = src->length;
    dest->name   = gifti_copy_char_list(src->name, src->length);
    dest->value  = gifti_copy_char_list(src->value, src->length);

    return 0;
}

/*---------------------------------------------------------------------*/
/*! dupliate the list of strings
*//*-------------------------------------------------------------------*/
char ** gifti_copy_char_list(char ** list, int len)
{
    char ** newlist = NULL;
    int     c;

    if( !list || len <= 0 ) return NULL;

    newlist = (char **)malloc(len * sizeof(char *));
    if( !newlist ) {
        fprintf(stderr,"** copy_char_list fails for %d pointers\n", len);
        return NULL;
    }

    for( c = 0; c < len; c++)                   /* big finish */
        newlist[c] = gifti_strdup(list[c]);

    return newlist;
}
