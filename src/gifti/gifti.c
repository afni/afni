#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "gifti.h"
#include "gifti_xml.h"  /* not for general consumption */

/*! global history and version strings, for printing */
static char * ggi_history[] =
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
};

static char ggi_version[] = "gifti library version 0.1, 31 July, 2007";

/* ---------------------------------------------------------------------- */
/* global lists of XML strings */

/* this should match GIFTI_IND_ORD_* */
char * gifti_index_order_list[] = {"Undefined", "HighestFirst", "LowestFirst"};

/* this should match GIFTI_CAT_* */
char * gifti_category_list[] = {
    "Undefined", "Coordinates", "Functional", "Normals",
    "Labels", "RGBA", "Shape", "Tensors", "TopologyTriangles"
};

/* this should match GIFTI_DATALOC_* */
char * gifti_dataloc_list[] = {"Undefined", "Internal", "External"};

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

/* this should match nifti_datatype_value_list (replaced by gifti_type_list?) */
char * nifti_datatype_name_list[] = {
    "Undefined",
    "NIFTI_TYPE_UINT8", "NIFTI_TYPE_INT16", "NIFTI_TYPE_INT32",
    "NIFTI_TYPE_FLOAT32", "NIFTI_TYPE_COMPLEX64", "NIFTI_TYPE_FLOAT64",
    "NIFTI_TYPE_RGB24", "NIFTI_TYPE_INT8", "NIFTI_TYPE_UINT16",
    "NIFTI_TYPE_UINT32", "NIFTI_TYPE_INT64", "NIFTI_TYPE_UINT64",
    "NIFTI_TYPE_FLOAT128", "NIFTI_TYPE_COMPLEX128", "NIFTI_TYPE_COMPLEX256"
};
/* this should match nifti_datatype_name_list */
int nifti_datatype_value_list[] = {
    0,
    2, 4, 8, 16, 32, 64,        /* should we use the NIFTI #defines? */
    128, 256, 512, 768, 1024, 1280,
    1536, 1792, 2048
};

/* this should match GIFTI_ENCODING_* */
char * gifti_encoding_list[] = {
    "Undefined", "ASCII", "Base64Binary", "GZipBase64Binary"
};

/* this should match GIFTI_ENDIAN_* */
char * gifti_endian_list[] = {"Undefined", "BigEndian", "LittleEndian"};
/* ---------------------------------------------------------------------- */

/* giftilib globals */
static gifti_globals G = { 1 };
int gifti_get_verb( void )     { return G.verb; }
int gifti_set_verb( int level ){ G.verb = level;  return 1; }

/* static prototypes */
static int str2list_index(char *list[], int max, const char *str);

/* return 0 on success, 1 on error */
int gifti_str2attr_gifti(gifti_image * gim, const char *attr, const char *val)
{
    if( !gim || !attr || !val ) {
        fprintf(stderr,"** GS2AG: bad params (%p,%p,%p)\n", gim, attr, val);
        return 1;
    }

    if( G.verb > 2 )
        fprintf(stderr,"+d setting GIFTI attr '%s' from '%s'\n", attr, val);

    if( !strcmp(attr, "Version") ) {
        if( gim->version ) free( gim->version );  /* lose any old copy */
        gim->version = strdup(val);
    } else {
        if( G.verb > 0 )
            fprintf(stderr,"** unknown GIFTI attr, '%s'='%s'\n",attr,val);
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

    return gxml_read_image(fname, read_data);
}

int gifti_write_image(gifti_image * gim, const char * fname, int write_data)
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
    
    if( G.verb > 2 ) fprintf(stderr,"-d freeing gifti_image\n");

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

    if( G.verb > 3 ) fprintf(stderr,"-d freeing %d nvpairs\n", p->length);

    if( p->name && p->value ) {
        for( c = 0; c < p->length; c++ ) {
            if( p->name[c] ) free(p->name[c]);
            if( p->value[c] ) free(p->value[c]);
        }
        free(p->name);
        free(p->value);
    }
    p->length = 0;

    return 0;
}

int gifti_free_LabelTable( LabelTable * t )
{
    int c;

    if( !t ) {
        if( G.verb > 3 ) fprintf(stderr,"** free w/NULL LabelTable ptr\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-d freeing %d LabelTables\n", t->length);

    if( t->index && t->label ) {
        for( c = 0; c < t->length; c++ )
            if( t->label[c] ) free(t->label[c]);
        free(t->index);
        free(t->label);
    }
    t->length = 0;

    return 0;
}

int gifti_free_DataArray_list( DataArray ** darray, int numDA )
{
    int c;

    if( !darray ) {
        if( G.verb > 3 ) fprintf(stderr,"** tried to free NULL darray list\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-d freeing %d DataArrays\n", numDA);

    if( !darray || numDA < 0 ) return 1;
    for( c = 0; c < numDA; c++ )
        if( gifti_free_DataArray(darray[c]) ) return 1;

    free(darray);

    return 0;
}

int gifti_free_DataArray( DataArray * darray )
{
    if( !darray ) {
        if( G.verb > 3 ) fprintf(stderr,"** tried to free NULL darray ptr\n");
        return 1;
    }

    if( G.verb > 3 ) fprintf(stderr,"-d freeing DataArray\n");

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

int gifti_free_CoordSystem( CoordSystem * cs )
{
    if( !cs ) return 0;  /* this is okay */

    if( G.verb > 3 ) fprintf(stderr,"-d freeing CoordSystem\n");

    if( cs->dataspace ) { free(cs->dataspace); cs->dataspace = NULL; }
    if( cs->xformspace ) { free(cs->xformspace); cs->xformspace = NULL; }

    free(cs);

    return 0;
}

int gifti_init_darray_from_attrs(DataArray * da, const char ** attr)
{
    int c;

    if( !da || !attr ) {
        if(G.verb>0) fprintf(stderr,"** G_IDFA: bad params (%p,%p)\n",da,attr);
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
int gifti_valid_darray(DataArray * da, int whine)
{
    int errs = 0;

    if( !da ) {
        if( whine || G.verb > 1 ) fprintf(stderr,"** invalid darray pointer\n");
        return 0;
    }

    if( da->category <= GIFTI_CAT_UNDEF || da->category > GIFTI_CAT_MAX ) {
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray category = %d\n", da->category);
        errs++;
    }

    if( ! gifti_valid_datatype(da->datatype, whine) ) /* message printed */
        errs++;

    if( da->location<=GIFTI_DATALOC_UNDEF || da->location>GIFTI_DATALOC_MAX ) {
        if( whine || G.verb > 1 )
            fprintf(stderr,"** invalid darray location = %d\n", da->location);
        errs++;
    }

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

    /* of sub-element, only verify MetaData */
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
                    nvp->name, nvp->value);
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

int gifti_valid_dims(DataArray * da, int whine)
{
    int c;

    if( !da ) {
        if( G.verb > 0 ) fprintf(stderr,"** GVD: no DataArray\n");
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

int gifti_str2attr_darray(DataArray * DA, const char *attr, const char *value)
{
    if( !DA || !attr || !value ) {
        if( G.verb > 0 )
            fprintf(stderr,"** G_S2A_D: bad params (%p,%p,%p)\n",
                    DA, attr, value);
        return 1;
    }

    if(G.verb > 3) fprintf(stderr,"+d setting DA attr '%s'='%s'\n",attr,value);

    if( !strcmp(attr, "Category") )
        DA->category = gifti_str2category(value);
    else if( !strcmp(attr, "DataType") )
        DA->datatype = gifti_str2datatype(value);
    else if( !strcmp(attr, "DataLocation") )
        DA->location = gifti_str2dataloc(value);
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
    else {
        if( G.verb > 1 )        /* might go into ex_atrs */
            fprintf(stderr,"** unknown DataArray attr, '%s'='%s'\n",attr,value);
        return 1;
    }

    return 0;
}

/* return 0 (UNDEF) on failure */
static int str2list_index( char * list[], int max, const char *str )
{
    int index;
    if( !list || !str ) {
        if( G.verb > 0 )
            fprintf(stderr,"** str2list: bad params (%p,%p)\n", list, str);
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

int gifti_str2category( const char * str )
{
    int rv = str2list_index(gifti_category_list, GIFTI_CAT_MAX, str);
    if( rv <= GIFTI_CAT_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad category, '%s'\n", str);
    return rv;
}

int gifti_str2dataloc( const char * str )
{
    int rv = str2list_index(gifti_dataloc_list, GIFTI_DATALOC_MAX, str);
    if( rv <= GIFTI_DATALOC_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad data location, '%s'\n", str);
    return rv;
}

int gifti_str2encoding( const char * str )
{
    int rv = str2list_index(gifti_encoding_list, GIFTI_ENCODING_MAX, str);
    if( rv <= GIFTI_ENCODING_UNDEF && G.verb > 1 )
        fprintf(stderr,"** bad data encoding, '%s'\n", str);
    return rv;
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

int clear_LabelTable(LabelTable * p)
{
    if( !p ) return 1;

    p->length = 0;
    p->index = NULL;
    p->label = NULL;

    return 0;
}

int clear_CoordSystem(CoordSystem * p)
{
    if( !p ) return 1;

    p->dataspace = NULL;
    p->xformspace = NULL;
    memset(p->xform,0,sizeof(p->xform));

    return 0;
}

int gifti_add_empty_darray(gifti_image * gim)
{
    DataArray * dptr;

    if( !gim ) return 1;

    if( G.verb > 3 ) fprintf(stderr,"+d alloc darray[%d]\n",gim->numDA);

    /* allocate for an additional pointer */
    gim->numDA++;
    gim->darray = (DataArray **)realloc(gim->darray,
                                        gim->numDA*sizeof(DataArray *));

    if( !gim->darray ) {
        fprintf(stderr,"** failed realloc darray, len %d\n", gim->numDA);
        gim->numDA = 0;
        return 1;
    }

    /* allocate the actual DataArray struct */
    dptr = (DataArray *)calloc(1, sizeof(DataArray));
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
        if(G.verb > 1)
            fprintf(stderr,"** GATN: bad params(%p,%p,%p)\n", p, name, value);
        return 1;
    }

    p->length++;
    p->name = (char **)realloc(p->name, p->length * sizeof(char *));
    p->value = (char **)realloc(p->name, p->length * sizeof(char *));

    if( !p->name || !p->value ) {
        fprintf(stderr,"** GATN: failed to realloc %d pointers\n",p->length);
        return 1;
    }

    index = p->length - 1;
    p->name[index] = strdup(name);
    p->value[index] = strdup(value);

    if( !p->name[index] || !p->value[index] ) {
        fprintf(stderr,"** GATN: failed to copy pair '%s'='%s'\n",name,value);
        return 1;
    }

    return 0;
}

int gifti_disp_nvpairs(const char * mesg, nvpairs * p)
{
    int c;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: nvpairs = NULL\n", stderr); return 1; }

    fprintf(stderr,"nvpairs struct, len = %d :\n", p->length);

    for(c = 0; c < p->length; c++ )
        fprintf(stderr,"    nvpair: '%s' = '%s'\n", p->name[c], p->value[c]);
    if( p->length > 0 ) fputc('\n', stderr);

    return 0;
}


int gifti_disp_LabelTable(const char * mesg, LabelTable * p)
{
    int c;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: LabelTable = NULL\n", stderr); return 1; }

    fprintf(stderr,"LabelTable struct, len = %d :\n", p->length);

    for(c = 0; c < p->length; c++ )
        fprintf(stderr,"    index %d, label '%s'\n", p->index[c], p->label[c]);
    if( p->length > 0 ) fputc('\n', stderr);

    return 0;
}


int gifti_disp_CoordSystem(const char * mesg, CoordSystem * p)
{
    int c1, c2;

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: CoordSystem = NULL\n", stderr); return 1; }

    fprintf(stderr,"CoordSystem struct\n"
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



int gifti_disp_DataArray(const char * mesg, DataArray * p, int subs)
{
    fprintf(stderr,"--------------------------------------------------\n");

    if( mesg ) { fputs(mesg, stderr); fputc(' ', stderr); }

    if( !p ){ fputs("disp: DataArray = NULL\n", stderr); return 1; }

    fprintf(stderr,"DataArray struct\n"
                   "    category %d = %s\n"
                   "    datatype %d = %s\n"
                   "    location %d = %s\n"
                   "    ind_ord  %d = %s\n"
                   "    num_dim    = %d\n"
                   "    dims       = %d, %d, %d, %d, %d, %d\n"
                   "    encoding %d = %s\n"
                   "    endian   %d = %s\n",
                p->category, gifti_category_list[p->category],
                p->datatype, gifti_datatype2str(p->datatype),
                p->location, gifti_dataloc_list[p->location],
                p->ind_ord, gifti_index_order_list[p->ind_ord],
                p->num_dim,
                p->dims[0], p->dims[1], p->dims[2],
                p->dims[3], p->dims[4], p->dims[5],
                p->encoding, gifti_encoding_list[p->encoding],
                p->endian, gifti_endian_list[p->endian]
           );

    if( subs ) gifti_disp_nvpairs("darray->meta", &p->meta);
    if( subs ) gifti_disp_CoordSystem("darray->coordsys", p->coordsys);
                
    fprintf(stderr,"    data       = %p\n"
                   "    nvals      = %u\n"
                   "    nbyper     = %d\n",
                p->data, (unsigned)p->nvals, p->nbyper);

    if( subs ) gifti_disp_nvpairs("darray->ex_atrs", &p->ex_atrs);
    fprintf(stderr,"--------------------------------------------------\n");

    return 0;
}



int gifti_disp_gifti_image(const char * mesg, gifti_image * p, int subs)
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


int gifti_gim_DA_size(gifti_image * p, int in_mb)
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

size_t gifti_darray_nvals(DataArray * da)
{
    size_t ndim = 1;
    int    c;

    if(!da){ fprintf(stderr,"** GDND, no ptr\n"); return 0; }

    if( ! gifti_valid_num_dim(da->num_dim, 0) ) {
        fprintf(stderr,"** DataArray has illegal num_dim = %d\n", da->num_dim);
        return 0;
    }

    for( c = 0; c < da->num_dim; c++ ) ndim *= da->dims[c];

    if( ndim <= 0 ) {
        gifti_disp_DataArray("** bad Dim list in ", da, 0);
        return 0;
    }

    return ndim;
}


/* find DataArray element #index of the given category */
DataArray * gifti_find_DA(gifti_image * gim, int category, int index)
{
    int c, nfound;

    if( !gim || category <= GIFTI_CAT_UNDEF
             || category > GIFTI_CAT_MAX
             || index < 0 ) {
        fprintf(stderr,"** find_DA: bad inputs (%p, %d, %d)\n",
                gim, category, index);
        return NULL;
    }

    if ( !gim-> darray ) return NULL;

    for( c = 0, nfound = 0; c < gim->numDA; c++ )
        if( gim->darray[c] && gim->darray[c]->category == category ) {
            if( nfound == index )
                return gim->darray[c];  /* success */
            nfound++;   /* else, increment counter and keep looking */
        }

    return NULL;
}


/* return an allocated list of DataArray pointers of the given category */
int gifti_find_DA_list(gifti_image * gim, int category, DataArray *** list,
                       int * len)
{
    int c, nfound;

    if( !gim || category <= GIFTI_CAT_UNDEF
             || category > GIFTI_CAT_MAX
             || !list
             || !len ) {
        fprintf(stderr,"** find_DA: bad inputs (%p, %d, %p, %p)\n",
                gim, category, list, len);
        return 1;
    }

    if ( !gim->darray ) return 1;

    /* create one big enough to hold everything */
    *len = gim->numDA;
    *list = (DataArray **)calloc(*len, sizeof(DataArray *));
    if( !*list ) {
        fprintf(stderr,"** find_DA_list: failed to alloc %d ptrs\n",*len);
        *len = 0;
        return 1;
    }

    for( c = 0, nfound = 0; c < gim->numDA; c++ )
        if( gim->darray[c] && gim->darray[c]->category == category )
            (*list)[nfound++] = gim->darray[c];

    /* if we didn't find any, nuke list, but do not return an error */
    if( nfound == 0 ) { free(*list); *len = 0; return 0; }

    /* otherwise, reallocate a smaller list */
    if( nfound < *len ) {
        *len  = nfound;
        *list = (DataArray **)realloc(*list, *len * sizeof(DataArray *));
        if( !*list ) {
            fprintf(stderr,"** find_DA_list: failed realloc of %d ptrs\n",*len);
            *len = 0;
            return 1;
        }
    }

    return 0;
}


int gifti_DA_rows_cols(DataArray * da, int * rows, int * cols)
{
    *rows = da->dims[0];  /* init */
    *cols = 1;
                                                                                
    if( da->num_dim == 1 ) return 0;  /* use default */
                                                                                
    if( da->ind_ord == GIFTI_IND_ORD_HIGH2LOW ) {
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
   int c, len = sizeof(ggi_history)/sizeof(char *);
   for( c = 0; c < len; c++ )
       fputs(ggi_history[c], stdout);
}

void gifti_disp_lib_version(void)
{
    printf("%s, compiled %s\n", ggi_version, __DATE__);
}

