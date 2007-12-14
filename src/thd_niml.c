/*----------------------------------------------------------------------
 * 19 May 2006: top-level routines to read NIML datasets         [rickr]
 *              routines to process NI_SURF_DSET datasets
 *----------------------------------------------------------------------
*/
#include "mrilib.h"
#include "suma_suma.h"

/* globals to control I/O of niml data      3 Aug 2006 [rickr] */
typedef struct {
    int add_nodes;       /* add to output    (AFNI_NSD_ADD_NODES == Y)  */
    int debug;           /* debug level      (AFNI_NIML_DEBUG level)    */
    int to_float;        /* convert to float (AFNI_NSD_TO_FLOAT == Y)   */
    int write_mode;      /* NI_TEXT_MODE     (AFNI_NIML_TEXT_DATA == Y) */
} ni_globals;
static ni_globals gni =         /* default values for globals   */
{
        0,                      /* add_nodes                    */
        0,                      /* debug                        */
        0,                      /* to_float                     */
        NI_BINARY_MODE          /* write_mode                   */
};

static int    are_sorted_ints(int *, int);
static int    loc_append_vals(char **, int *, char *, float, float, int, int);
static char * my_strndup(char *, int);
static int    nsd_add_colms_range(NI_group *, THD_3dim_dataset *);
static int    nsd_add_colms_type(THD_datablock *, NI_group *);
static int    nsd_add_sparse_data(NI_group *, THD_3dim_dataset *);
static int    nsd_add_str_atr_to_group(char*, char*, THD_datablock*, NI_group*);
static int    nsd_fill_index_list(NI_group *, THD_3dim_dataset *);
static int    nsd_string_atr_to_slist(char ***, int, ATR_string *);
static int    process_NSD_attrs(THD_3dim_dataset *);
static int    process_NSD_group_attrs(NI_group *, THD_3dim_dataset *);
static int    process_NSD_index_list(NI_group *, THD_datablock *);
static int    process_NSD_sparse_data(NI_group *, THD_3dim_dataset *);

/* list of AFNI_dataset group attributes to copy along */
static char * ni_surf_dset_attrs[] = {
                "label",
                "domain_parent_idcode",
                "geometry_parent_idcode",
                "sorted_node_def"
                                     };

#define NOTYPE_GET_MIN_MAX_POSN(data,len,min,minp,max,maxp)             \
        do { int ii;                                                    \
             min=max=data[0];  minp=maxp=0;                             \
             for(ii = 1; ii < len; ii++)                                \
                if(data[ii]<min){ min=data[ii]; minp=ii; }              \
                else if(data[ii]>max){ max=data[ii]; maxp=ii; }         \
        } while (0)

/* do not assume the dataset is of type MRI_float   4 Aug 2006 [rickr] */
static int get_blk_min_max_posn(THD_datablock * blk, int ind, int len,
                     float * fmin, int * imin, float * fmax, int * imax)
{
    float ffac = DBLK_BRICK_FACTOR(blk,ind);

ENTRY("get_blk_min_max_posn");

    if( ffac == 0.0 ) ffac = 1.0;

    switch(DBLK_BRICK_TYPE(blk, ind)){
        default:{
            fprintf(stderr,"** GBMMP, bad dtype\n");
            break;
            *fmin = *fmax = 0.0;  *imin = *imax = 0;
        }
        case MRI_byte:
        {
            byte * data = DBLK_ARRAY(blk,ind);
            byte   min, max;
            int    minp, maxp;
            NOTYPE_GET_MIN_MAX_POSN(data,len,min,minp,max,maxp);
            *fmin = min*ffac;  *fmax = max*ffac;
            *imin = minp;  *imax = maxp;
            break;
        }
        case MRI_short:
        {
            short * data = DBLK_ARRAY(blk,ind);
            short   min, max;
            int     minp, maxp;
            NOTYPE_GET_MIN_MAX_POSN(data,len,min,minp,max,maxp);
            *fmin = min*ffac;  *fmax = max*ffac;
            *imin = minp;  *imax = maxp;
            break;
        }
        case MRI_int:
        {
            int * data = DBLK_ARRAY(blk,ind);
            int   min, max;
            int   minp, maxp;
            NOTYPE_GET_MIN_MAX_POSN(data,len,min,minp,max,maxp);
            *fmin = min*ffac;  *fmax = max*ffac;
            *imin = minp;  *imax = maxp;
            break;
        }
        case MRI_float:
        {
            float * data = DBLK_ARRAY(blk,ind);
            float   min, max;
            int     minp, maxp;
            NOTYPE_GET_MIN_MAX_POSN(data,len,min,minp,max,maxp);
            *fmin = min*ffac;  *fmax = max*ffac;
            *imin = minp;  *imax = maxp;
            break;
        }
        case MRI_double:
        {
            double * data = DBLK_ARRAY(blk,ind);
            double   min, max;
            int      minp, maxp;
            NOTYPE_GET_MIN_MAX_POSN(data,len,min,minp,max,maxp);
            *fmin = min*ffac;  *fmax = max*ffac;
            *imin = minp;  *imax = maxp;
            break;
        }
    }

    RETURN(0);
}

/*----------------------------------------------------------------------*/
/*! Open a NIML file as an AFNI dataset.

    - read as niml
 *----------------------------------------------------------------------
*/
THD_3dim_dataset * THD_open_niml( char * fname )
{
    THD_3dim_dataset * dset = NULL;
    void             * nel;
    int                smode;

ENTRY("THD_open_niml");

    set_ni_globs_from_env();   /* 3 Aug 2006 [rickr] */

    nel = read_niml_file(fname, 1);  /* we need data for node_indices */
    if( !nel ) RETURN(NULL);

    smode = storage_mode_from_niml(nel);
    switch( smode )
    {
        case STORAGE_BY_3D:
            NI_free_element_data(nel);  /* nuke all data */
            dset = THD_niml_3D_to_dataset(nel, fname);
            if(gni.debug) fprintf(stderr,"-d opening 3D dataset '%s'\n",fname);
            if( !dset && gni.debug )
                fprintf(stderr,
                        "** THD_niml_3D_to_dataset failed on '%s'\n",fname);
        break;

        case STORAGE_BY_NIML:
            NI_free_element_data(nel);  /* nuke all data */
            if(gni.debug)fprintf(stderr,"-d opening NIML dataset '%s'\n",fname);
            dset = THD_niml_to_dataset(nel, 1); /* no data */
            if( !dset && gni.debug )
                fprintf(stderr,"** THD_niml_to_dataset failed on '%s'\n",fname);
        break;

        case STORAGE_BY_NI_SURF_DSET:
            if(gni.debug)fprintf(stderr,"-d opening NI_SURF_DSET '%s'\n",fname);
            dset = THD_ni_surf_dset_to_afni(nel, 0); /* no data */
        break;

        default:
            if( gni.debug )
                fprintf(stderr,"** unknown storage mode for '%s'\n", fname);
        break;
    }

    NI_free_element(nel);

    if( dset )
    {   
        char * pp = THD_trailname(fname, 0);
        EDIT_dset_items(dset, ADN_prefix, pp, ADN_none);
        NI_strncpy(dset->dblk->diskptr->brick_name, fname, THD_MAX_NAME);
        THD_set_storage_mode(dset, smode);
        if(gni.debug > 1) fprintf(stderr,"+d success for dataset '%s'\n",fname);
    }

    RETURN(dset);
}


/*----------------------------------------------------------------------*/
/*! Open a NIML file as an AFNI dataset.

    - read as niml
 *----------------------------------------------------------------------
*/
int THD_load_niml( THD_datablock * dblk )
{
    void   * nel;
    char   * fname;
    int      smode, rv;

ENTRY("THD_load_niml");

    if( !dblk || !dblk->diskptr || !dblk->diskptr->brick_name )
        RETURN(1);

    fname = dblk->diskptr->brick_name;
    smode = dblk->diskptr->storage_mode;

    if( gni.debug > 1 )
        fprintf(stderr,"-d THD_load_niml: file %s, smode %d\n", fname, smode);

    switch( smode )
    {
        case STORAGE_BY_3D:
            if(gni.debug) fprintf(stderr,"-d loading 3D dataset '%s'\n",fname);
            THD_load_3D(dblk);
            break;
        case STORAGE_BY_NIML:
            if(gni.debug)fprintf(stderr,"-d loading NIML dataset '%s'\n",fname);
            nel = read_niml_file(fname, 1);  /* read in data now */
            if( !nel ){
                fprintf(stderr,"** failed to load niml file '%s'\n",fname);
                RETURN(1);
            }
            rv = THD_add_bricks(dblk->parent, nel);
            NI_free_element(nel);  /* in any case */
            if( rv <= 0 ){
                fprintf(stderr,"** add bricks returned %d for '%s'\n",rv,fname);
                RETURN(1);
            }
            else if( rv < dblk->nvals ){
                fprintf(stderr,"** loaded only %d bricks for '%s'\n",rv,fname);
                RETURN(1);
            }
            break;
        case STORAGE_BY_NI_SURF_DSET:
            if(gni.debug)fprintf(stderr,"-d loading NI_SURF_DSET '%s'\n",fname);
            nel = read_niml_file(fname, 1);  /* read in data now */
            if( !nel ){
                fprintf(stderr,"** failed to load NI_SURF_DSET '%s'\n",fname);
                RETURN(1);
            }
            rv = THD_add_sparse_data(dblk->parent, nel);
            NI_free_element(nel);  /* in any case */
            if( rv <= 0 ){
                fprintf(stderr,"** add sdata returned %d for '%s'\n",rv,fname);
                RETURN(1);
            }
            else if( rv < dblk->nvals ){
                fprintf(stderr,"** loaded only %d vols for '%s'\n",rv,fname);
                RETURN(1);
            }
            break;
        default:
            fprintf(stderr,"** cannot load NIML dataset '%s' of mode %d\n",
                    fname, smode);
            break;
    }

    RETURN(0);
}


/*----------------------------------------------------------------------*/
/*! try to deduce the STORAGE mode from the niml data

    tested modes are:
        STORAGE_BY_3D
                element, AFNI_3D_dataset
        STORAGE_BY_NIML
                group, AFNI_dataset
                THD_niml_to_dataset()
                ngr->part[i]->name == "VOLUME_DATA"
        STORAGE_BY_NI_SURF_DSET
  ----------------------------------------------------------------------*/
int storage_mode_from_niml( void * nini )
{
    int ni_type;

ENTRY("storage_mode_from_niml");

    ni_type = NI_element_type( nini );

    if( ni_type == NI_ELEMENT_TYPE )                /* can only be 3D */
    {
        NI_element * nel = (NI_element *)nini;
        if( ! strcmp(nel->name, "AFNI_3D_dataset") )
            RETURN(STORAGE_BY_3D);

        /* cannot deal with simple niml "3dVol2Surf_dataset", yet */

        if(gni.debug)
            fprintf(stderr,"** SMFN: unknown NI_element %s\n", nel->name);
    }
    else if( ni_type == NI_GROUP_TYPE )             /* AFNI or SUMA */
    {
        NI_group * ng = (NI_group *)nini;
        char     * atr;
        if( ! strcmp(ng->name, "AFNI_dataset") )
        {
            atr = NI_get_attribute(ng, "dset_type");
            if( atr && 
                ( !strcmp(atr, "Node_Bucket") ||
                  !strcmp(atr, "Node_ROI")      )   ) /* then SUMA DSET */
                RETURN(STORAGE_BY_NI_SURF_DSET);
            RETURN(STORAGE_BY_NIML);                 /* else assume AFNI */
        }
        else if(gni.debug)
            fprintf(stderr,"** SMFN: NI_group, but bad name '%s'\n",ng->name);
    }
    else if(gni.debug) fprintf(stderr,"** SMFN: bad ni_type %d\n",ni_type);

    RETURN(STORAGE_UNDEFINED);
}

#undef  MY_BUFSIZE
#define MY_BUFSIZE (1024*1024)  /* 21 Nov 2007 */

/*! inhale any NIML data within a file */
void * read_niml_file( char * fname, int get_data )
{
    NI_stream    ns;
    NI_element * nel;
    char       * nname;

ENTRY("read_niml_file");

    if( !fname || !*fname )
    {
        if(gni.debug) fprintf(stderr,"** read_niml_file: empty filename\n");
        RETURN(NULL);
    }

    /* set the stream name */
    nname = (char *)calloc(sizeof(char), strlen(fname)+10);
    strcpy(nname, "file:");
    strcat(nname, fname);

    /* open the stream */
    ns = NI_stream_open(nname, "r");
    free(nname);
    if( !ns )
    {
        if(gni.debug)fprintf(stderr,"** RNF: failed to open file '%s'\n",fname);
        RETURN(NULL);
    }

    if( get_data && NI_stream_getbufsize(ns) < MY_BUFSIZE ) /* 21 Nov 2007: RWCox */
      NI_stream_setbufsize(ns,MY_BUFSIZE) ;

    /* read the file */
    NI_skip_procins(1);  NI_read_header_only(!get_data);
    nel = NI_read_element(ns, 333);
    NI_skip_procins(0);  NI_read_header_only(get_data);

    /* close the stream */
    NI_stream_close(ns);

    /* possibly check the results */
    if(gni.debug && !nel) fprintf(stderr,"** RNF: failed to read '%s'\n",fname);
    else if(gni.debug>1)  fprintf(stderr,"+d success for niml file %s\n",fname);

    RETURN(nel);
}


/*! write NIML data to a file */
int write_niml_file( char * fname, NI_group * ngr )
{
    NI_stream   ns;
    char      * str_name;

ENTRY("write_niml_file");

    if( !fname || !ngr ){
        fprintf(stderr,"** write_niml_file: empty parameters\n");
        RETURN(1);
    }

    /* set the output stream name (5 for 'file:' and 1 for '\0') */
    str_name = (char *)malloc((strlen(fname)+6) * sizeof(char) );
    strcpy(str_name, "file:");
    strcat(str_name, fname);

    ns = NI_stream_open(str_name, "w");
    free(str_name); /* lose this either way */

    if( !ns ){
        fprintf(stderr,"** cannot open NIML stream for file '%s'\n", fname);
        RETURN(1);
    }

    if( NI_write_element( ns, ngr, NI_TEXT_MODE ) <= 0 ){
        fprintf(stderr,"** failed to write NIML output file '%s'\n", fname);
        RETURN(1);
    }

    NI_stream_close(ns); /* close the stream */

    RETURN(0);
}


/*! Write out a NIML dataset (3D, NIML, NI_SURF_DSET).
    Return True or False, based on success.
*/
Boolean THD_write_niml( THD_3dim_dataset * dset, int write_data )
{
    NI_group * ngr;
    char     * prefix;
    int        smode, rv;
ENTRY("THD_write_niml");

    set_ni_globs_from_env();
    prefix   = DSET_PREFIX(dset);

    if( !prefix ) {
        if(gni.debug) fprintf(stderr,"** THD_write_niml: no dset prefix\n");
        RETURN(False);
    }

    smode = storage_mode_from_filename(prefix);
    if( gni.debug )
        fprintf(stderr,"-d THD_write_niml: file %s, smode %d\n", prefix, smode);
        
    switch(smode)
    {
        case STORAGE_BY_3D:
            THD_write_3D(NULL, NULL, dset);
            break;

        case STORAGE_BY_NIML:
            if( write_data ) ngr = THD_dataset_to_niml(dset);
            else             ngr = THD_nimlize_dsetatr(dset);
            if( !ngr ){
                fprintf(stderr,"** failed dset to niml on '%s'\n",prefix);
                RETURN(False);
            }
            NI_rename_group(ngr, "AFNI_dataset");
            NI_set_attribute(ngr, "self_prefix", prefix);
            rv = write_niml_file(prefix, ngr);
            NI_free_element(ngr); /* either way */
            if( rv ){
                fprintf(stderr,"** write_niml_file failed for '%s'\n",prefix);
                RETURN(False);
            }
            break;

        case STORAGE_BY_NI_SURF_DSET:
            ngr = THD_dset_to_ni_surf_dset(dset, write_data);
            if( !ngr )
            {
                fprintf(stderr,"** failed dset to ni_SD on '%s'\n",prefix);
                RETURN(False);
            }
            rv = write_niml_file(prefix, ngr);
            NI_free_element(ngr); /* either way */
            if( rv ){
                fprintf(stderr,"** write_niml_file failed for '%s'\n",prefix);
                RETURN(False);
            }
            break;

        default:
            fprintf(stderr,"** invalid storage mode %d to write '%s'\n",
                    smode, prefix);
            RETURN(False);
            break;
    }

    RETURN(True);
}


/*! Convert a string attribute (in NI_SURF_DSET form) to a list of
    strings.  Each element of the list will be allocated here.

    The NI_SURF_DSET form for strings uses ';' to separate them.

        slist           pointer to string list - will be allocated
        llen            length of list to be set
        atr             string attribute to get list from
                        (if NULL, strings will get default "#%d")

    return the number of strings found
*/
static int nsd_string_atr_to_slist(char *** slist, int llen, ATR_string * atr)
{
    int sind, posn, prev, copy_len;
    int found = 0;

ENTRY("nsd_string_atr_to_slist");

    if(!slist || llen < 1)
    {
        fprintf(stderr,"** NSATS: bad params\n");
        RETURN(0);
    }

    if( !atr )      /* we're outta here */
    {
        *slist = NULL;
        if(gni.debug > 1) fprintf(stderr,"NSATS: no attribute to parse\n");
        RETURN(0); 
    }

    if(gni.debug > 1)
    {
        if( atr ) fprintf(stderr,"+d getting string attrs from %s\n",atr->name);
        else      fprintf(stderr,"+d setting default strings\n");
    }

    /* allocate memory for the list */
    *slist = (char **)malloc(llen * sizeof(char *));

    posn = -1;
    for( sind = 0; sind < llen && posn < atr->nch; sind++ )
    {
        /* find end of next string (end with nul or ';') */
        prev = posn;
        for( posn = prev+1;
             posn < atr->nch && atr->ch[posn] && atr->ch[posn] != ';' ;
             posn++ )
            ;  /* just search */

        if( posn > prev+1 ) /* then we have found some */
        {
            copy_len = posn - prev - 1;
            if( copy_len > THD_MAX_LABEL-1 ) copy_len = THD_MAX_LABEL-1;
            (*slist)[sind] = my_strndup(atr->ch+prev+1, copy_len);
            found++;

            if(gni.debug>1)
                fprintf(stderr,"-d string %d = %s\n",sind,(*slist)[sind]);
        }
        else
        {
            (*slist)[sind] = (char *)malloc(10 * sizeof(char));
            sprintf((*slist)[sind], "#%d", sind);
        }
    }

    for( ; sind < llen; sind++ )
    {
        (*slist)[sind] = (char *)malloc(10 * sizeof(char));
        sprintf((*slist)[sind], "#%d", sind);
    }

    if(gni.debug>1) fprintf(stderr,"-d found %d of %d strings\n", found, llen);

    RETURN(found);
}

/*! create an AFNI dataset from a NI_SURF_DSET group */
THD_3dim_dataset * THD_ni_surf_dset_to_afni(NI_group * ngr, int read_data)
{
    THD_3dim_dataset * dset = NULL;
    int                rv;

ENTRY("THD_ni_surf_dset_to_afni");

    if( !ngr || NI_element_type(ngr) != NI_GROUP_TYPE ) RETURN(NULL);

    dset = EDIT_empty_copy(NULL);

    THD_dblkatr_from_niml(ngr, dset->dblk); /* store NIML attributes in dblk */

    rv = process_NSD_index_list(ngr, dset->dblk);      /* INDEX_LIST attr   */
    if( !rv ) rv = process_NSD_sparse_data(ngr, dset); /* SPARSE_DATA attr  */
    if( !rv ) rv = process_NSD_group_attrs(ngr, dset); /* store group attrs */
    if( !rv ) rv = process_NSD_attrs(dset);            /* apply other attrs */

    RETURN(dset);
}

/* process any INDEX_LIST attribute, setting nnodes and node_list in
   the datablock (going from NIML to AFNI)
*/
static int process_NSD_index_list(NI_group * ngr, THD_datablock * dblk )
{
    NI_element  * nel = NULL;
    void       ** elist = NULL;
    int           byte_order, nvals;

ENTRY("process_NSD_index_list");

    if( !ngr || !ISVALID_DBLK(dblk) )
    {
        if(gni.debug) fprintf(stderr,"** PNSDIL: bad params\n");
        RETURN(1);
    }

    /* grab the any INDEX_LIST element (should be only one) */
    nvals = NI_search_group_shallow(ngr, "INDEX_LIST", &elist);
    if( nvals <= 0 )
    {
        if( gni.debug ) fprintf(stderr,"-d no INDEX_LIST element\n");
        RETURN(0);
    }

    nel = (NI_element *)elist[0];       /* grab first element (only!?)  */
    NI_free(elist);                     /* and we're done with the list */

    /* make sure this is a single array of ints */
    if( !nel || nel->vec_num != 1 || nel->vec_len <= 0 )
    {
        if(gni.debug) fprintf(stderr,"** malformed INDEX_LIST element\n");
        RETURN(1);
    }

    if( nel->vec_typ[0] != NI_INT )
    {
        if(gni.debug)
            fprintf(stderr,"** INDEX_LIST has bad type %d\n",nel->vec_typ[0]);
        RETURN(1);
    }

    /* note the byte order, in case we need to swap the index bytes */
    byte_order = NI_get_byte_order(nel);
    if( byte_order == NATIVE_ORDER ) byte_order = mri_short_order();

    /* and copy the node list */
    dblk->nnodes = nel->vec_len;
    dblk->node_list = (int *)XtMalloc(dblk->nnodes * sizeof(int));
    memcpy(dblk->node_list, nel->vec[0], dblk->nnodes*sizeof(int));
    if( byte_order != mri_short_order() )
    {
        if(gni.debug > 1) fprintf(stderr,"+d swapping bytes in node list\n");
        nifti_swap_4bytes(dblk->nnodes, dblk->node_list);
    }

    if(gni.debug) fprintf(stderr,"+d have node list of len, %d\n",dblk->nnodes);

    RETURN(0);
}

/* initialize the datablock using the SPARSE_DATA NI_SURF_DSET element
   and possibly the INDEX_LIST element

   (going from NIML to AFNI)
*/
static int process_NSD_sparse_data(NI_group * ngr, THD_3dim_dataset * dset )
{
    THD_datablock * blk;
    THD_diskptr   * dkptr;
    THD_ivec3       nxyz;
    NI_element    * nel = NULL;
    void         ** elist = NULL;
    float           tr;
    char          * rhs;
    int             ind;

ENTRY("process_NSD_sparse_data");

    if( !ngr || !ISVALID_DSET(dset) )
    {
        if(gni.debug) fprintf(stderr,"** PNSDSD: bad params\n");
        RETURN(1);
    }
    blk   = dset->dblk;
    dkptr = blk->diskptr;

    /* grab the first SPARSE_DATA element (should be only) */
    ind = NI_search_group_shallow(ngr, "SPARSE_DATA", &elist);
    if( ind > 0 ) { nel = (NI_element *)elist[0]; NI_free(elist); }

    if(!nel || nel->vec_num <= 0 || nel->vec_len <= 0)
    {
        if(gni.debug) fprintf(stderr,"** missing SPARSE_DATA element\n");
        RETURN(1);
    }

    /* so nel points to the SPARSE_DATA element */

    if(gni.debug>1)fprintf(stderr,"-d found SPARSE_DATA in NI_SURF_DSET\n");

    /* if we have use ni_form to set the byte_order */
    dkptr->byte_order = NI_get_byte_order(nel);
    if( dkptr->byte_order == NATIVE_ORDER )
        dkptr->byte_order = mri_short_order();

    if(gni.debug>1)
        fprintf(stderr,"+d using byte order %s\n",
                BYTE_ORDER_STRING(dkptr->byte_order));

    /* verify that we have "data_type=Node_Bucket_data"
       acceptable should also be Node_ROI_data but on 
       output, all will become Node_Bucket_data ZSS: Dec 07  */
    rhs = NI_get_attribute(nel, "data_type");
    if( !rhs || 
         (  strcmp(rhs, "Node_Bucket_data") &&
            strcmp(rhs, "Node_ROI_data")       ) )
    {
        if(gni.debug)
          fprintf(stderr,"** SPARSE_DATA without data_type "
                         "Node_Bucket_data or Node_ROI_data\n");
        RETURN(1);
    }

    /* if we have a node list, verify that it matches the data in length */
    if( blk->nnodes > 0 && (blk->nnodes != nel->vec_len) )
    {
        if( blk->nnodes != nel->vec_len )
        {
            fprintf(stderr,"** node list len (%d) != data len (%d)\n",
                blk->nnodes, nel->vec_len);
            RETURN(1);
        }
        if(gni.debug > 1) fprintf(stderr,"-d length of nodes and data match\n");
    }

    /* node index list is now in INDEX_LIST attribute  29 Aug 2006 [rickr] */
    for( ind = 0; ind < nel->vec_num; ind++ )
        if( nel->vec_typ[ind] != NI_FLOAT &&
            nel->vec_typ[ind] != NI_INT )
        {
            if(gni.debug)
                fprintf(stderr,"** NI_SURF_DSET has non-float type %d\n",
                        nel->vec_typ[ind]);
            RETURN(1);
        }

    /* set nxyz */
    nxyz.ijk[0] = nel->vec_len;   nxyz.ijk[1] = nxyz.ijk[2] = 1;

    if(gni.debug > 1)
        fprintf(stderr,"+d setting datum, nxyz, nx to float, %d, %d\n",
                nel->vec_len, nel->vec_num);

    EDIT_dset_items(dset,
                        ADN_datum_all,  MRI_float,
                        ADN_nxyz,       nxyz,
                        ADN_nvals,      nel->vec_num,
                     ADN_none );

    /*--- check for a ni_timestep attribute ---*/
    rhs = NI_get_attribute(nel, "ni_timestep");
    if( rhs && nel->vec_num > 1 )  /* then make time dependant */
    {
        tr = strtod(rhs, NULL);
        if(gni.debug > 1) fprintf(stderr,"-d found TR = %f\n", tr);
        if( tr <= 0.0 ) tr = 1.0;   /* just be safe */
        EDIT_dset_items(dset,
                            ADN_func_type, ANAT_EPI_TYPE,
                            ADN_ntt      , nel->vec_num,
                            ADN_ttdel    , tr,
                            ADN_tunits   , UNITS_SEC_TYPE,
                        ADN_none);
    }

    RETURN(0);
}

/* apply known NI_SURF_DSET attributes  (niml -> afni) */
static int process_NSD_attrs(THD_3dim_dataset * dset)
{
    THD_datablock * blk;
    THD_diskptr   * dkptr;
    THD_ivec3       ori;
    THD_fvec3       del, org;
    ATR_string    * atr_str;
    int             ind, nvals;

ENTRY("process_NSD_attrs");

    /* orientation, grid and origin are meaningless, but apply defaults */

    /* set orientation as RAI */
    ori.ijk[0] = ORI_R2L_TYPE;
    ori.ijk[1] = ORI_A2P_TYPE;
    ori.ijk[2] = ORI_I2S_TYPE;

    /* set grid spacings to 1 mm, and origin to 0.0 */
    del.xyz[0] = del.xyz[1] = del.xyz[2] = 1.0;
    org.xyz[0] = org.xyz[1] = org.xyz[2] = 0.0;

    blk = dset->dblk;
    dkptr = blk->diskptr;

    EDIT_dset_items(dset,
                        ADN_xyzdel,      del,
                        ADN_xyzorg,      org,
                        ADN_xyzorient,   ori,
                        ADN_malloc_type, DATABLOCK_MEM_MALLOC,
                        ADN_type,        HEAD_ANAT_TYPE,
                    ADN_none);

    /* if not time dependant, set as bucket */
    if( ! dset->taxis || dset->taxis->ntt <= 1)
        EDIT_dset_items(dset, ADN_func_type,  ANAT_BUCK_TYPE, ADN_none);

    dkptr->storage_mode = STORAGE_BY_NI_SURF_DSET;

    /* now process some attributes */

    nvals = blk->nvals;

    /*--- init and fill any column labels ---*/
    atr_str = THD_find_string_atr(blk, "COLMS_LABS");
    if( !atr_str ) atr_str = THD_find_string_atr(blk, ATRNAME_BRICK_LABS);
    nsd_string_atr_to_slist(&blk->brick_lab, nvals, atr_str);

    /*--- init and fill any statistic symbols ---*/
    atr_str = THD_find_string_atr(blk , "COLMS_STATSYM");
    if( !atr_str ) atr_str = THD_find_string_atr(blk , "BRICK_STATSYM");
    if(  atr_str ) /* only do this if we have some codes */
    {
        char **sar ; int scode,np ; float parm[3];
        np = nsd_string_atr_to_slist(&sar, nvals, atr_str);
        if( sar )
        {
            for( ind = 0; ind < nvals; ind++ )
            {
                NI_stat_decode(sar[ind], &scode, parm,parm+1,parm+2 );
                if(scode >= AFNI_FIRST_STATCODE && scode <= AFNI_LAST_STATCODE)
                {
                    np = NI_stat_numparam(scode);
                    THD_store_datablock_stataux(blk, ind, scode, np, parm);
                }
                free(sar[ind]);
            }
            free(sar);
        }
    }

    RETURN(0);
}

/* store any attribute in the ni_surf_dset_attrs[] list

    - for each attr, if it has a valid rhs, store it
    - the self_idcode should be stored separately (as it
        will not be copied to a new dataset)

    return 0 on success
*/
static int process_NSD_group_attrs(NI_group * ngr, THD_3dim_dataset * dset )
{
    char       ** aname;
    char        * rhs;
    int         ac, natr;

ENTRY("process_NSD_group_attrs");

    natr = sizeof(ni_surf_dset_attrs) / sizeof(char *);

    for( ac = 0, aname = ni_surf_dset_attrs; ac < natr; ac++, aname++ )
    {
        rhs = NI_get_attribute(ngr, *aname);
        if( rhs && *rhs )
        {
            if(gni.debug>1)
                fprintf(stderr,"-d found group attr %s = %s\n",*aname,rhs);
            THD_set_string_atr(dset->dblk, *aname, rhs);
        }
        else if(gni.debug>2)
                fprintf(stderr,"-d did not find group attr %s\n",*aname);
    }

    /* idcode: from nel:self_idcode or ni_idcode */
    rhs = NI_get_attribute(ngr, "self_idcode");
    if( !rhs ) rhs = NI_get_attribute(ngr, "ni_idcode");
    if(  rhs ) NI_strncpy(dset->idcode.str, rhs, MCW_IDSIZE);
    /* else, keep the one from EDIT_empty_copy() */
    
    RETURN(0);
}

/*------------------------------------------------------------------------*/
/*! Load data from the SPARSE_DATA NIML element.          3 Jul 2006 [rickr]
 *  (adding it to the dataset block)
 *
 *  - Return value is the number of sub-bricks found.
 *  - Data must be of type float.
 *  - Free NIML data as it is applied.
 *------------------------------------------------------------------------*/
int THD_add_sparse_data(THD_3dim_dataset * dset, NI_group * ngr )
{
    THD_datablock  * blk;
    NI_element     * nel = NULL;
    float          * data;
    void          ** elist = NULL;
    int              nvals, ind, mind, sub, swap, len;
    int            * mlist = NULL;  /* master list */

ENTRY("THD_add_sparse_data");

    if( !dset || !ngr ) RETURN(0);
    blk = dset->dblk;
    nvals = blk->nvals;

    ind = NI_search_group_shallow(ngr, "SPARSE_DATA", &elist);
    if( ind > 0 ) { nel = (NI_element *)elist[0]; NI_free(elist); }
    if( !nel ) RETURN(0);

    /* if mlist is NULL, no mastery */
    if( DBLK_IS_MASTERED(blk) ) mlist = blk->master_ival;

    /*-- verify sizes --*/
    if( nel->vec_num != nvals )
    {
        if(gni.debug)
        {
            fprintf(stderr,"** TASD: vec_num = %d, but nvals = %d\n",
                    nel->vec_num, nvals);
            if( mlist ) fprintf(stderr,"   (dataset is mastered)\n");
        }
        if( !mlist ) RETURN(0); /* no mastery means failure here */
    }
    if( nel->vec_len != DSET_NX(dset) )
    {
        if(gni.debug) fprintf(stderr,"** TASD: vec_len = %d, but NX = %d\n",
                                    nel->vec_len, DSET_NX(dset));
        RETURN(0);
    }

    /*-- verify types --*/
    for( ind = 0; ind < nvals; ind++ )
    {
        mind = mlist ? mlist[ind] : ind;  /* maybe use master index */
        if( nel->vec_typ[mind] != NI_FLOAT &&
            nel->vec_typ[mind] != NI_INT )
        {
            if(gni.debug) fprintf(stderr,"** TASD: vec[%d] not float\n",mind);
            RETURN(0);
        }
        else if( ! nel->vec[mind] )
        {
            if(gni.debug) fprintf(stderr,"** TASD: vec[%d] not filled!\n",mind);
            RETURN(0);
        }
    }

    /* check for necessary swapping */
    swap = (blk->diskptr->byte_order != mri_short_order());
    if(gni.debug>1 && swap) fprintf(stderr,"+d will byte_swap data\n");
    len = nel->vec_len;

    /*-- we seem to have all of the data, now copy it --*/
    sub = 0;
    for( ind = 0; ind < nvals; ind++ )
    {
        mind = mlist ? mlist[ind] : ind;  /* maybe use master index */
        data = (float *)XtMalloc(len * sizeof(float));
        if(!data){
           fprintf(stderr,"**ASD alloc fail: %d bytes\n",len);
           RETURN(0);
        }
        if( nel->vec_typ[mind] == NI_FLOAT ) {
           memcpy(data, nel->vec[mind], len * sizeof(float));
           if( swap ) nifti_swap_4bytes(len, data);
        } else if( nel->vec_typ[mind] == NI_INT ) {/* ZSS: Dec. 07. Note that*/
           int *idata=NULL, ii=0;                  /* int dsets become floats*/ 
           idata = (int *)XtMalloc(len * sizeof(int));
           if(!idata){
              fprintf(stderr,"**ASD alloc fail: %d bytes\n",len);
              RETURN(0);
           }
           memcpy(idata, nel->vec[mind], len * sizeof(int));
           if( swap ) nifti_swap_4bytes(len, idata);
           for(ii=0; ii<len; ++ii) data[ii] = (float)idata[ii];
           XtFree((char*)idata); idata=NULL;
        } else {
           fprintf(stderr,"**ASD should never have been here.\n");
           RETURN(0);
        }
        mri_fix_data_pointer(data, DBLK_BRICK(blk,sub));
        sub++;

        /* we can only nuke the old stuff if we know we're done with it */
        if( !mlist ){ NI_free(nel->vec[mind]);  nel->vec[mind] = NULL; }
    }

    if( DBLK_IS_MASTERED(blk) && blk->master_bot <= blk->master_top )
        THD_apply_master_subrange(blk);

    RETURN(nvals);
}

/*! convert an AFNI dataset to a NIML group of type NI_SURF_DSET

    - set group attributes from ni_surf_dset_attrs
    - create SPARSE_DATA element, if requested
    - create attributes for COLMS_RANGE, COLMS_LABS, COLMS_TYPE, COLMS_STATSYM
    - apply HISTORY_NOTE
    
*/
NI_group * THD_dset_to_ni_surf_dset( THD_3dim_dataset * dset, int write_data )
{
    THD_datablock * blk;
    NI_group      * ngr;
    int             nx;

ENTRY("THD_dset_to_ni_surf_dset");

    if( !ISVALID_DSET(dset) ) RETURN(NULL);
    blk = dset->dblk;
    if( !blk ) RETURN(NULL);
    nx = DSET_NX(dset);

    if( blk->nnodes > 0 && blk->nnodes != nx ) {
        fprintf(stderr,"** datablock nnodes differs from nx %d, %d\n",
                blk->nnodes, nx);
        RETURN(NULL);
    } else if ( blk->nnodes > 0 && !blk->node_list ) {
        fprintf(stderr,"** datablock has nnodes but no node_list\n");
        RETURN(NULL);
    }

    THD_set_dataset_attributes(dset);  /* load attributes for processing */

    /* create group element */
    /* All gets forced into Node_Bucket for now */
    ngr = NI_new_group_element();
    NI_rename_group(ngr, "AFNI_dataset");
    NI_set_attribute(ngr, "dset_type", "Node_Bucket");
    NI_set_attribute(ngr, "self_idcode", dset->idcode.str);
    NI_set_attribute(ngr, "filename", blk->diskptr->brick_name);

    nsd_add_str_atr_to_group("BRICK_LABS", "COLMS_LABS", blk, ngr);
    nsd_add_colms_range(ngr, dset);
    nsd_add_colms_type(blk, ngr);
    nsd_add_str_atr_to_group("BRICK_STATSYM", "COLMS_STATSYM", blk, ngr);
    nsd_add_str_atr_to_group("HISTORY_NOTE", NULL, blk, ngr);
    nsd_fill_index_list(ngr, dset);                  /* add INDEX_LIST */
    if( write_data ) nsd_add_sparse_data(ngr, dset); /* add SPARSE_DATA */

    RETURN(ngr);
}


/*! add a COLMS_TYPE attribute element

   return 0 on success
*/
static int nsd_add_colms_type(THD_datablock * blk, NI_group * ngr)
{
    NI_element * nel;
    char       * str, * slist[1];  /* add_column requires a list of strings */
    int          c, plen;

ENTRY("nsd_add_colms_type");

    /* check usage */
    if( !blk || !ngr ) RETURN(1);

    /* create a new string: "Generic_Float;Generic_Float;..." */
    plen = 14*blk->nvals + 1;
    str = (char *)malloc(plen * sizeof(char));

    /* insert first string */
    strcpy(str, "Generic_Float");

    /* and then the rest */
    for( c = 1; c < blk->nvals; c++ )
        strcat(str, ";Generic_Float");

    /* now add it to the group */
    slist[0] = str;
    nel = NI_new_data_element("AFNI_atr", 1);
    nel->outmode = NI_TEXT_MODE;
    NI_set_attribute(nel, "atr_name", "COLMS_TYPE");
    NI_add_column(nel, NI_STRING, slist);
    NI_add_to_group(ngr, nel);

    free(str); /* nuke local copy */

    RETURN(0);
}


/* - find the given attribute in the datablock
   - now just add 1 to length (should we bother?)
   - put it in a data element
   - add it to the group

   aname  - AFNI attribute name
   niname - NIML attribute name (if NULL, use aname)
   blk    - datablock
   ngr    - NI_group to insert new element into

   return 0 on success
*/
static int nsd_add_str_atr_to_group(char * aname, char * niname,
                                    THD_datablock * blk, NI_group * ngr)
{
    ATR_string * atr;
    NI_element * nel;
    char       * dest;

ENTRY("nsd_add_str_atr_to_group");

    /* check usage */
    if( !aname || !blk || !ngr ) RETURN(1);

    atr = THD_find_string_atr(blk, aname);
    if( !atr ) RETURN(0);  /* nothing to add */

    if(gni.debug > 1){
        fprintf(stderr, "-d adding '%s' atr: ", niname?niname:aname);
        fwrite(atr->ch, sizeof(char), atr->nch, stderr);
        fputc('\n', stderr);
    }

    /* create a new string */
    dest = (char *)calloc(atr->nch+1, sizeof(char)); /* +1 for last '\0' */
    memcpy(dest, atr->ch, atr->nch);
    THD_zblock_ch(atr->nch, dest, ZSBLOCK);  /* swap out nul chars */
    dest[atr->nch] = '\0';

    /* now add it to the group */
    nel = NI_new_data_element("AFNI_atr", 1);
    nel->outmode = NI_TEXT_MODE;
    NI_set_attribute(nel, "atr_name", niname ? niname : aname);
    NI_add_column(nel, NI_STRING, &dest);
    NI_add_to_group(ngr, nel);

    if(gni.debug > 1) fprintf(stderr, "-d new atr is: '%s' ", dest);

    free(dest); /* nuke local copy */

    RETURN(0);
}


/* add a COLMS_RANGE attribute element to the group 
 * 
 * do not assume that the data is of type float, though
 * evaluate ranges as if it is
 */
static int nsd_add_colms_range(NI_group * ngr, THD_3dim_dataset * dset)
{
    THD_datablock * blk;
    NI_element    * nel;
    float           fmin, fmax;
    char          * str;
    int             ind, nx;
    int             len;  /* dynamic length of str */
    int             minp, maxp;

ENTRY("nsd_add_colms_range");

    nx = DSET_NX(dset);
    blk = dset->dblk;

    /*-- create the string --*/
    len = 512;
    str = (char *)malloc(len * sizeof(char));
    str[0] = '\0';

    /* stick the nodes in the list */
    get_blk_min_max_posn(blk, 0, nx, &fmin, &minp, &fmax, &maxp);
    loc_append_vals(&str, &len, "", fmin, fmax, minp, maxp);

    for( ind = 1; ind < blk->nvals; ind++ )  /* keep appending the next set */
    {
        get_blk_min_max_posn(blk, ind, nx, &fmin, &minp, &fmax, &maxp);
        loc_append_vals(&str, &len, ";", fmin, fmax, minp, maxp);
    }

    /*-- now we have the string, insert it as an attribute element --*/

    /* create initial element */
    nel = NI_new_data_element("AFNI_atr", 1);

    nel = NI_new_data_element("AFNI_atr", 1);
    nel->outmode = NI_TEXT_MODE;
    NI_set_attribute(nel, "atr_name", "COLMS_RANGE");
    NI_add_column(nel, NI_STRING, &str);
    NI_add_to_group(ngr, nel);

    if(gni.debug > 1) fprintf(stderr,"+d added COLMS_RANGE atr: '%s'\n", str);

    free(str); /* nuke allocated string */

    RETURN(0);
}


/*------------------------------------------------------------------------*/
/*! Add the INDEX_LIST attribute element from the AFNI dset to the
    NIML group.
 *
 *  If dset has no node_list, the user may still want it filled to a
 *    default list, based on AFNI_NSD_ADD_NODES (gni.add_nodes).
 *  If the datum is not float, convert it.
 * -----------------------------------------------------------------------*/
static int nsd_fill_index_list(NI_group * ngr, THD_3dim_dataset * dset)
{
    NI_element    * nel;
    THD_datablock * blk;
    char            str[4];
    int           * node_list, * new_list = NULL;
    int             c, nx;

ENTRY("nsd_fill_index_list");

    blk = dset->dblk;
    nx = DSET_NX(dset);

    node_list = blk->node_list;
    if( blk->nnodes <= 0 || ! blk->node_list )  /* no node list */
    {
        if( gni.add_nodes )  /* create a default list   30 Aug 2006 [rickr] */
        {
            if(gni.debug) fprintf(stderr,"+d creating default INDEX_LIST\n");
            new_list = (int *)malloc(nx * sizeof(int));
            if( !new_list ){
                fprintf(stderr,"** NFIL: failed to alloc %d nodes\n",nx);
                RETURN(1);
            }
            for( c = 0; c < nx; c++ ) new_list[c] = c;
            node_list = new_list;
        }
        else
        {
            if(gni.debug) fprintf(stderr,"-d no INDEX_LIST to add\n");
            RETURN(0);
        }
    }
    else if( blk->nnodes != nx )
    {
        fprintf(stderr,"** node list len (%d) != NX (%d), skipping nodes\n",
                blk->nnodes, nx);
        RETURN(1);
    }

    if(gni.debug) fprintf(stderr,"+d adding INDEX_LIST, len %d\n", nx);

    nel = NI_new_data_element("INDEX_LIST", nx);

    nel->outmode = gni.write_mode; /* ASCII or BINARY mode (from globals) */

    if( are_sorted_ints(node_list, nx)) strcpy(str, "Yes");
    else                                strcpy(str, "No");

    NI_set_attribute(nel, "sorted_node_def", str);
    if(gni.debug > 1) fprintf(stderr,"+d set sorted_node_def = %s\n", str);

    NI_add_column(nel, NI_INT, node_list);

    if( new_list ) free(new_list);    /* lose any new list */

    NI_add_to_group(ngr, nel);

    RETURN(0);
}

/*------------------------------------------------------------------------*/
/*! Add SPARSE_DATA from the AFNI dset to the NIML group.
 *
 *  If the datum is not float, convert it.
 * -----------------------------------------------------------------------*/
static int nsd_add_sparse_data(NI_group * ngr, THD_3dim_dataset * dset)
{
    NI_element    * nel;
    THD_datablock * blk;
    float         * fdata = NULL;
    int             ind, nx, c;

ENTRY("nsd_add_sparse_data");

    blk = dset->dblk;
    nx = DSET_NX(dset);

    if(gni.debug) fprintf(stderr,"+d adding SPARSE_DATA element\n");

    /* check whether we have all floats, of not prepare for conversion */
    /*                                              4 Aug 2006 [rickr] */
    for( ind = 0; ind < blk->nvals; ind++ )
        if( DBLK_BRICK_TYPE(blk, ind) != MRI_float ) /* then allocate floats */
        {
            if( ! gni.to_float ){
                fprintf(stderr,"** dset has non-floats and AFNI_NSD_TO_FLOAT\n"
                               "   is NO, life has become unbearable...\n");
                RETURN(1);
            }

            fdata = malloc(nx * sizeof(float));  /* create our float array */
            if( !fdata ) { 
                fprintf(stderr,"** NASD: failed to malloc conversion floats\n");
                RETURN(1);
            }
            if(gni.debug)
                fprintf(stderr,"+d converting NI_SURF_DSET to floats\n");

            break;  /* and terminate the loop */
        }

    /* create initial element of length nx */
    nel = NI_new_data_element("SPARSE_DATA", nx);

    if(gni.debug > 1) fprintf(stderr,"+d adding %d data columns\n", blk->nvals);

    /* insert data */
    for( ind = 0; ind < blk->nvals; ind++ )
    {
        float fac;
        if( DBLK_BRICK_TYPE(blk, ind) != MRI_float )
        {
            EDIT_convert_dtype(nx, DBLK_BRICK_TYPE(blk,ind),DBLK_ARRAY(blk,ind),
                                   MRI_float, fdata, 0);
            /* apply any factor */
            fac = DBLK_BRICK_FACTOR(blk,ind); if( fac == 0.0 ) fac = 1.0;
            if( fac != 1.0 ) for(c = 0; c < nx; c++) fdata[c] *= fac;

            NI_add_column(nel, NI_FLOAT, fdata);  /* and add to element */
        }
        else NI_add_column(nel, NI_FLOAT, DBLK_ARRAY(blk, ind)); /* use dblk */
    }

    if( fdata ) free(fdata);  /* fly! (thud) be free! (thud) */
    
    /* Next declare output to be Node_Bucket_data always. Someday 
    we will change that IF we ever feel the need. ZSS Dec 07 */
    NI_set_attribute(nel, "data_type", "Node_Bucket_data"); 

    set_sparse_data_attribs(nel, dset, 1);

    NI_add_to_group(ngr, nel);

    RETURN(0);
}


/*------------------------------------------------------------------------*/
/*! set element attribute specific to SPARSE DATA and the dataset 3 Aug 2006
--------------------------------------------------------------------------*/
int set_sparse_data_attribs(NI_element * nel, THD_3dim_dataset * dset,
                            int nodes_from_dset)
{
    char str[32];

ENTRY("set_sparse_data_attribs");

    if( !nel || !dset ) RETURN(1);

    nel->outmode = gni.write_mode; /* ASCII or BINARY mode (from globals) */

    /* check for need of the ni_timestep attribute */
    if( DSET_NUM_TIMES(dset) > 1 )  /* then it is time dependent */
    {
        strcpy(str, MV_format_fval(DSET_TIMESTEP(dset)));
        NI_set_attribute(nel, "ni_timestep", str);
        if(gni.debug > 1) fprintf(stderr,"+d setting ni_timestep = %s\n", str);
    }

    RETURN(0);
}


/*------------------------------------------------------------------------*/
/*! return whether the given list is sorted            23 Aug 2006 [rickr]
--------------------------------------------------------------------------*/
static int are_sorted_ints(int *list, int len)
{
    int c;

ENTRY("are_sorted_ints");

    if( !list || len <= 0 ) RETURN(0);
    for( c = 0; c < len - 1; c++ )
        if( list[c] > list[c+1] )
            RETURN(0);
    RETURN(1);
}


/*------------------------------------------------------------------------*/
/*! Like strndup, relies on length, not nul            11 Jul 2006 [rickr]
--------------------------------------------------------------------------*/
static char * my_strndup(char *str, int len)
{
   char *dup;
   if( str == NULL || len < 0 ) return NULL;
   dup = (char *)calloc(len+1, sizeof(char));
   strncpy(dup,str,len);
   dup[len] = '\0';
   return dup;
}

/* append 2 floats and ints to str, subject to total length, len,
   and pre-pended with sep */
static int loc_append_vals(char ** str, int * len, char * sep,
                           float f1, float f2, int i1, int i2)
{
    char lbuf[256], fs1[32];  /* for first call to MV_format_fval */
    int  req;

ENTRY("loc_append_vals");

    if( !str || !*str || !len || !sep ) RETURN(1);
    if( strlen(sep) > 32 )     RETURN(1);

    /* first, just stuff them in a sufficient buffer */
    /* (make a copy of first float, and then format it all) */
    strcpy(fs1, MV_format_fval(f1));
    sprintf(lbuf, "%s%s %s %d %d", sep, fs1, MV_format_fval(f2), i1, i2);

    req = strlen(*str) + strlen(lbuf) + 1;
    if( req > *len )
    {
        *len = req + 512;       /* include some extra space */
        *str = (char *)realloc(*str, *len * sizeof(char));
    }

    strcat(*str, lbuf); /* finally, copy the data in */

    RETURN(0);
}

/* ----------------------------------------------------------------------
 * get the byte order from any ni_form attribute
 * return LSB_FIRST, MSB_FIRST, or NATIVE_ORDER (if none is found)
*/
int NI_get_byte_order(NI_element * nel)
{
    char * rhs;
    int    order = NATIVE_ORDER;

ENTRY("NI_get_byte_order");

    if( !nel ) RETURN(NATIVE_ORDER);

    rhs = NI_get_attribute(nel, "ni_form");
    if( !rhs )
    {
        if(gni.debug > 1) fprintf(stderr,"-d no ni_form for byte order\n");
        RETURN(NATIVE_ORDER);
    }

    if( strstr(rhs, "lsbfirst") ) order = LSB_FIRST;
    if( strstr(rhs, "msbfirst") ) order = MSB_FIRST;

    if( gni.debug > 1 )
        fprintf(stderr,"-d found byte order string, %s\n",
                order == LSB_FIRST ? LSB_FIRST_STRING :
                order == MSB_FIRST ? MSB_FIRST_STRING :
                                     NATIVE_STRING );

    RETURN(order);
}


/* ---------------------------------------------------------------------- */
/* NIML globals access functions                       3 Aug 2006 [rickr] */
int set_ni_globs_from_env(void)
{
ENTRY("set_ni_globs_from_env");

    /* if datasets don't have nodes, the user may want to add a default list */
    gni.add_nodes = AFNI_yesenv("AFNI_NSD_ADD_NODES");        /* 30 Aug 2006 */

    gni.debug = AFNI_numenv("AFNI_NIML_DEBUG"); /* maybe the user wants info */

    /* if having no conversion is desired, block it */
    gni.to_float = AFNI_noenv("AFNI_NSD_TO_FLOAT") ? 0 : 1;

    /* if text desired, use it for writing */
    gni.write_mode = AFNI_yesenv("AFNI_NIML_TEXT_DATA") ? NI_TEXT_MODE :
                                                          NI_BINARY_MODE;

    RETURN(0);
}

void set_gni_add_nodes( int add_nodes ){ gni.add_nodes = add_nodes; }
int  get_gni_add_nodes( void          ){ return gni.add_nodes;  }

void set_gni_debug( int debug ){ gni.debug = debug; }
int  get_gni_debug( void      ){ return gni.debug;  }

void set_gni_to_float( int flag ){ gni.to_float = flag; }      /* 4 Aug 2006 */
int  get_gni_to_float( void     ){ return gni.to_float; }

void set_gni_write_mode( int mode ){ gni.write_mode = mode; }
int  get_gni_write_mode( void     ){ return gni.write_mode; }
