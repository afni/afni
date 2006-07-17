/*----------------------------------------------------------------------
 * 19 May 2006: top-level routines to read NIML datasets         [rickr]
 *              routines to process NI_SURF_DSET datasets
 *----------------------------------------------------------------------
*/
#include "mrilib.h"
#include "SUMA/SUMA_suma.h"

static int ni_debug = 0;   /* for global debugging */
void set_ni_debug( int debug ){ ni_debug = debug; }

static char * my_strndup(char *, int);
static int    nsd_add_colms_type(THD_datablock *, NI_group *);
static int    nsd_add_sparse_data(NI_group *, THD_3dim_dataset *);
static int    nsd_add_str_to_group(char *, char *, char *,
                                   THD_datablock *, NI_group *);
static int    nsd_string_atr_to_slist(char ***, int, int, ATR_string *);
static int    process_ni_sd_sparse_data(NI_group *, THD_3dim_dataset *);
static int    process_ni_sd_attrs(THD_3dim_dataset *);
static int    process_ni_sd_group_attrs(NI_group *, THD_3dim_dataset *);

/* list of AFNI_dataset group attributes to copy along */
static char * ni_surf_dset_attrs[] = {
                "label",
                "domain_parent_idcode",
                "geometry_parent_idcode",
                "sorted_node_def"
                                     };

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

    ni_debug = AFNI_yesenv("AFNI_NI_DEBUG");  /* maybe the user wants info */

    nel = read_niml_file(fname, 1);  /* we need data for node_indices */
    if( !nel ) RETURN(NULL);

    smode = storage_mode_from_niml(nel);
    switch( smode )
    {
        case STORAGE_BY_3D:
            NI_free_element_data(nel);  /* nuke all data */
            dset = THD_niml_3D_to_dataset(nel, fname);
            if(ni_debug) fprintf(stderr,"-d opening 3D dataset '%s'\n",fname);
            if( !dset && ni_debug )
                fprintf(stderr,"** THD_niml_to_dataset failed on '%s'\n",fname);
        break;

        case STORAGE_BY_NIML:
            NI_free_element_data(nel);  /* nuke all data */
            if(ni_debug) fprintf(stderr,"-d opening NIML dataset '%s'\n",fname);
            dset = THD_niml_to_dataset(nel, 1); /* no data */
            if( !dset && ni_debug )
                fprintf(stderr,"** THD_niml_to_dataset failed on '%s'\n",fname);
        break;

        case STORAGE_BY_NI_SURF_DSET:
            if(ni_debug) fprintf(stderr,"-d opening NI_SURF_DSET '%s'\n",fname);
            dset = THD_ni_surf_dset_to_afni(nel, 0); /* no data */
        break;

        default:
            if( ni_debug )
                fprintf(stderr,"** THD_open_niml failed to open '%s'\n",fname);
        break;
    }

    NI_free_element(nel);

    if( dset )
    {   
        char * pp = THD_trailname(fname, 0);
        EDIT_dset_items(dset, ADN_prefix,pp,ADN_none);
                /* rcr - is this still necessary? */
        NI_strncpy(dset->dblk->diskptr->brick_name,pp,THD_MAX_NAME);
        THD_set_storage_mode(dset, smode);
        if(ni_debug) fprintf(stderr,"+d success for dataset '%s'\n",fname);
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

    if( ni_debug )
        fprintf(stderr,"-d THD_load_niml: file %s, smode %d\n", fname, smode);

    switch( smode )
    {
        case STORAGE_BY_3D:
            if(ni_debug) fprintf(stderr,"-d loading 3D dataset '%s'\n",fname);
            THD_load_3D(dblk);
            break;
        case STORAGE_BY_NIML:
            if(ni_debug) fprintf(stderr,"-d loading NIML dataset '%s'\n",fname);
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
            if(ni_debug) fprintf(stderr,"-d loading NI_SURF_DSET '%s'\n",fname);
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

        if(ni_debug) fprintf(stderr,"** SMFN: NI_element but not 3D\n");
    }
    else if( ni_type == NI_GROUP_TYPE )             /* AFNI or SUMA */
    {
        NI_group * ng = (NI_group *)nini;
        char     * atr;
        if( ! strcmp(ng->name, "AFNI_dataset") )
        {
            atr = NI_get_attribute(ng, "dset_type");
            if( atr && !strcmp(atr, "Node_Bucket") ) /* then SUMA DSET */
                RETURN(STORAGE_BY_NI_SURF_DSET);
            RETURN(STORAGE_BY_NIML);                 /* else assume AFNI */
        }
        else if(ni_debug)
            fprintf(stderr,"** SMFN: NI_group, but bad name '%s'\n",ng->name);
    }
    else if(ni_debug) fprintf(stderr,"** SMFN: bad ni_type %d\n",ni_type);

    RETURN(STORAGE_UNDEFINED);
}


/*! inhale any NIML data within a file */
void * read_niml_file( char * fname, int get_data )
{
    NI_stream    ns;
    NI_element * nel;
    char       * nname;

ENTRY("read_niml_file");

    if( !fname || !*fname )
    {
        if(ni_debug) fprintf(stderr,"** read_niml_file: empty filename\n");
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
        if(ni_debug)fprintf(stderr,"** RNF: failed to open file '%s'\n",fname);
        RETURN(NULL);
    }

    /* read the file */
    NI_skip_procins(1);  NI_read_header_only(!get_data);
    nel = NI_read_element(ns, 333);
    NI_skip_procins(0);  NI_read_header_only(get_data);

    /* close the stream */
    NI_stream_close(ns);

    /* possibly check the results */
    if( ni_debug )
    {
        if( !nel ) fprintf(stderr,"** RNF: failed to read '%s'\n",fname);
        else       fprintf(stderr,"+d success reading niml file '%s'\n",fname);
    }

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

    ni_debug = AFNI_yesenv("AFNI_NI_DEBUG");  /* maybe the user wants info */
    prefix   = DSET_PREFIX(dset);

    if( !prefix ) {
        if(ni_debug) fprintf(stderr,"** THD_write_niml: no dset prefix\n");
        RETURN(False);
    }

    smode = storage_mode_from_filename(prefix);
    if( ni_debug )
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
        slip            number of initial entries to skip (prob 0 or 1)
        atr             string attribute to get list from
                        (if NULL, strings will get default "#%d")

    return the number of strings found
*/
static int nsd_string_atr_to_slist(char *** slist, int llen, int skip,
                                   ATR_string * atr)
{
    int sind, posn, prev, copy_len;
    int done = 0, found = 0;

ENTRY("nsd_string_atr_to_slist");

    if(!slist || llen < 1)
    {
        fprintf(stderr,"** NSATS: bad params\n");
        RETURN(0);
    }

    if(ni_debug)
    {
        if( atr ) fprintf(stderr,"+d getting string attrs from %s\n",atr->name);
        else      fprintf(stderr,"+d setting default strings\n");
    }
    *slist = (char **)malloc(llen * sizeof(char *));

    if( !atr ) done = 1;

    /* first, skip the given number of strings */
    posn = -1;
    for( sind = 0; !done && sind < skip; sind++ )
    {
        for( posn++;
             posn < atr->nch && atr->ch[posn] && atr->ch[posn] != ';' ;
             posn++ )
            ;  /* just search */

        /* have we exhausted the entire list? */
        if( posn >= atr->nch || atr->ch[posn] == '\0' ) done = 1;
    }

    for( sind = 0; !done && sind < llen; sind++ )
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

            if(ni_debug) fprintf(stderr,"-d #%d = %s\n",sind,(*slist)[sind]);
        }
        else
        {
            (*slist)[sind] = (char *)malloc(10 * sizeof(char));
            sprintf((*slist)[sind], "#%d", sind);
        }

        if( posn >= atr->nch ) break;  /* found everything available */
    }

    for( ; sind < llen; sind++ )
    {
        (*slist)[sind] = (char *)malloc(10 * sizeof(char));
        sprintf((*slist)[sind], "#%d", sind);
    }

    if(ni_debug) fprintf(stderr,"-d found %d of %d strings\n", found, llen);

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

    THD_dblkatr_from_niml(ngr, dset->dblk);          /* store NIML attributes */

    rv = process_ni_sd_sparse_data(ngr, dset);       /* from SPARSE_DATA attr */
    if( !rv ) rv = process_ni_sd_group_attrs(ngr, dset); /* store group attrs */
    if( !rv ) rv = process_ni_sd_attrs(dset);            /* apply other attrs */

    RETURN(dset);
}

/* initialize the datablock using the SPARSE_DATA NI_SURF_DSET element

    - validate the SPARSE_DATA element, including types
        (possibly int for node list, and all float data)
    - check for byte_order in ni_form
    - set nnodes and node_list in datablock (if first column is node_list)
        - note that the image data is necessary for this
    - set nx and nvals
    - check for ni_timestep
*/
static int process_ni_sd_sparse_data(NI_group * ngr, THD_3dim_dataset * dset )
{
    THD_datablock * blk;
    THD_diskptr   * dkptr;
    THD_ivec3       nxyz;
    NI_element    * nel = NULL;
    void         ** elist = NULL;
    float           tr;
    char          * rhs, * cp;
    int             ind, nvals;

ENTRY("process_ni_sd_sparse_data");

    if( !ngr || !ISVALID_DSET(dset) )
    {
        if(ni_debug) fprintf(stderr,"** IDFSD: bad params\n");
        RETURN(1);
    }
    blk   = dset->dblk;
    dkptr = blk->diskptr;

    /* grab the first SPARSE_DATA element (should be only) */
    nvals = NI_search_group_shallow(ngr, "SPARSE_DATA", &elist);
    if( nvals > 0 ) { nel = (NI_element *)elist[0]; NI_free(elist); }

    if(!nel || nel->vec_num <= 0 || nel->vec_len <= 0)
    {
        if(ni_debug) fprintf(stderr,"** missing SPARSE_DATA element\n");
        RETURN(1);
    }

    /* so nel points to the SPARSE_DATA element */

    if(ni_debug)fprintf(stderr,"-d found SPARSE_DATA in NI_SURF_DSET\n");

    /* if we have ni_form="binary.{l,m}sbfirst", use it for the byte_order */
    dkptr->byte_order = mri_short_order();
    rhs = NI_get_attribute(nel, "ni_form");
    if( rhs )
    {   int len = strlen(rhs);
        if( len >= 8 )
        {
            if(ni_debug) fprintf(stderr,"-d setting BYTEORDER from %s\n",rhs);
            cp = rhs+len-8;
            if( !strcmp(cp, "lsbfirst") )      dkptr->byte_order = LSB_FIRST;
            else if( !strcmp(cp, "msbfirst") ) dkptr->byte_order = MSB_FIRST;
            else if(ni_debug) fprintf(stderr,"** unknown ni_form, '%s'\n", rhs);
        }
    }
    if(ni_debug)
        fprintf(stderr,"+d using byte order %s\n",
                BYTE_ORDER_STRING(dkptr->byte_order));


    /* if the first column is of type NI_INT, assume it is a node list */
    ind = 0;
    nvals = nel->vec_num;    /* note the number of sub-bricks */
    if( nel->vec_typ[0] == NI_INT )
    {
        blk->nnodes = nel->vec_len;
        blk->node_list = (int *)XtMalloc(blk->nnodes * sizeof(int));
        memcpy(blk->node_list, nel->vec[0], blk->nnodes * sizeof(int));
        if( dkptr->byte_order != mri_short_order() )
            nifti_swap_4bytes(blk->nnodes, blk->node_list);

        nvals--;
        ind ++;
        if(ni_debug) fprintf(stderr,"-d found node_list len %d\n",nel->vec_len);
    }

    /* and check that the rest of the columns are of type float */
    for( ; ind < nel->vec_num; ind++ )
        if( nel->vec_typ[ind] != NI_FLOAT )
        {
            if(ni_debug)
                fprintf(stderr,"** NI_SURF_DSET has non-float type %d\n",
                        nel->vec_typ[ind]);
            RETURN(1);
        }


    /* also, verify that we have "data_type=Node_Bucket_data" */
    rhs = NI_get_attribute(nel, "data_type");
    if( !rhs || strcmp(rhs, "Node_Bucket_data") )
    {
        if(ni_debug)
          fprintf(stderr,"** SPARSE_DATA without data_type Node_Bucket_data\n");
        RETURN(1);
    }

    /* now set nx, nvals, and datum */
    nxyz.ijk[0] = nel->vec_len;   nxyz.ijk[1] = nxyz.ijk[2] = 1;

    if(ni_debug)
        fprintf(stderr,"+d setting datum, nxyz, nx to float, %d, %d\n",
                nel->vec_len, nvals);

    EDIT_dset_items(dset,
                        ADN_datum_all,  MRI_float,
                        ADN_nxyz,       nxyz,
                        ADN_nvals,      nvals,
                     ADN_none );

    /*--- check for a ni_timestep attribute ---*/
    rhs = NI_get_attribute(nel, "ni_timestep");
    if( rhs && nvals > 1 )  /* then make time dependant */
    {
        tr = strtod(rhs, NULL);
        if(ni_debug) fprintf(stderr,"-d found TR = %f\n", tr);
        if( tr <= 0.0 ) tr = 1.0;   /* just be safe */
        EDIT_dset_items(dset,
                            ADN_func_type, ANAT_EPI_TYPE,
                            ADN_ntt      , nvals,
                            ADN_ttdel    , tr,
                            ADN_tunits   , UNITS_SEC_TYPE,
                        ADN_none);
    }

    RETURN(0);
}

/* apply known NI_SURF_DSET attributes */
static int process_ni_sd_attrs(THD_3dim_dataset * dset)
{
    THD_datablock * blk;
    THD_diskptr   * dkptr;
    THD_ivec3       ori;
    THD_fvec3       del, org;
    ATR_string    * atr_str;
    int             ind, nvals;

ENTRY("process_ni_sd_attrs");

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


/*    - top level element
  rcr - do this?
statistics via "ni_stat" -> EDIT_STATUAUX4()
   -- */

    /*--- init and fill any column labels ---*/
    atr_str = THD_find_string_atr(blk, "COLMS_LABS");
    if( !atr_str ) atr_str = THD_find_string_atr(blk, ATRNAME_BRICK_LABS);
    nsd_string_atr_to_slist(&blk->brick_lab, nvals, 1, atr_str);

    /*--- init and fill any statistic symbols ---*/
    atr_str = THD_find_string_atr(blk , "COLMS_STATSYM");
    if( !atr_str ) atr_str = THD_find_string_atr(blk , "BRICK_STATSYM");
    if(  atr_str ) /* only do this if we have some codes */
    {
        char **sar ; int scode,np ; float parm[3];
        np = nsd_string_atr_to_slist(&sar, nvals, 1, atr_str);
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

    /* verify whether we have a node list using COLMS_TYPE */
    atr_str = THD_find_string_atr(blk , "COLMS_TYPE");
    if( atr_str && atr_str->ch )
    {
        if( ! strncmp(atr_str->ch,"Node_Index",10) )
        {
            if(ni_debug) fprintf(stderr,"-d COLMS_TYPE[0] is Node_Index\n");
            if( blk->nnodes <= 0 )
              fprintf(stderr,"** warning: Node_Index COLMS_TYPE w/out nodes\n");
        }
        else if( blk->nnodes > 0 )
        {
            fprintf(stderr,"** warning: have nnodes, but COLMS_TYPE[0] is '");
            for( ind = 0; ind < atr_str->nch && atr_str->ch[ind]
                                             && atr_str->ch[ind] != ';';
                 ind ++ )
                fputc(atr_str->ch[ind], stderr);
            fprintf(stderr,"'\n");
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
static int process_ni_sd_group_attrs(NI_group * ngr, THD_3dim_dataset * dset )
{
    char       ** aname;
    char        * rhs;
    int         ac, natr;

ENTRY("process_ni_sd_group_attrs");

    natr = sizeof(ni_surf_dset_attrs) / sizeof(char *);

    for( ac = 0, aname = ni_surf_dset_attrs; ac < natr; ac++, aname++ )
    {
        rhs = NI_get_attribute(ngr, *aname);
        if( rhs && *rhs )
        {
            if(ni_debug)
                fprintf(stderr,"-d found group attr %s = %s\n",*aname,rhs);
            THD_set_string_atr(dset->dblk, *aname, rhs);
        }
        else if(ni_debug)
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
    int              nvals, ind, swap, offset = 0, len;

ENTRY("THD_add_sparse_data");

    if( !dset || !ngr ) RETURN(0);
    blk = dset->dblk;
    nvals = blk->nvals;

    ind = NI_search_group_shallow(ngr, "SPARSE_DATA", &elist);
    if( ind > 0 ) { nel = (NI_element *)elist[0]; NI_free(elist); }
    if( !nel ) RETURN(0);

    if( blk->nnodes > 0 ) offset = 1;  /* note index list */

    /*-- verify sizes and types --*/
    if( nel->vec_num != nvals + offset )
    {
        if(ni_debug)
            fprintf(stderr,"** TASD: vec_num = %d, but nvals, off = %d, %d\n",
                    nel->vec_num, nvals, offset);
        RETURN(0);
    }
    if( nel->vec_len != DSET_NX(dset) )
    {
        if(ni_debug) fprintf(stderr,"** TASD: vec_len = %d, but NX = %d\n",
                                    nel->vec_len, DSET_NX(dset));
        RETURN(0);
    }

    if( blk->nnodes > 0 && nel->vec_typ[0] != NI_INT )
    {
        if(ni_debug) fprintf(stderr,"** have nnodes, but typ[0] not NI_INT\n");
        RETURN(0);
    }
    for( ind = offset; ind < nel->vec_num; ind++ )
        if( nel->vec_typ[ind] != NI_FLOAT )
        {
            if(ni_debug) fprintf(stderr,"** TASD: vec[%d] not float\n",ind);
            RETURN(0);
        }
        else if( ! nel->vec[ind] )
        {
            if(ni_debug) fprintf(stderr,"** TASD: vec[%d] not filled!\n",ind);
            RETURN(0);
        }

    /* check for necessary swapping */
    swap = (blk->diskptr->byte_order != mri_short_order());
    if(ni_debug && swap) fprintf(stderr,"+d will byte_swap data\n");
    len = nel->vec_len;

    /* if there is a node list, we can free vector 0 */
    if( blk->nnodes > 0 ){ NI_free(nel->vec[0]);  nel->vec[0] = NULL; }
        
    /*-- we seem to have all of the data, now copy it --*/
    for( ind = 0; ind < nvals; ind++ )
    {
        data = (float *)XtMalloc(len * sizeof(float));
        if(!data){fprintf(stderr,"**ASD alloc fail: %d bytes\n",len);RETURN(0);}
        memcpy(data, nel->vec[ind+offset], len * sizeof(float));
        if( swap ) nifti_swap_4bytes(len, data);
        mri_fix_data_pointer(data, DBLK_BRICK(blk,ind));
        NI_free(nel->vec[ind+offset]);  nel->vec[ind+offset] = NULL;
    }

    RETURN(nvals);
}

/*! convert an AFNI dataset to a NIML group of type NI_SURF_DSET

    - set group attributes from ni_surf_dset_attrs
    - create SPARSE_DATA element, if requested
    - create attributes for COLMS_RANGE, COLMS_LABS, COLMS_TYPE, COLMS_STATSYM
    - apply HISTORY_NOTE
    
*/
NI_group * THD_dset_to_ni_surf_dset( THD_3dim_dataset * dset, int set_data )
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

    /* rcr - missing ni_surf_dset_attrs[] - get from dset attrs? */

    /* create group element */
    ngr = NI_new_group_element();
    NI_rename_group(ngr, "AFNI_dataset");
    NI_set_attribute(ngr, "dset_type", "Node_Bucket");
    NI_set_attribute(ngr, "self_idcode", dset->idcode.str);
    NI_set_attribute(ngr, "filename", blk->diskptr->brick_name);

    nsd_add_str_to_group("BRICK_LABS", "COLMS_LABS", "Node Indices", blk, ngr);
    nsd_add_colms_type(blk, ngr);
    nsd_add_str_to_group("BRICK_STATSYM", "COLMS_STATSYM", "none", blk, ngr);
    nsd_add_str_to_group("HISTORY_NOTE", NULL, NULL, blk, ngr);
    if( set_data ) nsd_add_sparse_data(ngr, dset); /* add SPARSE_DATA element */

    RETURN(ngr);
}


/*! add a COLMS_TYPE attribute element

   return 0 on success
*/
static int nsd_add_colms_type(THD_datablock * blk, NI_group * ngr)
{
    NI_element * nel;
    char       * str, * slist[1];  /* add_column requires a list of strings */
    int          c, plen, ni_list = 0;

ENTRY("nsd_add_str_to_group");

    /* check usage */
    if( !blk || !ngr ) RETURN(1);

    /* do we have a Node_Index column? */
    if( blk->nnodes > 0 && blk->node_list ) ni_list = 1;

    /* create a new string: "Node_Index;Generic_Float;Generic_Float;..." */
    plen = ni_list*11 + 14*blk->nvals + 1;
    str = (char *)malloc(plen * sizeof(char));
    *str = '\0';

    if( ni_list ) strcpy(str, "Node_Index;");
    for( c = 0; c < blk->nvals; c++ )
        strcat(str, "Generic_Float;");
    if( *str ) str[strlen(str)-1] = '\0';  /* nuke trailing ';' */

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
   - create a new string from the atr string, and apply zblock and prefix
   - put it in a data element
   - add it to the group

   aname  - AFNI attribute name
   niname - NIML attribute name (if NULL, use aname)
   prefix - prefix to add for node column (if NULL, ignore)
   blk    - datablock
   ngr    - NI_group to insert new element into

   return 0 on success
*/
static int nsd_add_str_to_group(char * aname, char * niname, char * prefix,
                                THD_datablock * blk, NI_group * ngr)
{
    ATR_string * atr;
    NI_element * nel;
    char       * dest, * slist[1];  /* add_column requires a list of strings */
    int          plen, apply = 0;

ENTRY("nsd_add_str_to_group");

    /* check usage */
    if( !aname || !blk || !ngr ) RETURN(1);

    atr = THD_find_string_atr(blk, aname);
    if( !atr ) RETURN(0);  /* nothing to add */

    if(ni_debug){
        fprintf(stderr, "-d adding '%s' atr: ", niname?niname:aname);
        fwrite(atr->ch, sizeof(char), atr->nch, stderr);
        fputc('\n', stderr);
    }

    /* should we add a prefix column? */
    if( prefix && blk->nnodes > 0 && blk->node_list ) apply = 1;

    /* create a new string */
    plen = apply ? (strlen(prefix)+1) : 0;                /* +1 for separator */
    dest = (char *)calloc(atr->nch+plen+1, sizeof(char)); /* +1 for last '\0' */

    if( apply ) strcpy(dest, prefix);
    memcpy(dest+plen, atr->ch, atr->nch);
    if( prefix ) THD_zblock_ch(atr->nch+plen, dest, ZSBLOCK); /* only if list */
    dest[atr->nch+plen] = '\0';

    /* now add it to the group */
    slist[0] = dest;
    nel = NI_new_data_element("AFNI_atr", 1);
    nel->outmode = NI_TEXT_MODE;
    NI_set_attribute(nel, "atr_name", niname ? niname : aname);
    NI_add_column(nel, NI_STRING, slist);
    NI_add_to_group(ngr, nel);

    free(dest); /* nuke local copy */

    RETURN(0);
}


static int nsd_add_sparse_data(NI_group * ngr, THD_3dim_dataset * dset)
{
    NI_element    * nel;
    THD_datablock * blk;
    int             ind, nx;

ENTRY("nsd_add_sparse_data");

    blk = dset->dblk;
    nx = DSET_NX(dset);

    if(ni_debug) fprintf(stderr,"+d adding SPARSE_DATA element\n");

    /* create initial element of length nx */
    nel = NI_new_data_element("SPARSE_DATA", nx);

    /* possibly add node_list */
    if( blk->nnodes > 0 && blk->node_list )
    {
        NI_add_column(nel, NI_INT, blk->node_list);
        if(ni_debug) fprintf(stderr,"+d adding node_list data\n");
    }

    if(ni_debug) fprintf(stderr,"+d adding %d data columns\n", blk->nvals);

    /* insert data */
    for( ind = 0; ind < blk->nvals; ind++ )
        NI_add_column(nel, NI_FLOAT, DBLK_ARRAY(blk, ind));

    /* if text is not requested, use binary for write */
    if( ! AFNI_yesenv("AFNI_NIML_TEXT_DATA") )
        nel->outmode = NI_BINARY_MODE;
    NI_set_attribute(nel, "data_type", "Node_Bucket_data");

    NI_add_to_group(ngr, nel);

    RETURN(0);
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
