/*----------------------------------------------------------------------
 * 06 Feb 2008: top-level routines to read GIFTI datasets        [rickr]
 *
 * These main routines should match those in gifti_choice.c.
 *
 * THD_3dim_dataset * THD_open_gifti (char *fname)
 * THD_3dim_dataset * THD_load_gifti (THD_datablock *dblk)
 * Boolean            THD_write_gifti(THD_3dim_dataset *dset, int write_data)
 *
 * NI_group         * NI_read_gifti  (char *fname, int read_data)
 * int                NI_write_gifti (NI_group ngr, char *fname, int add_index)
 *
 *----------------------------------------------------------------------
 */

#include "mrilib.h"
#include "gifti_io.h"

/* add to thd_gifti.h */
NI_group * gifti_to_NSD(gifti_image * gim);


typedef struct {        /* put this in thd_gifti.h ? */
    int verb;
} gifti_globs_t;

gifti_globs_t gifti_globs = { 1 };
static gifti_globs_t * GP = &gifti_globs; /* for ease of access */

static int add_string_attribute(NI_group * ngr, char * aname, char * value);
static int disp_gifti_globs(char * mesg, gifti_globs_t * g);
static int gifti_globs_from_env(void);
static int gifti_has_NSD_form(gifti_image * gim, int whine);

static char * gifti_DA_meta_concat(gifti_image * gim, char * name,
                                   char * def, char * sep);
/*
 * - sub-brick selection in NI_read_gifti()?
 *
 *
 *
 */



/* ------------------------------- AFNI ------------------------------- */

THD_3dim_dataset * THD_open_gifti(char * fname)
{
    NI_group * ngr;

    fprintf(stderr,"** THD_open_gifti: testing...\n\n");

    ngr = NI_read_gifti(fname, 0);

    NI_free_element(ngr);

    return NULL;
}

/* presumably we've already whined, via 'open' */
int THD_load_gifti(THD_datablock * dblk){ return 1; }

Boolean THD_write_gifti(THD_3dim_dataset * dset, int write_data)
{
    char * prefix;

    ENTRY("THD_write_gifti");

    prefix = DSET_PREFIX(dset);
    fprintf(stderr,"** THD_write_gifti: '%s', coming soon\n", prefix);
    RETURN(False);
}

/* ------------------------------- NIML ------------------------------- */

/* read GIFTI and convert to NI_SURF_DSET */
NI_group * NI_read_gifti(char * fname, int read_data)
{
    NI_group    * ngr;
    gifti_image * gim;
    char        * cp;

    ENTRY("NI_read_gifti");

    gifti_globs_from_env();

    if( !fname ) {
        if( GP->verb > 0 ) fprintf(stderr,"** NI_read_gifti: null filename\n");
        RETURN(NULL);
    }

    if( GP->verb > 2 ) fprintf(stderr,"-- NI_read_gifti from '%s'\n", fname );

    gifti_set_verb(GP->verb);

    gim = gifti_read_image(fname, read_data);
    if( !gim ) {
        if( GP->verb > 1 )
            fprintf(stderr,"-- NI_read_gifti: failed to read '%s'\n", fname);
        RETURN(NULL);
    }

    /* set a filename attribute */
    if( !gifti_get_meta_value(&gim->meta, "filename") )
        gifti_add_to_nvpairs(&gim->meta, "filename", fname);

    ngr = gifti_to_NSD(gim);
    if( !ngr ) gifti_free_image(gim);

    RETURN(ngr);
}

int NI_write_gifti(NI_group ngr, char * fname, int add_index)
{
    fprintf(stderr,"** cannot write '%s', no compiled GIFTI support\n",
            fname ? fname : "NULL");
    return 1;
}

/* convert between dataset types: GIFTI to NI_SURF_DSET */
NI_group * gifti_to_NSD(gifti_image * gim)
{
    NI_group * ngr;
    char     * cp;

    ENTRY("gifti_to_NSD");

    if( !gim ){
        fprintf(stderr,"** gifti_to_NSD: to gifti_image\n");
        RETURN(NULL);
    }

    if( ! gifti_has_NSD_form(gim, GP->verb > 0) ) RETURN(NULL);

    ngr = NI_new_group_element();
    NI_rename_group(ngr, "AFNI_dataset");
    NI_set_attribute(ngr, "dset_type", "Node_Bucket");

    /* get or create an ID code */
    cp = gifti_get_meta_value(&gim->meta, "UniqueID");
    NI_set_attribute(ngr, "self_idcode", cp ? cp : UNIQ_idcode());

    cp = gifti_get_meta_value(&gim->meta, "filename");
    if( cp ) NI_set_attribute(ngr, "filename", cp);

    /* add COLMS_LABS from "Name" MetaData values */
    cp = gifti_DA_meta_concat(gim, "Name", "none", ";");
    add_string_attribute(ngr, "COLMS_LABS", cp);
    if( cp ) free(cp);  /* data has been copied */

    RETURN(ngr);
}

/* --------------------------- static functions ---------------------- */

/* add a string attribute */
static int add_string_attribute(NI_group * ngr, char * aname, char * value)
{
    NI_element * nel;

    if( !ngr || !aname || !value ) return 1;

    nel = NI_new_data_element("AFNI_atr", 1);
    nel->outmode = NI_TEXT_MODE;
    NI_set_attribute(nel, "atr_name", aname);
    NI_add_column(nel, NI_STRING, &value);
    NI_add_to_group(ngr, nel);

    return 1;
}

/* determine whether this gifti dataset has a form appropriate for a
 * NI_SURF_DSET dataset */
static int gifti_has_NSD_form(gifti_image * gim, int whine)
{
    if( !gim ) {
        if( whine ) fprintf(stderr,"bad NSD form: GIFTI image is NULL\n");
        return 0;
    }

    if( ! gim->darray ) {
        if(whine) fprintf(stderr,"bad NSD form: GIFTI image has no darray\n");
        return 0;
    }

    if( gim->numDA <= 0 ) {
        if(whine) fprintf(stderr,"bad NSD form: GIFTI image has no DA elems\n");
        return 0;
    }

    return 1;
}

/* find all DA meta data of the given name, and concat using separater sep
 *
 * if 'name' is not found, use 'defval'
 * if 'sep' is set, separate values with that string
 */
static char * gifti_DA_meta_concat(gifti_image * gim, char * name,
                                   char * def, char * sep)
{
    char * result, * val;
    int    c, length = 0, deflen = 0, seplen = 0;

    if( !gim || !name || !gim->darray || gim->numDA <= 0 )
        return NULL;

    if( sep ) seplen = strlen(sep);
    if( def ) deflen = strlen(def);

    if( GP->verb > 3 )
        fprintf(stderr,"-- making meta string from '%s', def '%s', sep '%s'\n",
                name, def ? def : "NULL", sep ? sep : "NULL");

    /* first compute the total length */

    for( c = 0; c < gim->numDA; c++ ) {
        if( gim->darray[c] ) {
            val = gifti_get_meta_value(&gim->darray[c]->meta, name);
            if( val ) length += strlen(val);
            else      length += deflen;
        }   else      length += deflen;
        length += seplen;
    }

    if( length <= 0 ) return NULL;

    /* allocate memory and fill */

    length += 1; /* for terminating nul char */

    result = (char *)malloc( length * sizeof(char) );
    if( ! result ) {
        fprintf(stderr,"** GDMC: failed to alloc %d chars\n", length);
        return NULL;
    }

    /* now fill the result with data that the length was computed from */

    *result = '\0'; /* now we can use strcat */
    for( c = 0; c < gim->numDA; c++ ) {
        if( gim->darray[c] ) {
            val = gifti_get_meta_value(&gim->darray[c]->meta, name);
            if( val ) strcat(result, val);
            else if( def ) strcat(result, def);
        }   else if( def ) strcat(result, def);

        if( sep ) strcat(result, sep);
    }

    if( GP->verb > 2 )
        fprintf(stderr,"++ made meta string from '%s': '%s'\n", name, result);

    return result;
}


static int gifti_globs_from_env(void)
{
    char * ept = NULL;

    ept = my_getenv("AFNI_NIML_DEBUG");
    if( ept ) GP->verb = atoi(ept);       /* adjust if set */

    if( GP->verb > 1 ) disp_gifti_globs("gifti_globs_from_env: ", GP);

    return 0;
}

static int disp_gifti_globs(char * mesg, gifti_globs_t * g)
{
    if( mesg ) fputs(mesg, stderr);

    fprintf(stderr,"gifti_globs_t:\n"
                   "    verb    = %d\n",
                   g->verb);
    return 0;
}

