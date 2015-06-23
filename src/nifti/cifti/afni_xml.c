#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "afni_xml.h"

#define AXML_MIN_BSIZE 2048
#define AXML_DEF_BSIZE 32768

/* local prototypes */

#ifndef XMLCALL
/* XMLCALL was added to expat in version 1.95.7 to define a calling convention,
 * as cdecl
 */
#if defined(XML_USE_MSC_EXTENSIONS)
#define XMLCALL __cdecl
#elif defined(__GNUC__) && defined(__i386)
#define XMLCALL __attribute__((cdecl))
#else
#define XMLCALL 
#endif
#endif /* not defined XMLCALL */

#ifndef XML_STATUS_ERROR
#define XML_STATUS_ERROR 0
#endif
#ifndef XML_STATUS_OK
#define XML_STATUS_OK 1
#endif

static void XMLCALL cb_start_ele    (void *, const char *, const char **);
static void XMLCALL cb_end_ele      (void *, const char *);
static void XMLCALL cb_char         (void *, const char *, int);
static void XMLCALL cb_instr        (void *, const char *, const char *);
static void XMLCALL cb_comment      (void *, const char *);
static void XMLCALL cb_cdata_start  (void *);
static void XMLCALL cb_cdata_end    (void *);
static void XMLCALL cb_default      (void *, const char *, int);
static void XMLCALL cb_xml_dec      (void *, const char *, const char * , int);
static void XMLCALL cb_start_doctype(void *, const char *, const char *,
                                     const char *, int);
static void XMLCALL cb_end_doctype  (void *);
static void XMLCALL cb_elem_dec     (void *, const char *, XML_Content *);
static XML_Parser init_xml_parser   (void *);

/* ---------------------------------------------------------------------- */
/* XML global struct and access functions                                 */
static afni_xml_control AXD_defaults = {
    1,          /* verb, default to 1 (0 means quiet)         */
    1,          /* dstore, flag whether to store data         */
    3,          /* indent, spaces per indent level            */
    AXML_DEF_BSIZE, /* buf_size, allocated for XML parsing    */

    0,          /* depth, current stack depth                 */
    0,          /* dskip depth (at positive depth, 0 is clear)*/
    0,          /* errors, number of encountered errors       */
    {NULL},     /* stack, afni_xml_t * [AXML_MAX_DEPTH]       */

    NULL,       /* zdata, compression buffer pointer          */
    NULL        /* gim, gifti_image *, for results            */
};

/* main control structure */
static afni_xml_control AXD = AXD_defaults;

/*---------- accessor functions for user-controllable variables ----------*/
/*----------     (one can pass -1 to set the default value)     ----------*/

/*! verb is the vebose level, with 0 meaning "only report errors" (up to 7) */
int axml_get_verb( void    ){ return AXD.verb; }
int axml_set_verb( int val )
{
    if     ( val == -1 ) AXD.verb = 1;
    else if( val >=  0 ) AXD.verb = val;
    return 0;
}

/*! the dstore flag controls whether data is read from a GIFTI file */
int axml_get_dstore( void    ){ return AXD.dstore; }
int axml_set_dstore( int val )
{
    if( val ) AXD.dstore = 1;   /* includes -1 as applying the default */
    else      AXD.dstore = 0;
    return 0;
}

/*! indent is the number of spaces per indent level written to a GIFTI file */
int axml_get_indent( void    ){ return AXD.indent; }
int axml_set_indent( int val )
{
    if      ( val == -1 ) AXD.indent = 3;
    else if ( val >=  0 ) AXD.indent = val;
    else return 1;  /* failure - no action */
    return 0;
}

/*! buf_size is the size of the XML I/O buffer given to expat */
int axml_get_buf_size( void    ){ return AXD.buf_size; }
int axml_set_buf_size( int val )
{
    if      ( val == -1 ) AXD.buf_size = AXML_DEF_BSIZE;
    else if ( val  >  0 ) AXD.buf_size = val;
    else return 1;      /* failure - no action */
    return 0;
}

/*----------------------- local prototypes -----------------------------*/

static int disp_axml_data(char * mesg, axml_data * dp, int show_all );
static int init_axml_data(afni_xml_control *xp, int doall);
static int reset_xml_buf(afni_xml_control * xd, char ** buf, int * bsize);

/*----------------------- main I/O functions ---------------------------*/

/* note: the buffer needs to be large enough to contain any contiguous
         piece of (CDATA?) text, o.w. it will require parsing in pieces */
afni_xml_t * axml_read_file(const char * fname, int read_data)
{
    afni_xml_control * xd = &AXD;        /* point to global struct */

    XML_Parser   parser;
    unsigned     blen;
    FILE       * fp;
    char       * buf = NULL;
    int          bsize;    /* be sure it doesn't change at some point */
    int          done = 0, pcount = 1;
 
    if( init_axml_data(xd, 0) ) /* reset non-user variables */
        return NULL;

    xd->dstore = read_data;  /* store for global access */

    if( !fname ) {
        fprintf(stderr,"** axml_read_image: missing filename\n");
        return NULL;
    }

    fp = fopen(fname, "r");
    if( !fp ) {
        fprintf(stderr,"** failed to open XML file '%s'\n", fname);
        return NULL;
    }

    /* create a new buffer */
    bsize = 0;
    if( reset_xml_buf(xd, &buf, &bsize) ) { fclose(fp); return NULL; }

    if(xd->verb > 1) fprintf(stderr,"-- reading xml file '%s'\n", fname);

/* rcr - here */

    /* create parser, init handlers */
    parser = init_xml_parser((void *)xd);

    while( !done )
    {
        if( reset_xml_buf(xd, &buf, &bsize) )  /* fail out */
            { gifti_free_image(xd->gim); xd->gim = NULL; break; }

        blen = fread(buf, 1, bsize, fp);
        done = blen < bsize;

        if(xd->verb > 3) fprintf(stderr,"-- XML_Parse # %d\n", pcount);
        pcount++;
        if( XML_Parse(parser, buf, blen, done) == XML_STATUS_ERROR) {
            fprintf(stderr,"** %s at line %u\n",
                    XML_ErrorString(XML_GetErrorCode(parser)),
                    (unsigned int)XML_GetCurrentLineNumber(parser));
            gifti_free_image(xd->gim);
            xd->gim = NULL;
            break;
        }
    }

    if(xd->verb > 1) {
        if(xd->gim)
            fprintf(stderr,"-- have gifti image '%s', "
                           "(%d DA elements = %lld MB)\n",
                    fname, xd->gim->numDA, gifti_gim_DA_size(xd->gim,1));
        else fprintf(stderr,"** gifti image '%s', failure\n", fname);
    }

    fclose(fp);
    if( buf ) free(buf);        /* parser buffer */
    XML_ParserFree(parser);

    if( dalist && xd->da_list )
        if( apply_da_list_order(xd, dalist, dalen) ) {
            fprintf(stderr,"** failed apply_da_list_order\n");
            gifti_free_image(xd->gim);
            xd->gim = NULL;
        }

    free_xd_data(xd);  /* free data buffers */

    return xd->gim;
}


/*----------------------- end main I/O functions -----------------------*/



static int init_axml_data(afni_xml_control *xp, int doall)
{
   int verb, dstore, indent, buf_size;   /* user vars */

   if( doall ) {         /* user modifiable - init all to defaults */
      *xp = AXD_defaults;
   } else {
      xp->depth = 0;
      xp->dskip = 0;
      xp->errors = 0;
      memset(xp->stack, 0, sizeof(xp->stack));
   }

   /* maybe show the user */
   if( dp->verb > 2 )
       disp_axml_data("-- user opts: ", xp, xp->verb > 3);

   return 0;
}


static int disp_axml_data(char * mesg, axml_data * dp, int show_all )
{
   if( mesg ) fputs(mesg, stderr);

   if( !dp ) return 1;

   fprintf(stderr,"axml_data :\n"
                  "   verb        : %d\n"
                  "   dstore      : %d\n"
                  "   indent      : %d\n"
                  "   buf_size    : %d\n"
          , dp->verb, dp->dstore, dp->indent, dp->buf_size);

   if( show_all )
      fprintf(stderr,
              "   depth       : %d\n"
              "   dskip       : %d\n"
              "   errors      : %d\n"
              , dp->depth, dp->dskip, dp->errors);

   return 0;
}

/* if bsize is no longer correct, update it and realloc the buffer */
static int reset_xml_buf(afni_xml_control * xd, char ** buf, int * bsize)
{
    if( *bsize == xd->buf_size ) {
        if( xd->verb > 3 )
            fprintf(stderr,"-- buffer kept at %d bytes\n", *bsize);
        return 0;
    }

    if( xd->verb > 2 )
        fprintf(stderr,"++ update buf, %d to %d bytes\n",*bsize,xd->buf_size);

    *bsize = xd->buf_size;
    *buf = (char *)realloc(*buf, *bsize * sizeof(char));

    if( ! *buf ) {
        fprintf(stderr,"** failed to alloc %d bytes of xml buf!\n", *bsize);
        *bsize = 0;
        return 1;
    }

    return 0;
}


