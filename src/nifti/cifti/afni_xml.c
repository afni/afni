#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <expat.h>
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


/* ---------------------------------------------------------------------- */
/* XML global struct and access functions                                 */

static afni_xml_control gAXD = {
   1,          /* verb, default to 1 (0 means quiet)         */
   1,          /* dstore, flag whether to store data         */
   3,          /* indent, spaces per indent level            */
   AXML_DEF_BSIZE, /* buf_size, allocated for XML parsing    */
   NULL,       /* stream                                     */

   0,          /* depth, current stack depth                 */
   0,          /* dskip depth (at positive depth, 0 is clear)*/
   0,          /* errors, number of encountered errors       */
   {NULL},     /* stack, of afni_xml_t *                     */

   NULL,       /* afni_xml_list * xroot                      */
};


/*---------- accessor functions for user-controllable variables ----------*/
/*----------     (one can pass -1 to set the default value)     ----------*/

/*! verb is the vebose level, with 0 meaning "only report errors" (up to 7) */
int axml_get_verb( void    ){ return gAXD.verb; }
int axml_set_verb( int val )
{
    if     ( val == -1 ) gAXD.verb = 1;
    else if( val >=  0 ) gAXD.verb = val;
    return 0;
}

/*! the dstore flag controls whether data is read from a GIFTI file */
int axml_get_dstore( void    ){ return gAXD.dstore; }
int axml_set_dstore( int val )
{
    if( val ) gAXD.dstore = 1;   /* includes -1 as applying the default */
    else      gAXD.dstore = 0;
    return 0;
}

/*! indent is the number of spaces per indent level written to a GIFTI file */
int axml_get_indent( void    ){ return gAXD.indent; }
int axml_set_indent( int val )
{
    if      ( val == -1 ) gAXD.indent = 3;
    else if ( val >=  0 ) gAXD.indent = val;
    else return 1;  /* failure - no action */
    return 0;
}

/*! buf_size is the size of the XML I/O buffer given to expat */
int axml_get_buf_size( void    ){ return gAXD.buf_size; }
int axml_set_buf_size( int val )
{
    if      ( val == -1 ) gAXD.buf_size = AXML_DEF_BSIZE;
    else if ( val  >  0 ) gAXD.buf_size = val;
    else return 1;      /* failure - no action */
    return 0;
}

/*----------------------- local prototypes -----------------------------*/

static XML_Parser init_xml_parser   (void *);

static void XMLCALL cb_default      (void *, const char *, int);
static void XMLCALL cb_start_ele(void *, const char *, const char **);
static void XMLCALL cb_stop_ele (void *, const char *);

static int epush(afni_xml_control *, const char *, const char **);
static int epop (afni_xml_control *, const char *);

static int add_to_xchild_list(afni_xml_t * parent, afni_xml_t * child);
static int add_to_xroot_list (afni_xml_control * xd, afni_xml_t * newp);
static int disp_axml_data (char * mesg, afni_xml_control * dp, int show_all );
static int free_xd_results(afni_xml_control * xd);
static int free_xd_buffers(afni_xml_control * xd);
static int init_axml_ctrl (afni_xml_control *xp, int doall);
static int reset_xml_buf  (afni_xml_control * xd, char ** buf, int * bsize);
static int white_first    (const char * str, int len);
static int white_last     (const char * str, int len);

static int show_depth     (afni_xml_control *, int show);
static int show_attrs     (afni_xml_control *, const char *, const char **);

static afni_xml_t * new_afni_xml(void);
static char       * strip_whitespace(const char * str, int slen);

/*----------------------- main I/O functions ---------------------------*/

/* note: the buffer needs to be large enough to contain any contiguous
         piece of (CDATA?) text, o.w. it will require parsing in pieces */
afni_xml_list axml_read_file(const char * fname, int read_data)
{
    afni_xml_control * xd = &gAXD;        /* point to global struct */
    afni_xml_list      xlist = {0, NULL};

    XML_Parser   parser;
    unsigned     blen;
    FILE       * fp;
    char       * buf = NULL;
    int          bsize;    /* be sure it doesn't change at some point */
    int          done = 0, pcount = 1;
 
    if( init_axml_ctrl(xd, 0) ) /* reset non-user variables */
        return xlist;

    xd->xroot = &xlist;      /* return struct */
    xd->dstore = read_data;  /* store for global access */

    if( !fname ) {
        fprintf(stderr,"** axml_read_image: missing filename\n");
        return xlist;
    }

    fp = fopen(fname, "r");
    if( !fp ) {
        fprintf(stderr,"** failed to open XML file '%s'\n", fname);
        return xlist;
    }

    /* create a new buffer */
    bsize = 0;
    if( reset_xml_buf(xd, &buf, &bsize) ) { fclose(fp); return xlist; }

    if(xd->verb > 1) fprintf(stderr,"-- reading xml file '%s'\n", fname);

    /* create parser, init handlers */
    parser = init_xml_parser((void *)xd);

    while( !done )
    {
        if( reset_xml_buf(xd, &buf, &bsize) )  /* fail out */
            { free_xd_results(xd); break; }

        blen = fread(buf, 1, bsize, fp);
        done = blen < bsize;

        if(xd->verb > 4) fprintf(stderr,"-- XML_Parse # %d\n", pcount);
        pcount++;
        if( XML_Parse(parser, buf, blen, done) == XML_STATUS_ERROR) {
            fprintf(stderr,"** %s at line %u\n",
                    XML_ErrorString(XML_GetErrorCode(parser)),
                    (unsigned int)XML_GetCurrentLineNumber(parser));
            
            free_xd_results(xd);
            break;
        }
    }

    if(xd->verb > 1) {
         fprintf(stderr,"++ done, status is SOMETHING\n");
    }

    fclose(fp);
    if( buf ) free(buf);        /* parser buffer */
    XML_ParserFree(parser);

/*
    if( dalist && xd->da_list )
        if( apply_da_list_order(xd, dalist, dalen) ) {
            fprintf(stderr,"** failed apply_da_list_order\n");
            gifti_free_image(xd->gim);
            xd->gim = NULL;
        }
*/

    free_xd_buffers(xd);  /* free data buffers */

    return xlist;
}

int axml_disp_xlist(char * mesg, afni_xml_list * axlist)
{
   FILE * fp = stderr;
   int    ind;

   if( gAXD.stream != NULL ) fp = gAXD.stream;

   if( mesg ) fputs(mesg, fp);

   if( ! axlist || ! axlist->xlist ) {
      fprintf(fp, "afni_xml_list is EMPTY\n");
      return 0;
   }

   fprintf(fp, "afni_xml_list, len = %d\n", axlist->len);
   for(ind = 0; ind < axlist->len; ind++ ) {
      fprintf(fp, "   afni_xml_t root %d of %d:\n", ind, axlist->len);
      axml_disp_xml_t(NULL, axlist->xlist[ind], 1);
   }

   return 0;
}

int axml_disp_xml_t(char * mesg, afni_xml_t * ax)
{
   FILE * fp = stderr;
   int    ind;

   if( gAXD.stream != NULL ) fp = gAXD.stream;

}


/*----------------------- end main I/O functions -----------------------*/



static int init_axml_ctrl(afni_xml_control *xp, int doall)
{
   if( doall ) {         /* user modifiable - init all to defaults */
      xp->verb     = 1;
      xp->dstore   = 1;
      xp->indent   = 3;
      xp->buf_size = AXML_DEF_BSIZE;
      xp->stream   = stderr;    /* rcr - ponder accessor (might need close) */
   }

   xp->depth = 0;
   xp->dskip = 0;
   xp->errors = 0;
   memset(xp->stack, 0, sizeof(xp->stack));

   xp->xroot = NULL;

   /* maybe show the user */
   if( xp->verb > 2 )
       disp_axml_data("-- user opts: ", xp, xp->verb > 3);

   return 0;
}


static int disp_axml_data(char * mesg, afni_xml_control * dp, int show_all )
{
   if( mesg ) fputs(mesg, stderr);

   if( !dp ) return 1;

   fprintf(stderr,"afni_xml_control :\n"
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

static int free_xd_results(afni_xml_control * xd)
{
   fprintf(stderr,"== rcr - free results\n");
   return 0;
}

static int free_xd_buffers(afni_xml_control * xd)
{
   fprintf(stderr,"== rcr - free xd buffers\n");
   return 0;
}

static XML_Parser init_xml_parser(void * user_data)
{
   XML_Parser parser = XML_ParserCreate(NULL);

   XML_SetUserData(parser, user_data);

   XML_SetStartElementHandler(parser, cb_start_ele);
   XML_SetEndElementHandler  (parser, cb_stop_ele);
   XML_SetDefaultHandler     (parser, cb_default);

   if( gAXD.verb > 3 ) fprintf(stderr,"-- parser initialized\n");

   return parser;
}

static void XMLCALL cb_start_ele(void *udata, const char *ename,
                                              const char **attr)
{
    afni_xml_control * xd = (afni_xml_control *)udata;
    (void)epush(xd, ename, attr);
}

static void XMLCALL cb_stop_ele(void *udata, const char *ename)
{
    afni_xml_control * xd = (afni_xml_control *)udata;
    (void)epop(xd, ename);
}

static int epush(afni_xml_control * xd, const char * ename, const char ** attr)
{
   afni_xml_t * acur = NULL;

   if( xd->depth < 0 || xd->depth > AXML_MAX_DEPTH ) {
       fprintf(stderr,"** push: stack depth %d out of [0,%d] range\n",
               xd->depth, AXML_MAX_DEPTH);
       xd->errors++;
       return 1;
   }

   if( xd->verb > 2 ) {       /* maybe we want to print something */
       show_depth(xd, 1);
       fprintf(stderr,"++ push '%s'\n", ename);
       if( xd->verb > 3 ) show_attrs(xd, ename, attr);
   }

   /* rcr - determine whether we should go into a skip block */
   if( 0 ) {
      xd->dskip = xd->depth;
   }

   /* if we are in a skip block, do nothing but monitor stack */
   if( xd->dskip ) {
       if( xd->verb > 3 )
           fprintf(stderr,"-- skip=%d, depth=%d, skipping push element '%s'\n",
                   xd->dskip, xd->depth, ename);
       xd->stack[xd->depth] = NULL;
   } else {
      /* --- control depth and corresponding vars --- */
      acur = new_afni_xml(ename, attr);
      if( ! acur ) { xd->dskip = xd->depth; return 1; }
      xd->stack[xd->depth] = acur;

      /* we have either a root element or a child */
      if( xd->depth == 0 ) {
         if( add_to_xroot_list(xd, acur) ) { xd->dskip = xd->depth; return 1; }
      } else {
         if( add_to_xchild_list(xd->stack[xd->depth-1], acur) ) {
            xd->dskip = xd->depth; return 1;
         }
      }
   }

   xd->depth++;

   return 0;
}


static int add_to_xroot_list(afni_xml_control * xd, afni_xml_t * newp)
{
   /* init */
   if( xd->xroot->len <= 0 ) { xd->xroot->len = 0; xd->xroot->xlist = NULL; }

   xd->xroot->len++;
   xd->xroot->xlist = (afni_xml_t **)realloc(xd->xroot->xlist,
                                     xd->xroot->len * sizeof(afni_xml_t *));
   if( ! xd->xroot->xlist ) {
      fprintf(stderr,"** failed to alloc %d AXMLT pointers\n", xd->xroot->len);
      return 1;
   }
   xd->xroot->xlist[xd->xroot->len-1] = newp;

   return 0;
}


static int add_to_xchild_list(afni_xml_t * parent, afni_xml_t * child)
{
   /* init */
   if( parent->nchild <= 0 ) { parent->nchild = 0; parent->xchild = NULL; }

   parent->nchild++;
   parent->xchild = (afni_xml_t **)realloc(parent->xchild,
                                   parent->nchild * sizeof(afni_xml_t *));
   if( ! parent->xchild ) {
      fprintf(stderr,"** failed to alloc %d AXML pointers\n", parent->nchild);
      return 1;
   }
   parent->xchild[parent->nchild-1] = child;

   return 0;
}


static afni_xml_t * new_afni_xml(const char * ename, const char ** attr)
{
   afni_xml_t * newp;
   int          c, aind, natr;

   newp = (afni_xml_t *)calloc(1, sizeof(afni_xml_t));
   if( ! newp ) {
      fprintf(stderr,"** failed to alloc afni_xml_t struct\n");
      return NULL;
   }

   /* be picky */
   newp->name   = strdup(ename);
   newp->xdata  = NULL;
   newp->xchild = NULL;

   if( attr ) {
      for(c=0, natr=0; attr[c]; c += 2, natr++)
         ;  /* just count */

      newp->attrs.length = natr;

      if( natr == 0 ) { /* we are done */
         newp->attrs.name = NULL;
         newp->attrs.value = NULL;
         return newp;
      }

      newp->attrs.name = (char **)malloc(natr*sizeof(char *));
      newp->attrs.value = (char **)malloc(natr*sizeof(char *));

      /* failure? */
      if( ! newp->attrs.name || ! newp->attrs.value ) {
         fprintf(stderr,"** NAX: failed to alloc 2 sets of %d char*\n", natr);
         newp->attrs.length = 0;
         if( newp->attrs.name )  free(newp->attrs.name);
         if( newp->attrs.value ) free(newp->attrs.value);
         newp->attrs.name = newp->attrs.value = NULL;
         return newp;
      }

      /* and get the attributes */
      for(c = 0, aind = 0; attr[c]; c += 2, aind++) {
         newp->attrs.name[aind]  = strdup(strip_whitespace(attr[c]));
         newp->attrs.value[aind] = strdup(strip_whitespace(attr[c+1]));
      }
   }
 
   return newp;
}


int epop(afni_xml_control * xd, const char * ename)
{

   if( xd->dskip ) {
      if( xd->dskip == xd->depth ) xd->dskip = 0;  /* clear */

      if( xd->verb > 3 )
          fprintf(stderr,"-- skip=%d, depth=%d, skipping pop element '%s'\n",
                  xd->dskip, xd->depth, ename);
   }

   if( ! xd->dskip ) {
      /* rcr - do some work */
   }

   xd->depth--;

   if( xd->verb > 4 ) {
       show_depth(xd, 1);
       fprintf(stderr,"++ pop '%s'\n", ename);
   }

   return 0;
}


static int show_attrs(afni_xml_control * xd, const char * ename,
                                             const char ** attr)
{
   int count;
   for( count = 0; attr[count]; count += 2 ){
      show_depth(xd, 0);
      fprintf(stderr,"      attr: %s='%s'\n", attr[count], attr[count+1]);
   }
   return 0;
}

static char * strip_whitespace(const char * str, int slen)
{
   static char * buf = NULL;
   static int    blen = 0;
   int           len, ifirst, ilast;  /* first non-white char and AFTER last */

   if( !str ) return (char *)str;

   len = strlen(str);
   if( slen < len ) len = slen;
   if( len <= 0 ) return (char *)str;

   /* make sure we have local space */
   if( len > blen ) { /* allocate a bigger buffer */
      buf = (char *)realloc(buf, (len+1) * sizeof(char));
      if( !buf ) {
         fprintf(stderr,"** failed to alloc wspace buf of len %d\n", len+1);
         return (char *)str;
      }
      blen = len;
   }

   ifirst = white_first(str, len);
   ilast  = white_last(str, len);

   if( ifirst == len ) *buf = '\0';
   else {
      strncpy(buf, str+ifirst, len-ifirst-ilast);
      buf[len-ifirst-ilast] = '\0';
   }

   return buf;
}


static void XMLCALL cb_default(void * udata, const char * str, int length)
{
   afni_xml_control * xd = (afni_xml_control *)udata;
   int                wlen = white_first(str,length);
   int                len = length;

   if( len == wlen ) {
       if( xd->verb < 6 ) return;

       str = "whitespace";     /* just note the whitespace */
       len = strlen(str);
   }

   if( xd->verb > 5 ) {
       show_depth(xd, 1);
       fprintf(stderr, "default XML element [%d]: '%s'\n",length,
                       strip_whitespace(str, len));
   }
}

static int show_depth(afni_xml_control * ax, int show)
{
   FILE * fp = stderr;
   if( ax->stream ) fp = ax->stream;

   if( show ) fprintf(fp, "%*s %02d ", ax->indent*ax->depth, "", ax->depth);
   else       fprintf(fp, "%*s    ", ax->indent*ax->depth, "");
   return 0;
}


static int white_first(const char * str, int len)
{
    int c;
    if( !str || !*str || len < 1 ) return 0;
    for( c = 0; c < len; c++ )
        if( !isspace(str[c]) ) return c;

    return len;
}

static int white_last(const char * str, int len)
{
    int c;
    if( !str || !*str || len < 1 ) return 0;
    for( c = len-1; c >= 0; c-- )
        if( !isspace(str[c]) ) return (len-1-c);

    return len;
}

