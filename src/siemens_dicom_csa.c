
/* ----------------------------------------------------------------------
 * code that deals with the CSA(1/2) header in Siemens mosaic images
 *
 *                                             R Reynolds   May 2011
 *
 * see http://nipy.sourceforge.net/nibabel/dicom/siemens_csa.html
 * (which seems to be based on SPMs implementation)
 *
 * select notes from that page:
 *
 *      - the field contents are always little-endian
 *      - the CSA Image Header Info (0029 1010) field can be of two
 *        types, CSA1 and CSA2
 *
 * CSA1 format:
 *
 *   0029 1010                  : S8, 8 byte field type
 *   header:
 *      uint32 n_tags           : number of tags, in [1,128] else fail
 *      uint32 unused           : val == 77
 *   each tag:
 *      S64   name              : nul-terminated name string
 *      int32 vm                : ??
 *      S4    vr                : possibly nul-term string
 *      int32 syngodt           : ??
 *      int32 nitems            : number of items in CSA
 *      int32 xx                : maybe == 77 or 205
 *   each item:
 *      int32 xx[4]             : length (in bytes, maybe modified), ??, ??, ??
 *                                SPM: if xx[0]-nitems < 0 or > remain bytes:
 *                                     then terminate
 *      uint8 value[item_len]   : actual data, item_len = xx[0]-nitems
 *
 * CSA2 format:
 *
 *   0029 1010                  : S8, 8 byte field type
 *   header:
 *      S4     hdr_id           : val == "SV10"
 *      uint8  unused1[4]       :
 *      uint32 n_tags           : number of tags (in [1,128], else fail)
 *      uint32 unused2          : val == 77
 *   each tag (same as for CSA1):
 *      S64   name              : nul-terminated name string
 *      int32 vm                : ??
 *      S4    vr                : possibly nul-term string
 *      int32 syngodt           : ??
 *      int32 nitems            : number of items in CSA
 *      int32 xx                : maybe == 77 or 205
 *   each item:
 *      int32 xx[4]             : length (in bytes), ??, ??, ??
 *                                Note: interpretation differs from above
 *                                if xx[0] < 0 or > remain bytes, terminate
 *      uint8 value[xx[0]]      : actual data (length does not used nitems)
 *
 * ---------------------------------------------------------------------- */

typedef struct {
   unsigned int  ntags;         /* in [1,128], tags in CSA  */
   unsigned int  unused;        /* should be 77             */
} siemens_csa1_header;

typedef struct {
   char          hdr_id[4];     /* should be "SV10"         */
   unsigned char unused1[4];    /* in [1,128]               */
   unsigned int  ntags;         /* in [1,128], tags in CSA  */
   unsigned int  unused2;       /* should be 77             */
} siemens_csa2_header;

typedef struct {
   char          name[64];      /* nul-terminated string    */
   int           vm;            /* ??                       */
   char          vr[4];         /* first 3 chars usable     */
   int           syngodt;       /* ??                       */
   int           nitems;        /* items under current tag  */
   int           xx;            /* 77 or 205                */
} siemens_csa_tag;

typedef struct {
   int           xx[4];         /* length, ??, ??, ??       */
   unsigned char value[1];      /* data (len or len-nitems) */
} siemens_csa_item;

static void swap_4(char * data, int nquads)
{
   char * dp = data;
   char   c, index;
   for( index = 0; index < nquads; index++ ) {
      c = dp[0]; dp[0] = dp[3]; dp[3] = c;
      c = dp[1]; dp[1] = dp[2]; dp[2] = c;
      dp += 4;
   }
}

static void swap_csa1_header(siemens_csa1_header * hdr)
{
   swap_4((char *)&hdr->ntags, 1);
   swap_4((char *)&hdr->unused, 1);
}

static void swap_csa2_header(siemens_csa2_header * hdr)
{
   swap_4((char *)&hdr->ntags, 1);
   swap_4((char *)&hdr->unused2, 1);
}

static void swap_csa_tag(siemens_csa_tag * hdr)
{
   swap_4((char *)&hdr->vm, 1);
   swap_4((char *)&hdr->syngodt, 1);
   swap_4((char *)&hdr->nitems, 1);
   swap_4((char *)&hdr->xx, 1);
}

static void swap_csa_item(siemens_csa_item * hdr)
{
   swap_4((char *)&hdr->xx, 4);
}

static int little_endian(void)
{
   int val = 1;
   char * cp = (char *)&val;
   return *cp;
}

/* return 1 or 2, for CSA1 or CSA2 */
/* (CSA2 starts with SV10) */
static int get_csa_type(unsigned char * str)
{
   if( !strncmp((char *)str, "SV10", 4) ) return 2;
   else                                   return 1;
}

static int print_csa1_header(siemens_csa1_header * header, FILE * stream)
{
   int size = sizeof(siemens_csa1_header);
   fprintf(stream, "-- CSA 1 header (size = %d)\n", size);
   fprintf(stream, "   ntags   : %u (in [1,128]?)\n", header->ntags);
   fprintf(stream, "   unused  : %u (77?)\n", header->unused);
   fputc('\n', stream);

   return size;
}

static int print_csa2_header(siemens_csa2_header * hdr, FILE * stream)
{
   int size = sizeof(siemens_csa2_header);

   fprintf(stream, "-- CSA 2 header (size = %d)\n", size);
   fprintf(stream, "   hdr_id  : %4.4s (SV10?)\n", hdr->hdr_id);
   fprintf(stream, "   unused1 : %u %u %u %u\n", hdr->unused1[0],
                   hdr->unused1[1], hdr->unused1[2], hdr->unused1[3] );
   fprintf(stream, "   ntags   : %u (in [1,128]?)\n", hdr->ntags);
   fprintf(stream, "   unused2 : %u (77?)\n", hdr->unused2);
   fputc('\n', stream);

   return size;
}


static int print_csa_tag(siemens_csa_tag * tag, FILE * stream)
{
   int size = sizeof(siemens_csa_tag);

   fprintf(stream, "-- CSA tag (size = %d)\n", size);
   fprintf(stream,"   tag name  : %-64.64s\n", tag->name);
   fprintf(stream,"   vm        : %d\n", tag->vm);
   fprintf(stream,"   vr        : %-4.4s\n", tag->vr);
   fprintf(stream,"   syngodt   : %d\n", tag->syngodt);
   fprintf(stream,"   nitems    : %d\n", tag->nitems);
   fprintf(stream,"   xx        : %d (77 or 205?)\n", tag->xx);
   fputc('\n', stream);

   return size;
}

/* print 'mesg' and then 'data' of length 'len' to 'fp'
 * if 'text': print as characters
 */
static int print_raw_data(char * mesg, unsigned char * data, int len,
                          FILE * fp, int text)
{
   int mlen = 0, posn = 0, indent = 0, nbytes;

   if( mesg ) {
      mlen = strlen(mesg);
      fputs(mesg, fp);
   }

   while( posn < len ) {
      if( posn % 32 == 0 ) {
         if( mesg ) {   /* then indent, unless this is the first time */
            if( indent > 0 ) fprintf(fp, "\n%*s", indent, "");
            else indent = mlen;
         }
      }

      if( text && isprint(data[posn]) ) fputc(data[posn], fp);
      else if( text )                   fprintf(fp, " 0x%02x", data[posn]);
      else                              fprintf(fp, " %02x", data[posn]);

      posn++;
   }

   fputc('\n', fp);
   fputc('\n', fp);

   return 0;
}

/* return xx[0] rounded up to multiple of 4 */
static int csa_item_value_size_aligned(siemens_csa_item * citem, int ilenoff)
{
   int nbytes = citem->xx[0], nbytes_plus;
   int total_size;

   if( nbytes % 4 ) return nbytes + 4 - (nbytes % 4);
   else             return nbytes;
}

static int csa_item_size(siemens_csa_item * citem, int ilenoff)
{
   int size = 4*sizeof(int);    /* base offset */
   int nbytes = citem->xx[0], nbytes_plus;
   int total_size;

   total_size = size - ilenoff + csa_item_value_size_aligned(citem, ilenoff);

   return total_size;
}

/* print presumably nul-terminated string */
static int print_csa_item_str(siemens_csa_item * citem, int ilenoff, FILE *fp)
{
   int nbytes = citem->xx[0];

   if( nbytes > 0 ) fprintf(fp, " %.*s", nbytes, citem->value);

   return nbytes ;
}

/* - subtract ilenoff from length
 * - if data, print data (as text?)
 * - if remain is too small, terminate
 */
static int print_csa_item(siemens_csa_item * citem, int ilenoff, int data,
                          int remain, FILE * stream)
{
   int size = 4*sizeof(int);    /* base offset */
   int nbytes = citem->xx[0], nbytes_plus;
   int total_size;

   /* round nbytes up to next multiple of 4 */
   nbytes_plus = csa_item_value_size_aligned(citem, ilenoff);
   total_size = csa_item_size(citem, ilenoff);

   fprintf(stream, "-- CSA item (size = %d, total = %d, remain = %d)\n",
                   size, total_size, remain);
   fprintf(stream, "   xx        : %d (offset %d)\n", nbytes, ilenoff);
   fputc('\n', stream);

   if( total_size > remain ) {
      if(g_MDH_verb > 1) fprintf(stderr,"** PCSAI: insufficient space\n\n");
      return total_size;
   }

   if( data && nbytes > 0 )
      print_raw_data("   item data : ", citem->value, nbytes, stderr, 1);

   return total_size;
}

static int insert_slice_time(float st)
{
   siemens_slice_times_t * ST = & g_siemens_slice_times;  /* global struct */

   ST->nused++;
   if( ST->nused > ST->nalloc ) {
      ST->nalloc = ST->nused + 4;       /* alloc a few extra */
      ST->times = (float  *)realloc(ST->times, ST->nalloc*sizeof(float ));
      if( ! ST->times ) {
         fprintf(stderr,"** MIST: failed malloc of %d floats\n", ST->nalloc);
         ST->nused = 0;
         ST->nalloc = 0;
         return 1;
      }
   }

   ST->times[ST->nused-1] = st;

   return 0;
}

/* loop through all CSA data fields
 *
 * if name, only print that field contents
 * if data, print item data (1=raw, 2=text)
 * verb is verbose level
 *
 */
static int process_csa_data(unsigned char * str, int len, int verb, int data,
                            char * name)
{
   siemens_csa1_header * c1ptr = (siemens_csa1_header *)str;
   siemens_csa2_header * c2ptr = (siemens_csa2_header *)str;
   siemens_csa_tag     * ctag  = NULL;
   siemens_csa_item    * citem = NULL;
   unsigned char       * ucp   = (unsigned char *)str;  /* main pointer */
   unsigned int          itag, ntags;
   char                * endp;
   float                 stime = 0.0;
   int                   tagsize = sizeof(siemens_csa_tag);
   int                   posn, ctype, remain, nitem, iitem, ilen, ilenoff=-1;
   int                   little = little_endian();

   if( !str ) {
      if(verb > 1) fprintf(stderr,"** PACSAD: bad data pointer\n");
      return 1;
   }

   if( verb > 2 ) {
      if( little ) fprintf(stderr,"-- little endian, so no swapping\n");
      else         fprintf(stderr,"-- big endian, will swap structures\n");
   }

   ctype = get_csa_type(ucp);
   posn = 0;  /* current index into ucp/str */
   if( ctype == 1 ) {
      if( ! little ) swap_csa1_header(c1ptr);
      if( verb > 1 ) print_csa1_header(c1ptr, stderr);
      posn += sizeof(siemens_csa1_header);
      ntags = c1ptr->ntags;
   } else {
      if( ! little ) swap_csa2_header(c2ptr);
      if( verb > 1 ) print_csa2_header(c2ptr, stderr);
      posn += sizeof(siemens_csa2_header);
      ntags = c2ptr->ntags;
   }

   if( ntags > 128 ) {
      if(verb > 1) fprintf(stderr,"** PACSAD: ntags (%d) > 128\n", ntags);
      return 1;
   }

   for( itag = 0; itag < ntags; itag++ ) {
      remain = len - posn;
      if( remain < tagsize ) {
         if(verb>1) fprintf(stderr,"** PACSAD: no room for next tag\n");
         return 1;
      }
      ctag = (siemens_csa_tag *)(ucp+posn);
      if( ! little ) swap_csa_tag(ctag);
      if( data || verb > 2 ) print_csa_tag(ctag, stderr);

      /* check for our name of interest */
      if ( name && !strcmp(ctag->name, name) ) {
         if( verb > 1 ) print_csa_tag(ctag, stderr);
         g_siemens_slice_times.nused = 0;  /* clear slice times */
      }

      posn += sizeof(siemens_csa_tag);

      /* if CSA1 and FIRST tag, note the item length offset */
      if( ilenoff < 0 ) {
         if( ctype == 1 ) {
            ilenoff = ctag->nitems;
            if(verb > 2) fprintf(stderr,"-- CSA1 item offset = %d\n",ilenoff);
            if( ilenoff < 0 ) ilenoff = 0;
         } else ilenoff = 0;
      }

      for( iitem = 0; iitem < ctag->nitems; iitem++ ) {
         citem = (siemens_csa_item *)(ucp+posn);
         if( ! little ) swap_csa_item(citem);
         if ( name && !strcmp(ctag->name, name) ) {

            if(verb==3) print_csa_item(citem, ilenoff, data, len-posn, stderr);
            if( verb > 1 ) print_csa_item_str(citem, ilenoff, stderr);

            /* our main purpose, actually insert each slice time */
            stime = strtod((char *)citem->value, &endp);
            if( endp && endp > (char *)citem->value )
               insert_slice_time(stime);
            else if (verb > 2) fprintf(stderr," <empty item>");
         }
         if( data || verb > 3 )
            print_csa_item(citem, ilenoff, data, len-posn, stderr);
         posn += csa_item_size(citem, ilenoff);
         if( posn > len ) break;
      }

      /* if we have processed our tag of interest, we're done */
      if ( name && !strcmp(ctag->name, name) ) {
         if( verb > 1 ) fputc('\n', stderr);
         if( verb < 5 ) return 0;  /* else print everything */
      }
   }

   return 0;
}

/* quick strstr function, but limited by len (not NUL char) */
static char * findstr(char * instr, char * sstr, int len)
{
   char * cptr = instr;
   int    posn = 0, slen;

   if( !instr || !sstr || len <= 0 ) return NULL;

   slen = strlen(sstr);
   len -= slen;

   while( posn <= len ){
      if( *cptr != *sstr ) { cptr++; posn++; continue; }
      if( ! strncmp(cptr, sstr, slen) ) return cptr;
      else { cptr++; posn++; }
   }

   return NULL;
}


/* - search field '0x0029 1010' for string MosaicRefAcqTimes
 * - from there, find text formatted floats
 * - stop at AutoInlineImageFilterEnabled, if found
 */
static int check_for_mosaic_slice_times(PRV_ELEMENT_ITEM * elementItem)
{
   unsigned el_gr = DCM_TAG_GROUP(elementItem->element.tag);
   unsigned el_el = DCM_TAG_ELEMENT(elementItem->element.tag);
   int      el_len = elementItem->element.length;
   char     start_txt[] = "MosaicRefAcqTimes";
   char     end_txt[]   = "AutoInlineImageFilterEnabled";
   siemens_slice_times_t * ST = & g_siemens_slice_times;  /* global struct */

   char * instr, * mstr;    /* input string and Mosaic string addr         */
   char * s2;               /* second search string, posn of AutoInline... */
   char * pstr;             /* position pointer, for reading times         */
   int    rem, rem2 = 0;    /* remainder counts                            */
   int    off, c, rv, diff; /* offset and counter vars                     */
   float  stime;            /* any read slice time                         */


   /* if this is not the correct element, nothing to do */
   if( el_gr != 0x0029 || el_el != 0x1010 ) return 0;

   /* we are in the correct field, start by clearing old results */
   ST->nused = 0;

   /* input string is field text, mstr is resulting MosaicRef text pointer */
   instr = (char *)elementItem->element.d.ot;
   mstr = findstr(instr, start_txt, el_len);

   if( ! mstr ) {
      if( g_MDH_verb > 2 ) fprintf(stderr, "-- CFMST, no Mosaic string\n");
      return 0;
   }

   off = mstr - instr;  /* offset of Mosaic string into field */
   rem = el_len - off;  /* remaining length of field */

   /* secondary remainder to be until any AutoInline... string */
   s2 = findstr(mstr, end_txt, rem);
   if( s2 ) rem2 = s2 - mstr;
   else     rem2 = 0;

   if( g_MDH_verb > 1 )
      fprintf(stderr, "== found %s in 0x0029 1010\n"
              "   off = %d of %d bytes (rem %d, %d)\n",
              start_txt, off, el_len, rem, rem2);

   if( s2 ) rem = rem2;  /* after verbose, update remaining length */

   if( rem <= 0 ) return 0;

   process_csa_data((unsigned char *)instr, el_len, g_MDH_verb,
                                            g_MDH_verb>3, start_txt);

   /* in really verbose mode, print out raw text */
   if( g_MDH_verb > 3 ) {
      unsigned char * ucp = (unsigned char *)mstr;
      fprintf(stderr, "-- remaining spaced hex or digit or '.' :\n");
      for(c=0; c<rem; c++)
        if( isdigit(mstr[c]) || mstr[c]=='.' ) fprintf(stderr," '%c'",mstr[c]);
        else fprintf(stderr," %02x", ucp[c]);
      fprintf(stderr, "(end)done\n");
   }

   if( g_MDH_verb > 1 )
      fprintf(stderr,"\n++ found %d slice times\n", ST->nused);

   return 0;
}
