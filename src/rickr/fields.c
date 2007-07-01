
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "nifti1.h"
#include "fields.h"

/* these may come from someplace else in the future */
static int swap_2 ( void * ptr );
static int swap_4 ( void * ptr );


/* ---------------------------------------------------------------------- */
/* dupe some functions from nifti_tool.c : this is a library file         */
/* ---------------------------------------------------------------------- */
static int gft_verb = 1;

int  get_field_verb( void )      { return gft_verb;  }
void set_field_verb( int level ) { gft_verb = level; }

/*----------------------------------------------------------------------
 * fill the nifti_image field list
 *----------------------------------------------------------------------*/
field_s * make_ana_field_array( void )
{
   ft_analyze_header   ah;
   field_s           * fields, * af;
   int                 rv, errs;

   fields = (field_s *)calloc(FT_ANA_NUM_FIELDS, sizeof(field_s));
   if( !fields ) {
      fprintf(stderr,"** MAFA: failed to alloc %d fields\n",FT_ANA_NUM_FIELDS);
      return NULL;
   }

   errs = 0;

   af = fields;
   FT_SFILL(ah, af, DT_INT32,     sizeof_hdr,     1, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, data_type,     10, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, db_name,       18, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     extents,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     session_error,  1, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, regular,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT8,      hkey_un0,       1, rv);  errs += rv;

   FT_SFILL(ah, af, DT_INT16,     dim,            8, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused8,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused9,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused10,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused11,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused12,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused13,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     unused14,       1, rv);  errs += rv;

   FT_SFILL(ah, af, DT_INT16,     datatype,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     bitpix,         1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     dim_un0,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   pixdim,         8, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   vox_offset,     1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   funused1,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   funused2,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   funused3,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   cal_max,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   cal_min,        1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   compressed,     1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_FLOAT32,   verified,       1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     glmax,          1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     glmin,          1, rv);  errs += rv;

   FT_SFILL(ah, af, FT_DT_STRING, descrip,       80, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, aux_file,      24, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT8,      orient,         1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT16,     originator,     5, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, generated,     10, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, scannum,       10, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, patient_id,    10, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, exp_date,      10, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, exp_time,      10, rv);  errs += rv;
   FT_SFILL(ah, af, FT_DT_STRING, hist_un0,       3, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     views,          1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     vols_added,     1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     start_field,    1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     field_skip,     1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     omax,           1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     omin,           1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     smax,           1, rv);  errs += rv;
   FT_SFILL(ah, af, DT_INT32,     smin,           1, rv);  errs += rv;


   if( errs > 0 ){
      fprintf(stderr, "** %d fill_fields errors "
                      "(note that pointers get aligned)\n", errs);
      free(fields);
      return NULL;
   }

   if( gft_verb > 3 )  /* failure here is not an error condition */
       check_total_size("ft_analyze_header test: ", fields,
                        FT_ANA_NUM_FIELDS, sizeof(ah));

   if( gft_verb > 3 )
      disp_field_s_list("analyze_fields: ", fields, FT_ANA_NUM_FIELDS);

   return fields;
}


/*----------------------------------------------------------------------
 * compare sizes to offset, including total
 *----------------------------------------------------------------------*/
int check_total_size( char * mesg, field_s * fields, int nfields, int tot_size ){
   field_s * fp;
   int       c, total;
   int       bad_offs;

   total = 0;
   bad_offs = 0;
   for( c = 0, fp = fields; c < nfields; c++, fp++ ){
      if( fp->offset != total ){
         if( gft_verb > 2 )
            fprintf(stderr,"** bad offset for field '%s'\n"
                           "   offset = %d, total = %d\n",
                           fp->name, fp->offset, total);
         bad_offs++;
      }

      total += fp->size * fp->len;
   }

   if( gft_verb > 1  || (gft_verb > 0 && bad_offs > 0) ){
      fputs(mesg, stderr);  c = 0;
      if( bad_offs > 0 ){
         fprintf(stderr,"** found %d bad offsets\n", bad_offs);  c++; }
      if( total != tot_size ){
         fprintf(stderr,"** computed total %d not equal to struct size %d\n",
                 total, tot_size);   c++; }
      if( c == 0 ) fputs("... okay\n", stderr);
   }

   if( bad_offs > 0 ) return 1;

   return 0;
}


/*----------------------------------------------------------------------
 * fill the field structure with the given data
 *----------------------------------------------------------------------*/
int fill_field( field_s * fp, int type, int offset, int num, char * name )
{
   fp->type   = type;
   fp->offset = offset;
   fp->size   = 1;     /* init before check */
   fp->len    = num;

   strncpy(fp->name, name, FT_FIELD_NAME_LEN-1);

   switch( type ){
      case DT_UNKNOWN:
      case DT_INT8:
      case FT_DT_STRING:
         fp->size = 1;
         break;

      case DT_INT16:
         fp->size = 2;
         break;

      case DT_INT32:
      case DT_FLOAT32:
         fp->size = 4;
         break;

      case FT_DT_POINTER:
      case FT_DT_CHAR_PTR:
         fp->size = (int)sizeof(void *);
         break;

      default:
         fprintf(stderr,"** fill_field: invalid type %d\n", type );
         return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
char * field_type_str( int type )
{
   if( type == DT_INT8 )        return "DT_INT8";
   if( type == DT_INT16 )       return "DT_INT16";
   if( type == DT_INT32 )       return "DT_INT32";
   if( type == DT_FLOAT32 )     return "DT_FLOAT32";
   if( type == FT_DT_STRING )   return "FT_DT_STRING";
   if( type == FT_DT_POINTER )  return "FT_DT_POINTER";
   if( type == FT_DT_CHAR_PTR ) return "FT_DT_CHAR_PTR"; /* longest: 14 */

   return "DT_UNKNOWN";  /* for DT_UNKNOWN, or as an else */
}

#define FT_MAX_DT_STR_LEN 14

/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
int disp_field_s_list( char * mesg, field_s * fp, int nfields )
{
   int c;

   if( mesg ) fputs(mesg, stdout);

   fprintf(stdout," %d fields:\n"
           "   name                  size   len   offset   type\n"
           "   -------------------   ----   ---   ------   --------------\n",
           nfields);

   for( c = 0; c < nfields; c++, fp++ )
      fprintf(stdout,"   %-*s  %4d    %3d   %4d     %-14s\n",
                     FT_FIELD_NAME_LEN-1, fp->name, fp->size, fp->len,
                     fp->offset, field_type_str(fp->type));

   return 0;
}


/*----------------------------------------------------------------------
 * display the contents of all of the field structures
 *----------------------------------------------------------------------*/
int disp_field(char *mesg, field_s *fieldp, void * str, int nfields,
               int header, int hex)
{
   field_s * fp;
   int       c;

   if( mesg ) fputs(mesg, stdout);

   if( header && gft_verb > 0 ){
      fprintf(stdout, "  name                offset  nvals  values\n");
      fprintf(stdout, "  ------------------- ------  -----  ------\n");
   }

   fp = fieldp;
   for( c = 0; c < nfields; c++, fp++ )
   {
      /* start by displaying the field information */
      if( gft_verb > 0 )
         fprintf(stdout, "  %-*.*s %4d    %3d    ",
                      FT_FIELD_NAME_LEN-1, FT_FIELD_NAME_LEN-1, fp->name,
                      fp->offset, fp->len);

      /* now, print the value(s), depending on the type */
      switch( fp->type ){
         case DT_UNKNOWN:
         default:
            fprintf(stdout,"(unknown data type)\n");
            break;

         case DT_INT8:    case DT_UINT8:
         case DT_INT16:   case DT_UINT16:
         case DT_INT32:   case DT_UINT32:
         case DT_FLOAT32: case DT_FLOAT64:
            disp_raw_data((char *)str+fp->offset, fp->type, fp->len,' ',1,hex);
            break;

         case FT_DT_POINTER:
            fprintf(stdout,"(raw data of unknown type)\n");
            break;

         case FT_DT_CHAR_PTR:  /* look for string of length <= 40 */
         {
            char * sp;
            int    len;

            /* start by sucking the pointer stored here */
            sp = *(char **)((char *)str + fp->offset);

            if( hex ) {
                disp_raw_data(sp, fp->type, fp->len, ' ', 1, hex);
                break;
            }

            if( ! sp ){ fprintf(stdout,"(NULL)\n");  break; }  /* anything? */

            /* see if we have a printable string here */
            for(len = 0; len <= 40 && *sp && isprint(*sp); len++, sp++ )
               ;
            if( len > 40 )
               fprintf(stdout,"(apparent long string)\n");
            else if ( len == 0 )
               fprintf(stdout,"(empty string)\n");
            else if( *sp && !isprint(*sp) )  /* if no termination, it's bad */
               fprintf(stdout,"(non-printable string)\n");
            else  /* woohoo!  a good string */
               fprintf(stdout,"'%.40s'\n",*(char **)((char *)str + fp->offset));            break;
         }

         case FT_DT_STRING:
         {
            char * charp = (char *)str + fp->offset;
            if( hex ) disp_raw_data(charp, fp->type, fp->len, ' ', 1, hex);
            else      fprintf(stdout,"%.*s\n", fp->len, charp);
            break;
         }
      }
   }

   return 0;
}


/*----------------------------------------------------------------------
 * no display, just return whether any fields differ
 *----------------------------------------------------------------------*/
int diff_field(field_s *fieldp, void * str0, void * str1, int nfields)
{
   field_s * fp;
   char    * cp0, * cp1;
   int       fnum, c, size;

   fp = fieldp;
   for( fnum = 0; fnum < nfields; fnum++, fp++ )
   {
      switch( fp->type ){
         case DT_UNKNOWN:     /* all basic types are easy */
         case DT_INT8:
         case DT_INT16:
         case DT_INT32:
         case DT_FLOAT32:
         case FT_DT_STRING:
            size = fp->size * fp->len;  /* total field size */
            cp0 = (char *)str0 + fp->offset;
            cp1 = (char *)str1 + fp->offset;
            for( c = 0; c < size; c++, cp0++, cp1++ )
               if( *cp0 != *cp1 ) break;

            if(c < size) return 1;  /* found a diff */

            break;

         case FT_DT_POINTER:     /* let's pass on these - no diff */
         case FT_DT_CHAR_PTR:

            break;
      }
   }

   return 0;   /* no diffs found */
}

/*----------------------------------------------------------------------
 * modify all fields in the list
 *----------------------------------------------------------------------*/
int modify_many_fields( void * basep, str_list * names, str_list * vals,
                        field_s * fields, int flen)
{
   field_s * fp;
   int       fc, lc;  /* field and list counters */

   if( !basep || !names || !vals || !fields ) {
      fprintf(stderr,"** MMF: NULL params\n");  return 1;
   }

   if( names->len <= 0 ) return 0;
   if( names->len != vals->len ){
      fprintf(stderr,"** MMF: have %d fields but %d new values\n",
              names->len, vals->len);
      return 1;
   }

   for( lc = 0; lc < names->len; lc++ )
   {
      /* is it in the list? */
      fp = fields;
      for( fc = 0; fc < flen; fc++, fp++ )
         if( strcmp(names->list[lc], fp->name) == 0 ) break;

      if( fc == flen )    /* do no modifications on failure */
      {
         fprintf(stderr,"** field '%s' not found in structure\n",
                 names->list[lc]);
         return 1;
      }

      if( modify_field( basep, fp, vals->list[lc]) )
         return 1;
   }

   return 0;
}


/*----------------------------------------------------------------------
 * modify a single field with the given value field
 *
 * pointer fields are not allowed here
 *----------------------------------------------------------------------*/
int modify_field(void * basep, field_s * field, char * data)
{
   float   fval;
   char  * posn = data;
   int     val, max, fc, nchars;

   if( gft_verb > 1 )
      fprintf(stderr,"+d modifying field '%s' with '%s'\n", field->name, data);

   if( !data || strlen(data) == 0 )
   {
      fprintf(stderr,"** no data for '%s' field modification\n",field->name);
      return 1;
   }

   switch( field->type )
   {
         case DT_UNKNOWN:
         case FT_DT_POINTER:
         case FT_DT_CHAR_PTR:
         default:
            fprintf(stderr,"** refusing to modify a pointer field, '%s'\n",
                    field->name);
            return 1;

         case DT_INT8:
         {
            max = 127;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               if( val > max || val < -(max+1) )
               {
                  fprintf(stderr,
                    "** mod val #%d (= %d) outside byte range [-%d,%d]\n",
                    fc, val, max+1, max);
                  return 1;
               }
               /* otherwise, we're good */
               (((char *)basep + field->offset))[fc] = (char)val;
               if( gft_verb > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT16:
         {
            max = 32767;
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               if( val > max || val < -(max+1) )
               {
                  fprintf(stderr,
                    "** mod val #%d (= %d) outside byte range [-%d,%d]\n",
                    fc, val, max+1, max);
                  return 1;
               }
               /* otherwise, we're good */
               ((short *)((char *)basep + field->offset))[fc] = (short)val;
               if( gft_verb > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_INT32:
         {
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %d%n", &val, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               ((int *)((char *)basep + field->offset))[fc] = val;
               if( gft_verb > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %d\n",
                          fc, field->name, val);
               posn += nchars;
            }
         }
         break;

         case DT_FLOAT32:
         {
            for( fc = 0; fc < field->len; fc++ )
            {
               if( sscanf(posn, " %f%n", &fval, &nchars) != 1 )
               {
                  fprintf(stderr,"** found %d of %d modify values\n",
                          fc,field->len);
                  return 1;
               }
               /* otherwise, we're good */
               ((float *)((char *)basep + field->offset))[fc] = fval;
               if( gft_verb > 1 )
                  fprintf(stderr,"+d setting posn %d of '%s' to %f\n",
                          fc, field->name, fval);
               posn += nchars;
            }
         }
         break;

         case FT_DT_STRING:
         {
            char * dest = (char *)basep + field->offset;
            nchars = strlen(data);
            strncpy(dest, data, field->len);
            if( nchars < field->len )  /* clear the rest */
               memset(dest+nchars, '\0', field->len-nchars);
         }
         break;
   }

   return 0;
}


/* display the given number of values of the given type */
int disp_raw_data( void * data, int type, int nvals, char space, int newline,
                   int hex )
{
   char * dp, fbuf[32];
   int    c, c2, size;

   ft_datatype_sizes( type, &size, NULL );   /* get nbyper */

   for( c = 0, dp = (char *)data; c < nvals; c++, dp += size )
   {
      if( hex ) {
         for( c2 = 0; c2 < size; c2++ )
            printf("%02x",*(unsigned char *)(dp+c2));
      } else {
          switch( type )
          {
             case DT_INT8:
                   printf("%d", *(char *)dp);
                   break;
             case DT_INT16:
                   printf("%d", *(short *)dp);
                   break;
             case DT_INT32:
                   printf("%d", *(int *)dp);
                   break;
             case DT_UINT8:
                   printf("%u", *(unsigned char *)dp);
                   break;
             case DT_UINT16:
                   printf("%u", *(unsigned short *)dp);
                   break;
             case DT_UINT32:
                   printf("%u", *(unsigned int *)dp);
                   break;
             case DT_FLOAT32:
             {
                   sprintf(fbuf,"%f", *(float *)dp);
                   clear_float_zeros(fbuf);
                   printf("%s", fbuf);
                   break;
             }
             case DT_FLOAT64:
             {
                   sprintf(fbuf,"%f", *(double *)dp);
                   clear_float_zeros(fbuf);
                   printf("%s", fbuf);
                   break;
             }
             default:
                   fprintf(stderr,"** disp_raw_data: unknown type %d\n", type);
                   return 1;
          }
      }
      if( c < nvals - 1 ) fputc(space,stdout);
   }

   if ( newline ) fputc('\n',stdout);

   return 0;
}


/*----------------------------------------------------------------------
 * remove trailing zeros from string of printed float
 * return  1 if something was cleared
 *         0 if not
 *----------------------------------------------------------------------*/
int clear_float_zeros( char * str )
{
   char * dp  = strchr(str, '.'), * valp;
   int    len;

   if( !dp ) return 0;      /* nothing to clear */

   len = strlen(dp);

   /* never clear what is just to the right of '.' */
   for( valp = dp+len-1; (valp > dp+1) && (*valp==' ' || *valp=='0'); valp-- )
       *valp = '\0';     /* clear, so we don't worry about break conditions */

   if( valp < dp + len - 1 ) return 1;
   return 0;
}


void ft_datatype_sizes( int datatype , int *nbyper, int *swapsize )
{
   int nb=0, ss=0 ;
   switch( datatype ){
     default:
     case DT_INT8:
     case DT_UINT8:       nb =  1 ; ss =  0 ; break ;

     case DT_INT16:
     case DT_UINT16:      nb =  2 ; ss =  2 ; break ;

     case DT_RGB24:       nb =  3 ; ss =  0 ; break ;

     case DT_INT32:
     case DT_UINT32:
     case DT_FLOAT32:     nb =  4 ; ss =  4 ; break ;

     case DT_COMPLEX64:   nb =  8 ; ss =  4 ; break ;

     case DT_FLOAT64:
     case DT_INT64:
     case DT_UINT64:      nb =  8 ; ss =  8 ; break ;

     case DT_FLOAT128:    nb = 16 ; ss = 16 ; break ;

     case DT_COMPLEX128:  nb = 16 ; ss =  8 ; break ;

     case DT_COMPLEX256:  nb = 32 ; ss = 16 ; break ;
   }

   /* finally, set any requested size */
   if( nbyper ) *nbyper = nb;
   if( swapsize ) *swapsize = ss;
}


/*----------------------------------------------------------------------
 * - do not duplicate the string
 * - only bother to alloc one pointer at a time (don't need efficiency here)
 * - return 0 on success
 *----------------------------------------------------------------------*/
int add_string(str_list * slist, char * str)
{
   if( slist->len == 0 ) slist->list = NULL;  /* just to be safe */
   slist->len++;
   slist->list = (char **)realloc(slist->list,slist->len*sizeof(char *));
   if( ! slist->list ){
      fprintf(stderr,"** failed to alloc %d (char *) elements\n",slist->len);
      return -1;
   }

   slist->list[slist->len-1] = str;

   return 0;
}


/* ---------------------------------------------------------------------- */
/* end nifti_tool.c functions                                             */
/* ---------------------------------------------------------------------- */


/*----------------------------------------------------------------------
 * swap the contents of all numerical fields
 *----------------------------------------------------------------------*/
int swap_fields(field_s *fieldp, void * str, int nfields)
{
   field_s * fp;
   int       c, c2;

   fp = fieldp;
   for( c = 0; c < nfields; c++, fp++ )
   {
      switch( fp->type ){
         default:       /* ignore anything we don't know about */
            break;

         case DT_INT16:   case DT_UINT16:
            for( c2 = 0; c2 < fp->len; c2++ )
                swap_2((char *)str+fp->offset+2*c2);
            break;
         case DT_INT32:   case DT_UINT32:
         case DT_FLOAT32:
            for( c2 = 0; c2 < fp->len; c2++ )
                swap_4((char *)str+fp->offset+4*c2);
            break;
         case DT_FLOAT64:
            for( c2 = 0; c2 < 2*fp->len; c2++ ) /* 4 at a time */
                swap_4((char *)str+fp->offset+4*c2);
            break;
      }
   }

   return 0;
}


/*------------------------------------------------------------
 * Reverse the order of the 4 bytes at this address.
 *------------------------------------------------------------
*/
static int swap_4( void * ptr )            /* destructive */
{
   unsigned char * addr = ptr;

   addr[0] ^= addr[3]; addr[3] ^= addr[0]; addr[0] ^= addr[3];
   addr[1] ^= addr[2]; addr[2] ^= addr[1]; addr[1] ^= addr[2];

   return 0;
}

/*------------------------------------------------------------
 * Reverse the order of the 2 bytes at this address.
 *------------------------------------------------------------
*/
static int swap_2( void * ptr )            /* destructive */
{
   unsigned char * addr = ptr;

   addr[0] ^= addr[1]; addr[1] ^= addr[0]; addr[0] ^= addr[1];

   return 0;
}


