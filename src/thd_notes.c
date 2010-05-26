/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
                         /**************************
                          * 3dNotes - T. Ross 8/99 *
                          * adapted by RWCox  9/99 *
                          **************************/

#include <sys/utsname.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>

/*---------------------------------------------------------------------------*/
/*!  Assemble a sort-of command line string from the arguments;
     free() this when done.
-----------------------------------------------------------------------------*/

char * tross_commandline( char *pname , int argc , char **argv )
{
   char *ch ;
   int ii , ll ;

   if( argc < 1 || argv == NULL ) return NULL ; /* ZSS, changed argc < 2 to argc < 1 */

   if( pname == NULL ) pname = argv[0] ;

   ii = strlen(pname) ; ch = AFMALL(char, ii+4) ; strcpy(ch,pname) ;
   if (argc < 2) {  /* ZSS */
      /* no options, get the hell outa here */
      return ch ;
   }
   for( ii=1 ; ii < argc ; ii++ ){
      if( argv[ii] == NULL || argv[ii][0] == '\0' ) continue ; /* skip */

      ll = strlen(argv[ii]) ;
      ch = AFREALL(ch ,char, strlen(ch)+ll+4 ) ;  /* expand output array */

      if( !THD_filename_ok(argv[ii]) ){       /* bad characters? */
         int jj ; char * aa = AFMALL(char, ll+1) ;

         strcpy(aa,argv[ii]) ;        /* edit out bad characters */
         for( jj=0 ; jj < ll ; jj++ )
            if( iscntrl(aa[jj]) ||
                isspace(aa[jj]) || (aa[jj] & 128) != 0 ) aa[jj] = ' ' ;

         strcat(ch," '") ; strcat(ch,aa) ; strcat(ch,"'") ; free(aa) ;

      } else {
         strcat(ch," ")  ; strcat(ch,argv[ii]) ;   /* just copy it */
      }
   }

   return ch ;
}

/*---------------------------------------------------------------------------*/
/*!  Get the current date/time string;  free() this when done.
-----------------------------------------------------------------------------*/

char * tross_datetime(void)
{
   time_t tnow = time(NULL) ; int i ; char * qh , * ch ;

   ch=ctime(&tnow); i=strlen(ch); qh=AFMALL(char, i+2); 
   strcpy(qh,ch); qh[i-1]='\0';
   return qh ;
}

/*---------------------------------------------------------------------------*/

#undef  NNAME
#define NNAME 1025
char * tross_hostname(void)  /* 19 Sep 1999 */
{
   char * cn = AFMALL(char, NNAME) ;
   gethostname( cn , NNAME ) ;
   return cn ;
}

/*---------------------------------------------------------------------------*/

#include <pwd.h>

char * tross_username(void)  /* 20 Sep 1999 */
{
   uid_t uu = getuid() ;
   struct passwd * pwd = getpwuid(uu) ;
   char * cn = AFMALL(char, NNAME) ;

   if( pwd == NULL ) strcpy(cn,"nobody") ;
   else              strcpy(cn,pwd->pw_name) ;
   return cn ;
}

/*---------------------------------------------------------------------------*/
/*!   Add a note after the last current note
-----------------------------------------------------------------------------*/

void tross_Add_Note( THD_3dim_dataset *dset, char *cn )
{
   ATR_int *notecount;
   int num_notes;
   char note_name[20], *ch ;

   if( !ISVALID_DSET(dset) || cn == NULL || cn[0] == '\0' ) return ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) {
      num_notes = 1;
      THD_set_int_atr(dset->dblk, "NOTES_COUNT", 1, &num_notes);
   } else {
      num_notes = notecount->in[0] + 1;
      if( num_notes > MAX_DSET_NOTES ){
         fprintf(stderr,"*** attempt to add too many notes to dataset!\n") ;
         return ;
      }
      notecount->in[0]++;
   }

   sprintf(note_name, "NOTE_NUMBER_%03d", num_notes);
   ch = tross_Encode_String(cn) ; if( ch == NULL ) return ;
   THD_set_string_atr(dset->dblk, note_name, ch);
   free(ch) ;

   ch = tross_datetime() ;
   sprintf(note_name, "NOTE_DATE_%03d", num_notes) ;
   THD_set_string_atr(dset->dblk, note_name, ch);
   free(ch);

   return ;
}

/*---------------------------------------------------------------------------*/
/*!   Delete a particular note
-----------------------------------------------------------------------------*/

void tross_Delete_Note(THD_3dim_dataset *dset, int inote)
{
   ATR_int *notecount;
   int num_notes;
   ATR_string *note_text;
   char note_name[20];

   if( !ISVALID_DSET(dset) || inote <= 0 || inote > MAX_DSET_NOTES ) return ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) return ;

   num_notes = notecount->in[0];
   if (inote > num_notes) return ;

   sprintf(note_name, "NOTE_NUMBER_%03d", inote);
   note_text = THD_find_string_atr(dset->dblk, note_name);
   if( note_text == NULL ) return ;
   THD_erase_one_atr( dset->dblk , note_name );

   sprintf(note_name, "NOTE_DATE_%03d", inote);
   note_text = THD_find_string_atr(dset->dblk, note_name);
   if( note_text != NULL ) THD_erase_one_atr( dset->dblk , note_name );

   notecount->in[0]-- ;  /* where the note count is reduced */

   /* Slide the higher numbered notes down */

   while (inote < num_notes) {
      /* find the next note */
      sprintf(note_name, "NOTE_NUMBER_%03d", inote+1);
      note_text=THD_find_string_atr(dset->dblk, note_name);
      if (note_text != NULL){
         /* rename to the previous name */
         sprintf(note_name, "NOTE_NUMBER_%03d", inote);
         strcpy(note_text->name, note_name);
      }

      sprintf(note_name,"NOTE_DATE_%03d",inote+1) ;
      note_text = THD_find_string_atr(dset->dblk, note_name);
      if (note_text != NULL){
         /* rename to the previous name */
         sprintf(note_name, "NOTE_DATE_%03d", inote);
         strcpy(note_text->name, note_name);
      }

      inote++ ;
   }

   /* No notes left, so remove count attribute */
   if (num_notes == 1)
      THD_erase_one_atr( dset->dblk, "NOTES_COUNT");
   return ;
}

/*---------------------------------------------------------------------------*/
/*! Get the number of Notes currently attached to the dataset.
    Doesn't include the History note.
-----------------------------------------------------------------------------*/

int tross_Get_Notecount( THD_3dim_dataset * dset )
{
   ATR_int *notecount;

   if( !ISVALID_DSET(dset) ) return -1 ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) return 0 ;
   return notecount->in[0];
}

/*---------------------------------------------------------------------------*/
/*! Get the inote-th Note attached to the dataset.
    free() this string when done with it - it is a copy
-----------------------------------------------------------------------------*/

char * tross_Get_Note( THD_3dim_dataset * dset , int inote )
{
   ATR_int *notecount;
   int num_notes;
   ATR_string *note ;
   char note_name[20], * ch ;

   if( !ISVALID_DSET(dset) || inote <= 0 || inote > MAX_DSET_NOTES ) return NULL ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) return NULL ;
   num_notes = notecount->in[0];
   if( inote > num_notes ) return NULL ;

   sprintf(note_name, "NOTE_NUMBER_%03d", inote);
   note = THD_find_string_atr(dset->dblk, note_name);
   if (note == NULL ) return NULL ;
   ch = tross_Expand_String( note->ch ) ;
   return ch ;
}

/*------------------------------------------------------------------------------*/

char * tross_Get_Notedate( THD_3dim_dataset * dset , int inote )
{
   ATR_int *notecount;
   int num_notes;
   ATR_string *note ;
   char note_name[20];

   if( !ISVALID_DSET(dset) || inote <= 0 || inote > MAX_DSET_NOTES ) return NULL ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) return NULL ;
   num_notes = notecount->in[0];
   if( inote > num_notes ) return NULL ;

   sprintf(note_name, "NOTE_DATE_%03d", inote);
   note = THD_find_string_atr(dset->dblk, note_name);
   if (note == NULL ) return NULL ;
   return tross_Expand_String( note->ch ) ;
}

/*---------------------------------------------------------------------------*/
/*! Add the history from the command line to the dataset.
-----------------------------------------------------------------------------*/

void tross_Make_History( char *pname, int argc, char **argv, THD_3dim_dataset *dset )
{
   char *ch ;

   if( argc < 2 || argv == NULL || !ISVALID_DSET(dset) ) return ;

   ch = tross_commandline( pname , argc , argv ) ; if( ch == NULL ) return ;
   tross_Append_History( dset , ch ) ;
   free(ch) ; return ;
}

/*---------------------------------------------------------------------------*/
/*!  Replace the History in new_dset with that from old_dset
-----------------------------------------------------------------------------*/

void tross_Copy_History( THD_3dim_dataset * old_dset , THD_3dim_dataset * new_dset )
{
   char * ch , * cn ;

   if( !ISVALID_DSET(old_dset) || !ISVALID_DSET(new_dset) ) return ;

   ch = tross_Get_History( old_dset ) ;      if( ch == NULL ) return ;
   cn = tross_Encode_String(ch) ; free(ch) ; if( cn == NULL ) return;
   THD_set_string_atr(new_dset->dblk, "HISTORY_NOTE", cn);
   free(cn) ; return ;
}

/*---------------------------------------------------------------------------*/
/*! Add the History in old_dset to that in new_dset [27 Feb 2003]
-----------------------------------------------------------------------------*/

void tross_Addto_History( THD_3dim_dataset *old_dset , THD_3dim_dataset *new_dset )
{
   char *ch ;

   if( !ISVALID_DSET(old_dset) || !ISVALID_DSET(new_dset) ) return ;

   ch = tross_Get_History( old_dset ) ; if( ch == NULL ) return ;
   tross_Append_History( new_dset , ch ) ; free(ch) ; return ;
}

/*---------------------------------------------------------------------------*/
/*! Erase the old History and replace it with this one.
   09 Dec 2000 - use this wisely.
-----------------------------------------------------------------------------*/

void tross_Replace_History( THD_3dim_dataset * dset , char * ch )
{
   char * cn ;

   if( !ISVALID_DSET(dset) || ch == NULL ) return ;

   cn = tross_Encode_String(ch) ; if( cn == NULL ) return ;
   THD_set_string_atr(dset->dblk, "HISTORY_NOTE", cn);
   free(cn) ; return ;
}

/*---------------------------------------------------------------------------*/
/*!  Append a string to the dataset history (create the history if need be).
-----------------------------------------------------------------------------*/

void tross_Append_History( THD_3dim_dataset *dset, char *cn )
{
   ATR_string * hist ;
   char * ch , * chold , * cdate , * cname , * cuser ;
   int idate , iname , iuser ;

   if( !ISVALID_DSET(dset) || cn == NULL || cn[0] == '\0' ) return ;

   hist = THD_find_string_atr(dset->dblk,"HISTORY_NOTE") ;
   cdate = tross_datetime() ; idate = strlen(cdate) ;
   cname = tross_hostname() ; iname = strlen(cname) ;  /* 19 Sep 1999 */
   cuser = tross_username() ; iuser = strlen(cuser) ;  /* 19 Sep 1999 */

   /*- add to the history -*/

   if( hist != NULL ){

      chold = tross_Expand_String(hist->ch) ; if( chold == NULL ) return ;
      chold = AFREALL( chold, char, 
		       strlen(chold)+idate+iuser+iname+strlen(cn)+12 ) ;

      strcat(chold,"\n") ;
      strcat(chold,"[") ; strcat(chold,cuser) ; strcat(chold,"@") ;
                          strcat(chold,cname) ; strcat(chold,": ") ;
                          strcat(chold,cdate) ;
      strcat(chold,"] ") ;
      strcat(chold,cn) ;
      ch = tross_Encode_String(chold) ; if( ch == NULL ){ free(chold); return; }
      THD_set_string_atr(dset->dblk, "HISTORY_NOTE", ch);
      free(ch) ; free(chold) ;

   /*- create the history -*/

   } else {
      chold = AFMALL(char, idate+iuser+iname+strlen(cn)+12 ) ;
      sprintf(chold,"[%s@%s: %s] %s",cuser,cname,cdate,cn) ;
      ch = tross_Encode_String(chold) ; if( ch == NULL ){ free(chold); return; }
      THD_set_string_atr(dset->dblk, "HISTORY_NOTE", ch);
      free(ch) ; free(chold) ;
   }

   free(cdate) ; free(cname) ; free(cuser) ; return ;
}

/*----------------------------------------------------------------------------*/
/*! Append multiple strings to the History, all on one line.  Usage:
      - tross_multi_Append_History(dset,str1,str2,str3,NULL) ;
      - As many str variables as desired (at least 1), of type char *, can be
        passed in.
      - The last one must be NULL.
------------------------------------------------------------------------------*/

#include <stdarg.h>

void tross_multi_Append_History( THD_3dim_dataset *dset, ... )
{
   va_list vararg_ptr ;
   int nstr=0 , nc , first=1 , ii ;
   char * str , * cpt ;

   va_start( vararg_ptr , dset ) ;

   str = AFMALL(char, 4) ; nstr = 0 ; str[0] = '\0' ;
   while(1){
      cpt = va_arg( vararg_ptr , char * ) ; if( cpt == NULL ) break ;
      nc = strlen(cpt) ;                    if( nc  == 0    ) continue ;
      nstr += nc ; str = AFREALL(str, char, nstr+8 ) ;
      if( !first ) strcat(str," ; ") ;
      strcat(str,cpt) ; first = 0 ;
   }

   va_end( vararg_ptr ) ;

   nstr = strlen(str) ;
   if( nstr > 0 ){
      for( ii=0 ; ii < nstr ; ii++ )
         if( str[ii]=='\n' || str[ii]=='\f' || str[ii]=='\r' || str[ii]=='\v' )
            str[ii] = ' ' ;

      tross_Append_History( dset , str ) ;
   }

   free(str) ; return ;
}

/*----------------------------------------------------------------------------*/
/*!  Get the history string; free() this when done.  If NULL is returned,
  there is no history (cf. Santayana).
------------------------------------------------------------------------------*/

char * tross_Get_History( THD_3dim_dataset *dset )
{
   ATR_string * hist ;
   char * ch ;

   if( !ISVALID_DSET(dset) ) return NULL ;

   hist = THD_find_string_atr(dset->dblk,"HISTORY_NOTE") ;
   if( hist == NULL ) return NULL ;

   ch = tross_Expand_String(hist->ch) ; return ch ;
}

/*-----------------------------------------------------------------------------*/
/*!  Store string at location inote;
   if inote > number of notes now present, gets added at the end of the list;
   otherwise, replaces existing note
-------------------------------------------------------------------------------*/

void tross_Store_Note( THD_3dim_dataset * dset , int inote , char * cn )
{
   ATR_int *notecount;
   int num_notes;
   ATR_string *note ;
   char note_name[20], *ch ;

   if( !ISVALID_DSET(dset) || inote <= 0 || inote > MAX_DSET_NOTES ||
                              cn == NULL || cn[0] == '\0'            ) return ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL){ tross_Add_Note( dset , cn ) ; return ; }
   num_notes = notecount->in[0];
   if( inote > num_notes ){ tross_Add_Note( dset , cn ) ; return ; }

   sprintf(note_name, "NOTE_NUMBER_%03d", inote);
   ch = tross_Encode_String(cn) ; if( ch == NULL ) return ;
   THD_set_string_atr(dset->dblk, note_name, ch);
   free(ch) ;

   ch = tross_datetime() ;
   sprintf(note_name, "NOTE_DATE_%03d", inote) ;
   THD_set_string_atr(dset->dblk, note_name, ch);
   free(ch);

   return ;
}

/*-----------------------------------------------------------------------*/
/*!  Break a string up into lines of length between lbot and ltop bytes;
  free() the result when done with it.
  NULL return means illegal input was found.
-------------------------------------------------------------------------*/

char * tross_breakup_string( char * str , int lbot , int ltop )
{
   char * sout ;
   int slen , ii , ibot,itop , ldif ;

   if( str == NULL || str[0] == '\0' || lbot > ltop || lbot < 4 ) return NULL ;

   slen = strlen(str) ; sout = AFMALL(char, slen+4) ;

   while( slen > lbot && isspace(str[slen-1]) ) slen-- ;  /* trim blanks off end */

   ibot = 0 ; ldif = ltop-lbot ;
   while(1){
      itop = ibot + ltop-1 ;    /* want to output str[ibot..itop] */

      /* if past end of str, then just output the rest and exit */

      if( itop >= slen ){
         memcpy( sout+ibot , str+ibot , slen-ibot ) ;
         sout[slen] = '\0' ;
         return sout ;
      }

      /* scan forwards to find a newline character before itop; */
      /* if one is present, output the string up to there,     */
      /* and continue again starting after the newline        */

      for( ii=ibot ; ii <= itop ; ii++ )
         if( str[ii] == '\n' ) break ;

      if( ii <= itop ){  /* found it! */
         memcpy( sout+ibot , str+ibot , ii-ibot+1 ) ;
         ibot = ii+1 ;
         if( ibot >= slen ){ sout[slen] = '\0'; return sout; }
         continue ;
      }

      /* scan backwards to find a whitespace character */

      for( ii=itop ; ii > itop-ldif ; ii-- )
         if( isspace(str[ii]) ) break ;

      /* found one before the minimum location      */
      /* copy up to the previous char into output, */
      /* then put a newline in for the whitespace */

      if( ii > itop-ldif ){
         memcpy( sout+ibot , str+ibot , ii-ibot ) ;
         sout[ii] = '\n' ;
         ibot = ii+1 ;
         continue ;         /* try to do next line */
      }

      /* scan ahead to next whitespace instead */

      for( ii=itop ; ii < slen ; ii++ )
         if( isspace(str[ii]) ) break ;

      /* found one */

      if( ii < slen ){
         memcpy( sout+ibot , str+ibot , ii-ibot ) ;
         sout[ii] = '\n' ;
         ibot = ii+1 ;
         continue ;
      }

      /* copy rest of input and exit */

      memcpy( sout+ibot , str+ibot , slen-ibot ) ;
      sout[slen] = '\0' ;
      return sout ;
   }
}

/*-----------------------------------------------------------------------*/
/*!  Return a printable version of a note string; free() this when done
-------------------------------------------------------------------------*/

char * tross_Expand_String( char * ch )
{
   char * cn = NULL ;
   int i, j, num_char;

   if( ch == NULL || ch[0] == '\0' ) return NULL ;

   num_char = strlen(ch) ;
   cn = (char *) malloc( sizeof(char) * (num_char+4) ) ;
   for( i=j=0 ; j < num_char ; j++ ){
      if( ch[j] != '\\' ){
         cn[i++] = ch[j] ;
      } else {
         switch (ch[++j] ){
            case 'r'  : cn[i++] = '\r' ; break;
            case 'n'  : cn[i++] = '\n' ; break;
            case '\\' : cn[i++] = '\\' ; break;
            case '"'  : cn[i++] = '\"' ; break;
            case 't'  : cn[i++] = '\t' ; break;
            case 'a'  : cn[i++] = '\a' ; break;
            case 'v'  : cn[i++] = '\v' ; break;
            case 'b'  : cn[i++] = '\b' ; break;
            default:    cn[i++] = '\\' ;         /* 13 Mar 2003 */
                        cn[i++] = ch[j]; break;
         }
      }
   }
   cn[i] = '\0' ; return cn ;
}

/*--------------------------------------------------------------------------*/

static int Dont_Encode_Slash = 0 ;

void tross_Dont_Encode_Slash( int q ){ Dont_Encode_Slash = q ; return ; }

/*--------------------------------------------------------------------------*/
/*!  Reverse of tross_Expand_String
----------------------------------------------------------------------------*/

char * tross_Encode_String( char * cn )
{
   char * ch = NULL ;
   int i , j , num_char ;

   if( cn == NULL || cn[0] == '\0' ) return NULL ;

   num_char = strlen(cn) ;
   ch = (char *) malloc( sizeof(char) * (2*num_char+4) ) ;
   for( i=j=0 ; j < num_char ; j++ ){
      switch( cn[j] ){
         default:   ch[i++] = cn[j]                ; break ;
         case '\r': ch[i++] = '\\' ; ch[i++] = 'r' ; break ;
         case '\n': ch[i++] = '\\' ; ch[i++] = 'n' ; break ;
         case '\"': ch[i++] = '\\' ; ch[i++] = '\"'; break ;
         case '\t': ch[i++] = '\\' ; ch[i++] = 't' ; break ;
         case '\a': ch[i++] = '\\' ; ch[i++] = 'a' ; break ;
         case '\v': ch[i++] = '\\' ; ch[i++] = 'v' ; break ;
         case '\b': ch[i++] = '\\' ; ch[i++] = 'b' ; break ;

         case '\\':                          ch[i++] = '\\';
                    if( !Dont_Encode_Slash ) ch[i++] = '\\';
         break ;
      }
   }
   ch[i] = '\0' ;
   for( i-- ; i > 0 ; i-- ) if( isspace(ch[i]) ) ch[i] = '\0' ; else break ;
   return ch ;
}
