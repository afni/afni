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

/*---------------------------------------------------------------------------
   Assemble a sort-of command line string from the arguments;
   free() this when done.
-----------------------------------------------------------------------------*/

char * tross_commandline( char * pname , int argc , char ** argv )
{
   char * ch ;
   int ii , ll ;

   if( argc < 2 || argv == NULL ) return NULL ;

   if( pname == NULL ) pname = argv[0] ;

   ii = strlen(pname) ; ch = malloc(ii+4) ; strcpy(ch,pname) ;

   for( ii=1 ; ii < argc ; ii++ ){
      ll = strlen(argv[ii]) ;
      ch = realloc( ch , strlen(ch)+ll+4 ) ;
      if( !THD_filename_ok(argv[ii]) ){
         int jj ; char * aa = malloc(ll+1) ;

         strcpy(aa,argv[ii]) ;        /* edit out bad characters */
         for( jj=0 ; jj < ll ; jj++ )
            if( iscntrl(aa[jj]) || isspace(aa[jj]) || aa[jj] > 127 ) aa[jj] = ' ' ;

         strcat(ch," '") ; strcat(ch,aa) ; strcat(ch,"'") ; free(aa) ;
      } else {
         strcat(ch," ")  ; strcat(ch,argv[ii]) ;
      }
   }

   return ch ;
}

/*---------------------------------------------------------------------------
  Get the current date/time string;  free() this when done.
-----------------------------------------------------------------------------*/

char * tross_datetime(void)
{
   time_t tnow = time(NULL) ; int i ; char * qh , * ch ;

   ch=ctime(&tnow); i=strlen(ch); qh=malloc(i+2); strcpy(qh,ch); qh[i-1]='\0';
   return qh ;
}

/*---------------------------------------------------------------------------
   Add a note after the last current note
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

/*---------------------------------------------------------------------------
   Delete a particular note
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

int tross_Get_Notecount( THD_3dim_dataset * dset )
{
   ATR_int *notecount;

   if( !ISVALID_DSET(dset) ) return -1 ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) return 0 ;
   return notecount->in[0];
}

/*---------------------------------------------------------------------------
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
   note=THD_find_string_atr(dset->dblk, note_name);
   if (note == NULL ) return NULL ;
   ch = tross_Expand_String( note->ch ) ;
   return ch ;
}

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
   note=THD_find_string_atr(dset->dblk, note_name);
   if (note == NULL ) return NULL ;
   return tross_Expand_String( note->ch ) ;
}

/*---------------------------------------------------------------------------
   Let's make HISTORY!
-----------------------------------------------------------------------------*/

void tross_Make_History( char * pname, int argc, char ** argv, THD_3dim_dataset *dset )
{
   char * ch ;

   if( argc < 2 || argv == NULL || !ISVALID_DSET(dset) ) return ;

   ch = tross_commandline( pname , argc , argv ) ; if( ch == NULL ) return ;
   tross_Append_History( dset , ch ) ;
   free(ch) ; return ;
}

/*---------------------------------------------------------------------------
   Replace the History in new_dset with that from old_dset
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

/*---------------------------------------------------------------------------
   Append a string to the dataset history (create the history if need be).
-----------------------------------------------------------------------------*/

void tross_Append_History( THD_3dim_dataset *dset, char *cn )
{
   ATR_string * hist ;
   char * ch , * chold , * cdate ;
   int ii , idate ;

   if( !ISVALID_DSET(dset) || cn == NULL || cn[0] == '\0' ) return ;

   hist = THD_find_string_atr(dset->dblk,"HISTORY_NOTE") ;
   cdate = tross_datetime() ; idate = strlen(cdate) ;

   /*- add to the history -*/

   if( hist != NULL ){

      chold = tross_Expand_String(hist->ch) ; if( chold == NULL ) return ;
      ii = strlen(chold) ; chold = realloc( chold , ii+idate+strlen(cn)+8 ) ;
      strcat(chold,"\n") ;
      strcat(chold,"[") ; strcat(chold,cdate) ; strcat(chold,"] ") ;
      strcat(chold,cn) ;
      ch = tross_Encode_String(chold) ; if( ch == NULL ){ free(chold); return; }
      THD_set_string_atr(dset->dblk, "HISTORY_NOTE", ch);
      free(ch) ; free(chold) ;

   /*- create the history -*/

   } else {
      chold = malloc( idate+strlen(cn)+8 ) ;
      sprintf(chold,"[%s] %s",cdate,cn) ;
      ch = tross_Encode_String(chold) ; if( ch == NULL ){ free(chold); return; }
      THD_set_string_atr(dset->dblk, "HISTORY_NOTE", ch);
      free(ch) ; free(chold) ;
   }

   free(cdate) ; return ;
}

/*----------------------------------------------------------------------------
  Get the history string; free() this when done.  If NULL is returned,
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

/*-----------------------------------------------------------------------------
   Store string at location inote;
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

/*-----------------------------------------------------------------------
   return a printable version of a note string; free() this when done
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
         }
      }
   }
   cn[i] = '\0' ; return cn ;
}

/*--------------------------------------------------------------------------
  reverse of tross_Expand_String
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
      }
   }
   ch[i] = '\0' ;
   for( i-- ; i > 0 ; i-- ) if( isspace(ch[i]) ) ch[i] = '\0' ; else break ;
   return ch ;
}
