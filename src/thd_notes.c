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

   { time_t tnow = time(NULL) ; int i ; char * qh ;
     ch=ctime(&tnow); i=strlen(ch); qh=malloc(i+4); strcpy(qh,ch); qh[i-1]='\0';
     sprintf(note_name, "NOTE_DATE_%03d", num_notes) ;
     THD_set_string_atr(dset->dblk, note_name, qh); free(qh);
   }

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

   if( !ISVALID_DSET(dset) || inote <= 0 || inote > MAX_DSET_NOTES ) return ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL){ tross_Add_Note( dset , cn ) ; return ; }
   num_notes = notecount->in[0];
   if( inote > num_notes ){ tross_Add_Note( dset , cn ) ; return ; }

   sprintf(note_name, "NOTE_NUMBER_%03d", inote);
   ch = tross_Encode_String(cn) ; if( ch == NULL ) return ;
   THD_set_string_atr(dset->dblk, note_name, ch);
   free(ch) ;

   { time_t tnow = time(NULL) ; int i ; char * qh ;
     ch=ctime(&tnow); i=strlen(ch); qh=malloc(i+4); strcpy(qh,ch); qh[i-1]='\0';
     sprintf(note_name, "NOTE_DATE_%03d", inote) ;
     THD_set_string_atr(dset->dblk, note_name, qh); free(qh);
   }

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
