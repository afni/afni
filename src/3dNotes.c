/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*******************************************************
 * 3dNotes                                             *
 * T. Ross 8/99                                        *
 * -- Modified by RWCox to use thd_notes.c functions   *
 *******************************************************/

#include "mrilib.h"

void Error_Exit(char *message) {
        fprintf (stderr, "\n\nError: %s\n", message);
        exit(1);
}

void Show_Help(void) {
   fprintf(stderr, 
   "3dNotes - a program to add, delete and show notes for AFNI datasets.\n"
   "(c)1999 Medical College of Wisconsin\nby - T. Ross\n\n"
   "Usage: 3dNotes [-a \"string\"] [-h \"string\"][-d num] [-help] dataset\n\n"
   "Where:\n"
   "dataset   Afni compatible dataset [required].\n"
   "-a   \"str\"   Add the string \"str\" to the list of notes.\n"
   "      Note that you can use the standard C escape codes,\n"
   "      \\n for newline \\t for tab, etc.\n"
   "-h   \"str\"   Append the string \"str\" to the dataset's history.  This\n"
   "      can only appear once on the command line.  As this is added to the\n"
   "      history, it cannot easily be deleted.  But, the history is\n"
   "      propagated to the children of this dataset.\n"
   "-HH  \"str\"   Replace any existing history note with \"str\".  This line\n"
   "     cannot be used with '-h'.\n"               /* 09 Dec 2000 */
   "-d   num   deletes note number num.\n"
   "-help      Displays this screen.\n\n"
   "The default action, with no options, is to display the notes for the\n"
   "dataset.  If there are options, all deletions occur first and esentially\n"
   "simutaneously.  Then, notes are added in the order listed on the command\n"
   "line.  If you do something like -d 10 -d 10, it will delete both notes 10\n"
   "and 11.  Don't do that.\n\n"
   );
   exit(0);
}


void Display_Notes(THD_3dim_dataset *dset) {
   ATR_int *notecount;
   ATR_string *note;
   int num_notes, i, j, num_char;
   char note_name[20];
   char * chn , * chd ;

   notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
   if (notecount == NULL) 
      Error_Exit("There are no notes in the dataset");
   num_notes = notecount->in[0];
   for (i=1; i<= num_notes; i++) {
      chn = tross_Get_Note( dset , i ) ;
      if( chn == NULL )
         Error_Exit("Could not get the next note;"
                    " is there a problem with the HEAD file?");

      chd = tross_Get_Notedate(dset,i) ;
      if( chd == NULL ){
        printf("\n----- NOTE %d [no date] -----\n%s\n",i,chn) ;
      } else {
        printf("\n----- NOTE %d [%s] -----\n%s\n",i,chd,chn) ;
        free(chd) ;
      }
      free(chn) ;
   }
}
   

int main (int argc, char * argv[]) {
        
   THD_3dim_dataset *dset=NULL;
   int narg = 1, i, curr_note=0, curr_del=0;
   char *notes[MAX_DSET_NOTES];
   char *history_note = NULL;
   int delnotes[MAX_DSET_NOTES], delindex, delnum;
   int HH=0 ;  /* 09 Dec 2000 */

   if (argc == 1)   /* no file listed */
      Show_Help();

   mainENTRY("3dNotes main"); machdep(); AFNI_logger("3dNotes",argc,argv);

   for (i=0; i<MAX_DSET_NOTES; i++) {
      notes[i] = NULL;
      delnotes[i] = 0;
   }


        /* Loop over arguements and pull out what we need */
        while( narg < argc && argv[narg][0] == '-' ){

                if( strncmp(argv[narg],"-help",5) == 0 ) {
                        Show_Help();
                }

                if( strncmp(argv[narg],"-a",2) == 0 ) {
                        narg++;
                        if (narg==argc)
                                Error_Exit("-a must be followed by a string");
                        notes[curr_note++] = argv[narg++];
                        continue;       
                }

                if( strncmp(argv[narg],"-h",2) == 0 ) {
                        narg++;
                        if (narg==argc)
                                Error_Exit("-h must be followed by a string");
                        if( history_note != NULL )
                           fprintf(stderr,
                                   "*** Warning: multiple -h options!\n") ;
                        history_note = argv[narg++]; HH = 0 ;
                        continue;
                }

                if( strncmp(argv[narg],"-HH",3) == 0 ) {  /* 09 Dec 2000 */
                        narg++;
                        if (narg==argc)
                                Error_Exit("-HH must be followed by a string");
                        if( history_note != NULL )
                           fprintf(stderr,
                                   "*** Warning: multiple -h options!\n") ;
                        history_note = argv[narg++]; HH = 1 ;
                        continue;
                }

                if( strncmp(argv[narg],"-d",2) == 0 ) {
                        narg++;
                        if (narg==argc)
                                Error_Exit("-d must be followed by a integer");
                        delnotes[curr_del] = (int)atol(argv[narg++]);
                        if (delnotes[curr_del++] < 1)
                           Error_Exit("Cannot delete a note numbered < 1");
                        continue;       
                }
   }

   if( narg >= argc )
      Error_Exit("No input dataset!?\n") ;

   dset = THD_open_one_dataset( argv[narg] ) ;
   if( dset == NULL          ) Error_Exit("Cannot open dataset") ; 
   if( DSET_IS_MINC(dset)    ) Error_Exit("Cannot use MINC dataset") ;
   if( DSET_IS_ANALYZE(dset) ) Error_Exit("Cannot use ANALYZE dataset") ;
   if( DSET_IS_1D(dset)      ) Error_Exit("Cannot use .1D dataset") ;
   if( DSET_IS_3D(dset)      ) Error_Exit("Cannot use .3D dataset") ;
   if( DSET_IS_CTFMRI(dset)  ) Error_Exit("Cannot use CTF dataset") ;
   if( DSET_IS_CTFSAM(dset)  ) Error_Exit("Cannot use CTF dataset") ;
   if( DSET_IS_NIFTI(dset)   ) Error_Exit("Cannot use NIFTI dataset") ;

   /* First, delete notes */
   do {
      delnum = 0;
      /* find the largest note to delete, 
         since numbering for those > than deleted changes */
      for(i=0; i<curr_del; i++)
         if (delnotes[i]>delnum) {
            delnum=delnotes[i];
            delindex = i;
         }
      if (delnum) {
         delnotes[delindex]=0;
         tross_Delete_Note(dset, delnum);
      }
   } while (delnum);  /* loop ends when no more to delete */

   /* Next, add notes */
   tross_Dont_Encode_Slash( 1 ) ;   /* 13 Mar 2003 */
   for (i=0; i<curr_note; i++)
      tross_Add_Note(dset, notes[i]);
   
   /* Append to the history */
   if (history_note != NULL){
        if( HH == 0 )
           tross_Append_History( dset, history_note);
        else
           tross_Replace_History( dset, history_note);  /* 09 Dec 2000 */
   }

   /* Display, if required */
   if ((curr_note == 0) && (curr_del == 0) && (history_note == NULL))
      Display_Notes(dset);
   else {
           THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
           THD_delete_3dim_dataset( dset , False ) ; 
   }

}
