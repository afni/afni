/*******************************************************
 * 3dNotes                                             *
 * T. Ross 8/99                                        *
 *                                                     *
 *******************************************************/

#include "mrilib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define MAX_NOTES 100   /* max number of notes per dataset */

void Error_Exit(char *message) {
        fprintf (stderr, "\n\nError: %s\n", message);
        exit(1);
}


void Show_Help(void) {
	fprintf(stderr, 
	"3dNotes - a program to add, delete and show notes for AFNI datasets.\n"
	"(c)1999 Medical College of Wisconsin\nby - T. Ross\n\n"
	"Usage: 3dNotes [-a \"string\"] [-d num] [-help] dataset\n\n"
	"Where:\n"
	"dataset	Afni compatible dataset [required].\n"
	"-a	\"str\"	Add the string \"str\" to the list of notes.\n"
	"		Note that you can use the standard C escape codes,\n"
	"		\\n for newline \\t for tab, etc.\n"
	"-d	num	deletes note number num.\n"
	"-help		Displays this screen.\n\n"
	"The default action, with no options, is to display the notes for the\n"
	"dataset.  If there are options, all deletions occur first and esentially\n"
	"simutaneously.  Then, notes are added in the order listed on the command\n"
	"line.  If you do something like -d 10 -d 10, it will delete both notes 10\n"
	"and 11.  Don't do that.\n\n"
	);
	exit(1);
}

void Add_Note(THD_3dim_dataset *dset, char *note) {
	ATR_int *notecount;
	int num_notes;
	char note_name[20];

	notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
	if (notecount == NULL) {
		num_notes = 1; 
		THD_set_int_atr(dset->dblk, "NOTES_COUNT", 1, &num_notes);
	} else {
		num_notes = notecount->in[0] + 1;
		notecount->in[0]++;
	}
	
	sprintf(note_name, "NOTE_NUMBER_%03d", num_notes);
	THD_set_string_atr(dset->dblk, note_name, note);
}

void Delete_Note(THD_3dim_dataset *dset, int note) {
	ATR_int *notecount;
	int num_notes;
	ATR_string *note_text;
	char note_name[20];

	notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
	if (notecount == NULL) 
		Error_Exit("Cannot delete notes, there are no notes");

	num_notes = notecount->in[0];
	if (note > num_notes)
		Error_Exit("Cannot delete note, number larger than number of notes");
	sprintf(note_name, "NOTE_NUMBER_%03d", note);
	THD_erase_one_atr( dset->dblk , note_name );
	notecount->in[0]--;
	/* Slide the higher numbered notes down */
	while (note < num_notes) {
		/* find the next note */
		sprintf(note_name, "NOTE_NUMBER_%03d", note+1);
		note_text=THD_find_string_atr(dset->dblk, note_name);
		if (note_text == NULL)
			Error_Exit("Could not find a note, is the a problem with the HEAD file?");
		/* rename to the previous name, and increment for the next loop interation */
		sprintf(note_name, "NOTE_NUMBER_%03d", note++);
		strcpy(note_text->name, note_name);
	}
	/* No notes left, so remove count attribute */
	if (num_notes == 1) 
		THD_erase_one_atr( dset->dblk, "NOTES_COUNT");

}


void Display_Notes(THD_3dim_dataset *dset) {
	ATR_int *notecount;
	ATR_string *note;
	int num_notes, i, j, num_char;
	char note_name[20];

	notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
	if (notecount == NULL) 
		Error_Exit("There are no notes in the dataset");
	num_notes = notecount->in[0];
	for (i=1; i<= num_notes; i++) {
		sprintf(note_name, "NOTE_NUMBER_%03d", i);
		note=THD_find_string_atr(dset->dblk, note_name);
		if (note == NULL)
			Error_Exit("Could not find the next note, is the a problem with the HEAD file?");
		printf("\n%d. ", i);
		num_char = note->nch;
		for(j=0; j<num_char; j++) {
			if (note->ch[j] != '\\')
				printf("%c", note->ch[j]);
			else {
				j++;
				switch (note->ch[j]) {
					case 'r' : printf("\r"); break;
					case 'n' : printf("\n"); break;
					case '\\' : printf("\\"); break;
					case '"' : printf("\""); break;
					case 't' : printf("\t"); break;
					case 'a' : printf("\a"); break;
					case 'v' : printf("\v"); break;
					case 'b' : printf("\b"); break;
				}
			}
		}
	}
	printf("\n");
}
	

int main (int argc, char * argv[]) {
        
	THD_3dim_dataset *dset=NULL;
	int narg = 1, i, curr_note=0, curr_del=0;
	char *notes[MAX_NOTES];
	int delnotes[MAX_NOTES], delindex, delnum;

	if (argc == 1)   /* no file listed */
		Show_Help();

	for (i=0; i<MAX_NOTES; i++) {
		notes[i] = NULL;
		delnotes[i] = 0;
	}

        /* Loop over arguements and pull out what we need */
        while( narg < argc && argv[narg][0] == '-' ){

                if( strncmp(argv[narg],"-a",2) == 0 ) {
                        narg++;
                        if (narg==argc)
                                Error_Exit("-a must be followed by a string");
			notes[curr_note++] = argv[narg++];
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

                if( strncmp(argv[narg],"-help",5) == 0 ) {
			Show_Help();
                }
	}

	if( narg >= argc )
		Error_Exit("No input dataset!?\n") ;

        dset = THD_open_one_dataset( argv[narg] ) ;
        if( dset == NULL )
        	Error_Exit("Cannot open dataset") ; 


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
			Delete_Note(dset, delnum);
		}
	} while (delnum);  /* loop ends when no more to delete */

	/* Next, add notes */
	for (i=0; i<curr_note; i++)
		Add_Note(dset, notes[i]);

	/* Display, if required */
	if ((curr_note == 0) && (curr_del == 0))
		Display_Notes(dset);
	else {
	        THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
	        THD_delete_3dim_dataset( dset , False ) ; 
	}

}
