#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include "mrilib.h"    
#include "3ddata.h"    
#include "TrackIO.h"
#include "Fat_Labels.h"



// setup labels, using label table if it's there
int Make_ROI_Output_Labels( char ***ROI_STR_LABELS,
                            int **ROI_LABELS, 
                            int N_nets,
                            int *NROI,
                            Dtable *ROI_dtable, 
                            int NameLabelsOut)
{
   int i,j,k,hh;
   char *str1=NULL;
   char *str_lab1=NULL;
 
   int MADE_WARNING=0;

   str1 = (char *)calloc(100, sizeof(char));
   str_lab1 = (char *)calloc(100, sizeof(char));

   if( ROI_dtable ) {
      INFO_message("Have labeltable for naming things.");
      if(!NameLabelsOut)
         INFO_message("... but won't use labels for dumped WM ROI files.");
   }
   else
       INFO_message("No refset labeltable for naming things.");

   for( hh=0 ; hh<N_nets ; hh++) {
      for( i=1 ; i<=NROI[hh] ; i++ ) {
         snprintf(str1, 100, "%d", ROI_LABELS[hh][i]);
         if( ROI_dtable ) {
            if( findin_Dtable_a(str1, ROI_dtable) ) 
               str_lab1 = strdup(findin_Dtable_a(str1, ROI_dtable));
            else{
               if(!MADE_WARNING) {
                  //WARNING_message("Have label_table, but no entry for ROI %s"
                  //                " in brick [%d]; using %03d.",
                  //                str1, hh, ROI_LABELS[hh][i]);
                  WARNING_message("Have label_table, but at least one ROI in"
                                  " your data set\n"
                                  "\tdoes NOT have an entry value.");
                    MADE_WARNING=1;
               }
               snprintf(str_lab1, 100, "%03d", ROI_LABELS[hh][i]);
            }
        
         }
         else
            snprintf(str_lab1, 100, "%03d", ROI_LABELS[hh][i]);

         snprintf(ROI_STR_LABELS[hh][i], 100, "%s", str_lab1);
      }  
   }

   /*   fprintf(stderr,"\nROI LABELS:\n");
   for( hh=0 ; hh<N_nets ; hh++) 
      for( i=1 ; i<=NROI[hh] ; i++ ) 
         fprintf(stderr,"\n%d\t%d\t%s",hh,i,ROI_STR_LABELS[hh][i]);
   fprintf(stderr,"\n");
   */

   if(str_lab1) {
      str_lab1 = NULL;
      free(str_lab1);
   }
   if(str1) {
      str1 = NULL;
      free(str1);
   }

   RETURN(1);
}

