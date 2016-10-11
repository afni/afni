#include <afni.h>
#include "suma_suma.h"
#include "readglob.h"


int list_for_DTI( char *dti_listname,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS  )
{
   int i,ii,j;
   char **NameVEC=NULL;
   char *NameXF=NULL;
   char **NameSCAL=NULL;
   char **NameP=NULL; // 4 of these
	NI_element *nel=NULL;

   NameVEC = calloc( N_DTI_VECT, sizeof(NameVEC));  
   for(i=0 ; i<N_DTI_VECT ; i++) 
      NameVEC[i] = calloc( 100, sizeof(char)); 
   NameSCAL = calloc( N_DTI_SCAL, sizeof(NameSCAL));  
   for(i=0 ; i<N_DTI_SCAL ; i++) 
      NameSCAL[i] = calloc( 100, sizeof(char)); 
   NameP = calloc( N_PLUS_DTIFILE, sizeof(NameP));  
   for(i=0 ; i<N_PLUS_DTIFILE ; i++) 
      NameP[i] = calloc( 100, sizeof(char)); 
   NameXF = (char *)calloc(100, sizeof(char)); 

   if( (NameVEC == NULL) || (NameSCAL == NULL) ||
       (NameXF == NULL) || (NameP == NULL)  ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(126);
   }
		  
   if (!(nel = ReadDTI_inputs(dti_listname))) {
      ERROR_message("Failed to read options in %s\n",
                    dti_listname);
      exit(19);
   }
         
   if (NI_getDTI_inputs( nel,
                         NameVEC,
                         NameXF,
                         NameSCAL,
                         NameP,
                         &i, &j)) {
      ERROR_message("Failed to get DTI list of files.");
      exit(1);
   }
   NI_free_element(nel); nel=NULL;

   for( ii=0 ; ii<N_DTI_SCAL ; ii++) {
      if( !(ii == 1)) { // because we don't use MD
         insetPARS[ii] = THD_open_dataset(NameSCAL[ii]);
         if( (insetPARS[ii] == NULL ) )
            ERROR_exit("Can't open listed dataset '%s': "
                       "for required scalar.",
                       NameSCAL[ii]);
         DSET_load(insetPARS[ii]) ; CHECK_LOAD_ERROR(insetPARS[ii]) ;
         fprintf(stderr,"\tFound file '%s' to be labeled '%s'\n",
                 NameSCAL[ii],DTI_SCAL_LABS[ii]);
      }
   }

   for( ii=0 ; ii<N_DTI_VECT ; ii++) {
      insetVECS[ii] = THD_open_dataset(NameVEC[ii]);
      if( (insetVECS[ii] == NULL ) )
         ERROR_exit("Can't open dataset '%s': for required vector dir.",
                    NameVEC[ii]);
      DSET_load(insetVECS[ii]) ; CHECK_LOAD_ERROR(insetVECS[ii]) ;
      fprintf(stderr,"\tFound file '%s' to be labeled '%s'\n",
              NameVEC[ii],DTI_VECT_LABS[ii]);
   }

   // double check all got filled:
   for( i=0 ; i<N_DTI_SCAL ; i++ ) 
     if( (insetPARS[i] == NULL) && !(i == 1) ) // b/c MD is not nec 
         ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_LABS[i] );
   for( i=0 ; i<N_DTI_VECT ; i++ ) 
      if( insetVECS[i] == NULL ) 
         ERROR_exit("Can't open dataset: '%s' file",DTI_VECT_LABS[i] );
   fprintf(stderr,"\n");			
         
   for(i=0 ; i<N_DTI_VECT ; i++)
      free(NameVEC[i]);
   free(NameVEC);
   for(i=0 ; i<N_DTI_SCAL ; i++) 
      free(NameSCAL[i]);
   free(NameSCAL);
   for(i=0 ; i<N_PLUS_DTIFILE ; i++) 
      free(NameP[i]);
   free(NameP);
   free(NameXF);

   return 0;
}

// ------------------------------------------------------------------

int glob_for_DTI( char *infix,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS,
                  int hardi_pref_len )
{
   int i,ii;
   char tprefix[THD_MAX_PREFIX];
   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2;           // "-wild_files_noAext_noAview"
   int wild_orig_name = 1;     // "-wild_files_orig_name"
   char temp_name[32];
   int pref_offset = 0;

   sprintf(tprefix,"%s*",infix);

   // this island of coding, globbing and sorting due to ZSS; see
   // apsearch.c program for original.
   wild_list = SUMA_append_replace_string(wild_list, tprefix, " ", 1); 
      
   INFO_message("SEARCHING for files with prefix '%s':",tprefix);
   fprintf(stderr,"\t");

   MCW_wildcards(wild_list, &nglob, &wglob ); 
   if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                           &nsort, &isrt))) {
      
      for( ii=0 ; ii<nsort ; ii++) {
         
         // check for first char being an underscore; if so, remove
         pref_offset = 0;
         if( *(wsort[ii]+hardi_pref_len) == '_')
            pref_offset = 1;
         
         snprintf(temp_name,31,"%s", 
                  wsort[ii]+hardi_pref_len+pref_offset);

         for( i=0 ; i<N_DTI_SCAL ; i++ ) {
            if ( !strcmp(DTI_SCAL_LABS[i], temp_name) ) {
               fprintf(stderr," '%s' ",DTI_SCAL_LABS[i]);
               insetPARS[i] = THD_open_dataset(wglob[isrt[ii]]);
               if( insetPARS[i] == NULL ) 
                  ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]] );
               DSET_load(insetPARS[i]) ; CHECK_LOAD_ERROR(insetPARS[i]) ;
               break;
            }
            else continue;
         }
         
         for( i=0 ; i<N_DTI_VECT ; i++ ) {
            if ( !strcmp(DTI_VECT_LABS[i], temp_name) ) {
               fprintf(stderr," '%s' ",DTI_VECT_LABS[i]);
               insetVECS[i] = THD_open_dataset(wglob[isrt[ii]]);
               if( insetVECS[i] == NULL ) 
                  ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]] );
               DSET_load(insetVECS[i]) ; CHECK_LOAD_ERROR(insetVECS[i]) ;
               break;
            }
            else continue;
         }
      }
         
      // double check all got filled:
      for( i=0 ; i<N_DTI_SCAL ; i++ ) 
         if( (insetPARS[i] == NULL) && !(i == 1) ) // b/c MD is not nec 
            ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_LABS[i] );
      for( i=0 ; i<N_DTI_VECT ; i++ ) 
         if( insetVECS[i] == NULL ) 
            ERROR_exit("Can't open dataset: '%s' file",DTI_VECT_LABS[i] );
      fprintf(stderr,"\n");			
         
      if (isrt) free(isrt); isrt = NULL;
      for (i=0; i<nglob; ++i) if (wsort[i]) free(wsort[i]);
      free(wsort); wsort = NULL;
      SUMA_ifree(wild_list);
      MCW_free_wildcards( nglob , wglob ) ;
   } 

   else {
      ERROR_message("Failed to sort");
      SUMA_ifree(wild_list);
      MCW_free_wildcards( nglob , wglob ) ;
      exit(1);
   }
         
   return 0;
}


