#include "mrilib.h"
#include "suma_datasets.h"
#include "readglob.h"

/*
  'FULL' is a switch about whether all scalars are nec:  
  FULL=0 for 3dDWUncert, because not all scalars are used;
  FULL=1 for 3dTrackID, because we *do* rely on all of them.

   [PT: July 15, 2020] from include "suma_suma.h" -> "suma_objs.h"
*/

int list_for_DTI( char *dti_listname,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS,
                  int *extrafile, int *pars_top,
                  char *wild_names[],
                  int FULL)
{
   int i,ii,jj;
   char **NameVEC=NULL;
   char *NameXF=NULL;
   char **NameSCAL=NULL;
   char **NamePLUS=NULL; // 4 of these
	NI_element *nel=NULL;
   int ii0=0; // for difference of 3dDWUnc and 3dTrac modes

   NameVEC = calloc( N_DTI_VECT, sizeof(NameVEC));  
   for(i=0 ; i<N_DTI_VECT ; i++) 
      NameVEC[i] = calloc( N_CHAR_PATH, sizeof(char)); 
   NameSCAL = calloc( N_DTI_SCAL, sizeof(NameSCAL));  
   for(i=0 ; i<N_DTI_SCAL ; i++) 
      NameSCAL[i] = calloc( N_CHAR_PATH, sizeof(char)); 
   NamePLUS = calloc( N_DTI_PLUS, sizeof(NamePLUS));  
   for(i=0 ; i<N_DTI_PLUS ; i++) 
      NamePLUS[i] = calloc( N_CHAR_PATH, sizeof(char)); 
   NameXF = (char *)calloc(N_CHAR_PATH, sizeof(char)); 
   
   if( (NameVEC == NULL) || (NameSCAL == NULL) ||
       (NameXF == NULL) || (NamePLUS == NULL)  ) {
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
                         NameXF, // is returned as null if not here
                         NameSCAL,
                         NamePLUS,
                         extrafile, pars_top)) {
      ERROR_message("Failed to get DTI list of files.");
      exit(1);
   }

   NI_free_element(nel); nel=NULL;

   if( !FULL ) // -> essentially for 3dDWUncert
      ii0 = 0;
   else        // -> for tracking, leave space for XF, if nec
      ii0 = 1; 

   // extrafile
   if(*extrafile && (FULL) ) { // i.e., for 3dTrackID
      insetPARS[0] = THD_open_dataset(NameXF);
      if( (insetPARS[0] == NULL ) )
            ERROR_exit("Can't open 'extra' listed dataset '%s': ",
                       NameXF);
      DSET_load(insetPARS[0]); 
      CHECK_LOAD_ERROR(insetPARS[0]);
      fprintf(stderr,"\tFound 'extra' file '%s' to be labeled '%s'\n",
                 NameXF, DTI_XTRA_LABS[0]);
      snprintf(wild_names[0],31,"%s", DTI_XTRA_LABS[0]); 
   }

   // scalar
   for( ii=0 ; ii<N_DTI_SCAL ; ii++) {
      if( !FULL && ( !(DTI_SCAL_LABS[ii] == "FA") ) ) {
         INFO_message(" -> Don't need %s\n",DTI_SCAL_LABS[ii]);
         continue; // because we don't use MD or RD for 3dDWUncert
      }
      else{ 
         insetPARS[ii0 + ii] = THD_open_dataset(NameSCAL[ii]);
         if( (insetPARS[ii0 + ii] == NULL ) )
            ERROR_exit("Can't open listed dataset '%s': "
                       "for required scalar.",
                       NameSCAL[ii]);
         DSET_load(insetPARS[ii0 + ii]); CHECK_LOAD_ERROR(insetPARS[ii0 + ii]);
         fprintf(stderr,"\tFound file '%s' to be labeled '%s'\n",
                 NameSCAL[ii], DTI_SCAL_LABS[ii]);
         snprintf(wild_names[ii0+ii],31,"%s", DTI_SCAL_LABS[ii]); 
      }
   }

   if(FULL){ // plus
      jj = 0;
      for( ii=0 ; ii<N_DTI_PLUS ; ii++) 
         if( strcmp(NamePLUS[ii],"\0") ) {
            i = N_DTI_SCAL + jj + ii0;
            insetPARS[i] = THD_open_dataset(NamePLUS[ii]);
            if( (insetPARS[i] == NULL ) )
               ERROR_exit("Can't open listed dataset '%s': "
                          "for required scalar.",
                          NamePLUS[ii]);
            DSET_load(insetPARS[i]); 
            CHECK_LOAD_ERROR(insetPARS[i]);
            fprintf(stderr,"\tFound file '%s' to be labeled '%s'\n",
                    NamePLUS[ii], DTI_PLUS_LABS[ii]);
            snprintf(wild_names[i],31,"%s", DTI_PLUS_LABS[ii]); 
            jj++;
         }
   }

   // vector
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
      if( insetPARS[ii0+i] == NULL) 
         if( !FULL ) {
            if( (DTI_SCAL_LABS[i] == "FA") )
               ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_LABS[i] );
         }
         else
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
   for(i=0 ; i<N_DTI_PLUS ; i++) 
      free(NamePLUS[i]);
   free(NamePLUS);
   free(NameXF);

   return 0;
}

// ------------------------------------------------------------------

/*
  'FULL' is a switch about whether all scalars are nec:  
  FULL=0 for 3dDWUncert, because not all scalars are used;
  FULL=1 for 3dTrackID, because we *do* rely on all of them.
 */
int glob_for_DTI( char *infix,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS,
                  int hardi_pref_len,
                  int FULL)
{
   int i,ii,ss;
   char tprefix[THD_MAX_PREFIX];
   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2;           // "-wild_files_noAext_noAview"
   int wild_orig_name = 1;     // "-wild_files_orig_name"
   char temp_name[32];
   int pref_offset = 0;
   int foundahome=0;

   sprintf(tprefix,"%s*",infix);

   // this island of coding, globbing and sorting due to ZSS; see
   // apsearch.c program for original.
   wild_list = SUMA_append_replace_string(wild_list, tprefix, " ", 1); 
      
   INFO_message("SEARCHING for files with prefix '%s':",tprefix);
   fprintf(stderr,"\tFINDING:");
   fprintf(stderr,"\t");

   MCW_wildcards(wild_list, &nglob, &wglob ); 
   if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                           &nsort, &isrt))) {
      
      for( ii=0 ; ii<nsort ; ii++) {
         
         foundahome = 0;

         // check for first char being an underscore; if so, remove
         pref_offset = 0;
         if( *(wsort[ii]+hardi_pref_len) == '_')
            pref_offset = 1;
         
         snprintf(temp_name,31,"%s", 
                  wsort[ii]+hardi_pref_len+pref_offset);

         for( i=0 ; i<N_DTI_SCAL ; i++ ) {
            if ( !strcmp(DTI_SCAL_LABS[i], temp_name) ) {
               foundahome = 1;
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
               foundahome = -1;
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
         // don't need MD or RD for 3dDWUncert
         if( !FULL && 
             ((DTI_SCAL_LABS[i] == "MD") || (DTI_SCAL_LABS[i] == "RD"))) {
            fprintf(stderr, "\nDon't need %s\n",DTI_SCAL_LABS[i]);
            continue;
         }
         else if( (insetPARS[i] == NULL) ) // b/c MD is not nec 
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





/*
  Same for 3dDWUncert and for 3dTrackID
*/
int glob_for_DTI_vec( char *infix,
                      THD_3dim_dataset **insetVECS,
                      int hardi_pref_len)
{
   int i,ii,ss;
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
      
   INFO_message("SEARCHING for vector files with prefix '%s':",tprefix);
   fprintf(stderr,"\tFINDING:");
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


/*
  This isn't great, but simpler than before: this applies to simple
  case of 3dWUncert, where we just need one scalar at the moment.

  Though, this is written to be able to get multiple ones, should such
  a time arise.
 */
int glob_for_DTI_scal_unc( char *infix,
                           THD_3dim_dataset **insetPARS,
                           int hardi_pref_len)
{
   int i,ii,ss;
   char tprefix[THD_MAX_PREFIX];
   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2;           // "-wild_files_noAext_noAview"
   int wild_orig_name = 1;     // "-wild_files_orig_name"
   char temp_name[32];
   int pref_offset = 0;
   int foundahome=0;

   sprintf(tprefix,"%s*",infix);

   // this island of coding, globbing and sorting due to ZSS; see
   // apsearch.c program for original.
   wild_list = SUMA_append_replace_string(wild_list, tprefix, " ", 1); 
      
   INFO_message("SEARCHING for files with prefix '%s':",tprefix);
   fprintf(stderr,"\tFINDING:");
   fprintf(stderr,"\t");

   MCW_wildcards(wild_list, &nglob, &wglob ); 
   if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                           &nsort, &isrt))) {
      
      for( ii=0 ; ii<nsort ; ii++) {
         
         foundahome = 0;

         // check for first char being an underscore; if so, remove
         pref_offset = 0;
         if( *(wsort[ii]+hardi_pref_len) == '_')
            pref_offset = 1;
         
         snprintf(temp_name,31,"%s", 
                  wsort[ii]+hardi_pref_len+pref_offset);

         for( i=0 ; i<N_DTI_SCAL ; i++ ) {
            if ( !strcmp(DTI_SCAL_LABS[i], temp_name) ) {
               foundahome = 1;
               fprintf(stderr," '%s' ",DTI_SCAL_LABS[i]);
               insetPARS[i] = THD_open_dataset(wglob[isrt[ii]]);
               if( insetPARS[i] == NULL ) 
                  ERROR_exit("Can't open dataset '%s'",wglob[isrt[ii]] );
               DSET_load(insetPARS[i]) ; CHECK_LOAD_ERROR(insetPARS[i]) ;
               break;
            }
            else continue;
         }
      }
         
      // double check all got filled:
      for( i=0 ; i<N_DTI_SCAL ; i++ ) 
         // don't need MD or RD for 3dDWUncert
         if( (DTI_SCAL_LABS[i] == "MD") || (DTI_SCAL_LABS[i] == "RD") ) {
            fprintf(stderr, "\nDon't need %s\n",DTI_SCAL_LABS[i]);
            continue;
         }
         else if( (insetPARS[i] == NULL) ) // b/c MD is not nec 
            ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_LABS[i] );

         
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





/*
  Need the 4 main scalar files for tracking stats

*/
int glob_for_DTI_trac( char *infix,
                       THD_3dim_dataset **insetPARS,
                       char **wild_names,
                       int hardi_pref_len,
                       int *pars_top,
                       int SEARCH_NO)
{
   int i,ii,ss;
   char tprefix[THD_MAX_PREFIX];
   char *wild_list=NULL;
   char **wglob=NULL, **wsort=NULL;
   int nglob, nsort, *isrt=NULL;
   int wild_all_files = 0, wild_ci=0;
   int wild_noext=2;           // "-wild_files_noAext_noAview"
   int wild_orig_name = 1;     // "-wild_files_orig_name"
   char temp_name[32];
   int pref_offset = 0;
   int foundahome=0;

   int pii0 = N_DTI_SCAL + 1; // at what index to start adding 'plus'
                              // scal sets; '+1' is because of N_DTI_XTRA=1
   int pii  = 0;              // count plus sets

   sprintf(tprefix,"%s*",infix);

   // this island of coding, globbing and sorting due to ZSS; see
   // apsearch.c program for original.
   wild_list = SUMA_append_replace_string(wild_list, tprefix, " ", 1); 
      
   INFO_message("SEARCHING for scalar files with prefix '%s':",tprefix);
   fprintf(stderr,"\tFINDING:");
   fprintf(stderr,"\t");

   MCW_wildcards(wild_list, &nglob, &wglob ); 
   if ((wsort = unique_str(wglob, nglob, wild_ci, wild_noext, 
                           &nsort, &isrt))) {
      
      for( ii=0 ; ii<nsort ; ii++) {
         
         foundahome = 0;

         // check for first char being an underscore; if so, remove
         pref_offset = 0;
         if( *(wsort[ii]+hardi_pref_len) == '_')
            pref_offset = 1;
         
         snprintf(temp_name,31,"%s", 
                  wsort[ii]+hardi_pref_len+pref_offset);

         if( !foundahome ) // a known scalar
            for( i=0 ; i<N_DTI_SCAL ; i++ ) {
               if ( !strcmp(DTI_SCAL_LABS[i], temp_name) ) {
                  foundahome = 1;
                  fprintf(stderr," '%s' ",DTI_SCAL_LABS[i]);
                  insetPARS[i+1] = THD_open_dataset(wglob[isrt[ii]]);
                  if( insetPARS[i+1] == NULL ) 
                     ERROR_exit("Can't open dataset '%s'", wglob[isrt[ii]]);
                  DSET_load(insetPARS[i+1]); 
                  CHECK_LOAD_ERROR(insetPARS[i+1]);
                  snprintf(wild_names[i+1],31,"%s", DTI_SCAL_LABS[i]); 
                  break;
               }
               else continue;
            }
         
         if( !foundahome) // a known unscalar
            for( i=0 ; i<N_DTI_VECT ; i++ ) {
               if ( !strcmp(DTI_VECT_LABS[i], temp_name) ) {
                  foundahome = -1;
                  //fprintf(stderr," not'%s' ",DTI_VECT_LABS[i]);
                  break;
               }
               else continue; 
            }
         
         if( !foundahome 
             && (pii < N_DTI_PLUS) 
             && !SEARCH_NO) {// an unknown scalar?
            foundahome = 1;
            //fprintf(stderr," pii(%d) pii0(%d) ", pii, pii0);
            insetPARS[pii0+pii] = THD_open_dataset(wglob[isrt[ii]]);
            if( insetPARS[pii0+pii] == NULL ) 
               ERROR_exit("Can't open dataset '%s'", wglob[isrt[ii]]);
            DSET_load(insetPARS[pii0 + pii]); 
            CHECK_LOAD_ERROR(insetPARS[pii0 + pii]);
            // only keep scalar sets
            // OLD: if( (DSET_NVALS(insetPARS[pii0+pii]) != 1) ) 
            // [PT: Aug 8, 2017] change next IF b/c 1D text files
            // weren't being ignored.
            if( (DSET_IS_3D(insetPARS[pii0+pii]) != 1) ) {
               fprintf( stderr, " not:'%s' ", temp_name);
               DSET_delete(insetPARS[pii0+pii]); // not scal: remove
               insetPARS[pii0 + pii] = NULL;
            }
            else{
               snprintf(wild_names[pii0+pii],31,"%s", temp_name); 
               pii++; 
               fprintf(stderr," '%s' ", temp_name);
            }
         }
      }
      fprintf(stderr,"\n");
      // double check all got filled:
      for( i=0 ; i<N_DTI_SCAL ; i++ ) 
         if( (insetPARS[1+i] == NULL) ) 
            ERROR_exit("Can't open dataset: '%s' file",DTI_SCAL_LABS[i] );

      *pars_top = pii0 + pii; // final upper brick

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







