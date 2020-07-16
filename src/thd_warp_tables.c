/* functions to access tables of datasets within a session */
/* made to deal with transformations across generic spaces */
#include "mrilib.h"
#undef DEBUG_WARPTABLES
/* extern atlas_space_list global_atlas_spaces; */

static int nspaces = 3; /* number of spaces to include in tables */
THD_3dim_dataset *
get_session_dset_id(THD_session *sess, MCW_idcode idcode, int space_index);
THD_3dim_dataset *
get_session_dset(THD_session *sess, int index, int space_index);
int
set_session_dset(THD_3dim_dataset *dset, THD_session *sess,
   int index, int space_index);
void set_nspaces(int n);
void set_atlas_nspaces(void);
int get_nspaces(void);   
void free_session_row(THD_session *sess, int index);
   
   
/* get the dataset with a specific ID code and a specific space index */
THD_3dim_dataset *
get_session_dset_id(THD_session *sess, MCW_idcode idcode, int space_index)
{
   THD_3dim_dataset *dset = NULL;
   THD_dsarr *dsrow;
   int i,j;
   
   ENTRY("get_session_dset_id");
   for(j=0; j < sess->num_dsset; j++){
      dsrow = sess->dsrow[j];   /* list of datasets in different spaces */
      for(i=0;i<nspaces;i++) { /* dsrow->nds;i++){ */
         dset = dsrow->ds[i];
         if( dset != NULL && EQUIV_IDCODES(idcode,dset->idcode) )
            RETURN(get_session_dset(sess, i, space_index));
      }
   }     
   RETURN(NULL);   /* no dataset with same ID code */
}

/* get the dataset at a particular session index for a specific space index */
THD_3dim_dataset *
get_session_dset(THD_session *sess, int index, int space_index)
{
   THD_3dim_dataset *dset = NULL;
   THD_dsarr *dsrow;
      
   ENTRY("get_session_dset");
#ifdef DEBUG_WARPTABLES
   fprintf(stderr,"getsession0 index %d %d\n",index, space_index);
#endif
   if(sess->dsrow==NULL)
      RETURN(NULL);
#ifdef DEBUG_WARPTABLES
   fprintf(stderr,"getsession1 index %d %d\n",index, space_index);
#endif
      if(index>= ((sess->ndsets)-1))
         RETURN(NULL);
#ifdef DEBUG_WARPTABLES
   fprintf(stderr,"getsession1.1 index %d %d\n",index, space_index);
#endif

      dsrow = sess->dsrow[index];   /* list of datasets in different spaces */
      if(dsrow==NULL)
          RETURN(NULL);
      dset = dsrow->ds[space_index];
      RETURN(dset);
}

/* put a dataset into a session */
int
set_session_dset(THD_3dim_dataset *dset, THD_session *sess,
   int index, int space_index)
{
   THD_dsarr  *dsrow;
#if 0
   THD_3dim_dataset *td;
   int i, rmrow;
#endif
   
   ENTRY("set_session_dset");
#ifdef DEBUG_WARPTABLES
   fprintf(stderr,"setsession0 index %d %d\n",index, space_index);
#endif
   /* dataset is NULL, so this may called to remove the dataset, space */
#if 0
   rmrow = 1;
   if(dset==NULL) {
      for(i=0;i<nspaces;i++) {
         if(i!=space_index) {
            td = GET_SESSION_DSET(sess, index, i);
            if(td!=NULL) {
               rmrow = 0;
               break;
            }
         }
      }
      if(rmrow){
         free_session_row(sess,index);
         RETURN(0);
      }         
   }
#endif

   if( sess->dsrow == NULL ) {
      /* need to allocate for first row  - index should always be zero */
      sess->dsrow = (THD_dsarr **) calloc( 1,sizeof(THD_dsarr *));
      dsrow = (THD_dsarr *) calloc(1, sizeof(THD_dsarr));
      if((sess->dsrow==NULL) || (dsrow == NULL)) {
         RETURN(1);
      }
      sess->dsrow[index] = dsrow;
      dsrow->nds = 1;
      dsrow->ds = (THD_3dim_dataset **) 
           calloc(nspaces, sizeof(THD_3dim_dataset *));
      if(dsrow->ds == NULL)   /* could not allocate memory for dataset row */
         RETURN(1);
      dsrow->ds[space_index] = dset;
      sess->ndsets = 1;
      RETURN(0);   
   }
   if(index >= ((sess->ndsets)-1)) {
#ifdef DEBUG_WARPTABLES
   fprintf(stderr,"setsession1 index %d\n",index);
#endif
      /* need to allocate for another row  - index should always be next one */
      sess->dsrow = (THD_dsarr **) realloc(sess->dsrow, 
                     (1+index)*sizeof(THD_dsarr *));
      dsrow = (THD_dsarr *) calloc(1, sizeof(THD_dsarr));
      if((sess->dsrow==NULL) || (dsrow == NULL)) {
         RETURN(1);
      }
      sess->dsrow[index] = dsrow;
      dsrow->nds = 1;
      dsrow->ds = (THD_3dim_dataset **) 
           calloc(nspaces, sizeof(THD_3dim_dataset *));
      if(dsrow->ds == NULL)   /* could not allocate memory for dataset row */
         RETURN(1);
      dsrow->ds[space_index] = dset;
      sess->ndsets++;
      RETURN(0);
   }
#ifdef DEBUG_WARPTABLES
   fprintf(stderr,"setsession2 index %d %d**********\n",index, space_index);
#endif

   dsrow = sess->dsrow[index];   /* list of datasets in different spaces */
   /* check for existing entry on this row and space_index */
   if(get_session_dset(sess, index, space_index)) {
      /* replace existing dataset with this new one */
      dsrow->ds[space_index] = dset;
   }
   else {
      /* allocate and set dataset */
      dsrow->nds++;
      /* for now skip sparse dataset table, just use all spaces */
/*      dsrow->ds = (THD_3dim_dataset *) 
          realloc(dsrow->ds, dsrow->nds * sizeof(THD_3dim_dataset *));*/
      dsrow->ds[space_index] = dset;
   }
   RETURN(0);
}

/* free the row from the session freeing memory */
void
free_session_row(THD_session *sess, int index)
{
   int i;
   THD_dsarr  *dsrow;
   THD_3dim_dataset **dsetptr;

   ENTRY("free_session_row");

   if(sess->dsrow==NULL) {
      EXRETURN;
   }

   dsrow = sess->dsrow[index];   /* list of datasets in different spaces */
   if(dsrow==NULL)
      EXRETURN;
   for (i=0; i<(dsrow->nds); i++) { /* free each space view for the row */
     dsetptr = dsrow->ds+i;
     if(dsetptr!=NULL)
        free(dsetptr);
   }
   free(dsrow);
   dsrow = NULL;
   
   EXRETURN;
}

/* free the session warp table */
void
free_session_table(THD_session *sess)
{
   int i;
   
   ENTRY("free_session_table");
   if(sess->dsrow==NULL)
      EXRETURN;

   for(i=0;i<sess->ndsets;i++)
       free_session_row(sess, i);   
   free(sess->dsrow);
   EXRETURN;
}

/* explicitly set the number of spaces to consider */
void
set_nspaces(int n)
{
   nspaces = n;
}

/* set number of spaces based on atlas spaces available */
void
set_atlas_nspaces()
{
   ATLAS_SPACE_LIST *asl=get_G_space_list();
   
   if (asl) nspaces = asl->nspaces;
   else nspaces = 0;
}

/* accessor function to say how many spaces we are currently dealing with */
int
get_nspaces()
{
   return(nspaces);
}

/* dump available spaces for a dataset row */
void
dump_spaces(THD_session *sess, int index)
{
   int i;
   THD_dsarr  *dsrow;
   THD_3dim_dataset **dsetptr, *dset;

   ENTRY("session_dump_row_spaces");

   if(sess->dsrow==NULL) {
      EXRETURN;
   }

   dsrow = sess->dsrow[index];   /* list of datasets in different spaces */
   if(dsrow==NULL)
      EXRETURN;
   for (i=0; i<(dsrow->nds); i++) { /* free each space view for the row */
      dsetptr = dsrow->ds+i;
      if(dsetptr!=NULL){
         dset = *dsetptr;
         printf(" %s ", dset->atlas_space);
      }
   }
   printf("\n");
   EXRETURN;
}

