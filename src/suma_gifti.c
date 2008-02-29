/*----------------------------------------------------------------------
 * 06 Feb 2008: top-level routines to read GIFTI surfaces        [zss]
 *
 * These main routines should match those in gifti_choice.c.
 *
 *----------------------------------------------------------------------
 */

#include "mrilib.h"
#include "gifti_io.h"
#include "suma_afni_surface.h"

#define GIFTI_NO_META "Unknown"

/*--- read gifti routines ---*/
static AFNI_SurfaceObject * gifti_surf_to_afni_surf(gifti_image * gim, 
                                                    int copy_data);
static byte is_gifti_surface(char * fname);
static byte gifti_surf_meta_to_afni_surf_meta(
               gifti_image * gim, 
               AFNI_SurfaceObject *aSO);

/*--- write gifti routines ---*/



/* ------------------------------- AFNI ------------------------------- */

AFNI_SurfaceObject * afni_open_gifti_surf(char * fname, int read_data)
{
   AFNI_SurfaceObject * aSO=NULL;
   gifti_image * gim=NULL;

   ENTRY("afni_open_gifti_surf");

   /* read gim */
   gifti_globs_from_env();     /* for thd_gifti */
   if( !fname ) {
     if( GP->verb > 0 ) fprintf( stderr,
                                 "** afni_open_gifti_surf: null filename\n");
     RETURN(NULL);
   }

   if( GP->verb > 2 ) fprintf(stderr,"-- NI_read_gifti from '%s'\n", fname );
   gifti_set_verb(GP->gverb);

   if (!is_gifti_surface(fname)) {
      if( GP->verb > 0 ) 
         fprintf( stderr,
                  "** afni_open_gifti_surf: %s is not a surface\n", fname);
      RETURN(NULL);
   }  
   gim = gifti_read_image(fname, read_data);
   if( !gim ) {
     if( GP->verb > 1 )
         fprintf(stderr,"-- NI_read_gifti: failed to read '%s'\n", fname);
     RETURN(NULL);
   }

   /* turn image to surface object */
   aSO = gifti_surf_to_afni_surf (gim, read_data);

   /* free gim */
   gifti_free_image(gim); gim = NULL; 

   RETURN(aSO);
}

static byte is_gifti_surface(char * fname)
{
   gifti_image * gim=NULL;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   
   ENTRY("is_gifti_surface");
   
   if( !fname ) RETURN(0);
   
   gim = gifti_read_image(fname, 0);
   
   if( !gim ) { 
      gifti_free_image(gim); gim = NULL; 
      RETURN(0);
   }
   
   if( !(dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0)) ||
       !(dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0))    ) {
      gifti_free_image(gim); gim = NULL; 
      RETURN(0);
   }
                  
   gifti_free_image(gim); gim = NULL;
   
   RETURN(1);
}
/* read GIFTI surface and convert to afni surface */
static AFNI_SurfaceObject *gifti_surf_to_afni_surf(gifti_image * gim, 
                                                   int read_data)
{
   AFNI_SurfaceObject * aSO=NULL;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   long long  rows=0, cols=0;
   
   ENTRY("gifti_surf_to_afni_surf");

   gifti_globs_from_env();     /* for thd_gifti */
   if( (dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0)) &&
       (dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0))    ) {
      if( GP->verb > 1 )
         fprintf( stderr,
                  "++ gifti_surf_to_afni_surf: found pointset and triangles\n"); 
   } else {
        fprintf(stderr,"** failed to find coordinate/triangle structs\n");
        RETURN(aSO);
   }
   if (dac->datatype != NIFTI_TYPE_FLOAT32) { 
      fprintf(stderr,"** NIFTI_INTENT_POINTSET not FLOAT32\n");
      RETURN(aSO);
   }
   if (dat->datatype != NIFTI_TYPE_INT32) { 
      fprintf(stderr,"** NIFTI_INTENT_TRIANGLE not INT32\n");
      RETURN(aSO);
   }
    
   aSO = SUMA_NewAfniSurfaceObject();  
   gifti_DA_rows_cols(  dac, 
                        &(rows),
                        &(cols) );
   aSO->ps->N_Node = (int)rows;
   aSO->ps->NodeDim = (int)cols;
   aSO->ps->NodeList = (float *)dac->data; dac->data = NULL;
   aSO->ps->EmbedDim = 3; /* look into that (and whether you need it later ) */
   
   gifti_DA_rows_cols(  dat, 
                        &(rows),
                        &(cols) );
   aSO->tr->N_FaceSet = (int)rows;
   aSO->tr->FaceSetDim = (int)cols;
   aSO->tr->FaceSetList = (int *)dat->data; dat->data = NULL;
  
   if (!gifti_surf_meta_to_afni_surf_meta(gim, aSO)) {
      fprintf(stderr,"** failed to include metadata!\n");
      RETURN(aSO);
   }
   
   
   
   RETURN(aSO);
}

static byte gifti_surf_meta_to_afni_surf_meta(
               gifti_image * gim, 
               AFNI_SurfaceObject *aSO)
{
   char *cp=NULL;
   int i=0, j=0;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */

   ENTRY("gifti_surf_meta_to_afni_surf_meta");
   
   /* Begin with nodelist */
   dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "AnatomicalStructurePrimary"))) {
      cp = GIFTI_NO_META;
   }
   aSO->ps->AnatomicalStructurePrimary = gifti_strdup(cp);
     
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "AnatomicalStructureSecondary"))) {
      cp = GIFTI_NO_META;
   }
   aSO->ps->AnatomicalStructureSecondary = gifti_strdup(cp);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "GeometricType"))) {
      cp = GIFTI_NO_META;
   }
   aSO->ps->GeometricType = gifti_strdup(cp);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "UniqueID"))) {
      cp = GIFTI_NO_META;
   }
   aSO->ps->UniqueID = gifti_strdup(cp);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "date"))) {
      cp = GIFTI_NO_META;
   }
   aSO->ps->date = gifti_strdup(cp); 
      
   if (dac->coordsys) {
      if (!(cp = dac->coordsys->dataspace)) {
         cp = GIFTI_NO_META;
      }
      aSO->ps->dataspace = gifti_strdup(cp); 

      if (!(cp = dac->coordsys->xformspace)) {
         cp = GIFTI_NO_META;
      }
      aSO->ps->xformspace = gifti_strdup(cp); 

      for (i=0; i<4; ++i) 
         for (j=0; j< 4; ++j) 
            aSO->ps->xform[i][j] = dac->coordsys->xform[i][j];
   } else {
      aSO->ps->dataspace = gifti_strdup("NIFTI_XFORM_UNKNOWN");
      aSO->ps->xformspace = gifti_strdup("NIFTI_XFORM_UNKNOWN");
      for (i=0; i<4; ++i) 
         for (j=0; j< 4; ++j) 
            if (i==j)   aSO->ps->xform[i][j] = 1.0; 
            else        aSO->ps->xform[i][j] = 0.0;
   }
   
   dac = NULL; /* make sure it is not used below */
   
   /* Now do the FaceSetList */
   dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0);
   
   if (!(cp = gifti_get_meta_value( &dat->meta, 
                                    "TopologicalType"))) {
      cp = GIFTI_NO_META;
   }
   aSO->tr->TopologicalType = gifti_strdup(cp);
      
   if (!(cp = gifti_get_meta_value( &dat->meta, 
                                    "UniqueID"))) {
      cp = GIFTI_NO_META;
   }
   aSO->tr->UniqueID = gifti_strdup(cp);
   
   if (!(cp = gifti_get_meta_value( &dat->meta, 
                                    "date"))) {
      cp = GIFTI_NO_META;
   }
   aSO->tr->date = gifti_strdup(cp);
   
   
   RETURN(1);
}

