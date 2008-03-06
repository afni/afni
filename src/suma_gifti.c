/*----------------------------------------------------------------------
 * 06 Feb 2008: top-level routines to read GIFTI surfaces        [zss]
 *
 * These main routines should match those in gifti_choice.c.
 *
 * Do not call anything that is in suma_datasets.c or libSUMA.a
 *----------------------------------------------------------------------
 */

#include "mrilib.h"
#include "gifti_io.h"
#include "suma_afni_surface.h"

#define GIFTI_NO_META "Unknown"

/*--- read/write gifti routines ---*/
static AFNI_SurfaceObject * gifti_surf_to_afni_surf(gifti_image * gim);
static gifti_image * afni_surf_to_gifti_surf(AFNI_SurfaceObject * aSO, 
                                             int CopyData);
static byte is_gifti_surface(char * fname);
static byte gifti_surf_meta_to_afni_surf_meta(
               gifti_image * gim, 
               AFNI_SurfaceObject *aSO);
static byte afni_surf_meta_to_gifti_surf_meta(
               AFNI_SurfaceObject *aSO,
               gifti_image * gim);




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
   aSO = gifti_surf_to_afni_surf (gim);

   /* free gim */
   gifti_free_image(gim); gim = NULL; 

   RETURN(aSO);
}

int afni_write_gifti_surf( AFNI_SurfaceObject *aSO, char * fname, 
                           int write_data, int encoding)
{
   gifti_image * gim=NULL;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   
   ENTRY("afni_write_gifti_surf");
   
   if (!(gim = afni_surf_to_gifti_surf(aSO, 1))) {
      fprintf( stderr, 
               "** Failed to gimate\n");
      RETURN(0);   
   }
   if (encoding > GIFTI_ENCODING_UNDEF && encoding < GIFTI_ENCODING_MAX) {
      /* enforce this encoding */
       if( !(dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0)) ||
          !(dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0))    ) {
         RETURN(0);
      }
      dac->encoding = encoding;
      dat->encoding = encoding;     
   }
   if (gifti_write_image(gim, fname, write_data)) {
      fprintf( stderr, 
               "** Failed to write_image\n");
      gifti_free_image(gim); gim = NULL;
      RETURN(0);   
   }
   
   gifti_free_image(gim); gim = NULL;
   
   RETURN(1);
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

/* convert GIFTI surface to afni surface */
static AFNI_SurfaceObject *gifti_surf_to_afni_surf(gifti_image * gim)
{
   AFNI_SurfaceObject * aSO=NULL;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   giiDataArray  * dan=NULL; /* normals */
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
   
   dan = gifti_find_DA(gim, NIFTI_INTENT_VECTOR, 0);
   if (dan && dan->datatype != NIFTI_TYPE_FLOAT32) { 
      fprintf(stderr,"** NIFTI_INTENT_VECTOR not FLOAT32\n");
      RETURN(aSO);
   }
   if (dan && dan->nvals != dac->nvals) {
      fprintf(stderr,"** NIFTI_INTENT_VECTOR number of values differs"
                     "   from that of NIFTI_INTENT_POINTSET\n");
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
   
   if (dan) {
      aSO->NodeNormList = (float *)dan->data; dan->data = NULL;
   } else aSO->NodeNormList = NULL;
   
   if (!gifti_surf_meta_to_afni_surf_meta(gim, aSO)) {
      fprintf(stderr,"** failed to include metadata!\n");
      RETURN(aSO);
   }
   
   
   
   RETURN(aSO);
}

/* convert afni surface to GIFTI surface */
static  gifti_image *afni_surf_to_gifti_surf(AFNI_SurfaceObject *aSO, 
                                             int CopyData)
{
   gifti_image * gim=NULL;
   giiDataArray  * da=NULL; 
   
   ENTRY("afni_surf_to_gifti_surf");

   gifti_globs_from_env();     /* for thd_gifti */
   
   if (!aSO) {
      fprintf( stderr,
               "++ afni_surf_to_gifti_surf: NULL input\n");
      RETURN(gim);         
   }
   
   /* form image */
   if( G.verb > 1 ) {  
      fprintf(stderr,"++ creating gifti_image \n" );
   }
   
   /* basic step - create empty image (with a version string) 
      from gifti_create_image */
   gim = (gifti_image *)calloc(1, sizeof(gifti_image));
   if(!gim){ fprintf(stderr,"** failed to alloc gifti_image\n"); return NULL; }

   gifti_clear_gifti_image(gim);
   gim->version = gifti_strdup(GIFTI_XML_VERSION);

   /* now add the pointset */
   if( gifti_add_empty_darray(gim, 1) ) {
      fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to add DA\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   da = gim->darray[gim->numDA-1];

   gifti_set_DA_defaults(da);
   da->intent   = NIFTI_INTENT_POINTSET;
   da->datatype = NIFTI_TYPE_FLOAT32;
   da->num_dim  = 2;
   da->dims[0]  = (long long)aSO->ps->N_Node;
   da->dims[1]  = (long long)aSO->ps->NodeDim;
   da->nvals = da->dims[0] * da->dims[1];
   if( GP->write_mode == NI_TEXT_MODE ) da->encoding = GIFTI_ENCODING_ASCII;

   if (!CopyData) {
      /* copy by reference */
      da->data = (void *)aSO->ps->NodeList; aSO->ps->NodeList = NULL;
   } else {
      if (!(da->data = malloc(sizeof(float)*da->dims[0]*da->dims[1]))) {
         fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to malloc\n");
         gifti_free_image(gim); gim = NULL; 
         RETURN(gim);
      }
      memcpy( da->data, (void *)aSO->ps->NodeList,
               sizeof(float)*da->dims[0]*da->dims[1]); 
      
   }
   /* coordsys is added in the afni_surf_meta_to_gifti_surf_meta function */
      
   /* and the triangles */
   if( gifti_add_empty_darray(gim, 1) ) {
      fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to add DA\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   da = gim->darray[gim->numDA-1];

   gifti_set_DA_defaults(da);
   da->intent   = NIFTI_INTENT_TRIANGLE;
   da->datatype = NIFTI_TYPE_INT32;
   da->num_dim  = 2;
   da->dims[0]  = (long long)aSO->tr->N_FaceSet;
   da->dims[1]  = (long long)aSO->tr->FaceSetDim;
   da->nvals = da->dims[0] * da->dims[1];
   if( GP->write_mode == NI_TEXT_MODE ) da->encoding = GIFTI_ENCODING_ASCII;

   if (!CopyData) {
      /* copy by reference */
      da->data = (void *)aSO->tr->FaceSetList; aSO->tr->FaceSetList = NULL;
   } else {
      if (!(da->data = malloc(sizeof(int)*da->dims[0]*da->dims[1]))) {
         fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to malloc\n");
         gifti_free_image(gim); gim = NULL; 
         RETURN(gim);
      }
      memcpy( da->data, (void *)aSO->tr->FaceSetList,
               sizeof(int)*da->dims[0]*da->dims[1]); 
      
   }
   
   /* and the normals */
   if( gifti_add_empty_darray(gim, 1) ) {
      fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to add DA\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   da = gim->darray[gim->numDA-1];

   gifti_set_DA_defaults(da);
   da->intent   = NIFTI_INTENT_VECTOR;
   da->datatype = NIFTI_TYPE_FLOAT32;
   da->num_dim  = 2;
   da->dims[0]  = (long long)aSO->ps->N_Node;
   da->dims[1]  = (long long)aSO->ps->NodeDim;
   da->nvals = da->dims[0] * da->dims[1];
   if( GP->write_mode == NI_TEXT_MODE ) da->encoding = GIFTI_ENCODING_ASCII;

   if (!CopyData || !aSO->NodeNormList) {
      /* copy by reference */
      da->data = (void *)aSO->NodeNormList; aSO->NodeNormList = NULL;
   } else {
      if (!(da->data = malloc(sizeof(float)*da->dims[0]*da->dims[1]))) {
         fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to malloc\n");
         gifti_free_image(gim); gim = NULL; 
         RETURN(gim);
      }
      memcpy( da->data, (void *)aSO->NodeNormList,
               sizeof(float)*da->dims[0]*da->dims[1]); 
      
   }
   
   /* and now for the grits */
   if (!afni_surf_meta_to_gifti_surf_meta(aSO, gim)) {
      fprintf(stderr,"** failed to include metadata!\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }

   if( G.verb > 1 ) {  
      gifti_disp_gifti_image("afni_surf_to_gifti_surf :",gim, G.verb > 3);  
   }
   
   RETURN(gim);
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

static byte afni_surf_meta_to_gifti_surf_meta(
               AFNI_SurfaceObject *aSO, 
               gifti_image * gim )
{
   char *cp=NULL;
   int i=0, j=0;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */

   ENTRY("afni_surf_meta_to_gifti_surf_meta");
   
   /* Begin with nodelist */
   dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0);
   if( gifti_add_to_meta(&dac->meta,
                         "AnatomicalStructurePrimary",
                         aSO->ps->AnatomicalStructurePrimary,
                         1) ) {
      fprintf(stderr,"** failed to add meta AnatomicalStructurePrimary.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "AnatomicalStructureSecondary",
                         aSO->ps->AnatomicalStructureSecondary,
                         1) ) {
      fprintf(stderr,"** failed to add meta AnatomicalStructureSecondary.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "GeometricType",
                         aSO->ps->GeometricType,
                         1) ) {
      fprintf(stderr,"** failed to add meta GeometricType.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "UniqueID",
                         aSO->ps->UniqueID,
                         1) ) {
      fprintf(stderr,"** failed to add meta UniqueID.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "date",
                         aSO->ps->date,
                         1) ) {
      fprintf(stderr,"** failed to add meta date.\n"); 
      RETURN(1); 
   }
   if (dac->coordsys) {
      gifti_free_CoordSystem(dac->coordsys); dac->coordsys = NULL;
   }
   
   dac->coordsys = (giiCoordSystem *)malloc(sizeof(giiCoordSystem));
   gifti_clear_CoordSystem(dac->coordsys);
   dac->coordsys->dataspace = gifti_strdup(aSO->ps->dataspace);
   dac->coordsys->xformspace = gifti_strdup(aSO->ps->xformspace);
   for (i=0; i<4; ++i) 
         for (j=0; j< 4; ++j) 
            dac->coordsys->xform[i][j] = aSO->ps->xform[i][j];
            
   dac = NULL; /* make sure it is not used below */
   
   /* Now do the FaceSetList */
   dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0);
   
   if( gifti_add_to_meta(&dat->meta,
                         "TopologicalType",
                         aSO->tr->TopologicalType,
                         1) ) {
      fprintf(stderr,"** failed to add meta TopologicalType.\n"); 
      RETURN(1); 
   }

   if( gifti_add_to_meta(&dat->meta,
                         "UniqueID",
                         aSO->tr->UniqueID,
                         1) ) {
      fprintf(stderr,"** failed to add meta UniqueID.\n"); 
      RETURN(1); 
   }

   if( gifti_add_to_meta(&dat->meta,
                         "date",
                         aSO->tr->date,
                         1) ) {
      fprintf(stderr,"** failed to add meta date.\n"); 
      RETURN(1); 
   }
   
   RETURN(1);
}
