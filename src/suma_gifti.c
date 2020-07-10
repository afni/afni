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
static NI_group * gifti_surf_to_afni_surf(gifti_image * gim);
static gifti_image * afni_surf_to_gifti_surf(NI_group * aSO);
static byte is_gifti_surface(char * fname);
static byte gifti_surf_meta_to_afni_surf_meta(
               gifti_image * gim, 
               NI_group *aSO);
static byte afni_surf_meta_to_gifti_surf_meta(
               NI_group *aSO,
               gifti_image * gim);
static int flip_float_triples(float * fdata, int ntrip);




/* ------------------------------- AFNI ------------------------------- */

NI_group * afni_open_gifti_surf(char * fname, int read_data)
{
   NI_group * aSO=NULL;
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

int afni_write_gifti_surf( NI_group *aSO, char * fname, 
                           int write_data, int encoding)
{
   gifti_image * gim=NULL;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   
   ENTRY("afni_write_gifti_surf");
   
   if (!(gim = afni_surf_to_gifti_surf(aSO))) {
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
static NI_group *gifti_surf_to_afni_surf(gifti_image * gim)
{
   NI_group * aSO=NULL;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   giiDataArray  * dan=NULL; /* normals */
   long long  rows=0, cols=0;
   NI_element *nelxyz=NULL, *nelnormals=NULL, *nelxform=NULL, *nelijk=NULL;

   int rai_convert = 0;

   ENTRY("gifti_surf_to_afni_surf");

   rai_convert = !AFNI_yesenv("AFNI_GIFTI_IN_RAI");

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
   nelxyz = SUMA_FindNgrNamedElement(aSO, "Node_XYZ");
   NI_SETA_INT(nelxyz,"N_Node",(int)rows);
   NI_SETA_INT(nelxyz,"NodeDim",(int)cols);
   NI_alter_veclen(nelxyz, (int)(rows*cols));
   NI_add_column(nelxyz, NI_FLOAT, (void *)dac->data); 
   NI_SETA_INT(nelxyz,"EmbedDim",3);

   /* maybe convert LPI to RAI   31 Jul 2013 [rickr/zaid] */
   if( rai_convert ) {
      if( cols != 3 ) {
         fprintf(stderr, "** GIFTI 2 AFNI: cols != 3, no RAI conversion\n");
         rai_convert = 0;
      } else flip_float_triples((float *)nelxyz->vec[0], rows);
   }
   
   gifti_DA_rows_cols(  dat, 
                        &(rows),
                        &(cols) );
   nelijk = SUMA_FindNgrNamedElement(aSO, "Mesh_IJK");
   NI_SETA_INT(nelijk, "N_FaceSet", (int)rows);
   NI_SETA_INT(nelijk, "FaceSetDim", (int)cols);
   NI_alter_veclen(nelijk, (int)(rows*cols));
   NI_add_column(nelijk, NI_INT, dat->data);

   if (dan) {
      nelnormals = SUMA_FindNgrNamedElement(aSO, "Node_Normals");
      gifti_DA_rows_cols(  dan, 
                        &(rows),
                        &(cols) );
      NI_alter_veclen(nelnormals, (int)(rows*cols));
      NI_add_column(nelnormals, NI_FLOAT, dan->data);
      /* maybe convert LPI to RAI   31 Jul 2013 [rickr/zaid] */
      if( rai_convert ) flip_float_triples((float *)nelnormals->vec[0], rows);
   } 
   
   if (!gifti_surf_meta_to_afni_surf_meta(gim, aSO)) {
      fprintf(stderr,"** failed to include metadata!\n");
      RETURN(aSO);
   }
   
   RETURN(aSO);
}

static int flip_float_triples(float * fdata, int ntrip)
{
   float * fp = fdata;
   int ind;
   if( !fp || ntrip < 0 ) return 1;

   for( ind = 0; ind < ntrip; ind++ ) {
      *fp = -*fp;  fp++;
      *fp = -*fp;  fp++;
      fp++;
   }
   return 0;
}

/* convert afni surface to GIFTI surface */
static  gifti_image *afni_surf_to_gifti_surf(NI_group *aSO)
{
   gifti_image * gim=NULL;
   giiDataArray  * da=NULL; 
   NI_element *nelxyz=NULL, *nelijk=NULL, *nelnormals=NULL, *nelxform=NULL;
   int           rai_convert=0;
   
   ENTRY("afni_surf_to_gifti_surf");

   /* maybe convert RAI back to LPI   1 Aug 2013 [rickr/zaid] */
   rai_convert = !AFNI_yesenv("AFNI_GIFTI_IN_RAI");

   gifti_globs_from_env();     /* for thd_gifti */
   
   if (!aSO) {
      fprintf( stderr,
               "++ afni_surf_to_gifti_surf: NULL input\n");
      RETURN(gim);         
   }
   
   /* form image */
   if( gifti_get_verb() > 1 ) {  
      fprintf(stderr,"++ creating gifti_image \n" );
   }
   
   /* basic step - create empty image (with a version string) 
      from gifti_create_image */
   gim = (gifti_image *)calloc(1, sizeof(gifti_image));
   if(!gim){ fprintf(stderr,"** failed to alloc gifti_image\n"); RETURN(NULL); }

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
   nelxyz = SUMA_FindNgrNamedElement(aSO, "Node_XYZ");
   da->intent   = NIFTI_INTENT_POINTSET;
   da->datatype = NIFTI_TYPE_FLOAT32;
   da->num_dim  = 2;
   da->dims[0]  = (long long)SUMA_NI_get_int(nelxyz,"N_Node");
   da->dims[1]  = (long long)SUMA_NI_get_int(nelxyz,"NodeDim");
   da->nvals = da->dims[0] * da->dims[1];
   if( GP->write_mode == NI_TEXT_MODE ) da->encoding = GIFTI_ENCODING_ASCII;
   if (!(da->data = malloc(sizeof(float)*da->dims[0]*da->dims[1]))) {
      fprintf( stderr,
            "++ afni_surf_to_gifti_surf: Failed to malloc\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   memcpy( da->data, (void *)nelxyz->vec[0],
            sizeof(float)*da->dims[0]*da->dims[1]); 

   /* convert AFNI's RAI back to GIFTI's LPI */
   if( rai_convert ) flip_float_triples((float *)da->data, da->dims[0]);

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
   nelijk = SUMA_FindNgrNamedElement(aSO, "Mesh_IJK");
   da->intent   = NIFTI_INTENT_TRIANGLE;
   da->datatype = NIFTI_TYPE_INT32;
   da->num_dim  = 2;
   da->dims[0]  = (long long)SUMA_NI_get_int(nelijk,"N_FaceSet");
   da->dims[1]  = (long long)SUMA_NI_get_int(nelijk,"FaceSetDim");
   da->nvals = da->dims[0] * da->dims[1];
   if( GP->write_mode == NI_TEXT_MODE ) da->encoding = GIFTI_ENCODING_ASCII;

   if (!(da->data = malloc(sizeof(int)*da->dims[0]*da->dims[1]))) {
      fprintf( stderr,
            "++ afni_surf_to_gifti_surf: Failed to malloc\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   memcpy( da->data, (void *)nelijk->vec[0],
            sizeof(int)*da->dims[0]*da->dims[1]); 

#if 0   /* GIFTI standard now says no normals with nodes  5 Feb 2010 */

   /* and the normals */
   if( gifti_add_empty_darray(gim, 1) ) {
      fprintf( stderr,
               "++ afni_surf_to_gifti_surf: Failed to add DA\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   da = gim->darray[gim->numDA-1];

   gifti_set_DA_defaults(da);
   nelnormals = SUMA_FindNgrNamedElement(aSO, "Node_Normals");
   da->intent   = NIFTI_INTENT_VECTOR;
   da->datatype = NIFTI_TYPE_FLOAT32;
   da->num_dim  = 2;
   da->dims[0]  = (long long)SUMA_NI_get_int(nelxyz,"N_Node");
   da->dims[1]  = (long long)SUMA_NI_get_int(nelxyz,"NodeDim");
   da->nvals = da->dims[0] * da->dims[1];
   if( GP->write_mode == NI_TEXT_MODE ) da->encoding = GIFTI_ENCODING_ASCII;

   if (!(da->data = malloc(sizeof(float)*da->dims[0]*da->dims[1]))) {
      fprintf( stderr,
            "++ afni_surf_to_gifti_surf: Failed to malloc\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }
   memcpy( da->data, nelnormals->vec[0],
            sizeof(float)*da->dims[0]*da->dims[1]); 
   /* convert AFNI's RAI back to GIFTI's LPI */
   if( rai_convert ) flip_float_triples((float *)da->data, da->dims[0]);

#endif  /* end normals */
   
   /* and now for the grits */
   if (!afni_surf_meta_to_gifti_surf_meta(aSO, gim)) {
      fprintf(stderr,"** failed to include metadata!\n");
      gifti_free_image(gim); gim = NULL; 
      RETURN(gim);
   }

   if( gifti_get_verb() > 1 ) {  
      gifti_disp_gifti_image("afni_surf_to_gifti_surf :",
                             gim, gifti_get_verb() > 3);  
   }
   
   RETURN(gim);
}
 
static byte gifti_surf_meta_to_afni_surf_meta(
               gifti_image * gim, 
               NI_group *aSO)
{
   char *cp=NULL;
   int i=0, j=0, k=0, rai_convert=0;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   NI_element *nelxyz=NULL, *nelijk=NULL, *nelnormals=NULL, *nelxform=NULL;
   double M[4][4];
   double *dv = NULL;
   
   ENTRY("gifti_surf_meta_to_afni_surf_meta");

   rai_convert = !AFNI_yesenv("AFNI_GIFTI_IN_RAI");

   /* Begin with nodelist */
   dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0);
   nelxyz = SUMA_FindNgrNamedElement(aSO, "Node_XYZ");
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "AnatomicalStructurePrimary"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelxyz,
                     "AnatomicalStructurePrimary",
                     cp);
     
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "AnatomicalStructureSecondary"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelxyz,
                     "AnatomicalStructureSecondary",
                     cp);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "GeometricType"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelxyz,
                     "GeometricType",
                     cp);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "UniqueID"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelxyz,
                     "idcode_str",
                     cp);
   
   if (!(cp = gifti_get_meta_value( &dac->meta, 
                                    "date"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelxyz,
                     "date",
                     cp); 
   NI_set_attribute(nelxyz,"inxformspace","no");
      
   nelxform = SUMA_FindNgrNamedElement(aSO, "Coord_System");
   dv = (double *)nelxform->vec[0];
   if (dac->numCS > 0 && dac->coordsys && dac->coordsys[0]) {
      if (!(cp = dac->coordsys[0]->dataspace)) {
         cp = GIFTI_NO_META;
      }
      NI_set_attribute( nelxform,
                        "dataspace",
                        cp); 

      if (!(cp = dac->coordsys[0]->xformspace)) {
         cp = GIFTI_NO_META;
      }
      NI_set_attribute( nelxform,
                        "xformspace",
                        cp); 

      /* XR = F XL F-1, (F-1 = F) 31 Jul 2013 [rickr/zaid] */ 
      if (rai_convert) {
         AFF44_LPI_RAI_FLIP ( M , dac->coordsys[0]->xform );
      } else {
         AFF44_COPY( M , dac->coordsys[0]->xform );
      }
      
      k = 0;
      for (i=0; i<4;++i) {
         for (j=0; j<4; ++j) {
            dv[k] = M[i][j];
            ++k;
         }   
      }
   } else {
      NI_set_attribute( nelxform,"dataspace","NIFTI_XFORM_UNKNOWN");
      NI_set_attribute( nelxform,"xformspace","NIFTI_XFORM_UNKNOWN");
      k=0;
      for (i=0; i<4; ++i) 
         for (j=0; j< 4; ++j) {
            if (i==j)   dv[k] = 1.0; 
            else        dv[k] = 0.0;
            ++k;
         }
   }
   dac = NULL; /* make sure it is not used below */
   
   /* Now do the FaceSetList */
   dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0);
   nelijk = SUMA_FindNgrNamedElement(aSO,"Mesh_IJK");
   if (!(cp = gifti_get_meta_value( &dat->meta, 
                                    "TopologicalType"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelijk,
                     "TopologicalType",
                     cp);
      
   if (!(cp = gifti_get_meta_value( &dat->meta, 
                                    "UniqueID"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelijk,
                     "idcode_str",
                     cp);
   
   if (!(cp = gifti_get_meta_value( &dat->meta, 
                                    "date"))) {
      cp = GIFTI_NO_META;
   }
   NI_set_attribute( nelijk,
                     "date",
                     cp);
   
   
   RETURN(1);
}

static byte afni_surf_meta_to_gifti_surf_meta(
               NI_group *aSO, 
               gifti_image * gim )
{
   int i=0, j=0;
   giiDataArray  * dac=NULL; /* coords */
   giiDataArray  * dat=NULL; /* triangles */
   NI_element *nelxyz=NULL, *nelijk=NULL, *nelnormals=NULL, *nelxform=NULL;
   double *dv=NULL, M[4][4];
   int k=0, rai_convert=0;
   
   ENTRY("afni_surf_meta_to_gifti_surf_meta");
   
   rai_convert = !AFNI_yesenv("AFNI_GIFTI_IN_RAI");
      
   /* Begin with nodelist */
   dac = gifti_find_DA(gim, NIFTI_INTENT_POINTSET, 0);
   nelxyz = SUMA_FindNgrNamedElement(aSO,"Node_XYZ");
   if( gifti_add_to_meta(&dac->meta,
                         "AnatomicalStructurePrimary",
                         NI_get_attribute(nelxyz,"AnatomicalStructurePrimary"),
                         1) ) {
      fprintf(stderr,"** failed to add meta AnatomicalStructurePrimary.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                      "AnatomicalStructureSecondary",
                      NI_get_attribute(nelxyz,"AnatomicalStructureSecondary"),
                      1) ) {
      fprintf(stderr,"** failed to add meta AnatomicalStructureSecondary.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "GeometricType",
                         NI_get_attribute(nelxyz,"GeometricType"),
                         1) ) {
      fprintf(stderr,"** failed to add meta GeometricType.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "UniqueID",
                         NI_get_attribute(nelxyz,"idcode_str"),
                         1) ) {
      fprintf(stderr,"** failed to add meta UniqueID.\n"); 
      RETURN(1); 
   }
   if( gifti_add_to_meta(&dac->meta,
                         "date",
                         NI_get_attribute(nelxyz,"date"),
                         1) ) {
      fprintf(stderr,"** failed to add meta date.\n"); 
      RETURN(1); 
   }

   /* free any old CoordSystems and add a single new one      */
   /* - coordsys now an array of pointers, 8 May 2008 [rickr] */
   gifti_free_CS_list(dac);
   if( gifti_add_empty_CS(dac) ) {
      fprintf(stderr,"** failed to add empty CoordSystem\n");
      RETURN(1);
   }
   
   nelxform = SUMA_FindNgrNamedElement(aSO,"Coord_System");
   dac->coordsys[0]->dataspace = 
         gifti_strdup(NI_get_attribute(nelxform,"dataspace"));
   dac->coordsys[0]->xformspace = 
         gifti_strdup(NI_get_attribute(nelxform,"xformspace"));
   dv = (double *)nelxform->vec[0];
   k = 0;
   for (i=0; i<4; ++i) 
         for (j=0; j< 4; ++j) { 
            dac->coordsys[0]->xform[i][j] = dv[k];
            ++k;
         }
   
   /* XR = F XL F-1, (F-1 = F) 31 Jul 2013 [rickr/zaid] */ 
   if (rai_convert) {
      AFF44_COPY( M , dac->coordsys[0]->xform );
      AFF44_LPI_RAI_FLIP ( dac->coordsys[0]->xform , M );
   }
   
   dac = NULL; /* make sure it is not used below */
   
   /* Now do the FaceSetList */
   dat = gifti_find_DA(gim, NIFTI_INTENT_TRIANGLE, 0);
   nelijk = SUMA_FindNgrNamedElement(aSO,"Mesh_IJK");
   if( gifti_add_to_meta(&dat->meta,
                         "TopologicalType",
                         NI_get_attribute(nelijk,"TopologicalType"),
                         1) ) {
      fprintf(stderr,"** failed to add meta TopologicalType.\n"); 
      RETURN(1); 
   }

   if( gifti_add_to_meta(&dat->meta,
                         "UniqueID",
                         NI_get_attribute(nelijk,"idcode_str"),
                         1) ) {
      fprintf(stderr,"** failed to add meta UniqueID.\n"); 
      RETURN(1); 
   }

   if( gifti_add_to_meta(&dat->meta,
                         "date",
                         NI_get_attribute(nelijk,"date"),
                         1) ) {
      fprintf(stderr,"** failed to add meta date.\n"); 
      RETURN(1); 
   }
   
   RETURN(1);
}
