/* thd_atlas.c */
/* functions to access atlas, template, space of datasets */

/* access current space of dataset */

#include "mrilib.h"
#include "thd_atlas.h"
#define SUMA_noFunc
#include "suma_suma.h"

#ifdef SOLARIS
# define strcasestr strstr  /* stupid Solaris */
#endif

extern int * SUMA_Dijkstra_generic (int N_Node, 
                     float *NodeList, int NodeDim, int dist_metric,
                     int *N_Neighbv, int **FirstNeighb, float **FirstNeighbDist,
                     int Nx, int Ny, 
                     byte *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path,
                     int verb);

static int debug_niml = 1;

static char *preferred_atlas_name = NULL;
static int **FirstNeighb=NULL;
static float **FirstNeighbDist=NULL; 
static int *N_Neighb;

char *
THD_get_space(THD_3dim_dataset *dset)
{
   char  *eee;

   ENTRY("thd_get_space");

#ifdef DEBUG_SPACES
   fprintf(stderr,"getting space for dset: %s\n", dset->dblk->diskptr->brick_name); 
#endif

   if(!dset) RETURN(NULL);
   
   if(dset->atlas_space[0] != '\0'){
#ifdef DEBUG_SPACES
      fprintf(stderr,"Space is %s. Assigned already.\n",dset->atlas_space);
#endif
      RETURN(dset->atlas_space);
   }
   switch(dset->view_type){
      default:
      case 0:
         MCW_strncpy(dset->atlas_space, "ORIG", THD_MAX_NAME);
         break;
      case 1:
         MCW_strncpy(dset->atlas_space, "ACPC", THD_MAX_NAME);
         break;
      /* view is +tlrc */
      case 2:
         /* check environment for default space */
         /* possible values are TLRC, MNI_ANAT, and MNI */
         /* but others are possible */
         {
#ifdef DEBUG_SPACES
         fprintf(stderr,"\n  eee0 \n");
#endif
         eee = getenv("AFNI_DEFAULT_STD_SPACE");
         if (eee) {
#ifdef DEBUG_SPACES
            fprintf(stderr,"\n  eee2 %s \n", eee);
#endif
            MCW_strncpy(dset->atlas_space, eee, THD_MAX_NAME);
         }
         /* no environment variable set, default to TLRC */
         else {               
#ifdef DEBUG_SPACES
            fprintf(stderr,"\n  eee3 \n");
#endif
             MCW_strncpy(dset->atlas_space, "TLRC", THD_MAX_NAME);}
         }
         break;
   }
#ifdef DEBUG_SPACES
   fprintf(stderr,"Space is %s\n",dset->atlas_space);
#endif

   RETURN(dset->atlas_space);
}

/* assign space codes used by whereami for specific atlases */
int
THD_space_code(char *space)
{
   ENTRY("THD_space_code");
   if (strcmp(space, "TLRC")==0)
      RETURN(AFNI_TLRC_SPC);
   if (strcmp(space, "MNI_ANAT")==0)
      RETURN(MNI_ANAT_SPC);
   if (strcmp(space, "MNI")==0)
      RETURN(MNI_SPC);
   RETURN(UNKNOWN_SPC);   /* if none of the above, non-standard space */
}


/* apply attribute space to dataset structure */
void
THD_apply_space_atr(THD_3dim_dataset *dset)
{
   /* allow for non-existent dset structures*/
   /* allow for environment variable */ 
}

/* assign space to dataset structure */
void
THD_assign_space(THD_3dim_dataset *dset, char * space)
{

}

/* make space attribute for dataset */
void
THD_make_space_atr(THD_3dim_dataset *dset)
{
   /* allow for non-existent dset structures*/
   /* allow for environment variable */ 

}


#if 0
/* return known ATLAS codes from atlas name */
AFNI_ATLAS_CODES THD_atlas_code(char *atlas_name)
{
   ENTRY("THD_atlas_code");
   /* check if atlas name is valid */
   if(atlas_name==NULL) RETURN(UNKNOWN_ATLAS);

   /* check for known names */
   if (strcmp(atlas_name, "TT_Daemon")==0)
      RETURN(AFNI_TLRC_ATLAS);
   if (strcmp(atlas_name, "CA_EZ_N27_MPM")==0)
      RETURN(CA_EZ_N27_MPM_ATLAS);
   if (strcmp(atlas_name, "CA_EZ_N27_PM")==0)
      RETURN(CA_EZ_N27_ML_ATLAS);
   if (strcmp(atlas_name, "CA_EZ_N27_ML")==0)
      RETURN(CA_EZ_N27_PMAPS_ATLAS);
   if (strcmp(atlas_name, "CA_EZ_N27_LR")==0)
      RETURN(CA_EZ_N27_LR_ATLAS);

    RETURN(CUSTOM_ATLAS);   /* if none of the above, non-standard space */
}

/* set one atlas as the preferred atlas name */
THD_set_atlas_name(char *atlas_name)
{
   if(preferred_atlas_name!=NULL)
      free(preferred_atlas_name);
   MCW_strncpy(preferred_atlas_name, atlas_name, THD_MAX_NAME);
}

#endif

/* atlas dataset structures */
/* atlas datasets will have NIML element attributes that contain 
   segmentation information via index values and segment labels (ROI labels),
   color scale information that colorizes each segment. 
   This info is along with the space info that will be part of usual datasets. */
/* need to also consider the probabilistic atlas type here too */   
/* read atlas labels */

/* read atlas color scale */
ATLAS_LUT *read_atlas_lut(THD_3dim_dataset *dset)
{
   ATLAS_LUT *atlasdset_lut;
   void *lut_atr;
     
   ENTRY("read_atlas_lut");
   
   /* get lookup table from dataset */
   lut_atr = THD_find_atr(dset->dblk, "ATLAS_LUT");
   if(lut_atr) {
      atlasdset_lut = malloc(sizeof(ATLAS_LUT));
      if(atlasdset_lut==NULL) {
         WARNING_message("Could not allocate memory for Atlas LUT\n");
         RETURN(NULL);
      }

      atlasdset_lut->rgblist = calloc(MAXINT, 3);
/*      memcpy( atlasdset_lut, lut_atr->in , lut_atr->nin ) ;*/
      RETURN(atlasdset_lut);
     }
   RETURN(NULL);
}

/* free list of atlas points */
void
free_atlas_point_list(ATLAS_POINT_LIST *apl)
{
   if(apl==NULL)
      return;
   if(debug_niml) {
      INFO_message("Freeing atlas point list with %d points", apl->n_points);
      print_atlas_point_list(apl);
   }

   /* if there were any points, free the whole structure too */
   if(apl->n_points >= 1)
      free(apl->at_point);
   free(apl);
}

/* print list of atlas points*/
void
print_atlas_point_list(ATLAS_POINT_LIST *apl)
{
   int i;
   ATLAS_POINT *ap;
   INFO_message("----- Atlas point list: -------");
   
   if(apl==NULL)
      return;
   for(i=0;i<apl->n_points;i++) {
      ap = apl->at_point+i;
      INFO_message("%d: \"%s\", \"%s\""
                     " %5.1f %5.1f %5.1f\n",
         ap->tdval, ap->name, ap->dsetpref,
         ap->xx, ap->yy, ap->zz);
   }
   INFO_message("");
}

/* convert a NIML table from a dataset to an atlas list structure */
ATLAS_POINT_LIST *
dset_niml_to_atlas_list(THD_3dim_dataset *dset)
{
   ATLAS_POINT_LIST *apl;
   int i, tdlev, tdval;
   char *temp_str;
   ATLAS_POINT *at_pt;
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   byte LocalHead = 0;

   float cog[3];
   short okey;
   ATR_string *atr=NULL;

   ENTRY("dset_niml_to_atlas_list");

   if (LocalHead) fprintf(stderr, "assigning NIML attributes to apl.\n");

   atr = THD_find_string_atr( dset->dblk ,
                              "ATLAS_LABEL_TABLE" ) ;

   if (atr) {
      if (LocalHead) fprintf(stderr, "Label table found in attributes.\n");

      ngr = NI_read_element_fromstring(atr->ch);
      if ((ngr==NULL) || (ngr->part_num == 0)) {
         WARNING_message("** WARNING: Poorly formatted ATLAS_LABEL_TABLE\n");
         if(ngr) NI_free_element(ngr) ;
         RETURN(NULL);
      }
   }
   else {
      if (LocalHead) fprintf(stderr, "Label table NOT found in attributes.\n");
      RETURN(NULL);
   }

   /* get each segmented region - the "atlas point" from 
      a NIML formatted string */ 
   apl = (ATLAS_POINT_LIST *) malloc(sizeof(ATLAS_POINT_LIST));
   /* assume the number of elements in the group is the number of structures */
   apl->n_points = ngr->part_num; 
   apl->dset = dset; /* mark the dataset as the repository of segmentation */
   apl->at_point = (ATLAS_POINT *) malloc(apl->n_points*sizeof(ATLAS_POINT));
   if(apl->at_point == NULL) {         WARNING_message("** WARNING: Poorly formatted ATLAS_LABEL_DTABLE\n");
         free(apl);
         RETURN(NULL);
      }

   for(i=0;i<apl->n_points;i++){
       /* read the NIML string from the NIML file */
      nel = (NI_element *)ngr->part[i] ;
/*      nel = SUMA_FindNgrDataElement(ngr, "ATLAS_POINT", "atlas_point");*/
      if(!nel) {
          NI_free_element(ngr) ;
          RETURN(NULL);
      }
      NI_GET_INT(nel, "VAL", tdval);
      NI_GET_INT(nel, "OKEY", okey);
      NI_GET_INT(nel, "GYoAR", tdlev);
      NI_GET_FLOATv(nel, "COG", cog, 3, 0);

      temp_str = NI_get_attribute(nel, "STRUCT");
      /* update the pointer for the current atlas point */
      at_pt = &apl->at_point[i];
      /* copy "STRUCT" name - segmentation name */
      NI_strncpy(at_pt->name,temp_str,ATLAS_CMAX);
      /* sub-brick label for probability maps */
      temp_str = NI_get_attribute(nel, "DSETPREF");
      if(temp_str==NULL)
         NI_strncpy(at_pt->dsetpref,"",ATLAS_CMAX);
      else
         NI_strncpy(at_pt->dsetpref,temp_str,ATLAS_CMAX);

      at_pt->tdval = tdval;
      at_pt->okey = okey;
      at_pt->tdlev = tdlev;
      at_pt->xx = cog[0];
      at_pt->yy = cog[1];
      at_pt->zz = cog[2];
   }    

   NI_free_element(ngr) ;
   RETURN(apl); 
}



/* write atlas labels */

/* write atlas color scale */

/* read transformation for space 1 to space 2 */

/* write transformation for space 1 to space 2 */


#if 0

x list available spaces from whereami
x interface for returning path from one space to another
x include space names and transforms defined on brainmap.org
display network with SUMA using @DO.examples
x start building transform combinations
build toy working directory - user directory - supplemental monkey, child,
x single subject. Regression testing
affine transformation for EPI 
  revisit with optimized transformations from Peter Fox
x whereami can provide reference for xforms used
x option to take coordinate transformation - piecewise and combined
functions to apply transforms to coordinates or volumes (already called in
whereami)
#endif
/* report xform chain to go between src and dest spaces */
ATLAS_XFORM_LIST *report_xform_chain(char *src, char *dest)
{

   ATLAS_XFORM_LIST  *xfl;
   ATLAS_XFORM *xf;
   int srci, desti;
   
   /* atlas testing */
   srci = find_atlas_space_index(src);
   if(srci<0){
       INFO_message("Could not find source space %s in database", src);
   }    
   desti = find_atlas_space_index(dest);
   if(desti<0){
       INFO_message("Could not find destination space %s in database", dest);
   }
   /* check if we're going nowhere */
   if(srci==desti) {
      if(debug_niml)
         INFO_message("Chain is from and to same space");
      xf = identity_xform();      /* assign identity matrix */
      free(xf->source); free(xf->dest);
      xf->source = nifti_strdup(src);
      xf->dest = nifti_strdup(src);
      xfl = (ATLAS_XFORM_LIST *) calloc(1, sizeof(ATLAS_XFORM));
      xfl->xform = xf;
      xfl->nxforms = 1;
   }
   else {
     xfl =
      get_xform_chain(global_atlas_spaces->space+srci,global_atlas_spaces->space+desti);
   }   
   print_xform_list(xfl);
   return(xfl);
}

/* report which spaces are available to go between src and all other spaces */
void report_available_spaces(char *src)
{
   ATLAS_SPACE_LIST *spl;
   
   spl = find_available_spaces(src);
   print_space_list(spl);
   free_space_list(spl);    
}

/* read various NIML files for atlas information*/
void atlas_read_all()
{
   NI_stream space_niml;
   int valid_space_niml;
   char * ept = NULL;
   char suppfilestr[261];
 
   ept = my_getenv("AFNI_NIML_DEBUG");
   if( ept ) debug_niml = atoi(ept);       /* adjust if set */

   if(debug_niml) 
      INFO_message("\nInitializing structures\n"); 
   if(!init_space_structs(&global_atlas_xfl, &global_atlas_alist,
                          &global_atlas_spaces, &global_atlas_templates))
      ERROR_message("Could not initialize structures for template spaces");


/*
global_atlas_xfl = (ATLAS_XFORM_LIST *) malloc(sizeof(ATLAS_XFORM_LIST));
global_atlas_xfl->nxforms = 0;
global_atlas_alist = (ATLAS_LIST *) malloc(sizeof(ATLAS_LIST));
global_atlas_alist->natlases = 0;
global_atlas_spaces = (ATLAS_SPACE_LIST *) malloc(sizeof(ATLAS_SPACE_LIST));
global_atlas_spaces->nspaces = 0;
global_atlas_templates = (ATLAS_TEMPLATE_LIST *) malloc(sizeof(ATLAS_TEMPLATE_LIST));
global_atlas_templates->ntemplates = 0;
*/
   if(debug_niml) 
      INFO_message("opening AFNI_atlas_spaces.niml");   

   space_niml = NI_stream_open("file:AFNI_atlas_spaces.niml","r");

   if(space_niml==NULL){
         WARNING_message("Could not open global AFNI_atlas_spaces_niml\n");
   }

   /* read atlas info from global atlas file */
   valid_space_niml = read_space_niml(space_niml, global_atlas_xfl,
          global_atlas_alist, global_atlas_spaces, global_atlas_templates);

   ept = my_getenv("AFNI_SUPP_ATLAS");
   if( ept ) {
      sprintf(suppfilestr, "file:%s", ept);
      if(debug_niml) 
         INFO_message("opening AFNI_supp_atlas_space.niml");   
      space_niml = NI_stream_open(suppfilestr,"r");
      if(space_niml==NULL){
            fprintf(stderr, "\nCould not open supplemental atlas niml file\n");
            return;
      }
      /* read atlas info from supplemental atlas file */
      /*  adding to existing structures */
      valid_space_niml = read_space_niml(space_niml, global_atlas_xfl,
             global_atlas_alist, global_atlas_spaces, global_atlas_templates);

   }


   /* read atlas info from local atlas file */
   ept = my_getenv("AFNI_LOCAL_ATLAS");
   if( ept ) {
      sprintf(suppfilestr, "file:%s", ept);
      if(debug_niml) 
         INFO_message("opening AFNI_local_atlas_space.niml");   
      space_niml = NI_stream_open(suppfilestr,"r");
      if(space_niml==NULL){
            fprintf(stderr, "\nCould not open supplemental atlas niml file\n");
            return;
      }
      /* read atlas info from local atlas file */
      /*  adding to existing structures */
      valid_space_niml = read_space_niml(space_niml, global_atlas_xfl,
             global_atlas_alist, global_atlas_spaces, global_atlas_templates);
   }

  
   /* set up the neighborhood for spaces */
   /*  how are the spaces related to each other */ 
   if(make_space_neighborhood(global_atlas_spaces, global_atlas_xfl)!=0)
     return;
   
}

/* initialize space structures for dealing with templates and atlases */
int   init_space_structs(ATLAS_XFORM_LIST **atlas_xfl,
   ATLAS_LIST **atlas_alist,
   ATLAS_SPACE_LIST **atlas_spaces,
   ATLAS_TEMPLATE_LIST **atlas_templates)
{
   *atlas_alist = (ATLAS_LIST *) malloc(sizeof(ATLAS_LIST));
   *atlas_spaces = (ATLAS_SPACE_LIST *) malloc(sizeof(ATLAS_SPACE_LIST));
   *atlas_templates = (ATLAS_TEMPLATE_LIST *) malloc(sizeof(ATLAS_TEMPLATE_LIST));
   *atlas_xfl = (ATLAS_XFORM_LIST *) malloc(sizeof(ATLAS_XFORM_LIST));
   (*atlas_xfl)->nxforms = 0;
   (*atlas_xfl)->xform = NULL;
   (*atlas_alist)->natlases = 0;
   (*atlas_spaces)->nspaces = 0;
   (*atlas_templates)->ntemplates = 0;

   if(!(*atlas_alist) || !(*atlas_spaces) || !(*atlas_templates) || !(*atlas_xfl)) {
      return(0);
   }
   return(1);      
}


/* read niml file elements into C structures */
int   read_space_niml(NI_stream space_niml, ATLAS_XFORM_LIST *atlas_xfl,
   ATLAS_LIST *atlas_alist,
   ATLAS_SPACE_LIST *atlas_spaces,
   ATLAS_TEMPLATE_LIST *atlas_templates)
{
   NI_element *nel;
   int found = 0; 

   nel = (NI_element *) 1;
   while(nel) {
      if(debug_niml) 
         INFO_message("reading elements\n");
      nel = NI_read_element(space_niml, 100);
      if(nel) {
         if(debug_niml) 
           INFO_message("nel name %s\n", nel->name);
         if (nel->type == NI_ELEMENT_TYPE) {
            if(strcmp(nel->name, "TEMPLATE_SPACE") == 0) {
               atlas_spaces->nspaces++;
               if(debug_niml){
                  INFO_message("Template space\n");
                  INFO_message("number of spaces now %d\n", atlas_spaces->nspaces);
               }
               if(atlas_spaces->nspaces==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for spaces");
                  atlas_spaces->space = (ATLAS_SPACE *) malloc(sizeof(ATLAS_SPACE));
               }
               else               
                  atlas_spaces->space = (ATLAS_SPACE *) realloc(
                      atlas_spaces->space, atlas_spaces->nspaces * sizeof(ATLAS_SPACE));
               atlas_read_atlas_space(
                    nel, &atlas_spaces->space[atlas_spaces->nspaces-1]);

               found = 1;
            }
            if(strcmp(nel->name, "XFORM") == 0) {
               atlas_xfl->nxforms++;
               if(debug_niml){
                  INFO_message("space XFORM\n");
                  INFO_message("number of xforms now %d\n", atlas_xfl->nxforms);
               }
               if(atlas_xfl->nxforms==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for xforms");
                  atlas_xfl->xform = (ATLAS_XFORM *) malloc(sizeof(ATLAS_XFORM));
               }
               else               
                  atlas_xfl->xform = (ATLAS_XFORM *) realloc(
                      atlas_xfl->xform, atlas_xfl->nxforms * sizeof(ATLAS_XFORM));
               atlas_read_xform(nel, &atlas_xfl->xform[atlas_xfl->nxforms-1]);
               found = 1;
            } 
            if(strcmp(nel->name, "atlas_dataset") == 0) {
              atlas_alist->natlases++;
              if(debug_niml){
                  INFO_message("Atlas dataset\n");
                  INFO_message("number of atlases now %d\n", atlas_alist->natlases);
               }
               if(atlas_alist->natlases==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for atlases");
                  atlas_alist->atlas = (ATLAS *) malloc(sizeof(ATLAS));
               }
               else               
                  atlas_alist->atlas = (ATLAS *) realloc(
                      atlas_alist->atlas, atlas_alist->natlases * sizeof(ATLAS));
               atlas_read_atlas(nel, &atlas_alist->atlas[atlas_alist->natlases-1]);

               found = 1;
            }

            if(strcmp(nel->name, "template_dataset") == 0) {
               if(debug_niml) 
                  INFO_message("template dataset\n");
               atlas_templates->ntemplates++;
               if(debug_niml){
                  INFO_message("Atlas template\n");
                  INFO_message("number of templates now %d\n", 
                      atlas_templates->ntemplates);
               }
               if(atlas_templates->ntemplates==1){
                  if(debug_niml)
                     INFO_message("initial memory allocation for templates");
                  atlas_templates->atlas_template = 
                      (ATLAS_TEMPLATE *) malloc(sizeof(ATLAS_TEMPLATE));
               }
               else               
                  atlas_templates->atlas_template = (ATLAS_TEMPLATE *) realloc(
                      atlas_templates->atlas_template, 
                      atlas_templates->ntemplates * sizeof(ATLAS_TEMPLATE));
               atlas_read_template(nel,
                   &atlas_templates->atlas_template[atlas_templates->ntemplates-1]);
               found = 1;
            }
         }      
         NI_free_element(nel);  /* don't need the NIML element anymore */
      }
   }


   return(found);
}

/* compute what spaces are neighbors of the others  */
/* in preparation for Dijkstra search */
int make_space_neighborhood(ATLAS_SPACE_LIST *at_spl, ATLAS_XFORM_LIST *atlas_xfl)
{
      /* See Ziad's NIH-5 labbook pp 46 for graph */


      int i, j, nspaces, inv_xf, neighbor_i;
      ATLAS_SPACE *atlas_space, *dest_space;
      ATLAS_XFORM *xform;
      
      nspaces = at_spl->nspaces;
      FirstNeighb = (int **) malloc(nspaces * sizeof(int *));
      FirstNeighbDist = (float **) malloc(nspaces * sizeof(float *));
      N_Neighb = (int *) malloc(nspaces * sizeof(int));

      if(debug_niml)
         INFO_message("initial memory allocation for neighbors");
      if((FirstNeighb==NULL) || (FirstNeighbDist==NULL) || 
         (N_Neighb==NULL)) {
          WARNING_message("Could not allocate space for atlas neighborhood.");
          return(-1);
      }
      /* loop through all the spaces and see if they can be directly transformed
        (or inversely transformed) to any other space in the list of spaces */
      for(i=0;i<nspaces;i++){
         neighbor_i = 0;
         atlas_space = at_spl->space+i;
         for(j=0;j<nspaces;j++) {
            if(i==j) continue;  /* don't need to match the same space */
            dest_space = at_spl->space+j;
            xform = get_xform_neighbor(
                     atlas_xfl, atlas_space, dest_space, &inv_xf);
            if(xform!=NULL){
               if(neighbor_i==0){
                  FirstNeighb[i] = (int *) malloc(sizeof(int));
                  FirstNeighbDist[i] = (float *) malloc(sizeof(float));
               }
               else {
                  FirstNeighb[i] = (int *) realloc(FirstNeighb[i], 
                   (neighbor_i+1)*sizeof(int));
                  FirstNeighbDist[i] = (float *) realloc(FirstNeighbDist[i], 
                   (neighbor_i+1)*sizeof(float));
               }
               if((FirstNeighb[i]==NULL) || (FirstNeighbDist[i]==NULL)) {
                  WARNING_message("Could not allocate space for atlas neighborhood");
                  return(-1);
               }
               
               FirstNeighb[i][neighbor_i] = j;
               FirstNeighbDist[i][neighbor_i++] = xform->dist;
               if(debug_niml){
                  INFO_message("neighbor found for space %d with space %d",i,j);
                  INFO_message("xform %s with dist %f", xform->xform_name, xform->dist);
               }
            }
         }
         N_Neighb[i] = neighbor_i;
      }

    return(0);
}

/* search through list of transformations for direct or inverse transformation
   of source template space to destination template space */
ATLAS_XFORM *
get_xform_neighbor(ATLAS_XFORM_LIST *atlas_xfl, ATLAS_SPACE *at_space, 
   ATLAS_SPACE *dest_space, int *inv_xf)
{
    int i;
    char *srcstr, *deststr, *xfsrc, *xfdest;
    ATLAS_XFORM *xf;
       
    srcstr = at_space->atlas_space;
    deststr = dest_space->atlas_space;

    *inv_xf = 0;
    
    for(i=0;i<atlas_xfl->nxforms;i++) {
       xf = atlas_xfl->xform+i;
       xfsrc = xf->source;
       xfdest = xf->dest;
       if((strcmp(srcstr, xfsrc)==0) && (strcmp(deststr,xfdest)==0)) {
          return(xf);
       }
       /* check if inverse direction is available */
       if((strcmp(deststr, xfsrc)==0) && (strcmp(srcstr,xfdest)==0)) {
          *inv_xf = 1;
          return(xf);
       }
    }
    return(NULL);
}

/* find shortest path to go from one space to a destination space */
/* return the list of transformations needed to accomplish this */
ATLAS_XFORM_LIST *
get_xform_chain(ATLAS_SPACE *at_space, ATLAS_SPACE *dest_space)
{
   int srci, desti;
   int N_n, kk, *nPath;
   float nDistance;
   ATLAS_XFORM_LIST *xfl=NULL;

   /* find index for input spaces */
   srci = find_atlas_space(global_atlas_spaces, at_space);
   desti = find_atlas_space(global_atlas_spaces, dest_space);

   /* if src and dest are the same, should return identity right away */    
   if ( !(nPath = SUMA_Dijkstra_generic ( 
                          global_atlas_spaces->nspaces, 
                          NULL, -1, 0,
                          N_Neighb, FirstNeighb, FirstNeighbDist,
                          srci, desti, 
                          NULL, NULL, 
                          1, 
                          &nDistance, &N_n, 0)) ) {
          return(NULL); /* no path found */
        } 
   else {
      if(debug_niml){
         INFO_message("Number of spaces to traverse %d with distance %.2f ",
                     N_n, nDistance);
         fprintf(stderr, "spaces in chain by index: ");
         for(kk=0; kk<N_n; ++kk) 
            fprintf(stderr, "%d ", nPath[kk]); 
         fprintf(stderr, "\n");
      }

      xfl = pathlist_to_xform_list(nPath, N_n,
                  global_atlas_xfl, global_atlas_spaces);
      free(nPath); nPath = NULL;
   }

   return(xfl);
}

/* find space index that matches space name */
int
find_atlas_space(ATLAS_SPACE_LIST *at_spl, ATLAS_SPACE *at_space)
{
   int i;
   ATLAS_SPACE *sp1;
   
   for(i=0;i<at_spl->nspaces;i++) {
      sp1 = at_spl->space+i;
      if(strcmp(sp1->atlas_space, at_space->atlas_space)==0)
         return(i);
   }
   return(-1);   /* not found */ 
}

/* find space index in space list that matches string */
int
find_atlas_space_index(char *spacename)
{
   int i;
   ATLAS_SPACE *sp1;

   if((spacename==NULL) || (strcmp(spacename, "")==0))
      return(-1);
      
   for(i=0;i<global_atlas_spaces->nspaces;i++) {
      sp1 = global_atlas_spaces->space+i;
      if(strcmp(sp1->atlas_space, spacename)==0)
         return(i);
   }
   return(-1);   /* not found */ 
}

/* return the atlas_space structure corresponding to the atlas_space name
   in the dataset structure */
ATLAS_SPACE *
dset_space(THD_3dim_dataset *dset)
{
   int spacei;
   
   spacei = find_atlas_space_index(dset->atlas_space);
   if(spacei==-1)
      return(NULL);
   
   return(global_atlas_spaces->space+spacei);
}

/* find all available spaces that can be transformed to from some original space */
ATLAS_SPACE_LIST *
find_available_spaces(char *src_space_name)
{
  int i, curr_i, nspaces=0;
  ATLAS_SPACE_LIST *spl;
  ATLAS_XFORM_LIST *xfl;
  ATLAS_SPACE *xs, *spl_space, *src_space;
  
  spl = NULL;
  
  /* find index of current space string in static space list */
  curr_i = find_atlas_space_index(src_space_name);
  src_space = global_atlas_spaces->space+curr_i;
  
  /* search through all spaces */
  for(i=0;i<global_atlas_spaces->nspaces;i++){
     if(i==curr_i) continue;   /* don't count the current space */
     xs = global_atlas_spaces->space+i;   /* pointer to indexed space */
     xfl = get_xform_chain(src_space, xs);
     if(xfl){   /* found a transformation */
        if(debug_niml)
            INFO_message("Found an available space: %s", xs->atlas_space);
        free_xform_list(xfl);  /* don't actually need the xform list */
        
        if(spl==NULL) {
           spl = (ATLAS_SPACE_LIST *) malloc(sizeof(ATLAS_SPACE_LIST));
           spl->space = (ATLAS_SPACE *) malloc(sizeof(ATLAS_SPACE));
           nspaces = 1;
        }
        else {
           nspaces++;
           spl->space = (ATLAS_SPACE *) realloc(
               spl->space, nspaces * sizeof(ATLAS_SPACE));
        }       

        if((spl==NULL)||(spl->space==NULL)) {
            WARNING_message("Could not allocate available space transformation");
            return(NULL);
        }
        spl_space = spl->space+nspaces-1;
        spl_space->atlas_space = nifti_strdup(xs->atlas_space); 
        spl_space->generic_space = nifti_strdup(xs->generic_space);

        if((spl_space->atlas_space == NULL) || (spl_space->generic_space == NULL)) {
           WARNING_message("Could not allocate template space strings");
           return(NULL);
        }
        spl->nspaces = nspaces;


     }
  }
  if(spl)
      spl->nspaces = nspaces;
  else{
     if(debug_niml)
         INFO_message("no spaces reachable from source space: %s", src_space_name);   
  }
  if(debug_niml)
      INFO_message("There are %d spaces available", spl->nspaces);
  return(spl);
}

ATLAS_XFORM_LIST *
pathlist_to_xform_list(int *nPath, int N_n, ATLAS_XFORM_LIST *atlas_xfl, 
   ATLAS_SPACE_LIST *at_spl)
{
   int kk, inv_xf;
   ATLAS_XFORM_LIST *xflc = NULL;
   ATLAS_XFORM *a_xform, *xxform;
   ATLAS_SPACE *sp1, *sp2;

   xflc = (ATLAS_XFORM_LIST *) malloc(sizeof(ATLAS_XFORM_LIST));
   xflc->xform =  (ATLAS_XFORM *)  malloc((N_n-1)*sizeof(ATLAS_XFORM));
   xflc->nxforms = N_n-1;
         
   for(kk=0;kk<N_n-1;++kk) {
       sp1 = at_spl->space+nPath[kk]; /* starting space */
       sp2 = at_spl->space+nPath[kk+1];   /* next space */
       a_xform = get_xform_neighbor(atlas_xfl,sp1, sp2, &inv_xf); 
       if(debug_niml){
         INFO_message("space%d %s to space%d %s using xform %s",
            kk, sp1->atlas_space, kk+1, sp2->atlas_space, a_xform->xform_name);
       }

       xxform = xflc->xform+kk;       
       if(copy_xform(a_xform, xxform)!=0){
          WARNING_message("Could not create copy of xform for path");
          return(NULL);
       }

           
       if(inv_xf)   /* if inverting xform, take inverse of inverse field */
           xxform->inverse = !(a_xform->inverse);

       if(debug_niml){
          print_xform(xxform);
       }

   }

   if(debug_niml){
      INFO_message("Number of xforms in chain is %d", xflc->nxforms);
   }

   return(xflc);
}


/* copy elements of xform structure, allocating space for each element */
/* dest_xform already exists as a structure, it just needs to be populated */
int
copy_xform(ATLAS_XFORM *src_xform, ATLAS_XFORM *dest_xform)
{
   memset(dest_xform, 0, sizeof(ATLAS_XFORM));
   dest_xform->xform_type = nifti_strdup(src_xform->xform_type);
   dest_xform->xform_name = nifti_strdup(src_xform->xform_name);
   dest_xform->source = nifti_strdup(src_xform->source);
   dest_xform->dest = nifti_strdup(src_xform->dest);
   dest_xform->coord_order = nifti_strdup(src_xform->coord_order);
   
   if((dest_xform->xform_type==NULL) ||(dest_xform->xform_name==NULL) ||
      (dest_xform->source==NULL) || (dest_xform->dest==NULL) ||
      (dest_xform->coord_order==NULL))
      return(1);
   dest_xform->dist = src_xform->dist;
   dest_xform->inverse = src_xform->inverse;
   dest_xform->prepost = src_xform->prepost;
   dest_xform->nelts = src_xform->nelts;
   if(dest_xform->nelts==0) return(0);
   dest_xform->xform = malloc(dest_xform->nelts*sizeof(float));
   if(dest_xform->xform==NULL) return(1);
   memcpy(dest_xform->xform, src_xform->xform, dest_xform->nelts*sizeof(float));
   return(0);
}

/* copy elements of xform structure, allocating space for each element */
/* dest_xform already exists as a structure, it just needs to be populated */
ATLAS_XFORM *
identity_xform()
{
   ATLAS_XFORM *dest_xform;
   float *fptr;

   dest_xform = (ATLAS_XFORM *) calloc(1,sizeof(ATLAS_XFORM));   
   dest_xform->xform_type = nifti_strdup("Identity");
   dest_xform->xform_name = nifti_strdup("Identity");
   dest_xform->source = nifti_strdup("");
   dest_xform->dest = nifti_strdup("");
   dest_xform->coord_order = nifti_strdup("rai");
   
   if((dest_xform->xform_type==NULL) ||(dest_xform->xform_name==NULL) ||
      (dest_xform->source==NULL) || (dest_xform->dest==NULL) ||
      (dest_xform->coord_order==NULL))
      return(NULL);
   dest_xform->dist = 0.01;
   dest_xform->inverse = 0;
   dest_xform->prepost = 1;

   dest_xform->nelts = 1;
   if(dest_xform->nelts==0) return(dest_xform);
   dest_xform->xform = malloc(dest_xform->nelts*sizeof(float));
   if(dest_xform->xform==NULL) return(NULL);
   fptr = (float *) dest_xform->xform;
   *fptr  = 1.0;
   return(dest_xform);
}

/* free all the static lists */
void free_atlas_structs()
{
   free_xform_list(global_atlas_xfl);
   free_atlas_list(global_atlas_alist);
   free_space_list(global_atlas_spaces);
   free_template_list(global_atlas_templates);
}

/* free list of xforms */
void
free_xform_list(ATLAS_XFORM_LIST *xfl)
{
   int i;
   
   if(xfl==NULL)
      return;
   for(i=(xfl->nxforms)-1;i>=0;i--){
      free_xform(xfl->xform+i);
   }
   free(xfl->xform);
   free(xfl);
}

/* free an atlas_xform structure */
void
free_xform(ATLAS_XFORM *xf)
{   
   if(xf==NULL)
      return;
   free(xf->xform);
   free(xf->xform_type); free(xf->xform_name); free(xf->source); free(xf->dest);
   free(xf->coord_order);
}    

/* free list of spaces */
void
free_space_list(ATLAS_SPACE_LIST *xsl)
{
   int i;
   
   if(xsl==NULL)
      return;
   for(i=0;i<xsl->nspaces;i++)
      free_space(xsl->space+i);
   free(xsl->space);
   free(xsl);
}

/* free an atlas_space structure */
void
free_space(ATLAS_SPACE *xs)
{   
   if(xs==NULL)
      return;
   free(xs->atlas_space); free(xs->generic_space); 
}    

/* free list of spaces */
void
free_template_list(ATLAS_TEMPLATE_LIST *xtl)
{
   int i;
   
   if(xtl==NULL)
      return;
   for(i=0;i<xtl->ntemplates;i++)
      free_template(xtl->atlas_template+i);

   if(xtl->ntemplates >= 1)
      free(xtl->atlas_template); 
   free(xtl);
}

/* free an atlas_template structure */
void
free_template(ATLAS_TEMPLATE *xt)
{   
   if(xt==NULL)
      return;
   free(xt->atlas_space); free(xt->atlas_template); 
} 

/* free list of atlases */
void
free_atlas_list(ATLAS_LIST *xal)
{
   int i;
   
   if(xal==NULL)
      return;
   for(i=0;i<xal->natlases;i++)
      free_atlas(xal->atlas+i);
   if(xal->natlases >= 1)
      free(xal->atlas);
   free(xal);
}

/* free an atlas structure */
void
free_atlas(ATLAS *xa)
{   
   if(xa==NULL)
      return;
   free(xa->atlas_space); free(xa->atlas_dset_name); 
} 

/* print list of xforms - short form - as a chain */
void
print_xform_list(ATLAS_XFORM_LIST *xfl)
{
   int i;
   ATLAS_XFORM *xf;
   INFO_message("----- Transform list: -------");
   
   if(xfl==NULL)
      return;
   for(i=0;i<xfl->nxforms;i++) {
      xf = xfl->xform+i;
         fprintf(stderr,"%s ", xf->xform_name);
      if(xf->inverse)
         fprintf(stderr,"I");  
      if(i==(xfl->nxforms)-1)
         fprintf(stderr,"\n");
      else
         fprintf(stderr," -> ");
   }
   INFO_message("");
}

/* print list of xforms in long form */
void
print_all_xforms(ATLAS_XFORM_LIST *xfl)
{
   int i;
   ATLAS_XFORM *xf;
   INFO_message("----- Transform list: -------");
   
   if(xfl==NULL)
      return;
   for(i=0;i<xfl->nxforms;i++) {
      xf = xfl->xform+i;
      print_xform(xf);
      INFO_message("-------");
   }
  INFO_message("");
}

/* print the attributes of a transformation including its elements */
void
print_xform(ATLAS_XFORM *xf)
{

   int i;
   float *xfptr;
   
   fprintf(stderr, "xform: %s\n", xf->xform_name);
   fprintf(stderr, "xform_type: %s\n", xf->xform_type);
   fprintf(stderr, "xform source: %s   dest: %s\n", xf->source, xf->dest);
   fprintf(stderr, "coord order: %s\n", xf->coord_order);
   fprintf(stderr, "xform dist: %f  inverse: %d   prepost: %d   nelts: %d\n", 
           xf->dist, xf->inverse, xf->prepost, xf->nelts);
   xfptr = (float *) xf->xform;  /* for now use floating point values */
                               /* transformations may require different kinds of
                                       values in the future */
   if(strcmp(xf->xform_type,"Affine")==0)
      print_affine_xform_data(xfptr);
   else {
       for (i=0;i<xf->nelts;i++)
          fprintf(stderr, "%f ", *xfptr++);
       fprintf(stderr,"\n");
   }
}

/* print xform data for affine matrix in 4 columns */
void
print_affine_xform_data(float *xfptr)
{
   int i, j;

   for (i=0;i<3;i++){
      for (j=0;j<4;j++)
         fprintf(stderr, "%f ", *xfptr++);
      fprintf(stderr,"\n");
   } 
   fprintf(stderr,"\n");
}

/* print list of spaces */
void
print_space_list(ATLAS_SPACE_LIST *xsl)
{
   int i;
   ATLAS_SPACE *xs;
   
   if(xsl==NULL)
      return;
   if(debug_niml)
      INFO_message("Space list has %d spaces\n",xsl->nspaces);
   INFO_message("----- List of available spaces: -------");
   for(i=0;i<xsl->nspaces;i++) {
      xs = xsl->space+i;
      INFO_message("%s", xs->atlas_space);
   }
   INFO_message("");
}

/* print list of atlases */
void
print_atlas_list(ATLAS_LIST *xal)
{
   int i;
   ATLAS *xa;
   
   INFO_message("----- Atlas list: -------");
   if(xal==NULL){
      INFO_message("** No atlases found **");
      return;
   }   
   for(i=0;i<xal->natlases;i++) {
      xa = xal->atlas+i;
      INFO_message("%s", xa->atlas_dset_name);
   }
}

/* print list of templates */
void
print_template_list(ATLAS_TEMPLATE_LIST *xtl)
{
   int i;
   ATLAS_TEMPLATE *xt;
   INFO_message("----- Template list: -------");
   if(xtl==NULL)
      return;
   for(i=0;i<xtl->ntemplates;i++) {
      xt = xtl->atlas_template+i;
      INFO_message("%s", xt->atlas_template);
   }
   INFO_message("");
}

/* calculate list of xforms */
ATLAS_XFORM_LIST *
calc_xform_list(ATLAS_XFORM_LIST *xfl)
{
   int i, nxf, sl1, sl2, cc;
   ATLAS_XFORM *xf, *xf2, *xf3, *oldxfptr=NULL;
   char *source, *dest;
   ATLAS_XFORM_LIST *cxfl;

   if(debug_niml)
      printf("calculating xform list\n");   
   if(xfl==NULL)
      return(NULL);
   nxf = (xfl->nxforms) - 1;

#if 0
   /* assign labels for overall list source and destination spaces, 
      checking if either first or last xforms is inverted */
   if(xfl->xform->inverse)
      source = nifti_strdup(xfl->xform->dest);
   else
      source = nifti_strdup(xfl->xform->source);

   xf2 = xfl->xform+nxf;
   if(xf2->inverse)
      dest = nifti_strdup(xf2->source);
   else
      dest = nifti_strdup(xf2->dest);
#endif

   /* make condensed transformation list structure */
   cxfl = (ATLAS_XFORM_LIST *)calloc(1,sizeof(ATLAS_XFORM_LIST));
   if(cxfl==NULL)
      ERROR_exit("Could not allocate space for condensed xform list\n");
   cxfl->xform =  (ATLAS_XFORM *)  malloc((xfl->nxforms)*sizeof(ATLAS_XFORM));
   if(cxfl->xform==NULL)
      ERROR_exit("Could not allocate space for condensed xform list xforms\n");

   cxfl->nxforms = 0;
   if(debug_niml)
      printf("starting to combine xforms\n");
   /* only one xform, just go home */
   if(xfl->nxforms==1){
      if(debug_niml)
         printf("only 1 xform\n");
      cxfl->nxforms = 1;
      cc = copy_xform(xfl->xform, cxfl->xform);
      if(cc!=0) {
          ERROR_exit("Could not copy only xform for condensed xform list");
      }
      if(cxfl->xform->inverse == 1){   /* check for inverse, even if only one */
         xf = cxfl->xform;
         invert_xform(cxfl->xform);
         source = nifti_strdup(xf->dest);
         dest = nifti_strdup(xf->source);
         free(xf->xform_name);
         free(xf->source); free(xf->dest);
         xf->source = source;
         xf->dest = dest;
         sl1 = strlen(xf->source); sl2 = strlen(xf->dest);
         xf->xform_name = (char *) malloc((sl1+sl2+3)*sizeof(char));
         sprintf(xf->xform_name, "%s::%s", xf->source, xf->dest);
      }
      return(cxfl);
   }
   /* do calculations on xforms a pair at a time */
   xf = xfl->xform;
   for(i=0;i<nxf;i++) {
      if(debug_niml)
         printf("xf %d with xf %d\n", i, i+1);

      xf2 = xfl->xform+i+1;

      if(xf2->inverse)
         dest = nifti_strdup(xf2->source);
      else
         dest = nifti_strdup(xf2->dest);

      if(xf->inverse)
         source = nifti_strdup(xf->dest);
      else
         source = nifti_strdup(xf->source);

      if(debug_niml)
        INFO_message("Multiplying %s type with %s type in chain\n", xf->xform_type,
          xf2->xform_type);

      xf3 = calc_xf(xf,xf2);   /* calculate xforms a pair of time */
      if(xf3) {
         free(xf3->xform_name);
         free(xf3->source); free(xf3->dest);

         xf3->source = source;
         xf3->dest = dest;
         sl1 = strlen(xf3->source); sl2 = strlen(xf3->dest);
         xf3->xform_name = (char *) malloc((sl1+sl2+3)*sizeof(char));
         sprintf(xf3->xform_name, "%s::%s", xf3->source, xf3->dest);

         if(i==(nxf-1)){
            if(debug_niml)
               printf(
                 "On last xform, copying last combined xform"
                 " to combined xform list\n");
            cc = copy_xform(xf3, cxfl->xform+(cxfl->nxforms));
            cxfl->nxforms++;
            if(debug_niml){
               print_xform(xf3);
               xf = xf3;
               print_xform(xf);
              }
            }
         else{
            if(debug_niml)
               printf("could combine xform %d with %d\n", i, i+1);
            xf = xf3; cc = 0;
/*            cc = copy_xform(xf3, xf); */ /* use combined xform for next pair */
            if(debug_niml)
               print_xform(xf);

         }

      }
      else {
         if(debug_niml)
            printf("could not calculate this combination of xforms - adding to chain\n");
         cc = copy_xform(xf, cxfl->xform+(cxfl->nxforms));
         cxfl->nxforms++;
         if((cc==0)&&(i<nxf-1))
             xf = xf2; cc = 0;
             /*copy_xform(xf2, xf); */ /* update start xform for next pair */
      }

      if(i>0)   /* free the temporary xform intermediate */
          free_xform(oldxfptr);
      oldxfptr = xf3;

      if(cc!=0) {
          ERROR_exit("Could not copy a xform for condensed xform list");
      }

   }

#if 0
   /* copy source name from 1st xform */
   nxf = cxfl->nxforms;
   xf = cxfl->xform;
   free(xf->source);
   xf->source = source;
   /* copy dest name from last xform */
   xf = cxfl->xform+nxf;
   free(xf->dest); 
   /* make combination name for 1st xform list that condense down to one */
   if(nxf==1){
      free(xf->xform_name);
      xf->dest = dest;
      sl1 = strlen(source); sl2 = strlen(dest);
      xf->xform_name = (char *) malloc((sl1+sl2+3)*sizeof(char));
      sprintf(xf->xform_name, "%s::%s", xf->source, xf->dest);
   }
#endif
   
   return(cxfl);
}

/* calculate product for pair of xforms */
/* return xform product - allocating space even if copy 
 If xf and xf2 are both affine transformations, 
   return xf2 * xf */
ATLAS_XFORM *
calc_xf(ATLAS_XFORM *xf, ATLAS_XFORM *xf2)
{
   ATLAS_XFORM *xf3;
   int cc;

/*   if(debug_niml)
      INFO_message("Multiplying %s type with %s type\n", xf->xform_type,
      xf2->xform_type);
*/
   
   xf3 = malloc(sizeof(ATLAS_XFORM));
   if(xf3==NULL)
      return(NULL);
   invert_xform(xf);   /* possibly need to invert transform */
   invert_xform(xf2);


   /* check for identity transformations - simplest */
   if(strcmp(xf->xform_type,"Identity")==0){
       cc = copy_xform(xf2,xf3);
       if(cc!=0) {
           return(NULL);
       }
       else return(xf3);
   }

   if(strcmp(xf2->xform_type,"Identity")==0){
       cc = copy_xform(xf,xf3);
       if(cc!=0) {
           return(NULL);
       }
       else return(xf3);
   }


   if(debug_niml)
      INFO_message("Multiplying %s type with %s type\n", xf->xform_type,
      xf2->xform_type);
   if(strcmp(xf->xform_type,"Affine")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_mult(xf,xf2,xf3);
         if(debug_niml)
            INFO_message("combining two affine matrices\n");
         if(cc!=0) {
             if(debug_niml)
               INFO_message("could not combine two affine matrices\n");

             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"2-piece")==0){
         cc = affine_2piece_mult(xf,xf2,xf3,0);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"12-piece")==0){
         cc = affine_12piece_mult(xf,xf2,xf3,0);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }      
   }

   if(strcmp(xf->xform_type,"2-piece")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_2piece_mult(xf,xf2,xf3,-1);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"2-piece")==0){
         cc = x2piece_2piece_mult(xf,xf2,xf3);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"12-piece")==0){
         cc = x2piece_12piece_mult(xf,xf2,xf3,0);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
   }

   if(strcmp(xf->xform_type,"12-piece")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_12piece_mult(xf,xf2,xf3,-1);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"2-piece")==0){
         cc = x2piece_12piece_mult(xf,xf2,xf3,-1);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
      if(strcmp(xf2->xform_type,"12-piece")==0){
         cc = x12piece_12piece_mult(xf,xf2,xf3);
         if(cc!=0) {
             return(NULL);
         }
         else return(xf3);
      }
   }

   INFO_message("AFNI doesn't know how to combine these transforms");
   return(NULL);
}

/* invert transformation - general form */
int
invert_xform(ATLAS_XFORM *xf)
{
   int cc =1;

   if(xf->inverse==0) return(0);
   if(strcmp(xf->xform_type,"Affine")==0){
      cc = invert_affine(xf);
   }
   if(strcmp(xf->xform_type,"12-piece")==0){
      cc = invert_12piece(xf);
   }
   if(strcmp(xf->xform_type,"2-piece")==0){
      cc = invert_2piece(xf);
   }

   xf->inverse = 0; /* should never be an inverse transform here */

   return(cc);
}

/* invert an affine matrix - do in place */
/* return error code - 0 for no error */
int
invert_affine(ATLAS_XFORM *xf)
{
   int i, j;
   matrix tempmat, invmat;
   float *xfptr;
   ENTRY("invert_affine");

   matrix_initialize (&tempmat);
   matrix_create(4,4,&tempmat);
   xfptr = (float *) xf->xform;
   for(i=0;i<3;i++)
      for(j=0;j<4;j++)
         tempmat.elts[i][j] = (double) *xfptr++; /* recast float to double*/
   tempmat.elts[3][0] =  tempmat.elts[3][1] = tempmat.elts[3][2] = 0.0;
   tempmat.elts[3][3] = 1.0;
   matrix_initialize (&invmat);
   matrix_inverse(tempmat, &invmat);

   xfptr = (float *) xf->xform;
   for(i=0;i<3;i++)
      for(j=0;j<4;j++)
        *xfptr++ = (float) invmat.elts[i][j];

   matrix_destroy(&invmat);
   matrix_destroy(&tempmat);

   return(0);
}

/* invert a 12 piece matrix - do in place */
/* return error code - 0 for no error */
int
invert_12piece(ATLAS_XFORM *xf)
{
/*    int cc;
 */   
   return(1);
}


/* invert a 2 piece matrix - do in place */
/* return error code - 0 for no error */
int
invert_2piece(ATLAS_XFORM *xf)
{
/*    int cc;
 */   
   return(1);
}

/* multiply affine transformations */
int
affine_mult(ATLAS_XFORM *xf, ATLAS_XFORM *xf2, ATLAS_XFORM *xf3)
{
   int cc, i, j;
   matrix sm1, sm2, sm3;
   float *xfptr, *xfptr2;
   
   cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);

   matrix_initialize(&sm1);
   matrix_initialize(&sm2);
   matrix_initialize(&sm3);
   matrix_create(4,4,&sm1);
   matrix_create(4,4,&sm2);

   xfptr = (float *) xf->xform;
   xfptr2 = (float *) xf2->xform;
   for(i=0;i<3;i++)
     for(j=0;j<4;j++) {
        sm1.elts[i][j] =  (double) *xfptr++;
        sm2.elts[i][j] =  (double) *xfptr2++;
     }      
   sm1.elts[3][0] =  sm1.elts[3][1] = sm1.elts[3][2] = 0.0;
   sm1.elts[3][3] = 1.0;
   sm2.elts[3][0] =  sm2.elts[3][1] = sm2.elts[3][2] = 0.0;
   sm2.elts[3][3] = 1.0;

   matrix_multiply(sm1, sm2, &sm3);
   
   xfptr = (float *) xf3->xform;
   for(i=0;i<3;i++)
     for(j=0;j<4;j++) {
        *xfptr++ = (float) sm3.elts[i][j];
     }      

   matrix_destroy(&sm1);
   matrix_destroy(&sm2);
   matrix_destroy(&sm3);
   
   if(xf->xform_type) free(xf->xform_type);
   xf->xform_type = nifti_strdup("Affine");

   return(0);
}

/* multiply affine transformation by 2-piece transform */
int
affine_2piece_mult(ATLAS_XFORM *xf, ATLAS_XFORM *xf2, ATLAS_XFORM *xf3, int dir)
{
   int cc;
   
   return(1);  /* can't do this yet */
   if(dir)
      cc = copy_xform(xf2,xf3);
   else   
      cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}


/* multiply affine transformation by 12-piece transform */
int
affine_12piece_mult(ATLAS_XFORM *xf, 
  ATLAS_XFORM *xf2, ATLAS_XFORM *xf3, int dir)
{
   int cc;
   
   return(1);
   
   if(dir)
      cc = copy_xform(xf2,xf3);
   else   
      cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}

/* multiply two 2-piece affine transformations */
int
x2piece_2piece_mult(ATLAS_XFORM *xf, 
  ATLAS_XFORM *xf2, ATLAS_XFORM *xf3)
{
   int cc;
   
   return(1);
   cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}

/* multiply a 2-piece and a 12-piece affine transformation */
int
x2piece_12piece_mult(ATLAS_XFORM *xf, 
  ATLAS_XFORM *xf2, ATLAS_XFORM *xf3, int dir)
{
   int cc;
   
   return(1);
   if(dir)
      cc = copy_xform(xf2,xf3);
   else   
      cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}


/* multiply two 12-piece affine transformations */
int
x12piece_12piece_mult(ATLAS_XFORM *xf, 
  ATLAS_XFORM *xf2, ATLAS_XFORM *xf3)
{
   int cc;
   
   return(1);
   cc = copy_xform(xf,xf3);  /* allocate xform structure */
   if (cc!=0)
      return(1);
   return(0);
}


/* apply xform to xyz */
int
apply_xform_general(ATLAS_XFORM *xf, float x,float y,float z, \
                        float *xout, float *yout, float *zout)
{
   int xgc = 1;
   if(strcmp(xf->xform_type,"Affine")==0){
      xgc = apply_xform_affine(xf, x, y, z, xout, yout, zout);
   }
   if(strcmp(xf->xform_type,"2-piece")==0){
      xgc = apply_xform_2piece(xf, x, y, z, xout, yout, zout);
   }
   
   if(strcmp(xf->xform_type,"brett_tt2mni")==0){
      xgc = apply_xform_brett_tt2mni(x, y, z, xout, yout, zout);
   }

   if(strcmp(xf->xform_type,"brett_mni2tt")==0){
      xgc = apply_xform_brett_mni2tt(x, y, z, xout, yout, zout);
   }
   
   if(strcmp(xf->xform_type,"12-piece")==0){
      xgc = apply_xform_12piece(xf, x, y, z, xout, yout, zout);
   }
   if(strcmp(xf->xform_type,"Identity")==0){
      *xout = x; *yout = y; *zout = z; xgc = 0;
   }
   return(xgc);
}


/* apply xform chain to xyz */
int
apply_xform_chain(ATLAS_XFORM_LIST *xfl, float x, float y, float z,
                  float *xout, float *yout, float *zout)
{

   int i, nxf, xgc;
   float xxout, yyout, zzout;
   ATLAS_XFORM * xf;
        
   nxf = xfl->nxforms;
   
   if(nxf==0) return(0);
   
   for(i=0;i<nxf;i++) {
      xf = xfl->xform+i;
      xgc = apply_xform_general(xf, x, y, z, &xxout, &yyout,  &zzout);
      if(xgc==0) {
        x = xxout;
        y = yyout;
        z = zzout;
      }
      else {
         WARNING_message("Could not transform between spaces");
         return(-1);
      }  
   } 

   *xout = xxout;
   *yout = yyout;
   *zout = zzout;

   return(0);
}

/* apply the forward affine transformation  to the xyz */
int
apply_xform_affine(ATLAS_XFORM *xf, float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   float *xfptr;
   
   if(xf->xform==NULL) return(1);

   xfptr = (float *) xf->xform;
   
   xfptr = (float *) xf->xform;
   
   *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   
    return(0);
}

/* apply the forward 2-piece transformation  to the xyz coordinate
  2-piece transformations apply one of two affine transformations
  depending on some dividing plane defined by x,y or z equal to 
  a particular value. This dividing plane  can be defined on the data
  before the transformation or it may be defined 'post' transformation.
  If a post transformation, the second xform applies an alternate
  xform to the coordinate transformed by the first affine xform.
*/
int
apply_xform_2piece(ATLAS_XFORM *xf, float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   float *xfptr;
   float lx,ly,lz;
   int apply_post;
   
   /* brett transform - tta to mni
   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.05434  1.08554   0

   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.0595194  1.18892   0 
   */
   if(xf->xform==NULL) return(1);

   xfptr = xf->xform;
   /* change input coords to lpi if xform defined that way (RAI input)*/
   if(strcmp(xf->coord_order,"lpi") == 0){
      x = -x; y =-y;      
   }

   /* If this is a pre transformation, check the x,y,z limits. 
      If xyz > limit, use the second transformation */
   if(xf->prepost==0){
      lx = *xfptr++; ly = *xfptr++; lz = *xfptr++;
      if(lx > -9998) {
         if(x>lx)
            xfptr += 12;
      }
      if(ly > -9998) {
         if(y>ly)
            xfptr += 12;
      }
      if(lz > -9998) {
         if(z>lz)
            xfptr += 12;
      }
   }

   *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);   

   if(xf->prepost){
      apply_post = 0;
      lx = *xfptr++; ly = *xfptr++; lz = *xfptr++;
      if(lx > -9998) {
         if(x>lx)
            apply_post = 1;
       }
      if(ly > -9998) {
         if(y>ly)
            apply_post = 1;
      }
      if(lz > -9998) {
         if(z>lz)
            apply_post = 1;
      }
      if(apply_post) {
         x = *xout; y = *yout; z = *zout;
         xfptr += 4;
         *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
         xfptr += 4;
         *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
         xfptr += 4;
         *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);   
      }         
   }
   if(strcmp(xf->coord_order,"lpi") == 0){
      *xout = -(*xout); *yout= -(*yout);      
   }

   return(0);
}

/* apply the forward 12-piece transformation  to the xyz */
int
apply_xform_12piece(ATLAS_XFORM *xf, float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
/*    float *xfptr;
   static THD_talairach_12_warp ww;
 */

   return(1);   /* doesn't work yet */

   if(xf->xform==NULL) return(1);
/*   LOAD_FVEC3( mv , x,y,z ) ;   
   tv = AFNI_forward_warp_vector(MNI_N27_to_TLRC_DSET->warp, mv);
*/   return(0);
}

/* apply Brett transform to transform from TT to MNI (Talairach to MNI)*/
/* special case of 2-piece transform and maybe the only one we will ever use */
int
apply_xform_brett_tt2mni(float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   
   /* brett transform - tta to mni
   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.05434  1.08554   0

   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.0595194  1.18892   0 
   */

   THD_3tta_to_3mni(&x, &y, &z);      /* xform tt to mni space - results in lpi order */
   *xout = -x; *yout = -y; *zout = z; /* put coords back in RAI */

   return(0);
}



/* apply Brett transform to transform from MNI (MNI to Talairach)*/
/* special case of 2-piece transform and maybe the only one we will ever use */
int
apply_xform_brett_mni2tt(float x, float y, float z, \
                                    float *xout, float *yout, float *zout)
{
   x = -x; y = -y;                   /* put coords in lpi from RAI first */   
   THD_3mni_to_3tta(&x, &y, &z);      /* xform mni to tt space - results in RAI order */

   return(0);
}

#if 0
      int N_Neighb_Max = 5; /* max number of neighbors a node can have*/
      int N_Node = 7;
      int N_Neighb[7];
      int N_np=4;
      int np[4][2];

      int *nPath=NULL, N_n=0;
      float nDistance=0.0;
      /* fill number of neighbors for nodes 0 to 6 */
      ii=0;
      
      
      
      N_Neighb[ii++] = 4;      
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 3;
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 2;
      N_Neighb[ii++] = 0;
      N_Neighb[ii++] = 1;
      
      /* fill neighborhood (edges) and distances */
      FirstNeighb = (int **)calloc(N_Node, sizeof(int*));
      FirstNeighbDist = (float **)calloc(N_Node, sizeof(float*));
      for (ii=0; ii<7;++ii) {
         FirstNeighb[ii] = (int *)calloc(N_Neighb_Max, sizeof(int)); 
         FirstNeighbDist[ii] = (float *)calloc(N_Neighb_Max, sizeof(float)); 
      }
      FirstNeighb[0][0]=2;    FirstNeighbDist[0][0]=1.0; /*1st neighb of node 0*/
      FirstNeighb[0][1]=1;    FirstNeighbDist[0][1]=1.0; /*2nd neighb of node 0*/
      FirstNeighb[0][2]=4;    FirstNeighbDist[0][2]=5.0; /*3rd neighb of node 0*/
      FirstNeighb[0][3]=6;    FirstNeighbDist[0][3]=2.0; /* ... */

      FirstNeighb[1][0]=0;    FirstNeighbDist[1][0]=1.0; /*1st neighb of node 1*/
      FirstNeighb[1][1]=2;    FirstNeighbDist[1][1]=2.0; /*2nd neighb of node 1*/
      
      FirstNeighb[2][0]=1;    FirstNeighbDist[2][0]=2.0; 
      FirstNeighb[2][1]=3;    FirstNeighbDist[2][1]=1.0;
      FirstNeighb[2][2]=0;    FirstNeighbDist[2][2]=1.0;
   
      FirstNeighb[3][0]=4;    FirstNeighbDist[3][0]=2.0;
      FirstNeighb[3][1]=2;    FirstNeighbDist[3][1]=1.0;
   
      FirstNeighb[4][0]=3;    FirstNeighbDist[4][0]=2.0;
      FirstNeighb[4][1]=0;    FirstNeighbDist[4][1]=5.0;
      
      FirstNeighb[5][0]=-1;   /* not necessary, but to emphasize */
      
      FirstNeighb[6][0]=0;    FirstNeighbDist[6][0]=2.0;
      
      if (!(fpout = fopen(prefix,"w"))) {
         fprintf(stderr,"** Error: Failed to open %s for writing.\n", prefix);
         exit(1);
      }
      fprintf(fpout, 
               "#Paths between nodes\n"
               "#Col. 0: Total number of nodes in path\n"
               "#    0 for no path\n"
               "#Col. 1: Distance\n"
               "#    -1 for no path\n"
               "#Col. 2: First node\n"
               "#Col. last: Last node\n");
      
      /* Now get the shortest distance between some nodes pairs*/
      ii=0;
      np[ii][0] = 0; np[ii][1] = 4; ++ii;  /* from node 0 to node 4 */
      np[ii][0] = 6; np[ii][1] = 5; ++ii;  /* from node 6 to node 5 */
      np[ii][0] = 1; np[ii][1] = 2; ++ii;  /* from node 1 to node 2 */
      np[ii][0] = 3; np[ii][1] = 3; ++ii;  /* from node 1 to node 2 */
      /* work  the node pairs */
      fprintf(fout, "#Internodal distance along graph \n");
      fprintf(fout, "#%-6s %-6s %-6s\n",
                    "From" , "to", "Dist." );
      for (ii=0; ii < N_np; ++ii) {
         if ( !(nPath = SUMA_Dijkstra_generic ( 
                           7, 
                           NULL, -1, 0,
                           N_Neighb, FirstNeighb, FirstNeighbDist,
                           np[ii][0], np[ii][1], 
                           NULL, NULL, 
                           1, 
                           &nDistance, &N_n, 0)) ) {
            nDistance = -1.0;
            if (fpout) fprintf(fpout, "0 -1.0 %d %d\n", np[ii][0], np[ii][1]);
         } else {
            if (fpout) {
               fprintf(fpout, "%d %.2f ", N_n, nDistance);
               for(kk=0; kk<N_n; ++kk) 
                  fprintf(fpout, 
                           "%d ", 
                           nPath[kk]); 
               fprintf(fpout, "\n");
            }
            free(nPath); nPath = NULL;
         }
      
         fprintf(fout, " %-6d %-6d %-4.2f\n", 
                       np[ii][0], np[ii][1], nDistance);
      }
      
      fprintf(stderr,"See file %s for full output\n", prefix);
      
      if (fpout) fclose(fpout); fpout=NULL;
      
    return(0);
}

#endif

/* read xform from NIML attributes into xform_struct */
int atlas_read_xform(NI_element *nel, ATLAS_XFORM *atlas_xf)
{
   float dist;
   int i;
   char *sptr;

   if(debug_niml) {
      INFO_message("xform_name %s", NI_get_attribute(nel, "xform_name"));
      INFO_message("xform_type %s", NI_get_attribute(nel, "xform_type"));
      INFO_message("xform source %s", NI_get_attribute(nel, "source"));
      INFO_message("xform dest   %s", NI_get_attribute(nel, "dest"));
      INFO_message("xform number of elements %d", nel->vec_num);
      INFO_message("xform prepost %s", NI_get_attribute(nel, "prepost")); 
      INFO_message("xform coord_order %s", NI_get_attribute(nel, "coord_order")); 
   }   
   atlas_xf->xform_type = nifti_strdup(NI_get_attribute(nel, "xform_type")); 
   atlas_xf->xform_name = nifti_strdup(NI_get_attribute(nel, "xform_name")); 
   atlas_xf->source = nifti_strdup(NI_get_attribute(nel, "source")); 
   atlas_xf->dest = nifti_strdup(NI_get_attribute(nel, "dest")); 
   if(NI_get_attribute(nel, "distance")){
      dist = atof( NI_get_attribute(nel, "distance"));
      if(dist<=0) {
         WARNING_message("Distance less than or equal to 0 reset to 1");
         dist = 1;
      }
   }
   else
      dist = 1;
   atlas_xf->dist = dist;

   sptr = NI_get_attribute(nel, "prepost");
   if(sptr){
       atlas_xf->prepost = atoi(sptr);
   }
   else
       atlas_xf->prepost = 0;   /*assume pre-xform (used for 2 and 12 part xforms */

   sptr = NI_get_attribute(nel, "coord_order");    
   if(sptr){
      atlas_xf->coord_order = nifti_strdup(sptr);
   }
   else
      atlas_xf->coord_order = nifti_strdup("rai");
   
   if((atlas_xf->xform_type == NULL) || (atlas_xf->source == NULL) ||
     (atlas_xf->dest == NULL) || (atlas_xf->xform_name==NULL) ||
     (atlas_xf->coord_order == NULL)) {
      WARNING_message("Could not allocate transformation type string");
      return(1);
   }
   
   atlas_xf->nelts = nel->vec_num;
   atlas_xf->inverse = 0;
   
   atlas_xf->xform = malloc(nel->vec_num * sizeof(float));
   if(atlas_xf->xform == NULL) {
      WARNING_message("Could not allocate transformation");
      return(1);
   }

  for(i=0;i<nel->vec_num;i++){
     memcpy((char *)(atlas_xf->xform)+i*sizeof(float), nel->vec[i], sizeof(float));
  }
  if(debug_niml)
      print_xform(atlas_xf);
   return(0);
}

/* read template info from NIML attributes into template structure */
int atlas_read_template(NI_element *nel, ATLAS_TEMPLATE *atlas_tpl)
{
   if(debug_niml) {
      INFO_message("atlas_template %s", NI_get_attribute(nel, "template_name"));
      INFO_message("templ_space %s", NI_get_attribute(nel, "atlas_space"));
   }

   atlas_tpl->atlas_template = nifti_strdup(NI_get_attribute(nel, "template_name")); 
   atlas_tpl->atlas_space = nifti_strdup(NI_get_attribute(nel, "atlas_space"));

   if((atlas_tpl->atlas_template == NULL) || (atlas_tpl->atlas_space == NULL)) {
      WARNING_message("Could not allocate template strings");
      return(1);
   }

   return(0);
}

/* read atlas info from NIML attributes into atlas structure */
int atlas_read_atlas(NI_element *nel, ATLAS *atlas)
{
   if(debug_niml) {
      INFO_message("atlas_name %s", NI_get_attribute(nel, "atlas_name"));
      INFO_message("atlas_space %s", NI_get_attribute(nel, "atlas_space"));
   }

   atlas->atlas_dset_name = nifti_strdup(NI_get_attribute(nel, "atlas_name")); 
   atlas->atlas_space = nifti_strdup(NI_get_attribute(nel, "atlas_space"));

   if((atlas->atlas_dset_name == NULL) || (atlas->atlas_space == NULL)) {
      WARNING_message("Could not allocate atlas strings");
      return(1);
   }

   return(0);
}

/* read template space info from NIML attributes into template space structure */
int atlas_read_atlas_space(NI_element *nel, ATLAS_SPACE *at_space)
{
   if(debug_niml) {
      INFO_message("space_name %s", NI_get_attribute(nel, "space_name"));
      INFO_message("generic_space %s", NI_get_attribute(nel, "generic_space"));
   }

   at_space->atlas_space = nifti_strdup(NI_get_attribute(nel, "space_name")); 
   at_space->generic_space = nifti_strdup(NI_get_attribute(nel, "generic_space"));

   if((at_space->atlas_space == NULL) || (at_space->generic_space == NULL)) {
      WARNING_message("Could not allocate template space strings");
      return(1);
   }

   return(0);
}



NI_element *NI_find_next_element(NI_stream ns, char *name)
{
   NI_element *nel;
 
   nel = (NI_element *) 1;
   while(nel) {
fprintf(stderr,"reading elements\n");   
      nel = NI_read_element(ns, 100);
      if(nel) {
         fprintf(stderr,"nel name %s\n", nel->name);
         if (nel->type == NI_ELEMENT_TYPE) {
            if(strcmp(name, nel->name) == 0) {
               fprintf(stderr, "name matches \n");
               return(nel);
            }
         }      
      }
   }
   return(NULL);
}

/* convert some of the atlas lists that are hard-coded in AFNI to NIML tables */
void
AFNI_atlas_list_to_niml()
{
   ATLAS_POINT_LIST *temp_apl;

/*    if(debug_niml)
      INFO_message("Converting TTO_list to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(TTO_list, TTO_COUNT);
   adjust_atlas_point_list(temp_apl, "Left", 200);

   if(debug_niml){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing TTO_list");
   }
   atlas_list_to_niml(temp_apl, "TT_atlas.niml", TTO_COUNT);
   if(debug_niml)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);
 */
   if(debug_niml)
      INFO_message("Converting CA_EZ_list to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(CA_EZ_list, CA_EZ_COUNT);

   if(debug_niml){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing CA_EZ_list");
   }
   atlas_list_to_niml(temp_apl, "CA_EZ_atlas.niml", CA_EZ_COUNT);
   if(debug_niml)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);
/* 
   if(debug_niml)
      INFO_message("Converting ML_EZ_list to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(ML_EZ_list, ML_EZ_COUNT);

   if(debug_niml){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing ML_EZ_list");
   }
   atlas_list_to_niml(temp_apl, "ML_EZ_atlas.niml", ML_EZ_COUNT);
   if(debug_niml)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);

   if(debug_niml)
      INFO_message("Converting LR_EZ_list to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(LR_EZ_list, LR_EZ_COUNT);

   if(debug_niml){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing LR_EZ_list");
   }
   atlas_list_to_niml(temp_apl, "LR_EZ_atlas.niml", LR_EZ_COUNT);
   if(debug_niml)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);

 */
}

#define TRIM_STRING(lbuf, ch){\
   int kk; \
   for( kk=strlen(lbuf)-1 ; kk > 0 && lbuf[kk] == ch ; kk-- )    \
      lbuf[kk] = '\0' ;                  /* trim trailing .'s */ \
}
static ATLAS_POINT_LIST *
AFNI_atlas_list_to_atlas_point_list(ATLAS_POINT *afni_at_pts, int npts)
{
  ATLAS_POINT *temp_atp;
  ATLAS_POINT_LIST *apl;
  int i;

  ENTRY("AFNI_atlas_list_to_atlas_point_list");
  apl = (ATLAS_POINT_LIST *) malloc(sizeof(ATLAS_POINT_LIST));
  apl->n_points = npts;
  apl->dset = NULL; /* no dataset associated with the atlas point list yet */
  apl->at_point = (ATLAS_POINT *) malloc(npts*sizeof(ATLAS_POINT));
  for(i=0;i<npts;i++){
     /* copy AFNI point value to new atlas point list structure */
     temp_atp = apl->at_point+i;
     temp_atp->tdval = afni_at_pts[i].tdval;
     temp_atp->tdlev = afni_at_pts[i].tdlev;
     temp_atp->okey = afni_at_pts[i].okey;
     temp_atp->xx = afni_at_pts[i].xx;
     temp_atp->yy = afni_at_pts[i].yy;
     temp_atp->zz = afni_at_pts[i].zz;
     NI_strncpy(temp_atp->name,afni_at_pts[i].name,ATLAS_CMAX);
     TRIM_STRING(temp_atp->name, '.');
     NI_strncpy(temp_atp->dsetpref,afni_at_pts[i].dsetpref,ATLAS_CMAX);
     TRIM_STRING(temp_atp->dsetpref, '.');
     if(debug_niml){
        INFO_message("atlas_point %d %s\n", afni_at_pts[i].tdval, 
                      afni_at_pts[i].name);
        INFO_message("atlas_point %d %s temp\n", temp_atp->tdval, 
                      temp_atp->name);
     }
  }
  RETURN(apl);
}

/* convert atlas list to a NIML table in a text file */
void
atlas_list_to_niml(ATLAS_POINT_LIST *atp, char *atlas_file, int n_regions)
{
   int i;
/*   char *atlas_pt_niml;*/
   char filestr[ATLAS_CMAX];

   ATLAS_POINT *at_pt;
   NI_stream atlas_niml;
   NI_group *ngr;
   NI_element *nel=NULL;

   ENTRY("atlas_list_to_niml");

   if(debug_niml) 
      INFO_message("opening %s", atlas_file);   
   sprintf(filestr, "file:%s", atlas_file);
   atlas_niml = NI_stream_open(filestr,"w");

   if(atlas_niml==NULL){
         WARNING_message("Could not open atlas file for NIML output %s");
         EXRETURN;
   }
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "atlas_point_list");
   /* get each segmented region - the "atlas point" into 
      a NIML formatted string */ 
   for(i=0;i<n_regions;i++) {
      at_pt = atp->at_point+i;
      nel = atlas_point_to_niml_element(at_pt);
      NI_add_to_group( ngr, nel); 
/*      atlas_pt_niml = atlas_point_to_niml_string(at_pt);*/
      /* write the NIML string to the NIML file */
/*      if(write_niml_string(atlas_pt_niml, atlas_niml)) {
         WARNING_message("Could not write atlas point to NIML file");
         NI_stream_close(atlas_niml);
         EXRETURN;
      }
*/
   }

   /* write the whole group out at once to a NIML file */
   if(NI_write_element( atlas_niml, ngr , NI_TEXT_MODE&NI_HEADERSHARP_FLAG)<0) {
         WARNING_message("Could not write atlas point list to NIML file");
   }  

   NI_free_element(ngr) ;
   NI_stream_close(atlas_niml);
   EXRETURN; 
}

/* adjust the point values in an atlas list by a fixed amount if the name */
/* of the structure starts with the specified match string - case insensitive*/
static void
adjust_atlas_point_list(ATLAS_POINT_LIST *atp, char *match_str, float addval)
{
   /* this function was written for adjusting the TT_daemon values to be unique
      previously left and right regions shared the same value */
   int i;
   ATLAS_POINT *at_pt;

   ENTRY("adjust_atlas_point_list");

   for(i=0;i<atp->n_points;i++) {
      at_pt = atp->at_point+i;
      if((strcasestr(at_pt->name, match_str))){
         at_pt->tdval = at_pt->tdval + addval;
      }
   }

   EXRETURN; 
}



#if 0
/* write a niml encoded string to a specified stream */
static int
write_niml_string(char *nimlstring, NI_stream ns)
{
   NI_element *nel=NULL;
   ENTRY("write_niml_string");
   nel = (NI_element *)NI_read_element_fromstring(nimlstring);
   /* write the niml element into a string - note this is written in text mode
         (ascii encoded) */
   if(NI_write_element( ns, nel , NI_TEXT_MODE&NI_HEADERSHARP_FLAG)<0)
     RETURN(-1);
   RETURN(0);
}
#endif

#if 0
just for reference here
typedef struct {
   /* tdval and tdlev stand for "Talairach Daemon" value and level */
   /* these are kept for historical purposes  */
   /* perhaps one day making an unusally boring PBS special */
   short tdval;         /* Leave this one to be the very first element */
   char name[ATLAS_CMAX] ;  /* Leave this one to be the second element */  
   short xx,yy,zz,tdlev,okey ; /* xx,yy,zz - RAI position of region */
                               /* tdlev = unknown, gyrus or area code */
                               /* okey = original value in atlas */
                               /*  this value was converted for TT daemon */
                               /*  atlas values because left and right */
                               /*  ROIs shared the same value */                               
   char dsetpref[ATLAS_CMAX];
} ATLAS_POINT ;
#endif

/* return niml element from atlas point structure */
NI_element *
atlas_point_to_niml_element(ATLAS_POINT *at_pt)
{
   float cog[3];
   NI_element *nel=NULL;
   short okey, tdval;

   ENTRY("atlas_point_to_niml_element");

   /* create niml element that corresponds to atlas point structure */
   nel = NI_new_data_element("ATLAS_POINT",0);
   NI_set_attribute( nel,"data_type", "atlas_point");
   NI_set_attribute(nel, "STRUCT", at_pt->name);

   /* original key value because I will be modifying the original values - 
      need to put this into TTO_list */
   /* TT atlas includes the same values for left and right structures,
      but I'll be adding 200 to these */
   tdval = at_pt->tdval;
   NI_SET_INT(nel, "VAL", tdval);
   okey = at_pt->okey;
   if(okey == -999) okey = tdval;  /* get original "key" value from tdval */
   NI_SET_INT(nel, "OKEY", okey);

   /* gyrus or area code */
   NI_SET_INT(nel, "GYoAR", at_pt->tdlev);

   /* center of mass/gravity coordinates */
   cog[0] = at_pt->xx; cog[1] =  at_pt->yy; cog[2] = at_pt->zz;
   NI_SET_FLOATv(nel, "COG", cog, 3);

   /* associated dataset file that gives the original name of the Analyze file
      suggesting the name of the sub-brick in the atlas where
      it is finally stored */
   if (strcmp(at_pt->dsetpref,"")!=0)
     NI_set_attribute(nel, "DSETPREF", at_pt->dsetpref);
   RETURN(nel);
}

/* return ascii encoded niml string from atlas point structure */
char *
atlas_point_to_niml_string(ATLAS_POINT *at_pt)
{
   NI_element *nel=NULL;
   NI_stream ns;
   char *encstr;
   nel = atlas_point_to_niml_element(at_pt);
    /* create a niml stream to write into */
    ns = NI_stream_open("str:", "w");

    if(ns==NULL)
       RETURN(NULL);
    /* write the niml element into a string - 
       note this is written in text mode (ascii encoded) */
    NI_write_element( ns , nel , NI_TEXT_MODE&NI_HEADERSHARP_FLAG) ;

    /* copy the string to a more permanent string and
       then can close stream and string */
    encstr = SUMA_copy_string( NI_stream_getbuf(ns));

    /* close stream - freeing elements */
    NI_stream_close( ns );

    RETURN(encstr);
}

/* reading functions */

/* convert a NIML table from a file to an atlas list structure */
static void
niml_to_atlas_list(ATLAS_POINT_LIST *atp, char *atlas_file)
{
   int more_niml, count, tdlev;
   char filestr[ATLAS_CMAX];
   char *temp_str;
   ATLAS_POINT *at_pt;
   NI_stream atlas_niml;
   NI_element *nel=NULL;

   float cog[3];
   char *encstr;
   short okey;

   ENTRY("niml_to_atlas_list");

   if(debug_niml) 
      INFO_message("opening atlas niml file %s",atlas_file);   
   sprintf(filestr, "file:%s", atlas_file);
   atlas_niml = NI_stream_open(filestr,"r");

   if(atlas_niml==NULL){
         WARNING_message("Could not open atlas file for NIML output %s");
         EXRETURN;
   }

   more_niml = 1;
   count = 0;
   /* get each segmented region - the "atlas point" from 
      a NIML formatted string */ 
   while(more_niml) {
      /* read the NIML string from the NIML file */
     nel = (NI_element *)NI_read_element_fromstring(encstr);
     if(!nel) {
         NI_stream_close(atlas_niml);
         EXRETURN;
     }
     NI_GET_INT(nel, "OKEY", okey);
     NI_GET_INT(nel, "GYoAR", tdlev);
     NI_GET_FLOATv(nel, "COG", cog, 3, 0);

     temp_str = NI_get_attribute(nel, "STRUCT");
     at_pt = &atp->at_point[count];
     NI_strncpy(at_pt->name,temp_str,ATLAS_CMAX);

     temp_str = NI_get_attribute(nel, "DSETPREF");
     if(temp_str==NULL)
        NI_strncpy(at_pt->dsetpref,"",ATLAS_CMAX);
     else
        NI_strncpy(at_pt->dsetpref,temp_str,ATLAS_CMAX);

     at_pt->tdval = okey;
     at_pt->xx = cog[0];
     at_pt->yy = cog[1];
     at_pt->zz = cog[2];
     if(debug_niml)
        fprintf(stderr,"Decoded:\n"
                    "STRUCT: %s\n"
                    "OKEY: %d\n"
                    "COG: %.3f %.3f %.3f\n"
                    "DSETPREF: %s\n"
                    , NI_get_attribute(nel, "STRUCT"),
                    okey, cog[0], cog[1], cog[2],
                    NI_get_attribute(nel, "DSETPREF"));
     count++;
   }    
   NI_stream_close(atlas_niml);
   EXRETURN; 
}

#if 0
static int
read_niml_string()    /* Now pretend we got encstr from our label table, by using the key
{
     okey, or okey+256 */
     EXRETURN;
}
#endif

