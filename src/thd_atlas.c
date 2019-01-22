/* thd_atlas.c */
/* functions to access atlas, template, space of datasets */

/* access current space of dataset */

#include "afni.h"
#define SUMA_noFunc
#include "suma_suma.h"

extern int * SUMA_Dijkstra_generic (int N_Node, 
                     float *NodeList, int NodeDim, int dist_metric,
                     int *N_Neighbv, int **FirstNeighb, float **FirstNeighbDist,
                     int Nx, int Ny, 
                     byte *isNodeInMeshp, 
                     int *N_isNodeInMesh, int Method_Number, 
                     float *Lfinal, int *N_Path,
                     int verb);
static void niml_to_atlas_list(ATLAS_POINT_LIST *atp, char *atlas_file);
static void adjust_atlas_point_list(ATLAS_POINT_LIST *atp, char *match_str,
            float addval);
static ATLAS_POINT_LIST *AFNI_atlas_list_to_atlas_point_list(
        ATLAS_POINT *afni_at_pts, int npts);
static int **FirstNeighb=NULL;
static float **FirstNeighbDist=NULL; 
static int *N_Neighb = NULL;
static char *jumpspace_name = NULL;

char * THD_get_space(THD_3dim_dataset *dset)
{

   ENTRY("THD_get_space");

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
          MCW_strncpy(dset->atlas_space, 
                      TT_whereami_default_spc_name(), THD_MAX_NAME);
         break;
   }
#ifdef DEBUG_SPACES
   fprintf(stderr,"Space is %s\n",dset->atlas_space);
#endif

   RETURN(dset->atlas_space);
}

/* return the generic space associated with the space of a dataset
   the principal goal is to give generic TLRC for all flavors of TLRC, TT_N27
   or generic MNI for all flavors of MNI, MNI_FSL, MNI_ANAT */
char * THD_get_generic_space(THD_3dim_dataset *dset)
{
   char *spcstr=NULL, *genspcstr=NULL;

   ENTRY("THD_get_generic_space");

   if(!dset) RETURN(NULL);
   spcstr = THD_get_space(dset); /* space from dataset structure - do not free */
   if(spcstr) 
       genspcstr = gen_space_str(spcstr); /* space string from space structure - also do not free */
   if(genspcstr)
      RETURN(genspcstr);
   else
      RETURN(spcstr);
}

/* return the VIEW (ORIG,ACPC or TLRC) for a dataset */
/* This is intended as the AFNI output view for a given dataset,
   i.e.what dataset view should be associated with an AFNI output file.
   Given an AFNI format input dataset, this will usually be the same
   as the view of the input (dset->dblk->diskptr->viewcode), but is not
   guaranteed to be the same. The function solves the problem of 
   NIFTI or other format input and naming the AFNI format output.

   For NIFTI format, the sform or qform codes determine whether the
   data is in TLRC, MNI or some other aligned space. If it is any aligned
   space, the dataset will be assigned a TLRC view and a space if one
   is already defined in an AFNI extension. Otherwise, the TLRC space
   is assigned to the dataset */
char * THD_get_view_space(THD_3dim_dataset *dset)
{
   char *spcstr=NULL, *space=NULL;

   ENTRY("THD_get_view_space");

   if(!dset) RETURN(NULL);
   spcstr = dset->dblk->diskptr->viewcode;
   if(spcstr != NULL)
      RETURN(spcstr);

   spcstr = THD_get_generic_space(dset); /* space from dataset structure - do not free */

   if (strcmp(spcstr, "ORIG")==0)
      RETURN("ORIG");
   if (strcmp(spcstr, "ACPC")==0)
      RETURN("ACPC");
   /* all other spaces are assumed to be TLRC */
   RETURN("TLRC");
}

/* assign space codes used by whereami for specific atlases */
int
THD_space_code(char *space)
{
   ENTRY("THD_space_code");
   
   if (wami_verb()) {
      WARNING_message("Better not use codes anymore");
   }
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
      atlasdset_lut = (ATLAS_LUT *)calloc(1,sizeof(ATLAS_LUT));
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
   if(wami_verb() > 1) {
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
         ap->tdval, ap->name, ap->sblabel,
         ap->xx, ap->yy, ap->zz);
   }
   INFO_message("");
}

/* return atlas point from  list of atlas points with name of label*/
ATLAS_POINT *
atlas_point_named(ATLAS_POINT_LIST *apl, char *name)
{
   int i;
   ATLAS_POINT *ap;
   
   if(apl==NULL)
      return(NULL);
   for(i=0;i<apl->n_points;i++) {
       ap = apl->at_point+i;
       if(strcmp(ap->name, name)==0)
            return(ap);
   }
   return(NULL);
}

/* return long name for atlas point with name provided */
char *
atlas_point_long_name_named(ATLAS_POINT_LIST *apl, char *name)
{
    ATLAS_POINT *ap;

    ap = atlas_point_named(apl, name);
    if(ap) return(ap->longname);
    return(NULL);
}


/* convert a NIML table from a dataset to an atlas list structure */
ATLAS_POINT_LIST * dset_niml_to_atlas_list(THD_3dim_dataset *dset)
{
   ATLAS_POINT_LIST *apl=NULL;
   int LocalHead = wami_lh();
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
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
      apl = niml_atlas_label_table_to_atlas_list(ngr);
      NI_free_element(ngr) ; ngr = NULL;
      RETURN(apl);
   }
   else {
      if (LocalHead) fprintf(stderr, "Label table NOT found in attributes.\n");
      RETURN(NULL);
   }
   
   RETURN(NULL);
}

/* convert a NIML table to an atlas list structure */
ATLAS_POINT_LIST * niml_atlas_label_table_to_atlas_list(NI_group *ngr)
{
   ATLAS_POINT_LIST *apl;
   int i, tdlev, tdval;
   char *temp_str;
   ATLAS_POINT *at_pt;
   NI_element *nel=NULL;
   int LocalHead = wami_lh();

   float cog[3];
   short okey;
   ATR_string *atr=NULL;

   ENTRY("niml_atlas_label_table_to_atlas_list");

   if (!ngr) RETURN(NULL);
   
   /* get each segmented region - the "atlas point" from 
      a NIML formatted string */ 
   apl = (ATLAS_POINT_LIST *) calloc(1, sizeof(ATLAS_POINT_LIST));
   /* assume the number of elements in the group is the number of structures */
   apl->n_points = ngr->part_num; 
   apl->at_point = (ATLAS_POINT *) calloc(apl->n_points,sizeof(ATLAS_POINT));
   if(apl->at_point == NULL) {         
         WARNING_message("** WARNING: Poorly formatted ATLAS_POINT_LIST\n");
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

      /* mod - drg 09/25/2018 */
      /* allow for longer name - later add hierarchial name for larger region*/
      temp_str = NI_get_attribute(nel, "LONGNAME");
      /* copy "LONGNAME" name - segmentation name */
      if(temp_str==NULL)
         NI_strncpy(at_pt->longname,"",ATLAS_CMAX);
      else
         NI_strncpy(at_pt->longname,temp_str,ATLAS_CMAX);

      /* sub-brick label for probability maps */
      temp_str = NI_get_attribute(nel, "SB_LABEL");
      if(temp_str==NULL)
         NI_strncpy(at_pt->sblabel,"",ATLAS_CMAX);
      else
         NI_strncpy(at_pt->sblabel,temp_str,ATLAS_CMAX);

      at_pt->tdval = tdval;
      at_pt->okey = okey;
      at_pt->tdlev = tdlev;
      at_pt->xx = cog[0];
      at_pt->yy = cog[1];
      at_pt->zz = cog[2];
   }    

   RETURN(apl); 
}

ATLAS_XFORM_LIST *identity_xform_chain(char *space_name) 
{
   ATLAS_XFORM *xf;
   ATLAS_XFORM_LIST *xfl=NULL;
   
   xf = identity_xform();      /* assign identity matrix */
   free(xf->source); free(xf->dest);
   xf->source = nifti_strdup(space_name);
   xf->dest = nifti_strdup(space_name);
   xfl = (ATLAS_XFORM_LIST *) calloc(1, sizeof(ATLAS_XFORM));
   xfl->xform = xf;
   xfl->nxforms = 1;
   return(xfl);
}


/* write atlas labels */

/* write atlas color scale */

/* read transformation for space 1 to space 2 */

/* write transformation for space 1 to space 2 */


/* report xform chain to go between src and dest spaces */
ATLAS_XFORM_LIST *report_xform_chain(char *src, char *dest, int report)
{
   ATLAS_XFORM_LIST  *xfl;
   int srci, desti;
   ATLAS_SPACE_LIST *asl=get_G_space_list();
   
   if (!src || !dest) return(NULL);
   
   /* atlas testing */
   if (!strcmp(src, dest)) {
      if(wami_verb() > 1)
         INFO_message("Chain is from and to same space %s", src);
      xfl = identity_xform_chain(src);
   } else {
      srci = find_atlas_space_index(src);
      if(srci<0 && wami_verb()){
         INFO_message("Could not find source space %s in database", src);
      }    
      desti = find_atlas_space_index(dest);
      if(desti<0 && wami_verb()){
         INFO_message("Could not find destination space %s in database", dest);
         print_space_list(NULL);
      }
      /* check if we're going nowhere */
      if(srci==desti) {
         if(wami_verb() > 1)
            INFO_message("Chain is from and to same space");
         xfl = identity_xform_chain(src);
      } else {
         if (srci >= 0 && desti >= 0) {
            xfl = get_xform_chain(asl->space+srci,
                              asl->space+desti);
         } else {
            xfl = NULL;
         }
      }
   }   
   if (report) print_xform_list(xfl);
   return(xfl);
}

/* report which spaces are available to go between src and all other spaces */
void report_available_spaces(char *src)
{
   ATLAS_SPACE_LIST *spl;
   
   spl = find_available_spaces(src, NULL);
   print_space_list(spl);
   free_space_list(spl);    
}


/* initialize space structures for dealing with templates and atlases */
int   init_space_structs(ATLAS_XFORM_LIST **atlas_xfl,
   ATLAS_LIST **atlas_alist,
   ATLAS_SPACE_LIST **atlas_spaces,
   ATLAS_TEMPLATE_LIST **atlas_templates)
{
   *atlas_alist = (ATLAS_LIST *) calloc(1,sizeof(ATLAS_LIST));
   *atlas_spaces = (ATLAS_SPACE_LIST *) calloc(1,sizeof(ATLAS_SPACE_LIST));
   *atlas_templates = (ATLAS_TEMPLATE_LIST *) 
                                    calloc(1, sizeof(ATLAS_TEMPLATE_LIST));
   *atlas_xfl = (ATLAS_XFORM_LIST *) calloc(1, sizeof(ATLAS_XFORM_LIST));
   (*atlas_xfl)->nxforms = 0;
   (*atlas_xfl)->xform = NULL;
   (*atlas_alist)->natlases = 0;
   (*atlas_spaces)->nspaces = 0;
   (*atlas_templates)->ntemplates = 0;

   if(   !(*atlas_alist) || !(*atlas_spaces) || 
         !(*atlas_templates) || !(*atlas_xfl)) {
      return(0);
   }
   return(1);      
}


/* read niml file elements into C structures */
int   read_space_niml_file( char *fname, ATLAS_XFORM_LIST *atlas_xfl,
   ATLAS_LIST *atlas_alist,
   ATLAS_SPACE_LIST *atlas_spaces,
   ATLAS_TEMPLATE_LIST *atlas_templates,
   THD_string_array *sar)
{
   NI_element *nel;
   NI_stream space_niml;
   char *fnamet=NULL, *fnameclean=NULL;
   int found = 0; 

   if (!fname) return(found);
   
   if (!(fnameclean = af_strnstr(fname, "file:", 5))) {
      fnamet = (char *)calloc(6+strlen(fname), sizeof(char));
      sprintf(fnamet,"file:%s", fname);
      space_niml = NI_stream_open(fnamet,"r");
      free(fnamet);
      fnameclean = fname;
   } else {
      space_niml = NI_stream_open(fname,"r");
   }
      
   if (!space_niml) {
      ERROR_message("Failed to NI_stream open %s\n", fnameclean);
      return(found);
   }
   
   nel = (NI_element *) 1;
   while(nel) {
      if(wami_verb() > 2) 
         INFO_message("reading elements\n");
      if ((nel = NI_read_element(space_niml, 100))) {
         found += add_atlas_nel(nel, atlas_xfl, 
                              atlas_alist,atlas_spaces,atlas_templates, sar,
                              THD_filepath(fnameclean)); 
         NI_free_element(nel);  /* don't need the NIML element anymore */
      }
   }

   NI_stream_close(space_niml);
   
   return(found);
}

int add_atlas_nel(NI_element *nel, ATLAS_XFORM_LIST *atlas_xfl,
   ATLAS_LIST *atlas_alist,
   ATLAS_SPACE_LIST *atlas_spaces,
   ATLAS_TEMPLATE_LIST *atlas_templates,
   THD_string_array *sar,
   char *parentdir) {
   
   int found = 0;
   
   if (!nel) return(found);
   
   if(wami_verb() > 2) 
     INFO_message("nel name %s\n", nel->name);
   if (nel->type == NI_ELEMENT_TYPE) {
      if(strcmp(nel->name, "TEMPLATE_SPACE") == 0) {
         atlas_spaces->nspaces++;
         if(wami_verb() > 1){
            INFO_message("Template space\n"
                         "number of spaces now %d\n",
                                    atlas_spaces->nspaces);
         }
         if(atlas_spaces->nspaces==1) {
            if(wami_verb() > 2)
               INFO_message("initial memory allocation for spaces");
            atlas_spaces->space = 
                  (ATLAS_SPACE *) calloc(1, sizeof(ATLAS_SPACE));
         } else  {             
            atlas_spaces->space = 
               (ATLAS_SPACE *) realloc( atlas_spaces->space, 
                           atlas_spaces->nspaces * sizeof(ATLAS_SPACE));
         }
         atlas_read_atlas_space(
              nel, &atlas_spaces->space[atlas_spaces->nspaces-1]);

         found = 1;
      }
      if(strcmp(nel->name, "XFORM") == 0) {
         atlas_xfl->nxforms++;
         if(wami_verb() > 2){
            INFO_message("space XFORM\n");
            INFO_message("number of xforms now %d\n", atlas_xfl->nxforms);
         }
         if(atlas_xfl->nxforms==1){
            if(wami_verb() > 2)
               INFO_message("initial memory allocation for xforms");
            atlas_xfl->xform = 
                  (ATLAS_XFORM *) calloc(1, sizeof(ATLAS_XFORM));
         }
         else               
            atlas_xfl->xform = (ATLAS_XFORM *) realloc(
                atlas_xfl->xform, 
                atlas_xfl->nxforms * sizeof(ATLAS_XFORM));
         atlas_read_xform(nel, &atlas_xfl->xform[atlas_xfl->nxforms-1]);
         found = 1;
      } 
      if(strcmp(nel->name, "ATLAS") == 0) {
        atlas_alist->natlases++;
        if(wami_verb() > 2){
            INFO_message("Number of atlases now %d\n", 
                         atlas_alist->natlases);
         }
         if(atlas_alist->natlases==1){
            if(wami_verb() > 2)
               INFO_message("initial memory allocation for atlases");
            atlas_alist->atlas = (ATLAS *) calloc(1, sizeof(ATLAS));
         } else {               
            atlas_alist->atlas = (ATLAS *) realloc(
                atlas_alist->atlas, atlas_alist->natlases * sizeof(ATLAS));
            memset(&(atlas_alist->atlas[atlas_alist->natlases-1]), 0, 
                     sizeof(ATLAS));
         }
         atlas_read_atlas(nel,
                          &atlas_alist->atlas[atlas_alist->natlases-1],
                          parentdir);
         if (sar && 
             atlas_alist->atlas[atlas_alist->natlases-1].name){
               ADDUTO_SARR(sar,
                  atlas_alist->atlas[atlas_alist->natlases-1].name);
         }  /* add the name of that atlas to the string array */
         found = 1;
      }

      if(strcmp(nel->name, "TEMPLATE") == 0) {
         atlas_templates->ntemplates++;
         if(wami_verb() > 2){
            INFO_message("Atlas template\n");
            INFO_message("number of templates now %d\n", 
                atlas_templates->ntemplates);
         }
         if(atlas_templates->ntemplates==1){
            if(wami_verb() > 2)
               INFO_message("initial memory allocation for templates");
            atlas_templates->atlas_template = 
                (ATLAS_TEMPLATE *) calloc(1,sizeof(ATLAS_TEMPLATE));
         }
         else               
            atlas_templates->atlas_template = (ATLAS_TEMPLATE *) realloc(
                atlas_templates->atlas_template, 
                atlas_templates->ntemplates * sizeof(ATLAS_TEMPLATE));
         atlas_read_template(nel,
            &atlas_templates->atlas_template[
                           atlas_templates->ntemplates-1]);
         found = 1;
      }
   }      
   return(found);
}

/* compute what spaces are neighbors of the others  */
/* in preparation for Dijkstra search */
int make_space_neighborhood( ATLAS_SPACE_LIST *at_spl, 
                             ATLAS_XFORM_LIST *atlas_xfl)
{
      /* See Ziad's NIH-5 labbook pp 46 for graph */


      int i, j, nspaces, inv_xf, neighbor_i;
      ATLAS_SPACE *atlas_space, *dest_space;
      ATLAS_XFORM *xform;
      
      nspaces = at_spl->nspaces;
      if(nspaces == 0) {
         if(wami_verb() > 1)
            INFO_message("no spaces to compute paths among");
         FirstNeighb = NULL; FirstNeighbDist = NULL; N_Neighb = NULL;
         return(-1);
      }
      FirstNeighb = (int **) calloc(nspaces , sizeof(int *));
      FirstNeighbDist = (float **) calloc(nspaces , sizeof(float *));
      N_Neighb = (int *) calloc(nspaces , sizeof(int));

      if(wami_verb() > 2)
         INFO_message("initial memory allocation for neighbors: nspaces = %d",
                      nspaces);
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
            dest_space = at_spl->space+j;
            if(wami_verb() > 1)
              INFO_message("Computing path from %s(%d) to %s(%d)", 
                           atlas_space->atlas_space, i,
                           dest_space->atlas_space, j);

            if(i==j) continue;  /* don't need to match the same space */
            xform = get_xform_neighbor(
                     atlas_xfl, atlas_space, dest_space, &inv_xf);
            if(xform!=NULL){
               if(neighbor_i==0){
                  FirstNeighb[i] = (int *) calloc(1, sizeof(int));
                  FirstNeighbDist[i] = (float *) calloc(1, sizeof(float));
               }
               else {
                  FirstNeighb[i] = (int *) realloc(FirstNeighb[i], 
                   (neighbor_i+1)*sizeof(int));
                  FirstNeighbDist[i] = (float *) realloc(FirstNeighbDist[i], 
                   (neighbor_i+1)*sizeof(float));
               }
               if((FirstNeighb[i]==NULL) || (FirstNeighbDist[i]==NULL)) {
                  WARNING_message("Could not allocate space for "
                                  "atlas neighborhood");
                  return(-1);
               }
               
               FirstNeighb[i][neighbor_i] = j;
               FirstNeighbDist[i][neighbor_i++] = xform->dist;
               if(wami_verb() > 1){
                  INFO_message("neighbor found for space %d with space %d",i,j);
                  INFO_message("xform %s with dist %f", 
                               xform->xform_name, xform->dist);
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
    int i, cc;
    char *srcstr, *deststr, *xfsrc, *xfdest;
    ATLAS_XFORM *xf, *xf2 = NULL;
       
    srcstr = at_space->atlas_space;
    deststr = dest_space->atlas_space;

    *inv_xf = 0;

    /* check if xform from src to dest space is in list of xforms */    
    for(i=0;i<atlas_xfl->nxforms;i++) {
       xf = atlas_xfl->xform+i;
       xfsrc = xf->source;
       xfdest = xf->dest;
       if((strcmp(srcstr, xfsrc)==0) && (strcmp(deststr,xfdest)==0)) {
          return(xf);
       }
    }

    /* if we made it here, then check for dest to src, and mark to invert */
    for(i=0;i<atlas_xfl->nxforms;i++) {
       xf = atlas_xfl->xform+i;
       xfsrc = xf->source;
       xfdest = xf->dest;
       /* check if inverse direction is available */
       if((strcmp(deststr, xfsrc)==0) && (strcmp(srcstr,xfdest)==0)) {
          /* is the original xform invertible */
          xf2 =  (ATLAS_XFORM *)  calloc(1, sizeof(ATLAS_XFORM));
          if(copy_xform(xf, xf2)!=0){
             WARNING_message("Could not create copy of xform for path");
             return(NULL);
          }
          xf2->inverse = 1;
          cc = invert_xform(xf2);
          free_xform(xf2);
          free(xf2);
          if(cc) {
             if(wami_verb() > 1){
               INFO_message("Can not invert transform in path from %s to %s",
                  xfsrc, xfdest);
             }
          }
          else {
             if(wami_verb() > 1){
               INFO_message("Using invertible transform in path from %s to %s",
                  xfsrc, xfdest);
             }
             *inv_xf = 1;
             return(xf);
          }
       }
    }

    return(NULL);
}

/* find shortest path to go from one space to a destination space */
/* return the list of transformations needed to accomplish this */
ATLAS_XFORM_LIST * get_xform_chain( ATLAS_SPACE *at_space, 
                                    ATLAS_SPACE *dest_space)
{
   int srci, desti;
   int N_n, kk, *nPath;
   float nDistance;
   ATLAS_XFORM_LIST *xfl=NULL;
   ATLAS_SPACE_LIST *asl=get_G_space_list();
   ATLAS_XFORM_LIST *axl=get_G_xform_list();
   /* you might want to return identity right here if 
      at_space->atlas_space == dest_space->atlas_space,
      even if find_atlas_space can't find them ... */
      
   /* find index for input spaces */
   if ((srci  = find_atlas_space(asl, at_space))<0) {
      ERROR_message("input space %s/%s not in atlas_spaces", 
               at_space->atlas_space, at_space->generic_space);
      print_space_list(asl);
      return(NULL);
   }
   if ((desti = find_atlas_space(asl, dest_space))<0) {
      ERROR_message("destination space %s/%s not in atlas_spaces", 
               dest_space->atlas_space, dest_space->generic_space);
      return(NULL);
   };
   /* if src and dest are the same, should return identity right away */
   /* check if neighborhood is defined */
   if((N_Neighb==NULL) || (FirstNeighbDist==NULL)) return (NULL);
   /* search neighborhood for shortest path between indices */
   if ( !(nPath = SUMA_Dijkstra_generic ( 
                          asl->nspaces, 
                          NULL, -1, 0,
                          N_Neighb, FirstNeighb, FirstNeighbDist,
                          srci, desti, 
                          NULL, NULL, 
                          1, 
                          &nDistance, &N_n, 0)) ) {
         if(wami_verb()>1) fprintf(stderr, 
            "No path found in Dijkstra from %d to %d space", srci, desti);
         return(NULL); /* no path found */
   } else {
      if(wami_verb() > 1){
         INFO_message("Number of spaces to traverse %d with distance %.2f ",
                     N_n, nDistance);
         fprintf(stderr, "spaces in chain by index: ");
         for(kk=0; kk<N_n; ++kk) 
            fprintf(stderr, "%d ", nPath[kk]); 
         fprintf(stderr, "\n");
      }

      xfl = pathlist_to_xform_list(nPath, N_n,
                  axl, asl);
      free(nPath); nPath = NULL;
   }

   return(xfl);
}

/* find space index that matches space name */
int find_atlas_space(ATLAS_SPACE_LIST *at_spl, ATLAS_SPACE *at_space)
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
int find_atlas_space_index(char *spacename)
{
   int i;
   ATLAS_SPACE *sp1;
   ATLAS_SPACE_LIST *asl=get_G_space_list();

   if((spacename==NULL) || (strcmp(spacename, "")==0)
      || !asl) {
      if (wami_verb()) {
         ERROR_message("Null input: spacename = %s, asl = %p",
               STR_PRINT(spacename), asl);
      }
      return(-1);
   }    
   for(i=0;i<asl->nspaces;i++) {
      sp1 = asl->space+i;
      if(strcmp(sp1->atlas_space, spacename)==0)
         return(i);
   }
   return(-1);   /* not found */ 
}

/* return the atlas_space structure corresponding to the atlas_space name
   in the dataset structure */
ATLAS_SPACE * dset_space(THD_3dim_dataset *dset)
{
   int spacei;
   ATLAS_SPACE_LIST *asl=get_G_space_list();
   
   spacei = find_atlas_space_index(dset->atlas_space);
   if(spacei==-1)
      return(NULL);
   
   return(asl->space+spacei);
}

/* find all available spaces that can be transformed to from some original space */
ATLAS_SPACE_LIST *find_available_spaces(  char *src_space_name, 
                                          ATLAS_SPACE_LIST *this_spl)
{
  int i, curr_i, nspaces=0;
  ATLAS_SPACE_LIST *spl;
  ATLAS_SPACE_LIST *search_list=NULL;
  ATLAS_XFORM_LIST *xfl;
  ATLAS_SPACE *xs, *spl_space, *src_space;
  
  if (!this_spl) search_list = get_G_space_list();
  else search_list = this_spl; 
  
  spl = NULL;
  
  /* find index of current space string in static space list */
  curr_i = find_atlas_space_index(src_space_name);
  src_space = search_list->space+curr_i;
  
  /* search through all spaces */
  for(i=0;i<search_list->nspaces;i++){
     if(i==curr_i) continue;   /* don't count the current space */
     xs = search_list->space+i;   /* pointer to indexed space */
     xfl = get_xform_chain(src_space, xs);
     if(xfl){   /* found a transformation */
        if(wami_verb() > 1)
            INFO_message("Found an available space: %s", xs->atlas_space);
        free_xform_list(xfl);  /* don't actually need the xform list */
        
        if(spl==NULL) {
           spl = (ATLAS_SPACE_LIST *) calloc(1, sizeof(ATLAS_SPACE_LIST));
           spl->space = (ATLAS_SPACE *) calloc(1, sizeof(ATLAS_SPACE));
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

        if( (spl_space->atlas_space == NULL) || 
            (spl_space->generic_space == NULL)) {
           WARNING_message("Could not allocate template space strings");
           return(NULL);
        }
        spl->nspaces = nspaces;

     }
  }
  if(spl) {
      spl->nspaces = nspaces;
      if(wami_verb() > 1)
         INFO_message("There are %d spaces available", spl->nspaces);
  } else {
     if(wami_verb() > 1) {
         print_space_list(search_list);  
         INFO_message("no spaces reachable from source space: %s", 
                      src_space_name);
      } 
  }

  return(spl);
}

ATLAS_XFORM_LIST * pathlist_to_xform_list(int *nPath, int N_n, 
                                          ATLAS_XFORM_LIST *atlas_xfl, 
                                          ATLAS_SPACE_LIST *at_spl)
{
   int kk, inv_xf;
   ATLAS_XFORM_LIST *xflc = NULL;
   ATLAS_XFORM *a_xform, *xxform;
   ATLAS_SPACE *sp1, *sp2;

   xflc = (ATLAS_XFORM_LIST *) calloc(1, sizeof(ATLAS_XFORM_LIST));
   xflc->xform =  (ATLAS_XFORM *)  calloc((N_n-1), sizeof(ATLAS_XFORM));
   xflc->nxforms = N_n-1;
         
   for(kk=0;kk<N_n-1;++kk) {
       sp1 = at_spl->space+nPath[kk]; /* starting space */
       sp2 = at_spl->space+nPath[kk+1];   /* next space */
       a_xform = get_xform_neighbor(atlas_xfl,sp1, sp2, &inv_xf); 
       if(wami_verb() > 1){
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

       if(wami_verb() > 1){
          print_xform(xxform);
       }

   }

   if(wami_verb() > 1){
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
   dest_xform->post = src_xform->post;
   dest_xform->nelts = src_xform->nelts;
   if(dest_xform->nelts==0) return(0);
   dest_xform->xform = calloc(dest_xform->nelts, sizeof(float));
   if(dest_xform->xform==NULL) return(1);
   memcpy(dest_xform->xform, src_xform->xform, dest_xform->nelts*sizeof(float));
   return(0);
}

/* copy elements of xform structure, allocating space for each element */
/* dest_xform already exists as a structure, it just needs to be populated */
ATLAS_XFORM *identity_xform()
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
   dest_xform->post = 1;

   dest_xform->nelts = 1;
   if(dest_xform->nelts==0) return(dest_xform);
   dest_xform->xform = calloc(dest_xform->nelts, sizeof(float));
   if(dest_xform->xform==NULL) return(NULL);
   fptr = (float *) dest_xform->xform;
   *fptr  = 1.0;
   return(dest_xform);
}


/* free list of xforms */
void free_xform_list(ATLAS_XFORM_LIST *xfl)
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
void free_xform(ATLAS_XFORM *xf)
{   
   if(xf==NULL)
      return;
   free(xf->xform);
   free(xf->xform_type); free(xf->xform_name); free(xf->source); free(xf->dest);
   free(xf->coord_order);
}    

/* free list of spaces */
void free_space_list(ATLAS_SPACE_LIST *xsl)
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
void free_space(ATLAS_SPACE *xs)
{   
   if(xs==NULL)
      return;
   free(xs->atlas_space); free(xs->generic_space); 
}    

/* free list of spaces */
void free_template_list(ATLAS_TEMPLATE_LIST *xtl)
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
void free_template(ATLAS_TEMPLATE *xa)
{   
   if(xa==NULL)
      return;
   if (xa->space) free(xa->space); 
   if (xa->template) free(xa->template);
   if (xa->description) free(xa->description); 
   if (xa->comment) free(xa->comment);
} 

/* free list of atlases */
void free_atlas_list(ATLAS_LIST *xal)
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

/* free an atlas dset holder */
void free_adh(ATLAS_DSET_HOLDER *adh) {
   if (adh) free(adh);
}

/* free an atlas structure */
void free_atlas(ATLAS *xa)
{   
   if(xa==NULL)
      return;
   if (xa->space) free(xa->space); 
   if (xa->dset_name) free(xa->dset_name);
   if (xa->description) free(xa->description); 
   if (xa->name) free(xa->name);
   if (xa->comment) free(xa->comment);
   if (xa->atlas_type) free(xa->atlas_type);
   if (xa->supp_web_info) free(xa->supp_web_info);
   if (xa->supp_web_type) free(xa->supp_web_type);
   if (xa->supp_conn_info) free(xa->supp_conn_info);
   if (xa->supp_conn_type) free(xa->supp_conn_type);
   if (xa->orient) free(xa->orient);

   if (xa->adh) free_adh(xa->adh);
} 

/* return 1 is combined xform is identity */
int is_identity_xform_list(ATLAS_XFORM_LIST *xfl, int combine)
{
   int i;
   ATLAS_XFORM_LIST *cxfl=NULL;
   ATLAS_XFORM *xf;

   if(xfl==NULL) {
      if (wami_verb()) fprintf(stderr,"NULL transform\n");
      return(0);
   }
   if (combine) {
      if (!(cxfl = calc_xform_list(xfl))) return(0);
      xfl = cxfl;
   }
   
   for(i=0;i<xfl->nxforms;i++) {
      xf = xfl->xform+i;
      if (strcmp(xf->xform_type,"Identity")) {
         if (cxfl) free_xform_list(cxfl);
         return(0);
      }
   }
   if (cxfl) free_xform_list(cxfl);
   return(1);
}

/* return 1 if xform from src to dest is Identity */
int is_identity_xform_chain(char *src, char *dest) 
{
   ATLAS_XFORM_LIST *xfl=NULL;
   int ans=0;
   
   if (!src || !dest) return(0);
   if (!strcmp(src,dest)) return(1);
   
   xfl = report_xform_chain(src, dest, 0);
   ans = is_identity_xform_list(xfl, 1);
   free_xform_list(xfl);
   return(ans);
}

void print_xform_chain(char *src, char *dest)
{
   ATLAS_XFORM_LIST *xfl=report_xform_chain(src, dest,1);
   if (xfl) free_xform_list(xfl);
   return;   
}

/* print list of xforms - short form - as a chain */
void print_xform_list(ATLAS_XFORM_LIST *xfl)
{
   int i;
   ATLAS_XFORM *xf;
   INFO_message("----- Transform list: -------");
   
   if(xfl==NULL) {
      fprintf(stderr,"NULL transform\n");
      return;
   }
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
void print_all_xforms(ATLAS_XFORM_LIST *xfl)
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
void print_xform(ATLAS_XFORM *xf)
{

   int i;
   float *xfptr;
   
   fprintf(stderr, "xform: %s\n", xf->xform_name);
   fprintf(stderr, "xform_type: %s\n", xf->xform_type);
   fprintf(stderr, "xform source: %s   dest: %s\n", xf->source, xf->dest);
   fprintf(stderr, "coord order: %s\n", xf->coord_order);
   fprintf(stderr, "xform dist: %f  inverse: %d   post: %d   nelts: %d\n", 
           xf->dist, xf->inverse, xf->post, xf->nelts);
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
void print_affine_xform_data(float *xfptr)
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
void print_space_list(ATLAS_SPACE_LIST *xsl)
{
   int i;
   ATLAS_SPACE *xs;
   
   if(xsl==NULL) {
      if(wami_verb() > 1)
         INFO_message("NULL Space list pointer, showing global list\n");
      xsl = get_G_space_list();
   }
   if(wami_verb() > 1)
      INFO_message("Space list has %d spaces\n",xsl->nspaces);
   INFO_message("----- List of available spaces: -------");
   for(i=0;i<xsl->nspaces;i++) {
      xs = xsl->space+i;
      INFO_message("%s", xs->atlas_space);
   }
}

/* print list of atlases */
void print_atlas_list(ATLAS_LIST *xal)
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
      print_atlas(xa, 0);
   }
}

/* print a single line entry description of an atlas */
void print_atlas(ATLAS *xa, int level) 
{
   if(level==0)
      INFO_message("Atlas name: %s, file: %s, space: %s\n",
                xa->name, xa->dset_name, xa->space);
   else
      INFO_message("Atlas name: %s, file: %s, space: %s\n"
                "dset %p, %d sub-bricks \n"
                "adh %p\n", 
                xa->name, xa->dset_name, xa->space, 
             ATL_DSET(xa), ATL_DSET(xa) ? DSET_NVALS(ATL_DSET(xa)):-1,
                xa->adh);
   return;
}


/* print table of atlases - name, dset, description, comments */
void print_atlas_table(ATLAS_LIST *xal)
{
   int i;
   ATLAS *xa;
   
   INFO_message("----- Atlas list: -------");
   if(xal==NULL){
      INFO_message("** No atlases found **");
      return;
   }

   INFO_message("Name             Space    Dataset              Description");
   INFO_message("__________________________________________________________");
   for(i=0;i<xal->natlases;i++) {
      xa = xal->atlas+i;
      INFO_message("%-25.25s %-15.15s %s %-60s",
                   xa->name, xa->space,
                   xa->dset_name,
                   ATL_DESCRIPTION_S(xa));
   }

   INFO_message("\n");
   for(i=0;i<xal->natlases;i++) {
      xa = xal->atlas+i;
      if (ATL_COMMENT(xa)) { 
         INFO_message("%s: %s", ATL_NAME(xa), ATL_COMMENT_S(xa));
      }
      else printf("no comment\n");
   }
   INFO_message("--------------------------");
}


/* print the comment for an atlas  - may span multiple lines with '\n' */
void print_atlas_comment(ATLAS *xa)
{
    if((xa) && ATL_COMMENT(xa))
       INFO_message("%s", ATL_COMMENT(xa));
}

/* print the atlas_type for an atlas */
void print_atlas_type(ATLAS *xa)
{
    if((xa) && ATL_TYPE(xa))
       INFO_message("%s", ATL_TYPE(xa));
}

/* print the atlas supplemental information for an atlas */
void print_atlas_supp_web_info(ATLAS *xa)
{
    if((xa) && ATL_SUPP_WEB_INFO(xa)) {
       INFO_message("%sroiname%s", ATL_SUPP_WEB_INFO_S(xa),
                    ATL_SUPP_WEB_TYPE_S(xa));
    }
    if((xa) && ATL_SUPP_CONN_INFO(xa)) {
       INFO_message("%sroiname%s", ATL_SUPP_CONN_INFO_S(xa),
                    ATL_SUPP_CONN_TYPE_S(xa));
    }

}

/* print the coordinate orientation for an atlas */
void print_atlas_orient(ATLAS *xa)
{
    if((xa) && ATL_ORIENT(xa))
       INFO_message("%s", ATL_ORIENT(xa));
}

/* print table of atlases - name, dset, description, comments */
void print_point_lists(ATLAS_LIST *xal)
{
   int i;
   ATLAS *xa;
   ATLAS_POINT_LIST *apl;

   INFO_message("----- Atlas point lists: -------");
   if(xal==NULL){
      INFO_message("** No atlases found **");
      return;
   }

   for(i=0;i<xal->natlases;i++) {
      xa = xal->atlas+i;
      INFO_message("Atlas name : %-25.25s, Dataset: %-54.54s",
                   xa->name,xa->dset_name);
      apl = atlas_point_list(xa->name);
      if(apl)
         print_atlas_point_list(apl);
      else{
         if(ATL_WEB_TYPE(xa))
            INFO_message("web-based atlas. No local point list");
         else
            INFO_message("**** No point list. Atlas needs repair!");
      }
      INFO_message(
              "__________________________________________________________");
   }

   INFO_message("\n");
   for(i=0;i<xal->natlases;i++) {
      xa = xal->atlas+i;
      if (ATL_COMMENT(xa)) { 
         INFO_message("%s: %s", ATL_NAME(xa), ATL_COMMENT_S(xa));
      }
   }
   INFO_message("--------------------------");
}

/* return max length of a label in an atlas point list */
int
atlas_max_label_length(ATLAS_POINT *ap, int n_points)
{
   int i,len,maxlen=0;
   
   if(ap==NULL)
      return(0);

   for(i=0;i<n_points;i++) {
       /* name (+long name?) */ 
       len = strlen(Atlas_name_choice(&ap[i]));
       if(len>maxlen) maxlen = len;
   }

   return(maxlen);
}

/* mark if any atlas points have a non-zero level associated with them */
int
atlas_level(ATLAS_POINT *ap, int n_points)
{
   int i;
   
   if(ap==NULL)
      return(0);

   for(i=0;i<n_points;i++) {
      if(ap[i].tdlev) return(1);
   }

   return(0);
}

/* print info about an atlas coordinate */
void print_atlas_coord (ATLAS_COORD ac) 
{
   INFO_message("----- Atlas Coord: -------");
   INFO_message("%f %f %f (%s), space_name %s%s\n",
      ac.x, ac.y, ac.z, ac.orcode, ac.space_name,
      is_known_coord_space(ac.space_name) ? "":"Lost in space");
}

int is_known_coord_space(char *space_name) {
   if (!space_name || space_name[0]=='\0' ||
       !strcmp(space_name,"Unknown")) return(0);
   if (find_atlas_space_index(space_name) >= 0) return(1);
   return(0);
}

/* print list of templates */
void print_template_list(ATLAS_TEMPLATE_LIST *xtl)
{
   int i, templen;
   ATLAS_TEMPLATE *xt;
   char *tempstr, *tempstr2=NULL;
   INFO_message("----- Template list: -------");
   if(xtl==NULL)
      return;
   for(i=0;i<xtl->ntemplates;i++) {
      xt = xtl->atlas_template+i;
      if(xt->description) {
         tempstr2 = ATL_DESCRIPTION_S(xt);
         templen = strlen(xt->template)+strlen(tempstr2) + 3;
         tempstr = (char *)calloc( templen, sizeof(char)); 
         sprintf(tempstr, "%s: %s", xt->template, ATL_DESCRIPTION_S(xt));
         show_wrapping_line(tempstr,"", 0, stdout);
         free(tempstr);
      }
      else {
         show_wrapping_line(xt->template,"", 0, stdout);
      }
      if(xt->comment){
/*         show_wrapping_line("Comment:\n","", 0, stdout);*/
         show_wrapping_line(ATL_COMMENT(xt),"   ", 0, stdout);
      }
   }
}


/* print the comment for a template  - may span multiple lines with '\n' */
void print_template_comment(ATLAS_TEMPLATE *xa)
{
    if((xa) && ATL_COMMENT(xa))
       INFO_message("%s", ATL_COMMENT(xa));
}


/* apply line indent per line, if we exceed MAX_LINE_CHARS, wrap */
/* from afni_history.c - rickr, moved by drg */
int show_wrapping_line(char * str, char * prefix, int indent, FILE * fp)
{
    int c, cline, len;

    if( !str ) return 0;

    if( prefix ) fputs(prefix, fp);

    len = strlen(str);
    if( len < 2 ) return 1;

    if( str[len-1] == '\n' ) len--;     /* ignore trailing newline */

    cline = 0;
    for( c = 0; c < len; c++ ) {
        if( str[c] == '\n' ) {          /* print newline and indent */
            fputc('\n', fp);
            fprintf(fp, "%*s", indent, "");
            cline = 0;
            continue;
        } else if ( cline > MAX_WAMI_LINE_CHARS ) {  /* fix, and continue */
            fputc('\n', fp);
            fprintf(fp, "%*s", indent, "");
            cline = 0;
        }
        fputc(str[c], fp);
        cline++;
    }

    fprintf(fp,"\n");

    return 0;
}


/* calculate list of xforms */
ATLAS_XFORM_LIST *calc_xform_list(ATLAS_XFORM_LIST *xfl)
{
   int i, nxf, sl1, sl2, cc;
   ATLAS_XFORM *xf, *xf2, *xf3, *oldxfptr=NULL;
   char *source, *dest;
   ATLAS_XFORM_LIST *cxfl;

   if(wami_verb() > 1)
      printf("calculating xform list\n");   
   if(xfl==NULL)
      return(NULL);
   nxf = (xfl->nxforms) - 1;

   /* make condensed transformation list structure */
   cxfl = (ATLAS_XFORM_LIST *)calloc(1,sizeof(ATLAS_XFORM_LIST));
   if(cxfl==NULL)
      ERROR_exit("Could not allocate space for condensed xform list\n");
   cxfl->xform =  (ATLAS_XFORM *)  calloc((xfl->nxforms),sizeof(ATLAS_XFORM));
   if(cxfl->xform==NULL)
      ERROR_exit("Could not allocate space for condensed xform list xforms\n");

   cxfl->nxforms = 0;
   if(wami_verb() > 1)
      printf("starting to combine xforms\n");
   /* only one xform, just go home */
   if(xfl->nxforms==1){
      if(wami_verb() > 1)
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
         xf->xform_name = (char *) calloc((sl1+sl2+3),sizeof(char));
         sprintf(xf->xform_name, "%s::%s", xf->source, xf->dest);
      }
      return(cxfl);
   }
   /* do calculations on xforms a pair at a time */
   xf = xfl->xform;
   for(i=0;i<nxf;i++) {
      if(wami_verb() > 1)
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

      if(wami_verb() > 1)
        INFO_message("Multiplying %s type with %s type in chain\n", xf->xform_type,
          xf2->xform_type);

      xf3 = calc_xf(xf,xf2);   /* calculate xforms a pair of time */
      if(xf3) {
         free(xf3->xform_name);
         free(xf3->source); free(xf3->dest);

         xf3->source = source;
         xf3->dest = dest;
         sl1 = strlen(xf3->source); sl2 = strlen(xf3->dest);
         xf3->xform_name = (char *) calloc((sl1+sl2+3),sizeof(char));
         sprintf(xf3->xform_name, "%s::%s", xf3->source, xf3->dest);

         if(i==(nxf-1)){
            if(wami_verb() > 1)
               printf(
                 "On last xform, copying last combined xform"
                 " to combined xform list\n");
            cc = copy_xform(xf3, cxfl->xform+(cxfl->nxforms));
            cxfl->nxforms++;
            if(wami_verb() > 1){
               print_xform(xf3);
               xf = xf3;
               print_xform(xf);
              }
            }
         else{
            if(wami_verb() > 1)
               printf("could combine xform %d with %d\n", i, i+1);
            xf = xf3; cc = 0;
/*            cc = copy_xform(xf3, xf); */ /* use combined xform for next pair */
            if(wami_verb() > 1)
               print_xform(xf);

         }

      }
      else {
         if(wami_verb() > 1)
            printf("could not calculate this combination of xforms "
                   "- adding to chain\n");
         cc = copy_xform(xf, cxfl->xform+(cxfl->nxforms));
         cxfl->nxforms++;
         /* if on last combination, add the last one too */
         if(i==nxf-1){
            cc = copy_xform(xf2, cxfl->xform+(cxfl->nxforms));
            cxfl->nxforms++;
         }

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

/*   if(wami_verb() > 1)
      INFO_message("Multiplying %s type with %s type\n", xf->xform_type,
      xf2->xform_type);
*/
   
   xf3 = calloc(1,sizeof(ATLAS_XFORM));
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


   if(wami_verb() > 1)
      INFO_message("Multiplying %s type with %s type\n", xf->xform_type,
      xf2->xform_type);
   if(strcmp(xf->xform_type,"Affine")==0){
      if(strcmp(xf2->xform_type,"Affine")==0){
         cc = affine_mult(xf,xf2,xf3);
         if(wami_verb() > 1)
            INFO_message("combining two affine matrices\n");
         if(cc!=0) {
             if(wami_verb() > 1)
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

   if(wami_verb())
      INFO_message("AFNI doesn't know how to combine these transforms\n"
           "Using the transformations sequentially");
      
   return(NULL);
}

/* invert transformation - general form */
int
invert_xform(ATLAS_XFORM *xf)
{
   int cc =1;

   if(xf->inverse==0) return(0);

   xf->inverse = 0;

   if(strcmp(xf->xform_type,"Identity")==0){
      return(0); /*  inverse of identity is identity */
   }

   if(strcmp(xf->xform_type,"Affine")==0){
      cc = invert_affine(xf);
   }

   /* for 12-piece, this just flips inverse setting 0->1*/
   if(strcmp(xf->xform_type,"12-piece")==0){
      cc = invert_12piece(xf); 
   }
   if(strcmp(xf->xform_type,"2-piece")==0){
      cc = invert_2piece(xf);
   }
   if(strcmp(xf->xform_type,"brett_mni2tt")==0){
      cc = invert_brett(xf);
   }


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

   if ( !xf || !xf->xform) RETURN(1);
   
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

   RETURN(0);
}

/* invert a 12 piece matrix - do in place */
/* don't actually invert, just mark for backward vector use */
/* return error code - 0 for no error */
int
invert_12piece(ATLAS_XFORM *xf)
{
   xf->inverse = !(xf->inverse);
  
   return(0);
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

/* invert a brett transform - do in place */
/* return error code - 0 for no error */
int
invert_brett(ATLAS_XFORM *xf)
{
   xf->inverse = !(xf->inverse);
   
   return(0);
}

/* multiply affine transformations */
int
affine_mult(ATLAS_XFORM *xf, ATLAS_XFORM *xf2, ATLAS_XFORM *xf3)
{
   int cc, i, j;
   matrix sm1, sm2, sm3, sm4;
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

   /* fill in last line with 0's and 1's to make invertible 4x4 */
   sm1.elts[3][0] =  sm1.elts[3][1] = sm1.elts[3][2] = 0.0;
   sm1.elts[3][3] = 1.0;
   sm2.elts[3][0] =  sm2.elts[3][1] = sm2.elts[3][2] = 0.0;
   sm2.elts[3][3] = 1.0;

   /* if xforms are different coordinate order (RAI/LPI)
      apply -1,-1,1 diagonal matrix to first matrix */
   /* may want to expand for all other coord orders */
   if(strcmp(xf->coord_order, xf2->coord_order)!=0) {
      matrix_initialize(&sm4);
      matrix_identity(4,&sm4);
      sm4.elts[0][0] = -1.0; 
      sm4.elts[1][1] = -1.0; 
      matrix_multiply(sm1, sm4, &sm3);
      matrix_multiply(sm4, sm3, &sm1);
      matrix_destroy(&sm4);
      if(xf3->coord_order) free(xf3->coord_order);
      xf3->coord_order = nifti_strdup(xf2->coord_order);
   }

   /* multiply destination direction first, then source s3->s2 x s2->s1 */
   /* mod - drg 02/27/20011  - was backwards, sm1*sm2 - yikes! */
   matrix_multiply(sm2, sm1, &sm3);
   
   xfptr = (float *) xf3->xform;
   for(i=0;i<3;i++)
     for(j=0;j<4;j++) {
        *xfptr++ = (float) sm3.elts[i][j];
     }      

   matrix_destroy(&sm1);
   matrix_destroy(&sm2);
   matrix_destroy(&sm3);
   
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
apply_xform_general(ATLAS_XFORM *xf, float x,float y,float z, 
                        float *xout, float *yout, float *zout)
{
   int xgc = 1;

   invert_xform(xf);   /* possibly need to invert transform */

   if(strcmp(xf->xform_type,"Affine")==0){
      xgc = apply_xform_affine(xf, x, y, z, xout, yout, zout);
   }
   if(strcmp(xf->xform_type,"2-piece")==0){
      xgc = apply_xform_2piece(xf, x, y, z, xout, yout, zout);
   }
   
   if(strcmp(xf->xform_type,"brett_tt2mni")==0){
      if(!xf->inverse)
         xgc = apply_xform_brett_tt2mni(x, y, z, xout, yout, zout);
      else
         xgc = apply_xform_brett_mni2tt(x, y, z, xout, yout, zout);
   }

   if(strcmp(xf->xform_type,"brett_mni2tt")==0){
      if(xf->inverse)
         xgc = apply_xform_brett_tt2mni(x, y, z, xout, yout, zout);
      else
         xgc = apply_xform_brett_mni2tt(x, y, z, xout, yout, zout);
   }
  
   if(strcmp(xf->xform_type,"12-piece")==0){
      xgc = apply_xform_12piece(xf, x, y, z, xout, yout, zout);
   }
   if(strcmp(xf->xform_type,"Identity")==0){
      *xout = x; *yout = y; *zout = z; xgc = 0;
   }

   if(wami_verb() > 2)
      INFO_message("xform RAI out x y z %f %f %f", *xout,*yout,*zout);

   return(xgc);
}


/* apply xform chain to xyz */
int
apply_xform_chain(ATLAS_XFORM_LIST *xfl, float x, float y, float z,
                  float *xout, float *yout, float *zout)
{

   int i, nxf, xgc;
   float xxout=0.0, yyout=0.0, zzout=0.0;
   ATLAS_XFORM * xf;
   
   *xout = xxout;
   *yout = yyout;
   *zout = zzout;
   if(!xfl || !xfl->xform) {
      return(0);
   }
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

   /* should expand coord_order to other orientations */
   if(strcmp(xf->coord_order,"lpi") == 0){
      x = -x; y =-y;      
   }


   xfptr = (float *) xf->xform;
   
   xfptr = (float *) xf->xform;
   
   *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);

   if(strcmp(xf->coord_order,"lpi") == 0){
      *xout = -(*xout); *yout= -(*yout);      
   }

   
    return(0);
}

/* apply the forward 2-piece transformation  to the xyz coordinate
  2-piece transformations apply one of two affine transformations
  depending on some dividing plane defined by x,y or z equal to 
  a particular value. This dividing plane  can be defined on the data
  before the transformation or it may be defined 'post' transformation.
  If a post transformation, the second xform applies an alternate
  xform to the coordinate transformed by the first affine xform.
  This diagram shows the various possibilities:

  pre-xform (post=0)
     any x,y,z < lx,ly,lz, apply 2nd xform
     else apply 1st xform
  post-xform (post=1)
     apply 1st xform to get x',y',z'
     any x',y',z' < lx,ly,lz, apply 2nd xform to x',y',z'

*/
int
apply_xform_2piece(ATLAS_XFORM *xf, float x, float y, float z, 
                                    float *xout, float *yout, float *zout)
{
   float *xfptr;
   float lx,ly,lz;
   int apply_2nd;
   
   /* brett transform - tta to mni
   1.0101  0        0         0
   0       1.02962 -0.05154   0
   0       0.05434  1.08554   0

   1 0 0 0
   0 1 0 0
   0 0 1.09523 0
   */
   if(!xf || xf->xform==NULL) return(1);

   xfptr = xf->xform;
   /* change input coords to lpi if xform defined that way (RAI input)*/
   if(strcmp(xf->coord_order,"lpi") == 0){
      x = -x; y =-y;      
   }

   /* x,y,z limits are the first three elements of the xform data */
   lx = *xfptr++; ly = *xfptr++; lz = *xfptr++;
   /* If this is a pre transformation (post=0), check the x,y,z limits. 
      If any xyz > limit, use the second transformation matrix */
   if(!xf->post){
      apply_2nd = 0;
      if(lx > -9998) {
         if(x<lx) apply_2nd = 1;
      }
      if(ly > -9998) {
         if(y<ly) apply_2nd = 1;
      }
      if(lz > -9998) {
         if(z<lz) apply_2nd = 1;
      }
      if(apply_2nd)
            xfptr += 12;
   }

   *xout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *yout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);
   xfptr += 4;
   *zout = (*xfptr * x) + (*(xfptr+1) * y) + (*(xfptr+2) * z) + *(xfptr+3);   

   if(xf->post){
      apply_2nd = 0;
      if(lx > -9998) {
         if(x<lx) apply_2nd = 1;
       }
      if(ly > -9998) {
         if(y<ly) apply_2nd = 1;
      }
      if(lz > -9998) {
         if(z<lz) apply_2nd = 1;
      }
      if(apply_2nd) {
         x = *xout; y = *yout; z = *zout;  /* if not using a concatenated matrix*/
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
   THD_talairach_12_warp *ww;
   int iw, ioff;
   THD_fvec3 tv, mv;
   float tx, ty, tz;
   char *wptr;

   if(xf->xform==NULL) return(1);
   LOAD_FVEC3( mv , x,y,z ) ;   

   /* load the transform */
   ww = myRwcNew( THD_talairach_12_warp ) ;
   ww->type = WARP_TALAIRACH_12_TYPE;
   ww->resam_type = 0;

   for (iw=0; iw < 12; ++iw) {
      /* 12 piece tranformations stored in 12x30 float blocks */
      /* each 30 float block is first 3x3 forward, 3x3 backward, 
         3 vector-forward, 3 vector-backward,
         bottom xyz corner, top xyz corner */
#if 0
THD_fvec3 *fvptr;
      fptr = (float *) xf->xform+iw*MAPPING_LINEAR_FSIZE;
      memcpy(&ww->warp[iw].mfor, fptr, 9*sizeof(float)); fptr += 9;
      memcpy(&ww->warp[iw].mbac, fptr, 9*sizeof(float)); fptr += 9;
      memcpy(&ww->warp[iw].bvec, fptr, 3*sizeof(float)); fptr += 3;
      memcpy(&ww->warp[iw].svec, fptr, 3*sizeof(float)); fptr += 3;
      memcpy(&ww->warp[iw].bot,  fptr, 3*sizeof(float)); fptr += 3;
      memcpy(&ww->warp[iw].top,  fptr, 3*sizeof(float));
#endif

      ww->warp[iw].type = MAPPING_LINEAR_TYPE ;

      ioff = iw * MAPPING_LINEAR_FSIZE ;   /* increments by 12=4x3 */
      wptr = (char *) xf->xform + iw*MAPPING_LINEAR_FSIZE*sizeof(float);
      COPY_INTO_STRUCT( ww->warp[iw] ,
                        MAPPING_LINEAR_FSTART ,
                        float ,
                        wptr,
                        MAPPING_LINEAR_FSIZE ) ;


   }

   if (!ww) {
      ERROR_message("Failed to form built-in warp.");
      return(1);
   } else {
/*THD_fvec3 AFNI_forward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )*/
      if (! xf->inverse )
         tv = AFNI_forward_warp_vector((THD_warp *)ww, mv);
      else
         tv = AFNI_backward_warp_vector((THD_warp *)ww, mv);
   }

   UNLOAD_FVEC3(tv, tx, ty, tz);
   *xout = tx; *yout = ty; *zout =tz;

   free(ww);

   return(0);
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
   THD_3mni_to_3tta(&x, &y, &z);  /* xform mni to tt space - results in RAI */

   *xout = x; *yout = y; *zout = z;
   return(0);
}


/* read xform from NIML attributes into xform_struct */
int atlas_read_xform(NI_element *nel, ATLAS_XFORM *atlas_xf)
{
   float dist;
   int i;
   char *sptr;

   if(wami_verb() > 2) {
      INFO_message("xform_name %s", NI_get_attribute(nel, "xform_name"));
      INFO_message("xform_type %s", NI_get_attribute(nel, "xform_type"));
      INFO_message("xform source %s", NI_get_attribute(nel, "source"));
      INFO_message("xform dest   %s", NI_get_attribute(nel, "dest"));
      INFO_message("xform number of elements %d", nel->vec_num);
      INFO_message("xform post %s", NI_get_attribute(nel, "post")); 
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

   sptr = NI_get_attribute(nel, "post");
   if(sptr){
       atlas_xf->post = atoi(sptr);
   }
   else
       atlas_xf->post = 0; /*assume pre-xform (used for 2 and 12 part xforms */

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
   
   atlas_xf->xform = calloc(nel->vec_num, sizeof(float));
   if(atlas_xf->xform == NULL) {
      WARNING_message("Could not allocate transformation");
      return(1);
   }

   /* ZSS To Daniel: There are cases where nel->vec is NULL but vec_num == 1
                     I don't think this should happen. That is what was 
                     causing the crash. Now I check for it. 
                     Under certain situations, I can generate plenty of
                     those instances. For example:
      cd /Users/ziad/SUMA_Class_New/data/suma_demo/afni
      3dROIstats -nzmean -nomeanout \
                 -mask lh.OccROIs.FULL.niml.dset v2s.lh.TS.FULL.niml.dset
   */
  if (wami_verb() && nel->vec_num && !nel->vec) {
   WARNING_message("Strange xform nel: Have vec_num=%d but NULL nel->vec",
                   nel->vec_num);
  }
  for(i=0;i<nel->vec_num && nel->vec;i++){
     /* ZSS to Daniel: This memcpy is suspect.
            You are copying only the first value
            from each vector vec[i]. Assuming you
            allocate for vec_len*vec_num you can do:
            memcpy((char *)(atlas_xf->xform)+(i*sizeof(float)*vec_len),
                   nel->vec[i], sizeof(float)*vec_len); 
            */
     memcpy((char *)(atlas_xf->xform)+i*sizeof(float), 
            nel->vec[i], sizeof(float));
  }
  if(wami_verb() > 2)
      print_xform(atlas_xf);
   return(0);
}

/* read template info from NIML attributes into template structure */
int atlas_read_template(NI_element *nel, ATLAS_TEMPLATE *atlas_tpl)
{
   char *s;

   if(wami_verb() > 2) {
      INFO_message("template_name %s", NI_get_attribute(nel, "template_name"));
      INFO_message("templ_space %s", NI_get_attribute(nel, "template_space"));
   }
   atlas_tpl->comment = NULL;
   atlas_tpl->description = NULL;

   atlas_tpl->template = 
      nifti_strdup(NI_get_attribute(nel, "template_name")); 
   atlas_tpl->space = 
      nifti_strdup(NI_get_attribute(nel, "template_space"));

   if ((s=NI_get_attribute(nel,"comment"))) 
      atlas_tpl->comment = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"description"))) 
      atlas_tpl->description = nifti_strdup(s);

   if((atlas_tpl->template == NULL) || (atlas_tpl->space == NULL)) {
      WARNING_message("Could not get template strings");
      return(1);
   }

   return(0);
}

/* read atlas info from NIML attributes into atlas structure */
int atlas_read_atlas(NI_element *nel, ATLAS *atlas, char *parentdir)
{
   char *s;
   
   if (ATL_DSET(atlas) || atlas->adh || ATL_NAME(atlas)) {
      ERROR_message("Unclean atlas structure.");
      return(1);
   }
   if(wami_verb() > 2) {
      INFO_message("atlas_name %s", NI_get_attribute(nel, "atlas_name"));
      INFO_message("atlas_space %s", NI_get_attribute(nel, "template_space"));
   }

   /* initialize the atlas fields */
   atlas->name = NULL;
   atlas->dset_name = NULL;
   atlas->comment = NULL;
   atlas->description = NULL;
   atlas->orient = NULL; /* coordinate order for web queries (Elsevier) */
   atlas->atlas_type =  NULL;  /* atlas can be "web"/ NULL */
   atlas->supp_web_info =  NULL;  /* supplemental info available at website */
   atlas->supp_web_type =  NULL;  /* atlas info may be .pdf, .html, ...*/
   atlas->supp_conn_info =  NULL;  /* supplemental connection info available at website */
   atlas->supp_conn_type =  NULL;  /* atlas info may be .pdf, .html, ...*/
   atlas->atlas_found = 0; /* flag for dataset available, -1 not found, 
                              1 found, 0 init value */

   if ((s=NI_get_attribute(nel, "dset_name"))) {
      atlas->dset_name = NULL;
      if (!THD_is_prefix_ondisk(s, 0) && 
          parentdir && !THD_filehaspath(s)) {
         char *ss=(char *)calloc(strlen(parentdir)+strlen(s)+2,sizeof(char*));
         sprintf(ss,"%s/%s",parentdir,s);
         if (THD_is_prefix_ondisk(ss, 0)) 
            atlas->dset_name = nifti_strdup(ss); 
         free(ss); ss=NULL;
      } 
      if (!atlas->dset_name) atlas->dset_name = nifti_strdup(s); 
   }
   if ((s=NI_get_attribute(nel, "template_space"))) 
      atlas->space = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"atlas_name"))) 
      atlas->name = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"description"))) 
      atlas->description = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"comment"))) 
      atlas->comment = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"orient"))) 
      atlas->orient = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"type"))) 
      atlas->atlas_type = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"supp_web_info"))) 
      atlas->supp_web_info = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"supp_web_type"))) 
      atlas->supp_web_type = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"supp_conn_info"))) 
      atlas->supp_conn_info = nifti_strdup(s);
   if ((s=NI_get_attribute(nel,"supp_conn_type"))) 
      atlas->supp_conn_type = nifti_strdup(s);

   if((atlas->dset_name == NULL) || (atlas->space == NULL)) {
      WARNING_message("bad atlas nel");
      return(1);
   }

   atlas->adh = NULL;
   
   return(0);
}

/* duplicate atlas info from one atlas structure to another */
/* pointers are copied, no new allocation */
int atlas_dup_atlas(ATLAS *srcatlas, ATLAS *destatlas)
{
   /* initialize the atlas fields */
   destatlas->dset_name = srcatlas->dset_name; 
   destatlas->space = srcatlas->space;
   destatlas->name = srcatlas->name;
   destatlas->description = srcatlas->description;
   destatlas->comment = srcatlas->comment;
   destatlas->atlas_found = srcatlas->atlas_found;
   destatlas->orient = srcatlas->orient;
   destatlas->atlas_type = srcatlas->atlas_type;
   destatlas->supp_web_info = srcatlas->supp_web_info;
   destatlas->supp_web_type = srcatlas->supp_web_type;
   destatlas->supp_conn_info = srcatlas->supp_conn_info;
   destatlas->supp_conn_type = srcatlas->supp_conn_type;

   destatlas->adh = srcatlas->adh;
   
   return(0);
}

/* read template space info from NIML attributes into template space structure */
int atlas_read_atlas_space(NI_element *nel, ATLAS_SPACE *at_space)
{
   if(wami_verb() > 2) {
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
      nel = NI_read_element(ns, 100);
      if(nel) {
         if (wami_verb()>2) fprintf(stderr,"nel name %s\n", nel->name);
         if (nel->type == NI_ELEMENT_TYPE) {
            if(strcmp(name, nel->name) == 0) {
               if (wami_verb()>2) fprintf(stderr, "name matches \n");
               return(nel);
            }
         }      
      }
   }
   return(NULL);
}

/* convert some of the atlas lists that are hard-coded in AFNI to NIML tables */
void AFNI_atlas_list_to_niml()
{
   ATLAS_POINT_LIST *temp_apl;
   
   INFO_message("This is a debugging function. Not for regular use.");
   
/*    if(wami_verb() > 1)
      INFO_message("Converting TTO_list_HARD to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(TTO_list_HARD, TTO_COUNT_HARD);
   adjust_atlas_point_list(temp_apl, "Left", 200);

   if(wami_verb() > 1){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing TTO_list_HARD");
   }
   atlas_list_to_niml(temp_apl, "TT_atlas.niml");
   if(wami_verb() > 1)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);
 */
   if(wami_verb() > 2)
      INFO_message("Converting CA_EZ_list_HARD to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(CA_EZ_list_HARD, CA_EZ_COUNT_HARD);

   if(wami_verb() > 1){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing CA_EZ_list_HARD");
   }
   atlas_list_to_niml(temp_apl, "CA_EZ_atlas.niml");
   if(wami_verb() > 1)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);
/* 
   if(wami_verb() > 1)
      INFO_message("Converting ML_EZ_list_HARD to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(ML_EZ_list_HARD, ML_EZ_COUNT_HARD);

   if(wami_verb() > 1){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing ML_EZ_list_HARD");
   }
   atlas_list_to_niml(temp_apl, "ML_EZ_atlas.niml");
   if(wami_verb() > 1)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);

   if(wami_verb() > 1)
      INFO_message("Converting LR_EZ_list_HARD to atlas_point_list");
   temp_apl = AFNI_atlas_list_to_atlas_point_list(LR_EZ_list_HARD, LR_EZ_COUNT_HARD);

   if(wami_verb() > 1){
      print_atlas_point_list(temp_apl);
      INFO_message("NIMLizing LR_EZ_list_HARD");
   }
   atlas_list_to_niml(temp_apl, "LR_EZ_atlas.niml");
   if(wami_verb() > 1)
      INFO_message("Freeing the atlas_point_list");
   free_atlas_point_list(temp_apl);

 */
}

#define TRIM_STRING(lbuf, ch){\
   int kk; \
   for( kk=strlen(lbuf)-1 ; kk > 0 && lbuf[kk] == ch ; kk-- )    \
      lbuf[kk] = '\0' ;                  /* trim trailing .'s */ \
}
/* this function was used originally to convert an old atlas format
 * that was hard coded here to the newer one used in AFNI header
*  attributes. This is likely not to be used again.
*/
static ATLAS_POINT_LIST * AFNI_atlas_list_to_atlas_point_list(
                                 ATLAS_POINT *afni_at_pts, int npts)
{
  ATLAS_POINT *temp_atp;
  ATLAS_POINT_LIST *apl;
  int i;

  ENTRY("AFNI_atlas_list_to_atlas_point_list");
  apl = (ATLAS_POINT_LIST *) calloc(1,sizeof(ATLAS_POINT_LIST));
  apl->n_points = npts;
  apl->at_point = (ATLAS_POINT *) calloc(npts,sizeof(ATLAS_POINT));
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
     NI_strncpy(temp_atp->sblabel,afni_at_pts[i].sblabel,ATLAS_CMAX);
     TRIM_STRING(temp_atp->sblabel, '.');
     NI_strncpy(temp_atp->longname, "", ATLAS_CMAX);
     if(wami_verb() > 1){
        INFO_message("atlas_point %d %s\n", afni_at_pts[i].tdval, 
                      afni_at_pts[i].name);
        INFO_message("atlas_point %d %s temp\n", temp_atp->tdval, 
                      temp_atp->name);
     }
  }
  RETURN(apl);
}

ATLAS_POINT_LIST * label_table_to_atlas_point_list(Dtable *dtbl)
{
   ATLAS_POINT *temp_atp;
   ATLAS_POINT_LIST *apl;
   int i, nn;
   char **la , **lb;
  
   ENTRY("label_table_to_atlas_point_list");

   nn = listize_Dtable( dtbl , &la , &lb ) ;
   if( nn == 0 || la == NULL || lb == NULL ) RETURN (NULL) ;

   apl = (ATLAS_POINT_LIST *) calloc(1,sizeof(ATLAS_POINT_LIST));
   apl->n_points = nn;
   apl->at_point = (ATLAS_POINT *) calloc(nn,sizeof(ATLAS_POINT));
   for(i=0;i<nn;i++){
     temp_atp = apl->at_point+i;
     temp_atp->tdval = (int)strtol(la[i],NULL,10);
     temp_atp->tdlev = 0;
     temp_atp->okey = (int)strtol(la[i],NULL,10);
     temp_atp->xx = 0.0;
     temp_atp->yy = 0.0;
     temp_atp->zz = 0.0;
     NI_strncpy(temp_atp->name,lb[i],ATLAS_CMAX);
     TRIM_STRING(temp_atp->name, '.');
     NI_strncpy(temp_atp->sblabel,lb[i],ATLAS_CMAX);
     TRIM_STRING(temp_atp->sblabel, '.');
     NI_strncpy(temp_atp->longname,"",ATLAS_CMAX);
     if(wami_verb() > 1){
        INFO_message("Dtable %d %s\n", (int)strtol(la[i],NULL,10), 
                      lb[i]);
        INFO_message("atlas_point %d %s temp\n", temp_atp->tdval, 
                      temp_atp->name);
     }
  }
  RETURN(apl);
}

/* convert atlas list to a NIML table in a text file */
void
atlas_list_to_niml(ATLAS_POINT_LIST *atp, char *atlas_file)
{
   int i;
/*   char *atlas_pt_niml;*/
   char filestr[ATLAS_CMAX];

   ATLAS_POINT *at_pt;
   NI_stream atlas_niml;
   NI_group *ngr;
   NI_element *nel=NULL;

   ENTRY("atlas_list_to_niml");

   if(wami_verb() > 1) 
      INFO_message("opening %s", atlas_file);   
   if (atlas_file == NULL) {
      sprintf(filestr, "stdout:");
   } else {
      sprintf(filestr, "file:%s", atlas_file);
   }
   atlas_niml = NI_stream_open(filestr,"w");
   if(atlas_niml==NULL){
         WARNING_message("Could not open atlas file for NIML output %s");
         EXRETURN;
   }
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "atlas_point_list");
   /* get each segmented region - the "atlas point" into 
      a NIML formatted string */ 
   for(i=0;i<atp->n_points;i++) {
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
static void adjust_atlas_point_list(ATLAS_POINT_LIST *atp, char *match_str, float addval)
{
   /* this function was written for adjusting the TT_daemon values to be unique
      previously left and right regions shared the same value */
   int i;
   ATLAS_POINT *at_pt;

   ENTRY("adjust_atlas_point_list");

   for(i=0;i<atp->n_points;i++) {
      at_pt = atp->at_point+i;
      if((strstr(at_pt->name, match_str))){
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
   char sblabel[ATLAS_CMAX];
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
   if (strcmp(at_pt->sblabel,"")!=0)
     NI_set_attribute(nel, "SB_LABEL", at_pt->sblabel);
   if (strcmp(at_pt->longname,"")!=0)
     NI_set_attribute(nel, "LONGNAME", at_pt->longname);

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
   char *encstr=NULL;
   short okey;

   ENTRY("niml_to_atlas_list");

   if(wami_verb() > 1) 
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

     temp_str = NI_get_attribute(nel, "SB_LABEL");
     if(temp_str==NULL)
        NI_strncpy(at_pt->sblabel,"",ATLAS_CMAX);
     else
        NI_strncpy(at_pt->sblabel,temp_str,ATLAS_CMAX);

     temp_str = NI_get_attribute(nel, "LONGNAME");
     if(temp_str==NULL)
        NI_strncpy(at_pt->longname,"",ATLAS_CMAX);
     else
        NI_strncpy(at_pt->longname,temp_str,ATLAS_CMAX);

     at_pt->tdval = okey;
     at_pt->xx = cog[0];
     at_pt->yy = cog[1];
     at_pt->zz = cog[2];
     if(wami_verb() > 1)
        fprintf(stderr,"Decoded:\n"
                    "STRUCT: %s\n"
                    "OKEY: %d\n"
                    "COG: %.3f %.3f %.3f\n"
                    "SB_LABEL: %s\n"
                    , NI_get_attribute(nel, "STRUCT"),
                    okey, cog[0], cog[1], cog[2],
                    NI_get_attribute(nel, "SB_LABEL"));
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

/* get default jump to space, usually "jump to MNI" for AFNI gui */
char *get_jump_space()
{
   char *spacename;

   if(jumpspace_name)
      return(jumpspace_name);

   spacename = getenv("AFNI_JUMPTO_SPACE") ;   
   if(spacename && (strlen(spacename)>0) && (strlen(spacename)<110)){
      jumpspace_name = nifti_strdup(spacename);
   }
   else
      jumpspace_name = nifti_strdup("MNI");

   return(jumpspace_name);
}

/* get default jump to space, usually "jump to MNI" */
void set_jump_space(char *spacename)
{
   if(spacename && (strlen(spacename)>0) && (strlen(spacename)<110)){
      if(jumpspace_name) free(jumpspace_name);
      jumpspace_name = nifti_strdup(spacename);
   }
}


