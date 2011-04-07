/* thd_atlas.c */
/* functions to access atlas, template, space of datasets */

/* access current space of dataset */

#include "mrilib.h"
#include "thd_atlas.h"
#define SUMA_noFunc
#include "suma_suma.h"

static int debug_niml = 1;

static char *preferred_atlas_name = NULL;

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

#if 0
/* NIML file tests */
void atlas_read_all()
{
   NI_stream space_niml;
   atlas_xform_list *atlas_xfl;
   
   space_niml = NI_stream_open("file:AFNI_atlas_space.niml","r");
   
   /* read atlas transformation list */
   atlas_xfl = read_space_xforms(space_niml);
   
   /* write out the list */
   
}

/* read space transformations from niml file */
atlas_xform_list *read_space_xforms(NI_stream space_niml)
{
   NI_element *nel;
   
   while ((nel = NI_read_element(space_niml, 1))) {
      /* Now access parts of nel */
      fprintf( stderr,"\nAccessing Attributes for element %s:\n", nel->name);
      fprintf( stderr,
               "Name attribute = %s\n",
               NI_get_attribute(nel, "Name"));
   }
}
#endif

/* read transformation for space 1 to space 2 */

/* write transformation for space 1 to space 2 */


#if 0

x list available spaces from whereami
x interface for returning path from one space to another
x include space names and transforms defined on brainmap.org
display network with SUMA using @DO.examples
x start building transform combinations
build toy working directory - user directory - supplemental monkey, child,
single subject. Regression testing
affine transformation for EPI 
  revisit with optimized transformations from Peter Fox
whereami can provide reference for xforms used
option to take coordinate transformation - piecewise and combined
functions to apply transforms to coordinates or volumes (already called in
whereami)
#endif

