/* thd_atlas.c */
/* functions to access atlas, template, space of datasets */

/* access current space of dataset */

#include "mrilib.h"
#include "thd_atlas.h"

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
atlas_lut *read_atlas_lut(THD_3dim_dataset *dset)
{
   atlas_lut *atlasdset_lut;
   void *lut_atr;
     
   ENTRY("read_atlas_lut");
   
   /* get lookup table from dataset */
   if(lut_atr = THD_find_atr(dset->dblk, "ATLAS_LUT")) {
   
      atlasdset_lut = malloc(sizeof(atlas_lut));
      if(atlasdset_lut==NULL) {
         WARNING_message("Could not allocate memory for Atlas LUT\n");
         RETURN(NULL);
      }

      atlasdset_lut->rgblist = calloc(MAXINT, 3);
/*      memcpy( atlasdset_lut, lut_atr->in , lut_atr->nin ) ;*/
     }
   else
      RETURN(NULL);

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
