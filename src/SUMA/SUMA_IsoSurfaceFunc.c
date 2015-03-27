/*! File to contain routines for creating isosurfaces.

NOTE: MarchingCube code was translated from Thomas Lewiner's C++
implementation of the paper:
Efficient Implementation of Marching Cubes´ Cases with Topological Guarantees
by Thomas Lewiner, Hélio Lopes, Antônio Wilson Vieira and Geovan Tavares 
in Journal of Graphics Tools. 
http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf
*/

#include "SUMA_suma.h"
#include "MarchingCubes/MarchingCubes.h"

/*! a macro version of MarchingCubes' set_data */
#define SUMA_SET_MC_DATA(mcb, val, i, j, k) {  \
   mcb->data[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] = val; \
}

/* 
   Decode the label of surface objects given by SUMA_THD_ROI_IsoSurfaces()
   Need to free returned label string 
*/
char *SUMA_Decode_ROI_IsoSurfacesLabels(char *SOlabel, int *key)
{
   static char FuncName[]={"SUMA_Decode_ROI_IsoSurfacesLabels"};
   char *labcp=NULL, *ss=NULL, *label=NULL;
   int k=0;
   
   SUMA_ENTRY;
   
   if (key) *key = -1;
   if (!SOlabel) SUMA_RETURN(labcp);
   labcp = SUMA_copy_string(SOlabel);
   label = NULL;
   if ((ss = strstr(labcp,"label::"))) {
      label = ss+strlen("label::");
      *ss='\0';
   }
   if ((ss = strstr(labcp,"key::"))) {
      *key = atoi(labcp+strlen("key::"));
   }
   if (label && label[0]!='\0') {
      do {
         labcp[k++] = *label; ++label;
      } while (*label != '\0');
      labcp[k++] = '\0';
   } else {
      SUMA_ifree(labcp);
   }           
   
   SUMA_RETURN(labcp); 
}

/*! 
   Return IsoSurfaces for each unique value in the dataset separately
   
   \param in_volu (THD_3dim_dataset *): The dataset in question
   \param isb (int): Sub-brick to process
   \param valmask (int *): If not NULL, then only consider values in this
                           array of integer keys. If NULL then use all 
                           the unique values in in_volu. See also cmask.
   \param N_valmask (int *): At input, provides the number of values in valmask.
                             Obviously, for this to be used valmask should
                             not be NULL.
                             At output, contains the number of unique values
                             for which surfaces were generated.
   \param cmask (byte *): This mask is only used if the function needs to 
                          determine the set of unique integers in in_volu
   \param debug (int): You know what.
   \return SOV (SUMA_SurfaceObject **): An array of surface object pointers.
                                       The array is always null terminated
                                       so you don't need a counter to get
                                       to its bottom. 
             Note that the label of each SOV[i] is set to help you track
             the label and key for each surface, to the extent that labels
             are available in in_volu. For instance, a label of :
               "key::276label::Left  Thalamus" means this is the surface
             for voxel key 276 and corresponding label: "Left  Thalamus".
   
         Note: Unless explicitly included in valmask, 0 is not considered
         as a valid ROI key.
*/
SUMA_SurfaceObject **SUMA_THD_ROI_IsoSurfaces(THD_3dim_dataset *in_volu, int isb,
                                              int *valmask, int *N_valmask,
                                              byte *cmask, byte cropit, 
                                              int debug)
{
   static char FuncName[]={"SUMA_THD_ROI_IsoSurfaces"};
   SUMA_SurfaceObject **SOV= NULL;
   char label[128]={""}, stmp[32]={""};
   int i, k, N_SO, nproc=0, n_unq, *unq=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (debug > 1) LocalHead = YUP;
   
   if (!in_volu) {
      SUMA_S_Err("No input volume");
      if (N_valmask) *N_valmask = -1;
      SUMA_RETURN(SOV);
   }
   
   if (valmask && !N_valmask) {
      SUMA_S_Err("Have valmask but not N_valmask");
      SUMA_RETURN(SOV);
   }
   
   if (valmask && *N_valmask < 1) {
      SUMA_S_Err("Nothing to do");
      SUMA_RETURN(SOV);
   }
   
   unq = THD_unique_vals( in_volu , isb , & n_unq , cmask);
   if (!unq) {
      SUMA_S_Err("Failed to get set of unique values");
      if (N_valmask) *N_valmask = -1;
      SUMA_RETURN(SOV);
   }  
   
   if (valmask) { /* only preserve values in valmask */
      for (i=0; i<n_unq; ++i) {
         for (k=0; k<*N_valmask; ++k) {
            if (unq[i] == valmask[k]) {
               unq[nproc] = unq[i]; ++nproc;
               break;
            }
         }   
      }
      n_unq = nproc;
   } else { /* get rid of 0 */
      for (i=0; i<n_unq; ++i) {
         if (unq[i] != 0) {
            unq[nproc] = unq[i]; ++nproc;
         }
      }
      n_unq = nproc;
   }
   
   if (!n_unq) {
      SUMA_S_Warn("Nothing to process");
      if (N_valmask) *N_valmask = 0;
      SUMA_RETURN(SOV);
   }
   if (LocalHead) {
      SUMA_LH("Will process the following values:");
      for (i=0; i<n_unq; ++i) {
         fprintf(SUMA_STDERR,"%d ", unq[i]);
      }
      fprintf(SUMA_STDERR,"\n");
   }
   
   /* Allocate for one more pointer so we don't need to rely on the 
      integer counter */
   SOV = (SUMA_SurfaceObject **)SUMA_calloc(n_unq+1, 
                                            sizeof(SUMA_SurfaceObject *));
   for (nproc = 0, i=0; i<n_unq; ++i) {
      if (debug || LocalHead) 
         SUMA_S_Note("Getting IsoSurface %d/%d for key %d", 
                      i+1, n_unq, unq[i]);
      SOV[nproc] = SUMA_THD_IsoSurface(in_volu, 
                                       (float)unq[i], (float)unq[i], 
                                       cropit, debug);
      if (!SOV[nproc]) {
         SUMA_LH("No SO for %d\n", unq[i]);
      } else {
         /* Set label to "key::KEYlabel::LABEL" and stick to this order.
            or make sure you update places in the code that parse it. */
         AFNI_get_dset_val_label(in_volu, (double)unq[i], label);
         sprintf(stmp,"key::%d",unq[i]);
         SOV[nproc]->Label = SUMA_append_replace_string(stmp,label,"label::",0); 
         ++nproc;
      }
   }
   
   if (N_valmask) *N_valmask = nproc;
   
   SUMA_RETURN(SOV);
}

/*!
   A less obtuse way to call IsoSurface extraction functions
   See SUMA_IsoSurface.c the types of values that should be passed
   
   if v0 == v1 --> isovalue of v0 extraction
   if v1 > v0 --> isorange of [v0;v1[ is used
   if v1 < v0 --> turn volume to mask (!=0) and set isovalue to 1
*/
SUMA_SurfaceObject *SUMA_THD_IsoSurface(THD_3dim_dataset *in_volu,
                       float v0, float v1, byte cropit,
                       int debug) 
{
   static char FuncName[]={"SUMA_THD_IsoSurface"};
   SUMA_SurfaceObject *SO= NULL;
   THD_3dim_dataset *in_vol=in_volu, *crop=NULL;
   SUMA_ISO_OPTIONS MaskMode=SUMA_ISO_UNDEFINED;
   SUMA_ISO_XFORMS xform=SUMA_ISO_XFORM_MASK;
   float *fv=NULL;
   int ii=0, n_mask=0, a, b, c, d, e, f;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (cropit) { /* Straight up, no messin' around */
      MRI_autobbox_clust(0);
      THD_autobbox_clip(0);
      THD_autobbox_npad(1);
   }
   
   if (v0 == v1) {
      MaskMode = SUMA_ISO_VAL;
      if (cropit) {
         MaskMode = SUMA_ISO_VAL;
         in_vol = EDIT_full_copy(in_volu,FuncName);
         EDIT_floatize_dataset(in_vol);
         fv = (float *)DSET_BRICK_ARRAY(in_vol,0);
         n_mask=0;
         for (ii=0; ii<DSET_NVOX(in_vol); ++ii) { 
            if (fv[ii] == v0) {
               fv[ii] = 1.0f;
               ++n_mask;
            } else {
               fv[ii] = 0.0f;
            }
         }
         v0 = 1.0; v1 = 0.0;
         if (!(crop = THD_autobbox(in_vol, &a, &b, &c, &d, &e, &f, FuncName))) {
            SUMA_S_Err("Failed to crop");
            SUMA_RETURN(NULL);
         }
         DSET_delete(in_vol); in_vol = crop; crop = NULL;
      }
   } else if (v1 > v0) {
      MaskMode = SUMA_ISO_RANGE;
      if (cropit) {
         MaskMode = SUMA_ISO_VAL;
         in_vol = EDIT_full_copy(in_volu,FuncName);
         EDIT_floatize_dataset(in_vol);
         fv = (float *)DSET_BRICK_ARRAY(in_vol,0);
         n_mask=0;
         for (ii=0; ii<DSET_NVOX(in_vol); ++ii) { 
            if (fv[ii] >= v0 || fv[ii] <= v1) {
               fv[ii] = 1.0f;
               ++n_mask;
            } else {
               fv[ii] = 0.0f;
            }
         }
         v0 = 1.0; v1 = 0.0;
         if (!(crop = THD_autobbox(in_vol, &a, &b, &c, &d, &e, &f, FuncName))) {
            SUMA_S_Err("Failed to crop");
            SUMA_RETURN(NULL);
         }
         DSET_delete(in_vol); in_vol = crop; crop = NULL;
      }
   } else if (v1 < v0) {/* make a mask*/
      MaskMode = SUMA_ISO_VAL;
      in_vol = EDIT_full_copy(in_volu,FuncName);
      EDIT_floatize_dataset(in_vol);
      fv = (float *)DSET_BRICK_ARRAY(in_vol,0);
      n_mask=0;
      for (ii=0; ii<DSET_NVOX(in_vol); ++ii) { 
         if (fv[ii]) {
            fv[ii] = 1.0f;
            ++n_mask;
         } else {
            fv[ii] = 0.0f;
         }
      }
      v0 = 1.0; v1 = 0.0;
      if (cropit) {
         if (!(crop = THD_autobbox(in_vol, &a, &b, &c, &d, &e, &f, FuncName))) {
            SUMA_S_Err("Failed to crop");
            SUMA_RETURN(NULL);
         }
         DSET_delete(in_vol); in_vol = crop; crop = NULL;
      }
   } else {
      SUMA_S_Err("Could not determine maskmode");
      SUMA_RETURN(SO);
   }
   
   Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)
               SUMA_calloc(1,sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));
   Opt->in_vol = in_vol;
   Opt->xform = xform;
   Opt->MaskMode = MaskMode;
   Opt->debug = debug;
   Opt->v0 = v0;
   Opt->v1 = v1;
   Opt->obj_type = -1;
   
   /* A la SUMA_IsoSurface.c */
   if (!SUMA_Get_isosurface_datasets (Opt)) {
      SUMA_SL_Err("Failed to get data.");
      SUMA_RETURN(SO);
   }
   /* Now call Marching Cube functions */
   if (!(SO = SUMA_MarchingCubesSurface(Opt))) {
      SUMA_S_Err("Failed to create surface.\n");
      SUMA_RETURN(SO);
   }
   
   Opt->in_vol = NULL; /* Not yours to kill */
   Opt->cmask = NULL;
   Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (in_vol && in_vol != in_volu) DSET_delete(in_vol);
   SUMA_RETURN(SO);
}
                        

/*!
   A function version of the program mc by Thomas Lewiner
   see main.c in ./MarchingCubes
*/
SUMA_SurfaceObject *SUMA_MarchingCubesSurface(
                        SUMA_GENERIC_PROG_OPTIONS_STRUCT * Opt)
{
   static char FuncName[]={"SUMA_MarchingCubesSurface"};
   SUMA_SurfaceObject *SO=NULL;
   int nxx, nyy, nzz, cnt, i, j, k, *FaceSetList=NULL;
   float *NodeList=NULL;
   SUMA_NEW_SO_OPT *nsoopt = NULL;
   THD_fvec3 fv, iv;
   MCB *mcp ;
   
   SUMA_ENTRY;
   
   if (Opt->obj_type < 0) {
      nxx = DSET_NX(Opt->in_vol);
      nyy = DSET_NY(Opt->in_vol);
      nzz = DSET_NZ(Opt->in_vol);

      if (Opt->debug > 1) {
         SUMA_S_Note("Nxx=%d\tNyy=%d\tNzz=%d\n", nxx, nyy, nzz);
      }

      mcp = MarchingCubes(-1, -1, -1);
      set_resolution( mcp, nxx, nyy, nzz ) ;
      init_all(mcp) ;
      cnt = 0;
      for(  k = 0 ; k < mcp->size_z ; k++ ) {
         for(  j = 0 ; j < mcp->size_y ; j++ ) {
            for(  i = 0 ; i < mcp->size_x ; i++ ) {
               SUMA_SET_MC_DATA ( mcp, Opt->mcfv[cnt], i, j, k); 
               ++cnt;
            }
         }
      }

   } else {
      /* built in */
      nxx = nyy = nzz = Opt->obj_type_res;
      mcp = MarchingCubes(-1, -1, -1);
      set_resolution( mcp, nxx, nyy, nzz) ;
      init_all(mcp) ;
      compute_data( *mcp , Opt->obj_type) ;
   }

   
   if (Opt->debug > 1) {
      SUMA_S_Note("running MarchingCubes...");
   }
   run(mcp) ;
   clean_temps(mcp) ;

   if (Opt->debug > 2) {
      fprintf(SUMA_STDERR,"%s:\nwriting out NodeList and FaceSetList...\n", 
                         FuncName);
      write1Dmcb(mcp);
   }

   if (Opt->debug > 1) {
      fprintf(SUMA_STDERR,"%s:\nNow creating SO...\n", FuncName);
   }

   NodeList = (float *)SUMA_malloc(sizeof(float)*3*mcp->nverts);
   FaceSetList = (int *)SUMA_malloc(sizeof(int)*3*mcp->ntrigs);
   if (!NodeList || !FaceSetList)  {
      SUMA_SL_Crit("Failed to allocate!");
      SUMA_RETURN(SO);
   }
   
   nsoopt = SUMA_NewNewSOOpt();
   if (Opt->obj_type < 0) {
      nsoopt->LargestBoxSize = -1;
      if (Opt->debug > 1) {
         fprintf(SUMA_STDERR,
                  "%s:\nCopying vertices, changing to DICOM \n"
                  "Orig:(%f %f %f) \nD:(%f %f %f)...\n", 
            FuncName, 
            DSET_XORG(Opt->in_vol), 
            DSET_YORG(Opt->in_vol), DSET_ZORG(Opt->in_vol),
            DSET_DX(Opt->in_vol), 
            DSET_DY(Opt->in_vol), DSET_DZ(Opt->in_vol));
      }
      for ( i = 0; i < mcp->nverts; i++ ) {
         j = 3*i; /* change from index coordinates to mm DICOM, 
                   next three lines are equivalent of SUMA_THD_3dfind_to_3dmm*/
         fv.xyz[0] = DSET_XORG(Opt->in_vol) + 
                     mcp->vertices[i].x * DSET_DX(Opt->in_vol);
         fv.xyz[1] = DSET_YORG(Opt->in_vol) + 
                     mcp->vertices[i].y * DSET_DY(Opt->in_vol);
         fv.xyz[2] = DSET_ZORG(Opt->in_vol) + 
                     mcp->vertices[i].z * DSET_DZ(Opt->in_vol);
         /* change mm to RAI coords */
		   iv = SUMA_THD_3dmm_to_dicomm( Opt->in_vol->daxes->xxorient, 
                                       Opt->in_vol->daxes->yyorient, 
                                       Opt->in_vol->daxes->zzorient, fv );
         NodeList[j  ] = iv.xyz[0];
         NodeList[j+1] = iv.xyz[1];
         NodeList[j+2] = iv.xyz[2];
      }
      for ( i = 0; i < mcp->ntrigs; i++ ) {
         j = 3*i;
         FaceSetList[j  ] = mcp->triangles[i].v3;
         FaceSetList[j+1] = mcp->triangles[i].v2;
         FaceSetList[j+2] = mcp->triangles[i].v1;
      }
   } else {
      nsoopt->LargestBoxSize = 100;
      /* built in */
      for ( i = 0; i < mcp->nverts; i++ ) {
         j = 3*i;
         NodeList[j  ] = mcp->vertices[i].x;
         NodeList[j+1] = mcp->vertices[i].y;
         NodeList[j+2] = mcp->vertices[i].z;
      }   
      for ( i = 0; i < mcp->ntrigs; i++ ) {
         j = 3*i;
         FaceSetList[j  ] = mcp->triangles[i].v3;
         FaceSetList[j+1] = mcp->triangles[i].v2;
         FaceSetList[j+2] = mcp->triangles[i].v1;
      }
   }
   

   SO = SUMA_NewSO(&NodeList, mcp->nverts, &FaceSetList, mcp->ntrigs, nsoopt);
   if (Opt->obj_type < 0) {
      /* not sure if anything needs to be done here ...*/
   } else {
      if (Opt->obj_type == 0) SO->normdir = 1;
      else SO->normdir = -1;
   }
   
   if (Opt->debug > 1) {
      fprintf(SUMA_STDERR,"%s:\nCleaning mcp...\n", FuncName);
   }
   clean_all(mcp) ;
   free(mcp);
   nsoopt=SUMA_FreeNewSOOpt(nsoopt); 

   SUMA_RETURN(SO);
}

SUMA_Boolean SUMA_Get_isosurface_datasets (
                     SUMA_GENERIC_PROG_OPTIONS_STRUCT * Opt)
{
   static char FuncName[]={"SUMA_Get_isosurface_datasets"};
   int i;
   float *fvec=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 2) LocalHead = YUP;
   
   if (!Opt->in_vol && !Opt->in_name) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   } 
   if (!Opt->in_vol) { /* open it */
     Opt->in_vol = THD_open_dataset( Opt->in_name );
   }
   if (!Opt->in_vol) {
      SUMA_S_Err("No volume could be had");
      SUMA_RETURN(NOPE);
   } 
   
   if (!ISVALID_DSET(Opt->in_vol)) {
      if (!Opt->in_name) {
         SUMA_SL_Err("NULL input volume.");
         SUMA_RETURN(NOPE);
      } else {
         SUMA_SL_Err("invalid volume.");
         SUMA_RETURN(NOPE);
      }
   } else if ( DSET_BRICK_TYPE(Opt->in_vol, 0) == MRI_complex) {
      SUMA_SL_Err("Can't do complex data.");
      SUMA_RETURN(NOPE);
   }
   
   Opt->nvox = DSET_NVOX( Opt->in_vol );
   if (DSET_NVALS( Opt->in_vol) != 1) {
      SUMA_SL_Err("Input volume can only have one sub-brick in it.\n"
                  "Use [.] selectors to choose sub-brick needed.");
      SUMA_RETURN(NOPE);
   }
   
   if (Opt->MaskMode != SUMA_ISO_ROIS) {
      Opt->mcfv = (float *)SUMA_malloc(sizeof(float)*Opt->nvox);
      if (!Opt->mcfv) {
         SUMA_SL_Crit("Failed to allocate for maskv");
         SUMA_RETURN(NOPE);
      }
      if (Opt->xform == SUMA_ISO_XFORM_MASK) {
         SUMA_LH("SUMA_ISO_XFORM_MASK");
         switch (Opt->MaskMode) {
            case SUMA_ISO_CMASK:
               SUMA_LH("SUMA_ISO_CMASK");
               if (Opt->cmask) {
                  /* here's the second order grand theft */
                  int    clen = strlen( Opt->cmask );
	               char * cmd;
                  byte *bmask;

	               cmd = (char *)malloc((clen + 1) * sizeof(char));
	               strcpy( cmd,  Opt->cmask);

	               bmask = EDT_calcmask( cmd, &Opt->ninmask, 0 );
                  SUMA_LHv("Have %d\n", Opt->ninmask);
	               free( cmd );			   /* free EDT_calcmask() string */

	               if ( bmask == NULL ) {
	                  SUMA_SL_Err("Failed to compute mask from -cmask option");
	                  SUMA_free(Opt->mcfv); Opt->mcfv=NULL;
                     SUMA_RETURN(NOPE);
	               } 
	               if ( Opt->ninmask != Opt->nvox ) {
	                  SUMA_SL_Err("Input and cmask datasets do not have "
		                           "the same dimensions\n" );
	                  SUMA_free(Opt->mcfv); Opt->mcfv=NULL;
	                  SUMA_RETURN(NOPE);
	               }
	               Opt->ninmask = THD_countmask( Opt->ninmask, bmask );
                  SUMA_LHv("Have %d\n", Opt->ninmask);
                  for (i=0; i<Opt->nvox; ++i) 
                     if (bmask[i]) Opt->mcfv[i] = (float)bmask[i]; 
                     else Opt->mcfv[i] = -1;
                  free(bmask);bmask=NULL;
               } else {
                  SUMA_SL_Err("NULL cmask"); SUMA_RETURN(NOPE);
               }
               break;
            case SUMA_ISO_VAL:
            case SUMA_ISO_RANGE:
               SUMA_LH("SUMA_ISO_VAL, SUMA_ISO_RANGE");
               /* load the dset */
               DSET_load(Opt->in_vol);
               fvec = (float *)SUMA_malloc(sizeof(float) * Opt->nvox);
               if (!fvec) {
                  SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
                  SUMA_RETURN(NOPE);
               }
               EDIT_coerce_scale_type( 
                  Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                  DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) , 
                  MRI_float               , fvec  ) ;  
               /* no need for data in input volume anymore */
               PURGE_DSET(Opt->in_vol);

               Opt->ninmask = 0;
               if (Opt->MaskMode == SUMA_ISO_VAL) {
                  for (i=0; i<Opt->nvox; ++i) {
                     if (fvec[i] == Opt->v0) { 
                        Opt->mcfv[i] = 1; ++ Opt->ninmask; 
                     }  else Opt->mcfv[i] = -1;
                  }
               } else if (Opt->MaskMode == SUMA_ISO_RANGE) {
                  for (i=0; i<Opt->nvox; ++i) {
                     if (fvec[i] >= Opt->v0 && fvec[i] < Opt->v1) { 
                        Opt->mcfv[i] = 1; ++ Opt->ninmask; 
                     }  else Opt->mcfv[i] = -1;
                  }
               } else {
                  SUMA_SL_Err("Bad Miracle.");
                  SUMA_RETURN(NOPE);
               }
               SUMA_free(fvec); fvec = NULL; 
                  /* this vector is not created in SUMA_ISO_CMASK mode ...*/
               break;
            default:
               SUMA_SL_Err("Unexpected value of MaskMode");
               SUMA_RETURN(NOPE);
               break;   
         }
      } else if (Opt->xform == SUMA_ISO_XFORM_SHIFT) {
         /* load the dset */
         DSET_load(Opt->in_vol);
         fvec = (float *)SUMA_malloc(sizeof(float) * Opt->nvox);
         if (!fvec) {
            SUMA_SL_Crit("Failed to allocate for dvec.\nOh misery.");
            SUMA_RETURN(NOPE);
         }
         EDIT_coerce_scale_type( 
            Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
            DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      
            MRI_float               , fvec  ) ;   
         /* no need for data in input volume anymore */
         PURGE_DSET(Opt->in_vol);
         Opt->ninmask = Opt->nvox;
         for (i=0; i<Opt->nvox; ++i) { 
            Opt->mcfv[i] = fvec[i] - Opt->v0;
         }
         SUMA_free(fvec); fvec = NULL; 
      } else if (Opt->xform == SUMA_ISO_XFORM_NONE) {
         /* load the dset */
         DSET_load(Opt->in_vol);
         fvec = (float *)SUMA_malloc(sizeof(float) * Opt->nvox);
         if (!fvec) {
            SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
            SUMA_RETURN(NOPE);
         }
         EDIT_coerce_scale_type( 
            Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
            DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) , 
            MRI_float               , fvec  ) ;   
         /* no need for data in input volume anymore */
         PURGE_DSET(Opt->in_vol);
         Opt->ninmask = Opt->nvox;
         for (i=0; i<Opt->nvox; ++i) { 
            Opt->mcfv[i] = fvec[i];
         }
         SUMA_free(fvec); fvec = NULL; 
      } else {
         SUMA_SL_Err("Bad Opt->xform.");
         SUMA_RETURN(NOPE);
      }

      if ( Opt->ninmask  <= 0 ) {
	      if (Opt->ninmask == 0) {
            SUMA_SL_Err("A negative value!\nNothing to do." );
         } else {
            SUMA_SL_Err("An empty mask!\n Nothing to do." );
	      }
         SUMA_RETURN(NOPE);
	   }

      if (Opt->debug > 1) {
         fprintf( SUMA_STDERR, 
                  "%s:\nInput dset %s has nvox = %d, nvals = %d",
		            FuncName, SUMA_CHECK_NULL_STR(Opt->in_name), 
                  Opt->nvox, DSET_NVALS(Opt->in_vol) );
	      fprintf( SUMA_STDERR, " (%d voxels in mask)\n", Opt->ninmask );
      }
   }
   SUMA_RETURN(YUP);
}

