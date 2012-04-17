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

/*!
   A less obtuse way to call IsoSurface extraction functions
   See SUMA_IsoSurface.c the types of values that should be passed
   
   if v0 == v1 --> isovalue of v0 extraction
   if v1 > v0 --> isorange of [v0;v1[ is used
   if v1 < v0 --> turn volume to mask (!=0) and set isovalue to 1
*/
SUMA_SurfaceObject *SUMA_THD_IsoSurface(THD_3dim_dataset *in_volu,
                       float v0, float v1,
                       int debug) 
{
   static char FuncName[]={"SUMA_THD_IsoSurface"};
   SUMA_SurfaceObject *SO= NULL;
   THD_3dim_dataset *in_vol=in_volu;
   SUMA_ISO_OPTIONS MaskMode=SUMA_ISO_UNDEFINED;
   SUMA_ISO_XFORMS xform=SUMA_ISO_XFORM_MASK;
   float *fv=NULL;
   int ii=0, n_mask=0;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (v0 == v1) MaskMode = SUMA_ISO_VAL;
   else if (v1 > v0) MaskMode = SUMA_ISO_RANGE;
   else if (v1 < v0) {/* make a mask*/
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

      if (Opt->debug) {
         fprintf(SUMA_STDERR,
                 "%s:\nNxx=%d\tNyy=%d\tNzz=%d\n", FuncName, nxx, nyy, nzz);
      }

      mcp = MarchingCubes(-1, -1, -1);
      set_resolution( mcp, nxx, nyy, nzz ) ;
      init_all(mcp) ;
      if (Opt->debug) fprintf(SUMA_STDERR,"%s:\nSetting data...\n", FuncName);
      cnt = 0;
      for(  k = 0 ; k < mcp->size_z ; k++ ) {
         for(  j = 0 ; j < mcp->size_y ; j++ ) {
            for(  i = 0 ; i < mcp->size_x ; i++ ) {
               SUMA_SET_MC_DATA ( mcp, Opt->mcdatav[cnt], i, j, k); 
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

   
   if (Opt->debug) 
      fprintf(SUMA_STDERR,"%s:\nrunning MarchingCubes...\n", FuncName);
   run(mcp) ;
   clean_temps(mcp) ;

   if (Opt->debug > 1) {
      fprintf(SUMA_STDERR,"%s:\nwriting out NodeList and FaceSetList...\n", 
                         FuncName);
      write1Dmcb(mcp);
   }

   if (Opt->debug) {
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
      if (Opt->debug) {
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
         j = 3*i; /* change from index coordinates to mm DICOM, next three lines are equivalent of SUMA_THD_3dfind_to_3dmm*/
         fv.xyz[0] = DSET_XORG(Opt->in_vol) + mcp->vertices[i].x * DSET_DX(Opt->in_vol);
         fv.xyz[1] = DSET_YORG(Opt->in_vol) + mcp->vertices[i].y * DSET_DY(Opt->in_vol);
         fv.xyz[2] = DSET_ZORG(Opt->in_vol) + mcp->vertices[i].z * DSET_DZ(Opt->in_vol);
         /* change mm to RAI coords */
		   iv = SUMA_THD_3dmm_to_dicomm( Opt->in_vol->daxes->xxorient, Opt->in_vol->daxes->yyorient, Opt->in_vol->daxes->zzorient, fv );
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
   
   if (Opt->debug) {
      fprintf(SUMA_STDERR,"%s:\nCleaning mcp...\n", FuncName);
   }
   clean_all(mcp) ;
   free(mcp);
   nsoopt=SUMA_FreeNewSOOpt(nsoopt); 

   SUMA_RETURN(SO);
}

/*!
  contains code shamelessly stolen from Rick who stole it from Bob. 
*/
SUMA_Boolean SUMA_Get_isosurface_datasets (
                     SUMA_GENERIC_PROG_OPTIONS_STRUCT * Opt)
{
   static char FuncName[]={"SUMA_Get_isosurface_datasets"};
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
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
   
   
   Opt->mcdatav = (double *)SUMA_malloc(sizeof(double)*Opt->nvox);
   if (!Opt->mcdatav) {
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
	               SUMA_free(Opt->mcdatav); Opt->mcdatav=NULL;
                  SUMA_RETURN(NOPE);
	            } 
	            if ( Opt->ninmask != Opt->nvox ) {
	               SUMA_SL_Err("Input and cmask datasets do not have "
		                        "the same dimensions\n" );
	               SUMA_free(Opt->mcdatav); Opt->mcdatav=NULL;
	               SUMA_RETURN(NOPE);
	            }
	            Opt->ninmask = THD_countmask( Opt->ninmask, bmask );
               SUMA_LHv("Have %d\n", Opt->ninmask);
               for (i=0; i<Opt->nvox; ++i) 
                  if (bmask[i]) Opt->mcdatav[i] = (double)bmask[i]; 
                  else Opt->mcdatav[i] = -1;
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
            Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
            if (!Opt->dvec) {
               SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
               SUMA_RETURN(NOPE);
            }
            EDIT_coerce_scale_type( 
               Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
               DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) , 
               MRI_double               , Opt->dvec  ) ;  
            /* no need for data in input volume anymore */
            PURGE_DSET(Opt->in_vol);

            Opt->ninmask = 0;
            if (Opt->MaskMode == SUMA_ISO_VAL) {
               for (i=0; i<Opt->nvox; ++i) {
                  if (Opt->dvec[i] == Opt->v0) { 
                     Opt->mcdatav[i] = 1; ++ Opt->ninmask; 
                  }  else Opt->mcdatav[i] = -1;
               }
            } else if (Opt->MaskMode == SUMA_ISO_RANGE) {
               for (i=0; i<Opt->nvox; ++i) {
                  if (Opt->dvec[i] >= Opt->v0 && Opt->dvec[i] < Opt->v1) { 
                     Opt->mcdatav[i] = 1; ++ Opt->ninmask; 
                  }  else Opt->mcdatav[i] = -1;
               }
            } else {
               SUMA_SL_Err("Bad Miracle.");
               SUMA_RETURN(NOPE);
            }
            SUMA_free(Opt->dvec); Opt->dvec = NULL; 
                  /* this vector is not even created in SUMA_ISO_CMASK mode ...*/
            break;
         default:
            SUMA_SL_Err("Unexpected value of MaskMode");
            SUMA_RETURN(NOPE);
            break;   
      }
   } else if (Opt->xform == SUMA_ISO_XFORM_SHIFT) {
      /* load the dset */
      DSET_load(Opt->in_vol);
      Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
      if (!Opt->dvec) {
         SUMA_SL_Crit("Failed to allocate for dvec.\nOh misery.");
         SUMA_RETURN(NOPE);
      }
      EDIT_coerce_scale_type( 
         Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
         DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      
         MRI_double               , Opt->dvec  ) ;   
      /* no need for data in input volume anymore */
      PURGE_DSET(Opt->in_vol);
      Opt->ninmask = Opt->nvox;
      for (i=0; i<Opt->nvox; ++i) { 
         Opt->mcdatav[i] = Opt->dvec[i] - Opt->v0;
      }
      SUMA_free(Opt->dvec); Opt->dvec = NULL; 
   } else if (Opt->xform == SUMA_ISO_XFORM_NONE) {
      /* load the dset */
      DSET_load(Opt->in_vol);
      Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
      if (!Opt->dvec) {
         SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
         SUMA_RETURN(NOPE);
      }
      EDIT_coerce_scale_type( 
         Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
         DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) , 
         MRI_double               , Opt->dvec  ) ;   
      /* no need for data in input volume anymore */
      PURGE_DSET(Opt->in_vol);
      Opt->ninmask = Opt->nvox;
      for (i=0; i<Opt->nvox; ++i) { 
         Opt->mcdatav[i] = Opt->dvec[i];
      }
      SUMA_free(Opt->dvec); Opt->dvec = NULL; 
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
   
   if (Opt->debug > 0) {
      fprintf( SUMA_STDERR, 
               "%s:\nInput dset %s has nvox = %d, nvals = %d",
		         FuncName, SUMA_CHECK_NULL_STR(Opt->in_name), 
               Opt->nvox, DSET_NVALS(Opt->in_vol) );
	   fprintf( SUMA_STDERR, " (%d voxels in mask)\n", Opt->ninmask );
   }
   
   SUMA_RETURN(YUP);
}

