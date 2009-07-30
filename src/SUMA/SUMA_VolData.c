/*! Dealings with volume data */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

const char *SUMA_WarpTypeName(SUMA_WARP_TYPES wt)
{
   switch (wt) {
      case BAD_WARP:
         return ("Bad Warp");
      case NO_WARP:
         return ("No Warp");
      case ROTATE_WARP:
         return ("3drotate Warp");
      case VOLREG_WARP:
         return ("3dvolreg Warp");
      case ALLINEATE_WARP:
         return ("3dAllineate Warp");
      case WARPDRIVE_WARP:
         return ("3dWarpdrive Warp");
      case TAGALIGN_WARP:
         return ("3dTagalign Warp");
      case N_WARP_TYPES:
         return ("Number of Warp Types");
      default:
         return ("You're a warped type");
   }
}

/*!
   \brief Find the closest node on the surface to a voxel in the volume (as defined in vp)
   \param SO (SUMA_SurfaceObject *)
      SO->NodeList is expected to be in dicomm (RAI) units
   \param vp (SUMA_VOLPAR *) Volume grid of Nvox voxels.
   \param closest_node (int *) Vector of Nvox elements. To contain the nodes closest to each voxel
   \param closest_dist (float *) Vector of Nvox elements. 
                                 To contain the distance between node and voxel centroid
                                 Pass NULL if you do not care for this info.
   \param vox_mask (byte *) Mask of voxels to process (NULL if you want to process all)
   \param verb (int): 0 be quiet
                    > 1 talk
   
*/
int SUMA_ClosestNodeToVoxels(SUMA_SurfaceObject *SO, SUMA_VOLPAR *vp, int *closest_node, float *closest_dist, byte *vox_mask, int verb) 
{
   static char FuncName[]={"SUMA_ClosestNodeToVoxels"};
   float *p=NULL;
   float d, dxyz;
   int i, j, k, n, nij, ijk, cnt = 0;
   THD_fvec3     fv , iv;
   
   SUMA_ENTRY;
   
   if (!SO || !vp || !closest_node) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(0);
   }
   
   if (verb) {
      fprintf(SUMA_STDERR,"%s: Have %d nodes in surface\n%dx%dx%d (%d) voxels in volume.\n", 
                  FuncName, SO->N_Node, vp->nx, vp->ny, vp->nz, vp->nx * vp->ny * vp->nz);
   }
   /* Now for each voxel, find the closest node (SLOW implementation) */
   cnt = 0;
   nij = vp->nx*vp->ny;
   for (i=0;i<vp->nx; ++i) {  
      for (j=0;j<vp->ny; ++j) {
         for (k=0;k<vp->nz; ++k) {
            ijk = SUMA_3D_2_1D_index(i,j,k,vp->nx,nij);
            /* fprintf(SUMA_STDERR," %4d %4d %4d,  ijk: %d\n", i, j, k, ijk); */            
            closest_node[ijk] = -1;
            if (closest_dist) closest_dist[ijk] = -1.0;
            if (!vox_mask || (vox_mask && vox_mask[ijk])) {
               iv.xyz[0] = (float)i; iv.xyz[1] = (float)j; iv.xyz[2] = (float)k;    
               fv = SUMA_THD_3dfind_to_3dmm_vp(vp, iv);
               iv = SUMA_THD_3dmm_to_dicomm(vp->xxorient, vp->yyorient, vp->zzorient,  fv);
               #if 0 /* macro not tested here so keeping older code below */
               SUMA_CLOSEST_NODE(SO, iv.xyz, closest_node[ijk], d);
               if (closest_dist) closest_dist[ijk] = (float)d;
               #else
               dxyz = 1023734552736672366372.0;
               for (n=0; n<SO->N_Node; ++n) {
                  p = &(SO->NodeList[SO->NodeDim*n]);
                  SUMA_SEG_LENGTH_SQ(p, iv.xyz, d);
                  if (d < dxyz) {
                     dxyz = d; closest_node[ijk] = n; 
                     if (closest_dist) closest_dist[ijk] = (float)d;
                  }
               }
               #endif
               if (closest_dist) { if (closest_dist[ijk] >= 0.0f) closest_dist[ijk] = (float)sqrt(closest_dist[ijk]); }
               if (verb) {
                  ++cnt;
                  if (!(cnt % 1000)) {
                     fprintf(SUMA_STDERR,". @ %4d %4d %4d   (%3.2f%%)\n", 
                              i, j, k, (float)cnt/(float)(vp->nx * vp->ny * vp->nz)*100.0); fflush(SUMA_STDERR);
                  }
               }
            }
         }
      }
   } 
   
   SUMA_RETURN(1);
}


/*! a copy of THD_handedness from ../thd_rotangles.c
Dunno why the original was giving me pain linking ... */
int SUMA_THD_handedness( THD_3dim_dataset * dset )
{
   static char FuncName[]={"SUMA_THD_handedness"};
   THD_dataxes * dax ;
   THD_mat33 q ;
   int col ;
   float val ;

   SUMA_ENTRY;

   if( !ISVALID_DSET(dset) ) SUMA_RETURN(1) ;

   LOAD_ZERO_MAT(q) ;
   dax = dset->daxes ;

   col = 0 ;
   switch( dax->xxorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   col = 1 ;
   switch( dax->yyorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   col = 2 ;
   switch( dax->zzorient ){
      case 0: q.mat[0][col] =  1.0 ; break ;
      case 1: q.mat[0][col] = -1.0 ; break ;
      case 2: q.mat[1][col] = -1.0 ; break ;
      case 3: q.mat[1][col] =  1.0 ; break ;
      case 4: q.mat[2][col] =  1.0 ; break ;
      case 5: q.mat[2][col] = -1.0 ; break ;
   }

   val = MAT_DET(q) ;
   if( val > 0.0 ) SUMA_RETURN( 1) ;  /* right handed */
   else            SUMA_RETURN(-1) ;  /* left handed */
}

/*!
   see help for SUMA_AfniPrefix below
*/
SUMA_Boolean SUMA_AfniExistsView(int exists, char *view)
{
   static char FuncName[]={"SUMA_AfniExistsView"};
   SUMA_Boolean ans = NOPE;
   
   SUMA_ENTRY;
   
   if (!exists) SUMA_RETURN(ans);
   
   if (strcmp(view,"+orig") == 0) {
      if (exists == 1 || exists == 3 || exists == 5 || exists == 7) ans = YUP; 
   } else if (strcmp(view,"+acpc") == 0) {
      if (exists == 2 || exists == 3 || exists == 6 || exists == 7) ans = YUP; 
   } else if (strcmp(view,"+tlrc") == 0) {
      if (exists == 4 || exists == 5 || exists == 6 || exists == 7) ans = YUP; 
   } 
   
   SUMA_RETURN(ans);

}

SUMA_Boolean SUMA_AfniView (char *nameorig, char *cview)
{
   static char FuncName[]={"SUMA_AfniView"};
   char *tmp1 = NULL, *tmp2 = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nameorig) SUMA_RETURN(NOPE);
   if (!cview) SUMA_RETURN(NOPE);

   tmp1 = SUMA_Extension(nameorig, ".HEAD", YUP);
   tmp2 = SUMA_Extension(tmp1, ".BRIK", YUP); SUMA_free(tmp1); tmp1 = NULL;
   /* is there a dot ?*/
   if (tmp2[strlen(tmp2)-1] == '.') tmp2[strlen(tmp2)-1] = '\0';
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Searching for view of %s\n", FuncName, tmp2);
   
   /* view */
   if (SUMA_isExtension(tmp2, "+orig")) { 
      sprintf(cview, "+orig"); 
   } else if (SUMA_isExtension(tmp2, "+acpc")) { 
      sprintf(cview, "+acpc"); 
   } else if (SUMA_isExtension(tmp2, "+tlrc")) { 
      sprintf(cview, "+tlrc"); 
   } else {
      cview[0]='\0';
   }
   SUMA_free(tmp2); tmp2 = NULL;

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_AfniExists(char *prefix, char *c2view) 
{
   static char FuncName[]={"SUMA_AfniExists"};
   char *head=NULL;
   SUMA_Boolean ans = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ans = NOPE;

   head = SUMA_append_replace_string(prefix,".HEAD", c2view,0);
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Checking existence of %s\n", FuncName, head);
   if (SUMA_filexists(head)) { ans = YUP; }
   SUMA_free(head); head = NULL; 
   
   SUMA_RETURN(ans);
}
/*!
   \brief Return AFNI's prefix, from a file name also checks for its validity
   \param name (char *) dset name (can contain path)
   \param view (char[4]) array to return view in it
   \param path (char *) if any, make sure it is not duplicated in name!
   \param exists (int *) function checks
                        for all three possible views
                        0: No afni dset with name/prefix
                        1: +orig
                        2: +acpc
                           3: +orig and +acpc 
                        4: +tlrc
                           5: +orig and +tlrc
                           6: +acpc and +tlrc
                           7: +orig and +acpc and +tlrc
                        \sa SUMA_AfniExistsView   
                        
   \return prefix (char *) dset prefix, free it with SUMA_free
   \sa SUMA_AfniView
*/
char *SUMA_AfniPrefix(char *nameorig, char *view, char *path, int *exists) 
{
   static char FuncName[]={"SUMA_AfniPrefix"};
   char *tmp1 = NULL, *tmp2 = NULL, *prfx = NULL, *name=NULL;
   char cview[10];
   int iview;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!nameorig) SUMA_RETURN(prfx);
   if (exists) *exists = 0;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Working with %s\n", FuncName, nameorig);
   
   if (!path) name = SUMA_copy_string(nameorig);
   else {
      if (path[strlen(path)-1] == '/') {
         name = SUMA_append_replace_string(path, nameorig, "", 0);
      } else {
         name = SUMA_append_replace_string(path, nameorig, "/", 0);
      }
   }
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: name now %s\n", FuncName, name);
   
   tmp1 = SUMA_Extension(name, ".HEAD", YUP);
   tmp2 = SUMA_Extension(tmp1, ".BRIK", YUP); SUMA_free(tmp1); tmp1 = NULL;
   /* is there a dot ?*/
   if (tmp2[strlen(tmp2)-1] == '.') tmp2[strlen(tmp2)-1] = '\0';
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Searching for view of %s\n", FuncName, tmp2);
   
   /* view */
   iview = -1;
   if (SUMA_isExtension(tmp2, "+orig")) { 
      iview = 0; sprintf(cview, "+orig"); 
      prfx = SUMA_Extension(tmp2, "+orig", YUP);
   } else if (SUMA_isExtension(tmp2, "+acpc")) { 
      iview = 1; sprintf(cview, "+acpc"); 
      prfx = SUMA_Extension(tmp2, "+acpc", YUP);
   } else if (SUMA_isExtension(tmp2, "+tlrc")) { 
      iview = 2; sprintf(cview, "+tlrc"); 
      prfx = SUMA_Extension(tmp2, "+tlrc", YUP);
   } else {
      prfx = SUMA_copy_string(tmp2);
      cview[0]='\0';
   }
   SUMA_free(tmp2); tmp2 = NULL;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Prefix %s\n", FuncName, prfx);
   
   /* can't tell what view is, so can't test properly quite yet */
   {   
      /* is name ok ? all I have to do is test with +orig */
      char *bname=SUMA_append_string(prfx,"+orig");
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Checking name validity using +orig for view %s\n", FuncName, bname);
      if( !THD_filename_ok(bname) ){
         SUMA_SL_Err("not a proper dset name!\n") ;
         SUMA_free(name); name = NULL;
         SUMA_free(prfx); prfx = NULL;
         SUMA_free(bname); bname = NULL;
         SUMA_RETURN(prfx);
      }
   }
   
   if (exists) {
      { /* look for any */
         char *head=NULL, c2view[10];
         iview = 0; *exists = 0;
         while (iview < 3) {
            if (iview == 0) sprintf(c2view, "+orig"); 
            else if (iview == 1) sprintf(c2view, "+acpc"); 
            else if (iview == 2) sprintf(c2view, "+tlrc");
            head = SUMA_append_replace_string(prfx,".HEAD", c2view,0);
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Checking existence of %s\n", FuncName, head);
            if (SUMA_filexists(head)) { *exists += (int)pow(2,iview); }
            SUMA_free(head); head = NULL; 
            ++iview;
         }
      }
      if (LocalHead) fprintf(SUMA_STDERR,"%s: exist number is %d\n", FuncName, *exists);
   }
   
   
   if (cview[0] != '\0') {
      if (LocalHead) {
         if (SUMA_AfniExistsView(*exists, cview)) {
            fprintf(SUMA_STDERR,"%s: dset with view %s does exist.\n", FuncName, cview); 
         } else {
            fprintf(SUMA_STDERR,"%s: dset with view %s does not exist.\n", FuncName, cview); 
         }
      }
   }
   
   if (view) {
      sprintf(view,"%s", cview);
   }
   if (name) SUMA_free(name); name = NULL;
   
   SUMA_RETURN(prfx);
}
                        
/*!
   \brief A function to find the skin of a volume
   \param dset (THD_3dim_dataset *) an AFNI volume
   \param fvec (float *) (nx * ny * nz) data vector
   \param thresh (double) consider only values in fvec > thresh
   \param N_skin (int *) number of voxels that are skin
   \return skin (byte *) (nx * ny * nz) vector containing 1 for skin voxels, 0 elsewhere.
*/
byte * SUMA_isSkin(THD_3dim_dataset *dset, float *fvec, double thresh, int *N_skin)
{
   static char FuncName[]={"SUMA_isSkin"};
   byte *isskin=NULL;
   int x, y, z, nx, ny, nz, i1D,  nxy;
  
   SUMA_ENTRY;
   
   if (!dset || !fvec) {
      SUMA_SL_Err("NULL input dset or fvec");
      SUMA_RETURN(isskin);
   }
   
   nx = DSET_NX(dset);
   ny = DSET_NY(dset);
   nz = DSET_NZ(dset);
   nxy = nx * ny;
   
   isskin = (byte *) SUMA_calloc(nxy * nz, sizeof(byte));
   if (!isskin) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   *N_skin = 0;
   for (z=0; z<nz; ++z) {
      for (y=0; y<ny; ++y) {
         x = 0;
         do { /* the upstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (fvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            ++x; 
         } while (x < nx && !isskin[i1D]);
         x = nx - 1;
         do { /* the downstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (fvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            --x; 
         } while (x >=0 && !isskin[i1D]);
      } /* y */
   } /* z */
   
   for (z=0; z<nz; ++z) {
      for (x=0; x<nx; ++x) {
         y = 0;
         do { /* the upstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (fvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            ++y; 
         } while (y < ny && !isskin[i1D]);
         y = ny - 1;
         do { /* the downstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (fvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            --y; 
         } while (y >=0 && !isskin[i1D]);
      } /* x */
   } /* z */
   
   for (x=0; x<nx; ++x) {
      for (y=0; y<ny; ++y) {
         z = 0;
         do { /* the upstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (fvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            ++z; 
         } while (z < nz && !isskin[i1D]);
         z = nz - 1;
         do { /* the downstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (fvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            --z; 
         } while (z >=0 && !isskin[i1D]);
      } /* y */
   } /* x */
      
   SUMA_RETURN(isskin);
}
/*!
   Returns the attributes of the surface's volume parent 
   A volume parent is defined as:
      The volume data set used to generate the surface. aka the master SPGR
   \ret NULL for troubles
*/
SUMA_VOLPAR *SUMA_Alloc_VolPar (void)
{
   static char FuncName[]={"SUMA_Alloc_VolPar"};
   SUMA_VOLPAR *VP;

   SUMA_ENTRY;

   VP = (SUMA_VOLPAR *)SUMA_malloc(sizeof(SUMA_VOLPAR));
   if (VP == NULL) {
      fprintf(SUMA_STDERR,"Error SUMA_Alloc_VolPar: Failed to allocate for VolPar\n");
      SUMA_RETURN (NULL);
   }
   VP->idcode_str = NULL;
   VP->isanat = 1;
   VP->nx = VP->ny = VP->nz = 0; /*!< number of voxels in the three dimensions */
   VP->dx = VP->dy = VP->dz = 0.0; /*!< delta x, y, z in mm */
   VP->xorg = VP->yorg = VP->zorg = 0.0; /*!< voxel origin in three dimensions */
   VP->prefix = NULL; /*!< parent volume prefix */
   VP->filecode = NULL; /*!< parent volume prefix + view */
   VP->dirname = NULL; /*!< parent volume directory name */
   VP->vol_idcode_str = NULL; /*!< idcode string OF parent volume*/
   VP->vol_idcode_date = NULL; /*!< idcode date */
   VP->xxorient = VP->yyorient = VP->zzorient = 0; /*!< orientation of three dimensions*/ 
   VP->CENTER_OLD = NULL; /*!< pointer to the named attribute (3x1) in the .HEAD file of the experiment-aligned Parent Volume */
   VP->CENTER_BASE = NULL; /*!< pointer to the named attribute (3x1) in the .HEAD file of the experiment-aligned Parent Volume */
   VP->MATVEC = NULL; /*!< pointer to the named attribute (12x1) in the .HEAD file of the experiment-aligned Parent Volume */
   VP->Hand = 1; /*!< Handedness of axis 1 RH, -1 LH*/
   VP->MATVEC_source = NO_WARP;
   SUMA_RETURN(VP);
}
SUMA_Boolean SUMA_Free_VolPar (SUMA_VOLPAR *VP)
{
   static char FuncName[]={"SUMA_Free_VolPar"};
   
   SUMA_ENTRY;

   if (VP->prefix != NULL) SUMA_free(VP->prefix);
   if (VP->idcode_str != NULL) SUMA_free(VP->idcode_str);
   if (VP->filecode != NULL) SUMA_free(VP->filecode);
   if (VP->dirname != NULL) SUMA_free(VP->dirname);
   if (VP->vol_idcode_str != NULL) SUMA_free(VP->vol_idcode_str);
   if (VP->vol_idcode_date != NULL) SUMA_free(VP->vol_idcode_date);
   if (VP->CENTER_OLD != NULL) SUMA_free(VP->CENTER_OLD);
   if (VP->CENTER_BASE != NULL) SUMA_free(VP->CENTER_BASE);
   if (VP->MATVEC != NULL) SUMA_free(VP->MATVEC);
   if (VP != NULL) SUMA_free(VP);
   SUMA_RETURN (YUP);
}


SUMA_VOLPAR *SUMA_VolParFromDset (THD_3dim_dataset *dset)
{
   ATR_float *atr=NULL, *atrkeep=NULL;
   static char FuncName[]={"SUMA_VolParFromDset"};
   SUMA_VOLPAR *VP=NULL;
   int ii, nxform = 0;
   MCW_idcode idcode;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   /* read the header of the parent volume */
   if (dset == NULL) {
      fprintf (SUMA_STDERR,"Error %s:\nNULL dset\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* allocate for VP */
   VP = SUMA_Alloc_VolPar();
   if (VP == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Alloc_VolPar\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* retain pertinent info */
   VP->isanat = ISANAT(dset);
   VP->nx = DSET_NX(dset);
   VP->ny = DSET_NY(dset);
   VP->nz = DSET_NZ(dset);
   VP->dx = DSET_DX(dset);
   VP->dy = DSET_DY(dset);
   VP->dz = DSET_DZ(dset);
   VP->xorg = DSET_XORG(dset);
   VP->yorg = DSET_YORG(dset);
   VP->zorg = DSET_ZORG(dset);
   ii = strlen(DSET_PREFIX(dset));
   VP->prefix = (char *)SUMA_malloc(ii+1);
   ii = strlen(DSET_FILECODE(dset));
   VP->filecode = (char *)SUMA_malloc(ii+1);
   ii = strlen(DSET_DIRNAME(dset));
   VP->dirname = (char *)SUMA_malloc(ii+1);
   ii = strlen(dset->idcode.str);
   VP->vol_idcode_str = (char *)SUMA_malloc(ii+1);
   ii = strlen(dset->idcode.date);
   VP->vol_idcode_date = (char *)SUMA_malloc(ii+1);
   if (  VP->prefix == NULL || 
         VP->filecode == NULL || 
         VP->vol_idcode_date == NULL || 
         VP->dirname == NULL || 
         VP->vol_idcode_str == NULL) {
      fprintf( SUMA_STDERR,
               "Error %s: Failed to allocate for strings. Kill me, please.\n", 
               FuncName);
      SUMA_Free_VolPar(VP);
      SUMA_RETURN (NULL);
   }
   VP->prefix = strcpy(VP->prefix, DSET_PREFIX(dset));
   VP->filecode = strcpy(VP->filecode, DSET_FILECODE(dset));
   VP->dirname = strcpy(VP->dirname, DSET_DIRNAME(dset));
   VP->vol_idcode_str = strcpy(VP->vol_idcode_str, dset->idcode.str);
   VP->vol_idcode_date = strcpy(VP->vol_idcode_date, dset->idcode.date);
   VP->xxorient = dset->daxes->xxorient;
   VP->yyorient = dset->daxes->yyorient;
   VP->zzorient = dset->daxes->zzorient;

   if (LocalHead) {      
      fprintf (SUMA_STDERR,"%s: dset->idcode_str = %s\n", 
               FuncName, dset->idcode.str);
      fprintf (SUMA_STDERR,"%s: VP->vol_idcode_str = %s\n", 
               FuncName, VP->vol_idcode_str);
   }

   /* Get the 3drotate matrix if possible*/
   VP->MATVEC_source = NO_WARP;
   if ((atr = THD_find_float_atr( dset->dblk , "ROTATE_MATVEC_000000" ))) {
      if (!atrkeep) {
         atrkeep = atr;
         VP->MATVEC_source = ROTATE_WARP;
      }
      ++nxform;
   }
   if ((atr = THD_find_float_atr( dset->dblk , "TAGALIGN_MATVEC" ))) {
      if (!atrkeep) {
         atrkeep = atr;
         VP->MATVEC_source = TAGALIGN_WARP;
      }
      ++nxform;
   }
   if ((atr = THD_find_float_atr( dset->dblk , "WARPDRIVE_MATVEC_INV_000000" ))){
      if (!atrkeep) {
         atrkeep = atr;
         VP->MATVEC_source = WARPDRIVE_WARP; 
      }
      ++nxform;
   }
   if ((atr = THD_find_float_atr( dset->dblk , "ALLINEATE_MATVEC_S2B_000000" ))){
      if (!atrkeep) {
         atrkeep = atr;
         VP->MATVEC_source = ALLINEATE_WARP;
      }
      ++nxform;
   }
   if ((atr = THD_find_float_atr( dset->dblk , "VOLREG_MATVEC_000000" ))) {
      if (!atrkeep) {
         atrkeep = atr;
         VP->MATVEC_source = VOLREG_WARP;
      }
      ++nxform;
   }
   atr = atrkeep;
   if (nxform > 1) {
      SUMA_S_Warnv("More than one (%d) plausible transforms found.\n"
                  "Abiding by %s\n", 
                  nxform, SUMA_WarpTypeName(VP->MATVEC_source)); 
   } 
   
   if (atr == NULL) {
      VP->MATVEC = NULL;
   }else {
      VP->MATVEC = (double *)SUMA_calloc(12, sizeof(double));
      if (VP->MATVEC != NULL) {
         if (atr->nfl == 12) {
            for (ii=0; ii<12; ++ii) VP->MATVEC[ii] = (double)atr->fl[ii];
         } else {   
            fprintf( SUMA_STDERR,
                     "Error %s: MATVEC does not have 12 elements.\n", 
                     FuncName);
         }
      } else {
         fprintf(SUMA_STDERR,
                 "Error %s: Failed to allocate for VP->MATVEC\n", 
                 FuncName);
      }
   }
   
   
   /* Get the center base coordinates */
   switch (VP->MATVEC_source) {
      case VOLREG_WARP:
         atr = THD_find_float_atr( dset->dblk , "VOLREG_CENTER_BASE");
         break;
      case ROTATE_WARP:
         atr = THD_find_float_atr( dset->dblk , "ROTATE_CENTER_BASE");
         break;
      default:
         atr = NULL;
         break;
   }
   
   if (atr == NULL) {
      VP->CENTER_BASE = NULL;
   } else {
      VP->CENTER_BASE = (double *)SUMA_calloc(3, sizeof(double));
      if (VP->CENTER_BASE != NULL) {
         if (atr->nfl == 3) {
            for (ii=0; ii<3; ++ii) VP->CENTER_BASE[ii] = atr->fl[ii];
         } else {   
            fprintf( SUMA_STDERR,
                     "Error %s: CENTER_BASE does not have 12 elements.\n", 
                     FuncName);
         }
      } else {
         fprintf( SUMA_STDERR,
                  "Error %s: Failed to allocate for VP->CENTER_BASE\n", 
                  FuncName);
      }
   }
   
   /* CENTER_OLD  */
   switch (VP->MATVEC_source) {
      case VOLREG_WARP:
         atr = THD_find_float_atr( dset->dblk , "VOLREG_CENTER_OLD");
         break;
      case ROTATE_WARP:
         atr = THD_find_float_atr( dset->dblk , "ROTATE_CENTER_OLD");
         break;
      default:
         atr = NULL;
         break;
   }

   if (atr == NULL) {
      VP->CENTER_OLD = NULL;
   } else {
      VP->CENTER_OLD = (double *)SUMA_calloc(3, sizeof(double));
      if (VP->CENTER_OLD != NULL) {
         if (atr->nfl == 3) {
            for (ii=0; ii<3; ++ii) VP->CENTER_OLD[ii] = atr->fl[ii];
         } else {   
            fprintf( SUMA_STDERR,
                     "Error %s: CENTER_OLD does not have 12 elements.\n", 
                     FuncName);
         }
      } else {
         fprintf( SUMA_STDERR,
                  "Error %s: Failed to allocate for VP->CENTER_OLD\n", FuncName);
      }
   }

   /* handedness */
   VP->Hand = SUMA_THD_handedness( dset );
   
   SUMA_RETURN (VP);
}

SUMA_VOLPAR *SUMA_VolPar_Attr (char *volparent_name)
{
   ATR_float *atr;
   static char FuncName[]={"SUMA_VolPar_Attr"};
   SUMA_VOLPAR *VP;
   THD_3dim_dataset *dset;
   int ii;
   MCW_idcode idcode;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   /* read the header of the parent volume */
   dset = THD_open_dataset(volparent_name);
   if (dset == NULL) {
      fprintf (SUMA_STDERR,
               "Error %s: Could not read %s\n", FuncName, volparent_name);
      SUMA_RETURN (NULL);
   }
   
   VP = SUMA_VolParFromDset(dset);
   if (!VP) {
      SUMA_SL_Err("Failed in SUMA_VolParFromDset");
   }

   /* now free the dset pointer */
   THD_delete_3dim_dataset( dset , False ) ;
   
   SUMA_RETURN (VP);
}

/*!
   \brief Form a string containing the info of the volume parent
   
   \param VP (SUMA_VOLPAR *) Volume parent structure.
   \return s (char *) pointer to NULL terminated string containing surface info.
   It is your responsability to free it.
   
   \sa SUMA_Show_VolPar
*/

char *SUMA_VolPar_Info (SUMA_VOLPAR *VP)
{
   static char FuncName[]={"SUMA_VolPar_Info"};
   char stmp[1000], *s=NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);
   
   if (VP) { 
      sprintf (stmp,"\nVP contents:\n");
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,
               "prefix: %s\tfilecode: %s\tdirname: %s\n"
               "Id code str:%s\tID code date: %s\n", 
               VP->prefix, VP->filecode, VP->dirname, 
               VP->vol_idcode_str, VP->vol_idcode_date);
      SS = SUMA_StringAppend (SS, stmp);
      if (VP->idcode_str) SS = SUMA_StringAppend (SS, "IDcode is NULL\n");
      else SS = SUMA_StringAppend_va (SS, "IDcode: %s\n", VP->idcode_str);
      
      sprintf (stmp,"isanat: %d\n", VP->isanat);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"Orientation: %d %d %d\n", 
         VP->xxorient, VP->yyorient, VP->zzorient);
      if (VP->Hand == 1) 
         SS = SUMA_StringAppend (SS, "Right Hand Coordinate System.\n");
      else if (VP->Hand == -1) 
         SS = SUMA_StringAppend (SS, "Left Hand Coordinate System.\n");
      else SS = SUMA_StringAppend (SS, "No hand coordinate system!\n");
      
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"Origin: %f %f %f\n", 
         VP->xorg, VP->yorg, VP->zorg);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"Delta: %f %f %f\n", 
         VP->dx, VP->dy, VP->dz);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"N: %d %d %d\n",
         VP->nx, VP->ny, VP->nz);
      SS = SUMA_StringAppend (SS, stmp);

      SS = SUMA_StringAppend_va( SS,
                                 "VolPar transform type: %d\n", 
                                 SUMA_WarpTypeName(VP->MATVEC_source));
      if (VP->MATVEC != NULL) {
         sprintf (stmp,"VP->MATVEC = \n\tMrot\tDelta\n");
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", 
         VP->MATVEC[0], VP->MATVEC[1], VP->MATVEC[2], VP->MATVEC[3]); 
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", 
         VP->MATVEC[4], VP->MATVEC[5], VP->MATVEC[6], VP->MATVEC[7]);
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n",
         VP->MATVEC[8], VP->MATVEC[9], VP->MATVEC[10], VP->MATVEC[11]);
         SS = SUMA_StringAppend (SS, stmp);
      } else {
         sprintf (stmp,"VP->MATVEC = NULL\n");
         SS = SUMA_StringAppend (SS, stmp);
      }      
      if (VP->CENTER_OLD != NULL) {
         sprintf (stmp,"VP->CENTER_OLD = %f, %f, %f\n", 
           VP->CENTER_OLD[0], VP->CENTER_OLD[1], VP->CENTER_OLD[2]); 
         SS = SUMA_StringAppend (SS, stmp);
      }else {
         sprintf (stmp,"VP->CENTER_OLD = NULL\n");
         SS = SUMA_StringAppend (SS, stmp);
      }
      if (VP->CENTER_BASE != NULL) {
         sprintf (stmp,"VP->CENTER_BASE = %f, %f, %f\n", 
            VP->CENTER_BASE[0], VP->CENTER_BASE[1], VP->CENTER_BASE[2]); 
         SS = SUMA_StringAppend (SS, stmp);
      } else {
         sprintf (stmp,"VP->CENTER_BASE = NULL\n");
         SS = SUMA_StringAppend (SS, stmp);
      }      
   }else{
      sprintf (stmp, "NULL Volume Parent Pointer.\n");
      SS = SUMA_StringAppend (SS, stmp);
   }
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);
}
/*!
   Show the contents of SUMA_VOLPAR structure 
   \sa SUMA_VolPar_Info 
*/
void SUMA_Show_VolPar(SUMA_VOLPAR *VP, FILE *Out)
{
   static char FuncName[]={"SUMA_Show_VolPar"};
   char *s;
   
   SUMA_ENTRY;

   if (Out == NULL) Out = SUMA_STDOUT;
   
   s =  SUMA_VolPar_Info(VP);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_VolPar_Info.\n", FuncName);
   }   
   
   SUMA_RETURNe;
      
}

/*!
   \brief Transform the coordinates of a surface object to AFNI-DICOM convention and transform the coordinates by SO->VolPar
   ans = SUMA_Align_to_VolPar (SO, SF_Struct);
   \param SO (SUMA_SurfaceObject *)
   \param S_Struct (void *) That is only needed for SureFit surfaces and is nothing but a type cast of a SUMA_SureFit_struct containing information on cropping.
                              send NULL for all other surfaces.
   \return YUP/NOPE
   For SureFit and FreeSurfer surfaces, the coordinates are first set in RAI (DICOM) coordinate system before applying SO->VolPar.
   For other surface formats, SO->VolPar is applied to whatever coordinates are in SO->NodeList
*/
SUMA_Boolean SUMA_Align_to_VolPar (SUMA_SurfaceObject *SO, void * S_Struct)
{
   static char FuncName[]={"SUMA_Align_to_VolPar"};
   char  orcode[3];
   float xx, yy, zz;
   THD_coorder * cord_surf, *cord_RAI;
   int i, ND, id;
   SUMA_SureFit_struct *SF;
   SUMA_FreeSurfer_struct *FS;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;

   /* allocate for cord structure */
   cord_surf = (THD_coorder *)SUMA_malloc(sizeof(THD_coorder));
   cord_RAI = (THD_coorder *)SUMA_malloc(sizeof(THD_coorder));
   if (cord_surf == NULL || cord_RAI == NULL) {
      fprintf(SUMA_STDERR,"Error %s: failed to allocate for cord\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* do the dance to get cord for RAI */
   THD_coorder_fill( NULL, cord_RAI);
   ND = SO->NodeDim;
   switch (SO->FileType) {
      case SUMA_INVENTOR_GENERIC:
      case SUMA_OPENDX_MESH:
      case SUMA_PLY:
      case SUMA_BYU:
      case SUMA_VEC:
         /* Do nothing */
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         /* For free surfer, all you need to do is 
          go from LPI to RAI (DICOM)*/
         sprintf(orcode,"LPI");
         THD_coorder_fill(orcode , cord_surf); 
         /*loop over XYZs and change them to dicom*/
         for (i=0; i < SO->N_Node; ++i) {
            id = i * ND;
            THD_coorder_to_dicom (cord_surf, 
                                  &(SO->NodeList[id]), 
                                  &(SO->NodeList[id+1]), 
                                  &(SO->NodeList[id+2])); 
         }
         break;
      case SUMA_SUREFIT:
         /* For SureFit, coordinates are actually a 
            float version of the indices */
         SF = (SUMA_SureFit_struct *)S_Struct;
         if (SF->caret_version < 5.2) {   
            THD_fvec3 fv, iv;
            float D[3];
            /* Calcluate Delta caused by cropping */
            for (i=0; i < 3; ++i) D[i] = SF->AC_WholeVolume[i] - SF->AC[i];
            SUMA_LHv("caret_version: %f\n"
                     "AC_WholeVolume:  [%f %f %f]\n"
                     "AC:              [%f %f %f]\n"
                     "Shift Values:    [%f, %f, %f]\n"
                     "Node 0 init:     [%f, %f, %f]\n"
                     "CropMin:         [%f, %f, %f]\n"
                     "CropMax:         [%f, %f, %f]\n", 
                     SF->caret_version,
                     SF->AC_WholeVolume[0],SF->AC_WholeVolume[1], 
                                             SF->AC_WholeVolume[2],
                     SF->AC[0], SF->AC[1], SF->AC[2],
                     D[0], D[1], D[2],
                     SO->NodeList[0], SO->NodeList[1],SO->NodeList[2],
                     SF->CropMin[0], SF->CropMin[1], SF->CropMin[2],
                     SF->CropMax[0], SF->CropMax[1], SF->CropMax[2]); 
            for (i=0; i < SO->N_Node; ++i) {
               id = i * ND;
               /* change float indices to mm coords */
               iv.xyz[0] = SO->NodeList[id] + D[0];
               iv.xyz[1] = SO->NodeList[id+1] + D[1];
               iv.xyz[2] = SO->NodeList[id+2] + D[2];
               fv = SUMA_THD_3dfind_to_3dmm( SO, iv );
               
               /* change mm to RAI coords */
               iv = SUMA_THD_3dmm_to_dicomm( SO->VolPar->xxorient, 
                                             SO->VolPar->yyorient, 
                                             SO->VolPar->zzorient,  fv );
               SO->NodeList[id] = iv.xyz[0];
               SO->NodeList[id+1] = iv.xyz[1];
               SO->NodeList[id+2] = iv.xyz[2];
            }
               SUMA_LHv("Node 0 RAI:     [%f, %f, %f]\n",
                        SO->NodeList[0], SO->NodeList[1],SO->NodeList[2]);  
         } else {
            float D[3];
            /* Calcluate Delta caused by cropping */
            for (i=0; i < 3; ++i) D[i] = SF->AC_WholeVolume[i] - SF->AC[i];
            SUMA_LHv("caret_version: %f\n"
                     "AC_WholeVolume:  [%f %f %f]\n"
                     "AC:              [%f %f %f]\n"
                     "Shift Values:    [%f, %f, %f]\n"
                     "Node 0 init:     [%f, %f, %f]\n"
                     "CropMin:         [%f, %f, %f]\n"
                     "CropMax:         [%f, %f, %f]\n", 
                     SF->caret_version,
                     SF->AC_WholeVolume[0],SF->AC_WholeVolume[1], 
                                             SF->AC_WholeVolume[2],
                     SF->AC[0], SF->AC[1], SF->AC[2],
                     D[0], D[1], D[2],
                     SO->NodeList[0], SO->NodeList[1],SO->NodeList[2],
                     SF->CropMin[0], SF->CropMin[1], SF->CropMin[2],
                     SF->CropMax[0], SF->CropMax[1], SF->CropMax[2]); 
            if (D[0] != 0.0 || D[1] != 0.0 ||D[2] != 0.0) {
               SUMA_S_Notev("Shift Values: [%f, %f, %f]\n", 
                            D[0], D[1], D[2]);
               SUMA_S_Note(  "If surface alignment is off"
                              "Please notify authors and send sample data.\n");
                
            }
            /* Caret, just LPI baby, take it to RAI*/
            for (i=0; i < SO->N_Node; ++i) {
               id = i * ND;
               SO->NodeList[id]   = -SO->NodeList[id];
               SO->NodeList[id+1] = -SO->NodeList[id+1];
            }
            
            SUMA_LHv("Node 0 RAI          :     [%f, %f, %f]\n",
                     SO->NodeList[0], SO->NodeList[1],SO->NodeList[2]);  
         }
         break;
      case SUMA_BRAIN_VOYAGER:
         #if 0 
         /* this contraption was used when BV's afni volumes had 0,0,0 for origin
          which is inappropriate */
         /* For Brain Voyager, all you need to do is 
          go from ASR to RAI (DICOM)
          Note: The center of the volume is at the 1st voxel's center and 
          that huge
          center shift, relative to standard AFNI dsets (centered about 
          middle of volume)
          might throw off 3dVolreg. If you want to shift volume's center to be in
          the middle voxel, you'll need to shift the surface coordinates
           before transforming
          them to RAI*/
         sprintf(orcode,"ASR");
         THD_coorder_fill(orcode , cord_surf); 
         /*loop over XYZs and change them to dicom*/
         for (i=0; i < SO->N_Node; ++i) {
            id = i * ND;
            THD_coorder_to_dicom (cord_surf, 
                                  &(SO->NodeList[id]), 
                                  &(SO->NodeList[id+1]), 
                                  &(SO->NodeList[id+2])); 
         }
         #else /*ZSS: Nov. 1 07 */
         if (SO->VolPar) {
            /* looks like coordinates are in float index units, go to dicomm */
            if (!SUMA_vec_3dfind_to_dicomm(SO->NodeList,SO->N_Node,SO->VolPar)) {
               SUMA_S_Err("Failed to xform coords.");
               SUMA_RETURN (NOPE);
            }
         }
         #endif
         break;
      case SUMA_GIFTI:  /* have to apply coord xform */
      
         break;
      default:
         fprintf( SUMA_STDERR,
                  "Warning %s: Unknown SO->FileType.\n"
                  "Assuming coordinates are in DICOM already.\n", FuncName);
         break;
   }
   
   if (!SUMA_Apply_VolReg_Trans (SO)) {
      fprintf( SUMA_STDERR,
               "Error %s: Failed in SUMA_Apply_VolReg_Trans.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   SUMA_RETURN (YUP);
}

/*!
   \brief Undo the transform the coordinates of a surface object from AFNI-DICOM convention to the native space
   Transforms in SO->VolPar are ignored
   ans = SUMA_Delign_to_VolPar (SO, SF_Struct);
   \param SO (SUMA_SurfaceObject *)
   \param S_Struct (void *) That is only needed for SureFit surfaces and is nothing but a type cast of a SUMA_SureFit_struct containing information on cropping.
                              send NULL for all other surfaces.
   \return YUP/NOPE
   For SureFit and FreeSurfer surfaces, the coordinates are first set in RAI (DICOM) coordinate system before applying SO->VolPar.
   For other surface formats, SO->VolPar is applied to whatever coordinates are in SO->NodeList
*/
SUMA_Boolean SUMA_Delign_to_VolPar (SUMA_SurfaceObject *SO, void * S_Struct)
{
   static char FuncName[]={"SUMA_Delign_to_VolPar"};
   char  orcode[3];
   float xx, yy, zz;
   THD_coorder * cord_surf, *cord_RAI;
   int i, ND, id;
   SUMA_SureFit_struct *SF;
   SUMA_FreeSurfer_struct *FS;
   
   SUMA_ENTRY;

   /* allocate for cord structure */
   cord_surf = (THD_coorder *)SUMA_malloc(sizeof(THD_coorder));
   cord_RAI = (THD_coorder *)SUMA_malloc(sizeof(THD_coorder));
   if (cord_surf == NULL || cord_RAI == NULL) {
      fprintf(SUMA_STDERR,"Error %s: failed to allocate for cord\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* do the dance to get cord for RAI */
   THD_coorder_fill( NULL, cord_RAI);
   ND = SO->NodeDim;
   switch (SO->FileType) {
      case SUMA_INVENTOR_GENERIC:
      case SUMA_OPENDX_MESH:
      case SUMA_PLY:
      case SUMA_BYU:
      case SUMA_VEC:
         /* Do nothing */
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         /* For free surfer, all you need to do is 
          go from LPI to RAI (DICOM)*/
         sprintf(orcode,"LPI");
         THD_coorder_fill(orcode , cord_surf); 
         /*loop over XYZs and change them to surface's order*/
         for (i=0; i < SO->N_Node; ++i) {
            id = i * ND;
            THD_dicom_to_coorder (cord_surf, &(SO->NodeList[id]), &(SO->NodeList[id+1]), &(SO->NodeList[id+2])); 
         }
         break;
      case SUMA_SUREFIT:
         SF = (SUMA_SureFit_struct *)S_Struct;
         if (SF->caret_version < 5.2) {   
            /* For SureFit, coordinates are actually a float version of the indices */
            SUMA_SL_Warn(  "Reverse implementation not finished\n"
                           "Send me a complaint, I must have forgotten\n"
                           "Coords will be left untouched. (saadz@mail.nih.gov)\n");
            #if 0               
            {   THD_fvec3 fv, iv;
               float D[3];
               /* Calcluate Delta caused by cropping */
               for (i=0; i < 3; ++i) D[i] = SF->AC_WholeVolume[i] - SF->AC[i];
               /* fprintf (SUMA_STDERR,"%s: Shift Values: [%f, %f, %f]\n", FuncName, D[0], D[1], D[2]); */
               for (i=0; i < SO->N_Node; ++i) {
                  id = i * ND;
                  /* change float indices to mm coords */
                  iv.xyz[0] = SO->NodeList[id] + D[0];
                  iv.xyz[1] = SO->NodeList[id+1] + D[1];
                  iv.xyz[2] = SO->NodeList[id+2] + D[2];
                  fv = SUMA_THD_3dfind_to_3dmm( SO, iv );

                  /* change mm to RAI coords */
                  iv = SUMA_THD_3dmm_to_dicomm( SO->VolPar->xxorient, SO->VolPar->yyorient, SO->VolPar->zzorient,  fv );
                  SO->NodeList[id] = iv.xyz[0];
                  SO->NodeList[id+1] = iv.xyz[1];
                  SO->NodeList[id+2] = iv.xyz[2];
               }
            }
            #endif
         } else {
            float D[3];
            /* Calcluate Delta caused by cropping */
            for (i=0; i < 3; ++i) D[i] = SF->AC_WholeVolume[i] - SF->AC[i];
            if (D[0] != 0.0f || D[1] != 0.0f ||D[2] != 0.0f) {
               fprintf (SUMA_STDERR,"Error %s: Shift Values: [%f, %f, %f]\n", FuncName, D[0], D[1], D[2]);
               fprintf (SUMA_STDERR,"Never encountered this case. Please notify authors and send sample data.\n");
               SUMA_RETURN (NOPE); 
            }

            /* just go back from RAI to LPI */
            for (i=0; i < SO->N_Node; ++i) {
               id = i * ND;
               SO->NodeList[id] = -SO->NodeList[id];
               SO->NodeList[id+1] = -SO->NodeList[id+1];
            }   
         }
         break;
      case SUMA_BRAIN_VOYAGER:
         #if 0
         /* For Brain Voyager, all you need to do is 
          go from AIR to RAI (DICOM)
          Note: The center of the volume is at the 1st voxel's center and that huge
          center shift, relative to standard AFNI dsets (centered about middle of volume)
          might throw off 3dVolreg. If you want to shift volume's center to be in
          the middle voxel, you'll need to shift the surface coordinates before transforming
          them to ASR*/
         sprintf(orcode,"ASR");
         THD_coorder_fill(orcode , cord_surf); 
         /*loop over XYZs and change them to native space*/
         for (i=0; i < SO->N_Node; ++i) {
            id = i * ND;
            THD_dicom_to_coorder (cord_surf, &(SO->NodeList[id]), &(SO->NodeList[id+1]), &(SO->NodeList[id+2])); 
         }
         #else /*ZSS: Nov. 1 07 */
         if (SO->VolPar) {
            /* to back to float index units */
            if (!SUMA_vec_dicomm_to_3dfind(SO->NodeList, SO->N_Node, SO->VolPar)) {
               SUMA_S_Err("Failed to xform coords.");
               SUMA_RETURN (NOPE);
            }
         } else {
            fprintf(SUMA_STDERR,"Error %s: Can't delign witout a volpar.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #endif
         break;
      default:
         fprintf(SUMA_STDERR,"Warning %s: Unknown SO->FileType. Assuming coordinates are in DICOM already.\n", FuncName);
         break;
   }
   
   #if 0
   /* I don't thin the inverse of that step will be needed .... */
   if (!SUMA_Apply_VolReg_Trans (SO)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Apply_VolReg_Trans.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   #endif

   SUMA_RETURN (YUP);
}

         
SUMA_Boolean SUMA_Apply_Coord_xform(float *NodeList,
                                    int N_Node,
                                    int NodeDim, 
                                    double Xform[4][4],
                                    int doinv,
                                    double *ppshift)
{
   static char FuncName[]={"SUMA_Apply_Coord_xform"};
   double x, y, z;
   int i=0, id = 0;
   mat44 A, A0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!NodeList) SUMA_RETURN(NOPE); 
   
   /* check for identity */
   if ( SUMA_IS_XFORM_IDENTITY(Xform)  ) {
      SUMA_LH("Indentity, nothing to do.");
      SUMA_RETURN(YUP);      
   }
   
   if (!doinv) {
      LOAD_MAT44( A, \
                  Xform[0][0], Xform[0][1], Xform[0][2], Xform[0][3],    \
                  Xform[1][0], Xform[1][1], Xform[1][2], Xform[1][3],    \
                  Xform[2][0], Xform[2][1], Xform[2][2], Xform[2][3]   );
   } else {
      LOAD_MAT44( A0, \
                  Xform[0][0], Xform[0][1], Xform[0][2], Xform[0][3],    \
                  Xform[1][0], Xform[1][1], Xform[1][2], Xform[1][3],    \
                  Xform[2][0], Xform[2][1], Xform[2][2], Xform[2][3]   );
      A = nifti_mat44_inverse(A0);
   }            
   
   for (i=0; i < N_Node; ++i) {
      id = NodeDim * i;
      x = (double)NodeList[id] ;
      y = (double)NodeList[id+1] ;
      z = (double)NodeList[id+2] ;
      if (ppshift) {
         x += ppshift[0];
         y += ppshift[1];
         z += ppshift[2];
      }
      
      /* Apply the rotation matrix XYZn = Mrot x XYZ + Delta*/
      NodeList[id]   = (float) (     A.m[0][0] * x + 
                                     A.m[0][1] * y + 
                                     A.m[0][2] * z +
                                     A.m[0][3] );
      NodeList[id+1] = (float) (     A.m[1][0] * x + 
                                     A.m[1][1] * y + 
                                     A.m[1][2] * z +
                                     A.m[1][3] );
      NodeList[id+2] = (float) (     A.m[2][0] * x + 
                                     A.m[2][1] * y + 
                                     A.m[2][2] * z +
                                     A.m[2][3] );
      if (ppshift) {
         NodeList[id  ] -= ppshift[0];
         NodeList[id+1] -= ppshift[1];
         NodeList[id+2] -= ppshift[2];
      }
    }

   SUMA_RETURN(YUP);
}

/*!
\brief applies the transform in SO->VolPar to SO->NodeList. This only makes sense if 
SO->NodeList is in DICOM to begin with...
\param SO (SUMA_SurfaceObject *) You better have NodeList and VolPar in there...
\return YUP/NOPE
\sa SUMA_Align_to_VolPar
NOTE: The field SO->VOLREG_APPLIED is set to NOPE if this function fails, YUP otherwise
*/

SUMA_Boolean SUMA_Apply_VolReg_Trans (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_Apply_VolReg_Trans"};
   int i, ND, id;
   double xform[3][4];
   SUMA_Boolean Bad=YUP;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;

   if (get_IgnoreXforms()) {
      SUMA_SL_Note("Ignoring any Volreg, TagAlign, Rotate, \n"
                   "WarpDrive, or Allineate transforms present\n"
                   "in Surface Volume.\n");
      SUMAg_CF->IgnoreVolreg = YUP;
      SO->APPLIED_A2Exp_XFORM = NO_WARP;
      SUMA_RETURN (YUP);
   }
   
   if (SO->APPLIED_A2Exp_XFORM != 0) {
      fprintf (SUMA_STDERR,
               "Error %s: \n"
               "Volreg (or Tagalign or rotate or warpdrive or allineate)\n"
               "already applied. Nothing done.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   ND = SO->NodeDim;
   
    /* perform the rotation needed to align 
      the surface with the current experiment's data */
   SUMA_LHv("MATVEC_source = %s\n",
             SUMA_WarpTypeName(SO->VolPar->MATVEC_source));
   switch (SO->VolPar->MATVEC_source) {
      case ROTATE_WARP:
      case VOLREG_WARP:
         if (  SO->VolPar->MATVEC != NULL || 
               SO->VolPar->CENTER_OLD != NULL || 
               SO->VolPar->CENTER_BASE != NULL) {
            Bad = NOPE;
            if (SO->VolPar->MATVEC == NULL) {
               fprintf(SUMA_STDERR,
                        "Error %s: SO->VolPar->VOLREG_MATVEC = NULL. \n"
                        "Cannot perform alignment.\n", FuncName);
               Bad = YUP;
            }
            if (SO->VolPar->CENTER_OLD == NULL) {
               fprintf(SUMA_STDERR,
                        "Error %s: SO->VolPar->CENTER_OLD = NULL.\n"
                        "Cannot perform alignment.\n", FuncName);
               Bad = YUP;
            }
            if (SO->VolPar->CENTER_BASE == NULL) {
               fprintf( SUMA_STDERR,
                        "Error %s: SO->VolPar->CENTER_BASE = NULL. \n"
                        "Cannot perform alignment.\n", FuncName);
               Bad = YUP;
            }
         }
         break;
      case TAGALIGN_WARP:
      case ALLINEATE_WARP:
      case WARPDRIVE_WARP:
         /* all good */
         if (  SO->VolPar->MATVEC != NULL) {
            Bad = NOPE;
         } else {
            fprintf(SUMA_STDERR,
                     "Error %s: SO->VolPar->MATVEC = NULL. \n"
                     "Cannot perform alignment.\n", FuncName);
            Bad = YUP;
         }
         break;
      case NO_WARP:
         SO->APPLIED_A2Exp_XFORM = NO_WARP;
         SUMA_RETURN (YUP);
      default:
         fprintf( SUMA_STDERR,
                  "Error %s: Warptype %d unaccounted for.\n", 
                  FuncName, SO->VolPar->MATVEC_source);
         Bad = YUP;
         break;
   }
   
   /* Now do the transformation */   

#if 1
   if (  SO->VolPar->MATVEC_source == VOLREG_WARP ||
         SO->VolPar->MATVEC_source == ROTATE_WARP ) {
      /* remove old center */      
      for (i=0; i < SO->N_Node; ++i) {
         id = ND * i;
         SO->NodeList[id  ] -= SO->VolPar->CENTER_OLD[0];
         SO->NodeList[id+1] -= SO->VolPar->CENTER_OLD[1];
         SO->NodeList[id+2] -= SO->VolPar->CENTER_OLD[2];
      }
   }
   
   /* Now apply affine */
   SUMA_Xform1Dto2D(SO->VolPar->MATVEC, xform);
   if (!SUMA_Apply_Coord_xform(SO->NodeList, SO->N_Node, SO->NodeDim,
                               xform, 0, NULL)) {
      SUMA_S_Err("Failed to apply AlndExp transform");
      Bad = YUP;
   } 
   
   if (  SO->VolPar->MATVEC_source == VOLREG_WARP ||
         SO->VolPar->MATVEC_source == ROTATE_WARP ) {
      /* put back new center */      
      for (i=0; i < SO->N_Node; ++i) {
         id = ND * i;
         SO->NodeList[id  ] += SO->VolPar->CENTER_BASE[0];
         SO->NodeList[id+1] += SO->VolPar->CENTER_BASE[1];
         SO->NodeList[id+2] += SO->VolPar->CENTER_BASE[2];
      }
   }
   
   SO->APPLIED_A2Exp_XFORM = SO->VolPar->MATVEC_source;
   Bad = NOPE;
#else
   /* the olde way, burn it a while after March 27 2008*/
   if (UseTagAlign) {
      float Mrot[3][3], Delta[3], x, y, z, NetShift[3];
      
      /* fillerup*/
      Mrot[0][0] = SO->VolPar->TAGALIGN_MATVEC[0];
      Mrot[0][1] = SO->VolPar->TAGALIGN_MATVEC[1];
      Mrot[0][2] = SO->VolPar->TAGALIGN_MATVEC[2];
      Delta[0]   = SO->VolPar->TAGALIGN_MATVEC[3];
      Mrot[1][0] = SO->VolPar->TAGALIGN_MATVEC[4];
      Mrot[1][1] = SO->VolPar->TAGALIGN_MATVEC[5];
      Mrot[1][2] = SO->VolPar->TAGALIGN_MATVEC[6];
      Delta[1]   = SO->VolPar->TAGALIGN_MATVEC[7];   
      Mrot[2][0] = SO->VolPar->TAGALIGN_MATVEC[8];
      Mrot[2][1] = SO->VolPar->TAGALIGN_MATVEC[9];
      Mrot[2][2] = SO->VolPar->TAGALIGN_MATVEC[10];
      Delta[2]   = SO->VolPar->TAGALIGN_MATVEC[11];
      
      NetShift[0] = Delta[0];
      NetShift[1] = Delta[1];
      NetShift[2] = Delta[2];
      
      /*
      fprintf (SUMA_STDERR,"%s: Applying xform.\nMrot[\t%f\t%f\t%f\n%f\t%f\t%f\n%f\t%f\t%f]\nDelta = [%f %f %f]\n", FuncName,\
               Mrot[0][0], Mrot[0][1], Mrot[0][2], Mrot[1][0], Mrot[1][1], Mrot[1][2], Mrot[2][0], Mrot[2][1], Mrot[2][2], \
               Delta[0], Delta[1], Delta[2]);
      
      */
      
      for (i=0; i < SO->N_Node; ++i) {
         id = ND * i;
         /* zero the center */ 
         x = SO->NodeList[id] ;
         y = SO->NodeList[id+1] ;
         z = SO->NodeList[id+2] ;
         
         /* Apply the rotation matrix XYZn = Mrot x XYZ*/
         SO->NodeList[id] = Mrot[0][0] * x + Mrot[0][1] * y + Mrot[0][2] * z;
         SO->NodeList[id+1] = Mrot[1][0] * x + Mrot[1][1] * y + Mrot[1][2] * z;
         SO->NodeList[id+2] = Mrot[2][0] * x + Mrot[2][1] * y + Mrot[2][2] * z;
         
         /*apply netshift*/
         SO->NodeList[id] += NetShift[0];
         SO->NodeList[id+1] += NetShift[1];
         SO->NodeList[id+2] += NetShift[2];
      }
      SO->TAGALIGN_APPLIED = YUP;   
   } else
      SO->TAGALIGN_APPLIED = NOPE;

   if (UseWarpAlign) {
      float Mrot[3][3], Delta[3], x, y, z, NetShift[3];
      
      /* fillerup*/
      Mrot[0][0] = SO->VolPar->WARPDRIVE_MATVEC[0];
      Mrot[0][1] = SO->VolPar->WARPDRIVE_MATVEC[1];
      Mrot[0][2] = SO->VolPar->WARPDRIVE_MATVEC[2];
      Delta[0]   = SO->VolPar->WARPDRIVE_MATVEC[3];
      Mrot[1][0] = SO->VolPar->WARPDRIVE_MATVEC[4];
      Mrot[1][1] = SO->VolPar->WARPDRIVE_MATVEC[5];
      Mrot[1][2] = SO->VolPar->WARPDRIVE_MATVEC[6];
      Delta[1]   = SO->VolPar->WARPDRIVE_MATVEC[7];   
      Mrot[2][0] = SO->VolPar->WARPDRIVE_MATVEC[8];
      Mrot[2][1] = SO->VolPar->WARPDRIVE_MATVEC[9];
      Mrot[2][2] = SO->VolPar->WARPDRIVE_MATVEC[10];
      Delta[2]   = SO->VolPar->WARPDRIVE_MATVEC[11];
      
      NetShift[0] = Delta[0];
      NetShift[1] = Delta[1];
      NetShift[2] = Delta[2];
      
      
      /* fprintf (SUMA_STDERR,"%s: Applying xform.\nMrot[\t%f\t%f\t%f\n%f\t%f\t%f\n%f\t%f\t%f]\nDelta = [%f %f %f]\n", FuncName,\
               Mrot[0][0], Mrot[0][1], Mrot[0][2], Mrot[1][0], Mrot[1][1], Mrot[1][2], Mrot[2][0], Mrot[2][1], Mrot[2][2], \
               Delta[0], Delta[1], Delta[2]); */
      
      
      
      for (i=0; i < SO->N_Node; ++i) {
         id = ND * i;
         /* zero the center */ 
         x = SO->NodeList[id] ;
         y = SO->NodeList[id+1] ;
         z = SO->NodeList[id+2] ;
         
         /* Apply the rotation matrix XYZn = Mrot x XYZ*/
         SO->NodeList[id] = Mrot[0][0] * x + Mrot[0][1] * y + Mrot[0][2] * z;
         SO->NodeList[id+1] = Mrot[1][0] * x + Mrot[1][1] * y + Mrot[1][2] * z;
         SO->NodeList[id+2] = Mrot[2][0] * x + Mrot[2][1] * y + Mrot[2][2] * z;
         
         /*apply netshift*/
         SO->NodeList[id] += NetShift[0];
         SO->NodeList[id+1] += NetShift[1];
         SO->NodeList[id+2] += NetShift[2];
      }
      SO->WARPDRIVE_APPLIED = YUP;   
   } else
      SO->WARPDRIVE_APPLIED = NOPE;
         
   if (UseVolreg) {
      float Mrot[3][3], Delta[3], x, y, z, NetShift[3];
      
      /* fillerup*/
      Mrot[0][0] = SO->VolPar->VOLREG_MATVEC[0];
      Mrot[0][1] = SO->VolPar->VOLREG_MATVEC[1];
      Mrot[0][2] = SO->VolPar->VOLREG_MATVEC[2];
      Delta[0]   = SO->VolPar->VOLREG_MATVEC[3];
      Mrot[1][0] = SO->VolPar->VOLREG_MATVEC[4];
      Mrot[1][1] = SO->VolPar->VOLREG_MATVEC[5];
      Mrot[1][2] = SO->VolPar->VOLREG_MATVEC[6];
      Delta[1]   = SO->VolPar->VOLREG_MATVEC[7];   
      Mrot[2][0] = SO->VolPar->VOLREG_MATVEC[8];
      Mrot[2][1] = SO->VolPar->VOLREG_MATVEC[9];
      Mrot[2][2] = SO->VolPar->VOLREG_MATVEC[10];
      Delta[2]   = SO->VolPar->VOLREG_MATVEC[11];
      
      NetShift[0] = SO->VolPar->VOLREG_CENTER_BASE[0] + Delta[0];
      NetShift[1] = SO->VolPar->VOLREG_CENTER_BASE[1] + Delta[1];
      NetShift[2] = SO->VolPar->VOLREG_CENTER_BASE[2] + Delta[2];
      
      /*
      fprintf (SUMA_STDERR,"%s: Applying Rotation.\nMrot[\t%f\t%f\t%f\n%f\t%f\t%f\n%f\t%f\t%f]\nDelta = [%f %f %f]\n", FuncName,\
               Mrot[0][0], Mrot[0][1], Mrot[0][2], Mrot[1][0], Mrot[1][1], Mrot[1][2], Mrot[2][0], Mrot[2][1], Mrot[2][2], \
               Delta[0], Delta[1], Delta[2]);
      fprintf (SUMA_STDERR,"VOLREG_CENTER_BASE = [%f %f %f]. VOLREG_CENTER_OLD = [%f %f %f]\n", \
         SO->VolPar->VOLREG_CENTER_BASE[0], SO->VolPar->VOLREG_CENTER_BASE[1], SO->VolPar->VOLREG_CENTER_BASE[2], \
         SO->VolPar->VOLREG_CENTER_OLD[0], SO->VolPar->VOLREG_CENTER_OLD[1], SO->VolPar->VOLREG_CENTER_OLD[2]);
      */
      
      for (i=0; i < SO->N_Node; ++i) {
         id = ND * i;
         /* zero the center */ 
         x = SO->NodeList[id  ] - SO->VolPar->VOLREG_CENTER_OLD[0];
         y = SO->NodeList[id+1] - SO->VolPar->VOLREG_CENTER_OLD[1];
         z = SO->NodeList[id+2] - SO->VolPar->VOLREG_CENTER_OLD[2];
         
         /* Apply the rotation matrix XYZn = Mrot x XYZ*/
         SO->NodeList[id  ] = Mrot[0][0] * x + Mrot[0][1] * y + Mrot[0][2] * z;
         SO->NodeList[id+1] = Mrot[1][0] * x + Mrot[1][1] * y + Mrot[1][2] * z;
         SO->NodeList[id+2] = Mrot[2][0] * x + Mrot[2][1] * y + Mrot[2][2] * z;
         
         /*apply netshift*/
         SO->NodeList[id  ] += NetShift[0];
         SO->NodeList[id+1] += NetShift[1];
         SO->NodeList[id+2] += NetShift[2];
      }
      SO->VOLREG_APPLIED = YUP;   
   } else
      SO->VOLREG_APPLIED = NOPE;
   
   if (UseRotate) {
      float Mrot[3][3], Delta[3], x, y, z, NetShift[3];
      
      /* fillerup*/
      Mrot[0][0] = SO->VolPar->ROTATE_MATVEC[0];
      Mrot[0][1] = SO->VolPar->ROTATE_MATVEC[1];
      Mrot[0][2] = SO->VolPar->ROTATE_MATVEC[2];
      Delta[0]   = SO->VolPar->ROTATE_MATVEC[3];
      Mrot[1][0] = SO->VolPar->ROTATE_MATVEC[4];
      Mrot[1][1] = SO->VolPar->ROTATE_MATVEC[5];
      Mrot[1][2] = SO->VolPar->ROTATE_MATVEC[6];
      Delta[1]   = SO->VolPar->ROTATE_MATVEC[7];   
      Mrot[2][0] = SO->VolPar->ROTATE_MATVEC[8];
      Mrot[2][1] = SO->VolPar->ROTATE_MATVEC[9];
      Mrot[2][2] = SO->VolPar->ROTATE_MATVEC[10];
      Delta[2]   = SO->VolPar->ROTATE_MATVEC[11];
      
      NetShift[0] = SO->VolPar->ROTATE_CENTER_BASE[0] + Delta[0];
      NetShift[1] = SO->VolPar->ROTATE_CENTER_BASE[0] + Delta[1];
      NetShift[2] = SO->VolPar->ROTATE_CENTER_BASE[0] + Delta[2];
      
      /*
      fprintf (SUMA_STDERR,"%s: Applying Rotation.\nMrot[\t%f\t%f\t%f\n%f\t%f\t%f\n%f\t%f\t%f]\nDelta = [%f %f %f]\n", FuncName,\
               Mrot[0][0], Mrot[0][1], Mrot[0][2], Mrot[1][0], Mrot[1][1], Mrot[1][2], Mrot[2][0], Mrot[2][1], Mrot[2][2], \
               Delta[0], Delta[1], Delta[2]);
      fprintf (SUMA_STDERR,"ROTATE_CENTER_BASE = [%f %f %f]. ROTATE_CENTER_OLD = [%f %f %f]\n", \
         SO->VolPar->ROTATE_CENTER_BASE[0], SO->VolPar->ROTATE_CENTER_BASE[1], SO->VolPar->ROTATE_CENTER_BASE[2], \
         SO->VolPar->ROTATE_CENTER_OLD[0], SO->VolPar->ROTATE_CENTER_OLD[1], SO->VolPar->ROTATE_CENTER_OLD[2]);
      */
      
      for (i=0; i < SO->N_Node; ++i) {
         id = ND * i;
         /* zero the center */ 
         x = SO->NodeList[id  ] - SO->VolPar->ROTATE_CENTER_OLD[0];
         y = SO->NodeList[id+1] - SO->VolPar->ROTATE_CENTER_OLD[1];
         z = SO->NodeList[id+2] - SO->VolPar->ROTATE_CENTER_OLD[1];
         
         /* Apply the rotation matrix XYZn = Mrot x XYZ*/
         SO->NodeList[id  ] = Mrot[0][0] * x + Mrot[0][1] * y + Mrot[0][2] * z;
         SO->NodeList[id+1] = Mrot[1][0] * x + Mrot[1][1] * y + Mrot[1][2] * z;
         SO->NodeList[id+2] = Mrot[2][0] * x + Mrot[2][1] * y + Mrot[2][2] * z;
         
         /*apply netshift*/
         SO->NodeList[id  ] += NetShift[0];
         SO->NodeList[id+1] += NetShift[1];
         SO->NodeList[id+2] += NetShift[2];
      }
      SO->ROTATE_APPLIED = YUP;   
   } else
      SO->ROTATE_APPLIED = NOPE;
#endif
   
   SUMA_RETURN (!Bad);
}

/*! The following functions are adaptations from thd_coords.c 
They are modified to avoid the use of dset data structure 
which is not fully preserved in the Surface Object Data structure.

Stolen Comment:
====================================================================
   3D coordinate conversion routines;
     tags for coordinate systems:
       fdind  = FD_brick voxel indices (ints)
       fdfind = FD_brick voxel indices (floats - added 30 Aug 2001)
       3dind  = THD_3dim_dataset voxel indices (ints)
       3dfind = THD_3dim_dataset floating point voxel indices
                  (used for subvoxel resolution)
       3dmm   = THD_3dim_dataset millimetric coordinates (floats)
       dicomm = DICOM 3.0 millimetric coordinates (floats)

     The 3dmm coordinate measurements are oriented the same way
     as the dicomm coordinates (which means that the ??del values
     can be negative if the voxel axes are reflected), but the 3dmm
     coordinates are in general a permutation of the DICOM coordinates.

     These routines all take as input and produce as output
     THD_?vec3 (i=int,f=float) structs.
======================================================================

*/

THD_fvec3 SUMA_THD_3dfind_to_3dmm( SUMA_SurfaceObject *SO, 
                              THD_fvec3 iv )
{
   static char FuncName[]={"SUMA_THD_3dfind_to_3dmm"};
   THD_fvec3     fv ;

   SUMA_ENTRY;

   fv.xyz[0] = SO->VolPar->xorg + iv.xyz[0] * SO->VolPar->dx ;
   fv.xyz[1] = SO->VolPar->yorg + iv.xyz[1] * SO->VolPar->dy ;
   fv.xyz[2] = SO->VolPar->zorg + iv.xyz[2] * SO->VolPar->dz ;
   SUMA_RETURN(fv) ;
}

/*! 
   Same as SUMA_THD_3dfind_to_3dmm, but without needing SO
*/
THD_fvec3 SUMA_THD_3dfind_to_3dmm_vp( SUMA_VOLPAR *vp, 
                                       THD_fvec3 iv )
{
   static char FuncName[]={"SUMA_THD_3dfind_to_3dmm_vp"};
   THD_fvec3     fv ;

   SUMA_ENTRY;

   fv.xyz[0] = vp->xorg + iv.xyz[0] * vp->dx ;
   fv.xyz[1] = vp->yorg + iv.xyz[1] * vp->dy ;
   fv.xyz[2] = vp->zorg + iv.xyz[2] * vp->dz ;
   SUMA_RETURN(fv) ;
}

/*!------------------------------------------------------------------*/

THD_fvec3 SUMA_THD_3dind_to_3dmm( SUMA_SurfaceObject *SO,
                                   THD_ivec3 iv )
{
   static char FuncName[]={"SUMA_THD_3dind_to_3dmm"};
   THD_fvec3     fv ;

   SUMA_ENTRY;

   fv.xyz[0] = SO->VolPar->xorg + iv.ijk[0] * SO->VolPar->dx ;
   fv.xyz[1] = SO->VolPar->yorg + iv.ijk[1] * SO->VolPar->dy ;
   fv.xyz[2] = SO->VolPar->zorg + iv.ijk[2] * SO->VolPar->dz ;
   SUMA_RETURN(fv) ;
}

/*!--------------------------------------------------------------------*/

THD_fvec3 SUMA_THD_3dmm_to_3dfind( SUMA_SurfaceObject *SO ,
                              THD_fvec3 fv )
{
   static char FuncName[]={"SUMA_THD_3dmm_to_3dfind"};
   THD_fvec3     iv ;

   SUMA_ENTRY;


   iv.xyz[0] = (fv.xyz[0] - SO->VolPar->xorg) / SO->VolPar->dx ;
   iv.xyz[1] = (fv.xyz[1] - SO->VolPar->yorg) / SO->VolPar->dy ;
   iv.xyz[2] = (fv.xyz[2] - SO->VolPar->zorg) / SO->VolPar->dz ;

        if( iv.xyz[0] < 0            ) iv.xyz[0] = 0 ;
   else if( iv.xyz[0] > SO->VolPar->nx-1 ) iv.xyz[0] = SO->VolPar->nx-1 ;

        if( iv.xyz[1] <  0           ) iv.xyz[1] = 0 ;
   else if( iv.xyz[1] > SO->VolPar->ny-1 ) iv.xyz[1] = SO->VolPar->ny-1 ;

        if( iv.xyz[2] < 0            ) iv.xyz[2] = 0 ;
   else if( iv.xyz[2] > SO->VolPar->nz-1 ) iv.xyz[2] = SO->VolPar->nz-1 ;

   SUMA_RETURN(iv) ;
}

/*!--------------------------------------------------------------------*/

THD_ivec3 SUMA_THD_3dmm_to_3dind( SUMA_SurfaceObject *SO  ,
                             THD_fvec3 fv )
{
   static char FuncName[]={"SUMA_THD_3dmm_to_3dind"};
   THD_ivec3     iv ;

   SUMA_ENTRY;

   iv.ijk[0] = (fv.xyz[0] - SO->VolPar->xorg) / SO->VolPar->dx + 0.499 ;
   iv.ijk[1] = (fv.xyz[1] - SO->VolPar->yorg) / SO->VolPar->dy + 0.499 ;
   iv.ijk[2] = (fv.xyz[2] - SO->VolPar->zorg) / SO->VolPar->dz + 0.499 ;

        if( iv.ijk[0] < 0            ) iv.ijk[0] = 0 ;
   else if( iv.ijk[0] > SO->VolPar->nx-1 ) iv.ijk[0] = SO->VolPar->nx-1 ;

        if( iv.ijk[1] < 0            ) iv.ijk[1] = 0 ;
   else if( iv.ijk[1] > SO->VolPar->ny-1 ) iv.ijk[1] = SO->VolPar->ny-1 ;

        if( iv.ijk[2] < 0            ) iv.ijk[2] = 0 ;
   else if( iv.ijk[2] > SO->VolPar->nz-1 ) iv.ijk[2] = SO->VolPar->nz-1 ;

   SUMA_RETURN(iv) ;
}

THD_ivec3 SUMA_THD_3dmm_to_3dind_warn( SUMA_SurfaceObject *SO  ,
                             THD_fvec3 fv , int *out)
{
   static char FuncName[]={"SUMA_THD_3dmm_to_3dind_warn"};
   THD_ivec3     iv ;

   SUMA_ENTRY;
   *out = 0;
   
   iv.ijk[0] = (fv.xyz[0] - SO->VolPar->xorg) / SO->VolPar->dx + 0.499 ;
   iv.ijk[1] = (fv.xyz[1] - SO->VolPar->yorg) / SO->VolPar->dy + 0.499 ;
   iv.ijk[2] = (fv.xyz[2] - SO->VolPar->zorg) / SO->VolPar->dz + 0.499 ;

        if( iv.ijk[0] < 0            ) { iv.ijk[0] = 0 ; *out = 1; }
   else if( iv.ijk[0] > SO->VolPar->nx-1 ) { iv.ijk[0] = SO->VolPar->nx-1 ; *out = 1; }

        if( iv.ijk[1] < 0            ) { iv.ijk[1] = 0 ; *out = 1; }
   else if( iv.ijk[1] > SO->VolPar->ny-1 ) { iv.ijk[1] = SO->VolPar->ny-1 ; *out = 1; }

        if( iv.ijk[2] < 0            ) { iv.ijk[2] = 0 ; *out = 1; }
   else if( iv.ijk[2] > SO->VolPar->nz-1 ) { iv.ijk[2] = SO->VolPar->nz-1 ; *out = 1; }

   SUMA_RETURN(iv) ;
}
/*! 
   \brief how many voxels in each of the RL AP IS directions
*/
void SUMA_VolDims(THD_3dim_dataset *dset, int *nRL, int *nAP, int *nIS)
{
   static char FuncName[]={"SUMA_VolDims"};
   
   SUMA_ENTRY;
   
   *nRL = *nAP = *nIS = -1;
   
   if (!dset) {
      SUMA_SL_Err("NULL dset");
      SUMA_RETURNe;
   }
   
   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: *nRL = DSET_NX(dset) ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: *nAP = DSET_NX(dset) ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: *nIS = DSET_NX(dset) ; break ;
   }

   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: *nRL = DSET_NY(dset) ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: *nAP = DSET_NY(dset) ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: *nIS = DSET_NY(dset) ; break ;
   }

   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: *nRL = DSET_NZ(dset) ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: *nAP = DSET_NZ(dset) ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: *nIS = DSET_NZ(dset) ; break ;
   }
   
   SUMA_RETURNe;
}

/*!---------------------------------------------------------------------
   convert from input image oriented x,y,z to Dicom x,y,z
     (x axis = R->L , y axis = A->P , z axis = I->S)

   N.B.: image distances are oriented the same as Dicom,
         just in a permuted order.
-----------------------------------------------------------------------*/
THD_fvec3 SUMA_THD_3dmm_to_dicomm( int xxorient, int yyorient, int zzorient, 
                              THD_fvec3 imv )
{
   static char FuncName[]={"SUMA_THD_3dmm_to_dicomm"};   
   THD_fvec3 dicv ;
   float xim,yim,zim , xdic = 0.0, ydic = 0.0, zdic = 0.0 ;

   SUMA_ENTRY;

   xim = imv.xyz[0] ; yim = imv.xyz[1] ; zim = imv.xyz[2] ;

   switch( xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xdic = xim ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: ydic = xim ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zdic = xim ; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_3dmm_to_dicomm: illegal xxorient code.\n Exiting.") ;
         exit (1);
   }

   switch( yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xdic = yim ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: ydic = yim ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zdic = yim ; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_3dmm_to_dicomm: illegal xxorient code.\n Exiting.") ;
         exit (1);
   }

   switch( zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xdic = zim ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: ydic = zim ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zdic = zim ; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_3dmm_to_dicomm: illegal xxorient code.\n Exiting.") ;
         exit (1);
   }

   dicv.xyz[0] = xdic ; dicv.xyz[1] = ydic ; dicv.xyz[2] = zdic ;
   SUMA_RETURN(dicv) ;
}

/*!---------------------------------------------------------------------
   convert to input image oriented x,y,z from Dicom x,y,z
-----------------------------------------------------------------------*/

THD_fvec3 SUMA_THD_dicomm_to_3dmm( SUMA_SurfaceObject *SO ,
                              THD_fvec3 dicv )
{
   static char FuncName[]={"SUMA_THD_dicomm_to_3dmm"};
   THD_fvec3 imv ;
   float xim,yim,zim , xdic,ydic,zdic ;

   SUMA_ENTRY;

   xdic = dicv.xyz[0] ; ydic = dicv.xyz[1] ; zdic = dicv.xyz[2] ;

   switch( SO->VolPar->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: xim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: xim = zdic ; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_dicomm_to_3dmm: illegal xxorient code.\n Exiting.") ;
         exit (1);
   }

   switch( SO->VolPar->yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: yim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: yim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: yim = zdic ; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_dicomm_to_3dmm: illegal xxorient code.\n Exiting.") ;
         exit (1);
   }

   switch( SO->VolPar->zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: zim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: zim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zim = zdic ; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_dicomm_to_3dmm: illegal xxorient code.\n Exiting.") ;
         exit (1);
   }

   imv.xyz[0] = xim ; imv.xyz[1] = yim ; imv.xyz[2] = zim ;
   SUMA_RETURN(imv) ;
}
/*!
   \brief changes orientation codes to string
   classic RAI codes: ORI_R2L_TYPE, ORI_A2P_TYPE, ORI_I2S_TYPE
   would result in orstr being set to: RAILPS 
*/
void SUMA_orcode_to_orstring (int xxorient,int  yyorient,int zzorient, char *orstr)
{
   static char FuncName[]={"SUMA_orcode_to_orstring"};
   
   SUMA_ENTRY;
   
   if (!orstr) { SUMA_SL_Err("NULL string"); SUMA_RETURNe; }
   
   orstr[0]='\0';
   switch( xxorient ){
      case ORI_R2L_TYPE: orstr[0] = 'R'; orstr[3] = 'L'; break ;
      case ORI_L2R_TYPE: orstr[0] = 'L'; orstr[3] = 'R'; break ;
      case ORI_P2A_TYPE: orstr[0] = 'P'; orstr[3] = 'A'; break ;
      case ORI_A2P_TYPE: orstr[0] = 'A'; orstr[3] = 'P'; break ;
      case ORI_I2S_TYPE: orstr[0] = 'I'; orstr[3] = 'S'; break ;
      case ORI_S2I_TYPE: orstr[0] = 'S'; orstr[3] = 'I'; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_dicomm_to_3dmm: illegal xxorient code.\n") ;
         SUMA_RETURNe;
   }

   switch( yyorient ){
      case ORI_R2L_TYPE: orstr[1] = 'R'; orstr[4] = 'L'; break ;
      case ORI_L2R_TYPE: orstr[1] = 'L'; orstr[4] = 'R'; break ;
      case ORI_P2A_TYPE: orstr[1] = 'P'; orstr[4] = 'A'; break ;
      case ORI_A2P_TYPE: orstr[1] = 'A'; orstr[4] = 'P'; break ;
      case ORI_I2S_TYPE: orstr[1] = 'I'; orstr[4] = 'S'; break ;
      case ORI_S2I_TYPE: orstr[1] = 'S'; orstr[4] = 'I'; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_dicomm_to_3dmm: illegal yyorient code.\n ") ;
         SUMA_RETURNe;
   }

   switch( zzorient ){
      case ORI_R2L_TYPE: orstr[2] = 'R'; orstr[5] = 'L'; break ;
      case ORI_L2R_TYPE: orstr[2] = 'L'; orstr[5] = 'R'; break ;
      case ORI_P2A_TYPE: orstr[2] = 'P'; orstr[5] = 'A'; break ;
      case ORI_A2P_TYPE: orstr[2] = 'A'; orstr[5] = 'P'; break ;
      case ORI_I2S_TYPE: orstr[2] = 'I'; orstr[5] = 'S'; break ;
      case ORI_S2I_TYPE: orstr[2] = 'S'; orstr[5] = 'I'; break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_THD_dicomm_to_3dmm: illegal zzorient code.\n ") ;
         SUMA_RETURNe;
   }
   
   SUMA_RETURNe;
}
SUMA_Boolean SUMA_orstring_to_orcode (char *orstr, int *orient)
{
   static char FuncName[]={"SUMA_orstring_to_orcode"};
   int i;
   
   SUMA_ENTRY;
   
   if (!orstr) { SUMA_SL_Err("NULL string"); SUMA_RETURN(NOPE); }
   if (!SUMA_ok_orstring(orstr)) { SUMA_SL_Err("Bad orientation string"); SUMA_RETURN(NOPE); }
   for (i=0; i<3; ++i) {
      switch (orstr[i]) {
         case 'R': orient[i] = ORI_R2L_TYPE; break;
         case 'L': orient[i] = ORI_L2R_TYPE; break;
         case 'A': orient[i] = ORI_A2P_TYPE; break;
         case 'P': orient[i] = ORI_P2A_TYPE; break;
         case 'I': orient[i] = ORI_I2S_TYPE; break;
         case 'S': orient[i] = ORI_S2I_TYPE; break;
         default: fprintf(SUMA_STDERR, " SUMA_orstring_to_orcode: Bad to the bones\n"); SUMA_RETURN(NOPE); 
      }
   }
   
   SUMA_RETURN(YUP);
}

int SUMA_ok_orstring (char *orstr)
{
   static char FuncName[]={"SUMA_ok_orstring"};
   int i, d[3];
   
   SUMA_ENTRY;
   
   if (!orstr) { SUMA_RETURN(NOPE); }
   d[0] = d[1] = d[2] = 0;
   for (i=0; i<3; ++i) {
      switch (orstr[i]) {
         case 'R': ++(d[0]); break;
         case 'L': ++(d[0]); break;
         case 'A': ++(d[1]); break;
         case 'P': ++(d[1]); break;
         case 'I': ++(d[2]); break;
         case 'S': ++(d[2]); break;
         default: SUMA_RETURN(NOPE); 
      }
   }
   if (d[0] != 1 || d[1] != 1 || d[2] != 1) SUMA_RETURN(NOPE);
    
   SUMA_RETURN(YUP);
}
int SUMA_flip_orient(int xxorient)
{
   static char FuncName[]={"SUMA_flip_orient"};
   
   SUMA_ENTRY;
   
   switch( xxorient ){
      case ORI_R2L_TYPE: SUMA_RETURN(ORI_L2R_TYPE); break ;
      case ORI_L2R_TYPE: SUMA_RETURN(ORI_R2L_TYPE); break ;
      
      case ORI_P2A_TYPE: SUMA_RETURN(ORI_A2P_TYPE); break ;
      case ORI_A2P_TYPE: SUMA_RETURN(ORI_P2A_TYPE); break ;
      
      case ORI_I2S_TYPE: SUMA_RETURN(ORI_S2I_TYPE); break ;
      case ORI_S2I_TYPE: SUMA_RETURN(ORI_I2S_TYPE); break ;

      default: 
         fprintf(SUMA_STDERR, "SUMA_opposite_orient: illegal zzorient code.\n ") ;
         SUMA_RETURN(-1);
   }
   
   SUMA_RETURN(-1);
   
}

SUMA_Boolean SUMA_CoordChange (char *orc_in, char *orc_out, float *XYZ, int N_xyz)
{
   static char FuncName[]={"SUMA_CoordChange"};
   int i, or_in[3], or_out[3], j, map[3], sgn[3], i3;
   float xyz[3];
   
   SUMA_ENTRY;
   
   if (!SUMA_orstring_to_orcode(orc_in, or_in)) {
      SUMA_SL_Err("Bad in code");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_orstring_to_orcode(orc_out, or_out)) {
      SUMA_SL_Err("Bad out code");
      SUMA_RETURN(NOPE);
   }
   
   /* figure out the mapping */
   for (j=0; j<3; ++j) { 
      i = 0;
      while (i<3) {
         if (or_in[i] == or_out[j] || or_in[i] == SUMA_flip_orient(or_out[j])) {
            map[j] = i;
            if (or_in[i] == SUMA_flip_orient(or_out[j])) sgn[j] = -1;
            else sgn[j] = 1;
            i = 3; /* break */
         }
         ++i;
      }
   }

   for (i=0; i<N_xyz; ++i) {
      i3 = 3*i;
      xyz[0] = XYZ[i3]; xyz[1] = XYZ[i3+1]; xyz[2] = XYZ[i3+2];
      XYZ[i3  ] = sgn[0] * xyz[map[0]]; 
      XYZ[i3+1] = sgn[1] * xyz[map[1]]; 
      XYZ[i3+2] = sgn[2] * xyz[map[2]]; 
   }
   
   SUMA_RETURN(YUP);
}
/*!
   \brief takes the origin as entered to to3d and 
   changes them to the origin field for AFNI header
   Based on info in README.attributes and function T3D_save_file_CB
   int to3d.c
*/ 
void SUMA_originto3d_2_originHEAD(THD_ivec3 orient, THD_fvec3 *origin)
{
   static char FuncName[]={"SUMA_originto3d_2_originHEAD"};
   
   SUMA_ENTRY;
   
   origin->xyz[0] = (ORIENT_sign[orient.ijk[0]] == '+')
                  ? (-origin->xyz[0]) : ( origin->xyz[0]) ;

   origin->xyz[1] = (ORIENT_sign[orient.ijk[1]] == '+')
                  ? (-origin->xyz[1]) : ( origin->xyz[1]) ;
   
   origin->xyz[2] = (ORIENT_sign[orient.ijk[2]] == '+')
                  ? (-origin->xyz[2]) : ( origin->xyz[2]) ;
   
   SUMA_RETURNe;
   
} 
/*!
   \brief takes the size as entered to to3d and 
   changes them to the delta field for AFNI header
   Based on info in README.attributes and function T3D_save_file_CB
   int to3d.c
*/ 

void SUMA_sizeto3d_2_deltaHEAD(THD_ivec3 orient, THD_fvec3 *delta)                 
{
   static char FuncName[]={"SUMA_sizeto3d_2_deltaHEAD"};
   
   SUMA_ENTRY;
   
   delta->xyz[0] = (ORIENT_sign[orient.ijk[0]] == '+')
                  ? (delta->xyz[0]) : ( -delta->xyz[0]) ;

   delta->xyz[1] = (ORIENT_sign[orient.ijk[1]] == '+')
                  ? (delta->xyz[1]) : ( -delta->xyz[1]) ;
   
   delta->xyz[2] = (ORIENT_sign[orient.ijk[2]] == '+')
                  ? (delta->xyz[2]) : ( -delta->xyz[2]) ;
   
   SUMA_RETURNe;
   
}    

/*!
   \brief SUMA_Boolean SUMA_vec_3dfind_to_3dmm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
   SUMA_Boolean SUMA_vec_3dmm_to_3dfind (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
   SUMA_Boolean SUMA_vec_dicomm_to_3dfind (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
   SUMA_Boolean SUMA_vec_3dfind_to_dicomm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
   SUMA_Boolean SUMA_vec_3dmm_to_dicomm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
   SUMA_Boolean SUMA_vec_dicomm_to_3dmm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
   a set of functions to change coordinate systems.
   
   \param NodeList (float *) vector of consecutive xyz (N_Node x 3) values
   \param N_Node (int) number of xyz triplets in NodeList
   \param VolPar (SUMA_VOLPAR *) volume parent structure (output of SUMA_VolPar_Attr )
   
   3dfind = float voxel/node index coordinate
   3dmm   = float voxel/node coordinate in volume space
   dicomm = float voxel/node coordinate in RAI space
   
   see also SUMA_THD_ functions ...
*/ 
SUMA_Boolean SUMA_vec_3dfind_to_3dmm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
{
   static char FuncName[]={"SUMA_vec_3dfind_to_3dmm"};
   THD_fvec3 fv, iv;
   int i, id;
   SUMA_SurfaceObject SO;
   
   SUMA_ENTRY;

   if (!NodeList || !VolPar) { SUMA_SL_Err("Null NodeList || Null VolPar"); SUMA_RETURN(NOPE); }
   /* create dummy struct */
   SO.NodeList = NodeList; SO.N_Node = N_Node; SO.VolPar = VolPar; SO.NodeDim = 3;
   
   for (i=0; i < SO.N_Node; ++i) {
      id = i * SO.NodeDim;
      /* change float indices to mm coords */
      iv.xyz[0] = SO.NodeList[id] ;
      iv.xyz[1] = SO.NodeList[id+1] ;
      iv.xyz[2] = SO.NodeList[id+2] ;
      fv = SUMA_THD_3dfind_to_3dmm( &SO, iv );
      SO.NodeList[id] = fv.xyz[0];
      SO.NodeList[id+1] = fv.xyz[1];
      SO.NodeList[id+2] = fv.xyz[2];
   }

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_vec_3dmm_to_3dfind (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
{
   static char FuncName[]={"SUMA_vec_3dmm_to_3dfind"};
   THD_fvec3 fv, iv;
   int i, id;
   SUMA_SurfaceObject SO;

   SUMA_ENTRY;

   if (!NodeList || !VolPar) { SUMA_SL_Err("Null NodeList || Null VolPar"); SUMA_RETURN(NOPE); }
   /* create dummy struct */
   SO.NodeList = NodeList; SO.N_Node = N_Node; SO.VolPar = VolPar; SO.NodeDim = 3;

   for (i=0; i < SO.N_Node; ++i) {
      id = i * SO.NodeDim;
      /* change float indices to mm coords */
      iv.xyz[0] = SO.NodeList[id] ;
      iv.xyz[1] = SO.NodeList[id+1] ;
      iv.xyz[2] = SO.NodeList[id+2] ;
      fv = SUMA_THD_3dmm_to_3dfind( &SO, iv );
      /* fprintf(SUMA_STDERR,"%s: In[%f %f %f] Out[%f %f %f]\n", 
                        FuncName, iv.xyz[0], iv.xyz[1], iv.xyz[2], 
                        fv.xyz[0], fv.xyz[1], fv.xyz[2]); */
      SO.NodeList[id] = fv.xyz[0];
      SO.NodeList[id+1] = fv.xyz[1];
      SO.NodeList[id+2] = fv.xyz[2];
   }

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_vec_dicomm_to_3dfind (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
{
   static char FuncName[]={"SUMA_vec_dicomm_to_3dfind"};

   SUMA_ENTRY;

   if (!NodeList || !VolPar) { SUMA_SL_Err("Null NodeList || Null VolPar"); SUMA_RETURN(NOPE); }
   
   
   if (!SUMA_vec_dicomm_to_3dmm(NodeList, N_Node, VolPar)) { SUMA_RETURN(NOPE); }
   if (!SUMA_vec_3dmm_to_3dfind(NodeList, N_Node, VolPar)) { SUMA_RETURN(NOPE); }

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_vec_3dfind_to_dicomm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
{
   static char FuncName[]={"SUMA_vec_3dfind_to_dicomm"};

   SUMA_ENTRY;

   if (!NodeList || !VolPar) { SUMA_SL_Err("Null NodeList || Null VolPar"); SUMA_RETURN(NOPE); }

   if (!SUMA_vec_3dfind_to_3dmm(NodeList, N_Node, VolPar)) { SUMA_RETURN(NOPE); }
   if (!SUMA_vec_3dmm_to_dicomm(NodeList, N_Node, VolPar)) { SUMA_RETURN(NOPE); }

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_vec_3dmm_to_dicomm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
{
   static char FuncName[]={"SUMA_vec_3dmm_to_dicomm"};
   THD_fvec3 fv, iv;
   int i, id;
   SUMA_SurfaceObject SO;
   
   SUMA_ENTRY;

   if (!NodeList || !VolPar) { SUMA_SL_Err("Null NodeList || Null VolPar"); SUMA_RETURN(NOPE); }
   /* create dummy struct */
   SO.NodeList = NodeList; SO.N_Node = N_Node; SO.VolPar = VolPar; SO.NodeDim = 3;

   for (i=0; i < SO.N_Node; ++i) {
      id = i * SO.NodeDim;
      iv.xyz[0] = SO.NodeList[id] ;
      iv.xyz[1] = SO.NodeList[id+1] ;
      iv.xyz[2] = SO.NodeList[id+2] ;

      /* change mm to RAI coords */
      fv = SUMA_THD_3dmm_to_dicomm( SO.VolPar->xxorient, SO.VolPar->yyorient, SO.VolPar->zzorient,  iv );
      SO.NodeList[id] = fv.xyz[0];
      SO.NodeList[id+1] = fv.xyz[1];
      SO.NodeList[id+2] = fv.xyz[2];
   }

   SUMA_RETURN(YUP);
}   

SUMA_Boolean SUMA_vec_dicomm_to_3dmm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar)
{
   static char FuncName[]={"SUMA_vec_dicomm_to_3dmm"};
   THD_fvec3 fv, iv;
   int i, id;
   SUMA_SurfaceObject SO;

   SUMA_ENTRY;

   if (!NodeList || !VolPar) { SUMA_SL_Err("Null NodeList || Null VolPar"); SUMA_RETURN(NOPE); }
   /* create dummy struct */
   SO.NodeList = NodeList; SO.N_Node = N_Node; SO.VolPar = VolPar; SO.NodeDim = 3;

   for (i=0; i < SO.N_Node; ++i) {
      id = i * SO.NodeDim;

      iv.xyz[0] = SO.NodeList[id] ;
      iv.xyz[1] = SO.NodeList[id+1] ;
      iv.xyz[2] = SO.NodeList[id+2] ;

      /* change mm to RAI coords */
      fv = SUMA_THD_dicomm_to_3dmm( &SO, iv );
      /* fprintf(SUMA_STDERR,"%s: In[%f %f %f] Out[%f %f %f]\n", 
                        FuncName, iv.xyz[0], iv.xyz[1], iv.xyz[2], 
                        fv.xyz[0], fv.xyz[1], fv.xyz[2]); */
      SO.NodeList[id] = fv.xyz[0];
      SO.NodeList[id+1] = fv.xyz[1];
      SO.NodeList[id+2] = fv.xyz[2];
   }

   SUMA_RETURN(YUP);
}


/*!
   \brief transforms XYZ coordinates from  AFNI'S RAI 
   talairach space to MNI space in in RAI or (LPI) 
   http://www.mrc-cbu.cam.ac.uk/Imaging/mnispace.html.
   see Bob's THD_tta_to_mni in thd_mnicoords.c
   
   \param NodeList (float *) vector of coordinates, 3 * N_Node long
   \param N_Node (int) number of nodes in vector above
   \param Coord (char *) one of two options
                  "RAI" meaning the MNI coordinates are to be in RAI
                  "LPI" meaning the MNI coordinates are to be in LPI
   \return flag (SUMA_Boolean) NOPE = failed
   
   - This function should be rewritten to call THD_tta_to_mni (although you'll have to deal 
   with the flipping option
   
   
*/
SUMA_Boolean SUMA_AFNItlrc_toMNI(float *NodeList, int N_Node, char *Coord)
{
   static char FuncName[]={"SUMA_AFNItlrc_toMNI"};
   SUMA_Boolean DoFlip = NOPE;
   int i, i3;
   float mx = 0.0,my = 0.0,mz  = 0.0, tx = 0.0,ty = 0.0,tz = 0.0 ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (strcmp(Coord,"RAI") == 0) DoFlip = NOPE;
   else if (strcmp(Coord,"LPI") == 0) DoFlip = YUP;
   else {
      SUMA_S_Err("Can't do. Either RAI or LPI");
      SUMA_RETURN(NOPE);
   }   

   
   for (i=0; i< N_Node; ++i) {
      i3 = 3*i;
      if (DoFlip) {
         if (!i) SUMA_LH("Flipping to LPI");
         tx = - NodeList[i3]; ty = -NodeList[i3+1] ;  /* flip xy from RAI to LPI */
         tz = NodeList[i3+2] ;
      } else {
         if (!i) SUMA_LH("No Flipping, RAI maintained");
         tx =  NodeList[i3]; ty = NodeList[i3+1] ;  /* flip xy from RAI to LPI */
         tz =  NodeList[i3+2] ;
      }
      mx = 1.01010 * tx ;
      my = 1.02962 * ty - 0.05154 * tz ;
      mz = 0.05434 * ty + 1.08554 * tz ;
      if( mz < 0.0 ) mz *= 1.09523 ;
      NodeList[i3] = mx;
      NodeList[i3+1] = my;
      NodeList[i3+2] = mz;
   }
   
   
   SUMA_RETURN(YUP);
}
/*!

   \brief transforms XYZ coordinates by transfrom in warp.
   ans = SUMA_AFNI_forward_warp_xyz( warp , XYZv,  N);
   
   \param warp (THD_warp *) afni warp structure
   \param XYZv (float *) pointer to coordinates vector.
   \param N (int) number of XYZ triplets in XYZv.
      The ith X,Y, Z coordinates would be XYZv[3i], XYZv[3i+1],XYZv[3i+2]   
   \return ans (YUP/NOPE) NOPE: NULL warp or bad warp->type
   
   - The following functions are adapted from afni.c & Vecwarp.c
*/
SUMA_Boolean SUMA_AFNI_forward_warp_xyz( THD_warp * warp , float *xyzv, int N)
{
   static char FuncName[]={"SUMA_AFNI_forward_warp_xyz"};
   THD_fvec3 new_fv, old_fv;
   int i, i3;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if( warp == NULL ) {
      fprintf (SUMA_STDERR, "Error %s: NULL warp.\n", FuncName);
      SUMA_RETURN (NOPE) ;
   }

   switch( warp->type ){

      default: 
         fprintf (SUMA_STDERR, "Error %s: bad warp->type\n", FuncName);
         SUMA_RETURN (NOPE); 
         break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* forward transform each possible case,
            and test if result is in bot..top of defined map */

         for (i=0; i < N; ++i) {
            i3 = 3*i;
            old_fv.xyz[0] = xyzv[i3];
            old_fv.xyz[1] = xyzv[i3+1];
            old_fv.xyz[2] = xyzv[i3+2];
            
            for( iw=0 ; iw < 12 ; iw++ ){
               map    = warp->tal_12.warp[iw] ;
               new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;

               if( new_fv.xyz[0] >= map.bot.xyz[0] &&
                   new_fv.xyz[1] >= map.bot.xyz[1] &&
                   new_fv.xyz[2] >= map.bot.xyz[2] &&
                   new_fv.xyz[0] <= map.top.xyz[0] &&
                   new_fv.xyz[1] <= map.top.xyz[1] &&
                   new_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
            }
            xyzv[i3] = new_fv.xyz[0];
            xyzv[i3+1] = new_fv.xyz[1];
            xyzv[i3+2] = new_fv.xyz[2];
            
         }
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         SUMA_LH("Have WarpAffineType");
         if (LocalHead) {
            for (i=0; i < 3; ++i) fprintf(SUMA_STDERR,"%.5f  %.5f  %.5f  %.5f\n", 
                        map.mfor.mat[i][0], map.mfor.mat[i][1], map.mfor.mat[i][2], map.bvec.xyz[i]);
         }
         for (i=0; i < N; ++i) {
            i3 = 3*i;
            old_fv.xyz[0] = xyzv[i3];
            old_fv.xyz[1] = xyzv[i3+1];
            old_fv.xyz[2] = xyzv[i3+2];
            new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;
            xyzv[i3] = new_fv.xyz[0];
            xyzv[i3+1] = new_fv.xyz[1];
            xyzv[i3+2] = new_fv.xyz[2];
         }
      }
      break ;

   }
   SUMA_RETURN(YUP);
}
