/*! Dealings with volume data */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

/*! a copy of THD_handedness from ../thd_rotangles.c
Dunno why the original was giving me pain linking ... */
int SUMA_THD_handedness( THD_3dim_dataset * dset )
{
   THD_dataxes * dax ;
   THD_mat33 q ;
   int col ;
   float val ;


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
   \brief Return AFNI's prefix, also checks for its validity
   \param name (char *) dset name
   \param view (char[4]) array to return view in it
   \return prefix (char *) dset prefix, free it with SUMA_free
*/
char *SUMA_AfniPrefix(char *name, char *view) 
{
   static char FuncName[]={"SUMA_AfniPrefix"};
   char *tmp1 = NULL, *tmp2 = NULL, *prfx = NULL;
   
   SUMA_ENTRY;
   
   if (!name) SUMA_RETURN(prfx);
   
   tmp1 = SUMA_Extension(name, ".HEAD", YUP);
   tmp2 = SUMA_Extension(tmp1, ".BRIK", YUP); SUMA_free(tmp1); tmp1 = NULL;
   /* is there a dot ?*/
   if (tmp2[strlen(tmp2)-1] == '.') tmp2[strlen(tmp2)-1] = '\0';
   if (view) {
      if (SUMA_isExtension(tmp2, "+orig")) sprintf(view, "+orig");
      else if (SUMA_isExtension(tmp2, "+acpc")) sprintf(view, "+acpc");
      else if (SUMA_isExtension(tmp2, "+tlrc")) sprintf(view, "+tlrc");
   }
   tmp1 = SUMA_Extension(tmp2, "+orig", YUP); SUMA_free(tmp2); tmp2 = NULL;
   tmp2 = SUMA_Extension(tmp1, "+acpc", YUP); SUMA_free(tmp1); tmp1 = NULL;
   prfx = SUMA_Extension(tmp2, "+tlrc", YUP); SUMA_free(tmp2); tmp2 = NULL;
   if( !THD_filename_ok(prfx) ){
      SUMA_SL_Err("prefix contains forbidden characters!\n") ;
      SUMA_free(prfx); prfx = NULL;
      SUMA_RETURN(prfx);
   }
   
   SUMA_RETURN(prfx);
}
/*!
   \brief A function to find the skin of a volume
   \param dset (THD_3dim_dataset *) an AFNI volume
   \param dvec (double *) (nx * ny * nz) data vector
   \param thresh (double) consider only values in dvec > thresh
   \param N_skin (int *) number of voxels that are skin
   \return skin (byte *) (nx * ny * nz) vector containing 1 for skin voxels, 0 elsewhere.
*/
byte * SUMA_isSkin(THD_3dim_dataset *dset, double *dvec, double thresh, int *N_skin)
{
   static char FuncName[]={"SUMA_isSkin"};
   byte *isskin=NULL;
   int x, y, z, nx, ny, nz, i1D,  nxy;
  
   SUMA_ENTRY;
   
   if (!dset || !dvec) {
      SUMA_SL_Err("NULL input dset or dvec");
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
            if (dvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            ++x; 
         } while (x < nx && !isskin[i1D]);
         x = nx - 1;
         do { /* the downstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (dvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            --x; 
         } while (x >=0 && !isskin[i1D]);
      } /* y */
   } /* z */
   
   for (z=0; z<nz; ++z) {
      for (x=0; x<nx; ++x) {
         y = 0;
         do { /* the upstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (dvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            ++y; 
         } while (y < ny && !isskin[i1D]);
         y = ny - 1;
         do { /* the downstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (dvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            --y; 
         } while (y >=0 && !isskin[i1D]);
      } /* x */
   } /* z */
   
   for (x=0; x<nx; ++x) {
      for (y=0; y<ny; ++y) {
         z = 0;
         do { /* the upstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (dvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
            ++z; 
         } while (z < nz && !isskin[i1D]);
         z = nz - 1;
         do { /* the downstroke */
            i1D = SUMA_3D_2_1D_index(x, y, z, nx, nxy);
            if (dvec[i1D] > thresh) { isskin[i1D] = 1; ++ *N_skin; }
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
   SUMA_RETURN(VP);
}
SUMA_Boolean SUMA_Free_VolPar (SUMA_VOLPAR *VP)
{
   static char FuncName[]={"SUMA_Free_VolPar"};
   
   SUMA_ENTRY;

   if (VP->prefix != NULL) SUMA_free(VP->prefix);
   if (VP->filecode != NULL) SUMA_free(VP->filecode);
   if (VP->dirname != NULL) SUMA_free(VP->dirname);
   if (VP->idcode_str != NULL) SUMA_free(VP->idcode_str);
   if (VP->idcode_date != NULL) SUMA_free(VP->idcode_date);
   if (VP->VOLREG_CENTER_OLD != NULL) SUMA_free(VP->VOLREG_CENTER_OLD);
   if (VP->VOLREG_CENTER_BASE != NULL) SUMA_free(VP->VOLREG_CENTER_BASE);
   if (VP->VOLREG_MATVEC != NULL) SUMA_free(VP->VOLREG_MATVEC);
   if (VP->TAGALIGN_MATVEC != NULL) SUMA_free(VP->TAGALIGN_MATVEC);
   if (VP != NULL) SUMA_free(VP);
   SUMA_RETURN (YUP);
}

SUMA_VOLPAR *SUMA_VolParFromDset (THD_3dim_dataset *dset)
{
   ATR_float *atr;
   static char FuncName[]={"SUMA_VolParFromDset"};
   SUMA_VOLPAR *VP;
   int ii;
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
   VP->idcode_str = (char *)SUMA_malloc(ii+1);
   ii = strlen(dset->idcode.date);
   VP->idcode_date = (char *)SUMA_malloc(ii+1);
   if (VP->prefix == NULL || VP->filecode == NULL || VP->idcode_date == NULL || VP->dirname == NULL || VP->idcode_str == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for strings. Kill me, please.\n", FuncName);
      SUMA_Free_VolPar(VP);
      SUMA_RETURN (NULL);
   }
   VP->prefix = strcpy(VP->prefix, DSET_PREFIX(dset));
   VP->filecode = strcpy(VP->filecode, DSET_FILECODE(dset));
   VP->dirname = strcpy(VP->dirname, DSET_DIRNAME(dset));
   VP->idcode_str = strcpy(VP->idcode_str, dset->idcode.str);
   VP->idcode_date = strcpy(VP->idcode_date, dset->idcode.date);
   VP->xxorient = dset->daxes->xxorient;
   VP->yyorient = dset->daxes->yyorient;
   VP->zzorient = dset->daxes->zzorient;

   if (LocalHead) {      
      fprintf (SUMA_STDERR,"%s: dset->idcode_str = %s\n", FuncName, dset->idcode.str);
      fprintf (SUMA_STDERR,"%s: VP->idcode_str = %s\n", FuncName, VP->idcode_str);
   }
   /* Get the tagalign matrix if possible*/
   atr = THD_find_float_atr( dset->dblk , "TAGALIGN_MATVEC" ) ;
   if (atr == NULL) {
      VP->TAGALIGN_MATVEC = NULL;
   }else {
      VP->TAGALIGN_MATVEC = (float *)SUMA_calloc(12, sizeof(float));
      if (VP->TAGALIGN_MATVEC != NULL) {
         if (atr->nfl == 12) {
            for (ii=0; ii<12; ++ii) VP->TAGALIGN_MATVEC[ii] = atr->fl[ii];
         } else {   
            fprintf(SUMA_STDERR,"Error %s: TAGALIGN_MATVEC does not have 12 elements.\n", FuncName);
         }
      } else {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VP->TAGALIGN_MATVEC\n", FuncName);
      }
   }
   
   /* Get the volreg matrix if possible*/
   atr = THD_find_float_atr( dset->dblk , "VOLREG_MATVEC_000000" ) ;
   if (atr == NULL) {
      VP->VOLREG_MATVEC = NULL;
   }else {
      VP->VOLREG_MATVEC = (float *)SUMA_calloc(12, sizeof(float));
      if (VP->VOLREG_MATVEC != NULL) {
         if (atr->nfl == 12) {
            for (ii=0; ii<12; ++ii) VP->VOLREG_MATVEC[ii] = atr->fl[ii];
         } else {   
            fprintf(SUMA_STDERR,"Error %s: VOLREG_MATVEC_000000 does not have 12 elements.\n", FuncName);
         }
      } else {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VP->VOLREG_MATVEC\n", FuncName);
      }
   }
   /* Get the center base coordinates */
   atr = THD_find_float_atr( dset->dblk , "VOLREG_CENTER_BASE");
   if (atr == NULL) {
      VP->VOLREG_CENTER_BASE = NULL;
   } else {
      VP->VOLREG_CENTER_BASE = (float *)SUMA_calloc(3, sizeof(float));
      if (VP->VOLREG_CENTER_BASE != NULL) {
         if (atr->nfl == 3) {
            for (ii=0; ii<3; ++ii) VP->VOLREG_CENTER_BASE[ii] = atr->fl[ii];
         } else {   
            fprintf(SUMA_STDERR,"Error %s: VOLREG_CENTER_BASE does not have 12 elements.\n", FuncName);
         }
      } else {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VP->VOLREG_CENTER_BASE\n", FuncName);
      }
   }
   
   /* VOLREG_CENTER_OLD  */
   atr = THD_find_float_atr( dset->dblk , "VOLREG_CENTER_OLD");
   if (atr == NULL) {
      VP->VOLREG_CENTER_OLD = NULL;
   } else {
      VP->VOLREG_CENTER_OLD = (float *)SUMA_calloc(3, sizeof(float));
      if (VP->VOLREG_CENTER_OLD != NULL) {
         if (atr->nfl == 3) {
            for (ii=0; ii<3; ++ii) VP->VOLREG_CENTER_OLD[ii] = atr->fl[ii];
         } else {   
            fprintf(SUMA_STDERR,"Error %s: VOLREG_CENTER_OLD does not have 12 elements.\n", FuncName);
         }
      } else {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VP->VOLREG_CENTER_OLD\n", FuncName);
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
      fprintf (SUMA_STDERR,"Error %s: Could not read %s\n", FuncName, volparent_name);
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
      sprintf (stmp,"prefix: %s\tfilecode: %s\tdirname: %s\nId code str:%s\tID code date: %s\n", \
         VP->prefix, VP->filecode, VP->dirname, VP->idcode_str, VP->idcode_date);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"isanat: %d\n", VP->isanat);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"Orientation: %d %d %d\n", \
         VP->xxorient, VP->yyorient, VP->zzorient);
      if (VP->Hand == 1) SS = SUMA_StringAppend (SS, "Right Hand Coordinate System.\n");
      else if (VP->Hand == -1) SS = SUMA_StringAppend (SS, "Left Hand Coordinate System.\n");
      else SS = SUMA_StringAppend (SS, "No hand coordinate system!\n");
      
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"Origin: %f %f %f\n", \
         VP->xorg, VP->yorg, VP->zorg);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"Delta: %f %f %f\n", \
         VP->dx, VP->dy, VP->dz);
      SS = SUMA_StringAppend (SS, stmp);
      sprintf (stmp,"N: %d %d %d\n",\
         VP->nx, VP->ny, VP->nz);
      SS = SUMA_StringAppend (SS, stmp);

      if (VP->TAGALIGN_MATVEC != NULL) {
         sprintf (stmp,"VP->TAGALIGN_MATVEC = \n\tMrot\tDelta\n");
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", \
         VP->TAGALIGN_MATVEC[0], VP->TAGALIGN_MATVEC[1], VP->TAGALIGN_MATVEC[2], VP->TAGALIGN_MATVEC[3]); 
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", \
         VP->TAGALIGN_MATVEC[4], VP->TAGALIGN_MATVEC[5], VP->TAGALIGN_MATVEC[6], VP->TAGALIGN_MATVEC[7]);
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", \
         VP->TAGALIGN_MATVEC[8], VP->TAGALIGN_MATVEC[9], VP->TAGALIGN_MATVEC[10], VP->TAGALIGN_MATVEC[11]);
         SS = SUMA_StringAppend (SS, stmp);
      } else {
         sprintf (stmp,"VP->TAGALIGN_MATVEC = NULL\n");
         SS = SUMA_StringAppend (SS, stmp);
      }      
      
      if (VP->VOLREG_MATVEC != NULL) {
         sprintf (stmp,"VP->VOLREG_MATVEC = \n\tMrot\tDelta\n");
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", \
         VP->VOLREG_MATVEC[0], VP->VOLREG_MATVEC[1], VP->VOLREG_MATVEC[2], VP->VOLREG_MATVEC[3]); 
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", \
         VP->VOLREG_MATVEC[4], VP->VOLREG_MATVEC[5], VP->VOLREG_MATVEC[6], VP->VOLREG_MATVEC[7]);
         SS = SUMA_StringAppend (SS, stmp);
         sprintf (stmp,"|%f\t%f\t%f|\t|%f|\n", \
         VP->VOLREG_MATVEC[8], VP->VOLREG_MATVEC[9], VP->VOLREG_MATVEC[10], VP->VOLREG_MATVEC[11]);
         SS = SUMA_StringAppend (SS, stmp);
      } else {
         sprintf (stmp,"VP->VOLREG_MATVEC = NULL\n");
         SS = SUMA_StringAppend (SS, stmp);
      }

      if (VP->VOLREG_CENTER_OLD != NULL) {
         sprintf (stmp,"VP->VOLREG_CENTER_OLD = %f, %f, %f\n", \
           VP->VOLREG_CENTER_OLD[0], VP->VOLREG_CENTER_OLD[1], VP->VOLREG_CENTER_OLD[2]); 
         SS = SUMA_StringAppend (SS, stmp);
      }else {
         sprintf (stmp,"VP->VOLREG_CENTER_OLD = NULL\n");
         SS = SUMA_StringAppend (SS, stmp);
      }

      if (VP->VOLREG_CENTER_BASE != NULL) {
         sprintf (stmp,"VP->VOLREG_CENTER_BASE = %f, %f, %f\n", \
            VP->VOLREG_CENTER_BASE[0], VP->VOLREG_CENTER_BASE[1], VP->VOLREG_CENTER_BASE[2]); 
         SS = SUMA_StringAppend (SS, stmp);
      } else {
         sprintf (stmp,"VP->VOLREG_CENTER_BASE = NULL\n");
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
      case SUMA_PLY:
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
            THD_coorder_to_dicom (cord_surf, &(SO->NodeList[id]), &(SO->NodeList[id+1]), &(SO->NodeList[id+2])); 
         }
         break;
      case SUMA_SUREFIT:
         /* For SureFit, coordinates are actually a float version of the indices */
         SF = (SUMA_SureFit_struct *)S_Struct;
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
         break;
      case SUMA_BRAIN_VOYAGER:
         /* For Brain Voyager, all you need to do is 
          go from AIR to RAI (DICOM)
          Note: The center of the volume is at the 1st voxel's center and that huge
          center shift, relative to standard AFNI dsets (centered about middle of volume)
          might throw off 3dVolreg. If you want to shift volume's center to be in
          the middle voxel, you'll need to shift the surface coordinates before transforming
          them to ASR*/
         sprintf(orcode,"ASR");
         THD_coorder_fill(orcode , cord_surf); 
         /*loop over XYZs and change them to dicom*/
         for (i=0; i < SO->N_Node; ++i) {
            id = i * ND;
            THD_coorder_to_dicom (cord_surf, &(SO->NodeList[id]), &(SO->NodeList[id+1]), &(SO->NodeList[id+2])); 
         }
         break;
      default:
         fprintf(SUMA_STDERR,"Warning %s: Unknown SO->FileType. Assuming coordinates are in DICOM already.\n", FuncName);
         break;
   }
   
   if (!SUMA_Apply_VolReg_Trans (SO)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Apply_VolReg_Trans.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   SUMA_RETURN (YUP);
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
   SUMA_Boolean UseVolreg, UseTagAlign, Bad=YUP;
   
   SUMA_ENTRY;

   if (SUMAg_CF->IgnoreVolreg) {
      SUMA_SL_Note("Ignoring any Volreg or TagAlign transforms present in Surface Volume.\n");
      SO->VOLREG_APPLIED = NOPE;
      SUMA_RETURN (YUP);
   }
   
   if (SO->VOLREG_APPLIED) {
      fprintf (SUMA_STDERR,"Error %s: Volreg already applied. Nothing done.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   ND = SO->NodeDim;
   
   
   UseVolreg = NOPE;
   UseTagAlign = NOPE;
   /* perform the rotation needed to align the surface with the current experiment's data */
   if (SO->VolPar->VOLREG_MATVEC != NULL || SO->VolPar->VOLREG_CENTER_OLD != NULL || SO->VolPar->VOLREG_CENTER_BASE != NULL) {
      Bad = NOPE;
      if (SO->VolPar->VOLREG_MATVEC == NULL) {
         fprintf(SUMA_STDERR,"Error %s: SO->VolPar->VOLREG_MATVEC = NULL. Cannot perform alignment.\n", FuncName);
         Bad = YUP;
      }
      if (SO->VolPar->VOLREG_CENTER_OLD == NULL) {
         fprintf(SUMA_STDERR,"Error %s: SO->VolPar->VOLREG_CENTER_OLD = NULL. Cannot perform alignment.\n", FuncName);
         Bad = YUP;
      }
      if (SO->VolPar->VOLREG_CENTER_BASE == NULL) {
         fprintf(SUMA_STDERR,"Error %s: SO->VolPar->VOLREG_CENTER_BASE = NULL. Cannot perform alignment.\n", FuncName);
         Bad = YUP;
      }
      if (!Bad) UseVolreg = YUP;
   }
   
   /* Check for Tagalign field */
   if (SO->VolPar->TAGALIGN_MATVEC) UseTagAlign = YUP;

   if (UseTagAlign && UseVolreg) {
      SUMA_SL_Note("Found both Volreg and TagAlign fields.\n"
                   "Using Volreg fields for alignment.");
      UseTagAlign = NOPE;
   }
   
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
      fprintf (SUMA_STDERR,"%s: Applying Rotation.\nMrot[\t%f\t%f\t%f\n%f\t%f\t%f\n%f\t%f\t%f]\nDelta = [%f %f %f]\n", FuncName,\
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
         x = SO->NodeList[id] - SO->VolPar->VOLREG_CENTER_OLD[0];
         y = SO->NodeList[id+1] - SO->VolPar->VOLREG_CENTER_OLD[1];
         z = SO->NodeList[id+2] - SO->VolPar->VOLREG_CENTER_OLD[2];
         
         /* Apply the rotation matrix XYZn = Mrot x XYZ*/
         SO->NodeList[id] = Mrot[0][0] * x + Mrot[0][1] * y + Mrot[0][2] * z;
         SO->NodeList[id+1] = Mrot[1][0] * x + Mrot[1][1] * y + Mrot[1][2] * z;
         SO->NodeList[id+2] = Mrot[2][0] * x + Mrot[2][1] * y + Mrot[2][2] * z;
         
         /*apply netshift*/
         SO->NodeList[id] += NetShift[0];
         SO->NodeList[id+1] += NetShift[1];
         SO->NodeList[id+2] += NetShift[2];
      }
      SO->VOLREG_APPLIED = YUP;   
   } else
      SO->VOLREG_APPLIED = NOPE;
   
      
   SUMA_RETURN (YUP);
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
