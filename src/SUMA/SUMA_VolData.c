/*! Dealings with volume data */
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 


THD_fvec3 SUMA_THD_3dfind_to_3dmm( SUMA_SurfaceObject *SO, THD_fvec3 iv );
THD_fvec3 SUMA_THD_3dind_to_3dmm( SUMA_SurfaceObject *SO, THD_ivec3 iv );
THD_fvec3 SUMA_THD_3dmm_to_3dfind( SUMA_SurfaceObject *SO , THD_fvec3 fv );
THD_ivec3 SUMA_THD_3dmm_to_3dind( SUMA_SurfaceObject *SO  , THD_fvec3 fv );
THD_fvec3 SUMA_THD_3dmm_to_dicomm( SUMA_SurfaceObject *SO , THD_fvec3 imv );
THD_fvec3 SUMA_THD_dicomm_to_3dmm( SUMA_SurfaceObject *SO , THD_fvec3 dicv );

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

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	if (VP->prefix != NULL) SUMA_free(VP->prefix);
	if (VP->filecode != NULL) SUMA_free(VP->filecode);
	if (VP->dirname != NULL) SUMA_free(VP->dirname);
	if (VP->idcode_str != NULL) SUMA_free(VP->idcode_str);
	if (VP->idcode_date != NULL) SUMA_free(VP->idcode_date);
	if (VP->VOLREG_CENTER_OLD != NULL) SUMA_free(VP->VOLREG_CENTER_OLD);
	if (VP->VOLREG_CENTER_BASE != NULL) SUMA_free(VP->VOLREG_CENTER_BASE);
	if (VP->VOLREG_MATVEC != NULL) SUMA_free(VP->VOLREG_MATVEC);
	if (VP != NULL) SUMA_free(VP);
	SUMA_RETURN (YUP);
}

SUMA_VOLPAR *SUMA_VolPar_Attr (char *volparent_name)
{
	ATR_float *atr;
	static char FuncName[]={"SUMA_VolPar_Attr"};
	SUMA_VOLPAR *VP;
	THD_3dim_dataset *dset;
	int ii;
	MCW_idcode idcode;
		
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
fprintf (SUMA_STDERR, "CRAP\n");
	/* read the header of the parent volume */
	dset = THD_open_dataset(volparent_name);
	if (dset == NULL) {
		fprintf (SUMA_STDERR,"Error %s: Could not read %s\n", FuncName, volparent_name);
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

	SUMA_RETURN (VP);
}

/*!
	Show the contents of SUMA_VOLPAR structure 
*/
void SUMA_Show_VolPar(SUMA_VOLPAR *VP, FILE *Out)
{
	static char FuncName[]={"SUMA_Show_VolPar"};
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

	if (Out == NULL) Out = SUMA_STDOUT;
	if (VP == NULL) { 
		fprintf(Out,"\nVP is NULL\n");
		SUMA_RETURNe;
	} 
	
	fprintf(Out,"\nVP contents:\n");
	fprintf(Out,"prefix: %s\tfilecode: %s\tdirname: %s\nId code str:%s\tID code date: %s\n", \
		VP->prefix, VP->filecode, VP->dirname, VP->idcode_str, VP->idcode_date);
	fprintf(Out,"isanat: %d\n", VP->isanat);
	fprintf(Out,"Orientation: %d %d %d\n", \
		VP->xxorient, VP->yyorient, VP->zzorient);
	fprintf(Out,"Origin: %f %f %f\n", \
		VP->xorg, VP->yorg, VP->zorg);
	fprintf(Out,"Delta: %f %f %f\n", \
		VP->dx, VP->dy, VP->dz);
	fprintf(Out,"N: %d %d %d\n",\
		VP->nx, VP->ny, VP->nz);
	
	if (VP->VOLREG_MATVEC != NULL) {
		fprintf(Out,"VP->VOLREG_MATVEC = \n\tMrot\tDelta\n");
		fprintf(Out,"|%f\t%f\t%f|\t|%f|\n", \
		VP->VOLREG_MATVEC[0], VP->VOLREG_MATVEC[1], VP->VOLREG_MATVEC[2], VP->VOLREG_MATVEC[3]); 
		fprintf(Out,"|%f\t%f\t%f|\t|%f|\n", \
		VP->VOLREG_MATVEC[4], VP->VOLREG_MATVEC[5], VP->VOLREG_MATVEC[6], VP->VOLREG_MATVEC[7]);
		fprintf(Out,"|%f\t%f\t%f|\t|%f|\n", \
		VP->VOLREG_MATVEC[8], VP->VOLREG_MATVEC[9], VP->VOLREG_MATVEC[10], VP->VOLREG_MATVEC[11]);
	} else fprintf(Out,"VP->VOLREG_MATVEC = NULL\n");

	if (VP->VOLREG_CENTER_OLD != NULL)
		fprintf(Out,"VP->VOLREG_CENTER_OLD = %f, %f, %f\n", \
		VP->VOLREG_CENTER_OLD[0], VP->VOLREG_CENTER_OLD[1], VP->VOLREG_CENTER_OLD[2]); 
	else fprintf(Out,"VP->VOLREG_CENTER_OLD = NULL\n");
	
	if (VP->VOLREG_CENTER_BASE != NULL)
		fprintf(Out,"VP->VOLREG_CENTER_BASE = %f, %f, %f\n", \
		VP->VOLREG_CENTER_BASE[0], VP->VOLREG_CENTER_BASE[1], VP->VOLREG_CENTER_BASE[2]); 
	else fprintf(Out,"VP->VOLREG_CENTER_BASE = NULL\n");
	
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
	
	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
			/* Do nothing */
			break;
		case SUMA_FREE_SURFER:
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
			{	THD_fvec3 fv, iv;
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
					iv = SUMA_THD_3dmm_to_dicomm( SO , fv );
					SO->NodeList[id] = iv.xyz[0];
					SO->NodeList[id+1] = iv.xyz[1];
					SO->NodeList[id+2] = iv.xyz[2];
				}
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
   SUMA_Boolean Bad=YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SO->VOLREG_APPLIED) {
      fprintf (SUMA_STDERR,"Error %s: Volreg already applied. Nothing done.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

	ND = SO->NodeDim;
   
   /* perform the rotation needed to align the surface with the current experiment's data */
	if (SO->VolPar->VOLREG_MATVEC != NULL || SO->VolPar->VOLREG_CENTER_OLD != NULL || SO->VolPar->VOLREG_CENTER_BASE != NULL) {
		Bad = NOPE;
		if (SO->VolPar->VOLREG_MATVEC == NULL) {
			fprintf(SUMA_STDERR,"Error %s: VP->VOLREG_MATVEC = NULL. Cannot perform alignment.\n", FuncName);
			Bad = YUP;
		}
		if (SO->VolPar->VOLREG_CENTER_OLD == NULL) {
			fprintf(SUMA_STDERR,"Error %s: VP->VOLREG_CENTER_OLD = NULL. Cannot perform alignment.\n", FuncName);
			Bad = YUP;
		}
		if (SO->VolPar->VOLREG_CENTER_BASE == NULL) {
			fprintf(SUMA_STDERR,"Error %s: VP->VOLREG_CENTER_BASE = NULL. Cannot perform alignment.\n", FuncName);
			Bad = YUP;
		}
	}

	if (!Bad && SO->VolPar->VOLREG_MATVEC != NULL) {
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

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


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

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

/*!---------------------------------------------------------------------
   convert from input image oriented x,y,z to Dicom x,y,z
     (x axis = R->L , y axis = A->P , z axis = I->S)

   N.B.: image distances are oriented the same as Dicom,
         just in a permuted order.
-----------------------------------------------------------------------*/

THD_fvec3 SUMA_THD_3dmm_to_dicomm( SUMA_SurfaceObject *SO ,
                              THD_fvec3 imv )
{
	static char FuncName[]={"SUMA_THD_3dmm_to_dicomm"};   
	THD_fvec3 dicv ;
   float xim,yim,zim , xdic = 0.0, ydic = 0.0, zdic = 0.0 ;

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   xim = imv.xyz[0] ; yim = imv.xyz[1] ; zim = imv.xyz[2] ;

   switch( SO->VolPar->xxorient ){
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

   switch( SO->VolPar->yyorient ){
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

   switch( SO->VolPar->zzorient ){
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

	if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   xdic = dicv.xyz[0] ; ydic = dicv.xyz[1] ; zdic = dicv.xyz[2] ;

   switch( SO->VolPar->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: xim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: xim = zdic ; break ;

      default: 
			fprintf(SUMA_STDERR, "SUMA_THD_3dmm_to_dicomm: illegal xxorient code.\n Exiting.") ;
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
			fprintf(SUMA_STDERR, "SUMA_THD_3dmm_to_dicomm: illegal xxorient code.\n Exiting.") ;
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
			fprintf(SUMA_STDERR, "SUMA_THD_3dmm_to_dicomm: illegal xxorient code.\n Exiting.") ;
			exit (1);
   }

   imv.xyz[0] = xim ; imv.xyz[1] = yim ; imv.xyz[2] = zim ;
   SUMA_RETURN(imv) ;
}

