#include "SUMA_suma.h"

/*! functions dealing with Drawable Object Manipulation */

/*!
	This function Links one Inode to another.
	This is different from CreateInode in that no new node is being allocated for, only a link is created.
	SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_Inode *ToIN)
	
	FromIN = SUMA_CreateInodeLink (FromIN, ToIN);
	
	\param FromIN (SUMA_INODE *) this is the pointer to the IDnode wich will be linked to ToIN.
		    As silly as this sounds, this should be a pointer to NULL otherwise the link will not
			 be established. The use of this function will help reduce the risk of linking pointers
			 that are pointing to allocated space.
	\param ToIN (SUMA_INODE *) pointer where the link is going
	\ret FromIN (SUMA_INODE *) if all is well, FromIN = ToIN
		otherwise, NULL is returned
		
		All this function does is check the FromIN is NULL and ToIN isn't and then it calls SUMA_AddLink (ToIN)
	\sa SUMA_BreakInodeLink
	\sa SUMA_isInodeLink
*/
SUMA_INODE *SUMA_CreateInodeLink (SUMA_INODE * FromIN, SUMA_INODE *ToIN)
{
	static char FuncName[] = {"SUMA_CreateInodeLink"};
	
	if (FromIN) {
		fprintf (SUMA_STDERR,"Error %s: FromIN Inode is not NULL. \n\tFromIN pointer is left undisturbed.\n", FuncName);
		return (FromIN);
	}
	if (!ToIN) {
		fprintf (SUMA_STDERR,"Error %s: ToIN is NULL.\n\t Can't link to NULL, returning NULL.\n", FuncName);
		return (NULL); 
	}
	
	/* add a link to ToIN */
	if (!SUMA_AddLink (ToIN)) {
		fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_AddLink.\n", FuncName);
		return (NULL);
	}
	
	/* now return the pointer to be linked to */
	return (ToIN);

}

/*! 
	This function determines if an Inode in a SO is a link or not.
	an Inode is a link if IN->ParentIDcode and HolderIDcode are different. 
	The function is nothing more than a strcmp.
	
	SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode)

	 returns NOPE if IN == NULL or IN->ParentIDcode != HolderIDcode
*/
SUMA_Boolean SUMA_isInodeLink (SUMA_INODE *IN, const char *HolderIDcode)
{
	static char FuncName[] = {"SUMA_isInodeLink"};
	
	if (!IN) {
		fprintf (SUMA_STDERR, "Warning %s: IN is null.\n", FuncName); 
		return (NOPE);
	} 
	if (!strcmp(IN->ParentIDcode, HolderIDcode)) return (YUP);
		
	return (NOPE);
}

/*! 
	function to remove the link of one IDnode to the next 
	SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode);
	\param IN (SUMA_INODE *) the linked inode
	\param HolderIDcode (const char *) the ID code that holds/contains IN
	\ret NULL if the link was broken or IN == NULL
		IN if IN is not a link but an actuak Inode (meaning IN->ParentIDcode == HolderIDcode
		

*/	
SUMA_INODE * SUMA_BreakInodeLink (SUMA_INODE *IN, const char *HolderIDcode) 
{
	static char FuncName[] = {"SUMA_BreakInodeLink"};
	if (!IN) {
		fprintf (SUMA_STDERR, "Warning %s: IN is null, nothing to do.\n", FuncName); 
		return (NULL);
	}
	if (!SUMA_isInodeLink (IN, HolderIDcode)) {
		fprintf (SUMA_STDERR, "Error %s: Inode IN is not a link. Nothing done.\n", FuncName);
		return (IN);
	} 
	
	/* release the link */
	if (SUMA_ReleaseLink (IN) < 0) {
		fprintf (SUMA_STDERR, "Error %s: IN has no links. Nothing done.\n", FuncName);
		return (IN);
	}
	
	/* OK, link released, not return NULL */
	return (NULL);
}

/*! 
	This function decrements the N_link field in IN by 1 and returns the resultant N_link value
	a -1 is returned if IN->N_link = 0 or IN == NULL


	ans = SUMA_ReleaseLink (IN);

	\param IN (SUMA_INODE *) pointer to SUMA_INODE structure
	\ret ans (int) value of IN->N_link
	\sa SUMA_AddLink
*/
int SUMA_ReleaseLink (SUMA_INODE * IN) 
{
	static char FuncName[]={"SUMA_ReleaseLink"};
	if (!IN) {
		fprintf (SUMA_STDERR,"Error %s: Inode is null. Returning -1.\n", FuncName);
		return (-1);
	}
	if (!IN->N_link) return (-1);
	else {
		IN->N_link--;
		return (IN->N_link);
	}
}
	
/*! 
	This function increments the N_link field in IN by 1 and returns the resultant N_link value
	a zero is returned in case of an error


	ans = SUMA_AddLink (IN);

	\param IN (SUMA_INODE *) pointer to SUMA_INODE structure
	\ret ans (int) value of IN->N_link
	\sa SUMA_ReleaseLink
*/
int SUMA_AddLink (SUMA_INODE * IN) 
{
	static char FuncName[]={"SUMA_AddLink"};
	if (!IN) {
		fprintf (SUMA_STDERR,"Error %s: Inode is null.\n", FuncName);
		
		return (0);
	} else {
		IN->N_link++;
		return (IN->N_link);
	}
}

/*! 
	Function to create a SUMA_INODE structure 
	ans = SUMA_CreateInode (data, ParentIDcode);
	
	\param data (void *) pointer to data location
	\param ParentIDcode (char[SUMA_IDCODE_LENGTH]) containing the IDcode of the creator of the data
	\ret ans (SUMA_INODE *) pointer to SUMA_INODE for data
		NULL if error is encountered
*/

SUMA_INODE *SUMA_CreateInode (void *data, char *ID)
{
	static char FuncName[]={"SUMA_CreateInode"};
	SUMA_INODE *IN;
	
	IN = (SUMA_INODE *)malloc (sizeof(SUMA_INODE));
	if (IN == NULL) {
		fprintf (SUMA_STDERR,"Error %s: failed to allocate for Inode.\n", FuncName);
		return (NULL);
	}
	
	IN->data = data;
	strcpy (IN->ParentIDcode, ID);
	IN->N_link = 0;
	
	return (IN);
}

/*!
Create a Surface Object data structure 
*/

SUMA_SurfaceObject *Alloc_SurfObject_Struct(int N)
{
	SUMA_SurfaceObject *SO;
	int i, j;
	
	SO = (SUMA_SurfaceObject *)malloc(sizeof(SUMA_SurfaceObject)*N);
	if (SO == NULL) {
		SUMA_alloc_problem("Alloc_SurfObject_Struct: could not allocate memory for SO");
	}
	
	for (i=0; i< N; ++i) {
		SO[i].Name_NodeParent = NULL;
		SO[i].EmbedDim = 3;
		SO[i].MF = NULL;
		SO[i].FN = NULL;
		SO[i].FN_Inode = NULL;
		SO[i].EL = NULL;
		SO[i].EL_Inode = NULL;
		SO[i].PolyArea = NULL;
		SO[i].SC = NULL;
		SO[i].Cx = NULL;
		SO[i].Cx_Inode = NULL;
		SO[i].VolPar = NULL;
		/* create vector of pointers */
		SO[i].Overlays = (SUMA_OVERLAYS **) malloc(sizeof(SUMA_OVERLAYS *) * SUMA_MAX_OVERLAYS);
		SO[i].Overlays_Inode = (SUMA_INODE **) malloc(sizeof(SUMA_INODE *) * SUMA_MAX_OVERLAYS); 
		/* fill pointers with NULL */
		for (j=0; j < SUMA_MAX_OVERLAYS; ++j) {
			SO[i].Overlays[j] = NULL;
			SO[i].Overlays_Inode[j] = NULL;
		}
		SO[i].N_Overlays = 0;
	}
	return (SO);
}/* Alloc_SurfObject_Struct */

/*!
Create a Displayable Object data structure 
*/
SUMA_DO *Alloc_DisplayObject_Struct (int N)
{
	SUMA_DO *dov;
	
	dov = (SUMA_DO *)malloc(sizeof(SUMA_DO)*N);
	if (dov == NULL) {
		SUMA_alloc_problem("Alloc_DisplayObject_Struct: could not allocate memory for SO");
	}
	return (dov);
}/*Alloc_DisplayObject_Struct*/

/*!
Free a Displayable Object data structure 
*/
SUMA_Boolean SUMA_Free_Displayable_Object (SUMA_DO *dov)
{
	switch (dov->ObjectType) {
		case SO_type:
			if (!SUMA_Free_Surface_Object ((SUMA_SurfaceObject *)dov->OP)) {
				fprintf(stderr,"Error SUMA_Free_Displayable_Object, could not free surface\n");
			}
			break;
		case AO_type:
			fprintf(stderr,"Error SUMA_Free_Displayable_Object, Not trained to free AO objects\n");
			break;
		case GO_type:
			fprintf(stderr,"Error SUMA_Free_Displayable_Object, Not trained to free GO objects\n");
			break;
	}	
	if (dov) free (dov);
	return (YUP);
}

SUMA_Boolean SUMA_Free_Displayable_Object_Vect (SUMA_DO *dov, int N)
{
	int i;
	SUMA_Boolean Ret = YUP;
	
	for (i=0; i < N; ++i) {
		if (&dov[i] != NULL) {
			Ret = Ret * SUMA_Free_Displayable_Object (&dov[i]);
		}
	}
	return(Ret);

}	
/*!
Add a displayable object to dov
*/
SUMA_Boolean SUMA_AddDO(SUMA_DO *dov, int *N_dov, void *op, SUMA_DO_Types DO_Type, SUMA_DO_CoordType DO_CoordType)
	{
		char FuncName[100];
		sprintf(FuncName, "SUMA_AddDO");
		
		/* make sure you did not exceed allocated space */
		if (*N_dov >= SUMA_MAX_DISPLAYABLE_OBJECTS) {
			SUMA_error_message (FuncName, "Reached limit of DOv storage",0);
			return(NOPE);
		}
		dov[*N_dov].OP = op;
		dov[*N_dov].ObjectType = DO_Type;
		dov[*N_dov].CoordType = DO_CoordType;
		*N_dov = *N_dov+1;
		return (YUP);
	}

/*!
Register a DO with surface viewer ShowDO vector 
if dov_id is present in cSV, nothing is done
if not found then dov_id is registered at the end of cSV->ShowDO 
and N_DO is updated
*/
SUMA_Boolean SUMA_RegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
	int i;
	static char FuncName[]={"SUMA_RegisterDO"};
	
	/* check to see if dov_id exists */
	i = 0;
	while (i < cSV->N_DO) {
		if (cSV->ShowDO[i] == dov_id) {
			/* found do nothing, return */
		return (YUP);
		}
		++i;
	}
	cSV->ShowDO[cSV->N_DO] = dov_id;
	cSV->N_DO += 1;

	#if 0
		fprintf (SUMA_STDERR,"%s: ShowDO is now:\n", FuncName);
		for (i=0; i< cSV->N_DO; ++i) {
			fprintf(SUMA_STDERR,"ShowDO[%d] = %d\t", i, cSV->ShowDO[i]);
		}
		fprintf(SUMA_STDERR,"\n");
	#endif

	return (YUP); 
}
/*!
remove DO with I.D. dov_id from ShowDO list of that current viewer
removal of dov_id element is done by replacing it with the last entry in ShowDO
list. If not found, nothing happens.
*/
SUMA_Boolean SUMA_UnRegisterDO(int dov_id, SUMA_SurfaceViewer *cSV)
{
	int i;
	static char FuncName[]={"SUMA_UnRegisterDO"};
	
	/* check to see if dov_id exists */
	i = 0;
	while (i < cSV->N_DO) {
		if (cSV->ShowDO[i] == dov_id) {
			/* found, replace it by the last in the list */
			cSV->ShowDO[i] = cSV->ShowDO[cSV->N_DO-1];
			/*remove the last element of the list */
			cSV->ShowDO[cSV->N_DO-1] = 0;
			cSV->N_DO -= 1; 
			
				#if 0
					fprintf (SUMA_STDERR,"%s: ShowDO is now:\n", FuncName);
					for (i=0; i< cSV->N_DO; ++i) {
						fprintf(SUMA_STDERR,"ShowDO[%d] = %d\t", i, cSV->ShowDO[i]);
					}
					fprintf(SUMA_STDERR,"\n");
				#endif

			
			return (YUP);
		}
		++i;
	}
	/* Not found, nothing happens */
	
	#if 0
		fprintf (SUMA_STDERR,"%s: ShowDO is now:\n", FuncName);
		for (i=0; i< cSV->N_DO; ++i) {
			fprintf(SUMA_STDERR,"ShowDO[%d] = %d\t", i, cSV->ShowDO[i]);
		}
		fprintf(SUMA_STDERR,"\n");
	#endif
	
	return (YUP); 
}

/*!
print out the data contained in dov 
*/
void SUMA_Show_DOv (SUMA_DO *dov, int N_dov, FILE *Out)
{
	int i;
	SUMA_SurfaceObject *so_op;
	
	if (Out == NULL) Out = stdout;
	fprintf(Out,"\nDOv contents (%d elements):\n", N_dov);
	for (i=0; i < N_dov; ++i) {
		switch (dov[i].ObjectType) {
			case SO_type:
				so_op = (SUMA_SurfaceObject *)dov[i].OP;
				if (so_op->FileType != SUMA_SUREFIT) {
					fprintf(Out,"DOv ID: %d\n\tName: %s/%s\n\tType: %d, Axis Attachment %d\n",\
						i, so_op->Name.Path, so_op->Name.FileName,\
						dov[i].ObjectType, dov[i].CoordType);
				} else {
					fprintf(Out,"DOv ID: %d\n\tNameCoord: %s/%s\n\tNameTopo: %s/%s\n\tType: %d, Axis Attachment %d\n",\
						i, so_op->Name_coord.Path, so_op->Name_coord.FileName,\
						so_op->Name_topo.Path, so_op->Name_topo.FileName,\
						dov[i].ObjectType, dov[i].CoordType);
				}	
				break;
			case AO_type:
				{
					SUMA_Axis* ao;
					ao = (SUMA_Axis*) dov[i].OP;
					fprintf(Out,"DOv ID: %d\n\tAxis Object\n\tType: %d, Axis Attachment %d\n", i,dov[i].ObjectType, dov[i].CoordType);
					fprintf(Out,"\tName: %s\tidcode: %s\n", ao->Name, ao->idcode_str);
				}
				break;
			case GO_type:
				fprintf(Out,"DOv ID: %d\n\tGrid Object\n", i);
				break;
			default:
				fprintf(Out,"DOv ID: %d\n\tUnknown Type!\n", i);
				break;
		}
	}
	return;
}

/*!
returns a vector of indices into dov for DO that meet DO_Type
You should free the returned pointer once you're done with it
N contains the number of elements found
*/
int * SUMA_GetDO_Type(SUMA_DO *dov, int N_dov, SUMA_DO_Types DO_Type, int *N)
{
	int *do_id, i;
	*N = 0;

	do_id = (int *)calloc (sizeof(int), SUMA_MAX_DISPLAYABLE_OBJECTS);

	if (do_id == NULL) {
		fprintf(stderr,"Error SUMA_GetDO_Type: Could not allocate for do_id\n");
		return (NULL);
	}
		i = 0;
		while (i < N_dov) {
			if (dov[i].ObjectType == DO_Type) {
				do_id[*N] = i;
				*N = *N + 1;
			}
		++i;
		}
		return(do_id);
}

