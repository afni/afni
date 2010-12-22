#include "SUMA_suma.h"

#include "SUMA_Homer.h"


#if 1

void usage_SUMA_Homer()
{
   printf ("\nUsage:  SUMA_Homer\n");
   exit (1);
}   


int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_Homer"}; 
   float *NodeList = NULL;
   int N_Node = 0, N_FaceSet = 0,
      N_parts = 0, ipart=0;
   int *FaceSetList = NULL;
   char sbuf[100], fName[100];
   SUMA_SURF_NORM SN;
   SUMA_OVERLAYS *NewColPlane=NULL;
   SUMA_SurfaceObject **SOv=NULL, *SOi=NULL;
   FILE *SpecOut = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
	SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   

   
   N_parts = 19;
   SOv = (SUMA_SurfaceObject **) SUMA_malloc(N_parts * sizeof(SUMA_SurfaceObject *));
   
   SpecOut = fopen("HJS.spec", "w");
   if (!SpecOut) {
      fprintf(SUMA_STDERR,"Error %s: Failed in opening spec file.\n", FuncName);
		exit(1);
   }
   
   fprintf (SpecOut,"\tGroup = HJS\n");
   fprintf (SpecOut,"\tStateDef = Duffed\n"); 

   for (ipart = 0; ipart < N_parts; ++ipart) {
      SOi = SUMA_HJS_Surface(ipart);
      if (!SOi) {
         SUMA_S_Err("Failed to get part");
         exit(1);
      }
      NodeList = SOi->NodeList; SOi->NodeList = NULL;
      FaceSetList = SOi->FaceSetList; SOi->FaceSetList = NULL;
      SUMA_Free_Surface_Object (SOi); SOi=NULL;
      
      /* SUMA_disp_vect(NodeList, 3*N_Node); */
      /* SUMA_disp_dvect(FaceSetList, 3*N_FaceSet);  */
      if (LocalHead) {
         int tmpmin=-100, n3, itmp;
         n3 = 3 * N_FaceSet;
         fprintf (SUMA_STDERR,"%s: N_Node %d, N_FaceSet %d\n", FuncName, N_Node, N_FaceSet);
         SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
         fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
         if (tmpmin < 0) {
            fprintf (SUMA_STDERR,"Error %s: Bad in return ass negative number\n", FuncName);
            for (itmp=0; itmp<n3; ++itmp) {
               fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
               if (FaceSetList[itmp] < 0) {
                  fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
               }
            } 
         }
      }
      
      /* Now create an SO for that thing */
      SOv[ipart] = SUMA_Alloc_SurfObject_Struct(1);
      /* calculate the curvatures */
      SOv[ipart]->NodeList = NodeList;
      SOv[ipart]->N_Node = N_Node;
      SOv[ipart]->FaceSetList = FaceSetList;
      SOv[ipart]->N_FaceSet = N_FaceSet;
      sprintf (fName, "Springfield/HomerJaySimpson_%d", ipart);
      SOv[ipart]->Group = SUMA_copy_string("HJS");
      SOv[ipart]->State = SUMA_copy_string("Duffed");
      SOv[ipart]->Name = SUMA_StripPath(fName);
      SOv[ipart]->FileType = SUMA_PLY;
      SOv[ipart]->FileFormat = SUMA_FF_NOT_SPECIFIED;
      SOv[ipart]->idcode_str = UNIQ_hashcode(fName); 
      SOv[ipart]->SUMA_VolPar_Aligned = NOPE;
      SOv[ipart]->VolPar = NULL;
      SOv[ipart]->NodeDim = 3;
      SOv[ipart]->FaceSetDim = 3;
      
      SUMA_MIN_MAX_SUM_VECMAT_COL (
         SOv[ipart]->NodeList, SOv[ipart]->N_Node, SOv[ipart]->NodeDim, 
         SOv[ipart]->MinDims, SOv[ipart]->MaxDims, SOv[ipart]->Center);

      SOv[ipart]->Center[0] /= SOv[ipart]->N_Node;
      SOv[ipart]->Center[1] /= SOv[ipart]->N_Node;
      SOv[ipart]->Center[2] /= SOv[ipart]->N_Node;

      SUMA_MIN_VEC (SOv[ipart]->MinDims, 3, SOv[ipart]->aMinDims );
      SUMA_MAX_VEC (SOv[ipart]->MaxDims, 3, SOv[ipart]->aMaxDims);
      
      /* Calculate SurfaceNormals */
      SN = SUMA_SurfNorm(SOv[ipart]->NodeList,  SOv[ipart]->N_Node, 
                  SOv[ipart]->FaceSetList, SOv[ipart]->N_FaceSet );
      SOv[ipart]->NodeNormList = SN.NodeNormList;
      SOv[ipart]->FaceNormList = SN.FaceNormList;

      /*create the structures for GL rendering */
      /*The data is being duplicated at the moment and perhaps I should just stick with the 1D stuf */
      SOv[ipart]->glar_NodeList = (GLfloat *) SOv[ipart]->NodeList; /* just copy the pointer, not the data */
      SOv[ipart]->glar_FaceSetList = (GLint *) SOv[ipart]->FaceSetList; /* just copy the pointer, not the data */
      SOv[ipart]->glar_FaceNormList = (GLfloat *) SOv[ipart]->FaceNormList; /* just copy the pointer, not the data */
      SOv[ipart]->glar_NodeNormList = (GLfloat *) SOv[ipart]->NodeNormList; /* just copy the pointer, not the data */

      /* a surface object does contribute to the rotation center of the viewer displaying it */
      SOv[ipart]->RotationWeight = SOv[ipart]->N_Node;
      SOv[ipart]->ViewCenterWeight = SOv[ipart]->N_Node;

      /* No selections yet, but make the preps */
      SOv[ipart]->ShowSelectedNode = YUP;
      SOv[ipart]->ShowSelectedFaceSet = YUP;
      SOv[ipart]->SelectedFaceSet = -1;
      SOv[ipart]->SelectedNode = -1;
      
      /* create the ball object*/
      SOv[ipart]->NodeMarker = SUMA_Alloc_SphereMarker ();
      if (SOv[ipart]->NodeMarker == NULL) {
         fprintf(SUMA_STDERR,"Error%s: Could not allocate for SOv[ipart]->NodeMarker\n", FuncName);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }
      /* create the FaceSetMarker object */
      SOv[ipart]->FaceSetMarker = SUMA_Alloc_FaceSetMarker();
      if (SOv[ipart]->FaceSetMarker == NULL) {
         fprintf(SUMA_STDERR,"Error%s: Could not allocate for SOv[ipart]->FaceSetMarker\n", FuncName);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }
      
      /* make it its own mapping reference */
      SOv[ipart]->LocalDomainParentID = SUMA_copy_string (SOv[ipart]->idcode_str);
      
      if (SUMA_existSO (SOv[ipart]->idcode_str, SUMAg_DOv, SUMAg_N_DOv)) {
         fprintf(SUMA_STDERR,"Error %s: Surface %d is specifed more than once, multiple copies ignored.\n",
             FuncName, ipart);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }
      
      SUMA_LH("Doing Metrics...");
      if (!SUMA_SurfaceMetrics (SOv[ipart], "Convexity, EdgeList, MemberFace", NULL)) {
         fprintf (SUMA_STDERR,
                  "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
         SUMA_Free_Surface_Object (SOv[ipart]);
         SUMA_RETURN (1);
      }

            {
               SUMA_DSET *dset=NULL;/* create the color plane for Convexity*/
             
             /* create an overlay plane */
               if (!(dset = (SUMA_DSET *)SUMA_GetCx(SOv[ipart]->idcode_str, 
                                                      SUMAg_CF->DsetList, 1))) {
                  SUMA_SL_Err("Failed to find dset!");
                  SUMA_RETURN (NOPE);
               }
               NewColPlane = SUMA_CreateOverlayPointer ("Convexity", dset, 
                                                   SOv[ipart]->idcode_str, NULL);
               if (!NewColPlane) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_CreateOverlayPointer.\n", 
                           FuncName);
                  SUMA_RETURN (NOPE);
               } 
               
               /* Add this plane to SOv[ipart]->Overlays */
               if (!SUMA_AddNewPlane (SOv[ipart], NewColPlane, NULL, -1, 0)) {
                  SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
                  SUMA_FreeOverlayPointer(NewColPlane);
                  SUMA_RETURN (NOPE);
               }
               
               if (!SUMAg_CF->scm) {   
                  SUMAg_CF->scm = SUMA_Build_Color_maps();
                  if (!SUMAg_CF->scm) {
                     SUMA_SL_Err("Failed to build color maps.\n");
                     SUMA_RETURN (NOPE);
                  }
               }
               if (!SUMA_SetConvexityPlaneDefaults(SOv[ipart], 
                                                SUMAg_CF->DsetList)) {
                  SUMA_SL_Err("Failed to set plane defaults."); 
                  SUMA_RETURN(NOPE);
               }

               /* colorize the plane */
               SUMA_ColorizePlane(NewColPlane);
               
               if (SOv[ipart]->SurfCont && !SOv[ipart]->SurfCont->curColPlane) {
                  SOv[ipart]->SurfCont->curColPlane = NewColPlane;
               }

            }

      /* all the previous stuff is nice and dandy but it takes a lot more to
         get this thing working */
      /* Write out the surfaces in PLY format and create a dummy spec file */
      if (!SUMA_Save_Surface_Object (  fName, SOv[ipart], 
                                       SUMA_PLY, SUMA_FF_NOT_SPECIFIED, NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", 
                              FuncName);
         exit (1);
      }

      fprintf (SpecOut,"NewSurface\n");
      fprintf (SpecOut, "\tSurfaceFormat = ASCII\n");
      fprintf (SpecOut, "\tSurfaceType = Ply\n");
      fprintf (SpecOut, "\tSurfaceName = %s\n", fName);
      fprintf (SpecOut, "\tMappingRef = SAME\n");
      fprintf (SpecOut, "\tSurfaceState = %s\n", SOv[ipart]->State);
      fprintf (SpecOut, "\tEmbedDimension = 3\n\n");

      
   }   
   if (SpecOut) fclose (SpecOut);
   SUMA_RETURN(0);
}
#endif
