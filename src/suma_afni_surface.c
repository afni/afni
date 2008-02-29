/* ---------------------------------------------------------------
   This file is to contain functions to form 
    surface object structures that are independent 
    of GIFTI and SUMA 
    No functions defined in suma_datasets.c sould be made 
    here as this object will go in libmri.a.
    
    This is only for the most basic of functions.
    Add funky stuff to suma_datasets.c       ZSS      Feb 28 08
---------------------------------------------------------------*/

#include "suma_suma.h"

AFNI_SurfaceObject *SUMA_NewAfniSurfaceObject(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObject"};
   AFNI_SurfaceObject *aSO=NULL;
   
   SUMA_ENTRY;
   
   aSO = (AFNI_SurfaceObject *)SUMA_calloc(1, sizeof(AFNI_SurfaceObject));
   aSO->ps = SUMA_NewAfniSurfaceObjectPointset();
   aSO->tr = SUMA_NewAfniSurfaceObjectTriangle();
   SUMA_RETURN(aSO);
}

AFNI_SurfaceObject_TRIANGLE *SUMA_NewAfniSurfaceObjectTriangle(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectTriangle"};
   AFNI_SurfaceObject_TRIANGLE *tr=NULL;
    
   tr=(AFNI_SurfaceObject_TRIANGLE*)
            SUMA_calloc(1,sizeof(AFNI_SurfaceObject_TRIANGLE));
   
   SUMA_RETURN(tr);
}

AFNI_SurfaceObject_POINTSET *SUMA_NewAfniSurfaceObjectPointset(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectPointset"};
   AFNI_SurfaceObject_POINTSET *ps=NULL;
   
   ps = (AFNI_SurfaceObject_POINTSET *)
            SUMA_calloc(1,sizeof(AFNI_SurfaceObject_POINTSET));
   
   SUMA_RETURN(ps);
}

#define IF_FREE(ggg) { if (ggg) SUMA_free(ggg); ggg = NULL; }
AFNI_SurfaceObject_POINTSET *SUMA_FreeAfniSurfaceObjectPointset(
                                    AFNI_SurfaceObject_POINTSET *ps) {
   static char FuncName[]={"SUMA_FreeAfniSurfaceObjectPointset"};
   
   SUMA_ENTRY;
   
   if (ps) {
      IF_FREE(ps->NodeList);
      IF_FREE(ps->AnatomicalStructurePrimary);
      IF_FREE(ps->AnatomicalStructurePrimary);
      IF_FREE(ps->GeometricType);
      IF_FREE(ps->UniqueID);
      IF_FREE(ps->date);
      IF_FREE(ps->dataspace);
      IF_FREE(ps->xformspace);
      SUMA_free(ps); ps = NULL;
   }
   
   SUMA_RETURN(NULL);                                    
}

AFNI_SurfaceObject_TRIANGLE *SUMA_FreeAfniSurfaceObjectTriangle(
                                    AFNI_SurfaceObject_TRIANGLE *tr){
   static char FuncName[]={"SUMA_FreeAfniSurfaceObjectTriangle"};
   
   SUMA_ENTRY;
   if (tr) {
      IF_FREE(tr->FaceSetList);
      IF_FREE(tr->TopologicalType);
      IF_FREE(tr->UniqueID);
      IF_FREE(tr->date);
      SUMA_free(tr); tr = NULL;
   }
   
   SUMA_RETURN(NULL);                                    
}

AFNI_SurfaceObject *SUMA_FreeAfniSurfaceObject(AFNI_SurfaceObject *aSO)
{
   static char FuncName[]={"SUMA_FreeAfniSurfaceObject"};
   
   SUMA_ENTRY;
   
   if (aSO) {
      aSO->ps = SUMA_FreeAfniSurfaceObjectPointset(aSO->ps);
      aSO->tr = SUMA_FreeAfniSurfaceObjectTriangle(aSO->tr);
      
      SUMA_free(aSO); aSO = NULL;     
   }
   
   SUMA_RETURN(NULL);
}
