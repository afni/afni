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

NI_group *SUMA_NewAfniSurfaceObject(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObject"};
   NI_group *aSO=NULL;
   NI_group *ngr=NULL;
   SUMA_ENTRY;
   
   aSO = NI_new_group_element();
   NI_rename_group(aSO, "SurfaceObject");
   
   ngr = SUMA_NewAfniSurfaceObjectTriangle();
   NI_add_to_group(aSO, ngr);
   ngr = SUMA_NewAfniSurfaceObjectPointset();
   NI_add_to_group(aSO, ngr);
   ngr = SUMA_NewAfniSurfaceObjectNormals();
   NI_add_to_group(aSO, ngr);
   SUMA_RETURN(aSO);
}

NI_group *SUMA_NewAfniSurfaceObjectTriangle(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectTriangle"};
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "Gifti_Triangle");
   nel = NI_new_data_element("Mesh_IJK", 1);
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}

NI_group *SUMA_NewAfniSurfaceObjectPointset(void)
{
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectPointset"};
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "Gifti_Pointset");
   nel = NI_new_data_element("Node_XYZ", 4251);
   NI_add_to_group(ngr, nel);
   nel = NI_new_data_element("Coord_System", 16);
   NI_add_column(nel,NI_DOUBLE,NULL);
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}
NI_group *SUMA_NewAfniSurfaceObjectNormals(void)
{ 
   static char FuncName[]={"SUMA_NewAfniSurfaceObjectNormals"};
   NI_element *nel=NULL;
   NI_group *ngr=NULL;
   
   SUMA_ENTRY;
   
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "Gifti_Normals");
   nel = NI_new_data_element("Node_Normals", 1);
   NI_add_to_group(ngr, nel);
   
   SUMA_RETURN(ngr);
}

#define IF_FREE(ggg) { if (ggg) SUMA_free(ggg); ggg = NULL; }


NI_group *SUMA_FreeAfniSurfaceObject(NI_group *aSO)
{
   static char FuncName[]={"SUMA_FreeAfniSurfaceObject"};
   
   SUMA_ENTRY;
   
   if (aSO) NI_free_element(aSO);
   
   SUMA_RETURN(NULL);
}

void SUMA_FindNgrNamedElementRec(NI_group *ngr, 
                                 char *elname, 
                                 NI_element **nelp)
{
   static char FuncName[]={"SUMA_FindNgrNamedElementRec"};
   NI_element *nel = NULL;
   char *rs=NULL;
   int ip;
   int LocalHead = 0;
   
   SUMA_ENTRY;
    
   if (!ngr || !elname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURNe; 
   }
  /* now read the elements in this group */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            if (LocalHead > 1)  {
                     fprintf( SUMA_STDERR,
                              "%s:  Looking for %s   in group %s \n",
                              FuncName, elname, ngr->name);
            }
            SUMA_FindNgrNamedElementRec(  (NI_group *)ngr->part[ip], 
                                          elname, 
                                          nelp);
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead > 1)  {
               fprintf( SUMA_STDERR,
                        "%s:>%d<  Looking for %s   name=%s \n"
                        "vec_len=%d vec_filled=%d, vec_num=%d\n", 
                        FuncName, ip, elname, 
                        nel->name, nel->vec_len, nel->vec_filled, 
                        nel->vec_num );
            }
            if (!strcmp(elname, nel->name)) { 
               *nelp=nel; 
               if (LocalHead) {
                  fprintf( SUMA_STDERR,
                        "%s: Found %s in group %s\n",
                        FuncName, nel->name, ngr->name);
               }
               SUMA_RETURNe; 
            }   
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this group element\n"
                        "ignoring.");
            break;
      }
   }


   SUMA_RETURNe;
}
NI_element *SUMA_FindNgrNamedElement(NI_group *ngr, char *elname)
{
   static char FuncName[]={"SUMA_FindNgrNamedElement"};
   NI_element *nel = NULL;
   char *rs=NULL;
   int ip;
   int LocalHead = 0;
   
   SUMA_ENTRY;
    
   if (!ngr || !elname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURN(nel); 
   }
   
   SUMA_FindNgrNamedElementRec(ngr, elname, &nel);
   if (LocalHead) {
      if (nel) {
         fprintf( SUMA_STDERR,
                  "%s: Found nel %s\n",
                  FuncName, elname);
      } else {
         fprintf( SUMA_STDERR,
                  "%s: nel %s not found\n",
                  FuncName, elname);
      }         
   }
   SUMA_RETURN(nel);
}

char *SUMA_NI_AttrOfNamedElement(NI_group *ngr, char *elname, char *attrname)
{
   static char FuncName[]={"SUMA_NI_AttrOfNamedElement"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname || !attrname) { 
      SUMA_S_Err("NULL input");
      fprintf(SUMA_STDERR,"%s: %p %p %p\n", FuncName, ngr, elname, attrname); 
      SUMA_RETURN(NULL); 
   }
   nel = SUMA_FindNgrNamedElement(ngr, elname);
   if (!nel) SUMA_RETURN(NULL);
   SUMA_RETURN(NI_get_attribute(nel,attrname));
}

int SUMA_NI_intAttrOfNamedElement(NI_group *ngr, char *elname, char *attrname)
{
   static char FuncName[]={"SUMA_NI_intAttrOfNamedElement"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname || !attrname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURN(0); 
   }
   nel = SUMA_FindNgrNamedElement(ngr, elname);
   if (!nel) SUMA_RETURN(0);
   SUMA_RETURN(SUMA_NI_get_int(nel,attrname));
}

double SUMA_NI_doubleAttrOfNamedElement(NI_group *ngr, char *elname, 
                                     char *attrname)
{
   static char FuncName[]={"SUMA_NI_doubleAttrOfNamedElement"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   if (!ngr || !elname || !attrname) { 
      SUMA_S_Err("NULL input "); 
      SUMA_RETURN(0); 
   }
   nel = SUMA_FindNgrNamedElement(ngr, elname);
   if (!nel) SUMA_RETURN(0);
   SUMA_RETURN(SUMA_NI_get_double(nel,attrname));
}

int SUMA_NI_get_int(NI_element *nel, char *attrname)
{
   static char FuncName[]={"SUMA_NI_get_int"};
   int n=0;
   char *s=NULL;
   
   SUMA_ENTRY;
   if (nel && attrname && (s=NI_get_attribute(nel,attrname))) {
      n = (int)strtol(s,NULL,10);
   }
   SUMA_RETURN(n);
}

double SUMA_NI_get_double(NI_element *nel, char *attrname)
{
   static char FuncName[]={"SUMA_NI_get_double"};
   double n=0;
   char *s=NULL;
   
   SUMA_ENTRY;
   if (nel && attrname && (s=NI_get_attribute(nel,attrname))) {
      n = strtod(s,NULL);
   }
   SUMA_RETURN(n);
}

void SUMA_NI_set_int(NI_element *nel, char *attrname, int n)
{
   static char FuncName[]={"SUMA_NI_set_int"};
   char sb[32]={""};
   
   SUMA_ENTRY;
   if (nel && attrname) {
      sprintf(sb,"%d",n);
      NI_set_attribute(nel, attrname, sb);
   }
   SUMA_RETURNe;
}

void SUMA_NI_set_double(NI_element *nel, char *attrname, double n)
{
   static char FuncName[]={"SUMA_NI_set_double"};
   char sb[32]={""};
   
   SUMA_ENTRY;
   if (nel && attrname) {
      sprintf(sb,"%f",n);
      NI_set_attribute(nel, attrname, sb);
   }
   SUMA_RETURNe;
}

