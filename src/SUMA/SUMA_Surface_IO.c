
#include "SUMA_suma.h"
#include "PLY/ply.h"

#undef STAND_ALONE

#if defined SUMA_SureFit_STAND_ALONE
#define STAND_ALONE 
#elif defined SUMA_FreeSurfer_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_Ply_Read_STAND_ALONE
#define STAND_ALONE
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif
   


/*!
   \brief a function to simplify loading surfaces the old way
   The only difference with SUMA_Load_Surface_Object_eng is that it 
   takes the file names needed in the first three character pointers
   usually: if_name1: (char *) name of entire surface file or the coordinates file
            if_name2: (char *) name of triangulation file
            vp_name: (char *) name of volume parent file, just for SureFit surfaces.
*/
SUMA_SurfaceObject *SUMA_Load_Surface_Object_Wrapper ( char *if_name, char *if_name2, char *vp_name, 
                                                   SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *sv_name, int debug)
{
   static char FuncName[]={"SUMA_Load_Surface_Object_Wrapper"};
   SUMA_SurfaceObject *SO=NULL;
   void *SO_name=NULL;
   SUMA_SFname *SF_name = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   switch (SO_FT) {
      case SUMA_SUREFIT:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         if (!vp_name) { /* initialize to empty string */
            SF_name->name_param[0] = '\0'; 
         }
         else {
            sprintf(SF_name->name_param,"%s", vp_name);
         }
         SO_name = (void *)SF_name;
         if (debug > 0) 
            fprintf (SUMA_STDOUT,
                     "Reading %s and %s...\n", 
                     SF_name->name_coord, SF_name->name_topo);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_SUREFIT, 
                                        SUMA_ASCII, sv_name);
         break;
      case SUMA_VEC:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         sprintf(SF_name->name_coord,"%s", if_name);
         sprintf(SF_name->name_topo,"%s", if_name2); 
         SO_name = (void *)SF_name;
         if (debug > 0) 
            fprintf (SUMA_STDOUT,
                     "Reading %s and %s...\n", 
                     SF_name->name_coord, SF_name->name_topo);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_VEC, SUMA_ASCII, sv_name);
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         if (SUMA_isExtension(SO_name, ".asc")) 
            SO = SUMA_Load_Surface_Object (SO_name, SUMA_FREE_SURFER, 
                                           SUMA_ASCII, sv_name);
         else
            SO = SUMA_Load_Surface_Object_eng ( SO_name, SUMA_FREE_SURFER, 
                                                SUMA_BINARY_BE, sv_name, 0);
         break;  
      case SUMA_OPENDX_MESH:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (  SO_name, SUMA_OPENDX_MESH, 
                                          SUMA_ASCII, sv_name);
         break;  
      case SUMA_PLY:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (  SO_name, SUMA_PLY, 
                                          SUMA_FF_NOT_SPECIFIED, sv_name);
         break;  
      case SUMA_MNI_OBJ:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (  SO_name, SUMA_MNI_OBJ, 
                                          SUMA_ASCII, sv_name);
         break;  
      case SUMA_BRAIN_VOYAGER:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (  SO_name, SUMA_BRAIN_VOYAGER, 
                                          SUMA_BINARY, sv_name);
         break;  
      case SUMA_BYU:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (SO_name, SUMA_BYU, SUMA_ASCII, sv_name);
         break;  
      case SUMA_GIFTI:
         SO_name = (void *)if_name; 
         if (debug > 0) 
            fprintf (SUMA_STDOUT,"Reading %s ...\n",if_name);
         SO = SUMA_Load_Surface_Object (  SO_name, SUMA_GIFTI,
                                          SUMA_FF_NOT_SPECIFIED, sv_name);
         break;  
      
      default:
         fprintf (SUMA_STDERR,"Error %s: Bad format.\n", FuncName);
         exit(1);
   }

   if (SF_name) SUMA_free(SF_name); SF_name = NULL;
   SUMA_RETURN(SO);
}

/*!
   \brief Removes the standard extension from a dataset filename
   \param Name (char *) name 
   \param form SUMA_DSET_FORMAT
   \return (char *) no_extension (you have to free that one with SUMA_free)
*/

char *SUMA_RemoveSurfNameExtension (char*Name, SUMA_SO_File_Type oType)
{
   static char FuncName[]={"SUMA_RemoveSurfNameExtension"};
   char *noex = NULL, *tmp = NULL;
   
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("NULL Name"); SUMA_RETURN(NULL); }
  
   switch (oType) {
      case SUMA_SUREFIT:
         tmp  =  SUMA_Extension(Name, ".coord", YUP);
         noex  =  SUMA_Extension(tmp, ".topo", YUP); SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_VEC:
         tmp  =  SUMA_Extension(Name, ".1D.coord", YUP);
         noex  =  SUMA_Extension(tmp, ".1D.topo", YUP); 
         SUMA_free(tmp); tmp = NULL;
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         noex  =  SUMA_Extension(Name, ".asc", YUP);
         break;  
      case SUMA_PLY:
         noex  =  SUMA_Extension(Name,".ply" , YUP); 
         break;  
      case SUMA_MNI_OBJ:
         noex  =  SUMA_Extension(Name,".obj" , YUP); 
         break;  
      case SUMA_OPENDX_MESH:
         noex  =  SUMA_Extension(Name,".dx" , YUP); 
         break;  
      case SUMA_INVENTOR_GENERIC:
         noex  =  SUMA_Extension(Name,".iv" , YUP); 
         break;
      case SUMA_BRAIN_VOYAGER:
         noex  =  SUMA_Extension(Name,".srf" , YUP); 
         break;
      case SUMA_BYU:
         tmp  =  SUMA_Extension(Name,".byu" , YUP);
         noex  =  SUMA_Extension(tmp, ".g", YUP); SUMA_free(tmp); tmp = NULL; 
         break;
      case SUMA_GIFTI:
         noex  =  SUMA_Extension(Name,".gii" , YUP); 
         break;
      default:
         /* do nothing, 
         get back fprintf (SUMA_STDERR,"Warning %s: Bad format.\n", FuncName); */
         noex = SUMA_copy_string(Name);
         break;
   }
   
   SUMA_RETURN(noex);
}

/*!
   \brief much like SUMA_Prefix2SurfaceName, but handles the case where namecoord and nametopo are not the same
   consider it a more general version of SUMA_Prefix2SurfaceName
*/
void * SUMA_2Prefix2SurfaceName (char *namecoord, char *nametopo, char *path, 
                                 char *vp_name, SUMA_SO_File_Type oType, 
                                 SUMA_Boolean *exists)
{
   static char FuncName[]={"SUMA_2Prefix2SurfaceName"};
   SUMA_Boolean exist1, exist2;
   SUMA_SFname *SF_name1 = NULL, *SF_name2 = NULL;
   
   SUMA_ENTRY;
   
   *exists = YUP; /* initialize to bad case, for safety */
   
   if (!nametopo && !namecoord) { SUMA_RETURN(NULL); }
   
   if (!nametopo) 
      SUMA_RETURN(SUMA_Prefix2SurfaceName (namecoord, path, vp_name, 
                                           oType, exists));
   if (!namecoord) SUMA_RETURN(SUMA_Prefix2SurfaceName (nametopo, path, vp_name, 
                                                        oType, exists));
   
   if (strcmp(namecoord, nametopo) == 0) 
      SUMA_RETURN(SUMA_Prefix2SurfaceName (nametopo, path, vp_name, 
                                           oType, exists));
   if (oType != SUMA_SUREFIT && oType != SUMA_VEC) {
      SUMA_SL_Err("Wrong usage of function, namecoord and nametopo \n"
                  "are different but surface type is neither "
                  "SUMA_SUREFIT nor SUMA_VEC");
      SUMA_RETURN(NULL);
   }
   
   /* the pain in the behind case */
   SF_name1 = SUMA_Prefix2SurfaceName (namecoord, path, vp_name, oType, &exist1);
   if (!SF_name1) {
      SUMA_SL_Err("Failed to create name");
      SUMA_RETURN(NULL);
   }
   SF_name2 = SUMA_Prefix2SurfaceName (nametopo, path, vp_name, oType, &exist2);
   if (!SF_name2) {
      SUMA_free(SF_name1); SF_name1= NULL;
      SUMA_SL_Err("Failed to create name");
      SUMA_RETURN(NULL);
   }
   
   if (exist1 || exist2) *exists = YUP;
   else                  *exists = NOPE;              /* 4 Nov 2005 [rickr] */
   
   sprintf(SF_name1->name_topo, "%s", SF_name2->name_topo);
   SUMA_free(SF_name2); SF_name2= NULL;
   
   SUMA_RETURN(SF_name1);
}  
/*!
   \brief A function to take a prefix (or name) and turn it into the structure needed
          by SUMA_Save_Surface_Object   
   also sets *exists = YUP if completed filename exists on disk. For surfaces requiring
            two files, *exists = YUP if any of the files exists       
   - free returned pointer with SUMA_free
   
   \sa SUMA_2Prefix2SurfaceName if you have a surface with different names for coord and topo )
*/

void * SUMA_Prefix2SurfaceName ( char *prefix_in, char *path, char *vp_name, 
                                 SUMA_SO_File_Type oType, SUMA_Boolean *exists)
{
   static char FuncName[]={"SUMA_Prefix2SurfaceName"};
   SUMA_SFname *SF_name = NULL;
   char *ppref = NULL, *prefix=NULL;
   void *SO_name = NULL;
   
   SUMA_ENTRY;
   
   if (!prefix_in) {
      fprintf (SUMA_STDERR,"Error %s: NULL name input\n", FuncName);
      SUMA_RETURN(NULL);
   }
   /* trim the prefix if necessary */
   if (!(prefix = SUMA_RemoveSurfNameExtension (prefix_in, oType))) {
      fprintf (SUMA_STDERR,"Error %s: Failed to remove extension\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   if (path) {
      if (path[strlen(path)-1] == '/') {
         ppref = SUMA_append_replace_string(path, prefix, "", 0);
      } else {
         ppref = SUMA_append_replace_string(path, prefix, "/", 0);
      }
   } else {
      ppref = SUMA_copy_string(prefix);
   }
   
   switch (oType) {
      case SUMA_SUREFIT:
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         snprintf(SF_name->name_coord, 
                  (SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH+1)*sizeof(char),
                   "%s.coord", ppref);
         snprintf(SF_name->name_topo,  
                  (SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH+1)*sizeof(char),
                   "%s.topo", ppref); 
         if (  SUMA_filexists(SF_name->name_topo) || 
               SUMA_filexists(SF_name->name_coord)) *exists = YUP;
         else *exists = NOPE;
         if (!vp_name) { /* initialize to empty string */
            SF_name->name_param[0] = '\0'; 
         }
         else {
            snprintf(SF_name->name_param, 
                     (SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH+1)*sizeof(char),
                     "%s", vp_name);
         }
         SO_name = (void *)SF_name;
         break;
      case SUMA_VEC:
         if (SF_name) SUMA_free(SF_name);
         SF_name = (SUMA_SFname *) SUMA_malloc(sizeof(SUMA_SFname));
         snprintf(SF_name->name_coord,
                  (SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH+1)*sizeof(char),
                   "%s.1D.coord", ppref);
         snprintf(SF_name->name_topo,
                  (SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH+1)*sizeof(char),
                   "%s.1D.topo", ppref); 
         if (  SUMA_filexists(SF_name->name_topo) || 
               SUMA_filexists(SF_name->name_coord)) *exists = YUP;
         else *exists = NOPE;
         SO_name = (void *)SF_name;
         break;
      case SUMA_FREE_SURFER:
      case SUMA_FREE_SURFER_PATCH:
         SO_name = (void *)SUMA_append_string(ppref,".asc"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;  
      case SUMA_OPENDX_MESH:
         SO_name = (void *)SUMA_append_string(ppref,".dx"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;  
      case SUMA_PLY:
         SO_name = (void *)SUMA_append_string(ppref,".ply"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;  
      case SUMA_MNI_OBJ:
         SO_name = (void *)SUMA_append_string(ppref,".obj"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;  
      case SUMA_BRAIN_VOYAGER:
         SO_name = (void *)SUMA_append_string(ppref,".srf"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;
      case SUMA_BYU:
         SO_name = (void *)SUMA_append_string(ppref,".g"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;
      case SUMA_INVENTOR_GENERIC:
         SO_name = (void *)SUMA_append_string(ppref,".iv"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;
      case SUMA_GIFTI:
         SO_name = (void *)SUMA_append_string(ppref,".gii"); 
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;
      default:
         fprintf (SUMA_STDERR,"Error %s: Unknown format.\n", FuncName);
         SO_name = (void *)SUMA_copy_string(ppref);
         if (SUMA_filexists((char*)SO_name)) *exists = YUP;
         else *exists = NOPE;
         break;
   }
   
   if (ppref) SUMA_free(ppref);
   if (prefix) SUMA_free(prefix); 
   
   SUMA_RETURN(SO_name);
}

/*! Read MNI_OBJ surface */
SUMA_Boolean SUMA_MNI_OBJ_Read(char * f_name, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_MNI_OBJ_Read"};
   char *fl=NULL, *flp = NULL;
   int ii=0, Found = 0, nread=0;
   double num = 0;
	int LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Sucking file");

   nread = SUMA_suck_file( f_name , &fl ) ;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(NOPE);
   }
	
   /* find 'P' */
   ii = 0; flp = fl;
   while (ii<nread && *flp != 'P') ++flp;
	if (ii == nread) {
      SUMA_S_Err("Failed to find 'P'");
      SUMA_RETURN(NOPE);
   }
   SUMA_SKIP_TO_NEXT_BLANK(flp, NULL);
   
   /* P is followed by 6 values, the last one being the number of points */
   ii=0;
   Found = 1;
   while (ii < 5) {
      if (Found) {
         SUMA_ADVANCE_PAST_NUM(flp, num, Found);
      } else {
         SUMA_S_Err("Don't understand format");
         SUMA_RETURN(NOPE);
      }
      ++ii;
   }
	/* get the sixth number */
   SUMA_ADVANCE_PAST_INT(flp, SO->N_Node, Found);
   if (!Found || SO->N_Node < 0 || SO->N_Node > 999999999) {
      SUMA_S_Errv("Bad N_Node of %d\n", SO->N_Node);
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Expecting %d nodes\n", SO->N_Node);
   SO->NodeDim = 3;
   if (!(SO->NodeList = (float *)
            SUMA_strtol_vec(flp, SO->NodeDim*SO->N_Node, &Found, 
                            SUMA_float, &flp))) {
      SUMA_S_Err("Failed to read node XYZ");
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Reading %d node normals\n", SO->N_Node);
   if (!(SO->NodeNormList = (float *)
            SUMA_strtol_vec(flp, SO->NodeDim*SO->N_Node, &Found, 
                            SUMA_float, &flp))) {
      SUMA_S_Err("Failed to read node XYZ");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: >>", FuncName);
      for (ii=0; ii<200; ++ii) {
         fprintf(SUMA_STDERR,"%c", flp[ii]);
      }
      fprintf(SUMA_STDERR,"\n");
   }
   
   SO->FaceSetDim = 3;
   SUMA_ADVANCE_PAST_INT(flp, SO->N_FaceSet, Found);
   if (!Found || SO->N_FaceSet < 0 || SO->N_FaceSet > 999999999) {
      SUMA_S_Errv("Bad N_FaceSet of %d\n", SO->N_FaceSet);
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Expecting to reading %d triangles\n", SO->N_FaceSet);
   
   /* skip past 5+SO->N_FaceSet integers for some reason */
   ii=0;
   Found = 1;
   while (ii < 5+SO->N_FaceSet) {
      if (Found) {
         SUMA_ADVANCE_PAST_NUM(flp, num, Found);
      } else {
         SUMA_S_Err("Don't understand format");
         SUMA_RETURN(NOPE);
      }
      ++ii;
   }
   SUMA_SKIP_BLANK(flp, NULL);
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: >>", FuncName);
      if (LocalHead) for (ii=0; ii<500 && flp[ii]; ++ii) {
         fprintf(SUMA_STDERR,"%c", flp[ii]);
      }
      fprintf(SUMA_STDERR,"\n");
   }
   if (!(SO->FaceSetList = (int *)
            SUMA_strtol_vec(flp, SO->FaceSetDim*SO->N_FaceSet, &Found, 
                            SUMA_int, &flp))) {
      SUMA_S_Err("Failed to read node XYZ");
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) { 
      for (ii=0; ii<10; ++ii) {fprintf(SUMA_STDERR,"%d ", SO->FaceSetList[ii]);}
   }
   /* fill up a few more fields */
   SO->FileType = SUMA_MNI_OBJ;
   SO->FileFormat = SUMA_ASCII;
            
   SO->Name = SUMA_StripPath(f_name);
   
   SUMA_RETURN(YUP);
}
SUMA_Boolean SUMA_MNI_OBJ_Write(char * f_name_in, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_MNI_OBJ_Write"};
   int ii=0;
   char *f_name=NULL, *f_name2=NULL;
   FILE *fid = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->NodeList || !SO->FaceSetList) {
      SUMA_S_Err("Null or incomplete surface");
      SUMA_RETURN(NOPE);
   }
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      SUMA_S_Err("NodeDim and FaceSetDim must be 3\n");
      SUMA_RETURN(NOPE);
   }
   if (!SO->NodeNormList) {
      SUMA_RECOMPUTE_NORMALS(SO)
   }
   if (!f_name_in) {
      fprintf (SUMA_STDERR, "Error %s: NULL filename\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   f_name = SUMA_Extension(f_name_in,".obj" , YUP); 
   f_name2  = SUMA_append_string(f_name,".obj");
   if (!THD_ok_overwrite() && SUMA_filexists (f_name2)) {
      fprintf (SUMA_STDERR, 
               "Error %s: file %s exists, will not overwrite.\n", 
               FuncName, f_name2);
      SUMA_free(f_name2);f_name2 = NULL;
      SUMA_free(f_name);f_name = NULL;
      SUMA_RETURN (NOPE);
   }
   SUMA_free(f_name); f_name = NULL;
   f_name = f_name2; f_name2 = NULL;
   
   if (!(fid = fopen(f_name,"w"))) {
      SUMA_S_Err("Could not open file for writing");
      SUMA_free(f_name); f_name = NULL; 
      SUMA_RETURN(NOPE);
   }
   
    
   fprintf(fid,"P 0.3 0.3 0.4 10 1 %d\n", SO->N_Node);
      /* don't know what numbers between O and SO->N_Node mean ...*/
   for (ii=0; ii < SO->N_Node; ++ii) {
      fprintf(fid," %f", SO->NodeList[3*ii]);
      fprintf(fid," %f", SO->NodeList[3*ii+1]);
      fprintf(fid," %f\n", SO->NodeList[3*ii+2]);
   }   
   for (ii=0; ii < SO->N_Node; ++ii) {
      fprintf(fid," %f", SO->NodeNormList[3*ii]);
      fprintf(fid," %f", SO->NodeNormList[3*ii+1]);
      fprintf(fid," %f\n", SO->NodeNormList[3*ii+2]);
   }
   
   /* number of triangles */
   fprintf(fid,"\n %d\n", SO->N_FaceSet);
   
   /* stuff I don't understand */
   fprintf(fid," 0 1 1 1 1\n"); /* dunno what that is */
   for (ii=0; ii<SO->N_FaceSet; ++ii) {
      if (!(ii%8)) fprintf(fid,"\n");
      fprintf(fid," %d", ii);  /* that is just a space filler. Don't quite
                                 know what these indices refer to in MNI_OBJ */
   }
      
   /* the triangle list */
   for (ii=0; ii<SO->FaceSetDim*SO->N_FaceSet; ++ii) {
      if (!(ii%8)) fprintf(fid,"\n");
      fprintf(fid," %d", SO->FaceSetList[ii]);  
   }
   
   fprintf(fid,"\n"); 
   
   SUMA_free(f_name); f_name = NULL; 
   
   fclose (fid);
   
   SUMA_RETURN(YUP);
}   

/*!**  
Function: SUMA_SureFit_Read_Coord 
Usage : 
Ret = SUMA_SureFit_Read_Coord (coordname, SureFit)
   
   
Input paramters : 
\param coordname (char *) name of .coord file
\param SureFit (SUMA_SureFit_struct *) pointer to the SureFit structure
   
Returns : 
\return  (SUMA_Boolean) YUP/NOPE for success/failure
   
Support : 
\sa   
\sa   
   
Side effects : 
   
   
   
***/
SUMA_Boolean SUMA_SureFit_Read_Coord (char * f_name, SUMA_SureFit_struct *SF)
{/*SUMA_SureFit_Read_Coord*/
   static char FuncName[]={"SUMA_SureFit_Read_Coord"}; 
   FILE *sf_file;
	int ex, EndHead, FoundHead, evl, cnt, skp, ND, id;
	char stmp[100], head_strt[100], head_end[100], s[1000], delimstr[] = {' ', '\0'}, *st;
	int LocalHead = 0;
   
	SUMA_ENTRY;
	
	ND = 3;
	
	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}
	
	sprintf(SF->name_coord, "%s", f_name);
	
	/* start reading */
	sf_file = fopen (f_name,"r");
	if (sf_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			SUMA_RETURN (NOPE);
		}

	/* read until you reach the begin header BeginHeader */
	ex = 1;
	FoundHead = 0;
	sprintf(head_strt,"BeginHeader");
	while (ex != EOF && !FoundHead)
	{
		ex = fscanf (sf_file,"%s",s);
		if (strlen (s) >= strlen(head_strt)) 
			{
				evl = SUMA_iswordin (s,head_strt);
				if (evl == 1) FoundHead = 1;
			}
	}
	
	if (!FoundHead) {
		fprintf(SUMA_STDERR,"Error %s: BeginHeader not found in %s.\nPerhaps you are using old versions of Caret/SureFit files.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}
	EndHead = 0;
	sprintf(head_end,"EndHeader");
	sprintf(delimstr," ");
	
	while (ex != EOF && !EndHead)	{
		ex = fscanf (sf_file,"%s",s);
		/*fprintf(stdout,"%s\n", s);*/
		if (strlen (s) >= strlen(head_end)) 
			{
				evl = SUMA_iswordin (s,head_end);
				if (evl == 1) EndHead = 1;
			}
		/* look for some tokens */
		skp = 0;
		if (!EndHead) {
			st = strtok(s, delimstr);
			sprintf(stmp,"encoding");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found encoding\n");*/
				ex = fscanf (sf_file,"%s",(SF->encoding_coord));
				skp = 1;
			}
			
			sprintf(stmp,"configuration_id");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found configuration_id\n");*/
				ex = fscanf (sf_file,"%s",(SF->configuration_id));
				skp = 1;
			}
			
			sprintf(stmp,"coordframe_id");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found configuration_id\n");*/
				ex = fscanf (sf_file,"%s",(SF->coordframe_id));
				skp = 1;
			}
         
         sprintf(stmp,"caret-version");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found caret-version\n");*/
				ex = fscanf (sf_file,"%f",&(SF->caret_version));
				skp = 1;
			}

		}
	}
	/* Now read the Number of Nodes */
	fscanf(sf_file, "%d", &SF->N_Node);
	if (LocalHead) fprintf (stdout,"Expecting %d nodes.\n", SF->N_Node);
   if (SF->N_Node <= 3) {
      SUMA_S_Err("Too few nodes!");
      SUMA_RETURN (NOPE);
   }
	
	/* allocate space */
	SF->NodeList = (float *)SUMA_calloc(SF->N_Node * ND, sizeof(float));
	SF->NodeId = (int *)SUMA_calloc (SF->N_Node, sizeof(int));
	
	if (SF->NodeList == NULL || SF->NodeId == NULL) {
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for NodeList &/| NodeId.\n", FuncName);
		SUMA_RETURN (NOPE);
	}
	
	/* Now read the nodes until the end of the file */
		cnt = 0;
		while (ex != EOF && cnt < SF->N_Node)	{
			id = cnt * ND;
			ex = fscanf (sf_file,"%d %f %f %f",&(SF->NodeId[cnt]), \
					&(SF->NodeList[id]), &(SF->NodeList[id+1]), &(SF->NodeList[id+2]));
			++cnt;
		}
	if (cnt != SF->N_Node) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d Nodes, read %d.\n"
                           "First triplet: %f %f %f\n"
                           "Last triplet: %f %f %f\n", FuncName, SF->N_Node, cnt,
                           SF->NodeList[0], SF->NodeList[1], SF->NodeList[2],
                           SF->NodeList[3*(cnt-1)], SF->NodeList[3*(cnt-1)+1], SF->NodeList[3*(cnt-1)+2]);
		SUMA_RETURN (NOPE);
	}
	fclose (sf_file);
	SUMA_RETURN (YUP); 
}/*SUMA_SureFit_Read_Coord*/

SUMA_Boolean SUMA_SureFit_Read_Topo (char * f_name, SUMA_SureFit_struct *SF)
{/*SUMA_SureFit_Read_Topo*/
	static char FuncName[]={"SUMA_SureFit_Read_Topo"}; 
	int ex = 0, EndHead, FoundHead, evl, cnt, skp, jnk, i, ip, NP, nread=0;
	char stmp[100], head_strt[100], head_end[100], s[1000], 
         delimstr[] = {' ', '\0'}, *st, *eop, *fl0, *fl1, *op2, *fl,
         *fleh, *flns, *flbh;
	int LocalHead = 0,  Found=0;
	double tmpdbl;
   
	SUMA_ENTRY;

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}

   SUMA_LH("Sucking file");

   nread = SUMA_suck_file( f_name , &fl ) ;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(NOPE);
   }
	
	sprintf(SF->name_topo, "%s", f_name);
	
	/* find BeginHeader and EndHeader tags*/
	fl0 = fl; /* beginning */
   fl1 = fl + nread; /* end */

   eop = SUMA_MIN_PAIR(fl1, fl+5000);
   SUMA_ADVANCE_PAST(fl,eop,"BeginHeader",Found,1);
	if (!Found) {
		fprintf(SUMA_STDERR,"Error %s: BeginHeader not found in %s.\nPerhaps you are using old versions of Caret/SureFit files.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}
   flbh = fl;
   
   
   SUMA_ADVANCE_PAST(fl,eop,"EndHeader",Found,1);
	if (!Found) {
		fprintf(SUMA_STDERR,"Error %s: EndHeader not found in %s.\nPerhaps you are using old versions of Caret/SureFit files.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}
   fleh = fl;
   
   /* find the header fields */
   fl = flbh;
   SUMA_ADVANCE_PAST(fl,fleh,"encoding",Found,1);
   if (Found) {
      op2 = fl;
      SUMA_SKIP_LINE(op2, fleh);
      snprintf(SF->encoding_topo, (op2-fl)*sizeof(char), "%s", fl);
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: Found encoding (%d) >>>%s<<<\n", FuncName, (int)(op2-fl), SF->encoding_topo);
      }  
   }
   fl = flbh;
   SUMA_ADVANCE_PAST(fl,fleh,"perimeter_id",Found,1);
   if (Found) {
      op2 = fl;
      SUMA_SKIP_LINE(op2, fleh);
      snprintf(SF->perimeter_id, (op2-fl)*sizeof(char), "%s", fl);
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: Found perimeter_id >>>%s<<<\n", FuncName, SF->perimeter_id);
      }  
   }
   
   fl = flbh;
   SUMA_ADVANCE_PAST(fl,fleh,"date",Found,1);
   if (Found) {
      op2 = fl;
      SUMA_SKIP_LINE(op2, fleh);
      snprintf(SF->date, (op2-fl)*sizeof(char), "%s", fl);
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: Found date >>>%s<<<\n", FuncName, SF->date);
      }  
   }
   
   
   /* is next string a number ? */
   fl = fleh;
   SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found);
   if (Found) {
      SF->N_Node_Specs = (int)tmpdbl;
      if (LocalHead) fprintf (stdout,"Expecting %d Node_Specs .\n", SF->N_Node_Specs);
      SF->N_FaceSet = -1; /* got to read it later*/
      SF->tag_version = -1; /* don't know */
      goto NODE_SPECS;
   }else {
      fl = fleh;
      eop = SUMA_MIN_PAIR(fl1, fl+500);
      SUMA_ADVANCE_PAST(fl, eop, "tag-version", Found, 0);
      if (Found) {
         /* read tag-version */
         SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found);
         SF->tag_version = (float) tmpdbl;
         if (Found) {
            if (LocalHead) fprintf (stdout,"Found tag-version %f\n", tmpdbl);
            if ((int)tmpdbl != 1) {
               SUMA_S_Warn("tag-version not equal to 1.\nSUMA may not know how to read this file.\n");
            }
         } else {
            if (LocalHead) fprintf (stdout,"Found tag-version but no number! Trying hope.\n");
            /* skip till end of line */
            SUMA_SKIP_LINE(fl, eop);   
         }
         
         SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found);
         if (Found) {
            SF->N_FaceSet = (int)tmpdbl;
            if (LocalHead) fprintf (stdout,"Found number of FaceSets: %d\n", SF->N_FaceSet);
         } else {
            SUMA_S_Err("No FaceSets number!");
            SUMA_RETURN (NOPE);
         }
         SF->N_Node_Specs = -1; /* not set */
         goto FACESETS;
      }else {
         SUMA_S_Err("Don't know how to interpret file!");
         SUMA_RETURN (NOPE);
      }
   }


	NODE_SPECS:
	SF->FN.N_Node = SF->N_Node_Specs;
	SF->FN.N_Neighb_max = 0;

	/* allocate for Node Specs Matrix and First_Neighb structure*/
	SF->Specs_mat = (int **) SUMA_allocate2D(SF->N_Node_Specs, 6, sizeof(int));
	/*assume maximum number of neighbors is SUMA_MAX_NUMBER_NODE_NEIGHB */
	SF->FN.FirstNeighb = (int **) SUMA_allocate2D(SF->FN.N_Node, SUMA_MAX_NUMBER_NODE_NEIGHB, sizeof (int));
	SF->FN.N_Neighb = (int *) SUMA_calloc (SF->FN.N_Node, sizeof(int));
	SF->FN.NodeId = (int *) SUMA_calloc (SF->FN.N_Node, sizeof(int));
	
	if (SF->Specs_mat == NULL || SF->FN.FirstNeighb == NULL || SF->FN.N_Neighb == NULL || SF->FN.NodeId == NULL ){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for SF->Specs_mat &/| SF->FN.FirstNeighb &/| SF->FN.N_Neighb &/| SF->FN.NodeId.\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	
	/* Now read the node specs */
	if (LocalHead) fprintf (stdout,"About to read specs\n");
	cnt = 0;
   do {
      for (i=0; i<6; ++i) {
         SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found);
         if (Found) {
            SF->Specs_mat[cnt][i] = (int)tmpdbl;
         } else {
            fprintf(SUMA_STDERR, "Error %s: Failed reading Specs_mat at number %d\n", FuncName, cnt);
            SUMA_RETURN (NOPE);
         }
      }
      SF->FN.NodeId[cnt] = SF->Specs_mat[cnt][0];
		SF->FN.N_Neighb[cnt] = SF->Specs_mat[cnt][1];
		if (SF->FN.N_Neighb[cnt] > SUMA_MAX_NUMBER_NODE_NEIGHB-1) {
			fprintf (SUMA_STDERR,"Error %s: Node %d has more neighbors (%d) than the maximum allowed (%d)\n", \
				FuncName, SF->FN.NodeId[cnt], SF->FN.N_Neighb[cnt], SUMA_MAX_NUMBER_NODE_NEIGHB-1);
			SUMA_RETURN (NOPE);
		}
		if (SF->FN.N_Neighb[cnt] > SF->FN.N_Neighb_max) SF->FN.N_Neighb_max = SF->FN.N_Neighb[cnt];
		
		/* Now Read in the Neighbors info */
		for (i=0; i < SF->FN.N_Neighb[cnt]; ++ i) {
			SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found); /* skip first number */
         SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found);
         if (Found) {
            SF->FN.FirstNeighb[cnt][i] = (int)tmpdbl;
         } else {
            fprintf (SUMA_STDERR,"Error %s: Failed to read neighbor index! cnt = %d, i = %d\n", FuncName, cnt, i);
            SUMA_RETURN (NOPE);
         }
		}
		/* seal with -1 */
		SF->FN.FirstNeighb[cnt][SF->FN.N_Neighb[cnt]] = -1;
		
		++cnt;
   } while (cnt < SF->N_Node_Specs);
   
	if (cnt != SF->N_Node_Specs) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d NodeSpecs, read %d.\n", FuncName, SF->N_Node_Specs, cnt);
		SUMA_RETURN (NOPE);
	}
   
   FACESETS:
   if (SF->N_FaceSet < 0) { 
      /* have to read it still */
      SUMA_ADVANCE_PAST_NUM(fl, tmpdbl, Found);
      if (Found) {
         SF->N_FaceSet = (int)tmpdbl;
         if (LocalHead) fprintf (stdout,"Found number of FaceSets: %d\n", SF->N_FaceSet);
      } else {
         SUMA_S_Err("No FaceSets number!");
         SUMA_RETURN (NOPE);
      }
   }
   
	if (LocalHead) fprintf (stdout, "Reading facesets\n");
   if (SF->N_FaceSet < 3) {
      fprintf(SUMA_STDERR, "Error %s: Too few (%d) triangles.\n", FuncName, SF->N_FaceSet);
      SUMA_RETURN (NOPE);
   }
	
	NP = 3;	
   SF->FaceSetList = (int *)SUMA_strtol_vec(fl, SF->N_FaceSet * NP, 
                                            &ex, SUMA_int, NULL);
   if (!SF->FaceSetList || ex != SF->N_FaceSet * NP) {
      fprintf(SUMA_STDERR, "Error %s: Failed to read all FaceSets. "
                           "Expected %d vals, read %d.\nOr NULL output.\n", 
                           FuncName, SF->N_FaceSet*NP, ex);
      SUMA_RETURN (NOPE);
   }
	
   SUMA_RETURN (YUP);
}/*SUMA_SureFit_Read_Topo*/

/* Old version , could not handle versions with tag-version string */
SUMA_Boolean SUMA_SureFit_Read_Topo_old (char * f_name, SUMA_SureFit_struct *SF)
{/*SUMA_SureFit_Read_Topo_old*/
	static char FuncName[]={"SUMA_SureFit_Read_Topo_old"}; 
   FILE *sf_file;
	int ex, EndHead, FoundHead, evl, cnt, skp, jnk, i, ip, NP;
	char stmp[100], head_strt[100], head_end[100], s[1000], delimstr[] = {' ', '\0'}, *st;
	int LocalHead = 1;
	
	SUMA_ENTRY;

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}
	
	sprintf(SF->name_topo, "%s", f_name);
	
	/* start reading */
	sf_file = fopen (f_name,"r");
	if (sf_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			SUMA_RETURN (NOPE);
		}

	/* read until you reach the begin header BeginHeader */
	ex = 1;
	FoundHead = 0;
	sprintf(head_strt,"BeginHeader");
	while (ex != EOF && !FoundHead)
	{
		ex = fscanf (sf_file,"%s",s);
		if (strlen (s) >= strlen(head_strt)) 
			{
				evl = SUMA_iswordin (s,head_strt);
				if (evl == 1) FoundHead = 1;
			}
	}
	if (!FoundHead) {
		fprintf(SUMA_STDERR,"Error %s: BeginHeader not found in %s.\nPerhaps you are using old versions of Caret/SureFit files.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}
	EndHead = 0;
	sprintf(head_end,"EndHeader");
	sprintf(delimstr," ");
	
	while (ex != EOF && !EndHead)	{
		ex = fscanf (sf_file,"%s",s);
		/*fprintf(stdout,"%s\n", s);*/
		if (strlen (s) >= strlen(head_end)) 
			{
				evl = SUMA_iswordin (s,head_end);
				if (evl == 1) EndHead = 1;
			}
		/* look for some tokens */
		skp = 0;
		if (!EndHead) {
			st = strtok(s, delimstr);
			sprintf(stmp,"encoding");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found encoding\n");*/
				ex = fscanf (sf_file,"%s",(SF->encoding_topo));
				skp = 1;
			}
			
			sprintf(stmp,"perimeter_id");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found perimeter_id\n");*/
				ex = fscanf (sf_file,"%s",(SF->perimeter_id));
				skp = 1;
			}
			
			sprintf(stmp,"date");
			if (!skp && SUMA_iswordin (st, stmp) == 1) {
				/*fprintf(stdout,"Found date\n");*/
				ex = fscanf (sf_file,"%s\n",(SF->date));
				skp = 1;
			}

		}
	}
	/* Now read the Number of Nodes Specs */
	ex = fscanf (sf_file,"%s",s);
   SF->N_Node_Specs = atoi(s);
   /* fscanf(sf_file, "%d", &SF->N_Node_Specs); */
	if (LocalHead) fprintf (stdout,"Expecting %d Node_Specs (from string %s) .\n", SF->N_Node_Specs, s);
	
   if (!SF->N_Node_Specs || SUMA_iswordin (s, "tag-") == 1) {
      if (LocalHead) fprintf (stdout,"Looks like SF file is in new format (%s).\n", s);
      goto FACESETS;
   }
   
	SF->FN.N_Node = SF->N_Node_Specs;
	SF->FN.N_Neighb_max = 0;

	/* allocate for Node Specs Matrix and First_Neighb structure*/
	SF->Specs_mat = (int **) SUMA_allocate2D(SF->N_Node_Specs, 6, sizeof(int));
	/*assume maximum number of neighbors is SUMA_MAX_NUMBER_NODE_NEIGHB */
	SF->FN.FirstNeighb = (int **) SUMA_allocate2D(SF->FN.N_Node, SUMA_MAX_NUMBER_NODE_NEIGHB, sizeof (int));
	SF->FN.N_Neighb = (int *) SUMA_calloc (SF->FN.N_Node, sizeof(int));
	SF->FN.NodeId = (int *) SUMA_calloc (SF->FN.N_Node, sizeof(int));
	
	if (SF->Specs_mat == NULL || SF->FN.FirstNeighb == NULL || SF->FN.N_Neighb == NULL || SF->FN.NodeId == NULL ){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for SF->Specs_mat &/| SF->FN.FirstNeighb &/| SF->FN.N_Neighb &/| SF->FN.NodeId.\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	
	/* Now read the node specs */
	/*fprintf (stdout,"About to read specs\n");*/
	cnt = 0;
	while (ex != EOF && cnt < SF->N_Node_Specs)	{
		ex = fscanf (sf_file,"%d %d %d %d %d %d",&(SF->Specs_mat[cnt][0]), &(SF->Specs_mat[cnt][1]), \
				&(SF->Specs_mat[cnt][2]), &(SF->Specs_mat[cnt][3]), &(SF->Specs_mat[cnt][4]), &(SF->Specs_mat[cnt][5]));
		SF->FN.NodeId[cnt] = SF->Specs_mat[cnt][0];
		SF->FN.N_Neighb[cnt] = SF->Specs_mat[cnt][1];
		if (SF->FN.N_Neighb[cnt] > SUMA_MAX_NUMBER_NODE_NEIGHB-1) {
			fprintf (SUMA_STDERR,"Error %s: Node %d has more neighbors (%d) than the maximum allowed (%d)\n", \
				FuncName, SF->FN.NodeId[cnt], SF->FN.N_Neighb[cnt], SUMA_MAX_NUMBER_NODE_NEIGHB-1);
			SUMA_RETURN (NOPE);
		}
		if (SF->FN.N_Neighb[cnt] > SF->FN.N_Neighb_max) SF->FN.N_Neighb_max = SF->FN.N_Neighb[cnt];
		
		/* Now Read in the Neighbors info */
		for (i=0; i < SF->FN.N_Neighb[cnt]; ++ i) {
			ex = fscanf (sf_file,"%d %d", &jnk, &(SF->FN.FirstNeighb[cnt][i]));
		}
		/* seal with -1 */
		SF->FN.FirstNeighb[cnt][SF->FN.N_Neighb[cnt]] = -1;
		
		++cnt;
	}
	if (cnt != SF->N_Node_Specs) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d NodeSpecs, read %d.\n", FuncName, SF->N_Node_Specs, cnt);
		SUMA_RETURN (NOPE);
	}
	
   FACESETS:
   
	/*fprintf (stdout, "Done with Node Specs.\n");*/
	ex = fscanf (sf_file,"%d", &(SF->N_FaceSet));
	if (LocalHead) fprintf (stdout, "Expecting to read %d facesets.\n", SF->N_FaceSet);
   if (SF->N_FaceSet < 3) {
      fprintf(SUMA_STDERR, "Error %s: Too few (%d) triangles.\n", FuncName, SF->N_FaceSet);
      SUMA_RETURN (NOPE);
   }
	
	NP = 3;
	SF->FaceSetList = (int *) SUMA_calloc(SF->N_FaceSet * 3, sizeof(int));
	if (SF->FaceSetList == NULL){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for SF->FaceSetList.\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	
   /*fprintf (stdout,"About to read FaceSets\n");*/
	cnt = 0;
	while (ex != EOF && cnt < SF->N_FaceSet)	{
		ip = NP * cnt;
		ex = fscanf (sf_file,"%d %d %d ",&(SF->FaceSetList[ip]), &(SF->FaceSetList[ip+1]), \
				&(SF->FaceSetList[ip+2]));
		++cnt;
	}
	if (cnt != SF->N_FaceSet) {
		fprintf(SUMA_STDERR, "Error %s: Expecting %d FaceSets, read %d.\n", FuncName, SF->N_FaceSet, cnt);
		SUMA_RETURN (NOPE);
	}
	fclose (sf_file);
	
SUMA_RETURN (YUP);
}/*SUMA_SureFit_Read_Topo_old*/

/*!
Show data structure containing SureFit surface object
*/
void SUMA_Show_SureFit (SUMA_SureFit_struct *SF, FILE *Out)
{	int cnt, id, ND, NP;
	static char FuncName[]={"SUMA_Show_SureFit"};
	
	SUMA_ENTRY;

	ND = 3;
	NP = 3;
	if (Out == NULL) Out = SUMA_STDOUT;
   fprintf (Out, "\n%s: Coord Info\n", SF->name_coord);
	fprintf (Out, "caret-version %f\n", SF->caret_version);
	fprintf (Out, "N_Node %d\n", SF->N_Node);
	fprintf (Out, "encoding_coord: %s\nconfiguration id: %s, coordframe_id: %s \n", SF->encoding_coord,SF->configuration_id, SF->coordframe_id);
	if (!SF->NodeId) {
      fprintf (Out, "NULL NodeId:\n");
   }
   if (!SF->NodeList) {
      fprintf (Out, "NULL NodeList:\n");
   }
   if (SF->NodeId && SF->NodeList) {
      fprintf (Out, "First 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		   SF->NodeId[0], SF->NodeList[0], SF->NodeList[1], SF->NodeList[2],
		   SF->NodeId[1], SF->NodeList[3], SF->NodeList[4], SF->NodeList[5]);
	   if (SF->N_Node > 2) {
         fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		      SF->NodeId[SF->N_Node-2], SF->NodeList[ND*(SF->N_Node-2)], SF->NodeList[ND*(SF->N_Node-2)+1], SF->NodeList[ND*(SF->N_Node-2)+2],
		      SF->NodeId[SF->N_Node-1], SF->NodeList[ND*(SF->N_Node-1)], SF->NodeList[ND*(SF->N_Node-1)+1], SF->NodeList[ND*(SF->N_Node-1)+2]);
	   }
   } 
   fprintf (Out, "\n%s: Topo Info\n", SF->name_topo);
	fprintf (Out, "N_Node_Specs %d\n", SF->N_Node_Specs);
	fprintf (Out, "ecnoding_topo: %s, date %s\n",  SF->encoding_topo, SF->date);
	fprintf (Out, "N_FaceSet %d\n", SF->N_FaceSet);
	if (!SF->FaceSetList) {
      fprintf (Out, "NULL SF->FaceSetList:\n");
   }
   if (SF->N_FaceSet > 2 && SF->FaceSetList) {
	   fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
		   SF->FaceSetList[0], SF->FaceSetList[1], SF->FaceSetList[2],
		   SF->FaceSetList[3], SF->FaceSetList[4], SF->FaceSetList[5]);
      fprintf (Out, "Last 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
		   SF->FaceSetList[NP*(SF->N_FaceSet-2)], SF->FaceSetList[NP*(SF->N_FaceSet-2) + 1], SF->FaceSetList[NP*(SF->N_FaceSet-2) + 2],
		   SF->FaceSetList[NP*(SF->N_FaceSet-1)], SF->FaceSetList[NP*(SF->N_FaceSet-1) + 1], SF->FaceSetList[NP*(SF->N_FaceSet-1) + 2]);
	} else if (SF->FaceSetList){
      fprintf (Out, "First polygon:\n\t%d %d %d\n", \
		   SF->FaceSetList[0], SF->FaceSetList[1], SF->FaceSetList[2] );
   }
   fprintf (Out, "\nNode Specs (%d):\n", SF->N_Node_Specs);
	if (SF->Specs_mat) {
      fprintf (Out, "First Entry: \t%d %d %d %d %d %d\n", \
	   SF->Specs_mat[0][0], SF->Specs_mat[0][1],SF->Specs_mat[0][2], SF->Specs_mat[0][3],SF->Specs_mat[0][4], SF->Specs_mat[0][5]);
	} else {
      fprintf (Out, "NULL SF->Specs_mat\n");
   }
   if (SF->FN.FirstNeighb) {
      cnt = 0;
	   while (cnt < SF->FN.N_Neighb[0]) {
		   fprintf (Out, "\t%d %d\n", cnt, SF->FN.FirstNeighb[0][cnt]); 
		   ++cnt;
	   }
   } else {
      fprintf (Out, "NULL SF->FN.FirstNeighb\n");
   }
	if (SF->Specs_mat) {
      fprintf (Out, "Last Entry: \t%d %d %d %d %d %d\n", \
		   SF->Specs_mat[SF->N_Node_Specs-1][0], SF->Specs_mat[SF->N_Node_Specs-1][1],SF->Specs_mat[SF->N_Node_Specs-1][2],\
		   SF->Specs_mat[SF->N_Node_Specs-1][3],SF->Specs_mat[SF->N_Node_Specs-1][4], SF->Specs_mat[SF->N_Node_Specs-1][5]);
	} 
   if (SF->FN.N_Neighb) {
      cnt = 0;
	   while (cnt < SF->FN.N_Neighb[SF->N_Node_Specs-1]) {
		   fprintf (Out, "\t%d %d\n", cnt, SF->FN.FirstNeighb[SF->N_Node_Specs-1][cnt]); 
		   ++cnt;
	   }
   }

	SUMA_RETURNe;
}

/*!
   \brief writes a surface in SureFit format
   ans = SUMA_SureFit_Write (Fname,SO);
   
   \param  Fname (SUMA_SFname *) uses the SureFit filename structure to store
                                 the names (and paths) of the NodeList (name_coord)
                                 and the FaceSetList (name_topo) files.
                                 If strlen(name_coord) == 0 then the coord file is not
                                 written out. 
                                 If strlen(name_topo) == 0 then topo file is not written out
   \param SO (SUMA_SurfaceObject *) pointer to SO structure.
   \return YUP/NOPE
   
   \sa SUMA_SureFit_Read_Topo() 
   \sa SUMA_SureFit_Read_Coord() 
   
   The function will not overwrite pre-existing files.
   

NOTE: Header info is incomplete. 
for .coord:
BeginHeader
configuration_id NA
coordframe_id NA
encoding ASCII
EndHeader

for .topo:
BeginHeader
date NA
encoding ASCII
perimeter_id NA
EndHeader

also, the last 4 integers in the node neighbor list lines are set to 0. I have never used them and do not know what they are for.

The naming convention of SureFit surfaces is not enforced.

*/  
SUMA_Boolean SUMA_SureFit_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO)
{
   
   static char FuncName[]={"SUMA_SureFit_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   SUMA_ENTRY;

   if (strlen(Fname->name_coord)) {
      if (!THD_ok_overwrite() && SUMA_filexists(Fname->name_coord)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }
   }
   
   if (strlen(Fname->name_topo)) {
      if (!THD_ok_overwrite() && SUMA_filexists(Fname->name_topo)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }
   }
   
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (strlen(Fname->name_coord)) {
      outFile = fopen(Fname->name_coord, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }

      /* write header */
      fprintf (outFile,"BeginHeader\nconfiguration_id NA\ncoordframe_id NA\nencoding ASCII\nEndHeader\n");
      fprintf (outFile,"%d\n", SO->N_Node);

      j=0;
      for (i=0; i<SO->N_Node; ++i) {
         j=SO->NodeDim * i;
         fprintf (outFile, "%d %f %f %f\n", i, SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
      }

      fclose (outFile);
   }
   
   if (strlen(Fname->name_topo)) {
      outFile = fopen(Fname->name_topo, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }

      /* make sure you have the first neighbor list ! */
      if (!SO->FN) {
         fprintf (SUMA_STDERR, "%s: Must compute Node Neighborhood list.\n", FuncName);
         if (!SUMA_SurfaceMetrics(SO, "EdgeList", NULL)){
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #if 0 /* better to use SUMA_SurfaceMetrics */
         if (!SO->EL) {
            fprintf (SUMA_STDERR, "%s: Computing Edge List...\n", FuncName);
            SO->EL = SUMA_Make_Edge_List (SO->FaceSetList, SO->N_FaceSet, SO->N_Node, SO->NodeList);
         }
         if (!SO->EL) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Make_Edge_List.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         fprintf (SUMA_STDERR, "%s: Computing FirstNeighb list.\n", FuncName);
         SO->FN = SUMA_Build_FirstNeighb (SO->EL, SO->N_Node);
         if (!SO->FN) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_Build_FirstNeighb.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #endif

      }
      /* write header */
      fprintf (outFile,"BeginHeader\ndate NA\nencoding ASCII\nperimeter_id NA\nEndHeader\n");
      fprintf (outFile,"%d\n", SO->N_Node);
	   j = 0;
	   while (j < SO->FN->N_Node)	{
         /* dunno what last 4 ints of upcoming line are */
		   fprintf (outFile,"%d %d 0 0 0 0\n", SO->FN->NodeId[j], SO->FN->N_Neighb[j]);

		   /* Now write the Neighbors info */
		   for (i=0; i < SO->FN->N_Neighb[j]; ++ i) {
			   fprintf (outFile,"%d %d\n", i, SO->FN->FirstNeighb[j][i]);
		   }
		   ++j;
	   }

	   fprintf (outFile,"%d\n", SO->N_FaceSet);

      j=0;
      for (i=0; i<SO->N_FaceSet; ++i) {
         j = SO->FaceSetDim * i;
         fprintf (outFile, "%d %d %d\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
      }

      fclose (outFile);
   }
   SUMA_RETURN (YUP);

}

/*!
free data structure containing SureFit surface object
*/
SUMA_Boolean SUMA_Free_SureFit (SUMA_SureFit_struct *SF)  
{
	static char FuncName[]={"SUMA_Free_SureFit"};
	SUMA_Boolean LocalHead = NOPE;
   
	SUMA_ENTRY;

	if (!SF) SUMA_RETURN (YUP);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%p, %p, %p, %p, %p, %p, %p\n", 
         SF->NodeList, SF->NodeId, SF->Specs_mat, SF->FN.FirstNeighb,
         SF->FN.N_Neighb, SF->FN.NodeId, SF->FaceSetList);
   }
   if (SF->NodeList != NULL) SUMA_free(SF->NodeList);
	if (SF->NodeId != NULL) SUMA_free(SF->NodeId);
	if (SF->Specs_mat != NULL) SUMA_free2D ((char **)SF->Specs_mat, SF->N_Node_Specs);
	if (SF->FN.FirstNeighb != NULL) SUMA_free2D((char **)SF->FN.FirstNeighb, SF->FN.N_Node);
	if (SF->FN.N_Neighb != NULL) SUMA_free(SF->FN.N_Neighb);
	if (SF->FN.NodeId != NULL) SUMA_free(SF->FN.NodeId);
	if (SF->FaceSetList != NULL) SUMA_free(SF->FaceSetList);
	if (SF!= NULL) SUMA_free(SF);
	
	SUMA_RETURN (YUP);
}

/*!
Read in some parameters from a .param file
*/
SUMA_Boolean SUMA_Read_SureFit_Param (char *f_name, SUMA_SureFit_struct *SF)
{
	static char FuncName[]={"SUMA_Read_SureFit_Param"};
	int ex, evl; 
   FILE *sf_file;
	SUMA_Boolean Done;
	char delimstr[] = {' ', '\0'}, stmp[100], s[1000], *st;
   SUMA_Boolean LocalHead = NOPE;
   
	SUMA_ENTRY;

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"File %s does not exist or cannot be read.\n", f_name);
		SUMA_RETURN (NOPE);
	}

	sprintf(SF->name_param, "%s", f_name);

	/* start reading */
	sf_file = fopen (f_name,"r");
	if (sf_file == NULL)
		{
			fprintf (SUMA_STDERR,"Error %s: Could not open input file ",FuncName);
			SUMA_RETURN (NOPE);
		}

	/* read until you reach something you like */
	SF->AC_WholeVolume[0] = SF->AC_WholeVolume[1] = SF->AC_WholeVolume[2] = 0.0f;
	SF->AC[0] = SF->AC[1] = SF->AC[2] = 0.0f;
   
   ex = 1;
	Done = NOPE;			
	sprintf(delimstr,"=");
	while (ex != EOF && !Done)
	{
		ex = fscanf (sf_file,"%s",s);
		if (LocalHead) fprintf(SUMA_STDERR, "Working >>>%s<<<\n", s);
		sprintf(stmp,"ACx_WholeVolume");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACx_WholeVolume */
			if (LocalHead) fprintf(SUMA_STDERR, "Found ACx_WholeVolume:");
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			if (st) {
            SF->AC_WholeVolume[0] = atof(st);
			   if (LocalHead) fprintf(SUMA_STDERR, " %f\n", SF->AC_WholeVolume[0]);
         } else {
            if (LocalHead) fprintf(SUMA_STDERR, "Empty field.\n"); 
         }
			continue;
		}
		sprintf(stmp,"ACy_WholeVolume");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACy_WholeVolume */
			if (LocalHead) fprintf(SUMA_STDERR, "Found ACy_WholeVolume:");
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			if (st) {
            SF->AC_WholeVolume[1] = atof(st);
            if (LocalHead) fprintf(SUMA_STDERR, " %f\n", SF->AC_WholeVolume[1]);
         } else {
            if (LocalHead) fprintf(SUMA_STDERR, "Empty field.\n"); 
         }
			
			continue;
		}
		sprintf(stmp,"ACz_WholeVolume");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACz_WholeVolume */
			if (LocalHead) fprintf(SUMA_STDERR, "Found ACz_WholeVolume:");
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			if (st) {
            SF->AC_WholeVolume[2] = atof(st);
            if (LocalHead) fprintf(SUMA_STDERR, " %f\n", SF->AC_WholeVolume[2]);
         } else {
            if (LocalHead) fprintf(SUMA_STDERR, "Empty field.\n"); 
         }
			continue;
		}
		 
		sprintf(stmp,"ACx");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACx */
			if (LocalHead) fprintf(SUMA_STDERR, "Found ACx:");
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			if (st) {
            SF->AC[0] = atof(st);
            if (LocalHead) fprintf(SUMA_STDERR, " %f\n", SF->AC[0]);
         } else {
            if (LocalHead) fprintf(SUMA_STDERR, "Empty field.\n"); 
         }
			continue;
		}
		sprintf(stmp,"ACy");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACy */
			if (LocalHead) fprintf(SUMA_STDERR, "Found ACy:");
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			if (st) {
            SF->AC[1] = atof(st);
			   if (LocalHead) fprintf(SUMA_STDERR, " %f\n", SF->AC[1]);
			} else {
            if (LocalHead) fprintf(SUMA_STDERR, "Empty field.\n"); 
         }
         continue;
		}
		sprintf(stmp,"ACz");
		evl = SUMA_iswordin (s,stmp);
		if (evl == 1) {
			/* found ACz */
			if (LocalHead) fprintf(SUMA_STDERR, "Found ACz:");
			/* go past the = sign and grab the value */
			st = strtok(s, delimstr);
			st = strtok(NULL, delimstr);
			if (st) {
            SF->AC[2] = atof(st);
			   if (LocalHead) fprintf(SUMA_STDERR, " %f\n", SF->AC[2]);
			} else {
            if (LocalHead) fprintf(SUMA_STDERR, "Empty field.\n"); 
         }
         continue;
		}
		
	}
	
	fclose(sf_file);
	
	/* Sanity Checks */
	if (SF->AC[0] == 0.0f && SF->AC[1] == 0.0f && SF->AC[2] == 0.0f) {
		if (SF->caret_version < 5.2) fprintf (SUMA_STDERR,"Warning %s: All values for AC are 0.0.\n", FuncName);
		/* SUMA_RETURN (NOPE); */
	}
	
	if (SF->AC_WholeVolume[0] == 0.0f && SF->AC_WholeVolume[1] == 0.0f && SF->AC_WholeVolume[2] == 0.0f) {
		if (SF->caret_version < 5.2) fprintf (SUMA_STDERR,"Warning %s: All values for AC_WholeVolume are 0.0. \n", FuncName);
		/* SUMA_RETURN (NOPE); */
	}
	
	if (SF->AC[0] == SF->AC_WholeVolume[0] && SF->AC[1] == SF->AC_WholeVolume[1] && SF->AC[2] == SF->AC_WholeVolume[2])
	{
		if (SF->caret_version < 5.2) SUMA_SL_Warn("Idetincal values for AC and AC_WholeVolume.\nCheck your params file if not using Talairach-ed surfaces.\n");
      /* looks like that's OK for TLRC surfaces ...*/
      /*
      fprintf (SUMA_STDERR,"Error %s: Idetincal values for AC and AC_WholeVolume. Check your params file.\n", FuncName);
		SUMA_RETURN (NOPE);
      */
	}
	if (SF->AC[0] < 0.0f || SF->AC[1] < 0.0f || SF->AC[2] < 0.0f || SF->AC_WholeVolume[0] < 0.0f || SF->AC_WholeVolume[1] < 0.0f || SF->AC_WholeVolume[2] < 0.0f) 
	{
		fprintf (SUMA_STDERR,"Error %s: Negative values in AC or AC_WholeVolume. Check your params file.\n", FuncName);
		SUMA_RETURN (NOPE);
	}
	
	SUMA_RETURN (YUP);
}
#ifdef SUMA_SureFit_STAND_ALONE



void usage_SUMA_SureFit_Main ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_SureFit CoordRoot TopoRoot \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSUMA_SureFit_STAND_ALONE -Wall -o $1 $1.c -I./ -I//usr/X11R6/include SUMA_lib.a\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \tFri Feb 8 16:29:06 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_SureFit_Main"}; 
   char SF_name[200];
	SUMA_SureFit_struct *SF;
	
   SUMA_mainENTRY;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
	/* Allocate for SF */
	SF = (SUMA_SureFit_struct *) SUMA_calloc(1,sizeof(SUMA_SureFit_struct));	
	if (SF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for SF\n", FuncName);
		exit(1);
	}
   
   if (argc < 3)
       {
          usage_SUMA_SureFit_Main ();
          exit (1);
       }
   
	sprintf(SF_name, "%s.coord", argv[1]);
	if (!SUMA_SureFit_Read_Coord (SF_name, SF)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Coord.\n", FuncName);
		exit(1);
	}
	
	sprintf(SF_name, "%s.topo", argv[2]);
	if (!SUMA_SureFit_Read_Topo (SF_name, SF)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SureFit_Read_Topo.\n", FuncName);
		exit(1);
	}
	
	SUMA_Show_SureFit (SF, NULL);
	fprintf(stdout, "freeing ..\n");
	if (!SUMA_Free_SureFit (SF)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Free_SureFit.\n", FuncName);
		exit(1);
	}
	
	if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

	SUMA_RETURN (0);
}/* Main */
#endif


/*! Functions to read and manipulate FreeSurfer surfaces*/

#define SUMA_FS_ANNOT_TAG_COLORTABLE   1
#define SUMA_FS_STRLEN 50
typedef struct {
   int i;
   int r;
   int g;
   int b;
   int flag;
   char name[SUMA_FS_STRLEN];
} SUMA_FS_COLORTABLE_ENTRY;

typedef struct {
   char *fname;
   int nbins;
   SUMA_FS_COLORTABLE_ENTRY *bins;
} SUMA_FS_COLORTABLE;

SUMA_FS_COLORTABLE *SUMA_CreateFS_ColorTable(int nbins, int len, SUMA_FS_COLORTABLE *cto)
{
   static char FuncName[]={"SUMA_CreateFS_ColorTable"};
   SUMA_FS_COLORTABLE *ct = NULL;
   
   SUMA_ENTRY;
   
   if (!cto) {
      ct = (SUMA_FS_COLORTABLE*) SUMA_calloc(1,sizeof(SUMA_FS_COLORTABLE));
      if (!ct) {
         SUMA_SL_Crit("Failed to allocate for ct");
         SUMA_RETURN(NULL);
      }
      ct->nbins = nbins;
      ct->bins = (SUMA_FS_COLORTABLE_ENTRY *) 
                     SUMA_calloc(nbins, sizeof(SUMA_FS_COLORTABLE_ENTRY));

      ct->fname = (char *)SUMA_malloc((len + 1)*sizeof(char));
      if (!ct->bins || !ct->fname) {
         SUMA_SL_Crit("Failed to allocate for ct fields");
         if (ct->bins) SUMA_free(ct->bins);
         if (ct->fname) SUMA_free(ct->fname);
         SUMA_free(ct);
         SUMA_RETURN(NULL);
      }
      ct->fname[0] = '\0';
      SUMA_RETURN(ct);
   } else {
      cto->bins = (SUMA_FS_COLORTABLE_ENTRY *) SUMA_realloc(cto->bins, nbins * 
                                             sizeof(SUMA_FS_COLORTABLE_ENTRY));
      cto->nbins = nbins;
      SUMA_RETURN(cto);
   }
}

SUMA_FS_COLORTABLE *SUMA_FreeFS_ColorTable (SUMA_FS_COLORTABLE *ct)
{
   static char FuncName[]={"SUMA_FreeFS_ColorTable"};
   
   SUMA_ENTRY;
   
   if (!ct) SUMA_RETURN(NULL);
   
   if (ct->bins) SUMA_free(ct->bins);
   if (ct->fname) SUMA_free(ct->fname);
   
   SUMA_free(ct);
   
   SUMA_RETURN(NULL);
}

char *SUMA_FS_ColorTable_Info(SUMA_FS_COLORTABLE *ct)
{
   static char FuncName[]={"SUMA_FS_ColorTable_Info"};
   char *s=NULL;
   int i;
   SUMA_STRING *SS = NULL;

   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   if (!ct) SS = SUMA_StringAppend(SS,"NULL ct");
   else {
      if (ct->fname) SS = SUMA_StringAppend_va(SS, "fname: %s\nnbins: %d\n", ct->fname, ct->nbins);
      else SS = SUMA_StringAppend_va(SS, "fname: NULL\nnbins: %d\n", ct->nbins);
      if (!ct->bins) SS = SUMA_StringAppend_va(SS, "NULL bins\n");
      else {
         for (i=0; i<ct->nbins; ++i) {
            SS = SUMA_StringAppend_va(SS, "bin[%d]: %d   %d %d %d %d : %s\n", 
                                       i, ct->bins[i].i, ct->bins[i].r, ct->bins[i].g, 
                                       ct->bins[i].b, ct->bins[i].flag,
                                       ct->bins[i].name);
         }
      }   
   }
   
   SS = SUMA_StringAppend(SS,NULL);
   s = SS->s; 
   SUMA_free(SS);
   
   SUMA_RETURN(s);

}

SUMA_Boolean SUMA_Show_FS_ColorTable(SUMA_FS_COLORTABLE *ct, FILE *fout)
{
   static char FuncName[]={"SUMA_Show_FS_ColorTable"};
   char *s=NULL;
   
   SUMA_ENTRY;
   
   if (!fout) fout = stdout;
   
   s = SUMA_FS_ColorTable_Info(ct);
   if (s) {
      fprintf(fout, "%s\n", s);
      SUMA_free(s);
   } else {
      SUMA_SL_Err("Failed in SUMA_FS_ColorTable_Info");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_readFScolorLUT(char *f_name, SUMA_FS_COLORTABLE **ctp)
{
   static char FuncName[]={"SUMA_readFScolorLUT"};
   SUMA_FS_COLORTABLE *ct=NULL;
   SUMA_FS_COLORTABLE_ENTRY *ce;
   char *fl=NULL, *fl2=NULL, *colfile=NULL, *florig=NULL, *name=NULL, *fle=NULL;
   double dum;
   int i, r, g, b, v, nalloc, nchar, ok, ans, cnt;
   SUMA_Boolean state = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (f_name) colfile = f_name;
   else {
       char *eee = getenv("FREESURFER_HOME");
       if (eee) { 
         colfile = SUMA_append_replace_string(eee,"FreeSurferColorLUT.txt", "/", 0);
       } else {
         SUMA_S_Err("FREESURFER_HOME environment variable not set");
         SUMA_RETURN(NOPE);
       }
   }
   if (ctp && *ctp) {
      SUMA_S_Err("Colortable pointer pointer must point to null!");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_filexists(colfile)) {
      SUMA_S_Errv("File %s not found.\n", colfile); 
      state = NOPE; goto CLEANUP;
      SUMA_RETURN(NOPE);
   }
   /* suck file */
   
   if (!(fl = florig = SUMA_file_suck(colfile, &nchar))) {
      SUMA_S_Errv("Faile to read %s\n", colfile);
      state = NOPE; goto CLEANUP;
   }
   fle = fl+nchar;
   nalloc = 256;
   ct = SUMA_CreateFS_ColorTable(nalloc, strlen(colfile), NULL);
   snprintf(ct->fname, (strlen(colfile)+1)*sizeof(char),"%s", colfile);
   ok = 1;
   cnt = 0;
   while (ok && fl < fle) {
      SUMA_SKIP_BLANK(fl, fle);
      do {
         /* skip comment, if any */
         SUMA_IS_COMMENT_LINE(fl, fle, '#', ans);
         if (ans) {  
            SUMA_LH("Skipping comment..."); 
            SUMA_SKIP_LINE(fl, fle);
         }
      } while (ans); 
      SUMA_SKIP_BLANK(fl, fle); if (fl == fle) goto DONEREAD; 
      if (cnt >= nalloc) {
         nalloc = nalloc+256;
         ct = SUMA_CreateFS_ColorTable(nalloc, -1, ct);
      }
      ce = &ct->bins[cnt]; 
      
      /* read first number */
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok); 
      if (!ok && fl!=fle) { SUMA_S_Err("Failed to read i"); state = NOPE; goto CLEANUP; }
      SUMA_LHv("index %f\n", dum);
      ce->i=(int)dum;
      SUMA_GET_BETWEEN_BLANKS(fl, NULL, fl2);
      if (fl2 > fl) {
         SUMA_COPY_TO_STRING(fl, fl2, name);
         SUMA_LHv("name %s\n", name); 
         snprintf((ce->name), (SUMA_FS_STRLEN-1)*sizeof(char), "%s", name);
         SUMA_free(name); name = NULL;
         fl = fl2;
      }
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
      if (!ok) { SUMA_S_Err("Failed to read r"); state = NOPE; goto CLEANUP; }
      SUMA_LHv("r %f\n", dum);
      ce->r=(int)dum;
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
      if (!ok) { SUMA_S_Err("Failed to read g"); state = NOPE; goto CLEANUP; }
      SUMA_LHv("g %f\n", dum);
      ce->g=(int)dum;
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
      if (!ok) { SUMA_S_Err("Failed to read b"); state = NOPE; goto CLEANUP; }
      SUMA_LHv("b %f\n", dum);
      ce->b=(int)dum;
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
      if (!ok) { SUMA_S_Err("Failed to read v"); state = NOPE; goto CLEANUP; }
      SUMA_LHv("v %f\n", dum);
      ce->flag=(int)dum;
      ++cnt;
   }
      
   DONEREAD:
   ct = SUMA_CreateFS_ColorTable(cnt,  -1, ct);
   if (LocalHead) SUMA_Show_FS_ColorTable(ct, NULL);

   CLEANUP:
   if (ctp) *ctp = ct;
   if (!f_name && colfile) SUMA_free(colfile); colfile = NULL;
   if (florig) SUMA_free(florig);
   
   SUMA_RETURN(state);
}

SUMA_COLOR_MAP *SUMA_FScolutToColorMap(char *fscolutname, int lbl1, int lbl2, int show) 
{
   static char FuncName[]={"SUMA_FScolutToColorMap"};
   SUMA_COLOR_MAP *SM=NULL;
   SUMA_FS_COLORTABLE *ct=NULL;
   int cnt =0, ism = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(SUMA_readFScolorLUT(fscolutname, &ct))) {
      SUMA_S_Err("Failed baby, failed.");
      SUMA_RETURN(SM);
   }
   
   if (show) {
      SUMA_Show_FS_ColorTable(ct, NULL);
   }
   
   /* allocate for SM */
   SM = (SUMA_COLOR_MAP*) SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   SM->top_frac = 0.0f;
   SM->SO = NULL; 
   SM->N_Col = lbl2-lbl1+1;
   SM->cname = (char **)SUMA_calloc(SM->N_Col, sizeof(char*));
   SM->M = (float**)SUMA_allocate2D (SM->N_Col, 3, sizeof(float));
   SM->Name = SUMA_copy_string(ct->fname);
   SM->Sgn = 0;
   SM->frac = NULL;
   
   /* first find lbl1 */
   cnt = 0;
   while (cnt < ct->nbins && ct->bins[cnt].i != lbl1) ++cnt;
   
   if (ct->bins[cnt].i == lbl1) { /* Found the starting point */
      ism = 0;
      while (cnt < ct->nbins && ct->bins[cnt].i <= lbl2 && ism < SM->N_Col) {
         SUMA_LHv("ct->bins[cnt].i %d <> lbl1+ism %d\n", 
                  ct->bins[cnt].i, lbl1+ism);
         if (ct->bins[cnt].i == lbl1+ism) {
            SM->M[ism][0] = (float)(ct->bins[cnt].r) / 255.0;
            SM->M[ism][1] = (float)(ct->bins[cnt].g) / 255.0;
            SM->M[ism][2] = (float)(ct->bins[cnt].b) / 255.0;
            SM->cname[ism] = SUMA_copy_string(ct->bins[cnt].name);
            SUMA_LHv("FSi %d --> SMi %d\n", ct->bins[cnt].i, ism);
            ++cnt;   
         } else {
            SM->M[ism][0] = SM->M[ism][1] = SM->M[ism][2] = SUMA_DUNNO_GRAY;
            SM->cname[ism] = SUMA_copy_string("undefined"); 
            SUMA_LH("Got gap\n");
         }
         ++ism;
      }
   } 

   SM->M0[0] = SM->M[0][0]; 
   SM->M0[1] = SM->M[0][1]; 
   SM->M0[2] = SM->M[0][2]; 
 
   SUMA_RETURN(SM);
}

/*!
   \brief
      function to read FS's annotation file 
      
   \param f_name (char *)
   \param ROIout (FILE *)
   \param cmapout (FILE *)
   
   - If a colormap is found, it should be added to SUMAg_CF->scm
   - Based on code provided by Bruce Fischl
*/
SUMA_Boolean SUMA_readFSannot (char *f_name, char *f_ROI, char *f_cmap, char *f_col, int Showct)
{
   static char FuncName[]={"SUMA_readFSannot"};
   int n_labels = -1, ex, ni, chnk, j, annot, r, g, b, imap;
   int tg, len, nbins, i;
   FILE *fl=NULL;
   FILE *fr=NULL;
   FILE *fc=NULL;
   FILE *fo=NULL; 
   SUMA_FS_COLORTABLE *ct=NULL;
   SUMA_FS_COLORTABLE_ENTRY *cte;
   SUMA_Boolean bs = NOPE;
   int *rv=NULL, *gv=NULL, *bv=NULL, *anv=NULL, *niv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}else if (LocalHead) {
		fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	}
   if (f_ROI) { /* check for existence of ROI file */
      if (SUMA_filexists(f_ROI)) { 
         fprintf(SUMA_STDERR,"Error %s: File %s exists, will not overwrite.\n", FuncName, f_ROI);
	      SUMA_RETURN (NOPE);
      }else {
         fr = fopen(f_ROI, "w");
         if (!fr) {
            SUMA_SL_Err("Failed to open file for writing.\n");
            SUMA_RETURN(NOPE);
         } 
      }
   }
   
   if (f_cmap) { /* check for existence of ROI file */
      if (SUMA_filexists(f_cmap)) { 
         fprintf(SUMA_STDERR,"Error %s: File %s exists, will not overwrite.\n", FuncName, f_cmap);
	      SUMA_RETURN (NOPE);
      }else {
         fc = fopen(f_cmap, "w");
         if (!fc) {
            SUMA_SL_Err("Failed to open file for writing.\n");
            SUMA_RETURN(NOPE);
         }  
      }
   }
   
   if (f_col) { /* check for existence of ROI file */
      if (SUMA_filexists(f_col)) { 
         fprintf(SUMA_STDERR,"Error %s: File %s exists, will not overwrite.\n", FuncName, f_col);
	      SUMA_RETURN (NOPE);
      }else {
         fo = fopen(f_col, "w");
         if (!fo) {
            SUMA_SL_Err("Failed to open file for writing.\n");
            SUMA_RETURN(NOPE);
         }  
      }
   }
   
   bs = NOPE;
   fl = fopen(f_name, "r");
   if (!fl) {
      SUMA_SL_Err("Failed to open file for reading.\n");
      SUMA_RETURN(NOPE);
   }
   chnk = sizeof(int);
   ex = fread (&n_labels, chnk, 1, fl);
   if (n_labels < 0 || n_labels > 500000) { /* looks like swapping is needed */
      if (LocalHead) {
         SUMA_SL_Warn("Byte swapping binary data.");
      }
      SUMA_swap_4( &n_labels ) ;
      bs = YUP;
   }
   
   
   niv = (int *) SUMA_malloc(sizeof(int)*n_labels);
   rv = (int *) SUMA_malloc(sizeof(int)*n_labels);
   gv = (int *) SUMA_malloc(sizeof(int)*n_labels);
   bv = (int *) SUMA_malloc(sizeof(int)*n_labels);
   anv = (int *) SUMA_malloc(sizeof(int)*n_labels);
   if (!rv || !gv || !bv || !anv || !niv) {
      SUMA_SL_Crit("Failed to allocate.");
      SUMA_RETURN(NOPE);
   }

   if (ex != EOF) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Expecting to read %d labels\n", FuncName, n_labels);
   }   
   SUMA_LH("Reading annotations...");
   for (j=0; j<n_labels; ++j) {
      SUMA_READ_INT (&ni, bs, fl, ex);
      if (ni < 0 || ni >= n_labels) {
         fprintf(SUMA_STDERR,"Error %s:\n Read a node index of %d. Bad because  < 0 or > %d\n", FuncName, ni, n_labels);
         SUMA_RETURN(NOPE);
      }
      SUMA_READ_INT (&annot, bs, fl, ex);
      niv[j] = ni;
      anv[j] = annot;
      rv[j] = annot & 0x0000ff;
      gv[j] = (annot >> 8) & 0x0000ff;
      bv[j] = (annot >> 16) & 0x0000ff;
      if (LocalHead && ( j < 5 || j > n_labels - 5)) {
         fprintf (SUMA_STDERR, "annot[%d]: %d = %d %d %d\n", 
                                 ni, anv[j], rv[j], gv[j], bv[j]);
      }
   }
   
   SUMA_LH("Searching for colortables");
   while (!feof(fl)) {
      SUMA_READ_INT (&tg, bs, fl, ex);
      SUMA_LHv("tg = %d (%d is FS_ANNOT_TAG_COLORTABLE)\n", tg,  SUMA_FS_ANNOT_TAG_COLORTABLE);
      if (tg == SUMA_FS_ANNOT_TAG_COLORTABLE) {
         if (ct) {
            SUMA_S_Warn("Already have a color table\n"
                        "ignoring second one in file.\n"
                        "If you want it, post a message to\n"
                        "AFNI's message board\n");
            break;
         }
         SUMA_LH("Found color table");
         SUMA_READ_INT (&nbins, bs, fl, ex);
         SUMA_READ_INT (&len, bs, fl, ex);
         ct = SUMA_CreateFS_ColorTable(nbins, len, NULL);
         fread(ct->fname, sizeof(char), len, fl) ;
         SUMA_LHv("fname: %s\n", ct->fname);
         for (i = 0 ; i < nbins ; i++)
         {
                cte = &ct->bins[i] ; cte->i = i;
                SUMA_READ_INT (&len, bs, fl, ex);
                if (len < 0 || len > SUMA_FS_STRLEN ) {
                     SUMA_SL_Err("Too long a name");
                     SUMA_RETURN(NOPE);
                }
                fread(cte->name, sizeof(char), len, fl) ;
                SUMA_READ_INT (&(cte->r), bs, fl, ex);
                SUMA_READ_INT (&(cte->g), bs, fl, ex);
                SUMA_READ_INT (&(cte->b), bs, fl, ex);
                SUMA_READ_INT (&(cte->flag), bs, fl, ex);
                SUMA_LHv("name: %s, r,g,b,f = %d %d %d %d\n", 
                     cte->name, cte->r, cte->g, cte->b, cte->flag);
         }
         
      }
      /* reset flag values, on mac for some reason feof seems to fail even if on 
         linux say feof(fl) would return true at that point. When that happens,
         tg keeps its last value and a new colortable of junk gets built
         */
      tg = -1;  nbins = -1;
   }             
   
   if (fc) { /* write the colormap to a file */
      fprintf(fc, "#name\n#bin  r  g  b  flag \n");
      for (i=0; i<ct->nbins; ++i) {
         fprintf(fc, "#%s\n", ct->bins[i].name);
         fprintf(fc, "%d   %f %f %f %d\n", 
                     i, (float)ct->bins[i].r/255.0, (float)ct->bins[i].g/255.0, 
                     (float)ct->bins[i].b/255.0, ct->bins[i].flag );
      }
   }
   
   if (fr) { /* write the annotation, ROI style  */
      if (ct) {
         fprintf(fr, "#NodeID  ROI(indexed into labels cmap)  r  g  b \n");
         for (i=0; i < n_labels; ++i) {
               j = 0;
               imap = -1;
               while (j < ct->nbins && imap < 0) {
                  if (  SUMA_ABS(ct->bins[j].r - rv[i]) < SUMA_EPSILON  &&
                        SUMA_ABS(ct->bins[j].b - bv[i]) < SUMA_EPSILON &&
                        SUMA_ABS(ct->bins[j].g - gv[i]) < SUMA_EPSILON  ) {
                     imap = j;
                  }
                  ++j;
               }
               if (imap < 0) {
                  static int iwarn;
                  if (!iwarn) {
                     SUMA_SL_Warn("Node Color (label) not found in cmap.\nMarking with annotation value.\nFurther occurences will not be reported.");
                     ++iwarn;
                  }
                  imap = anv[i];
               }
               fprintf(fr, "%d  %d  %f %f %f   \n",
                        niv[i], imap, (float)rv[i]/255.0, (float)gv[i]/255.0, (float)bv[i]/255.0);
         }
      } else { /* no color map */
         fprintf(fr, "#NodeID  Annotation\n");
         for (i=0; i < n_labels; ++i) {
            fprintf(fr, "%d  %d \n", niv[i], anv[i]);
         }
      }
   }
   
   if (fo) { /* write the annotation, ROI style  */
      if (ct) {
         fprintf(fo, "#NodeID  r  g  b \n");
         for (i=0; i < n_labels; ++i) {
               fprintf(fo, "%d  %f %f %f   \n",
                        niv[i], (float)rv[i]/255.0, (float)gv[i]/255.0, (float)bv[i]/255.0);
         }
      } else { /* no color map */
         fprintf(fo, "#NodeID  Annotation\n");
         for (i=0; i < n_labels; ++i) {
            fprintf(fo, "%d  %d \n", niv[i], anv[i]);
         }
      }
   }
   
   if (Showct) {
      if (ct) {
         SUMA_Show_FS_ColorTable(ct, NULL);
         ct = SUMA_FreeFS_ColorTable(ct);
      }else {
         fprintf(SUMA_STDOUT,"No color table found.\n");
      }
   }
   
   /* package the results for SUMA */
   /* 1- Transform ct to a SUMA_COLOR_MAP (do that BEFORE the free operation above) 
         The cname field in SUMA_COLOR_MAP was created for that purpose.
         First allocate for cmap then use SUMA_copy_string to fill it with 
         the names*/
   /* 2- Create a vector from the labels and create a data set from it */ 
   
   if (fl) fclose (fl); fl = NULL;
   if (fr) fclose (fr); fr = NULL;
   if (fc) fclose (fc); fc = NULL;
   if (fo) fclose (fo); fo = NULL;
   
   if (niv) SUMA_free(niv); niv = NULL;
   if (rv) SUMA_free(rv); rv = NULL;
   if (gv) SUMA_free(gv); gv = NULL;
   if (bv) SUMA_free(bv); bv = NULL;
   if (anv) SUMA_free(anv); anv = NULL;
   
   SUMA_RETURN(YUP);
} 


/*!

   \brief   v = SUMA_readFScurv (curvname, nrows, ncols, rowmajor, SkipCoords);
            Not terribly useful of a function because it is practically a .1D
            format!
   \param   curvname (char *) name of ascii curvature file
   \param   nrows (int *) will contain the number of elements in file
   \param   ncols (int *) will contain the number of columns in file.
                           This is 5 for curvature files if SkipCoords is not set
                           (node index, x, y, z, data)
                           2 if SkipCoords
                           (node index, data)
   \param   rowmajor (SUMA_Boolean) if (YUP) return results in row major order, else column major
   \param   SkipCoords (SUMA_Boolean) if (YUP) reads only node index and data value (1st and last column
                                       of curvature files (skips the intermediary coordinates)
   \return   v (float *) vector containing curvature file data
   
*/

float * SUMA_readFScurv (char *f_name, int *nrows, int *ncols, SUMA_Boolean rowmajor, SUMA_Boolean SkipCoords)   
{
   static char FuncName[]={"SUMA_readFScurv"};
   MRI_IMAGE *im = NULL;
   float *v = NULL, jnk, *far;
   int cnt, ex, id, i, ncol, nvec;
   char c, comment[SUMA_MAX_STRING_LENGTH];
   FILE *fs_file = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NULL);
	}else if (LocalHead) {
		fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	}
   
   if (SkipCoords) *ncols = 2;
   else *ncols = 5; /* constant */	

   if (*ncols !=2 && *ncols != 5) {
      SUMA_SL_Crit("ncols must be either 2 or 5");
      SUMA_RETURN(NULL);
   }


   #if 0
      /* thought is had the number of nodes at the top ! */
      /* start reading */
	   fs_file = fopen (f_name,"r");
	   if (fs_file == NULL) {
         SUMA_SL_Err ("Could not open input file");
         SUMA_RETURN (v);
	   }

	   /* read first character and check if it is a comment */
	   ex = fscanf (fs_file,"%c",&c);
	   if (c == '#') {
		   if (LocalHead) fprintf (SUMA_STDOUT, "%s: Found comment\n", FuncName); 

         /*skip till next line */
		   cnt = 0;
		   while (ex != EOF && c != '\n') {
			   ex = fscanf (fs_file,"%c",&c);
			   if (cnt < SUMA_MAX_STRING_LENGTH-2) {
				   sprintf(comment, "%s%c", comment, c);
				   ++cnt;
			   } else {
				   fprintf(SUMA_STDERR,"Error %s: Too long a comment in curvature file, increase SUMA_MAX_STRING_LENGTH\n", FuncName);
				   SUMA_RETURN (NOPE);
			   }
		   }
	   }


	   /* read in the number of nodes */
	   ex = fscanf(fs_file, "%d", nrows);

      if (*nrows <= 0) {
         SUMA_SL_Crit("Trouble parsing curvature file.\nNull or negative number of nodes\n");
         SUMA_RETURN(NULL);
      }

	   if (LocalHead) fprintf (SUMA_STDOUT, "%s: Allocating for data (%dx%d) \n", FuncName, *nrows, *ncols);

      v = (float *) SUMA_calloc(*nrows * *ncols, sizeof(float));
      if (!v) {
         SUMA_SL_Crit("Failed to allocate for v");
         SUMA_RETURN(v);
      }

      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Parsing file...\n", FuncName);

      if (rowmajor) {
         cnt = 0;
	      if (*ncols == 5) {
            while (ex != EOF && cnt < *nrows) {
		         id = *ncols * cnt;
		         ex = fscanf(fs_file, "%f %f %f %f %f", &(v[id]), &(v[id+1]),&(v[id+2]), &(v[id+3]), &(v[id+4]) );
		         ++cnt;
	         }
         } else {
            while (ex != EOF && cnt < *nrows) {
		         id = *ncols * cnt;
		         ex = fscanf(fs_file, "%f %f %f %f %f", &(v[id]), &jnk, &jnk, &jnk, &(v[id+1]) );
		         ++cnt;
	         }
         } 

      } else {
         cnt = 0;
	      if (*ncols == 5) {
            while (ex != EOF && cnt < *nrows) {
		         ex = fscanf(fs_file, "%f %f %f %f %f", &(v[cnt]), &(v[cnt+ *nrows]),&(v[cnt+ 2 * *nrows]), &(v[cnt+ 3 * *nrows]), &(v[cnt+ 4 * *nrows]));
		         ++cnt;
	         }
         } else if (*nrows == 2) {
            while (ex != EOF && cnt < *nrows) {
		         ex = fscanf(fs_file, "%f %f %f %f %f", &(v[cnt]), &jnk, &jnk, &jnk, &(v[cnt+ *nrows]));
		         ++cnt;
	         }
         } else {

         }
      }

      if (cnt != *nrows) {
		   fprintf(SUMA_STDERR,"Error %s: Expected %d rows, %d read.\n", FuncName, *nrows, cnt);
		   SUMA_free(v); v = NULL;
         SUMA_RETURN (NULL);
	   }
   #else 
      /* now load the input data */
      SUMA_LH("Reading file...");
      im = mri_read_1D (f_name);

      if (!im) {
         SUMA_SL_Err("Failed to read 1D file");
         SUMA_RETURN(NULL);
      }

      far = MRI_FLOAT_PTR(im);
      nvec = im->nx;
      ncol = im->ny;
      /* data in column major order at this point */

      if (!nvec) {
         SUMA_SL_Err("Empty file");
         SUMA_RETURN(NULL);
      }

      if (ncol != 5) {
         SUMA_SL_Err("Must have 5 columns in data file.");
         mri_free(im); im = NULL;   /* done with that baby */
         SUMA_RETURN(NULL);
      }
      
      *nrows = nvec;
      
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Allocating for data (%dx%d) \n", FuncName, *nrows, *ncols);

      v = (float *) SUMA_calloc(*nrows * *ncols, sizeof(float));
      
      if (!v) {
         SUMA_SL_Crit("Failed to allocate for v");
         SUMA_RETURN(v);
      }

      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Parsing file...\n", FuncName);

      if (rowmajor) {
	      if (*ncols == 5) {
            SUMA_LH("RowMajor, All Coords");
            for (i=0; i< *nrows; ++i) {
               id = *ncols*i;
               v[id] = far[i];
               v[id+1] = far[i+*nrows];
               v[id+2] = far[i+2 * *nrows];
               v[id+3] = far[i+3 * *nrows];
               v[id+4] = far[i+4 * *nrows];
            }   
         } else {
            SUMA_LH("RowMajor, Skipping Coords");
            for (i=0; i< *nrows; ++i) {
               id = *ncols*i;
               v[id] = far[i];
               v[id+1] = far[i+4 * *nrows];
            }
         } 
      } else {
	      if (*ncols == 5) {
            SUMA_LH("ColMajor, All Coords");
            for (i=0; i<*ncols * *nrows; ++i) v[i] = far[i];
         } else if (*ncols == 2) {
            SUMA_LH("ColMajor, Skipping Coords");
            for (i=0; i<*nrows; ++i) v[i] = far[i];
            for (i=*nrows; i< 2 * *nrows; ++i) v[i] = far[i+3 * *nrows];
         } 
      }
      mri_free(im); im = NULL; far = NULL;
   #endif
   SUMA_RETURN(v);
}

   
/* just call engine with debug set                20 Oct 2003 [rickr] */
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS)
{/* SUMA_FreeSurfer_Read */
   static char FuncName[]={"SUMA_FreeSurfer_Read"};

   SUMA_ENTRY;

   SUMA_RETURN(SUMA_FreeSurfer_Read_eng(f_name, FS, 1));
}/* SUMA_FreeSurfer_Read */

   
/* - changed function name to XXX_eng             20 Oct 2003 [rickr]
 * - added debug parameter
 * - print non-error info only if debug
*/
/*!**  
Usage : 
Ret = SUMA_FreeSurfer_Read_eng (surfname, FreeSurfer, debug)
	
	For a full surface definition, it is assumed that the first line can be a comment.
	The second line contains the number of nodes followed by the number of FaceSets
	The NodeList follows with X Y Z 0
	The FaceSetList follows with i1 i2 i3 0
   
	
Input paramters : 
\param surfname (char *) name of surface (or patch) file output by:
        mris_convert <surface_name> <surface_name.asc>
   or if it is a patch by
        mris_convert -p <patch_name> <patch_name.asc>
       
\param FreeSurfer (SUMA_FreeSurfer_struct *) pointer to the FreeSurfer structure

\param debug (int) flag specifying whether to output non-error info
   
Returns : 
\return  (SUMA_Boolean) YUP/NOPE for success/failure
   
Support : 
\sa   LoadFreeSurf.m
\sa   
   
Side effects : 
   
   
   
***/
SUMA_Boolean SUMA_FreeSurfer_Read_eng (char * f_name, SUMA_FreeSurfer_struct *FS, int debug)
{/*SUMA_FreeSurfer_Read_eng*/
	static char FuncName[]={"SUMA_FreeSurfer_Read_eng"};
   char stmp[50]; 
   FILE *fs_file;
	int ex, cnt, jnki, amax[3], maxamax, maxamax2, id, ND, id2, NP, ip, *NodeId;
	float jnkf, *NodeList;
	char c;
	SUMA_Boolean LocalHead = NOPE;
	   
	SUMA_ENTRY;

	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}else if ( debug > 1) {
		fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	}
	
	
	/* start reading */
	fs_file = fopen (f_name,"r");
	if (fs_file == NULL)
		{
			SUMA_error_message (FuncName,"Could not open input file ",0);
			SUMA_RETURN (NOPE);
		}

	sprintf(FS->name, "%s", f_name);
	
	/* read first character and check if it is a comment */
	ex = fscanf (fs_file,"%c",&c);
	if (LocalHead) fprintf (SUMA_STDOUT, "%s: --->%c<---\n", FuncName, c);
   if (c == '#') {
		if (LocalHead) fprintf (SUMA_STDOUT, "%s: Found comment\n", FuncName); 
		
      /*skip till next line */
		cnt = 0;
		FS->comment[cnt] = '#'; 
		while (ex != EOF && c != '\n') {
			ex = fscanf (fs_file,"%c",&c);
			if (cnt < SUMA_MAX_STRING_LENGTH-2) {
				++cnt;
				FS->comment[cnt] = c;
			} else {
				fprintf(SUMA_STDERR,"Error %s: Too long a comment in FS file, increase SUMA_FS_MAX_COMMENT_LENGTH\n", FuncName);
				SUMA_RETURN (NOPE);
			}
		}
      ++cnt;
		FS->comment[cnt] = '\0';
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Comment:-->%s<--", FuncName, FS->comment);
	}
	
	/* find out if surface is patch */
	sprintf(stmp,"patch");
	if (SUMA_iswordin (FS->comment, stmp) == 1) {
		FS->isPatch = YUP;
	   SUMA_LH("is patch");
   }
	else {
		FS->isPatch = NOPE;
      SUMA_LH("ain't patch");
	}
		
	/* read in the number of nodes and the number of facesets */
	ex = fscanf(fs_file, "%d %d", &(FS->N_Node), &(FS->N_FaceSet));
	
   if (FS->N_Node <= 0 || FS->N_FaceSet <= 0) {
      SUMA_SL_Crit("Trouble parsing FreeSurfer file.\nNull or negative number of nodes &/| facesets.\n");
      SUMA_RETURN(NOPE);
   }
   
	if (LocalHead) fprintf (SUMA_STDOUT, "%s: Allocating for NodeList (%dx3) and FaceSetList(%dx3)\n", FuncName, FS->N_Node, FS->N_FaceSet);
   
   /* allocate space for NodeList and FaceSetList */
	FS->NodeList = (float *)SUMA_calloc(FS->N_Node * 3, sizeof(float));
	FS->FaceSetList = (int *)SUMA_calloc(FS->N_FaceSet * 3, sizeof(int));
	FS->NodeId = (int *)SUMA_calloc(FS->N_Node, sizeof(int));
	if (FS->NodeList == NULL || FS->FaceSetList == NULL || FS->NodeId == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for FS->NodeList &/| FS->FaceSetList &/| FS->NodeId\n", FuncName);
		SUMA_RETURN (NOPE);
	} 
	if (FS->isPatch) {
		FS->FaceSetIndexInParent = (int *)SUMA_calloc(FS->N_FaceSet, sizeof(int));
		if (FS->FaceSetIndexInParent == NULL) {
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for FS->FaceSetIndexInParent\n", FuncName);
			SUMA_RETURN (NOPE);
		}
	} else {
		FS->FaceSetIndexInParent = NULL;
	}
	
	if (!FS->isPatch) {
	   if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading full surface...\n", FuncName);
		/* read in the nodes */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_Node) {
			FS->NodeId[cnt] = cnt;
			id = 3 * cnt;
			ex = fscanf(fs_file, "%f %f %f %f", &(FS->NodeList[id]), &(FS->NodeList[id+1]),&(FS->NodeList[id+2]), &jnkf);
			++cnt;
		}
		if (cnt != FS->N_Node) {
			fprintf(SUMA_STDERR,"Error %s: File %s; Expected %d nodes, %d read.\n", FuncName, f_name, FS->N_Node, cnt);
			SUMA_RETURN (NOPE);
		}

		/* read in the facesets */
		cnt = 0;
		while (ex != EOF && cnt < FS->N_FaceSet) {
			ip = 3 * cnt;
			ex = fscanf(fs_file, "%d %d %d %d", &(FS->FaceSetList[ip]), &(FS->FaceSetList[ip+1]),&(FS->FaceSetList[ip+2]), &jnki);
			++cnt;
		}
		if (cnt != FS->N_FaceSet) {
			fprintf(SUMA_STDERR,"Error %s: File %s; expected %d FaceSets, %d read.\n", FuncName, f_name, FS->N_FaceSet, cnt);
			SUMA_RETURN (NOPE);
		}
	} /* read a full surface */
	else { /* that's a patch */
	   #if 0 /* old way, simple parsing ... */
         if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading patch olde way...\n", FuncName);
		   /* Node IDs are a reference to those in the parent surface */
		   cnt = 0;
         while (ex != EOF && cnt < FS->N_Node) {
			   ex = fscanf(fs_file, "%d", &(FS->NodeId[cnt]));
			   id = 3 * cnt;
            /* fprintf (SUMA_STDERR, "FS->NodeId[cnt] = %d: cnt = %d, id=%d, id1 = %d, id2 = %d\n", FS->NodeId[cnt], cnt, id, id+1, id+2); */ 
			   ex = fscanf(fs_file, "%f %f %f", &(FS->NodeList[id]),&(FS->NodeList[id+1]),&(FS->NodeList[id+2]));
			   ++cnt;
		   }
         if (cnt != FS->N_Node) {
			   fprintf(SUMA_STDERR,"Error %s: Expected %d nodes, %d read.\n", FuncName, FS->N_Node, cnt);
			   SUMA_RETURN (NOPE);
		   }

         if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading FaceSets...\n", FuncName);
		   /* read in the facesets */
		   cnt = 0;
		   while (ex != EOF && cnt < FS->N_FaceSet) {
			   ex = fscanf(fs_file, "%d", &(FS->FaceSetIndexInParent[cnt]));
			   ip = 3 * cnt;
			   ex = fscanf(fs_file, "%d %d %d",  &(FS->FaceSetList[ip]), &(FS->FaceSetList[ip+1]),&(FS->FaceSetList[ip+2]));
			   ++cnt;
		   }
		   if (cnt != FS->N_FaceSet) {
			   fprintf(SUMA_STDERR,"Error %s: Expected %d FaceSets, %d read.\n", FuncName, FS->N_FaceSet, cnt);
			   SUMA_RETURN (NOPE);
		   }
      #else 
      {
         char *fl=NULL, *eop=NULL, *florig=NULL;
         int ans, pnodes, ptri, Found, nchar;
         double dbuf;
         
         if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading patch new way...\n", FuncName);
         /* suck in the bastard, perhaps this should be done from the start, maybe in the future */  
         fl = florig = SUMA_file_suck(f_name, &nchar);
         if (!nchar || !fl) { 
            SUMA_SL_Err("Failed to read patch file.");
            SUMA_RETURN(NOPE);
         }
         /* skip comment, if any */
         SUMA_IS_COMMENT_LINE(fl, NULL, '#', ans);
         if (ans) {  
            SUMA_LH("Skipping comment..."); 
            SUMA_SKIP_LINE(fl, NULL);
         }else {
            SUMA_SL_Err("Expected comment line....");
         }
         /* read in first two nums */
         SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); pnodes = (int)dbuf;
         SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); ptri = (int)dbuf;
         
         /* Node IDs are a reference to those in the parent surface */
		   Found = 1;
         cnt = 0;
         while (Found && cnt < FS->N_Node) {
			   eop = fl+50; /* don't you dare use (fl+50) instead of eop below!, else you will serch till the end all the time!
                           Macro Danger! */
            SUMA_ADVANCE_PAST(fl, eop,"vno=",Found,0);  /* The new patch format ... */
            SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->NodeId[cnt] = (int)dbuf;
			   id = 3 * cnt;
            /* fprintf (SUMA_STDERR, "FS->NodeId[cnt] = %d: cnt = %d, id=%d, id1 = %d, id2 = %d\n", FS->NodeId[cnt], cnt, id, id+1, id+2); */ 
			   SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->NodeList[id] = (float)dbuf;
            SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->NodeList[id+1] = (float)dbuf;
            SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->NodeList[id+2] = (float)dbuf;
			   ++cnt;
		   }
         if (cnt != FS->N_Node) {
			   fprintf(SUMA_STDERR,"Error %s: Expected %d nodes, %d read.\n", FuncName, FS->N_Node, cnt);
			   SUMA_RETURN (NOPE);
		   }
         if (LocalHead) fprintf (SUMA_STDOUT, "%s: Reading FaceSets...\n", FuncName);
		   /* read in the facesets */
		   Found = 1;
		   cnt = 0;
         while (Found && cnt < FS->N_FaceSet) {
			   SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->FaceSetIndexInParent[cnt] = (int)dbuf;
			   ip = 3 * cnt;
			   SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->FaceSetList[ip  ] = (int)dbuf;
			   SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->FaceSetList[ip+1] = (int)dbuf;
			   SUMA_ADVANCE_PAST_NUM(fl, dbuf, Found); FS->FaceSetList[ip+2] = (int)dbuf;
			   ++cnt;
		   }
		   if (cnt != FS->N_FaceSet) {
			   fprintf(SUMA_STDERR,"Error %s: Expected %d FaceSets, %d read.\n", FuncName, FS->N_FaceSet, cnt);
			   SUMA_RETURN (NOPE);
		   }
         SUMA_free(florig); fl = florig = NULL; /*  */
      }   
      #endif   
      
		/* The FaceSet List which will be read next, uses indices into the NodeList of the parent surface
		This means that it expects a NodeList of the size of the NodeList in the parent surface. 
		One could read the maximum number of nodes in the parent surface and create a NodeList of that size.
		However, that would require keeping track of the link between the patch file and the parent file.
		Instead, I will search through the FaceSetList for the highest index and allocate a new nodelist to match it*/
      
		SUMA_MAX_VEC(FS->FaceSetList, FS->N_FaceSet * 3, maxamax); ++maxamax;
      /* make sure that the node list does not refer to nodes of an index higher than that in NodeId */
      SUMA_MAX_VEC(FS->NodeId, FS->N_Node, maxamax2); ++maxamax2;
      if (maxamax2 > maxamax) {
         fprintf(SUMA_STDERR,"Error %s: Found NodeId in the NodeList larger than Ids found in FaceSetList.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Copying NodeList, allocating for new nodelist %dx3 elements...\n", \
         FuncName, maxamax);
         
		NodeList = (float *)SUMA_calloc(maxamax * 3, sizeof(float));
		NodeId = (int *)SUMA_calloc (maxamax, sizeof(int));
      
      if (NodeList == NULL || NodeId == NULL)
		{
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for NodeList or NodeId\n", FuncName);
			SUMA_RETURN (NOPE);
		} 
		/*Now copy pertinent nodes into NodeList */
		
      for (cnt=0; cnt< FS->N_Node; ++cnt) {
			id = 3*cnt; 
			id2 = 3*FS->NodeId[cnt];
         /* fprintf (SUMA_STDERR, "%s: id = %d id2 = %d\n", FuncName, id, id2); */
			NodeList[id2] = FS->NodeList[id];
			NodeList[id2+1] = FS->NodeList[id+1];
			NodeList[id2+2] = FS->NodeList[id+2];
		}
		
      /* this is redundant here, but should be done to match what comes out of a full surface */
      for (cnt=0; cnt< maxamax; ++cnt) {
         NodeId[cnt] = cnt;
      }
      
      /* Now free FS->NodeList & FS->NodeId */
		SUMA_free(FS->NodeList);
		SUMA_free(FS->NodeId);
      
		/*make FS->NodeList be NodeList */
		FS->NodeList = NodeList;
      FS->NodeId = NodeId;
		FS->N_Node = maxamax;
	} /* read a patch */
	
	fclose (fs_file);
	SUMA_RETURN (YUP);
	
}/* SUMA_FreeSurfer_Read_eng*/

SUMA_Boolean SUMA_FreeSurfer_WritePatch (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine, SUMA_SurfaceObject *SO_parent)
{
   static char FuncName[]={"SUMA_FreeSurfer_WritePatch"};
   int cnt, i, iface;
   int *FaceSetIndexInParent=NULL;
   SUMA_Boolean *isInPatch=NULL;
   FILE *fout=NULL;
   
   SUMA_ENTRY;
   
   if (!fileNm || !SO || !SO_parent || !SO_parent->EL) {
      SUMA_SL_Err("NULL input params");
      SUMA_RETURN(NOPE);
   }
   
   if (!THD_ok_overwrite() && SUMA_filexists(fileNm)) {
      SUMA_SL_Err("Output file exists, will not overwrite");
      SUMA_RETURN(NOPE);
   }
   
   fout = fopen(fileNm,"w");
   if (!fout) {
      SUMA_SL_Err("Failed to open file for writing.\nCheck permissions.");
      SUMA_RETURN(NOPE);
   }
   
   if (firstLine) {
      fprintf(fout, "%s\n", firstLine);
   } else {
      if (!SO->Label) SO->Label = SUMA_SurfaceFileName (SO, NOPE);
      fprintf(fout, "#!ascii version of patch %s\n", SO->Label);
   }
   
   /* which nodes are in this patch ? */
   isInPatch = SUMA_MaskOfNodesInPatch(SO, &cnt);
   if (!isInPatch) {
      SUMA_SL_Crit("Failed in SUMA_MaskOfNodesInPatch");
      SUMA_RETURN(NOPE);
   }

   /* number of nodes and number of triangles in mesh */
   fprintf(fout, "%d %d\n", cnt, SO->N_FaceSet);
   /* write the node coordinates */
   for (i=0; i < SO->N_Node; ++i) {
      if (isInPatch[i]) {
         fprintf(fout, "%d\n%f\t%f\t%f\n", i, SO->NodeList[3*i], SO->NodeList[3*i+1], SO->NodeList[3*i+2]);
      }
   }
   for (i=0; i < SO->N_FaceSet; ++i) {
      iface = SUMA_whichTri (SO_parent->EL, SO->FaceSetList[3*i], SO->FaceSetList[3*i+1], SO->FaceSetList[3*i+2], 0);
      if (iface < 0) {
         SUMA_SL_Warn("Parent surface does not contain triangle in patch!\nTriangle skipped.");
      } else {
         fprintf(fout, "%d\n%d\t%d\t%d\n", iface, SO->FaceSetList[3*i], SO->FaceSetList[3*i+1], SO->FaceSetList[3*i+2]);
      }
   }
   
   
   SUMA_free(FaceSetIndexInParent); FaceSetIndexInParent = NULL;
   SUMA_free(isInPatch); isInPatch = NULL;

   fclose(fout);
   SUMA_RETURN(YUP);
}
/*!
   
   Thanks for info from Graham Wideman, https://wideman-one.com/gw/brain/fs/surfacefileformats.htm
*/
SUMA_Boolean SUMA_FreeSurfer_ReadBin_eng (char * f_name, SUMA_FreeSurfer_struct *FS, int debug)
{/*SUMA_FreeSurfer_ReadBin_eng*/
	static char FuncName[]={"SUMA_FreeSurfer_ReadBin_eng"};
   char stmp[50]; 
   FILE *fs_file;
	int ex, End, rmax,  chnk, cnt, i, amax[3], maxamax, maxamax2, id, ND, id2, NP, ip, *NodeId, magic;
	float jnkf, *NodeList;
	char c;
   byte m1, m2, m3;
	SUMA_Boolean bs;
   SUMA_Boolean LocalHead = NOPE;
	   
	SUMA_ENTRY;
   
   if (debug) LocalHead = YUP;
   
	/* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}else if ( debug > 1) {
		fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	}
	
	/* start reading */
	fs_file = fopen (f_name,"r");
	if (fs_file == NULL)
		{
			SUMA_SL_Err ("Could not open input file ");
			SUMA_RETURN (NOPE);
		}

   SUMA_WHAT_ENDIAN(End);
   if (End == MSB_FIRST) {
      SUMA_LH("No swapping needed");
      bs = NOPE;
   } else {
      bs = YUP;
      SUMA_LH("Swapping needed");
   }
   
   ex = fread (&m1, 1, 1, fs_file);
   ex = fread (&m2, 1, 1, fs_file);
   ex = fread (&m3, 1, 1, fs_file);
   magic = (m1 << 16) + (m2 << 8) + m3 ;
   if (magic == (-2 & 0x00ffffff)) {
      SUMA_LH("OK tri");
   } else {
      SUMA_SL_Err("Failed to identify magic number for a triangulated surface.\n");
      SUMA_RETURN(NOPE);
   }
   chnk = sizeof(char);
   ex = fread (&c, chnk, 1, fs_file);
   rmax = 0;
   while (c != '\n' &&rmax < 5000) {
      if (LocalHead) fprintf(SUMA_STDERR,"%c",c);
      ex = fread (&c, chnk, 1, fs_file);
      ++rmax;
   }
   if (rmax >= 5000) {
      SUMA_SL_Err("Unexpected tres tres long comment.");
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("End of comment");
   /* read one more to skip second \n */
   ex = fread (&c, chnk, 1, fs_file);
   if (c != '\n') {
      SUMA_SL_Err("Failed to find second newline.");
      SUMA_RETURN(NOPE);
   }else {
      SUMA_LH("Found end of comment");
   }
   
   /* read the number of nodes and the number of triangles */
   SUMA_READ_INT (&FS->N_Node, bs, fs_file, ex);
   if (FS->N_Node < 0 || FS->N_Node > 2000000) {
      SUMA_SL_Err("Failed to get number of nodes");
      SUMA_RETURN(NOPE);
   }else {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Expecting to read %d nodes.\n", FuncName, FS->N_Node);
   }
   
   SUMA_READ_INT (&FS->N_FaceSet, bs, fs_file, ex);
   if (FS->N_FaceSet < 0 || FS->N_FaceSet > 2000000) {
      SUMA_SL_Err("Failed to get number of triangles");
      SUMA_RETURN(NOPE);
   }else {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Expecting to read %d triangles.\n", FuncName, FS->N_FaceSet);
   }
   
   /* allocate */
	FS->NodeList = (float *)SUMA_calloc(FS->N_Node * 3, sizeof(float));
	FS->FaceSetList = (int *)SUMA_calloc(FS->N_FaceSet * 3, sizeof(int));
   if (!FS->NodeList || !FS->FaceSetList) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   
   /*read in the meat */
   for (i=0; i<FS->N_Node * 3; ++i) {
      SUMA_READ_FLOAT (&(FS->NodeList[i]), bs, fs_file, ex);
      if (ex == EOF) {
         SUMA_SL_Err("Premature end of file!");
         SUMA_free(FS->NodeList); SUMA_free(FS->FaceSetList);  FS->NodeList = NULL ; FS->FaceSetList = NULL;
         SUMA_RETURN(NOPE);
      }
   }
   for (i=0; i<FS->N_FaceSet * 3; ++i) {
      SUMA_READ_INT (&(FS->FaceSetList[i]), bs, fs_file, ex);
      if (ex == EOF) {
         SUMA_SL_Err("Premature end of file!");
         SUMA_free(FS->NodeList); SUMA_free(FS->FaceSetList);  FS->NodeList = NULL ; FS->FaceSetList = NULL;
         SUMA_RETURN(NOPE);
      }   
   }
   
   fclose(fs_file);
   SUMA_LH("Returning");
   SUMA_RETURN(YUP);
}
/*! 
	free memory allocated for FreeSurfer structure  
*/
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS)
{
	static char FuncName[]={"SUMA_Free_FreeSurfer"};
   
   SUMA_ENTRY;

	if (FS->FaceSetList != NULL) SUMA_free(FS->FaceSetList);
	if (FS->NodeList != NULL) SUMA_free(FS->NodeList);
	if (FS->NodeId != NULL) SUMA_free(FS->NodeId);
	if (FS->FaceSetIndexInParent != NULL) SUMA_free(FS->FaceSetIndexInParent);
	if (FS != NULL) SUMA_free(FS);
	SUMA_RETURN (YUP);
}

/*! 
	Show elements of FreeSurfer structure 
*/
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out)
{	
	static char FuncName[]={"SUMA_Show_FreeSurfer"};
	int ND = 3, id, ip;
	
	SUMA_ENTRY;

	if (Out == NULL) Out = SUMA_STDOUT;
	if (FS->comment) fprintf (Out, "Comment: %s\n", FS->comment);
   else fprintf (Out, "Comment: NULL\n");
	fprintf (Out, "N_Node %d\n", FS->N_Node);
	if (FS->NodeId) {
      fprintf (Out, "First 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		   FS->NodeId[0], FS->NodeList[0], FS->NodeList[1], FS->NodeList[2],
		   FS->NodeId[1], FS->NodeList[3], FS->NodeList[4], FS->NodeList[5]);
	   if (FS->N_Node > 2) {
         fprintf (Out, "Last 2 points [id] X Y Z:\n\t[%d] %f %f %f\n\t[%d] %f %f %f\n", \
		      FS->NodeId[FS->N_Node-2], FS->NodeList[3*(FS->N_Node-2)], FS->NodeList[3*(FS->N_Node-2)+1], FS->NodeList[3*(FS->N_Node-2)+2],
		      FS->NodeId[FS->N_Node-1], FS->NodeList[3*(FS->N_Node-1)], FS->NodeList[3*(FS->N_Node-1)+1], FS->NodeList[3*(FS->N_Node-1)+2]);
      }
   } else {
      fprintf (Out, "NULL NodeId\n");
      fprintf (Out, "First 2 points X Y Z:\n\t %f %f %f\n\t %f %f %f\n", \
		   FS->NodeList[0], FS->NodeList[1], FS->NodeList[2],
		   FS->NodeList[3], FS->NodeList[4], FS->NodeList[5]);
	   if (FS->N_Node > 2) {
         fprintf (Out, "Last 2 points X Y Z:\n\t %f %f %f\n\t %f %f %f\n", \
		      FS->NodeList[3*(FS->N_Node-2)], FS->NodeList[3*(FS->N_Node-2)+1], FS->NodeList[3*(FS->N_Node-2)+2],
		      FS->NodeList[3*(FS->N_Node-1)], FS->NodeList[3*(FS->N_Node-1)+1], FS->NodeList[3*(FS->N_Node-1)+2]);
      }
   }
	fprintf (Out, "N_FaceSet %d\n", FS->N_FaceSet);
	if (!FS->isPatch) {
		if (FS->N_FaceSet > 2) {
        fprintf (Out, "First 2 polygons:\n\t%d %d %d\n\t%d %d %d\n", \
			FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2],
			FS->FaceSetList[3], FS->FaceSetList[4], FS->FaceSetList[5]);
        fprintf (Out, "Last 2 polygons:\n%d %d %d\n%d %d %d\n", \
	   		FS->FaceSetList[3 * (FS->N_FaceSet-2)], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 2],
		   	FS->FaceSetList[3 * (FS->N_FaceSet-1)], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 2]);
      }else {
         fprintf (Out, "First polygon:\n\t%d %d %d\n", \
			FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2]);
      }
	} else {
		if (FS->N_FaceSet > 2) {
         fprintf (Out, "First 2 polygons:\n\t[parent ID:%d] %d %d %d\n\t[parent ID:%d] %d %d %d\n", \
		   	FS->FaceSetIndexInParent[0], FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2],
		   	FS->FaceSetIndexInParent[1], FS->FaceSetList[3], FS->FaceSetList[4], FS->FaceSetList[5]);
		   fprintf (Out, "Last 2 polygons:\n\t[parent ID:%d]%d %d %d\n\t[parent ID:%d]%d %d %d\n", \
		   	FS->FaceSetIndexInParent[FS->N_FaceSet-2], FS->FaceSetList[3 * (FS->N_FaceSet-2)], \
		   	FS->FaceSetList[3 * (FS->N_FaceSet-2) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-2) + 2], \
		   	FS->FaceSetIndexInParent[FS->N_FaceSet-1], FS->FaceSetList[3 * (FS->N_FaceSet-1)], \
		   	FS->FaceSetList[3 * (FS->N_FaceSet-1) + 1], FS->FaceSetList[3 * (FS->N_FaceSet-1) + 2]);
      } else {
         fprintf (Out, "First polygon:\n\t[parent ID:%d] %d %d %d\n", \
		   	FS->FaceSetIndexInParent[0], FS->FaceSetList[0], FS->FaceSetList[1], FS->FaceSetList[2]);
      }
	}
	SUMA_RETURNe;

}

/*!
   \brief Change the face vector to a SUMA FaceSetList vector
   Polygons are automatically triangulated
   
   \param face (long *) vector of ace indices. Faces are separated
                       by -1 entries
   \param N (int *) to contain the number of faces is FaceSetList
   \SUMA_RETURN FaceSetList (int *) 3Nx1 vector of triangles making up mesh.
*/

int * SUMA_BYU_PolyFaceToTriFace(int *face, int *N)
{
   static char FuncName[]={"SUMA_BYU_PolyFaceToTriFace"};
   int i, k, N_alloc, iface, iface0, iFS3;
   int *FaceSetList=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Can't guess ahead of time, make sure you check down the line */
   N_alloc = *N*3;
   FaceSetList = (int *)SUMA_malloc(N_alloc*sizeof(int));   
   if (!FaceSetList) {
      fprintf (SUMA_STDERR,"Error %s: Failed to reallocate.\n", FuncName);
      SUMA_RETURN(NULL);
   } 
   iFS3 =0; /* index of triangulated facet */
   iface = 0;
   iface0 = 0;
   while (iface < *N) {
      iface0 = iface ; /* 1s node in polygon */
      if (iface0 < 0) {
         fprintf(SUMA_STDERR, "Error %s: Unexpected end flag", FuncName);
         SUMA_free(FaceSetList); 
         SUMA_RETURN(NULL);
      }
      if (LocalHead) fprintf(SUMA_STDERR,
            "%s: iface0 = %d, face[%d] = %d: ", 
            FuncName, iface0, iface0, (int)face[iface0]) ;
      do {
         if (iFS3+3 > N_alloc) {
            N_alloc = 2 * N_alloc;
            FaceSetList = (int *)realloc((void *)FaceSetList, N_alloc * sizeof(int));
            if (!FaceSetList) {
               fprintf (SUMA_STDERR,"Error %s: Failed to reallocate.\n", FuncName);
               SUMA_RETURN(NULL);
            } 
         }
         FaceSetList[iFS3] = SUMA_ABS(face[iface0]); /* first node in polygon is first node of triangles forming polygon */
         if (FaceSetList[iFS3] < 0) {
            fprintf (SUMA_STDERR,"Negative index loaded (loc 0)\n");
         }
         if (LocalHead) fprintf(SUMA_STDERR,
            "t(%d, ", (int)face[iface0]);
         if (iface == iface0) ++iface;
         if (LocalHead) fprintf(SUMA_STDERR,
            "%d, ", (int)face[iface]);
         ++iFS3;
         FaceSetList[iFS3] = SUMA_ABS(face[iface]); /* node 2 */
         if (FaceSetList[iFS3] < 0) {
            fprintf (SUMA_STDERR,"Negative index loaded (loc 1)\n");
         }
         if (LocalHead) fprintf(SUMA_STDERR,
            "%d) ", (int)face[iface+1]);
         ++iFS3; 
         FaceSetList[iFS3] = SUMA_ABS(face[iface+1]); /* node 3 */
         if (FaceSetList[iFS3] < 0) {
            fprintf (SUMA_STDERR,"Negative index loaded (loc 2)\n");
         }
         ++iFS3; ++iface; 
      } while (face[iface] >= 0);
      if (LocalHead) fprintf(SUMA_STDERR," iFS3/N_alloc = %d/%d\n", iFS3, N_alloc);
      /* ++iface; skip -1 */
      ++iface; /* goto next */
   }
   
   *N = iFS3 / 3;

   /* reallocate */

      if (LocalHead) {
         int tmpmin=-100, n3, itmp;
         n3 = 3 * *N;
         fprintf (SUMA_STDERR,"%s: N_FaceSet %d\n", FuncName, *N);
         SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
         fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
         if (tmpmin < 0) {
            fprintf (SUMA_STDERR,"Error %s: Bad ass pre-alloc negative number\n", FuncName);
            for (itmp=0; itmp<n3; ++itmp) {
               fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
               if (FaceSetList[itmp] < 0) {
                  fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
               }
            } 
         }
      }

   FaceSetList = (int *)SUMA_realloc((void *)FaceSetList, iFS3 * sizeof(int));
      if (LocalHead) {
         int tmpmin=-100, n3, itmp;
         n3 = 3 * *N;
         fprintf (SUMA_STDERR,"%s: N_FaceSet %d\n", FuncName, *N);
         SUMA_MIN_VEC (FaceSetList, n3, tmpmin);
         fprintf (SUMA_STDERR,"Minimum index is %d\n", tmpmin);
         if (tmpmin < 0) {
            fprintf (SUMA_STDERR,"Error %s: Bad post realloc ass negative number\n", FuncName);
            for (itmp=0; itmp<n3; ++itmp) {
               fprintf (SUMA_STDERR, "%d: %d\n", itmp, FaceSetList[itmp]);
               if (FaceSetList[itmp] < 0) {
                  fprintf (SUMA_STDERR,"%s: Min of %d, at %d\n", FuncName, FaceSetList[itmp], itmp);
               }
            } 
         }
      }
   
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Returning (iFS3 = %d, N = %d...)\n", FuncName, iFS3, *N);
   
   SUMA_RETURN(FaceSetList); 
}

/*! \brief Load a BYU surface model

*/
SUMA_Boolean SUMA_BYU_Read(char *f_name, SUMA_SurfaceObject *SO, 
                           int debug, byte hide_negcols) 
{
   static char FuncName[]={"SUMA_BYU_Read"};
   int ok, nread, i, k, lessen=0, nodemin, nodemax, 
      *face=NULL, n = -1, nalloc=-1;
   double dum;
   char *fl=NULL, *fli;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* suck the file */
   fl = SUMA_file_suck( f_name , &nread );
   fli = fl;
   if (!fl) {
      SUMA_SL_Err("Failed to read file.");
      SUMA_RETURN(NOPE);
   }
     
   ok = 0;
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
   if (!ok) { SUMA_S_Err("Failed to read PART_NUM"); SUMA_free(fli); SUMA_RETURN(NOPE); }
   SUMA_LHv("PART_NUM %f\n", dum);
   if ((int)dum != 1) {
      SUMA_S_Warnv("Not ready to deal with PART_NUM > 1. Have %d\n", (int)dum);
   }
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);SO->N_Node = (int)dum;
   if (!ok) { SUMA_S_Err("Failed to read VERTEX_NUM"); SUMA_free(fli); SUMA_RETURN(NOPE); }
   SUMA_LHv("VERTEX_NUM %f\n", dum);
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);SO->N_FaceSet=(int)dum;
   if (!ok) { SUMA_S_Err("Failed to read POLY_NUM"); SUMA_free(fli); SUMA_RETURN(NOPE); }
   SUMA_LHv("POLY_NUM %f\n", dum);
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
   if (!ok) { SUMA_S_Err("Failed to read EDGE_NUM"); SUMA_free(fli); SUMA_RETURN(NOPE); }
   SUMA_LHv("EDGE_NUM %f\n", dum);
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
   if (!ok) { SUMA_S_Err("Failed to read POLY1"); SUMA_free(fli); SUMA_RETURN(NOPE); }
   SUMA_LHv("POLY1 %f\n", dum);
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
   if (!ok) { SUMA_S_Err("Failed to read POLY2"); SUMA_free(fli); SUMA_RETURN(NOPE); }
   SUMA_LHv("POLY2 %f\n", dum);
   
   /* Now allocate for node list */
   SO->NodeDim = 3;
   if (!(SO->NodeList = (float *)SUMA_malloc(SO->N_Node*sizeof(float)*SO->NodeDim))) {
      SUMA_S_Err("Failed to allocate for NodeList");
      SUMA_free(fli); SUMA_RETURN(NOPE);
   }
   for (i=0; i<SO->N_Node*SO->NodeDim; ++i) {
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
      if (!ok) { 
         SUMA_S_Err("Failed to read coordinate"); 
         SUMA_free(fli); SUMA_free(SO->NodeList); SO->NodeList = NULL;
         SUMA_RETURN(NOPE); 
      }
      SO->NodeList[i] = (float)dum;
      SUMA_LHv("nodec %f\n", dum);

   }
   
   #if 1
   /* Now read all facesets (not triangles necessarily ) */
   nodemin = 1000; nodemax = -1;
   SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
   nalloc = 0;
   n = 0;
   while(ok) {
      if (n+1 > nalloc) {
         face = (int *)SUMA_realloc(face, (nalloc+10000)*sizeof(int)); nalloc+=10000;      
      }
      face[n] = (int) dum;
      nodemin = SUMA_MIN_PAIR(SUMA_ABS(face[n]), nodemin);
      nodemax = SUMA_MAX_PAIR(SUMA_ABS(face[n]), nodemax);
      ++n;
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
   }
   
   /* Now change the polyfaced things to triangles */
   SO->FaceSetDim = 3;
   SO->FaceSetList =  SUMA_BYU_PolyFaceToTriFace(face, &n);
   SO->N_FaceSet = n;
   if (LocalHead) { SUMA_WRITE_ARRAY_1D(SO->FaceSetList, n*3, 3, "triangulated"); }
   SUMA_free(face); face = NULL;
   #else
   /* Now allocate for facesetlist list */
   SO->FaceSetDim = 3;
   if (!(SO->FaceSetList = (int *)SUMA_malloc(SO->N_FaceSet*sizeof(int)*SO->FaceSetDim))) {
      SUMA_S_Err("Failed to allocate for FaceSetList");
      SUMA_free(SO->NodeList); SO->NodeList = NULL;
      SUMA_free(fli); SUMA_RETURN(NOPE);
   }
   lessen = 0;
   nodemin = 1000; nodemax = -1;
   for (i=0; i<SO->N_FaceSet; ++i) {
      for (k=0; k<3;++k) {
         SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
         if (!ok) { 
            SUMA_S_Err("Failed to read polynode"); 
            SUMA_free(fli); SUMA_free(SO->NodeList); SO->NodeList = NULL;
            SUMA_free(SO->FaceSetList); SO->FaceSetList=NULL;
            SUMA_RETURN(NOPE); 
         }
         SO->FaceSetList[3*i+k] = (int)dum;
         nodemin = SUMA_MIN_PAIR(SO->FaceSetList[3*i+k], nodemin);
         nodemax = SUMA_MAX_PAIR(SO->FaceSetList[3*i+k], nodemax);
         SUMA_LHv("facei %f\n", dum);
      }
      SUMA_ADVANCE_PAST_NUM(fl, dum, ok);
      if (!ok || dum > 0) { 
         SUMA_S_Err("Failed to read closing negative node"); 
         SUMA_free(fli); SUMA_free(SO->NodeList); SO->NodeList = NULL;
         SUMA_free(SO->FaceSetList); SO->FaceSetList=NULL;
         SUMA_RETURN(NOPE); 
      }
   }
   #endif
   if (LocalHead && nodemin > 0) { SUMA_S_Notev("Smallest node index in list is %d\n", nodemin); }
   if (LocalHead && nodemax >= SO->N_Node) { SUMA_S_Notev("Largest node index in list is %d\n", nodemax); }
   if (nodemin > 0 && nodemax == SO->N_Node) {
      SUMA_S_Note("FaceSetList is 1 based, changing to 0\n");
      lessen = 1;
   } 
   if (nodemax > SO->N_Node || nodemin < 0) {
      SUMA_S_Errv("Node indices not strictly between [0, %d]\nhave [%d, %d]\n",
                  SO->N_Node -1, nodemin, nodemax); 
      SUMA_free(fli); SUMA_free(SO->NodeList); SO->NodeList = NULL;
      SUMA_free(SO->FaceSetList); SO->FaceSetList=NULL;
      SUMA_RETURN(NOPE); 
   }
   if (lessen) {
      for (i=0; i<SO->FaceSetDim*SO->N_FaceSet; ++i) SO->FaceSetList[i] -= 1;
   }
   SO->FileType = SUMA_BYU;
   SO->Name = SUMA_StripPath(f_name);
   SO->FileFormat = SUMA_ASCII; 
   if (LocalHead) SUMA_Print_Surface_Object(SO, NULL);
   
   SUMA_LH("Done.");
   
   SUMA_free(fli); fli=NULL;
   
   SUMA_RETURN(YUP);
}

/*!
   \brief Load a GIFTI surface from a .gii file 
*/
SUMA_Boolean SUMA_GIFTI_Read(char *f_name, SUMA_SurfaceObject *SO,
                             int debug)
{
   static char FuncName[]={"SUMA_GIFTI_Read"};
   NI_group *aSO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SO->aSO) {
      SUMA_S_Err("SO has aSO already!");
      SUMA_RETURN(NOPE);
   }
   
      
   if (!(aSO = afni_open_gifti_surf(f_name,1))) {
      SUMA_RETURN(NOPE);
   }
   
   SO->FileType = SUMA_GIFTI;
   SO->Name = SUMA_StripPath(f_name);
   SO->FileFormat = SUMA_XML_SURF;  

   if (!SUMA_MergeAfniSO_In_SumaSO(&aSO, SO)) {
      SUMA_S_Err("Failed to merge SOs");
      aSO = SUMA_FreeAfniSurfaceObject(aSO);
      SUMA_RETURN(NOPE);
   }

   
   if (LocalHead) SUMA_Print_Surface_Object (SO, NULL);
                 
   SUMA_RETURN(YUP);
}
/*!
   \brief Load a brain voyager surface model from a .srf file.
   
   Thanks to Nikolaus Kriegeskorte for code snippets and help.
   
   The following fields are set in SO:
   SO->NodeDim
   SO->FaceSetDim
   SO->NodeList
   SO->FaceSetList
   SO->N_Node;
   SO->N_FaceSet;
   SO->Name;
   SO->FileType;
   SO->FileFormat
   
   see readsrf.m for more info

*/
SUMA_Boolean SUMA_BrainVoyager_Read(char *f_name, SUMA_SurfaceObject *SO, int debug, byte hide_negcols) 
{
   static char FuncName[]={"SUMA_BrainVoyager_Read"};
	float FileVersion, cx, cy, cz, *fbuf = NULL;
	int i, ii, chnk, ex, surf_type, n_neighbors, bs, cnt_inmesh=0, *ibuf=NULL;
	char buffer[256];
   float fbuffer[256];
   FILE *fl=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* check for existence */
	if (!SUMA_filexists(f_name)) {
		fprintf(SUMA_STDERR,"Error %s: File %s does not exist or cannot be read.\n", FuncName, f_name);
		SUMA_RETURN (NOPE);
	}else {
      if ( debug > 1) {
		   fprintf(SUMA_STDERR,"%s: File %s exists and will be read.\n", FuncName, f_name);
	   }
   }
	
   fl = fopen(f_name, "r");
   if (!fl) {
      SUMA_SL_Err("Failed to open file for reading.\n");
      SUMA_RETURN(NOPE);
   }
   
   SO->N_Node=0;
	SO->N_FaceSet=0;
   
   /* read version, and number of nodes and facesets, assume no swapping for the moment */
   bs = 0;
   chnk = sizeof(float);
   ex = fread (&FileVersion, chnk, 1, fl);
   chnk = sizeof(int);
   ex = fread (&surf_type, chnk, 1, fl); /* must be 0, per website www.brainvoyager.com... */
   ex = fread (&(SO->N_Node), chnk, 1, fl);
   ex = fread (&(SO->N_FaceSet), chnk, 1, fl);
   if (FileVersion < 0 || FileVersion > 500000 || SO->N_Node < 0 || SO->N_FaceSet < 0) { /* trouble perhaps ? */
      SUMA_LH("Byte swapping needed...");
      bs = 1;
   }
   if (bs) {
      SUMA_SWAP_THIS(&FileVersion, sizeof(float));
      SUMA_SWAP_THIS(&surf_type, sizeof(int));
      SUMA_SWAP_THIS(&(SO->N_Node), sizeof(int));
      SUMA_SWAP_THIS(&(SO->N_FaceSet), sizeof(int));
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s:\nSurfType is %d\nN_Node = %d, N_FaceSet = %d\n", 
                           FuncName, surf_type, SO->N_Node, SO->N_FaceSet);
   }
   
   if (FileVersion < 0 || FileVersion > 500000) { /* trouble perhaps ? */
      SUMA_SL_Err("Version number < 0 || > 500000 \nSeems like bad news to me, quitting...");
      fclose(fl);
      SUMA_RETURN(NOPE);
   }
   
   
   if (SO->N_Node < 0 || SO->N_FaceSet < 0) {
      SUMA_SL_Err("Negative values for N_Node and N_FaceSet.");
      fclose(fl);
      SUMA_RETURN(NOPE);
   } 
   
   SUMA_READ_FLOAT(&cx, bs, fl, ex);
   SUMA_READ_FLOAT(&cy, bs, fl, ex);
   SUMA_READ_FLOAT(&cz, bs, fl, ex);
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s:\ncenter = [%f, %f, %f]\n(Niko adds 30 to cy ...)\n", FuncName, cx, cy, cz);
   }
   SO->NodeDim = 3;
   SO->FaceSetDim = 3;
   fbuf = (float *)SUMA_malloc(SO->N_Node*sizeof(float));
   SO->NodeList = (float *)SUMA_malloc(SO->NodeDim*SO->N_Node*sizeof(float));
   SO->FaceSetList = (int *)SUMA_malloc(SO->FaceSetDim*SO->N_FaceSet*sizeof(int));
   ibuf = (int*)SUMA_malloc(SO->N_Node*sizeof(int));
   if (!ibuf || !fbuf || !SO->NodeList || !SO->FaceSetList) {
      SUMA_SL_Crit("Failed to allocate.");
      SUMA_RETURN(NOPE);
   }
   
   /* read the coords */
   SUMA_LH("Reading coords...");
   ex = fread(fbuf, sizeof(float), SO->N_Node, fl);
   if (ex != SO->N_Node) { SUMA_SL_Warn("Failed to read all node X info"); }
   if (bs) SUMA_SWAP_VEC(fbuf,SO->N_Node,sizeof(float));
   for (i=0; i<SO->N_Node; ++i) SO->NodeList[3*i] = fbuf[i];
   ex = fread(fbuf, sizeof(float), SO->N_Node, fl);
   if (ex != SO->N_Node) { SUMA_SL_Warn("Failed to read all node Y info"); }
   if (bs) SUMA_SWAP_VEC(fbuf,SO->N_Node,sizeof(float));
   for (i=0; i<SO->N_Node; ++i) SO->NodeList[3*i+1] = fbuf[i];
   ex = fread(fbuf, sizeof(float), SO->N_Node, fl);
   if (ex != SO->N_Node) { SUMA_SL_Warn("Failed to read all node Z info"); }
   if (bs) SUMA_SWAP_VEC(fbuf,SO->N_Node,sizeof(float));
   for (i=0; i<SO->N_Node; ++i) SO->NodeList[3*i+2] = fbuf[i];
   /* no need for buffer anymore ... */
   SUMA_free(fbuf); fbuf = NULL;
   if (LocalHead) { 
      char *sdbg = SUMA_ShowMeSome((void *)SO->NodeList, SUMA_float, SUMA_MIN_PAIR(20, SO->N_Node), 20, NULL);
      fprintf(SUMA_STDERR,"%s NodeList:\n%s\n", FuncName, sdbg);
      SUMA_free(sdbg);sdbg = NULL;
   }
   /* skip the node normals, which would be read much like the x y z coords*/
   fseek(fl, SO->N_Node*3*sizeof(float), SEEK_CUR);
   
   /* skip the curvature color info */
   ex = fread(fbuffer, sizeof(float), 8, fl); /* colors of convex and concave stuff */
   if (bs) SUMA_SWAP_VEC(fbuffer,8, sizeof(float));
   if (LocalHead) { 
      char *sdbg = SUMA_ShowMeSome((void *)fbuffer, SUMA_float, 8,8, NULL);
      fprintf(SUMA_STDERR,"%s colorstuff:\n%s\n", FuncName, sdbg);
      SUMA_free(sdbg);sdbg = NULL;
   }
   if (0) { /* don't skip mesh color, it is use to hide triangles ! */
      fseek(fl, SO->N_Node*sizeof(int), SEEK_CUR); /* jump over mesh color */
      cnt_inmesh = SO->N_Node;
   } else { 
      if (!ibuf) {
         SUMA_SL_Crit("Failed to allocate.");
         SUMA_RETURN(NOPE);
      }
      /* read into buffer */
      ex = fread(ibuf, sizeof(int), SO->N_Node, fl);
      if (ex != SO->N_Node) { SUMA_SL_Warn("Failed to read all node color info"); }
      if (bs) SUMA_SWAP_VEC(ibuf,SO->N_Node,sizeof(int));
      /* Hide negative node indices (per info from Hester Breman*/
      cnt_inmesh = 0;
      for (i=0; i<SO->N_Node; ++i) { 
         if (ibuf[i] >= 0) {
            ibuf[cnt_inmesh] = i; 
            ++cnt_inmesh;
         }
      }
   }  
   
   
   /* skip nearest neighbor info */
   for (i=0; i<SO->N_Node; ++i) {
      ex = fread(&n_neighbors, sizeof(int), 1, fl);
      if (bs) SUMA_SWAP_THIS(&n_neighbors, sizeof(int));
      fseek(fl, n_neighbors*sizeof(int), SEEK_CUR);
   }
   
   /* read dems triangles */
   SUMA_LH("Reading FaceSets...");
   ex = fread(SO->FaceSetList, sizeof(int), SO->N_FaceSet * SO->FaceSetDim , fl);
   if (ex != SO->N_FaceSet * SO->FaceSetDim) { 
      fprintf(SUMA_STDERR,"Error %s: Failed to read all faceset info.\nRead %d values, expected %d\n", FuncName, ex, SO->N_FaceSet * SO->FaceSetDim );
      SUMA_RETURN(NOPE);
   }
   
   if (bs) SUMA_SWAP_VEC(SO->FaceSetList,(SO->N_FaceSet * SO->FaceSetDim),sizeof(int));
   if (LocalHead) { 
      char *sdbg = SUMA_ShowMeSome((void *)SO->FaceSetList, SUMA_int, SUMA_MIN_PAIR(20, SO->N_FaceSet * SO->FaceSetDim), 20, NULL);
      fprintf(SUMA_STDERR,"%s FaceSetList:\n%s\n", FuncName, sdbg);
      SUMA_free(sdbg);sdbg = NULL;
   }
   fclose(fl); fl = NULL;
   
   /* decide on whether some nodes need to be hidden, flat maps in BV contain the entire mesh! */
   if (hide_negcols && cnt_inmesh < SO->N_Node) {
      SUMA_PATCH *patch=NULL;
      SUMA_MEMBER_FACE_SETS *Memb = NULL;
      fprintf(SUMA_STDERR,"%s: %d nodes have negative colors \nand have been removed from mesh %s\n",
         FuncName, SO->N_Node - cnt_inmesh, f_name);
      Memb =  SUMA_MemberFaceSets (SO->N_Node, SO->FaceSetList, SO->N_FaceSet, SO->NodeDim, NULL);
      if (!Memb->NodeMemberOfFaceSet) {
            SUMA_SL_Crit("Failed to create Memb FaceSets!");
            SUMA_RETURN(NOPE);
      }
      SUMA_LH("Patchin");
      if (!(patch = SUMA_getPatch (  ibuf, cnt_inmesh, 
                           SO->FaceSetList, SO->N_FaceSet, 
                           Memb, SO->NodeDim))) {

         SUMA_SL_Err("Failed to create patch, proceeding but mesh might be a mess.");
      } else {
         SUMA_LH("Switchin");
         if (SO->FaceSetList) SUMA_free(SO->FaceSetList); SO->FaceSetList = NULL;
         SO->FaceSetList = patch->FaceSetList; patch->FaceSetList = NULL;
         SO->N_FaceSet = patch->N_FaceSet; 
         /* done with Memb */
         SUMA_Free_MemberFaceSets(Memb); Memb = NULL;   
         /* done with patch */
         SUMA_freePatch(patch); patch = NULL;
      }
      /* Does this merit a warning?*/
      if (strstr(f_name, "FLAT") && debug) {
         SUMA_S_Note(
            "\n"
            "****************************************************************\n"
            "Viewing BrainVoyager's Flat Maps:\n"
            "---------------------------------\n"
            "BV, it seems, shows both flattened cortical surfaces\n"
            "using the same mesh. Each side of the flat surface \n"
            "represents another hemisphere.\n"
            "SUMA, which by default displays both backward and forward facing\n"
            "triangles, will display both sides simultaneously. To look at\n"
            "each side separately, set 'Backface Culling' to \n"
            "'cull the FrontFace'. Backface Culling modes are toggled\n"
            "with the 'B' button. Read SUMA's GUI help 'ctrl+h' for reminder.\n" 
            "****************************************************************\n"
            "\n"
            );
      }
   } else {
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: All nodes preserved in mesh.\n", FuncName);
      }
   }

   if (ibuf) SUMA_free(ibuf); ibuf = NULL;

   
   SO->FileType = SUMA_BRAIN_VOYAGER;
   SO->Name = SUMA_StripPath(f_name);
   SO->FileFormat = SUMA_BINARY;    /* files are likely all BINARY_LE, must take care of that at some point*/
   
   SUMA_LH("Done.");
   
   SUMA_RETURN(YUP);
}

#ifdef SUMA_FreeSurfer_STAND_ALONE


void usage_SUMA_FreeSurfer_Main ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_FreeSurfer f_name \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSUMA_FreeSurfer_STAND_ALONE -Wall -o $1 $1.c -I./ -I//usr/X11R6/include SUMA_lib.a\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \tFri Feb 8 16:29:06 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   char FS_name[200];
	SUMA_FreeSurfer_struct *FS;
	
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_FreeSurfer-Main-");
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
	/* Allocate for FS */
	FS = (SUMA_FreeSurfer_struct *)
             SUMA_calloc(1,sizeof(SUMA_FreeSurfer_struct));	
	if (FS == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for FS\n", FuncName);
		exit(1);
	}
   
   if (argc < 2)
       {
          usage_SUMA_FreeSurfer_Main ();
          exit (1);
       }
   
	sprintf(FS_name, "%s", argv[1]);
   
   if (!SUMA_isExtension(FS_name, ".asc")) {
	   if (!SUMA_FreeSurfer_ReadBin_eng (FS_name, FS, 0)) {
		   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_FreeSurferBin_Read\n", FuncName);
		   exit(1);
	   }
   }else {
      if (!SUMA_FreeSurfer_Read_eng (FS_name, FS, 0)) {
		   fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_FreeSurfer_Read\n", FuncName);
		   exit(1);
	   }
   }
	
	
	SUMA_Show_FreeSurfer (FS, NULL);
	fprintf(stdout, "freeing ..\n");
	if (!SUMA_Free_FreeSurfer (FS)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Free_FreeSurfer.\n", FuncName);
		exit(1);
	}
	
	if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

	return (0);
}/* Main */
#endif

/*** Code to read ply format data 
Ply functions are based on code by Greg Turk. 
---------------------------------------------------------------

Copyright (c) 1994 The Board of Trustees of The Leland Stanford
Junior University.  All rights reserved.   
  
Permission to use, copy, modify and distribute this software and its   
documentation for any purpose is hereby granted without fee, provided   
that the above copyright notice and this permission notice appear in   
all copies of this software and that you do not sell the software.   
  
THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,   
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY   
WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.   


*/

/* user's vertex and face definitions for a polygonal object */

typedef struct Vertex {
  float x,y,z;             /* the usual 3-space position of a vertex */
} Vertex;

typedef struct Face {
  unsigned char intensity; /* this user attaches intensity to faces */
  unsigned char nverts;    /* number of vertex indices in list */
  int *verts;              /* vertex index list */
} Face;

/* information needed to describe the user's data to the PLY routines */

PlyProperty vert_props[] = { /* list of property information for a vertex */
  {"x", PLY_FLOAT, PLY_FLOAT, offsetof(Vertex,x), 0, 0, 0, 0},
  {"y", PLY_FLOAT, PLY_FLOAT, offsetof(Vertex,y), 0, 0, 0, 0},
  {"z", PLY_FLOAT, PLY_FLOAT, offsetof(Vertex,z), 0, 0, 0, 0},
};

PlyProperty face_props[] = { /* list of property information for a vertex */
  {"intensity", PLY_UCHAR, PLY_UCHAR, offsetof(Face,intensity), 0, 0, 0, 0},
  {"vertex_indices", PLY_INT, PLY_INT, offsetof(Face,verts),
   1, PLY_UCHAR, PLY_UCHAR, offsetof(Face,nverts)},
};


/*!
   \brief Reads a Ply formatted file into a SUMA_SurfaceObject structure
   ans = SUMA_Ply_Read (f_name, SO);
   
   \param f_name (char *) name (and path) of .ply file to read. Extension .ply is optional
   \param SO (SUMA_SurfaceObject *) pointer to a structure to return surface in f_name in
   \return ans (SUMA_Boolean) YUP/NOPE
   
   The following fields are set in SO:
   SO->NodeDim
   SO->FaceSetDim
   SO->NodeList
   SO->FaceSetList
   SO->N_Node;
   SO->N_FaceSet;
   SO->Name;
   SO->FileType;
   SO->FileFormat
   
   \sa SUMA_Ply_Write()
   
   This function is a wrap around code by Greg Turk. 
   
*/
SUMA_Boolean SUMA_Ply_Read (char * f_name, SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_Ply_Read"};
   int i,j,k, j3, ji;
   PlyFile *ply = NULL;
   int nelems;
   char **elist = NULL;
   int file_type;
   float version;
   int nprops;
   int num_elems;
   PlyProperty **plist = NULL;
   Vertex **vlist = NULL;
   Face **flist = NULL;
   char *elem_name;
   int num_comments;
   char **comments = NULL;
   int num_obj_info;
   char **obj_info = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* open a PLY file for reading */
   ply = ply_open_for_reading(f_name, &nelems, &elist, &file_type, &version);
   if (!ply) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed to find/read %s.\n", 
               FuncName, f_name);
      SUMA_RETURN (NOPE);
   }
   
   /* print what we found out about the file */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: version %f\n", FuncName, version);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: type %d\n", FuncName, file_type);

   /* go through each kind of element that we learned is in the file */
   /* and read them */

   for (i = 0; i < nelems; i++) {

    /* get the description of the first element */
    elem_name = elist[i];
    plist = ply_get_element_description (ply, elem_name, &num_elems, &nprops);

    /* print the name of the element, for debugging */
    if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: element %s %d\n", FuncName, elem_name, num_elems);

    /* if we're on vertex elements, read them in */
    if (equal_strings ("vertex", elem_name)) {

      /* create a vertex list to hold all the vertices */
      #ifdef USE_PLY_VERTEX
      vlist = (Vertex **) SUMA_malloc (sizeof (Vertex *) * num_elems);
      #endif
      
      SO->NodeList = (float *) SUMA_calloc (3*num_elems, sizeof(float));
      if (!SO->NodeList) {
         fprintf (SUMA_STDERR, 
                  "Error %s: Failed to allocate for SO->NodeList.\n", 
                  FuncName);
         SUMA_RETURN(NOPE);
      }
      
      /* set up for getting vertex elements */

      ply_get_property (ply, elem_name, &vert_props[0]);
      ply_get_property (ply, elem_name, &vert_props[1]);
      ply_get_property (ply, elem_name, &vert_props[2]);
      
      SO->NodeDim = 3;
      SO->N_Node = num_elems;
      /* grab all the vertex elements */
      for (j = 0; j < num_elems; j++) {

        /* grab and element from the file */
        #ifdef USE_PLY_VERTEX
        //vlist[j] = (Vertex *) SUMA_malloc (sizeof (Vertex));
        //ply_get_element (ply, (void *) vlist[j]);
        /* print out vertex x,y,z for debugging */
        if (LocalHead) fprintf (SUMA_STDERR, "%s vertex: %g %g %g\n", FuncName, vlist[j]->x, vlist[j]->y, vlist[j]->z);
        /* copy to NodeList */
        j3 = SO->NodeDim*j;
        SO->NodeList[j3] = vlist[j]->x;
        SO->NodeList[j3+1] = vlist[j]->y;
        SO->NodeList[j3+2] = vlist[j]->z;
        
        #else
        j3 = SO->NodeDim*j;
        ply_get_element (ply, (void *) &(SO->NodeList[j3]));
        /* print out vertex x,y,z for debugging */
        if (LocalHead) fprintf (SUMA_STDERR, "%s vertex: %g %g %g\n", FuncName, 
         SO->NodeList[j3], SO->NodeList[j3+1], SO->NodeList[j3+2]);
        #endif
         
      }
    }

    /* if we're on face elements, read them in */
    if (equal_strings ("face", elem_name)) {

      /* create a list to hold all the face elements */
      flist = (Face **) SUMA_malloc (sizeof (Face *) * num_elems);

      /* set up for getting face elements */

      ply_get_property (ply, elem_name, &face_props[0]);
      ply_get_property (ply, elem_name, &face_props[1]);

      /* grab all the face elements */
      for (j = 0; j < num_elems; j++) {

        /* grab and element from the file */
        flist[j] = (Face *) SUMA_malloc (sizeof (Face));
        ply_get_element (ply, (void *) flist[j]);

        /* print out face info, for debugging */
        if (LocalHead) {
         fprintf (SUMA_STDERR,"%s face: %d, list = ", FuncName, flist[j]->intensity);
         for (k = 0; k < flist[j]->nverts; k++)
            fprintf (SUMA_STDERR,"%d ", flist[j]->verts[k]);
         fprintf (SUMA_STDERR,"\n");
        }
        
      }
      /* copy face elements to SO structure */
      SO->FaceSetDim = flist[0]->nverts;
      SO->N_FaceSet = num_elems;
      SO->FaceSetList = (int *) SUMA_calloc (SO->FaceSetDim * num_elems, sizeof(int));
      if (!SO->FaceSetList) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SO->NodeList.\n", FuncName);
         if (SO->NodeList) SUMA_free(SO->NodeList); 
         SUMA_RETURN(NOPE);
      }
      
      for (j = 0; j < num_elems; j++) {
         if (flist[j]->nverts != SO->FaceSetDim) {
            fprintf (SUMA_STDERR, "Error %s: All FaceSets must have the same dimension for SUMA.\n", FuncName);
            if (SO->NodeList) SUMA_free(SO->NodeList); 
            if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
            SO->NodeList = NULL;
            SO->FaceSetList = NULL;
            SUMA_RETURN(NOPE);
         }
         ji = SO->FaceSetDim * j;
         for (k = 0; k < flist[j]->nverts; k++)
            SO->FaceSetList[ji+k] = flist[j]->verts[k];
      }
    }

   /* fill up a few more fields */
   SO->FileType = SUMA_PLY;
   if (file_type == PLY_ASCII) SO->FileFormat = SUMA_ASCII;
      else if (file_type == PLY_BINARY_BE) SO->FileFormat = SUMA_BINARY_BE;
         else if (file_type == PLY_BINARY_LE) SO->FileFormat = SUMA_BINARY_LE;
            else {
               fprintf (SUMA_STDERR, "Error %s: PLY_TYPE %d not recognized.\n", FuncName, file_type);
            }
            
   SO->Name = SUMA_StripPath(f_name);
   
   /* print out the properties we got, for debugging */
      if (LocalHead) {
         for (j = 0; j < nprops; j++)
            fprintf (SUMA_STDERR, "%s property %s\n", FuncName, plist[j]->name);
      }
   }

   /* grab and print out the comments in the file */
   comments = ply_get_comments (ply, &num_comments);
   if (LocalHead) {   
      for (i = 0; i < num_comments; i++)
         fprintf (SUMA_STDERR, "%s comment = '%s'\n", FuncName, comments[i]);
   }
   
   /* grab and print out the object information */
   obj_info = ply_get_obj_info (ply, &num_obj_info);
   if (LocalHead) {   
      for (i = 0; i < num_obj_info; i++)
         fprintf (SUMA_STDERR, "%s obj_info = '%s'\n", FuncName, obj_info[i]);
   }
   
   /* free the allocations necessary for vertex and facesetlists */
   for (j = 0; j < SO->N_FaceSet; j++) {
      SUMA_free(flist[j]);
   }
   SUMA_free(flist); flist = NULL;
   
   #ifdef USE_PLY_VERTEX
   for (j = 0; j < SO->N_Node; j++) {
      SUMA_free(vlist[j]);
   }
   SUMA_free(vlist); vlist = NULL;
   #endif
   /* close the PLY file, ply structure is freed within*/
   ply_close (ply);
   
   /* free plist */
   for (j = 0; j < nprops; j++) if (plist[j]) SUMA_free (plist[j]);
   if (plist) SUMA_free(plist);
   
   /* free comments */
   for (i = 0; i < num_comments; i++) if (comments[i]) SUMA_free (comments[i]);
   if (comments) SUMA_free (comments);
   
   /* free elist */
   for (i = 0; i < nelems; i++) if (elist[i]) SUMA_free (elist[i]);
   if (elist) SUMA_free (elist);
   
   /* free obj_info */
   for (i = 0; i < num_obj_info; i++) if (obj_info[i]) SUMA_free (obj_info[i]);
   if (obj_info) SUMA_free (obj_info);
    
   SUMA_RETURN(YUP);
}

/*!
   \brief Writes an SO into a .ply file
   ans = SUMA_Ply_Write (f_name, SO);
   \param f_name (char *) name of .ply file. if .ply is not attached it will be added.
   \param SO (SUMA_SurfaceObject *) Surface object to write out. 
      if SO->FileFormat = SUMA_BINARY_BE or SUMA_BINARY_LE the surface is written in binary ply format.
      SUMA_BINARY is set to SUMA_BINARY_BE
   \return ans (SUMA_Boolean) success flag.
   
   In its current incarnation, the function does not overwrite a pre-existing file.
      
*/ 
SUMA_Boolean SUMA_Ply_Write (char * f_name_in, SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_Ply_Write"};
   int i,j;
   PlyFile *ply = NULL;
   int nelems;
   int file_type;
   float version;
   int nverts ;
   int nfaces ;
   char *f_name, *f_name2, *elem_names[] = { "vertex", "face" };/* list of the kinds of elements in the user's object */
   int n_elem_names = 2;
   Vertex **verts = NULL;
   Face *faces = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!f_name_in) {
      fprintf (SUMA_STDERR, "Error %s: NULL filename\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   f_name = SUMA_Extension(f_name_in,".ply" , YUP); 
   f_name2  = SUMA_append_string(f_name,".ply");
   if (!THD_ok_overwrite() && SUMA_filexists (f_name2)) {
      fprintf (SUMA_STDERR, 
               "Error %s: file %s exists, will not overwrite.\n", 
               FuncName, f_name2);
      SUMA_free(f_name2);f_name2 = NULL;
      SUMA_free(f_name);f_name = NULL;
      SUMA_RETURN (NOPE);
   }
   SUMA_free(f_name2); f_name2 = NULL;
      
   nverts = SO->N_Node;
   nfaces = SO->N_FaceSet;
   
   /* must have XYZ */
   if (SO->NodeDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: SO->NodeDim != 3.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* create the object in ply format */
   verts = (Vertex **) SUMA_malloc (nverts*sizeof(Vertex *));
   faces = (Face *) SUMA_malloc (nfaces*sizeof(Face));
   if (!verts || !faces) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      if (verts) SUMA_free(verts);
      if (faces) SUMA_free(faces);
      SUMA_RETURN (NOPE);
   }

   for (i = 0; i < nfaces; i++) {
      faces[i].intensity = '\001';
      faces[i].nverts = SO->FaceSetDim;
      faces[i].verts = &(SO->FaceSetList[SO->FaceSetDim*i]);
   }
   
   /* open either a binary or ascii PLY file for writing */
   /* (the file will be called "test.ply" because the routines */
   /*  enforce the .ply filename extension) */

   switch (SO->FileFormat) {
      case SUMA_BINARY_BE:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_BINARY_BE, &version);
         break;
      
      case SUMA_BINARY_LE:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_BINARY_LE, &version);
         break;
      
      case SUMA_ASCII:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_ASCII, &version);
         break;
      
      case SUMA_BINARY:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_BINARY_BE, &version);
         break;
      
      case SUMA_FF_NOT_SPECIFIED:
         ply = ply_open_for_writing(f_name, n_elem_names, elem_names, PLY_ASCII, &version);
         break;      
      
      default:
         fprintf (SUMA_STDERR, "Error %s: %d Unrecognized file format.\n", FuncName, SO->FileFormat);
         SUMA_RETURN (NOPE);
         break;  
   }

   if (!ply) {
      fprintf (SUMA_STDERR,"Error %s: Failed to create %s.ply\n", FuncName, f_name);
      if (verts) SUMA_free(verts);
      if (faces) SUMA_free(faces);
      SUMA_RETURN (NOPE);
   }
   /* describe what properties go into the vertex and face elements */

   ply_element_count (ply, "vertex", nverts);
   ply_describe_property (ply, "vertex", &vert_props[0]);
   ply_describe_property (ply, "vertex", &vert_props[1]);
   ply_describe_property (ply, "vertex", &vert_props[2]);

   ply_element_count (ply, "face", nfaces);
   ply_describe_property (ply, "face", &face_props[0]);
   ply_describe_property (ply, "face", &face_props[1]);

   /* write a comment and an object information field */
   ply_put_comment (ply, "author: Greg Turk");
   ply_put_obj_info (ply, "random information");

   /* we have described exactly what we will put in the file, so */
   /* we are now done with the header info */
   ply_header_complete (ply);

   /* set up and write the vertex elements */
   ply_put_element_setup (ply, "vertex");
   for (i = 0; i < nverts; i++)
    ply_put_element (ply, (void *) &(SO->NodeList[SO->NodeDim*i]));

   /* set up and write the face elements */
   ply_put_element_setup (ply, "face");
   for (i = 0; i < nfaces; i++)
    ply_put_element (ply, (void *) &faces[i]);

   /* close the PLY file */
   ply_close (ply);

   /* free */
   if (verts) SUMA_free(verts);
   if (faces) SUMA_free(faces);
   if (f_name) SUMA_free(f_name);
   SUMA_RETURN (YUP);
}


/*! 
   \brief Function to write a surface object to a FreeSurfer .asc file format
   ans = SUMA_Boolean SUMA_FS_Write (fileNm, SO, firstLine);
   \param  fileNm (char *) name (and path) of file.
   \param  SO (SUMA_SurfaceObject *) Surface Object
   \param firstLine (char *) string to place as comment (begins with #) in fileNm
   \return YUP/NOPE
   
   
   The function will not overwrite pre-existing.
   Written by Brenna Bargall
*/
SUMA_Boolean SUMA_FS_Write (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine) 
{
   static char FuncName[]={"SUMA_FS_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   SUMA_ENTRY;
   
   if (!THD_ok_overwrite() && SUMA_filexists(fileNm)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, fileNm);
      SUMA_RETURN (NOPE);
   }
   
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }

   outFile = fopen(fileNm, "w");
   if (!outFile) {
      fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, fileNm);
      SUMA_RETURN (NOPE);
   } 
   
   if (firstLine) fprintf (outFile,"#%s\n", firstLine);
   else fprintf (outFile,"#!ascii version of FreeSurfer surface\n");
   fprintf (outFile, "%d %d\n", SO->N_Node, SO->N_FaceSet);

   j=0;
   for (i=0; i<SO->N_Node; ++i) {
      j=SO->NodeDim * i;
      fprintf (outFile, "%f  %f  %f  0\n", SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
   }

   j=0;
   for (i=0; i<SO->N_FaceSet; ++i) {
      j = SO->FaceSetDim * i;
      fprintf (outFile, "%d %d %d 0\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
   }
    
   
   fclose(outFile);

   SUMA_RETURN (YUP);
   
}

SUMA_Boolean SUMA_isSOinXformedSpace(SUMA_SurfaceObject *SO, NI_element **nelp)
{
   static char FuncName[]={"SUMA_isSOinXformedSpace"};
   NI_element *nel=NULL;
   
   SUMA_ENTRY;
   
   if (nelp) *nelp = NULL;
   
   if (!SO || !SO->aSO) {
      SUMA_S_Warn("Can't tell, returning NO");
      SUMA_RETURN(0);
   }
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Coord_System");
   if (nelp) *nelp = nel;
   if (!nel) {
      SUMA_S_Warn("Can't tell, returning Nein");
      SUMA_RETURN(0);
   }
   SUMA_RETURN(NI_YES_ATTR(nel, "inxformspace"));
}
 
SUMA_Boolean SUMA_GetSOCoordXform(SUMA_SurfaceObject *SO, double xform[4][4])
{
   static char FuncName[]={"SUMA_GetSOCoordXform"};
   NI_element *nel=NULL;
   double *x1d = NULL;
   int i=0,j=0,k=0;
   SUMA_ENTRY;
   
   if (!SO || !SO->aSO) { SUMA_RETURN(0);}
   
   if (!(nel = SUMA_FindNgrNamedElement(SO->aSO, "Coord_System"))) {
      SUMA_RETURN(0);
   }
   
   x1d = (double *)nel->vec[0];
   k = 0;
   for (i=0; i<4;++i) {
      for (j=0; j<4; ++j) {
         xform[i][j] = x1d[k];
         ++k;
      }   
   }
    
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_PutSOCoordXform(SUMA_SurfaceObject *SO, double xform[4][4])
{
   static char FuncName[]={"SUMA_PutSOCoordXform"};
   NI_element *nel=NULL;
   double *x1d = NULL;
   int i=0,j=0,k=0;
   SUMA_ENTRY;
   
   if (!SO || !SO->aSO) { SUMA_RETURN(0);}
   
   if (!(nel = SUMA_FindNgrNamedElement(SO->aSO, "Coord_System"))) {
      SUMA_RETURN(0);
   }
   if (nel->vec_num) x1d = (double *)nel->vec[0];
   else x1d = (double *)SUMA_calloc(16, sizeof(double));
   k = 0;
   for (i=0; i<4;++i) {
      for (j=0; j<4; ++j) {
         x1d[k] = xform[i][j];
         ++k;
      }   
   }
   if (!nel->vec_num)  {
      NI_add_column (nel, NI_DOUBLE, x1d);
      SUMA_free(x1d); x1d=NULL;
   } 
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_GIFTI_Write (  char *fileNm, SUMA_SurfaceObject *SO,
                                 SUMA_SO_File_Format forceencode) 
{
   static char FuncName[]={"SUMA_GIFTI_Write"};
   int i, j, gii_encode=-1;
   float *NodeList=NULL;
   double xform[4][4];
   NI_group *aSO=NULL;
   NI_element *nel=NULL;
   FILE *outFile = NULL;
   SUMA_Boolean suc = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   suc = YUP;
   
   if (!SO) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   
   if (!THD_ok_overwrite() && SUMA_filexists(fileNm)) {
      fprintf (SUMA_STDERR, 
               "Error %s: file %s exists, will not overwrite.\n",
               FuncName, fileNm);
      SUMA_RETURN (NOPE);
   }
   
   if (!SO->aSO) {
      SUMA_LH("Adding aSO");
      SUMA_MergeAfniSO_In_SumaSO(NULL,SO); 
   } else {
      SUMA_LH("Have aSO");
   }
   
   SUMA_LH("Coordinate xform");
   /* Are the coordinates transformed? */
   if (SUMA_isSOinXformedSpace(SO, &nel)) { /* make copy of nodelist */
      SUMA_LH("Copying coords");
      NodeList = (float *)SUMA_malloc(sizeof(float)*SO->NodeDim*SO->N_Node);
      memcpy( (void *)NodeList,(void *)SO->NodeList, 
               sizeof(float)*SO->NodeDim*SO->N_Node );
      /* undo the transform in coordinate copy*/
      if (!SUMA_GetSOCoordXform(SO, xform)) {
         SUMA_S_Err("Failed to get xform!");
         SUMA_free(NodeList); NodeList = NULL;
         SUMA_RETURN(NOPE);
      }
      if (!SUMA_Apply_Coord_xform(NodeList, SO->N_Node, SO->NodeDim,
                                  xform, 1, NULL)) {
         SUMA_S_Err("Failed to apply inverse xform!");
         SUMA_free(NodeList); NodeList = NULL;
         SUMA_RETURN(NOPE);
      }
      /* for normals, recomputing may need to be done if 
      xform is not identity. Not sure what GIFTI definition
      is yet */
      if (!SUMA_IS_XFORM_IDENTITY(xform)) {
         SUMA_S_Warn("Normals are not be properly handled when,\n"
                     "pointset xform is not identity matrix.");
      } else {
         SUMA_LH("Xform is identity");
      }
   } else {
      SUMA_LH("Copying coords pointer");
      NodeList = SO->NodeList;
   }
   
   /* Now fill up aSO */
   SUMA_LH("Filling up aSO");
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Node_XYZ");
   if (nel->vec_num) { 
      SUMA_S_Crit("Unexpected non NULL coords pointer!");
      SUMA_RETURN(NOPE);
   }
   /* Now put the Nodelist in the element.
      No pointer tricks here, keep niml separate */
   
   NI_add_column(nel, NI_FLOAT, (void*)NodeList);
   
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Mesh_IJK");
   if (nel->vec_num) { 
      SUMA_S_Crit("Unexpected non NULL mesh pointer!");
      SUMA_RETURN(NOPE);
   }
   NI_add_column(nel, NI_INT, (void*)SO->FaceSetList);
      
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Node_Normals");
   if (nel->vec_num) { 
      SUMA_S_Crit("Unexpected non NULL node normals pointer!");
      SUMA_RETURN(NOPE);
   }
   if (SO->NodeNormList) NI_add_column(nel, NI_FLOAT, (void*)SO->NodeNormList);
      

   /* show me aSO */
   if (LocalHead) {
      SUMA_ShowAfniSurfaceObject( SO->aSO, NULL, 
                                  1, "SUMA_GIFTI_Write Debug:\n");
   }
   /* write it out  */
   /* gii_encode values set based on #define values in  gifti_io.h */
   gii_encode = -1;
   if (forceencode == SUMA_XML_SURF) { 
      gii_encode = 0; /* let gii function defaults rule */
   } else if (forceencode == SUMA_XML_B64_SURF) { 
      gii_encode = 2;
   } else if (forceencode == SUMA_XML_B64GZ_SURF) { 
      gii_encode = 3;
   } else if (forceencode == SUMA_XML_ASCII_SURF) {
      gii_encode = 1;
   }

   if (!afni_write_gifti_surf(SO->aSO, fileNm, 1, gii_encode)) {
      SUMA_S_Errv("Failed to write SO to %s\n", fileNm);
      suc = NOPE;
   }
   
   /* make sure all is back to initial state */
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Node_XYZ");
   if (NodeList != SO->NodeList) {
      SUMA_free(NodeList); NodeList = NULL;
   } 
   NI_remove_column(nel,0);
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Mesh_IJK");
   NI_remove_column(nel,0);
   nel = SUMA_FindNgrNamedElement(SO->aSO, "Node_Normals");
   NI_remove_column(nel,0);
   
   SUMA_RETURN(suc);
}
/*! 
   \brief Function to write a surface object to a BYU file format
   ans = SUMA_Boolean SUMA_BYU_Write (fileNm, SO);
   \param  fileNm (char *) name (and path) of file.
   \param  SO (SUMA_SurfaceObject *) Surface Object
   \param base1 (int) if (base1) then 1st node is indexed 1 else it is 0
   \return YUP/NOPE
   
   
   The function will not overwrite pre-existing.
   Written by Brenna Bargall
*/
SUMA_Boolean SUMA_BYU_Write (char *fileNm, SUMA_SurfaceObject *SO, int base1) 
{
   static char FuncName[]={"SUMA_BYU_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   SUMA_ENTRY;
   
   if (!THD_ok_overwrite() && SUMA_filexists(fileNm)) {
      fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, fileNm);
      SUMA_RETURN (NOPE);
   }
   
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }

   outFile = fopen(fileNm, "w");
   if (!outFile) {
      fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, fileNm);
      SUMA_RETURN (NOPE);
   } 
   if (!base1) {
      SUMA_S_Warn("Not sure what to do when base1 is off.\n");
   }
   fprintf (outFile, 
            "%7d %7d %7d %7d\n %7d %7d\n",
             1, SO->N_Node, SO->N_FaceSet, 
             (SO->EL ? SO->EL->N_Distinct_Edges:-1), (base1 ? 1:0), (base1 ? SO->N_FaceSet:SO->N_FaceSet-1));

   j=0;
   for (i=0; i<SO->N_Node; ++i) {
      j=SO->NodeDim * i;
      fprintf (outFile, "%e  %e  %e \n", SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
   }

   j=0;
   for (i=0; i<SO->N_FaceSet; ++i) {
      j = SO->FaceSetDim * i;
      if (!base1) {
         fprintf (outFile, "%7d %7d %7d\n", 
                        SO->FaceSetList[j], SO->FaceSetList[j+1], -SO->FaceSetList[j+2]);
      } else {
         fprintf (outFile, "%7d %7d %7d\n", 
                        SO->FaceSetList[j]+1, SO->FaceSetList[j+1]+1, -(SO->FaceSetList[j+2]+1));
      }
   }
    
   
   fclose(outFile);

   SUMA_RETURN (YUP);
   
}

/*!
   \brief writes the NodeList and FaceSetList of SO to 2 ascii files
   ans = SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
   \param  Fname (SUMA_SFname *) uses the SureFit filename structure to store
                                 the names (and paths) of the NodeList (name_coord)
                                 and the FaceSetList (name_topo) files.
                                 if (strlen(name_coord) == 0) no coord is written
                                 if (strlen(name_topo) == 0) no topo is written
   \param SO (SUMA_SurfaceObject *) pointer to SO structure.
   \return YUP/NOPE
   
   \sa SUMA_VEC_Read
   The function will not overwrite pre-existing files.
   
*/
SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO)
{
   
   static char FuncName[]={"SUMA_VEC_Write"};
   int i, j;
   FILE *outFile = NULL;
   
   SUMA_ENTRY;

   if (strlen(Fname->name_coord)) {
      if (!THD_ok_overwrite() && SUMA_filexists(Fname->name_coord)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }
   }
   if (strlen(Fname->name_topo)) {
      if (!THD_ok_overwrite() && SUMA_filexists(Fname->name_topo)) {
         fprintf (SUMA_STDERR, "Error %s: file %s exists, will not overwrite.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }
   }
   if (SO->NodeDim != 3 || SO->FaceSetDim != 3) {
      fprintf (SUMA_STDERR, "Error %s: Must have NodeDim and FaceSetDim = 3.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (strlen(Fname->name_coord)) {
      outFile = fopen(Fname->name_coord, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_coord);
         SUMA_RETURN (NOPE);
      }

      j=0;
      for (i=0; i<SO->N_Node; ++i) {
         j=SO->NodeDim * i;
         fprintf (outFile, "%f  %f  %f \n", SO->NodeList[j], SO->NodeList[j+1], SO->NodeList[j+2]);
      }

      fclose (outFile);
   }
   
   if (strlen(Fname->name_topo)) {   
      outFile = fopen(Fname->name_topo, "w");
      if (!outFile) {
         fprintf (SUMA_STDERR, "Error %s: Failed in opening %s for writing.\n",FuncName, Fname->name_topo);
         SUMA_RETURN (NOPE);
      }
      j=0;
      for (i=0; i<SO->N_FaceSet; ++i) {
         j = SO->FaceSetDim * i;
         fprintf (outFile, "%d %d %d\n", SO->FaceSetList[j], SO->FaceSetList[j+1], SO->FaceSetList[j+2]);
      }

      fclose (outFile);
   }
   
   SUMA_RETURN (YUP);

}

/*!
   \brief function to read 1D (vec) format surfaces
   \sa SUMA_VEC_Write
*/
SUMA_Boolean SUMA_VEC_Read(SUMA_SFname *Fname, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_VEC_Read"};
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int icnt;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || !Fname) {
      SUMA_SL_Err("NULL input");
      SUMA_RETURN(NOPE);
   }
   if (SO->NodeList || SO->FaceSetList) {
      SUMA_SL_Err("Non NULL SO->NodeList || SO->FaceSetList");
      SUMA_RETURN(NOPE);   
   }

   im = mri_read_1D (Fname->name_coord);
   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NOPE);
   }
   far = MRI_FLOAT_PTR(im);
   SO->N_Node = im->nx;
   SO->NodeDim = im->ny;
   if (!SO->N_Node) {
      SUMA_SL_Err("Empty file");
      SUMA_RETURN(NOPE);
   }
   if (SO->NodeDim !=  3 ) {
      SUMA_SL_Err("File must have\n"
                  "3 columns.");
      mri_free(im); im = NULL;   /* done with that baby */
      SUMA_RETURN(NOPE);
   }

   SO->NodeList = (float *)SUMA_calloc (SO->N_Node*SO->NodeDim, sizeof(float));
   if (!SO->NodeList) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for NodeList.\n", FuncName);
      if (SO->NodeList) SUMA_free(SO->NodeList);
      if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
      SUMA_RETURN (NOPE);
   }

   for (icnt=0; icnt < SO->N_Node; ++icnt) {
      SO->NodeList[3*icnt] = far[icnt];
      SO->NodeList[3*icnt+1] = far[icnt+SO->N_Node];
      SO->NodeList[3*icnt+2] = far[icnt+2*SO->N_Node];
   }   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: SO->NodeList\n Node 0: %f, %f, %f \n Node %d: %f, %f, %f \n",
         FuncName,
         SO->NodeList[0], SO->NodeList[1], SO->NodeList[2], SO->N_Node -1, 
         SO->NodeList[3*(SO->N_Node-1)], SO->NodeList[3*(SO->N_Node-1)+1], SO->NodeList[3*(SO->N_Node-1)+2]);
   }
   mri_free(im); im = NULL;

   im = mri_read_1D (Fname->name_topo);
   if (!im) {
      SUMA_SL_Err("Failed to read 1D file");
      SUMA_RETURN(NOPE);
   }
   far = MRI_FLOAT_PTR(im);
   SO->N_FaceSet = im->nx;
   SO->FaceSetDim = im->ny;
   if (!SO->N_FaceSet) {
      SUMA_SL_Err("Empty file");
      SUMA_RETURN(NOPE);
   }
   if (SO->FaceSetDim !=  3 ) {
      SUMA_SL_Err("File must have\n"
                  "3 columns.");
      mri_free(im); im = NULL;   /* done with that baby */
      SUMA_RETURN(NOPE);
   }

   SO->FaceSetList = (int *)SUMA_calloc (SO->N_FaceSet*SO->FaceSetDim, sizeof(int));
   if (!SO->FaceSetList) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for FaceSetList.\n", FuncName);
      if (SO->NodeList) SUMA_free(SO->NodeList);
      if (SO->FaceSetList) SUMA_free(SO->FaceSetList);
      SUMA_RETURN (NOPE);
   }

   for (icnt=0; icnt < SO->N_FaceSet; ++icnt) {
      SO->FaceSetList[3*icnt] = (int)far[icnt];
      SO->FaceSetList[3*icnt+1] = (int)far[icnt+SO->N_FaceSet];
      SO->FaceSetList[3*icnt+2] = (int)far[icnt+2*SO->N_FaceSet];
   }   

   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: SO->FaceSetList\n Node 0: %d, %d, %d \n Node %d: %d, %d, %d \n",
         FuncName,
         SO->FaceSetList[0], SO->FaceSetList[1], SO->FaceSetList[2], SO->N_FaceSet -1, 
         SO->FaceSetList[3*(SO->N_FaceSet-1)], SO->FaceSetList[3*(SO->N_FaceSet-1)+1], SO->FaceSetList[3*(SO->N_FaceSet-1)+2]);
   } 
   mri_free(im); im = NULL;

   SUMA_RETURN(YUP);
}

/*!
   \brief A function to write a Surface Object into a Surefit ascii format 
   \param F_prefix (char *) Prefix of surace filenames. Output will be of the form:
         Prefix.N_NODE.topo
         Prefix.N_NODE.coord where N_Node is the number of nodes making up the surface.
   \param SO (SUMA_SurfaceObject *) surface object
   \return YUP/NOPE
   
*/


#ifdef SUMA_Ply_Read_STAND_ALONE
void usage_SUMA_Ply_Read_Main ()
   
  {/*Usage*/
          printf ("\nUsage:  SUMA_Ply_Read -s f_name \n");
          printf ("\t reads in a .ply file and writes it out to copy_f_name.ply\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \t Wed Jan  8 13:44:29 EST 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_Ply_Read_Main"}; 
	int kar;
   char *f_name=NULL, out_f_name[200];
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean brk;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   
   if (argc < 3)
       {
          usage_SUMA_Ply_Read_Main ();
          exit (1);
       }
   
   kar = 1;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_Ply_Read_Main();
          exit (1);
		}
		
		if (!brk && (strcmp(argv[kar], "-s") == 0)) {
			kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -s ");
				exit (1);
			}
			f_name = argv[kar];
			/*fprintf(SUMA_STDOUT, "Found: %s\n", f_name);*/

			brk = YUP;
		}
      
      if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (!f_name) {
      fprintf (SUMA_STDERR,"Error %s: Missing filename.\n", FuncName);
      exit(1);
   }
   
   SO = SUMA_Alloc_SurfObject_Struct(1);   
   if (!SUMA_Ply_Read (f_name, SO)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Ply_Read.\n", FuncName);
      exit (1);
   } 
   
   SO->Label = SUMA_SurfaceFileName (SO, NOPE);
   sprintf (out_f_name , "copy_%s", SO->Label);   
   fprintf (SUMA_STDERR,"%s: Success apparent. Now writing SO to %s\n", FuncName, out_f_name);
   if (!SUMA_Ply_Write (out_f_name, SO)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Ply_Write.\n", FuncName);
      exit (1);
   } 
      
   SUMA_Free_Surface_Object (SO);
   
   return (0);
 } /* Main */  
#endif


/*!
   \brief Handles opening an ROI file
   \param filename (char *)
   \param data(void *) 
   
   - results are placed in SUMAg_DOv
   
*/
void SUMA_OpenDrawnROI (char *filename, void *data)
{
   static char FuncName[]={"SUMA_OpenDrawnROI"};
   DList *list=NULL;
   SUMA_DRAWN_ROI **ROIv=NULL;
   int i, N_ROI;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *over=NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");   

   /* check for type ... */
   
   if (SUMA_isExtension(filename, ".niml.roi")) {
      /* load niml ROI */
      if (!( ROIv = SUMA_OpenDrawnROI_NIML (filename, &N_ROI, YUP))) {
         SUMA_SLP_Err("Failed to read NIML ROI.");
         SUMA_RETURNe;
      }
   }else if (SUMA_isExtension(filename, ".1D.roi")) {
      /* load 1D ROI */
      /* You need to select a parent surface */
      SUMA_SLP_Warn("Assuming parent surface.");
      SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SUMAg_SVv[0].Focus_SO_ID].OP);
      if (!( ROIv = SUMA_OpenDrawnROI_1D (filename, SO->idcode_str, 
                                          &N_ROI, YUP))) {
         SUMA_SLP_Err("Failed to read NIML ROI.");
         SUMA_RETURNe;
      }
   }else {
      SUMA_SLP_Err(  "Failed to recognize\n"
                     "ROI type from filename.");
      SUMA_RETURNe;
   } 
   
   /* put those ROIs in SUMAg_DOv */
   for (i=0; i < N_ROI; ++i) {
      /* add ROI to DO list */
      if (!SUMA_AddDO ( SUMAg_DOv, &SUMAg_N_DOv, 
                        (void *)ROIv[i], ROIdO_type, SUMA_WORLD)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      }
   }
   /* free ROIv */
   if (ROIv) SUMA_free(ROIv); ROIv = NULL;

   /* if there are no currentROIs selected, set currentROI to the
      first in the list */
   if (!SUMAg_CF->X->DrawROI->curDrawnROI) {
      i = 0;
      do {
         if (SUMAg_DOv[i].ObjectType == ROIdO_type) 
            SUMAg_CF->X->DrawROI->curDrawnROI =
                                             (SUMA_DRAWN_ROI *)SUMAg_DOv[i].OP;
         ++i;
      } while (i < SUMAg_N_DOv && !SUMAg_CF->X->DrawROI->curDrawnROI);
   }
   
   if (SUMAg_CF->X->DrawROI->curDrawnROI) {
      SUMA_InitializeDrawROIWindow(SUMAg_CF->X->DrawROI->curDrawnROI);   
   }
   
   /* Now update the Paint job on the ROI plane */
   SO = SUMA_findSOp_inDOv(
               SUMAg_CF->X->DrawROI->curDrawnROI->Parent_idcode_str, 
               SUMAg_DOv, SUMAg_N_DOv);
   if (!SUMA_Paint_SO_ROIplanes_w (SO, SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURNe;
   }
   
   /* find the overlay plane */
   over = SUMA_Fetch_OverlayPointer(
            SO->Overlays, SO->N_Overlays, 
            SUMAg_CF->X->DrawROI->curDrawnROI->ColPlaneName,
            &i);       
   if (over) SUMA_InitializeColPlaneShell(SO, over);

   /* put a nice redisplay here */
   if (!list) list = SUMA_CreateList ();
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA( list, SE_Redisplay_AllVisible, 
                                       SES_Suma, NULL); 
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to redisplay.");
      SUMA_RETURNe;
   }
   
   
   SUMA_RETURNe; 
}

/*!
   Since parent information does not exist in ROI 1D files, 
   you need to specify the parent surface.
   
   ans = SUMA_OpenDrawnROI_1D (filename, Parent_idcode_str);
   
   \param filename (char *) name of 1D file (see below for formats)
   \param Parent_idcode_str (char *) idcode of parent surface
   \param N_ROI (int *) will contain the number of ROIs read in
   \param ForDisplay (SUMA_Boolean) YUP: prepares ROI for display
   \return ans (SUMA_Boolean) YUP for good NOPE for not good.
   
   - The ROI is added to SUMAg_DOv
   
   - The 1D file can have multiple formats. Some are particularly
   inefficient and should not be used. But we aim to please so 
   anything goes. You can have a varying number of columns which 
   are i, l, r, g, b. These columns stand for index, label, red,
   green and blue, respectively.
      * format 1: i 
      Only a bunch of node (int) indices are supplied. Label of all nodes
      defaults to 0 and a default color is given
      * format 2: i l
      Each node carries a label (int) with it. This way you can
      have multiple ROIs specified in one 1D file. The same
      color is assigned to each of the ROIs. 
      * format 3: i r g b
      A bunch of nodes with r g b (float) triplet specifying the nodes'
      colors. Obviously, since all nodes belong to the same ROI, 
      the specification of an r g b for each node is redundant since
      all nodes in the same ROI have the same color. Use the 
      niml format if that sounds crazy to you. If you want a different
      color for each node then you should not load the data as an ROI
      but as a node color file. 
      * format 4: i l r g b
      A bunch of nodes with labels and r g b .
      Like format 2 but with the possibility of specifying the colors of
      each ROI. 
       
*/ 
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_1D (char *filename, char *Parent_idcode_str,
                                        int *N_ROI, SUMA_Boolean ForDisplay)
{
   static char FuncName[]={"SUMA_OpenDrawnROI_1D"};
   MRI_IMAGE *im = NULL;
   int ncol, nrow, *iLabel=NULL, *iNode = NULL, *isort=NULL,
      i, N_Labels = 0, *iStart=NULL, *iStop=NULL, cnt = 0;
   float *far=NULL, *r=NULL, *g=NULL, *b=NULL, *RGB=NULL;
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");
   
   *N_ROI = 0;
   
   im = mri_read_1D (filename);
   
   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }
   
   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;
   
   if (!ncol) {
      SUMA_SL_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   if (nrow != 1 && nrow != 2 && nrow != 4 && nrow != 5) {
      SUMA_SL_Err("File must have\n"
                  " 1,2,4 or 5 columns.");
      mri_free(im); im = NULL;   /* done with that baby */
      SUMA_RETURN(NULL);
   }
   
   if (0 && LocalHead) {
      SUMA_disp_vect(far, ncol*nrow);
   }
      
   switch (nrow) {
      case 1:
         /* Node index only*/
         SUMA_LH ("1D format: i");
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         iLabel = (int *)SUMA_malloc(1*sizeof(int));
         if (!iNode ||!iLabel) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         for (i=0; i < ncol; ++i) iNode[i] = (int)far[i];
         mri_free(im); im = NULL;   /* done with that baby */
         
         iLabel[0] = 0;
         N_Labels = 1;
         iStart = (int *)SUMA_malloc(1*sizeof(int));
         iStop = (int *)SUMA_malloc(1*sizeof(int));
         RGB = (float *)SUMA_malloc(3*1*sizeof(float));
         iStart[0] = 0;
         iStop[0] = ncol-1;
         RGB[0] = 1.0; RGB[1] = 1.0; RGB[2] = 0;
         break;
      case 2:
         /* Node index & Node Label */
         SUMA_LH("1D format: i l");
         /* copy the node indices and labels for cleanliness */
         iLabel = (int *)SUMA_malloc(ncol*sizeof(int));
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         if (!iNode || !iLabel) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         for (i=0; i < ncol; ++i) iLabel[i] = (int)far[i+ncol];
         /* sort the Labels and the iNode accordingly */
         isort = SUMA_z_dqsort( iLabel, ncol);
         for (i=0; i < ncol; ++i) iNode[i] = (int)far[isort[i]];

         mri_free(im); im = NULL;   /* done with that baby */

         /* Count the number of distinct labels */
         N_Labels = 1;
         for (i=1; i < ncol; ++i) if (iLabel[i] != iLabel[i-1]) ++N_Labels;
         /* store where each label begins and ends */
         iStart = (int *)SUMA_malloc(N_Labels*sizeof(int));
         iStop = (int *)SUMA_malloc(N_Labels*sizeof(int));
         RGB = (float *)SUMA_malloc(3*N_Labels*sizeof(float));
         if (!iStart || !iStop) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         cnt = 0;
         iStart[cnt] = 0;
         iStop[cnt] = ncol -1;
         RGB[3*cnt] = 1.0; RGB[3*cnt+1] = 1.0; RGB[3*cnt+2] = 0;
         for (i=1; i < ncol; ++i) {
            if (iLabel[i] != iLabel[i-1]) {
               iStop[cnt] = i-1;
               ++cnt; 
               iStart[cnt] = i;
               iStop[cnt] = ncol -1;
               RGB[3*cnt] = 1.0; RGB[3*cnt+1] = 1.0; RGB[3*cnt+2] = 0;
            }
         }
         break;
      case 4:
         /* Node index, R G B */
         SUMA_LH("1D format: i R G B");
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         iLabel = (int *)SUMA_malloc(1*sizeof(int));
         r = (float *)SUMA_malloc(ncol*sizeof(float));
         g = (float *)SUMA_malloc(ncol*sizeof(float));
         b = (float *)SUMA_malloc(ncol*sizeof(float));
         if (!iNode || !iLabel || !r || !g || !b) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         
         for (i=0; i < ncol; ++i) {
            iNode[i] = (int)far[i];
            r[i] = (float)far[i+ncol];
            g[i] = (float)far[i+2*ncol];
            b[i] = (float)far[i+3*ncol];
         }
         
         iLabel[0] = 0;
         N_Labels = 1;
         iStart = (int *)SUMA_malloc(1*sizeof(int));
         iStop = (int *)SUMA_malloc(1*sizeof(int));
         RGB = (float *)SUMA_malloc(3*1*sizeof(float));
         mri_free(im); im = NULL;   /* done with that baby */
        
         iStart[0] = 0;
         iStop[0] = ncol-1;
         RGB[0] = r[0]; RGB[1] = g[0]; RGB[2] = b[0];
         break;
      case 5:
         /* Node index, Node Label, R G B */
         SUMA_LH("1D format: i l R G B");
         /* copy the node indices and labels for cleanliness */
         iLabel = (int *)SUMA_malloc(ncol*sizeof(int));
         iNode = (int *)SUMA_malloc(ncol*sizeof(int));
         r = (float *)SUMA_malloc(ncol*sizeof(float));
         g = (float *)SUMA_malloc(ncol*sizeof(float));
         b = (float *)SUMA_malloc(ncol*sizeof(float));
         if (!iNode || !iLabel || !r || !g || !b) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         for (i=0; i < ncol; ++i) iLabel[i] = (int)far[i+ncol];
         /* sort the Labels and the iNode accordingly */
         isort = SUMA_z_dqsort( iLabel, ncol);
         for (i=0; i < ncol; ++i) {
            iNode[i] = (int)far[isort[i]];
            r[i] = (float)far[isort[i]+2*ncol];
            g[i] = (float)far[isort[i]+3*ncol];
            b[i] = (float)far[isort[i]+4*ncol];
         }     
         mri_free(im); im = NULL;   /* done with that baby */

         /* Count the number of distinct labels */
         N_Labels = 1;
         for (i=1; i < ncol; ++i) if (iLabel[i] != iLabel[i-1]) ++N_Labels;
         /* store where each label begins and ends */
         iStart = (int *)SUMA_malloc(N_Labels*sizeof(int));
         iStop = (int *)SUMA_malloc(N_Labels*sizeof(int));
         RGB = (float *)SUMA_malloc(3*N_Labels*sizeof(float));
         if (!iStart || !iStop || !RGB) {
            SUMA_SL_Err("Failed to allocate");
            SUMA_RETURN(NULL);
         }
         cnt = 0;
         iStart[cnt] = 0;
         iStop[cnt] = ncol -1;
         RGB[3*cnt] = r[0]; RGB[3*cnt+1] = g[0]; RGB[3*cnt+2] = b[0];
         for (i=1; i < ncol; ++i) {
            if (iLabel[i] != iLabel[i-1]) {
               iStop[cnt] = i-1;
               ++cnt; 
               iStart[cnt] = i;
               iStop[cnt] = ncol -1;
               RGB[3*cnt] = r[i]; RGB[3*cnt+1] = g[i]; RGB[3*cnt+2] = b[i];
            }
         }
         break;
      default:
         SUMA_SLP_Err("Unrecognized 1D format");
         mri_free(im); im = NULL;   /* done with that baby */
         break;
   }
   
   
   ROIv = (SUMA_DRAWN_ROI **)SUMA_calloc(N_Labels,sizeof(SUMA_DRAWN_ROI*));
   
   for (i=0; i < N_Labels; ++i) {
      int Value, N_Node, *Node=NULL;
      float fillcolor[3], edgecolor[3];
      int edgethickness;
      char stmp[20], *Label=NULL;
      SUMA_PARSED_NAME *NewName=NULL; 
      
      edgethickness = 3;
      fillcolor[0] = RGB[3*i]; 
      fillcolor[1] = RGB[3*i+1]; 
      fillcolor[2] = RGB[3*i+2]; 
      edgecolor[0] = 0; edgecolor[1] = 0; edgecolor[2] = 1; 
      Value = iLabel[iStart[i]]; /* the index label of this ROI */
      N_Node = iStop[i] - iStart[i] + 1; /* Number of Nodes in this ROI */
      Node = &(iNode[iStart[i]]); 
                     /* pointer to location of first index in this ROI */
      /* prepare a label for these ROIs */
      NewName = SUMA_ParseFname (filename, NULL);
      if (!NewName) {
         Label = SUMA_copy_string("BadLabel");
      }else {
         sprintf(stmp,"(%d)", Value);
         Label = SUMA_append_string(stmp,NewName->FileName_NoExt);
      }
      SUMA_LH("Transforming to Drawn ROIs...");
      ROIv[i] = SUMA_1DROI_to_DrawnROI( Node, N_Node , 
                                    Value, Parent_idcode_str,
                                    Label, NULL,
                                    fillcolor, edgecolor, edgethickness, 
                                    SUMAg_DOv, SUMAg_N_DOv,
                                    ForDisplay);
      if (nrow == 5 || nrow == 4) {
         SUMA_LH("Marking as color by fillcolor");
         ROIv[i]->ColorByLabel = NOPE;
      }
      if (Label) SUMA_free(Label); Label = NULL;
      if (NewName) SUMA_Free_Parsed_Name(NewName); NewName = NULL;
      if (LocalHead) 
         fprintf (SUMA_STDERR,   "%s: ROI->Parent_idcode_str %s\n", 
                                 FuncName, ROIv[i]->Parent_idcode_str);

   }
 
   SUMA_LH("Freeing...");
   
   if (iLabel) SUMA_free(iLabel); iLabel = NULL;
   if (isort) SUMA_free(isort); isort = NULL;
   if (iNode) SUMA_free(iNode); iNode = NULL;
   if (iStart) SUMA_free(iStart); iStart = NULL;
   if (iStop) SUMA_free(iStop); iStop = NULL;
   if (r) SUMA_free(r); r = NULL;
   if (g) SUMA_free(g); g = NULL;
   if (b) SUMA_free(b); b = NULL;
   if (RGB) SUMA_free(RGB); RGB = NULL;
   
   *N_ROI = N_Labels;
   SUMA_RETURN(ROIv);
     
}

/*!
   \brief Loads a niml ROI 
   
   \param ForDisplay (SUMA_Boolean) 
      YUP: Performs checks to see if ROI with similar idcode already
      exists and if parent surface is loaded.
      NOPE: Does not check for above conditions.
*/
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_NIML (char *filename, 
                                          int *N_ROI, 
                                          SUMA_Boolean ForDisplay)
{ /* begin embedded function */
   static char FuncName[]={"SUMA_OpenDrawnROI_NIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+100], *nel_idcode, *att=NULL;
   NI_element *nel = NULL;
   NI_element **nelv=NULL;
   NI_stream ns ;
   int n_read=0, idat, answer, inel, iDO, N_nel, iwarn=0;
   SUMA_NIML_ROI_DATUM *niml_ROI_datum_buff=NULL;
   SUMA_NIML_DRAWN_ROI * nimlROI=NULL;
   SUMA_DRAWN_ROI **ROIv=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean found = YUP, AddNel = YUP, 
            AlwaysReplace = NOPE, NeverReplace = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   *N_ROI = 0;
   
   if (SUMAg_CF->nimlROI_Datum_type < 0) {
      SUMA_SL_Err("Bad niml type code");
      SUMA_RETURN(NULL);
   }
   if (LocalHead) 
      fprintf( SUMA_STDERR, "%s: roi_type code = %d\n", 
               FuncName, SUMAg_CF->nimlROI_Datum_type) ;

   sprintf(stmp,"file:%s", filename);
   ns = NI_stream_open( stmp , "r" ) ;
   if( ns == NULL ){
      SUMA_SL_Err("Can't open ROI file"); 
      SUMA_RETURN(NULL);
   }
   
   nelv = (NI_element **) 
         SUMA_calloc(SUMA_MAX_DISPLAYABLE_OBJECTS, sizeof(NI_element *));
   if (!nelv) {
      SUMA_SLP_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   NeverReplace = NOPE;
   AlwaysReplace = NOPE;
   inel = 0;
   do {
      SUMA_LH("Calling NI_read_element");
      nel = NI_read_element(ns,1) ;
      
      if (nel && nel->vec_num) { /* Must check for ROIs that are empty.
                                    They cause a crash that was meticulously
                                    reported by Mike Arcaro.  
                                    Empty ROIs can be created if one
                                    starts a new ROI then undo drawing operations
                                    and save all ROIs to niml format. */ 
         found = YUP;
         SUMA_LH("Have nel, will travel");
         if (LocalHead && 0) SUMA_nel_stdout (nel);
         
         if (strcmp(nel->name,SUMA_Dset_Type_Name(SUMA_NODE_ROI))) {
            SUMA_SLP_Err ( "ni element not of the \n"
                           "Node ROI variety.\n"
                           "Element discarded.");
            NI_free_element(nel) ; nel = NULL;
            SUMA_RETURN(NULL);
         }
         SUMA_LH("Type check redux");
         /* somewhat redundant test */
         if (nel->vec_typ[0] != SUMAg_CF->nimlROI_Datum_type) {
            SUMA_SLP_Err ("Datum type mismatch.");
            NI_free_element(nel) ; nel = NULL;
            SUMA_RETURN(NULL);
         }

         SUMA_LH("IDs please");
         /* make sure id is set properly (the curse of old versions) */
         att = NI_get_attribute( nel , "self_idcode");
         if (!att) { /* try old way */
            if ((att = NI_get_attribute( nel , "idcode_str"))) {
               NI_set_attribute(nel, "self_idcode", att);
               NI_kill_attribute (nel,"idcode_str"); 
               att = NI_get_attribute( nel , "self_idcode");
            } else if ((att = NI_get_attribute( nel , "Object_ID"))) {
               NI_set_attribute(nel, "self_idcode", att);
               NI_kill_attribute (nel,"Object_ID"); 
               att = NI_get_attribute( nel , "self_idcode");
            }
         }
         if (att) {
            nel_idcode = att; 
         } else { /* put one in, anything is fine */
            att = (char*)SUMA_calloc(SUMA_IDCODE_LENGTH, 
                                     sizeof(char)); 
            UNIQ_idcode_fill(att);
            NI_set_attribute(nel,"self_idcode",att);
            SUMA_free(att); att = NULL;
            nel_idcode = NI_get_attribute(nel,"self_idcode"); 
         }
         
         if (ForDisplay) {
            /* find out if a displayable object exists 
               with the same idcode_str */
            SUMA_LH("Checking for self id...");
            if (!nel_idcode) { 
               SUMA_S_Err("Must have id by now!");
               SUMA_RETURN(NULL);
            } 
            if (SUMA_existDO(nel_idcode, SUMAg_DOv, SUMAg_N_DOv)) {
               if (AlwaysReplace) {
                  AddNel = YUP; 
               }
               if (NeverReplace) {
                  AddNel = NOPE;
               }
               if (!AlwaysReplace && !NeverReplace) {   /* ASk */
                  sprintf(stmp, "Found duplicate ROIs.\n"
                                "Replace ROI %s (%s) by\n" 
                                "version in file ?", 
                  NI_get_attribute( nel , "Label"), nel_idcode); 

                  answer = SUMA_ForceUser_YesNo (SUMAg_SVv[0].X->TOPLEVEL, 
                                    stmp, 
                                    0, SWP_DONT_CARE);
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "%s: Got %d, You ?\n", FuncName, answer);
                  switch (answer) {
                     case SUMA_YES:
                        SUMA_LH("YES");
                        AddNel = YUP;
                        break;

                     case SUMA_NO:
                        SUMA_LH("NO");
                        /* don't add this one */
                        AddNel = NOPE;
                        break;

                     case SUMA_YES_ALL:
                        SUMA_LH("YES ALL");
                        /* cancel Check_Prior */
                        AddNel = YUP;
                        AlwaysReplace = YUP;
                        break;

                     case SUMA_NO_ALL:
                        SUMA_LH("NO ALL");
                        /* don't add this one and set flag to 
                           ignore the doubles */
                        AddNel = NOPE;
                        NeverReplace = YUP;
                        break;

                     default:
                        SUMA_SLP_Crit(
                           "Don't know what to do with this button.");
                        SUMA_RETURN(NULL);
                        break;
                  }
               } 
            } else {
               AddNel = YUP;
            } 
         
            /* make sure element's parent exists */
            if (AddNel) {
               SUMA_LH("Checking for Parent surface...");
               att = NI_get_attribute( nel , "domain_parent_idcode");
               if (!att) { /* try old way */
                  if ((att = NI_get_attribute( nel , "Parent_idcode_str"))) {
                     NI_set_attribute(nel, "domain_parent_idcode", att);
                     NI_kill_attribute (nel,"Parent_idcode_str");
                     att = NI_get_attribute(nel,"domain_parent_idcode"); 
                  } else if ((att = NI_get_attribute( nel , "Parent_ID"))) {
                     NI_set_attribute(nel, "domain_parent_idcode", att);
                     NI_kill_attribute (nel,"Parent_ID"); 
                     att = NI_get_attribute(nel,"domain_parent_idcode"); 
                  }
               }
               iDO = -1;
               if (att) {
                  iDO = SUMA_whichDO(  att, 
                                       SUMAg_DOv, SUMAg_N_DOv);
               }
              
               if (iDO < 0) {
                  if (!iwarn) {
                     SUMA_S_Warnv(
                        "ROI's parent surface not loaded, or ROI is\n"
                        "unparented. Looking for adoptive parents...\n"
                        "(Message muted for remainder of ROIs in:\n"
                        " %s )\n"
                        , filename );
                     ++iwarn;
                  }
                  iDO = SUMA_BiggestLocalDomainParent(SUMAg_DOv, SUMAg_N_DOv); 
                  if (iDO < 0) {
                     SUMA_S_Err("Can't find adoptive surface");
                     AddNel = NOPE;
                  } else {
                     SO = (SUMA_SurfaceObject *)SUMAg_DOv[iDO].OP;
                     SUMA_LHv("Found LDP in %s\n", SO->Label);
                     NI_set_attribute(nel,
                                       "domain_parent_idcode", 
                                       SO->idcode_str);
                     AddNel = YUP;
                  }
               }
            }
         } else {
            AddNel = YUP; /* ignore checks */
         }         
         
         if (AddNel) {
            SUMA_LHv("Adding Nel %d\n", inel);
            nelv[inel] = nel;
            ++inel; 
         }else {
            SUMA_LHv("Skipping Nel %d\n", inel);
         }
         
         ++n_read;
      }else {
         SUMA_LH("NULL nel, to the spruce goose");
         found = NOPE;
      } 
      
   } while (found);
   
   NI_stream_close(ns) ;
   N_nel = inel;
   
   if( !n_read){
      SUMA_SL_Err("Found no elements in file!"); 
      SUMA_free(nelv);
      SUMA_RETURN(NULL);
   }
   if (!N_nel) {
      SUMA_free(nelv);
      SUMA_RETURN(NULL);
   }
   /* Now turn those nel into ROIS */
   ROIv = (SUMA_DRAWN_ROI **) SUMA_calloc(N_nel, sizeof(SUMA_DRAWN_ROI*));
   for (inel=0; inel < N_nel; ++inel) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: Processing nel %d/%d...\n", FuncName, inel, N_nel);
      nel = nelv[inel];
      nel_idcode = NI_get_attribute( nel , "self_idcode"); 
      if (!nel_idcode) {
         SUMA_S_Err("An id must be present by now!\n");
         SUMA_RETURN(NULL);
      }
      /* store nel in nimlROI struct */

      /* allocate for nimlROI */
      nimlROI = (SUMA_NIML_DRAWN_ROI *)
                     SUMA_calloc(1,sizeof(SUMA_NIML_DRAWN_ROI));
      nimlROI->Type = (int)strtod(NI_get_attribute( nel , "Type"), NULL);
      nimlROI->idcode_str = 
         SUMA_copy_string(NI_get_attribute( nel , "self_idcode"));
      nimlROI->Parent_idcode_str = 
         SUMA_copy_string(NI_get_attribute( nel , "domain_parent_idcode"));
      nimlROI->Label = SUMA_copy_string(NI_get_attribute( nel , "Label"));
      nimlROI->iLabel = (int)strtod(NI_get_attribute( nel , "iLabel"), NULL);
      nimlROI->N_ROI_datum = nel->vec_len;
      nimlROI->ColPlaneName = 
         SUMA_copy_string(NI_get_attribute( nel , "ColPlaneName"));
      if (SUMA_StringToNum (NI_get_attribute( nel , "FillColor"), 
                           (void*)nimlROI->FillColor, 3,1) < 0) {
         SUMA_SLP_Err("Failed in reading FillColor.");
         SUMA_free(nelv);
         SUMA_RETURN(NULL);
      }
      if (SUMA_StringToNum (NI_get_attribute( nel , "EdgeColor"), 
                           (void*)nimlROI->EdgeColor, 3,1) < 0) {
         SUMA_SLP_Err("Failed in reading EdgeColor.");
         SUMA_free(nelv);
         SUMA_RETURN(NULL);
      }
      nimlROI->EdgeThickness = 
         (int)strtod(NI_get_attribute( nel , "EdgeThickness"), NULL);              
      
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: vec_type[0] = %d (%d)\n", 
            FuncName, nel->vec_typ[0], SUMAg_CF->nimlROI_Datum_type) ;
         fprintf (SUMA_STDERR,
            "%s: vec_len =%d\tvec_num = %d\n"
            "idcode_str %s, Parent_idcode_str %s\n",
            FuncName, nel->vec_len, nel->vec_num,
            nimlROI->idcode_str, nimlROI->Parent_idcode_str);
      }

      nimlROI->ROI_datum = 
         (SUMA_NIML_ROI_DATUM *)                   
               SUMA_calloc(nimlROI->N_ROI_datum,sizeof(SUMA_NIML_ROI_DATUM));

      /* DO NOT use niml_ROI_datum_buff = (SUMA_NIML_ROI_DATUM *)nel->vec[idat];
      inside the loop.
      For the SUMA_NIML_DRAWN_ROI you have one column of (SUMA_NIML_ROI_DATUM *)
      ni_type = "SUMA_NIML_ROI_DATUM". If you had for type:
      "SUMA_NIML_ROI_DATUM, int" then you'd have two columns with the second
      column being a vector of ints. The only caveat is that the second column
      must be of equal length to the first. */
      niml_ROI_datum_buff = (SUMA_NIML_ROI_DATUM *)nel->vec[0]; 
      /* now fill the ROI_datum structures */
      SUMA_LH("Filling ROI datum structures...");
      for (idat=0; idat< nimlROI->N_ROI_datum ; ++idat) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: i=%d\n", FuncName, idat);
         nimlROI->ROI_datum[idat].action = niml_ROI_datum_buff[idat].action;
         nimlROI->ROI_datum[idat].Type = niml_ROI_datum_buff[idat].Type;
         nimlROI->ROI_datum[idat].N_n = niml_ROI_datum_buff[idat].N_n;
         if (nimlROI->ROI_datum[idat].N_n > 0) {
            if (LocalHead) 
               fprintf (SUMA_STDERR,
                        "%s: Copying nPath, %d values\n", 
                        FuncName, nimlROI->ROI_datum[idat].N_n);
            nimlROI->ROI_datum[idat].nPath = 
               (int *)SUMA_malloc(sizeof(int)*nimlROI->ROI_datum[idat].N_n);
            memcpy(  nimlROI->ROI_datum[idat].nPath, 
                     niml_ROI_datum_buff[idat].nPath,    
                     sizeof(int)*nimlROI->ROI_datum[idat].N_n);
         } else {
            SUMA_LH("Null nPath");
            nimlROI->ROI_datum[idat].nPath = NULL;
         } 
         if (LocalHead) { 
            fprintf (SUMA_STDERR,"%s: Segment %d\tType %d\tN_n %d\taction %d\n", 
               FuncName, idat, nimlROI->ROI_datum[idat].Type,  
               nimlROI->ROI_datum[idat].N_n,nimlROI->ROI_datum[idat].action);
         }
      }

      /* Does ROI already exist with the same idcode_str ?*/
      
      SUMA_LH("Checking for duplicates...");
      if ((iDO = SUMA_whichDO(nel_idcode, SUMAg_DOv, SUMAg_N_DOv)) >= 0) {
         SUMA_LH("Duplicate found ... Deleteing old one...");
         /* ROI already exists delete it */
         if (!SUMA_DeleteROI ((SUMA_DRAWN_ROI *)SUMAg_DOv[iDO].OP)) {
            SUMA_SLP_Err("Failed to delete ROI");
            SUMA_RETURN(NULL); 
         }
      }
      
      /* transfom nimlROI to a series of drawing actions */
      SUMA_LH("Transforming ROI to a series of actions...");
      ROIv[inel] = SUMA_NIMLDrawnROI_to_DrawnROI (nimlROI, ForDisplay);
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: ROI->Parent_idcode_str %s\n", 
                  FuncName, ROIv[inel]->Parent_idcode_str);
      
      /* manually free nimlROI fields that received copies 
      of allocated space as opposed to pointer copies */
      if (nimlROI->idcode_str) SUMA_free(nimlROI->idcode_str);
      if (nimlROI->Parent_idcode_str) SUMA_free(nimlROI->Parent_idcode_str); 
      if (nimlROI->Label) SUMA_free(nimlROI->Label);
      if (nimlROI->ColPlaneName) SUMA_free(nimlROI->ColPlaneName);


      /* free nimlROI */
      nimlROI = SUMA_Free_NIMLDrawROI(nimlROI);

      /* free nel and get it ready for the next load */
      NI_free_element(nel) ; nel = NULL; 
            
   }
   
   /* free nelv */
   SUMA_free(nelv);
   
   *N_ROI = N_nel;
   SUMA_RETURN(ROIv);
} 

/*!
   \brief turns a bunch of ROIs into a NI dataset
            A new version of SUMA_ROIv2dataset that allows 
            the use of dsets as groups
   \param ROIv (SUMA_DRAWN_ROI**) vector of ROI structures
   \param N_ROIv (int) number of ROI structures
   \param Parent_idcode_str (char *) idcode of parent surface
   \param Pad_to (int)  create Dset that has a full node listing 
                        from node 0 to node Pad_to (a total of Pad_to + 1 nodes)
                        Use -1 to turn off padding.
   \param Pad_val (int) use this value (usually 0) to label a node being padded
                        as oppsed to a node being a part of an ROI. This option
                        is only useful with Pad_to
   \return nel (NI_element *) structure to data set
                              NULL if failed
*/
SUMA_DSET *SUMA_ROIv2Grpdataset (SUMA_DRAWN_ROI** ROIv, int N_ROIv, 
                                 char *Parent_idcode_str, 
                                 int Pad_to, int Pad_val) 
{
   static char FuncName[]={"SUMA_ROIv2Grpdataset"};
   int ii, i, nn, cnt, N_NodesTotal = 0, MaxIndex = 0,
      *ip=NULL, *NodesTotal=NULL, *LabelsTotal=NULL,
      *NodesTotal_p=NULL, *LabelsTotal_p=NULL;
   SUMA_DSET *dset =NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Now you have the ROIs, concatenate all NodesInROI vectors into 1*/
   /* count the total number of nodes */
   N_NodesTotal = 0;
   for (ii=0; ii < N_ROIv; ++ii) {
      SUMA_ROI_CRUDE_COUNT_NODES(ROIv[ii], cnt);
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: ROI #%d: %d nodes\n", FuncName, ii, cnt);
      }
      N_NodesTotal += cnt;
   }
   if (LocalHead) 
      fprintf (SUMA_STDERR,"%s: %d nodes total.\n", FuncName, N_NodesTotal);

   NodesTotal = (int *)SUMA_calloc(N_NodesTotal, sizeof(int));
   LabelsTotal = (int *)SUMA_calloc(N_NodesTotal, sizeof(int));

   if (!NodesTotal || !LabelsTotal) {
      SUMA_S_Err("Failed to allocate.");
      SUMA_RETURN(dset);
   }

   cnt = 0;
   N_NodesTotal = 0;
   MaxIndex = -1;
   for (ii=0; ii <  N_ROIv; ++ii) {
      SUMA_LH("Appending ROI");
      /* You do not need the Unique operation in SUMA_NodesInROI,
      but it is nice to have so that you can report the nodes that 
      were part of more than one ROI. If you do not Set the Unique 
      flag in SUMA_NodesInROI you will likely get node duplication
      but these nodes are in the same ROI and therefore are not
      duplicates to be removed....*/
      ip = SUMA_NodesInROI (ROIv[ii], &nn, YUP);
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Nodes in ROI #%d\n", FuncName, ii);
         SUMA_disp_dvect (ip, nn);
      }
      for (i=0; i < nn; ++i) {
         NodesTotal[cnt] = ip[i];
         LabelsTotal[cnt] = ROIv[ii]->iLabel;
         if (ip[i] > MaxIndex) MaxIndex = ip[i];
         ++cnt;
      }
      N_NodesTotal += nn;
      SUMA_freeDrawnROI (ROIv[ii]); ROIv[ii] = NULL; /* free the Drawn ROI */
      SUMA_free(ip);ip=NULL;
   }

   if (LocalHead) {
      SUMA_disp_dvect (NodesTotal, N_NodesTotal);
   }

   /* Now you want to make sure that no two nodes are listed twice */
   /* sort NodesTotal and rearrange LabelsTotal accordingly */
   {  int *isort = NULL, *LabelsTotal_r = NULL,
         *NodesTotal_u = NULL, N_NodesTotal_u, *iu = NULL;
      char report[200];

      isort = SUMA_z_dqsort(NodesTotal, N_NodesTotal);
      LabelsTotal_r = SUMA_reorder (LabelsTotal, isort, N_NodesTotal);
      SUMA_free(LabelsTotal);
      LabelsTotal = LabelsTotal_r; LabelsTotal_r = NULL;
      SUMA_free(isort); isort = NULL;

      /* now get the unique set of nodes */
      NodesTotal_u = SUMA_UniqueInt_ind (NodesTotal, N_NodesTotal, 
                                        &N_NodesTotal_u, &iu);
      /* reorder LabelsTotal to contain data from the nodes 
         left in NodesTotal_u */
      LabelsTotal_r = SUMA_reorder (LabelsTotal, iu, N_NodesTotal_u);
      SUMA_free(NodesTotal); NodesTotal = NULL;
      SUMA_free(LabelsTotal); LabelsTotal = NULL;
      SUMA_free(iu); iu = NULL;
      NodesTotal = NodesTotal_u; NodesTotal_u = NULL;
      LabelsTotal = LabelsTotal_r; LabelsTotal_r = NULL;

      if (N_NodesTotal - N_NodesTotal_u) {
         snprintf(report, 199,
                         "%d/%d nodes had duplicate entries.\n"
                         "(ie same node part of more than 1 ROI)\n"
                         "Duplicate entries were eliminated.", 
                         N_NodesTotal - N_NodesTotal_u , N_NodesTotal);

         N_NodesTotal = N_NodesTotal_u; N_NodesTotal_u = 0;
         SUMA_SLP_Warn(report);
      }
   }
   
   if (Pad_to > 0) {
      SUMA_LH("Padding to desired length");
      if (Pad_to < MaxIndex) {
         SUMA_SL_Err("ROI contains node index > padding limit\n"
                     "No padding done.");
         if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
         if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
         SUMA_RETURN(NULL);
      }else {
         NodesTotal_p =  (int *)SUMA_calloc(Pad_to+1, sizeof(int));
         LabelsTotal_p = (int *)SUMA_calloc(Pad_to+1, sizeof(int));
         if (!NodesTotal_p || !LabelsTotal_p) {
            SUMA_SL_Crit("Failed to allocate for NodesTotal_p || "
                         "LabelsTotal_p");
            if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
            if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
            SUMA_RETURN(NULL);
         }
         if (Pad_val)  for(i=0; i<=Pad_to; ++i) LabelsTotal_p[i] = Pad_val;
         for(i=0; i<=Pad_to; ++i) NodesTotal_p[i] = i;
         for(i=0; i<N_NodesTotal; ++i) {
            LabelsTotal_p[NodesTotal[i]] = LabelsTotal[i];
         }
         SUMA_free(NodesTotal); 
            NodesTotal = NodesTotal_p; NodesTotal_p = NULL;
         SUMA_free(LabelsTotal);  
            LabelsTotal = LabelsTotal_p; LabelsTotal_p = NULL;
         N_NodesTotal = Pad_to + 1;
      }
   }
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: N_NodesTotal = %d\nCreating dset\n", 
               FuncName, N_NodesTotal);
   }
   /* construct a NIML data set for the output */
   dset = SUMA_CreateDsetPointer(
         NULL,         /* usually the filename */
         SUMA_NODE_ROI,                /* mix and match */
         NULL,    /* no idcode, let the function create one from the filename*/
         Parent_idcode_str,       /* no domain str specified */
         N_NodesTotal    /* Number of nodes allocated for */
         ); /* DO NOT free dset, it is store in DsetList */


   
   if (!dset) {
      SUMA_SL_Err("Failed in SUMA_CreateDsetPointer");
      SUMA_RETURN(NULL);
   }

   /* Add the index column */
   SUMA_LH("Adding index column...");
   if (!SUMA_AddDsetNelCol (  dset, "node index", 
                              SUMA_NODE_INDEX, (void *)NodesTotal, NULL, 1)) {
      SUMA_SL_Err("Failed in SUMA_AddNelCol");
      SUMA_RETURN(dset);
   }

   /* Add the label column */
   SUMA_LH("Adding label column...");
   if (!SUMA_AddDsetNelCol (dset, "integer label", 
                            SUMA_NODE_ILABEL, (void *)LabelsTotal, NULL, 1)) {
      SUMA_SL_Err("Failed in SUMA_AddNelCol");
      SUMA_RETURN(dset);
   }
   
   /* make it easy */
   dset->dnel = SUMA_FindDsetDataElement(dset);
   dset->inel = SUMA_FindDsetNodeIndexElement(dset);
   
   SUMA_LH("cleanup ...");
   if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
   if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
   
   SUMA_RETURN(dset);
}
/*!
   \brief turns a bunch of ROIs into a list of node vectors
            Each list is for a particular ROI. 
            The ordering of the node indices is preserved as
            it appears in the ROI
   \param ROIv (SUMA_DRAWN_ROI**) vector of ROI structures
   \param N_ROIv (int) number of ROI structures
   \return nl (DList *) linked list of node indices for each ROI
                              NULL if failed
*/

void SUMA_free_ROI_Extract(void *dd)
{
   SUMA_ROI_EXTRACT *ddd=(SUMA_ROI_EXTRACT *)dd;
   if (ddd) {
      if (ddd->vals) SUMA_free(ddd->vals); 
      SUMA_free(ddd);
   }
   return;
}

SUMA_ROI_EXTRACT *SUMA_GetROIExtractLabeled(DList *ddl, int i)
{
   static char FuncName[]={"SUMA_GetROIExtractLabeled"};
   DListElmt *el=NULL;
   SUMA_ROI_EXTRACT *dd=NULL;
   
   SUMA_ENTRY;
   
   if (!ddl) SUMA_RETURN(NULL);
   
   el=dlist_head(ddl);
   while (el) {
      dd = (SUMA_ROI_EXTRACT *)el->data;
      if (dd->label == i) SUMA_RETURN(dd);
      el = dlist_next(el);
   }
   
   SUMA_RETURN(NULL);
}

DList *SUMA_ROIv2NodeLists (SUMA_DRAWN_ROI** ROIv, int N_ROIv, int purgedups) 
{
   static char FuncName[]={"SUMA_ROIv2NodeLists"};
   int ii, i, nn, cnt, nodemin=9999999, nodemax=-1, MaxNodeIndex=-1,
      N_NodesMax=0;
   byte *visited=NULL;
   SUMA_DSET *dset =NULL;
   DList *ddl=NULL;
   DListElmt *Elm=NULL, *eldd=NULL;
   SUMA_ROI_EXTRACT *dd=NULL;
   SUMA_ROI_DATUM *ROI_Datum=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Now you have the ROIs, concatenate all NodesInROI vectors into 1*/
   /* count the total number of nodes */
   ddl = (DList *)SUMA_calloc(1, sizeof(DList));
   dlist_init(ddl, SUMA_free_ROI_Extract);
   N_NodesMax = 0; nodemin=100000; nodemax=0, MaxNodeIndex=0;
   for (ii=0; ii < N_ROIv; ++ii) {
      if ((cnt=SUMA_NodeRange_DrawnROI (ROIv[ii], &nodemin, &nodemax)) >= 0) {
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s: ROI #%d: %d nodes,%d/%d min/max nodes\n", 
                     FuncName, ii, cnt, nodemin, nodemax);
         }
         /* is this a new label ? */
         dd = SUMA_GetROIExtractLabeled(ddl, ROIv[ii]->iLabel);
         if (!dd) {
            dd = (SUMA_ROI_EXTRACT*)SUMA_calloc(1,sizeof(SUMA_ROI_EXTRACT));
            dd->label = ROIv[ii]->iLabel;
            dd->N_alloc = cnt;
            dd->vals = (int *)SUMA_calloc(dd->N_alloc, sizeof(int));
            dd->N_vals = 0;
            dlist_ins_next(ddl,dlist_tail(ddl),dd);
         } else { /* This allows for multiple ROI of the same label 
                     to be combined */
            dd->N_alloc += cnt;
            dd->vals = (int *)SUMA_realloc(dd->vals, dd->N_alloc*sizeof(int));
         }
         if (MaxNodeIndex < nodemax) MaxNodeIndex = nodemax;
      } else {
         SUMA_S_Err( "Cannont handle failure in NodeRange function\n"
                     "Must have as manu elements in ddl as in ROIv");
         SUMA_RETURN(NULL);
      }
   }
   

   /* allocate a vector to keep track of visited nodes */
   SUMA_LHv("Ready to fill up, MaxNodeIndex %d\n", MaxNodeIndex);
   {
      if (purgedups) 
         visited = (byte *)SUMA_malloc(sizeof(byte)*(MaxNodeIndex+1));
      else visited = NULL;
      #if 0
      eldd = dlist_head(ddl);
      for (ii=0; ii < N_ROIv; ++ii) {
         SUMA_LHv("Working %d/%d\n", ii, N_ROIv);
         dd = (SUMA_ROI_EXTRACT *)eldd->data;
         if (visited) memset((void*)visited, 0, sizeof(byte)*(MaxNodeIndex+1));
         Elm = dlist_head(ROIv[ii]->ROIstrokelist);
         while (Elm && Elm->data) {
            ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
            SUMA_LHv("Will check on %d nodes\n", ROI_Datum->N_n);
            for (i=0; i < ROI_Datum->N_n; ++i) {
               if (!visited || !visited[ROI_Datum->nPath[i]]) {
                  dd->vals[dd->N_vals] = ROI_Datum->nPath[i];
                  if (visited) visited[ROI_Datum->nPath[i]]=1;
                  ++dd->N_vals;
               }
            }
            Elm = dlist_next(Elm);
         }
         eldd = dlist_next(eldd);   
      }
      #else
      eldd = dlist_head(ddl);
      while(eldd) {
         dd = (SUMA_ROI_EXTRACT *)eldd->data;
         if (visited) memset((void*)visited, 0, sizeof(byte)*(MaxNodeIndex+1));
         for (ii=0; ii < N_ROIv; ++ii) {
            if (ROIv[ii]->iLabel == dd->label) {
               Elm = dlist_head(ROIv[ii]->ROIstrokelist);
               while (Elm && Elm->data) {
                  ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
                  SUMA_LHv("Will check on %d nodes, label %d, (%d)\n", 
                           ROI_Datum->N_n, dd->label, ROIv[ii]->iLabel);
                  for (i=0; i < ROI_Datum->N_n; ++i) {
                     if (!visited || !visited[ROI_Datum->nPath[i]]) {
                        dd->vals[dd->N_vals] = ROI_Datum->nPath[i];
                        if (visited) visited[ROI_Datum->nPath[i]]=1;
                        ++dd->N_vals;
                     }
                  }
                  Elm = dlist_next(Elm);
               }
            } /* add ROI's of this label */
         } /* ii */
         eldd = dlist_next(eldd);
      } 
      #endif   
      
   } 
   
   if (visited) SUMA_free(visited); visited = NULL;
   
   
   SUMA_RETURN(ddl);
}
   
/*!
   \brief turns a bunch of ROIs into a NI dataset
   
   \param ROIv (SUMA_DRAWN_ROI**) vector of ROI structures
   \param N_ROIv (int) number of ROI structures
   \param Parent_idcode_str (char *) idcode of parent surface
   \param Pad_to (int)  create Dset that has a full node listing 
                        from node 0 to node Pad_to (a total of Pad_to + 1 nodes)
                        Use -1 to turn off padding.
   \param Pad_val (int) use this value (usually 0) to label a node being padded
                        as oppsed to a node being a part of an ROI. This option
                        is only useful with Pad_to
   \return nel (NI_element *) structure to data set
                              NULL if failed
*/
NI_element *SUMA_ROIv2dataset (SUMA_DRAWN_ROI** ROIv, int N_ROIv, char *Parent_idcode_str, int Pad_to, int Pad_val) 
{
   static char FuncName[]={"SUMA_ROIv2dataset"};
   int ii, i, nn, cnt, N_NodesTotal = 0, MaxIndex = 0,
      *ip=NULL, *NodesTotal=NULL, *LabelsTotal=NULL,
      *NodesTotal_p=NULL, *LabelsTotal_p=NULL;
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_SL_Err("Obsolete, use SUMA_ROIv2Grpdataset");
   SUMA_RETURN(NULL);
   
   /* Now you have the ROIs, concatenate all NodesInROI vectors into 1*/
   /* count the total number of nodes */
   N_NodesTotal = 0;
   for (ii=0; ii < N_ROIv; ++ii) {
      SUMA_ROI_CRUDE_COUNT_NODES(ROIv[ii], cnt);
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: ROI #%d: %d nodes\n", FuncName, ii, cnt);
      }
      N_NodesTotal += cnt;
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: %d nodes total.\n", FuncName, N_NodesTotal);

   NodesTotal = (int *)SUMA_calloc(N_NodesTotal, sizeof(int));
   LabelsTotal = (int *)SUMA_calloc(N_NodesTotal, sizeof(int));

   if (!NodesTotal || !LabelsTotal) {
      SUMA_S_Err("Failed to allocate.");
      SUMA_RETURN(nel);
   }

   cnt = 0;
   N_NodesTotal = 0;
   MaxIndex = -1;
   for (ii=0; ii <  N_ROIv; ++ii) {
      SUMA_LH("Appending ROI");
      /* You do not need the Unique operation in SUMA_NodesInROI,
      but it is nice to have so that you can report the nodes that 
      were part of more than one ROI. If you do not Set the Unique 
      flag in SUMA_NodesInROI you will likely get node duplication
      but these nodes are in the same ROI and therefore are not
      duplicates to be removed....*/
      ip = SUMA_NodesInROI (ROIv[ii], &nn, YUP);
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Nodes in ROI #%d\n", FuncName, ii);
         SUMA_disp_dvect (ip, nn);
      }
      for (i=0; i < nn; ++i) {
         NodesTotal[cnt] = ip[i];
         LabelsTotal[cnt] = ROIv[ii]->iLabel;
         if (ip[i] > MaxIndex) MaxIndex = ip[i];
         ++cnt;
      }
      N_NodesTotal += nn;
      SUMA_freeDrawnROI (ROIv[ii]); ROIv[ii] = NULL; /* free the Drawn ROI */
      SUMA_free(ip);ip=NULL;
   }

   if (LocalHead) {
      SUMA_disp_dvect (NodesTotal, N_NodesTotal);
   }

   /* Now you want to make sure that no two nodes are listed twice */
   /* sort NodesTotal and rearrange LabelsTotal accordingly */
   {  int *isort = NULL, *LabelsTotal_r = NULL,
         *NodesTotal_u = NULL, N_NodesTotal_u, *iu = NULL;
      char report[100];

      isort = SUMA_z_dqsort(NodesTotal, N_NodesTotal);
      LabelsTotal_r = SUMA_reorder (LabelsTotal, isort, N_NodesTotal);
      SUMA_free(LabelsTotal);
      LabelsTotal = LabelsTotal_r; LabelsTotal_r = NULL;
      SUMA_free(isort); isort = NULL;

      /* now get the unique set of nodes */
      NodesTotal_u = SUMA_UniqueInt_ind (NodesTotal, N_NodesTotal, &N_NodesTotal_u, &iu);
      /* reorder LabelsTotal to contain data from the nodes left in NodesTotal_u */
      LabelsTotal_r = SUMA_reorder (LabelsTotal, iu, N_NodesTotal_u);
      SUMA_free(NodesTotal); NodesTotal = NULL;
      SUMA_free(LabelsTotal); LabelsTotal = NULL;
      SUMA_free(iu); iu = NULL;
      NodesTotal = NodesTotal_u; NodesTotal_u = NULL;
      LabelsTotal = LabelsTotal_r; LabelsTotal_r = NULL;

      if (N_NodesTotal - N_NodesTotal_u) {
         sprintf(report, "%d/%d nodes had duplicate entries.\n"
                         "(ie same node part of more than 1 ROI)\n"
                         "Duplicate entries were eliminated.", 
                         N_NodesTotal - N_NodesTotal_u , N_NodesTotal);

         N_NodesTotal = N_NodesTotal_u; N_NodesTotal_u = 0;
         SUMA_SLP_Warn(report);
      }
   }

   if (Pad_to > 0) {
      SUMA_LH("Padding to desired length");
      if (Pad_to < MaxIndex) {
         SUMA_SL_Err("ROI contains node index > padding limit\nNo padding done.");
         if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
         if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
         SUMA_RETURN(NULL);
      }else {
         NodesTotal_p =  (int *)SUMA_calloc(Pad_to+1, sizeof(int));
         LabelsTotal_p = (int *)SUMA_calloc(Pad_to+1, sizeof(int));
         if (!NodesTotal_p || !LabelsTotal_p) {
            SUMA_SL_Crit("Failed to allocate for NodesTotal_p || LabelsTotal_p");
            if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
            if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
            SUMA_RETURN(NULL);
         }
         if (Pad_val)  for(i=0; i<=Pad_to; ++i) LabelsTotal_p[i] = Pad_val;
         for(i=0; i<=Pad_to; ++i) NodesTotal_p[i] = i;
         for(i=0; i<N_NodesTotal; ++i) {
            LabelsTotal_p[NodesTotal[i]] = LabelsTotal[i];
         }
         SUMA_free(NodesTotal); NodesTotal = NodesTotal_p; NodesTotal_p = NULL;
         SUMA_free(LabelsTotal);  LabelsTotal = LabelsTotal_p; LabelsTotal_p = NULL;
         N_NodesTotal = Pad_to + 1;
      }
   }
   
   /* construct a NIML data set for the output */
   SUMA_LH("Creating nel ");
   nel = SUMA_NewNel ( SUMA_NODE_ROI, /* one of SUMA_DSET_TYPE */
                       Parent_idcode_str, /* idcode of Domain Parent */
                       NULL, /* idcode of geometry parent, not useful here*/
                       N_NodesTotal,/* Number of elements */
                       NULL,
                       NULL); 

   if (!nel) {
      SUMA_SL_Err("Failed in SUMA_NewNel");
      SUMA_RETURN(nel);
   }

   /* Add the index column */
   SUMA_LH("Adding index column...");
   if (!SUMA_AddNelCol (nel, "node index", SUMA_NODE_INDEX, (void *)NodesTotal, NULL, 1)) {
      SUMA_SL_Err("Failed in SUMA_AddNelCol");
      SUMA_RETURN(nel);
   }

   /* Add the label column */
   SUMA_LH("Adding label column...");
   if (!SUMA_AddNelCol (nel, "integer label", SUMA_NODE_ILABEL, (void *)LabelsTotal, NULL, 1)) {
      SUMA_SL_Err("Failed in SUMA_AddNelCol");
      SUMA_RETURN(nel);
   }
   
   SUMA_LH("cleanup ...");
   if (NodesTotal) SUMA_free(NodesTotal); NodesTotal = NULL;
   if (LabelsTotal) SUMA_free(LabelsTotal); LabelsTotal = NULL;
   
   SUMA_RETURN(nel);
}
   

/*!
   \brief handles savinf SO to ascii filename
   
   \param filename (char *)
   \param data(void *) pointer to SUMA_SAVESO_STRUCT containing sv and SO be saved
   
   - This function frees the SUMA_SAVESO_STRUCT before returning
*/
void SUMA_SaveSOascii (char *filename, void *data)
{
   static char FuncName[]={"SUMA_SaveSOascii"};
   char *newname = NULL, *newprefix = NULL, *tmp1= NULL, *tmp2= NULL;
   FILE *Fout = NULL;
   static int answer;
   int ND=-1, NP=-1, ii=-1, id=-1,ip=-1; 
   GLfloat *glar_ColorList = NULL;
   SUMA_SAVESO_STRUCT *SaveSO_data = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");   
      
   if (!data) {
      SUMA_SLP_Err("NULL data");
      SUMA_RETURNe;
   }
   
   SaveSO_data = (SUMA_SAVESO_STRUCT *)data;
   if (!SaveSO_data->SO || !SaveSO_data->sv) {
      SUMA_SLP_Err("Null SO or Null sv");
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
   
   /* remove any of the extensions to be used */
   tmp1 = SUMA_Extension(filename, ".1D.xyz", YUP);
   tmp2 = SUMA_Extension(tmp1, ".1D.tri", YUP);
   newprefix = SUMA_Extension(tmp2, ".1D.col", YUP);
   if (tmp1) SUMA_free(tmp1); tmp1 = NULL;
   if (tmp2) SUMA_free(tmp2); tmp2 = NULL;
   
   /* add a .xyz extension */
   if (newname) SUMA_free(newname); newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.xyz", NOPE); 
   if (!newname) {
      SUMA_SL_Err("Invalid filename");
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
   SUMA_LH(newname);
   /* check for filename existence */
   if (SUMA_filexists (newname)) {
      answer = SUMA_ForceUser_YesNo (SUMAg_SVv[0].X->TOPLEVEL, 
                                    "Prefix exists, overwrite?", 
                                    SUMA_NO, SWP_DONT_CARE);
      if (answer == SUMA_NO ||answer == SUMA_NO_ALL) {
         if (newname) SUMA_free(newname); newname = NULL;
         if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
         SUMA_RETURNe; 
      }
   } 

   /* add a .tri extension */
   if (answer != SUMA_YES_ALL && answer != SUMA_YES) {
      if (newname) SUMA_free(newname);newname = NULL;
      newname = SUMA_Extension(newprefix, ".1D.tri", NOPE); 
      if (!newname) {
         SUMA_SL_Err("Invalid filename");
         if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
         SUMA_RETURNe;
      }
      SUMA_LH(newname);
      /* check for filename existence */
      if (SUMA_filexists (newname)) {
         answer = SUMA_ForceUser_YesNo (SUMAg_SVv[0].X->TOPLEVEL, 
                                       "Prefix exists, overwrite?", 
                                       SUMA_NO, SWP_DONT_CARE);
         if (answer == SUMA_NO ||answer == SUMA_NO_ALL) {
            if (newname) SUMA_free(newname);newname = NULL;
            if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
            SUMA_RETURNe; 
         }
      } 
   }
   
   /* add a .col extension */
   if (answer != SUMA_YES_ALL  && answer != SUMA_YES) {
      if (newname) SUMA_free(newname); newname = NULL;
      newname = SUMA_Extension(newprefix, ".1D.col", NOPE); 
      if (!newname) {
         SUMA_SL_Err("Invalid filename");
         if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
         SUMA_RETURNe;
      }
      SUMA_LH(newname);
      /* check for filename existence */
      if (SUMA_filexists (newname)) {
         answer = SUMA_ForceUser_YesNo (SUMAg_SVv[0].X->TOPLEVEL, 
                                       "Prefix exists, overwrite?", 
                                       SUMA_NO, SWP_DONT_CARE);
         if (answer == SUMA_NO ||answer == SUMA_NO_ALL) {
            if (newname) SUMA_free(newname);newname = NULL;
            if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
            SUMA_RETURNe; 
         }
      } 
   }
   
   /* OK, names are acceptable, proceed */
   ND = SaveSO_data->SO->NodeDim;
   NP = SaveSO_data->SO->FaceSetDim;

   if (newname) SUMA_free(newname);newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.xyz", NOPE);  
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write .1D.xyz %s.\n", FuncName, newname); 
   Fout = fopen(newname, "w");
   if (Fout == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, newname);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }

   fprintf(Fout, "#FileContents = Node coordinates\n#RowFormat = X Y Z\n#N_Nodes = %d\n#Source = SUMA, surface %s (idcode: %s)\n",
          SaveSO_data->SO->N_Node, SaveSO_data->SO->Label, SaveSO_data->SO->idcode_str);
   for (ii=0; ii < SaveSO_data->SO->N_Node; ++ii) {
      id = ND * ii;
      fprintf(Fout, "%f\t%f\t%f\n", \
         SaveSO_data->SO->NodeList[id], SaveSO_data->SO->NodeList[id+1],SaveSO_data->SO->NodeList[id+2]);
   }
   fclose (Fout);

   if (newname) SUMA_free(newname);newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.tri", NOPE);  
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write .1D.tri %s.\n", FuncName, newname); 
   Fout = fopen(newname, "w");
   if (Fout == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, newname);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
   
   fprintf(Fout, "#FileContents = Triangles\n#RowFormat = n1 n2 n3\n#N_Tri = %d\n#Source = SUMA, surface %s (idcode: %s)\n",
          SaveSO_data->SO->N_FaceSet, SaveSO_data->SO->Label, SaveSO_data->SO->idcode_str);
   for (ii=0; ii < SaveSO_data->SO->N_FaceSet; ++ii) {
      ip = NP * ii;
      fprintf(Fout, "%d\t%d\t%d\n", \
         SaveSO_data->SO->FaceSetList[ip], SaveSO_data->SO->FaceSetList[ip+1],SaveSO_data->SO->FaceSetList[ip+2]);
   }
   fclose (Fout);

   if (newname) SUMA_free(newname);newname = NULL;
   newname = SUMA_Extension(newprefix, ".1D.col", NOPE);  
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write .1D.col %s.\n", FuncName, newname); 
   Fout = fopen(newname, "w");
   if (Fout == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, newname);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
   }
    glar_ColorList = SUMA_GetColorList (SaveSO_data->sv, SaveSO_data->SO->idcode_str);
    if (!glar_ColorList) {
      fprintf(SUMA_STDERR, "Error %s: NULL glar_ColorList. BAD.\n", FuncName);
      if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
      SUMA_RETURNe;
    }
   fprintf(Fout, "#FileContents = Node Colors\n#RowFormat = n R G B\n#N_Nodes = %d\n#Source = SUMA, surface %s (idcode: %s)\n",
          SaveSO_data->SO->N_Node, SaveSO_data->SO->Label, SaveSO_data->SO->idcode_str);
   for (ii=0; ii < SaveSO_data->SO->N_Node; ++ii) {
      ip = 4 * ii;
      fprintf(Fout, "%d\t%f\t%f\t%f\n", \
         ii, glar_ColorList[ip], glar_ColorList[ip+1], glar_ColorList[ip+2]);
   }
   fclose (Fout);

   if (LocalHead) fprintf(SUMA_STDERR, "%s: Wrote files to disk.\n", 
      FuncName);

   if (newname) SUMA_free(newname);newname = NULL;
   if (SaveSO_data) SUMA_free(SaveSO_data); SaveSO_data = NULL;
   if (newprefix) SUMA_free(newprefix); 
   SUMA_RETURNe;
}
/*!
   \brief handles saving xform opts to filename.
   
   \param filename (char *)
   \param data(void *) pointer to SUMA_XFORM stucture to be saved.
*/
void SUMA_SaveXformOpts (char *filename, void *data)
{
   static char FuncName[]={"SUMA_SaveXformOpts"};
   SUMA_XFORM *xf=(SUMA_XFORM *)data;
   char *fn=NULL;
   int suc;
   NI_element *dotopts=NULL; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");   
   if (!data) {
      SUMA_S_Err("NULL input");
      SUMA_RETURNe;
   }
   
   SUMA_LHv("Xform %s, filename %s\n", xf->name, filename);
    
   if (!strcmp(xf->name,"Dot")) {
      if (!(dotopts = SUMA_FindNgrNamedElement(xf->XformOpts, "dotopts"))) {
         SUMA_S_Err("No dotopts");
         SUMA_RETURNe;
      }  
      fn = SUMA_Extension(filename, ".niml.xfopts", NOPE);
      fn = SUMA_append_replace_string("file:",fn,"",2);
      NEL_WRITE_1D(dotopts, fn, suc);
      SUMA_free(fn);
   } else {
      fn = SUMA_Extension(filename, ".niml.xfopts", NOPE);
      fn = SUMA_append_replace_string("file:",fn,"",2);
      NEL_WRITE_TXH(xf->XformOpts, fn, suc);
   }
   
   SUMA_RETURNe;
} 

/*!
   \brief handles saving ROI to filename.
   
   \param filename (char *)
   \param data(void *) pointer to DrawnROI stucture to be saved.
          If you pass null then SUMAg_CF->X->DrawROI->curDrawnROI is used.
*/
void SUMA_SaveDrawnROI (char *filename, void *data)
{
   static char FuncName[]={"SUMA_SaveDrawnROI"};
   SUMA_DRAWN_ROI *DrawnROI=NULL;
   SUMA_SurfaceObject *SO= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_LH("Called");   
   if (!data) {
      DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
   } else {
      DrawnROI = (SUMA_DRAWN_ROI *)data;
   }
   
   /* is there a DrawnROI to work with ? */
   if (!DrawnROI) {
      SUMA_SLP_Err("No ROI selected.");
      SUMA_RETURNe;
   }
   
   /* Find the parent SO of that ROI */
   SO = SUMA_findSOp_inDOv(DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   if (!SO) {
      SUMA_SLP_Err("No Parent surface found.");
      SUMA_RETURNe;
   }
   
   /* switch the type of saving */
   switch (SUMAg_CF->X->DrawROI->SaveMode) {
      case SW_DrawROI_SaveMode1D:
         if (!SUMA_SaveDrawnROI_1D (filename, SO, DrawnROI, SUMAg_CF->X->DrawROI->SaveWhat)) {
            SUMA_SLP_Err("Failed to save ROI to disk");
            SUMA_RETURNe;
         }   
         break;
      case SW_DrawROI_SaveModeNIML:
         if (!SUMA_SaveDrawnROINIML (filename, SO, DrawnROI, SUMAg_CF->X->DrawROI->SaveWhat, SUMA_ASCII)) {
            SUMA_SLP_Err("Failed to save ROI to disk");
            SUMA_RETURNe;
         }
         break;
      case SW_DrawROI_SaveMode:
      case SW_N_DrawROI_SaveMode:
      default:
         SUMA_SL_Err("WhatYouTalkinAbout?");
         SUMA_RETURNe;
         break;
   }
   
   SUMA_RETURNe;
} 

SUMA_Boolean SUMA_SaveDrawnROI_1D (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat) 
{
   static char FuncName[]={"SUMA_SaveDrawnROI_1D"};
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   SUMA_DRAWN_ROI **ROIv = NULL;
   int N_ROI=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LH("Called");   

   if (SaveWhat == SW_DrawROI_SaveWhatThis) {
      if (!SUMA_Write_DrawnROI_1D (&DrawnROI, 1, filename)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
   }else if (SaveWhat == SW_DrawROI_SaveWhatRelated){
      /* get the pointers to the ROIs that are related to SO*/
      if (!(ROIv = SUMA_Find_ROIrelatedtoSO (SO, SUMAg_DOv, SUMAg_N_DOv, &N_ROI))) {
         SUMA_SLP_Err("Failed to write ROIs related to SO.");
         SUMA_RETURN(NOPE);
      }
      if (!SUMA_Write_DrawnROI_1D (ROIv, N_ROI, filename)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
        
      if (ROIv) SUMA_free(ROIv);    
   } else {
      SUMA_SLP_Err("SaveWhat option not nderstood");
      SUMA_RETURN(NOPE);
   }
   
   

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_SaveDrawnROINIML (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat, int Format) 
{
   static char FuncName[]={"SaveDrawnROINIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   SUMA_DRAWN_ROI **ROIv = NULL;
   int N_ROI=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   SUMA_LH("Called");   

   if (SaveWhat == SW_DrawROI_SaveWhatThis) {
      if (!SUMA_Write_DrawnROI_NIML (&DrawnROI, 1, filename, Format)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
   }else if (SaveWhat == SW_DrawROI_SaveWhatRelated){
      /* get the pointers to the ROIs that are related to SO*/
      if (!(ROIv = SUMA_Find_ROIrelatedtoSO (SO, SUMAg_DOv, SUMAg_N_DOv, &N_ROI))) {
         SUMA_SLP_Err("Failed to write ROIs related to SO.");
         SUMA_RETURN(NOPE);
      }
      if (!SUMA_Write_DrawnROI_NIML (ROIv, N_ROI, filename, Format)) {
         sprintf(stmp,"Failed to write %s", filename);
         SUMA_SLP_Err(stmp);
         SUMA_RETURN(NOPE);
      }
        
      if (ROIv) SUMA_free(ROIv);    
   } else {
      SUMA_SLP_Err("SaveWhat option not nderstood");
      SUMA_RETURN(NOPE);
   }
  
   SUMA_RETURN(YUP);
}

/*!
   \brief writes a vector of SUMA_DRAWN_ROI * to disk in NIML format
*/

SUMA_Boolean SUMA_Write_DrawnROI_NIML (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename, int Format) 
{
   static char FuncName[]={"SUMA_Write_DrawnROI_NIML"};
   char stmp[SUMA_MAX_NAME_LENGTH+20];
   char *newname=NULL;
   int i;
   NI_element *nel ;
   NI_stream ns ;
   SUMA_NIML_DRAWN_ROI *niml_ROI = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   SUMA_Boolean WriteBin = NOPE, LocalHead = NOPE;

   SUMA_ENTRY;

   if (Format == SUMA_ASCII) WriteBin = NOPE;
   else if (Format == SUMA_BINARY) WriteBin = YUP;
   else {
      SUMA_SL_Err("Wrong format");
      SUMA_RETURN(NOPE);
   }
   
   if (SUMAg_CF->nimlROI_Datum_type < 0) {
      SUMA_SL_Err("Bad niml type code");
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) fprintf(SUMA_STDERR, "%s: roi_type code = %d\n", FuncName, SUMAg_CF->nimlROI_Datum_type) ;

   /* add a .niml.roi extension */
   if (strlen(filename) >= SUMA_MAX_NAME_LENGTH-20) {
      SUMA_SLP_Err("Give me a break, what kind of a filename is this ?");
      SUMA_RETURN(NOPE);
   }

   sprintf(stmp,"file:%s", filename);
   newname = SUMA_Extension(stmp, ".niml.roi", NOPE); 
   SUMA_LH(newname);
   ns = NI_stream_open( newname , "w" ) ;

   /* write the various ROIs */
   for (i=0; i < N_ROI; ++i) {
      ROI = ROIv[i];
      if (!ROI) {
         SUMA_SL_Err("NULL ROI!");
         NI_stream_close( ns ) ; 
         SUMA_RETURN(NOPE);
      }
      /* Transform the ROI to niml friendly structure */
      if (!(niml_ROI = SUMA_DrawnROI_to_NIMLDrawnROI (ROI))) {
         SUMA_SL_Err("NULL niml_ROI!");
         NI_stream_close( ns ) ; 
         SUMA_RETURN(NOPE);
      }
 
      /* Now create a ni element */
      if (LocalHead) 
         fprintf( SUMA_STDERR,
                  "%s: Creating new element of %d segments\n", 
                  FuncName, niml_ROI->N_ROI_datum);
      nel = NI_new_data_element( SUMA_Dset_Type_Name(SUMA_NODE_ROI),  
                                 niml_ROI->N_ROI_datum);

      SUMA_LH("Adding column...");
      NI_add_column( nel , SUMAg_CF->nimlROI_Datum_type, niml_ROI->ROI_datum );

      SUMA_LH("Setting attributes...");
      NI_set_attribute (nel, "self_idcode", niml_ROI->idcode_str);
      NI_set_attribute (nel, "domain_parent_idcode", 
                              niml_ROI->Parent_idcode_str);
      NI_set_attribute (nel, "Label", niml_ROI->Label);
      sprintf(stmp,"%d", niml_ROI->iLabel);
      NI_set_attribute (nel, "iLabel", stmp);
      sprintf(stmp,"%d", niml_ROI->Type);
      NI_set_attribute (nel, "Type", stmp);
      NI_set_attribute (nel, "ColPlaneName", niml_ROI->ColPlaneName);
      sprintf(stmp,"%f %f %f", niml_ROI->FillColor[0], niml_ROI->FillColor[1],
                              niml_ROI->FillColor[2]);
      NI_set_attribute (nel, "FillColor",stmp);
      sprintf(stmp,"%f %f %f", niml_ROI->EdgeColor[0], niml_ROI->EdgeColor[1],
                              niml_ROI->EdgeColor[2]);
      NI_set_attribute (nel, "EdgeColor",stmp);
      sprintf(stmp,"%d", niml_ROI->EdgeThickness);
      NI_set_attribute (nel, "EdgeThickness", stmp);                   
      
      if (LocalHead) SUMA_nel_stdout (nel);

      if (!WriteBin) {
         SUMA_LH ("Writing element, Text mode.");
         if (NI_write_element(   ns , nel , 
                                 NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
           SUMA_SL_Err("Badness, failed to write nel");
           NI_stream_close( ns ) ; 
           SUMA_RETURN(NOPE);
         }
      } else {
         SUMA_LH ("Writing element, Binary mode.");
         if (NI_write_element( ns , nel , NI_BINARY_MODE) < 0) {
           SUMA_SL_Err("Badness, failed to write nel");
           NI_stream_close( ns ) ; 
           SUMA_RETURN(NOPE);
         }
      } 

      /* free nel */
      NI_free_element(nel) ; nel = NULL;

      /* free the niml_ROI structure */
      niml_ROI = SUMA_Free_NIMLDrawROI (niml_ROI); niml_ROI = NULL;
      
   }
   
   NI_stream_close( ns ) ; 
   
   if (newname) SUMA_free(newname);
   
   SUMA_RETURN(YUP);
}


/*!
   \brief A function to take a SUMA_DRAWN_ROI struct and return an equivalent
   SUMA_1D_DRAWN_ROI struct. 
   
   - Do not free SUMA_1D_DRAWN_ROI manually, many of its fields are 
   pointer copies of values in SUMA_DRAWN_ROI.
   
   \sa SUMA_Free_1DDrawROI
*/
SUMA_1D_DRAWN_ROI * SUMA_DrawnROI_to_1DDrawROI (SUMA_DRAWN_ROI *ROI)
{
   static char FuncName[]={"SUMA_DrawnROI_to_1DDrawROI"};
   SUMA_1D_DRAWN_ROI *ROI_1D=NULL;
   SUMA_ROI_DATUM *ROI_Datum=NULL;
   DListElmt *Elm = NULL;
   int i = -1, cnt = 0, *isort=NULL, *iLabel=NULL, *iNode=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ROI) {
      SUMA_SL_Err("Null ROI");
      SUMA_RETURN(NULL);
   }
   
   /* count the total number of nodes in ROI */
   
   /* allocate for nimlROI */
   ROI_1D = (SUMA_1D_DRAWN_ROI *)SUMA_calloc(1,sizeof(SUMA_1D_DRAWN_ROI));
   Elm = NULL;
   ROI_1D->N = 0;
   do {
      if (!Elm) Elm = dlist_head(ROI->ROIstrokelist); 
      else Elm = Elm->next;
      ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
      ROI_1D->N += ROI_Datum->N_n;
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
      
   ROI_1D->Type = (int)ROI->Type;
   ROI_1D->idcode_str = ROI->idcode_str;
   ROI_1D->Parent_idcode_str = ROI->Parent_idcode_str;
   ROI_1D->Label = ROI->Label;
   ROI_1D->iNode = NULL;
   ROI_1D->iLabel = NULL;
   iNode = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   iLabel = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   if (!iNode || !iLabel) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   /* now fill the node indices and the node values */
   Elm = NULL;
   cnt = 0;
   do {
      if (!Elm) Elm = dlist_head(ROI->ROIstrokelist);
      else Elm = Elm->next;
      ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
      for (i=0; i < ROI_Datum->N_n; ++i) {
         iNode[cnt] = ROI_Datum->nPath[i];
         iLabel[cnt] = ROI->iLabel;
         ++cnt;
      }
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
   
   /* some node entries are redundant, clear those up */
   /* first sort iNode */
   isort = SUMA_z_dqsort( iNode, ROI_1D->N);

   /* Now sort the labels accordingly */
   ROI_1D->iLabel = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   ROI_1D->iNode = (int *) SUMA_calloc(ROI_1D->N, sizeof(int));
   if (!ROI_1D->iNode || !ROI_1D->iLabel) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   for (i=0; i < ROI_1D->N; ++i) {
      ROI_1D->iLabel[i] = iLabel[isort[i]];
   }
   if (iLabel) SUMA_free(iLabel); iLabel = NULL; /* done with unsorted version of iLabel */
   
   /* Now remove redundant entries */
   cnt = 0;
   ROI_1D->iNode[cnt] = iNode[0]; 
   ROI_1D->iLabel[cnt] = ROI_1D->iLabel[0];
   ++cnt;
   for (i=1;i<ROI_1D->N;++i)
    {
      if ((iNode[i] != iNode[i- 1]))
         {
            ROI_1D->iNode[cnt] = iNode[i];
            ROI_1D->iLabel[cnt] = ROI_1D->iLabel[i]; 
            ++cnt;   
         }
   }

   
   /* you would reallocate here, because cnt is always <= ROI_1D->N,
   but it is not worth the effort because cnt is only slightly
   less than ROI_1D->N */
   
   /* just update ROI_1D->N */
   ROI_1D->N = cnt;
   
   if (isort) SUMA_free(isort); isort = NULL;
   if (iNode) SUMA_free(iNode); iNode = NULL;
   
   SUMA_RETURN(ROI_1D);
}
/*!
   \brief A function to find the range of node values in an ROI
   \returns the maximum number of nodes in this ROI.  
   
*/
int SUMA_NodeRange_DrawnROI (SUMA_DRAWN_ROI *ROI, int *min, int *max)
{
   static char FuncName[]={"SUMA_NodeRange_DrawnROI"};
   
   SUMA_ROI_DATUM *ROI_Datum=NULL;
   DListElmt *Elm = NULL;
   int i = -1, cnt = 0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!ROI || !min || !max) {
      SUMA_SL_Err("Null ROI");
      SUMA_RETURN(-1);
   }
   
   /* count the total number of nodes in ROI */
   
   
   /* now fill the node indices and the node values */
   Elm = NULL;
   *min = -1;
   *max = -1;
   cnt = 0;
   do {
      if (!Elm) Elm = dlist_head(ROI->ROIstrokelist);
      else Elm = Elm->next;
      ROI_Datum = (SUMA_ROI_DATUM *)Elm->data;
      for (i=0; i < ROI_Datum->N_n; ++i) {
         if (*min < 0) *min = ROI_Datum->nPath[i];
         else if (*min > ROI_Datum->nPath[i]) *min = ROI_Datum->nPath[i];
         
         if (*max < ROI_Datum->nPath[i]) *max = ROI_Datum->nPath[i];
         ++cnt;
      }
   } while (Elm != dlist_tail(ROI->ROIstrokelist));
   
   SUMA_RETURN(cnt);
}


/*!
   \brief frees a ROI_1D structure. These structures are created by
    the likes of SUMA_DrawnROI_to_1DDrawROI
    
    \sa SUMA_DrawnROI_to_1DDrawROI
*/
SUMA_1D_DRAWN_ROI * SUMA_Free_1DDrawROI (SUMA_1D_DRAWN_ROI *ROI_1D)
{
   static char FuncName[]={"SUMA_Free_1DDrawROI"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!ROI_1D) SUMA_RETURN(NULL);
   
   if (ROI_1D->iLabel) SUMA_free(ROI_1D->iLabel); 
   if (ROI_1D->iNode) SUMA_free(ROI_1D->iNode);
   
   SUMA_free(ROI_1D);
   
   SUMA_RETURN(NULL);
}


/*!
   \brief writes a vector of SUMA_DRAWN_ROI * to disk in afni's 1D ascii format
*/

SUMA_Boolean SUMA_Write_DrawnROI_1D (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename) 
{
   static char FuncName[]={"SUMA_Write_DrawnROI_1D"};
   char *newname=NULL;
   int i,j;
   SUMA_1D_DRAWN_ROI *ROI_1D = NULL;
   SUMA_DRAWN_ROI *ROI = NULL;
   FILE *fout=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* add a .1D.roi extension */
   newname = SUMA_Extension(filename, ".1D.roi", NOPE); 
   if (!newname) {
      SUMA_SL_Err("Invalid filename");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH(newname);

   fout = fopen(newname,"w");
   if (!fout) {
      SUMA_SL_Err("Failed to open file for writing.");
      SUMA_RETURN(NOPE);
   }

   /* write the various ROIs */
   for (i=0; i < N_ROI; ++i) {
      ROI = ROIv[i];
      if (!ROI) {
         SUMA_SL_Err("NULL ROI!");
         fclose(fout);
         SUMA_RETURN(NOPE);
      }
      /* Transform the ROI to niml friendly structure */
      if (!(ROI_1D = SUMA_DrawnROI_to_1DDrawROI (ROI))) {
         SUMA_SL_Err("NULL niml_ROI!");
         fclose(fout);
         SUMA_RETURN(NOPE);
      }

      /* write it out in a NIML friendly format */
      /* begin with attributes */
      fprintf (fout,"# %s\n", SUMA_Dset_Type_Name(SUMA_NODE_ROI));
      fprintf (fout,"#  ni_type = \"SUMA_1D_ROI_DATUMorint,int?\"\n");
      fprintf (fout,"#  ni_dimen = \"%d\"\n",  ROI_1D->N);
      fprintf (fout,"#  ni_datasize = \"???\"\n");
      fprintf (fout,"#  idcode_str = \"%s\"\n", ROI_1D->idcode_str);
      fprintf (fout,"#  Parent_idcode_str = \"%s\"\n", ROI_1D->Parent_idcode_str);
      fprintf (fout,"#  Label = \"%s\"\n", ROI_1D->Label);
      fprintf (fout,"# >\n");
      for (j=0; j < ROI_1D->N; ++j) 
         fprintf (fout," %d %d\n", ROI_1D->iNode[j], ROI_1D->iLabel[j]);
      fprintf (fout,"# </%s>\n", SUMA_Dset_Type_Name(SUMA_NODE_ROI));
      fprintf (fout,"\n");
       
      /* free the ROI_1D structure */
      ROI_1D = SUMA_Free_1DDrawROI (ROI_1D); ROI_1D = NULL;
   }
   
   fclose(fout) ; 
   if (newname) SUMA_free(newname);
   
   SUMA_RETURN(YUP);
}


SUMA_FORM_AFNI_DSET_STRUCT *SUMA_New_FormAfniDset_Opt(void)
{
   static char FuncName[]={"SUMA_New_FormAfniDset_Opt"};
   SUMA_FORM_AFNI_DSET_STRUCT *Opt=NULL;
   
   SUMA_ENTRY;
   
   Opt = (SUMA_FORM_AFNI_DSET_STRUCT*)
            SUMA_calloc(1,sizeof(SUMA_FORM_AFNI_DSET_STRUCT));
   
   Opt->master = NULL;
   Opt->mset = NULL;
   Opt->mask = NULL;
   Opt->prefix = NULL;
   Opt->prefix_path = NULL;
   Opt->orcode = NULL;
   Opt->do_ijk = 1;
   Opt->dimen_ii=0;
   Opt->dimen_jj=0;
   Opt->dimen_kk=0;
   Opt->datum=MRI_short;
   Opt->dval=1.0;
   Opt->fval=0.0;
   Opt->mmask=NULL;
   Opt->full_list = 0;
   Opt->exists = -1;
   SUMA_RETURN(Opt);
}

SUMA_FORM_AFNI_DSET_STRUCT *SUMA_Free_FormAfniDset_Opt(SUMA_FORM_AFNI_DSET_STRUCT *Opt)
{
   static char FuncName[]={"SUMA_Free_FormAfniDset_Opt"};
   SUMA_ENTRY;
   
   if (!Opt) SUMA_RETURN(NULL);
   
   if (Opt->master) SUMA_free(Opt->master);
   if (Opt->mask) SUMA_free(Opt->mask);  
   if (Opt->mset) {
      SUMA_SL_Warn("mset is not freed in this function.\nMake sure it is not a lost pointer.\nSet mset to NULL to avoid seeing this message");
   } 
   if (Opt->prefix) SUMA_free(Opt->prefix);
   if (Opt->prefix_path) SUMA_free(Opt->prefix_path);
   if (Opt->mmask) SUMA_free(Opt->mmask);
   if (Opt->orcode) SUMA_free(Opt->orcode);
   SUMA_free(Opt);
   
   SUMA_RETURN(NULL);
}

/*!
   (stolen from 3dUndump)
   \param NodeList (float *) (3*N_valsx1) XYZ or IJK coordinates. 
                             Can be NULL if Opt->full_list = 1.
                             &(NodeList[3*n]) is the pointer for the coordinates of entry ni  
   \param vals     (float *) (N_valsx1) vector of values. Can be NULL, in which case the
                             default value in Opt is used.
   \param N_vals   (int) number of values. If Opt->full_list then N_vals must be equal to the 
                             number of voxels in the data set (dimen_ii*dimen_jj*dimen_kk) 
                             as specified by Opt's fields or the master set.
   \param Opt (SUMA_FORM_AFNI_DSET_STRUCT *) The famed options structure:
      master: Name of master dset. NULL if none
      mask: Name of mask dset. NULL if none. 
      prefix: Prefix of output. Must have.
      orcode: Orientation string. NULL if none, like RAI, LPI, etc.
      do_ijk: 1 = coordinates in NodeList are voxel indices (i j k)
              0 = coordinates in NodeList are voxel dicomm coordinates (x y z)
      dimen_ii(_jj, _kk): Number of voxels in ii (jj, kk) directions. 
                          Do not use with master option
                          Must use either master or dimen options.
      datum: output data type: MRI_short(default), MRI_byte, MRI_float only.
      dval: Default data value (default 1). Used if vals is NULL
      fval: Default fill value (default 0). 
      mmask: (byte *) mask of (dimen_ii*dimen_jj*dimen_kk x 1) values always. This can
                      be used to pass a predefined mask. NULL if not
                      interested in it. Cannot specify it along with mask
                      option above. If both NodeList and Vals are null, then
                      mmask is used as data, replaces vals in a way. If mmask points
                      to non NULL, the memory at that pointer is freed when Opt is freed.
      full_list: 1 = full list, coordinates are inferred from 1D index of array.
                     N_vals == dimen_ii*dimen_jj*dimen_kk. Requires NodeList == NULL
                 0 = not a full list NodeList != NULL
      exists: A flag that indicates upon returning from this function whether the 
               dset's name exists already or not.
               -1: untested
               1: exists
               0: Does not exist
   - See  SUMA_Free_FormAfniDset_Opt for freeing Opt and its contents
   
   \return dset (THD_3dim_dataset *) output dset.
                                     Write it to disk with :   DSET_write(dset) ;
                                     Delete it with: DSET_delete(dset); dset = NULL;

   - FUNCTION NOT FULLY TESTED for all options, USE WITH CARE : Feb 08 05
*/
THD_3dim_dataset *SUMA_FormAfnidset (float *NodeList, float *vals, int N_vals, SUMA_FORM_AFNI_DSET_STRUCT *Opt)
{
   static char FuncName[]={"SUMA_FormAfnidset"};
   THD_coorder cord;
   int ii=0,jj=0,kk=0,ll=0,ijk=0 , nx=0,ny=0,nz=0 , nxyz=0 ;
   float      xx,yy,zz,vv=0.0 , dx,dy,dz;
   short               sv=0   ;
   byte                bv=0   ;
   float *fbr=NULL, fval_float, dval_float;
   byte *bbr=NULL, *mmask=NULL, fval_byte, dval_byte;
   short *sbr=NULL, fval_short, dval_short;
   char *orcode=NULL;
   float xxdown =0.0,xxup=0.0 , yydown=0.0,yyup=0.0 , zzdown=0.0,zzup=0.0 ;
   SUMA_Boolean LocalHead = NOPE;
   THD_3dim_dataset *dset=NULL, *mset=NULL, *maskset=NULL;

   SUMA_ENTRY;
   
   /* check for badiosity */
   if( Opt->do_ijk == 0 && Opt->master == NULL ) {
      SUMA_SL_Err("Can't use mm coords without master.") ;
      SUMA_RETURN(NULL);
   }
   if( (Opt->master == NULL && Opt->mset == NULL) && Opt->dimen_ii < 2 ) {
      SUMA_SL_Err("Must use exactly one of Opt->master or Opt->dimen options");
      SUMA_RETURN(NULL);
   }
   if (Opt->master && Opt->mset) {
      SUMA_SL_Err("Cannot use Opt->master and Opt->mset");
      SUMA_RETURN(NULL);
   }
   
   fval_byte = (byte)Opt->fval;
   fval_short = (short)Opt->fval;
   fval_float = (float)Opt->fval;
   dval_byte = (byte)Opt->dval;
   dval_short = (short)Opt->dval;
   dval_float = (float)Opt->dval;
   
   if( (Opt->datum == MRI_short && dval_short == fval_short) ||
    (Opt->datum == MRI_float && dval_float == fval_float) ||
    (Opt->datum == MRI_byte  && dval_byte  == fval_byte )   ){

      SUMA_SL_Warn("dval and fval are the same!") ;
   }

   if (Opt->full_list && NodeList) {
      SUMA_SL_Err("Opt->full_list && NodeList");
      SUMA_RETURN(NULL);
   }
   if (!Opt->full_list && !NodeList &&!Opt->mmask) {
      SUMA_SL_Err("!Opt->full_list && !NodeList && !Opt->mmask");
      SUMA_RETURN(NULL);
   }

   if (!Opt->prefix || !Opt->prefix_path) {
      SUMA_SL_Err("Need a prefix and a prefix_path Joe.");
      SUMA_RETURN(NULL);
   }

   if (!NodeList && !vals && !Opt->mmask) {
      SUMA_SL_Warn("Creating a dataset of constants. (!NodeList && !vals && !Opt->mmask)");
   }
   
   if (Opt->master) {
      mset = THD_open_dataset(Opt->master);
      if( mset == NULL ) {
         SUMA_SL_Err("-master: can't open dataset" ) ; 
         SUMA_RETURN(dset);
      }
   }
   if (Opt->mset) mset = Opt->mset;
   
   if ((Opt->master || Opt->mset) && Opt->orcode) {
      SUMA_SL_Err("Cannot have bothpt->master && Opt->orcode");
      SUMA_RETURN(dset);      
   }
   
   if (Opt->mask && Opt->mmask) {
      SUMA_SL_Err("Cannot have both Opt->mask && Opt->mmask");
      SUMA_RETURN(dset);
   }

   if (Opt->mask) {
      maskset = THD_open_dataset( Opt->mask) ;
      if( maskset == NULL ) {
         SUMA_SL_Err("-mask: can't open dataset" ) ; 
         if (mset) {  DSET_delete(mset); mset = NULL; }
         SUMA_RETURN(dset);
      }
   }
   
   
  /*-- set orcode to value from -master, if this is needed --*/

  if( mset != NULL ){
      orcode = malloc(4) ;
      orcode[0] = ORIENT_typestr[mset->daxes->xxorient][0] ;
      orcode[1] = ORIENT_typestr[mset->daxes->yyorient][0] ;
      orcode[2] = ORIENT_typestr[mset->daxes->zzorient][0] ;
      orcode[3] = '\0' ;
   } else if (Opt->orcode) {
      orcode = malloc(4) ; orcode = strcpy(orcode, Opt->orcode);
   } else {
      SUMA_SL_Err("Huh?");
      if (mset) {  DSET_delete(mset); mset = NULL; }
      if (maskset) {  DSET_delete(maskset); maskset = NULL; }
      SUMA_RETURN(dset);
   }

   THD_coorder_fill( orcode , &cord ) ;  /* setup coordinate order */ 

   /*-- make empty dataset --*/
   if( mset != NULL ){                 /* from -master */

      dset = EDIT_empty_copy( mset ) ;
      EDIT_dset_items( dset ,
                          ADN_prefix    , Opt->prefix ,
                          ADN_datum_all , Opt->datum ,
                          ADN_nvals     , 1 ,
                          ADN_ntt       , 0 ,
                          ADN_func_type , ISANAT(mset) ? mset->func_type
                                                       : FUNC_FIM_TYPE ,

                          ADN_directory_name , Opt->prefix_path ,
                          ADN_none ) ;

   } else {                            /* from nothing */
     THD_ivec3 iv_nxyz   , iv_xyzorient ;
     THD_fvec3 fv_xyzorg , fv_xyzdel ;

     LOAD_IVEC3( iv_nxyz , Opt->dimen_ii , Opt->dimen_jj , Opt->dimen_kk ) ;
     LOAD_IVEC3( iv_xyzorient , cord.xxor , cord.yyor , cord.zzor ) ;
     LOAD_FVEC3( fv_xyzdel ,
                 ORIENT_sign[iv_xyzorient.ijk[0]]=='+' ? 1.0 : -1.0 ,
                 ORIENT_sign[iv_xyzorient.ijk[1]]=='+' ? 1.0 : -1.0 ,
                 ORIENT_sign[iv_xyzorient.ijk[2]]=='+' ? 1.0 : -1.0  ) ;
     LOAD_FVEC3( fv_xyzorg ,
                 ORIENT_sign[iv_xyzorient.ijk[0]]=='+' ? -0.5*Opt->dimen_ii : 0.5*Opt->dimen_ii,
                 ORIENT_sign[iv_xyzorient.ijk[1]]=='+' ? -0.5*Opt->dimen_jj : 0.5*Opt->dimen_jj,
                 ORIENT_sign[iv_xyzorient.ijk[2]]=='+' ? -0.5*Opt->dimen_kk : 0.5*Opt->dimen_kk ) ;

     dset = EDIT_empty_copy( NULL ) ;

     EDIT_dset_items( dset ,
                       ADN_nxyz      , iv_nxyz ,
                       ADN_xyzdel    , fv_xyzdel ,
                       ADN_xyzorg    , fv_xyzorg ,
                       ADN_xyzorient , iv_xyzorient ,
                       ADN_prefix    , Opt->prefix ,
                       ADN_datum_all , Opt->datum ,
                       ADN_nvals     , 1 ,
                       ADN_ntt       , 0 ,
                       ADN_type      , HEAD_FUNC_TYPE ,
                       ADN_func_type , FUNC_FIM_TYPE ,
                       ADN_directory_name , Opt->prefix_path ,
                    ADN_none ) ;

   }

   if( THD_is_file(DSET_HEADNAME(dset)) ) {
      SUMA_LHv("Note Output dataset %s already exists \n", DSET_HEADNAME(dset)) ;
      Opt->exists = 1;
   } else {
      Opt->exists = 0;
   }
   
   /*-- make empty brick array for dataset --*/

   EDIT_substitute_brick( dset , 0 , Opt->datum , NULL ) ;  /* will make array */

   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxyz = nx*ny*nz;

   if (Opt->full_list && N_vals != nxyz) {
      SUMA_SL_Err("Opt->full_list && N_vals != nx*ny*nz");
      SUMA_RETURN(NULL);
   }
   /* 19 Feb 2004: check and make mask if desired */

   if( maskset != NULL &&
       ( DSET_NX(maskset) != nx ||
         DSET_NY(maskset) != ny ||
         DSET_NZ(maskset) != nz   ) ) {
     SUMA_SL_Err("mask dataset doesn't match dimension of output dataset") ;
     if (mset) {  DSET_delete(mset); mset = NULL; }
     if (maskset) {  DSET_delete(maskset); maskset = NULL; }
     DSET_delete(dset); dset = NULL;
     SUMA_RETURN(NULL);
   }

   if( maskset != NULL ){
     mmask = THD_makemask( maskset , 0 , 1.0,-1.0 ) ;
     SUMA_SL_Warn("can't create mask for some reason!") ;
     DSET_delete(maskset) ;
   } else mmask = Opt->mmask;

   if( mmask == NULL ){
   } else {
      int nmask = THD_countmask( nxyz , mmask ) ;
      if( nmask == 0 ){
         SUMA_SL_Warn("0 voxels in mask -- ignoring it!") ;
         if (!Opt->mmask) free((void *)mmask) ; mmask = NULL ;
      } else {
         fprintf(SUMA_STDERR,"%s:++ %d voxels found in mask\n", FuncName, nmask) ;
      }
   }

   /*-- fill new dataset brick with the -fval value --*/
   switch( Opt->datum ){
      case MRI_short:
         if (0) fprintf(SUMA_STDERR,"%s: Filling with %d\n", FuncName, fval_short);
         sbr = (short *) DSET_BRICK_ARRAY(dset,0) ;
         for( ii=0 ; ii < nxyz ; ii++ ) sbr[ii] = fval_short ;
      break ;

      case MRI_float:
         fbr = (float *) DSET_BRICK_ARRAY(dset,0) ;
         for( ii=0 ; ii < nxyz ; ii++ ) fbr[ii] = fval_float ;
      break ;

      case MRI_byte:
         bbr = (byte *) DSET_BRICK_ARRAY(dset,0) ;
         for( ii=0 ; ii < nxyz ; ii++ ) bbr[ii] = fval_byte ;
      break ;
   }

   /* 24 Nov 2000: get the bounding box for the dataset */

   dx = fabs(dset->daxes->xxdel) ; if( dx <= 0.0 ) dx = 1.0 ;
   dy = fabs(dset->daxes->yydel) ; if( dy <= 0.0 ) dy = 1.0 ;
   dz = fabs(dset->daxes->zzdel) ; if( dz <= 0.0 ) dz = 1.0 ;

   if( !Opt->do_ijk ){
#ifndef EXTEND_BBOX
      xxdown = dset->daxes->xxmin - 0.501 * dx ;
      xxup   = dset->daxes->xxmax + 0.501 * dx ;
      yydown = dset->daxes->yymin - 0.501 * dy ;
      yyup   = dset->daxes->yymax + 0.501 * dy ;
      zzdown = dset->daxes->zzmin - 0.501 * dz ;
      zzup   = dset->daxes->zzmax + 0.501 * dz ;
#else
      xxdown = dset->daxes->xxmin ;
      xxup   = dset->daxes->xxmax ;
      yydown = dset->daxes->yymin ;
      yyup   = dset->daxes->yymax ;
      zzdown = dset->daxes->zzmin ;
      zzup   = dset->daxes->zzmax ;
#endif
   }

   for (ll=0; ll<N_vals; ++ll) {
      /* stick 'em in */
      if (!Opt->full_list) {
         xx = NodeList[3*ll]; yy = NodeList[3*ll+1]; zz = NodeList[3*ll+2]; 
         if (Opt->do_ijk) {
            ii = (int) rint(xx) ; jj = (int) rint(yy) ; kk = (int) rint(zz) ;
            if( ii < 0 || ii >= nx ){
               fprintf(stderr,
                       "Warning %s: entry %d: i index=%d is invalid, ignoring...\n",
                       FuncName,ll,ii) ;
               continue ;
            }
            if( jj < 0 || jj >= ny ){
               fprintf(stderr,
                       "Warning %s: entry %d: j index=%d is invalid, ignoring...\n",
                       FuncName, ll,jj) ;
               continue ;
            }
            if( kk < 0 || kk >= nz ){
               fprintf(stderr,
                       "Warning %s: entry %d: k index=%d is invalid\n",
                       FuncName,ll,kk) ;
               continue ;
            }
         } else {   /* inputs are coordinates => must convert to index */

            THD_fvec3 mv , dv ;                              /* temp vectors */
            THD_ivec3 iv ;

            THD_coorder_to_dicom( &cord , &xx,&yy,&zz ) ;    /* to Dicom order */
            LOAD_FVEC3( dv , xx,yy,zz ) ;
            mv = THD_dicomm_to_3dmm( dset , dv ) ;           /* to Dataset order */

            /* 24 Nov 2000: check (xx,yy,zz) for being inside the box */

            if( mv.xyz[0] < xxdown || mv.xyz[0] > xxup ){
               fprintf(stderr,"+++ Warning %s: line %d: x coord=%g is outside %g .. %g\n" ,
                       FuncName,ll,mv.xyz[0] , xxdown,xxup ) ;
               continue ;
            }
            if( mv.xyz[1] < yydown || mv.xyz[1] > yyup ){
               fprintf(stderr,"+++ Warning %s: line %d: y coord=%g is outside %g .. %g\n" ,
                       FuncName,ll,mv.xyz[1] , yydown , yyup ) ;
               continue ;
            }
            if( mv.xyz[2] < zzdown || mv.xyz[2] > zzup ){
               fprintf(stderr,"+++ Warning %s: line %d: z coord=%g is outside %g .. %g\n" ,
                       FuncName,ll,mv.xyz[2] , zzdown , zzup ) ;
               continue ;
            }

            iv = THD_3dmm_to_3dind( dset , mv ) ;            /* to Dataset index */
            ii = iv.ijk[0]; jj = iv.ijk[1]; kk = iv.ijk[2];  /* save */
         }

         ijk = ii + jj*nx + kk*nx*ny ;
      } else {
         ijk = ll; 
      }

      if (vals) vv = vals[ll];
      else vv = dval_float ;   

      if( mmask == NULL || mmask[ijk] ){
         switch( Opt->datum ){
            case MRI_float:{
              if( fbr[ijk] != fval_float && fbr[ijk] != vv )
                fprintf(stderr,"Overwrite voxel %d %d %d\n",ii,jj,kk) ;
              fbr[ijk] = vv ;
            }
            break ;
            case MRI_short:{
              sv = SHORTIZE(vv) ;
              if( sbr[ijk] != fval_short && sbr[ijk] != sv )
                fprintf(stderr,"Overwrite voxel %d %d %d\n",ii,jj,kk) ;
              sbr[ijk] = sv ;
            }
            break ;
            case MRI_byte:{
              bv = BYTEIZE(vv) ;
              if( bbr[ijk] != fval_byte && bbr[ijk] != bv )
                fprintf(stderr,"Overwrite voxel %d %d %d\n",ii,jj,kk) ;
              bbr[ijk] = bv ;
            }
            break ;
         }
      }

   }
   
   if (orcode) free(orcode); orcode = NULL;
   if (mmask && !Opt->mmask) free(mmask); mmask = NULL;
   if (mset && !Opt->mset) DSET_delete(mset); mset = NULL; 

   SUMA_RETURN(dset);      
}

/*! 
   \brief A function to change a SurfaceObject to niml format 
   \param SO (SUMA_SurfaceObject *)
   \param optlist (char *) string indicating what parts of SO to put in ngr
                           choose any combination of: NodeList, FaceSetList, 
                           EdgeList, MemberFace, NodeNeighb, VolPar, facenormals, 
                           NodeNormals, PolyArea, 
   \param No_LinksExternalElements (int) 1: Do not include IDs of elements not 
                           created inside this group, i.e. not mentioned in optlist
                                       0: So include IDs of elements that may reside 
                                       on disk.
   \return ngr (NI_group *) a NI group element formed from SO. 
                           NOTE: That element has the SAME ID as SO!
   \sa SUMA_nimlSO2SO
*/
NI_group *SUMA_SO2nimlSO(SUMA_SurfaceObject *SO, char *optlist, int nlee) 
{
   static char FuncName[]={"SUMA_SO2nimlSO"};
   NI_group *ngr = NULL;
   NI_element *nel = NULL;
   char stmp[500];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO) { 
      SUMA_SL_Err("Null SO"); SUMA_RETURN(ngr);
   }
   
   /* create group and name it */
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "SurfaceObject");
   
   /** BEGIN ATTRIBUTES COMMON TO ALL OBJECTS **/ 
   
   /* set the object type attribute */
   switch (SO->FaceSetDim) {
      case 3:
         NI_set_attribute(ngr, "Object_Type", "Triangulated_Surface");
         break;
      default:
         NI_set_attribute(ngr, "Object_Type", SUMA_EMPTY_ATTR);
         SUMA_SL_Warn("FaceSetDim not supported");
         break;
   }      
   
   /* set the object ID */
   if (SO->idcode_str) {
      NI_set_attribute(ngr, "self_idcode", SO->idcode_str);
   } else {
      NI_set_attribute(ngr, "self_idcode", SUMA_EMPTY_ATTR);
   }  
   
   /* set the object Label */
   if (SO->Label) {
      NI_set_attribute(ngr, "Object_Label", SO->Label);
   } else {
      NI_set_attribute(ngr, "Object_Label", SUMA_EMPTY_ATTR);
   }
   
   
   /* set the parent ID */
   if (SO->LocalDomainParentID) {
      NI_set_attribute(ngr, "domain_parent_idcode", SO->LocalDomainParentID);
   } else {
      NI_set_attribute(ngr, "domain_parent_idcode", SUMA_EMPTY_ATTR);
   }
   
   /* set the grand parent ID */
   if (SO->DomainGrandParentID) {
      NI_set_attribute(ngr, "Grand_domain_parent_idcode", 
                            SO->DomainGrandParentID);
   } else {
      NI_set_attribute(ngr, "Grand_domain_parent_idcode", SUMA_EMPTY_ATTR);
   }
   
   /** END ATTRIBUTES COMMON TO ALL OBJECTS **/      
   
   /** BEGIN ATTRIBUTES specific to Surfaces**/
   if (SO->Group_idcode_str) {
      NI_set_attribute(ngr, "Subject_ID", SO->Group_idcode_str);
   } else {
      NI_set_attribute(ngr, "Subject_ID", SUMA_EMPTY_ATTR);
   }
   
   if (SO->Group) {
      NI_set_attribute(ngr, "Subject_Label", SO->Group);
   } else {
      NI_set_attribute(ngr, "Subject_Label", SUMA_EMPTY_ATTR);
   }
   
   if (SO->OriginatorID) {
      NI_set_attribute(ngr, "Instance_ID", SO->OriginatorID);
   } else {
      NI_set_attribute(ngr, "Instance_ID", SUMA_EMPTY_ATTR);
   }
   
   if (SO->OriginatorLabel) {
      NI_set_attribute(ngr, "Instance_Label", SO->OriginatorLabel);
   } else {
      NI_set_attribute(ngr, "Instance_Label", SUMA_EMPTY_ATTR);
   }
   
   NI_SET_INT(ngr, "do_type", SO->do_type);
   
   switch (SO->Side) {
      case SUMA_NO_SIDE:
         NI_set_attribute(ngr, "Side", "none");
         break;
      case SUMA_LEFT:
         NI_set_attribute(ngr, "Side", "left");
         break;
      case SUMA_RIGHT:
         NI_set_attribute(ngr, "Side", "right");
         break;
      case SUMA_LR:
         NI_set_attribute(ngr, "Side", "lr");
         break;
      default:
         NI_set_attribute(ngr, "Side", SUMA_EMPTY_ATTR);
         break;
   }
   
   if (SO->State) {
      NI_set_attribute(ngr, "Layer_Name", SO->State);
   } else {
      NI_set_attribute(ngr, "Layer_Name", SUMA_EMPTY_ATTR);
   }
   
   if (SO->AnatCorrect) {
      NI_set_attribute(ngr, "Anatomically_Correct", "yes");
   } else {
      NI_set_attribute(ngr, "Anatomically_Correct", "no");
   }
   
   sprintf(stmp,"%d", SO->EmbedDim);
   NI_set_attribute(ngr, "Embedding_Dimension", stmp);
   
   if (SO->FileType >=0) {
      sprintf(stmp,"%s", SUMA_SurfaceTypeString(SO->FileType));
      NI_set_attribute(ngr, "Surface_Creation_Software",  stmp);
   } else {
      NI_set_attribute(ngr, "Surface_Creation_Software", SUMA_EMPTY_ATTR);
   }
   
   NI_set_attribute(ngr, "Surface_Creation_History", SUMA_EMPTY_ATTR); 
   
   
   sprintf(stmp,"%d", SO->normdir);
   NI_set_attribute(ngr, "Node_Normal_Direction", stmp);
   
   if (!nlee || SUMA_iswordin(optlist,"FaceSetList")) {
      if (SO->facesetlist_idcode_str) {
         NI_set_attribute(ngr, "Mesh_Element_ID", SO->facesetlist_idcode_str);
      } else {
         if (SO->idcode_str) {
            sprintf(stmp, "facesetlist_idcode_str_%s", SO->idcode_str);
            SUMA_NEW_ID(SO->facesetlist_idcode_str, stmp);
            NI_set_attribute(ngr, "Mesh_Element_ID", SO->facesetlist_idcode_str);
         } else  {
            NI_set_attribute(ngr, "Mesh_Element_ID", SUMA_EMPTY_ATTR);
         }
      }
   }
   
   if (!nlee || SUMA_iswordin(optlist,"NodeList")) {
      if (SO->nodelist_idcode_str) {
         NI_set_attribute(ngr, "NodeList_Element_ID", SO->nodelist_idcode_str);
      } else {
         if (SO->idcode_str) {
            sprintf(stmp, "nodelist_idcode_str_%s", SO->idcode_str);
            SUMA_NEW_ID(SO->nodelist_idcode_str, stmp);
            NI_set_attribute(ngr, "NodeList_Element_ID", 
                                  SO->nodelist_idcode_str);
         } else  {
            NI_set_attribute(ngr, "NodeList_Element_ID", SUMA_EMPTY_ATTR);
         }
      }
   }
   if (!nlee || SUMA_iswordin(optlist,"facenormals")) {
      if (SO->facenormals_idcode_str) {
         NI_set_attribute(ngr, "Polygon_Normals_Element_ID", 
                               SO->facenormals_idcode_str);
      } else {
         if (SO->idcode_str) {
            sprintf(stmp, "facenormals_idcode_str_%s", SO->idcode_str);
            SUMA_NEW_ID(SO->facenormals_idcode_str, stmp);
            NI_set_attribute(ngr, "Polygon_Normals_Element_ID", 
                                  SO->facenormals_idcode_str);
         } else  {
            NI_set_attribute(ngr, "Polygon_Normals_Element_ID", SUMA_EMPTY_ATTR);
         }
      }
   }
   
   if (!nlee || SUMA_iswordin(optlist,"NodeNormals")) {
      if (SO->nodenormals_idcode_str) {
         NI_set_attribute(ngr, "Node_Normals_Element_ID", 
                               SO->nodenormals_idcode_str);
      } else {
         if (SO->idcode_str) {
            sprintf(stmp, "nodenormals_idcode_str_%s", SO->idcode_str);
            SUMA_NEW_ID(SO->nodenormals_idcode_str, stmp);
            NI_set_attribute(ngr, "Node_Normals_Element_ID", 
                                  SO->nodenormals_idcode_str);
         } else  {
            NI_set_attribute(ngr, "Node_Normals_Element_ID", SUMA_EMPTY_ATTR);
         }
      }
   }
   
   if (!nlee || SUMA_iswordin(optlist,"PolyArea")) {
      if (SO->polyarea_idcode_str) {
         NI_set_attribute(ngr, "Polygon_Area_Element_ID", 
                               SO->polyarea_idcode_str);
      } else {
         if (SO->idcode_str) {
            sprintf(stmp, "polyarea_idcode_str_%s", SO->idcode_str);
            SUMA_NEW_ID(SO->polyarea_idcode_str, stmp);
            NI_set_attribute(ngr, "Polygon_Area_Element_ID", 
                                  SO->polyarea_idcode_str);
         } else  {
            NI_set_attribute(ngr, "Polygon_Area_Element_ID", SUMA_EMPTY_ATTR);
         }
      }
   }

   if (!nlee || SUMA_iswordin(optlist,"EdgeList")) {
      /* add here the edge list, the node neighbor list 
         and the face neighbor list IDs*/
      if (SO->EL && SO->EL->idcode_str) {
         NI_set_attribute(ngr, "SUMA_Edge_List_Element_ID", SO->EL->idcode_str);
      } else {
         NI_set_attribute(ngr, "SUMA_Edge_List_Element_ID", SUMA_EMPTY_ATTR);
      }
   }
   
   if (!nlee || SUMA_iswordin(optlist,"MemberFace")) {
      if (SO->MF && SO->MF->idcode_str) {
         NI_set_attribute(ngr, "SUMA_Node_Face_Member_Element_ID", 
                               SO->MF->idcode_str);
      } else {
         NI_set_attribute(ngr, "SUMA_Node_Face_Member_Element_ID", 
                               SUMA_EMPTY_ATTR);
      }
   }
   
   if (!nlee || SUMA_iswordin(optlist,"NodeNeighb")) {
      if (SO->FN && SO->FN->idcode_str) {
         NI_set_attribute(ngr, "SUMA_Node_First_Neighb_Element_ID", 
                               SO->FN->idcode_str);
      } else {
         NI_set_attribute(ngr, "SUMA_Node_First_Neighb_Element_ID", 
                               SUMA_EMPTY_ATTR);
      }
   }

   if (!nlee) {
      /* add the parent volume (SurfVol, NOT SurfVol_AlndExp) IDcode if present. 
        That ID does not usually refer to the volume from which VolPar 
        is created. Except in the case 
        where you are viewing the surfaces on the orignal volume (SurfVol) then 
        this field and SurfVol (afni dset *) ->idcode.str and 
        VolPar->vol_idcode_str should be identical */
      if (SO->parent_vol_idcode_str) {
         NI_set_attribute(ngr, "SUMA_Afni_Parent_Vol_ID", 
                               SO->parent_vol_idcode_str);
      } else {
         NI_set_attribute(ngr, "SUMA_Afni_Parent_Vol_ID", SUMA_EMPTY_ATTR);
      }
   }
   
   /** END ATTRIBUTES specific to Surfaces**/ 
   
   /** BEGIN Adding data elements **/
   
   /* add the node list */
   if (SUMA_iswordin(optlist,"NodeList")) {
      SUMA_LH("Adding Nodelist nel...");
      nel = SUMA_NodeXYZ2NodeXYZ_nel (SO, SO->NodeList, 0, SUMA_NEW_NODE_XYZ);
      if (!nel) { SUMA_SL_Err("Failed to create nel"); NI_free_element(ngr); SUMA_RETURN(NULL); }
      NI_add_to_group( ngr, nel); 
   }
   
   /* add the faceset list */
   if (SUMA_iswordin(optlist,"FaceSetList")) {
      SUMA_LH("Adding Nodelist nel...");
      nel = SUMA_Mesh_IJK2Mesh_IJK_nel (SO, SO->FaceSetList, 0, SUMA_NEW_MESH_IJK);
      if (!nel) { SUMA_SL_Err("Failed to create nel"); NI_free_element(ngr); SUMA_RETURN(NULL); }
      NI_add_to_group( ngr, nel); 
   }   
   
   /* add the edge list */
   if (SUMA_iswordin(optlist,"EdgeList")) {
      SUMA_LH("Adding EdgeList nel...");
      SUMA_SL_Warn("Option not implemented yet.");
      if (SO->EL) {
         /* if (!nel) { SUMA_SL_Err("Failed to create nel"); NI_free_element(ngr); SUMA_RETURN(NULL); } 
         NI_add_to_group( ngr, nel); */
      }
   }
   
   /* add the member face list */
   if (SUMA_iswordin(optlist,"MemberFace")) {
      SUMA_LH("Adding Member of FaceSet nel...");
      SUMA_SL_Warn("Option not implemented yet.");
      if (SO->MF) {
         /* if (!nel) { SUMA_SL_Err("Failed to create nel"); NI_free_element(ngr); SUMA_RETURN(NULL); } 
         NI_add_to_group( ngr, nel); */
      }
   }
   
   /* add the node neighbor list */
   if (SUMA_iswordin(optlist,"NodeNeighb")) {
      SUMA_LH("Adding node neighbors nel...");
      SUMA_SL_Warn("Option not implemented yet.");
      if (SO->FN) {
         /* if (!nel) { SUMA_SL_Err("Failed to create nel"); NI_free_element(ngr); SUMA_RETURN(NULL); } 
         NI_add_to_group( ngr, nel); */
      }
   }
   
   /* add the VolPar element */
   if (SUMA_iswordin(optlist,"VolPar")) {
      SUMA_LH("Adding VolPar nel ...");
      if (SO->VolPar) {
         nel = SUMA_SOVolPar2VolPar_nel (SO, SO->VolPar, SUMA_SURFACE_VOLUME_PARENT);
         if (!nel) { SUMA_SL_Err("Failed to create nel"); NI_free_element(ngr); SUMA_RETURN(NULL); } 
         NI_add_to_group( ngr, nel); 
      }
   }
   
   /** END Adding data elements **/ 
   if (LocalHead) {
      int suc;
      SUMA_SL_Warn("writing SO group to DISK!");
      NEL_WRITE_TX(ngr, "file:Test_SO2NIML_write_asc_1D", suc);
   }
   
   SUMA_RETURN(ngr);
}

SUMA_Boolean SUMA_isnimlSO(NI_group *ngr)
{
   static char FuncName[]={"SUMA_isnimlSO"};
   
   SUMA_ENTRY;
   if (!ngr || !ngr->name || strcmp(ngr->name, "SurfaceObject")) {
      SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(YUP);
}

SUMA_SurfaceObject *SUMA_nimlSO2SO(NI_group *ngr) 
{
   static char FuncName[]={"SUMA_nimlSO2SO"};
   NI_element *nel = NULL;
   char stmp[500], *tmp;
   int ip;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr) {  SUMA_SL_Err("Null ngr"); SUMA_RETURN(SO); }
   
   if (!SUMA_isnimlSO(ngr)) {
      fprintf (SUMA_STDERR,   
               "Error %s: group name (%s) is not (SUMA_SurfaceObject)\n"
               "Object does not appear to be a surface.", 
               FuncName, ngr->name);
   }
   
   /* a new surface */
   SO = SUMA_Alloc_SurfObject_Struct(1); 
   if (!SO) { SUMA_SL_Err("Failed to create SO."); SUMA_RETURN(SO); }
   
   /** BEGIN ATTRIBUTES COMMON TO ALL OBJECTS **/ 
   tmp = SUMA_copy_string(NI_get_attribute(ngr,"self_idcode"));
   if (SUMA_IS_EMPTY_STR_ATTR(tmp)) { 
      SUMA_SL_Warn(  "No ID in nel.\n"
                     "That's not cool yall.\n"
                     "I'll be adding a new one now.");
      SUMA_NEW_ID(SO->idcode_str, NULL); 
      NI_set_attribute(ngr, "Group_ID", SO->idcode_str);
   } else SO->idcode_str = SUMA_copy_string(tmp);
   
   tmp = NI_get_attribute(ngr, "Object_Type");
   if (SUMA_IS_EMPTY_STR_ATTR(tmp)) { 
      SUMA_SL_Err("Missing Object Type."); 
      SUMA_Free_Surface_Object(SO); 
      SO = NULL; 
      SUMA_RETURN(SO); 
   }
   if (!strcmp(tmp, "Triangulated_Surface")) SO->FaceSetDim = 3;
   else {
      fprintf (SUMA_STDERR,
               "Error %s: Object_Type %s not recognized.\n", FuncName, tmp);
      SUMA_Free_Surface_Object(SO); SO = NULL; SUMA_RETURN(SO); 
   }   
   
   NI_GET_INT(ngr, "do_type", SO->do_type);

   tmp = NI_get_attribute(ngr, "Object_Label");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->Label = SUMA_copy_string(tmp);  
   
   /* set the parent ID */
   tmp = NI_get_attribute(ngr, "domain_parent_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->LocalDomainParentID = SUMA_copy_string(tmp);  
   
   
   /* set the grand parent ID */
   tmp = NI_get_attribute(ngr, "Grand_domain_parent_idcode");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->DomainGrandParentID = SUMA_copy_string(tmp);  
   
   
   /** END ATTRIBUTES COMMON TO ALL OBJECTS **/      
   
   /** BEGIN ATTRIBUTES specific to Surfaces**/
   tmp = NI_get_attribute(ngr, "Subject_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->Group_idcode_str = SUMA_copy_string(tmp); 
   
   tmp = NI_get_attribute(ngr, "Subject_Label");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->Group = SUMA_copy_string(tmp); 
   
   tmp = NI_get_attribute(ngr, "Instance_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->OriginatorID = SUMA_copy_string(tmp);    
   
   tmp = NI_get_attribute(ngr, "Instance_Label");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->OriginatorLabel = SUMA_copy_string(tmp); 
   
   
   tmp = NI_get_attribute(ngr, "Side");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) {
      if (!strcmp(tmp,"none")) SO->Side = SUMA_NO_SIDE;
      else if (!strcmp(tmp,"left")) SO->Side = SUMA_LEFT;
      else if (!strcmp(tmp,"right")) SO->Side = SUMA_RIGHT;
      else if (!strcmp(tmp,"lr")) SO->Side = SUMA_LR;
   } 

   tmp = NI_get_attribute(ngr, "Layer_Name");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->State = SUMA_copy_string(tmp); 

   tmp = NI_get_attribute(ngr, "Anatomically_Correct");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) {
      if (!strcmp(tmp,"yes")) SO->AnatCorrect = 1; 
      else SO->AnatCorrect = 0; 
   }
    
   tmp = NI_get_attribute(ngr, "Embedding_Dimension");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->EmbedDim = atoi(tmp); 
   
   tmp = NI_get_attribute(ngr, "Surface_Creation_Software");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->FileType = SUMA_SurfaceTypeCode(tmp); 
      
   tmp = NI_get_attribute(ngr, "SUMA_Afni_Parent_Vol_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) 
      SO->parent_vol_idcode_str = SUMA_copy_string(tmp); 
      
   tmp = NI_get_attribute(ngr, "Node_Normal_Direction");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp)) SO->normdir = atoi(tmp);

   /** END ATTRIBUTES specific to Surfaces**/ 
   
   /* now read the elements in this group */
   for( ip=0 ; ip < ngr->part_num ; ip++ ){ 
      /* do not free elements as you process them, free with group at end */
      switch( ngr->part_typ[ip] ){
         /*-- a sub-group ==> recursion! --*/
         case NI_GROUP_TYPE:
            SUMA_SL_Err("Not ready from groups inside surface group.\n"
                        " Group ignored"); 
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)ngr->part[ip] ;
            if (LocalHead)  {
               fprintf(SUMA_STDERR,
                        "%s:     name=%s \n"
                        "vec_len=%d vec_filled=%d, vec_num=%d\n", 
                        FuncName, nel->name, 
                        nel->vec_len, nel->vec_filled, nel->vec_num );
            }

            /*--- NodeList ---*/
            if(   strcmp(nel->name,"Node_XYZ") == 0 || 
                  strcmp(nel->name,"NewNode_XYZ") == 0) { /* Get Da NodeList */
               if (LocalHead) 
                  fprintf (SUMA_STDERR,
                           "%s:\nGetting NodeList...\n",  FuncName);
               if (!SUMA_NodeXYZ_nel2NodeXYZ(SO, nel)) {
                  SUMA_SL_Err("Failed in SUMA_NodeXYZ_nel2NodeXYZ");
                  SUMA_Free_Surface_Object(SO); SO = NULL; SUMA_RETURN(SO);
               }
            } else if ( strcmp(nel->name,"Mesh_IJK") == 0 || 
                        strcmp(nel->name,"NewMesh_IJK") == 0) { 
                                             /* Get Da FaceSetList */ 
               if (LocalHead) 
                  fprintf (SUMA_STDERR,"%s:\nGetting FaceSetList...\n", 
                                             FuncName);
               if (!SUMA_Mesh_IJK_nel2Mesh_IJK(SO, nel)) {
                  SUMA_SL_Err("Failed in SUMA_Mesh_IJK_nel2Mesh_IJK");
                  SUMA_Free_Surface_Object(SO); SO = NULL; SUMA_RETURN(SO);
               }
            } else if ( strcmp(nel->name,"SurfaceVolumeParent") == 0) { 
                                                   /* Get Da FaceSetList */ 
               if (LocalHead) fprintf (SUMA_STDERR,"%s:\nGetting VolPar...\n", 
                                             FuncName);
               if (!SUMA_VolPar_nel2SOVolPar(SO, nel)) {
                  SUMA_SL_Err("Failed in SUMA_VolPar_nel2SOVolPar");
                  SUMA_Free_Surface_Object(SO); SO = NULL; SUMA_RETURN(SO);
               }
            } else {
               fprintf (SUMA_STDERR,
                        "Warning %s:\n nel (%s) unknown, ignoring it.\n", 
                        FuncName, nel->name);
            }
            break;
         default:
            SUMA_SL_Err("Don't know what to make of this "
                        "group element, ignoring.");
            break;
      }
   }
   
   if (!SO->NodeList || !SO->FaceSetList) { 
               /* perhaps you'll remove this condition in the future ...*/
      SUMA_SL_Err("Looks like NodeList and/or FaceSetList "
                  "not in group. Balking.\n");
      SUMA_Free_Surface_Object(SO); SO = NULL; SUMA_RETURN(SO);
   }
   
   /* check on all elements that need to be loaded */   
   tmp = NI_get_attribute(ngr, "Mesh_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->FaceSetList) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for FaceSetList element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) SUMA_Mesh_IJK_nel2Mesh_IJK(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }

   tmp = NI_get_attribute(ngr, "NodeList_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->NodeList) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for NodeList element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) SUMA_NodeXYZ_nel2NodeXYZ(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }
   
   tmp = NI_get_attribute(ngr, "facenormals_idcode_str");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->FaceNormList) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for facenormals element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) xxxxxx(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }
     
   tmp = NI_get_attribute(ngr, "Node_Normals_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->NodeNormList) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for Node_Normals element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) xxxxxx(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }
   
   tmp = NI_get_attribute(ngr, "Polygon_Area_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->PolyArea) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for Polygon_Area element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) xxxxxx(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }
   
   tmp = NI_get_attribute(ngr, "SUMA_Edge_List_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->EL) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for Edge_List element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) xxxxxx(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }

   tmp = NI_get_attribute(ngr, "SUMA_Node_Face_Member_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->MF) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for Node_Face_Member element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) xxxxxx(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }
   
   tmp = NI_get_attribute(ngr, "SUMA_Node_First_Neighb_Element_ID");
   if (!SUMA_IS_EMPTY_STR_ATTR(tmp))  { 
      if (!SO->FN) { /* element was not part of this group, usually recover from other files on disk, for the moment, warning */
         fprintf (SUMA_STDERR,"Warning %s:\n group %s called for Node_First_Neighb element_ID %s which was not found.\n", FuncName, ngr->name, tmp);  
         /* 
         in the future, try to load from separate file
         nel = SUMA_Find_nel_File(tmp); (for example) xxxxxx(SO, nel) ;  NI_free_nel(nel); nel = NULL; 
         */     
      }
   }
   
   
   if (SO->MF && SO->MF->idcode_str) {
      NI_set_attribute(ngr, "SUMA_Node_Face_Member_Element_ID", SO->MF->idcode_str);
   } else {
      NI_set_attribute(ngr, "SUMA_Node_Face_Member_Element_ID", SUMA_EMPTY_ATTR);
   }
   
   if (SO->FN && SO->FN->idcode_str) {
      NI_set_attribute(ngr, "SUMA_Node_First_Neighb_Element_ID", SO->FN->idcode_str);
   } else {
      NI_set_attribute(ngr, "SUMA_Node_First_Neighb_Element_ID", SUMA_EMPTY_ATTR);
   }
   

    
   SUMA_RETURN(SO);
}

/*  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Begin OpenDX functions <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */


SUMA_OPEN_DX_STRUCT ** SUMA_Free_OpenDX_StructVec(SUMA_OPEN_DX_STRUCT **dxv, int nobj)
{
   static char FuncName[]={"SUMA_Free_OpenDX_StructVec"};
   int i;
   
   SUMA_ENTRY;
   
   if (!dxv) SUMA_RETURN(NULL);
   for (i=0; i<nobj; ++i) {
      dxv[i] = SUMA_Free_OpenDX_Struct(dxv[i]);
   }
   SUMA_free(dxv); 
   SUMA_RETURN(NULL);
}


SUMA_Boolean SUMA_OpenDX_Write(char *fname, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_OpenDX_Write"};
   
   SUMA_ENTRY;
   
   SUMA_SL_Err("Not supported yet");
   
   SUMA_RETURN(NOPE);
}





/*!
   \brief returns structure containing object of a certain name in dxv
   do not free returned structure since it is a copy of pointer in dxv
*/
SUMA_OPEN_DX_STRUCT *SUMA_Find_OpenDX_Object_Name(SUMA_OPEN_DX_STRUCT **dxv, int iop, char *nm, int *nf)
{
   static char FuncName[]={"SUMA_Find_OpenDX_Object_Name"};
   int i;
   SUMA_OPEN_DX_STRUCT *dx = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *nf = 0;
   for (i=0; i<iop; ++i) {
      if (strstr(dxv[i]->object, nm)) { 
         if (!dx) dx = dxv[i]; 
         ++ (*nf);
      }
   }
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Found %d objects\n", FuncName, *nf);
   SUMA_RETURN(dx);
}
/*!
   \brief returns structure containing object of a certain class in dxv
   do not free returned structure since it is a copy of pointer in dxv
*/
SUMA_OPEN_DX_STRUCT *SUMA_Find_OpenDX_Object_Class(SUMA_OPEN_DX_STRUCT **dxv, int iop, char *nm, int *nf)
{
   static char FuncName[]={"SUMA_Find_OpenDX_Object_Class"};
   int i;
   SUMA_OPEN_DX_STRUCT *dx = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *nf = 0;
   for (i=0; i<iop; ++i) {
      if (strstr(dxv[i]->class, nm)) { 
         if (!dx) dx = dxv[i]; 
         ++ (*nf);
      }
   }
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Found %d objects\n", FuncName, *nf);
   SUMA_RETURN(dx);
}


char * SUMA_OpenDX_Read_CruiseVolHead(char *fname, THD_3dim_dataset *dset, int LoadData)
{
   static char FuncName[]={"SUMA_OpenDX_Read_CruiseVolHead"};
   int i = 0, nf, iop, chunk, End, bs, doff, data_type=SUMA_notypeset;
   THD_ivec3 iv3;
   THD_mat33 m33;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   float ep[3], sp[3];
   char *scom=NULL, form[10], swp[10], orstr[10], xfov[100], yfov[100], zfov[100], *prefix = NULL, *dsetheadname = NULL;
   SUMA_OPEN_DX_STRUCT **dxv=NULL, *dxp=NULL, *dxc=NULL, *dxf=NULL, *dxa=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !fname) {
      SUMA_SL_Err("NULL fname || NULL dset!");
      SUMA_RETURN(NOPE);
   }
   
   prefix = SUMA_AfniPrefix(fname, NULL, NULL, NULL);
   if( !THD_filename_ok(prefix) ) {
      SUMA_SL_Err("Bad prefix");
      SUMA_RETURN(NOPE);
   } 
   dsetheadname = SUMA_append_string(prefix,"+orig.HEAD");
   if (SUMA_filexists(dsetheadname)) {
      SUMA_SL_Err("Bad prefix, output dset exists");
      SUMA_RETURN(NOPE);
   }
   SUMA_free(dsetheadname); dsetheadname = NULL;
   
   SUMA_LH("Reading objects");
   dxv = SUMA_OpenDX_Read(fname, &iop);
   if (!dxv) {
      SUMA_SL_Err("Failed to read DX file.");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH("Checking for field object");
   dxf = SUMA_Find_OpenDX_Object_Class(dxv, iop, "field", &nf);
   if (!dxf || nf != 1) { SUMA_SL_Err("Failed to find one and only one field object"); goto CLEAN_EXIT; }
   dxp = dxc = NULL;   
   for (i=0; i<dxf->n_comp; ++i) {
      if (strstr(dxf->comp_name[i],"positions")) {
         dxp = SUMA_Find_OpenDX_Object_Name(dxv, iop, dxf->comp_value[i], &nf); 
         if (!dxp || nf != 1) { SUMA_SL_Err("Failed to find one and only one positions object"); goto CLEAN_EXIT; }
      }
      if (strstr(dxf->comp_name[i],"connections")) {
         dxc = SUMA_Find_OpenDX_Object_Name(dxv, iop, dxf->comp_value[i], &nf); 
         if (!dxc || nf != 1) { SUMA_SL_Err("Failed to find one and only one connections object"); goto CLEAN_EXIT; }
      }
      if (strstr(dxf->comp_name[i],"data")) {
         dxa = SUMA_Find_OpenDX_Object_Name(dxv, iop, dxf->comp_value[i], &nf); 
         if (!dxa || nf != 1) { SUMA_SL_Err("Failed to find one and only one data object"); goto CLEAN_EXIT; }
      }
   }
   
   if (!dxp || !dxv || !dxa) {
      SUMA_SL_Err("Failed to find necessary objects"); SUMA_RETURN(NOPE);
   }
   
   /* sanity */
   SUMA_LH("Sanity checks");
   if (!dxa->data_format) {
      SUMA_SL_Err("Not a binary data file!");
      goto CLEAN_EXIT; 
   }
   if (!dxa->data) {
      SUMA_SL_Err("No data file!");
      goto CLEAN_EXIT; 
   }
   if (dxp->n_counts != 3) {
      SUMA_SL_Err("counts field must have 3 values");
      goto CLEAN_EXIT; 
   }
   if (dxp->n_delta != 9) {
      SUMA_SL_Err("delta must contain 9 elements!");
      goto CLEAN_EXIT; 
   }
   
   /* form the command */
   SUMA_LH("Forming command");
   /* 3d what? */
   chunk = 0;
   form[0] = '\0';
   data_type = SUMA_CTypeName2VarType(dxa->type);
   switch (data_type) {
      case SUMA_float:
         sprintf(form,"3Df");
         chunk = sizeof(float);
         break;
      case SUMA_double:
         sprintf(form,"3Dd");
         chunk = sizeof(double);
         break;
      case SUMA_int:
         sprintf(form,"3Di");
         chunk = sizeof(int);
         break;
      case SUMA_short:
         sprintf(form,"3Ds");
         chunk = sizeof(short);
         break;
      case SUMA_byte:
         sprintf(form,"3Db");
         chunk = sizeof(byte);
         break;
      default:
         SUMA_SL_Err("No support for this type");
         goto CLEAN_EXIT; 
         break;
   }
   /* byte ordering */
   SUMA_WHAT_ENDIAN(End);
   bs = 0;
   swp[0] = '\0';
   if (End != dxa->data_format) {
      SUMA_LH("swapping needed");
      bs = 1;
      switch (chunk) {
         case 1:
            break;
         case 2:
            sprintf(swp,"-2swap");
            break;
         case 4:
            sprintf(swp,"-4swap");
            break;
         case 8:
            sprintf(swp,"-8swap");
            break;
         default:
            SUMA_SL_Err("Patchunk!");
            break;
      }
   }
   if (dxa->data_off)  doff = (int)strtol(dxa->data_off,NULL, 10);
   else doff = 0;
   
   /* direction that varies the fastest (x) is in the last delta
      count is ordered like delta. I do not know about origin yet,
      I assume that origin follows the ordering of count
      see http://opendx.npaci.edu/docs/html/pages/usrgu068.htm 
      for reference, someday....*/
   
   /* number of voxels */
   LOAD_IVEC3( nxyz   , dxp->counts[2]    , dxp->counts[1]    , dxp->counts[0] ) ;
   
   /* orientation */
   /* find closest orientation in RAI, delta matrix does allow for obliqueness (non diagonal matrix) but we ignore it */
   LOAD_MAT(m33, 
               dxp->delta[6], dxp->delta[7], dxp->delta[8], 
               dxp->delta[3], dxp->delta[4], dxp->delta[5],
               dxp->delta[0], dxp->delta[1], dxp->delta[2] );
   orixyz = THD_matrix_to_orientation( m33 );

   /* dimensions */
   {  
      float fb[3]; 
      SUMA_NORM_VEC((&(dxp->delta[6])),3, fb[0]);
      SUMA_NORM_VEC((&(dxp->delta[3])),3, fb[1]);
      SUMA_NORM_VEC((&(dxp->delta[0])),3, fb[2]);
      LOAD_FVEC3( dxyz   , fb[0]    , fb[1]    , fb[2]    ) ;
      SUMA_sizeto3d_2_deltaHEAD(orixyz, &dxyz);
   }
   
   /* origin , assumed to be center of first voxel*/
   LOAD_FVEC3( orgxyz , dxp->origin[2]    , dxp->origin[1] , dxp->origin[0]  ) ;
   SUMA_originto3d_2_originHEAD(orixyz, &orgxyz);
    
   /* start point (edge of origin voxel) and end point (opposite to start ) */
   sp[0] = dxp->origin[2] + SUMA_ABS(dxyz.xyz[0]) / 2.0;
   sp[1] = dxp->origin[1] + SUMA_ABS(dxyz.xyz[1]) / 2.0;
   sp[2] = dxp->origin[0] + SUMA_ABS(dxyz.xyz[2]) / 2.0;
   ep[0] = dxp->origin[2] + (nxyz.ijk[0] - 0.5) * SUMA_ABS(dxyz.xyz[0]);
   ep[1] = dxp->origin[1] + (nxyz.ijk[1] - 0.5) * SUMA_ABS(dxyz.xyz[1]);
   ep[2] = dxp->origin[0] + (nxyz.ijk[2] - 0.5) * SUMA_ABS(dxyz.xyz[2]);
   SUMA_orcode_to_orstring (orixyz.ijk[0], orixyz.ijk[1], orixyz.ijk[2], orstr);
   sprintf(xfov," -xFOV %.2f%c-%.2f%c", sp[0], orstr[0], ep[0], orstr[3]);
   sprintf(yfov," -yFOV %.2f%c-%.2f%c", sp[1], orstr[1], ep[1], orstr[4]);
   sprintf(zfov," -zFOV %.2f%c-%.2f%c", sp[2], orstr[2], ep[2], orstr[5]);
  
   
   scom = (char *)SUMA_calloc((strlen(dxa->data)+500), sizeof(char));
   sprintf(scom,"to3d %s %s %s %s -prefix %s %s:%d:0:%d:%d:%d:%s ", swp, xfov, yfov, zfov, prefix, form, doff, dxp->counts[2], dxp->counts[1], dxp->counts[0], dxa->data);
   
   
   if (dset) { /* form the dset header */
         int nvals_read = 0;
         EDIT_dset_items( dset ,
                            ADN_prefix      , prefix ,
                            ADN_datum_all   , data_type ,
                            ADN_nxyz        , nxyz ,
                            ADN_xyzdel      , dxyz ,
                            ADN_xyzorg      , orgxyz ,
                            ADN_xyzorient   , orixyz ,
                            ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                            ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                            ADN_type        , HEAD_ANAT_TYPE ,
                            ADN_func_type   , ANAT_BUCK_TYPE ,
                          ADN_none ) ;

         if (LoadData) {
            void *vec=NULL;
            if (!(vec = SUMA_BinarySuck(dxa->data, data_type, dxa->data_format, doff, -1, &nvals_read))) {
               SUMA_SL_Err("Failed to read data file"); goto CLEAN_EXIT;
            }
            EDIT_substitute_brick( dset , 0 , data_type , vec) ;      
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Read %d values from file.\n", FuncName, nvals_read);
            /* DSET_write(dset) ; */
         }
   }
   

   
   CLEAN_EXIT:
   dxv = SUMA_Free_OpenDX_StructVec(dxv, iop);
   if (prefix) SUMA_free(prefix); prefix = NULL;
   
   SUMA_RETURN(scom);
}

/*!
   \brief reads an OpenDX surface. Sample surfaces provided by Aaron Carass
   
   The following fields are set in SO:
   SO->NodeDim
   SO->FaceSetDim
   SO->NodeList
   SO->FaceSetList
   SO->N_Node;
   SO->N_FaceSet;
   SO->Name;
   SO->FileType;
   SO->FileFormat

*/
SUMA_Boolean SUMA_OpenDX_Read_SO(char *fname, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_OpenDX_Read_SO"};
   int i = 0, nf, iop;
   SUMA_OPEN_DX_STRUCT **dxv=NULL, *dxp=NULL, *dxc = NULL, *dxf = NULL, *dxo = NULL;
   SUMA_Boolean ans = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !fname) {
      SUMA_SL_Err("NULL fname || NULL SO!");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH("Reading objects");
   dxv = SUMA_OpenDX_Read(fname, &iop);
   if (!dxv) {
      SUMA_SL_Err("Failed to read DX file.");
      SUMA_RETURN(NOPE);
   }
   
   
   SUMA_LH("Checking for field object");
   dxf = SUMA_Find_OpenDX_Object_Class(dxv, iop, "field", &nf);
   if (!dxf || nf != 1) { SUMA_SL_Err("Failed to find one and only one field object"); goto CLEAN_EXIT; }
   /* get names of objects that contain positions and connections */
   dxp = dxc = dxo = NULL;   
   for (i=0; i<dxf->n_comp; ++i) {
      if (strstr(dxf->comp_name[i],"positions")) {
         dxp = SUMA_Find_OpenDX_Object_Name(dxv, iop, dxf->comp_value[i], &nf); 
         if (!dxp || nf != 1) { SUMA_SL_Err("Failed to find one and only one positions object"); goto CLEAN_EXIT; }
      }
      if (strstr(dxf->comp_name[i],"connections")) {
         dxc = SUMA_Find_OpenDX_Object_Name(dxv, iop, dxf->comp_value[i], &nf); 
         if (!dxc || nf != 1) { SUMA_SL_Err("Failed to find one and only one connections object"); goto CLEAN_EXIT; }
      }
      if (strstr(dxf->comp_name[i],"origin")) {
         dxo = SUMA_Find_OpenDX_Object_Name(dxv, iop, dxf->comp_value[i], &nf); 
         if (!dxo || nf != 1) { SUMA_SL_Err("Failed to find one and only one origin object.\nOrigin ignored"); }
      }
   }
   
   if (!dxp || !dxv) {
      SUMA_SL_Err("Failed to find necessary objects"); goto CLEAN_EXIT;
   }
   
   SUMA_LH("checking...");
   if (SUMA_CTypeName2VarType (dxp->type) != SUMA_float) {
      SUMA_SL_Err("Expected floats for positions"); goto CLEAN_EXIT;
   }
   if (dxp->bad_data) {
      SUMA_SL_Err("Problem reading data for positions"); goto CLEAN_EXIT;
   }
   if (dxp->rank != 1) {
      SUMA_SL_Err("Expected rank of 1 for positions"); goto CLEAN_EXIT;
   }
   if (dxp->shape != 3) {
      SUMA_SL_Err("Expected rank of 3 for positions"); goto CLEAN_EXIT;
   }
   if (SUMA_CTypeName2VarType (dxc->type) != SUMA_int) {
      SUMA_SL_Err("Expected ints for connections"); goto CLEAN_EXIT;
   }
   if (dxc->bad_data) {
      SUMA_SL_Err("Problem reading data for connections"); goto CLEAN_EXIT;
   }   
   if (dxc->rank != 1) {
      SUMA_SL_Err("Expected rank of 1 for connections"); goto CLEAN_EXIT;
   }
   if (dxc->shape != 3) {
      SUMA_SL_Err("Expected rank of 3 for connections"); goto CLEAN_EXIT;
   }
   /* if dxo */
   if (dxo) {
      if (SUMA_CTypeName2VarType (dxo->type) != SUMA_float) {
         SUMA_SL_Err("Expected floats for origin.\nOrigin ignored"); dxo = NULL;
      }
      if (!dxo->datap || dxo->shape * dxo->items != 3) {
         SUMA_SL_Err("Unknown origin format, ignoring origin"); dxo = NULL;
      }  
   }
   
   SUMA_LH("Take the gold");
   SO->FileType = SUMA_OPENDX_MESH;
   SO->FileFormat = SUMA_ASCII;
   
   SO->NodeDim = dxp->shape;
   SO->NodeList = (float *)dxp->datap; dxp->datap = NULL; /* datap will not be freed at end anymore */
   SO->N_Node = dxp->items;
   
   SO->FaceSetDim = dxc->shape;
   SO->FaceSetList = (int *)dxc->datap; dxc->datap = NULL; /* datap will not be freed at end anymore */
   SO->N_FaceSet = dxc->items;;
   SO->Name = SUMA_StripPath(fname);
   
   if (dxo) {
      float *fvec=(float*)dxo->datap;
      SUMA_LH("Adding origin");
      i = 0;
      while (i < dxp->items*dxp->shape) {
         SO->NodeList[i] += fvec[0]; ++i;
         SO->NodeList[i] += fvec[1]; ++i;
         SO->NodeList[i] += fvec[2]; ++i;
      }
   }
   
   /* all is well here */
   ans = YUP;

   CLEAN_EXIT:
   SUMA_LH("Frenching dxv");
   for (i=0; i<iop; ++i) {
      dxv[i] = SUMA_Free_OpenDX_Struct(dxv[i]);
   }
   if (dxv) SUMA_free(dxv); dxv = NULL;
   SUMA_RETURN(YUP);
}
/*  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> END OpenDX functions <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */
