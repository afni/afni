/* This file should not contai functions that REQUIRE the compilation or the headers of SUMA.
This file might be compiled and used by AFNI 
*/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "xutil.h"
#include "SUMA_Algorithms.h"
#include "SUMA_DataSets.h"
#include "SUMA_Macros.h"   /* This one's generic */

#if defined SUMA_TEST_DATA_SETS_STAND_ALONE
#define STAND_ALONE
#elif defined SUMA_Test_DSET_IO_STANDALONE
#define STAND_ALONE
#elif defined SUMA_ConvertDset_STANDALONE
#define STAND_ALONE
#else
#endif

#if defined SUMA_COMPILED
   /* compiling with SUMA headers, include SUMA stuff */
   #include "SUMA_suma.h"
#endif

#ifdef STAND_ALONE
   #if defined SUMA_COMPILED
      /* need to define these global variables because function calls are made to functions in files that declare these variables as extern */
      SUMA_CommonFields *SUMAg_CF;
      SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
      SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                          SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
      int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
      SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
      int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   #endif
#else
  #if defined SUMA_COMPILED
      extern SUMA_CommonFields *SUMAg_CF;
      extern int SUMAg_N_DOv; 
      extern SUMA_DO *SUMAg_DOv;
  #endif 
#endif

#ifndef SUMA_COMPILED
   /* define the macros for InOut debug and memory allocation and freeing */
   #define SUMA_free( p ) \
	   free( p )
   #define SUMA_calloc( nmemb,  size) \
	   calloc( nmemb, size)
   #define SUMA_malloc(size) \
	   malloc(size)
   #define SUMA_realloc( ptr, size) \
	   realloc( ptr, size)
   #define SUMA_RETURN(m_rvar) { \
      return(m_rvar);\
   }
   #define SUMA_RETURNe { \
      return;\
   }
   #define SUMA_ENTRY { \
   }
   #define SUMA_mainENTRY { }
   
   #define SUMA_STDERR stderr
   #define SUMA_STDOUT stdout
   
   #define SUMA_SLP_Err SUMA_S_Err
   #define SUMA_SL_Err SUMA_S_Err
   #define SUMA_L_Err SUMA_S_Err
   
   #define SUMA_SLP_Note SUMA_S_Note
   #define SUMA_SL_Note SUMA_S_Note
   #define SUMA_L_Note SUMA_S_Note
   
   #define SUMA_SLP_Warn SUMA_S_Warn
   #define SUMA_SL_Warn SUMA_S_Warn
   #define SUMA_L_Warn SUMA_S_Warn
   
   #define SUMA_SLP_Crit SUMA_S_Crit
   #define SUMA_SL_Crit SUMA_S_Crit
   #define SUMA_L_Crit SUMA_S_Crit
   
   #define SUMA_LH(msg) {\
      if (LocalHead) fprintf (SUMA_STDERR, "%s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Warn(msg) {\
      fprintf (SUMA_STDERR, "Warning %s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Note(msg) {\
      fprintf (SUMA_STDERR, "Notice %s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Err(msg) {\
      fprintf (SUMA_STDERR, "Error %s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Crit(msg) {\
      fprintf (SUMA_STDERR, "Critical error %s:\n %s\n", FuncName, msg);  \
   }
    
#endif


/*! Taken from filexists 
returns 1 if file can be read/found
*/
int SUMA_filexists (char *f_name)
{/*SUMA_filexists*/
    FILE *outfile;
    static char FuncName[]={"SUMA_filexists"};
   
   SUMA_ENTRY;

    outfile = fopen (f_name,"r");
    if (outfile == NULL) {
       SUMA_RETURN(0); 
   }
    else {
       fclose (outfile); 
   }
    
   SUMA_RETURN(1);
       
}/*SUMA_filexists*/


/*!
   \brief function that tests whether a string contains N numbers
   
   \param str (char *) null terminated string
   \param N (void *) This is an integer in disguise
   \return 1: If str is NULL or N numbers were found in str
*/
int SUMA_isNumString (char *s, void *p)
{
   static char FuncName[]={"SUMA_isNumString"};
   char *endp, *strtp;
   int nd, N;
   int eos, FoundTip;
   double d;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(1); 
   
   N = (int)p;
   
   /* clean s by removing trailing junk then replacing non characters by space*/
   FoundTip = 0;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.'  && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = 1;
      }
   }
   
   if (LocalHead) fprintf (stderr, "%s: string now:%s:\n", FuncName, s);
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = 0;
   while (!eos) {
      d = strtod(strtp, &endp);
      if (LocalHead) fprintf (stderr, "%s: value %f, ERANGE: %d, EDOM %d, errno %d\n", FuncName, d, ERANGE, EDOM, errno); 
      

      if (endp == strtp && *endp=='\0') { 
         eos = 1;
      } else {
         strtp = endp;
         ++nd;
      }
   }
   
   if (LocalHead) fprintf (stderr,"%s: Read %d/%d values.\n", FuncName, nd,N);
   if (N != nd) {
      SUMA_RETURN(0);
   } else {
      SUMA_RETURN(1);
   }
   
}   

/*!
   \brief function that parses a string of numbers into a float vector
   
   \param str (char *) null terminated string
   \param fv (float*) vector where values will be stored
   \param N (int) This is the number of values desired
   \return int: This is the number of values read. 
      The function will register in fv more that N values 
      (to keep from running over preallocated space), but it
      will return the full number of values found.
      
      -1 in case of error
   \sa SUMA_isNumString
*/
int SUMA_StringToNum (char *s, float *fv, int N)
{
   static char FuncName[]={"SUMA_StringToNum"};
   char *endp, *strtp;
   int nd;
   int eos, FoundTip;
   double d;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(0); 
      
   /* clean s by removing trailing junk then replacing non characters by space*/
   FoundTip = 0;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.' && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = 1;
      }
   }
   
   if (LocalHead) fprintf (stderr, "%s: string now:%s:\n", FuncName, s);
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = 0;
   while (!eos) {
      d = strtod(strtp, &endp);
      if (LocalHead) fprintf (stderr, "%s: value %f, ERANGE: %d, EDOM %d, errno %d\n", FuncName, d, ERANGE, EDOM, errno); 
      
      if (endp == strtp && *endp=='\0') { 
         eos = 1;
      } else {
         if (nd < N) fv[nd] = (float)d;
         strtp = endp;
         ++nd;
      }
   }
   
   if (LocalHead) fprintf (stderr,"%s: Read %d/%d values.\n", FuncName, nd, N);
   
   SUMA_RETURN(nd);
   
}   

/*!
   Creates a NI elem. to store surface data 
   N_el is the number of data elements stored in each column
   N_el can be the number of nodes for example
*/
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* MeshParent_idcode, 
                          char * GeomParent_idcode, int N_el, 
                          char *filename, char *thisidcode)
{
   static char FuncName[]={"SUMA_NewNel"};
   NI_element *nel=NULL;
   char idcode[SUMA_IDCODE_LENGTH], *namecode;
   
   SUMA_ENTRY;
   
   nel = NI_new_data_element(SUMA_Dset_Type_Name(dtp), N_el);
   
   /* assign an idcode */
   if (!thisidcode) {
      if (!filename) {
         UNIQ_idcode_fill(idcode);
         NI_set_attribute (nel, "idcode", idcode); /* create one */
      } else { 
         namecode = UNIQ_hashcode(filename);  /* from filename */
         NI_set_attribute (nel, "idcode", namecode); SUMA_free(namecode);
      }
   } else {
      NI_set_attribute (nel, "idcode", thisidcode);
   }
   
   /* set the idcodes of the parents */
   if (MeshParent_idcode) {
      NI_set_attribute (nel, "MeshParent_idcode", MeshParent_idcode);
   } else {
      NI_set_attribute (nel, "MeshParent_idcode", NULL);
   }
   if (GeomParent_idcode) {
      NI_set_attribute (nel, "GeomParent_idcode", GeomParent_idcode);
   } else {
      NI_set_attribute (nel, "GeomParent_idcode", NULL);
   }
  
   if (filename) NI_set_attribute (nel, "filename", filename);
   
   SUMA_RETURN(nel);  
}

/*!
   
   Adds an attribute to nel for that explains the last added column's 
   contents. You should call this function after each SUMA_AddNelCol call 
   col_attr (void *) is a pointer to a structure containing special
   attributes of the data in the last column added. At the moment,
   this pointer is not being used, but you can imagine needing it if
   you have to store certain stats or parameters that go with each column.
       
*/
int SUMA_AddColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col_attr, int col_index)
{
   static char FuncName[]={"SUMA_AddColAttr"};
   char Name[500], Attr[500];
   
   SUMA_ENTRY;
   
   if (!nel) SUMA_RETURN(0);
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }

   /* save the type of the column */
   sprintf(Name, "TypeCol_%d", col_index);
   NI_set_attribute ( nel, Name, SUMA_Col_Type_Name(ctp));
   
   sprintf(Attr, "AttrCol_%d", col_index);
   switch (ctp) {
      case SUMA_NODE_INDEX:
         /* form the string of attributes for this column */
         NI_set_attribute ( nel, Attr, NULL);
         break;
     
      case SUMA_NODE_INT:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_ILABEL:
         NI_set_attribute ( nel, Attr, NULL);
         break;   
      
      case SUMA_NODE_X:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Y:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Z:
         NI_set_attribute ( nel, Attr, NULL);
         break; 
         
      case SUMA_NODE_R:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_G:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_B:
         NI_set_attribute ( nel, Attr, NULL);
         break;    
       
      case SUMA_NODE_A:
         NI_set_attribute ( nel, Attr, NULL);
         break;
         
      case SUMA_NODE_Rb:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Gb:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_Bb:
         NI_set_attribute ( nel, Attr, NULL);
         break;    
       
      case SUMA_NODE_Ab:
         NI_set_attribute ( nel, Attr, NULL);
         break;
      
      case SUMA_NODE_FLOAT:
         NI_set_attribute ( nel, Attr, NULL);
         break;     
      
      case SUMA_NODE_3C:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
         
      case SUMA_NODE_STRING:
         NI_set_attribute ( nel, Attr, NULL);
         break;     
      
      case SUMA_NODE_CX:
         NI_set_attribute ( nel, Attr, NULL);
         break;  
      
      default:
         NI_set_attribute ( nel, Attr, NULL);
         break;          
   }
   
   SUMA_RETURN(1);   
}

/*!
   Adds some generic attributes.
   For the moment, the range is added for numeric columns 
   if col_index is -1, then it is assumed that the attributes are for the latest column added (vec_num -1)
*/
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, int stride, int col_index) 
{
   static char FuncName[]={"SUMA_AddGenColAttr"};
   static char stmp[500], Name[500];
   float amin = 0.0, amax = 0.0, *fv;
   int aminloc = -1, amaxloc = -1, *iv;
   byte *bv;
   SUMA_ENTRY;
   
   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   sprintf(Name, "RangeCol_%d", col_index);

   if (!col) { 
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      sprintf(stmp, "0 0 -1 -1");
   } else {
      switch (SUMA_ColType2TypeCast(ctp)) {
         case SUMA_int:
            iv = (int *)col;
            SUMA_MIN_MAX_VEC_STRIDE(iv ,nel->vec_filled, amin, amax, aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_float:
            fv = (float *)col;
            SUMA_MIN_MAX_VEC_STRIDE(fv ,nel->vec_filled, amin, amax, aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%f %f %d %d", amin, amax, aminloc, amaxloc);
            break;
         case SUMA_byte:
            bv = (byte *)col;
            SUMA_MIN_MAX_VEC_STRIDE(bv ,nel->vec_filled, amin, amax, aminloc, amaxloc, stride);
            snprintf(stmp, 500*sizeof(char),"%d %d %d %d", (int)amin, (int)amax, aminloc, amaxloc);
            break;
         case SUMA_string:
            NI_add_column_stride ( nel, NI_STRING, (char **)col, stride );
            stmp[0] = '\0';
            break;
         default:
            fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
            SUMA_RETURN(0);
            break; 
      }
   }
   
   NI_set_attribute ( nel, Name, stmp);
   
   SUMA_RETURN(1);  
}

/*!
   \brief Sets the column range values
   col_index can be -1 if you want the attributes of the last column
*/
int SUMA_GetColRange(NI_element *nel, int col_index, float range[2], int loc[2])
{
   static char FuncName[]={"SUMA_GetColRange"};
   char *rs = NULL, Name[500];
   float nums[4];
   
   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (col_index < 0) col_index = nel->vec_num-1;
   if (col_index < 0 || !nel->vec_num ) { SUMA_SL_Err("No columns in data set!"); SUMA_RETURN(0); }
   if (nel->vec_num <= col_index) { SUMA_SL_Err("col_index >= nel->vec_num!"); SUMA_RETURN(0); }
   
   SUMA_ENTRY;
   
   sprintf(Name, "RangeCol_%d", col_index);
   rs = NI_get_attribute(nel, Name);
   
   if (!rs) { SUMA_SL_Err("No range field."); SUMA_RETURN(0); }
   if (SUMA_StringToNum(rs, nums, 4) != 4) { SUMA_SL_Err("Failed to read 4 nums from range."); SUMA_RETURN(0); }
   range[0] = nums[0]; range[1] = nums[1]; 
   loc[0] = (int)nums[2]; loc[1] = (int)nums[3];
      
   SUMA_RETURN(1);
}

/*!
   Adds a column to Nel
   The vectors added are nel->vec_len long so col should contain at least
   nel->vec_len * stride elements.
   
   What to do when filling up to nel->vec_filled only ? Does one need to 
   write another version of NI_add_column_stride ? (see file niml/niml_element)
   Is the use of vec_filled what I think it is ?
   
   Mar 23 04: Made modifications to NI_add_column and _stride
   so that data are copied up to nel->vec_filled instead of 
   nel->vec_len if nel->vec_filled is > 0 and < vec_len
   
   If you wish to allocate space for a column (nel->vec_len long)
   then pass NULL for col
*/

int SUMA_AddNelCol ( NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddNelCol"};
   SUMA_ENTRY;
   
   if (!nel) { SUMA_SL_Err("Null Nel"); SUMA_RETURN(0); }
   if (!col) { 
      /* Do not complain, that is not a bad thing.
      People can use this to allocate for a column
      without filling it up */
      /* SUMA_SL_Err("Null Col"); SUMA_RETURN(0); */
   }
   
   switch (SUMA_ColType2TypeCast(ctp)) {
      case SUMA_int:
         NI_add_column_stride ( nel, NI_INT, (int *)col, stride);
         break;
      case SUMA_float:
         NI_add_column_stride ( nel, NI_FLOAT, (float *)col, stride );      
         break;
      case SUMA_byte:
         NI_add_column_stride ( nel, NI_BYTE, (byte *)col, stride );      
         break;
      case SUMA_string:
         NI_add_column_stride ( nel, NI_STRING, (char **)col, stride );
         break;
      default:
         fprintf (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   
   /* set some generic attributes */
   SUMA_AddGenColAttr (nel, ctp, col, stride, -1);
   /* add the attributes of that column */
   SUMA_AddColAttr (nel, ctp, col_attr, -1);
   
   SUMA_RETURN(1);
}

/*!
   \brief Function to fill the contents of a pre-existing column 
   created with SUMA_AddNelCol.  
   if vec_filled > 0 && vec_filled <= vec_len, filling is done
   up to vec_filled.
   vec_filled must be set BEFORE YOU CALL THIS FUNCTION
*/
int SUMA_FillNelCol (NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride) 
{  
   static char FuncName[]={"SUMA_FillNelCol"};
   int icol = -1;
   int *iv, N_i;
   SUMA_ENTRY;
   
   /* find the index into vec of the column of type ctp,
      complain if you find more than 1 */
   iv = SUMA_GetColIndex (nel, ctp, &N_i);
   if (N_i != 1) {
      SUMA_SL_Err("Found more than one column.\n");
      SUMA_RETURN(-1);
   }
   icol = iv[0];
   SUMA_free(iv); iv = NULL;
   
   /* Now use the function NI_fill_column_stride
   that I have yet to write in nim_element.c
   (a modification of NI_add_column_stride) and
   you're all set */ 
   switch (SUMA_ColType2TypeCast(ctp)) {
      case SUMA_int:
         NI_fill_column_stride ( nel, NI_INT, (int *)col, icol, stride);
         break;
      case SUMA_float:
         NI_fill_column_stride ( nel, NI_FLOAT, (float *)col, icol, stride );      
         break;
      case SUMA_byte:
         NI_fill_column_stride ( nel, NI_BYTE, (byte *)col, icol, stride );      
         break;
      case SUMA_string:
         NI_fill_column_stride ( nel, NI_STRING, (char **)col, icol, stride );
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(0);
         break; 
   }
   /* set some generic attributes */
   SUMA_AddGenColAttr (nel, ctp, col, stride, icol);
   /* add the attributes of that column */
   SUMA_AddColAttr (nel, ctp, col_attr, icol);
   
   SUMA_RETURN(1);
}
 
SUMA_VARTYPE SUMA_ColType2TypeCast (SUMA_COL_TYPE ctp) 
{
   static char FuncName[]={"SUMA_ColType2TypeCast"};
   
   SUMA_ENTRY;
   
   switch (ctp) {
      case SUMA_NODE_INT:
      case SUMA_NODE_ILABEL:
      case SUMA_NODE_INDEX:
         SUMA_RETURN(SUMA_int);
         break;
      case SUMA_NODE_FLOAT:
      case SUMA_NODE_CX:
      case SUMA_NODE_X:
      case SUMA_NODE_Y:
      case SUMA_NODE_Z:
      case SUMA_NODE_R:
      case SUMA_NODE_G:
      case SUMA_NODE_B:
      case SUMA_NODE_A:
      case SUMA_NODE_3C:
         SUMA_RETURN(SUMA_float);      
         break;
      case SUMA_NODE_BYTE:
      case SUMA_NODE_Rb:
      case SUMA_NODE_Gb:
      case SUMA_NODE_Bb:
      case SUMA_NODE_Ab:
         SUMA_RETURN(SUMA_byte);          
         break;
      case SUMA_NODE_STRING:
         SUMA_RETURN(SUMA_string);
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         SUMA_RETURN(SUMA_notypeset);
         break; 
   }
   
   SUMA_RETURN(SUMA_notypeset);
}
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr)
{
   static char FuncName[]={"SUMA_Dset_Format_Name"};
   
   SUMA_ENTRY;
   
   switch(fr) {
      case SUMA_ERROR_DSET_FORMAT:
         SUMA_RETURN ("Error_Dset_Format");
         break;
      case SUMA_NO_DSET_FORMAT:
         SUMA_RETURN ("Dset_Format_Undefined");
         break;
      case SUMA_ASCII_NIML:
         SUMA_RETURN ("Ascii_Niml");
         break;
      case SUMA_BINARY_NIML:
         SUMA_RETURN ("Binary_Niml");
         break;
      case SUMA_NIML:
         SUMA_RETURN ("Niml");
         break;
      case SUMA_1D:
         SUMA_RETURN ("Afni_1D");
         break;
      default:
         SUMA_RETURN("Cowabonga-gimlauron");
         break;
   }   
   
}

SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name)
{
   static char FuncName[]={"SUMA_Dset_Format"};

   SUMA_ENTRY;
   
   if (!strcmp(Name,"Error_Dset_Format")) SUMA_RETURN (SUMA_ERROR_DSET_FORMAT);
   if (!strcmp(Name,"Dset_Format_Undefined")) SUMA_RETURN (SUMA_NO_DSET_FORMAT);
   if (!strcmp(Name,"Ascii_Niml")) SUMA_RETURN (SUMA_ASCII_NIML);
   if (!strcmp(Name,"Binary_Niml")) SUMA_RETURN (SUMA_BINARY_NIML);
   if (!strcmp(Name,"Niml")) SUMA_RETURN (SUMA_NIML);
   if (!strcmp(Name,"Afni_1D")) SUMA_RETURN (SUMA_1D);
   SUMA_RETURN(SUMA_ERROR_DSET_FORMAT);
}

char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp)
{
   static char FuncName[]={"SUMA_Dset_Type_Name"};
   
   SUMA_ENTRY;
   
   switch (tp) {
      case SUMA_NO_DSET_TYPE:
         SUMA_RETURN("Dset_Type_Undefined");
         break;
      case SUMA_ERROR_DSET_TYPE:
         SUMA_RETURN("Error_Dset_Type");
         break;
      case SUMA_NODE_BUCKET:
         SUMA_RETURN("Node_Bucket");
         break;
      case SUMA_NODE_ROI:
         SUMA_RETURN("Node_ROI");
         break;
      case SUMA_NODE_RGB:
         SUMA_RETURN("Node_RGB");
         break;
      case SUMA_NODE_RGBA:
         SUMA_RETURN("Node_RGBA");
         break;
      case SUMA_NODE_RGBb:
         SUMA_RETURN("Node_RGBb");
         break;
      case SUMA_NODE_RGBAb:
         SUMA_RETURN("Node_RGBAb");
         break;
      case SUMA_NODE_XYZ:
         SUMA_RETURN("Node_XYZ");
         break;
      case SUMA_VIEWER_SETTING:
         SUMA_RETURN("Viewer_Visual_Setting");
         break;
      case SUMA_NODE_CONVEXITY:
         SUMA_RETURN("Node_Convexity");
         break;
      default:
         SUMA_RETURN("Cowabonga-gothdo");
         break;
   }
}

SUMA_DSET_TYPE SUMA_Dset_Type (char *Name)
{
   static char FuncName[]={"SUMA_Dset_Type"};
   
   SUMA_ENTRY;
   
   if (!strcmp(Name,"Dset_Type_Undefined")) SUMA_RETURN (SUMA_NO_DSET_TYPE);
   if (!strcmp(Name,"Error_Dset_Type")) SUMA_RETURN (SUMA_ERROR_DSET_TYPE);
   if (!strcmp(Name,"Node_Bucket")) SUMA_RETURN (SUMA_NODE_BUCKET);
   if (!strcmp(Name,"Node_ROI")) SUMA_RETURN (SUMA_NODE_ROI);
   if (!strcmp(Name,"Node_RGB")) SUMA_RETURN (SUMA_NODE_RGB);
   if (!strcmp(Name,"Node_RGBA")) SUMA_RETURN (SUMA_NODE_RGBA);
   if (!strcmp(Name,"Node_RGBb")) SUMA_RETURN (SUMA_NODE_RGBb);
   if (!strcmp(Name,"Node_RGBAb")) SUMA_RETURN (SUMA_NODE_RGBAb);
   if (!strcmp(Name,"Node_XYZ")) SUMA_RETURN (SUMA_NODE_XYZ);
   if (!strcmp(Name,"Viewer_Visual_Setting")) SUMA_RETURN (SUMA_VIEWER_SETTING);
   if (!strcmp(Name,"Cowabonga")) SUMA_RETURN (SUMA_ERROR_DSET_TYPE);
   if (!strcmp(Name,"Node_Convexity")) SUMA_RETURN (SUMA_NODE_CONVEXITY);
   SUMA_RETURN (SUMA_ERROR_DSET_TYPE);
}

char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp)
{
   static char FuncName[]={"SUMA_Col_Type_Name"};
   
   SUMA_ENTRY;
   
   switch (tp) {
      case SUMA_NO_COL_TYPE:
         SUMA_RETURN("Col_Type_Undefined");
         break;
      case SUMA_ERROR_COL_TYPE:
         SUMA_RETURN ("Error_Col_Type");
         break;
      case SUMA_NODE_INT:
         SUMA_RETURN("Generic_Int");
         break;
      case SUMA_NODE_INDEX:
         SUMA_RETURN("Node_Index");
         break;
      case SUMA_NODE_ILABEL:
         SUMA_RETURN("Node_Index_Label");
         break;
      case SUMA_NODE_FLOAT:
         SUMA_RETURN("Generic_Float");
         break;
      case SUMA_NODE_3C:
         SUMA_RETURN("XYZ_triplets");
         break;
      case SUMA_NODE_X:
         SUMA_RETURN("X_coord");
         break;
      case SUMA_NODE_Y:
         SUMA_RETURN("Y_coord");
         break;
      case SUMA_NODE_Z:
         SUMA_RETURN("Z_coord");
         break;
      case SUMA_NODE_R:
         SUMA_RETURN("R_col");
         break;
      case SUMA_NODE_G:
         SUMA_RETURN("G_col");
         break;
      case SUMA_NODE_B:
         SUMA_RETURN("B_col");
         break;
      case SUMA_NODE_STRING:
         SUMA_RETURN("Generic_String");
         break;
      case SUMA_NODE_CX:
         SUMA_RETURN("Convexity");
         break;
      
      default:
         SUMA_RETURN("Cowabonga-Jo");
         break;
   }
   
}

SUMA_COL_TYPE SUMA_Col_Type (char *Name)
{
   static char FuncName[]={"SUMA_Col_Type"};
   
   SUMA_ENTRY;
      
   if (!strcmp(Name,"Col_Type_Undefined")) SUMA_RETURN (SUMA_NO_COL_TYPE);
   if (!strcmp(Name,"Error_Col_Type")) SUMA_RETURN (SUMA_ERROR_COL_TYPE);
   if (!strcmp(Name,"Generic_Int")) SUMA_RETURN (SUMA_NODE_INT);
   if (!strcmp(Name,"Node_Index")) SUMA_RETURN (SUMA_NODE_INDEX);
   if (!strcmp(Name,"Generic_Float")) SUMA_RETURN (SUMA_NODE_FLOAT);
   if (!strcmp(Name,"XYZ_triplets")) SUMA_RETURN (SUMA_NODE_3C);
   if (!strcmp(Name,"X_coord")) SUMA_RETURN (SUMA_NODE_X);
   if (!strcmp(Name,"Y_coord")) SUMA_RETURN (SUMA_NODE_Y);
   if (!strcmp(Name,"Z_coord")) SUMA_RETURN (SUMA_NODE_Z);
   if (!strcmp(Name,"R_col")) SUMA_RETURN (SUMA_NODE_R);
   if (!strcmp(Name,"G_col")) SUMA_RETURN (SUMA_NODE_G);
   if (!strcmp(Name,"B_col")) SUMA_RETURN (SUMA_NODE_B);
   if (!strcmp(Name,"Generic_String")) SUMA_RETURN (SUMA_NODE_STRING);
   if (!strcmp(Name,"Convexity")) SUMA_RETURN (SUMA_NODE_CX);
   // if (!strcmp(Name,"")) SUMA_RETURN ();
   SUMA_RETURN (SUMA_ERROR_COL_TYPE);

}


int SUMA_ShowNel (NI_element *nel)
{
   static char FuncName[]={"SUMA_ShowNel"};
   NI_stream nstdout;
   
   SUMA_ENTRY;
   
   nstdout = NI_stream_open( "fd:1","w");
   if( nstdout == NULL ){ 
      fprintf(stderr,"%s: Can't open fd:1\n", FuncName); 
      SUMA_RETURN(0); 
   }
   fprintf (stdout, "\n-----------nel stdout begin-----------\n");
   NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
   fprintf (stdout, "\n-----------nel stdout end  -----------\n");
   NI_stream_close(nstdout);
   
   SUMA_RETURN(1);
}



int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i)
{
   static char FuncName[]={"SUMA_GetColIndex"};
   int *iv=NULL, i=0;
   char stmp[500], *atr;

   SUMA_ENTRY;
   
   *N_i = -1;
   iv = (int *)SUMA_calloc(nel->vec_num, sizeof(int));
   if (!iv) {
      SUMA_RETURN(NULL);
   }   
   
   *N_i = 0;
   for (i=0; i < nel->vec_num; ++i) {
      sprintf(stmp,"TypeCol_%d",i);
      atr = NI_get_attribute(nel,stmp);
      if (SUMA_Col_Type(atr) == tp) {
         iv[*N_i] = i;
         *N_i = *N_i + 1;

      }
   }
   
   SUMA_RETURN(iv);
}


/*!
   \brief adds a history note to the ni-element
   
   \param nel (NI_element *)
   \param CallingFunc (char *) name of function / program calling
   \param N_arg (int) number of arguments in arg
   \param arg (char **) vector of strings 
   \return ans (int) 0 Failed
                     1 OK 
*/
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg)
{
   static char FuncName[]={"SUMA_AddNelHist"}; 
   char *stmp=NULL, *stmpn = NULL, *sold=NULL;
   int N_tot, i;
   
   SUMA_ENTRY;
   
   if (!arg) SUMA_RETURN(0);
   if (!arg[0]) SUMA_RETURN(0);
   if (!nel) SUMA_RETURN(0);
   if (!N_arg) SUMA_RETURN(0);
   
   sold = NI_get_attribute(nel, "History");
   if (sold) stmp = SUMA_append_string (sold, "\n");
   
   if (CallingFunc) {
      stmp = SUMA_append_replace_string (stmp, CallingFunc, "",1);
      stmp = SUMA_append_replace_string (stmp, ":", " ", 1);
   }
   
   for (i=0; i < N_arg; ++i) 
      stmp = SUMA_append_replace_string (stmp, arg[i], " ", 1);
      
   if (stmp) {
      NI_set_attribute ( nel, "History", stmp);
      SUMA_free(stmp);
   }
   
   SUMA_RETURN(1);
}

/*!
   \brief truncates a string to a certain length.
   Adds ... as the last characters of the string
   The original string is not modified.
    
   s_tr = SUMA_truncate_string(s1, n);
   
   - free returned pointer with: if(s_tr) SUMA_free(s_tr);
*/
char *SUMA_truncate_string(char *buf, int n)
{
   static char FuncName[]={"SUMA_truncate_string"};
   char *atr = NULL;
   int i;

   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   if (n < 5) {
      fprintf(stderr,"Error %s:\nNot worth the effort. N < 5.", FuncName);
      SUMA_RETURN(NULL);
   }
   
   if (strlen(buf) <= n) {
      atr = (char *) SUMA_calloc(strlen(buf)+2, sizeof(char));
      sprintf(atr, "%s", buf);
      SUMA_RETURN (atr);
   }else {
      atr = (char *) SUMA_calloc(n+2, sizeof(char));
      i=0;
      while (i < n - 3) {
         atr[i] = buf[i];
         ++i;
      }
      atr[i-3] = atr[i-2] = atr[i-1] = '.';
      atr[i] = '\0';
   }
   
   SUMA_RETURN(atr);  
}

/*!
   \brief returns a copy of a null terminated string . 
   s_cp = SUMA_copy_string(s1);
   
   - free returned pointer with: if(s_cp) SUMA_free(s_cp);
*/
char *SUMA_copy_string(char *buf)
{
   static char FuncName[]={"SUMA_copy_string"};
   char *atr = NULL;
   int i;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   atr = (char *) SUMA_calloc(strlen(buf)+2, sizeof(char));
   
   i=0;
   while (buf[i]) {
      atr[i] = buf[i];
      ++i;
   }
   atr[i] = '\0';
   
   SUMA_RETURN(atr);  
}

/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_string(s1, s2);
  
   - s1 and s2 are copied into a new string
   -free returned pointer with:  if(s_ap) SUMA_free(s_ap);
   - None of the strings passed to the function
   are freed.
   
   \sa SUMA_append_replace_string
*/
char * SUMA_append_string(char *s1, char *s2)
{
   static char FuncName[]={"SUMA_append_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1;

   
   SUMA_ENTRY;
   
   if (!s1 && !s2) SUMA_RETURN(NULL);
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   atr = (char *) SUMA_calloc(N_s1+N_s2+2, sizeof(char));
   
   /* copy first string */
   i=0;
   cnt = 0;
   while (s1[i]) {
      atr[cnt] = s1[i];
      ++i;
      ++cnt;
   }
   
   i=0;
   while (s2[i]) {   
      atr[cnt] = s2[i];
      ++i;
      ++cnt;
   }
   atr[cnt] = '\0';
   
   SUMA_RETURN(atr);  
}    


/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_replace_string(s1, s2, spc, whichTofree);
  
  \param s1 (char *) string 1
  \param s2 (char *) string 2
  \param spc (char *) spacing string
  \param whichTofree (int) 0 free none, 
                           1 free s1
                           2 free s2
                           3 free s1 and s2
   \return s_ap (char *) a string formed by "%s%s%s", s1, spc, s2
   
   - s1 and s2 are copied into a new string with spc in between
   - s1 (but not s2 or spc ) IS FREED inside this function
   -free returned pointer with:  if(s_ap) SUMA_free(s_ap);
   
   \sa SUMA_append_string
*/
char * SUMA_append_replace_string(char *s1, char *s2, char *Spc, int whichTofree)
{
   static char FuncName[]={"SUMA_append_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1, N_Spc=0;

   
   SUMA_ENTRY;
   
   if (!s1 && !s2) SUMA_RETURN(NULL);
   
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   if (!Spc) N_Spc = 0;
   else N_Spc = strlen(Spc);
   
   atr = (char *) SUMA_calloc(N_s1+N_s2+N_Spc+2, sizeof(char));
   
   /* copy first string */
   i=0;
   cnt = 0;
   if (s1) {
      while (s1[i]) {
         atr[cnt] = s1[i];
         ++i;
         ++cnt;
      }
   }
     
   i=0;
   if (Spc) {
      while (Spc[i]) {
         atr[cnt] = Spc[i];
         ++i;
         ++cnt;
      }
   }
   
   i=0;
   if (s2) {
      while (s2[i]) {   
         atr[cnt] = s2[i];
         ++i;
         ++cnt;
      }
   }
   atr[cnt] = '\0';
   
   switch (whichTofree) {
      case 0:
         break;
      case 1:
         if (s1) free(s1);
         break;
      case 2:
         if (s2) free(s2);
         break;
      case 3:
         if (s1) free(s1);
         if (s2) free(s2);
         break;
      default: 
         fprintf(stderr, "Error %s:\nBad freeing parameter\n"
                         "No variables were freed.\n",
                         FuncName);
         break;
   }  

   SUMA_RETURN(atr);  
}    

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \return SS (SUMA_STRING *) pointer to string structure with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the correct dimension and 
   a null character is placed at the end.
   \sa SUMA_SS2S
*/
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring)
{
   static char FuncName[]={"SUMA_StringAppend"};
   int N_inc = 0, N_cur = 0;
   int N_chunk = 1000;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SS) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Allocating for SS.\n", FuncName);
      SS = (SUMA_STRING *) SUMA_malloc (sizeof(SUMA_STRING));
      SS->s = (char *) SUMA_calloc (N_chunk, sizeof(char));
      SS->s[0] = '\0';
      SS->N_alloc = N_chunk;
      SUMA_RETURN (SS);
   }
   
   if (newstring) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Appending to SS->s.\n", FuncName);
      N_inc = strlen (newstring);
      N_cur = strlen (SS->s);
      if (SS->N_alloc < N_cur+N_inc+1) { /* must reallocate */
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Must reallocate for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+N_inc+N_chunk+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
      }
      /* append */
      sprintf (SS->s, "%s%s", SS->s, newstring);
   }else {
      /* shrink SS->s to small size */
      N_cur = strlen (SS->s);
      if (SS->N_alloc > N_cur+1) {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Shrink realloc for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /*put a null at the end */
         SS->s[SS->N_alloc-1] = '\0';
      }
   }
   
   SUMA_RETURN (SS);

}

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   A variable argument version of SUMA_StringAppend
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \param ..... the remaining parameters a la printf manner
   \return SS (SUMA_STRING *) pointer to string structure with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the correct dimension and 
   a null character is placed at the end.
   
   - For this function, the formatted length of newstring should not be > than MAX_APPEND-1 
   If that occurs, the string will be trunctated and no one should get hurt
   
   NOTE: DO NOT SEND NULL pointers in the variable argument parts or crashes will occur on SUN
   Such NULL pointers do not result in null vararg_ptr and cause a seg fault in vsnprintf
   
   \sa SUMA_StringAppend
   \sa SUMA_SS2S
*/

#define MAX_APPEND 1000

SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... )
{
   static char FuncName[]={"SUMA_StringAppend_va"};
   char sbuf[MAX_APPEND];
   int nout;
   va_list vararg_ptr ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SS) {
      SUMA_LH("NULL SS");
      /* let the other one handle this */
      SUMA_RETURN (SUMA_StringAppend(SS,newstring));
   }
   
   if (newstring) {
      SUMA_LH("newstring ...");
      /* form the newstring and send it to the olde SUMA_StringAppend */
      va_start( vararg_ptr ,  newstring) ;
      if (strlen(newstring) >= MAX_APPEND -1 ) {
         SUMA_SL_Err("newstring too long.\nCannot use SUMA_StringAppend_va");
         SUMA_RETURN(SUMA_StringAppend(SS,"Error SUMA_StringAppend_va: ***string too long to add ***"));
      }
      if (LocalHead) {
         SUMA_LH("Calling vsnprintf");
         if (vararg_ptr) {
            SUMA_LH("Non NULL vararg_ptr");
         } else {
            SUMA_LH("NULL vararg_ptr");
         }
      }
      nout = vsnprintf (sbuf, MAX_APPEND * sizeof(char), newstring, vararg_ptr); 
      if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Calling va_end, nout = %d\n", FuncName, nout);
      va_end(vararg_ptr);  /* cleanup */
      
      if (nout < 0) {
         SUMA_SL_Err("Error reported by  vsnprintf");
         SUMA_RETURN(SUMA_StringAppend(SS,"Error SUMA_StringAppend_va: ***Error reported by  vsnprintf"));
      }
      if (nout >= MAX_APPEND) {
         SUMA_SL_Warn("String trunctated by vsnprintf");
         SUMA_StringAppend(SS,sbuf);
         SUMA_RETURN(SUMA_StringAppend(SS,"WARNING: ***Previous string trunctated because of its length. ***"));
      }
      SUMA_LH("Calling StringAppend");
      SUMA_RETURN (SUMA_StringAppend(SS,sbuf));
   }else {
      SUMA_LH("NULL newstring");
      /* let the other one handle this */
      SUMA_RETURN (SUMA_StringAppend(SS,newstring));
   }
   
   /* should not be here */
   SUMA_RETURN (NULL);

}

      

/*** 
   Functions to deal with SUMA's datasets
   They don't quite belong in SUMA_DataSets.c because
   they are specific to SUMA
***/

/*!
   \brief look for a dataset with a particular idcode
*/
SUMA_DSET * SUMA_FindDset (char *idcode, DList *DsetList)
{
   static char FuncName[]={"SUMA_FindDset"};
   SUMA_DSET *dset = NULL, *dsetf = NULL;
   char *dsetid;
   DListElmt *el=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
    
   dsetf = NULL;
   if (!DsetList) { SUMA_SL_Err("NULL DsetList"); SUMA_RETURN(dsetf); }
   if (!DsetList->size) { SUMA_RETURN(dsetf); }
   if (!idcode) { SUMA_SL_Err("NULL idcode"); SUMA_RETURN(dsetf); }
   el = NULL;
   do { 
      if (!el) el = dlist_head(DsetList);
      else el = dlist_next(el);
      dset = (SUMA_DSET *)el->data;
      if (dset->nel) {
         dsetid = NI_get_attribute(dset->nel, "idcode");
         if (dsetid) {
            if (!strcmp(dsetid, idcode))  dsetf = dset; /* match */
         } 
      } 
   } while ( (el != dlist_tail(DsetList)) && !dsetf ); 
   SUMA_RETURN(dsetf);
}

/*!
   \brief Function to free a Dset 
   
   - YOU SHOULD NOT FREE individual dsets yourself
   That is done by the dlist_destroy function
*/
void SUMA_FreeDset(void *vp)
{
   static char FuncName[]={"SUMA_FreeDset"};
   int i;
   SUMA_DSET *dset;
   
   SUMA_ENTRY;
   
   dset = (SUMA_DSET *)vp;
   
   if (!dset) SUMA_RETURNe;
   if (dset->N_links) {
      SUMA_SL_Err("dset structure has links to it.\n"
                  "structure not freed.\n"
                  "That is a now a memory leak.\n");
      SUMA_RETURNe;
   }
   if (dset->nel) NI_free_element(dset->nel); dset->nel = NULL; /* you can keep ni_free from freeing a nel->vec[i] 
                                                          vector by copying nel->vec[i] to a pointer then
                                                          setting nel->vec[i] = NULL */ 
   #if 0
   if (dset->filename) SUMA_free(dset->filename);
   if (dset->Label) SUMA_free(dset->Label);
   if (dset->idcode) SUMA_free(dset->idcode);
   if (dset->domain_idcode) SUMA_free(dset->domain_idcode);
   if (dset->NodeDef) SUMA_free(dset->NodeDef);
   if (dset->N_mx) SUMA_free(dset->N_mx);
   if (dset->data) {
      for (i=0; i<dset->N_sub; ++i) {
         if (dset->data[i]) SUMA_free(dset->data[i]);
      }
   }
   if (dset->tp) SUMA_free(dset->tp);
   #endif
   
   SUMA_free(dset); dset = NULL;
   
   SUMA_RETURNe;
}

/*!
   \brief Pointer linking and unliking functions.
   They are to be used for all pointer linking
   in SUMA. For now they are just used for the datasets
   but I plan to move all "inode" stuff in here.
   
   These functions are generalized versions of SUMA_LinkToDset
   and SUMA_UnlinkFromDset
*/
void *SUMA_LinkToPointer(void *ptr)
{
   static char FuncName[]={"SUMA_LinkToPointer"};
   SUMA_LinkedPtr *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ptr) {
      SUMA_SL_Err("NULL pointer");
      SUMA_RETURN(NULL);
   }
   dset = (SUMA_LinkedPtr *)ptr;
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Link Requested to pointer %p. \n"
                                      "LinkedPtrType = %d, owner_id = %s\n"
                                      "N_links was %d\n", 
                                      FuncName, dset, dset->LinkedPtrType, dset->owner_id, dset->N_links);
   dset->N_links = dset->N_links + 1;
   
   SUMA_RETURN((void *)dset);
}
void *SUMA_UnlinkFromPointer(void *ptr)
{
   static char FuncName[]={"SUMA_UnlinkFromPointer"};
   SUMA_LinkedPtr *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!ptr) {
      SUMA_SL_Err("NULL pointer");
      SUMA_RETURN(NULL);
   }
   dset = (SUMA_LinkedPtr *)ptr;
   if (LocalHead) fprintf(SUMA_STDERR, "%s:\n Unink Requested from pointer %p.\n"
                                       "LinkedPtrType = %d, owner_id = %s\n"
                                       "N_links was %d\n", 
                                       FuncName, dset, dset->LinkedPtrType, dset->owner_id, dset->N_links);
   if (dset->N_links > 0) dset->N_links = dset->N_links - 1;
   else if (dset->N_links == 0) { SUMA_SL_Err("N_links ==0\nThis should not happen here.\n");   SUMA_RETURN(NULL); }
   
   SUMA_RETURN(NULL);
}
#if 0
SUMA_DSET *SUMA_LinkToDset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_LinkToDset"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Link Requested to dset %p. N_links was %d\n", FuncName, dset, dset->N_links);
   dset->N_links = dset->N_links + 1;
   
   SUMA_RETURN(dset);
}
SUMA_DSET *SUMA_UnlinkFromDset(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_UnlinkFromDset"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s:\n Unink Requested from dset %p. N_links was %d\n", FuncName, dset, dset->N_links);
   if (dset->N_links > 0) dset->N_links = dset->N_links - 1;
   else if (dset->N_links == 0) { SUMA_SL_Err("N_links ==0\nThis should not happen here.\n");   SUMA_RETURN(NULL); }
   
   SUMA_RETURN(NULL);
}
#endif
SUMA_DSET * SUMA_NewDsetPointer(void)
{
   static char FuncName[]={"SUMA_NewDsetPointer"};
   SUMA_DSET *dset = NULL;
   SUMA_ENTRY;

   dset = (SUMA_DSET *)SUMA_malloc(sizeof(SUMA_DSET));
   if (!dset) {
      SUMA_SL_Err("Failed to allocate for dset");
      SUMA_RETURN(dset);
   }

   /* initialize */
   dset->nel = NULL;
   dset->N_links = 0;
   dset->owner_id[0] = '\0';
   dset->LinkedPtrType = SUMA_LINKED_DSET_TYPE;
   SUMA_RETURN(dset);
}
/*!
   \brief Function to allocate and initialize a dataset and add 
   it to the list of data sets .
   
   dset = SUMA_CreateDsetPointer (
                              char *filename, char *idcode,
                              char *domain_idcode, int N_Alloc
                              ) ;    
   \param filename (char *): Name of dset, typically, filename with path
   \param idcode (char *): identifier to use for dset.
                               If idcode is NULL then a new one is
                               generated from filename.
   \param domain_idcode(char *): idcode of domain. (used for both MeshDomain 
                              and geometry domain)
   \return dset (SUMA_DSET *): Element of DsetList containing dataset that 
                              was created by the function.

   -  This function does the following:
      xxxxxxx
      
   \sa SUMA_AddNelCol
   \sa SUMA_InsertDsetPointer
*/
SUMA_DSET * SUMA_CreateDsetPointer (  
                              char *filename, SUMA_DSET_TYPE tp,
                              char *idcode,
                              char *domain_idcode,
                              int N_Alloc 
                              ) 
{
   static char FuncName[]={"SUMA_CreateDsetPointer"};
   int ilist = -1, i = -1;
   char *Label=NULL;
   SUMA_DSET *dset=NULL;
   DListElmt *Elm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!filename) { SUMA_SL_Err("Need Dset filename"); SUMA_RETURN(dset); }
   
   #if 0
   if (N_Alloc != N_NodeDef) {
      SUMA_SL_Err("Not ready to deal with N_Alloc != N_NodeDef");
      SUMA_RETURN(dset);
   }
   #endif
   
   /* make sure that this dataset is unique */
   if (!idcode) { /* No id is given yet */
      idcode = UNIQ_hashcode(filename);   /* form one from the filename */
   }
   
   dset = SUMA_NewDsetPointer();
   
   dset->nel = SUMA_NewNel(tp, domain_idcode, domain_idcode, N_Alloc, filename, idcode);
   
   Label = SUMA_truncate_string(filename, 20); 
   NI_set_attribute(dset->nel, "label", Label); SUMA_free(Label); Label = NULL;
   
   NI_set_attribute(dset->nel, "sorted_node_def", "Yes");
   
   #if 0
   /* add the NodeDef column */
   if (NodeDef) {
      SUMA_LH("Adding NodeDef");
      if (!SUMA_AddNelCol(dset->nel, SUMA_NODE_INDEX, (void *)(NodeDef), NULL, 1)) {
         SUMA_SL_Err("Failed to add column.\n");
         SUMA_free(dset); dset = NULL;
         SUMA_RETURN(NULL);
      }
      if (sorted_node_def) NI_set_attribute(dset->nel, "sorted_node_def", "Yes");
      else NI_set_attribute(dset->nel, "sorted_node_def", "No");
   } else {
      if (!sorted_node_def) {
         SUMA_SL_Err("Makes no sense.\nNo NodeDef vector and sorted_node_def != 1\nYou crazy ?\n");
         SUMA_free(dset); dset = NULL;
         SUMA_RETURN(NULL);
      }
      NI_set_attribute(dset->nel, "sorted_node_def", "Yes");
   }
   #endif
   SUMA_RETURN(dset);
}    

/*!
   \brief inserts a dataset pointer into the DsetList
   \param dset (SUMA_DSET *)
   \param DsetList (DList *): List of dset objects.
*/   
int SUMA_InsertDsetPointer (SUMA_DSET *dset, DList *DsetList)
{
   static char FuncName[]={"SUMA_InsertDsetPointer"};
   char *s=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!DsetList)  { SUMA_SL_Err("Need Dset List"); SUMA_RETURN(0); }
   if (!dset) { SUMA_SL_Err("dset is NULL"); SUMA_RETURN(0); }
   if (!dset->nel) { SUMA_SL_Err("dset->nel is NULL\nMothing to do"); SUMA_RETURN(0); }
    
   /* does this set exist ? */
   s= SDSET_ID(dset); if (!s) { SUMA_SL_Err("dset has no idcode.\n"); SUMA_RETURN(0); }
   if (SUMA_FindDset (s,  DsetList)) {
      SUMA_SL_Err("Dset with similar idcode\n"
                  "found in list.\n");
      SUMA_RETURN(0);
   }
   
   /* insert into list */
   if (dlist_ins_next(DsetList, dlist_tail(DsetList), (void *)dset) < 0) {
      SUMA_SL_Err("Failed to insert dset into list");
      SUMA_FreeDset(dset); dset = NULL;
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

char *SUMA_ShowMeSome (void *dt, SUMA_VARTYPE tp, int N_dt, int mxshow)
{
   static char FuncName[]={"SUMA_ShowMeSome"};
   int i, imx, firsthalf, secondhalf;
   double *dtd;
   int *dti;
   byte *dtb;
   char **dts;
   float *dtf;
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
   
   if (mxshow > N_dt) mxshow = N_dt;
   
   if (mxshow <= 0) SUMA_RETURN(s);
   
   firsthalf = mxshow / 2;
   secondhalf = mxshow - firsthalf;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   switch (tp) {
      case SUMA_double:
         dtd = (double*)dt;
         for (i=0; i <= firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%f, ", dtd[i]);
         if (firsthalf+1 < mxshow-secondhalf) SS = SUMA_StringAppend_va(SS, "..., ");
         for (i=mxshow-secondhalf; i<mxshow-1; ++i) SS = SUMA_StringAppend_va(SS, "%f, ", dtd[i]);
         SS = SUMA_StringAppend_va(SS, "%f", dtd[i]);
         break;
      case SUMA_float:
         dtf = (float*)dt;
         for (i=0; i <= firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%f, ", dtf[i]);
         if (firsthalf+1 < mxshow-secondhalf) SS = SUMA_StringAppend_va(SS, "..., ");
         for (i=mxshow-secondhalf; i<mxshow-1; ++i) SS = SUMA_StringAppend_va(SS, "%f, ", dtf[i]);
         SS = SUMA_StringAppend_va(SS, "%f", dtf[i]);
         break;
      case SUMA_int:
         dti = (int*)dt;
         for (i=0; i <= firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dti[i]);
         if (firsthalf+1 < mxshow-secondhalf) SS = SUMA_StringAppend_va(SS, "..., ");
         for (i=mxshow-secondhalf; i<mxshow-1; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dti[i]);
         SS = SUMA_StringAppend_va(SS, "%d", dti[i]);
         break;
      case SUMA_byte:
         dtb = (byte*)dt;
         for (i=0; i <= firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dtb[i]);
         if (firsthalf+1 < mxshow-secondhalf) SS = SUMA_StringAppend_va(SS, "..., ");
         for (i=mxshow-secondhalf; i<mxshow-1; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", dtb[i]);
         SS = SUMA_StringAppend_va(SS, "%d", dtb[i]);
         break;
      case SUMA_string:
         dts = (char **)dt;
         for (i=0; i <= firsthalf; ++i) SS = SUMA_StringAppend_va(SS, "%s, ", dts[i]);
         if (firsthalf+1 < mxshow-secondhalf) SS = SUMA_StringAppend_va(SS, "..., ");
         for (i=mxshow-secondhalf; i<mxshow-1; ++i) SS = SUMA_StringAppend_va(SS, "%s, ", dts[i]);
         SS = SUMA_StringAppend_va(SS, "%s", dts[i]);
         break;
      default:
         SS = SUMA_StringAppend_va(SS, "Type not supported.");
   }  

   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

/*!
   \brief Function to return info on SUMA_DSET
   
   - You must free the returned string on your own
*/
char *SUMA_DsetInfo (SUMA_DSET *dset, int detail)
{
   static char FuncName[]={"SUMA_DsetInfo"};
   int i;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (dset) {
      if (dset->nel) {
         SS = SUMA_StringAppend_va(SS, "Dset Name: %s (%d)\n", 
            dset->nel->name, SUMA_Dset_Type(dset->nel->name));
         if (SDSET_FILENAME(dset)) 
            SS = SUMA_StringAppend_va(SS, "filename: %s\n", SDSET_FILENAME(dset));
         else SS = SUMA_StringAppend_va(SS, "filename: NULL\n");
         if (SDSET_LABEL(dset)) 
            SS = SUMA_StringAppend_va(SS, "label: %s\n", SDSET_LABEL(dset));
         else SS = SUMA_StringAppend_va(SS, "label: NULL\n");
         if (SDSET_ID(dset)) 
            SS = SUMA_StringAppend_va(SS, "idcode: %s\n", SDSET_ID(dset));
         else SS = SUMA_StringAppend_va(SS, "idcode: NULL\n");
         if (SDSET_IDGDOM(dset)) 
            SS = SUMA_StringAppend_va(SS, "GeomParent_idcode: %s\n", SDSET_IDGDOM(dset));
         else SS = SUMA_StringAppend_va(SS, "GeomParent_idcode: NULL\n");
         if (SDSET_IDMDOM(dset)) 
            SS = SUMA_StringAppend_va(SS, "MeshParent_idcode: %s\n", SDSET_IDMDOM(dset));
         else SS = SUMA_StringAppend_va(SS, "MeshParent_idcode: NULL\n");
         
         SS = SUMA_StringAppend_va(SS, "vec_num (N_subsets): %d\n", dset->nel->vec_num);
         SS = SUMA_StringAppend_va(SS, "vec_filled (N_NodeDef): %d\n", dset->nel->vec_filled);
         SS = SUMA_StringAppend_va(SS, "vec_len (N_Alloc): %d\n", dset->nel->vec_len);
         if (SDSET_SORTED(dset)) 
            SS = SUMA_StringAppend_va(SS, "sorted_node_def: %s\n", SDSET_SORTED(dset));
         else SS = SUMA_StringAppend_va(SS, "sorted_node_def: NULL\n");

         /* where is the node index (NodeDef) column ? */
         SS = SUMA_StringAppend_va(SS, "Looking for Node Index (NodeDef) column:\n");
         {   
            int *iv, N_i;
            iv = SUMA_GetColIndex (dset->nel, SUMA_NODE_INDEX, &N_i);
            if (!iv) {
               SS = SUMA_StringAppend_va(SS, "\tFailed to find a Node Index column.\n");
            } else {
               SS = SUMA_StringAppend_va(SS, "\t%d Node Index columns found.\n",
                                          N_i);
               SUMA_free(iv); iv = NULL;
            }
         }
         for (i=0; i < dset->nel->vec_num; ++i) {
            SS = SUMA_StringAppend_va(SS, "vec[%d]:\n", i);
            sprintf (stmp,"TypeCol_%d", i);
            SS = SUMA_StringAppend_va(SS, "\tColumn %d's name: %s\n",
                                       i, NI_get_attribute(dset->nel, stmp));
            ctp = SUMA_Col_Type(NI_get_attribute(dset->nel, stmp));
            sprintf(stmp,"attrCol_%d", i);
            SS = SUMA_StringAppend_va(SS, "\tColumn %d's attribute: %s\n", 
                                       i, NI_get_attribute(dset->nel, stmp));
            if (dset->nel->vec[i]) {
               s = SUMA_ShowMeSome((void*)(  dset->nel->vec[i]), 
                                             SUMA_ColType2TypeCast (ctp) 
                                             , dset->nel->vec_len, 5);
               SS = SUMA_StringAppend_va(SS, "         %s\n", s); SUMA_free(s); s = NULL;
            } else SS = SUMA_StringAppend_va(SS, "         NULL\n");
         }
         if (detail) { /* write the entire element to SS */
            NI_stream ns = NI_stream_open("str:", "w");
            NI_write_element(ns, dset->nel, NI_TEXT_MODE);
            SS = SUMA_StringAppend(SS, "\n Full NI element in text mode:\n"); 
            SS = SUMA_StringAppend(SS, NI_stream_getbuf(ns)); /* don't use StringAppend_va because it does not all 
                                                                the concatenation of very long strings. */
            SS = SUMA_StringAppend(SS, "\n");
            NI_stream_close(ns);
         }
      } else {
         SS = SUMA_StringAppend(SS, "NULL dset->nel.");
      }
   } else {
      SS = SUMA_StringAppend(SS, "NULL dset.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

/*!
   \brief Returns a pointer to the column containing NodeDef
   (if it exists, in dset). Do not free this pointer!
*/
int * SUMA_GetNodeDef(SUMA_DSET *dset)
{
   static char FuncName[]={"SUMA_GetNodeDef"};
   int *iv, N_i, *NodeDef = NULL;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   iv = SUMA_GetColIndex (dset->nel, SUMA_NODE_INDEX, &N_i);
   if (!iv) {
      SUMA_LH("No such column found.");
      SUMA_RETURN(NULL);
   } else {
      SUMA_LH("Column found.");
      NodeDef = (int*)(dset->nel->vec[iv[0]]);
      if (N_i > 1) {
         SUMA_SL_Warn("Found more than one node index vector.\nReturning first one found.\n");
      }
      SUMA_free(iv); iv = NULL;
   }
   
   SUMA_RETURN(NodeDef); 
}

/*!
   \brief Look for datasets statifying the following:
      type SUMA_NODE_CONVEXITY 
      idcode_str for a geometry domain and a mesh domain
   \param ReturnDsetPointer if 1 then return pointer is to dset element (SUMA_DSET *)
                            if 0 then return pointer is to Cx (float *)
   \return A POINTER copy to Cx (float *)
*/    
void * SUMA_GetCx(char *idcode_str, DList *DsetList, int ReturnDsetPointer) 
{
   static char FuncName[]={"SUMA_GetCx"};
   float *Cx = NULL;
   char *tp_name, *idg, *idm;
   int *iv = NULL, N_i=-1, N_found = -1;
   DListElmt *el;
   SUMA_DSET *dset=NULL;

   SUMA_ENTRY;

   if (!dlist_size(DsetList)) SUMA_RETURN(Cx);
   if (!idcode_str)  SUMA_RETURN(Cx);
   tp_name = SUMA_Dset_Type_Name(SUMA_NODE_CONVEXITY);

   el = NULL;
   Cx = NULL;
   N_found = 0;
   do {
      if (!el) el = dlist_head(DsetList);
      else el = el->next;
      dset = (SUMA_DSET *)el->data;
      if (dset->nel) {
         if (strcmp(SDSET_TYPE_NAME(dset), tp_name) == 0) {
            /* matched type, now look for matching domain */
            idg = SDSET_IDGDOM(dset); idm = SDSET_IDMDOM(dset);
            if (idg && idm) {
               if (!strcmp(SDSET_IDGDOM(dset), idcode_str)) {
                  if (!N_found) {
                     /* find the column of type SUMA_NODE_CX */
                     iv = SUMA_GetColIndex (dset->nel, SUMA_NODE_CX, &N_i);
                     if (!iv) { SUMA_SL_Err("SUMA_NODE_CX not found."); SUMA_RETURN(NULL); }
                     if (N_i != 1) { SUMA_SL_Err("more than 1 SUMA_NODE_CX found."); SUMA_RETURN(NULL); }
                     Cx = (float *)dset->nel->vec[iv[0]];
                     SUMA_free(iv); iv = NULL;
                  }
                  ++ N_found;
               }
            }
         } 
      }

   }  while (el != dlist_tail(DsetList));

   if (N_found > 1) {
      SUMA_SL_Warn ("More than one convexity dataset found.\nReturning first one encountered.");
   }

   if (ReturnDsetPointer) {SUMA_RETURN((void*)dset);}
   else {SUMA_RETURN((void *)Cx);}
}

/*!
   \brief Copies the contents of a NI_element column into
   a new float vector
   V = SUMA_Col2Float (nel,  ind,  FilledOnly);
   
   \param nel (NI_element *)
   \param ind (int) index of column to be copied
   \param FilledOnly (int) 0 = allocate for and read all of the column 
                              (up to nel->vec_len)
                           1 = allocate for and read the filled portion 
                               of the column (up to nel->vec_filled)
   \return V (float *) vector (allocated by the function) containing
                     the column's contents.
 */
float * SUMA_Col2Float (NI_element *nel, int ind, int FilledOnly)
{
   static char FuncName[]={"SUMA_Col2Float"};
   char stmp[50];
   int i = -1, N_read = -1, *iv = NULL;
   float *V=NULL, *fv = NULL;
   SUMA_COL_TYPE ctp;
   SUMA_VARTYPE vtp;
   
   if (!nel) { SUMA_RETURN(NULL); }
   
   if (ind < 0 || ind > nel->vec_num - 1) {
      SUMA_SL_Err("Bad index");
      SUMA_RETURN(NULL);
   }
   
   if (FilledOnly) {
      N_read = nel->vec_filled;
   } else {
      N_read = nel->vec_len;
   }
   snprintf (stmp,50*sizeof(char),"TypeCol_%d", ind);
   ctp = SUMA_Col_Type(NI_get_attribute(nel, stmp));

   V = (float *)SUMA_malloc(sizeof(float)*N_read);
   if (!V) { SUMA_SL_Crit("Failed to allocate for V."); SUMA_RETURN(NULL); }
   vtp = SUMA_ColType2TypeCast (ctp) ;
   switch (vtp) {
      case SUMA_int:
         iv = (int *)nel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = (float)iv[i];
         break;
      case SUMA_float:
         fv = (float *)nel->vec[ind];
         for (i=0; i<N_read; ++i) V[i] = fv[i];
         break;
      default:
         SUMA_SL_Err("This type is not supported.\n");
         SUMA_free(V);
         SUMA_RETURN(NULL);
         break;
   }
   
   SUMA_RETURN(V);
}

/*!
   \brief Load a surface-based dataset from disk
   
   \param Name (char *) THe name of the file
   \param form (SUMA_DSET_FORMAT) The format of the file
                                  can choose SUMA_NO_DSET_FORMAT
                                  and have the function attempt 
                                  to guess
   \return (SUMA_DSET *) dset 
   The datset does not get associated with a surface (owner_id[0] = '\0')
   You'll have to do this manually later on if you wish
   You typically want to insert that dataset into SUMA's DsetList list...
*/
SUMA_DSET *SUMA_LoadDset (char *Name, SUMA_DSET_FORMAT form)
{  
   static char FuncName[]={"SUMA_LoadDset"};
   SUMA_DSET *dset = NULL;
   
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("NULL Name"); SUMA_RETURN(dset); }
   
   switch (form) {
      case SUMA_NIML:
      case SUMA_ASCII_NIML:
      case SUMA_BINARY_NIML:
         dset = SUMA_LoadNimlDset(Name, 1);
         break;
      case SUMA_1D:
         dset = SUMA_Load1DDset(Name, 1);
         break;
      case SUMA_NO_DSET_FORMAT:
         if (!dset) dset = SUMA_LoadNimlDset(Name, 0);
         if (!dset) dset = SUMA_Load1DDset(Name, 0);
      default:
         SUMA_SLP_Err("Bad format specification");
         SUMA_RETURN(dset);   
   }
   
   if (!dset) {
      SUMA_SL_Err("Failed to read dset");
      SUMA_RETURN(dset);   
   }  
   SUMA_RETURN(dset);
}

/*!

   \brief Load a surface-based data set of the niml format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
   - Reads one ni element only
*/
SUMA_DSET *SUMA_LoadNimlDset (char *Name, int verb)
{
   static char FuncName[]={"SUMA_LoadNimlDset"};
   char *FullName = NULL, *niname = NULL;
   NI_stream ns = NULL;
   NI_element *nel = NULL;
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("Null Name"); SUMA_RETURN(dset); }
   
   /* work the name */
   if (!SUMA_filexists(Name)) {
      /* try the extension game */
      FullName = SUMA_Extension(Name, ".niml.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         if (verb)  { SUMA_SL_Err("Failed to find dset file."); }
         SUMA_RETURN(dset);
      }
   }else {
      FullName = SUMA_copy_string(Name);
   }
   
   /* got the name, now load it */
   niname = SUMA_append_string("file:", FullName);
   
   ns = NI_stream_open(niname, "r");
   if (!ns) {
      SUMA_SL_Crit("Failed to open NI stream for reading.");
      SUMA_RETURN(dset);
   }
   nel = NI_read_element(ns, 1) ;
   NI_stream_close( ns ) ; ns = NULL;
   
   /* Now store that baby in Dset */
   dset = SUMA_NewDsetPointer();
   dset->nel = nel; nel = NULL;

   /* done, clean up and out you go */
   if (niname) SUMA_free(niname); niname = NULL;      
   if (FullName) SUMA_free(FullName); FullName = NULL;
   SUMA_RETURN(dset);
}

/*!

   \brief Load a surface-based data set of the 1D format
   \param Name (char *) name or prefix of dataset
   \param verb (int) level of verbosity. 0 mute, 1 normal, 2 dramatic perhaps
   \return dset (SUMA_DSET *)
   
*/
SUMA_DSET *SUMA_Load1DDset (char *Name, int verb)
{
   static char FuncName[]={"SUMA_Load1DDset"};
   char *FullName = NULL;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int i;
   SUMA_DSET *dset=NULL;
   
   SUMA_ENTRY;
   
   if (!Name) { SUMA_SL_Err("Null Name"); SUMA_RETURN(dset); }
   
   /* work the name */
   if (!SUMA_filexists(Name)) {
      /* try the extension game */
      FullName = SUMA_Extension(Name, ".1D.dset", NOPE);
      if (!SUMA_filexists(FullName)) {
         if (verb)  { SUMA_SL_Err("Failed to find dset file."); }
         SUMA_RETURN(dset);
      }
   }else {
      FullName = SUMA_copy_string(Name);
   }
   
   /* got the name, now read it */
   im = mri_read_1D (Name);
   if (!im) {
      SUMA_SLP_Err("Failed to read file");
      SUMA_RETURN(NULL);
   }   
   
   far = MRI_FLOAT_PTR(im);
   dset = SUMA_CreateDsetPointer(
                                 FullName,         /* usually the filename */
                                 SUMA_NODE_BUCKET, /* mix and match */
                                 NULL,       /* no idcode, let the function create one from the filename*/
                                 NULL,       /* no domain str specified */
                                 im->nx    /* Number of nodes allocated for */
                                 ); 
   
   /* now add the columns */
   for (i=0; i<im->ny; ++i) {
      if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_FLOAT, (void *)(&(far[i*im->nx])), NULL ,1)) {
         SUMA_SL_Crit("Failed in SUMA_AddNelCol");
         SUMA_FreeDset((void*)dset); dset = NULL;
         SUMA_RETURN(dset);
      }
   }

   /* done, clean up and out you go */
   if (im) mri_free(im); im = NULL; 
   if (FullName) SUMA_free(FullName); FullName = NULL;
   SUMA_RETURN(dset);
}

#ifdef SUMA_ConvertDset_STANDALONE
void usage_ConverDset()
{
   
}
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"ConvertDset"};

   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_mainENTRY;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
   SUMA_ParseInput_basics (argv, argc);

   /* dset and its contents are freed in SUMA_Free_CommonFields */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

	SUMA_RETURN (0);
}    
#endif

#ifdef SUMA_Test_DSET_IO_STANDALONE
void usage_Test_DSET_IO ()
   
  {/*Usage*/
          printf ("\nUsage:  \n");
          printf ("\t ..... \n\n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"Test_DSET_IO"}; 
   int *NodeDef=NULL;
   int i, i3, N_NodeDef, N_Alloc, flg;
   float *r=NULL, *g=NULL, *b=NULL, *rgb=NULL;
   char stmp[500], idcode[50], **s, *si;
   NI_element *nel=NULL;
   NI_stream ns;
   int found = 0, NoStride = 0;
   SUMA_DSET * dset = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_mainENTRY;
   
	/* allocate space for CommonFields structure */
	SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
	
   SUMA_ParseInput_basics (argv, argc);

   /* Create some sample data*/
      /* let us create some colors to go on each node */
      N_Alloc = 50;
      NodeDef = (int *)SUMA_malloc(N_Alloc * sizeof(int));
      r = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      g = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      b = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      s = (char **)SUMA_malloc(N_Alloc * sizeof(char *));
      N_NodeDef = N_Alloc;
      for (i=0; i<N_NodeDef; ++i) {
         NodeDef[i] = i;
         r[i] = sin((float)i/N_NodeDef*5);
         g[i] = sin((float)i/N_NodeDef*10);
         b[i] = cos((float)i/N_NodeDef*7);
         sprintf(stmp,"teststr_%d", i);
         s[i] = SUMA_copy_string(stmp);
      }
      /* what if you had a vector of say, triplets */
      rgb = (float *)SUMA_malloc(3 * N_Alloc * sizeof(float));
      for (i=0; i<N_NodeDef; ++i) {
         i3 = 3*i;
         rgb[i3] = r[i];
         rgb[i3+1] = g[i];
         rgb[i3+2] = b[i];
      }
   
   dset = SUMA_CreateDsetPointer(
                                 "SomethingLikeFileName",         /* usually the filename */
                                 SUMA_NODE_BUCKET,                /* mix and match */
                                 NULL,    /* no idcode, let the function create one from the filename*/
                                 NULL,       /* no domain str specified */
                                 N_Alloc    /* Number of nodes allocated for */
                                 ); /* DO NOT free dset, it is store in DsetList */
   if (!SUMA_InsertDsetPointer(dset, SUMAg_CF->DsetList)) {
      SUMA_SL_Err("Failed to insert dset into list");
      exit(1);
   }  
                           
	/* form the dataset */
   if (!SUMA_AddNelCol (   dset->nel, /* the famed nel */ 
                           SUMA_NODE_INDEX, /* the column's type (description),
                                               one of SUMA_COL_TYPE */
                           (void *)NodeDef, /* the list of node indices */
                           NULL  /* that's an optional structure containing 
                                    attributes of the added column. 
                                    Not used at the moment */
                           ,1 /* stride, useful when you need to copy a column
                                 from a multiplexed vector. Say you have in p 
                                 [rgb rgb rgb rgb], to get the g column you 
                                 send in p+1 for the column pointer and a stride
                                 of 3 */
                           )) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);                    
      }
      
      NoStride = 0;
      if (NoStride) {
         /* insert separate r, g and b column */
         if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_R, (void *)r, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_G, (void *)g, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, SUMA_NODE_B, (void *)b, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      } else {
         /* insert from multiplexed rgb vector */
         if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_R, (void *)rgb, NULL ,3 )) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_G, (void *)(rgb+1), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_B, (void *)(rgb+2), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      }
      
      /* add a string column, just for kicks ..*/
      if (!SUMA_AddNelCol (dset->nel, SUMA_NODE_STRING, (void *)s, NULL, 1)) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);  
      }
      
      NEL_WRITE_TX (dset->nel, "file:Test_write_asc.niml", flg); if (!flg) exit(1);
      NEL_WRITE_BI (dset->nel, "file:Test_write_bin.niml", flg); if (!flg) exit(1);
	   NEL_WRITE_1D (dset->nel, "file:Test_write_asc1D.niml", flg); if (!flg) exit(1);
      
      
       
      /* How about loading some data */
      /* Now create a new dataset nel 
      no need to worry about loosing previous dset because it is in 
      SUMAg_CF->DsetList*/
      dset = SUMA_NewDsetPointer();
      NEL_READ(dset->nel, "file:Test_write_bin.niml"); if (!dset->nel) exit(1);
      /* insert the baby into the list */
      SUMA_LH("Inserting newly read element into list\n");
      if (!SUMA_InsertDsetPointer(dset, SUMAg_CF->DsetList)) {
         SUMA_SL_Err("Failed to insert dset into list");
         exit(1);
      }
           
      /* show me the whole thing. Don't do this for an enormous nel */
         /* SUMA_ShowNel(dset->nel); */
         
      
      /* I want the pointer to the green column but do not know its index */
         {   
            int j, *iv, N_i;
            float *fp;
            fprintf (stderr,"---Looking for green column ---\n");
            iv = SUMA_GetColIndex (dset->nel, SUMA_NODE_G, &N_i);
            if (!iv) {
               fprintf (stderr,"Error %s: Failed to find column.\n"
                           , FuncName);
            } else {
               fprintf (stderr,"\t%d columns of type SUMA_NODE_G found.\n",
                           N_i);
               if (N_i) {
                  fprintf (stderr,"\tReporting values at index %d\n", iv[0]);
                  fp = (float *)dset->nel->vec[iv[0]]; /* I know we only have one 
                                                   such col. here */
                  for (j=0; j < dset->nel->vec_len; ++j) {
                     fprintf (stderr,"%f, ", fp[j]);
                  }
                  SUMA_free(iv); iv = NULL;
               }
            }
            
                  
         }
          
   
   /* Now show me that baby,*/
   si = SUMA_DsetInfo (dset, 0);
   fprintf (SUMA_STDERR,"Output of DsetInfo:\n%s\n", si); SUMA_free(si); si=NULL; 
   
   if (LocalHead) fprintf(stderr," %s:-\nFrenching ...\n", FuncName);

   /* free other stuff */
   if (r) SUMA_free(r); r = NULL;
   if (g) SUMA_free(g); g = NULL;
   if (b) SUMA_free(b); b = NULL;
   if (rgb) SUMA_free(rgb); rgb = NULL;
   if (NodeDef) SUMA_free(NodeDef); NodeDef = NULL;  
   if (s) {
      for (i=0; i<N_NodeDef; ++i) {
         if (s[i]) SUMA_free(s[i]);
      }
      SUMA_free(s);
   }

   /* dset and its contents are freed in SUMA_Free_CommonFields */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

	SUMA_RETURN (0);
}/* Main */
#endif
   






#ifdef SUMA_TEST_DATA_SETS_STAND_ALONE
void  SUMA_TestDataSets_Usage()
   
  {/*Usage*/
          printf ("\nUsage:   \n");
          printf ("\t ..... \n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t"
                  "Thu May 29 14:42:58 EDT 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_TestDataSets-Main-"}; 
	char *VolParName, *specfilename = NULL;
   int kar, *Node = NULL, N_Node=-1;
   int SurfIn, brk, LocalHead = 1;
    
   /* Example 1: 5 nodes, 3 floats per node */
	/* allocate space for CommonFields structure */
	if (LocalHead) fprintf (stderr,
                     "%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   SUMA_DSET * dset = NULL;
   
   SUMA_mainENTRY;
   

	/* Work the options */
	kar = 1;
	brk = 0;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 SUMA_TestDataSets_Usage ();
          exit (1);
		}
      
		if (!brk) {
			fprintf (stderr,
               "Error %s: Option %s not understood. Try -help for usage\n",
                FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = 0;
			kar ++;
		}
		
	}/* loop accross command ine options */

   /* Say you want to save a bunch of node values */
   { /* BEGIN: Test to save a data set with a bunch of node values */
      int i, i3, *node = NULL, N_Node;
      float *r=NULL, *g=NULL, *b=NULL, *rgb=NULL;
      char stmp[500], idcode[50], **s;
      NI_element *nel=NULL;
      NI_stream ns;
      int found = 0, NoStride = 0;
      
      UNIQ_idcode_fill (idcode);
      
      /* let us create some colors to go on each node */
      N_Node = 50;
      node = (int *)SUMA_malloc(N_Node * sizeof(int));
      r = (float *)SUMA_malloc(N_Node * sizeof(float));
      g = (float *)SUMA_malloc(N_Node * sizeof(float));
      b = (float *)SUMA_malloc(N_Node * sizeof(float));
      s = (char **)SUMA_malloc(N_Node * sizeof(char *));
      for (i=0; i<N_Node; ++i) {
         node[i] = i;
         r[i] = sin((float)i/N_Node*5);
         g[i] = sin((float)i/N_Node*10);
         b[i] = cos((float)i/N_Node*7);
         sprintf(stmp,"teststr_%d", i);
         s[i] = SUMA_copy_string(stmp);
      }
      /* what if you had a vector of say, triplets */
      rgb = (float *)SUMA_malloc(3 * N_Node * sizeof(float));
      for (i=0; i<N_Node; ++i) {
         i3 = 3*i;
         rgb[i3] = r[i];
         rgb[i3+1] = g[i];
         rgb[i3+2] = b[i];
      }
   
      /* Now create that data element and write it out */
      nel = SUMA_NewNel (  SUMA_NODE_RGB, /* one of SUMA_DSET_TYPE */
                           idcode, /* idcode of Domain Parent */
                           NULL, /* idcode of geometry parent, not useful here*/
                           N_Node,/* Number of elements */
                           "Test",
                           NULL); 
      if (!nel) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_NewNel", FuncName);
         exit(1);
      }
      
      /* Add the columns */
      if (!SUMA_AddNelCol (nel, /* the famed nel */ 
                           SUMA_NODE_INDEX, /* the column's type (description),
                                               one of SUMA_COL_TYPE */
                           (void *)node, /* the list of node indices */
                           NULL  /* that's an optional structure containing 
                                    attributes of the added column. 
                                    Not used at the moment */
                           ,1 /* stride, useful when you need to copy a column
                                 from a multiplexed vector. Say you have in p 
                                 [rgb rgb rgb rgb], to get the g column you 
                                 send in p+1 for the column pointer and a stride
                                 of 3 */
                           )) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);                    
      }
      
      NoStride = 0;
      if (NoStride) {
         /* insert separate r, g and b column */
         if (!SUMA_AddNelCol (nel, SUMA_NODE_R, (void *)r, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, SUMA_NODE_G, (void *)g, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, SUMA_NODE_B, (void *)b, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      } else {
         /* insert from multiplexed rgb vector */
         if (!SUMA_AddNelCol (nel, SUMA_NODE_R, (void *)rgb, NULL ,3 )) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, SUMA_NODE_G, (void *)(rgb+1), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, SUMA_NODE_B, (void *)(rgb+2), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      }
      
      /* add a string column, just for kicks ..*/
      if (!SUMA_AddNelCol (nel, SUMA_NODE_STRING, (void *)s, NULL, 1)) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);  
      }
      
      /* Test writing results in asc, 1D format */ 
         if (LocalHead) fprintf(stderr," %s:-\nWriting ascii 1D ...\n"
                        , FuncName);
         /* open the stream */
         ns = NI_stream_open( "file:Test_write_asc_1D" , "w" ) ;
         if( ns == NULL ){
           fprintf (stderr,"Error  %s:\nCan't open Test_write_asc_1D!"
                        , FuncName); exit(1);
         }

         /* write out the element */
         if (NI_write_element( ns , nel ,
                               NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
            fprintf (stderr,"Error  %s:\nFailed in NI_write_element"
                           , FuncName);
            exit(1);
         }

         /* close the stream */
         NI_stream_close( ns ) ; 
      
      /* Test writing results in asc niml format */
         if (LocalHead) fprintf(stderr," %s:-\nWriting ascii ...\n"
                                       , FuncName);
         /* open the stream */
         ns = NI_stream_open( "file:Test_write_asc" , "w" ) ;
         if( ns == NULL ){
           fprintf (stderr,"Error  %s:\nCan't open Test_write_asc!", FuncName);
           exit(1);
         }

         /* write out the element */
         if (NI_write_element( ns , nel , NI_TEXT_MODE ) < 0) {
            fprintf (stderr,"Error  %s:\nFailed in NI_write_element", FuncName);
            exit(1);
         }

         /* close the stream */
         NI_stream_close( ns ) ;
      
      /* Test writing results in binary niml format */
         if (LocalHead) fprintf(stderr," %s:-\nWriting binary ...\n"
                                       , FuncName);
         /* open the stream */
         ns = NI_stream_open( "file:Test_write_bin" , "w" ) ;
         if( ns == NULL ){
            fprintf (stderr,"Error %s:\nCan't open Test_write_bin!"
                           , FuncName); exit(1);
         }

         /* write out the element */
         if (NI_write_element( ns , nel , NI_BINARY_MODE ) < 0) {
            fprintf (stderr,"Error %s:\nFailed in NI_write_element"
                           , FuncName);
            exit(1);
         }

         /* close the stream */
         NI_stream_close( ns ) ;

         /* free nel */
         NI_free_element(nel) ; nel = NULL;
       
      /* How about loading some data */
         ns = NI_stream_open( "file:Test_write_bin" , "r");
         nel = NI_read_element(ns,1) ;
         /* close the stream */
         NI_stream_close( ns ) ;
         
      /* show me the whole thing. Don't do this for an enormous nel */
         SUMA_ShowNel(nel);
         
      /* What type is it ? */
         fprintf (stderr,"\tNel type: %s (%d)\n",  
                        nel->name, SUMA_Dset_Type(nel->name));
         
      /* What are the columns's types and attributes ? */
         for (i=0; i < nel->vec_num; ++i) {
            sprintf(stmp,"TypeCol_%d", i);
            fprintf (stderr,"\tColumn %d's name: %s\n",
                     i, NI_get_attribute(nel, stmp));
            sprintf(stmp,"attrCol_%d", i);
            fprintf (stderr,"\tColumn %d's attribute: %s\n", 
                     i, NI_get_attribute(nel, stmp));
         }
      
      /* I want the pointer to the green column but do not know its index */
         {   
            int j, *iv, N_i;
            float *fp;
            fprintf (stderr,"---Looking for green column ---\n");
            iv = SUMA_GetColIndex (nel, SUMA_NODE_G, &N_i);
            if (!iv) {
               fprintf (stderr,"Error %s: Failed to find column.\n"
                           , FuncName);
            } else {
               fprintf (stderr,"\t%d columns of type SUMA_NODE_G found.\n",
                           N_i);
               if (N_i) {
                  fprintf (stderr,"\tReporting values at index %d\n", iv[0]);
                  fp = (float *)nel->vec[iv[0]]; /* I know we only have one 
                                                   such col. here */
                  for (j=0; j < nel->vec_len; ++j) {
                     fprintf (stderr,"%f, ", fp[j]);
                  }
                  SUMA_free(iv); iv = NULL;
               }
            }
            
                  
         }
          
      /* free nel */
      NI_free_element(nel) ; nel = NULL;
      
      if (LocalHead) fprintf(stderr," %s:-\nFrenching ...\n", FuncName);
      
      /* free other stuff */
      if (r) SUMA_free(r); r = NULL;
      if (g) SUMA_free(g); g = NULL;
      if (b) SUMA_free(b); b = NULL;
      if (rgb) SUMA_free(rgb); rgb = NULL;
      if (node) SUMA_free(node); node = NULL;  
      if (s) {
         for (i=0; i<N_Node; ++i) {
            if (s[i]) SUMA_free(s[i]);
         }
         SUMA_free(s);
      } 
   } /* END: Test to save a data set with a bunch of node values */
 	  
   return(0);
}/* Main */

#endif

