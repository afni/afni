#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "xutil.h"
#include "SUMA_DataSets.h"

#if defined SUMA_TEST_DATA_SETS_STAND_ALONE
#define STAND_ALONE
#else
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even 
   if they will not be used by this main */
#else
#endif

/*!
   Creates a NI elem. to store surface data 
   N_el is the number of data elements stored in each column
   N_el can be the number of nodes for example
*/
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* DomParent_idcode, 
                          char * GeomParent_idcode, int N_el)
{
   static char FuncName[]={"SUMA_NewDataStruct"};
   NI_element *nel=NULL;
   char idcode[SUMA_IDCODE_LENGTH];
   
   nel = NI_new_data_element(SUMA_Dset_Type_Name(dtp), N_el);
   
   /* assign an idcode */
   UNIQ_idcode_fill(idcode);
   NI_set_attribute (nel, "idcode", idcode);
   
   /* set the idcodes of the parents */
   if (DomParent_idcode) {
      NI_set_attribute (nel, "DomParent_idcode", DomParent_idcode);
   } else {
      NI_set_attribute (nel, "DomParent_idcode", NULL);
   }
   if (GeomParent_idcode) {
      NI_set_attribute (nel, "GeomParent_idcode", GeomParent_idcode);
   } else {
      NI_set_attribute (nel, "GeomParent_idcode", NULL);
   }
  
   return(nel);  
}

/*!
   
   Adds an attribute to nel for that explains the last added column's 
   contents. You should call this function after each SUMA_AddNelCol call 
   col_attr (void *) is a pointer to a structure containing special
   attributes of the data in the last column added. At the moment,
   this pointer is not being used, but you can imagine needing it if
   you have to store certain stats or parameters that go with each column.
       
*/
int SUMA_AddColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col_attr)
{
   static char FuncName[]={"SUMA_AddColAttr"};
   char Name[500], Attr[500];
   
   if (!nel) return(0);
   
   /* save the type of the column */
   sprintf(Name, "TypeCol_%d", nel->vec_num-1);
   NI_set_attribute ( nel, Name, SUMA_Col_Type_Name(ctp));
   
   sprintf(Attr, "AttrCol_%d", nel->vec_num-1);
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
       
      case SUMA_NODE_FLOAT:
         NI_set_attribute ( nel, Attr, NULL);
         break;     
      
      default:
         NI_set_attribute ( nel, Attr, NULL);
         break;          
   }
   
   return(1);   
}

/*!
   Adds a column to Nel
*/

int SUMA_AddNelCol ( NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride)
{
   static char FuncName[]={"SUMA_AddNelCol"};
   
   if (!nel) return(0);
   if (!col) return(0);
   
   switch (ctp) {
      case SUMA_NODE_INT:
      case SUMA_NODE_ILABEL:
      case SUMA_NODE_INDEX:
         NI_add_column_stride ( nel, NI_INT, (int *)col, stride);
         break;
         
      case SUMA_NODE_FLOAT:
      case SUMA_NODE_X:
      case SUMA_NODE_Y:
      case SUMA_NODE_Z:
      case SUMA_NODE_R:
      case SUMA_NODE_G:
      case SUMA_NODE_B:
         NI_add_column_stride ( nel, NI_FLOAT, (float *)col, stride );      
         break;
      default:
         fprintf  (stderr,"Error %s: Bad column type.\n", FuncName);
         return(0);
         break; 
   }
   
   /* add the attributes of that column */
   SUMA_AddColAttr (nel, ctp, col_attr);
   
   return(1);
}
 

char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr)
{
   static char FuncName[]={"SUMA_Dset_Format_Name"};
   
   switch(fr) {
      case SUMA_ERROR_DSET_FORMAT:
         return ("Error_Dset_Format");
         break;
      case SUMA_NO_DSET_FORMAT:
         return ("Dset_Format_Undefined");
         break;
      case SUMA_ASCII_NIML:
         return ("Ascii_Niml");
         break;
      case SUMA_BINARY_NIML:
         return ("Binary_Niml");
         break;
      case SUMA_1D:
         return ("Afni_1D");
         break;
      default:
         return("Cowabonga");
         break;
   }   
   
}

SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name)
{
   static char FuncName[]={"SUMA_Dset_Format"};
   if (!strcmp(Name,"Error_Dset_Format")) return (SUMA_ERROR_DSET_FORMAT);
   if (!strcmp(Name,"Dset_Format_Undefined")) return (SUMA_NO_DSET_FORMAT);
   if (!strcmp(Name,"Ascii_Niml")) return (SUMA_ASCII_NIML);
   if (!strcmp(Name,"Binary_Niml")) return (SUMA_BINARY_NIML);
   if (!strcmp(Name,"Afni_1D")) return (SUMA_1D);
   return(SUMA_ERROR_DSET_FORMAT);
}

char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp)
{
   static char FuncName[]={"SUMA_Dset_Type_Name"};
   
   switch (tp) {
      case SUMA_NO_DSET_TYPE:
         return("Dset_Type_Undefined");
         break;
      case SUMA_ERROR_DSET_TYPE:
         return("Error_Dset_Type");
         break;
      case SUMA_NODE_BUCKET:
         return("Node_Bucket");
         break;
      case SUMA_NODE_ROI:
         return("Node_ROI");
         break;
      case SUMA_NODE_RGB:
         return("Node_RGB");
         break;
      default:
         return("Cowabonga");
         break;
   }
}

SUMA_DSET_TYPE SUMA_Dset_Type (char *Name)
{
   if (!strcmp(Name,"Dset_Type_Undefined")) return (SUMA_NO_DSET_TYPE);
   if (!strcmp(Name,"Error_Dset_Type")) return (SUMA_ERROR_DSET_TYPE);
   if (!strcmp(Name,"Node_Bucket")) return (SUMA_NODE_BUCKET);
   if (!strcmp(Name,"Node_ROI")) return (SUMA_NODE_ROI);
   if (!strcmp(Name,"Node_RGB")) return (SUMA_NODE_RGB);
   if (!strcmp(Name,"Cowabonga")) return (SUMA_ERROR_DSET_TYPE);
   return (SUMA_ERROR_DSET_TYPE);
}

char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp)
{
   switch (tp) {
      case SUMA_NO_COL_TYPE:
         return("Col_Type_Undefined");
         break;
      case SUMA_ERROR_COL_TYPE:
         return ("Error_Col_Type");
         break;
      case SUMA_NODE_INT:
         return("Generic_Int");
         break;
      case SUMA_NODE_INDEX:
         return("Node_Index");
         break;
      case SUMA_NODE_ILABEL:
         return("Node_Index_Label");
         break;
      case SUMA_NODE_FLOAT:
         return("Generic_Float");
         break;
      case SUMA_NODE_X:
         return("X_coord");
         break;
      case SUMA_NODE_Y:
         return("Y_coord");
         break;
      case SUMA_NODE_Z:
         return("Z_coord");
         break;
      case SUMA_NODE_R:
         return("R_col");
         break;
      case SUMA_NODE_G:
         return("G_col");
         break;
      case SUMA_NODE_B:
         return("B_col");
         break;
      default:
         return("Cowabonga");
         break;
   }
   
}

SUMA_COL_TYPE SUMA_Col_Type (char *Name)
{
   if (!strcmp(Name,"Col_Type_Undefined")) return (SUMA_NO_COL_TYPE);
   if (!strcmp(Name,"Error_Col_Type")) return (SUMA_ERROR_COL_TYPE);
   if (!strcmp(Name,"Generic_Int")) return (SUMA_NODE_INT);
   if (!strcmp(Name,"Node_Index")) return (SUMA_NODE_INDEX);
   if (!strcmp(Name,"Generic_Float")) return (SUMA_NODE_FLOAT);
   if (!strcmp(Name,"X_coord")) return (SUMA_NODE_X);
   if (!strcmp(Name,"Y_coord")) return (SUMA_NODE_Y);
   if (!strcmp(Name,"Z_coord")) return (SUMA_NODE_Z);
   if (!strcmp(Name,"R_col")) return (SUMA_NODE_R);
   if (!strcmp(Name,"G_col")) return (SUMA_NODE_G);
   if (!strcmp(Name,"B_col")) return (SUMA_NODE_B);
   // if (!strcmp(Name,"")) return ();
   return (SUMA_ERROR_COL_TYPE);

}


int SUMA_ShowNel (NI_element *nel)
{
   static char FuncName[]={"SUMA_ShowNel"};
   NI_stream nstdout;
   
   nstdout = NI_stream_open( "fd:1","w");
   if( nstdout == NULL ){ 
      fprintf(stderr,"%s: Can't open fd:1\n", FuncName); 
      return(0); 
   }
   fprintf (stdout, "\n-----------nel stdout begin-----------\n");
   NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
   fprintf (stdout, "\n-----------nel stdout end  -----------\n");
   NI_stream_close(nstdout);
   
   return(1);
}



int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i)
{
   int *iv=NULL, i=0;
   char stmp[500];
   
   *N_i = -1;
   iv = (int *)calloc(nel->vec_num, sizeof(int));
   if (!iv) {
      return(NULL);
   }   
   
   *N_i = 0;
   for (i=0; i < nel->vec_num; ++i) {
      sprintf(stmp,"TypeCol_%d",i);
      if (SUMA_Col_Type(stmp) == tp) {
         iv[*N_i] = i;
         *N_i++;
      }
   }
   
   return(iv);
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
   
   if (!arg) return(0);
   if (!arg[0]) return(0);
   if (!nel) return(0);
   if (!N_arg) return(0);
   
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
      free(stmp);
   }
   
   return(1);
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

   if (!buf) return(NULL);
   
   atr = (char *) calloc(strlen(buf)+2, sizeof(char));
   
   i=0;
   while (buf[i]) {
      atr[i] = buf[i];
      ++i;
   }
   atr[i] = '\0';
   
   return(atr);  
}

/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_string(s1, s2);
  
   - s1 and s2 are copied into a new string
   -free returned pointer with:  if(s_ap) free(s_ap);
   - None of the strings passed to the function
   are freed.
   
   \sa SUMA_append_replace_string
*/
char * SUMA_append_string(char *s1, char *s2)
{
   static char FuncName[]={"SUMA_append_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1;

   
   if (!s1 && !s2) return(NULL);
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   atr = (char *) calloc(N_s1+N_s2+2, sizeof(char));
   
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
   
   return(atr);  
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
   -free returned pointer with:  if(s_ap) free(s_ap);
   
   \sa SUMA_append_string
*/
char * SUMA_append_replace_string(char *s1, char *s2, char *Spc, int whichTofree)
{
   static char FuncName[]={"SUMA_append_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1, N_Spc=0;

   
   if (!s1 && !s2) return(NULL);
   
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   if (!Spc) N_Spc = 0;
   else N_Spc = strlen(Spc);
   
   atr = (char *) calloc(N_s1+N_s2+N_Spc+2, sizeof(char));
   
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

   return(atr);  
}    








#ifdef SUMA_TEST_DATA_SETS_STAND_ALONE
void  SUMA_TestDataSets_Usage()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m  \n");
          printf ("\t ..... \n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t"
                  "Thu May 29 14:42:58 EDT 2003 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_TestDataSets-Main-"}; 
	char *VolParName, *specfilename = NULL, *AfniHostName;
   int kar, *Node = NULL, N_Node=-1;
   int SurfIn, brk, LocalHead = 1;
    
   /* Example 1: 5 nodes, 3 floats per node */
	/* allocate space for CommonFields structure */
	if (LocalHead) fprintf (stderr,
                     "%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   if (argc < 0)
       {
          SUMA_TestDataSets_Usage ();
          exit (1);
       }
		
	/* Work the options */
	kar = 1;
	brk = 0;
	SurfIn = 0;
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
      char stmp[500], idcode_str[50];
      NI_element *nel=NULL;
      NI_stream ns;
      int found = 0, NoStride = 0;
      
      UNIQ_idcode_fill (idcode_str);
      
      /* let us create some colors to go on each node */
      N_Node = 50;
      node = (int *)malloc(N_Node * sizeof(int));
      r = (float *)malloc(N_Node * sizeof(float));
      g = (float *)malloc(N_Node * sizeof(float));
      b = (float *)malloc(N_Node * sizeof(float));
      for (i=0; i<N_Node; ++i) {
         node[i] = i;
         r[i] = sin((float)i/N_Node*5);
         g[i] = sin((float)i/N_Node*10);
         b[i] = cos((float)i/N_Node*7);
      }
      /* what if you had a vector of say, triplets */
      rgb = (float *)malloc(3 * N_Node * sizeof(float));
      for (i=0; i<N_Node; ++i) {
         i3 = 3*i;
         rgb[i3] = r[i];
         rgb[i3+1] = g[i];
         rgb[i3+2] = b[i];
      }
   
      /* Now create that data element and write it out */
      nel = SUMA_NewNel (  SUMA_NODE_RGB, /* one of SUMA_DSET_TYPE */
                           idcode_str, /* idcode of Domain Parent */
                           NULL, /* idcode of geometry parent, not useful here*/
                           N_Node); /* Number of elements */
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

         if (!SUMA_AddNelCol (nel, SUMA_NODE_G, (void *)rgb+1, NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, SUMA_NODE_B, (void *)rgb+2, NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
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
            
            iv = SUMA_GetColIndex (SUMA_NODE_G, &N_i);
            if (!iv) {
               fprintf (stderr,"Error %s: Failed to find column.\n"
                           , FuncName, stmp);
            } else {
         
               fp = (float *)nel->vec[iv[0]]; /* I know we only have one 
                                                such col. here */
               fprintf (stderr,"Column %s (%d): \t", stmp, i); 
               for (j=0; j < nel->vec_len; ++j) {
                  fprintf (stderr,"%f, ", fp[j]);
               }
               free(iv); iv = NULL;
            }
            
                  
         }
          
      /* free nel */
      NI_free_element(nel) ; nel = NULL;
      
      if (LocalHead) fprintf(stderr," %s:-\nFrenching ...\n", FuncName);
      
      /* free other stuff */
      if (r) free(r); r = NULL;
      if (g) free(g); g = NULL;
      if (b) free(b); b = NULL;
      if (rgb) free(rgb); rgb = NULL;
      if (node) free(node); node = NULL;   
   } /* END: Test to save a data set with a bunch of node values */
 	  
   return(0);
}/* Main */

#endif
