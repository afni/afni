#include "SUMA_suma.h"

#if defined SUMA_TEST_DATA_SETS_STAND_ALONE
#define STAND_ALONE
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */
#else
/* extern SUMA_CommonFields *SUMAg_CF;
   extern SUMA_DO *SUMAg_DOv;
   extern SUMA_SurfaceViewer *SUMAg_SVv;
   extern int SUMAg_N_SVv; 
   extern int SUMAg_N_DOv;   */
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
   
   nel = NI_new_data_element(SUMA_Dset_Name(dtp), N_el);
   
   /* assign an idcode */
   UNIQ_idcode_fill(idcode);
   NI_set_attribute (nel, "idcode", idcode);
   
   /* set the idcodes of the parents */
   NI_set_attribute (nel, "DomParent_idcode", DomParent_idcode);
   NI_set_attribute (nel, "GeomParent_idcode", GeomParent_idcode);
   
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
   
   /* save the name of the column */
   sprintf(Name, "NameCol_%d", nel->vec_num-1);
   sprintf(Attr, "AttrCol_%d", nel->vec_num-1);
   
   switch (ctp) {
      case SUMA_NODE_INDEX:
         /* form the string of attributes for this column */
         NI_set_attribute ( nel, Name, "Node_Index");
         NI_set_attribute ( nel, Attr, "NULL");
         break;
     
      case SUMA_NODE_INT:
         NI_set_attribute ( nel, Name, "Generic Int");
         NI_set_attribute ( nel, Attr, "NULL");
         break;
         
      case SUMA_NODE_X:
         NI_set_attribute ( nel, Name, "X_coord");
         NI_set_attribute ( nel, Attr, "NULL");
         break;
      
      case SUMA_NODE_Y:
         NI_set_attribute ( nel, Name, "Y_coord");
         NI_set_attribute ( nel, Attr, "NULL");
         break;
      
      case SUMA_NODE_Z:
         NI_set_attribute ( nel, Name, "Z_coord");
         NI_set_attribute ( nel, Attr, "NULL");
         break; 
         
      case SUMA_NODE_R:
         NI_set_attribute ( nel, Name, "R_col");
         NI_set_attribute ( nel, Attr, "NULL");
         break;
      
      case SUMA_NODE_G:
         NI_set_attribute ( nel, Name, "G_col");
         NI_set_attribute ( nel, Attr, "NULL");
         break;
      
      case SUMA_NODE_B:
         NI_set_attribute ( nel, Name, "B_col");
         NI_set_attribute ( nel, Attr, "NULL");
         break;    
       
      case SUMA_NODE_FLOAT:
         NI_set_attribute ( nel, Name, "Generic Float");
         NI_set_attribute ( nel, Attr, "NULL");
         break;     
      
      default:
         NI_set_attribute ( nel, Name, "Unknown");
         NI_set_attribute ( nel, Attr, "NULL");
         break;          
   }
   
   return(1);   
}

/*!
   Adds a column to Nel
*/

int SUMA_AddNelCol (NI_element *nel, SUMA_COL_TYPE ctp, void *col, void *col_attr)
{
   static char FuncName[]={"SUMA_AddNelCol"};
   
   if (!nel) return(0);
   if (!col) return(0);
   
   switch (ctp) {
      case SUMA_NODE_INT:
      case SUMA_NODE_INDEX:
         NI_add_column ( nel, NI_INT, (int *)col );
         break;
         
      case SUMA_NODE_FLOAT:
      case SUMA_NODE_X:
      case SUMA_NODE_Y:
      case SUMA_NODE_Z:
      case SUMA_NODE_R:
      case SUMA_NODE_G:
      case SUMA_NODE_B:
         NI_add_column ( nel, NI_FLOAT, (float *)col );      
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
 
#if 0
         {
            /* Must break coordinates to 3 columns ...*/
            int i, i3;
            float *tmpx=NULL, tmpy=NULL, tmpz=NULL, *fcol=NULL;
            if (!tmpx || !tmpy || !tmpz) {
               fprintf(stderr,"Error %s: Failed to allocate.\n", FuncName);
               return(0);
            }
            tmpx = (float *)malloc(nel->vec_len,sizeof(float));
            tmpy = (float *)malloc(nel->vec_len,sizeof(float));
            tmpz = (float *)malloc(nel->vec_len,sizeof(float));
            fcol = (float *)col;
            for (i=0; i < nel->vec_len; ++i) {
               i3 = 3*i;
               tmpx[i] = fcol[i3];
               tmpy[i] = fcol[i3+1];
               tmpz[i] = fcol[i3+2];
            }
            NI_add_column ( nel, NI_FLOAT, tmpx );
            NI_add_column ( nel, NI_FLOAT, tmpy );
            NI_add_column ( nel, NI_FLOAT, tmpz );
            free(tmpx); tmpx=NULL;
            free(tmpy); tmpy=NULL;
            free(tmpz); tmpz=NULL;
         }

#endif

char * SUMA_Dset_Name (SUMA_DSET_TYPE tp)
{
   static char FuncName[]={"SUMA_Dset_Name"};
   
   switch (tp) {
      case SUMA_NO_DSET_TYPE:
         return("No_Type_Set");
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





























#ifdef SUMA_TEST_DATA_SETS_STAND_ALONE
void  SUMA_TestDataSets_Usage()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m  -spec xxx -ROIin xxx\n");
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
	SUMA_SurfSpecFile Spec;   
   SUMA_Boolean SurfIn, brk, LocalHead = YUP;
    
   /* Example 1: 5 nodes, 3 floats per node */
	/* allocate space for CommonFields structure */
	if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_Create_CommonFields ...\n", FuncName);
   
   if (argc < 2)
       {
          SUMA_TestDataSets_Usage ();
          exit (1);
       }
		
   SUMAg_CF = SUMA_Create_CommonFields ();
	if (SUMAg_CF == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
		exit(1);
	}
   if (LocalHead) fprintf (SUMA_STDERR,"%s: SUMA_Create_CommonFields Done.\n", FuncName);
   
   /* initialize Volume Parent and AfniHostName to nothing */
	VolParName = NULL;
	AfniHostName = NULL; 

	
   SUMA_LH("Allocating for DOv ...");   
	/* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
	   
	/* Work the options */
	kar = 1;
	brk = NOPE;
	SurfIn = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			SUMA_TestDataSets_Usage ();
          exit (1);
		}
      
		if (!brk && (strcmp(argv[kar], "-iodbg") == 0)) {
			fprintf(SUMA_STDOUT,"Warning %s: SUMA running in in/out debug mode.\n", FuncName);
			SUMAg_CF->InOut_Notify = YUP;
			brk = YUP;
		}
      
		#if SUMA_MEMTRACE_FLAG
         if (!brk && (strcmp(argv[kar], "-memdbg") == 0)) {
			   fprintf(SUMA_STDOUT,"Warning %s: SUMA running in memory trace mode.\n", FuncName);
			   SUMAg_CF->MemTrace = YUP;
			   brk = YUP;
		   }
      #endif
      
		if (!brk && strcmp(argv[kar], "-spec") == 0)
		{ 
			kar ++;
		  if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -spec ");
				exit (1);
			}
			
			specfilename = argv[kar];
			/*fprintf(SUMA_STDOUT, "Found: %s\n", specfilename);*/
			brk = YUP;
		} 
		

		if (!brk) {
			fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
		
	}/* loop accross command ine options */

	if (specfilename == NULL) {
		fprintf (SUMA_STDERR,"Error %s: No spec filename specified.\n", FuncName);
		exit(1);
	}

   SUMA_LH("Loading surfaces ...");
   /* load the specs file and the specified surfaces*/
	/* Load The spec file */
	if (!SUMA_Read_SpecFile (specfilename, &Spec)) {
		fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile\n", FuncName);
		exit(1);
	}	

	/* make sure only one group was read in */
	if (Spec.N_Groups != 1) {
		fprintf(SUMA_STDERR,
         "Error %s: One and only one group of surfaces"
         " is allowed at the moment (%d found).\n", FuncName, Spec.N_Groups);
		exit(1);
	}

	/* load the surfaces specified in the specs file, one by one*/			
	if (!SUMA_LoadSpec (&Spec, SUMAg_DOv, &SUMAg_N_DOv, VolParName)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec.\n", FuncName);
		exit(1);
	}
	
   /* Say you want to save a bunch of node values */
   { /* BEGIN: Test to save a data set with a bunch of node values */
      SUMA_SurfaceObject *SO=NULL;
      int i, i3, *node = NULL;
      float *r=NULL, *g=NULL, *b=NULL, *rgb=NULL;
      char stmp[500];
      NI_element *nel=NULL;
      NI_stream ns;
      SUMA_Boolean found = NOPE;
      
      found = NOPE;
      i = 0;
      SO = NULL;
      while (i < SUMAg_N_DOv && !SO) {
         if (SUMA_isSO(SUMAg_DOv[i])) {
            SO = (SUMA_SurfaceObject *)SUMAg_DOv[i].OP;
         }
           ++i;
      }
      if (!SO) {
         SUMA_S_Err("Failed to find a surface.");
         exit(1);
      }
      
      /* let us create some colors to go on each node */
      node = (int *)SUMA_malloc(SO->N_Node * sizeof(int));
      r = (float *)SUMA_malloc(SO->N_Node * sizeof(float));
      g = (float *)SUMA_malloc(SO->N_Node * sizeof(float));
      b = (float *)SUMA_malloc(SO->N_Node * sizeof(float));
      for (i=0; i<SO->N_Node; ++i) {
         node[i] = i;
         r[i] = sin((float)i/SO->N_Node*5);
         g[i] = sin((float)i/SO->N_Node*10);
         b[i] = cos((float)i/SO->N_Node*7);
      }
      /* what if you had a vector of say, triplets */
      rgb = (float *)SUMA_malloc(3 * SO->N_Node * sizeof(float));
      for (i=0; i<SO->N_Node; ++i) {
         i3 = 3*i;
         rgb[i3] = r[i];
         rgb[i3+1] = g[i];
         rgb[i3+2] = b[i];
      }
   
      /* Now create that data element and write it out */
      nel = SUMA_NewNel (  SUMA_NODE_RGB, /* one of SUMA_DSET_TYPE */
                           SO->idcode_str, /* idcode of Domain Parent */
                           NULL, /* idcode of geometry parent, not useful here*/
                           SO->N_Node); /* Number of elements */
      if (!nel) {
         SUMA_S_Err("Failed in SUMA_NewNel");
         exit(1);
      }
      
      /* Add the columns */
      if (!SUMA_AddNelCol (nel, /* the famed nel */ 
                           SUMA_NODE_INDEX, /* the column's type (description), one of SUMA_COL_TYPE */
                           (void *)node, /* the list of node indices */
                           NULL  /* that's an optional structure containing attributes of the 
                                       added column. Not used at the moment */
                           )) {
         SUMA_S_Err("Failed in SUMA_AddNelCol");
         exit(1);                    
      }
      
      if (!SUMA_AddNelCol (nel, SUMA_NODE_R, (void *)r, NULL )) {
         SUMA_S_Err("Failed in SUMA_AddNelCol");
         exit(1);
      }
                         
      if (!SUMA_AddNelCol (nel, SUMA_NODE_G, (void *)g, NULL)) {
         SUMA_S_Err("Failed in SUMA_AddNelCol");
         exit(1);
      }

      if (!SUMA_AddNelCol (nel, SUMA_NODE_B, (void *)b, NULL)) {
         SUMA_S_Err("Failed in SUMA_AddNelCol");
         exit(1);
      }
   
      /* Test writing results in asc, 1D format */ 
         SUMA_LH ("Writing ascii 1D ...\n");
         /* open the stream */
         ns = NI_stream_open( "file:Test_write_asc_1D" , "w" ) ;
         if( ns == NULL ){
           SUMA_S_Err("Can't open Test_write_asc_1D!"); exit(1);
         }

         /* write out the element */
         if (NI_write_element( ns , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
            SUMA_S_Err("Failed in NI_write_element");
            exit(1);
         }

         /* close the stream */
         NI_stream_close( ns ) ; 
      
      /* Test writing results in asc niml format */
         SUMA_LH ("Writing ascii ...\n");
         /* open the stream */
         ns = NI_stream_open( "file:Test_write_asc" , "w" ) ;
         if( ns == NULL ){
           SUMA_S_Err("Can't open Test_write_asc!"); exit(1);
         }

         /* write out the element */
         if (NI_write_element( ns , nel , NI_TEXT_MODE ) < 0) {
            SUMA_S_Err("Failed in NI_write_element");
            exit(1);
         }

         /* close the stream */
         NI_stream_close( ns ) ;
      
      /* Test writing results in binary niml format */
         SUMA_LH ("Writing binary ...\n");
         /* open the stream */
         ns = NI_stream_open( "file:Test_write_bin" , "w" ) ;
         if( ns == NULL ){
           SUMA_S_Err("Can't open Test_write_bin!"); exit(1);
         }

         /* write out the element */
         if (NI_write_element( ns , nel , NI_BINARY_MODE ) < 0) {
            SUMA_S_Err("Failed in NI_write_element");
            exit(1);
         }

         /* close the stream */
         NI_stream_close( ns ) ;
       
          
      SUMA_LH ("Frenching ...");
      /* free nel */
      NI_free_element(nel) ; nel = NULL;
      
      /* free other stuff */
      if (r) SUMA_free(r); r = NULL;
      if (g) SUMA_free(g); g = NULL;
      if (b) SUMA_free(b); b = NULL;
      if (rgb) SUMA_free(rgb); rgb = NULL;
      if (node) SUMA_free(node); node = NULL;   
   } /* END: Test to save a data set with a bunch of node values */
 	
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) SUMA_error_message(FuncName,"DO Cleanup Failed!",1);
	if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
  
   return(0);
}/* Main */

#endif
