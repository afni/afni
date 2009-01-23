/* This program is meant be run withOUT SUMA's .hness and libraries */
#ifdef SUMA_COMPILED
   #undef SUMA_COMPILED
#endif


#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "../niml/niml_private.h"
#include "xutil.h"


#include "SUMA_suma.h"


void  SUMA_TestDataSets_Usage()
   
  {/*Usage*/
          printf ("\nUsage:   \n");
          printf ("\t ..... \n\n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \t"
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
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
	/* Work the options */
	kar = 1;
	brk = 0;
	while (kar < argc) { /* loop accross command line options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 SUMA_TestDataSets_Usage ();
          exit (1);
		}
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
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
                           "le index", 
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
         if (!SUMA_AddNelCol (nel, "le R", SUMA_NODE_R, (void *)r, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, "le G", SUMA_NODE_G, (void *)g, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (nel, "le B", SUMA_NODE_B, (void *)b, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      } else {
         /* insert from multiplexed rgb vector */
         if (!SUMA_AddNelCol (nel, "le R", SUMA_NODE_R, (void *)rgb, NULL ,3 )) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (   nel, "le G", SUMA_NODE_G, 
                                 (void *)(rgb+1), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddNelCol (   nel, "le B", SUMA_NODE_B, 
                                 (void *)(rgb+2), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      }
      
      /* add a string column, just for kicks ..*/
      if (!SUMA_AddNelCol (   nel, "la string", SUMA_NODE_STRING, 
                              (void *)s, NULL, 1)) {
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
         SUMA_ShowNel((void*)nel);
         
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
 	
   #ifdef SUMA_COMPILED
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   #endif
     
   SUMA_RETURN(0);
}/* Main */

