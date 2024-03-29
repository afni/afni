/* This should always run even when compiled WITHOUT SUMA_COMPILED */

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

void usage_Test_DSET_IO ()
   
  {/*Usage*/
          char *sb=NULL;
          sb = SUMA_help_basics();
          printf ("\n"
                  "Usage:  \n"
                  "%s"
                  "\n", sb);
          SUMA_free(sb);
          exit (0);
  }/*Usage*/

/* Create a toy CIFTI dataset that is made up of two surfaces isotopic with the surface named in sdomain and the volume grid in vdomain */
SUMA_DSET *SUMA_Create_Fake_CIFTI(char *sdomain0, char *sdomain1, char *vdomain)
{
   static char FuncName[]={"SUMA_Create_Fake_CIFTI"};
   SUMA_DSET *sdset = NULL;
   SUMA_SurfaceObject *SO0 = NULL, *SO1 = NULL;
   THD_3dim_dataset *vdset = NULL;
   byte *mask;
   int N_mask, i, k, *dind=NULL, *dmaxind, *ind=NULL, *IndOffset=NULL;
   float *v1=NULL, *v2=NULL, *fv=NULL;
   SUMA_DO_Types *dtp=NULL;
   char **dsstr=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   /* 
      Define indices for which there is some data on ld3, assume for left hemi 
      For convenience, the data values at these nodes will be generated to be 
      some silly version of the indices themselves.
   */
   int indLeft[] = {57, 58, 40, 39, 52, 53, 59, 81, 45, 47, 48, 49, 21};
   int N_indLeft = 13;
   /* Ditto for the right hemisphere.*/
   int indRight[] = {30, 30, 32, 80, 88, 11, 90, 35, 36, 29 };
   int N_indRight = 10;
   int N_Alloc;
   
   SUMA_ENTRY;
   
   /* Hard code the domains, no time to get fancy here */
   if (!sdomain0) sdomain0 = "lh:MNI_N27:ld20:smoothwm";
   if (!sdomain1) sdomain1 = "rh:MNI_N27:ld20:smoothwm";
   if (!vdomain) vdomain = "ciftivolmsk+tlrc.HEAD";
   
   /* Load the domains */
   
   if (!(SO0 = SUMA_Load_Surface_Object_Wrapper(sdomain0, NULL, NULL,
                                         SUMA_PREDEFINED, SUMA_FF_NOT_SPECIFIED, 
                                         NULL, 2))) {
      SUMA_S_Err("Failed to load SO0 %s", sdomain0);
      SUMA_RETURN(NULL);
   }
   
   if (!(SO1 = SUMA_Load_Surface_Object_Wrapper(sdomain1, NULL, NULL,
                                         SUMA_PREDEFINED, SUMA_FF_NOT_SPECIFIED, 
                                         NULL, 2))) {
      SUMA_S_Err("Failed to load SO1 %s", sdomain1);
      SUMA_RETURN(NULL);
   }
   
   if (!(vdset = THD_open_dataset(vdomain))) {
      SUMA_S_Err("Failed to load vdset %s", vdomain);
      SUMA_Free_Surface_Object(SO0); SO0 = NULL;
      SUMA_Free_Surface_Object(SO1); SO1 = NULL;
      SUMA_RETURN(NULL);
   }
   mask = THD_makemask( vdset , 0 , 1.0, -1.0 );
   N_mask = THD_countmask(DSET_NVOX(vdset), mask);
   fv = THD_extract_to_float ( 0, vdset);
      
   /* Number of entries, we assume this is not a full dataset */
   N_Alloc = N_indLeft + N_indRight + N_mask;

   
   /* Create the index column and some data */
   SUMA_S_Warn("Check what is to be preserved in output dset and free the rest");
   IndOffset = (int *)SUMA_calloc(3+1,sizeof(int));
   dmaxind = (int *)SUMA_calloc(3,sizeof(int));
   dsstr = (char **)SUMA_calloc(3, sizeof(char *));
   dtp = (SUMA_DO_Types *)SUMA_calloc(3,sizeof(SUMA_DO_Types));
   /* This is the domain index vector: dind[k] is the datum index of row k on 
   domain(k). This means that you could have duplicate indices whereby
   dind[k1] == dind[k2], but only if domain(k1) != domain(k2). 
   For surface-only datasets, you could not have duplicate indices because
   the dataset was defined over one domain only. To make sure the distinction 
   is clear in the code, I will create a new with MD in the name for 
   Multi Domain*/  
   dind = (int *)SUMA_calloc(N_Alloc, sizeof(int));
   v1 = (float *)SUMA_calloc(N_Alloc, sizeof(float));
   v2 = (float *)SUMA_calloc(N_Alloc, sizeof(float));
   
   IndOffset[0] = 0;
   dmaxind[0] = SO0->N_Node;
   dsstr[0] = SUMA_copy_string(sdomain0);
   dtp[0] = SO_type;
   k = 0;
   i = 0;
   while (i < N_indLeft) { /* Think of this as CIFTI brain model */
      dind[k] = indLeft[i];
      v1[k] = cos(i/300.0);
      v2[k] = sin(i/300.0);
      ++k; ++i;
   }
   IndOffset[1] = IndOffset[0]+N_indLeft;
   dmaxind[1] = SO1->N_Node;
   dsstr[1] = SUMA_copy_string(sdomain1);
   dtp[1] = SO_type;
   i = 0;
   while (i < N_indRight) {
      dind[k] = indRight[i];
      v1[k] = sin(i/300.0);
      v2[k] = cos(i/300.0);
      ++k; ++i;
   }
   IndOffset[2] = IndOffset[1]+N_indRight;
   dmaxind[2] = DSET_NVOX(vdset);
   dsstr[2] = SUMA_copy_string(vdomain);
   dtp[2] = VO_type;
   i = 0;
   while (i < DSET_NVOX(vdset)) {
      if (mask[i]) {
         dind[k] = i;
         v1[k] = fv[i];
         v2[k] = k-IndOffset[2]; 
         ++k;
      }
      ++i;
   }
   IndOffset[3] = IndOffset[2]+N_mask; /* for convenience, always create the final unreachable index */
   

   /* Put the dataset together */
   sdset = SUMA_CreateDsetPointer( "cifti_toy", 
                                  SUMA_CIFTI_BUCKET, 
                                  NULL,
                                  NULL,       /* no domain str specified */
                                  N_Alloc    /* Number of nodes allocated for */
                                    );  
   /* Setup the CIFTI domains */
   SUMA_CIFTI_Set_Domains(sdset, 3, dind, IndOffset, dmaxind, dtp, dsstr);
   
   if (!SUMA_AddDsetNelCol (sdset, "Need", 
                                  SUMA_NODE_FLOAT, (void *)v1, NULL ,1)) {
      SUMA_S_Err("Failed in SUMA_AddDsetNelCol");
   }
   
   if (!SUMA_AddDsetNelCol (sdset, "Coffee", 
                                  SUMA_NODE_FLOAT, (void *)v2, NULL ,1)) {
      SUMA_S_Err("Failed in SUMA_AddDsetNelCol");
   }
   
   SUMA_Free_Surface_Object(SO0); SO0 = NULL;
   SUMA_Free_Surface_Object(SO1); SO1 = NULL;
   DSET_delete(vdset);
   SUMA_ifree(v1); SUMA_ifree(v2); SUMA_ifree(dind);
   SUMA_ifree(IndOffset); SUMA_ifree(dmaxind); SUMA_ifree(dtp);
   for (i=0; i<3; ++i) SUMA_ifree(dsstr[i]);
   SUMA_ifree(dsstr);
   SUMA_ifree(mask);
   SUMA_ifree(fv);
   
   SUMA_RETURN(sdset);
}

   
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"SUMA_TestDsetIO"}; 
   int *NodeDef=NULL;
   byte *maskrow, *maskcol;
   int i, i3, N_NodeDef, N_Alloc, flg;
   float *r=NULL, *g=NULL, *b=NULL, *rgb=NULL;
   char stmp[500], idcode[50], **s, *si, *OutName = NULL;
   NI_element *nel=NULL;
   NI_stream ns;
   int found = 0, NoStride = 0;
   byte *bt=NULL;
   SUMA_DSET * dset = NULL, *ndset=NULL, *cdset = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   SUMA_LH("Creating CIFTI toy");
   if (!(cdset = SUMA_Create_Fake_CIFTI(NULL, NULL, NULL))) {
      SUMA_S_Err("Failed to create fake CIFTI");
      exit(1);
   }
   SUMA_WriteDset_eng("ToyCifti.niml.dset", cdset, SUMA_ASCII_NIML, 1, 1, 1);
   SUMA_FreeDset(cdset); cdset = NULL;

   SUMA_LH("Creating Surface Data ...");
   /* Create some sample data*/
      /* let us create some colors to go on each node */
      N_Alloc = 50;
      NodeDef = (int *)SUMA_malloc(N_Alloc * sizeof(int));
      r = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      g = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      b = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      bt = (byte *)SUMA_malloc(N_Alloc * sizeof(byte));
      s = (char **)SUMA_malloc(N_Alloc * sizeof(char *));
      maskrow = (byte *)SUMA_malloc(N_Alloc * sizeof(byte));
      maskcol = (byte *)SUMA_malloc(10*sizeof(byte)); 
      for (i=0; i<10; ++i) { 
         if (i==1 || i == 3) maskcol[i]=0; 
         else maskcol[i] = 1; 
      }
      N_NodeDef = N_Alloc;
      for (i=0; i<N_NodeDef; ++i) {
         NodeDef[i] = i;
         r[i] = sin((float)i/N_NodeDef*5);
         g[i] = sin((float)i/N_NodeDef*10);
         b[i] = cos((float)i/N_NodeDef*7);
         bt[i] = (byte)(4*b[i]);
         sprintf(stmp,"teststr_%d", i);
         s[i] = SUMA_copy_string(stmp);
         if (i==3 || i== 7 || i==33) maskrow[i] = 1; else maskrow[i]=0;
      }
      /* what if you had a vector of say, triplets */
      rgb = (float *)SUMA_malloc(3 * N_Alloc * sizeof(float));
      for (i=0; i<N_NodeDef; ++i) {
         i3 = 3*i;
         rgb[i3] = r[i];
         rgb[i3+1] = g[i];
         rgb[i3+2] = b[i];
      }
      
  {
      float *xc, *de, *amp;
      int dof;
      float par[3];
      /* store some statistics */
      xc = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      de = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      amp = (float *)SUMA_malloc(N_Alloc * sizeof(float));
      for (i=0; i<N_NodeDef; ++i) {
         xc[i] = rand()%1000/1000.0 * 1.0;
         de[i] = rand()%1000/1000.0 * 30;
         amp[i] = rand()%1000/1000.0 * 5.0;
      }
     
      SUMA_LH("Creating dset pointer");
      dset = SUMA_CreateDsetPointer(
                                    "ExpandingRing_ResponseDelay",         /* some label */
                                    SUMA_NODE_BUCKET,                /* mix and match */
                                    NULL,    /* no idcode, let the function create one from the filename*/
                                    NULL,       /* no domain str specified */
                                    N_Alloc    /* Number of nodes allocated for */
                                    ); /* DO NOT free dset, if it is stored in DsetList */
      #ifdef SUMA_COMPILED
      SUMA_LH("inserting dset pointer into list");
      if (!SUMA_InsertDsetPointer(&dset, SUMAg_CF->DsetList,0)) {
         SUMA_SL_Err("Failed to insert dset into list");
         exit(1);
      }  
      #endif
         /* form the dataset */
   SUMA_LH("Adding stat NodeDef column ...");
   if (!SUMA_AddDsetNelCol (   dset, /* the famed nel */ 
                           "Node Indices", 
                           SUMA_NODE_INDEX, /* the column's type (description),
                                               one of SUMA_COL_TYPE */
                           (void *)NodeDef, /* column pointer p, here it is
                                             the list of node indices */
                           NULL  /* that's an optional structure containing 
                                    attributes of the added column. 
                                    Not used at the moment */
                           ,1 /* stride, useful when you need to copy a column
                                 from a multiplexed vector. Say you have in p 
                                 [rgb rgb rgb rgb], to set the g column you 
                                 send in p+1 for the column pointer and a stride
                                 of 3 */
                           )) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);                    
      }
      
      SUMA_LH("Adding stat other columns...");
         par[0] = 120; par[1] = 2; par[2] = 2;
         if (!SUMA_AddDsetNelCol (dset, "XcorrCoef", 
                                  SUMA_NODE_XCORR, (void *)xc, (void *)par ,1)) {
            fprintf (stderr,
                     "Error  %s:\nFailed in SUMA_AddDsetNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddDsetNelCol (dset, "Delay", 
                                  SUMA_NODE_FLOAT, (void *)de, NULL ,1)) {
            fprintf (stderr,
                     "Error  %s:\nFailed in SUMA_AddDsetNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddDsetNelCol (dset, "Amplitude", SUMA_NODE_FLOAT, (void *)amp, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddDsetNelCol", FuncName);
            exit(1);
         }
      SUMA_LH("History note");
      if (!SUMA_AddNgrHist(dset->ngr, FuncName, argc, argv)) {
         SUMA_SL_Err("History addition failed.");
         exit(1);
      }
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("SampleDset", dset, SUMA_ASCII_NIML, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("SampleDset", dset, SUMA_ASCII_NIML, 1, 1); 
      #endif
      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL;
      }

      #ifdef SUMA_COMPILED
      /* Now create a new dataset nel 
      no need to worry about losing previous dset because it is in 
      SUMAg_CF->DsetList*/
      #else
      /* free dset by hand */
      SUMA_LH("Freeing datasets ...");
      if (dset) SUMA_FreeDset((void *)dset);
      dset = NULL;
      #endif
      
   }
   SUMA_LH("Creating dset pointer");
   dset = SUMA_CreateDsetPointer(
                                 "SomethingLikeFileName",         /* usually the filename */
                                 SUMA_NODE_BUCKET,                /* mix and match */
                                 NULL,    /* no idcode, let the function create one from the filename*/
                                 NULL,       /* no domain str specified */
                                 N_Alloc    /* Number of nodes allocated for */
                                 ); /* DO NOT free dset, it is store in DsetList */
   #ifdef SUMA_COMPILED
   SUMA_LH("inserting dset pointer into list");
   if (!SUMA_InsertDsetPointer(&dset, SUMAg_CF->DsetList,0)) {
      SUMA_SL_Err("Failed to insert dset into list");
      exit(1);
   }  
   #endif
                           
   /* form the dataset */
   SUMA_LH("Adding NodeDef column ...");
   if (!SUMA_AddDsetNelCol (   dset, /* the famed nel */ 
                           "le Node Def", 
                           SUMA_NODE_INDEX, /* the column's type (description),
                                               one of SUMA_COL_TYPE */
                           (void *)NodeDef, /* column pointer p, here it is
                                             the list of node indices */
                           NULL  /* that's an optional structure containing 
                                    attributes of the added column. 
                                    Not used at the moment */
                           ,1 /* stride, useful when you need to copy a column
                                 from a multiplexed vector. Say you have in p 
                                 [rgb rgb rgb rgb], to set the g column you 
                                 send in p+1 for the column pointer and a stride
                                 of 3 */
                           )) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);                    
      }
      
      SUMA_LH("Adding other columns...");
      NoStride = 0;
      if (NoStride) {
         /* insert separate r, g and b column */
         if (!SUMA_AddDsetNelCol (dset, "Le R", SUMA_NODE_R, (void *)r, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddDsetNelCol (dset, "Le G", SUMA_NODE_G, (void *)g, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddDsetNelCol (dset, "Le B", SUMA_NODE_B, (void *)b, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
      } else {
         /* insert from multiplexed rgb vector */
         if (!SUMA_AddDsetNelCol (dset, "le R", SUMA_NODE_R, (void *)rgb, NULL ,3 )) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddDsetNelCol (dset, "Le G", SUMA_NODE_G, (void *)(rgb+1), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }

         if (!SUMA_AddDsetNelCol (dset, "Le B", SUMA_NODE_B, (void *)(rgb+2), NULL ,3)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
         #if 0
         SUMA_LH("Testing insert column ...");
         /* Test NI_inset_column_stride here please */
         if (!SUMA_InsertDsetNelCol (dset, "Le G2", SUMA_NODE_G, (void *)(rgb+1), NULL ,3, 0)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
         }
         #endif
      }
      { int suc; SUMA_LH("Where are the attributes?"); NEL_WRITE_TX(dset->ngr,"fd:1",suc); }
      /* add the byte column, just to check multi type nightmares */
      if (!SUMA_AddDsetNelCol (dset, "Le byte moi", SUMA_NODE_BYTE, (void *)bt, NULL ,1)) {
            fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
            exit(1);
      }
      
      
      SUMA_LH("Testing write ops before adding string columns ...");
      /* before adding a string column ... */
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_write_all_num", dset, SUMA_1D, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_write_all_num", dset, SUMA_1D, 1, 1); 
      #endif

      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL;
      }
      
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_writebi_all_num", dset, SUMA_BINARY_NIML, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_writebi_all_num", dset, SUMA_BINARY_NIML, 1, 1); 
      #endif
      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL; 
      }
      
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_writeas_all_num", dset, SUMA_ASCII_NIML, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_writeas_all_num", dset, SUMA_ASCII_NIML, 1, 1); 
      #endif
      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL;
      }
      
      /* Now shuffle some columns (then put them back) */
      SUMA_S_Note("NOTE THAT SHUFFLING here does not take care of attributes inside dset, but only dnel");
      NI_move_column(dset->dnel, -1, 2);
      SUMA_ShowNel(dset->dnel);
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_writeas_all_shuff_num", dset, SUMA_ASCII_NIML, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_writeas_all_shuff_num", dset, SUMA_ASCII_NIML, 1, 1); 
      #endif
      NI_move_column(dset->dnel, 2, -1);
      SUMA_ShowNel(dset->dnel);
     
      /* zero out some columns and test operations */
      SUMA_LH("Trying masking operations");
      ndset = SUMA_MaskedCopyofDset(dset, maskrow, maskcol, 1, 0); 
      SUMA_LH("Done");
      /* try also:
         ndset = SUMA_MaskedCopyofDset(dset, maskrow, maskcol, 0, 1);
         ndset = SUMA_MaskedCopyofDset(dset, maskrow, maskcol, 0, 0);
         ndset = SUMA_MaskedCopyofDset(dset, maskrow, NULL, 1, 0); 
         ndset = SUMA_MaskedCopyofDset(dset, NULL, NULL, 1, 0); 
         ndset = SUMA_MaskedCopyofDset(dset, maskrow, maskcol, 1, 0); 
      */
      if (!ndset) {
         SUMA_SL_Err("Failed in SUMA_MaskedCopyofDset");
      } else {
         #ifdef SUMA_COMPILED
         OutName = SUMA_WriteDset_s ("Test_writeas_MaskedCopy_num", ndset, SUMA_ASCII_NIML, 1, 1); 
         #else
         OutName = SUMA_WriteDset_ns ("Test_writeas_MaskedCopy_num", ndset, SUMA_ASCII_NIML, 1, 1); 
         #endif
         if (!OutName) {
            SUMA_SL_Err("Write Failed.");
         } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
            SUMA_free(OutName); OutName = NULL;
         }
         SUMA_free(ndset); ndset = NULL;
      }
      
      SUMA_LH("Adding a string column");
      /* add a string column, just for kicks ..*/
      if (!SUMA_AddDsetNelCol (dset, "la string", SUMA_NODE_STRING, (void *)s, NULL, 1)) {
         fprintf (stderr,"Error  %s:\nFailed in SUMA_AddNelCol", FuncName);
         exit(1);  
      }
      
      /* now try to create a masked copy, this should fail */
      fprintf (stderr,"%s: Attempting to mask a not all numeric dset, this should fail\n", FuncName);
      ndset = SUMA_MaskedCopyofDset(dset, maskrow, maskcol, 1, 0); 
      if (ndset) {
         fprintf (stderr,"Error  %s:\nWhat the hell? This should not be supported.", FuncName);
         exit(1);
      }else{
         fprintf (stderr,"%s: Good, failed.\n", FuncName);
      }  
      /* after adding a string column ... */
      SUMA_LH("Writing datasets ...");
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_writeas", dset, SUMA_ASCII_NIML, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_writeas", dset, SUMA_ASCII_NIML, 1, 1); 
      #endif
      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL;
      }
      
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_writebi", dset, SUMA_BINARY_NIML, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_writebi", dset, SUMA_BINARY_NIML, 1, 1); 
      #endif
      
      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL;
      }
      
      SUMA_LH("Writing to 1D a dataset that is not all numbers.\nThis should fail.\n");
      #ifdef SUMA_COMPILED
      OutName = SUMA_WriteDset_s ("Test_write", dset, SUMA_1D, 1, 1); 
      #else
      OutName = SUMA_WriteDset_ns ("Test_write", dset, SUMA_1D, 1, 1); 
      #endif
      if (!OutName) {
         SUMA_SL_Err("Write Failed.");
      } else { fprintf (stderr,"%s:\nDset written to %s\n", FuncName, OutName); 
         SUMA_free(OutName); OutName = NULL;
      }
      
     
      
        
      /* How about loading some data */
      #ifdef SUMA_COMPILED
      /* Now create a new dataset nel 
      no need to worry about losing previous dset because it is in 
      SUMAg_CF->DsetList*/
      #else
      /* free dset by hand */
      SUMA_LH("Freeing datasets ...");
      if (dset) SUMA_FreeDset((void *)dset);
      dset = NULL;
      #endif
      
      SUMA_LH("Fresh dataset ...");
      dset = SUMA_NewDsetPointer();
      SUMA_LH("Reading dataset ...");
      DSET_READ(dset, "file:Test_writebi.niml.dset"); if (!dset->ngr) exit(1);
      /* insert the baby into the list */
      
      #ifdef SUMA_COMPILED
      SUMA_LH("Inserting newly read element into list\n");
      if (!SUMA_InsertDsetPointer(&dset, SUMAg_CF->DsetList, 0)) {
         char *newid = NULL;
         SUMA_SL_Err("Failed to insert dset into list");
         /* Now change the idcode of that baby */
         newid = UNIQ_hashcode(SDSET_ID(dset));
         NI_set_attribute(dset->dnel, "self_idcode", newid); SUMA_free(newid);
         SUMA_LH("Trying to insert dset with a new id ");
         if (!SUMA_InsertDsetPointer(&dset, SUMAg_CF->DsetList, 0)) {
            SUMA_SL_Err("Failed to insert dset into list\nI failed to succeed, snif.");
            exit(1);
         }
         SUMA_LH("Lovely, that worked...");
      }
      #endif
           
      /* show me the whole thing. Don't do this for an enormous nel */
         /* SUMA_ShowNel((void*)dset->nel); */
         
      
      /* I want the pointer to the green column but do not know its index */
         {   
            int j, *iv, N_i;
            float *fp;
            fprintf (stderr,"---Looking for green column ---\n");
            iv = SUMA_GetDsetColIndex (dset, SUMA_NODE_G, &N_i);
            if (!iv) {
               fprintf (stderr,"Error %s: Failed to find column.\n"
                           , FuncName);
            } else {
               fprintf (stderr,"\t%d columns of type SUMA_NODE_G found.\n",
                           N_i);
               if (N_i) {
                  fprintf (stderr,"\tReporting values at index %d\n", iv[0]);
                  fp = (float *)dset->dnel->vec[iv[0]]; /* I know we only have one 
                                                   such col. here */
                  for (j=0; j < SDSET_VECLEN(dset); ++j) {
                     fprintf (stderr,"%f, ", fp[j]);
                  }
                  SUMA_free(iv); iv = NULL;
               }
            }
            
                  
         }
          
   
   /* Now show me that baby,*/
   SUMA_LH("I wanna Show You Some Info");
   si = SUMA_DsetInfo (dset, 0);
   fprintf (SUMA_STDERR,"Output of DsetInfo:\n%s\n", si); SUMA_free(si); si=NULL; 
   
   if (LocalHead) fprintf(stderr," %s:-\nFrenching ...\n", FuncName);

   /* free other stuff */
   if (r) SUMA_free(r); r = NULL;
   if (g) SUMA_free(g); g = NULL;
   if (b) SUMA_free(b); b = NULL;
   if (rgb) SUMA_free(rgb); rgb = NULL;
   if (maskrow) SUMA_free(maskrow); maskrow = NULL;
   if (maskcol) SUMA_free(maskcol); maskcol = NULL;
   if (NodeDef) SUMA_free(NodeDef); NodeDef = NULL;  
   if (s) {
      for (i=0; i<N_NodeDef; ++i) {
         if (s[i]) SUMA_free(s[i]);
      }
      SUMA_free(s);
   }

   #ifdef SUMA_COMPILED
   /* dset and its contents are freed in SUMA_Free_CommonFields */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   #else
   /* free dset by hand */
   if (dset) SUMA_FreeDset((void *)dset);
   #endif
   
   SUMA_RETURN (0);
}/* Main */
   
