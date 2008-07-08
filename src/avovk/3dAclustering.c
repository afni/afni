#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "../mrilib.h"
#include "cluster.h"
#include "thd_segtools.h"


// from command.c file

static void display_help(void)
{ printf ("Clustering 4segmentation, command-line version.\n");
  printf ("USAGE: cluster [options]\n");
  printf ("options:\n");
  printf ("  -v, --version Version information\n");
  printf ("  -f filename   File loading\n");
  printf ("  -cg a|m       Specifies whether to center each row\n"
          "                in the data\n"
          "                a: Subtract the mean of each row\n"
          "                m: Subtract the median of each row\n"
          "                (default is no centering)\n");
  printf ("  -ng           Specifies to normalize each row in the data\n"
          "                (default is no normalization)\n");
  printf ("  -ca a|m       Specifies whether to center each column \n"
          "                in the data\n"
          "                a: Subtract the mean of each column\n"
          "                m: Subtract the median of each column\n"
          "                (default is no centering)\n");
  printf ("  -na           Specifies to normalize each column in the data\n"
          "                (default is no normalization)\n");
  printf ("  -u jobname    Allows you to specify a different name for the \n"
          "                output files.\n"
          "                (default is derived from the input file name)\n");
  printf ("  -g [0..8]     Specifies the distance measure for gene clustering \n"
         );
  printf ("                0: No gene clustering\n"
          "                1: Uncentered correlation\n"
          "                2: Pearson correlation\n"
          "                3: Uncentered correlation, absolute value\n"
          "                4: Pearson correlation, absolute value\n"
          "                5: Spearman's rank correlation\n"
          "                6: Kendall's tau\n"
          "                7: Euclidean distance\n"
          "                8: City-block distance\n"
          "                (default: 1)\n");
  printf ("  -k number     Specifies whether to run k-means clustering\n"
          "                instead of hierarchical clustering, and the number\n"
          "                of clusters k to use. \n"
          "                Default is kmeans with k = 3 clusters\n");
  printf ("  -c number     Force the program to do hierarchical clsutering\n"
          "                and specifies the number of clusters for tree\n"
          "                cutting after hierarchical clustering.\n"
          "       Options -c and -k are mutually exclusive\n");
  printf ("  -r number     For k-means clustering, the number of times the\n"
          "                k-means clustering algorithm is run\n"
          "                (default: 1)\n");
  printf ("  -m [msca]     Specifies which hierarchical clustering method to"
          "                use:\n"
          "                m: Pairwise complete-linkage\n"
          "                s: Pairwise single-linkage\n"
          "                c: Pairwise centroid-linkage\n"
          "                a: Pairwise average-linkage\n"
          "                (default: m)\n");
  return;
}



/* ========================================================================= */

int main(int argc, char **argv)
{ 
   int ii=0, ncol=0, nrow=0, nl=0, nc=0, posi=0, posj=0, posk=0;
   //int nclust=atoi(argv[2]);

   //from command.c

   int i = 1;
   char* filename = NULL;
   int l = 0;
   int s = 0;
   int x = 2;
   int y = 1;
   int Rows, Columns;
   char arraymetric = '\0';
   char method = 'm';
   char cg = '\0';
   char ca = '\0';
   int ng = 0;
   int na = 0;
   char *prefix = NULL;
   char *maskname = NULL;
   THD_3dim_dataset *in_set=NULL, *clust_set=NULL;
   THD_3dim_dataset *mset =NULL, *dist_set=NULL ;
   byte *mask=NULL;
   int nmask=-1, mnx=-1, mny=-1, mnz=-1;
   OPT_KMEANS oc;
   
   
   oc.r = 1;
   oc.k = 0;
   oc.kh = 0;
   oc.jobname = NULL;
   oc.distmetric = 'u';
   oc.verb = 1;
   while (i < argc)
   { const char* const argument = argv[i];
    i++;
    if (strlen(argument)<2)
    { printf("ERROR: missing argument\n");
      return 0;
    }
    if (argument[0]!='-')
    { printf("ERROR: unknown argument\n");
      return 0;
    }
    if(!strcmp(argument,"--version") || !strcmp(argument,"-v"))
    { clusterlib_display_version();
      return 0;
    }
    if(     !strcmp(argument,"--help") 
         || !strcmp(argument,"-h") 
         || !strcmp(argument,"-help") )
    { display_help();
      return 0;
    }
    if(!strcmp(argument,"-cg"))
    { if (i==argc || strlen(argv[i])>1 || !strchr("am",argv[i][0]))
      { printf ("Error reading command line argument cg\n");
        return 0;
      }
      cg = argv[i][0];
      i++;
      continue;
    }
    if(!strcmp(argument,"-ca"))
    { if (i==argc || strlen(argv[i])>1 || !strchr("am",argv[i][0]))
      { printf ("Error reading command line argument ca\n");
        return 0;
      }
      ca = argv[i][0];
      i++;
      continue;
    }
    if(!strcmp(argument,"-prefix"))
    { if (i==argc)
      { printf ("Need name after -prefix\n");
        return 0;
      }
      prefix = argv[i];
      i++;
      continue;
    }

   if(!strcmp(argument,"-mask"))
    { if (i==argc)
      { printf ("Need name after -mask\n");
        return 0;
      }
      maskname = argv[i];
      i++;
      continue;
    }
    if(!strcmp(argument,"-ng"))
    { ng = 1;
      continue;
    }
    if(!strcmp(argument,"-na"))
    { na = 1;
      continue;
    }
    switch (argument[1])
    { case 'l': l=1; break;
      case 'u':
      { if (i==argc)
        { printf ("Error reading command line argument u: "
                  "no job name specified\n");
          return 0;
        }
        oc.jobname = clusterlib_setjobname(argv[i],0);
        i++;
        break;
      }
      case 'f':
      { if (i==argc)
        { printf ("Error reading command line argument f: "
                  "no file name specified\n");
          return 0;
        }
        filename = argv[i];
        i++;
        break;
      }
      case 'g':
      { int g;
        if (i==argc)
        { printf ("Error reading command line argument g: parameter missing\n");
          return 0;
        }
        g = clusterlib_readnumber(argv[i]);
        if (g < 0 || g > 9)
        { printf ("Error reading command line argument g: "
                  "should be between 0 and 9 inclusive\n");
          return 0;
        }
        i++;
        oc.distmetric = clusterlib_getmetric(g);
        break;
      }

      case 'k':
      { if (i==argc)
        { printf ("Error reading command line argument k: "
                  "parameter missing\n");
          return 0;
        }
        if (oc.kh > 0) {
            ERROR_message("-k and -c options are mutually exclusive\n");
            return 0;
        }
        oc.k = clusterlib_readnumber(argv[i]);
        if (oc.k < 1)
        { printf ("Error reading command line argument k: "
                  "a positive integer is required\n");
          return 0;
        }
        i++;
        break;
      }
    case 'c':
      { if (i==argc)
        { printf ("Error reading command line argument c: parameter missing\n");
          return 0;
        }
        if (oc.k > 0) {
            ERROR_message("-k and -c options are mutually exclusive\n");
            return 0;
        }
        oc.kh = clusterlib_readnumber(argv[i]);
        if (oc.kh < 1)
        { printf ("Error reading command line argument c: "
                  "a positive integer is required\n");
          return 0;
        }
        i++;
        break;
      }
   case 'r':
      { if (i==argc)
        { printf ("Error reading command line argument r: parameter missing\n");
          return 0;
        }
        oc.r = clusterlib_readnumber(argv[i]);
        if (oc.r < 1)
        { printf ("Error reading command line argument r: "
                  "a positive integer is required\n");
          return 0;
        }
        i++;
        break;
      }
   case 'm':
      { if (i==argc || strlen(argv[i])>1 || !strchr("msca",argv[i][0]))
	  { printf ("Error reading command line argument m: "
               "should be 'm', 's', 'c', or 'a'\n");
	    return 0;
	  }
        method = argv[i][0];
        i++;
        break;
      }
      default: 
         printf ("Unknown option %s\n", argv[i-1]);
         return 0;
    }
    
   }
   if (oc.k <= 0 && oc.kh <= 0) oc.k = 3;
   
   if(oc.jobname == NULL) oc.jobname = clusterlib_setjobname(filename,1);

   /*  else
   { display_help();
    return 0;
   }
   */

    if (argc < 2) {
      display_help();
      return 0;
    }

   
   /* load dsets and prepare array data for sending to clustering functions */
   
   if (!prefix) {
      prefix = "clusty";
      THD_force_ok_overwrite(1) ;   /* don't worry about overwriting */
   }
   
   /* Read in dset */
   if (oc.verb) fprintf(stderr,"Patience, reading %s... ", filename);
   in_set = THD_open_dataset(filename);
   CHECK_OPEN_ERROR(in_set,filename) ;
   ncol = DSET_NVALS(in_set);
   nrow = DSET_NVOX(in_set);
   DSET_load(in_set) ; CHECK_LOAD_ERROR(in_set) ;

   /* Read in mask */
   if (maskname) {
      mset = THD_open_dataset(maskname) ;
      CHECK_OPEN_ERROR(mset,maskname) ;
      mnx = DSET_NX(mset); mny = DSET_NY(mset); mnz = DSET_NZ(mset);
      DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
      mask = THD_makemask( mset, 0, 1.0f,0.0f ); DSET_delete(mset);
      if( mask == NULL ) ERROR_exit("Can't make mask") ;
      nmask = THD_countmask( mnx*mny*mnz , mask ) ;
      INFO_message("%d voxels in the [%dx%dx%d] mask",nmask, mnx, mny, mnz) ;
      if( nmask < 1 ) ERROR_exit("mask %s is empty?!", maskname) ;
      if (  mask &&
            (mnx != DSET_NX(in_set) || 
             mny != DSET_NY(in_set) || 
             mnz != DSET_NZ(in_set) ) ) {
         ERROR_exit("Dimension mismatch between mask and input dset");      
      }
   }

   /* Now call clustering function */
   if (!thd_Acluster ( in_set,
                     mask, nmask,
                     &clust_set,
                     &dist_set ,
                     oc)) {
      ERROR_exit("Failed in thd_Acluster");                 
   }
   

   /* add history to output data and write them  out */
   if( oc.verb && 
       (clust_set || dist_set)) 
         ININFO_message("\nWriting datasets: %s",prefix) ;
   if (clust_set) {
      EDIT_dset_items(  clust_set , ADN_prefix  , prefix, ADN_none);
      tross_Copy_History( in_set , clust_set ) ;
      tross_Make_History( "3dAclustering" , argc, argv , clust_set ) ;
      DSET_write(clust_set); DSET_unload(clust_set); 
      DSET_delete(clust_set); clust_set = NULL;
   }
   if (dist_set) {
      tross_Copy_History( in_set , dist_set ) ;
      tross_Make_History( "3dAclustering" , argc, argv , dist_set ) ;
      DSET_write(dist_set); DSET_unload(dist_set); 
      DSET_delete(dist_set); dist_set = NULL;
   }

 
   
   if (mask) free(mask); mask = NULL;
   if (oc.jobname) free(oc.jobname); oc.jobname = NULL;
   

   fprintf (stderr,"\n");
   return 0;
   }

