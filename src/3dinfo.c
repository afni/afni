/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd_ttatlas_query.h"
#include "thd_atlas.h"

void Syntax(void)
{
   printf("\n"
    "Prints out sort-of-useful information from a 3D dataset's header\n"
    "Usage: 3dinfo [-verb OR -short] dataset [dataset ...]\n"
    "  -verb means to print out lots of stuff\n"
    "  -VERB means even more stuff\n"
    "  -short means to print out less stuff [now the default]\n"
    "\n"
    "Alternative Usage (without either of the above options):\n"
    "  3dinfo -label2index label dataset\n"
    "  * Prints to stdout the index corresponding to the sub-brick with\n"
    "    the name label, or a blank line if label not found.\n"
    "  * If this option is used, then the ONLY output is this sub-brick index.\n"
    "    This is intended to be used in a script, as in this tcsh fragment:\n"
    "      set face = `3dinfo -label2index Face#0 AA_Decon+orig`\n"
    "      set hous = `3dinfo -label2index House#0 AA_Decon+orig`\n"
    "      3dcalc -a AA_Decon+orig\"[$face]\" -b AA_Decon+orig\"[$hous]\" ...\n"
    "  * Added per the request and efforts of Colm Connolly.\n"
    "\n"
    "Alternate Alternative Usage:\n"
    "  3dinfo <OPTION> [OPTION ..] dataset [dataset ...]\n"
    "  Outputs a specific piece of information depending on OPTION.\n"
    "  ========= Options producing one value (string) ============\n"
    "   -is_nifti: 1 if dset is NIFTI format, 0 otherwise\n"
    "   -space: dataset's space\n"
    "   -av_space: AFNI format's view extension for the space\n"
    "   -is_oblique: 1 if dset is oblique\n"
    "   -prefix: Return the prefix\n"
    "   -prefix_noext: Return the prefix without extensions\n"
    "   -n[i|j|k]: Return the number of voxels in i, j, k dimensions\n"
    "   -nijk: Return ni*nj*nk"
    "   -nt: Return number of points in time, or number of sub-bricks\n"
    "  ==== Options producing multiple values (strings of multiple lines)====\n"
    "   -labeltable: Show label table, if any\n"
    "   -labeltable_as_atlas_points: Show label table in atlas point format.\n"
    "\n"
    " Example:\n"
    "    Produce a table of results using 1-value-options\n"
    "    3dinfo  -prefix_noext -prefix -space -ni -nj -nk -nt  \\\n"
    "            DSET1+orig DSET2.nii\n"
    "\n"
   ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

typedef enum {
   CLASSIC=0, DSET_SPACE, AV_DSET_SPACE, IS_NIFTI,
   IS_OBLIQUE, PREFIX , PREFIX_NOEXT, NI, NJ, NK, NT, NIJK,
   LTABLE, LTABLE_AS_ATLAS_POINT_LIST,
   N_FIELDS } INFO_FIELDS; /* Leave N_FIELDS at the end */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg , verbose = -1 ;
   char *outbuf ;
   char *labelName = NULL;
   INFO_FIELDS sing[512]; 
   int iis=0, N_sing = 0;
   
   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   mainENTRY("3dinfo main") ; machdep() ; 

   iarg = 1 ;
   while (iarg < argc && argv[iarg][0] == '-') {
           if( strncmp(argv[iarg],"-verb" ,5) == 0 ){ verbose =  0; iarg++; }
           if( strncmp(argv[iarg],"-VERB" ,5) == 0 ){ verbose =  1; iarg++; }
      else if( strncmp(argv[iarg],"-short",5) == 0 ){ verbose = -1; iarg++; }

      else if ( strncmp(argv[iarg],"-label2",7) == 0 )
      {
        iarg++;
        if (iarg >= argc)
           ERROR_exit( "3dinfo needs an argument after -label2number\n");
        labelName = malloc(sizeof(char) * 2048);
        strcpy(labelName, argv[iarg]);
        iarg++;
      }
      else if( strncmp(argv[iarg],"-space",6) == 0) { 
         sing[N_sing++] = DSET_SPACE; iarg++;
      } else if( strncmp(argv[iarg],"-av_space",6) == 0) { 
         sing[N_sing++] = AV_DSET_SPACE; iarg++;
      } else if( strncmp(argv[iarg],"-is_nifti",6) == 0) { 
         sing[N_sing++] = IS_NIFTI; iarg++;
      } else if( strncmp(argv[iarg],"-is_oblique",6) == 0) { 
         sing[N_sing++] = IS_OBLIQUE; iarg++;
      } else if( strcmp(argv[iarg],"-prefix") == 0) {
         sing[N_sing++] = PREFIX; iarg++;
      } else if( strcmp(argv[iarg],"-prefix_noext") == 0) {
         sing[N_sing++] = PREFIX_NOEXT; iarg++;
      } else if( strncmp(argv[iarg],"-ni",3) == 0) {
         sing[N_sing++] = NI; iarg++;
      } else if( strncmp(argv[iarg],"-nj",3) == 0) {
         sing[N_sing++] = NJ; iarg++;
      } else if( strncmp(argv[iarg],"-nk",3) == 0) {
         sing[N_sing++] = NK; iarg++;
      } else if( strncmp(argv[iarg],"-nt",3) == 0) {
         sing[N_sing++] = NT; iarg++;
      } else if( strncmp(argv[iarg],"-nijk",3) == 0) {
         sing[N_sing++] = NIJK; iarg++;
      } else if( strcmp(argv[iarg],"-labeltable") == 0) {
         sing[N_sing++] = LTABLE; iarg++;
      } else if( strcmp(argv[iarg],"-labeltable_as_atlas_points") == 0) {
         sing[N_sing++] = LTABLE_AS_ATLAS_POINT_LIST; iarg++;
      } else {
         ERROR_exit("Option %s unknown", argv[iarg]);
      }
   }
   
   if (N_sing == 0) {
      sing[N_sing++] = CLASSIC;
   }
   
   if (sing[iis] == CLASSIC) PRINT_VERSION("3dinfo") ;
   
   THD_allow_empty_dataset(1) ;  /* 21 Mar 2007 */

   for( ; iarg < argc ; iarg++ ){

     if( argv[iarg][0] == '\0' ) continue ;  /* bad filename */

     dset = THD_open_dataset( argv[iarg] ) ;

     if( dset == NULL ){  /* open failed */

       /* 23 Jan 2008: try again with +orig, +acpc, +tlrc appended */

       if( strchr(argv[iarg],'+')==NULL && strstr(argv[iarg],".nii")==NULL ){
         char str[THD_MAX_NAME] ; int vv , ll=strlen(argv[iarg]) ;
         for( vv=0 ; vv <= LAST_VIEW_TYPE && dset == NULL ; vv++ ){
           strcpy(str,argv[iarg]); if( str[ll-1] == '.' ) str[ll-1] = '\0';
           strcat(str,"+") ; strcat(str,VIEW_codestr[vv]) ;
           dset = THD_open_dataset(str) ;
         }
       }

       if( dset == NULL ){  /* still not open? */
         ERROR_exit("Can't open dataset %s\n", argv[iarg]) ;
       }
     }

     for (iis = 0; iis < N_sing; ++iis) {
        switch (sing[iis]) {
         case CLASSIC:
            if (labelName == NULL )  /*** get and output info ***/
            {
             outbuf = THD_dataset_info( dset , verbose ) ;
             if( outbuf != NULL ){
               printf("\n") ;
               puts(outbuf) ;
               free(outbuf) ; outbuf = NULL ;
             } else {
               ERROR_exit("Can't get info for dataset %s",argv[iarg]) ;
             }
            }
            else   /*** get and output label ***/
            {
             int nval_per = dset->dblk->nvals;
             int foundLabel = 0;
             int ival=0;

             for (ival=0 ; ival < nval_per && !foundLabel; ival++ )
             {
               if (strcmp(DSET_BRICK_LAB(dset,ival), labelName) == 0)
               {
                 printf("%d\n", ival); foundLabel = 1;
               }
             } /* end of for (ival=0 ; ival < nval_per ; ival++ ) */
             if (!foundLabel) printf("\n");
            }

            THD_delete_3dim_dataset( dset , False ) ;
            free(labelName);
            break;
         case DSET_SPACE:
            fprintf(stdout, "%s", dset->atlas_space);
            break;
         case AV_DSET_SPACE:
                 if (!strncmp(dset->atlas_space,"ORIG",4)) 
                  fprintf(stdout, "+orig");
            else if (!strncmp(dset->atlas_space,"ACPC",4)) 
                  fprintf(stdout, "+acpc");
            else if (!strncmp(dset->atlas_space,"TLRC",4)) 
                  fprintf(stdout, "+tlrc");
            else if (!strncmp(dset->atlas_space,"MNI",3)) 
                  fprintf(stdout, "+tlrc");
            else
                  fprintf(stdout, "-----");
            break;
         case IS_NIFTI:
            if (  dset->dblk->diskptr && 
                  dset->dblk->diskptr->storage_mode == STORAGE_BY_NIFTI ) {
               fprintf(stdout,"1");
            } else {
               fprintf(stdout,"0");
            }
            break;
         case IS_OBLIQUE:
            if (dset_obliquity(dset,NULL) > 0) {
               fprintf(stdout,"1");
            } else {
               fprintf(stdout,"0");
            }
            break;
         case PREFIX:
            fprintf(stdout,"%s", DSET_PREFIX(dset));
            break;
         case PREFIX_NOEXT:
            { 
               char *ppp=DSET_prefix_noext(dset);
               fprintf(stdout,"%s", ppp);
               free(ppp);
            }
            break;
         case NI:
            fprintf(stdout,"%d", DSET_NX(dset));
            break;
         case NJ:
            fprintf(stdout,"%d", DSET_NY(dset));
            break;
         case NK:
            fprintf(stdout,"%d", DSET_NZ(dset));
            break;
         case NIJK:
            fprintf(stdout,"%d", DSET_NVOX(dset));
            break;
         case NT:
            fprintf(stdout,"%d", DSET_NUM_TIMES(dset));
            break;
         case LTABLE:
            {
               char *str;
               if ((str = Dtable_to_nimlstring(DSET_Label_Dtable(dset),                                                          "VALUE_LABEL_DTABLE"))) {
                  fprintf(stdout,"%s", str);
                  free(str); 
               } else {
                  fprintf(stdout,"NO_LABEL_TABLE");
               }
            }
            break;
         case LTABLE_AS_ATLAS_POINT_LIST:
            {
               char *str;
               ATLAS_POINT_LIST *apl=NULL;
               if ((apl = 
                     label_table_to_atlas_point_list(DSET_Label_Dtable(dset)))) {
                  atlas_list_to_niml(apl,NULL);
                  free_atlas_point_list(apl); 
               } else {
                  fprintf(stdout,"NO_LABEL_TABLE");
               }
            }
            break;   
         default:
            ERROR_message("Info field not set properly (%d)\n", sing[iis]);
            exit(1);
        }
        if (sing[iis] != CLASSIC) {
         if (N_sing > 1 && iis < N_sing-1) fprintf(stdout,"\t");
         else fprintf(stdout,"\n");
        }
      }
   }

   exit(0) ;
}
