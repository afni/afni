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
    "   -nijk: Return ni*nj*nk\n"
    "   -nv: Return number of points in time\n"
    "        use -nv for the more generic number of sub-bricks\n"
    "   -nvi: The maximum sub-brick index (= nv -1 )\n"
    "   -nt: Return number of points in time\n"
    "        use -nv for the more generic number of sub-bricks\n"
    "   -nti: The maximum time index (= nt -1 )\n"
    "  ==== Options producing one value per sub-brick ========\n"
    "   -fac: Return the float scaling factor\n"
    "   -datum: The data storage type\n"
    "   -min: The minimum value, scaled by fac\n"
    "   -max: The maximum value, scaled by fac\n"
    "   -minus: The minimum value, unscaled.\n"
    "   -maxus: The maximum value, unscaled.\n"
    "  ==== Options producing multiple values (strings of multiple lines)====\n"
    "       You can specify the delimiter between sub-brick parameters with\n"
    "       -sb_delim DELIM. Default DELIM is \"|\"\n"
    "   -labeltable: Show label table, if any\n"
    "   -labeltable_as_atlas_points: Show label table in atlas point format.\n"
    "\n"
    "  === Options affection output format ===\n"
    "   -header_line: Output as the first line the names of attributes\n"
    "                 in each field (column)\n"
    "   -sb_delim SB_DELIM: Delimiter string between sub-brick values\n"
    "                       Default SB_DELIM is \"|\"\n"
    "   -NA_flag NAFLAG: String to use when a field is not found or not\n"
    "                    applicable. Default is \"NA\"\n"
    "   -atr_delim ATR_DELIM: Delimiter string between attributes\n"
    "                         Default ATR_DELIM is the tab character.\n"
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
   IS_OBLIQUE, PREFIX , PREFIX_NOEXT, 
   NI, NJ, NK, NT, NTI, 
   NV, NVI, NIJK,
   LTABLE, LTABLE_AS_ATLAS_POINT_LIST,
   FAC, DATUM, LABEL,
   MIN, MAX, MINUS, MAXUS,
   N_FIELDS } INFO_FIELDS; /* Keep synchronized with Field_Names  
                              Leave N_FIELDS at the end */

char Field_Names[][32]={
   {"-classic-"}, {"space"}, {"AV_space"}, {"is_nifti"},
   {"is_oblique"}, {"prefix"}, {"prefix_noext"}, 
   {"Ni"}, {"Nj"}, {"Nk"}, {"Nt"}, {"Nti"}, 
   {"Nv"}, {"Nvi"}, {"Nijk"}, 
   {"label_table"}, {"LT_as_atlas_point_list"}, 
   {"factor"}, {"datum"}, {"label"}, 
   {"min"}, {"max"}, {"minus"}, {"maxus"},
   {"\0"} }; /* Keep synchronized with INFO_FIELDS */
     
int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg , verbose = -1 ;
   char *outbuf ;
   char *labelName = NULL;
   char *sbdelim = {"|"};
   char *NAflag = {"NA"};
   char *atrdelim = {"\t"};
   INFO_FIELDS sing[512]; 
   int iis=0, N_sing = 0, isb=0, withhead = 0;
   
   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   mainENTRY("3dinfo main") ; machdep() ; 

   iarg = 1 ;
   while (iarg < argc && argv[iarg][0] == '-') {
           if( strncmp(argv[iarg],"-verb" ,5) == 0 ){ verbose =  0; iarg++; continue; }
      else if( strncmp(argv[iarg],"-VERB" ,5) == 0 ){ verbose =  1; iarg++; continue; }
      else if( strncmp(argv[iarg],"-short",5) == 0 ){ verbose = -1; iarg++; continue; }
      else if( strcmp(argv[iarg],"-header_line") == 0 ){ withhead = 1; iarg++; continue; }
      else if ( strncmp(argv[iarg],"-label2",7) == 0 )
      {
        iarg++;
        if (iarg >= argc)
           ERROR_exit( "3dinfo needs an argument after -label2number\n");
        labelName = malloc(sizeof(char) * 2048);
        strcpy(labelName, argv[iarg]);
        iarg++; continue;
      }
      else if( strcmp(argv[iarg],"-sb_delim") == 0) { 
         iarg++; 
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -sb_delim\n");
         sbdelim = argv[iarg];
         iarg++; continue;
      } 
      else if( strcmp(argv[iarg],"-NA_flag") == 0) { 
         iarg++; 
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -NA_flag\n");
         NAflag = argv[iarg];
         iarg++; continue;
      } 
      else if( strcmp(argv[iarg],"-atr_delim") == 0) { 
         iarg++; 
         if (iarg >= argc)
           ERROR_exit( "3dinfo needs a string after -atr_delim\n");
         atrdelim = argv[iarg];
         iarg++; continue;
      } 
      else if( strncmp(argv[iarg],"-space",6) == 0) { 
         sing[N_sing++] = DSET_SPACE; iarg++; continue;
      } else if( strncmp(argv[iarg],"-av_space",6) == 0) { 
         sing[N_sing++] = AV_DSET_SPACE; iarg++; continue;
      } else if( strncmp(argv[iarg],"-is_nifti",6) == 0) { 
         sing[N_sing++] = IS_NIFTI; iarg++; continue;
      } else if( strncmp(argv[iarg],"-is_oblique",6) == 0) { 
         sing[N_sing++] = IS_OBLIQUE; iarg++; continue;
      } else if( strcmp(argv[iarg],"-prefix") == 0) {
         sing[N_sing++] = PREFIX; iarg++; continue;
      } else if( strcmp(argv[iarg],"-prefix_noext") == 0) {
         sing[N_sing++] = PREFIX_NOEXT; iarg++; continue;
      } else if( strncmp(argv[iarg],"-ni",3) == 0) {
         sing[N_sing++] = NI; iarg++; continue;
      } else if( strncmp(argv[iarg],"-nj",3) == 0) {
         sing[N_sing++] = NJ; iarg++; continue;
      } else if( strncmp(argv[iarg],"-nk",3) == 0) {
         sing[N_sing++] = NK; iarg++; continue;
      } else if( strcmp(argv[iarg],"-nt") == 0) {
         sing[N_sing++] = NT; iarg++; continue;
      } else if( strcmp(argv[iarg],"-nti") == 0) {
         sing[N_sing++] = NTI; iarg++; continue;
      } else if( strcmp(argv[iarg],"-nv") == 0) {
         sing[N_sing++] = NV; iarg++; continue;
      } else if( strcmp(argv[iarg],"-nvi") == 0) {
         sing[N_sing++] = NVI; iarg++; continue;
      } else if( strncmp(argv[iarg],"-nijk",3) == 0) {
         sing[N_sing++] = NIJK; iarg++; continue;
      } else if( strcmp(argv[iarg],"-labeltable") == 0) {
         sing[N_sing++] = LTABLE; iarg++; continue;
      } else if( strcmp(argv[iarg],"-labeltable_as_atlas_points") == 0) {
         sing[N_sing++] = LTABLE_AS_ATLAS_POINT_LIST; iarg++; continue;
      } else if( strcmp(argv[iarg],"-fac") == 0) {
         sing[N_sing++] = FAC; iarg++; continue;
      } else if( strcmp(argv[iarg],"-datum") == 0) {
         sing[N_sing++] = DATUM; iarg++; continue;
      } else if( strcmp(argv[iarg],"-label") == 0) {
         sing[N_sing++] = LABEL; iarg++; continue;
      } else if( strcmp(argv[iarg],"-min") == 0) {
         sing[N_sing++] = MIN; iarg++; continue;
      } else if( strcmp(argv[iarg],"-max") == 0) {
         sing[N_sing++] = MAX; iarg++; continue;
      } else if( strcmp(argv[iarg],"-minus") == 0) {
         sing[N_sing++] = MINUS; iarg++; continue;
      } else if( strcmp(argv[iarg],"-maxus") == 0) {
         sing[N_sing++] = MAXUS; iarg++; continue;
      } else {
         ERROR_exit("Option %s unknown", argv[iarg]);
      }
   }
   
   if (N_sing == 0) {
      sing[N_sing++] = CLASSIC;
   }
   
   if (sing[iis] == CLASSIC) PRINT_VERSION("3dinfo") ;
   
   THD_allow_empty_dataset(1) ;  /* 21 Mar 2007 */

   if (withhead) {
      int havenew=0;
      for (iis = 0; iis < N_sing; ++iis) {
         if (sing[iis] != CLASSIC) {
            ++havenew;
            fprintf(stdout, "%s", Field_Names[sing[iis]]);
         }
         if (havenew) {
            if (N_sing > 1 && iis < N_sing-1) fprintf(stdout,"%s",atrdelim);
            else fprintf(stdout,"\n");
         }
      }
   }

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
         case NTI:
            fprintf(stdout,"%d", DSET_NUM_TIMES(dset)-1);
            break;
         case NV:
            fprintf(stdout,"%d", DSET_NVALS(dset));
            break;
         case NVI:
            fprintf(stdout,"%d", DSET_NVALS(dset)-1);
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
         case FAC:
            {
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  fprintf(stdout,"%f%s",
                        DSET_BRICK_FACTOR(dset,isb),
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case DATUM:
            {
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  fprintf(stdout,"%s%s",
                        MRI_TYPE_name[DSET_BRICK_TYPE(dset,isb)],
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case LABEL:
            {
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  fprintf(stdout,"%s%s",
               DSET_BRICK_LABEL(dset,isb) ? DSET_BRICK_LABEL(dset,isb):NAflag,
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
               }
               break;
            }
         case MIN:
         case MINUS:
         case MAX:
         case MAXUS:
            {
               float vv=0.0, tf=0.0;
               for (isb=0; isb<DSET_NVALS(dset); ++isb) {
                  tf = DSET_BRICK_FACTOR(dset,isb) ; if (tf == 0.0) tf = 1.0;
                  if( ISVALID_STATISTIC(dset->stats) ){
                          if (sing[iis] == MINUS) 
                        vv = dset->stats->bstat[isb].min/tf;
                     else if (sing[iis] == MAXUS) 
                        vv = dset->stats->bstat[isb].max/tf; 
                     else if (sing[iis] == MIN) 
                        vv = dset->stats->bstat[isb].min;
                     else if (sing[iis] == MAX) 
                        vv = dset->stats->bstat[isb].max;
                     fprintf(stdout,"%g%s",
                        vv,
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
                  } else {
                     fprintf(stdout,"%s%s",
                        NAflag,
                        (isb == (DSET_NVALS(dset)-1)) ? "" : sbdelim);
                  }
               }
               break;
            }
         default:
            ERROR_message("Info field not set properly (%d)\n", sing[iis]);
            exit(1);
        }
        if (sing[iis] != CLASSIC) {
         if (N_sing > 1 && iis < N_sing-1) fprintf(stdout,"%s",atrdelim);
         else fprintf(stdout,"\n");
        }
      }
   }

   exit(0) ;
}
