#define MAIN
#define SUMA_noFunc

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include "suma_suma.h"


         

int is_archive(char *name)
{
   int a;
   SUMA_PARSED_NAME *pn;
   if (!name) return(0);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(0);
   }
   a = is_archive_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   return(a);
}

int is_archive_pn(SUMA_PARSED_NAME *FN)
{
   int a = 0;
   if (!FN) return(0);
   if (!strcmp(FN->Ext,".tar")) {
      a = 1; 
   } else if (!strcmp(FN->Ext,".tgz")){ 
      a = 1|2; /* With compression flag? 
                  Not sure if we should handle this here
                  Could have an is_compressed query separately...*/
   }
   return(a);
}

int is_pdf(char *name)
{
   int a;
   SUMA_PARSED_NAME *pn;
   if (!name) return(0);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(0);
   }
   a = is_pdf_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   return(a);
}

int is_pdf_pn(SUMA_PARSED_NAME *FN)
{
   int a = 0;
   if (!FN) return(0);
   if (!strcmp(FN->Ext,".pdf")) {
      a = 1; 
   }
   return(a);
}

int is_image(char *name)
{
   int a;
   SUMA_PARSED_NAME *pn;
   if (!name) return(0);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(0);
   }
   a = is_image_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   return(a);
}

int is_image_pn(SUMA_PARSED_NAME *FN)
{
   int a = 0;
   if (!FN) return(0);
   if (!strcmp(FN->Ext,".jpg")) {
      a = 1; 
   }
   return(a);
}

int is_url(char *name) 
{
   int a;
   SUMA_PARSED_NAME *pn;
   if (!name) return(0);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(0);
   }
   a = is_url_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   return(a);
}

int is_url_pn(SUMA_PARSED_NAME *FN) 
{
   int a = 0;
   if (!FN) return(0);
   if (FN->OnDisk) return(0); /* should not be on disk */
   if (strstr(FN->NameAsParsed,"http:")==FN->NameAsParsed) return(1);
   if (strstr(FN->NameAsParsed,"file:")==FN->NameAsParsed) return(1);
   if (strstr(FN->NameAsParsed,"afni.nimh.nih.gov")==FN->NameAsParsed) return(1);
   return(0);
}

int is_local_html(SUMA_PARSED_NAME *FN) 
{
   int a = 0;
   if (!FN) return(0);
   if (!FN->OnDisk) return(0); /* should be on disk */
   if (!strcmp(FN->Ext,".html")) return(1);
   return(0);
}

int is_xmat(char *name) 
{
   int a;
   SUMA_PARSED_NAME *pn;
   if (!name) return(0);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(0);
   }
   a = is_xmat_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   return(a);
}

int is_xmat_pn(SUMA_PARSED_NAME *FN) 
{
   int a = 0;
   if (!FN) return(0);
   if (!FN->OnDisk) return(0); /* should be on disk */
   if (!strcmp(FN->Ext,".xmat")) {
      a = 1; 
   }
   return(a);
}

int ao_with_editor(char *fname)
{
   char cmd[1024];
   static char *viewer=NULL;
   int s;
   if (!viewer && !(viewer=GetAfniTextEditor())) {
      ERROR_message("No text editor");
      return(-1);
   }
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", viewer, fname);
   s = system(cmd);
   return(s);
}

int ao_with_browser(char *fname)
{
   if (!fname) return(-2);
   
   return(whereami_browser(fname));
}

int ao_with_downloader(char *fname, byte back)
{
   char cmd[1024];
   int s;
   static char *downloader=NULL;
   if (!downloader && !(downloader=GetAfniWebDownloader())) {
      ERROR_message("No downloader");
      return(-1);
   }
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"%s %s %c", 
            downloader, fname, back ? '&' : ' ');
   s = system(cmd);
   return(s);
}

int ao_with_afniweb(char *fname)
{
   char ww[1024];
   int s=-3;
   if (!fname) return(-2);
   if (is_url(fname) ) return(ao_with_downloader(fname, 0));
   
   /* Try different locations */
   if (is_pdf(fname)) {
      snprintf(ww,1023*sizeof(char),
               "https://afni.nimh.nih.gov/pub/dist/"
               "edu/data/CD.expanded/afni_handouts/%s",fname);
      if (!(s = ao_with_downloader(ww, 0))) return(ao_with_pdf_viewer(fname));
      else {
         fprintf(stderr,"Status %d on %s\n", s, ww);
      }
      /* repeat for other locations, maybe papers? (Nothing for now...) */
      return(s);   
   }
   if (is_archive(fname)) {
      snprintf(ww,1023*sizeof(char),
               "https://afni.nimh.nih.gov/pub/dist/"
               "tgz/%s",fname);
      if ((s = ao_with_downloader(ww, 0))) {
         /* try class material */
         snprintf(ww,1023*sizeof(char),
               "https://afni.nimh.nih.gov/pub/dist/edu/"
               "data/%s",fname);
         if ((s = ao_with_downloader(ww, 0))) {
            snprintf(ww,1023*sizeof(char),
               "https://afni.nimh.nih.gov/pub/dist/edu/"
               "data/CD/%s",fname);
            if ((s = ao_with_downloader(ww, 0))) {
               snprintf(ww,1023*sizeof(char),
               "https://afni.nimh.nih.gov/pub/dist/"
               "data/%s",fname);
               if ((s = ao_with_downloader(ww, 0))) {
                  fprintf(stderr,"Status %d on %s\n Search also failed under\n"
                                 "https://afni.nimh.nih.gov/pub/dist/tgz\n"
                                 "https://afni.nimh.nih.gov/pub/dist/edu/data/\n"
                                 "https://afni.nimh.nih.gov/pub/dist/data/\n"
                                 , s, ww);
               }
            }
         }
      }   
   }
   return(s);
}

int ao_with_readme(char *fname)
{
   char *rout=NULL;
   int s=-3;
   
   if (!fname) return(-2);
   
   if ((rout = find_readme_file(fname))) {
      view_text_file(rout); free(rout);
      return(0);
   }
   return(s);
}

int ao_with_pdf_viewer(char *fname)
{
   char cmd[1024];
   static char *pdfviewer=NULL;
   int s;
   
   if (!pdfviewer && !(pdfviewer=GetAfniPDFViewer())) {
      ERROR_message("No pdf viewer");
      return(-1);
   }
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", pdfviewer, fname);
   s = system(cmd);
   return(s);
}
                           
int ao_with_image_viewer(char *fname)
{
   char cmd[1024];
   static char *imageviewer=NULL;
   int s;
   
   if (!imageviewer && !(imageviewer=GetAfniImageViewer())) {
      ERROR_message("No image viewer");
      return(-1);
   }
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", imageviewer, fname);
   s = system(cmd);
   return(1);
}
                           
int ao_with_afni(char *fname)
{
   char cmd[1024];
   int s;
   
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"afni %s &", fname);
   s = system(cmd);
   return(s);
}

int ao_with_1dplot(char *fname)
{
   char cmd[1024];
   int s;
   
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"1dplot %s &", fname);
   s = system(cmd);
   return(s);
}

int ao_with_ExamineXmat(char *fname)
{
   char cmd[1024];
   int s;
   if (!fname) return(-2);
   
   snprintf(cmd,1023*sizeof(char),"ExamineXmat -input %s &", fname);
   s = system(cmd);
   return(s);
}

int ao_with_suma(char *name)
{
   char cmd[1024];
   int a;
   SUMA_PARSED_NAME *pn;
   
   if (!name) return(-2);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(-3);
   }
   
   a = ao_with_suma_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   
   return(a);
}


int ao_with_suma_pn(SUMA_PARSED_NAME *FN) 
{
   int a = 0, s;
   char cmd[1024];
   if (!FN) return(-2);
   if (!FN->OnDisk) return(-3); /* should be on disk */
   
   s = -4;
   if (FN->StorageMode == STORAGE_BY_NI_TRACT || !strcmp(FN->Ext, ".tract")) {
      snprintf(cmd,1023*sizeof(char),
               "suma -noniml -tract %s &", FN->NameAsParsed);
      s = system(cmd);
   } else if (!strcmp(FN->Ext, ".dset")) {
      ERROR_message("Not quite ready yet, have to differentiate between gdset\n"
                    "and surface dsets, and for those, we need to know the std\n"
                    " mesh...");
   } else if (!strcmp(FN->Ext, ".asc") ||
              !strcmp(FN->Ext, ".gii") ||
              !strcmp(FN->Ext, ".ply") ) {
      /* You still need to check if gii is a dset, rather than a surface... */
      snprintf(cmd,1023*sizeof(char),
               "suma -noniml -i %s &", FN->NameAsParsed);
      s = system(cmd);    
   } else {
      /* Hail Mary ... */
      snprintf(cmd,1023*sizeof(char),
               "suma -noniml -i %s &", FN->NameAsParsed);
      s = system(cmd);  
   }
   
   return(s); 
}

/*----------------------------------------------------------------------------*/
void afni_open_usage(int detail) 
{
   int i = 0;
   
   ENTRY("afni_open_usage");
   /* print help message in three sections */
   fprintf(stdout,
   "\n"
   "A program to open various AFNI/SUMA files\n"
   "\n"
   "  afni_open [OPTIONS] FILE1 [FILE2 ...]\n"
   "\n"
   "Examples:\n"
   "  afni_open  xmat.1D.xmat\n"
   "  afni_open -aw roi_11.pdf\n"
   "  afni_open -r driv\n"
   "\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
"Options:\n"
"===========\n"
"  -w METHOD: Use METHOD to open FILES.\n"
"             Acceptable values for METHOD are:\n"
"             editor: Open with text editor.\n"
"             downloader: Fetch with wget or curl.\n"
"             browser: Open in browser\n"
"             afni: Open with AFNI\n"
"             suma: Open with SUMA\n"
"             1dplot: Open with 1dplot\n"
"             ExamineXmat: Open with ExamineXmat\n"
"             iviewer: Open with image viewer\n"
"             afniweb: Get from afni website.\n"
"             readme: Search for appropriate README\n"
"                     This option is in the same spirit of \n"
"                     apsearch -view_readme option. To see a list of\n"
"                     all readme files, run:\n"
"                     apsearch -list_all_afni_readmes\n"
"  -e: Same as -w editor\n"
"  -d: Same as -w downloader\n"
"  -x: Same as -w ExamineXmat\n"
"  -b: Same as -w browser\n"
"  -r: Same as -w readme\n"
"  -aw: Same as -w afniweb\n"
"\n"
"     If no method is specifed, the program tries to guess\n"
"     from the filename.\n"
"\n"
"  -global_help: Show help for global options.\n"
"  -gopts_help:  Show help for global options.\n"
"  -help: You're looking at it.\n"
"\n"
"Global Options:\n"
"===============\n"
"%s\n%s", 
   detail > 1 ? SUMA_Offset_SLines(get_help_help(),2):"",
   detail > 1 ? get_gopt_help():""); 
   PRINT_COMPILE_DATE ;
   }
   return;
}

int main(int argc, char **argv)
{
   int iarg, i;
   char *fname=NULL, *uprog=NULL;
   THD_string_array *fnamev = NULL;
   SUMA_PARSED_NAME *FN;
   

   mainENTRY("afni_open main"); machdep() ; 
      
   if (argc <= 1) {
      afni_open_usage(0);
      return(1); 
   }
   
   iarg = 1 ; 
   while( iarg < argc ){
      if (strcmp(argv[iarg],"-global_help") == 0 ||
          strcmp(argv[iarg],"-gopts_help") == 0) { 
         printf(
      "--------------------------------------------------------------------\n"
      "Global Options: options available to most AFNI programs, but usually\n"
      "                not found in the -help output.\n"
      "--------------------------------------------------------------------\n"
             "%s\n%s", SUMA_Offset_SLines(get_help_help(),3), get_gopt_help());
         return(0); 
      }      
      
      if (strcmp(argv[iarg],"-help") == 0 ||
          strcmp(argv[iarg],"-h") == 0) { 
         afni_open_usage(strlen(argv[iarg]) > 3 ? 2:1);
         return(0); 
      }
      
      if (strcmp(argv[iarg],"-e") == 0) { 
         uprog = "editor";
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-b") == 0) { 
         uprog = "browser";
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-d") == 0) { 
         uprog = "downloader"; 
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-x") == 0) { 
         uprog = "ExamineXmat"; 
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-r") == 0) { 
         uprog = "readme"; 
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-aw") == 0) { 
         uprog = "afniweb"; 
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-w") == 0 ||
          strcmp(argv[iarg],"-with") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need tool after -w/-with\n"); return(1);
         }
         uprog = argv[iarg];
         if (strcmp(argv[iarg],"browser") &&
             strcmp(argv[iarg],"editor") &&
             strcmp(argv[iarg],"suma") &&
             strcmp(argv[iarg],"afni") &&
             strcmp(argv[iarg],"downloader") &&
             strcmp(argv[iarg],"ExamineXmat") &&
             strcmp(argv[iarg],"iviewer") &&
             strcmp(argv[iarg],"afniweb") &&
             strcmp(argv[iarg],"readme") &&
             strcmp(argv[iarg],"1dplot") ) {
            ERROR_message("Not ready/bad -w %s", argv[iarg]);
            exit(1);
         }
         ++iarg;
         continue;  
      }
      
      if (strcmp(argv[iarg],"-i") == 0 ||
          strcmp(argv[iarg],"-input") == 0) { 
         ++iarg;
         if (iarg >= argc) {
            fprintf( stderr,
                     "** Error: Need file after -i/-input\n"); return(1);
         }
         while (iarg < argc && argv[iarg][0] != '-') {
            if ( ! fnamev ) INIT_SARR(fnamev);
            ADDUTO_SARR(fnamev, argv[iarg]); 
            ++iarg;
         }
         continue; 
      }
      
      if (iarg < argc && argv[iarg][0] == '-'){ /* bad news in tennis shoes */
         fprintf(stderr,"** Error %s: bad option %s\n", argv[0], argv[iarg]);
         suggest_best_prog_option(argv[0], argv[iarg]);
         return 1;
      } else {
         break;
      }
   }
   
   /* All the rest are files? */
   while (iarg < argc && argv[iarg][0] != '-') {
      if ( ! fnamev ) INIT_SARR(fnamev);
      ADDUTO_SARR(fnamev, argv[iarg]); 
      ++iarg;
   }
   
   if (!fnamev) {
      ERROR_message("Nothing to do");
      return 1;
   }

   for (i=0; i<fnamev->num; ++i) {
      fname = fnamev->ar[i];
      if (!(FN = SUMA_ParseFname (fname, NULL))) {
         ERROR_message("Failed to parse %s, cwd %s\n", fname, NULL);
         exit(1);
      }
      if (uprog) {
                if (!strcmp(uprog,"browser")) {
            ao_with_browser(FN->NameAsParsed);
         } else if (!strcmp(uprog,"editor")) {
            ao_with_editor(FN->NameAsParsed);
         } else if (!strcmp(uprog,"afni")) {
            ao_with_afni(FN->NameAsParsed);
         } else if (!strcmp(uprog,"suma")) {
            ao_with_suma_pn(FN);
         } else if (!strcmp(uprog,"1dplot")) {
            ao_with_1dplot(FN->NameAsParsed);
         } else if (!strcmp(uprog,"ExamineXmat")) {
            ao_with_ExamineXmat(FN->NameAsParsed);
         } else if (!strcmp(uprog,"downloader")) {
            ao_with_downloader(FN->NameAsParsed, 0);
         } else if (!strcmp(uprog,"iviewer")) {
            ao_with_image_viewer(FN->NameAsParsed);
         } else if (!strcmp(uprog,"afniweb")) {
            ao_with_afniweb(FN->NameAsParsed);
         } else if (!strcmp(uprog,"readme")) {
            ao_with_readme(FN->NameAsParsed);
         } else {
            ERROR_message("Not ready for prog. %s", uprog);
            exit(1);
         } 
         continue;
      }
      /* Now comes the decision stream 
         There is no one function fits all here, we will need to think about
         how we go about deciding what is what. Two field types are of 
         most import:
         FN->StorageMode (or FN->StorageModeNm)
         FN->TypeExt
         Also, what should be done might be dictated by the size of the file at
         hand, and wheter or not it is in binary versus ascii mode. Those fields
         are not a part of SUMA_PARSED_NAME, but we can easily add them.
         We still need things like: 
         is_url, is_pdf, is_script, is_..., is_surface_dset, 
         is_surface, is_xmat, is_prog, is_archive, is_web_archive
      */
      if (is_url_pn(FN)) {
         if (!is_archive_pn(FN)) {
            ao_with_browser(FN->NameAsParsed);
         } else {
            ao_with_downloader(FN->NameAsParsed, 0);
         }
      } else if (is_local_html(FN)) {
         ao_with_browser(FN->NameAsParsed);
      } else if (FN->StorageMode == STORAGE_BY_1D) {
         ao_with_editor(FN->NameAsParsed);
      } else if (is_pdf_pn(FN)) {
         ao_with_pdf_viewer(FN->NameAsParsed);
      } else if (is_image_pn(FN)) {
         ao_with_image_viewer(FN->NameAsParsed);
      } else if (FN->StorageMode == STORAGE_BY_BRICK ||
                 FN->StorageMode == STORAGE_BY_MINC ||
                 FN->StorageMode == STORAGE_BY_VOLUMES ||
                 FN->StorageMode == STORAGE_BY_ANALYZE ||
                 FN->StorageMode == STORAGE_BY_CTFMRI ||
                 FN->StorageMode == STORAGE_BY_CTFSAM ||
                 FN->StorageMode == STORAGE_BY_3D ||
                 FN->StorageMode == STORAGE_BY_NIFTI) {
         ao_with_afni(FN->NameAsParsed);
      } else if (FN->StorageMode == STORAGE_BY_NI_TRACT ||
                 FN->StorageMode == STORAGE_BY_GIFTI) {
         ao_with_suma_pn(FN);
      } else if (is_xmat_pn(FN)) {
         ao_with_ExamineXmat(FN->NameAsParsed);
      } else {
         ERROR_message("Not sure what %s is", FN->NameAsParsed);
      }
                 
      if (FN) SUMA_Free_Parsed_Name (FN);
   }
   
   if (fnamev) DESTROY_SARR(fnamev); fnamev=NULL;
   return 0;  
}
