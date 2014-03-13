#define MAIN
#define SUMA_noFunc

#include "mrilib.h"
#include "afni.h"
#include <stdio.h>
#include <stdlib.h>
#include "matrix.h"
#include "suma_suma.h"


         
void afni_open_usage(int detail) 
{
   int i = 0;
   
   ENTRY("afni_open_usage");
   /* print help message in three sections */
   fprintf(stdout,
   "\n"
   "A program to open various AFNI/SUMA files\n"
   "\n"
   "  afni_open FILE\n"
   "\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
"Parameters:\n"
"===========\n"
"  -e: Open for editing\n"
"  -v: Open for viewing\n"
"  -d: Download if possible\n"
"\n"
"Global Options:\n"
"===============\n"
"%s", 
   detail > 1 ? get_gopt_help():""); 
   PRINT_COMPILE_DATE ;
   }
   return;
}

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
   if (!viewer && !(viewer=GetAfniTextEditor())) {
      ERROR_message("No text editor");
      return(0);
   }
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", viewer, fname);
   system(cmd);
   return(1);
}

int ao_with_downloader(char *fname)
{
   char cmd[1024];
   static char *downloader=NULL;
   if (!downloader && !(downloader=GetAfniWebDownloader())) {
      ERROR_message("No text editor");
      return(0);
   }
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", downloader, fname);
   system(cmd);
   return(1);
}

int ao_with_pdf_viewer(char *fname)
{
   char cmd[1024];
   static char *pdfviewer=NULL;
   if (!pdfviewer && !(pdfviewer=GetAfniPDFViewer())) {
      ERROR_message("No pdf viewer");
      return(0);
   }
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", pdfviewer, fname);
   system(cmd);
   return(1);
}
                           
int ao_with_image_viewer(char *fname)
{
   char cmd[1024];
   static char *imageviewer=NULL;
   if (!imageviewer && !(imageviewer=GetAfniImageViewer())) {
      ERROR_message("No image viewer");
      return(0);
   }
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"%s %s &", imageviewer, fname);
   system(cmd);
   return(1);
}
                           
int ao_with_afni(char *fname)
{
   char cmd[1024];
   
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"afni %s &", fname);
   system(cmd);
   return(1);
}

int ao_with_1dplot(char *fname)
{
   char cmd[1024];
   
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"1dplot %s &", fname);
   system(cmd);
   return(1);
}

int ao_with_ExamineXmat(char *fname)
{
   char cmd[1024];
   
   if (!fname) return(0);
   
   snprintf(cmd,1023*sizeof(char),"ExamineXmat -input %s &", fname);
   system(cmd);
   return(1);
}

int ao_with_suma(char *name)
{
   char cmd[1024];
   int a;
   SUMA_PARSED_NAME *pn;
   
   if (!name) return(0);
   if (!(pn = SUMA_ParseFname (name,NULL))) {
      return(0);
   }
   
   a = ao_with_suma_pn(pn);
   SUMA_Free_Parsed_Name (pn);
   
   return(a);
}


int ao_with_suma_pn(SUMA_PARSED_NAME *FN) 
{
   int a = 0;
   char cmd[1024];
   if (!FN) return(0);
   if (!FN->OnDisk) return(0); /* should be on disk */
   
   if (FN->StorageMode == STORAGE_BY_NI_TRACT || !strcmp(FN->Ext, ".tract")) {
      snprintf(cmd,1023*sizeof(char),
               "suma -noniml -tract %s &", FN->NameAsParsed);
      system(cmd);
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
      system(cmd);    
   } else {
      /* Hail Mary ... */
      snprintf(cmd,1023*sizeof(char),
               "suma -noniml -i %s &", FN->NameAsParsed);
      system(cmd);  
   }
   
   return(1); 
}
/*----------------------------------------------------------------------------*/
int main(int argc, char **argv)
{
   int iarg, mode, i;
   char *fname=NULL, *uprog=NULL;
   THD_string_array *fnamev = NULL;
   SUMA_PARSED_NAME *FN;
   

   mainENTRY("afni_open main"); machdep() ; 
      
   mode=0;
   if (argc <= 1) {
      afni_open_usage(0);
      return(1); 
   }
   
   iarg = 1 ; 
   while( iarg < argc ){
      if (strcmp(argv[iarg],"-e") == 0) { 
         mode = 1; 
         ++iarg;
         continue; 
      }
      
      if (strcmp(argv[iarg],"-v") == 0) { 
         mode = 2; 
         ++iarg;
         continue; 
      }

      if (strcmp(argv[iarg],"-d") == 0) { 
         mode = 3; 
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
             strcmp(argv[iarg],"ExamineXmat") &&
             strcmp(argv[iarg],"1dplt") ) {
            ERROR_message("Not ready for %s", argv[iarg]);
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
            whereami_browser(FN->NameAsParsed);
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
         } else {
            ERROR_message("Not ready for %s", uprog);
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
            whereami_browser(FN->NameAsParsed);
         } else {
            ao_with_downloader(FN->NameAsParsed);
         }
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
