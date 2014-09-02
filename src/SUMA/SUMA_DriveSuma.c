/*USE This sample to start writing standalone programs.
Change DriveSuma to the program name of your choosing.
*/
#include "SUMA_suma.h"

/* NICE THINGS TO ADD 
   + support -surf_group and -switch_group
   + support view_surf (for hiding say L/R hemis)
   + DONE: create syntax for series of repeated key strokes with delay between strokes and perhaps a forced redisplay
   + DONE: make recorder save pictures
   + add passing of DOs as is done in Julia's program
   + DONE: support for quit action
   + DONE: support control of intensity range
*/



  

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"DriveSuma"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   float ctr[3] = { 0.0, 0.0, 0.0};
   int cnt=0, i=0, exflag=0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;");
   /* force talk option whether users specify it or not */
   ps->cs->talk_suma = 1;
   if (ps->cs->rps > 0) { ps->cs->nelps = (float)ps->cs->talk_suma * ps->cs->rps; }
   else { ps->cs->nelps = (float) ps->cs->talk_suma * -1.0; }

   
   if (!(Opt = SUMA_DriveSuma_ParseInput (argv, argc, ps))) {
      exit(1);
   }
   if (argc < 1) {
      SUMA_S_Err("No options, use -h or -help for usage");
      exit (1);
   }

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* open communication */
   SUMA_LH("Talking to suma");
   ps->cs->istream = SUMA_DRIVESUMA_LINE;
   ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2; /* not used yet */
   ps->cs->kth = 1; /* make sure all surfaces get sent */
   if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
      SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
      ps->cs->Send = NOPE;
      ps->cs->afni_Send = NOPE;
      ps->cs->talk_suma = NOPE;
   } 

   
   if (Opt->b1) { /* sample code for Ben Singer */
      /* create a surface of sorts, set up a few attributes */
      SO = SUMA_CreateIcosahedron (50.0, 12, ctr, "n", 1);
      if (!SO) { SUMA_S_Err("Failed to create Icosahedron"); exit(1); }
      if (!SO->State) {SO->State = SUMA_copy_string("DC"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("DS"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string("IcoSurf"); }
      if (SO->Label) { 
         if (SO->idcode_str) SUMA_free(SO->idcode_str); 
         SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); 
      }
      SO->normdir = 1;
      if (ps->cs->talk_suma) {   /* strcutre setup during program default options parsing */
            SUMA_LH("Sending Ico"); /* send the surface */
            SUMA_SendSumaNewSurface(SO, ps->cs);
      }
   
      SUMA_LH("An example for modifying mesh and redisplaying");
      cnt = 0;
      while (cnt < 20) {
         /* Do some mesh action */
         for (i=0; i<SO->N_Node*SO->NodeDim; ++i) SO->NodeList[i] *= 0.9;
            /* recalculate surface normals */
            SUMA_RECOMPUTE_NORMALS(SO); 
            if (ps->cs->Send) {
               if (!SUMA_SendToSuma (SO, ps->cs, 
                                     (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\n"
                               "Communication halted.");
               }
            }
         ++cnt;
      }
   } else {
      /* interpret command line commands */
      for (i=0; i<Opt->N_com; ++i) {
         if (LocalHead) {
            SUMA_LH("Have the following commands");
            fprintf(SUMA_STDERR,"Command %d: %s\n", i, Opt->com[i]);
         }
         if (!(exflag = SUMA_ProcessCommand(Opt->com[i], ps->cs, Opt->s))) {
            SUMA_S_Errv("Failed in processing command\n%s\n", Opt->com[i]); 
            exit(1);
         }   
         if (exflag == -1) { /*gone daddy gone */ 
            SUMA_S_Note("There's no more reason to exist.\n"
                        "Farewell dear friends.\n");
            exit(0);
         }
      }
   }
   
   SUMA_LH("Freedom");
   /* you don't want to exit rapidly because the SUMA might 
       not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
