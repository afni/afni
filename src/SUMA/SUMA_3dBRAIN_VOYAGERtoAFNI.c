#include "SUMA_suma.h"

void usage_3dBRAIN_VOYAGERtoAFNI (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_3dBRAIN_VOYAGERtoAFNI"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: 3dBRAIN_VOYAGERtoAFNI <-input BV_VOLUME.vmr> \n"
               "                             [-bs] [-qx] [-tlrc|-acpc|-orig] [<-prefix PREFIX>]\n"
               " \n"
               " Converts a BrainVoyager vmr dataset to AFNI's BRIK format\n"
               " The conversion is based on information from BrainVoyager's\n"
               " website: www.brainvoyager.com. \n"
               " Sample data and information provided by \n"
               "  Adam Greenberg and Nikolaus Kriegeskorte.\n"
               "\n"
               "  If you get error messages about the number of\n"
               " voxels and file size, try the options below.\n"
               " I hope to automate these options once I have\n"
               " a better description of the BrainVoyager QX format.\n"
               "\n"
               "  Optional Parameters:\n"
               "  -bs: Force byte swapping.\n"
               "  -qx: .vmr file is from BrainVoyager QX\n"
               "  -tlrc: dset in tlrc space\n"
               "  -acpc: dset in acpc-aligned space\n"
               "  -orig: dset in orig space\n"
               "  If unspecified, the program attempts to guess the view from\n"
               "  the name of the input.\n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; 
      SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *
   SUMA_3dBRAIN_VOYAGERtoAFNI_ParseInput(char *argv[], int argc, 
                                          SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_3dBRAIN_VOYAGERtoAFNI_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->b1 = 0;
   Opt->b2 = 0;
   Opt->Icold = -1;
   Opt->out_prefix = NULL;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_3dBRAIN_VOYAGERtoAFNI(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input\n");
				exit (1);
			}
         Opt->in_name = argv[kar];
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix\n");
				exit (1);
			}
         Opt->out_prefix = SUMA_copy_string(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-tlrc") == 0)) {
         
         Opt->Icold = VIEW_TALAIRACH_TYPE;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-acpc") == 0)) {
         
         Opt->Icold = VIEW_ACPCALIGNED_TYPE;
			brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-orig") == 0)) {
         
         Opt->Icold = VIEW_ORIGINAL_TYPE;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-bs") == 0)) {
         
         Opt->b1 = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-qx") == 0)) {
         
         Opt->b2 = 1;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need integer argument after -debug\n");
				exit (1);
			}
         Opt->debug = atoi(argv[kar]);
			brk = YUP;
		}
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}


char * SUMA_BrainVoyager_Read_vmr(char *fnameorig, THD_3dim_dataset *dset, int LoadData, 
                                  byte Qxforce, byte bsforce, int viewforce, char *outname)
{
   static char FuncName[]={"SUMA_BrainVoyager_Read_vmr"};
   int  i = 0, nf, iop, dchunk, End, bs, doff, ex,
         data_type=SUMA_notypeset, view, endian, dblock;
   THD_ivec3 iv3;
   unsigned long len;
   short qxver;
   short nvox[3]; /* values are unsigned, so check for sign for bs... */
   THD_mat33 m33;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   float ep[3], sp[3];
   char sview[10], *fname = NULL, *scom=NULL, form[10], swp[10], orstr[10], xfov[100], yfov[100], zfov[100], *prefix = NULL, *dsetheadname = NULL;
   FILE *fid = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !fnameorig) {
      SUMA_SL_Err("NULL fname || NULL dset!");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_isExtension(fnameorig, ".vmr")) {
      SUMA_SL_Err("vmr dset is expected to have .vmr for an extension");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_filexists(fnameorig)) {
      SUMA_SL_Err("file does not exist");
      SUMA_RETURN(NOPE);
   }
   
   /* make fname be the new name without the extension*/
   if (!outname) {
      fname = SUMA_Extension(fnameorig,".vmr", YUP);
   } else {
      fname = SUMA_Extension(outname, ".vmr", YUP);
   }
   prefix = SUMA_AfniPrefix(fname, NULL, NULL, NULL);
   if( !THD_filename_ok(prefix) ) {
      SUMA_SL_Err("Bad prefix");
      goto CLEAN_EXIT;
   }
   
   /* someday, guess format on your own...*/
   qxver = 0;
   if (Qxforce) {
      SUMA_LH("Forcing qxver");
      qxver = 1;  /* set to 1 if qx format */
   } 
   
   /* view ? */
   SUMA_LHv("Viewforce = %d; [%d %d]\n", viewforce, VIEW_ORIGINAL_TYPE, VIEW_TALAIRACH_TYPE);
   if (viewforce >=  VIEW_ORIGINAL_TYPE && viewforce <= VIEW_TALAIRACH_TYPE) {
      view = viewforce;
   } else {
      view = VIEW_ORIGINAL_TYPE;
      if (SUMA_iswordin_ci(fnameorig, "_tal") == 1) { view = VIEW_TALAIRACH_TYPE; }
      else if (SUMA_iswordin_ci(fnameorig, "_acpc") == 1) { view = VIEW_ACPCALIGNED_TYPE; } 
      else { view = VIEW_ORIGINAL_TYPE;  } 
   }
   switch (view) {
      case VIEW_ORIGINAL_TYPE:
         sprintf(sview,"+orig"); break;
      case VIEW_ACPCALIGNED_TYPE:
         sprintf(sview,"+acpc"); break;
      case VIEW_TALAIRACH_TYPE: 
         sprintf(sview,"+tlrc"); break;
      default:
         SUMA_SL_Err("Bad view");
         goto CLEAN_EXIT; 
   }
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: View %s, %d\n", FuncName, sview, view);    
   
   dsetheadname = SUMA_append_replace_string(prefix,".HEAD", sview, 0);
   if (SUMA_filexists(dsetheadname)) {
      SUMA_S_Errv("Bad prefix, output dset %s exists\n", dsetheadname);
      goto CLEAN_EXIT;
   }
   SUMA_free(dsetheadname); dsetheadname = NULL;
   
   fid = fopen(fnameorig,"r"); 
   if (!fid) {
      SUMA_SL_Err("Could not open file for reading");
      goto CLEAN_EXIT;      
   }
   
   if (qxver) { 
      SUMA_LH("Reading dimensions, 2+ 1st 6 bytes and deciding on swap");
      doff = 4*sizeof(short);/* data offset 2 + 6 bytes */
   } else {
      SUMA_LH("Reading dimensions, 1st 6 bytes and deciding on swap");
      doff = 3*sizeof(short);/* data offset */
   }
   SUMA_WHAT_ENDIAN(endian);
   bs = 0;
   swp[0] = '\0';
   data_type = SUMA_byte; /* voxel data is bytes */
   dchunk = sizeof(byte); /* voxel data is bytes */
   sprintf(form,"3Db");
   if (qxver) { SUMA_READ_NUM(&(qxver), fid, ex, sizeof(short)); if (LocalHead) fprintf(SUMA_STDERR,"%s: QXver = %d\n", FuncName, qxver);}/* is this a QX format?*/
   SUMA_READ_NUM(&(nvox[2]), fid, ex, sizeof(short)); /* Z first */
   SUMA_READ_NUM(&(nvox[1]), fid, ex, sizeof(short));
   SUMA_READ_NUM(&(nvox[0]), fid, ex, sizeof(short));
   
   if ((nvox[0] < 0 || nvox[1] < 0 || nvox[2] < 0 )) {
      SUMA_LH("Byte swapping needed");
      bs = 1;
   }
   if (bsforce) {
      SUMA_LH("Byte swapping forced");
      bs = 1;
   }
   
   
   if (bs) {
      if (qxver) SUMA_swap_2(&(qxver));
      SUMA_swap_2(&(nvox[0]));
      SUMA_swap_2(&(nvox[1]));
      SUMA_swap_2(&(nvox[2]));
      sprintf(swp,"-2swap");
      SUMA_OTHER_ENDIAN(endian);
   }
   
   /* 1 1 1???? */
   if (nvox[0] == nvox[1]  && nvox[1] == nvox[2] && nvox[2] == 1) {
      if (LocalHead) { fprintf(SUMA_STDERR,"Warning %s: Voxel nums all 1. Trying from file size\n", FuncName); }
      len = THD_filesize( fnameorig ) ;
      len -= doff;
      len /= dchunk;
      nvox[0] = (int)pow((double)len, 1.0/3.0); 
      if (nvox[0] * nvox[0] * nvox[0] != len) {
         fprintf(SUMA_STDERR,"Error %s: Bad voxel numbers and could not infer number from filesize.\n"
                             "Size of file: %lld, data offset: %d, datum size: %d, data number: %ld\n"
                             "Inferred nvox:%d\n",
                             FuncName,
                             THD_filesize( fnameorig ), doff, dchunk, len, 
                             nvox[0]);
         goto CLEAN_EXIT;
      }
      nvox[2] = nvox[1] = nvox[0];
      if (LocalHead) { fprintf(SUMA_STDERR,"Warning %s: Using filesize inferred number of voxels: %d %d %d\n", 
                              FuncName, nvox[0], nvox[1], nvox[2]); }
   }
   
   if (LocalHead) fprintf(SUMA_STDERR,"Number of voxels: %d %d %d. qxver %d\n", nvox[0], nvox[1], nvox[2], qxver);
   

   /* check against filesize */
   
   len = THD_filesize( fnameorig ) ;
   dblock = nvox[0]*nvox[1]*nvox[2]*dchunk;
   if (len != (dblock + doff)) {
      if (!qxver) {
         fprintf(SUMA_STDERR, "Mismatch between file size    %ld\n"
                              "and expected size             %d = (%d*%d*%d*%d+%d) \n",
                              len, (dblock + doff), nvox[0], nvox[1], nvox[2], dchunk, doff);
         goto CLEAN_EXIT; 
      }else if (len < dblock + doff) {
         fprintf(SUMA_STDERR, "Mismatch between file size    %ld\n"
                              "and minimum expected size     %d = (%d*%d*%d*%d+%d) \n",
                              len, (dblock + doff), nvox[0], nvox[1], nvox[2], dchunk, doff);
         goto CLEAN_EXIT; 
      }else {
         SUMA_LH("Proceeding");
      }
   }
   if (LocalHead) fprintf(SUMA_STDERR,"File size passes test\n");

   /* orientation */
   
   orixyz.ijk[0] = ORI_A2P_TYPE;
   orixyz.ijk[1] = ORI_S2I_TYPE;
   orixyz.ijk[2] = ORI_R2L_TYPE;       
   
   /* load number of voxels */
   LOAD_IVEC3( nxyz   , nvox[0]    , nvox[1]    , nvox[2] ) ;
  
   /* form the command */
   SUMA_LH("Forming command");
   
   {  
      float delta[3]={1.0, 1.0, 1.0};
      float origin[3]={0.0, 0.0, 0.0}; 
      /* set origin of 0th voxel*/
      origin[0] = (float)((nvox[0]*delta[0]))/2.0;    /* ZSS: Changed from 0 0 0 Nov 1 07 */
      origin[1] = (float)((nvox[1]*delta[1]))/2.0;
      origin[2] = (float)((nvox[2]*delta[2]))/2.0;
       
      /* dimensions, same for vmr*/
      LOAD_FVEC3( dxyz , delta[0], delta[1], delta[2]   ) ;
      SUMA_sizeto3d_2_deltaHEAD(orixyz, &dxyz);
      /* origin */
      LOAD_FVEC3( orgxyz , origin[0], origin[1], origin[2]   ) ;
      SUMA_originto3d_2_originHEAD(orixyz, &orgxyz);
      
   }
    
   /* start point (edge of origin voxel) and end point (opposite to start ) */
   sp[0] = orgxyz.xyz[0] + SUMA_ABS(dxyz.xyz[0]) / 2.0;
   sp[1] = orgxyz.xyz[1] + SUMA_ABS(dxyz.xyz[1]) / 2.0;
   sp[2] = orgxyz.xyz[2] + SUMA_ABS(dxyz.xyz[2]) / 2.0;
   ep[0] = orgxyz.xyz[0] + (nxyz.ijk[0] - 0.5) * SUMA_ABS(dxyz.xyz[0]);
   ep[1] = orgxyz.xyz[1] + (nxyz.ijk[1] - 0.5) * SUMA_ABS(dxyz.xyz[1]);
   ep[2] = orgxyz.xyz[2] + (nxyz.ijk[2] - 0.5) * SUMA_ABS(dxyz.xyz[2]);
   SUMA_orcode_to_orstring (orixyz.ijk[0], orixyz.ijk[1], orixyz.ijk[2], orstr);
   sprintf(xfov," -xFOV %.2f%c-%.2f%c", sp[0], orstr[0], ep[0], orstr[3]);
   sprintf(yfov," -yFOV %.2f%c-%.2f%c", sp[1], orstr[1], ep[1], orstr[4]);
   sprintf(zfov," -zFOV %.2f%c-%.2f%c", sp[2], orstr[2], ep[2], orstr[5]);
  
   scom = (char *)SUMA_calloc((strlen(fnameorig)+500), sizeof(char));
   sprintf(scom,"to3d %s %s %s %s -prefix %s %s:%d:0:%d:%d:%d:%s ", 
            swp, xfov, yfov, zfov, prefix, form, doff, 
            nvox[0], nvox[1], nvox[0], fnameorig);
   
   SUMA_LH("HERE");
   
   if (dset) { /* form the dset header */
         int nvals_read = 0;
         SUMA_LH("Filling header");
         EDIT_dset_items( dset ,
                            ADN_prefix      , prefix ,
                            ADN_datum_all   , data_type ,
                            ADN_nxyz        , nxyz ,
                            ADN_xyzdel      , dxyz ,
                            ADN_xyzorg      , orgxyz ,
                            ADN_xyzorient   , orixyz ,
                            ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                            ADN_view_type   , view ,
                            ADN_type        , HEAD_ANAT_TYPE ,
                            ADN_func_type   , ANAT_BUCK_TYPE ,
                          ADN_none ) ;

         if (LoadData) {
            void *vec=NULL;
            SUMA_LH("Loading data");
            if (!(vec = SUMA_BinarySuck(fnameorig, data_type, endian, 3*sizeof(short), -1, &nvals_read))) {
               SUMA_SL_Err("Failed to read data file"); goto CLEAN_EXIT;
            }
            if (qxver) {
               if (nvals_read < nvox[0]*nvox[1]*nvox[2]) {
                  SUMA_SL_Warn("Failed to read expected number of voxels\n proceeding...");
               }
            } else {
               if (nvals_read != nvox[0]*nvox[1]*nvox[2]) {
                  SUMA_SL_Warn("Failed to read the appropriate number of voxels\n proceeding...");
               }
            }  
            EDIT_substitute_brick( dset , 0 , data_type , vec) ;      
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Read %d values from file.\n", FuncName, nvals_read);
            /* DSET_write(dset) ; */
         }
   }
   

   
   CLEAN_EXIT:
   if (prefix) SUMA_free(prefix); prefix = NULL;
   if (fname) SUMA_free(fname); fname = NULL;
   if (fid) fclose(fid); fid = NULL;
   SUMA_RETURN(scom);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"3dBRAIN_VOYAGERtoAFNI"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_OPEN_DX_STRUCT **dx = NULL;
   THD_3dim_dataset *dset=NULL;
   char *sto3d = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "");
   
   if (argc < 2) {
      usage_3dBRAIN_VOYAGERtoAFNI(ps);
      exit (1);
   }
   
   Opt = SUMA_3dBRAIN_VOYAGERtoAFNI_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   dset = EDIT_empty_copy( NULL ) ;
   tross_Make_History( "3dBRAIN_VOYAGERtoAFNI" , argc,argv , dset) ;
   if (!(sto3d = SUMA_BrainVoyager_Read_vmr(Opt->in_name, dset, 1, Opt->b2, Opt->b1, Opt->Icold, Opt->out_prefix))) {
      if (Opt->debug) SUMA_SL_Err("Failed in SUMA_BrainVoyager_Read_vmr");
      exit(1);   
   }
   SUMA_LHv("Old command would be %s\n", sto3d);
   
   if (dset) {
      SUMA_LH("Writing Dset");
      DSET_write(dset) ;
      if (LocalHead) { 
         fprintf(SUMA_STDERR,"%s: Can use the following command to create dset with to3d:\n%s\n", FuncName,sto3d); 
      } 
   } else {
      /* the olde way */
      if (system(sto3d)) {
         fprintf(SUMA_STDERR, "Error %s: Failed while executing shell command:\n%s\n"
                              "Check to3d's error messages, and disk writing permissions.\n", FuncName, sto3d);
      }
   }
   
   
      
   if (sto3d) SUMA_free(sto3d); sto3d = NULL;
   if (dset) { DSET_delete(dset); dset = NULL; }
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
