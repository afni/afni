#include "mrilib.h"
#include "thd.h"

/*******************************************************************/
/* Stuff to read CTF MRI and SAM datasets from the NIH MEG system. */
/* 04 Dec 2002: first cut by RWCox.                                */
/*******************************************************************/

enum { coronal=0, sagittal, axial };

#define LEFT_ON_LEFT      0
#define LEFT_ON_RIGHT     1

enum { Modality_MRI=0, Modality_CT, Modality_PET, Modality_SPECT, Modality_OTHER };

#define VERSION_1_STR   "CTF_MRI_FORMAT VER 1.0"
#define VERSION_2_STR   "CTF_MRI_FORMAT VER 2.0"
#define VERSION_21_STR  "CTF_MRI_FORMAT VER 2.1"
#define VERSION_22_STR  "CTF_MRI_FORMAT VER 2.2"

/****************************************************************************/
/*********** additional header structs for  CTF MRI version 2.0 file        */
/****************************************************************************/

typedef struct HeadModel_Info {
     short               Nasion_Sag;          /* fiduciary point voxel locations */
     short               Nasion_Cor;          /* Sag = sagittal direction */
     short               Nasion_Axi;          /* Cor = coronal direction */
     short               LeftEar_Sag;         /* Axi = axial direction */
     short               LeftEar_Cor;
     short               LeftEar_Axi;
     short               RightEar_Sag;
     short               RightEar_Cor;
     short               RightEar_Axi;
     float               defaultSphereX;      /* default sphere parameters in mm */
     float               defaultSphereY;      /* (in head based coordinate system */
     float               defaultSphereZ;
     float               defaultSphereRadius;
} HeadModel_Info;

typedef struct Image_Info {                   /* scan and/or sequence parameters */
     short              modality;             /* 0=MRI, 1=CT, 2=PET, 3=SPECT, 4=OTHER */
     char               manufacturerName[64];
     char               instituteName[64];
     char               patientID[32];
     char               dateAndTime[32];
     char               scanType[32];
     char               contrastAgent[32];
     char               imagedNucleus[32];
     float              Frequency;
     float              FieldStrength;
     float              EchoTime;
     float              RepetitionTime;
     float              InversionTime;
     float              FlipAngle;
     short              NoExcitations;
     short              NoAcquisitions;
     char               commentString[256];
     char               forFutureUse[64];
} Image_Info;

typedef struct Version_2_Header {
     char               identifierString[32];   /* "CTF_MRI_FORMAT VER 2.x"            */
     short              imageSize;              /* always = 256                        */
     short              dataSize;               /* 1 = 8 bit data, 2 = 16 bit data     */
     short              clippingRange;          /* max. integer value in data          */
     short              imageOrientation;       /* 0 = left on left, 1 = left on right */
     float              mmPerPixel_sagittal;    /* voxel dimensions in mm              */
     float              mmPerPixel_coronal;     /* voxel dimensions in mm              */
     float              mmPerPixel_axial;       /* voxel dimensions in mm              */
     HeadModel_Info     headModel;              /* structure defined above (34 bytes)  */
     Image_Info         imageInfo;              /* structure defined above (638 bytes) */
     float              headOrigin_sagittal;    /* voxel location of head origin       */
     float              headOrigin_coronal;     /* voxel location of head origin       */
     float              headOrigin_axial;       /* voxel location of head origin       */

                                                /* Euler angles to align MR to head          */
                                                /* coordinate system (angles in degrees!)    */
     float              rotate_coronal;         /* 1. rotate in coronal plane by this angle  */
     float              rotate_sagittal;        /* 2. rotate in sagittal plane by this angle */
     float              rotate_axial;           /* 3. rotate in axial plane by this angle    */

     short              orthogonalFlag;         /* 1 if image is orthogonalized to head frame */
     short              interpolatedFlag;       /* 1 if slices interpolated during conversion */
     float              originalSliceThickness;
     float              transformMatrix[4][4];  /* 4x4 transformation matrix (head to mri) */
     unsigned char      unused[202];            /* pad header to 1028 bytes                */
} Version_2_Header;

/*---------------------------------------------------------------*/
/*! Swap the 4 bytes pointed to by ppp: abcd -> dcba. */

static void swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

/*---------------------------------------------------------------*/
/*! Swap the 8 bytes pointed to by ppp: abcdefgh -> hgfedcba. */

static void swap_8(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;
   unsigned char b4, b5, b6, b7;

   b0 = *pntr    ; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   b4 = *(pntr+4); b5 = *(pntr+5); b6 = *(pntr+6); b7 = *(pntr+7);

   *pntr     = b7; *(pntr+1) = b6; *(pntr+2) = b5; *(pntr+3) = b4;
   *(pntr+4) = b3; *(pntr+5) = b2; *(pntr+6) = b1; *(pntr+7) = b0;
}

/*---------------------------------------------------------------*/
/*! Swap the 2 bytes pointed to by ppp: ab -> ba. */

static void swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/*-------------------------*/
/*! Macro for bad return. */

#define BADBAD(s)                                                \
  do{ fprintf(stderr,"** THD_open_ctfmri(%s): %s\n",fname,s);    \
      RETURN(NULL);                                              \
  } while(0)

/*-----------------------------------------------------------------*/
/*! Open a CTF .mri file as an unpopulated AFNI dataset.
    It will be populated later, in THD_load_ctfmri().
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_ctfmri( char *fname )
{
   FILE *fp ;
   Version_2_Header hh ;
   int ii,nn , swap ;
   THD_3dim_dataset *dset=NULL ;
   char prefix[THD_MAX_PREFIX] , *ppp , tname[12] , ori[4] ;
   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   int iview ;
   int ngood , length , datum_type , datum_len , oxx,oyy,ozz ;
   int   nx,ny,nz ;
   float dx,dy,dz , xorg,yorg,zorg ;

ENTRY("THD_open_ctfmri") ;

   /* open input file */

   if( fname == NULL || *fname == '\0' ) BADBAD("bad input filename");
   fp = fopen( fname , "rb" ) ;
   if( fp == NULL )                      BADBAD("can't open input file");

   /* read 1028 byte header */

   nn = fread( &hh , sizeof(hh) , 1 , fp ) ;
   fclose(fp) ;
   if( nn <= 0 )                         BADBAD("can't read input file");

   /* check if header string matches what we want */

   hh.identifierString[31] = '\0' ;  /* make sure is terminated */
   if( strcmp(hh.identifierString,VERSION_22_STR) != 0 ) BADBAD("bad version string");
   if( hh.imageSize == 0 )                               BADBAD("bad imageSize") ;

   /* determine if must swap header */

   swap = (hh.imageSize != 256) ;

   if( swap ){                          /* swap bytes in various header fields */
     swap_2(&hh.imageSize              ) ;
     swap_2(&hh.dataSize               ) ;
     swap_2(&hh.clippingRange          ) ;
     swap_2(&hh.imageOrientation       ) ;
     swap_4(&hh.mmPerPixel_sagittal    ) ;
     swap_4(&hh.mmPerPixel_coronal     ) ;
     swap_4(&hh.mmPerPixel_axial       ) ;
     swap_4(&hh.headOrigin_sagittal    ) ;
     swap_4(&hh.headOrigin_coronal     ) ;
     swap_4(&hh.headOrigin_axial       ) ;
     swap_4(&hh.rotate_coronal         ) ;
     swap_4(&hh.rotate_sagittal        ) ;
     swap_4(&hh.rotate_axial           ) ;
     swap_2(&hh.orthogonalFlag         ) ;
     swap_2(&hh.interpolatedFlag       ) ;
     swap_4(&hh.originalSliceThickness ) ;
     swap_2(&hh.headModel.Nasion_Sag   ) ;
     swap_2(&hh.headModel.Nasion_Cor   ) ;
     swap_2(&hh.headModel.Nasion_Axi   ) ;
     swap_2(&hh.headModel.LeftEar_Sag  ) ;
     swap_2(&hh.headModel.LeftEar_Cor  ) ;
     swap_2(&hh.headModel.LeftEar_Axi  ) ;
     swap_2(&hh.headModel.RightEar_Sag ) ;
     swap_2(&hh.headModel.RightEar_Cor ) ;
     swap_2(&hh.headModel.RightEar_Axi ) ;

     swap_4(&hh.transformMatrix[0][0]  ) ;
     swap_4(&hh.transformMatrix[0][1]  ) ;
     swap_4(&hh.transformMatrix[0][2]  ) ;
     swap_4(&hh.transformMatrix[0][3]  ) ;
     swap_4(&hh.transformMatrix[1][0]  ) ;
     swap_4(&hh.transformMatrix[1][1]  ) ;
     swap_4(&hh.transformMatrix[1][2]  ) ;
     swap_4(&hh.transformMatrix[1][3]  ) ;
     swap_4(&hh.transformMatrix[2][0]  ) ;
     swap_4(&hh.transformMatrix[2][1]  ) ;
     swap_4(&hh.transformMatrix[2][2]  ) ;
     swap_4(&hh.transformMatrix[2][3]  ) ;
     swap_4(&hh.transformMatrix[3][0]  ) ;
     swap_4(&hh.transformMatrix[3][1]  ) ;
     swap_4(&hh.transformMatrix[3][2]  ) ;
     swap_4(&hh.transformMatrix[3][3]  ) ;
   }

   /* simple checks on header stuff */

   if( hh.imageSize != 256           ||
       hh.dataSize  <  1             ||
       hh.dataSize  >  2             ||
       hh.mmPerPixel_sagittal <= 0.0 ||
       hh.mmPerPixel_coronal  <= 0.0 ||
       hh.mmPerPixel_axial    <= 0.0 ||
       hh.headOrigin_sagittal <= 0.0 ||
       hh.headOrigin_coronal  <= 0.0 ||
       hh.headOrigin_axial    <= 0.0   ) BADBAD("bad header data") ;

   /* debugging code to print header information */
#if 0
   printf("\n") ;
   printf("*** CTF MRI filename   = %s\n",fname                 ) ;
   printf("identifierString       = %s\n",hh.identifierString   ) ;
   printf("imageSize              = %d\n",hh.imageSize          ) ;
   printf("dataSize               = %d\n",hh.dataSize           ) ;
   printf("clippingRange          = %d\n",hh.clippingRange      ) ;
   printf("imageOrientation       = %d\n",hh.imageOrientation   ) ;
   printf("mmPerPixel_sagittal    = %f\n",hh.mmPerPixel_sagittal) ;
   printf("mmPerPixel_coronal     = %f\n",hh.mmPerPixel_coronal ) ;
   printf("mmPerPixel_axial       = %f\n",hh.mmPerPixel_axial   ) ;
   printf("headOrigin_sagittal    = %f\n",hh.headOrigin_sagittal) ;
   printf("headOrigin_coronal     = %f\n",hh.headOrigin_coronal ) ;
   printf("headOrigin_axial       = %f\n",hh.headOrigin_axial   ) ;
   printf("rotate_coronal         = %f\n",hh.rotate_coronal     ) ;
   printf("rotate_sagittal        = %f\n",hh.rotate_sagittal    ) ;
   printf("rotate_axial           = %f\n",hh.rotate_axial       ) ;
   printf("orthogonalFlag         = %d\n",hh.orthogonalFlag     ) ;
   printf("interpolatedFlag       = %d\n",hh.interpolatedFlag   ) ;
   printf("originalSliceThickness = %f\n",hh.originalSliceThickness ) ;
   printf("\n") ;
   printf("headModel.Nasion_Sag   = %d\n",hh.headModel.Nasion_Sag  ) ;
   printf("headModel.Nasion_Cor   = %d\n",hh.headModel.Nasion_Cor  ) ;
   printf("headModel.Nasion_Axi   = %d\n",hh.headModel.Nasion_Axi  ) ;
   printf("headModel.LeftEar_Sag  = %d\n",hh.headModel.LeftEar_Sag ) ;
   printf("headModel.LeftEar_Cor  = %d\n",hh.headModel.LeftEar_Cor ) ;
   printf("headModel.LeftEar_Axi  = %d\n",hh.headModel.LeftEar_Axi ) ;
   printf("headModel.RightEar_Sag = %d\n",hh.headModel.RightEar_Sag) ;
   printf("headModel.RightEar_Cor = %d\n",hh.headModel.RightEar_Cor) ;
   printf("headModel.RightEar_Axi = %d\n",hh.headModel.RightEar_Axi) ;
   printf("\n") ;
   printf("transformMatrix:\n"
          "  [ %9.4f %9.4f %9.4f %9.4f ]\n"
          "  [ %9.4f %9.4f %9.4f %9.4f ]\n"
          "  [ %9.4f %9.4f %9.4f %9.4f ]\n"
          "  [ %9.4f %9.4f %9.4f %9.4f ]\n" ,
        hh.transformMatrix[0][0] , hh.transformMatrix[0][1] ,
        hh.transformMatrix[0][2] , hh.transformMatrix[0][3] ,
        hh.transformMatrix[1][0] , hh.transformMatrix[1][1] ,
        hh.transformMatrix[1][2] , hh.transformMatrix[1][3] ,
        hh.transformMatrix[2][0] , hh.transformMatrix[2][1] ,
        hh.transformMatrix[2][2] , hh.transformMatrix[2][3] ,
        hh.transformMatrix[3][0] , hh.transformMatrix[3][1] ,
        hh.transformMatrix[3][2] , hh.transformMatrix[3][3]  ) ;
#endif

   /* determine if file is big enough to hold all data it claims */

   nn = THD_filesize(fname) ;
   if( nn < sizeof(hh) + hh.dataSize*hh.imageSize*hh.imageSize*hh.imageSize )
     BADBAD("input file too small") ;

   /*** from here, a lot of code is adapted from thd_analyzeread.c ***/

   datum_len = hh.dataSize ;
   switch( datum_len ){
     case 1:  datum_type = MRI_byte ; break ;
     case 2:  datum_type = MRI_short; break ;
   }
   nx = ny = nz = hh.imageSize ;

   /* set orientation:
      for now, assume (based on 1 sample) that data is stored in ASL or ASR order */

   ori[0] = 'A' ;          /* x is A-P */
   ori[1] = 'S' ;          /* y is S-I */

   /* determine if z is L-R or R-L from position of markers */

   ori[2] = (hh.headModel.LeftEar_Sag <= hh.headModel.RightEar_Sag) ? 'L' : 'R' ;

   oxx = ORCODE(ori[0]); oyy = ORCODE(ori[1]); ozz = ORCODE(ori[2]);
   if( !OR3OK(oxx,oyy,ozz) ){
     oxx = ORI_A2P_TYPE; oyy = ORI_S2I_TYPE; ozz = ORI_L2R_TYPE;   /** ASL? **/
   }

   orixyz.ijk[0] = oxx ; orixyz.ijk[1] = oyy ; orixyz.ijk[2] = ozz ;

   /* now set grid size, keeping in mind that
       A-P is positive and P-A is negative,
       R-L is positive and L-R is negative,
       I-S is positive and S-I is negative.       */

   switch( ori[0] ){
     case 'A':  dx =  hh.mmPerPixel_coronal ; xorg = hh.headOrigin_coronal ; break ;
     case 'P':  dx = -hh.mmPerPixel_coronal ; xorg = hh.headOrigin_coronal ; break ;
     case 'R':  dx =  hh.mmPerPixel_sagittal; xorg = hh.headOrigin_sagittal; break ;
     case 'L':  dx = -hh.mmPerPixel_sagittal; xorg = hh.headOrigin_sagittal; break ;
     case 'I':  dx =  hh.mmPerPixel_axial   ; xorg = hh.headOrigin_axial   ; break ;
     case 'S':  dx = -hh.mmPerPixel_axial   ; xorg = hh.headOrigin_axial   ; break ;
   }
   switch( ori[1] ){
     case 'A':  dy =  hh.mmPerPixel_coronal ; yorg = hh.headOrigin_coronal ; break ;
     case 'P':  dy = -hh.mmPerPixel_coronal ; yorg = hh.headOrigin_coronal ; break ;
     case 'R':  dy =  hh.mmPerPixel_sagittal; yorg = hh.headOrigin_sagittal; break ;
     case 'L':  dy = -hh.mmPerPixel_sagittal; yorg = hh.headOrigin_sagittal; break ;
     case 'I':  dy =  hh.mmPerPixel_axial   ; yorg = hh.headOrigin_axial   ; break ;
     case 'S':  dy = -hh.mmPerPixel_axial   ; yorg = hh.headOrigin_axial   ; break ;
   }
   switch( ori[2] ){
     case 'A':  dz =  hh.mmPerPixel_coronal ; zorg = hh.headOrigin_coronal ; break ;
     case 'P':  dz = -hh.mmPerPixel_coronal ; zorg = hh.headOrigin_coronal ; break ;
     case 'R':  dz =  hh.mmPerPixel_sagittal; zorg = hh.headOrigin_sagittal; break ;
     case 'L':  dz = -hh.mmPerPixel_sagittal; zorg = hh.headOrigin_sagittal; break ;
     case 'I':  dz =  hh.mmPerPixel_axial   ; zorg = hh.headOrigin_axial   ; break ;
     case 'S':  dz = -hh.mmPerPixel_axial   ; zorg = hh.headOrigin_axial   ; break ;
   }

   /* At this point, (xorg,yorg,zorg) are voxel indices;
      now, translate them into shifts such that if voxel
      index ii is the location of x=0, then xorg+ii*dx=0. */

   xorg = -dx*xorg ; yorg = -dy*yorg ; zorg = -dz*zorg ;

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   dset->idcode.str[0] = 'C' ;  /* overwrite 1st 3 bytes */
   dset->idcode.str[1] = 'T' ;
   dset->idcode.str[2] = 'F' ;

   ppp = THD_trailname(fname,0) ;                   /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */

   nxyz.ijk[0] = nx ; dxyz.xyz[0] = dx ;  /* setup axes lengths and voxel sizes */
   nxyz.ijk[1] = ny ; dxyz.xyz[1] = dy ;
   nxyz.ijk[2] = nz ; dxyz.xyz[2] = dz ;

   orixyz.ijk[0] = oxx ; orgxyz.xyz[0] = xorg ;
   orixyz.ijk[1] = oyy ; orgxyz.xyz[1] = yorg ;
   orixyz.ijk[2] = ozz ; orgxyz.xyz[2] = zorg ;

   iview = VIEW_ORIGINAL_TYPE ;

   /*-- actually send the values above into the dataset header --*/

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , datum_type ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_nvals       , 1 ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_view_type   , iview ,
                      ADN_func_type   , ANAT_MRAN_TYPE ,
                    ADN_none ) ;

   /*-- set byte order (for reading from disk) --*/

   ii = mri_short_order() ;
   if( swap ) ii = REVERSE_ORDER(ii) ;
   dset->dblk->diskptr->byte_order = ii ;

   /*-- flag to read data from disk using ANALYZE mode --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_CTFMRI ;
   strcpy( dset->dblk->diskptr->brick_name , fname ) ;

   /*-- add a set of tags for MEG fiducial points, if present --*/

   if( hh.headModel.LeftEar_Sag != hh.headModel.RightEar_Sag ){
     THD_usertaglist *tagset = myXtNew(THD_usertaglist) ;
     int nas_ii,nas_jj,nas_kk , lft_ii,lft_jj,lft_kk , rgt_ii,rgt_jj,rgt_kk ;
     THD_fvec3 fv ; THD_ivec3 iv ;

     tagset->num = 3 ;
     TAGLIST_SETLABEL( tagset , "CTF MEG Fiducials" ) ;

     /* load voxel indexes into dataset of the 3 tag points;
        note we have to permute these into the dataset axes order */

     switch( ori[0] ){
       case 'P':
       case 'A':  nas_ii = hh.headModel.Nasion_Cor ;
                  lft_ii = hh.headModel.LeftEar_Cor ;
                  rgt_ii = hh.headModel.RightEar_Cor ; break ;
       case 'R':
       case 'L':  nas_ii = hh.headModel.Nasion_Sag ;
                  lft_ii = hh.headModel.LeftEar_Sag ;
                  rgt_ii = hh.headModel.RightEar_Sag ; break ;
       case 'I':
       case 'S':  nas_ii = hh.headModel.Nasion_Axi ;
                  lft_ii = hh.headModel.LeftEar_Axi ;
                  rgt_ii = hh.headModel.RightEar_Axi ; break ;
     }
     switch( ori[1] ){
       case 'P':
       case 'A':  nas_jj = hh.headModel.Nasion_Cor ;
                  lft_jj = hh.headModel.LeftEar_Cor ;
                  rgt_jj = hh.headModel.RightEar_Cor ; break ;
       case 'R':
       case 'L':  nas_jj = hh.headModel.Nasion_Sag ;
                  lft_jj = hh.headModel.LeftEar_Sag ;
                  rgt_jj = hh.headModel.RightEar_Sag ; break ;
       case 'I':
       case 'S':  nas_jj = hh.headModel.Nasion_Axi ;
                  lft_jj = hh.headModel.LeftEar_Axi ;
                  rgt_jj = hh.headModel.RightEar_Axi ; break ;
     }
     switch( ori[2] ){
       case 'P':
       case 'A':  nas_kk = hh.headModel.Nasion_Cor ;
                  lft_kk = hh.headModel.LeftEar_Cor ;
                  rgt_kk = hh.headModel.RightEar_Cor ; break ;
       case 'R':
       case 'L':  nas_kk = hh.headModel.Nasion_Sag ;
                  lft_kk = hh.headModel.LeftEar_Sag ;
                  rgt_kk = hh.headModel.RightEar_Sag ; break ;
       case 'I':
       case 'S':  nas_kk = hh.headModel.Nasion_Axi ;
                  lft_kk = hh.headModel.LeftEar_Axi ;
                  rgt_kk = hh.headModel.RightEar_Axi ; break ;
     }

     TAG_SETLABEL( tagset->tag[0] , "Nasion" ) ;
     LOAD_IVEC3( iv , nas_ii,nas_jj,nas_kk ) ;  /* compute DICOM  */
     fv = THD_3dind_to_3dmm( dset , iv ) ;      /* coordinates of */
     fv = THD_3dmm_to_dicomm( dset , fv ) ;     /* this point     */
     UNLOAD_FVEC3( fv , tagset->tag[0].x , tagset->tag[0].y , tagset->tag[0].z ) ;
     tagset->tag[0].val = 0.0 ;
     tagset->tag[0].ti  = 0 ;
     tagset->tag[0].set = 1 ;

     TAG_SETLABEL( tagset->tag[1] , "Left Ear" ) ;
     LOAD_IVEC3( iv , lft_ii,lft_jj,lft_kk ) ;
     fv = THD_3dind_to_3dmm( dset , iv ) ;
     fv = THD_3dmm_to_dicomm( dset , fv ) ;
     UNLOAD_FVEC3( fv , tagset->tag[1].x , tagset->tag[1].y , tagset->tag[1].z ) ;
     tagset->tag[1].val = 0.0 ;
     tagset->tag[1].ti  = 0 ;
     tagset->tag[1].set = 1 ;

     TAG_SETLABEL( tagset->tag[2] , "Right Ear" ) ;
     LOAD_IVEC3( iv , rgt_ii,rgt_jj,rgt_kk ) ;
     fv = THD_3dind_to_3dmm( dset , iv ) ;
     fv = THD_3dmm_to_dicomm( dset , fv ) ;
     UNLOAD_FVEC3( fv , tagset->tag[2].x , tagset->tag[2].y , tagset->tag[2].z ) ;
     tagset->tag[2].val = 0.0 ;
     tagset->tag[2].ti  = 0 ;
     tagset->tag[2].set = 1 ;

     dset->tagset = tagset ;
   }

   RETURN(dset) ;
}

/*------------------------------------------------------------------*/
/*! Actually load data from a CTF MRI file into a dataset.
    Adapted from THD_load_analyze().
--------------------------------------------------------------------*/

void THD_load_ctfmri( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nv , nxy,nxyz,nxyzv , ibr,nbad ;
   FILE *fp ;
   void *ptr ;

ENTRY("THD_load_ctfmri") ;

   /*-- check inputs --*/

   if( !ISVALID_DATABLOCK(dblk)                         ||
       dblk->diskptr->storage_mode != STORAGE_BY_CTFMRI ||
       dblk->brick == NULL                                ) EXRETURN ;

   dkptr = dblk->diskptr ;

   /* open and position file at start of data (after header) */

   fp = fopen( dkptr->brick_name , "rb" ) ;  /* .img file */
   if( fp == NULL ) EXRETURN ;
   fseek( fp , sizeof(Version_2_Header) , SEEK_SET ) ;

   /*-- allocate space for data --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ; nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ; nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ; nxyzv = nxyz * nv ;

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /*-- malloc space for each brick separately --*/

   for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
     if( DBLK_ARRAY(dblk,ibr) == NULL ){
       ptr = malloc( DBLK_BRICK_BYTES(dblk,ibr) ) ;
       mri_fix_data_pointer( ptr ,  DBLK_BRICK(dblk,ibr) ) ;
       if( ptr == NULL ) nbad++ ;
     }
   }

   /*-- if couldn't get them all, take our ball and go home in a snit --*/

   if( nbad > 0 ){
     fprintf(stderr,
             "\n** failed to malloc %d ANALYZE bricks out of %d\n\a",nbad,nv);
     for( ibr=0 ; ibr < nv ; ibr++ ){
       if( DBLK_ARRAY(dblk,ibr) != NULL ){
         free(DBLK_ARRAY(dblk,ibr)) ;
         mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,ibr) ) ;
       }
     }
     fclose(fp) ; EXRETURN ;
   }


   /*-- read data from .img file into sub-brick arrays! --*/

   for( ibr=0 ; ibr < nv ; ibr++ )
     fread( DBLK_ARRAY(dblk,ibr), 1, DBLK_BRICK_BYTES(dblk,ibr), fp ) ;

   fclose(fp) ;

   /*-- swap bytes? --*/

   if( dkptr->byte_order != mri_short_order() ){
     for( ibr=0 ; ibr < nv ; ibr++ ){
       switch( DBLK_BRICK_TYPE(dblk,ibr) ){
         case MRI_short:
           mri_swap2( DBLK_BRICK_NVOX(dblk,ibr) , DBLK_ARRAY(dblk,ibr) ) ;
         break ;
       }
     }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_ctfsam( char *fname ){ return NULL; }
void THD_load_ctfsam( THD_datablock *dblk ){ }
