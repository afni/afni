#include "mrilib.h"
#include "thd.h"

/*******************************************************************/
/* Stuff to read CTF MRI and SAM datasets from the NIH MEG system. */
/* 04 Dec 2002: first cut by RWCox.                                */
/*******************************************************************/

/****************************************************************/
/*********** header structs for CTF MRI version 2.0 file        */
/****************************************************************/

enum { coronal=0, sagittal, axial };

#define LEFT_ON_LEFT      0
#define LEFT_ON_RIGHT     1

enum { Modality_MRI=0, Modality_CT, Modality_PET, Modality_SPECT, Modality_OTHER };

#define VERSION_1_STR   "CTF_MRI_FORMAT VER 1.0"
#define VERSION_2_STR   "CTF_MRI_FORMAT VER 2.0"
#define VERSION_21_STR  "CTF_MRI_FORMAT VER 2.1"
#define VERSION_22_STR  "CTF_MRI_FORMAT VER 2.2"

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

/* this struct isn't used in AFNI */
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

/* the header for the .mri files */
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
/*! Macro for bad return.  */

#undef  BADBAD
#define BADBAD(s)                                                \
  do{ fprintf(stderr,"** THD_open_ctfmri(%s): %s\n",fname,s);    \
      RETURN(NULL);                                              \
  } while(0)

/*-----------------------------------------------------------------*/
/*! Function to count slices like CTF does. */

int CTF_count( double start, double end , double delta )
{
   int nn=0 ; double cc ;
   for( cc=start ; cc <= (end+1.0e-6) ; cc += delta ) nn++ ;
   return nn ;
}

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
   int ngood , length , datum_type=0 , datum_len , oxx,oyy,ozz ;
   int   nx,ny,nz ;
   float dx,dy,dz=0.0, xorg,yorg,zorg=0.0;

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

     swap_4(&hh.transformMatrix[0][0]  ) ;   /* this stuff not used yet */
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
       hh.mmPerPixel_axial    <= 0.0   ) BADBAD("bad header data") ;

   /*- 16 Mar 2005: instead of complaining about negative Origins,
                    just reset them to something semi-reasonable;
       I just get by with a little help from my friends
       - Zuxiang Li in this case.                                -*/

   if( hh.headOrigin_sagittal <= 0.0 ) hh.headOrigin_sagittal = hh.imageSize/2.;
   if( hh.headOrigin_coronal  <= 0.0 ) hh.headOrigin_coronal  = hh.imageSize/2.;
   if( hh.headOrigin_axial    <= 0.0 ) hh.headOrigin_axial    = hh.imageSize/2.;

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
   if( nn < hh.dataSize*hh.imageSize*hh.imageSize*hh.imageSize )
     BADBAD("input file too small") ;

   /*** from here, a lot of code is adapted from thd_analyzeread.c ***/

   datum_len = hh.dataSize ;
   switch( datum_len ){                         /* the only 2 cases */
     case 1:  datum_type = MRI_byte ; break ;
     case 2:  datum_type = MRI_short; break ;
   }
   nx = ny = nz = hh.imageSize ;              /* volumes are cubes! */

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

   /* now set grid size, keeping in mind that
       A-P is positive and P-A is negative,
       R-L is positive and L-R is negative,
       I-S is positive and S-I is negative.   */

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

   MCW_hash_idcode( fname , dset ) ;  /* 06 May 2005 */

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

   /*-- flag to read data from disk using CTF MRI mode --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_CTFMRI ;
   strcpy( dset->dblk->diskptr->brick_name , fname ) ;

   /*-- for fun, add a set of tags for MEG fiducial points, if present --*/

   if( hh.headModel.LeftEar_Sag != hh.headModel.RightEar_Sag ){
     THD_usertaglist *tagset = myRwcNew(THD_usertaglist) ;
     int nas_ii,nas_jj,nas_kk=0 , lft_ii,lft_jj,lft_kk=0 , 
         rgt_ii,rgt_jj,rgt_kk=0 ;
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
   int nx,ny,nz,nv , nxy,nxyz,nxyzv , ibr=0,nbad ;
   FILE *fp ;
   void *ptr ;

ENTRY("THD_load_ctfmri") ;

   /*-- check inputs --*/

   if( !ISVALID_DATABLOCK(dblk)                         ||
       dblk->diskptr->storage_mode != STORAGE_BY_CTFMRI ||
       dblk->brick == NULL                                ) EXRETURN ;

   dkptr = dblk->diskptr ;

   /* open and position file at start of data (after header) */

   fp = fopen( dkptr->brick_name , "rb" ) ;  /* .mri file */
   if( fp == NULL ) EXRETURN ;

   /*-- allocate space for data --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ; nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ; nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ; nxyzv = nxyz * nv ;

   /* 26 Feb 2005: seek backwards from end,
                   instead of forwards from start */

#if 0
   fseek( fp , sizeof(Version_2_Header) , SEEK_SET ) ;  /* old */
#else
   switch( DBLK_BRICK_TYPE(dblk,0) ){
     default:  ERROR_exit("Unrecognized type in CTF file") ; break ;
     case MRI_float: ibr = sizeof(float) ; break ;   /* illegal */
     case MRI_short: ibr = sizeof(short) ; break ;
     case MRI_byte:  ibr = sizeof(byte)  ; break ;
   }
   fseek( fp , -ibr*nxyzv , SEEK_END ) ;                /* new */
#endif

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /*-- malloc space for each brick separately --*/

   for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
     if( DBLK_ARRAY(dblk,ibr) == NULL ){
       ptr = AFMALL(void, DBLK_BRICK_BYTES(dblk,ibr) ) ;
       mri_fix_data_pointer( ptr ,  DBLK_BRICK(dblk,ibr) ) ;
       if( ptr == NULL ) nbad++ ;
     }
   }

   /*-- if couldn't get them all, take our ball and go home in a snit --*/

   if( nbad > 0 ){
     fprintf(stderr,
             "\n** failed to malloc %d CTR MRI bricks out of %d\n\a",nbad,nv);
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
         default: break ;
         case MRI_short:
           mri_swap2( DBLK_BRICK_NVOX(dblk,ibr) , DBLK_ARRAY(dblk,ibr) ) ;
         break ;
       }
     }
   }

   EXRETURN ;
}

/****************************************************************************
 SAM static image files are structured as follows:

  char     Identity[8] = "SAMIMAGE"; // uniquely identifies image file
  SAM_HDR  SAMHeader;                // SAM header
  double   Voxel[0];                 // 1st SAM voxel (units=A-m, (A-m)^2,
  double   Voxel[1];                 // 2nd SAM voxel        Z, T, F, or P)
              "
              "
  double   Voxel[V];                 // last SAM voxel

  Coefficients & image voxels are ordered in X,Y,Z sequence, with Z the least
  significant index (most rapidly changing), Y is next, and then X.
  Coordinate indices always advance in the positive direction. This implies
  that Voxel[0] is in the right, posterior, inferior position relative to
  the region of interest (bounding box of image).

  RWCox: the data storage order seems to be IRP, based on the above
         comments, and on the CTF head coordinates system being PRI.
*****************************************************************************/

#define COV_VERSION      1                  /* this is version 1 -- got it? */
#define SAM_VERSION      1                  /* this, too! */

/** SAM file types **/

#define SAM_TYPE_IMAGE         0            /* flags file as a SAM static image file */
#define SAM_TYPE_WT_ARRAY      1            /* flags file as SAM coefficients for regular target array */
#define SAM_TYPE_WT_LIST       2            /* flags file as SAM coefficients for target list */

/** define SAM unit types **/

#define      SAM_UNIT_COEFF    0            /* SAM coefficients A-m/T */
#define      SAM_UNIT_MOMENT   1            /* SAM source (or noise) strength A-m */
#define      SAM_UNIT_POWER    2            /* SAM source (or noise) power (A-m)^2 */
#define      SAM_UNIT_SPMZ     3            /* SAM z-deviate */
#define      SAM_UNIT_SPMF     4            /* SAM F-statistic */
#define      SAM_UNIT_SPMT     5            /* SAM T-statistic */
#define      SAM_UNIT_SPMP     6            /* SAM probability */
#define      SAM_UNIT_MUSIC    7            /* MUSIC metric */

/* 'SAM_HDR' is to be used for both SAM coefficients & SAM static images */
typedef struct {
   int    Version;       /* file version number (should be 1) */
   char   SetName[256];  /* name of parent dataset */
   int    NumChans;      /* # of channels used by SAM */
   int    NumWeights;    /* # of SAM virtual channels (0=static image) */
   int    pad_bytes1;    /* ** align next double on 8 byte boundary */
   double XStart;        /* x-start coordinate (m) */
   double XEnd;          /* x-end coordinate (m) */
   double YStart;        /* y-start coordinate (m) */
   double YEnd;          /* y-end coordinate (m) */
   double ZStart;        /* z-start coordinate (m) */
   double ZEnd;          /* z-end coordinate (m) */
   double StepSize;      /* voxel step size (m) */
   double HPFreq;        /* highpass frequency (Hz) */
   double LPFreq;        /* lowpass frequency (Hz) */
   double BWFreq;        /* bandwidth of filters (Hz) */
   double MeanNoise;     /* mean primary sensor noise (T) */
   char   MriName[256];  /* MRI image file name */
   int    Nasion[3];     /* MRI voxel index for nasion */
   int    RightPA[3];    /* MRI voxel index for right pre-auricular */
   int    LeftPA[3];     /* MRI voxel index for left pre-auricular */
   int    SAMType;       /* SAM file type */
   int    SAMUnit;       /* SAM units (a bit redundant, but may be useful) */
   int    pad_bytes2;    /* ** align end of structure on 8 byte boundary */
} SAM_HDR;

/*** 26 Feb 2005: version 2 of the SAM header ***/

#if 0
typedef struct {
   int     Version;         /* file version number (should be 2) */
   char    SetName[256];    /* name of parent dataset */
   int     NumChans;        /* # of channels used by SAM */
   int     NumWeights;      /* # of SAM virtual channels (0=static image) */
   int     pad_bytes1;      /* ** align next double on 8 byte boundary */
   double  XStart;          /* x-start coordinate (m) */
   double  XEnd;            /* x-end coordinate (m) */
   double  YStart;          /* y-start coordinate (m) */
   double  YEnd;            /* y-end coordinate (m) */
   double  ZStart;          /* z-start coordinate (m) */
   double  ZEnd;            /* z-end coordinate (m) */
   double  StepSize;        /* voxel step size (m) */
   double  HPFreq;          /* highpass frequency (Hz) */
   double  LPFreq;          /* lowpass frequency (Hz) */
   double  BWFreq;          /* bandwidth of filters (Hz) */
   double  MeanNoise;       /* mean primary sensor noise (T) */
   char    MriName[256];    /* MRI image file name */
   int     Nasion[3];       /* MRI voxel index for nasion */
   int     RightPA[3];      /* MRI voxel index for right pre-auricular */
   int     LeftPA[3];       /* MRI voxel index for left pre-auricular */
   int     SAMType;         /* SAM file type */
   int     SAMUnit;         /* SAM units (a bit redundant, but may be useful) */
   int     pad_bytes2;      /* ** align end of structure on 8 byte boundary */
   double  MegNasion[3];    /* MEG dewar coordinates for nasion (m) */
   double  MegRightPA[3];   /* MEG dewar coordinates for R pre-auricular (m) */
   double  MegLeftPA[3];    /* MEG dewar coordinates for L pre-auricular (m) */
   char    SAMUnitName[32]; /* SAM units (redundant, but useful too!) */
} SAM_HDR_v2;
#endif


/*-------------------------*/
/*! Macro for bad return. */

#undef  BADBAD
#define BADBAD(s)                                                \
  do{ fprintf(stderr,"** THD_open_ctfsam(%s): %s\n",fname,s);    \
      RETURN(NULL);                                              \
  } while(0)

/*-----------------------------------------------------------------*/
/*! Open a CTF .svl (SAM) file as an unpopulated AFNI dataset.
    It will be populated later, in THD_load_ctfsam().
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_ctfsam( char *fname )
{
   FILE *fp ;
   SAM_HDR hh ;
   char Identity[9] ;
   int ii,nn , swap ;
   THD_3dim_dataset *dset=NULL ;
   char prefix[THD_MAX_PREFIX] , *ppp , tname[12] , ori[4] ;
   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   int iview ;
   int ngood , length , datum_type , datum_len , oxx,oyy,ozz ;
   int   nx,ny,nz ;
   float dx,dy,dz , xorg,yorg,zorg ;

ENTRY("THD_open_ctfsam") ;

   /* open input file */

   if( fname == NULL || *fname == '\0' ) BADBAD("bad input filename");
   fp = fopen( fname , "rb" ) ;
   if( fp == NULL )                      BADBAD("can't open input file");

   /* read header [1st 8 bytes are "SAMIMAGE"] */

   fread( Identity , 1,8 , fp ) ; Identity[8] = '\0' ;
   fread( &hh , sizeof(hh) , 1 , fp ) ;
   fclose(fp) ;

   if( strcmp(Identity,"SAMIMAGE") != 0 ) BADBAD("Identity != SAMIMAGE") ;
   if( hh.Version                  == 0 ) BADBAD("bad header Version") ;

   swap = (hh.Version < 0) || (hh.Version > 3) ;    /* byte swap? */

   if( swap ){                   /* swap various header fields */
     swap_4( &hh.Version    ) ;
     swap_4( &hh.NumChans   ) ;
     swap_4( &hh.NumWeights ) ;
     swap_8( &hh.XStart     ) ;
     swap_8( &hh.XEnd       ) ;
     swap_8( &hh.YStart     ) ;
     swap_8( &hh.YEnd       ) ;
     swap_8( &hh.ZStart     ) ;
     swap_8( &hh.ZEnd       ) ;
     swap_8( &hh.StepSize   ) ;
     swap_8( &hh.HPFreq     ) ;
     swap_8( &hh.LPFreq     ) ;
     swap_8( &hh.BWFreq     ) ;
     swap_8( &hh.MeanNoise  ) ;
     swap_4( &hh.Nasion[0]  ) ;
     swap_4( &hh.RightPA[0] ) ;
     swap_4( &hh.LeftPA[0]  ) ;
     swap_4( &hh.Nasion[1]  ) ;
     swap_4( &hh.RightPA[1] ) ;
     swap_4( &hh.LeftPA[1]  ) ;
     swap_4( &hh.Nasion[2]  ) ;
     swap_4( &hh.RightPA[2] ) ;
     swap_4( &hh.LeftPA[2]  ) ;
     swap_4( &hh.SAMType    ) ;
     swap_4( &hh.SAMUnit    ) ;
   }

   /* simple checks on header values */

   if( hh.Version  < 0        ||
       hh.Version  > 3        ||  /* 26 Feb 2005 */
       hh.XStart   >= hh.XEnd ||
       hh.YStart   >= hh.YEnd ||
       hh.ZStart   >= hh.ZEnd ||
       hh.StepSize <= 0.0       ) BADBAD("bad header data") ;

#if 0
   printf("\n") ;
   printf("**CTF SAM : %s\n",fname) ;
   printf("Version   = %d\n",hh.Version) ;
   printf("NumChans  = %d\n",hh.NumChans) ;
   printf("NumWeights= %d\n",hh.NumWeights) ;
   printf("XStart    = %g\n",hh.XStart) ;
   printf("Xend      = %g\n",hh.XEnd) ;
   printf("YStart    = %g\n",hh.YStart) ;
   printf("YEnd      = %g\n",hh.YEnd) ;
   printf("ZStart    = %g\n",hh.ZStart) ;
   printf("Zend      = %g\n",hh.ZEnd) ;
   printf("StepSize  = %g\n",hh.StepSize) ;
   printf("HPFreq    = %g\n",hh.HPFreq) ;
   printf("LPFreq    = %g\n",hh.LPFreq) ;
   printf("BWFreq    = %g\n",hh.BWFreq) ;
   printf("MeanNoise = %g\n",hh.MeanNoise) ;
   printf("Nasion[0] = %d\n",hh.Nasion[0]) ;
   printf("Nasion[1] = %d\n",hh.Nasion[1]) ;
   printf("Nasion[2] = %d\n",hh.Nasion[2]) ;
   printf("RightPA[0]= %d\n",hh.RightPA[0]) ;
   printf("RightPA[1]= %d\n",hh.RightPA[1]) ;
   printf("RightPA[2]= %d\n",hh.RightPA[2]) ;
   printf("LeftPA[0] = %d\n",hh.LeftPA[0]) ;
   printf("LeftPA[1] = %d\n",hh.LeftPA[1]) ;
   printf("LeftPA[2] = %d\n",hh.LeftPA[2]) ;
   printf("SAMtype   = %d\n",hh.SAMType) ;
   printf("SAMunit   = %d\n",hh.SAMUnit) ;
   printf("SetName   = %s\n",hh.SetName) ;
   printf("MriName   = %s\n",hh.MriName) ;
   printf("headersize= %d\n",sizeof(hh)+8) ;
#endif

   hh.StepSize *= 1000.0 ;   /* convert distances from m to mm */
   hh.XStart   *= 1000.0 ;   /* (who the hell uses meters for brain imaging?) */
   hh.YStart   *= 1000.0 ;   /* (blue whales?  elephants?) */
   hh.ZStart   *= 1000.0 ;
   hh.XEnd     *= 1000.0 ;
   hh.YEnd     *= 1000.0 ;
   hh.ZEnd     *= 1000.0 ;

   dx = dy = dz = hh.StepSize ;  /* will be altered below */

#if 0
   nx = (int)((hh.ZEnd - hh.ZStart)/dz + 0.99999); /* dataset is stored in Z,Y,X order */
   ny = (int)((hh.YEnd - hh.YStart)/dy + 0.99999); /* but AFNI calls these x,y,z       */
   nz = (int)((hh.XEnd - hh.XStart)/dx + 0.99999);
#else
   nx = CTF_count( hh.ZStart , hh.ZEnd , hh.StepSize ) ;
   ny = CTF_count( hh.YStart , hh.YEnd , hh.StepSize ) ;
   nz = CTF_count( hh.XStart , hh.XEnd , hh.StepSize ) ;
#endif

   /* determine if file is big enough to hold all data it claims */

   nn = THD_filesize(fname) ;
   if( nn < sizeof(double)*nx*ny*nz ) BADBAD("input file too small") ;

   datum_type = MRI_float ;  /* actually is double, but AFNI doesn't grok that */
                             /* will be converted to floats when reading data */

   /* set orientation = IRP = xyz ordering */

   ori[0] = 'I'; ori[1] = 'R'; ori[2] = 'P';

   oxx = ORCODE(ori[0]); oyy = ORCODE(ori[1]); ozz = ORCODE(ori[2]);
   if( !OR3OK(oxx,oyy,ozz) ){
     oxx = ORI_I2S_TYPE; oyy = ORI_R2L_TYPE; ozz = ORI_P2A_TYPE;   /** IRP? **/
   }

   orixyz.ijk[0] = oxx ; orixyz.ijk[1] = oyy ; orixyz.ijk[2] = ozz ;

   /* now set grid size, keeping in mind that
      A-P is positive and P-A is negative,
      R-L is positive and L-R is negative,
      I-S is positive and S-I is negative.       */

   switch( ori[0] ){
     case 'A':  dx =  hh.StepSize ; xorg = -hh.XStart ; break ;
     case 'P':  dx = -hh.StepSize ; xorg = -hh.XStart ; break ;
     case 'R':  dx =  hh.StepSize ; xorg =  hh.YStart ; break ;
     case 'L':  dx = -hh.StepSize ; xorg =  hh.YStart ; break ;
     case 'I':  dx =  hh.StepSize ; xorg =  hh.ZStart ; break ;
     case 'S':  dx = -hh.StepSize ; xorg =  hh.ZStart ; break ;
   }
   switch( ori[1] ){
     case 'A':  dy =  hh.StepSize ; yorg = -hh.XStart ; break ;
     case 'P':  dy = -hh.StepSize ; yorg = -hh.XStart ; break ;
     case 'R':  dy =  hh.StepSize ; yorg =  hh.YStart ; break ;
     case 'L':  dy = -hh.StepSize ; yorg =  hh.YStart ; break ;
     case 'I':  dy =  hh.StepSize ; yorg =  hh.ZStart ; break ;
     case 'S':  dy = -hh.StepSize ; yorg =  hh.ZStart ; break ;
   }
   switch( ori[2] ){
     case 'A':  dz =  hh.StepSize ; zorg = -hh.XStart ; break ;
     case 'P':  dz = -hh.StepSize ; zorg = -hh.XStart ; break ;
     case 'R':  dz =  hh.StepSize ; zorg =  hh.YStart ; break ;
     case 'L':  dz = -hh.StepSize ; zorg =  hh.YStart ; break ;
     case 'I':  dz =  hh.StepSize ; zorg =  hh.ZStart ; break ;
     case 'S':  dz = -hh.StepSize ; zorg =  hh.ZStart ; break ;
   }

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   dset->idcode.str[0] = 'C' ;  /* overwrite 1st 3 bytes */
   dset->idcode.str[1] = 'T' ;
   dset->idcode.str[2] = 'F' ;

   MCW_hash_idcode( fname , dset ) ;  /* 06 May 2005 */

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
                      ADN_type        , HEAD_FUNC_TYPE ,
                      ADN_view_type   , iview ,
                      ADN_func_type   , FUNC_FIM_TYPE ,
                    ADN_none ) ;

   /*-- set byte order (for reading from disk) --*/

   ii = mri_short_order() ;
   if( swap ) ii = REVERSE_ORDER(ii) ;
   dset->dblk->diskptr->byte_order = ii ;

   /*-- flag to read data from disk using CTF SAM mode --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_CTFSAM ;
   strcpy( dset->dblk->diskptr->brick_name , fname ) ;

   RETURN(dset) ;
}

/*------------------------------------------------------------------*/
/*! Actually load data from a CTF SAM file into a dataset.
--------------------------------------------------------------------*/

void THD_load_ctfsam( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nv , nxy,nxyz,nxyzv , ibr,nbad , ii,swap ;
   FILE *fp ;
   void *ptr ;
   double *dbar ;
   float  *ftar ;

ENTRY("THD_load_ctfsam") ;

   /*-- check inputs --*/

   if( !ISVALID_DATABLOCK(dblk)                         ||
       dblk->diskptr->storage_mode != STORAGE_BY_CTFSAM ||
       dblk->brick == NULL                                ) EXRETURN ;

   dkptr = dblk->diskptr ;

   /*-- allocate space for data --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ; nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ; nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ; nxyzv = nxyz * nv ;

   /* position file 8*nxyzv bytes before end of file */

   fp = fopen( dkptr->brick_name , "rb" ) ;  /* .svl file */
   if( fp == NULL ) EXRETURN ;

   /* 26 Feb 2005: instead of skipping the header,
                   whose size varies with the SAM version number,
                   just seek backwards from the end to the correct size */

   fseek( fp , -sizeof(double)*nxyzv , SEEK_END ) ;

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /*-- malloc space for each brick separately --*/

   for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
     if( DBLK_ARRAY(dblk,ibr) == NULL ){
       ptr = AFMALL(void, DBLK_BRICK_BYTES(dblk,ibr) ) ;
       mri_fix_data_pointer( ptr ,  DBLK_BRICK(dblk,ibr) ) ;
       if( ptr == NULL ) nbad++ ;
     }
   }

   /*-- if couldn't get them all, take our ball and go home in a snit --*/

   if( nbad > 0 ){
     fprintf(stderr,
             "\n** failed to malloc %d CTR MRI bricks out of %d\n\a",nbad,nv);
     for( ibr=0 ; ibr < nv ; ibr++ ){
       if( DBLK_ARRAY(dblk,ibr) != NULL ){
         free(DBLK_ARRAY(dblk,ibr)) ;
         mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,ibr) ) ;
       }
     }
     fclose(fp) ; EXRETURN ;
   }

   /*-- SAM data is stored as doubles,
        but we have to store it in AFNI as floats --*/

   dbar = (double *) calloc(sizeof(double),nxyz) ;     /* workspace */
   swap = ( dkptr->byte_order != mri_short_order() ) ;

   for( ibr=0 ; ibr < nv ; ibr++ ){            /* loop over sub-bricks */
     fread( dbar, 1, sizeof(double)*nxyz, fp ) ; /* read data to workspace */
     ftar = DBLK_ARRAY(dblk,ibr) ;               /* float array in dataset */
     for( ii=0 ; ii < nxyz ; ii++ ){             /* loop over voxels */
       if( swap ) swap_8(dbar+ii) ;                /* swap it */
       ftar[ii] = dbar[ii] ;                       /* save it as a float */
     }
   }

   fclose(fp) ; free(dbar) ;  /* toss out the trash */
   EXRETURN ;
}
