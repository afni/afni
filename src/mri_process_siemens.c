
/* functions for processing Siemens mosaic images     21 Dec 2010 [rickr]
 *
 * used in mri_read_dicom.c (for to3d) and dimon_afni.c (for Dimon)
 *
 * no globals allowed
 */

static char * extract_bytes_from_file(FILE *fp, off_t start, size_t len,
                                      int strize);
static int check_for_3D_acquisition( char ** epos, int * slices);

/* global noting whether to use this functionality */
int use_new_mosaic_code = 1;  /* 1 means use mri_process_siemens.c  [rickr] */

/*------------------------------------------------------------------------
 * attempt to process as a Siemens mosaic image        20 Dec 2010 [rickr]
 * (rewritten - try to make this usable for mri_read_dicom.c as well)
 *
 *      Sinfo   : pointer to sexinfo struct to fill     (return data)
 *      Sstr    : pointer to sexinfo string to fill     (return data)
 *      epos    : pointers to DICOM field strings
 *      fname   : file name to read
 *      assume  : assume dicom mosaic
 *      nx,y,z  : as processed outside, for error checking
 *
 * consider result as mosaic if sexinfo->mosaic_num > 1
 *
 * return  1 if mosaic
 *         0 if apparently not
 *        -1 on fatal error
 */
int process_siemens_mosaic(
        Siemens_extra_info * Sinfo, char ** Sstr, char ** epos,
        char * fname, int assume, int nx, int ny, int nz)
{
   FILE * fp=NULL;
   char * s_start=NULL, * s_start2=NULL, * s_end=NULL; /* KRH 25 Jul 2003 */
   int    mos_nx=0, mos_ny=0 , mos_ix=0, mos_iy=0;
   int    len=0,loc=0 , aa,bb , ii ;
   int    verb = g_dicom_ctrl.verb;   /* just a convenience */

   ENTRY("process_siemens_mosaic") ;
   
   /* check for crashable params */
   if( ! Sinfo || ! Sstr || ! epos ) {
      fprintf(stderr,"** process_siemens_mosaic, bad input (%p, %p, %p)\n",
              Sinfo, Sstr, epos);
      RETURN(-1);
   }

   /*-- 28 Oct 2002: Check if this is a Siemens mosaic.        --*/
   /*-- 02 Dec 2002: Don't use Acquisition Matrix anymore;
    *                instead, use the Siemens extra info
    *                in epos[E_SIEMENS_2].                     --*/
   /*-- 24 Dec 2002: Extract Sstr even if not a mosaic. --*/

   if( !        epos[E_ID_MANUFACTURER]            ||
       ! strstr(epos[E_ID_MANUFACTURER],"SIEMENS") ||
       !        epos[E_SIEMENS_2]                  ){
      static int nwarn=0 ;
      if( verb > 2 || ( nwarn < NWMAX && verb > 1 ) )
          fprintf(stderr,"-- not a Siemens Mosaic \n");
      nwarn++;
      RETURN(0);
   }

   sscanf(epos[E_SIEMENS_2],"%x%x%d [%d" , &aa,&bb , &len,&loc ) ;
   if( len <= 0 || loc <= 0 ){
      /* probably not an error */
      if( verb > 1 ) fprintf(stderr,"-- bad SIEMENS len %d, loc %d\n",len,loc);
      RETURN(0);
   }

   if( verb > 1 ) fprintf(stderr,"-- SIEMENS len %d, loc %d\n", len, loc);

   /* ok, suck in the info */
   fp = fopen( fname , "rb" ) ;
   if( ! fp ) {
      if( verb > 0 ) fprintf(stderr,"-- failed to open %s as Siemens\n",fname);
      RETURN(0);
   }

   *Sstr = extract_bytes_from_file(fp,(off_t)loc,(size_t)len,1);
   fclose(fp); /* in any case */

   /*-- process str_sexinfo only if this is marked as a mosaic image --*/
   /*-- preliminary processing of sexinfo EVEN IF NOT MARKED AS MOSAIC --*/
   /*-- (PSYCHOTIC, UNMARKED MOSAICS TURNING UP IN THE WILD!) KRH, 11/6/05 --*/

   /* if assume_dicom_mosaic is not set, then require "MOSAIC" string */
   /*                                             13 Mar 2006 [rickr] */

   if ( ! assume    &&
        (! epos[E_ID_IMAGE_TYPE] || ! strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC"))){
      if( verb > 1 ) fprintf(stderr,"-- file %s, not MOSAIC\n", fname);
      RETURN(0);
   }

   /* maybe we failed to read the text ... */
   if( ! *Sstr ) {
      if( verb > 1 ) fprintf(stderr,"-- failed to extract Siemens string\n");
      RETURN(0);
   }

   /* 31 Oct 2002: extract extra Siemens info from Sstr */

   /* KRH 25 Jul 2003 try to shrink Sstr between BEGIN and END,
    * particularly if there are 2 BEGINs (require both BEGIN and END) */
   /* Now there might be something between BEGIN and ### ...
    *                                   27 Jan 2012 [dglen, rickr]    */
   s_start = strstr(*Sstr,    "### ASCCONV BEGIN");
   if( s_start ) s_end = strstr(s_start, "### ASCCONV END");
   if( s_end ) {
      char *s_tmp;
      int s_size;

      s_start2 = strstr(s_start+21, "### ASCCONV BEGIN");
 
      /* update start to start2, if it seems appropriate */
      if( s_start2 && (s_start2 < s_end) ) s_start = s_start2;
 
      s_size = s_end - s_start + 19 ;
      s_tmp = (char *)calloc(s_size+1, sizeof(char));
      if( ! s_tmp ) {
         fprintf(stderr,"** failed to alloc %d bytes for Siemens temp str\n", 
                 s_size);
         RETURN(0);
      }
      memcpy(s_tmp,s_start,s_size);
      s_tmp[s_size] = '\0';
      free(*Sstr);
      *Sstr = s_tmp;
   }

   Sinfo->good = 0 ;              /* start by marking it as bad */
   for(ii = 0; ii < 3; ii++)
        Sinfo->have_data[ii] = 0; /* 25 Feb 03 Initialize new member KRH */

   get_siemens_extra_info( *Sstr , Sinfo, epos );
   
   if( ! Sinfo->good ){
      static int nwarn=0 ;
      if( nwarn < NWMAX ) {
         fprintf(stderr,"** DICOM WARNING: indecipherable Siemens Mosaic info"
                 " in file %s\n", fname);
         if( verb>1 ) fprintf(stderr,"  mosaic info = %s\n",elist[E_SIEMENS_2]);
      }
      if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM NOTICE: no more indecipherable messages\n");
      nwarn++ ;
      RETURN(0);
   }

   /* if it is not seen as a mosaic, bail */
   if( Sinfo->mosaic_num <= 1 ) {
      if( verb>1 ) fprintf(stderr,"-- mosaic_num = %d\n",Sinfo->mosaic_num);
      RETURN(0);
   }

   /* finally, we seem to have a mosaic image */

   /* compute size of mosaic layout
    * as 1st integer whose square is >= # of images in mosaic */

   for( mos_ix=1; mos_ix*mos_ix < Sinfo->mosaic_num; mos_ix++ ); /* nada */
   Sinfo->mos_ix = mos_iy = mos_ix ;

   /* nx should now be mos_ix times (actual image NX), so set mos_nx to that
    * of the image (and similarly for mos_ny) */

   Sinfo->mos_nx = mos_nx = nx / mos_ix ;
   Sinfo->mos_ny = mos_ny = ny / mos_iy ;  /* sub-image dimensions */

   /* sub-images must fit nicely into super-image layout */
   if( mos_ix*mos_nx != nx || mos_iy*mos_ny != ny ) {
      static int nwarn=0 ;
      if( nwarn < NWMAX ) {
         fprintf(stderr,
                 "\n** DICOM WARNING: bad Siemens Mosaic dims:"
                 " nx=%d ny=%d ix=%d iy=%d imx=%d imy=%d\n",
                 mos_nx,mos_ny , mos_ix,mos_iy , nx,ny ) ;
         fprintf(stderr,"   (consider the option -use_last_elem)\n");
      }
      if( nwarn == NWMAX )
         fprintf(stderr,"++ NOTICE: no more Mosaic dims messages\n");
      nwarn++ ;
      RETURN(0);
   }

   /* should be tagged as a 1 slice image thus far */
   if( nz > 1 ){
     if( verb > 0 ) fprintf(stderr, "** DICOM ERROR: %dx%d Mosaic of "
                            "%dx%d images in file %s, but also have nz=%d\n",
                            mos_ix,mos_iy,mos_nx,mos_ny,fname,nz) ;
     RETURN(-1) ;  /* fatal error */
   }

   /* warn about being a mosaic */
   /* (since this is common, print once and don't leave on output line) */
   if( verb > 0 ) {
      static int nwarn=0 ;
      if( nwarn < NWMAX )
         fprintf(stderr,"\n++ DICOM NOTICE: %dx%d Siemens Mosaic of "
                 "%d %dx%d images in file %s\n",
                 mos_ix, mos_iy, Sinfo->mosaic_num, mos_nx, mos_ny, fname) ;
      if( 0 && nwarn == NWMAX ) /* don't print in this case */
         fprintf(stderr,"++ DICOM NOTICE: no more Siemens images messages\n") ;
      nwarn++ ;
   }

   /* note - set mosaic and mos_nz upon return */

   RETURN(1);   /* return signifies valid mosaic */
}

/*-------------------------------------------------------------------------
 * moved from mri_read_dicom.c
 * - read 'len' bytes from file 'fp' at offset 'start'
 * - if strize, replace NUL bytes with spaces
 * - return the read (and maybe processed) string */
static char * extract_bytes_from_file(FILE *fp, off_t start, size_t len,
                                      int strize)
{
   char *ar ;
   size_t nn , ii ;

   if( fp == NULL || len == 0 ) return NULL ;    /* bad inputs? */
   ar = (char *)calloc(len+1, sizeof(char)) ;    /* make space for data */
   if( !ar ) {
     fprintf(stderr,"** EBFF: failed to alloc %lld chars\n",(long long)len);
     return NULL;
   }
   lseek( fileno(fp) , start , SEEK_SET ) ;      /* set file position */
   nn = fread( ar , 1 , len , fp ) ;             /* read data */
   if( nn == 0 ){ free(ar); return NULL; }       /* bad read? */

   if( strize ){                                 /* convert to C string? */
     for( ii=0 ; ii < nn ; ii++ )                /* scan for NULs and    */
       if( ar[ii] == '\0' ) ar[ii] = ' ' ;       /* replace with blanks  */
   }
   return ar ;
}


/*-------------------------------------------------------------------------
 * read data into image array, based on mosaic (fill image and image array)
 * (from mri_read_dicom.c                               22 Dec 2010 [rickr]
 *
 * - fp         : open file pointer                     (input/output)
 * - im         : current image                         (output)
 * - imar       : mosaic image array                    (output)
 * - flip_slices: whether to reverse slice order        (output)
 * - mi         : mosaic info                           (input)
 * - datum      : type for image array                  (input)
 * - bpp        : bytes per pixel                       (input)
 * - kor        : slice direction orientation           (input)
 * - swap       : whether to byte swap data             (input)
 * - dx,y,z     : slice dimensions                      (input)
 * - dt         : TR, if > 0                            (input)
 *
 * return 0 on success
 *        1 on error
 */
int read_mosaic_data( FILE *fp, MRI_IMAGE *im, MRI_IMARR *imar,
   int * flip_slices, Siemens_extra_info *mi, int datum, int bpp, int kor,
   int swap, float dx, float dy, float dz, float dt)
{   /*-- 28 Oct 2002:  is a 2D mosaic --*******************/

   char * dar=NULL, * iar ;
   int    nvox, yy, xx, nxx, ii, jj, slice ;
   int    mos_nx, mos_ny, mos_nz, mos_ix, mos_iy, mosaic_num;
   int    verb = g_dicom_ctrl.verb;  /* typing ease, default level is 1 */

   ENTRY("read_mosaic_data");

   if( !mi->good ) {
      if( verb ) fprintf(stderr,"** apply_z_orient but not mosaic");
      RETURN(1);
   }

   /* determine if slices should be reversed */
   *flip_slices = flip_slices_mosaic(mi, kor);

   /* just to make it a little easier to read */
   mos_nx = mi->mos_nx;   mos_ny = mi->mos_ny;
   mos_ix = mi->mos_ix;   mos_iy = mi->mos_ix; /* always square */
   mos_nz = mos_ix * mos_iy ;   /* number of slices in mosaic */

   if( verb > 1 )
      fprintf(stderr, "-- read_mosaic_data flip_slices %d "
                      "mos_nx,ny,nz = %d,%d,%d  mos_ix = %d\n",
              *flip_slices,mos_nx,mos_ny,mos_nz, mos_ix);

   mosaic_num = mi->mosaic_num;

   nvox = mos_nx*mos_ny*mos_nz ;         /* total number of voxels */

   if( g_dicom_ctrl.read_data ) {
      dar = (char*)calloc(bpp,nvox) ; /* make space for super-image */
      if(dar==NULL)  {  /* exit if can't allocate memory */
         ERROR_message("Could not allocate memory for mosaic volume");
         RETURN(1);
      }

      fread( dar , bpp , nvox , fp ) ;    /* read data directly into it */

      if( swap ){                        /* swap bytes? */
        switch( bpp ){
          default: break ;
          case 2:  swap_twobytes (   nvox, dar ) ; break ;  /* short */
          case 4:  swap_fourbytes(   nvox, dar ) ; break ;  /* int, float */
          case 8:  swap_fourbytes( 2*nvox, dar ) ; break ;  /* complex */
        }
      }
   }

   /* load data from dar into images */
   nxx = mos_nx * mos_ix ;              /* # pixels per mosaic line */

   for (ii=0;ii<mosaic_num;ii++) {
      /* find right slice - may be reading the series of slices backwards */
      if(*flip_slices) slice = mosaic_num - ii -1;
      else slice = ii;
      xx = slice % mos_ix; /* xx,yy are indices for position in mosaic matrix */
      yy = slice / mos_iy;
      /* im = mri_new( mos_nx , mos_ny , datum ) ; */
      im = mri_new_7D_generic(mos_nx,mos_ny, 1,1,1,1,1, datum,
                              g_dicom_ctrl.read_data);
      if( !im ) {
         fprintf(stderr,"** RMD: failed to allocate %d voxel image\n",
                 mos_nx * mos_ny);
         RETURN(1);
      }

      /* if reading data, actually copy data into MRI image */
      if( g_dicom_ctrl.read_data ) {
         iar = mri_data_pointer( im ) ;             /* sub-image array */

         for( jj=0 ; jj < mos_ny ; jj++ )  /* loop over rows inside sub-image */
           memcpy( iar + jj*mos_nx*bpp ,
                   dar + xx*mos_nx*bpp + (jj+yy*mos_ny)*nxx*bpp ,
                   mos_nx*bpp                                    ) ;
       }

       if( dx > 0.0 && dy > 0.0 && dz > 0.0 ){
         im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
       }
       if( dt > 0.0 ) im->dt = dt ;
       if( swap ) im->was_swapped = 1 ;

       ADDTO_IMARR(imar,im) ;
   } /* end of ii sub-image loop */

   if( dar )
      free(dar); /* don't need no more; copied all data out of it now */

   /* truncate zero images out of tail of mosaic */
   if( mosaic_num < IMARR_COUNT(imar) )
       TRUNCATE_IMARR(imar,mosaic_num) ;

   if( verb > 1 )
      fprintf(stderr,"\nmri_read_dicom Mosaic: mos_nx=%d mos_ny=%d mos_ix=%d"
                     " mos_iy=%d slices=%d\n",
              mos_nx,mos_ny,mos_ix,mos_iy,IMARR_COUNT(imar)) ;
      /* MCHECK ; */

   RETURN(0);
}

/* determine if slice order is reversed in Siemens mosaic files */
int flip_slices_mosaic (Siemens_extra_info *mi, int kor)
{
  /* kor = orientation of slices, which spatial direction is z-axis */
  /*       where 1=L-R, 2=P-A, 3=I-S */
  ENTRY("flip_slices_mosaic");

  if( g_dicom_ctrl.verb > 1 ) {
     printf("flip_slices_mosaic kor = %d\n", kor);
     printf("ImageNumbSag,Cor,Tra= %d,%d,%d\n",
            mi->ImageNumbSag, mi->ImageNumbCor, mi->ImageNumbTra);
  }

  switch(abs(kor)) {
     case 1:
       if(mi->ImageNumbSag==1) RETURN(1);
       RETURN(0);
     case 2:
       if(mi->ImageNumbCor==1) RETURN(1);
       RETURN(0);
     case 3:
       if(mi->ImageNumbTra==1) RETURN(1);
       RETURN(0);
     default :
       /* should never get here */
       RETURN(0);
  }
}

/*------------------------------------------------------------------------
 * finish filling the orientation string, and get the appropriate z offset
 * (from the code formerly known as mri_read_dicom.c)  22 Dec 2010 [rickr]
 *
 * require:
 *   - siemens extra info       : struct pointer
 *   - kor                      : must start in {1,2,3}, can end negative
 *
 * set
 *   - orients[4,5]             : from {R,L,A,P,I,S}
 *   - kor                      : might be negated
 *   - z offset                 : z-coordinate of initial slice
 *
 * return: 0 on success
 *         1 on error
 */
int apply_z_orient(Siemens_extra_info * Sinfo, char * orients, int * kor,
                   float * zoff)
{
   float z0=0.0, z1=0.0 ;

   ENTRY("apply_z_orient");

   /* validate the inputs */
   if( !Sinfo || !orients || !zoff || !kor) {
      if( g_dicom_ctrl.verb )
         fprintf(stderr,"** apply_z_orient, bad params (%p,%p,%p,%p)\n",
                 Sinfo, orients, zoff, kor);
      RETURN(1);
   }

   if( !Sinfo->good ) {
      if(g_dicom_ctrl.verb) fprintf(stderr,"** apply_z_orient but not mosaic");
      RETURN(1);
   }

   if( *kor > 3 || *kor < 1 ) {
      if(g_dicom_ctrl.verb )
         fprintf(stderr,"** apply_z_orient, bad kor = %d\n",*kor);
      RETURN(1);
   }

   /* 25 Feb 2003 changing error checking for mosaics missing one or more *
    * dimension of slice coordinates                                 KRH  */
   if (Sinfo->have_data[*kor-1]) {
     z0 = Sinfo->slice_xyz[0][*kor-1] ;   /* kor from orients above */
     z1 = Sinfo->slice_xyz[1][*kor-1] ;   /* z offsets of 1st 2 slices */
   } else {                  /* warn if sexinfo was bad */
     static int nwarn=0 ;
     if( nwarn < NWMAX )
       fprintf(stderr,"++ DICOM WARNING: Unusable coords in Siemens Mosaic\n");
     if( nwarn == NWMAX )
       fprintf(stderr,"++ DICOM NOTICE: no more Siemens coord messages\n");
     nwarn++ ;
   }

   /* finish z orientation now */

   if( z1-z0 < 0.0 ) *kor = -*kor ;     /* reversed orientation */
   if( orients[4] == '\0' ){
     switch( *kor ){
       case  1: orients[4] = 'R'; orients[5] = 'L'; break;
       case -1: orients[4] = 'L'; orients[5] = 'R'; break;
       case  2: orients[4] = 'A'; orients[5] = 'P'; break;
       case -2: orients[4] = 'P'; orients[5] = 'A'; break;
       case  3: orients[4] = 'I'; orients[5] = 'S'; break;
       case -3: orients[4] = 'S'; orients[5] = 'I'; break;
       default: orients[4] ='\0'; orients[5] ='\0'; break;
     }
   }
   orients[6] = '\0';

   /* negate z0 if zorient is positively directed */
   if( *kor > 0 ) *zoff = -z0;
   else           *zoff =  z0;

   if( g_dicom_ctrl.verb > 1 )
      fprintf(stderr,"-- apply_z_orient: z0,z1=(%f,%f), kor=%d, orients=%s\n",
              z0, z1, *kor, orients);

   RETURN(0);
}


/*---------------------------------------------------------------------------*/
/*! Parse the Siemens extra stuff for mosaic information.
    Ad hoc, based on sample data and no documentation.
    (moved from mri_read_dicom.c)     23 Dec 2010 [rickr]

    Fill 'mi' given 'str'.

    return 0 on success, 1 on error
-----------------------------------------------------------------------------*/
int get_siemens_extra_info(char *str, Siemens_extra_info *mi, char ** epos)
{
   char *cpt=NULL , *dpt, *ept ;
   int nn , mm , snum , last_snum=-1 ;
   int have_x[2] = {0,0},
       have_y[2] = {0,0},
       have_z[2] = {0,0};
   int verb = g_dicom_ctrl.verb;  /* typing convenience */
   float val ;
   char name[1024] ;
  
   ENTRY("get_siemens_extra_info");

   /*-- check for good input --*/

   if( ! mi ) { fprintf(stderr,"** GSEI: missing SEI %p\n", mi); RETURN(1); }

   mi->good = 0 ;
   for( snum=0 ; snum < NMOMAX ; snum++ )
     mi->slice_xyz[snum][0] = mi->slice_xyz[snum][1]
                            = mi->slice_xyz[snum][2] = -9999.9 ;

   /* okay, but do nothing */
   if( str == NULL || *str == '\0' ) {
      if( verb > 1 ) fprintf(stderr,"-- GSEI: no siemens string\n");
      RETURN(0);
   }

   /*-- find string that starts the slice information array --*/
   /* 04 Mar 2003 reworked this section to skip "fake matches" *
    * of the target string present in some mosaic files in the *
    * binary section                                     --KRH */
   nn = 0;
   ept = str;   /* use temporary pointer instead of passed pointer to Siemens */
   if(verb > 1){
     printf("Siemens extra info 1\n");
     printf("nn %d strlen str %d\n",  nn, (int) strlen(str));
   }

   /* require at least 3 of the 4 parameters in slice information */
   while ((nn < 3) && (strlen(ept) > 20)) {  /* mod drg, fredtam */
     /* use the updated ept for new searches      27 Jan 2012 [rickr, dglen] */
     cpt = strstr( ept , "sSliceArray.asSlice[" ) ; /* 20 characters minimum */
     if( cpt == NULL ) {
        if( verb > 1 ) fprintf(stderr,"-- GSEI: no sSliceArray.asSlice\n");
        RETURN(0);
     }

     /* interpret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;

     ept = cpt + 20; /* skip to end of "false match", advance to next KRH */
   }


   /*-- scan for coordinates, until can't find a good string to scan --*/
   while(1){
     if( nn   <  3                   ) break ;  /* bad conversion set */
     if( snum <  0 || snum >= NMOMAX ) break ;  /* slice number out of range */

     /* 21 Feb 2003 rework this section to allow for missing coordinates in
      * some mosaic files                                        --KRH  */
     if( strcmp(name,"sPosition.dSag") == 0 ){
       mi->slice_xyz[snum][0] = val;
       if (snum < 2) have_x[snum] = 1;
       last_snum = snum;
     } else if( strcmp(name,"sPosition.dCor") == 0 ){
       mi->slice_xyz[snum][1] = val;
       if (snum < 2) have_y[snum] = 1;
       last_snum = snum;
     } else if( strcmp(name,"sPosition.dTra") == 0 ){
       mi->slice_xyz[snum][2] = val;
       if (snum < 2) have_z[snum] = 1;
       last_snum = snum;
     }

     /* skip to next slice array assignment string (might not be coordinate) */

     dpt = cpt + mm ;                           /* just after 'val' */
     cpt =  dpt ;
     while( isspace(*cpt) ) cpt++ ;             /* skip over whitespace */
     if( cpt-dpt > 16 ) break ;                 /* too much space */
     if( strncmp(cpt,"sSliceArray.asSlice[",20) != 0 ) break ;  /* bad line */
     /* 04 Mar 2003 moved around to allow for locating "fake matches"  *
      * of the target text in some mosaic files' binary sections       */

     /* interpret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;
   }

   /* if got at least 1 slice info, mark data as being good */
   if( last_snum >= 0 ){
     mi->good       = 1 ;
     if (have_x[0] && have_x[1]) mi->have_data[0] = 1;
     if (have_y[0] && have_y[1]) mi->have_data[1] = 1;
     if (have_z[0] && have_z[1]) mi->have_data[2] = 1;
     mi->mosaic_num = last_snum+1 ;
   }

   /* maybe it is a 3D acquisition     2 Jun 2016 [rickr,DRG] */
   if( check_for_3D_acquisition(epos, &mi->mosaic_num) ) RETURN(1);

   ept = str;
   if( (cpt = strstr(str,"\nsSliceArray.ucImageNumbSag")) ){
      sscanf(cpt, "%1022s = %x", name, &mi->ImageNumbSag);
      if( verb > 2 ) printf("ImageNumbSag in header %x\n",mi->ImageNumbSag);
   }
   else mi->ImageNumbSag = 0;

   if( (cpt = strstr(str,"\nsSliceArray.ucImageNumbTra")) ){
      sscanf(cpt, "%1022s = %x", name, &mi->ImageNumbTra);
      if( verb > 2 ) printf("ImageNumbTra in header %x\n",mi->ImageNumbTra);
   }
   else mi->ImageNumbTra = 0;

   if( (cpt = strstr(str,"\nsSliceArray.ucImageNumbCor")) ){
      sscanf(cpt, "%1022s = %x", name, &mi->ImageNumbCor);
      if( verb > 2 ) printf("ImageNumbCor in header %x\n",mi->ImageNumbCor);
   }
   else mi->ImageNumbCor = 0;

   RETURN(0);
}

/* adjust mi data if 3D volume acquisition
 * return 0 on success                          2 Jun 2016 [rickr,DRG] */
static int check_for_3D_acquisition(char ** epos, int * nslices)
{
   char * cp;
   int    nread=0, lslices=0;  /* local slices */

   ENTRY("check_for_3D_acquisition");
   
   if( ! nslices ) RETURN(0);

   /* maybe we already know what is here */ 
   if( *nslices > 1 ) RETURN(0);

   /* --------------- check that E_MR_ACQ_TYPE shows 3D --------------- */
   if( ! epos[E_MR_ACQ_TYPE] ) RETURN(0);

   cp = af_strnstr(epos[E_MR_ACQ_TYPE], "//", 60);
   if( ! cp ) RETURN(0);

   if( strncmp(cp+2, "3D", 2) ) RETURN(0);

   /* --------------- have 3D, check E_SIEMENS_3D_NSLICES --------------- */

   if( ! epos[E_SIEMENS_3D_NSLICES] ) RETURN(0);

   cp = af_strnstr(epos[E_SIEMENS_3D_NSLICES], "//", 60);
   if( ! cp ) RETURN(0);

   nread = sscanf(cp+2,"%d" , &lslices);
   if( nread == 0 ) {
      if( g_dicom_ctrl.verb > 1 )
         ERROR_message("** CF3DM: failed to read DICOM 3D nslices from:\n"
                       "   '%.60s'\n", epos[E_SIEMENS_3D_NSLICES]);
      RETURN(1);
   }

   if( lslices <= 1 ) RETURN(0);

   if( g_dicom_ctrl.verb > 1 )
      INFO_message("++ CF3DM: found %d slices in DICOM 3D\n", lslices);

   *nslices = lslices;

   RETURN(0);
}

