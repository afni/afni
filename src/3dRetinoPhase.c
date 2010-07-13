#include "mrilib.h"

typedef enum { 
   NEG=-3, CONT=-2, CCW=-1, 
   NOT_SET=0, 
   CW=1, EXP=2, POS=3
   } PHAZE_DIRS;
typedef enum { 
   k_EXP=0, k_CON, k_CW, k_CCW, k_N } STIM_TYPES;
typedef enum { 
   ECC=0, POL=1 } FIELD_PARAMS;
   
typedef struct {
   /* to set outside of processor function */
   float ftap;    /* taper fraction at ends of data */
   float dt;      /* TR */
   float pre;     /* period in seconds, before stimulus began */
   float Fstim[2];   /* stimulus fundamental freq. in Hz */ 
   int nvals;     /* number of points in time series */
   int verb;
   THD_3dim_dataset *iset;
   int *vox;      /* vox[k] is the 1D index of the kth voxel in mask*/
   int nmask;     /* total number of voxels in mask */
   char *prefix; 
   char *oext;    /* output extension */
   int dir;       /* +1 for CW, -1 for CCW
                           +2 for Exp. -2 for contracting*/
   int n[2];           /* number of rings, wedges */
   int spectra;
   int fixsum;
   
   /* to be set upon first calling of processor function */
   int nfft;      /* number of points in fft */
   float fstep;   /* Delta freq. Hz*/
   float Fresp[2];   /* FMRI response fundamental freq. in Hz */ 
   int   stk[2];  /* floor, and ceil, of index of fresp in fft array */
   float stw[2];  /* weight of floor and ceil for index of fresp in fft array */
   THD_3dim_dataset *phz;
   THD_3dim_dataset *amp;
   int dof[2];    
   
   
} RP_UD;

#define ABS(a) ( (a) < 0 ? (-a):(a) )
#define SIGN(a) ( (a) < 0 ? -1:1 )
#define PHASE_R2D(a) ( (a)*180.0/PI )
#define PHASE_360(a) ( (a) < 0.0 ? 360.0+(a):(a) )
int Phase_Type_to_Dir(int p) {
   switch(p) {
      case k_CON:
         return(CONT);
      case k_EXP:
         return(EXP);
      case k_CW:
         return(CW);
      case k_CCW:
         return(CCW);
      default:
         return(NOT_SET);
   }
}

int Phase_Dir_to_Type(int p) {
   switch(p) {
      case CONT:
         return(k_CON);
      case EXP:
         return(k_EXP);
      case CW:
         return(k_CW);
      case CCW:
         return(k_CCW);
      default:
         return(-1);
   }
}
  
char * Phase_Dirs_string(int p) {
   static char ps[64]={"ERROR"};
   switch(p) {
      case CONT:
         sprintf(ps,"Contracting (Eccentricity -)");
         break;
      case CCW:
         sprintf(ps,"Counter clockwise (Polar -)");
         break;
      case CW:
         sprintf(ps,"Clockwise (Polar +)");
         break;
      case EXP:
         sprintf(ps,"Expanding (Eccentricity +)");
         break;
      case POS:
         sprintf(ps,"Positive + (Polar, or Ecc)");
         break;
      case NEG:
         sprintf(ps,"Negative - (Polar, or Ecc)");
         break;
      case NOT_SET:
         sprintf(ps,"Not_set");
         break;
      default:
         sprintf(ps,"Horreur");
         break;
   }
   return(ps);
}
char * Phase_Dirs_lbl(int p) {
   static char ps[64]={"ERROR"};
   switch(p) {
      case CONT:
         sprintf(ps,"ecc-");
         break;
      case CCW:
         sprintf(ps,"pol-");
         break;
      case CW:
         sprintf(ps,"pol+");
         break;
      case EXP:
         sprintf(ps,"ecc+");
         break;
      case POS:
         sprintf(ps,"+");
         break;
      case NEG:
         sprintf(ps,"-");
         break;
      case NOT_SET:
         sprintf(ps,".");
         break;
      default:
         sprintf(ps,"Horreur");
         break;
   }
   return(ps);
}
char * Phase_Dirs_ulbl(int p) {
   static char ps[64]={"ERROR"};
   switch(p) {
      case CONT:
      case EXP:
         sprintf(ps,"ecc");
         break;
      case CCW:
      case CW:
         sprintf(ps,"pol");
         break;
      default:
         sprintf(ps,"Horreur");
         break;
   }
   return(ps);
}

int Dir_is_eccentricity(d) {
   if (d == CONT || d == EXP) return(1);
   return(0);
}

int Dir_is_polar(d) {
   if (d == CW || d == CCW) return(1);
   return(0);
}


int Dir2Type(p) {
   switch (p){
      case CONT:
      case EXP:
         return(ECC);
         break;
      case CCW:
      case CW:
         return(POL);
         break;
      default:
         return(-1);
         break;
   }  
}

Show_RP_UD(RP_UD *u, char *str) {
   if (str) {
      fprintf(stderr,"%s", str);
   }
   fprintf(stderr,"     nfft=%d, fstep=%.3f\n"
                  "     nvals=%d\n"
                  "     stk=[%d,%d]\n"
                  "     stw=[%.3f, %.3f]\n"
                  "     ftap=%f\n"
                  "     dt=%f, pre = %f\n"
                  "     Eccentricity: fstim=%f, fresp=%f\n"
                  "     Polar: fstim=%f, fresp=%f\n"
                  "     dof=[%d %d]\n"
                  "     verb=%d, spectra=%d\n"
                  "     iset=%p\n"
                  "     vox=%p, nmask=%d\n"
                  "     phz=%p\n"
                  "     amp=%p\n"
                  "     prefix=%s, oext=%s\n"
                  "     %d wedges, %d rings\n"
                  "     fixsum=%d\n"
                  "     This call dir=%d (%s) \n"
                  ,
                  u->nfft, u->fstep, u->nvals, 
                  u->stk[0], u->stk[1],
                  u->stw[0], u->stw[1],
                  u->ftap, u->dt, u->pre,
                  u->Fstim[ECC], u->Fresp[ECC],
                  u->Fstim[POL], u->Fresp[POL], 
                  u->dof[0], u->dof[1],
                  u->verb, u->spectra,
                  u->iset, u->vox, u->nmask,
                  u->amp, u->phz,
                  u->prefix ? u->prefix:"NULL", 
                  u->oext ? u->oext:"NULL", 
                  u->n[POL], u->n[ECC], u->fixsum,
                  u->dir, Phase_Dirs_string(u->dir));
}

SetFreqBin(float fresp, float fstep, int stk[], float stw[]) {
   float stf = fresp/fstep;   /* stimulus freq. index */
   
   stk[0] = (int)floor(stf);    /* floor of freq. index */
      stw[0] = (1.0 - stf          + stk[0]);  /*floor weight*/
   stk[1] = (int)ceil (stf);    /* ceil of freq. index */
      stw[1] = (1.0 - stk[1] + stf  );        /*ceil weight*/
}


THD_3dim_dataset * Combine_Opposites(THD_3dim_dataset *dset1, 
                                     THD_3dim_dataset *dset2, 
                                     RP_UD *rpud)
{
   float *phi1=NULL, *phi2=NULL, *sum=NULL, *dif=NULL;    
   int nvox, i;
   THD_3dim_dataset *oset = NULL;
   char stmp[256+strlen(rpud->prefix)];
   double radpersec = 0.0, n=1.0;
    
   radpersec = (360.0*rpud->Fresp[Dir2Type(rpud->dir)]);
   
   n = (float)rpud->n[Dir2Type(rpud->dir)];
   
   nvox = DSET_NVOX(dset1);
   phi1 = (float *)calloc(nvox, sizeof(float));
   phi2 = (float *)calloc(nvox, sizeof(float));
   sum = (float *)calloc(nvox, sizeof(float));
   dif = (float *)calloc(nvox, sizeof(float));
   
   EDIT_coerce_scale_type( nvox , DSET_BRICK_FACTOR(dset1,0) ,
                              DSET_BRICK_TYPE(dset1,0), 
                              DSET_ARRAY(dset1, 0) ,      /* input  */
                              MRI_float, phi1  ) ;
   EDIT_coerce_scale_type( nvox , DSET_BRICK_FACTOR(dset2,0) ,
                              DSET_BRICK_TYPE(dset2,0), 
                              DSET_ARRAY(dset2, 0) ,      /* input  */
                              MRI_float, phi2  ) ;
   
   for (i=0; i<nvox;++i) {
      dif[i] = (phi1[i]-phi2[i])/2.0;
      if (ABS(dif[i]) < 90/n || !rpud->fixsum) { /* should this be 90 / n ? */ 
         sum[i] = (phi1[i]+phi2[i])/2.0;
      } else { /* Too big a difference in phase, likely 
               summing about zero degrees where wrapping from
               360 occurs*/
         sum[i] = (phi1[i]+phi2[i])/2.0-180.0/n; 
         if (sum[i]<0) sum[i] = 360/n + sum[i];
      }
      
      /* now change difference of phase from degrees to seconds for output */
      /* dif[i] /= radpersec; */
   }
   
   /* put the results in a dset for output */
   oset = EDIT_empty_copy( dset1 ) ;
   sprintf(stmp,"%s.%s.field%s",
                 rpud->prefix, Phase_Dirs_ulbl(rpud->dir),rpud->oext);
   EDIT_dset_items( oset ,
                   ADN_prefix , stmp,
                   ADN_datum_all, MRI_float ,
                   ADN_nvals  , 2 ,
                 ADN_none ) ;
                 
   EDIT_substitute_brick( oset , 0 , MRI_float  , sum ) ; /* do not free sum */
   EDIT_substitute_brick( oset , 1 , MRI_float  , dif ) ; /* do not free dif */
   if (Dir_is_eccentricity(rpud->dir)) {
      EDIT_BRICK_LABEL(oset , 0, "Eccentricity");
   } else if (Dir_is_polar(rpud->dir)) {
      EDIT_BRICK_LABEL(oset , 0, "Polar Angle");
   } else {
      ERROR_message("rpud->dir makes no sense here");
   }
   EDIT_BRICK_LABEL(oset , 1, "Hemo. Offset");
   
   free(phi1); phi1 = NULL;
   free(phi2); phi2 = NULL;
   
   return(oset);
}

static void RP_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          void *ud, int nbriks, float *val          )
{
   static int nvox , ncall , N_nzfreq=0, *stimharm=NULL;
   static byte *nzfreq=NULL;
   static float *xtap=NULL,  *mag=NULL, *phz=NULL, stf;
   static complex *comp_array=NULL;
   int ii , jj;
   double nzpow=0.0, preoff = 0.0;
   RP_UD *rpud = (RP_UD *)ud; 
   static int st = -1;
   /** is this a "notification"? **/

   if( val == NULL ){
      if (rpud->verb) {
         INFO_message("First call npts=%d\n", npts);
         Show_RP_UD(rpud, "Top of init:\n");
      }
      if( npts > 0 ){  /* the "start notification" */
         if (ncall != 0) {
            INFO_message("Repeat initialization, make sure cleanup was done\n"
                         "done well at end notification.\n");
         }
         ncall = 0 ;
         st = Dir2Type(rpud->dir);
         if (st < 0) {
            ERROR_message("Bad rpud->dir, assuming POLAR stimulus");
            st = POL;
         }
         /* nfft ? */
         if (rpud->nfft == 0) rpud->nfft = csfft_nextup(rpud->nvals);
         INFO_message("Data length = %d ; FFT length = %d",
                        rpud->nvals,rpud->nfft) ;

         if( rpud->ftap > 0.0f ) {
           xtap = mri_setup_taper( npts , rpud->ftap ) ; 
         }
         
         /* freq. step */
         rpud->fstep = 1.0f/(rpud->nfft*rpud->dt);
         
         /* response frequency */
         rpud->Fresp[st] = 
            rpud->Fstim[st]*(float)rpud->n[st];
         
         /* allocate for fft array */
         comp_array = (complex *) calloc( sizeof(complex) , rpud->nfft);
         mag        = (float *)   calloc( sizeof(float)   , rpud->nfft);
         phz        = (float *)   calloc( sizeof(float)   , rpud->nfft);
         nzfreq     = (byte *)    calloc( sizeof(byte)    , rpud->nfft);
         stimharm   = (int *)     calloc( sizeof(int)     , rpud->nfft);

         /* signal bin */
         SetFreqBin(rpud->Fresp[st], 
                     rpud->fstep, rpud->stk, rpud->stw);
         {
            int nharm = 1, stk[2];
            float fharm=0.0, stw[2];
            while ((fharm = nharm*rpud->Fresp[st]) < 
                     rpud->nfft/2.0*rpud->fstep) {
               SetFreqBin(fharm, rpud->fstep, stk, stw);
               stimharm[stk[0]] = nharm;
               stimharm[stk[1]] = nharm; 
               if (rpud->verb > 1) 
                  INFO_message("Freq. indices [%d(%.3f) %d(%.3f)] is harm %d\n", 
                              stk[0], stw[0], stk[1], stw[1], nharm);
               ++nharm;
            }
         }
         /* mark frequencies to be used for noise variance estimate */
         N_nzfreq=0;
         for( jj=0 ; jj < rpud->nfft/2 ; jj++ ) {
            if (  jj > rpud->stk[1]    /* above freq. */
               && !stimharm[jj] ) { /* not harmonic */
               nzfreq[jj] = 1; ++N_nzfreq;
               if (rpud->verb > 1) INFO_message("Freq. index %d is noise\n", jj);
            }
         }
         rpud->dof[0] = 2; /* assuming estimate of phase and amp at one freq.*/
         rpud->dof[1] = 2*N_nzfreq;   /* phase and amp from freq. of noise */

         /* init output sets */
         if (rpud->spectra) {
            for (ii=0; ii<2; ++ii) {
               THD_3dim_dataset *oset=NULL;
               char stmp[10+strlen(rpud->prefix)];

               if (ii) {
                  if (rpud->verb) INFO_message("Init. amp");
                  sprintf(stmp,"%s.%s.amp%s",
                        rpud->prefix, Phase_Dirs_lbl(rpud->dir),rpud->oext);
               } else {
                  if (rpud->verb) INFO_message("Init. phz");
                  sprintf(stmp,"%s.%s.phz%s",
                        rpud->prefix, Phase_Dirs_lbl(rpud->dir),rpud->oext);
               }
               oset = EDIT_empty_copy( rpud->iset ) ;
               EDIT_dset_items( oset ,
                               ADN_prefix , stmp,
                               ADN_datum_all, MRI_float ,
                               ADN_nvals  , rpud->nfft ,
                               ADN_ntt    , rpud->nfft ,
                             ADN_none ) ;

               DSET_UNMSEC(rpud->iset) ;
               if( DSET_TIMEUNITS(rpud->iset) == UNITS_SEC_TYPE ){
                  EDIT_dset_items( oset ,
                                 ADN_tunits , UNITS_HZ_TYPE ,
                                 ADN_ttdel  , rpud->fstep ,
                                 ADN_nsl    , 0 ,
                                 ADN_none ) ;
               } else {
                  WARNING_message("Units not seconds?");
               }
               for( jj=0 ; jj < rpud->nfft ; jj++ ) {
                  EDIT_substitute_brick( oset , jj , MRI_float  , NULL ) ;
               }
               if (ii) {
                  rpud->amp = oset; oset = NULL;
               } else {
                  rpud->phz = oset; oset = NULL;
               }
            }
         } else {
            rpud->amp = rpud->phz = NULL;
         }
         
         if (rpud->verb) {
            Show_RP_UD(rpud, "End of init:\n");
         }
      } else {  /* the "end notification" */
         if (rpud->verb) {
            INFO_message("Last call\n");
         }
         st = -1;
         if (xtap) free(xtap); xtap = NULL;
         if (comp_array) free(comp_array); comp_array = NULL;
         if (mag) free(mag); mag = NULL;
         if (phz) free(phz); mag = NULL;
         if (nzfreq) free(nzfreq); nzfreq = NULL;
      }
      return ;
   }
   if (rpud->verb > 3) {
            INFO_message("call %d\n", ncall);
   }
   /* Now do the FFT */
   for( jj=0 ; jj < rpud->nvals ; jj++ ) {
      comp_array[jj].r = ts[jj]; comp_array[jj].i = 0.0f ;
   }
   
   if( xtap != NULL ){                 
      for( jj=0 ; jj < rpud->nvals ; jj++ ){
         comp_array[jj].r *= xtap[jj] ; comp_array[jj].i *= xtap[jj] ;
      }
   }
   for( jj=rpud->nvals ; jj < rpud->nfft ; jj++ )
       comp_array[jj].r = comp_array[jj].i = 0.0f ;  /* zero pad */
   
   csfft_cox( -1 , rpud->nfft, comp_array ) ;   /* DFT */
   
   /* Calculate phase, magnitude, etc. */
   nzpow = 0.0;
   preoff = rpud->pre * rpud->Fresp[st] * 2.0 * PI; 
                                                   /* pre stim offset */
 
   for( jj=0 ; jj < rpud->nfft ; jj++ ) {
      mag[jj] = CABS(comp_array[jj]) ; 
      if (nzfreq[jj]) {
         nzpow += (mag[jj]*mag[jj]); 
      }
      phz[jj] = atan2(comp_array[jj].i, comp_array[jj].r);
      
      /* remove offset due to pre stimulus period */
      if (preoff >= 0) {
         phz[jj] -= preoff;
         if (phz[jj] < -PI) phz[jj] = 2.0*PI+phz[jj];
      }
      /* go from -pi ... pi to 0 ... 360 */
      phz[jj] = PHASE_R2D(phz[jj]); 
      phz[jj] = PHASE_360(phz[jj]);
      /* and change sign to reflect direction of stimulus */
      if (rpud->dir < 0) phz[jj] = 360.0 - phz[jj]; 
      /* Now divide by n so that phase is expressed in units of visual field */
      if (rpud->n[st] > 1) 
         phz[jj] /= (float)rpud->n[st]; 
      
      /* At this stage, phz[jj] is 
               theta+ if rpud->dir is +ve, 
           and theta- otherwise 
         (see labbook NIH-5, pp 103) */
   }    
   
   
   /* Store the output vals */
      /* take phase from the closest frequency with highest amplitude */
   if (mag[rpud->stk[0]] > mag[rpud->stk[1]]){ 
      val[0] =  phz[rpud->stk[0]];
   } else {
      val[0] =  phz[rpud->stk[1]];
   }
   /* linear interpolation between closest frequecies for amplitude */
   val[1] = (  mag[rpud->stk[0]]*rpud->stw[0] + 
               mag[rpud->stk[1]]*rpud->stw[1] );
   val[1] *= val[1]; /* square for power*/
   val[1] /= (nzpow/(double)N_nzfreq); /* normalize by avg power of noise */ 
   if (rpud->vox) { 
      jj=rpud->vox[ncall];
   } else {
      jj=ncall;
   } 
   if (rpud->spectra) {
      THD_insert_series(jj, rpud->amp, rpud->nfft, MRI_float, mag, 1);
      THD_insert_series(jj, rpud->phz, rpud->nfft, MRI_float, phz, 1);
   }    
   ncall++ ; return ;
}

byte *MaskSetup(THD_3dim_dataset *old_dset, THD_3dim_dataset *mask_dset, 
                RP_UD *rpud, byte *cmask, int *ncmask, 
                float mask_bot, float mask_top, int *mcount) 
{ 
   byte *mmm=NULL;
   int ii=0, kk=0;
   
   /* ------------- Mask business -----------------*/
   if( mask_dset == NULL ){
      mmm = NULL ;
      if( rpud->verb ) 
         INFO_message("%d voxels in the entire dataset (no mask)\n",
                     DSET_NVOX(old_dset)) ;
   } else {
      if( DSET_NVOX(mask_dset) != DSET_NVOX(old_dset) )
        ERROR_exit("Input and mask datasets are not same dimensions!\n");
      mmm = THD_makemask( mask_dset , 0 , mask_bot, mask_top ) ;
      *mcount = THD_countmask( DSET_NVOX(old_dset) , mmm ) ;
      if( *mcount <= 0 ) {
         ERROR_message("No voxels in the mask!\n") ;
         return(NULL);
      }
      if( rpud->verb ) INFO_message("%d voxels in the mask\n",mcount) ;
      DSET_delete(mask_dset) ;
   }

   if( cmask != NULL ){
      if( *ncmask != DSET_NVOX(old_dset) )
        ERROR_exit("Input and cmask datasets are not same dimensions!\n");
      if( mmm != NULL ){
         for( ii=0 ; ii < DSET_NVOX(old_dset) ; ii++ ) 
            mmm[ii] = (mmm[ii] && cmask[ii]) ;
         free(cmask) ;
         *mcount = THD_countmask( DSET_NVOX(old_dset) , mmm ) ;
         if( *mcount <= 0 ) {
            ERROR_message("No voxels in the mask+cmask!\n") ;
            return(NULL);
         }
         if( rpud->verb ) 
            INFO_message("%d voxels in the mask+cmask\n",*mcount) ;
      } else {
         mmm = cmask ;
         *mcount = THD_countmask( DSET_NVOX(old_dset) , mmm ) ;
         if( *mcount <= 0 ) {
            ERROR_message("No voxels in the cmask!\n") ;
            return(NULL);
         }
         if( rpud->verb ) INFO_message("%d voxels in the cmask\n",*mcount) ;
      }
   }

   if (mmm) {
      rpud->nmask=*mcount;
      rpud->vox = (int *)calloc(sizeof(int),*mcount) ;
      kk=0;
      for (ii=0; ii < DSET_NVOX(old_dset) ; ii++ ) {
         if (mmm[ii]) { rpud->vox[kk]=ii; ++kk; }
      } 
   }
   
   return(mmm);         
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset **new_dset=NULL, *old_dset=NULL, *mask_dset=NULL;
   char stmp[1024], **in_name=NULL;
   int iarg=1 , ii, jj, kk, ll, nvox, nvals=1, isfloat=0, nset=0, stype=0;
   int detrend=0, datum = MRI_float, mcount = 0, ncmask=0;
   byte *mmm=NULL, *cmask=NULL;
   float mask_bot=1.0 , mask_top=-1.0 ;
   RP_UD rpud; 
   
   in_name = (char **)calloc(sizeof(char*),k_N);
   new_dset = (THD_3dim_dataset **)calloc(sizeof(THD_3dim_dataset*),k_N);
   
   rpud.ftap = 0.0f; 
   rpud.dt = 0.0f;
   rpud.Fstim[ECC] = rpud.Fstim[POL] = 0.0f;
   rpud.Fresp[ECC] = rpud.Fresp[POL] = 0.0f;
   rpud.nfft = 0;
   rpud.verb = 1;
   rpud.iset = NULL;
   rpud.nmask=0;
   rpud.vox = NULL;
   rpud.amp = NULL;
   rpud.phz = NULL;
   rpud.prefix = "RetinoPhase";
   rpud.oext=NULL;
   rpud.dir = NOT_SET;
   rpud.n[ECC] = rpud.n[POL] = 1;
   rpud.spectra = 0;
   rpud.pre = 0.0f;
   rpud.fixsum = 1;
   
   /* rpud. = ; */
   
   
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dRetinoPhase [-prefix ppp]  dataset\n"
            "   where dataset is a time series from a retinotpy stimulus\n"
            "\n"
            " -exp EXP: These four options specify the type of retinotpy \n"
            " -con CON: stimulus. EXP and CON are for expanding and \n"
            " -cw  CW : contracting rings, respectively. CW and CCW are\n"
            " -ccw CCW: for clockwise and counter clockwise moving polar\n"
            "           polar angle mapping stimuli. You can specify one, \n"
            "           or all stimuli in one command. When all are specified\n"
            "           polar angle stimuli, and eccentricity stimuli of \n"
            "           opposite directions are combined.\n"
            " -prefix PREF: Prefix of output datasets. \n"
            "           PREF is suffixed with the following:\n"
            "           .ecc+ for positive (expanding) eccentricity (EXP)\n"
            "           .ecc- for negative (contracting) eccentricity (CON)\n"
            "           .pol+ for clockwise polar angle mapping (CW)\n"
            "           .pol- for counterclockwise polar angle mapping (CCW)\n"
            " -spectra: Output amplitude and phase spectra datasets.\n"
            " -Tstim T: Period of stimulus in seconds. This parameter does\n"
            "           not depend on the number of wedges or rings (Nr/Nw).\n"
            "           It is the duration of a full cycle of the stimulus.\n"
            "           Use -Tpol TPOL, and -Tecc TECC, to specify periods\n"
            "           for each stimulus type separately. -Tstim sets both \n"
            "           periods to T.\n"
            " -nrings Nr: Nr is the number of rings in the stimulus. \n"
            "              The default is 1.\n"
            " -nwedges Nw: Nw is the number of wedges in the stimulus. \n"
            "              The default is 1.\n"
            " -detrend: least-squares remove linear drift before DFT\n"
            "             [for more complex detrending, use 3dDetrend first]\n"
            " -pre_stim PRE: Blank period, in seconds, before stimulus began \n"
            " -sum_adjust y/n: Adjust sum of angles for wrapping based on the\n"
            "                  angle difference. Default is 'y'\n"
            "\n"
       /* options left over from 3dDFT.c     
            " -abs     == output float dataset = abs(DFT)\n"
            " -nfft N  == use 'N' for DFT length (must be >= #time points)\n"
            " -taper f == taper 'f' fraction of data at ends (0 <= f <= 1).\n"
            "             [Hamming 'raised cosine' taper of f/2 of the ]\n"
            "             [data length at each end; default is no taper]\n" 
          Bring them back after testing. */
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dRetinoPhase main"); machdep(); AFNI_logger("3dRetinoPhase",argc,argv);
   AUTHOR("ZIAD") ;
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*-- options --*/


   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-mask",5) == 0 ){
         if( mask_dset != NULL )
           ERROR_exit("Cannot have two -mask options!\n") ;
         if( iarg+1 >= argc )
           ERROR_exit("-mask option requires a following argument!\n");
         mask_dset = THD_open_dataset( argv[++iarg] ) ;
         if( mask_dset == NULL )
           ERROR_exit("Cannot open mask dataset!\n") ;
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
           ERROR_exit("Cannot deal with complex-valued mask dataset!\n");
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-mrange",5) == 0 ){
         if( iarg+2 >= argc )
           ERROR_exit("-mrange option requires 2 following arguments!\n");
         mask_bot = strtod( argv[++iarg] , NULL ) ;
         mask_top = strtod( argv[++iarg] , NULL ) ;
         if( mask_top < mask_top )
           ERROR_exit("-mrange inputs are illegal!\n") ;
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
         if( iarg+1 >= argc )
           ERROR_exit("-verb option requires 1 integer!\n");
         rpud.verb = (int)strtod( argv[++iarg] , NULL ) ;
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-cmask") == 0 ){  /* 16 Mar 2000 */
         if( iarg+1 >= argc )
            ERROR_exit("-cmask option requires a following argument!\n");
         cmask = EDT_calcmask( argv[++iarg] , &ncmask, 0 ) ;
         if( cmask == NULL ) ERROR_exit("Can't compute -cmask!\n");
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-taper") == 0 ){ 
         if( iarg+1 >= argc )
                  ERROR_exit("-taper option requires an argument!\n");       
         rpud.ftap = (float)strtod(argv[++iarg],NULL) ;
         if( rpud.ftap < 0.0f || rpud.ftap > 1.0f )
           ERROR_exit("Illegal value after -taper: %g",rpud.ftap) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-sum_adjust") == 0 ){ 
         if( iarg+1 >= argc )
                  ERROR_exit("-sum_adjust requires an argument\n");       
         ++iarg;
         if (argv[iarg][0] == 'y' || argv[iarg][0] == 'Y') {
            rpud.fixsum = 1;
         } else if (argv[iarg][0] == 'n' || argv[iarg][0] == 'N') {
            rpud.fixsum = 0;
         } else {
            ERROR_exit("Illegal value (%s) after -sum_adjust\n", argv[iarg]);
         }
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-fstim") == 0 ){  
        rpud.Fstim[ECC] = rpud.Fstim[POL] = (float)strtod(argv[++iarg],NULL) ;
        if( rpud.Fstim[ECC] <= 0.0f )
          ERROR_exit("Illegal frequency after -fstim: %g",rpud.Fstim[ECC]) ;
        iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-fecc") == 0 ){  
        rpud.Fstim[ECC] = (float)strtod(argv[++iarg],NULL) ;
        if( rpud.Fstim[ECC] <= 0.0f )
          ERROR_exit("Illegal frequency after -fecc: %g",rpud.Fstim[ECC]) ;
        iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-fpol") == 0 ){  
        rpud.Fstim[POL] = (float)strtod(argv[++iarg],NULL) ;
        if( rpud.Fstim[POL] <= 0.0f )
          ERROR_exit("Illegal frequency after -fpol: %g",rpud.Fstim[POL]) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-Tstim") == 0 ){  
        rpud.Fstim[ECC]= rpud.Fstim[POL]= 1.0/(float)strtod(argv[++iarg],NULL) ;
        if( rpud.Fstim[ECC] <= 0.0f )
          ERROR_exit("Illegal period after -Tstim: %g",1.0/rpud.Fstim[ECC]) ;
        iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-Tecc") == 0 ){  
        rpud.Fstim[ECC]= 1.0/(float)strtod(argv[++iarg],NULL) ;
        if( rpud.Fstim[ECC] <= 0.0f )
          ERROR_exit("Illegal period after -Tecc: %g",1.0/rpud.Fstim[ECC]) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-Tpol") == 0 ){  
        rpud.Fstim[POL]= 1.0/(float)strtod(argv[++iarg],NULL) ;
        if( rpud.Fstim[POL] <= 0.0f )
          ERROR_exit("Illegal period after -Tpol: %g",1.0/rpud.Fstim[POL]) ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-pre_stim", 5) == 0 ){  
        if( iarg+1 >= argc )
                  ERROR_exit("-pre_stim requires an argument\n");   
                      
        rpud.pre = (float)strtod(argv[++iarg],NULL) ;
        if( rpud.pre <= 0.0f )
          ERROR_exit("Illegal value after -pre_stim: %g",rpud.pre) ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-nw",3) == 0 ){  
         if( iarg+1 >= argc )
                  ERROR_exit("-nwedges  requires an integer\n");       
        rpud.n[POL] = (int)strtod(argv[++iarg],NULL) ;
        if( rpud.n[POL] <1 || rpud.n[POL] > 10 )
          ERROR_exit("Bad value of %d for -nwedges. "
                     "Should be a small positive integer.",rpud.n[POL]) ;
        iarg++ ; continue ;
      }
      if( strncmp(argv[iarg],"-nr",3) == 0 ){  
         if( iarg+1 >= argc )
                  ERROR_exit("-nrings  requires an integer\n");       
        rpud.n[ECC] = (int)strtod(argv[++iarg],NULL) ;
        if( rpud.n[ECC] <1 || rpud.n[ECC] > 10 )
          ERROR_exit("Bad value of %d for -nrings. "
                     "Should be a small positive integer.",rpud.n[ECC]) ;
        iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-dir") == 0 ){  
         ERROR_exit("-dir now obsolete, use -exp, -con, -cw, or -ccw\n");
        if( iarg+1 >= argc )
            ERROR_exit("-dir option requires an argument!\n");
        ++iarg;
        if      (!strncmp(argv[iarg],"exp", 3)) rpud.dir = EXP;
        else if (!strncmp(argv[iarg],"con", 3) ||
                 !strncmp(argv[iarg],"cnt", 3)) rpud.dir = CONT;
        else if (!strncmp(argv[iarg],"cw",  2) ||
                 !strncmp(argv[iarg],"clo", 3)) rpud.dir = CW;
        else if (!strncmp(argv[iarg],"ccw", 3) ||
                 !strncmp(argv[iarg],"cou", 3)) rpud.dir = CCW;
        else if (!strncmp(argv[iarg],"+",   1) ||
                 !strncmp(argv[iarg],"pos", 3)) rpud.dir = POS;
        else if (!strncmp(argv[iarg],"-",   1) ||
                 !strncmp(argv[iarg],"neg", 3)) rpud.dir = NEG;
        else {
         ERROR_exit("Illegal value %s after -dir\n", argv[iarg]);
        }
        iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-input") == 0 ){
         ERROR_exit("-input now obsolete, use -exp, -con, -cw, or -ccw\n");
         
         if( iarg+1 >= argc )
            ERROR_exit("-input option requires a  dset\n");
         in_name[0] = argv[++iarg] ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-exp") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-exp option requires a  dset\n");
         in_name[k_EXP] = argv[++iarg] ;
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-con") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-con option requires a  dset\n");
         in_name[k_CON] = argv[++iarg] ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-cw") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-cw option requires a  dset\n");
         in_name[k_CW] = argv[++iarg] ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ccw") == 0 ){
         if( iarg+1 >= argc )
            ERROR_exit("-ccw option requires a  dset\n");
         in_name[k_CCW] = argv[++iarg] ;
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-prefix") == 0 ){
         rpud.prefix = argv[++iarg] ;
         if( !THD_filename_ok(rpud.prefix) )
           ERROR_exit("-prefix %s is illegal!",rpud.prefix) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-detrend") == 0 ){
        detrend = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-spectra") == 0 ){
        rpud.spectra = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nfft") == 0 ){
        rpud.nfft = (int)strtod(argv[++iarg],NULL) ;
        if( rpud.nfft <= 2 ){
          WARNING_message("Illegal -nfft value on command line") ;
          rpud.nfft = 0 ;
        } else {
          ii = csfft_nextup(rpud.nfft) ;
          if( ii > rpud.nfft ){
            WARNING_message("Replacing -nfft=%d with next largest" 
                            "legal value=%d",
                            rpud.nfft,ii) ;
            rpud.nfft = ii ;
          }
        }
        iarg++ ; continue ;
      }

      ERROR_exit("ILLEGAL option: %s\n",argv[iarg]) ;
   }

   if( !in_name )
     ERROR_exit("No datasets on command line!?") ;

   nset = 0;
   for (stype=0; stype<k_N; ++stype) {
      if (in_name[stype]) { 
         old_dset = THD_open_dataset( in_name[stype] ) ;
         if( !ISVALID_DSET(old_dset) )
           ERROR_exit("Can't open dataset %s\n",in_name[stype]);


         if( DSET_NVALS(old_dset) < 2 )
           ERROR_exit("Can't use dataset with < 2 values per voxel!\n") ;

         if( DSET_NUM_TIMES(old_dset) < 2 ){
           WARNING_message("Input dataset is not 3D+time; assuming TR=1.0") ;
           EDIT_dset_items( old_dset ,
                              ADN_ntt    , DSET_NVALS(old_dset) ,
                              ADN_ttorg  , 0.0 ,
                              ADN_ttdel  , 1.0 ,
                              ADN_tunits , UNITS_SEC_TYPE ,
                            NULL ) ;
         }

         if (!nset) {
            rpud.nvals = DSET_NUM_TIMES(old_dset);
            rpud.dt = old_dset->taxis->ttdel;
            mmm = MaskSetup(old_dset, mask_dset, &rpud, cmask, 
                           &ncmask, mask_bot, mask_top, &mcount);
         }
         rpud.iset = old_dset;
         rpud.dir = Phase_Type_to_Dir(stype);
         
         /*------------- ready to compute new dataset -----------*/
         if (rpud.verb) 
            INFO_message("Going to meet maker for %s\n", in_name[stype]);
         
         
         rpud.oext = "";
         if (STRING_HAS_SUFFIX(rpud.prefix, ".niml.dset")) {
            rpud.oext = ".niml.dset";
            rpud.prefix[strlen(rpud.prefix)-strlen(rpud.oext)] = '\0';
         } else if (STRING_HAS_SUFFIX(rpud.prefix, ".1D")) {
            rpud.oext = ".1D";
            rpud.prefix[strlen(rpud.prefix)-strlen(rpud.oext)] = '\0';
         } else if (STRING_HAS_SUFFIX(rpud.prefix, ".1D.dset")) {
            rpud.oext = ".1D.dset";
            rpud.prefix[strlen(rpud.prefix)-strlen(rpud.oext)] = '\0';
         } else if (STRING_HAS_SUFFIX(in_name[stype], ".niml.dset")) {
            rpud.oext = ".niml.dset";
         } else if (STRING_HAS_SUFFIX(in_name[stype], ".1D") || 
                    STRING_HAS_SUFFIX(in_name[stype], ".1D.dset")) {
            rpud.oext = ".1D.dset";
         }
         sprintf(stmp,"%s.%s%s",
                     rpud.prefix, Phase_Dirs_lbl(rpud.dir),rpud.oext);

         new_dset[stype] = MAKER_4D_to_typed_fbuc(
                       old_dset ,             /* input dataset */
                       stmp ,               /* output prefix */
                       datum ,                /* output datum  */
                       0 ,                    /* ignore count  */
                       1 ,                   /* linear detrend */
                       2 ,                   /* number of briks */
                       RP_tsfunc ,         /* timeseries processor */
                       &rpud,                  /* data for tsfunc */
                       mmm
                    ) ;

         if (rpud.verb) INFO_message("Output time\n");
         if( new_dset[stype] != NULL ){
            tross_Copy_History( old_dset , new_dset[stype] ) ;
            tross_Make_History("3dRetinoPhase" , argc, argv , new_dset[stype]) ;
            sprintf(stmp,"Phz@%.3fHz", rpud.Fresp[Dir2Type(rpud.dir)]);
            EDIT_BRICK_LABEL(new_dset[stype], 0, stmp);
            sprintf(stmp,"PwR@%.3fHz", rpud.Fresp[Dir2Type(rpud.dir)]);
            EDIT_BRICK_LABEL(new_dset[stype], 1, stmp);
            EDIT_BRICK_TO_FIFT(new_dset[stype],1,rpud.dof[0],rpud.dof[1]);

            DSET_write( new_dset[stype] ) ;
            WROTE_DSET( new_dset[stype] ) ;
         } else {
            ERROR_exit("Unable to compute output dataset!\n") ;
         }

         if (rpud.amp) {
            tross_Copy_History( old_dset , rpud.amp ) ;
            tross_Make_History( "3dRetinoPhase" , argc, argv , rpud.amp ) ;
            for (ii=0; ii<rpud.nfft; ++ii) {
               sprintf(stmp,"Amp@%.3fHz", ii*rpud.fstep);
               EDIT_BRICK_LABEL(rpud.amp, ii, stmp);
            }
            DSET_write( rpud.amp ) ;
            WROTE_DSET( rpud.amp ) ;
            DSET_delete(rpud.amp); rpud.amp = NULL;
         } else if (rpud.spectra) {
            ERROR_exit("Unable to compute output dataset!\n") ;
         }
         if (rpud.phz) {
            tross_Copy_History( old_dset , rpud.phz ) ;
            tross_Make_History( "3dRetinoPhase" , argc, argv , rpud.phz ) ;
            for (ii=0; ii<rpud.nfft; ++ii) {
               sprintf(stmp,"Phz@%.3fHz", ii*rpud.fstep);
               EDIT_BRICK_LABEL(rpud.phz, ii, stmp);
            }
            DSET_write( rpud.phz ) ;
            WROTE_DSET( rpud.phz ) ;
            DSET_delete(rpud.phz); rpud.phz = NULL;
         } else if (rpud.spectra) {
            ERROR_exit("Unable to compute output dataset!\n") ;
         }
         
         DSET_delete(old_dset); old_dset = NULL;
         ++nset;
      } /* if stimulus type given */
   } /* for each stimulus type */

   /* Now see if you can do combos */
   if (new_dset[k_EXP] && new_dset[k_CON]) {
      THD_3dim_dataset *cset=NULL;
      
      rpud.dir = CONT; /* not really, but sign does not matter at this point */
      cset = Combine_Opposites(new_dset[k_EXP], new_dset[k_CON], 
                        &rpud);
      tross_Copy_History( cset , new_dset[k_EXP] ) ;
      DSET_write( cset ) ;
      WROTE_DSET( cset ) ;
      DSET_delete(cset); cset = NULL;                  
   }
   
   if (new_dset[k_CW] && new_dset[k_CCW]) {
      THD_3dim_dataset *cset=NULL;
      
      rpud.dir = CW; /* not really, but sign does not matter at this point */
      cset = Combine_Opposites(new_dset[k_CW], new_dset[k_CCW], 
                        &rpud);
      tross_Copy_History( cset , new_dset[k_CW] ) ;
      DSET_write( cset ) ;
      WROTE_DSET( cset ) ;
      DSET_delete(cset); cset = NULL;                  
   }
   
   
   for (stype=0; stype<k_N; ++stype) 
      if (new_dset[stype]) DSET_delete(new_dset[stype]);
         
   free(in_name);
   free(new_dset);

   exit(0) ;
}
