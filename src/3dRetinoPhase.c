#include "mrilib.h"
#include "matrix.h"
#include "plug_delay_V2.h"

static void RP_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[], 
                          double ts_mean , double ts_slope ,
                          void *ud, int nbriks, float *val  );

static void DEL_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean , double ts_slope ,
                          void *ud, int nbriks, float *val  );

typedef enum {  FFT_PHASE = 1, HILB_DELAY = 2 } PHASE_METHODS;

typedef enum { 
   NEG=-3, CONT=-2, CCW=-1, 
   NOT_SET=0, 
   CLW=1, EXP=2, POS=3
   } PHAZE_DIRS;
typedef enum { 
   k_EXP=0, k_CON, k_CLW, k_CCW, k_N } STIM_TYPES;
typedef enum { 
   ECC=0, POL=1 } FIELD_PARAMS;
   
typedef struct {
   int pmeth; /* 1 --> phase, 2 -->delay */
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
   int dir;       /* +1 for CLW, -1 for CCW
                           +2 for Exp. -2 for contracting*/
   int n[2];           /* number of rings, wedges */
   int spectra;
   int fixsum;
   int ort_adj; /* number of orts removed from data */
   
   /* to be set upon first calling of processor function */
   int nfft;      /* number of points in fft */
   float fstep;   /* Delta freq. Hz*/
   float Fresp[2];   /* FMRI response fundamental freq. in Hz */ 
   int   stk[2];  /* floor, and ceil, of index of fresp in fft array */
   float stw[2];  /* weight of floor and ceil for index of fresp in fft array */
   THD_3dim_dataset *phz;
   THD_3dim_dataset *amp;
   int dof[2];    
   
   /* ops specific to hilbert phase estimation */
   float **rvec;  /* reference time series */
   int rvec_len; /* number of time points in rvec */
   int rvec_num; /* number of rvecs */
   int iref; /* which rvec are we dealing with? */
   int Dsamp;    /* correct slice timing offset */
   
} RP_UD;

#define PHASE_R2D(a) ( (a)*180.0/PI )
#define PHASE_360(a) ( (a) < 0.0 ? 360.0+(a):(a) )
int Phase_Type_to_Dir(int p) {
   switch(p) {
      case k_CON:
         return(CONT);
      case k_EXP:
         return(EXP);
      case k_CLW:
         return(CLW);
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
      case CLW:
         return(k_CLW);
      case CCW:
         return(k_CCW);
      default:
         return(-1);
   }
}

char * Phase_Method_string(int p) {
   static char ps[64]={"ERROR"};
   switch(p) {
      case FFT_PHASE:
         sprintf(ps,"FFT-Phase");
         break;
      case HILB_DELAY:
         sprintf(ps,"Hilbert-delay");
         break;
      default:
         sprintf(ps,"Calamity");
         break;
   }
   return(ps);
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
      case CLW:
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
      case CLW:
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
      case CLW:
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
   if (d == CLW || d == CCW) return(1);
   return(0);
}


int Dir2Type(p) {
   switch (p){
      case CONT:
      case EXP:
         return(ECC);
         break;
      case CCW:
      case CLW:
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
   fprintf(stderr,"Phase Method %d, %s\n"
                  "     nfft=%d, fstep=%.3f\n"
                  "     nvals=%d\n"
                  "     stk=[%d,%d]\n"
                  "     stw=[%.3f, %.3f]\n"
                  "     ftap=%f\n"
                  "     dt=%f, pre = %f\n"
                  "     Eccentricity: fstim=%f, fresp=%f\n"
                  "     Polar: fstim=%f, fresp=%f\n"
                  "     dof=[%d %d], ort_adj=%d\n"
                  "     verb=%d, spectra=%d\n"
                  "     iset=%p\n"
                  "     vox=%p, nmask=%d\n"
                  "     phz=%p\n"
                  "     amp=%p\n"
                  "     prefix=%s, oext=%s\n"
                  "     %d wedges, %d rings\n"
                  "     fixsum=%d\n"
                  "     This call dir=%d (%s) \n"
                  "     Dsamp = %d, rvec=%p, %d vals, %d refs, iref = %d \n"
                  ,
                  u->pmeth, Phase_Method_string(u->pmeth),
                  u->nfft, u->fstep, u->nvals, 
                  u->stk[0], u->stk[1],
                  u->stw[0], u->stw[1],
                  u->ftap, u->dt, u->pre,
                  u->Fstim[ECC], u->Fresp[ECC],
                  u->Fstim[POL], u->Fresp[POL], 
                  u->dof[0], u->dof[1], u->ort_adj,
                  u->verb, u->spectra,
                  u->iset, u->vox, u->nmask,
                  u->amp, u->phz,
                  u->prefix ? u->prefix:"NULL", 
                  u->oext ? u->oext:"NULL", 
                  u->n[POL], u->n[ECC], u->fixsum,
                  u->dir, Phase_Dirs_string(u->dir),
                  u->Dsamp, u->rvec, u->rvec_len, u->rvec_num, u->iref);
}

int SetFreqBin(float fresp, float fstep, int stk[], float stw[], int nfft) {
   float stf = fresp/fstep;   /* stimulus freq. index */
   
   stk[0] = (int)floor(stf);    /* floor of freq. index */
      stw[0] = (1.0 - stf          + stk[0]);  /*floor weight*/
   stk[1] = (int)ceil (stf);    /* ceil of freq. index */
      stw[1] = (1.0 - stk[1] + stf  );        /*ceil weight*/
   if (stk[0] > nfft/2 || stk[1] > nfft/2) {
      ERROR_message("Frequency indices %d and/or %d outside max of %d\n", 
                    stk[0], stk[1], nfft/2);
      return(0);
   }
   return(1);
}


THD_3dim_dataset * Combine_Opposites(THD_3dim_dataset *dset1, 
                                     THD_3dim_dataset *dset2, 
                                     RP_UD *rpud)
{
   float *phi1=NULL, *phi2=NULL, *xc1=NULL, *xc2=NULL, *mxxc=NULL,
         *sum=NULL, *dif=NULL;    
   int nvox, i;
   THD_3dim_dataset *oset = NULL;
   char stmp[256+strlen(rpud->prefix)];
   double radpersec = 0.0, n=1.0;
    
   radpersec = (360.0*rpud->Fresp[Dir2Type(rpud->dir)]);
   
   n = (float)rpud->n[Dir2Type(rpud->dir)];
   
   nvox = DSET_NVOX(dset1);
   phi1 = (float *)calloc(nvox, sizeof(float));
   xc1 = (float *)calloc(nvox, sizeof(float));
   phi2 = (float *)calloc(nvox, sizeof(float));
   xc2 = (float *)calloc(nvox, sizeof(float));
   sum = (float *)calloc(nvox, sizeof(float));
   dif = (float *)calloc(nvox, sizeof(float));
   mxxc = (float *)calloc(nvox, sizeof(float));
   
   EDIT_coerce_scale_type( nvox , DSET_BRICK_FACTOR(dset1,0) ,
                              DSET_BRICK_TYPE(dset1,0), 
                              DSET_ARRAY(dset1, 0) ,      /* input  */
                              MRI_float, phi1  ) ;
   EDIT_coerce_scale_type( nvox , DSET_BRICK_FACTOR(dset1,1) ,
                              DSET_BRICK_TYPE(dset1,1), 
                              DSET_ARRAY(dset1, 1) ,      /* input  */
                              MRI_float, xc1  ) ;
   EDIT_coerce_scale_type( nvox , DSET_BRICK_FACTOR(dset2,0) ,
                              DSET_BRICK_TYPE(dset2,0), 
                              DSET_ARRAY(dset2, 0) ,      /* input  */
                              MRI_float, phi2  ) ;
   EDIT_coerce_scale_type( nvox , DSET_BRICK_FACTOR(dset2,1) ,
                              DSET_BRICK_TYPE(dset2,1), 
                              DSET_ARRAY(dset2, 1) ,      /* input  */
                              MRI_float, xc2  ) ;
   
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
      /* keep track of max coef == union masking */
      if (xc2[i] > xc1[i]) mxxc[i] = xc2[i]; else mxxc[i] = xc1[i];
   }
   
   /* put the results in a dset for output */
   oset = EDIT_empty_copy( dset1 ) ;
   sprintf(stmp,"%s.%s.field%s",
                 rpud->prefix, Phase_Dirs_ulbl(rpud->dir),rpud->oext);
   EDIT_dset_items( oset ,
                   ADN_prefix , stmp,
                   ADN_datum_all, MRI_float ,
                   ADN_nvals  , 3 ,
                 ADN_none ) ;
                 
   EDIT_substitute_brick( oset , 0 , MRI_float  , sum ) ; /* do not free sum */
   EDIT_substitute_brick( oset , 1 , MRI_float  , dif ) ; /* do not free dif */
   EDIT_substitute_brick( oset , 2 , MRI_float  , mxxc ) ;/* do not free mxxc */
   if (Dir_is_eccentricity(rpud->dir)) {
      EDIT_BRICK_LABEL(oset , 0, "Eccentricity");
   } else if (Dir_is_polar(rpud->dir)) {
      EDIT_BRICK_LABEL(oset , 0, "Polar Angle");
   } else {
      ERROR_message("rpud->dir makes no sense here");
   }
   EDIT_BRICK_LABEL(oset , 1, "Hemo. Offset");
   if (rpud->pmeth == FFT_PHASE) {
      sprintf(stmp,"Max.PwR@%.3fHz", rpud->Fresp[Dir2Type(rpud->dir)]);
      EDIT_BRICK_LABEL(oset , 2, stmp); 
   } else {
      EDIT_BRICK_LABEL(oset , 2, "Max.Corr.Coef.");
      /* Could assume dset1 and dset2 have the same length series, and dofs 
         and do:
         EDIT_BRICK_TO_FICO(dset1, 2, rpud->rvec_len, 
                                  rpud->dof[0],rpud->dof[1]);
         But it is safer to deprive users of significance levels which
         don't quite apply with the max operation */
   }                               
   free(phi1); phi1 = NULL;
   free(phi2); phi2 = NULL;
   free(xc1); xc1 = NULL;
   free(xc2); xc2 = NULL;
   
   return(oset);
}

static void RP_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean , double ts_slope ,
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
   
   if (!rpud) {
      ERROR_exit("NULL rpud!!!\n"
                 "rpud %p, ud %p\n", rpud, ud);
   }
   if( val == NULL ){
      if (rpud->verb > 1) {
         INFO_message("First call npts=%d\n", npts);
         Show_RP_UD(rpud, "Top of init:\n");
      }
      if (rpud->pmeth != FFT_PHASE) {
         ERROR_message("This should not happen");
      }
      if( npts > 0 ){  /* the "start notification" */
         ncall = 0 ;
         st = Dir2Type(rpud->dir);
         if (st < 0) {
            ERROR_message("Bad rpud->dir, assuming POLAR stimulus");
            st = POL;
         }
         if (rpud->Dsamp) {
            WARNING_message(
               "Does not deal with slice timing offset correctly. Needs fixing");
         }
         /* nfft ? */
         if (rpud->nfft == 0) rpud->nfft = csfft_nextup(rpud->nvals);
         if (rpud->verb > 1) {
            INFO_message(
               "Data length = %d ; FFT length = %d; st %d, direc %d (%s)",
                        rpud->nvals,rpud->nfft, 
                        st, rpud->dir, Phase_Dirs_lbl(rpud->dir)) ;
         }

         if( rpud->ftap > 0.0f ) {
           xtap = mri_setup_taper( rpud->nvals , rpud->ftap ) ; 
         }
         
         /* freq. step */
         rpud->fstep = 1.0f/(rpud->nfft*rpud->dt);
         
         /* response frequency */
         rpud->Fresp[st] = 
            rpud->Fstim[st]*(float)rpud->n[st];
         
         if (rpud->Fresp[st] <= 0.0f) {
            ERROR_message("Bad rpud->Fresp !");
            return;
         }
         
         /* allocate for fft array */
         comp_array = (complex *) calloc( sizeof(complex) , rpud->nfft);
         mag        = (float *)   calloc( sizeof(float)   , rpud->nfft);
         phz        = (float *)   calloc( sizeof(float)   , rpud->nfft);
         nzfreq     = (byte *)    calloc( sizeof(byte)    , rpud->nfft);
         stimharm   = (int *)     calloc( sizeof(int)     , rpud->nfft);

         /* signal bin */
         if (!SetFreqBin(rpud->Fresp[st], 
                    rpud->fstep, rpud->stk, rpud->stw,
                    rpud->nfft)) {
            ERROR_message("Failed to set bins. Fresp=%f, "
                          "fstep=%f, stk=[%d,%d], stw=[%f,%f], nfft=%d\n"
                          "Is your dataset's TR of %f sec valid?", 
                          rpud->Fresp[st], rpud->fstep, 
                          rpud->stk[0], rpud->stk[1], 
                          rpud->stw[0], rpud->stw[1],rpud->nfft,
                          rpud->dt);
            return;          
         }
         {
            int nharm = 1, stk[2];
            float fharm=0.0, stw[2];
            while (( (fharm = nharm*rpud->Fresp[st]) - 
                     rpud->nfft/2.0*rpud->fstep ) < -0.00001) {
                        /* Difference was added to get around round off errors
                        which might get you a frequency at the precipice as 
                        in the bug reported by Phil Burton     Nov. 2014 */ 
               if (!SetFreqBin(fharm, rpud->fstep, stk, stw, rpud->nfft)) {
                  ERROR_message("Failed to set bins. Fresp=%f, fdiff=%f "
                          "fstep=%f, stk=[%d,%d], stw=[%f,%f], nfft=%d\n"
                          "Is your dataset's TR of %f sec valid?", 
                          rpud->Fresp[st], fharm-rpud->nfft/2.0*rpud->fstep,
                          rpud->fstep, 
                          rpud->stk[0], rpud->stk[1], 
                          rpud->stw[0], rpud->stw[1],rpud->nfft,rpud->dt);
                  break;   
               }
               stimharm[stk[0]] = nharm;
               stimharm[stk[1]] = nharm; 
               if (rpud->verb > 2) 
                  INFO_message("Freq. indices [%d(%.3f) %d(%.3f)] is harm %d"
                               " (fharm=%f. lim=%f)\n", 
                              stk[0], stw[0], stk[1], stw[1], nharm,
                              fharm, rpud->nfft/2.0*rpud->fstep);
               ++nharm;
            }
         }
         /* mark frequencies to be used for noise variance estimate */
         N_nzfreq=0;
         for( jj=0 ; jj < rpud->nfft/2 ; jj++ ) {
            if (  jj > rpud->stk[1]    /* above freq. */
               && !stimharm[jj] ) { /* not harmonic */
               nzfreq[jj] = 1; ++N_nzfreq;
               if (rpud->verb > 2) INFO_message("Freq. index %d is noise\n", jj);
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
                  if (rpud->verb > 1) INFO_message("Init. amp");
                  sprintf(stmp,"%s.%s.amp%s",
                        rpud->prefix, Phase_Dirs_lbl(rpud->dir),rpud->oext);
               } else {
                  if (rpud->verb > 1) INFO_message("Init. phz");
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
         
         if (rpud->verb >1) {
            Show_RP_UD(rpud, "@ End of init:\n");
         }
      } else {  /* the "end notification" */
         if (rpud->verb > 1) {
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

static void DEL_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean , double ts_slope ,
                          void *ud, int nbriks, float *val          )
{
   RP_UD *rpud = (RP_UD *)ud; 
   static int  ncall = 0, reverse=0;
   static float scale=0.0;
   char * label=NULL;            /* string containing stat. summary of results */
   int   errcode=0, ixyz=0;
   float slp=0.0, delu=0.0, del=0.0,  xcor=0.0, xcorCoef=0.0,vts=0.0,
         vrvec=0.0, dtx=0.0;
   static int st = -1;
	
   if (!rpud) {
      ERROR_exit("NULL rpud!!!\n"
                 "rpud %p, ud %p\n", rpud, ud);
   }
   
   /* Now intialize */
   if( val == NULL ){
      if (rpud->verb > 1) {
         INFO_message("First call npts=%d\n", npts);
         Show_RP_UD(rpud, "Top of init:\n");
      }
      if (rpud->pmeth != HILB_DELAY) {
         ERROR_message("This should not happen");
      }

      if( npts > 0 ){  /* the "start notification" */
         ncall = 0 ;
         st = Dir2Type(rpud->dir);
         if (st < 0) {
            ERROR_message("Bad rpud->dir, assuming POLAR stimulus");
            st = POL;
         }
         
         if (rpud->nfft != 0) { /* should allow for this and taper, in future */
            ERROR_message("Control of nfft not allowed for hilbert delay");
            return;
         }
         if (rpud->verb > 1) {
            INFO_message("Data length = %d (npts=%d);  st %d, direc %d (%s)",
                        rpud->nvals, npts, 
                        st, rpud->dir, Phase_Dirs_lbl(rpud->dir)) ;
         }
         /* response frequency */
         rpud->Fresp[st] = 
            rpud->Fstim[st]*(float)rpud->n[st];
         
         if (rpud->Fresp[st] <= 0.0f) {
            ERROR_message("Bad rpud->Fresp !");
            return;
         }
         if (!rpud->rvec || rpud->rvec_num < 1) {
            ERROR_message("No reference time series");
            return;
         }
         if (rpud->rvec_len != rpud->nvals) {
            ERROR_message( "Reference time series has %d vals, "
                           "time series has %d", rpud->rvec_len, rpud->nvals);
            return;
         }
         if (rpud->Dsamp) {
            WARNING_message(
               "Does not deal with slice timing offset correctly. Needs fixing");
         }
         rpud->dof[0] = 2; /* two fit params*/
         rpud->dof[1] = rpud->ort_adj;   /* number of orts */
         if (rpud->dof[1] < 2) rpud->dof[1] = 2; /* always linear detrend...*/

         if (rpud->dir == CLW || rpud->dir == EXP) reverse = 1;
         else reverse = 0;
         
         /* initialize hilbert */
         set_delay_verb(rpud->verb);
         hilbertdelay_V2reset();
         
         if (rpud->verb > 1) {
            Show_RP_UD(rpud, "@ End of init:\n");
         }
      } else {  /* the "end notification" */
         if (rpud->verb > 1) {
            INFO_message("Last call\n");
         }
         st = -1;
      }
      return ;
   }
   
   if (rpud->verb > 3) {
            INFO_message("call %d\n", ncall);
   }
   
   /* get slice offset time 
      WARNING: NOT ALLOWING for offset from time series cropping*/   		
   if (rpud->Dsamp) {
   	dtx = (float) (tzero / tdelta);
   } else {
   	dtx = 0.0;
	}
   
   if (rpud->vox) { 
      ixyz=rpud->vox[ncall];
   } else {
      ixyz=ncall;
   }
   
   errcode = 
      hilbertdelay_V2 (ts, /* voxel time series */
                       rpud->rvec[rpud->iref], /* reference ts*/
                       npts, /* length of time series */
                       1, 0, /* Num. of segments and percent overlap */
                       1 ,0, /* not cleanup mode, no detrend */   
                       dtx, /* timing offset */
                       1, /* remove bias */
                       &delu,&slp,&xcor,&xcorCoef,&vts,&vrvec);	
		
   if (errcode == 0) { /* If there are no errors, proceed */
      hunwrap (delu, 1.0/rpud->dt, 1.0/rpud->Fresp[st], slp, 
               0, METH_DEGREES, reverse, 
               1.0/rpud->n[st], &del );

   } else if (errcode == ERROR_LONGDELAY) {					
		if (0) {
         WARNING_message("Errcode LONGDELAY at voxel %d\n", ixyz);
      }	

		del = 0.0;		/* Set all the variables to Null and don't set xcorCoef 
                           to an impossible value*/
   	xcorCoef = 0.0;/*  because the data might still be OK */
   	xcor = 0.0;

	} else if (errcode == ERROR_QUIT) {
      exit(1);
   } else {
		if (0) {
         WARNING_message("Errcode %d at voxel %d\n", errcode, ixyz);
      }	

		del = 0.0; /* Set all the variables to Null and set xcorCoef 
                     to an impossible value*/
   	xcorCoef = NOWAYXCORCOEF;						
   	xcor = 0.0;
	}	
	
	
   /*----- Save results for this voxel -----*/
   val[0] = del;
	val[1] = xcorCoef;
 
   

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
      if( rpud->verb ) INFO_message("%d voxels in the mask dset %s\n",
                                 *mcount, DSET_PREFIX(mask_dset)) ;
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
   THD_3dim_dataset **new_dset=NULL, *old_dset=NULL, 
                     *mask_dset=NULL, *idset=NULL;
   char stmp[1024], **in_name=NULL;
   int iarg=1 , ii, jj, kk, ll, nvox, nvals=1, isfloat=0, nset=0, stype=0;
   int datum = MRI_float, mcount = 0, ncmask=0, dtused = 0;
   byte *mmm=NULL, *cmask=NULL;
   float mask_bot=1.0 , mask_top=-1.0, 
         *x0=NULL, *x1=NULL, *d0=NULL, *d1=NULL, *best=NULL ;
   RP_UD rpud; 
   
   in_name = (char **)calloc(sizeof(char*),k_N);
   new_dset = (THD_3dim_dataset **)calloc(sizeof(THD_3dim_dataset*),k_N);
   
   rpud.pmeth = FFT_PHASE;
   rpud.Dsamp = 0;
   rpud.rvec = NULL; rpud.rvec_num = 0; rpud.rvec_len = 0; rpud.iref = 0; 
   rpud.ftap = 0.0f; 
   rpud.dt = 0.0f;
   rpud.Fstim[ECC] = rpud.Fstim[POL] = 0.0f;
   rpud.Fresp[ECC] = rpud.Fresp[POL] = 0.0f;
   rpud.nfft = 0;
   rpud.verb = 0;
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
   rpud.ort_adj = 0;
   
   /* rpud. = ; */
   
   
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dRetinoPhase [-prefix ppp]  dataset\n"
"   where dataset is a time series from a retinotpy stimulus\n"
"\n"
" -exp EXP: These four options specify the type of retinotpy \n"
" -con CON: stimulus. EXP and CON are for expanding and \n"
" -clw CLW : contracting rings, respectively. CLW and CCW are\n"
" -ccw CCW: for clockwise and counter clockwise moving polar\n"
"           polar angle mapping stimuli. You can specify one, \n"
"           or all stimuli in one command. When all are specified\n"
"           polar angle stimuli, and eccentricity stimuli of \n"
"           opposite directions are combined.\n"
" -prefix PREF: Prefix of output datasets. \n"
"           PREF is suffixed with the following:\n"
"           .ecc+ for positive (expanding) eccentricity (EXP)\n"
"           .ecc- for negative (contracting) eccentricity (CON)\n"
"           .pol+ for clockwise polar angle mapping (CLW)\n"
"           .pol- for counterclockwise polar angle mapping (CCW)\n"
"  At a minimum each input gets a phase dataset output. It contains\n"
"     response phase (or delay) in degrees.\n"
"     If both directions are given for polar and/or eccentricity\n"
"     then a visual field angle data set is created.\n"
"     The visual field angle is obtained by averaging phases of opposite\n"
"     direction stimuli. The hemodynamic offset is half the phase difference.\n"
"\n"  
"  Each output also contains a thresholding sub-brick. Its type \n"
"     depends on the phase estimation method (-phase_estimate).\n"
"\n"
"                 Note on the thresholding sub-bricks\n"
"                 -----------------------------------\n"
"  Both FFT and DELAY values of -phase_estimate produce thresholding \n"
"     sub-bricks with the phase estimates. Those thresholds have associated \n"
"     significance levels, but they should be taken with a grain of \n"
"     salt. There is no correction for autocorrelation, so the DOFs \n"
"     are generous.\n"
"  The program also attaches a thresholding sub-brick to the\n"
"     visual field angle datasets which are estimated by averaging the phase\n"
"     estimates in order to remove the hemodynamic offset. This composite \n"
"     thresholding sub-brick contains at each voxel/node, the maximum\n"
"     threshold from the datasets of stimli of opposite direction.\n"
"  This thresholding sub-brick is for convenience, allowing you to\n"
"     threshold with a mask that is the union of the individual\n"
"     thresholded maps. Significance levels are purposefully not\n"
"     attached. I don't know how to compute them properly.\n"
"\n"    
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
" -ort_adjust: Number of DOF lost in detrending outside of this \n"
"              program.\n"
" -pre_stim PRE: Blank period, in seconds, before stimulus began \n"
" -sum_adjust y/n: Adjust sum of angles for wrapping based on the\n"
"                  angle difference. Default is 'y'\n"
" -phase_estimate METH: Select method of phase estimation\n"
"       METH == FFT  uses the phase of the fundamental frequency.\n"
"       METH == DELAY uses the 3ddelay approach for estimating\n"
"                     the phase. This requires the use of option\n"
"                     -ref_ts . See references [3] and [4] below. \n"
"       The DELAY option appears to be good as the FFT for high SNR\n"
"          and high duty cycle. See results produced by @Proc.PK.All_D\n"
"          in the demo archive AfniRetinoDemo.tgz.\n"
"       However,the DELAY option seems much better for low duty cycle stimuli.\n"
"       It is not set as the default for backward compatibility. Positive and \n"
"          negative feedback about this option are welcome.\n"
"\n"
"     Thanks to Ikuko Mukai and Masaki Fukunaga for making the case \n"
"        for DELAY's addition; they were right. \n"
"\n"
" -ref_ts REF_TS: 0 lag reference time series of response. This is\n"
"                 needed for the DELAY phase estimation method.\n"
"      With the DELAY method, the phase results are comparable to \n"
"        what you'd get with the following 3ddelay command:\n"
"        For illustration, say you have stimuli of 32 second periods\n"
"        with the polar stimuli having two wedges. After creating \n"
"        the reference time series with waver (32 sec. block period \n"
"        eccentricity, 32/2=16 sec. block period for polar), run \n" 
"        4 3ddelay commands as such:\n"
"                      for an expanding ring of 32 second period:\n"
"           3ddelay  -input exp.niml.dset \\\n"
"                    -ideal_file ECC.1D   \\\n"
"                    -fs 0.5  -T 32 \\\n"
"                    -uD -nodsamp \\\n"
"                    -phzreverse -phzscale 1.0 \\\n"
"                    -prefix ecc+.del.niml.dset\\n"
"\n"
"              Repeat for contracting ring, remove -phzreverse \n"
"\n"
"                       for clockwise two wedge of 32 second period:\n"
"           3ddelay  -input clw.niml.dset \\\n"
"                    -ideal_file POL.1D   \\\n"
"                    -fs 0.5  -T 16 \\\n"
"                    -uD -nodsamp \\\n"
"                    -phzreverse -phzscale 0.5 \\\n"
"                    -prefix pol+.del.niml.dset\\n"
"\n"
"              Repeat for counterclockwise remove -phzreverse \n"
"     Instead of the 3ddelay mess, all you do is run 3dRetinoPhase with the \n"
"        following extra options: "
"              -phase_estimate DELAY -ref_ts ECC.1D\n"
"        or    -phase_estimate DELAY -ref_ts POL.1D\n"  
"\n"
"     If you are not familiar with the use of program 'waver' for creating\n"
"     reference time series, take a look at demo script @Proc.PK.All_D in\n"
"     AfniRetinoDemo.tgz.\n"
"\n"
" -multi_ref_ts MULTI_REF_TS: Multiple 0 lag reference time series. \n"
"                             This allows you to test multiple regressors.\n"
"                             The program will run a separate analysis for \n"
"                             each regressor (column), and combine the results\n"
"                             in the output dataset this way:\n"
"       ([.] denotes output sub-brick)\n"
"       [0]: Phase from regressor that yields the highest correlation coeff.\n"
"       [1]: Maximum correlation coefficient.\n"
"       [2]: Number of regressor that yields the highest correlation coeff.\n"
"            Counting begins at 1 (not 0)\n"
"       [3]: Phase from regressor 1\n"
"       [4]: Correlation coefficient from regressor 1\n"
"       [5]: Phase from regressor 2\n"
"       [6]: Correlation coefficient from regressor 2\n"
"       ... etc.\n"
"       In general, for regressor k (k starts at 1)\n"
"          [2*k+1] contains the Phase and [2*k+2] the Correlation coefficient\n"
"\n"
"  N.B: If MULTI_REF_TS has only one timeseries, -multi_ref_ts produces\n"
"       an output identical to that of -ref_ts. \n"
"\n"        
"  See usage in @RetinoProc and demo data in\n"
"  https://afni.nimh.nih.gov/pub/dist/tgz/AfniRetinoDemo.tgz \n"
"\n"
"References for this program:\n"
"   [1] RW Cox.  AFNI: Software for analysis and visualization of functional\n"
"                      magnetic resonance neuroimages.  \n"
"                      Computers and Biomedical Research, 29: 162-173, 1996.\n"
"   [2] Saad Z.S., et al.  SUMA: An Interface For Surface-Based Intra- And\n" 
"                      Inter-Subject Analysis With AFNI.\n"
"     Proc. 2004 IEEE International Symposium on Biomedical Imaging, 1510-1513\n"
"   If you use the DELAY method:\n"
"   [3] Saad, Z.S., et al. Analysis and use of FMRI response delays. \n"
"         Hum Brain Mapp, 2001. 13(2): p. 74-93.\n"
"   [4] Saad, Z.S., E.A. DeYoe, and K.M. Ropella, Estimation of FMRI \n"
"         Response Delays.  Neuroimage, 2003. 18(2): p. 494-504.\n"
"\n"       
/* options left over from 3dDFT.c , or not yet tested    
" -abs     == output float dataset = abs(DFT)\n"
" -nfft N  == use 'N' for DFT length (must be >= #time points)\n"
" -taper f == taper 'f' fraction of data at ends (0 <= f <= 1).\n"
"             [Hamming 'raised cosine' taper of f/2 of the ]\n"
"             [data length at each end; default is no taper]\n" 
" -slice_time_adjust y/n: Adjust for slice timing difference.\n"

Bring them back after testing. */
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dRetinoPhase main"); machdep(); AFNI_logger("3dRetinoPhase",argc,argv);
   /* AUTHOR("ZIAD") ; */
#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*-- options --*/
   set_obliquity_report(0); /* silence obliquity */

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
         if( mask_top < mask_bot )
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
      
      if( strcmp(argv[iarg],"-slice_time_adjust") == 0 ){ 
         if( iarg+1 >= argc )
                  ERROR_exit("-slice_time_adjust requires an argument\n");       
         ++iarg;
         if (argv[iarg][0] == 'y' || argv[iarg][0] == 'Y') {
            rpud.Dsamp = 1;
         } else if (argv[iarg][0] == 'n' || argv[iarg][0] == 'N') {
            rpud.Dsamp = 0;
         } else {
            ERROR_exit("Illegal value (%s) after -slice_time_adjust\n", 
                        argv[iarg]);
         }
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-phase_estimate") == 0 ){ 
         if( iarg+1 >= argc )
                  ERROR_exit("-phase_estimate requires an argument\n");       
         ++iarg;
         if (!strcmp(argv[iarg],"FFT") || !strcmp(argv[iarg],"fft")) {
            rpud.pmeth = FFT_PHASE;
         } else if ( !strcmp(argv[iarg],"3DDELAY") || 
                     !strcmp(argv[iarg],"3ddelay") ||
                     !strcmp(argv[iarg],"delay") ||
                     !strcmp(argv[iarg],"DELAY")) {
            rpud.pmeth = HILB_DELAY;
         } else {
            ERROR_exit("Illegal value (%s) after -phase_estimate\n", 
                        argv[iarg]);
         }
         iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-ref_ts") == 0 ||
          strcmp(argv[iarg],"-multi_ref_ts") == 0){ 
         MRI_IMAGE * flim=NULL;
         float *fp=NULL;
         int iv=0;
         if( iarg+1 >= argc )
               ERROR_exit("-ref_ts/-multi_ref_ts require an argument\n");       
         ++iarg;
         if (!(flim = mri_read_1D(argv[iarg]))) {
            ERROR_exit("Illegal value (%s) after -phase_estimate\n", 
                        argv[iarg]);
         }
         rpud.rvec_len = flim->nx;
         rpud.rvec_num = flim->ny;
         if (flim->ny != 1) {
            if (!strcmp(argv[iarg-1],"-ref_ts")) { 
               ERROR_exit("Only one column can be present in -ref_ts file.\n"
                          "Have %d columns in %s.\n" 
                          "If you want to run the multi_ref version use\n" 
                          "-multi_ref_ts option instead, but understand that\n"
                          "the output datasets have a differing content.\n"
                           ,flim->ny, argv[iarg]);
            } else {
               /* multiref mode */
            }
         }
         rpud.rvec = (float **)calloc(flim->ny, sizeof(float *));
         fp = MRI_FLOAT_PTR(flim);
         for (iv=0; iv<flim->ny; ++iv) {
            rpud.rvec[iv] = (float *)calloc(flim->nx, sizeof(float));
            for (ii=0; ii<flim->nx; ++ii) {
               rpud.rvec[iv][ii] = *fp; ++fp;
            }
         }
         mri_free(flim); flim = NULL;
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
        if( rpud.pre < 0.0f )
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
         ERROR_exit("-dir now obsolete, use -exp, -con, -clw, or -ccw\n");
        if( iarg+1 >= argc )
            ERROR_exit("-dir option requires an argument!\n");
        ++iarg;
        if      (!strncmp(argv[iarg],"exp", 3)) rpud.dir = EXP;
        else if (!strncmp(argv[iarg],"con", 3) ||
                 !strncmp(argv[iarg],"cnt", 3)) rpud.dir = CONT;
        else if (!strncmp(argv[iarg],"cw",  2) ||
                 !strncmp(argv[iarg],"clo", 3) ||
                 !strncmp(argv[iarg],"clw", 3)) rpud.dir = CLW;
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

      if( strcmp(argv[iarg],"-cw") == 0 ||
          strcmp(argv[iarg],"-clw") == 0){
         if( iarg+1 >= argc )
            ERROR_exit("-clw (or -cw) option requires a  dset\n");
         in_name[k_CLW] = argv[++iarg] ;
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

      if( strcmp(argv[iarg],"-ort_adjust") == 0 ){
        rpud.ort_adj += atoi(argv[++iarg]); 
        iarg++ ; continue ;
      }
      
      if( strcmp(argv[iarg],"-spectra") == 0 ){
        rpud.spectra = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-detrend") == 0 ){
        dtused = 1;
        iarg++ ; continue ;
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
   
   if (dtused && rpud.verb) {
      INFO_message("Linear detrend is always on, -detrend is useless.") ; 
   }
   
   if( !in_name )
     ERROR_exit("No datasets on command line!?") ;
   
   if (rpud.verb && rpud.rvec && rpud.rvec_num > 1) {
      INFO_message("Multi-ref mode with %d reference time series\n"
                   , rpud.rvec_num);
   }

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
            if (rpud.nvals < 10) {
               ERROR_exit( "Dataset has too few (%d) time points.\n", 
                           rpud.nvals);
            }
            rpud.dt = DSET_TR_SEC(old_dset);
            if (rpud.dt > 100) {
               ERROR_exit("TR of %f sec of %s is very suspicously high.\n"
                       "Program assumes this is a mistake in the header.\n"
                       "You can use 3drefit's -TR option to fix the problem.\n",
                       rpud.dt, in_name[stype]);
            }
            mmm = MaskSetup(old_dset, mask_dset, &rpud, cmask, 
                           &ncmask, mask_bot, mask_top, &mcount);
         } else {
            if (DSET_NUM_TIMES(old_dset) != rpud.nvals) {
               ERROR_exit( "Dataset %s has %d time points while %s has %d\n",
                           in_name[stype], DSET_NUM_TIMES(old_dset),
                           DSET_PREFIX(rpud.iset), rpud.nvals); 
            }
            if (DSET_TR_SEC(old_dset) !=  rpud.dt) {
               ERROR_exit( "Dataset %s has a TR of %f sec while %s has %f sec\n",
                           in_name[stype], DSET_TR_SEC(old_dset),
                           DSET_PREFIX(rpud.iset), rpud.dt); 
            }
         }
         rpud.iset = old_dset;
         rpud.dir = Phase_Type_to_Dir(stype);
         
         /*------------- ready to compute new dataset -----------*/
         if (rpud.verb) 
            INFO_message("Processing %s, %d time points\n", 
                           in_name[stype], DSET_NVALS(old_dset));
         
         
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
         
         if (rpud.verb > 1) INFO_message("Output time\n");
         if (rpud.pmeth == FFT_PHASE) {
            new_dset[stype] = MAKER_4D_to_typed_fbuc(
                          old_dset ,             /* input dataset */
                          stmp ,               /* output prefix */
                          datum ,                /* output datum  */
                          0 ,                    /* ignore count  */
                          1 ,                   /* linear detrend */
                          2 ,                   /* number of briks */
                          RP_tsfunc,          /* timeseries processor */
                          (void *)(&rpud),    /* data for tsfunc */
                          mmm,
                          0   /* Allow auto scaling of output */
                       ) ;
            
            if( new_dset[stype] != NULL ){
               sprintf(stmp,"Phz@%.3fHz", rpud.Fresp[Dir2Type(rpud.dir)]);
               EDIT_BRICK_LABEL(new_dset[stype], 0, stmp);
               sprintf(stmp,"PwR@%.3fHz", rpud.Fresp[Dir2Type(rpud.dir)]);
               EDIT_BRICK_LABEL(new_dset[stype], 1, stmp);
               EDIT_BRICK_TO_FIFT(new_dset[stype],1,rpud.dof[0],rpud.dof[1]);
            } else {
               ERROR_exit("Unable to compute output dataset!\n") ;
            }
         } else if (rpud.pmeth == HILB_DELAY){
            for (rpud.iref = 0; rpud.iref < rpud.rvec_num; ++rpud.iref) {
               if (!(idset = MAKER_4D_to_typed_fbuc(
                             old_dset ,             /* input dataset */
                             stmp ,               /* output prefix */
                             datum ,                /* output datum  */
                             0 ,                    /* ignore count  */
                             1 ,                   /* linear detrend */
                             2 ,                   /* number of briks */
                             DEL_tsfunc,          /* timeseries processor */
                             (void *)(&rpud),    /* data for tsfunc */
                             mmm,
                             0   /* Allow auto scaling of output */
                          ))) {
                  ERROR_exit("Unable to compute %d output dataset!\n", 
                              rpud.iref) ;        
               }
               if (rpud.verb) {
                  INFO_message("Done with delays for %d/%d\n", 
                               rpud.iref, rpud.rvec_num);
               }
               if (!new_dset[stype]) {
                  /* first, pass, take output as is */
                  new_dset[stype] = idset; 
                  /* fix labels */
                  if (rpud.rvec_num == 1) {
                     sprintf(stmp,"Phz_Delay");
                     EDIT_BRICK_LABEL(new_dset[stype], 0, stmp);
                     sprintf(stmp,"Corr.Coef.");
                     EDIT_BRICK_LABEL(new_dset[stype], 1, stmp);
                     EDIT_BRICK_TO_FICO(new_dset[stype], 1, rpud.rvec_len, 
                                     rpud.dof[0],rpud.dof[1]);
                  } else {
                     if (rpud.verb > 1) {
                        INFO_message("Adding Summary results\n", rpud.rvec_num);
                     }
                     sprintf(stmp,"Phz_Delay@Max.Corr.");
                     EDIT_BRICK_LABEL(new_dset[stype], 0, stmp);
                     sprintf(stmp,"Max.Corr.Coef.");
                     EDIT_BRICK_LABEL(new_dset[stype], 1, stmp);
                     /* add the best phase winner */
                     best = (float *)malloc(DSET_NVOX(idset)*sizeof(float));
                     for (ii=0; ii<DSET_NVOX(idset); ++ii) best[ii] = 1;
                     EDIT_add_brick(new_dset[stype], MRI_float, 
                                    0.0, best);
                     EDIT_BRICK_LABEL(new_dset[stype], 2, "best_fit_regressor");
                     /* the bulk of idset is now in the summary output
                        make a copy of it because we'll also append it below 
                        Note that only idset's bricks are being transferred,
                        so there is some leakage each time a new idset
                        gets generated without wiping the old one clean...*/
                     idset = EDIT_full_copy(idset,"gingrich");
                  }
               }
               if (rpud.rvec_num > 1) { /* have multiple series */
                  if (rpud.verb > 1) {
                     INFO_message("Adding %d/%d results\n", 
                                  rpud.iref+1,rpud.rvec_num);
                  }
                  /* add phase */
                  EDIT_add_brick(new_dset[stype], DSET_BRICK_TYPE(idset,0), 0.0,
                                 DSET_ARRAY(idset,0));
                  sprintf(stmp,"Phz_Delay@%02d", rpud.iref+1);
                  EDIT_BRICK_LABEL(new_dset[stype], 
                                   DSET_NVALS(new_dset[stype])-1, stmp);
                  /* add threshold */
                  EDIT_add_brick(new_dset[stype], DSET_BRICK_TYPE(idset,1), 0.0,
                                 DSET_ARRAY(idset,1));
                  sprintf(stmp,"Corr.Coef.@%02d", rpud.iref+1);
                  EDIT_BRICK_LABEL(new_dset[stype],
                                   DSET_NVALS(new_dset[stype])-1, stmp);
                  EDIT_BRICK_TO_FICO(new_dset[stype], 
                                     DSET_NVALS(new_dset[stype])-1, 
                                     rpud.rvec_len, 
                                     rpud.dof[0],rpud.dof[1]);
                  
                  /* swap values in 1st two sets with the max. */
                  if (rpud.iref > 0) {
                     d0 = (float *)DSET_ARRAY(new_dset[stype], 0);
                     x0 = (float *)DSET_ARRAY(new_dset[stype], 1);
                     best = (float *)DSET_ARRAY(new_dset[stype], 2);
                     d1 = (float *)DSET_ARRAY(new_dset[stype], 
                                              DSET_NVALS(new_dset[stype])-2);
                     x1 = (float *)DSET_ARRAY(new_dset[stype], 
                                     DSET_NVALS(new_dset[stype])-1);
                     for (ii=0; ii<DSET_NVOX(idset); ++ii) {
                        if (x1[ii] > x0[ii]) {
                           x0[ii] = x1[ii];
                           d0[ii] = d1[ii];
                           best[ii] = rpud.iref+1;
                        }
                     }                
                  } 
               }
            }
         }
         

         /* write the output */
         if (rpud.verb) {
            INFO_message("Writing delay results");
         }
         tross_Copy_History( old_dset , new_dset[stype] ) ;
         tross_Make_History("3dRetinoPhase", argc, argv ,new_dset[stype]) ;
         /* refresh stats, what with the constant updating */
         DSET_KILL_STATS(new_dset[stype]); THD_load_statistics(new_dset[stype]) ;
         DSET_write( new_dset[stype] ) ;
         WROTE_DSET( new_dset[stype] ) ;
         
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
      if (rpud.verb) {
         INFO_message("Combining bidirectional eccentricity data");
      }
      rpud.dir = CONT; /* not really, but sign does not matter at this point */
      cset = Combine_Opposites(new_dset[k_EXP], new_dset[k_CON], 
                        &rpud);
      tross_Copy_History( cset , new_dset[k_EXP] ) ;
      DSET_write( cset ) ;
      WROTE_DSET( cset ) ;
      DSET_delete(cset); cset = NULL;                  
   }
   
   if (new_dset[k_CLW] && new_dset[k_CCW]) {
      THD_3dim_dataset *cset=NULL;
      if (rpud.verb) {
         INFO_message("Combining bidirectional polar data");
      }
      rpud.dir = CLW; /* not really, but sign does not matter at this point */
      cset = Combine_Opposites(new_dset[k_CLW], new_dset[k_CCW], 
                        &rpud);
      tross_Copy_History( cset , new_dset[k_CLW] ) ;
      DSET_write( cset ) ;
      WROTE_DSET( cset ) ;
      DSET_delete(cset); cset = NULL;                  
   }
   
   if (rpud.verb>1) {
      INFO_message("Cleanup");
   }
   for (stype=0; stype<k_N; ++stype) 
      if (new_dset[stype]) DSET_delete(new_dset[stype]);
         
   free(in_name);
   free(new_dset);

   exit(0) ;
}
