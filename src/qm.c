#include "mrilib.h"
#include <time.h>
#include <sys/types.h>
#include <unistd.h>


int main( int argc , char *argv[] )
{
   MRI_IMAGE *ima , *imb ;
   GA_setup stup ;
   float xxx,yyy,zzz ; int ii , iarg=1 ;
   int meth=GA_MATCH_PEARSON_SCALAR ;
   float rad=0.0f ; int sm=GA_SMOOTH_GAUSSIAN , mask=0 , twopass=0 , verb=1 ;
   char *fout=NULL ;
   MRI_IMAGE *maskim=NULL ; byte *maskar=NULL ; int nx,ny,nz , nrand,nmask=0 ;
   int npmax = 999999999 ;

   if( argc < 3 ){
     printf("Usage: qm [-n nnn] [-q] [-mask] [-rank] [-mi] [-cr] [-rad r] [-out f] [-med] base targ\n");
     exit(0);
   }

   mainENTRY("qm") ;
   srand48((long)time(NULL)+(long)getpid()) ;

   while( iarg < argc && argv[iarg][0] == '-' ){
     if( strcmp(argv[iarg],"-n") == 0 ){
       npmax = (int)strtod(argv[++iarg],NULL); iarg++; continue ;
     }
     if( strcmp(argv[iarg],"-q") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-rank") == 0 ){
       meth = GA_MATCH_SPEARMAN_SCALAR ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-cr") == 0 ){
       meth = GA_MATCH_CORRATIO_SCALAR ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-mi") == 0 ){
       meth = GA_MATCH_KULLBACK_SCALAR ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-rad") == 0 ){
       rad = (float)strtod(argv[++iarg],NULL) ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-out") == 0 ){
       fout = argv[++iarg] ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-med") == 0 ){
       sm = GA_SMOOTH_MEDIAN ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-mask") == 0 ){
       mask = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-two") == 0 ){
       twopass = 1 ; iarg++ ; continue ;
     }

     ERROR_exit("Illegal option '%s'",argv[iarg]) ;
   }

   if( iarg > argc-2 ) ERROR_exit("need 2 image args!?") ;
   ima = mri_read( argv[iarg++] ); if( ima == NULL ) ERROR_exit("bad base");
   imb = mri_read( argv[iarg++] ); if( imb == NULL ) ERROR_exit("bad targ");

   nx = ima->nx ; ny = ima->ny ; nz = ima->nz ;
   if( mask ){
     maskar = mri_automask_image( ima ) ;
     for( ii=0 ; ii < 5 ; ii++ ){
       THD_mask_dilate           ( nx,ny,nz , maskar, 3 ) ;
       THD_mask_fillin_completely( nx,ny,nz , maskar, 3 ) ;
     }
     nmask = THD_countmask(nx*ny*nz,maskar) ;
     if(verb)INFO_message("%d/%d voxels in mask/image",nmask,nx*ny*nz) ;
     maskim = mri_new_vol_empty(nx,ny,nz,MRI_byte) ;
     mri_fix_data_pointer(maskar,maskim) ;
   } else {
     nmask = nx*ny*nz ;
   }

   ima->xo = -ima->nx * ima->dx; imb->xo = -imb->nx * imb->dx;
   ima->yo = -ima->ny * ima->dy; imb->yo = -imb->ny * imb->dy;
   ima->zo = -ima->nz * ima->dz; imb->zo = -imb->nz * imb->dz;

   memset(&stup,0,sizeof(GA_setup)) ;

   stup.match_code    = meth ;
   stup.smooth_code   = sm ;
   stup.smooth_radius = rad ;
   stup.interp_code   = MRI_NN ;
   stup.npt_match     = nmask / 16 ;
   if( stup.npt_match > npmax ) stup.npt_match = npmax ;

   stup.wfunc         = mri_genalign_affine ;
   stup.wfunc_numpar  = 6 ;
   stup.wfunc_param   = (GA_param *)calloc(12,sizeof(GA_param)) ;

#define DEFPAR(p,nm,bb,tt,id,dd,ll)               \
 do{ stup.wfunc_param[p].min      = (bb) ;        \
     stup.wfunc_param[p].max      = (tt) ;        \
     stup.wfunc_param[p].delta    = (dd) ;        \
     stup.wfunc_param[p].toler    = (ll) ;        \
     stup.wfunc_param[p].ident    = (id) ;        \
     stup.wfunc_param[p].val_init = (id) ;        \
     strcpy( stup.wfunc_param[p].name , (nm) ) ;  \
     stup.wfunc_param[p].fixed = 0 ;              \
 } while(0)

   xxx = 0.444*fabs(ima->nx*ima->dx) ;
   yyy = 0.444*fabs(ima->ny*ima->dy) ;
   zzz = 0.444*fabs(ima->nz*ima->dz) ;
   if(verb)INFO_message("Max displacement allowed = (%.1f,%.1f,%.1f) mm",xxx,yyy,zzz) ;

   DEFPAR( 0, "x-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 1, "y-shift" , -yyy , yyy , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 2, "z-shift" , -zzz , zzz , 0.0 , 0.0 , 0.0 ) ;

   DEFPAR( 3, "z-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;  /* degrees */
   DEFPAR( 4, "x-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 5, "y-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;

   if( MRI_DIMENSIONALITY(ima) == 2 ){
     stup.wfunc_param[2].fixed = 1 ;
     stup.wfunc_param[4].fixed = 1 ;
     stup.wfunc_param[5].fixed = 1 ;
     nrand = 77 ;
     if(verb)INFO_message("2D files") ;
   } else {
     nrand = 21 ;
     if(verb)INFO_message("3D files") ;
   }

   mri_genalign_scalar_setup( ima , maskim , imb , &stup ) ;

   if(verb)INFO_message("Start random setup") ;
   mri_genalign_scalar_ransetup( &stup , nrand ) ;
   if(verb)INFO_message("val_init: %.2f %.2f %.2f  %.2f %.2f %.2f\n",
                stup.wfunc_param[0].val_init ,
                stup.wfunc_param[1].val_init ,
                stup.wfunc_param[2].val_init ,
                stup.wfunc_param[3].val_init ,
                stup.wfunc_param[4].val_init ,
                stup.wfunc_param[5].val_init  );
   if(verb)INFO_message("vbest = %g",stup.vbest) ;

   if(verb)INFO_message("Start optimization") ;
   stup.npt_match   = nmask / 8 ;
   if( stup.npt_match > npmax ) stup.npt_match = npmax ;
   stup.interp_code = MRI_LINEAR ;
   mri_genalign_scalar_setup( ima , maskim , imb , &stup ) ;
   ii = mri_genalign_scalar_optim( &stup , 0.04 , 0.0001 , 6666 ) ;
   if(verb)INFO_message("val_out:: %.2f %.2f %.2f  %.2f %.2f %.2f\n",
                stup.wfunc_param[0].val_out ,
                stup.wfunc_param[1].val_out ,
                stup.wfunc_param[2].val_out ,
                stup.wfunc_param[3].val_out ,
                stup.wfunc_param[4].val_out ,
                stup.wfunc_param[5].val_out  );
   if(verb)INFO_message("nfunc = %d",ii) ;
   if(verb)INFO_message("vbest = %g",stup.vbest) ;

   if( twopass ){
   if(verb)INFO_message("Restart optimization") ;
     stup.smooth_code   = 0  ;
     stup.smooth_radius = 0.0f ;
     stup.interp_code   = MRI_CUBIC ;
     stup.npt_match     = nmask / 4 ;
     if( stup.npt_match > npmax ) stup.npt_match = npmax ;
     mri_genalign_scalar_setup( NULL,NULL,NULL, &stup ) ;
     stup.wfunc_param[0].val_init = stup.wfunc_param[0].val_out ;
     stup.wfunc_param[1].val_init = stup.wfunc_param[1].val_out ;
     stup.wfunc_param[2].val_init = stup.wfunc_param[2].val_out ;
     stup.wfunc_param[3].val_init = stup.wfunc_param[3].val_out ;
     stup.wfunc_param[4].val_init = stup.wfunc_param[4].val_out ;
     stup.wfunc_param[5].val_init = stup.wfunc_param[5].val_out ;
     ii = mri_genalign_scalar_optim( &stup , 0.01 , 0.0001 , 6666 ) ;
     INFO_message("val_fin:: %.2f %.2f %.2f  %.2f %.2f %.2f\n",
                  stup.wfunc_param[0].val_out ,
                  stup.wfunc_param[1].val_out ,
                  stup.wfunc_param[2].val_out ,
                  stup.wfunc_param[3].val_out ,
                  stup.wfunc_param[4].val_out ,
                  stup.wfunc_param[5].val_out  );
     if(verb)INFO_message("nfunc = %d",ii) ;
     if(verb)INFO_message("vbest = %g",stup.vbest) ;
   }

   if( fout != NULL ){
     MRI_IMAGE *wim = mri_genalign_scalar_warpim( &stup ) ;
     if( wim != NULL ){
       if( ima->kind != MRI_float ){
         MRI_IMAGE *qim = mri_to_mri( ima->kind , wim ) ;
         mri_free(wim) ; wim = qim ;
       }
       if(verb)INFO_message("Writing %d X %d X %d image of %s",
                wim->nx , wim->ny , wim->nz , MRI_TYPE_name[wim->kind] ) ;
       mri_write( fout , wim ) ; mri_free(wim) ;
       if(verb)INFO_message("Output %s written",fout) ;
     }
   }

   exit(0) ;
}
