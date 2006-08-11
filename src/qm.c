#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *ima , *imb ;
   GA_setup stup ;
   float xxx ; int ii ;

   if( argc < 3 ){
     printf("Usage: qm base targ\n"); exit(0);
   }

   mainENTRY("qm") ;

   ima = mri_read( argv[1] ); if( ima == NULL ) ERROR_exit("bad base");
   imb = mri_read( argv[2] ); if( imb == NULL ) ERROR_exit("bad targ");

   memset(&stup,0,sizeof(GA_setup)) ;

   stup.match_code    = GA_MATCH_PEARSON_SCALAR ;
   stup.smooth_code   = GA_SMOOTH_GAUSSIAN ;
   stup.smooth_radius = 0.0f ;
   stup.interp_code   = MRI_CUBIC ;
   stup.npt_match     = ima->nvox / 8 ;

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

   xxx = 0.1*(fabs(ima->nx*ima->dx)+fabs(ima->ny*ima->dy)+fabs(ima->nz*ima->dz));

   DEFPAR( 0, "x-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 1, "y-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 2, "z-shift" , -xxx , xxx , 0.0 , 0.0 , 0.0 ) ;

   DEFPAR( 3, "z-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;  /* degrees */
   DEFPAR( 4, "x-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;
   DEFPAR( 5, "y-angle" , -30.0 , 30.0 , 0.0 , 0.0 , 0.0 ) ;

   if( MRI_DIMENSIONALITY(ima) == 2 ){
     stup.wfunc_param[2].fixed = 1 ;
     stup.wfunc_param[4].fixed = 1 ;
     stup.wfunc_param[5].fixed = 1 ;
   }

   mri_genalign_scalar_setup( ima , NULL , imb , &stup ) ;

   mri_genalign_scalar_ransetup( &stup , 166 ) ;
   INFO_message("val_init: %.2f %.2f %.2f  %.2f %.2f %.2f\n",
                stup.wfunc_param[0].val_init ,
                stup.wfunc_param[1].val_init ,
                stup.wfunc_param[2].val_init ,
                stup.wfunc_param[3].val_init ,
                stup.wfunc_param[4].val_init ,
                stup.wfunc_param[5].val_init  );

   ii = mri_genalign_scalar_optim( &stup , 0.01 , 0.0001 , 6666 ) ;
   INFO_message("nstep = %d\n",ii) ;
   INFO_message("val_out:: %.2f %.2f %.2f  %.2f %.2f %.2f\n",
                stup.wfunc_param[0].val_out ,
                stup.wfunc_param[1].val_out ,
                stup.wfunc_param[2].val_out ,
                stup.wfunc_param[3].val_out ,
                stup.wfunc_param[4].val_out ,
                stup.wfunc_param[5].val_out  );

   exit(0) ;
}
