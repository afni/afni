
typedef void w3func(int,int,float *,float *,float *,float *) ;

typedef struct {
   float xbot,xtop , ybot,ytop , zbot,ztop ;
} warp3D_box ;

typedef struct {
   w3func     *func ;
   warp3D_box *box ;
   char       *name ;
} warp3D_basis ;

typedef struct {
   float aa ;
   float bxx , byy , bzz ;
   int nbasis ;
   float        *baswt ;
   warp3D_basis *basfn ;
} warp3D_transform ;

void warp3D_forward_inplace( warp3D_transform *warp ,
                             MRI_IMAGE *imin , MRI_IMAGE *imout ) ;

void warp3D_inverse_inplace( warp3D_transform *warp ,
                             MRI_IMAGE *imin , MRI_IMAGE *imout ) ;
