
#ifndef _R_IDISP_H_
#define _R_IDISP_H_

/*    --- 3ddata.h ---              */

int r_idisp_fd_brick         ( char * info, FD_brick         * bp );
int r_idisp_thd_3dim_dataset ( char * info, THD_3dim_dataset * dp );
int r_idisp_thd_datablock    ( char * info, THD_datablock    * dp );
int r_idisp_thd_dataxes      ( char * info, THD_dataxes      * dp );
int r_idisp_thd_diskptr      ( char * info, THD_diskptr      * dp );


/*    --- mrilib.h ---              */

int r_idisp_mri_image  ( char * info, MRI_IMAGE   * ip );
int r_idisp_mri_imarr  ( char * info, MRI_IMARR   * ip, int images );


/*    --- raw data types ---        */

int r_idisp_mat33d     ( char * info, double   mat[3][3] );
int r_idisp_mat33f     ( char * info, float    mat[3][3] );
int r_idisp_vec3d      ( char * info, double * vec );
int r_idisp_vec3i      ( char * info, int    * vec );


/*   ---  cox_render.h ---          */

#ifdef _COX_RENDER_HEADER_

int r_idisp_cren_stuff  ( char * info, CREN_stuff * cp );

#endif /* _COX_RENDER_HEADER_ */

#endif /* _R_IDISP_H_ */
