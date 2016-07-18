#ifndef _MRI_PROCESS_SIEMENS_H_
#define _MRI_PROCESS_SIEMENS_H_

/* typedefs and functions common to libmri and Dimon */

typedef struct {
  int good ;                       /* data in here is good?                  */
  int have_data[3] ;               /* do we have slices 0 and 1 in           *
                                    * each dimension to determine z-spacing? *
                                    * added 25 Feb 2003 KRH                  */
  int mos_ix, mos_nx, mos_ny;      /* mosaic properties                      */
  int mosaic_num ;                 /* how many slices in 1 'image'           */
  float slice_xyz[NMOMAX][3] ;     /* Sag, Cor, Tra coordinates              */
  int ImageNumbSag, ImageNumbTra, ImageNumbCor; /* reverse the slices        */
} Siemens_extra_info ;

int process_siemens_mosaic(
        Siemens_extra_info * Sinfo, char ** Sstr, char ** epos,
        char * fname, int assume, int nx, int ny, int nz);

int get_siemens_extra_info(char *str, Siemens_extra_info *mi, char ** epos) ;


int read_mosaic_data(FILE *fp, MRI_IMAGE *im, MRI_IMARR *imar,
   int *flip_slices, Siemens_extra_info *mi, int datum, int bpp, int kor,
   int swap, float dx, float dy, float dz, float dt);

int flip_slices_mosaic (Siemens_extra_info *mi, int kor);
int apply_z_orient(Siemens_extra_info * Sinfo, char * orients, int * kor,
                   float * zoff);


#endif  /* ifndef  _MRI_PROCESS_SIEMENS_H_ */
