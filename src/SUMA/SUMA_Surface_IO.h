#ifndef SUMA_SURFACE_IO_INCLUDED
#define SUMA_SURFACE_IO_INCLUDED

SUMA_Boolean SUMA_SureFit_Read_Coord (char * f_name, SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_SureFit_Read_Topo (char * f_name, SUMA_SureFit_struct *SF);
void SUMA_Show_SureFit (SUMA_SureFit_struct *SF, FILE *Out);
SUMA_Boolean SUMA_Free_SureFit (SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS);
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS);
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out);
SUMA_Boolean SUMA_Ply_Read (char * f_name, SUMA_SurfaceObject *SO); 
SUMA_Boolean SUMA_Ply_Write (char * f_name, SUMA_SurfaceObject *SO); 
SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_FS_Write (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine);
SUMA_Boolean SUMA_SureFit_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
void SUMA_SaveDrawnROI (char *filename, void *data);
SUMA_Boolean SUMA_SaveDrawnROI_1D (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat); 
SUMA_Boolean SUMA_SaveDrawnROINIML (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat, int Format); 
SUMA_1D_DRAWN_ROI * SUMA_Free_1DDrawROI (SUMA_1D_DRAWN_ROI *ROI1D);
SUMA_Boolean SUMA_Write_DrawnROI_1D (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename); 
SUMA_1D_DRAWN_ROI * SUMA_DrawnROI_to_1DDrawROI (SUMA_DRAWN_ROI *ROI);
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_NIML (char *filename, int *N_ROI);
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_1D(char *filename, char *Parent_idcode_str, int *N_ROI);
void SUMA_OpenDrawnROI (char *filename, void *data);



#endif
