#ifndef SUMA_SURFACE_IO_INCLUDED
#define SUMA_SURFACE_IO_INCLUDED

#define SUMA_MAX_OPEN_DX_FIELD_COMPONENTS 500
#define SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES 500
#define SUMA_MAX_OPEN_DX_OBJECTS  500

typedef struct {
   int rank;
   int shape;
   int items;
   int bad_data;
   char *type;
   char *object;
   char *class;
   char *data;
   char *data_off;
   int data_format;
   void *datap;
   int n_comp;
   char *comp_name[SUMA_MAX_OPEN_DX_FIELD_COMPONENTS];
   char *comp_value[SUMA_MAX_OPEN_DX_FIELD_COMPONENTS];
   int n_attr;
   char *attr_name[SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES];
   char *attr_string[SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES];
   int *counts;
   int n_counts;
   float *delta;
   int n_delta;
   float *origin;
   int n_origin;
} SUMA_OPEN_DX_STRUCT;

#define SUMA_OK_OPENDX_DATA_TYPE(tp) ( (  tp == SUMA_int || \
                                          tp == SUMA_float ||  \
                                          tp == SUMA_double || \
                                          tp == SUMA_byte )   \
                                           ? 1 : 0 )

#define SUMA_NCOL_OPENDX(dx) ( ( ( (dx)->shape == 0 ) ? 1 : ((dx)->shape) ) )

SUMA_SurfaceObject *SUMA_Load_Surface_Object_Wrapper ( char *if_name, char *if_name2, char *vp_name, 
                                                   SUMA_SO_File_Type SO_FT, SUMA_SO_File_Format SO_FF, char *sv_name, int debug);
char *SUMA_RemoveSurfNameExtension (char*Name, SUMA_SO_File_Type oType);
void *SUMA_Prefix2SurfaceName (char *prefix, char *path, char *vp_name, SUMA_SO_File_Type oType, SUMA_Boolean *exists);
void * SUMA_2Prefix2SurfaceName (char *namecoord, char *nametopo, char *path, char *vp_name, SUMA_SO_File_Type oType, SUMA_Boolean *exists);
SUMA_Boolean SUMA_SureFit_Read_Coord (char * f_name, SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_SureFit_Read_Topo (char * f_name, SUMA_SureFit_struct *SF);
void SUMA_Show_SureFit (SUMA_SureFit_struct *SF, FILE *Out);
SUMA_Boolean SUMA_Free_SureFit (SUMA_SureFit_struct *SF);
SUMA_Boolean SUMA_FreeSurfer_Read (char * f_name, SUMA_FreeSurfer_struct *FS);
SUMA_Boolean SUMA_FreeSurfer_Read_eng (char * f_name, SUMA_FreeSurfer_struct *FS, int debug);
SUMA_Boolean SUMA_FreeSurfer_ReadBin_eng (char * f_name, SUMA_FreeSurfer_struct *FS, int debug);
SUMA_Boolean SUMA_Free_FreeSurfer (SUMA_FreeSurfer_struct *FS);
void SUMA_Show_FreeSurfer (SUMA_FreeSurfer_struct *FS, FILE *Out);
SUMA_Boolean SUMA_Ply_Read (char * f_name, SUMA_SurfaceObject *SO); 
SUMA_Boolean SUMA_Ply_Write (char * f_name, SUMA_SurfaceObject *SO); 
SUMA_Boolean SUMA_VEC_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_VEC_Read(SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_FS_Write (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine);
SUMA_Boolean SUMA_SureFit_Write (SUMA_SFname *Fname, SUMA_SurfaceObject *SO);
void SUMA_SaveDrawnROI (char *filename, void *data);
SUMA_Boolean SUMA_SaveDrawnROI_1D (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat); 
SUMA_Boolean SUMA_SaveDrawnROINIML (char *filename, SUMA_SurfaceObject *SO, SUMA_DRAWN_ROI *DrawnROI, int SaveWhat, int Format); 
SUMA_1D_DRAWN_ROI * SUMA_Free_1DDrawROI (SUMA_1D_DRAWN_ROI *ROI1D);
SUMA_Boolean SUMA_Write_DrawnROI_1D (SUMA_DRAWN_ROI **ROIv, int N_ROI, char *filename); 
SUMA_1D_DRAWN_ROI * SUMA_DrawnROI_to_1DDrawROI (SUMA_DRAWN_ROI *ROI);
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_NIML (char *filename, int *N_ROI, SUMA_Boolean ForDisplay);
SUMA_DRAWN_ROI ** SUMA_OpenDrawnROI_1D(char *filename, char *Parent_idcode_str, int *N_ROI, SUMA_Boolean ForDisplay);
void SUMA_OpenDrawnROI (char *filename, void *data);
SUMA_DSET *SUMA_ROIv2Grpdataset (SUMA_DRAWN_ROI** ROIv, int N_ROIv, char *Parent_idcode_str, int Pad_to, int Pad_val) ;
NI_element *SUMA_ROIv2dataset (SUMA_DRAWN_ROI** ROIv, int N_ROIv, char *Parent_idcode_str, int pad_to, int pad_val); 
void SUMA_SaveSOascii (char *filename, void *data);
float * SUMA_readFScurv (char *f_name, int *nrows, int *ncols, SUMA_Boolean rowmajor, SUMA_Boolean SkipCoords);
SUMA_Boolean SUMA_BrainVoyager_Read(char *f_name, SUMA_SurfaceObject *SO, int debug); 
SUMA_Boolean SUMA_FreeSurfer_WritePatch (char *fileNm, SUMA_SurfaceObject *SO, char *firstLine, SUMA_SurfaceObject *SOparent);
SUMA_FORM_AFNI_DSET_STRUCT *SUMA_New_FormAfniDset_Opt(void);
SUMA_FORM_AFNI_DSET_STRUCT *SUMA_Free_FormAfniDset_Opt(SUMA_FORM_AFNI_DSET_STRUCT *Opt);
THD_3dim_dataset *SUMA_FormAfnidset (float *NodeList, float *vals, int N_vals, SUMA_FORM_AFNI_DSET_STRUCT *Opt);
NI_group *SUMA_SO2nimlSO(SUMA_SurfaceObject *SO, char *optlist, int nlee) ;
SUMA_SurfaceObject *SUMA_nimlSO2SO(NI_group *ngr); 
SUMA_OPEN_DX_STRUCT *SUMA_Alloc_OpenDX_Struct(void);
SUMA_OPEN_DX_STRUCT *SUMA_Free_OpenDX_Struct(SUMA_OPEN_DX_STRUCT *dx);
SUMA_OPEN_DX_STRUCT ** SUMA_Free_OpenDX_StructVec(SUMA_OPEN_DX_STRUCT **dxv, int nobj);
void SUMA_Show_OpenDX_Struct(SUMA_OPEN_DX_STRUCT **dxv, int N_dxv, FILE *out);
SUMA_Boolean SUMA_OpenDX_Write(char *fname, SUMA_SurfaceObject *SO);
SUMA_Boolean SUMA_OpenDx_Object_Data(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx);
SUMA_Boolean SUMA_OpenDx_Object_Attr(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx);
SUMA_Boolean SUMA_OpenDx_Object_Components(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx);
void * SUMA_OpenDx_Object_Header_Field(char *op, int nchar, const char *attr, char **op_end);
SUMA_OPEN_DX_STRUCT **SUMA_OpenDX_Read(char *fname, int *nobj);
SUMA_OPEN_DX_STRUCT *SUMA_Find_OpenDX_Object_Name(SUMA_OPEN_DX_STRUCT **dxv, int iop, char *nm, int *nf);
SUMA_OPEN_DX_STRUCT *SUMA_Find_OpenDX_Object_Class(SUMA_OPEN_DX_STRUCT **dxv, int iop, char *nm, int *nf);
SUMA_Boolean SUMA_OpenDX_Read_SO(char *fname, SUMA_SurfaceObject *SO);
char * SUMA_OpenDX_Read_CruiseVolHead(char *fname, THD_3dim_dataset *dset, int loaddata);


#endif
