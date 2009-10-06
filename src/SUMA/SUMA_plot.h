#ifndef SUMA_PLOT_INCLUDED
#define SUMA_PLOT_INCLUDED
   /* begin functions defined in plot_motif.c */
   extern void pm_donebut_CB( Widget w , XtPointer cd , XtPointer cb );
   extern void pm_psfile_CB( Widget w , XtPointer cd , XtPointer cb );
   extern void pm_psprint_CB( Widget w , XtPointer cd , XtPointer cb );
   extern void pm_expose_CB( Widget w , XtPointer cd , XtPointer cb );
   extern void pm_resize_CB( Widget w , XtPointer cd , XtPointer cb );
   extern void pm_input_CB( Widget w , XtPointer cd , XtPointer cb );
   /* end functions defined in plot_motif.c */

   
SUMA_Boolean SUMA_OverlayGraphAtNode(SUMA_OVERLAYS *Sover,
               SUMA_SurfaceObject *SO,
               int inode);
void SUMA_rowgraph_mtdkill( MEM_topshell_data * mp );
void SUMA_memplot_clone(void *mpv);

typedef struct {
   SUMA_OVERLAYS *Sover;
   float **tsa;
   int tsa_dims[2];
   int tsnode;
   char write_name[100];
}SUMA_MEMPLOT_USERDATA; /* Modify REFILL_MPUD and SUMA_clear_mpud_contents 
                           when adding new fields here */
SUMA_MEMPLOT_USERDATA * SUMA_clear_mpud_contents(SUMA_MEMPLOT_USERDATA *mpud); 

SUMA_Boolean SUMA_OverlayGraphAtNode(SUMA_OVERLAYS *Sover,
               SUMA_SurfaceObject *SO,
               int inode);

#endif
