
/***** List of plugin initialization functions when compiled for CYGWIN ****/

extern PLUGIN_interface * PLUGIN_init_plug_clust          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_copy           (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_rename         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_tag            (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_power          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_stats          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_lsqfit         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_imreg          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_edit           (int) ;
/** extern PLUGIN_interface * PLUGIN_init_plug_nlfit          (int) ; **/
extern PLUGIN_interface * PLUGIN_init_plug_realtime       (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_3ddot          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_coorder        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_compress       (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_volreg         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_drawdset       (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_maskave        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_deconvolve     (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_render         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_notes          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_histog         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_scatplot       (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_nudge          (int) ;
/** extern PLUGIN_interface * PLUGIN_init_plug_wavelets       (int) ; **/
extern PLUGIN_interface * PLUGIN_init_plug_second_dataset (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_betafit        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_zeropad        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_3Ddump_V2      (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_4Ddump         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_delay_V2       (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_extract        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_stavg          (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_reorder        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_roiedit        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_hemisub        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_maskcalc       (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_maxima         (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_fourier        (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_threshold      (int) ;
extern PLUGIN_interface * PLUGIN_init_plug_permtest       (int) ;

typedef struct {
   vptr_func *pfunc ;
   char      *pname ;
} FIXED_plugin ;

#define NUM_FIXED_plugin_funcs (sizeof(FIXED_plugin_funcs)/sizeof(FIXED_plugin)-1)

static FIXED_plugin FIXED_plugin_funcs[] = {
     { (vptr_func *) PLUGIN_init_plug_clust          , "plug_clust"          } ,
     { (vptr_func *) PLUGIN_init_plug_copy           , "plug_copy"           } ,
     { (vptr_func *) PLUGIN_init_plug_rename         , "plug_rename"         } ,
     { (vptr_func *) PLUGIN_init_plug_tag            , "plug_tag"            } ,
     { (vptr_func *) PLUGIN_init_plug_power          , "plug_power"          } ,
     { (vptr_func *) PLUGIN_init_plug_stats          , "plug_stats"          } ,
     { (vptr_func *) PLUGIN_init_plug_lsqfit         , "plug_lsqfit"         } ,
     { (vptr_func *) PLUGIN_init_plug_imreg          , "plug_imreg"          } ,
     { (vptr_func *) PLUGIN_init_plug_edit           , "plug_edit"           } ,
/**     { (vptr_func *) PLUGIN_init_plug_nlfit          , "plug_nlfit"          } , **/
     { (vptr_func *) PLUGIN_init_plug_realtime       , "plug_realtime"       } ,
     { (vptr_func *) PLUGIN_init_plug_3ddot          , "plug_3ddot"          } ,
     { (vptr_func *) PLUGIN_init_plug_coorder        , "plug_coorder"        } ,
     { (vptr_func *) PLUGIN_init_plug_compress       , "plug_compress"       } ,
     { (vptr_func *) PLUGIN_init_plug_volreg         , "plug_volreg"         } ,
     { (vptr_func *) PLUGIN_init_plug_drawdset       , "plug_drawdset"       } ,
     { (vptr_func *) PLUGIN_init_plug_maskave        , "plug_maskave"        } ,
     { (vptr_func *) PLUGIN_init_plug_deconvolve     , "plug_deconvolve"     } ,
     { (vptr_func *) PLUGIN_init_plug_render         , "plug_render"         } ,
     { (vptr_func *) PLUGIN_init_plug_notes          , "plug_notes"          } ,
     { (vptr_func *) PLUGIN_init_plug_histog         , "plug_histog"         } ,
     { (vptr_func *) PLUGIN_init_plug_scatplot       , "plug_scatplot"       } ,
     { (vptr_func *) PLUGIN_init_plug_nudge          , "plug_nudge"          } ,
/**      { (vptr_func *) PLUGIN_init_plug_wavelets       , "plug_wavelets"       } , **/
     { (vptr_func *) PLUGIN_init_plug_second_dataset , "plug_second_dataset" } ,
     { (vptr_func *) PLUGIN_init_plug_betafit        , "plug_betafit"        } ,
     { (vptr_func *) PLUGIN_init_plug_zeropad        , "plug_zeropad"        } ,
     { (vptr_func *) PLUGIN_init_plug_3Ddump_V2      , "plug_3Ddump_V2"      } ,
     { (vptr_func *) PLUGIN_init_plug_4Ddump         , "plug_4Ddump"         } ,
     { (vptr_func *) PLUGIN_init_plug_delay_V2       , "plug_delay_V2"       } ,
     { (vptr_func *) PLUGIN_init_plug_extract        , "plug_extract"        } ,
     { (vptr_func *) PLUGIN_init_plug_stavg          , "plug_stavg"          } ,
     { (vptr_func *) PLUGIN_init_plug_reorder        , "plug_reorder"        } ,
     { (vptr_func *) PLUGIN_init_plug_roiedit        , "plug_roiedit"        } ,
     { (vptr_func *) PLUGIN_init_plug_hemisub        , "plug_hemisub"        } ,
     { (vptr_func *) PLUGIN_init_plug_maskcalc       , "plug_maskcalc"       } ,
     { (vptr_func *) PLUGIN_init_plug_maxima         , "plug_maxima"         } ,
     { (vptr_func *) PLUGIN_init_plug_fourier        , "plug_fourier"        } ,
     { (vptr_func *) PLUGIN_init_plug_threshold      , "plug_threshold"      } ,
     { (vptr_func *) PLUGIN_init_plug_permtest       , "plug_permtest"       } ,

     { NULL,NULL }
} ;
