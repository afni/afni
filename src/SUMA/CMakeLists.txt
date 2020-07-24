project(SUMA)
# SUMA library
add_afni_library(
  SUMA
  MarchingCubes/MarchingCubes.c
  PLY/obj_io.c
  PLY/plyfile.c
  SUMA_BrainWrap.c
  SUMA_Color.c
  SUMA_CreateDO.c
  SUMA_DataSets.c
  SUMA_display.c
  SUMA_display_b.c
  SUMA_DOmanip.c
  SUMA_dot.c
  SUMA_driver.c
  SUMA_Engine.c
  SUMA_ExpEval.c
  SUMA_GeomComp.c
  SUMA_global.c
  SUMA_help.c
  SUMA_HomerFunc.c
  SUMA_input.c
  SUMA_IsoSurfaceFunc.c
  SUMA_IV_FaceSetsextract.c
  SUMA_IV_XYZextract.c
  SUMA_Load_Surface_Object.c
  SUMA_LocalStat.c
  SUMA_MiscFunc.c
  SUMA_niml.c
  SUMA_ParseCommands.c
  SUMA_plot.c
  SUMA_SegFunc.c
  SUMA_SegOpts.c
  SUMA_spharm.c
  SUMA_SphericalMapping.c
  SUMA_Surface_IO.c
  SUMA_SurfaceToSurface.c
  SUMA_SurfNorm.c
  SUMA_SVmanip.c
  SUMA_trackball.c
  SUMA_VolData.c
  SUMA_volume_render.c
  SUMA_xColBar.c
)
target_include_directories(
  SUMA PUBLIC PLY ${CMAKE_CURRENT_SOURCE_DIR}
)
target_compile_definitions(SUMA PUBLIC SUMA_COMPILED)

# if system GLw is used it is linked against (may require a patch. See
# afni/src/other_builds/OS_notes.linux_fedora_25_64.txt). Otherwise
# the c files are included directly in the library
if(NOT (USE_SYSTEM_GLW))
  target_sources(SUMA PRIVATE GLw_local/GL/GLwDrawA.c)
  target_include_directories(
    SUMA PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/GLw_local
  )
endif()

target_link_libraries(
  SUMA
  PUBLIC $<TARGET_NAME_IF_EXISTS:GLw::GLw>
          AFNI::track_tools
  PRIVATE AFNI::mrix
          $<IF:$<NOT:$<BOOL:${APPLE}>>,$<TARGET_NAME_IF_EXISTS:AFNI::GLUT>,>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
          $<TARGET_NAME_IF_EXISTS:XQuartzGL::GLUT>
          $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
          $<TARGET_NAME_IF_EXISTS:XQuartzGL::GLU>
          segtools
          $<IF:$<BOOL:${APPLE}>,X11::Xmu,>
          AFNI::3DEdge
          NIFTI::nifti2
          NIFTI::nifticdf
)

# suma GUI:
add_afni_executable(suma SUMA_suma.c)
target_link_libraries(
  suma
  PRIVATE SUMA
          AFNI::mrix
          AFNI::segtools
          $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
          $<TARGET_NAME_IF_EXISTS:XQuartzGL::GLU>
)

# Add build time check for correct linking in suma gui.
# For further information see:
# https://github.com/afni/afni/issues/131
# https://github.com/afni/afni/issues/158
if(NOT APPLE)
# check for X linking error that occurs on some linux systems
check_suma_binary()
endif()


# Programs (for which the source files have SUMA_ prepended)
set(PROGRAMS
    prompt_user
    MakeColorMap
    ScaleToMap
    inflate_compare
    ConvertDset
    ROI2dataset
    SurfSmooth
    SurfPatch
    SurfQual
    SurfClust
    ConvexHull
    3dSkullStrip
    3dSeg
    3dCRUISEtoAFNI
    3dGenPriors
    3dGenFeatureDist
    3dBRAIN_VOYAGERtoAFNI
    3dVol2Surf
    SurfMeasures
    FSread_annot
    SampBias
    3dSurfMask
    SurfToSurf
    ROIgrow
    AnalyzeTrace
    DriveSuma
    SurfDist
    SpharmReco
    SpharmDeco
    SurfDsetInfo
    SurfLocalStat
    SurfFWHM
    NikoMap
    SurfInfo
    3dSurf2Vol
    SurfRetinoMap
    ParseName
    RestSym
    SurfExtrema
    3dinfill
    HalloSuma
    InstaTract
    niprobe
    inspec
    quickspec
    CreateIcosahedron
    MapIcosahedron
    3dHist
    SurfaceMetrics
    ConvertSurface
    ProgramTemplate
    # Load_Surface_Object Homer
)
foreach(program ${PROGRAMS})
  add_afni_executable(${program} SUMA_${program}.c)

  target_link_libraries(
    ${program}
    PRIVATE SUMA
            AFNI::mrix
            AFNI::track_tools
            AFNI::segtools
            NIFTI::nifti2
            NIFTI::nifticdf
  )
endforeach(program)

add_afni_executable(volume_render SUMA_volume_render.c)
target_link_libraries(
  volume_render
  PRIVATE SUMA AFNI::mrix $<TARGET_NAME_IF_EXISTS:XQuartzGL::GLU>
          $<TARGET_NAME_IF_EXISTS:OpenGL::GLU> NIFTI::nifti2
          NIFTI::nifticdf
)
set_target_properties(
  volume_render PROPERTIES COMPILE_FLAGS -DDO_VOLUME_MAIN
)

# Targets where name matches source file name
foreach(prog_with_name_as_source prompt_popup)
  add_afni_executable(
    ${prog_with_name_as_source} ${prog_with_name_as_source}.c
  )
  target_link_libraries(
    ${prog_with_name_as_source}
    PRIVATE SUMA
            AFNI::mrix
            Motif::Motif
            NIFTI::nifti2
            NIFTI::nifticdf
  )
endforeach()

# targets with suma prepended
foreach(suma_target pixmap2eps;glxdino;paperplane)
  # the following might be desired for paperplane:
  # add_definitions(-DSUMA_COMPILED -DnoGLwidget)
  set(target_with_suma SUMA_${suma_target})
  add_afni_executable(${target_with_suma} ${target_with_suma}.c)

  target_link_libraries(
    ${target_with_suma}
    PRIVATE SUMA
            AFNI::mrix
            Motif::Motif
            X11::Xmu
            NIFTI::nifti2
            NIFTI::nifticdf
            $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
            $<TARGET_NAME_IF_EXISTS:XQuartzGL::GLU>
  )
endforeach()

# Some other programs:
add_afni_executable(SUMA_Read_SpecFile SUMA_Load_Surface_Object.c)
target_link_libraries(
  SUMA_Read_SpecFile PRIVATE SUMA AFNI::mrix NIFTI::nifti2
                             NIFTI::nifticdf
)

add_afni_executable(CompareSurfaces SUMA_compare_surfaces.c)
target_link_libraries(
  CompareSurfaces PRIVATE SUMA AFNI::mrix NIFTI::nifti2
                          NIFTI::nifticdf
)

add_afni_executable(Surf2VolCoord SUMA_Surf2VolCoord_demo.c)
target_link_libraries(
  Surf2VolCoord PRIVATE SUMA AFNI::mrix NIFTI::nifti2 NIFTI::nifticdf
)

add_afni_executable(
  path_optimize SUMA_path_optimize.c SUMA_SurfWarp.c
)
target_link_libraries(
  path_optimize PRIVATE SUMA AFNI::mrix NIFTI::nifti2
                        NIFTI::nifticdf
)

# gts dependent programs
if(TARGET gts)
  add_afni_library(suma_gts SUMA_gts.c SUMA_gts_insert.c)

  target_link_libraries(
    suma_gts
    PUBLIC SUMA gts GLib2::glib
    PRIVATE AFNI::mrix NIFTI::nifti2 NIFTI::nifticdf
  )

  add_afni_executable(SurfMesh SUMA_coarsen.c)
  target_link_libraries(
    SurfMesh PRIVATE suma_gts AFNI::mrix NIFTI::nifti2 NIFTI::nifticdf
  )

  foreach(program BrainSkin IsoSurface)
    add_afni_executable(${program} SUMA_${program}.c)
    target_link_libraries(
      ${program} PRIVATE suma_gts AFNI::mrix NIFTI::nifti2
                         NIFTI::nifticdf
    )
  endforeach()
else()
  message(
    STATUS
      "GTS library not found - not compiling SurfMesh, BrainSkin, and IsoSurface"
  )
endif()
