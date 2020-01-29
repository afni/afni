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
  SUMA_xColBar.c)
target_include_directories(
  SUMA PUBLIC PLY ${CMAKE_CURRENT_SOURCE_DIR})
target_compile_definitions(SUMA PUBLIC SUMA_COMPILED)

# if system GLw is used it is linked against (may require a
# patch. See
# afni/src/other_builds/OS_notes.linux_fedora_25_64.txt).
# Otherwise the c files are included directly in the library
if(NOT (USE_SYSTEM_GLW))
  target_sources(SUMA PRIVATE GLw_local/GLwDrawA.c)
  target_include_directories(
    SUMA PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/GLw_local)
endif()

target_link_libraries(
  SUMA
  PUBLIC $<TARGET_NAME_IF_EXISTS:GLw::GLw>
  PRIVATE AFNI::mrix
          Motif::Motif
          ${X11_Xt_LIB}
          ${X11_LIBRARIES}
          ${X11_Xmu_LIB}
          ${X11_Xi_LIB}
          AFNI::mri
          $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
          $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
          track_tools
          segtools
          AFNI::3DEdge
          NIFTI::nifti2
          NIFTI::nifticdf)

# suma GUI:
add_afni_executable(suma SUMA_suma.c)
target_link_libraries(
  suma
  PRIVATE SUMA
          AFNI::mrix
          Motif::Motif
          ${X11_Xt_LIB}
          ${X11_LIBRARIES}
          AFNI::track_tools
          AFNI::segtools
          $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
          $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
          $<TARGET_NAME_IF_EXISTS:OpenGL::OpenGL>)

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
    volume_render
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
            $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
            $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
            $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
            $<TARGET_NAME_IF_EXISTS:OpenGL::OpenGL>
            Motif::Motif
            ${X11_Xt_LIB}
            ${X11_LIBRARIES}
            NIFTI::nifti2
            NIFTI::nifticdf)
endforeach(program)
set_target_properties(
  volume_render PROPERTIES COMPILE_FLAGS -DDO_VOLUME_MAIN)

# Targets where name matches source file name
foreach(prog_with_name_as_source prompt_popup)
  add_afni_executable(${prog_with_name_as_source}
                 ${prog_with_name_as_source}.c)
  target_link_libraries(
    ${prog_with_name_as_source}
    PRIVATE 
      SUMA
      AFNI::mrix
      Motif::Motif
      ${X11_Xt_LIB}
      ${X11_LIBRARIES}
      NIFTI::nifti2
      NIFTI::nifticdf
      $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
      $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
      $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
        $<TARGET_NAME_IF_EXISTS:OpenGL::OpenGL>)
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
            ${X11_Xt_LIB}
            ${X11_LIBRARIES}
            ${X11_Xmu_LIB}
            NIFTI::nifti2
            NIFTI::nifticdf
            $<TARGET_NAME_IF_EXISTS:OpenGL::GLU>
            $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
            $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
            $<TARGET_NAME_IF_EXISTS:OpenGL::OpenGL>)
endforeach()

# The following is a jumbled list of programs that are
# possibly accidentally excluded:
# add_afni_executable(SumaProgramTemplate SUMA_ProgramTemplate.c)
# target_link_libraries( SumaProgramTemplate PRIVATE SUMA
# AFNI::mri NIFTI::nifti2 NIFTI::nifticdf
# $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
# $<TARGET_NAME_IF_EXISTS:GLUT::GLUT> )
#
# # Source files missing add_afni_executable(resolvitivity
# SUMA_resolvitivity.c) list(APPEND # PROGRAMS
# resolvitivity) add_afni_executable(3dfilter SUMA_3dfilter.c) #
# error: too few arguments to function 'Debug_Move'
# add_afni_executable(toy_circle # SUMA_toy_circle.c
# SUMA_SurfWarp.c)

# Some other programs:
add_afni_executable(SUMA_Read_SpecFile
               SUMA_Load_Surface_Object.c)
target_link_libraries(
  SUMA_Read_SpecFile
  PRIVATE SUMA
          AFNI::mri
          NIFTI::nifti2
          NIFTI::nifticdf
          ${X11_LIBRARIES}
          $<TARGET_NAME_IF_EXISTS:OpenGL::OpenGL>
          $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>)

add_afni_executable(CompareSurfaces SUMA_compare_surfaces.c)
target_link_libraries(
  CompareSurfaces
  PRIVATE SUMA AFNI::mri NIFTI::nifti2 NIFTI::nifticdf
          $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
          $<TARGET_NAME_IF_EXISTS:OpenGL::OpenGL>)

add_afni_executable(Surf2VolCoord SUMA_Surf2VolCoord_demo.c)
target_link_libraries(
  Surf2VolCoord
  PRIVATE SUMA AFNI::mri NIFTI::nifti2 NIFTI::nifticdf
          $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>)

add_afni_executable(path_optimize SUMA_path_optimize.c
                             SUMA_SurfWarp.c)
target_link_libraries(
  path_optimize
  PRIVATE SUMA AFNI::mri NIFTI::nifti2 NIFTI::nifticdf
          ${X11_Xt_LIB} 
          $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
          $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>)

# gts dependent programs
if(GTS_FOUND)
  add_afni_library(suma_gts SUMA_gts.c SUMA_gts_insert.c)

  target_link_libraries(
    suma_gts
    PUBLIC SUMA gts GLib2::glib
    PRIVATE $<TARGET_NAME_IF_EXISTS:OpenGL::GL> AFNI::mri
            NIFTI::nifti2 NIFTI::nifticdf
            $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
            $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>)

  add_afni_executable(SurfMesh SUMA_coarsen.c)
  target_link_libraries(
    SurfMesh
    PRIVATE suma_gts AFNI::mri NIFTI::nifti2
            NIFTI::nifticdf
            $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
            $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>)

  foreach(program BrainSkin IsoSurface)
    add_afni_executable(${program} SUMA_${program}.c)
    target_link_libraries(
      ${program}
      PRIVATE suma_gts
              AFNI::mri
              NIFTI::nifti2
              NIFTI::nifticdf
              $<TARGET_NAME_IF_EXISTS:AFNI::GLUT>
              $<TARGET_NAME_IF_EXISTS:GLUT::GLUT>
              $<TARGET_NAME_IF_EXISTS:OpenGL::GL>)
  endforeach()
else()
  message(
    STATUS
      "GTS library not found - not compiling SurfMesh, BrainSkin, and IsoSurface"
  )
endif()
