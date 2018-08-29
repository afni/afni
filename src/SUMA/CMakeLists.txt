PROJECT(SUMA)

##
## Define options to customize the build-process
##

OPTION(AFNI_BUILD_LOCAL_GLW
       "Build and use AFNI's local copy of libGLw"
       OFF)

##
## Check for and configure for external dependencies
##

FIND_PACKAGE(OpenGL REQUIRED)
FIND_PACKAGE(GLUT REQUIRED)
INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})

# gts (and glib headers) are only needed for SurfMesh
INCLUDE(FindPkgConfig)
pkg_check_modules(GTS gts)
IF(GTS_FOUND)
  # Macro to turn a list into a string (why doesn't CMake have this
  # built-in?)
  MACRO (LIST_TO_STRING _string _list)
  SET (${_string})
    FOREACH (_item ${_list})
      SET (${_string} "${${_string}} ${_item}")
    ENDFOREACH (_item)
  ENDMACRO (LIST_TO_STRING)

  LIST_TO_STRING(XGTS_CFLAGS "${GTS_CFLAGS}")
  MESSAGE(STATUS "GTS library found - compiling SurfMesh")
ELSE(GTS_FOUND)
  MESSAGE(STATUS "GTS library not found - compiling without SurfMesh")
ENDIF(GTS_FOUND)

#
# libSUMA
#
SET(LIBSUMA_SRCS
  SUMA_trackball.c SUMA_SVmanip.c SUMA_input.c SUMA_MiscFunc.c
  SUMA_IV_XYZextract.c SUMA_IV_FaceSetsextract.c SUMA_SurfNorm.c
  SUMA_DOmanip.c SUMA_Load_Surface_Object.c SUMA_CreateDO.c
  SUMA_help.c SUMA_display.c SUMA_display_b.c SUMA_ParseCommands.c SUMA_Engine.c
  SUMA_Surface_IO.c SUMA_VolData.c SUMA_niml.c SUMA_Color.c SUMA_GeomComp.c
  SUMA_SphericalMapping.c SUMA_DataSets.c SUMA_HomerFunc.c SUMA_xColBar.c
  SUMA_IsoSurfaceFunc.c SUMA_BrainWrap.c SUMA_global.c SUMA_ExpEval.c
  SUMA_SurfaceToSurface.c SUMA_LocalStat.c SUMA_spharm.c
  SUMA_dot.c SUMA_plot.c SUMA_volume_render.c SUMA_SegFunc.c SUMA_SegOpts.c
  SUMA_driver.c
  PLY/plyfile.c PLY/obj_io.c MarchingCubes/MarchingCubes.c)

# add libGLw symbols if requested
IF(AFNI_BUILD_LOCAL_GLW)
  LIST(APPEND LIBSUMA_SRCS GLw_local/GLwDrawA.c)
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/GLw_local)
ELSE(AFNI_BUILD_LOCAL_GLW)
  FIND_LIBRARY(GLW_LIBRARY GLw)
ENDIF(AFNI_BUILD_LOCAL_GLW)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/PLY ${CMAKE_SOURCE_DIR}/src/avovk/)

# TODO determine what this is for
ADD_DEFINITIONS(-DSUMA_COMPILED -DnoGLwidget)

SET(SUMA_LIBS
  ${AFNI_LIBS}
  ${TRACKTOOLS_LIBS}
  SUMA
  ${THREEDEDGE_LIBS}
  ${OPENGL_LIBRARIES}
  ${GLUT_LIBRARIES}
  ${SEGTOOLS_LIBS})

ADD_LIBRARY(SUMA ${LIBSUMA_SRCS})
TARGET_LINK_LIBRARIES(SUMA
   # -lXm should be loaded before e.g. Xt which is linked to by mri etc
   -Wl,--no-as-needed ${MOTIF_LIBRARIES} -Wl,--as-needed
   ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES}
   mri tracktools ${THREEDEDGE_LIBS} segtools)
# need to link against GLw if not included in the library
IF(NOT AFNI_BUILD_LOCAL_GLW)
  TARGET_LINK_LIBRARIES(SUMA ${GLW_LIBRARY})
ENDIF(NOT AFNI_BUILD_LOCAL_GLW)

#
# Programs
#
SET(PROGRAMS
  suma prompt_user MakeColorMap ScaleToMap
  inflate_compare SurfaceMetrics ConvertSurface
  ConvertDset ROI2dataset SurfSmooth SurfPatch SurfQual SurfClust
  ConvexHull 3dSkullStrip 3dSeg 3dCRUISEtoAFNI 3dGenPriors 3dGenFeatureDist
  3dBRAIN_VOYAGERtoAFNI 3dVol2Surf SurfMeasures FSread_annot SampBias
  volume_render 3dSurfMask SurfToSurf ROIgrow AnalyzeTrace DriveSuma SurfDist
  SpharmReco SpharmDeco SurfDsetInfo SurfLocalStat SurfFWHM NikoMap SurfInfo
  3dSurf2Vol SurfRetinoMap ParseName RestSym SurfExtrema 3dinfill 3dHist
  HalloSuma InstaTract niprobe inspec quickspec CreateIcosahedron
  MapIcosahedron)

# automatically add targets for all programs following to frequently used
# naming scheme
FOREACH(program ${PROGRAMS})
  ADD_EXECUTABLE(${program} SUMA_${program}.c)
ENDFOREACH(program)

# SUMA prefixed programs or programs without SUMA_ prefix altogether
FOREACH(program SUMA_glxdino SUMA_paperplane SUMA_pixmap2eps
                SUMA_Load_Surface_Object SUMA_inflate_compare SUMA_Homer
				prompt_popup)
  ADD_EXECUTABLE(${program} ${program}.c)
  LIST(APPEND PROGRAMS ${program})
ENDFOREACH(program)

#
# Additonal targets
#
ADD_EXECUTABLE(SumaProgramTemplate SUMA_ProgramTemplate.c)
LIST(APPEND PROGRAMS SumaProgramTemplate)

ADD_EXECUTABLE(SUMA_Read_SpecFile SUMA_Load_Surface_Object.c)
LIST(APPEND PROGRAMS SUMA_Read_SpecFile)

ADD_EXECUTABLE(CompareSurfaces SUMA_compare_surfaces.c)
LIST(APPEND PROGRAMS CompareSurfaces)


# Source files missing
#ADD_EXECUTABLE(resolvitivity SUMA_resolvitivity.c)
#LIST(APPEND PROGRAMS resolvitivity)

#ADD_EXECUTABLE(3dfilter SUMA_3dfilter.c)
#LIST(APPEND PROGRAMS 3dfilter)

ADD_EXECUTABLE(Surf2VolCoord SUMA_Surf2VolCoord_demo.c)
LIST(APPEND PROGRAMS Surf2VolCoord)

#error: too few arguments to function 'Debug_Move'
#ADD_EXECUTABLE(toy_circle SUMA_toy_circle.c SUMA_SurfWarp.c)
#LIST(APPEND PROGRAMS toy_circle)

ADD_EXECUTABLE(path_optimize SUMA_path_optimize.c SUMA_SurfWarp.c)
TARGET_LINK_LIBRARIES(path_optimize ${SUMA_LIBS})
LIST(APPEND PROGRAMS path_optimize)

# Source files missing
#ADD_EXECUTABLE(spharm_test SUMA_spharm.c)
#LIST(APPEND PROGRAMS spharm_test)

IF(GTS_FOUND)
  SET(SUMAGTS_SRCS SUMA_gts.c SUMA_gts_insert.c)
  ADD_LIBRARY(SUMAGTS ${SUMAGTS_SRCS})
  TARGET_LINK_LIBRARIES(SUMAGTS ${SUMA_LIBS} ${GLUT_LIBRARIES} ${GLIB2_LIBRARIES})

  ADD_EXECUTABLE(SurfMesh SUMA_coarsen.c)
  SET_TARGET_PROPERTIES(SurfMesh PROPERTIES COMPILE_FLAGS ${XGTS_CFLAGS})
  TARGET_LINK_LIBRARIES(SurfMesh SUMAGTS ${GTS_LIBRARIES})
  LIST(APPEND PROGRAMS SurfMesh)

  FOREACH(program BrainSkin IsoSurface)
    ADD_EXECUTABLE(${program} SUMA_${program}.c)
    TARGET_LINK_LIBRARIES(${program} SUMAGTS ${GTS_LIBRARIES})
    LIST(APPEND PROGRAMS ${program})
  ENDFOREACH(program)
ENDIF(GTS_FOUND)

# all programs link against SUMA_LIBS
FOREACH(program ${PROGRAMS})
  TARGET_LINK_LIBRARIES(${program} ${SUMA_LIBS})
ENDFOREACH(program)

SET_TARGET_PROPERTIES(volume_render PROPERTIES COMPILE_FLAGS -DDO_VOLUME_MAIN)

#
# Install targets
#
INSTALL(TARGETS SUMA SUMAGTS ${PROGRAMS}
  RUNTIME DESTINATION ${AFNI_INSTALL_BIN_DIR} COMPONENT Runtime
  LIBRARY DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Runtime
  ARCHIVE DESTINATION ${AFNI_INSTALL_LIB_DIR} COMPONENT Development)
