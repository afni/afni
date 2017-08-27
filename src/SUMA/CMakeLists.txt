cmake_minimum_required(VERSION 2.6)
project(SUMA)

add_subdirectory(gts)
add_subdirectory(PLY)

find_package(Xm REQUIRED)
find_package(GLEW REQUIRED)
find_package(GLw REQUIRED)
find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)
find_package(Xmu REQUIRED)
find_package(Xt REQUIRED)
find_package(Xext REQUIRED)
find_package(X11 REQUIRED)
find_package(ZLIB REQUIRED)
find_package(glib 2.0 REQUIRED)

include_directories(
	${XM_INCLUDE_DIRS}
	${GLEW_INCLUDE_DIRS}
	${GLW_INCLUDE_DIRS}
	${GLUT_INCLUDE_DIR}
	${OPENGL_INCLUDE_DIRS}
	${X11_Xmu_INCLUDE_PATH}
	${X11_Xrender_INCLUDE_PATH}
	${XEX11_Xrender_INCLUDE_PATH}
	${X11_INCLUDE_DIRS}
	${ZLIB_INCLUDE_DIRS}
	${GLIB_INCLUDE_DIRS}

	$<TARGET_PROPERTY:AFNI::niml,INTERFACE_INCLUDE_DIRECTORIES>

	#-I./ -I../
	#-Igts/src
	#-I/sw/include
	${3DEDGE_INCLUDE_DIRS}
)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DHAVE_ZLIB -DSUMA_COMPILED")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DUSE_TRACING")

list(APPEND SUMA_SRCS SUMA_trackball.c SUMA_SVmanip.c SUMA_input.c
	SUMA_MiscFunc.c SUMA_IV_XYZextract.c SUMA_IV_FaceSetsextract.c
	SUMA_SurfNorm.c SUMA_DOmanip.c SUMA_Load_Surface_Object.c
	SUMA_CreateDO.c SUMA_help.c SUMA_display.c SUMA_display_b.c
	SUMA_ParseCommands.c SUMA_Engine.c SUMA_Surface_IO.c SUMA_VolData.c
	SUMA_niml.c SUMA_Color.c SUMA_GeomComp.c SUMA_SphericalMapping.c
	SUMA_DataSets.c SUMA_HomerFunc.c SUMA_xColBar.c SUMA_IsoSurfaceFunc.c
	SUMA_BrainWrap.c SUMA_global.c SUMA_ExpEval.c SUMA_SurfaceToSurface.c
	SUMA_LocalStat.c SUMA_spharm.c SUMA_dot.c SUMA_plot.c
	SUMA_volume_render.c SUMA_driver.c)

list(APPEND SUMA_SRCS SUMA_SegFunc.c SUMA_SegOpts.c SUMA_CoordMatch.c)

add_library(SUMA ${SUMA_SRCS})
target_include_directories(SUMA 
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE
    
     )
target_link_libraries(SUMA ply ${OPENGL_LIBRARIES} NIFTI::nifti2 NIFTI::nifticdf)
