/* NOTE:  When changing version consider the impact on versions in
  nifti2_io_version.h nifti1_io_version.h nifticdf_version.h and znzlib.h
*/
#define NIFTI2_IO_VERSION_MAJOR 2
#define NIFTI2_IO_VERSION_MINOR 1
#define NIFTI2_IO_VERSION_PATCH 0

/* main string macros: NIFTI2_IO_VERSION and NIFTI2_IO_SOURCE_VERSION */
#define NIFTI2_IO_VERSION_TO_STRING(x) NIFTI2_IO_VERSION_TO_STRING0(x)
#define NIFTI2_IO_VERSION_TO_STRING0(x) #x
#define NIFTI2_IO_VERSION                                   \
   NIFTI2_IO_VERSION_TO_STRING(NIFTI2_IO_VERSION_MAJOR)     \
   "." NIFTI2_IO_VERSION_TO_STRING(NIFTI2_IO_VERSION_MINOR) \
   "." NIFTI2_IO_VERSION_TO_STRING(NIFTI2_IO_VERSION_PATCH)

#define NIFTI2_IO_SOURCE_VERSION "NIFTI2_IO version " NIFTI2_IO_VERSION
