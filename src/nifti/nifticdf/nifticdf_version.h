/* NOTE:  When changing version consider the impact on versions in
  nifti2_io_version.h nifti1_io_version.h nifticdf_version.h and znzlib.h
*/
#define NIFTICDF_VERSION_MAJOR 2
#define NIFTICDF_VERSION_MINOR 1
#define NIFTICDF_VERSION_PATCH 0

/* main string macros: NIFTICDF_VERSION and NIFTICDF_SOURCE_VERSION */
#define NIFTICDF_VERSION_TO_STRING(x) NIFTICDF_VERSION_TO_STRING0(x)
#define NIFTICDF_VERSION_TO_STRING0(x) #x
#define NIFTICDF_VERSION                                 \
  NIFTICDF_VERSION_TO_STRING(NIFTICDF_VERSION_MAJOR)     \
  "." NIFTICDF_VERSION_TO_STRING(NIFTICDF_VERSION_MINOR) \
  "." NIFTICDF_VERSION_TO_STRING(NIFTICDF_VERSION_PATCH)

#define NIFTICDF_SOURCE_VERSION "NIFTICDF version " NIFTICDF_VERSION
