# This module attempts to streamline how AFNI's cmake build system handles system
# dependencies.
#
# The two most frequent use cases expected for the build are to try to build using ever
# system dependency possible and to try to minimize the use of system dependencies.
# Building while using all system dependencies (resolving all system dependencies on
# a specific platform) is likely to be used for releasing an AFNI package on a specific
# OS. Building while attempting to minimize the usage of system dependencies allows the
# creation of somewhat self contained bundles that can be distributed to users and
# hopefully minimize issues with missing/poorly specified dependencies.  The two
# use-cases described (controlled by setting the cmake cache variable USE_SYSTEM_ALL to
# "ON" or "OFF") can have various gotchas.
#
# We will consider the GLW C library as an example of the sort of issues we are
# attempting to resolve here... This software made certain variables global for
# downsteam users to use. AFNI uses these variables (widget drawing classes) but at some
# point (8 years ago) the symbol visibility was changed. Now, while neurodebian
# distributes a version of GLW with these widget variables once more externally visible,
# no other operating system package manager has been found that does this. The solution
# (a stopgap?), is to build glw ourselves. Until something changes our build will have
# to use our own build of glw. Actually we directly incorporate the .c files into
# libSUMA.so but hopefully the example is instructive.
#
# Just as the idiosyncratic handling of glw occurs here, this module will attempt to
# keep track of all combinations of OS and system dependencies that are broken in order
# to automate certain choices for dependency usage and to minimize confusion when
# a "broken" configuration is explicitly requested by the user.

if(USE_SYSTEM_GLW)
    message(WARNING "Using the system dependency GLW is not advised. It is typically incompatible with the AFNI build")
else()
    set(USE_SYSTEM_GLW OFF)
endif()

if(DEFINED USE_SYSTEM_NETCDF AND NOT USE_SYSTEM_NETCDF)
    message(WARNING "Building netcdf is not advised due to the large number of required
    dependencies. Consider using a system installation of netcdf provided by your
    favorite package manager (e.g. on Ubuntu: 'apt install libnetcdf-dev'")
else()
    set(USE_SYSTEM_NETCDF ON)
endif()

if(USE_SYSTEM_VOLPACK AND APPLE)
    message(WARNING [=[On OSX volpack is a little more difficult to install yourself,
    consider setting USE_SYSTEM_VOLPACK to 'OFF']=])
endif()

