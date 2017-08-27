dnl UD macros for netcdf configure


dnl Convert a string to all uppercase.
dnl
define([uppercase],
[translit($1, abcdefghijklmnopqrstuvwxyz, ABCDEFGHIJKLMNOPQRSTUVWXYZ)])

dnl
dnl Check for an m4(1) preprocessor utility.
dnl
AC_DEFUN(UD_PROG_M4,
[
    AC_CHECKING(for m4 preprocessor)
    case "${M4-unset}" in
	unset) AC_CHECK_PROGS(M4, m4 gm4, m4) ;;
	*) AC_CHECK_PROGS(M4, $M4 m4 gm4, m4) ;;
    esac
    AC_MSG_CHECKING(m4 flags)
    case "${M4FLAGS-unset}" in
	unset) M4FLAGS=-B10000 ;;
    esac
    AC_MSG_RESULT($M4FLAGS)
    AC_SUBST(M4FLAGS)
])

dnl
dnl Check for an ar(1) utility.
dnl
AC_DEFUN(UD_PROG_AR,
[
    AC_CHECKING(for ar utility)
    case "${AR-unset}" in
	unset) AC_CHECK_PROGS(AR, ar, ar) ;;
	*) AC_CHECK_PROGS(AR, $AR ar, ar) ;;
    esac
    AC_MSG_CHECKING(ar flags)
    case "${ARFLAGS-unset}" in
	unset) ARFLAGS=cru ;;
    esac
    AC_MSG_RESULT($ARFLAGS)
    AC_SUBST(ARFLAGS)
])

dnl
dnl Check for an nm(1) utility.
dnl
AC_DEFUN(UD_PROG_NM,
[
    AC_CHECKING(for nm utility)
    case "${NM-unset}" in
	unset) AC_CHECK_PROGS(NM, nm, nm) ;;
	*) AC_CHECK_PROGS(NM, $NM nm, nm) ;;
    esac
    AC_MSG_CHECKING(nm flags)
    case "${NMFLAGS-unset}" in
	unset) NMFLAGS= ;;
    esac
    AC_MSG_RESULT($NMFLAGS)
    AC_SUBST(NMFLAGS)
])

dnl
dnl Set the top-level source-directory.
dnl
AC_DEFUN(UD_SRCDIR,
[
    AC_CHECKING(for top-level source-directory)
    SRCDIR=`(cd $srcdir && pwd)`
    AC_MSG_RESULT($SRCDIR)
    AC_SUBST(SRCDIR)
])

dnl
dnl Check for a Standard C compiler.  Prefer a native one over the
dnl GNU one to reduce the chance that the environment variable LIBS
dnl will have to be set to reference the GNU C runtime library.
dnl
AC_DEFUN(UD_PROG_CC,
[
    # Because we must have a C compiler, we treat an unset CC
    # the same as an empty CC.
    case "${CC}" in
	'')
	    case `uname` in
		ULTRIX)
		    # The native ULTRIX C compiler isn't standard.
		    ccs='gcc cc'
		    ;;
		*)
		    # xlc is before c89 because AIX's sizeof(long long)
		    # differs between the two.
		    #
		    ccs='xlc c89 acc cc gcc'
		    ;;
	    esac
	    for cc in $ccs; do
		AC_CHECK_PROG(CC, $cc, $cc)
		case "$CC" in
		    '') ;;
		    *)  break
			;;
		esac
	    done
	    case "${CC}" in
		'')
		    AC_MSG_ERROR("Could not find C compiler")
		    ;;
	    esac
	    ;;
    esac
    #
    # On some systems, a discovered compiler nevertheless won't
    # work (due to licensing, for example); thus, we check the
    # compiler with a test program.
    # 
    AC_MSG_CHECKING(C compiler \"$CC\")
    AC_TRY_COMPILE(, ,
	AC_MSG_RESULT(works),
	AC_MSG_RESULT(failed to compile test program))
    AC_SUBST(CC)
    case "$CC" in
	*gcc*)
	    GCC=yes		# Expected by autoconf(1) macros
	    ;;
    esac
    case `uname -sr` in
	'HP-UX A.09'*)
	    AC_DEFINE(_HPUX_SOURCE)
	    ;;
    esac
])

dnl
dnl Check for a C++ compiler.  Prefer a native one over the
dnl GNU one to reduce the chance that the environment variable LIBS
dnl will have to be set to reference the GNU C runtime library.
dnl
AC_DEFUN(UD_PROG_CXX,
[
    case "${CXX-unset}" in
	unset)
	    case `uname` in
		AIX)
		    preferred_cxx='xlC'
		    ;;
	    esac
	    possible_cxxs="${preferred_cxx} CC cxx c++ g++ gcc"
	    ;;
	'') AC_MSG_WARN("Empty CXX variable")
	    possible_cxxs=
	    ;;
	*)  possible_cxxs=$CXX
	    ;;
    esac
    case "${possible_cxxs}" in
	'') CXX=
	    ;;
	*)  AC_LANG_SAVE()
	    AC_LANG_CPLUSPLUS()
	    for cxx in $possible_cxxs; do
		AC_CHECK_PROG(CXX, $cxx, $cxx)
		case "$CXX" in
		    '') ;;
		    *)  # On some systems, a discovered compiler nevertheless
			# won't work (because it's a script to a non-existant
			# executable, for example); thus, we check the
			# compiler with a test program.  We also test
			# for <iostream.h> and the standard C++ library
			# because we need these to work.
			# 
			AC_MSG_CHECKING(C++ compiler \"$CXX\")
			AC_TRY_RUN(
 			    [
				#include <iostream.h>
				int main() {
				    cout << "";
				    return 0;
				}
			    ],
			    [
				AC_MSG_RESULT(works)
				break
			    ],
			    [
				AC_MSG_WARN($CXX failed on test program)
				CXX=
				unset ac_cv_prog_CXX
			    ])
			;;
		esac
	    done
	    AC_LANG_RESTORE()
	    case "${CXX}" in
		'') AC_MSG_WARN("Could not find working C++ compiler")
		    AC_MSG_WARN(Setting CXX to the empty string)
		    ;;
	    esac
	    ;;
    esac
    case "${CXX}" in
	'') AC_MSG_WARN(The C++ interface will not be built)
	    ;;
    esac
    AC_SUBST(CXX)
    case `uname` in
	'HP-UX A.09'*)
	    AC_DEFINE(_HPUX_SOURCE)
	    ;;
    esac
])


dnl
dnl like AC_LONG_DOUBLE, except checks for 'long long'
dnl
AC_DEFUN(UD_C_LONG_LONG,
[AC_MSG_CHECKING(for long long)
AC_CACHE_VAL(ac_cv_c_long_long,
[if test "$GCC" = yes; then
  ac_cv_c_long_long=yes
else
AC_TRY_RUN([int main() {
long long foo = 0;
exit(sizeof(long long) < sizeof(long)); }],
ac_cv_c_long_long=yes, ac_cv_c_long_long=no, :)
fi])dnl
AC_MSG_RESULT($ac_cv_c_long_long)
if test $ac_cv_c_long_long = yes; then
  AC_DEFINE(HAVE_LONG_LONG)
fi
])

dnl UD_CHECK_SIZEOF(TYPE)
AC_DEFUN(UD_CHECK_SIZEOF,
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(sizeof_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(size of $1)
AC_CACHE_VAL(AC_CV_NAME,
[AC_TRY_RUN([#include <stdio.h>
#include <sys/types.h>
#if STDC_HEADERS
#include <stdlib.h>
#endif
main()
{
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  fprintf(f, "%d\n", sizeof($1));
  exit(0);
}], AC_CV_NAME=`cat conftestval`, AC_CV_NAME=0, AC_CV_NAME=0)])dnl
AC_MSG_RESULT($AC_CV_NAME)
AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME)
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
])


dnl 
dnl UD_CHECK_IEEE
dnl If the 'double' is not an IEEE double
dnl or the 'float' is not and IEEE single,
dnl define NO_IEEE_FLOAT
dnl
AC_DEFUN(UD_CHECK_IEEE,
[
AC_MSG_CHECKING(for IEEE floating point format)
AC_TRY_RUN([#ifndef NO_FLOAT_H
#include <float.h>
#endif

#define EXIT_NOTIEEE	1
#define EXIT_MAYBEIEEE	0

int
main()
{
#if	defined(FLT_RADIX)	&& FLT_RADIX != 2
		return EXIT_NOTIEEE;
#elif	defined(DBL_MAX_EXP)	&& DBL_MAX_EXP != 1024
		return EXIT_NOTIEEE;
#elif	defined(DBL_MANT_DIG)	&& DBL_MANT_DIG != 53
		return EXIT_NOTIEEE;
#elif 	defined(FLT_MAX_EXP)	&& !(FLT_MAX_EXP == 1024 || FLT_MAX_EXP == 128)
		return EXIT_NOTIEEE;
#elif	defined(FLT_MANT_DIG)	&& !(FLT_MANT_DIG == 53 || FLT_MANT_DIG == 24)
		return EXIT_NOTIEEE;
#else
	/* (assuming eight bit char) */
	if(sizeof(double) != 8)
		return EXIT_NOTIEEE;
	if(!(sizeof(float) == 4 || sizeof(float) == 8))
		return EXIT_NOTIEEE;

	return EXIT_MAYBEIEEE;
#endif
}],ac_cv_c_ieeefloat=yes, ac_cv_c_ieeefloat=no, :)
AC_MSG_RESULT($ac_cv_c_ieeefloat)
if test $ac_cv_c_ieeefloat = no; then
  AC_DEFINE(NO_IEEE_FLOAT)
fi
])

dnl Check for utility for generating makefile dependencies.
dnl Should only be used at the UPC.
dnl
AC_DEFUN(UD_PROG_CC_MAKEDEPEND,
[
    AC_MSG_CHECKING(how to make dependencies)
    case `uname -s` in
	IRIX*|OSF1)
	    CC_MAKEDEPEND='cc -M'
	    ;;
	SunOS)
	    case `uname -r` in
		4*)
		    CC_MAKEDEPEND='cc -M'
		    ;;
		5*|*)
		    CC_MAKEDEPEND='cc -xM'
		    ;;
	    esac
	    ;;
	ULTRIX)
	    case `uname -m` in
		RISC)
		    CC_MAKEDEPEND='cc -M'
		    ;;
		VAX)	# Can't handle prototypes in netcdf.h
		    ;;
	    esac
	    ;;
	AIX)	# Writes to .u files rather than standard out
	    ;;
	HP-UX)	# Writes escaped newlines to standard error
	    ;;
    esac
    case "${CC_MAKEDEPEND}" in
	'')
	    CC_MAKEDEPEND=false
	    ;;
    esac
    AC_MSG_RESULT($CC_MAKEDEPEND)
    AC_SUBST(CC_MAKEDEPEND)
])


dnl Check for Fortran-90 compiler.
dnl
AC_DEFUN(UD_PROG_F90,
[
    case "${F90+set}" in
	set)
	    AC_MSG_CHECKING(user-defined Fortran-90 compiler \"$F90\")
	    cat <<EOF >conftest.f90
		subroutine foo(bar)
		integer, intent(in) :: bar
		end subroutine foo
EOF
	    doit='$F90 -c ${F90FLAGS} conftest.f90'
	    if AC_TRY_EVAL(doit); then
		AC_MSG_RESULT(works)
	    else
		AC_MSG_RESULT(failed to compile test program)
		unset F90
	    fi
	    rm -f conftest.*
	    ;;
	*)
	    case "${FC+set}" in
		set)
		    F90=$FC
		    F90FLAGS="${F90FLAGS-${FFLAGS--O}}"
		    F90LIBS="${F90LIBS-${FLIBS}}"
		    cat <<EOF >conftest.f90
			program foo
			call bar(1)
			end program foo
			subroutine bar(bof)
			integer, intent(in) :: bof
			end subroutine bar
EOF
		    AC_MSG_CHECKING(\"$F90\" as Fortran-90 compiler)
		    doit='$F90 -o conftest ${F90FLAGS} conftest.f90 ${F90LIBS}'
		    if AC_TRY_EVAL(doit); then
			doit=./conftest
			if AC_TRY_EVAL(doit); then
			    AC_MSG_RESULT(works)
			else
			    AC_MSG_RESULT(failed to build executable program)
			    unset F90
			fi
		    else
			AC_MSG_RESULT(failed to build test program)
			unset F90
		    fi
		    rm -f conftest*
		    ;;
	    esac
	    case "${F90-unset}" in
		unset)
		    cat <<EOF >conftest.f90
			program foo
			call bar(1)
			end program foo
			subroutine bar(bof)
			integer, intent(in) :: bof
			end subroutine bar
EOF
		    for f90 in xlf90 f90; do
			AC_CHECK_PROG(F90, $f90, $f90)
			case "${F90}" in
			    '')
				;;
			    *)
				AC_MSG_CHECKING(Fortran-90 compiler \"$F90\")
				doit='$F90 -o conftest ${F90FLAGS} conftest.f90 ${F90LIBS}'
				if AC_TRY_EVAL(doit); then
				    doit=./conftest
				    if AC_TRY_EVAL(doit); then
					AC_MSG_RESULT(works)
					break;
				    else
					AC_MSG_RESULT(
					    failed to build executable program)
					unset F90
					unset ac_cv_prog_F90
				    fi
				else
				    AC_MSG_RESULT(failed to build test program)
				    unset F90
				    unset ac_cv_prog_F90
				fi
				;;
			esac
		    done
		    rm -f conftest*
		    case "${F90}" in
			'') AC_MSG_WARN(
			    "Could not find working Fortran-90 compiler")
			    ;;
		    esac
		    ;;
	    esac
	    ;;
    esac
    case "${F90}" in
	'')
	    AC_MSG_WARN("The Fortran-90 interface will not be built")
	    ;;
	*f90*)
	    case `uname -s` in
		IRIX*)
		    NETCDF_MOD=NETCDF.mod
		    ;;
		*)
		    NETCDF_MOD=netcdf.mod
		    ;;
	    esac
	    AC_SUBST(NETCDF_MOD)
	    ;;
    esac
    AC_SUBST(F90)
    AC_SUBST(F90FLAGS)
    AC_SUBST(F90LIBS)
])

dnl Check for Fortran-77 compiler.
dnl
AC_DEFUN(UD_PROG_FC,
[
    AC_BEFORE([UD_FORTRAN_TYPES])
    case "${FC+set}" in
	set)
	    case "$FC" in
		'')
		    AC_MSG_WARN(Fortran-77 compiler is explicitly null)
		    ;;
		*)
		    AC_MSG_CHECKING(user-defined Fortran-77 compiler \"$FC\")
		    cat <<EOF >conftest.f
                        CALL FOO
                        END
EOF
		    doit='$FC -c ${FFLAGS} conftest.f'
		    if AC_TRY_EVAL(doit); then
			AC_MSG_RESULT(works)
		    else
			AC_MSG_RESULT(failed to compile test program)
			FC=
		    fi
		    rm -f conftest.*
		    ;;
	    esac
	    ;;
	*)
	    case "${F90+set}" in
		set)
		    FC=$F90
		    FFLAGS="${FFLAGS-${F90FLAGS--O}}"
		    FLIBS="${FLIBS-${F90LIBS-}}"
		    AC_MSG_CHECKING(\"$FC\" as Fortran-77 compiler)
		    cat <<EOF >conftest.f
                        CALL FOO
                        END
EOF
		    doit='$FC -c ${FFLAGS} conftest.f'
		    if AC_TRY_EVAL(doit); then
			AC_MSG_RESULT(works)
		    else
			AC_MSG_RESULT(failed to compile test program)
			unset FC
		    fi
		    rm -f conftest.*
		    ;;
	    esac
	    case "${FC-unset}" in
		unset)
		    case `uname -sr` in
			AIX*)
			    # xlf90(1) thinks fortran/ftest.F has bad syntax.
			    forts="xlf f77"
			    ;;
			BSD/OS*|FreeBSD*)
			    forts="f77 fort77 g77"
			    ;;
			HP-UX*)
			    # f77(1) doesn't have the -L option.
			    forts=fort77
			    FLIBS=-lU77
			    ;;
			IRIX*)
			    # f90(1) can't link with c89(1)-compiled objects
			    forts=f77
			    ;;
			IRIX64*)
			    forts='f77 g77 fort77'
			    ;;
			Linux*)
			    forts="f77 fort77 g77"
			    ;;
			OSF1*)
			    # The use of f90(1) results in the following for
			    # an unknown reason (`make' works in the fortran/
			    # directory):
			    # f90 -c -I../libsrc ftest.F 
			    # Last chance handler: pc = 0xa971b8, 
			    # sp = 0x3fece0, ra = 0xa971b8
			    # Last chance handler: internal exception: unwinding
			    forts="f77"
			    ;;
			'SunOS 4'*)
			    forts='f77 g77 fort77'
			    ;;
			'SunOS 5'*)
			    # SunOS's f90(1) has problems passing a C `char'
			    # as a Fortran `integer*1' => use f77(1)
			    forts="f77"
			    ;;
			sn*|UNICOS*|unicos*)
			    forts="fort77 cf77 f77 g77 f90"
			    ;;
			*)
			    forts="xlf fort77 ghf77 f77 cf77 g77 xlf90 f90"
			    ;;
		    esac
		    for fc in $forts; do
			AC_CHECK_PROG(FC, $fc, $fc)
			case "${FC}" in
			    '')
				;;
			    *)
				#
				# On some systems, a discovered compiler
				# nevertheless won't work (due to licensing,
				# for example); thus, we check the compiler
				# with a test program.
				# 
				cat <<EOF >conftest.f
				    CALL FOO
				    END
EOF
				doit='$FC -c ${FFLAGS} conftest.f'
				if AC_TRY_EVAL(doit); then
				    break
				else
				    AC_MSG_RESULT(
					failed to compile test program)
				    unset FC
				    unset ac_cv_prog_FC
				fi
				;;
			esac
		    done
		    rm -f conftest.*
		    case "${FC}" in
			'') AC_MSG_WARN(
				"Could not find working Fortran-77 compiler")
			    ;;
		    esac
		    ;;
	    esac
	    ;;
    esac
    case "${FC}" in
	'') AC_MSG_WARN("The Fortran-77 interface will not be built")
	    ;;
    esac
    AC_SUBST(FC)
    AC_SUBST(FFLAGS)
    AC_SUBST(FLIBS)
    #
    # Set the make(1) macro for compiling a .F file.
    #
    case "${FPP-}" in
    '')
	AC_MSG_CHECKING(for Fortran .F compiler)
	AC_MSG_RESULT($COMPILE_F)
	case "${COMPILE_F-unset}" in
	unset)
	    case "${FC}" in
	    '')
		COMPILE_F=
		;;
	    *)
		AC_MSG_CHECKING(if Fortran-77 compiler handles *.F files)
		cat >conftest.h <<\EOF
#define J 1
EOF
		cat >conftest.F <<\EOF
#include "conftest.h"
#define N 5
		  real r(J,N)
		  end
EOF
		doit='$FC -o conftest ${FFLAGS} conftest.F ${FLIBS}'
		if AC_TRY_EVAL(doit); then
		    COMPILE_F='$(COMPILE.f) $(FPPFLAGS)'
		    AC_MSG_RESULT(yes)
		else
		    COMPILE_F=
		    AC_MSG_RESULT(no)
		fi
		rm -f conftest*
		;;
	    esac
	    ;;
	esac
	;;
    *)
	unset COMPILE_F
	;;
    esac
    case "${COMPILE_F-}" in
	'') UD_PROG_FPP;;
    esac
    AC_SUBST(COMPILE_F)
    FPPFLAGS=${FPPFLAGS-}
    AC_SUBST(FPPFLAGS)
])


dnl Check for Fortran preprocessor.
dnl
AC_DEFUN(UD_PROG_FPP,
[
    AC_MSG_CHECKING(for Fortran preprocessor)
    case "$FPP" in
    '')
	AC_REQUIRE([AC_PROG_CPP])
	FPP="$CPP"
	;;
    esac
    AC_MSG_RESULT($FPP)
    AC_SUBST(FPP)
])


dnl Check for a Fortran type equivalent to a netCDF type.
dnl
dnl UD_CHECK_FORTRAN_NCTYPE(forttype, possibs, nctype)
dnl
AC_DEFUN(UD_CHECK_FORTRAN_NCTYPE,
[
    AC_MSG_CHECKING(for Fortran-equivalent to netCDF \"$3\")
    for type in $2; do
	cat >conftest.f <<EOF
               $type foo
               end
EOF
	doit='$FC -c ${FFLAGS} conftest.f'
	if AC_TRY_EVAL(doit); then
	    break;
	fi
    done
    rm -f conftest.f conftest.o
    AC_DEFINE_UNQUOTED($1, $type)
    AC_MSG_RESULT($type)
    $1=$type
])


dnl Check for a Fortran type equivalent to a C type.
dnl
dnl UD_CHECK_FORTRAN_CTYPE(v3forttype, v2forttype, ctype, min, max)
dnl
AC_DEFUN(UD_CHECK_FORTRAN_CTYPE,
[
    AC_MSG_CHECKING(for Fortran-equivalent to C \"$3\")
    cat >conftest.f <<EOF
        subroutine sub(values, minval, maxval)
        implicit        none
        $2              values(5), minval, maxval
        minval = values(2)
        maxval = values(4)
        if (values(2) .ge. values(4)) then
            minval = values(4)
            maxval = values(2)
        endif
        end
EOF
    doit='$FC -c ${FFLAGS} conftest.f'
    if AC_TRY_EVAL(doit); then
	mv conftest.o conftestf.o
	cat >conftest.c <<EOF
#include <limits.h>
#include <float.h>
void main()
{
$3		values[[]] = {0, $4, 0, $5, 0};
$3		minval, maxval;
void	$FCALLSCSUB($3*, $3*, $3*);
$FCALLSCSUB(values, &minval, &maxval);
exit(!(minval == $4 && maxval == $5));
}
EOF
	doit='$CC -o conftest ${CPPFLAGS} ${CFLAGS} ${LDFLAGS} conftest.c conftestf.o ${LIBS}'
	if AC_TRY_EVAL(doit); then
	    doit=./conftest
	    if AC_TRY_EVAL(doit); then
		AC_MSG_RESULT($2)
		$1=$2
		AC_DEFINE_UNQUOTED($1,$2)
	    else
		AC_MSG_RESULT(no equivalent type)
		unset $1
	    fi
	else
	    AC_MSG_ERROR(Could not compile-and-link conftest.c and conftestf.o)
	fi
    else
	AC_MSG_ERROR(Could not compile conftest.f)
    fi
    rm -f conftest*
])


dnl Check for a Fortran data type.
dnl
dnl UD_CHECK_FORTRAN_TYPE(varname, ftypes)
dnl
AC_DEFUN(UD_CHECK_FORTRAN_TYPE,
[
    for ftype in $2; do
	AC_MSG_CHECKING(for Fortran \"$ftype\")
	cat >conftest.f <<EOF
      subroutine sub(value)
      $ftype value
      end
EOF
	doit='$FC -c ${FFLAGS} conftest.f'
	if AC_TRY_EVAL(doit); then
	    AC_MSG_RESULT(yes)
	    $1=$ftype
	    AC_DEFINE_UNQUOTED($1, $ftype)
	    break
	else
	    AC_MSG_RESULT(no)
	fi
    done
    rm -f conftest*
])


dnl Check for the name format of a Fortran-callable C routine.
dnl
dnl UD_CHECK_FCALLSCSUB
AC_DEFUN([UD_CHECK_FCALLSCSUB],
[
    AC_REQUIRE([UD_PROG_FC])
    case "$FC" in
	'') ;;
	*)
	    AC_REQUIRE([UD_PROG_NM])
	    AC_BEFORE([UD_CHECK_FORTRAN_CTYPE])
	    AC_BEFORE([UD_CHECK_CTYPE_FORTRAN])
	    AC_MSG_CHECKING(for C-equivalent to Fortran routine \"SUB\")
	    cat >conftest.f <<\EOF
              call sub()
              end
EOF
	    doit='$FC -c ${FFLAGS} conftest.f'
	    if AC_TRY_EVAL(doit); then
		FCALLSCSUB=`$NM $NMFLAGS conftest.o | awk '
		    /SUB_/{print "SUB_";exit}
		    /SUB/ {print "SUB"; exit}
		    /sub_/{print "sub_";exit}
		    /sub/ {print "sub"; exit}'`
		case "$FCALLSCSUB" in
		    '') AC_MSG_ERROR(not found)
			;;
		    *)  AC_MSG_RESULT($FCALLSCSUB)
			;;
		esac
	    else
		AC_MSG_ERROR(Could not compile conftest.f)
	    fi
	    rm -f conftest*
	    ;;
    esac
])


dnl Check for a C type equivalent to a Fortran type.
dnl
dnl UD_CHECK_CTYPE_FORTRAN(ftype, ctypes, fmacro_root)
dnl
AC_DEFUN(UD_CHECK_CTYPE_FORTRAN,
[
    cat >conftestf.f <<EOF
           $1 values(4)
           data values /-1, -2, -3, -4/
           call sub(values)
           end
EOF
    for ctype in $2; do
	AC_MSG_CHECKING(if Fortran \"$1\" is C \"$ctype\")
	cat >conftest.c <<EOF
	    void $FCALLSCSUB(values)
		$ctype values[[4]];
	    {
		exit(values[[1]] != -2 || values[[2]] != -3);
	    }
EOF
	doit='$CC -c ${CPPFLAGS} ${CFLAGS} conftest.c'
	if AC_TRY_EVAL(doit); then
	    doit='$FC ${FFLAGS} -c conftestf.f'
	    if AC_TRY_EVAL(doit); then
	        doit='$FC -o conftest ${FFLAGS} ${FLDFLAGS} conftestf.o conftest.o ${LIBS}'
	        if AC_TRY_EVAL(doit); then
		    doit=./conftest
		    if AC_TRY_EVAL(doit); then
		        AC_MSG_RESULT(yes)
		        cname=`echo $ctype | tr ' abcdefghijklmnopqrstuvwxyz' \
			    _ABCDEFGHIJKLMNOPQRSTUVWXYZ`
		        AC_DEFINE_UNQUOTED(NF_$3[]_IS_C_$cname)
		        break
		    else
		        AC_MSG_RESULT(no)
		    fi
	        else
		    AC_MSG_ERROR(Could not link conftestf.o and conftest.o)
	        fi
	    else
		AC_MSG_ERROR(Could not compile conftestf.f)
	    fi
	else
	    AC_MSG_ERROR(Could not compile conftest.c)
	fi
    done
    rm -f conftest*
])


dnl Get information about Fortran data types.
dnl
AC_DEFUN([UD_FORTRAN_TYPES],
[
    AC_REQUIRE([UD_PROG_FC])
    case "$FC" in
    '')
	;;
    *)
	AC_REQUIRE([UD_CHECK_FCALLSCSUB])
	UD_CHECK_FORTRAN_TYPE(NF_INT1_T, byte integer*1 "integer(kind(1))")
	UD_CHECK_FORTRAN_TYPE(NF_INT2_T, integer*2 "integer(kind(2))")

	case "${NF_INT1_T}" in
	    '') ;;
	    *)  UD_CHECK_CTYPE_FORTRAN($NF_INT1_T, "signed char", INT1)
		UD_CHECK_CTYPE_FORTRAN($NF_INT1_T, "short", INT1)
		UD_CHECK_CTYPE_FORTRAN($NF_INT1_T, "int", INT1)
		UD_CHECK_CTYPE_FORTRAN($NF_INT1_T, "long", INT1)
		;;
	esac
	case "${NF_INT2_T}" in
	    '') ;;
	    *)  UD_CHECK_CTYPE_FORTRAN($NF_INT2_T, short, INT2)
		UD_CHECK_CTYPE_FORTRAN($NF_INT2_T, int, INT2)
		UD_CHECK_CTYPE_FORTRAN($NF_INT2_T, long, INT2)
		;;
	esac
	UD_CHECK_CTYPE_FORTRAN(integer, int long, INT)
	UD_CHECK_CTYPE_FORTRAN(real, float double, REAL)
	UD_CHECK_CTYPE_FORTRAN(doubleprecision, double float, DOUBLEPRECISION)

	UD_CHECK_FORTRAN_NCTYPE(NCBYTE_T, byte integer*1 integer, byte)

	UD_CHECK_FORTRAN_NCTYPE(NCSHORT_T, integer*2 integer, short)
dnl	UD_CHECK_FORTRAN_CTYPE(NF_SHORT_T, $NCSHORT_T, short, SHRT_MIN, SHRT_MAX)

dnl	UD_CHECK_FORTRAN_NCTYPE(NCLONG_T, integer*4 integer, long)
dnl	UD_CHECK_FORTRAN_CTYPE(NF_INT_T, integer, int, INT_MIN, INT_MAX)

dnl	UD_CHECK_FORTRAN_NCTYPE(NCFLOAT_T, real*4 real, float)
dnl	UD_CHECK_FORTRAN_CTYPE(NF_FLOAT_T, $NCFLOAT_T, float, FLT_MIN, FLT_MAX)

dnl	UD_CHECK_FORTRAN_NCTYPE(NCDOUBLE_T, real*8 doubleprecision real, double)
dnl	UD_CHECK_FORTRAN_CTYPE(NF_DOUBLE_T, $NCDOUBLE_T, double, DBL_MIN, DBL_MAX)
	;;
    esac
])


dnl Setup for making a manual-page database.
dnl
AC_DEFUN(UD_MAKEWHATIS,
[
    #
    # NB: We always want to define WHATIS to prevent the
    # $(MANDIR)/$(WHATIS) make(1) target from being just $(MANDIR)/ and
    # conflicting with the (directory creation) target with the same name.
    #
    WHATIS=whatis
    case `uname -sr` in
	BSD/OS*|FreeBSD*)
	    # Can't generate a user-database -- only /usr/share/man/whatis.db.
	    MAKEWHATIS_CMD=
	    ;;
	'IRIX64 6.5'|'IRIX 6.5')
	    MAKEWHATIS_CMD='/usr/lib/makewhatis -M $(MANDIR) $(MANDIR)/whatis'
	    ;;
	'IRIX 6'*)
	    # Can't generate a user-database.
	    MAKEWHATIS_CMD=
	    ;;
	HP-UX*)
	    # Can't generate a user-database -- only /usr/lib/whatis.
	    MAKEWHATIS_CMD=
	    ;;
	'Linux '*)
	    # /usr/sbin/makewhatis doesn't work
	    MAKEWHATIS_CMD=
	    ;;
	ULTRIX*)
	    # Can't generate a user-database -- only /usr/lib/whatis.
	    MAKEWHATIS_CMD=
	    ;;
	*)
	    if test -r /usr/man/windex; then
		WHATIS=windex
	    fi
	    AC_CHECK_PROGS(prog, catman makewhatis /usr/lib/makewhatis)
	    case "$prog" in
		*catman*)
		    MAKEWHATIS_CMD=$prog' -w -M $(MANDIR)'
		    ;;
		*makewhatis*)
		    MAKEWHATIS_CMD=$prog' $(MANDIR)'
		    ;;
	    esac
	    ;;
    esac
    AC_SUBST(WHATIS)
    AC_SUBST(MAKEWHATIS_CMD)
    AC_MSG_CHECKING(for manual-page index command)
    AC_MSG_RESULT($MAKEWHATIS_CMD)
])


dnl Check for the math library.
dnl
AC_DEFUN(UD_CHECK_LIB_MATH,
[
    AC_CHECKING(for math library)
    case "${MATHLIB}" in
	'')
	    AC_CHECK_LIB(c, tanh, MATHLIB=,
		    AC_CHECK_LIB(m, tanh, MATHLIB=-lm, MATHLIB=))
	    ;;
	*)
	    AC_MSG_RESULT($MATHLIB (user defined))
	    ;;
    esac
    AC_SUBST(MATHLIB)
])


dnl Check for the ftruncate() function and its correct behavior.
dnl
AC_DEFUN(UD_CHECK_FTRUNCATE,
[
    AC_MSG_CHECKING(for working ftruncate())
    AC_TRY_RUN(dnl
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
main()
{
    char*	path = tmpnam(NULL);
    int		exitStatus = 1;

    if (path != NULL)
    {
	int	fd = open(path, O_RDWR | O_CREAT | O_TRUNC, 0666);

	if (fd != -1)
	{
	    if (write(fd, "0", 1) == 1)
	    {
		off_t	pos = lseek(fd, 0, SEEK_CUR);

		if (pos != (off_t)-1)
		{
		    if (ftruncate(fd, 512) != -1)
		    {
			if (pos == lseek(fd, 0, SEEK_CUR))
			{
			    if (lseek(fd, 0, SEEK_SET) == 0)
			    {
				char	buf[[512]];

				if (read(fd, buf, 512) == 512)
				    exitStatus = 0;
			    }
			}
		    }
		}
	    }
	    close(fd);
	    unlink(path);
	}
    }

    return exitStatus;
}
    ,
	AC_MSG_RESULT(yes)
	AC_DEFINE(HAVE_FTRUNCATE)
    ,
	AC_MSG_RESULT(no)
    )
])


dnl Set the binary distribution directory.
dnl
AC_DEFUN([UD_FTPBINDIR], [dnl
    AC_MSG_CHECKING([binary distribution directory])
    case ${FTPBINDIR-unset} in
	unset)
	    system=`(system) 2>/dev/null || echo dummy_system`
	    FTPBINDIR=${FTPDIR-/home/ftp}/pub/binary/$system
	    ;;
    esac
    AC_SUBST(FTPBINDIR)dnl
    AC_MSG_RESULT($FTPBINDIR)
])


dnl
dnl
dnl

dnl
dnl These headers won't get C style comments
dnl
dnl UD_CONFIG_HEADER(HEADER-TO-CREATE ...)
AC_DEFUN(UD_CONFIG_HEADER,
[define(UD_LIST_HEADER, $1)])

dnl The big finish.
dnl Just like AC_OUTPUT except it calls UD_OUTPUT_HEADER for UD_LIST_HEADER.
dnl Produce config.status, config.h, and links, and configure subdirs.
dnl UD_OUTPUT([FILE...] [, EXTRA-CMDS] [, INIT-CMDS])
define(UD_OUTPUT,
[trap '' 1 2 15
AC_CACHE_SAVE
trap 'rm -fr conftest* confdefs* core core.* *.core $ac_clean_files; exit 1' 1 2 15

test "x$prefix" = xNONE && prefix=$ac_default_prefix
# Let make expand exec_prefix.
test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

# Any assignment to VPATH causes Sun make to only execute
# the first set of double-colon rules, so remove it if not needed.
# If there is a colon in the path, we need to keep it.
if test "x$srcdir" = x.; then
changequote(, )dnl
  ac_vpsub='/^[ 	]*VPATH[ 	]*=[^:]*$/d'
changequote([, ])dnl
fi

trap 'rm -f $CONFIG_STATUS conftest*; exit 1' 1 2 15

ifdef([AC_LIST_HEADER], [DEFS=-DHAVE_CONFIG_H], [AC_OUTPUT_MAKE_DEFS()])

# Without the "./", some shells look in PATH for config.status.
: ${CONFIG_STATUS=./config.status}

echo creating $CONFIG_STATUS
rm -f $CONFIG_STATUS
cat > $CONFIG_STATUS <<EOF
#! /bin/sh
# Generated automatically by configure.
# Run this file to recreate the current configuration.
# This directory was configured as follows,
dnl hostname on some systems (SVR3.2, Linux) returns a bogus exit status,
dnl so uname gets run too.
# on host `(hostname || uname -n) 2>/dev/null | sed 1q`:
#
[#] [$]0 [$]ac_configure_args
#
# Compiler output produced by configure, useful for debugging
# configure, is in ./config.log if it exists.

changequote(, )dnl
ac_cs_usage="Usage: $CONFIG_STATUS [--recheck] [--version] [--help]"
changequote([, ])dnl
for ac_option
do
  case "[\$]ac_option" in
  -recheck | --recheck | --rechec | --reche | --rech | --rec | --re | --r)
    echo "running [\$]{CONFIG_SHELL-/bin/sh} [$]0 [$]ac_configure_args --no-create --no-recursion"
    exec [\$]{CONFIG_SHELL-/bin/sh} [$]0 [$]ac_configure_args --no-create --no-recursion ;;
  -version | --version | --versio | --versi | --vers | --ver | --ve | --v)
    echo "$CONFIG_STATUS generated by autoconf version AC_ACVERSION"
    exit 0 ;;
  -help | --help | --hel | --he | --h)
    echo "[\$]ac_cs_usage"; exit 0 ;;
  *) echo "[\$]ac_cs_usage"; exit 1 ;;
  esac
done

ac_given_srcdir=$srcdir
ifdef([AC_PROVIDE_AC_PROG_INSTALL], [ac_given_INSTALL="$INSTALL"
])dnl

changequote(<<, >>)dnl
ifdef(<<AC_LIST_HEADER>>,
<<trap 'rm -fr `echo "$1 AC_LIST_HEADER" | sed "s/:[^ ]*//g"` conftest*; exit 1' 1 2 15>>,
<<trap 'rm -fr `echo "$1" | sed "s/:[^ ]*//g"` conftest*; exit 1' 1 2 15>>)
changequote([, ])dnl
EOF
cat >> $CONFIG_STATUS <<EOF

AC_OUTPUT_FILES($1)
ifdef([UD_LIST_HEADER], [UD_OUTPUT_HEADER(UD_LIST_HEADER)])dnl
ifdef([AC_LIST_HEADER], [AC_OUTPUT_HEADER(AC_LIST_HEADER)])dnl
ifdef([AC_LIST_LINKS], [AC_OUTPUT_LINKS(AC_LIST_FILES, AC_LIST_LINKS)])dnl
ifelse([$3], , ,
[EOF
cat >> $CONFIG_STATUS <<EOF
$3
EOF
cat >> $CONFIG_STATUS <<\EOF])
$2
exit 0
EOF
chmod +x $CONFIG_STATUS
rm -fr confdefs* $ac_clean_files
test "$no_create" = yes || ${CONFIG_SHELL-/bin/sh} $CONFIG_STATUS || exit 1
dnl config.status should not do recursion.
ifdef([AC_LIST_SUBDIRS], [AC_OUTPUT_SUBDIRS(AC_LIST_SUBDIRS)])dnl
])dnl


dnl This is a subroutine of UD_OUTPUT.
dnl Acts just like AC_OUTPUT_HEADER except that no C style comments are
dnl produced.
dnl UD_OUTPUT_HEADER(HEADER-FILE...)
define(UD_OUTPUT_HEADER,
[changequote(<<, >>)dnl
# These sed commands are passed to sed as "A NAME B NAME C VALUE D", where
# NAME is the cpp macro being defined and VALUE is the value it is being given.
#
# ac_d sets the value in "#define NAME VALUE" lines.
ac_dA='s%^\([ 	]*\)#\([ 	]*define[ 	][ 	]*\)'
ac_dB='\([ 	][ 	]*\)[^ 	]*%\1#\2'
ac_dC='\3'
ac_dD='%g'
# ac_u turns "#undef NAME" with trailing blanks into "#define NAME VALUE".
ac_uA='s%^\([ 	]*\)#\([ 	]*\)undef\([ 	][ 	]*\)'
ac_uB='\([ 	]\)%\1#\2define\3'
ac_uC=' '
ac_uD='\4%g'
# ac_e turns "#undef NAME" without trailing blanks into "#define NAME VALUE".
ac_eA='s%^\([ 	]*\)#\([ 	]*\)undef\([ 	][ 	]*\)'
ac_eB='<<$>>%\1#\2define\3'
ac_eC=' '
ac_eD='%g'
changequote([, ])dnl

UDCONFIG_HEADERS=${UDCONFIG_HEADERS-"$1"}
for ac_file in .. $UDCONFIG_HEADERS; do if test "x$ac_file" != x..; then
  # Support "outfile[:infile]", defaulting infile="outfile.in".
  case "$ac_file" in
  *:*) ac_file_in=`echo "$ac_file"|sed 's%.*:%%'`
       ac_file=`echo "$ac_file"|sed 's%:.*%%'` ;;
  *) ac_file_in="${ac_file}.in" ;;
  esac

  echo udcreating $ac_file

  rm -f conftest.frag conftest.in conftest.out
  cp $ac_given_srcdir/$ac_file_in conftest.in

EOF

# Transform confdefs.h into a sed script conftest.vals that substitutes
# the proper values into config.h.in to produce config.h.  And first:
# Protect against being on the right side of a sed subst in config.status.
# Protect against being in an unquoted here document in config.status.
rm -f conftest.vals
dnl Using a here document instead of a string reduces the quoting nightmare.
dnl Putting comments in sed scripts is not portable.
cat > conftest.hdr <<\EOF
changequote(<<, >>)dnl
s/[\\&%]/\\&/g
s%[\\$`]%\\&%g
s%<<#define>> \([A-Za-z_][A-Za-z0-9_]*\) *\(.*\)%${ac_dA}\1${ac_dB}\1${ac_dC}\2${ac_dD}%gp
s%ac_d%ac_u%gp
s%ac_u%ac_e%gp
changequote([, ])dnl
EOF
sed -n -f conftest.hdr confdefs.h > conftest.vals
rm -f conftest.hdr

# Break up conftest.vals because some shells have a limit on
# the size of here documents, and old seds have small limits too.
# Maximum number of lines to put in a single here document.
ac_max_here_lines=12

rm -f conftest.tail
while :
do
  ac_lines=`grep -c . conftest.vals`
  # grep -c gives empty output for an empty file on some AIX systems.
  if test -z "$ac_lines" || test "$ac_lines" -eq 0; then break; fi
  # Write a limited-size here document to conftest.frag.
  echo '  cat > conftest.frag <<CEOF' >> $CONFIG_STATUS
  sed ${ac_max_here_lines}q conftest.vals >> $CONFIG_STATUS
  echo 'CEOF
  sed -f conftest.frag conftest.in > conftest.out
  rm -f conftest.in
  mv conftest.out conftest.in
' >> $CONFIG_STATUS
  sed 1,${ac_max_here_lines}d conftest.vals > conftest.tail
  rm -f conftest.vals
  mv conftest.tail conftest.vals
done
rm -f conftest.vals

dnl Now back to your regularly scheduled config.status.
cat >> $CONFIG_STATUS <<\EOF
  rm -f conftest.frag conftest.h
  case "$ac_file" in
  *.h|*.hh|*.H)
      echo "/* $ac_file.  Generated automatically by configure.  */" \
          > conftest.h
      ;;
  esac
  cat conftest.in >> conftest.h
  rm -f conftest.in
  if cmp -s $ac_file conftest.h 2>/dev/null; then
    echo "$ac_file is unchanged"
    rm -f conftest.h
  else
    rm -f $ac_file
    mv conftest.h $ac_file
  fi
fi; done

])
