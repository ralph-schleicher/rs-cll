dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.50])

AC_INIT([rs-cll], [20130517.1949], [rs@ralph-schleicher.de])
AM_INIT_AUTOMAKE([foreign no-define dist-bzip2])

AC_CONFIG_SRCDIR([lib/rs-cll.asd])
AC_CONFIG_FILES([Makefile lib/Makefile doc/Makefile])

dnl Optional features.
AM_MAINTAINER_MODE

ax_lisp_dir="\${datadir}/lisp"
AC_ARG_WITH([lisp-dir], AC_HELP_STRING([--with-lisp-dir=ARG], [use ARG as the top-level installation directory for Lisp files [DATADIR/lisp]]),
[case $withval in
  yes | no)
    AC_MSG_ERROR([invalid value '$withval' for --with-lisp-dir])
    ;;
  *)
    ax_lisp_dir=$withval
    ;;
esac])
AC_MSG_CHECKING([for top-level installation directory for Lisp files])
AC_MSG_RESULT([$ax_lisp_dir])
lispdir=$ax_lisp_dir
AC_SUBST([lispdir])

AC_SUBST([rs_clldir], ['$(lispdir)/$(PACKAGE_TARNAME)-$(PACKAGE_VERSION)'])

AC_SUBST([PACKAGE_NAME])
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([PACKAGE_STRING])
AC_SUBST([PACKAGE_BUGREPORT])
AC_SUBST([PACKAGE_TARNAME])

AC_OUTPUT
