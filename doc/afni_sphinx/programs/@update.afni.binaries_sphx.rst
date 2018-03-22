*********************
@update.afni.binaries
*********************

.. _ahelp_@update.afni.binaries:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    ------------------------------------------------------------
    @update.afni.binaries   - upgrade AFNI binaries
    
    Update the AFNI binaries, either via '-defaults' or by the
    '-bindir' and/or '-package' options.
    
    examples:
    
        individual user: initial install or udpate
    
           The default package is linux_openmp_64 (Linux) or macosx_10.7_local
           (OS X).  The default binary directory is ~/abin.
    
           @update.afni.binaries -defaults -do_extras
    
    
        root user: initial install or udpate
    
           @update.afni.binaries -package linux_openmp_64 -bindir /usr/local/AFNIbin
           @update.afni.binaries -local_package linux_openmp_64.tgz -bindir /usr/local/AFNIbin
    
    
        other examples:
    
           @update.afni.binaries -defaults
           @update.afni.binaries -defaults -package macosx_10.7_local
           @update.afni.binaries -package linux_openmp_64
           @update.afni.binaries -package linux_openmp_64 -bindir ~/abin
           @update.afni.binaries -package linux_openmp_64 -programs suma 3dRSFC
           @update.afni.binaries -package linux_openmp_64 -programs file_tool python_scripts/*.py
    
           @update.afni.binaries -local_package macosx_10.7_local.tgz
    
    options:
    
        -help                 : show this help
        -help_sys_progs       : list system programs that block update
    
           See -sys_ok for details.
    
        -apsearch yes/no      : specify getting apsearch updates
        -bindir ABIN          : set AFNI binary directory to ABIN
        -curl                 : default to curl instead of wget
    
        -defaults             : install current package into abin
        -d                    : (short for -defaults)
    
            This would be the method to 'update the package that I
            am currently using'.
    
            This option implies -do_dotfiles and -apsearch yes.
    
            The package would be decided by 'afni -ver' and the
            directory would come from 'which afni'.  If either of
            these is not appropriate, the package would be determined
            by the OS (Linux or OSX allowed, 32 or 64-bits), and the
            install dir would be ~/abin.
    
            If -bindir or -package cannot be determined, it must be
            supplied by the user.
    
            26 Sep 2012 : -update_apsearch is applied by default
                          (if installed afni is in PATH)
    
        -do_dotfiles          : if needed, try to initialize dot files
    
            If .cshrc (or maybe .tcshrc) or .bashrc do not have the
            AFNI binary directory in the file (grep), add a line to
            update the PATH in each file.
    
            Both files are updated (if need be).
    
          * This option has no effect for the root user.
    
        -do_extras            : do extra niceties (beyond simple install)
    
            This is a convenience option that implies:
    
               -apsearch yes
               -do_dotfiles
    
        -no_cert_verify       : do not verify the server CA certificate
    
            This option is regarding SSL/TLS Certificate Verification
            via some CA (certificate authority) list.  It may be needed
            if the client CA list does not recognize the certificate
            provided by the afni server.
    
            For curl, this appends the '--insecure' option.
            For wget, this appends the '--no-check-certificate' option.
    
            To check whether curl requires this, look for WinSSL in the
            output from: curl -V
    
            See https://curl.haxx.se/docs/sslcerts.html for details.
    
        -no_recur             : do not download and run new @uab script
    
        -local_package PACKAGE : install local PACKAGE.tgz package
    
            This is a way to install an existing tgz file without needed
            to download it.
    
        -prog_list PROGRAMS   : install given programs, not whole PACKAGE
    
            With this option, the listed programs would be installed,
            rather than the entire PACKAGE.
    
            Note: directories are not allowed (e.g. meica.libs)
    
        -package PACKAGE      : install distribution package PACKAGE
                                (see also -local_package)
    
        -prog_list PROGRAMS   : install given programs, not whole PACKAGE
    
            With this option, the listed programs would be installed,
            rather than the entire PACKAGE.
    
            Note: directories are not allowed (e.g. meica.libs)
    
            For example, consider:
    
              -prog_list suma python_scripts/*.py
    
            In this case, suma and the individual python files would all
            end up in abin, with no directories.
    
        -proto PROTOCOL       : access afni host via this PROTOCOL
    
            e.g. -proto http
            default: https
    
            Use this option to specify the download protocol.  PROTOCOL may
            https, http or NONE (meaning not to prefix site name with any).
    
        -quick                : quick mode, no fancies
    
            This option blocks unwanted or unneeded actions, mostly for
            testing.  It basically applies:
    
               -no_recur
               -apsearch no
    
        -sys_ok               : OK to update, even if system progs found
    
            If any system program (e.g. man, sudo, xterm, yum) is found,
            the default behavior is not to continue the update.  Note
            that if 'afni -ver' shows a Debian package, then updates
            should be done via apt-get, not this program.
    
            Use -sys_ok to all the update to proceed.
    
            See -help_sys_progs for a list of checked system programs.
    
        -test                 : just attempt the download and quit
    
        -test_protos          : test download protocols and exit
    
        -revert               : revert binaries to previous version
    
            Revert the AFNI binaries to those in directory
            ABIN/auto_backup.PACKAGE, where ABIN would otherwise be
            considered the installation directory.
    
            Use this option if the last update of the binaries got
            you a lump of coal.
    
            There should be only 1 backup to revert to.  One cannot
            revert back 2 levels, say.
    
    Note that the user must have write permissions in the ABIN
    directory.
