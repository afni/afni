.. contents:: 
    :depth: 4 

*******
3dnewid
*******

.. code-block:: none

    Assigns a new ID code to a dataset; this is useful when making
    a copy of a dataset, so that the internal ID codes remain unique.
    
    Usage: 3dnewid dataset [dataset ...]
     or
           3dnewid -fun [n]
           to see what n randomly generated ID codes look like.
           (If the integer n is not present, 1 ID code is printed.)
     or
           3dnewid -fun11
           to get an 11 character ID code (for use in scripting).
     or
           3dnewid -hash STR
           to get a unique hashcode of STR
           (Unlike the other ways of using 3dnewid, if STR is the)
           (same in 2 different runs, the output will be the same.)
           (The -hash algorithm begins at step 2 in the list below.)
     or
           3dnewid -MD5 STR
           to get the MD5 hash of STR, should be same as -hash output 
           without the prefix and without the + and / char substitutions.
    
    How ID codes are created (here and in other AFNI programs):
    ----------------------------------------------------------
    The AFNI ID code generator attempts to create a globally unique
    string identifier, using the following steps.
    1) A long string is created from the system identifier
       information ('uname -a'), the current epoch time in seconds
       and microseconds, the process ID, and the number of times
       the current process has called the ID code function.
    2) This string is then hashed into a 128 bit code using the
       MD5 algorithm. (cf. file thd_md5.c)
    3) This bit code is then converted to a 22 character string
       using Base64 encoding, replacing '/' with '-' and '+' with '_'.
       With these changes, the ID code can be used as a Unix filename
       or an XML name string. (cf. file thd_base64.c)
    4) A 4 character prefix is attached at the beginning to produce
       the final ID code.  If you set the environment variable
       IDCODE_PREFIX to something, then its first 3 characters and an
       underscore will be used for the prefix of the new ID code,
       provided that the first character is alphabetic and the other
       2 alphanumeric; otherwise, the default prefix 'NIH_' will be
       used.
    The source code is function UNIQ_idcode() in file niml_uuid.c
    
    ++ Compile date = Nov  9 2017 {AFNI_17.3.03:macosx_10.7_local}
