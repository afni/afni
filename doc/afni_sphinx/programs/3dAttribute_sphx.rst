***********
3dAttribute
***********

.. _ahelp_3dAttribute:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 3dAttribute [options] aname dset
    Prints (to stdout) the value of the attribute 'aname' from
    the header of dataset 'dset'.  If the attribute doesn't exist,
    prints nothing and sets the exit status to 1.
    
    Options:
      -name = Include attribute name in printout
      -all  = Print all attributes [don't put aname on command line]
              Also implies '-name'.  Attributes print in whatever order
              they are in the .HEAD file, one per line.  You may want
              to do '3dAttribute -all elvis+orig | sort' to get them
              in alphabetical order.
      -center = Center of volume in RAI coordinates.
                Note that center is not itself an attribute in the 
               .HEAD file. It is calculated from other attributes.
      Special options for string attributes:
        -ssep SSEP    Use string SSEP as a separator between strings for
                      multiple sub-bricks. The default is '~', which is what
                      is used internally in AFNI's .HEAD file. For tcsh,
                      I recommend ' ' which makes parsing easy, assuming each
                      individual string contains no spaces to begin with.
                      Try -ssep 'NUM'
        -sprep SPREP  Use string SPREP to replace blank space in string 
                      attributes.
        -quote        Use single quote around each string.
        Examples:
           3dAttribute -quote -ssep ' '  BRICK_LABS SomeStatDset+tlrc.BRIK
           3dAttribute -quote -ssep 'NUM' -sprep '+' BRICK_LABS SomeStatDset+tlrc.BRIK
    
    ++ Compile date = Mar  7 2018 {AFNI_18.0.25:linux_ubuntu_12_64}
