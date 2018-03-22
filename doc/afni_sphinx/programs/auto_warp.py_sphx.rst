************
auto_warp.py
************

.. _ahelp_auto_warp.py:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    #++ auto_warp.py version: 0.03
    
        ===========================================================================
        auto_warp.py     - Non-linear regisration 
        
        Basic Usage:
          auto_warp.py -base TT_N27+tlrc -input anat.nii  \
                       -skull_strip_input yes
    
        ---------------------------------------------
        REQUIRED OPTIONS:
        
        -base   : name of reference or template volume
        -input  : name of dataset to be registered
        
        MAJOR OPTIONS:
    
        -help       : this help message
    
        OTHER OPTIONS:
    
        -qblur bB bS : specify 3dQwarp blurs for base and source volumes
        -qworkhard i0 i1: set the two values for 3dQwarp's -workhard option
        -qw_opts 'OPTS': Pass all of OPTS as extra options directly to 3dQwarp 
    
    A full list of options for auto_warp.py:
    
       -base               
          use:                Template volume.
       -input              
          use:                dataset to be aligned to the template
       -keep_rm_files      
          use:                Don't delete any of the temporary files created here
       -prep_only          
          use:                Do preprocessing steps only without alignment
       -help               
          use:                The main help describing this program with options
       -limited_help       
          use:                The main help without all available options
       -option_help        
          use:                Help for all available options
       -version            
          use:                Show version number and exit
       -ver                
          use:                Show version number and exit
       -verb               
          use:                Be verbose in messages and options
       -save_script        
          use:                save executed script in given file
       -skip_affine        
          use:                Skip the affine registration process
                              Equivalent to -affine_input_xmat ID 
                              (apply identity transformation)
                              
          allowed:            yes, no
          default:            no
       -skull_strip_base   
          use:                Do not skullstrip base/template dataset
          allowed:            yes, no
          default:            no
       -skull_strip_input  
          use:                Do not skullstrip input dataset
          allowed:            yes, no
          default:            no
       -ex_mode            
          use:                Command execution mode.
                              quiet: execute commands quietly
                              echo: echo commands executed
                              dry_run: only echo commands
                              
          allowed:            quiet, echo, dry_run, script
          default:            script
       -overwrite          
          use:                Overwrite existing files
       -suffix             
          default:            _al
       -child_anat         
          use:                Names of child anatomical datasets
       -qblur              
          use:                3dQwarp base and source blurs (FWHM)
                              
       -qw_opts            
          use:                3dQwarp miscellaneous options.
                              Parameters will get passed directly to 3dQwarp.
                              
       -qworkhard          
          use:                3dQwarp -workhard values
                              
          default:            [0, 1]
       -warp_dxyz          
          use:                Resolution used for computing warp (cubic only)
                              
          default:            [0.0]
       -affine_dxyz        
          use:                Resolution used for computing initial transform (cubic only)
                              
          default:            [0.0]
       -affine_input_xmat  
          use:                Affine transform to put input in standard space.
                              Special values are:
                                  'AUTO' to use @auto_tlrc
                                  'ID' to do nothing
                                  'FILE.1D' for a pre-computed matrix FILE.1D will
                                            get applied to the input before Qwarping
                              
          default:            AUTO
       -smooth_anat        
          use:                Smooth anatomy before registration
                              
       -smooth_base        
          use:                Smooth template before registration
                              
       -unifize_input      
          use:                To unifize or not unifize the input
                              
          allowed:            yes, no
          default:            y e s
       -output_dir         
          use:                Set directory for output datasets
                              
          default:            awpy
       -followers          
          use:                Specify follower datasets
                              
       -affine_followers_xmat
          use:                Specify follower datasets' affine transforms
                              
