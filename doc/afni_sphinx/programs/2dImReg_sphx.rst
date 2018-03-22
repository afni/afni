*******
2dImReg
*******

.. _ahelp_2dImReg:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    This program performs 2d image registration.  Image alignment is      
    performed on a slice-by-slice basis for the input 3d+time dataset,    
    relative to a user specified base image.                              
    
     ** Note that the script @2dwarper.Allin can do similar things, **
     ** with nonlinear (polynomial) warping on a slice-wise basis.  **
                                                                          
    Usage:                                                                
    2dImReg                                                               
    -input fname           Filename of input 3d+time dataset to process   
    -basefile fname        Filename of 3d+time dataset for base image     
                             (default = current input dataset)            
    -base num              Time index for base image  (0 <= num)          
                             (default:  num = 3)                          
    -nofine                Deactivate fine fit phase of image registration
                             (default:  fine fit is active)               
    -fine blur dxy dphi    Set fine fit parameters                        
       where:                                                             
         blur = FWHM of blurring prior to registration (in pixels)        
                   (default:  blur = 1.0)                                 
         dxy  = Convergence tolerance for translations (in pixels)        
                   (default:  dxy  = 0.07)                                
         dphi = Convergence tolerance for rotations (in degrees)          
                   (default:  dphi = 0.21)                                
                                                                          
    -prefix pname     Prefix name for output 3d+time dataset              
                                                                          
    -dprefix dname    Write files 'dname'.dx, 'dname'.dy, 'dname'.psi     
                        containing the registration parameters for each   
                        slice in chronological order.                     
                        File formats:                                     
                          'dname'.dx:    time(sec)   dx(pixels)           
                          'dname'.dy:    time(sec)   dy(pixels)           
                          'dname'.psi:   time(sec)   psi(degrees)         
    -dmm              Change dx and dy output format from pixels to mm    
                                                                          
    -rprefix rname    Write files 'rname'.oldrms and 'rname'.newrms       
                        containing the volume RMS error for the original  
                        and the registered datasets, respectively.        
                        File formats:                                     
                          'rname'.oldrms:   volume(number)   rms_error    
                          'rname'.newrms:   volume(number)   rms_error    
                                                                          
