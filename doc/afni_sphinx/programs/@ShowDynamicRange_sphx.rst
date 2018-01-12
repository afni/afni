.. contents:: 
    :depth: 4 

*****************
@ShowDynamicRange
*****************

.. code-block:: none

    Usage @ShowDynamicRange <afni dset>
    The script checks the dynamic range of the time series data
    at locations inside the brain.
    
    The input dataset is an epi timeseries that has just been assembled
    from your reconstructed images
    
    The output consists of the following:
    - A dataset whose prefix ends with minpercchange
      wich shows the percent signal change that an increment of 1 digitized
      value in the time series corresponds to.
    - A dataset whose prefix ends with .range
      which shows the number of discreet levels used to 
      represent the time series.
    
    The scripts output the average range and the average %change corresponding
    to a unit digitized signal
    
    To be safe, one should have a dynamic range that does not introduce noise 
    at the level of expected response differences between tasks.
    For example, if a unit step corresponds to 0.3% signal change then you may
    not be able to detect differences of comparable magnitude in the FMRI 
    response to two tasks.
    These differences may be obscured by digitization noise.
