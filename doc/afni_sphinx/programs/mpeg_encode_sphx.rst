.. _ahelp_mpeg_encode:

***********
mpeg_encode
***********

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Error:  Cannot open parameter file:  -help
    Usage:  mpeg_encode [options] param_file
    Options:
    	-stat stat_file:  append stats to stat_file
    	-quiet n:  don't report remaining time for at least n seconds
    	-realquiet:  output nothing at all if successful
    	-no_frame_summary:  suppress frame summary lines
    	-float_dct:  use more accurate floating point DCT
    	-gop gop_num:  encode only the numbered GOP
    	-combine_gops:  combine GOP files instead of encode
    	-frames first_frame last_frame:  encode only the specified frames
    	-combine_frames:  combine frame files instead of encode
    	-nice:  run slave processes nicely
    	-max_machines num_machines:  use at most num_machines machines
    	-snr:  print signal-to-noise ratio
    	-bit_rate_info rate_file:  put bit rate in specified file
