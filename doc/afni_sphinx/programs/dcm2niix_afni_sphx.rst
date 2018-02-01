*************
dcm2niix_afni
*************

.. _dcm2niix_afni:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Chris Rorden's dcm2niiX version v1.0.20170411 GCC4.8.4 (64-bit Linux)
    usage: dcm2niix_afni [options] <in_folder>
     Options :
      -1..-9 : gz compression level (1=fastest, 9=smallest)
      -b : BIDS sidecar (y/n, default n)
      -f : filename (%a=antenna  (coil) number, %c=comments, %d=description, %e echo number, %f=folder name, %i ID of patient, %j seriesInstanceUID, %k studyInstanceUID, %m=manufacturer, %n=name of patient, %p=protocol, %s=series number, %t=time, %u=acquisition number, %z sequence name; default 'nnn_%e')
      -h : show help
      -i : ignore derived, localizer and 2D images (y/n, default n)
      -t : text notes includes private patient details (y/n, default n)
      -m : merge 2D slices from same series regardless of study time, echo, coil, orientation, etc. (y/n, default n)
      -o : output directory (omit to save to input folder)
      -p : Philips precise float (not display) scaling (y/n, default y)
      -s : single file mode, do not convert other images in folder (y/n, default n)
      -t : text notes includes private patient details (y/n, default n)
      -v : verbose (n/y or 0/1/2 [no, yes, logorrheic], default 0)
      -x : crop (y/n, default n)
      -z : gz compress images (y/i/n, default y) [y=pigz, i=internal, n=no]
     Defaults file : /home/ptaylor/.dcm2nii.ini
     Examples :
      dcm2niix_afni /Users/chris/dir
      dcm2niix_afni -o /users/cr/outdir/ -z y ~/dicomdir
      dcm2niix_afni -f mystudy%s ~/dicomdir
      dcm2niix_afni -o "~/dir with spaces/dir" ~/dicomdir
