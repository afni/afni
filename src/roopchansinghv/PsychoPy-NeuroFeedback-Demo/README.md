
# AFNI Real-Time Interface in Python

## Python scripts and modules to enable communication with AFNI's real-time data interface to facilitate neuro-feedback experiments

This project contains both code to communicate with AFNI's real-time interface via network, and example code to do simple
processing and stimulus generation, based on the received data.

The code within this package has been derived from python modules in the [AFNI](https://github.com/afni/afni) software suite,
which were repackaged and modularized to improve portability.

The communication is handled by the `afniRTI` module, and the shipping demonstration experiment is implemented using
[PsychoPy](http://www.psychopy.org).

The experiment can be run by PsychoPy running on Windows, Mac OS X, and Linux - with AFNI running on a Linux or OS X computer
sending the data (via TCP) to the computer running the experiment in PschoPy.

Stimulus initialization and experimental setup is done in the 'setupExperiment' routine, processing and modeling of AFNI data
is done in the 'compute_TR_data' routine, and updating of stimuli to deliver feedback is done by the 'runExperiment' module.

Code organization and initial version provided by J. Naegele.  Assistance with Windows compatibility provided by S. J. Fede.

