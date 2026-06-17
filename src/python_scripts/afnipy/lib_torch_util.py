#!/usr/bin/env python

# A library of supplementary functions when working with Torch/PyTorch.
#
# As you might have guess, Torch must be importable for this to run.
#
# written by PA Taylor (NIMH, NIH, USA)
# 
# ==========================================================================

import sys, os
import torch

from   afnipy   import afni_base as ab

# ==========================================================================

# list of allowed keywords for device selection in a general case
# ('auto' is not a device, but a default keyword for letting a
# progression of conditions select)
LIST_allowed_device_general = ['auto', 'cpu', 'mps', 'cuda']
STR_allowed_device_general  = ', '.join(LIST_allowed_device_general)

# ==========================================================================

def select_device_general(dev_in='auto', verb=1):
    """Provide a str dev_in, and first see if it is a viable choice.  If
it is an explicit choice, see if it is possible on this system; or if
'auto', follow a chain of logic to try to choose one.

For list of allowed dev_in values, see: LIST_allowed_device_general.

Logic flow of 'auto' case:
+ try to use 'mps'
+ try to use 'cuda'
+ try to use 'cpu'

Parameters
----------
dev_in : str
    string to try to use/find for device

Returns
-------
is_fail : int
    0 for success, nonzero for failure
dev_out : str
    name of device to use.

    """

    BAD_RETURN = (-1, '')

    if dev_in not in LIST_allowed_device_general :
        msg = "Input {} is not in list of allowed devices:".format(dev_in)
        msg+= "{}".format(STR_allowed_device_general)
        ab.EP1(msg)
        return BAD_RETURN

    # simplest case
    if dev_in == 'cpu' :
        return 0, 'cpu'

    # text for cases below where user asked for non-cpu but will get cpu.
    msg_to_cpu = "User requested {}, ".format(dev_in)
    msg_to_cpu+= "but will use {}".format('cpu')

    # check available resources
    HAS_CUDA = torch.cuda.is_available()
    HAS_MPS  = torch.backends.mps.is_available()
    HAS_MPS += torch.backends.mps.is_built()

    if dev_in == 'auto' :
        if   HAS_MPS :   dev_out = 'mps'
        elif HAS_CUDA :  dev_out = 'cuda'
        else:            dev_out = 'cpu'
        
        if verb : ab.IP("Automatic device is: {}".format(dev_out))

        return 0, dev_out

    if dev_in == 'cuda' :
        if HAS_CUDA :  
            dev_out = 'cuda'
        else:          
            if verb : ab.WP(msg_to_cpu)
            dev_out = 'cpu'

        return 0, dev_out

    if dev_in == 'mps' :
        if HAS_MPS :  
            dev_out = 'mps'
        else:          
            if verb : ab.WP(msg_to_cpu)
            dev_out = 'cpu'

        return 0, dev_out

    # is an error to reach here
    ab.EP1("Could not find device for dev_in: {}".format(dev_in))
    return BAD_RETURN

# -------------------------------------------------------------------------

def torch_can_compile(verb=1):
    """torch.compile() traces the graph once and emits optimised machine
code.  On Apple Silicon this targets the AMX/NEON units.  Requires
PyTorch >= 2.0 and is skipped gracefully on older versions

Parameters
----------
verb : int
    leven of verbosity

Returns
-------
is_fail : int
    0 for success, nonzero for failure
can_compile : bool
    True/False answering: This device can do compiling?

    """

    CAN_COMP   = False
    BAD_RETURN = (-1, CAN_COMP)

    if hasattr(torch, 'compile'):
        msg = "torch.compile() enabled - first call will be slow"
        CAN_COMP = True
    else:
        msg = "torch.compile() not available (need PyTorch >= 2.0); skipping"
        CAN_COMP = False

    if verb : 
        ab.IP(msg)

    return 0, CAN_COMP

# =========================================================================

if __name__ == "__main__":

    ab.IP("No examples")
