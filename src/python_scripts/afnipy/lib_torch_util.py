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
import platform

from   afnipy   import afni_base         as ab
from   afnipy   import lib_system_check  as lsc

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
  - if 'mps' available, then:
    if arch is x86_64, use 'cpu'; else, use 'mps'
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

    # what is the cpu architecture?
    SI       = lsc.SysInfo()
    cpu_arch = SI.cpu

    # text for cases below where user asked for non-cpu but will get cpu.
    msg_to_cpu = "User requested {}, ".format(dev_in)
    msg_to_cpu+= "but will use {}".format('cpu')

    # check available resources
    HAS_CUDA = torch.cuda.is_available()
    HAS_MPS  = torch.backends.mps.is_available()
    HAS_MPS += torch.backends.mps.is_built()

    if dev_in == 'auto' :
        if HAS_MPS :
            if cpu_arch == 'x86_64' :
                # don't default to mps even if available on these
                # archs; at least on Intel 64, mps led to crashing
                # (user could still choose explicitly, if desired)
                dev_out = 'cpu'
            else:
                dev_out = 'mps'
        elif HAS_CUDA :
            dev_out = 'cuda'
        else:
            dev_out = 'cpu'
        
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

# -------------------------------------------------------------------------

def set_torch_cpus(num_cpu=-1, verb=1):
    """Set how many CPUs to use. There are different ways to specify,
including the user providing a value to use (num_cpu). One can also do
nothing and just let system decide, which it will do on macOS with
regards to the count of performance cores.

Parameters
----------
num_cpu : int
    user can specify number of CPUs to use
verb : int
    verbosity level

Returns
-------
is_fail : int
    0 for success, nonzero for failure

    """

    BAD_RETURN = -1

    try : 
        # store platform system name 
        sysname = platform.system()
    except:
        ab.EP1("Failed to determine system type")
        return BAD_RETURN

    if num_cpu > 0 :
        # user-specified route

        torch.set_num_threads(num_cpu)
        # interop threads need to be specified early on in any processing
        torch.set_num_interop_threads(num_cpu)

        if verb:
            ab.IP("User opt: using {} CPU thread(s)".format(num_cpu))

        return 0

    if sysname == 'Darwin':
        # if on macOS: estimate based on number of performance cores

        # M-series chips have 4–12 performance cores; use them all.
        # torch.get_num_threads() respects PYTORCH_CPU_ALLOC_CONF if set,
        # so only override when the user has not already done so.
        n_perf_cores = _count_arm_perf_cores()
        torch.set_num_threads(n_perf_cores)

        if verb:
            ab.IP("macOS: using {} CPU thread(s)".format(n_perf_cores))

        return 0

    # default
    num_threads = torch.get_num_threads()
    if verb:
        ab.IP("Default: using {} CPU thread(s)".format(num_threads))

    return 0

def _count_arm_perf_cores() -> int:
    """Return the number of performance cores on Apple Silicon.

    Uses sysctl if available (macOS); falls back to logical CPU count.
    On M1 that's 4 P-cores; on M1 Pro/Max/Ultra it's 8–16.
    """
    try:
        import subprocess
        out = subprocess.check_output(
            ['sysctl', '-n', 'hw.perflevel0.logicalcpu'],
            stderr=subprocess.DEVNULL
        )
        return max(1, int(out.strip()))
    except Exception:
        return max(1, os.cpu_count() or 4)

# =========================================================================

if __name__ == "__main__":

    ab.IP("No examples")
