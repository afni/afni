#!/usr/bin/env python
# -*- coding: utf-8 -*-

# [PT: Mar 1, 2019] adding new opt to have an input volume to aim for
# in affx step
# 
# ======================================================================


# way to run script example
# python make_template_dask.py \
#  -dsets /dsets/*.HEAD -init_base ~/abin/MNI_2009c.nii.gz -ex_mode dry_run
#
# command working for john:
# python -m pdb make_template_dask.py -dsets /data/DSST/template_making/testdata/*.nii.gz -init_base /usr/local/apps/afni/current/linux_centos_7_64/MNI152_2009_template.nii.gz -bokeh_port 8790

daskmode = "None"  # by default, don't use dask. Just use single computer linearly

# debugging = False # does not use dask
# debugging_localcluster = False # does not use cluster
# assert(not (debugging_localcluster and debugging))

import pickle
import sys

#from shutil import which
#if not which('3dinfo'):
#    raise EnvironmentError("Is AFNI on your path?")

# debugging = True
# if debugging:
# #     # this allows the script to be run from python or ipython
# #     # with the appropriate sys.argv values
#     from pathlib import Path
#     try:
#         pickle_path = Path(__file__).with_name('template_generation_call.pickle')
#         with open(pickle_path, 'rb') as f:
#             sys.argv = pickle.load(f)
#     except:
#         data = ['/Users/rodgersleejg/data/afni/src/python_scripts/afni_python/make_template_dask.py','-ok_to_exist', '-dsets', '/Users/rodgersleejg/Documents/nih/code/template_making/running_template_making2/../testdata/sub-01_T1w.nii.gz', '-init_base', '/Users/rodgersleejg/abin/MNI152_2009_template.nii.gz']
#         with open(pickle_path, 'wb') as f:
#             pickle.dump(data, f, pickle.HIGHEST_PROTOCOL)

g_help_string = """
    ===========================================================================
    make_template_dask.py    make a template from a bunch of datasets

    This Python script iteratively aligns datasets initially to an example
    base dataset and then to each other to make a new common template.

    The program proceeds by creating progressively more refined templates:
        rigid mean
        affine mean
        nonlinear means at 5 progressively smaller neighborhood sizes

    This program was written for Dask, a parallelization scheme to allow
    programs to run on large multi-core computers or on cluster
    environments. The number of worker CPUs requested is equal to the
    number of subjects.

    ---------------------------------------------
    REQUIRED OPTIONS:

    -dsets   : names of input datasets
    -init_base   : initial base template, mostly for AC-PC
                   or similar rigid registration
    -template_name : name of new template dataset
    -dask_mode  : type of parallelization - SLURM, localcluster or None
                  (default is None)

    MAJOR OPTIONS:
    -help       : this help message
    -outdir ssss: put all output into a specific, new directory 
                  (default is iterative_template_dir)
    -overwrite  : overwrite and replace existing datasets
    -no_strip   : skip skullstripping
    -no_unifize : skip unifizing
    -no_rigid   : skip rigid alignment phase
    -ok_to_exist: skip over preexisting results to continue as quickly
                  as possibly with a restart
    -max_workers MW: maximum number of workers for process
    -aff_vol_rsz VOL: Rescale the affine step's mean to this value (>0)
    -findtypical_final: find the single-subj vol most-well aligned to ave temp
    -final_space FINSP: give a 'space' name to the final output

    ---------------------------------------------
"""

from afni_python.regwrap import RegWrap
ps = RegWrap('make_template_dask.py')
ps.init_opts()
ps.version()
rv = ps.get_user_opts(g_help_string)
ps.process_input()
if rv is not None: ps.ciao(1)
# n_workers = 3
from dask import delayed
# AFNI modules
from afni_python import dask_job_wrapper
from afni_python import construct_template_graph

daskmode = ps.daskmode

if (daskmode != "None"):
    import socket

    # SLURM cluster - use lots of workers on nodes of SLURM cluster
    if (daskmode == "SLURM"):
    # if "cn" in socket.gethostname():
    # if False:
    # parallelization library
        # TODO: generalize to other clusters
        bad_host_strings = ['felix','helix','biowulf']
        if any(pattern in socket.gethostname() for pattern in bad_host_strings):
            raise EnvironmentError("Need to run from a cluster node (possibly with sinteractive).")

        from dask_jobqueue import SLURMCluster
        from dask.distributed import Client

        # user may want to limit number of workers through option
        if ps.max_workers:
            n_workers = ps.max_workers
        # number of workers is set to number of subjects
        #  1 worker/subject
        else:
            n_workers = len(ps.dsets.parlist)

        # specify max number of threads
        if ps.max_threads:
            n_threads = ps.max_threads
        else:
            n_threads = 4

        # user may specifiy queue/partition
        if ps.cluster_queue:
            cluster_queue = ps.cluster_queue
        else:
            cluster_queue = 'norm'  # or nimh default?

       # minimum amount of memory acceptable per node
        if ps.cluster_memory:
            cluster_memory = ps.cluster_memory
        else:
            cluster_memory = '20g'

        # any user constraints for cluster nodes
        if ps.cluster_constraint:
            cluster_constraint = "--constraint=%s" % ps.cluster_constraint
        else:
            cluster_constraint = "--constraint=10g"

        # any user constraints for cluster walltime limits
        if ps.cluster_walltime:
            cluster_walltime = "--time=%s" % ps.cluster_walltime
        else:
            cluster_walltime = "--time=36:00:00"

        cluster = SLURMCluster(
            queue=cluster_queue,
            memory =  cluster_memory,
            processes=1,
            cores = 8,
            job_extra = [cluster_constraint, cluster_walltime] )

        print("starting %d workers!" % n_workers)
        cluster.start_workers(n_workers)
        # client with dummy process resources limiting threads actually used by Dask client
        # still requests hardware threads
#        client = Client(cluster, diagnostics_port = ps.bokeh_port)
        client = Client(cluster, diagnostics_port = ps.bokeh_port,threads_per_worker=1)
#         resources = {'foo':1})
        # client_with_foo = Client(processes = False,
        #    n_workers= 2,    threads_per_worker=10,    resources = {'foo':1} )
        
        # client = Client(processes = False, Diagnostics_port = ps.bokeh_port)
        using_cluster = True

    # LocalCluster - use multiple workers on largish computer
    else:
        from dask.distributed import Client, LocalCluster
        # either use user limit or some larger number of CPUs (16 by default here)
        if ps.max_workers:
            n_workers = ps.max_workers
        else:
            n_workers = 16

        if ps.max_threads:
            n_threads = ps.max_threads
        else:
            n_threads = 4

        # client with dummy process resources limiting threads actually used by Dask client
        # still requests hardware threads
        # cluster = LocalCluster(n_workers=1,threads_per_worker=2)
        client = Client(processes = False,
            n_workers= n_workers,
            threads_per_worker=n_threads,
            diagnostics_port = ps.bokeh_port,
            resources = {'foo':1}
            )
            
else:   # dask not used, so fake delayed function. Just regular, linear system

    def delayed(fn):
        return fn


        # client = Client(processes = False, Diagnostics_port = ps.bokeh_port)
        # client = Client()


# BEGIN common functions across scripts (loosely of course)

# Main:
if __name__ == '__main__':
    # freeze_support()
    # for Dask, compute "graph" and execute it later. For non-Dask, just run the program
    task_graph = construct_template_graph.get_task_graph(ps,delayed)

    # if doing anything "Dasky" (multi-tasking on cluster or multi-threaded processes)
    if (daskmode != "None"):
        # task_graph = dask_job_wrapper.run(ps,delayed,client) # useful for reloading modules

        # The following command executes the task graph that
#        affine = client.compute(task_graph)
        affine = client.compute(task_graph, threads_per_worker=1)
#                              ,resources = {'foo':1})
        
        # This is a blocking call that waits for everything to be computed and will return the results.
        result = client.gather(affine)
        print("Really finished making template")

    print("Finished making template")
    ps.ciao(0)

