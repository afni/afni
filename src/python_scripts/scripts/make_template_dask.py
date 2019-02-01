#!/usr/bin/env python
# -*- coding: utf-8 -*-

# way to run script example
# make_template_dask.py  -ok_to_exist \
# -dsets [space_separated_list_of_datasets] \
# -init_base ~/abin/MNI152_2009_template.nii.gz \
# -dask_mode SLURM -max_workers 20

daskmode = "None"  # by default, don't use dask. Just use single computer linearly
# debugging = False # does not use dask
# debugging_localcluster = False # does not use cluster

import os
from time import sleep
from afni_python.regwrap import RegWrap
from dask import delayed
# AFNI modules

from afni_python import construct_template_graph


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
    -max_workers: maximum number of workers for process

    ---------------------------------------------
"""

ps = RegWrap('make_template_dask.py')
ps.init_opts()
ps.version()
rv = ps.get_user_opts(g_help_string)
ps.process_input()
if rv is not None: ps.ciao(1)

# show current setting for OpenMP
ps.report_omp()

# n_workers = 3

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
            n_threads = 8

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
            cluster_constraint = " "

        # any user constraints for cluster walltime limits
        if ps.cluster_walltime:
            cluster_walltime = "--time=%s" % ps.cluster_walltime
        else:
            cluster_walltime = "--time=20:00:00"
        omp_count = os.environ.get("OMP_NUM_THREADS","8")
        print("OMP_NUM_THREADS being passed to the dask cluster: %s"% omp_count)
        cluster = SLURMCluster(
            queue=cluster_queue,
            memory =  cluster_memory,
            processes=1,
            cores = n_threads,
            job_extra = [cluster_constraint, cluster_walltime],
            extra = ['--resources big_jobs=2'],
            env_extra=['export OMP_NUM_THREADS="%s"'% omp_count] 
            )

        print("starting %d workers!" % n_workers)
        cluster.start_workers(n_workers)
        client = Client(cluster)

        min_workers = 0.5 * n_workers
        while ((client.status == "running") and (len(client.scheduler_info()["workers"]) < min_workers)):
            current_pool_size = len(client.scheduler_info()["workers"])
            print("Waiting for a sufficient number of workers. Currently have {current_pool_size}, waiting for {min_workers}")
            sleep(1.0)

        using_cluster = True

        # Display dashboard address for both local and cluster clients
        print("The dashboard can be opened on this host at: %s" % client.cluster.dashboard_link)
    else:
    # LocalCluster - use multiple workers on largish computer
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
        omp_count = os.environ.get("OMP_NUM_THREADS","8")
        print("OMP_NUM_THREADS in environment or default is: %s"% omp_count)

        print("starting a localcluster of %d workers!" %n_workers) 
        client = Client(processes = False,
            n_workers= n_workers,
            threads_per_worker=n_threads,
            diagnostics_port = ps.bokeh_port,
            )
            
else:
   # dask not used, so fake delayed function. Just regular, linear system

    def delayed(fn):
        return fn

# Main:
if __name__ == '__main__':
    # for Dask, compute "graph" and execute it later. For non-Dask, just run the program
    task_graph_dict = construct_template_graph.get_task_graph(ps,delayed)
    graph_output_key = list(task_graph_dict.keys())[-1]

    # if doing anything "Dasky" (multi-tasking on cluster or multi-threaded processes)
    if (daskmode != "None"):
        # Commented out code allows one to update worker environment on the fly.
        # from afni_python import dask_job_wrapper
        # task_graph = dask_job_wrapper.run(ps,delayed,client) # useful for reloading modules

        # The following command executes the task graph and returns futures 
        if(daskmode == "SLURM"):
           template_futures = client.compute(task_graph_dict[graph_output_key],
             resources = {'big_jobs' : 1})
        else:
           template_futures = client.compute(task_graph_dict[graph_output_key])

        # This is a blocking call that waits for everything to be computed and
        # will return the results.
        result = client.gather(template_futures)
        print("Really finished making template")

    print("Finished making template")
    ps.ciao(0)

