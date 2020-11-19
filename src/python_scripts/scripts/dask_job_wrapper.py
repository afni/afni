#!/usr/bin/env python
# -*- coding: utf-8 -*-

#     This module exists to wrap jobs on dask-works so that
#     the modules in the relevant jobs can be reloaded. Otherwise the
#     environment is static and new workers must be started to alter the
#     environment. Perhaps there is a cleaner way to do this but for now this
#     hacky approach shall suffice


def run(ps, delayed):
    from afni_python import construct_template_graph
    import importlib
    importlib.reload(construct_template_graph)

    task_graph = construct_template_graph.get_task_graph(ps, delayed)
    return task_graph
