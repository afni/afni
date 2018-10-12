#!/usr/bin/env python

########################################################################
## 05/2018 Justin Rajendra
## do stuff with info from bids json file or any json file

## system libraries
import sys, os, glob, subprocess, csv, re, argparse, signal, textwrap, json
import afni_base
from collections import OrderedDict

########################################################################
## functions and classes

## run some command or give error
def exec_or_error(cmd_str,error_msg="ERROR!!!"):
    """Execute subprocess.Popen(cmd_str,stdout=subprocess.PIPE,shell=True)
       Return status and shell output.
       On non 0 status print error_msg and exit.
    """
    print("")
    print(cmd_str)
    sys.stdout.flush()
    p = subprocess.Popen(cmd_str,stdout=subprocess.PIPE,shell=True)
    cmd_output = p.communicate()[0]
    print("")
    if p.returncode != 0:
        print("\n"+error_msg+"\n\n"+cmd_output)
        sys.exit(1)
    return p.returncode,cmd_output

## read json file and return python dictionary
def json_import(json_file):
    """ validate and load a json file, return dict"""
    if not os.path.isfile(json_file):
        print("\nError: "+os.path.basename(json_file)+ " not found!!\n")
        sys.exit(1)
    with open(json_file) as js:
        try:
            json_data = json.load(js,object_pairs_hook=OrderedDict)
        except:
            print("\nError: "+os.path.basename(json_file)+
                  " is not a valid json file!!\n")
            sys.exit(1)
    return json_data

########################################################################
## json info for 1 dataset with matching json file (epi and T1 for now)
class json_info_dset(object):
    def __init__(self,name):
        if type(name) is not str:
            print("\nError: "+str(name)+ " must be of type 'str'!!\n")
            sys.exit(1)

        ## namey things
        self.name = name
        self.rel_path = os.path.dirname(name)
        self.full_path_name = os.path.abspath(name)
        self.full_path = os.path.dirname(self.full_path_name)
        self.full_path_prefix = os.path.splitext(self.full_path_name)[0]
        self.file_name = os.path.basename(name)
        self.prefix = os.path.splitext(self.file_name)[0]
        self.ext = os.path.splitext(self.file_name)[1]

        ## read in and get list of fields
        self.dict = json_import(name)
        self.fields = self.dict.keys()

        ## some useful fields
        if "EchoTime" in self.fields:
            self.te_sec = self.dict["EchoTime"]
            self.te = self.te_sec * 1000
        else:
            self.te_sec = None
            self.te = None
        if "RepetitionTime" in self.fields:
            self.tr = self.dict["RepetitionTime"]
        else:
            self.tr = ""
        if "SliceTiming" in self.fields:
            self.slice_list = self.dict["SliceTiming"]
            self.slice_str = ' '.join(map(str,self.slice_list))
        else:
            self.slice_list = None
            self.slice_str = None

        ## find matching nifti and return afni_name object
        if os.path.isfile(self.full_path_prefix+".nii.gz"):
            self.nii = afni_base.afni_name(self.full_path_prefix+".nii.gz")
        elif os.path.isfile(self.full_path_prefix+".nii"):
            self.nii = afni_base.afni_name(self.full_path_prefix+".nii")
        else:
            self.nii = afni_base.afni_name("None")

        return

    def field(self,field_in):
        """ check and return field_in in a json_info.dict"""
        if field_in not in self.dict:
            return None
        else:
            return self.dict[field_in]

########################################################################
## afni_proc results info to json file
class abids_proc_out(object):
    def __init__(self,name):
        if type(name) is not str:
            print("\nError: "+str(name)+ " must be of type 'str'!!\n")
            sys.exit(1)

        ## namey things
        self.name = name
        self.rel_path = os.path.dirname(name)
        self.full_path_name = os.path.abspath(name)
        self.full_path = os.path.dirname(self.full_path_name)
        self.full_path_prefix = os.path.splitext(self.full_path_name)[0]
        self.file_name = os.path.basename(name)
        self.prefix = os.path.splitext(self.file_name)[0]
        self.ext = os.path.splitext(self.file_name)[1]

        ## read in and get list of fields
        self.dict = json_import(name)
        self.fields = self.dict.keys()

        ## some useful fields
        if "EchoTime" in self.fields:
            self.te_sec = self.dict["EchoTime"]
            self.te = self.te_sec * 1000
        else:
            self.te_sec = None
            self.te = None
        if "RepetitionTime" in self.fields:
            self.tr = self.dict["RepetitionTime"]
        else:
            self.tr = ""
        if "SliceTiming" in self.fields:
            self.slice_list = self.dict["SliceTiming"]
            self.slice_str = ' '.join(map(str,self.slice_list))
        else:
            self.slice_list = None
            self.slice_str = None

        ## find matching nifti and return afni_name object
        if os.path.isfile(self.full_path_prefix+".nii.gz"):
            self.nii = afni_base.afni_name(self.full_path_prefix+".nii.gz")
        elif os.path.isfile(self.full_path_prefix+".nii"):
            self.nii = afni_base.afni_name(self.full_path_prefix+".nii")
        else:
            self.nii = afni_base.afni_name("None")

        return