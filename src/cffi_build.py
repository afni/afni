# file "example_build.py"

# Note: we instantiate the same 'cffi.FFI' class as in the previous
# example, but call the result 'ffibuilder' now instead of 'ffi';
# this is to avoid confusion with the other 'ffi' object you get below
from pathlib import Path
import os
import re
import subprocess
from cffi import FFI
ffibuilder = FFI()


def expand_header_file(header_file,included_libs=None):
   """
   Returns text contained in header_file
   """
   if not included_libs:
      included_libs = set()
   output_vals = []
   header_file = Path(header_file)
   if not header_file.exists():
      print(header_file.name)
      header_file =  list(Path.cwd().glob('**/' + header_file.name))[0]
      print(header_file)
      
   header_text = header_file.read_text().replace('\\n',' ')


   for line in header_text.splitlines():
      if line.startswith('#include "'):
         p = re.compile('.*"(.*.h)".*')
         sub_header_file = p.findall(line)[0]
         if sub_header_file in included_libs:
            continue
         included_libs.add(sub_header_file)
         included_libs, sub_header_text = expand_header_file(sub_header_file, included_libs)
         output_vals.append(sub_header_text)
      elif line.startswith('#include <'):
         continue
      else:
         output_vals.append(line)
   out_text =  '\n'.join(output_vals)
   return included_libs, out_text 


header_file = Path('mrilib.h')
included_libs, header_text = expand_header_file(header_file)

# preprocessed aggregated header file
tempfile = Path('temp.h')
tempfile.write_text(header_text)
pp = subprocess.run('gcc -E -P {f}'.format(f= tempfile),
   shell =True,
    stdout= subprocess.PIPE, 
    stderr = subprocess.PIPE)
processed_header_text = pp.stdout.decode('utf-8')
tempfile.unlink()

# Swap out typedef values
pattern = re.compile("(typedef )?(struct(.*)? {[^|]*?})")
found_groups = pattern.findall(processed_header_text)
for group in found_groups:
   prefix = group[0]
   definition = group[1]
   if len(group) ==3:
      def_name = group[2]
   else:
      def_name = ''
   processed_header_text = processed_header_text.replace(definition,"{p}struct {name} {body}".format(p = prefix,name = def_name, body = "{ ... }"))
ffibuilder.set_source("_afni1","""
        float qmed_float     ( int , float * ) ;
    """,
    libraries=["mri"],
    library_dirs=["/Users/rwcox/cffi_test",],
)

ffibuilder.cdef(processed_header_text)

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
