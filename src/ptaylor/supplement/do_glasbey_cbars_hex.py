
# Make Glasbey colorbar, for ROIs
# git clone https://github.com/taketwo/glasbey.git



import sys
import afni_base as ab

# specify how many colors we want: as an intermediate step, N+1 are
# created, but we purge the first one because it is white.
ncolors = [16, 256, 512, 1024, 2048]

def ReadIn_CodeOut(fname, oname):
    '''Open up a file of RGB values (0..255 for each channel), and return
a string that can be directly put into AFNI's pbardefs.h.'''

    fff = open(fname, 'r')
    x = fff.readlines()
    fff.close()

    N     = len(x)
    data  = []

    data.append('''static char {}_CMD[] = {{\n'''.format(oname))
    data.append('''"{} "\n'''.format(oname))

    # skip the 0th line, which is 'white'
    for i in range(1, N):
        line = x[i]
        strs = line.split(',')
        if len(strs) != 3 :
            sys.exit("ERROR! line:\n{}\n is not RGB nums!".format(i))

        ohex = '''"#{:02x}{:02x}{:02x} "\n'''.format( int(strs[0]),
                                                      int(strs[1]),
                                                      int(strs[2]) )
        data.append(ohex)
        
    data.append('''};\n''')

    fout  = 'pbardef_' + oname
    fff   = open(fout, 'w')
    
    for x in data:
        fff.write("{}".format(x))
    fff.close()

    return data

    
# --------------------------------------------------------------------------

if __name__ == "__main__" :


    for n in ncolors:

        print("Ncolors = {}".format(n))

        nn     = n + 1
        sn     = str(n)
        snn    = str(nn)
        ncolor = snn + ' '
        ofile  = "ROI_glasbey_" + snn
        oname  = "ROI_glasbey_" + sn

        cmd = "python glasbey.py "
        cmd+= "--no-black "
        cmd+= "--format=byte " # so we can convert to hex
        cmd+= ncolor
        cmd+= ofile

        ab.simple_shell_exec(cmd)

        ddd = ReadIn_CodeOut(ofile, oname)

    print("DONE!")
