

import afni_base as ab

ncolors = [2048] #[256, 512, 1024]

def ReadIn_CodeOut(fname, oname):
    ''' open up a file, and return a 2-tuple: (raw) data set and header.'''

    fff = open(fname, 'r')
    x = fff.readlines()
    fff.close()

    N     = len(x)
    Nout  = N - 1

    fout  = 'pbardef_' + oname
    
    data  = []

    data.append('''#define      {}_num {}\n'''.format(oname, Nout))
    data.append('''static char *{0}_name = "{0}" ;\n'''.format( oname))
    data.append('''static float {0}_data[3*{0}_num] =\n'''.format(oname))

    # skip the 0th line, which is 'white'
    for i in range(1, N):
        line = x[i]
        
        if i==1 :
            line = "{" + line
        elif i==N-1 :
            line = line.strip() + "} ;\n"

        data.append("    " + line)


    fff = open(fout, 'w')
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
        cmd+= "--format=float "
        cmd+= ncolor
        cmd+= ofile

        ab.simple_shell_exec(cmd)

        ddd = ReadIn_CodeOut(ofile, oname)
        
    print("DONE!")
