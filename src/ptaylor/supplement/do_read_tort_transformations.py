import sys

RADtoDEG = 180./3.141592653589793 

def read_tortoise_transformations(my_file):
    '''
    Read in a TORTOISE (DIFFPREP-)produced *_transformations.txt file
    and output a simple text file of 6 columns in TORT order, but with 
    rot in deg:  translations (mm) and 3 rotations (deg).

    output order of columns:

        Column #1 > x (for axial data = RL)
        Column #2 > y (for axial data = AP)
        Column #3 > z (for axial data = IS)
        Column #4 > Rx
        Column #5 > Ry
        Column #6 > Rz

    Tested/used on TORTOISE v3.1; may work on transformations.txt
    files from different versions of the same software.

    '''

    fff = open( my_file ,'r')
    raw_data = fff.readlines()
    fff.close()

    data_list1 = []
    for line in raw_data:
        aa = []
        #w = line.translate(None,"[]")
        w = line.replace("[", "")
        w = w.replace("]", "")
        x = w.split(',')
        Nx = len(x)
        if Nx < 6:
            sys.exit("** Problem in this line:\n %s\n\n" % line)
        for i in range (6):
            aa.append( float(x[i]) )
        for i in range(3, 6):
            aa[i]*= RADtoDEG

        enorm = 0.
        for i in range(6):
            enorm+= aa[i]**2
        enorm = enorm**0.5 

        data_list1.append(aa+[enorm])

    return data_list1


def write_tortoise_transformations(ofile, ilist):

    Nrow = len(ilist)

    fff = open(ofile, mode='w')

    for i in range(Nrow):
        rr = ilist[i]
        for cc in rr:
            fff.write("{:15.8f}".format(cc))
        if i<Nrow-1:
            fff.write("\n")

    fff.close()

    return 0

# =========================================================================

if __name__ == "__main__":

    arg_list = sys.argv[1:]

    tort_mot = read_tortoise_transformations(arg_list[0])
    
    out      = write_tortoise_transformations(arg_list[1], tort_mot)

    print("++ Wrote out file: {}".format(arg_list[1]))

    sys.exit(0)
