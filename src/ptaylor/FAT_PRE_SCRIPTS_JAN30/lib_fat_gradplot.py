import numpy as np
import sys as sys

import scipy as sp             # Has to be 0.18!!!
import scipy.spatial as spsp
import lib_voronoi_reddy as vu # Some functions for spherical area
                               # calculations for polygons, but using
                               # official ver for the Spher Vor calc
                               # now; these functions by TJ Reddy

import matplotlib.pyplot as plt
#from matplotlib.patches import Polygon
import matplotlib.colors as clr

from mpl_toolkits.basemap import Basemap
from mpl_toolkits.mplot3d.art3d import Text3D
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import colorsys as csys

import sklearn.cluster as sc

EPS_rad = 0.001   # min length of rad to calc angle
RADtoANG = 180/np.pi
SIG = 3
SIG2 = SIG*SIG
FAC_denom = 0.5/np.pi/SIG2
TOL = np.exp(-0.5)
BIG_VAL = 10.0**8

h_lat = 0.25
h_lon = h_lat

axview = [ [-90,95], [-45,210], [-165,210], [-285,210]]
axview_lab = [ 'I.', 'II.', 'III.', 'IV.']

# =====================================================================

#draw a vector in 3D Axes: from stackoverflow:
#http://stackoverflow.com/questions/11140163/python-matplotlib-plotting-a-3d-cube-a-sphere-and-a-vector
from matplotlib.patches import FancyArrowPatch
from mpl_toolkits.mplot3d import proj3d

class Arrow3D(FancyArrowPatch):
    def __init__(self, xs, ys, zs, *args, **kwargs):
        FancyArrowPatch.__init__(self, (0,0), (0,0), *args, **kwargs)
        self._verts3d = xs, ys, zs

    def draw(self, renderer):
        xs3d, ys3d, zs3d = self._verts3d
        xs, ys, zs = proj3d.proj_transform(xs3d, ys3d, zs3d, renderer.M)
        self.set_positions((xs[0],ys[0]),(xs[1],ys[1]))
        FancyArrowPatch.draw(self, renderer)

aar   = np.array( [-1.6, 1.6] )
aup   = np.array( [ 0, 0] )
adown = np.array( [ 0, 0] )

px = [[aar, adown, aup], 
      [aar, aup, aup],
      [aar, aup, aup], 
      [aar, adown, aup]]
py = [[adown, aar, aup], 
      [adown, aar, aup], 
      [aup, aar, aup], 
      [adown, aar, aup]]
pz = [[adown, adown, aar], 
      [adown, adown, aar],
      [aup, adown, aar], 
      [adown, aup, aar]]


# =====================================================================

def Proc_N_Print( X, R, file_prefix, 
                  FS, DO_PLOT, FTYPE, MATDPI, DO_hold_image,
                  BMS=(0,0),
                  Rlist_OI=np.zeros(None)):
    '''Input:
    X: 4xN array (col 0 is magnitudes; cols 1,2,3 are unit vec comps),
    fff: filename for parsing,
    and awaaaay we go.'''

    Nrefs, Nr = np.shape(R)

    Ngrad, Ny = np.shape(X)
    print "\t Number of grads      :", Ngrad
    my_grads_RPT    = All_CtS( X[:,1:] )
    my_grads_RPTneg = All_CtS(-X[:,1:] )

    Bmean, Bstd = BMS[:]
     
    # arrays and meshgrid for output
    my_lon, my_lat, x, y = Make_LongLat_Arr()
    my_vals    = Calc_My_Vals(my_lon, my_lat, my_grads_RPT)
    my_valsneg = Calc_My_Vals(my_lon, my_lat, my_grads_RPTneg)

    # ---------------------------------------------------------------------
    # Generic background/underlaying grid stuff.

    # Make the underlying map:
    # robinson ('robin'), eckert IV ('eck4'), Kavrayskiy VII ('kav7'):
    # 'hammer', 'mbtfpq', 'sinu', 
    # pretty good; can do 'cyl', 'gall', 'mill' for rect proj
    # see: http://matplotlib.org/basemap/users/mapsetup.html
    my_proj = 'eck4'
    my_map = Basemap(projection=my_proj,lon_0=0, lat_0=0 )

    mapimg = plt.figure(file_prefix)
    mapimg.set_size_inches(8,4)
    my_map.drawparallels( np.arange(-90,90+30,30),
                          labelstyle="+/-",
                          labels=[1,0,0,0],
                          color='silver',
                          fontsize = FS)
    # labelled meridians
    my_merids = np.arange(-180,180+60,60)
    my_map.drawmeridians( my_merids,
                          labelstyle="+/-",
                          labels=[0,0,0,1],
                          color='silver',
                          fontsize = FS)
    # non-labelled ones
    my_merids2 = np.arange(-150,180+60,60)
    my_map.drawmeridians( my_merids2,
                          labelstyle="+/-",
                          labels=[0,0,0,0],
                          color='silver')
    my_map.drawmapboundary() 

    if Bmean < 10:
        info_str = 'b-val: %.2f $\pm$ %.2f' % (Bmean, Bstd)
    else:
        info_str = 'b-val: %.0f $\pm$ %.0f' % (Bmean, Bstd)
    plt.title('Gradient Projection ('+info_str+')', fontsize= FS+1) # add a title

    # ----------------------------------------------------

    # color-related levels and things
    N_maxlev = int(np.max(my_vals))+1
    MAP_of_COL  = MakeMyBar('wgap_jet',N_maxlev) #plt.cm.YlOrRd
    MAP_of_COL2 = plt.cm.gray_r #plt.cm.Blues 
    v = np.linspace(0.5,N_maxlev-0.5,N_maxlev, endpoint=True)
    v2 = np.linspace(1,N_maxlev-1,N_maxlev-1, endpoint=True)

    csneg = my_map.contourf( x,
                             y, 
                             my_valsneg,  
                             v, #N_maxlev   !!! change??
                             latlon=True, 
                             cmap= MAP_of_COL2,
                             vmin=0,
                             vmax=N_maxlev)
    
    cs = my_map.contourf( x,
                          y, 
                          my_vals,  
                          v, #N_maxlev
                          latlon=True, 
                          cmap= MAP_of_COL,
                          vmin=0,
                          vmax=N_maxlev)

    cb = plt.colorbar(cs,cmap=MAP_of_COL,ticks=v2, fraction=0.01)
    cb2 = plt.colorbar(csneg,cmap=MAP_of_COL2,ticks=v2, fraction=0.01)

    if Nrefs: #file_refset:

        Ngrad, Ny = np.shape(X)
        my_refs_RPT    = All_CtS( R[:,1:] )
        my_refs_RPTneg = All_CtS(-R[:,1:] )
        my_refvals     = Calc_My_Vals(my_lon, my_lat, my_refs_RPT)
        my_refvals_neg = Calc_My_Vals(my_lon, my_lat, my_refs_RPTneg)

        cs_ref = my_map.contour( x,
                                 y, 
                                 my_refvals,
                                 1,
                                 latlon=True, 
                                 colors='k',
                                 linestyles = 'solid')
        cs_ref = my_map.contour( x,
                                 y, 
                                 my_refvals_neg,
                                 1,
                                 latlon=True, 
                                 colors='k',
                                 linestyles = 'dashed')
        for c in cs_ref.collections:
            c.set_dashes([(0, (2.0, 2.0))])

        # ====================== labels

        if not(np.shape(Rlist_OI)):
            my_val_labels = list(np.arange(Ngrad)+1)
        else:
            my_val_labels = list(Rlist_OI)
        my_val_labels_neg = list(-np.array(my_val_labels))

        for gg in range(len(my_val_labels)):
            print "gg = %d" % gg

            #x, y = np.meshgrid( np.exp(-0.5/SIG2*((T-G[k][1]-180) % 360 - 180 )**2),
            #                    np.exp(-0.5/SIG2*( P+90-G[k][2]  )**2)) 
            a,b = (my_refs_RPT[gg][1] + 180) % 360 -180, my_refs_RPT[gg][2]-90
            myx, myy = my_map(a,b)

            print " (%.0f, %0f)---> (%.0f, %0f)" % (a,b, myx, myy)
            myx2, myy2 = my_map(a+2, b+2)

            plt.annotate(str(my_val_labels[gg]) , 
                         xy=(myx, myy), xycoords='data',
                         xytext=(myx2, myy2), textcoords='data',
                         arrowprops=None, #dict(arrowstyle="->"),
                         fontsize=8)

        for gg in range(len(my_val_labels_neg)):
            print "gg = %d" % gg

            #x, y = np.meshgrid( np.exp(-0.5/SIG2*((T-G[k][1]-180) % 360 - 180 )**2),
            #                    np.exp(-0.5/SIG2*( P+90-G[k][2]  )**2)) 
            a,b = (my_refs_RPTneg[gg][1] + 180) % 360 -180, my_refs_RPTneg[gg][2]-90
            myx, myy = my_map(a,b)

            print " (%.0f, %0f)---> (%.0f, %0f)" % (a,b, myx, myy)
            myx2, myy2 = my_map(a+2, b+2)

            plt.annotate(str(my_val_labels_neg[gg]) , 
                         xy=(myx, myy), xycoords='data',
                         xytext=(myx2, myy2), textcoords='data',
                         arrowprops=None, #dict(arrowstyle="->"),
                         fontsize=8)


        # ======================



    # ---------------------------------------------------------------------

    if DO_PLOT:
        name_out = file_prefix + "_MAP"
        name_out_full = name_out + '.' + FTYPE
        mapimg.savefig( name_out_full, dpi=MATDPI )
    else:
        name_out_full = ''

    
    plt.ion()
    plt.show()

    if DO_hold_image:
        raw_input()

    plt.close("all")


# ---------------------------------------------------------------------

def MakeName(file_prefix, fff, extra =''):
    
    # what's in the name
    if fff.__contains__('.'):
        fff_parta = fff.split('.')[:-1][0]
    else:
        fff_parta = str(fff)
    fff_partb = fff_parta.replace('/','_')
    fname_vor = file_prefix + '__' + fff_partb 
    if extra:
        fname_vor+= '_' + extra
    
    return fname_vor

# ---------------------------------------------------------------------

def Gen_bval_clust_levels_ref(X, my_EPS = -1):
    '''X is a 1D array of magnitudes.
    Return a vector/array of values to cluster on.'''
      
    #print "ALL MAGS:", X

    # for ~unit magnitude, EPS is 10/1000; else, assume b~1000, so EPS
    # is 10.  Or, user can have entered one.
    if my_EPS <= 0:
        if np.max(X) < 2:
            my_EPS = .01
        else:
            my_EPS = 10
        print "++ Using default epsilon: ", my_EPS

    db = sc.DBSCAN(eps = my_EPS, min_samples=1).fit(X)

    # how many levels there are, which may include a level of b=0
    # values; will store min/max of each, and then sort later
    Nlabels = max(db.labels_)+1
    ran_minmax = np.zeros((Nlabels,2))

    for i in range( Nlabels ):
        ar = X[db.labels_ == i]  # select all immediately
        if np.shape(ar)[0] > 0:
            ran_minmax[i,:] = np.min(ar), np.max(ar)
        else:
            ran_minmax[i,:] = -1,-1 
            print "** ERROR: how are there no grads here?"
            sys.exit(242)

    #print "RANGES: \n", ran_minmax
    # rearranges both columns lockedly, based on sorting 0th col
    ran_minmax_sort = np.sort(ran_minmax,axis=0)
    print "++ Sorted ranges: \n\t", ran_minmax_sort
    
    print "++ Found %d sets of (nonzero) b-values." % Nlabels
    if Nlabels > 1:
        print "   Separating at:"
    # limits/intervals: half way between upper and lower limits within
    # the reference set
    Y = np.zeros(Nlabels+1)
    for i in range(1,Nlabels):
        Y[i] = 0.5*(ran_minmax_sort[i,0] + ran_minmax_sort[i-1,1])
        print "\t %.4f" % Y[i]
    Y[Nlabels] = BIG_VAL   # final one, a huge number


    return Y

def WriteOutGradStats(filename, ql):
    fo = open( filename +'_STAT.txt', 'w')

    fo.write("# %4s   %6s  %6s  %6s  %6s  %6s  %6s  %6s  #  %-20s" % ( 'Ngrad', 
                                                                      'Bmean',
                                                                      'Bstd',
                                                                      'devi',
                                                                      'maxA', 
                                                                      'minA',
                                                                      'meanA', 
                                                                      'stdA', 
                                                                      'filename') )
    for x in ql:
        if x[2] > 10:  # in case of large/physical scaled b-value
            fo.write("\n  %5d  %6.1f  %6.1f  %.4f  %.4f  %.4f  %.4f  %.4f  #  %-20s" \
                     % (x[1], 
                        x[2], 
                        x[3], 
                        x[4],
                        x[5],
                        x[6],
                        x[7],
                        x[8],
                        x[0]))
        else:
            fo.write("\n  %5d  %.5f  %.4f  %.4f  %.4f  %.4f  %.4f  %.4f  #  %-20s" \
                     % (x[1], 
                        x[2], 
                        x[3], 
                        x[4],
                        x[5],
                        x[6],
                        x[7],
                        x[8],
                        x[0]))
    fo.close()

    return 1



def make_voronoi_region_dict(sv):
    '''Own function to take a spherical voronoi object and return a
dictionary of polygons, so that the area of each can be calculated
~easily.

    '''

    dd = {}
    idx = 0
    for region in sv.regions:
        dd[idx] = sv.vertices[region]
        idx+=1
    return dd

def calculate_all_polygon_areas(sv_dict, rad=1):
    '''Use voronoi utility (by T. Reddy) to calculate polygon area on a
sphere.  Return array of areas.

    '''

    Npoly = len(sv_dict)
    dd = np.zeros(Npoly)

    for i in range(Npoly):
        dd[i] = vu.calculate_surface_area_of_a_spherical_Voronoi_polygon(sv_dict[i], rad)
    return dd


# ----------------------------------------------------------------
# ----------------------------------------------------------------

def Convert_Z_to_BandColor(Z):

    ii = int(Z)
    if ii: # because sign(0) -> 0
        ss = np.sign(ii)
    else:
        ss = 1
    jj = 1. - 0.2*abs(ii)
    if jj < 0.2:  # put a floor before getting to black; would blend
                  # in with outlines
        return 0.2
    else:
        return ss*jj    


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

def make_cmap(colors, position=None, bit=False,Nsegs=256):
    '''
    make_cmap takes a list of tuples which contain RGB values. The RGB
    values may either be in 8-bit [0 to 255] (in which bit must be set to
    True when called) or arithmetic [0 to 1] (default). make_cmap returns
    a cmap with equally spaced colors.
    Arrange your tuples so that the first color is the lowest value for the
    colorbar and the last is the highest.
    position contains values from 0 to 1 to dictate the location of each color.
    '''
    import matplotlib as mpl
    import numpy as np
    bit_rgb = np.linspace(0,1,256)
    if position == None:
        position = np.linspace(0,1,len(colors))
    else:
        if len(position) != len(colors):
            sys.exit("position length must be the same as colors")
        elif position[0] != 0 or position[-1] != 1:
            sys.exit("position must start with 0 and end with 1")
    if bit:
        for i in range(len(colors)):
            colors[i] = (bit_rgb[colors[i][0]],
                         bit_rgb[colors[i][1]],
                         bit_rgb[colors[i][2]])
    cdict = {'red':[], 'green':[], 'blue':[]}
    for pos, color in zip(position, colors):
        cdict['red'].append((pos, color[0], color[0]))
        cdict['green'].append((pos, color[1], color[1]))
        cdict['blue'].append((pos, color[2], color[2]))

    cmap = mpl.colors.LinearSegmentedColormap('my_colormap',cdict,Nsegs)
    return cmap

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




def Do_Voronoi(x, 
               file_prefix, 
               rad=1., 
               do_plot=0,
               FTYPE = 'jpg',
               MATDPI=80,
               FS=10,
               DO_hold_image=0, 
               BMS=(0,0)):

    '''Enter x array (and possibly a radius, whose default value is 1).
We'll use that and its negative projection to do voronoi calculations.
What could be more fun?

    '''
    
    X = np.concatenate((x,-x)) # the fully monty
    lx, ly = np.shape(X)
    #vor = vu.Voronoi_Sphere_Surface(X, rad)
    vor = spsp.SphericalVoronoi(X, rad)
    vor.sort_vertices_of_regions()
    vor_regs = make_voronoi_region_dict(vor)
    vor_areas = calculate_all_polygon_areas(vor_regs)

    if do_plot:

        Amean = np.mean(vor_areas)
        Astd  = np.std(vor_areas)
        Amin  = np.min(vor_areas)
        Amax  = np.max(vor_areas)
        vor_areas_norm = np.array(vor_areas) - Amean
        vor_areas_perc = 100.*vor_areas_norm/Amean
        if Astd:
            vor_areas_norm/= Astd
        print "++ Polygon area mean and std: (%.5f, %.5f)" % (Amean, Astd)

        tcolors = [
            (0,0,205),      # MediumBlue
            (65,105,225),   # RoyalBlue
            (135,206,250),  # sky blue
            (224,255,255),  # light cyan
            (255,255,255),  # white
            (255,255,255),  # white
            (245,245,150),  # sooomething
            (255,215,0),    # Gold
            (255,69,0),     # OrangeRed
            (220,20,60),    # crimson
        ]

        test_cmap = make_cmap(tcolors, bit=True,Nsegs=len(tcolors))

        mm = plt.cm.ScalarMappable(cmap=test_cmap) #plt.cm.RdBu)#BrBG_r)
        col_range = [-100,100]
        mm.set_array(col_range)
        mm.set_clim(vmin=col_range[0],vmax=col_range[1])

        # plot
        fig = plt.figure()
        overall_size = 6.
        fig.set_size_inches(overall_size*9./8., overall_size)
        for bb in range(2):
            for cc in range(2):
                idx = 2*bb+cc
                ax = fig.add_subplot('22'+str(idx+1), projection='3d')

                for ii in range(len(vor_regs)):
                    ireg = np.array(vor_regs[ii])

                    #col_wt = Convert_Z_to_BandColor(vor_areas_norm[ii])

                    hsv = np.array(csys.rgb_to_hsv(abs(X[ii,0]),abs(X[ii,1]),abs(X[ii,2])))
                    #hsv[1] = 1-abs(vor_areas_perc[ii]) #1-abs(np.tanh(vor_areas_norm[ii]))
                    hsv[2] = 0.75 #1
                    my_rgb = csys.hsv_to_rgb(hsv[0], hsv[1], hsv[2])

                    #print hsv
                    
                    asdf = mm.to_rgba(vor_areas_perc[ii])
                    '''asdf2 = np.zeros(4)
                    asdf2[:3] = abs(X[ii])
                    asdf2[3] = abs(col_wt)
                    '''

                    #fill in the Voronoi region (polygon) that contains the
                    #generator:
                    polygon = Poly3DCollection([ireg],alpha=1)#1.0)
                    polygon.set_color(asdf) #[0,0,0])# my_rgb)#mm.to_rgba(vor_areas_norm[ii]))#my_rgb) #random_color)
                    polygon.set_linewidth(1)
                    polygon.set_edgecolor(my_rgb)#np.array([0,0,0])) #*(1-abs(vor_areas_perc[ii]))) 
                    #[0,0,0])#clr.rgb2hex(abs(X[ii]))) #0,0,0]))
                    #textt = Text3D( x = np.mean(voronoi_region[:,0]), \
                    #                y = np.mean(voronoi_region[:,1]), \
                      #              z = np.mean(voronoi_region[:,2]), \
                      #              s = str(ii))
                    ax.add_collection3d(polygon)

                ax.set_xlim3d(aar[0]*0.7, aar[1]*0.7);
                ax.set_ylim3d(aar[0]*0.7, aar[1]*0.7);
                ax.set_zlim3d(aar[0]*0.7, aar[1]*0.7);
                ax.set_xticks([]); ax.set_xticklabels(['']*len(aar))
                ax.set_yticks([]); ax.set_yticklabels(['']*len(aar))
                ax.set_zticks([]); ax.set_zticklabels(['']*len(aar))
                ax.view_init(azim=axview[idx][0], elev=axview[idx][1])
                plt.tick_params(axis='both', which='major', labelsize=10)
                
                arr_a = Arrow3D(px[idx][0], px[idx][1],px[idx][2],
                        mutation_scale=40, 
                        lw=2, 
                        arrowstyle="-|>",
                        color="r")
                arr_b = Arrow3D(py[idx][0], py[idx][1],py[idx][2],
                        mutation_scale=40, 
                        lw=2, 
                        arrowstyle="-|>", 
                        color="g")
                arr_c = Arrow3D(pz[idx][0], pz[idx][1],pz[idx][2],
                        mutation_scale=40, 
                        lw=2, 
                        arrowstyle="-|>", 
                        color="b")
                ax.text2D(x=0.2,y=0.9, s=axview_lab[idx], fontsize=FS+2,
                         horizontalalignment='center',
                         verticalalignment='center',fontdict=None, 
                          withdash=False,transform=ax.transAxes)
                plt.tight_layout(h_pad = -2, w_pad = -2)

                
                ax.add_artist(arr_a)
                ax.add_artist(arr_b)
                ax.add_artist(arr_c)
                #ax.w_xaxis.line.set_lw(0.)
                #ax.w_yaxis.line.set_lw(0.)
                #ax.w_zaxis.line.set_lw(0.)
                ax.set_axis_off()

        
        if BMS[0] >10:
            info_textT = "$b$-val: %.0f $\pm$ %.0f" \
                         % (BMS[0], BMS[1])
        else:
            info_textT = "$b$-val: %.2f $\pm$ %.2f" \
                         % (BMS[0], BMS[1])

        info_textB = "\nArea\n(mean, SD) = (%.4f, %.4f)" \
                     % (Amean, Astd)
        info_textB+= "\n(min, max) = (%.4f, %.4f)" \
                     % (Amin, Amax)
        


        colbar = fig.add_axes([0.82, 0.31, 0.05, 0.35])
        clb = fig.colorbar(mm, cax=colbar)
        clb.ax.set_title('$\mathsf{\Delta A \%}$',fontsize = FS)
        clb.ax.tick_params(labelsize=FS)

        #plt.colorbar(im_tmpX, cax=colbar, cmap=plt.cm.RdBu )

        fig.subplots_adjust(bottom=0.04,top=0.94, right=0.8)
        fig.text( x=0.4, 
                  y=0.08,
                  s=info_textB, 
                  horizontalalignment='center',
                  verticalalignment='center',
                  fontsize=FS)
        fig.text( x=0.4,
                  y=0.95,
                  s=info_textT, 
                  horizontalalignment='center',
                  verticalalignment='center',
                  fontsize=FS)

        plt.ion()
        plt.show()
        name_out = file_prefix + "_VOR"
        name_out_full = name_out + '.' + FTYPE
        plt.savefig( name_out_full, dpi=MATDPI )
        if DO_hold_image:
            raw_input()
        else:
            plt.close("all")

    std_area = np.std(vor_areas)
    mean_area = np.mean(vor_areas)
    max_area = np.max(vor_areas)
    min_area = np.min(vor_areas)

    theor_area = 4*np.pi*rad*rad / float(lx)

    print "\t ratio stdev/mean is  : %.4f" % (std_area/mean_area)
    print "\t max of the area is   : %.4f" % max_area
    print "\t min of the area is   : %.4f" % min_area
    print "\t mean of the area is  : %.4f" % mean_area
    print "\t stdev of the area is : %.4f" % std_area
    print "\t Theoretical area is  : %.4f" % theor_area

    return vor_areas, mean_area, std_area, max_area, min_area, \
        vor, vor_regs

# ----------------------------------------------------------------
# ----------------------------------------------------------------

def MATR_all_dotprods(X):
    '''Input one Nx3 arrays in spherical polar coor (r, phi, theta).
    Calculate the dot prod angle of each pairwise combination-- LHT.
    return: min, max, mean and stdev of angles.

    '''

    A = MATR_sincosvals(X)
    L,M = np.shape(A)

    tmp = np.zeros(L)        # store values for each row: give each
                             # element a large value, so the 'self'
                             # dot prod is never picked
    dots_min = np.zeros(L)   # store min values of matr
    dots_sum = np.zeros(L)

    for i in range(L):
        tmp*= 0.
        tmp+= 361  # max could just be 181...
        for j in range(L):
            if not(i==j):
                tmp[j] = MATR_trig_dotprod_ang(A[i], A[j])
        dots_min[i] = np.min(tmp)
        dots_sum[i] = np.sum(tmp) - 361   # subtract '361' because it's a placeholder....
        #if dots_min[i] < 5:
        #    print "MINI for i=",i
        #    print "\t", tmp
    
    #np.min(dots_min), np.max(dots_min), np.mean(dots_min), np.std(dots_min)

    return dots_min, dots_sum


def MATR_summary_mins(A):
    return np.mean(A), np.std(A), np.min(A), np.max(A)


def MATR_summary_sums(A):
    nn = len(A) 
    if nn < 1:
        return 0,0,0,0
    nn = float(nn)
    return np.mean(A)/nn, np.std(A)/nn, np.min(A)/nn, np.max(A)/nn

    
def Calc_char_length_sphere(N):
    '''A special length that would estimate something like a
    characteristic separation on a sphere where there are N (>=2?)
    evenly spaced points.

    '''

    # take area of a r=1 sphere divided into N pieces, with Apatch
    # =4*pi/N; then say that each area is like a circle on the sphere,
    # of radius alpha, and Acirc = pi*alpha**2 (!!! don't think this
    # is actually accurate? should there be a factor of sin(alpha)
    # times it?.  Solve for alpha, and the typical separation is twice
    # that distance.
    alpha_rad = np.arccos(1.-2./N)    # np.sqrt(4./N) #np.arcsin(np.sqrt(4./N))
    alpha_def = alpha_rad*RADtoANG
    sep = 2*alpha_def

    return sep
    



def MATR_trig_dotprod_ang(A,B):

    '''Dot prod, in terms of sin/cos of phi/thet angles.  Returns the
    angle in deg.

    '''

    out = A[2]*B[2] * (A[1]*B[1] + A[0]*B[0]) + A[3]*B[3]
    #print np.arccos(out)*RADtoANG
    return np.arccos(out)*RADtoANG


def MATR_sincosvals(X, angscale = RADtoANG):
    '''Input Nx3 arr in spherical polar coor (r, phi, theta).  Angles are
    assumed to be in degrees, by default (set angscale=1 for radians).

    Output Nx4 arr of [sin(phi), cos(phi), sin(theta), cos(theta)],
    where phi is the azimuthal angle and theta is polar angle.

    '''
    angscale*= 1.0

    L,M = np.shape(X)
    P   = np.zeros((L,4))

    P[:,0] = np.sin(X[:,1]/angscale) # sin phi
    P[:,1] = np.cos(X[:,1]/angscale) # cos phi
    P[:,2] = np.sin(X[:,2]/angscale) # sin thet
    P[:,3] = np.cos(X[:,2]/angscale) # cos thet

    return P

# ----------------------------------------------------------------


def Make_LongLat_Arr ():

    my_lon = np.arange(-180, 180+h_lon, h_lon) # ->  azim ang
    my_lat = np.arange( -90,  90+h_lat, h_lat) # -> polar ang
    x, y = np.meshgrid(my_lon, my_lat) 

    return my_lon, my_lat, x, y


# ----------------------------------------------------------------
# ----------------------------------------------------------------


# vectorized form.
def Calc_My_Vals(T, P, G):
    ''' input arrays of N lon (T) and M lat (P) values. 
    Return an NxM grid of values -> to be based on grads.'''

    # in G, phi is in [-180, 180]
    #   and theta is in [0, 180]

    # in T, phi is in [-180, 180]
    # in P, theta is in [-90, 90]


    N = len(T)
    M = len(P)
    Gx, Gy = np.shape(G) # rows of: rad, azim, pol

    MAT = np.zeros((M,N),dtype=int)  #!!!!!!!!assume ints!

    for k in range(Gx):
        # preserve locations, also get wraps

        # for Gaussian
#        x, y = np.meshgrid( np.exp(-0.5/SIG2*((T-G[k][1]-180) % 360 - 180 )**2),
#                            np.exp(-0.5/SIG2*((P-G[k][2]-90)  % 180 -  90 )**2)) 
#        MAT+= x*y*FAC_denom

        # binarized, though a wee bit smooth
        # the y-coor doesn't wrap around, but the x-coor should--
        # that's why we shift it to be in [0, 360] and then do modular differencing.
        x, y = np.meshgrid( np.exp(-0.5/SIG2*((T-G[k][1]-180) % 360 - 180 )**2),
                            np.exp(-0.5/SIG2*( P+90-G[k][2]  )**2)) 

        MAT+= np.greater_equal(x*y,TOL) # *FAC_denom


    return MAT




# ----------------------------------------------------------------

def All_CtS( X ):
    '''Input a Nx3 array of Cartesian coors, get an Nx3 array of spherical
coors.'''

    N, L = np.shape(X)
    if not(L == 3):
        print "**ERROR! Need a Nx3 array in All_CtS()."
        sys.exit(3)

    Y = np.zeros((N,L))
    for i in range(N):
        Y[i] = CARTtoSPH(X[i])

    return Y

# ----------------------------------------------------------------
'''   !!!! OLDER VERSION: NOT SURE WHY THE mode DEG shifting stuff was done??
def CARTtoSPH( X, mode = 'DEG' ):
    'Input one array of (x,y,z) values (length=3), and get one array
    of (r, \phi, \theta) values returned.
    '

    if not(len(X) == 3 and type(X) == np.ndarray):
        print "**ERROR! Need a 3-array only in CARTtoSPH()."
        sys.exit(2)

    Y = np.zeros(3)
    
    Y[0] = np.sqrt(sum (X*X))
    if abs(Y[0]) > EPS_rad:
        Y[1] = np.arctan2(X[0], X[1])
        Y[2] = np.arccos(X[2]/Y[0])
        if mode=='DEG':
            Y[1] = (Y[1]*RADtoANG % 360) -180    # [-180, 180]
            Y[2] = (Y[2]*RADtoANG % 180) -90   # [-90, 90]
    else:
        print "Radius too small!"
        Y[1] = 0
        Y[2] = 0
    
    return Y
'''


def CARTtoSPH( X, mode = 'DEG' ):
    '''Input one array of (x,y,z) values (length=3), and get one array
    of (r, \phi, \theta) values returned.
    '''

    if not(len(X) == 3 and type(X) == np.ndarray):
        print "**ERROR! Need a 3-array only in CARTtoSPH()."
        sys.exit(2)

    Y = np.zeros(3)
    
    Y[0] = np.sqrt(sum (X*X))
    if abs(Y[0]) > EPS_rad:
        Y[1] = np.arctan2(X[1], X[0])
        Y[2] = np.arccos(X[2]/Y[0])
        if mode=='DEG':
            Y[1] = Y[1]*RADtoANG    # [-180, 180]
            Y[2] = Y[2]*RADtoANG    # [0, 180]
    else:
        print "Radius too small!"
        Y[1] = 0
        Y[2] = 0
    
    return Y

def SepMagnDir(X):

    Nx, Ny = np.shape(X)
    
    if not( Ny == 3 ):
        sys.exit(34)

    Y = np.zeros((Nx,4))

    for i in range(Nx):
        x = X[i,:]
        xmagn = np.sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2])
        Y[i,0] = xmagn
        Y[i,1:] = x/xmagn

    return Y


def Convert_CSV_to_lists(my_file):
    ''' Reads in: a CSV file of *ints*.  
    Returns: a (group) list of (individual student) lists, 
    of the ints information.'''

    fff = open( my_file ,'r')
    raw_data = fff.readlines()
    fff.close()
    data_list1 = []
    for line in raw_data:
        # separate the lines at every comma
        x = line.split()     
    
        # for all separated strings in the new list, 
        # remove whitespace from the beginning and end of a string, but
        # not from the middle; replace the old string with the new one.
        for i in range(len(x)):
            x[i] = float(x[i].strip())

        # append the new x, which is a list of strings
        data_list1.append( x )

    return data_list1

def Convert_CSV_to_arrays(my_file):
    ''' Reads in: a CSV file of *ints*.  
    Returns: a (group) list of (individual student) lists, 
    of the ints information. 

    *Also now returns an array of original index locs, for 
    future filtering.'''

    # list of original line numbers: 0 counting
    list_orig_ind = []
    idx = 0

    fff = open( my_file ,'r')
    raw_data = fff.readlines()
    fff.close()
    data_list1 = []
    for line in raw_data:
        # separate the lines at every comma
        x = line.split()     
    
        # for all separated strings in the new list, 
        # remove whitespace from the beginning and end of a string, but
        # not from the middle; replace the old string with the new one.
        for i in range(len(x)):
            x[i] = float(x[i].strip())

        
        if 1:
            xmagn = np.sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2])
            #if xmagn>0:
            #    for i in range(len(x)):
            #        x[i] = x[i] / xmagn
            '''
            else:
                print "*+ NB: there is a 0 magnitude grad here:",\
                    my_file
                x[0] = 1.
                x[1] = 0.
                x[2] = 0.
            '''
        # append the new x, which is a list of strings

        if xmagn>0:
            data_list1.append( x )
            list_orig_ind.append( idx )
            idx+=1
        else:
            print "*+ NB: there is a 0 magnitude grad here:",\
                    my_file
            print "*+ \t-> filtering it out!"
            idx+=1
    return np.array(data_list1), np.array(list_orig_ind)

def MakeMyBar(name,nlev=512):

    wind = 0.002
    sfac = 0.999

    if name == 'hot_cold_gap':

        # window/level parameters
        midlev = 0.5

        cdict_00 = {'red': ((0.0, 0.0, 0.0),
                            (midlev-wind     , 0., 0.90),
                            (midlev-wind*sfac, 0.90, 0.0),
                            (midlev+wind*sfac, 0.0, 0.0),
                            (midlev+wind     , 0.0, 0.90),
                            (1.0, 1.0, 1.0)),
                    
                    'green': ((0.0, 0.0, 0.0),
                              (midlev-wind     , 0.90, 0.90),
                              (midlev-wind*sfac, 0.90, 0.0),
                              (midlev+wind*sfac, 0.0, 0.0),
                              (midlev+wind     , 0.0, 0.90),
                              (1.0, 0.0, 0.90)),
                    
                    'blue': ((0.0, 0.50, 0.50),
                             (midlev-wind     , 1.0, 0.00),
                             (midlev-wind*sfac, 0.0, 0.00),
                             (midlev+wind*sfac, 0.0, 0.0),
                             (midlev+wind     , 0.0, 0.00),
                             (1.0, 0.0, 0.0))}

        out = clr.LinearSegmentedColormap('my_colormap',
                                          cdict_00,
                                          nlev)

    elif name == 'wgap_jet':
        '''cdict_01 = {'red': ((0.,1,1),
                            (wind*sfac, 0, 0),
                            (wind, 0, 0),
                            (0.11, 0, 0),
                            (0.66, 1, 1),
                            (0.89, 1, 1),
                            (1, 0.5, 0.5)),
                    'green': ((0.,1,1),
                              (wind*sfac, 0, 0),
                              (wind, 0, 0),
                              (0.11, 0, 0),
                              (0.375, 1, 1),
                              (0.64, 1, 1),
                              (0.91, 0, 0),
                              (1, 0, 0)),
                    'blue': ((0.,1,1),
                             (wind*sfac, 0, 0),
                             (wind, .5, 0.5),
                             (0.11, 1, 1),
                             (0.34, 1, 1),
                             (0.65, 0, 0),
                             (1, 0, 0))}
        
        out = clr.LinearSegmentedColormap('my_colormap',
                                          cdict_01,
                                          nlev)'''

        if nlev < 5 :
            out = clr.LinearSegmentedColormap.from_list('my_colormap',
                                                        [(0      , (1,1,1) ),
                                                         (1./nlev, (0,0,1) ),
                                                         (0.5    , (0,1,0) ),
                                                         (1      , (1,0,0) )],
                                                        N=nlev
                                                    )
        else:
            out = clr.LinearSegmentedColormap.from_list('my_colormap',
                                                        [(0      , (1,1,1) ),
                                                         (1./nlev, (0,0,1) ),
                                                         (0.25   , (0,1,1) ),
                                                         (0.5    , (0,1,0) ),
                                                         (0.75   , (1,1,0) ),
                                                         (1      , (1,0,0) )],
                                                        N=nlev
                                                    )


    else:
        print "Unrecognized colormap specialification."
        sys.exit(151)
        
    return out

    
# -------------------------------------------------------------------------
