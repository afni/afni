ALLO = color.o curve.o frame.o frstpt.o labmod.o memplt.o    \
       line.o perim.o periml.o phdot.o phline.o point.o      \
       points.o pwrit.o pwritf.o set.o setdsh.o              \
       setfrm.o setlin.o setw.o srface.o tick4.o vector.o    \
       zzaxxx.o zzaxyy.o zzchar.o zzclip.o zzlabl.o zzlgin.o \
       zzline.o zzlinx.o zzliny.o zzlogx.o zzlogy.o          \
       zzperi.o zzphph.o zzphys.o                            \
       ppak_perim.o ppak_pwrit.o ppak_pwritf.o ppak_srface.o \
       plot_cox.o plot_ps.o plot_ps2.o plot_x11.o            \
       plot_motif.o plot_ts.o ppak_commons.o plot_strip.o    \
       find_color_name.o

#IFLAGS = -I. -I..

# For IRIX 6.2
# CC     = cc -O2 -n32
# AR     = /usr/bin/ar
# RANLIB = /sbin/touch
# RM     = /bin/rm -f

# For Linux
# CC     = gcc -O2 -m486 -ffast-math
# AR     = /usr/bin/ar
# RANLIB = /usr/bin/ranlib
# RM     = /bin/rm -f
# LFLAGS = -s -L. -L.. -L/usr/X11R6/lib -lcoxplot -lconverted_from_fortran \
#          -lXm -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm

libcoxplot.a:$(ALLO) coxplot.h
	$(RM) $@
	$(AR) q libcoxplot.a $(ALLO)
	$(RANLIB) libcoxplot.a

xxx:xxx.o libcoxplot.a
	$(RM) $@
	$(CC) -o xxx xxx.o $(LFLAGS) -L. -L.. -L/usr/X11/lib \
              -lcoxplot -lconverted_from_fortran -lXm -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm

qqq:qqq.o libcoxplot.a
	$(RM) $@
	$(CC) -o qqq qqq.o -s -L. -L.. -L/usr/X11R6/lib -lcoxplot -lconverted_from_fortran -lX11 -lm

clean:
	$(RM) *.o *.P

.c.o:
	$(RM) $@
	$(CC) -c $*.c -I. -I.. $(IFLAGS)

# dependencies

plot_cox.o:coxplot.h
