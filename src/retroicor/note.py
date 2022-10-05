
note_on_quants = """

Let's define the following quantities related to data acquisition and
processing:

     N : number of time points in signal (number)
         also length of Fourier transform (FT) of signal
         (though the FT has N/2 independent units, for a real signal)
    ts : time sampling rate of signal (unit = s)
         also the 'step' along the t-axis in the time domain
    T0 : time duration of signal (unit = s)
    fs : sampling frequency (unit = Hz)
    F0 : harmonic frequency (unit = Hz)
         also the 'step' along the f-axis in the frequency domain
    fN : Nyquist frequency (unit = Hz)

     ... and the following relations apply:

         T0 = N * ts
         fs = 1 / ts
         fN = fs / 2
         F0 = 1 / T0 = 1/(N * ts) = fs / N

     ... probably:
         T0 >> ts
         F0 << fs

     ... at a given k-th harmonic (index) corresponds to the following
         physical frequency:
         
         f_k = k * F0

     ... at a given n-th time point (index) corresponds to the
         following physical time:

         t_n = n * ts

     ... To apply a minimum search frequency, fmin (units = Hz), find
         the minimum 'kstar' harmonic >= fmin (where kstar is the
         integer value of that harmonic).  This corresponds to:

         min(kstar) s.t. kstar * F0 >= fmin
         -->  kstar = ceil(fmin/F0)
         -->  kstar = ceil(N*fmin/fs)


"""

