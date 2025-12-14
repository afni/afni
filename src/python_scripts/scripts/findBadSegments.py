#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec  2 07:59:59 2025

@author: peterlauren
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.spatial.distance import pdist, squareform
import copy
from collections import defaultdict
import sys
import statistics

# Get cluster modulatity (MOD)
def getMOD(weights, clusters):
    
    # Determine m, the normalization factor
    upper_tri_exclusive = np.triu(weights, k=1)
    m = np.sum(upper_tri_exclusive)
    TwoM = m * 2
    
    # get vertex and cluster count
    numVertices = np.shape(weights)[0]
    numClusters = len(clusters)
    
    # Build MOD from clusters
    mod = 0
    for i in range(0,numClusters):
        insum = 0
        for j in range(0,numVertices):
            if j != i and j in clusters[i]: insum = insum + weights[i,j]
        totSum = 0
        for j in range(0,numVertices):
            if j != i: totSum = insum + weights[i,j]
            
        mod = mod + (insum/TwoM - (totSum/TwoM)**2)
        
    return mod

def clusters_from_labels(labels):
    d = defaultdict(list)
    for v,c in enumerate(labels):
        d[c].append(v)
    return list(d.values())

# Use MOD as basis to merge clusters
def merge_clusters_by_MOD(weights, clusters, rankVector, getMOD):
    numVertices = len(clusters)

    # --- Start with a label per vertex ---
    labels = np.arange(numVertices)

    # initial modularity
    bestMOD = getMOD(weights, clusters_from_labels(labels))

    # --- Process vertices in ranking order ---
    for u in rankVector:
        Cu = labels[u]

        # collect all existing cluster IDs
        clusterIDs = np.unique(labels)

        # test merging Cu with every other cluster Cj
        for Cj in clusterIDs:
            # print(' u, Cj = '+str(u)+' , '+str(Cj))
            if Cj == Cu:
                continue
            
            # copy label state
            new_labels = labels.copy()

            # merge cluster Cj into cluster Cu
            new_labels[ new_labels == Cj ] = Cu

            # compute resulting MOD
            testMOD = getMOD(weights, clusters_from_labels(new_labels))

            # if MOD increases, keep the merge
            if testMOD > bestMOD:
                bestMOD = testMOD
                labels = new_labels  # accept
                # Cu stays the same label, so continue merging into that

    # Rebuild final cluster list
    final_clusters = clusters_from_labels(labels)
    return final_clusters, bestMOD

def cumulatives_weights_low_end_outlier_indices(vectorWeightSums, rankVector, 
                                         cardiacPeaks):

    # Calculate Q1 and Q3
    q1, q3 = np.percentile(vectorWeightSums, [25, 75])
    
    # Calculate IQR
    iqr = q3 - q1
    
    # Define lower bound
    lower_bound = q1 - 1.5 * iqr
    
    # Identify low-end outliers
    low_end_outliers = vectorWeightSums[vectorWeightSums < lower_bound]
    
    # Get outlier peak indices
    outlier_peak_indices = rankVector[-len(low_end_outliers):]
    
    # Get outlier time series indices
    outlier_ts_indices = [cardiacPeaks[i] for i in outlier_peak_indices]
    
    return outlier_ts_indices


def best_lower_multiplier(data, k_values=np.linspace(1.0, 1.6, 601)):
    q1, q3 = np.percentile(data, [25, 75])
    iqr = q3 - q1

    best_k = None
    best_gap = -np.inf

    # Sort values below Q1
    below_q1 = np.sort(data[data < q1])

    for k in k_values:
        cutoff = q1 - k * iqr

        # distance between cutoff and the nearest remaining point
        diffs = below_q1 - cutoff
        diffs = diffs[diffs > 0]  # points above cutoff
        if len(diffs) == 0:
            continue
        smallest_gap = diffs.min()

        # maximize the margin → avoid unstable boundaries
        if smallest_gap > best_gap:
            best_gap = smallest_gap
            best_k = k

    return best_k

def gap_based_multiplier(gaps, base_multiplier=1.4):
    # Ensure gaps is a NumPy array
    gaps = np.asarray(gaps)

    # If no gaps or only one gap, fall back to base multiplier
    if gaps.size < 2:
        return float(base_multiplier)

    # Identify unusually large gaps
    median_gap = np.median(gaps)
    if median_gap == 0:
        return float(base_multiplier)

    ratio = gaps / median_gap
    large_gaps = ratio[ratio > 2.0]  # example threshold

    # If no large gaps, keep baseline
    if large_gaps.size == 0:
        return float(base_multiplier)

    # Compute extra factor from the size of the largest gap
    extra = min(np.max(large_gaps) / 5.0, 1.0)  # clip to avoid extremes

    return float(base_multiplier + extra)

def cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, rankVector, 
                                         cardiacPeaks):

    # Calculate Q1 and Q3
    q1, q3 = np.percentile(vectorWeightSums, [25, 75])
    
    # Calculate IQR
    iqr = q3 - q1
    
    # k_opt = best_lower_multiplier(vectorWeightSums)
    k_opt = gap_based_multiplier(vectorWeightSums)
    
    # Define lower bound
    lower_bound = q1 - k_opt * iqr
    
    # Identify low-end outliers
    low_end_outliers = vectorWeightSums[vectorWeightSums < lower_bound]
    
    # Get outlier peak indices
    num_outliers = len(low_end_outliers)
    if len(low_end_outliers)>0:
        outlier_peak_indices = rankVector[-len(low_end_outliers):]
    else: outlier_peak_indices = []
    
    # Get outlier time series indices
    # limit = len(cardiacPeaks)-1
    outlier_ts_indices = []
    if 0 in outlier_peak_indices: 
        outlier_ts_indices += [[cardiacPeaks[0], cardiacPeaks[2]]]
        outlier_peak_indices = outlier_peak_indices[outlier_peak_indices != 0]
    last = len(cardiacPeaks) - 1
    lastM2 = last - 2
    if any(item > lastM2 for item in outlier_peak_indices):
        outlier_ts_indices += [[cardiacPeaks[last-1], cardiacPeaks[last]]]
        outlier_peak_indices = outlier_peak_indices[outlier_peak_indices <= last-2]
    outlier_ts_indices += [[cardiacPeaks[i-1], cardiacPeaks[i+2]]  
        for i in outlier_peak_indices]
    
    return outlier_ts_indices

def getCardiacPeaktPeakOutliers(cardiacTimeSeries, cardiacPeaks):

    # Calculate Q1 and Q3
    q1, q3 = np.percentile(cardiacTimeSeries[cardiacPeaks], [25, 75])
    
    # Calculate IQR
    iqr = q3 - q1
    
    # Define lower bound
    lower_bound = q1 - 2 * iqr
    upper_bound = q3 + 2 * iqr
    
    # Get true outliers
    low_outliers = [
    i
    for i in range(len(cardiacPeaks))
    if cardiacTimeSeries[cardiacPeaks[i]] < lower_bound
    ]
    high_outliers = [
    i
    for i in range(len(cardiacPeaks))
    if cardiacTimeSeries[cardiacPeaks[i]] > upper_bound
    ]
    outliers = low_outliers + high_outliers    
    
    # Define lower bound
    lower_bound = q1 - 1.9 * iqr
    upper_bound = q3 + 1.9 * iqr
    
    # Get marginal (just missed out) outliers
    low_outliers = [
    i
    for i in range(len(cardiacPeaks))
    if cardiacTimeSeries[cardiacPeaks[i]] < lower_bound
    ]
    high_outliers = [
    i
    for i in range(len(cardiacPeaks))
    if cardiacTimeSeries[cardiacPeaks[i]] > upper_bound
    ]
    
    return outliers

def compute_respiratory_peaks(
    respiratoryTimeSeriesFile,
    respiratoryPeaksFile,
    respiratoryTroughsFile
):
    # ts = respiratoryTimeSeriesFile
    # peaks = np.asarray(respiratoryPeaksFile)
    # troughs = np.asarray(respiratoryTroughsFile)
    ts = np.asarray(respiratoryTimeSeriesFile)
    peaks = np.asarray(respiratoryPeaksFile)
    troughs = np.asarray(respiratoryTroughsFile)

    out_peak_indices = []
    out_peak_values  = []
    out_ranges       = []

    # Ensure sorted
    peaks.sort()
    troughs.sort()

    # Iterate trough-to-trough
    for t0, t1 in zip(troughs[:-1], troughs[1:]):

        # Peaks inside this trough interval
        mask = (peaks > t0) & (peaks < t1)
        seg_peaks = peaks[mask]

        if len(seg_peaks) == 0:
            continue  # no peak in this respiratory cycle

        # ---- Peak aggregation ----
        if len(seg_peaks) == 1:
            peak_idx = seg_peaks[0]
            peak_val = ts[peak_idx]
        else:
            peak_idx = int(np.round(seg_peaks.mean()))
            peak_val = ts[seg_peaks].mean()
            out_ranges.append((t0, t1))  # ✅ only ambiguous cycles logged

        # ---- Trough values (handle multiple troughs implicitly) ----
        trough_vals = ts[[t0, t1]]
        trough_mean = trough_vals.mean()

        # ---- Adjusted peak height ----
        adjusted_peak = peak_val - trough_mean

        out_peak_indices.append(peak_idx)
        out_peak_values.append(adjusted_peak)

    return (
        np.array(out_peak_indices),
        np.array(out_peak_values),
        out_ranges
    )

def outputCardiacPlots(cardiacTimeSeries, cardiacPeaks, samp_freq,
                       peak_outliers, 
                       outlier_ts_ranges, 
                       output_file_name):
    print('Plot cardiac results')
    y = cardiacTimeSeries              # length 
    x = np.arange(len(y))             # original index
    x_scaled = x / samp_freq          # scaled index
    cardiacPeaks_scaled = np.array(cardiacPeaks) / samp_freq

    # Limit length of each row for clarity
    print('Limit length of each row for clarity')
    points_per_row = 3000             
    num_rows = int(np.ceil(len(y) / points_per_row))

    fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)
    if num_rows == 1:
        axes = [axes]

    peakVals = cardiacTimeSeries[cardiacPeaks]

    for row in range(num_rows):
        start = row * points_per_row
        end = min((row + 1) * points_per_row, len(y))

        ax = axes[row]

        # --- plot scaled x ---
        ax.plot(
            x_scaled[start:end],
            y[start:end],
            linewidth=0.5,
            solid_capstyle='butt',
            solid_joinstyle='miter',
            color="black"
        )

        # Peaks
        ax.plot(cardiacPeaks_scaled, peakVals, "bo")

        # Main outlier peaks
        ax.plot(cardiacPeaks_scaled[peak_outliers],
                cardiacTimeSeries[cardiacPeaks[peak_outliers]],
                "ro")

        # --- Draw bands (also scaled) ---
        for band_start, band_end in outlier_ts_ranges:
            if band_end <= start or band_start >= end:
                continue
            ax.axvspan(
                max(band_start, start) / samp_freq,
                min(band_end, end) / samp_freq,
                color='red',
                alpha=0.15
            )

        # --- scaled x-limits ---
        ax.set_xlim(x_scaled[start], x_scaled[end - 1])
        ax.set_ylabel("ECG Amplitude")

    # Set axes and save plot to file
    print('Set axes and save plot to file')
    ax.set_xlabel(f"Time (seconds)")
    axes[-1].set_xlabel("Time (s)")
    plt.tight_layout()
    OutDir = directory
    plt.savefig('%s/cardiacOutliersWithPeaks.pdf' % (OutDir))
    plt.show()
    
# Defaults
expand_factor = 1 # Reverses the rudtion cause by -prefilt_max_freq in physio_calc

# Read arguments
print('Read arguments')
i = 1
while i < len(sys.argv):
    match sys.argv[i]:
        case "-directory":
            i += 1
            if i >= len(sys.argv):
                print("Error: -directory requires an argument")
                exit(1)
            directory = sys.argv[i]
            i += 1
            
        case "-card_file":
            i += 1
            if i >= len(sys.argv):
                print("Error: -card_file requires an argument")
                exit(1)
            cardiacTimeSeriesFile = sys.argv[i]
            i += 1

        case "-resp_file":
            i += 1
            if i >= len(sys.argv):
                print("Error: -resp_file requires an argument")
                exit(1)
            respiratoryTimeSeriesFile = sys.argv[i]
            i += 1

        case "-freq":
            i += 1
            if i >= len(sys.argv):
                print("Error: -freq requires an argument")
                exit(1)
            samp_freq = float(sys.argv[i])
            i += 1
            
        case '-expand_factor':
            i += 1
            if i >= len(sys.argv):
                print("Error: -expand_factor requires an argument")
                exit(1)
            expand_factor = int(sys.argv[i])
            i += 1

        case "-help":
            print("Usage:\n python ./findBadSegments.py -directory <directory>")
            print("\t[-freq <samp_freq>]\n")
            print('where "directory" is the "physio_physio_extras/" directory ') 
            print('produced by physio_calc.py.  samp_freq is the sampling frequency') 
            print('in Hertz') 
            exit()

        case "-h":
            print("-h not recognized.  Did you mean -help?")
            exit()

        case _:
            print(f"Unrecognized option: {sys.argv[i]}")
            print('Use "-help" for correct usage.')
            exit(1)

print('Get cardiac peaks file and respiratory peaks and troughs file')
cardiacPeaksFile = directory + 'physio_card_peaks_00.1D'
respiratoryPeaksFile = directory + 'physio_resp_peaks_00.1D'
respiratoryTroughsFile = directory + 'physio_resp_troughs_00.1D'

#Whether to use clustering which tends to be slow
print('Set clustering to false')
useClustering = False

# Load cardiac time series
print('Load cardiac time series')
with open(cardiacTimeSeriesFile) as f:
    for line in f:
        cardiacTimeSeries=[float(line.strip()) for line in f if line.strip()]

# Median shrink cardiac time series to match cardiac peaks        
if expand_factor > 1:
    num_blocks = int(len(cardiacTimeSeries)/expand_factor)
    inc = 0
    temp = np.array([])
    for i in range(0,num_blocks):
        start = i * expand_factor
        end = start + expand_factor
        temp = np.append(temp, statistics.median(cardiacTimeSeries[start:end]))
    cardiacTimeSeries = temp
        
# Load cardiac peaks
print('Load cardiac peaks')
with open(cardiacPeaksFile) as f:
    for line in f:
        cardiacPeaks = [int(line.strip()) for line in f if line.strip()]

# Plot cardiac peaks on cardiac time series
print('Plot cardiac peaks on cardiac time series')
x = []    
end = len(cardiacTimeSeries)
for i in range(0,end): x.append(i)
plt.subplot(211)
plt.plot(x, cardiacTimeSeries, "g") #Lines connecting peaks and troughs
if len(cardiacPeaks) > 0:
    peakVals = []
    for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
    plt.xlim(1608,2208)  # Zoom in on plot
    plt.plot(cardiacPeaks, peakVals, "ro") # Peaks

# Build an array of vecs, one for each peak.
print('Build an array of vecs, one for each peak')
cardiacPeaks = np.asarray(cardiacPeaks)
cardiacTimeSeries = np.asarray(cardiacTimeSeries)
vec = np.column_stack([
    cardiacTimeSeries[cardiacPeaks] - np.roll(cardiacTimeSeries[cardiacPeaks], 1),
    np.roll(cardiacTimeSeries[cardiacPeaks], -1) - cardiacTimeSeries[cardiacPeaks],
    cardiacPeaks - np.roll(cardiacPeaks, 1),
    np.roll(cardiacPeaks, -1) - cardiacPeaks
])
vec[0] = vec[1]
vec[-1] = vec[0]

# Normalize time differences to be in the same range as the value differences
print('Normalize time differences to be in the same range as the value differences')
scaleFactor = (vec[:,0:1].max()-vec[:,0:1].min())/(vec[:,2:3].max()-vec[:,2:3].min())
vec[:,2:4] =vec[:,2:4]*scaleFactor

# Make NxN matrix where N is the number of segments
# Fill the matrix with the weights
print('Make weights matrix')
weights = squareform(pdist(vec, metric='euclidean'))
weights = np.exp(1/(weights+1))

# Each vertex is initially regarded as a community or cluster.
print('Each vertex is initially regarded as a community or cluster')
numPeaks = len(cardiacTimeSeries)
clusters = [[i] for i in range(0,numPeaks)]

# Make an array of clusters where each cluster is an array of members containing 
# the iindices of the cluster members. 
print('Make an array of clusters where each cluster is an array of members containing ')
numPeaks = len(cardiacPeaks)
clusters = [[i] for i in range(0,numPeaks)]
    
# make vector of weight sums and get vector of their ranks starting with the highest weight sum
print('Make weight sum and rank vectors')
vectorWeightSums = np.sum(weights,axis=1)
rankVector = np.argsort(vectorWeightSums)[::-1]

if useClustering:
    print('clustering')
    # Starting with the first cluster (vertex) merge in other clusters only in cases 
    # where MOD is increased by doing so.
    clusters = [[i] for i in range(numPeaks)]

    # Merge clusters, starting with merginging into the cluster (initially single
    # vertex) with the highest sum of weights with other vertices and working down
    # towards the vertex with the lowest cumulative weights.
    final_clusters, bestMOD = merge_clusters_by_MOD(weights, clusters, rankVector, getMOD)

# Identify outliers on low end of the cumulatives weights
print('Identify outliers on low end of the cumulatives weights')
outlier_ts_ranges = cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, 
                                                rankVector, cardiacPeaks)
num_anomalies = len(outlier_ts_ranges)

# Find peak outliers
print('Find peak outliers')
peakVals = []
for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
peakRankVector = np.argsort(peakVals)[::-1]
peak_outliers = getCardiacPeaktPeakOutliers(cardiacTimeSeries, 
                                                                   cardiacPeaks)


# Plot cardiac results
if directory[-1] == '/':
    OutDir = directory[:-1]
else:
    OutDir = directory
pageLimit = 1000000
if len(cardiacTimeSeries) <= pageLimit:
    output_file_name = OutDir + '/cardiacOutliersWithPeaks.pdf'
    outputCardiacPlots(cardiacTimeSeries, cardiacPeaks, samp_freq,
                       peak_outliers,
                       outlier_ts_ranges, 
                       output_file_name)
else:
    num_files = int(len(cardiacTimeSeries)/pageLimit)
    peak_outliers = np.array(peak_outliers)
    peak_outliers = np.array(peak_outliers)
    for i in range(0,num_files):
        start = pageLimit * i
        end = start + pageLimit
        cardPeaks = cardiacPeaks[(cardiacPeaks >= start) & (cardiacPeaks < end)]-start
        output_file_name = OutDir + '/cardiacOutliersWithPeaks_'+str(i)+'.pdf'
        outputCardiacPlots(cardiacTimeSeries[start:end], 
                       cardPeaks, 
                       samp_freq, peak_outliers-start,
                       outlier_ts_ranges-start, 
                       output_file_name+'_'+str(i))

# Load respiratory time series
print('Load respiratory time series')
with open(respiratoryTimeSeriesFile) as f:
    for line in f:
        respiratoryTimeSeries=[float(line.strip()) for line in f if line.strip()]

# Median shrink respiratory time series to match respiratory peaks and troughs
if expand_factor > 1:
    num_blocks = int(len(respiratoryTimeSeries)/expand_factor)
    inc = 0
    temp = np.array([])
    for i in range(0,num_blocks):
        start = i * expand_factor
        end = start + expand_factor
        temp = np.append(temp, statistics.median(respiratoryTimeSeries[start:end]))
    respiratoryTimeSeries = temp
                
# Load respiratory peaks
print('Load respiratory peaks')
with open(respiratoryPeaksFile) as f:
    for line in f:
        respiratoryPeaks = [int(line.strip()) for line in f if line.strip()]

# Load respiratory troughs
print('Load respiratory troughs')
with open(respiratoryTroughsFile) as f:
    for line in f:
        respiratoryTroughs = [int(line.strip()) for line in f if line.strip()]


# Consider each peak and each trough as a vertex while recording whether each is 
# a peak into a Boolean list If there are more than one peak between two troughs 
# then the region, between the troughs is marked as anomalous. If there are more 
# than one trough between two peaks then the region, between the peaks is marked 
# as anomalous. Assign the vertex values according to equation \ref{respiratoryV2}.
# Build an array of $vec$s, one for each peak, according to equation \ref{vdisttdist2}.

# Get modified peaks (original peaks minus the mean of the adjacent peaks)
print('Get modified peaks (original peaks minus the mean of the adjacent peaks)')
resp_peak_indices, resp_peak_values, resp_outliers = compute_respiratory_peaks(respiratoryTimeSeries, 
    respiratoryPeaks, respiratoryTroughs)

# Build an array of vecs, one for each peak.
print('Build an array of vecs, one for each peak')
vec = np.column_stack([
    resp_peak_values - np.roll(resp_peak_values, 1),
    np.roll(resp_peak_values, -1) - resp_peak_values,
    resp_peak_indices - np.roll(resp_peak_indices, 1),
    np.roll(resp_peak_indices, -1) - resp_peak_indices
])
vec[0] = vec[1]
vec[-1] = vec[0]

# Normalize time differences to be in the same range as the value differences
print('Normalize time differences to be in the same range as the value differences')
scaleFactor = (vec[:,0:1].max()-vec[:,0:1].min())/(vec[:,2:3].max()-vec[:,2:3].min())
vec[:,2:4] =vec[:,2:4]*scaleFactor

# Make NxN matrix where N is the number of segments
# Fill the matrix with the weights
print('Make respiratory weights vector')
weights = squareform(pdist(vec, metric='euclidean'))
weights = np.exp(1/(weights+1))

# Get weight sum vectors and rank indices
print('Get weight sum vectors and rank indices')
vectorWeightSums = np.sum(weights,axis=1)
rankVector = np.argsort(vectorWeightSums)[::-1]

# Identify outliers on low end of the cumulatives weights
print('Identify outliers on low end of the cumulatives weights')
outlier_ts_ranges = cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, 
                                                rankVector, resp_peak_indices)
num_anomalies = len(outlier_ts_ranges)

# PLot respiratory results
print('PLot respiratory results')
y = respiratoryTimeSeries              # length ~24,199
x = np.arange(len(y))             # continuous index
x_scaled = x / samp_freq          # scaled index
cardiacPeaks_scaled = np.array(cardiacPeaks) / samp_freq
respiratoryPeaks_scaled = np.array(respiratoryPeaks) / samp_freq
respiratoryTroughs_scaled = np.array(respiratoryTroughs) / samp_freq

#Limit row width for clarity
points_per_row = 3000
num_rows = int(np.ceil(len(y) / points_per_row))

fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)
if num_rows == 1:
    axes = [axes]

respiratoryTimeSeries = np.array(respiratoryTimeSeries)
respiratoryPeaks = np.array(respiratoryPeaks)
peakVals = respiratoryTimeSeries[respiratoryPeaks]
respiratoryTroughs = np.array(respiratoryTroughs)
troughVals = respiratoryTimeSeries[respiratoryTroughs]

for row in range(num_rows):
    start = row * points_per_row
    end = min((row + 1) * points_per_row, len(y))

    ax = axes[row]

    # --- plot scaled x ---
    ax.plot(
        x_scaled[start:end],
        y[start:end],
        linewidth=0.5,
        solid_capstyle='butt',
        solid_joinstyle='miter',
        color="black"
    )

    # Peaks and troughs
    ax.plot(respiratoryPeaks_scaled, peakVals, "ro")
    ax.plot(respiratoryTroughs_scaled, troughVals, "bo")

    # --- Draw bands (also scaled) ---
    for band_start, band_end in outlier_ts_ranges:
        if band_end <= start or band_start >= end:
            continue
        ax.axvspan(
            max(band_start, start) / samp_freq,
            min(band_end, end) / samp_freq,
            color='red',
            alpha=0.15
        )

    # --- scaled x-limits ---
    ax.set_xlim(x_scaled[start], x_scaled[end - 1])
    ax.set_ylabel("Respiratory Amplitude")

print('Make respiratory axes')
ax.set_xlabel(f"Time (seconds)")
plt.tight_layout()
OutDir = directory
plt.savefig('%s/respOutliersWithPeaks.pdf' % (OutDir))
# plt.savefig(output_file_name)
plt.show()


