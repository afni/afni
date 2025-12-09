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

def getMOD(weights, clusters):
    
    # Determine m, the normalization factor
    upper_tri_exclusive = np.triu(weights, k=1)
    m = np.sum(upper_tri_exclusive)
    TwoM = m * 2
    
    # get vertex and cluster count
    numVertices = np.shape(weights)[0]
    numClusters = len(clusters)
    
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

def cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, rankVector, 
                                         cardiacPeaks):

    # Calculate Q1 and Q3
    q1, q3 = np.percentile(vectorWeightSums, [25, 75])
    
    # Calculate IQR
    iqr = q3 - q1
    
    # Define lower bound
    lower_bound = q1 - 1.5 * iqr
    secondary_lower_bound = q1 - 1.4 * iqr
    
    # Identify low-end outliers
    low_end_outliers = vectorWeightSums[vectorWeightSums < lower_bound]
    secondary_low_end_outliers = vectorWeightSums[vectorWeightSums < 
            secondary_lower_bound] 
    secondary_low_end_outliers = secondary_low_end_outliers[i not in low_end_outliers]
    
    # Get outlier peak indices
    num_outliers = len(low_end_outliers)
    if len(low_end_outliers)>0:
        outlier_peak_indices = rankVector[-len(low_end_outliers):]
    else: outlier_peak_indices = []
    num_secondary_outliers = len(secondary_low_end_outliers)
    secondary_outlier_start = -(num_outliers+num_secondary_outliers)
    if secondary_outlier_start < 0:
        if num_outliers > 0:
            secondary_outlier_peak_indices = rankVector[secondary_outlier_start:-num_outliers]
        else:
            secondary_outlier_peak_indices = rankVector[secondary_outlier_start:]
    
    # Get outlier time series indices
    # limit = len(cardiacPeaks)-1
    outlier_ts_indices = [[cardiacPeaks[i-1], cardiacPeaks[i+2]]  
        for i in outlier_peak_indices]
    secondary_outliers = [[cardiacPeaks[i-1], cardiacPeaks[i+2]]  
        for i in secondary_outlier_peak_indices]
    
    return outlier_ts_indices, secondary_outliers

def getCardiacPeaktPeakOutliers(cardiacTimeSeries, cardiacPeaks):

    # Calculate Q1 and Q3
    q1, q3 = np.percentile(cardiacTimeSeries[cardiacPeaks], [25, 75])
    
    # Calculate IQR
    iqr = q3 - q1
    
    # Define lower bound
    lower_bound = q1 - 2 * iqr
    upper_bound = q3 + 2 * iqr
    
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
    secondaryOutliers = low_outliers + high_outliers
    secondaryOutliers = list(set(secondaryOutliers) - set(outliers))
    
    return outliers, secondaryOutliers

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

for i in range(0, len(sys.argv)):
    match sys.argv[i]:
        case "-directory":
            i = i + 1
            directory = sys.argv[i]
        # Code to execute if subject_string matches "pattern1":

# Hard code filenames which will subsequently be entered as arguments
# directory = '/home/peterlauren/retroicor/retro_2025-12-01-21-20-51/physio_physio_extras/'
cardiacTimeSeriesFile = directory + 'physio_card_filtered_ts_00.1D'
cardiacPeaksFile = directory + 'physio_card_peaks_00.1D'
respiratoryTimeSeriesFile = directory + 'physio_resp_filtered_ts_00.1D'
respiratoryPeaksFile = directory + 'physio_resp_peaks_00.1D'
respiratoryTroughsFile = directory + 'physio_resp_troughs_00.1D'

#Whether to use clustering which tends to be slow
useClustering = False

# Load cardiac time series
with open(cardiacTimeSeriesFile) as f:
    for line in f:
        cardiacTimeSeries=[float(line.strip()) for line in f if line.strip()]
        
# Load cardiac peaks
with open(cardiacPeaksFile) as f:
    for line in f:
        cardiacPeaks = [int(line.strip()) for line in f if line.strip()]

# Plot cardiac peaks on cardiac time series
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
scaleFactor = (vec[:,0:1].max()-vec[:,0:1].min())/(vec[:,2:3].max()-vec[:,2:3].min())
vec[:,2:4] =vec[:,2:4]*scaleFactor

# Make NxN matrix where N is the number of segments
# Fill the matrix with the weights
weights = squareform(pdist(vec, metric='euclidean'))
weights = 1/(weights+1)

# Each vertex is initially regarded as a community or cluster.
numPeaks = len(cardiacTimeSeries)
clusters = [[i] for i in range(0,numPeaks)]

# Make an array of clusters where each cluster is an array of members containing 
# the iindices of the cluster members. 
numPeaks = len(cardiacPeaks)
clusters = [[i] for i in range(0,numPeaks)]

# Starting with the first cluster (vertex) merge in other clusters only in cases 
# where MOD is increased by doing so.
clusters = [[i] for i in range(numPeaks)]

# Merge clusters, starting with merginging into the cluster (initially single
# vertex) with the highest sum of weights with other vertices and working down
# towards the vertex with the lowest cumulative weights.
vectorWeightSums = np.sum(weights,axis=1)
rankVector = np.argsort(vectorWeightSums)[::-1]

if useClustering:
    final_clusters, bestMOD = merge_clusters_by_MOD(weights, clusters, rankVector, getMOD)

# Identify outliers on low end of the cumulatives weights
outlier_ts_ranges, secondary_ts_outlier_ranges = cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, 
                                                rankVector, cardiacPeaks)
num_anomalies = len(outlier_ts_ranges)

# Find peak outliers
peakVals = []
for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
peakRankVector = np.argsort(peakVals)[::-1]
peak_outliers, secondaryPeakOutliers = getCardiacPeaktPeakOutliers(cardiacTimeSeries, 
                                                                   cardiacPeaks)


# Example data
y = cardiacTimeSeries              # length ~24,199
x = np.arange(len(y))             # continuous index

points_per_row = 3000
num_rows = int(np.ceil(len(y) / points_per_row))

fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)

if num_rows == 1:
    axes = [axes]  # ensure iterable
    
peakVals = []
for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
for row in range(num_rows):
    start = row * points_per_row
    end = min((row + 1) * points_per_row, len(y))
    
    ax = axes[row]
    ax.plot(
        x[start:end],
        y[start:end],
        linewidth=0.5,
        solid_capstyle='butt',
        solid_joinstyle='miter',
        color="black"
    )
    ax.plot(cardiacPeaks, peakVals, "bo") # Peaks
    ax.plot(cardiacPeaks[peak_outliers], 
            cardiacTimeSeries[cardiacPeaks[peak_outliers]], "ro") # Peak outliers
    ax.plot(cardiacPeaks[secondaryPeakOutliers], 
            cardiacTimeSeries[cardiacPeaks[secondaryPeakOutliers]], "o", color="pink") # 2ary peak outliers
    
    # Outliers: Draw band only if the band intersects this row’s x-range
    for band_start, band_end in outlier_ts_ranges:
        if band_end <= start or band_start >= end:
            continue

        ax.axvspan(
            max(band_start, start),
            min(band_end, end),
            color='red',
            alpha=0.15,
            zorder=1
        )
    
    # Secondary outliers: Draw band only if the band intersects this row’s x-range
    for band_start, band_end in secondary_ts_outlier_ranges:
        if band_end <= start or band_start >= end:
            continue

        ax.axvspan(
            max(band_start, start),
            min(band_end, end),
            color='yellow',
            alpha=0.75,
            zorder=1
        )

    ax.set_xlim(x[start], x[end - 1])
    ax.set_ylabel(f"ECG Amplitude")
    ax.set_ylabel(f"Time Sample")

axes[-1].set_xlabel("Sample index")
plt.tight_layout()
OutDir = directory
plt.savefig('%s/cardiacOutliersWithPeaks.pdf' % (OutDir))
plt.show()


# Load respiratory time series
with open(respiratoryTimeSeriesFile) as f:
    for line in f:
        respiratoryTimeSeries=[float(line.strip()) for line in f if line.strip()]
        
# Load respiratory peaks
with open(respiratoryPeaksFile) as f:
    for line in f:
        respiratoryPeaks = [int(line.strip()) for line in f if line.strip()]

# Load respiratory troughs
with open(respiratoryTroughsFile) as f:
    for line in f:
        respiratoryTroughs = [int(line.strip()) for line in f if line.strip()]


# Consider each peak and each trough as a vertex while recording whether each is a peak into a Boolean list


# If there are more than one peak between two troughs then the region, between the troughs is marked as anomalous.


# If there are more than one trough between two peaks then the region, between the peaks is marked as anomalous. 


# Assign the vertex values according to equation \ref{respiratoryV2}.


# Build an array of $vec$s, one for each peak, according to equation \ref{vdisttdist2}.

# Get modified peaks (original peaks minus the mean of the adjacent peaks)
resp_peak_indices, resp_peak_values, resp_outliers = compute_respiratory_peaks(respiratoryTimeSeries, 
    respiratoryPeaks, respiratoryTroughs)

# Build an array of vecs, one for each peak.
vec = np.column_stack([
    resp_peak_values - np.roll(resp_peak_values, 1),
    np.roll(resp_peak_values, -1) - resp_peak_values,
    resp_peak_indices - np.roll(resp_peak_indices, 1),
    np.roll(resp_peak_indices, -1) - resp_peak_indices
])
vec[0] = vec[1]
vec[-1] = vec[0]

# Normalize time differences to be in the same range as the value differences
scaleFactor = (vec[:,0:1].max()-vec[:,0:1].min())/(vec[:,2:3].max()-vec[:,2:3].min())
vec[:,2:4] =vec[:,2:4]*scaleFactor

# Make NxN matrix where N is the number of segments
# Fill the matrix with the weights
weights = squareform(pdist(vec, metric='euclidean'))
weights = 1/(weights+1)

# Merge clusters, starting with merginging into the cluster (initially single
# vertex) with the highest sum of weights with other vertices and working down
# towards the vertex with the lowest cumulative weights.
vectorWeightSums = np.sum(weights,axis=1)
rankVector = np.argsort(vectorWeightSums)[::-1]

# Identify outliers on low end of the cumulatives weights
outlier_ts_ranges, secondary_ts_outlier_ranges = cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, 
                                                rankVector, resp_peak_indices)
num_anomalies = len(outlier_ts_ranges)

# Example data
y = respiratoryTimeSeries              # length ~24,199
x = np.arange(len(y))             # continuous index

points_per_row = 3000
num_rows = int(np.ceil(len(y) / points_per_row))

fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)

if num_rows == 1:
    axes = [axes]  # ensure iterable
    
peakVals = []
for i in respiratoryPeaks: peakVals.append(respiratoryTimeSeries[i])
troughVals = []
for i in respiratoryTroughs: troughVals.append(respiratoryTimeSeries[i])
for row in range(num_rows):
    start = row * points_per_row
    end = min((row + 1) * points_per_row, len(y))
    
    ax = axes[row]
    ax.plot(
        x[start:end],
        y[start:end],
        linewidth=0.5,
        solid_capstyle='butt',
        solid_joinstyle='miter',
        color="black"
    )
    ax.plot(np.array(respiratoryPeaks), peakVals, "ro") # Peaks
    ax.plot(np.array(respiratoryTroughs), troughVals, "bo") # Peaks
    # ax.plot(cardiacPeaks[outlier_ts_ranges], 
    #         cardiacTimeSeries[cardiacPeaks[peak_outliers]], "ro") # Peak outliers
    # ax.plot(cardiacPeaks[secondaryPeakOutliers], 
    #         cardiacTimeSeries[cardiacPeaks[secondaryPeakOutliers]], "o", color="pink") # 2ary peak outliers
    
    # Outliers: Draw band only if the band intersects this row’s x-range
    for band_start, band_end in outlier_ts_ranges:
        if band_end <= start or band_start >= end:
            continue

        ax.axvspan(
            max(band_start, start),
            min(band_end, end),
            color='red',
            alpha=0.15,
            zorder=1
        )
    
    # Secondary outliers: Draw band only if the band intersects this row’s x-range
    for band_start, band_end in secondary_ts_outlier_ranges:
        if band_end <= start or band_start >= end:
            continue

        ax.axvspan(
            max(band_start, start),
            min(band_end, end),
            color='yellow',
            alpha=0.75,
            zorder=1
        )

    ax.set_xlim(x[start], x[end - 1])
    ax.set_ylabel(f"ECG Amplitude")
    ax.set_ylabel(f"Time Sample")

axes[-1].set_xlabel("Sample index")
plt.tight_layout()
OutDir = directory
plt.savefig('%s/respOutliersWithPeaks.pdf' % (OutDir))
plt.show()


