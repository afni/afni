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
    outlier_peak_indices = rankVector[-len(low_end_outliers):]
    secondary_outlier_start = -(len(low_end_outliers)+len(secondary_low_end_outliers))
    secondary_outlier_peak_indices = rankVector[secondary_outlier_start:-len(low_end_outliers)]
    
    # Get outlier time series indices
    # limit = len(cardiacPeaks)-1
    outlier_ts_indices = [[cardiacPeaks[i-1], cardiacPeaks[i+2]]  
        for i in outlier_peak_indices]
    secondary_outliers = [[cardiacPeaks[i-1], cardiacPeaks[i+2]]  
        for i in secondary_outlier_peak_indices]
    
    return outlier_ts_indices, secondary_outliers

# Hard code filenames which will subsequently be entered as arguments
directory = '/home/peterlauren/retroicor/retro_2025-12-01-21-20-51/physio_physio_extras/'
cardiacTimeSeriesFile = directory + 'physio_card_filtered_ts_00.1D'
cardiacPeaksFile = directory + 'physio_card_peaks_00.1D'

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


# Example data
y = cardiacTimeSeries              # length ~24,199
x = np.arange(len(y))             # continuous index

points_per_row = 3000
num_rows = int(np.ceil(len(y) / points_per_row))

fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)

if num_rows == 1:
    axes = [axes]  # ensure iterable
    
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
OutDir = "."
plt.savefig('%s/cardiacOutliers.pdf' % (OutDir))
plt.show()




















# Show anomalous regions on cardiac time series
if len(outlier_ts_indices) > 0:
    x = []    
    end = len(cardiacTimeSeries)
    for i in range(0,end): x.append(i)
    plt.subplot(211)
    plt.plot(x, cardiacTimeSeries, "black", linewidth=0.2) #Lines connecting peaks and troughs
    if len(cardiacPeaks) > 0:
        peakVals = []
        for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
        plt.xlim(0,3000)  # Zoom in on plot
        plt.plot(cardiacPeaks, peakVals, "go") # Peaks
        plt.plot(1908, cardiacTimeSeries[1908], "ro") # Peaks
        # plt.savefig('%s/Outlier1.pdf' % (OutDir))








# Find smallest clusters
cluster_sizes = [len(sublist) for sublist in final_clusters]


# Manually save example of what were observed as the smallest clusters in the
# final clusters set
OutDir = "."

# Plot cardiac peaks on cardiac time series
x = []    
end = len(cardiacTimeSeries)
for i in range(0,end): x.append(i)
plt.subplot(211)
plt.plot(x, cardiacTimeSeries, "gray") #Lines connecting peaks and troughs
if len(cardiacPeaks) > 0:
    peakVals = []
    for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
    plt.xlim(1608,2208)  # Zoom in on plot
    plt.plot(cardiacPeaks, peakVals, "go") # Peaks
    plt.plot(1908, cardiacTimeSeries[1908], "ro") # Peaks
    plt.savefig('%s/Outlier1.pdf' % (OutDir))

# Plot cardiac peaks on cardiac time series
x = []    
end = len(cardiacTimeSeries)
for i in range(0,end): x.append(i)
plt.subplot(211)
plt.plot(x, cardiacTimeSeries, "gray") #Lines connecting peaks and troughs
if len(cardiacPeaks) > 0:
    peakVals = []
    for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
    plt.xlim(4774,5304)  # Zoom in on plot
    plt.plot(cardiacPeaks, peakVals, "go") # Peaks
    plt.plot(5074, cardiacTimeSeries[5074], "ro") # Peaks
    plt.savefig('%s/Outlier2.pdf' % (OutDir))

# Plot cardiac peaks on cardiac time series
x = []    
end = len(cardiacTimeSeries)
for i in range(0,end): x.append(i)
plt.subplot(211)
plt.plot(x, cardiacTimeSeries, "gray") #Lines connecting peaks and troughs
if len(cardiacPeaks) > 0:
    peakVals = []
    for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
    plt.xlim(1000,1800)  # Zoom in on plot
    plt.plot(cardiacPeaks, peakVals, "go") # Peaks
    plt.plot(1400, cardiacTimeSeries[1400], "ro") # Peaks
    plt.savefig('%s/Outlier3.pdf' % (OutDir))

    

# Remove adopted clusters from the list of lists.

# Move through each cluster and merge with each of the other clusters where 
# doing so increases MOD.

# When MOD no longer increases beyond a predefined threshold, stop.

# The smallest clusters, or clusters less than a certain size, are flagged as 
# cardiac outliers.
