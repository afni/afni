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

# Hard code filenames which will subsequently be entered as arguments
directory = '/home/peterlauren/retroicor/retro_2025-12-01-21-20-51/physio_physio_extras/'
cardiacTimeSeriesFile = directory + 'physio_card_filtered_ts_00.1D'
cardiacPeaksFile = directory + 'physio_card_peaks_00.1D'

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

vectorWeightSums = np.sum(weights,axis=1)
rankVector = np.argsort(vectorWeightSums)[::-1]

final_clusters, bestMOD = merge_clusters_by_MOD(weights, clusters, rankVector, getMOD)

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
    plt.xlim(21137,21837)  # Zoom in on plot
    plt.plot(cardiacPeaks, peakVals, "go") # Peaks
    plt.plot(21537, cardiacTimeSeries[21537], "ro") # Peaks
    plt.savefig('%s/Outlier3.pdf' % (OutDir))

    

# Remove adopted clusters from the list of lists.

# Move through each cluster and merge with each of the other clusters where 
# doing so increases MOD.

# When MOD no longer increases beyond a predefined threshold, stop.

# The smallest clusters, or clusters less than a certain size, are flagged as 
# cardiac outliers.
