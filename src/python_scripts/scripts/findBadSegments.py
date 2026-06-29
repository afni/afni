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
import os
import statistics
from afnipy import lib_physio_opts    as lpo
from afnipy import lib_physio_reading as lpr
from pathlib import Path
from scipy.sparse import csr_matrix

def louvain_phase1(weights, max_iter=100):

    # Convert to sparse CSR format
    W = csr_matrix(weights)
    W.setdiag(1e-6)

    n = W.shape[0]

    # Initial community assignment
    labels = np.arange(n)

    # Node weighted degrees
    k = np.array(W.sum(axis=1)).flatten()

    # Total graph weight *2
    m2 = k.sum()

    # Community total degrees
    comm_tot = k.copy()

    improved = True
    iteration = 0
    
    # Reduce modularity in order to get low end (by size) outlier clusters
    # gamma = 1.001
    gamma = 1.003


    while improved and iteration < max_iter:

        improved = False
        iteration += 1
        total_gain = 0

        for node in np.random.permutation(n):
        
            current_comm = labels[node]
            ki = k[node]
        
            # neighbors
            start = W.indptr[node]
            end = W.indptr[node + 1]
        
            neighbors = W.indices[start:end]
            weights_to_neighbors = W.data[start:end]
        
            # weights from node to neighboring communities
            neigh_comm_wts = {}
        
            for nbr, wt in zip(neighbors, weights_to_neighbors):
        
                nbr_comm = labels[nbr]
        
                neigh_comm_wts[nbr_comm] = (
                    neigh_comm_wts.get(nbr_comm, 0.0) + wt
                )
        
            # remove node from current community
            comm_tot[current_comm] -= ki
        
            best_comm = current_comm
            best_gain = 0.0
        
            for test_comm, ki_in in neigh_comm_wts.items():
        
                # modified Louvain gain
                gain = ki_in - gamma * (comm_tot[test_comm] * ki) / m2
            
                if gain > best_gain:
                    best_gain = gain
                    best_comm = test_comm
        
            # restore/add node to chosen community
            comm_tot[best_comm] += ki
            
            if best_comm != current_comm:
                total_gain += best_gain
                labels[node] = best_comm
                improved = True
        
        print('Num communities = ', len(np.unique(labels)), '. total_gain = ', total_gain)
                    
    clusters = clusters_from_labels(labels)
    return clusters

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
    mod = 0.0
    
    for i in range(numClusters):
    
        cluster = set(clusters[i])   # if not already a set
    
        insum = 0.0
        totSum = 0.0
    
        row = weights[i]
    
        for j in range(numVertices):
            if j != i:
                w = row[j]
                totSum += w
    
                if j in cluster:
                    insum += w
    
        mod += insum / TwoM - (totSum / TwoM) ** 2    
        
    return mod

def clusters_from_labels(labels):

    d = defaultdict(list)

    for v, c in enumerate(labels):
        d[c].append(v)

    return [d[k] for k in sorted(d)]

# Use MOD as basis to merge clusters
def merge_clusters_by_MOD(weights, clusters, rankVector, getMOD):
    numVertices = len(clusters)

    # --- Start with a label per vertex ---
    labels = np.arange(numVertices)
    
    k = np.array(weights.sum(axis=1)).flatten()
    m2 = k.sum()

    # initial modularity
    bestMOD = getMOD(weights, clusters_from_labels(labels))
    
    # --- Process vertices in ranking order ---
    print('Process vertices in ranking order')
    for node in rankVector:
        Cu = labels[node]

        # collect all existing cluster IDs
        clusterIDs = list(set(labels))

        # test merging Cu with every other cluster Cj
        numClusters = len(clusterIDs)
        print('Number of clusters = ', numClusters)
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

        # Break if number of clusters not reduced
        clusterIDs = list(set(labels))
        if (len(clusterIDs) == numClusters): break

    # Rebuild final cluster list
    print('Rebuild final cluster list')
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

def getOutliersFromClusters(cluster_sizes, clusters):

    # Log transform cluster sizes as size distribution tends to be skewed.
    log_sizes = np.log(cluster_sizes)
    
    # Get first and third quartiles
    q1, q3 = np.percentile(log_sizes, [25, 75])
    
    # Get inter-quartile range
    iqr = q3 - q1
    
    # Get lower bound for non-outliers
    lower_bound = q1 - 1.5 * iqr
    
    # Get true outliers
    outliers = []
    for i in range(len(cluster_sizes)):
        if log_sizes[i] < lower_bound:
            outliers.extend(clusters[i])
    
    return outliers

def getCardiacPeaktPeakOutliers(cardiacTimeSeries, cardiacPeaks):

    # Calculate Q1 and Q3
    q1, q3 = np.percentile(cardiacTimeSeries[cardiacPeaks], [25, 75])
    
    # Calculate IQR
    iqr = q3 - q1
    
    # Define lower bound
    lower_bound = q1 - 2 * iqr
    
    # Get true outliers
    low_outliers = [
    i
    for i in range(len(cardiacPeaks))
    if cardiacTimeSeries[cardiacPeaks[i]] < lower_bound
    ]
    outliers = low_outliers    
    
    # Define lower bound
    lower_bound = q1 - 1.9 * iqr
    
    # Get marginal (just missed out) outliers
    low_outliers = [
    i
    for i in range(len(cardiacPeaks))
    if cardiacTimeSeries[cardiacPeaks[i]] < lower_bound
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

def getCardiacAndRespiratoryTimeSeriesPeaksTroughs(directory):
    print('Get cardiac peaks file and respiratory peaks and troughs file')
    cardiacPeaksFile = directory + 'physio_card_peaks_00.1D'
    respiratoryPeaksFile = directory + 'physio_resp_peaks_00.1D'
    respiratoryTroughsFile = directory + 'physio_resp_troughs_00.1D'
    
    # Load cardiac time series
    print('Load cardiac time series')
    with open(cardiacTimeSeriesFile) as f:
        for line in f:
            cardiacTimeSeries=[float(line.strip()) for line in f if line.strip()]            
        
    # Load cardiac peaks
    print('Load cardiac peaks')
    with open(cardiacPeaksFile) as f:
        for line in f:
            cardiacPeaks = [int(line.strip()) for line in f if line.strip()]

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

    return (
        cardiacTimeSeries,
        cardiacPeaks,
        respiratoryTimeSeries,
        respiratoryPeaks,
        respiratoryTroughs,
    )

def outputRespiratoryPlots(respiratoryTimeSeries, respiratoryPeaks, 
                       respiratoryTroughs, samp_freq,
                       peak_outliers, 
                       outlier_ts_ranges, troughPeakMismatchRanges,
                       output_file_name):
    y = respiratoryTimeSeries              # length 
    x = np.arange(len(y))             # original index
    x_scaled = x / samp_freq          # scaled index
    respiratoryPeaks_scaled = np.array(respiratoryPeaks) / samp_freq
    respiratoryTroughs_scaled = np.array(respiratoryTroughs) / samp_freq

    # Limit length of each row for clarity
    print('Limit length of each row for clarity')
    points_per_row = 3000             
    num_rows = int(np.ceil(len(y) / points_per_row))

    fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)
    if num_rows == 1:
        axes = [axes]

    # Ensure index array is of integer type
    if respiratoryPeaks.dtype!=int:
        respiratoryPeaks = respiratoryPeaks.astype(int)
      
    # Get peak values
    respiratoryTimeSeries = np.array(respiratoryTimeSeries)
    respiratoryPeaks = np.array(respiratoryPeaks)
    peakVals = respiratoryTimeSeries[respiratoryPeaks]
    respiratoryTroughs = np.array(respiratoryTroughs)
    troughVals = respiratoryTimeSeries[respiratoryTroughs]    

    # Output rows
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
    
        # --- Draw  peak-trough mismatch regions bands (also scaled) ---
        for band_start, band_end in outlier_ts_ranges:
            if band_end <= start or band_start >= end:
                continue
            ax.axvspan(
                max(band_start, start) / samp_freq,
                min(band_end, end) / samp_freq,
                color='red',
                alpha=0.15
            )
    
        # --- Draw generically bad regions bands (also scaled) ---
        for bandrange in troughPeakMismatchRanges:
            # print("DEBUG:", bandrange)
            if bandrange[1] <= start or bandrange[0] >= end:
                continue
            ax.axvspan(
                max(bandrange[0], start) / samp_freq,
                min(bandrange[1], end) / samp_freq,
                color='blue',
                alpha=0.15
            )
        # --- scaled x-limits ---
        ax.set_xlim(x_scaled[start], x_scaled[end - 1])
        ax.set_ylabel("Respiratory Amplitude")

    # Set axes and save plot to file
    print('Set axes and save plot to file')
    ax.set_xlabel(f"Time (seconds)")
    axes[-1].set_xlabel("Time (s)")
    plt.tight_layout()

    plt.savefig(output_file_name)
    plt.show()
    
    return (
        peakVals,
        troughVals
        )

    # Analyze cases where peaks and troughs are mismatched
    # analyzePeakTroughMismatches(troughPeakMismatchRanges, respiratoryPeaks,
    #     peakVals, respiratoryTroughs, troughVals, cardiacPeaks)
    
    

def outputCardiacPlots(cardiacTimeSeries, cardiacPeaks, samp_freq,
                       peak_outliers, 
                       outlier_ts_ranges, 
                       output_file_name):
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

    # Ensure index array is of integer type
    if cardiacPeaks.dtype!=int:
        cardiacPeaks = cardiacPeaks.astype(int)
      
    # Get peak values
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

    plt.savefig(output_file_name)
    plt.show()
    
def PlotCardiacPeaksOnCardiacTimeSeries(cardiacTimeSeries, cardiacPeaks):
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
        
def buildCardiacPeakVectors(cardiacPeaks, cardiacTimeSeries):
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
    
    return (
        cardiacPeaks, 
        cardiacTimeSeries,
        vec
        )

def makeRankVector(vec):
    # Make NxN matrix where N is the number of segments
    # Fill the matrix with the weights
    print('Make weights matrix')
    weights = squareform(pdist(vec, metric='euclidean'))
    weights = np.exp(1/(weights+1))
        
    # make vector of weight sums and get vector of their ranks starting with the highest weight sum
    print('Make weight sum and rank vectors')
    vectorWeightSums = np.sum(weights,axis=1)
    rankVector = np.argsort(vectorWeightSums)[::-1]
    
    return (rankVector, vectorWeightSums, weights)

def buildRespiratoryPeakVectors(resp_peak_values, resp_peak_indices):
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
    
    return (
        resp_peak_values, 
        resp_peak_indices,
        vec
        )
        
def applyClustering(cardiacPeaks, weights):
    print('clustering')
    
    # Make clusters from weights
    clusters = louvain_phase1(weights)
    
    # Get sizes of clusters
    cluster_sizes = [len(cluster) for cluster in clusters]
    
    # Find low end outliers among the cluster sizes()
    outlier_peak_indices = getOutliersFromClusters(cluster_sizes, clusters)
    
    # Get outlier peak indices
    peakVals = []
    for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
    # peakRankVector = np.argsort(peakVals)[::-1]
    peak_outliers = getCardiacPeaktPeakOutliers(cardiacTimeSeries, cardiacPeaks)
    
    # Get outlier time series indices
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
    
    # Sort anomalous bands in  order of location
    sorted_ts_ranges = sorted(outlier_ts_indices, key=lambda item: item[0])
    
    # Merge overlapping ranges
    num_ranges = len(sorted_ts_ranges)
    i = 0
    merged_ranges = []
    
    while i < num_ranges:
        start, end = sorted_ts_ranges[i]
    
        j = i + 1
        while j < num_ranges and sorted_ts_ranges[j][0] <= end:
            end = max(end, sorted_ts_ranges[j][1])
            j += 1
    
        merged_ranges.append([start, end])
        i = j
        
    # Reverse the orders of the merged ranges
    merged_ranges = merged_ranges[::-1]

    return (
        sorted_ts_ranges,
        peak_outliers,
        merged_ranges,
    )
        
def findAnomalousBands(vectorWeightSums, rankVector, cardiacPeaks):
    # Identify outliers on low end of the cumulative weights
    print('Identify outliers on low end of the cumulatives weights')
    outlier_ts_ranges = cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, 
                                                    rankVector, cardiacPeaks)
    
    # Find peak outliers
    print('Find peak outliers')
    peakVals = []
    for i in cardiacPeaks: peakVals.append(cardiacTimeSeries[i])
    # peakRankVector = np.argsort(peakVals)[::-1]
    peak_outliers = getCardiacPeaktPeakOutliers(cardiacTimeSeries, cardiacPeaks)
    
    # Sort anomalous bands in  order of location
    sorted_ts_ranges = sorted(outlier_ts_ranges, key=lambda item: item[0])
    
    # Merge overlapping ranges
    num_ranges = len(sorted_ts_ranges)
    i = 0
    merged_ranges = []
    
    while i < num_ranges:
        start, end = sorted_ts_ranges[i]
    
        j = i + 1
        while j < num_ranges and sorted_ts_ranges[j][0] <= end:
            end = max(end, sorted_ts_ranges[j][1])
            j += 1
    
        merged_ranges.append([start, end])
        i = j
        
    # Reverse the orders of the merged ranges
    merged_ranges = merged_ranges[::-1]

    return (
        outlier_ts_ranges,
        peak_outliers,
        merged_ranges,
    )

def correctCardiacPeaks(cardiacPeaks):
    # Get median cardiac peak spacing
    median_peak_spacing = int(np.median([b - a for a, b in zip(cardiacPeaks[:-1], cardiacPeaks[1:])]))
    
    # Replace cardiac peaks in ranges with peaks spaced at roughly the median spacing
    num_ranges = len(merged_ranges)
    i = 0
    while i < num_ranges:
        width = merged_ranges[i][1] - merged_ranges[i][0]
        num_peaks = int(np.round(width/median_peak_spacing))
        new_peaks = []
        for j in range(1,num_peaks): new_peaks.append(merged_ranges[i][0] + 
                                                      (j*median_peak_spacing))
        
        start, end = merged_ranges[i]
    
        mask = (cardiacPeaks < start) | (cardiacPeaks > end)
        cardiacPeaks = np.concatenate([cardiacPeaks[mask], new_peaks])
        cardiacPeaks.sort()  
        
        i = i + 1
        
    return cardiacPeaks

def writeRespiratoryResultsToFiles(OutDir, respiratoryTimeSeries, 
            respiratoryPeaks, respiratoryTroughs, samp_freq, peak_outliers, 
            outlier_ts_ranges, troughPeakMismatchRanges, useClustering):
    
    print('Write respiratory results to files')

    # Create output directory if it doesn't already exist
    if OutDir[-1] == '/':
        OutDir = OutDir[:-1]
    Path(OutDir).mkdir(parents=True, exist_ok=True)
    
    # Write corrected cardiac peaks to file.
    print('OutDir = '+OutDir)
    np.savetxt(OutDir + '/correctedRespiratoryPeaks_1D.txt', respiratoryPeaks, 
               fmt="%.2f")
                
    if useClustering:
        output_file_name = OutDir + '/respOutliersWithPeaks_clustering.pdf'
    else:
        output_file_name = OutDir + '/respOutliersWithPeaks_LECW.pdf'

    (
     respiratoryPeakVals, 
     respiratoryTroughVals,
     ) = outputRespiratoryPlots(respiratoryTimeSeries, respiratoryPeaks, 
                           respiratoryTroughs, samp_freq,
                           peak_outliers,
                           outlier_ts_ranges, troughPeakMismatchRanges,
                           output_file_name)

    # Analyze cases where peaks and troughs are mismatched
    analyzePeakTroughMismatches(troughPeakMismatchRanges, respiratoryPeaks,
        respiratoryPeakVals, respiratoryTroughs, respiratoryTroughVals, cardiacPeaks)
    
    # Make corrected respiratory time series
    makeCorrectedRespiratoryTimeSeries(respiratoryTimeSeries, respiratoryPeaks, 
                respiratoryTroughs, outlier_ts_ranges, OutDir, samp_freq)
    
    return (
     peak_outliers,
     outlier_ts_ranges,
     respiratoryPeakVals, 
     respiratoryTroughs, 
     respiratoryTroughVals
     )
    
def writeCardiacResultsToFiles(OutDir, cardiacTimeSeries, cardiacPeaks, samp_freq,
                           peak_outliers, outlier_ts_ranges, useClustering):
    
    print('Write cardiac results to files')

    # Create output directory if it doesn't already exist
    if OutDir[-1] == '/':
        OutDir = OutDir[:-1]
    Path(OutDir).mkdir(parents=True, exist_ok=True)
    
    # Write corrected cardiac peaks to file.
    print('OutDir = '+OutDir)
    np.savetxt(OutDir + '/correctedCardiacPeaks_1D.txt', cardiacPeaks, fmt="%.2f")
                
    if useClustering:
        output_file_name = OutDir + '/cardiacOutliersWithPeaks_clustering.pdf'
    else:
        output_file_name = OutDir + '/cardiacOutliersWithPeaks_LECW.pdf'

    outputCardiacPlots(cardiacTimeSeries, cardiacPeaks, samp_freq,
                           peak_outliers,
                           outlier_ts_ranges, 
                           output_file_name)
    
    # Make corrected cardiac time series
    makeCorrectedCardiacTimeSeries(cardiacTimeSeries, cardiacPeaks, 
            outlier_ts_ranges, OutDir, samp_freq)
    
def merge_ranges(ranges):
    if not ranges:
        return []

    # sort by start value
    ranges = sorted(ranges, key=lambda x: x[0])

    merged = [ranges[0]]

    for current in ranges[1:]:
        previous = merged[-1]

        # overlap (or touching ranges if you use <=)
        if current[0] <= previous[1]:
            previous[1] = max(previous[1], current[1])
        else:
            merged.append(current)

    return merged
    
def makeCorrectedRespiratoryTimeSeries(respiratoryTimeSeries, respiratoryPeaks, 
            respiratoryTroughs, outlier_ts_ranges, OutDir, samp_freq):
    
    # Keep separate list of new points to display added points in green.
    added_peaks = []
    added_troughs = []
    
    # Merge overlapping peaks
    merged_outlier_ts_ranges = merge_ranges(outlier_ts_ranges)
    
    # Estimate global respiratory period
    # resp_intervals = np.diff(respiratoryPeaks)

    # Build valid respiratory cycles
    valid_cycles = []
    
    respiratoryPeaks = np.asarray(respiratoryPeaks)
    respiratoryTroughs = np.asarray(respiratoryTroughs)

    for i in range(len(respiratoryPeaks)-1):
    
        p1 = respiratoryPeaks[i]
        p2 = respiratoryPeaks[i+1]
    
        troughs = respiratoryTroughs[
            (respiratoryTroughs > p1) &
            (respiratoryTroughs < p2)
        ]
    
        if len(troughs) == 1:
            valid_cycles.append(
                (p1, troughs[0], p2)
            )
        
    # calculate respiratory phase once
    peak_to_trough = []
    trough_to_peak = []
    
    for p1,t,p2 in valid_cycles:
        peak_to_trough.append(t-p1)
        trough_to_peak.append(p2-t)
        
    if len(valid_cycles) > 0:
    
        frac = np.median(
            np.array(peak_to_trough) /
            (np.array(peak_to_trough)+np.array(trough_to_peak))
        )
    
    else:
        frac = 0.5   # fallback: trough halfway between peaks    
    
    for bad_region in merged_outlier_ts_ranges:
        
        # Remove existing peaks and troughs in bad region
        mask = (
            (respiratoryPeaks < bad_region[0]) |
            (respiratoryPeaks > bad_region[1])
        )        
        respiratoryPeaks = respiratoryPeaks[mask]
        mask = (
            (respiratoryTroughs < bad_region[0]) |
            (respiratoryTroughs > bad_region[1])
        )        
        respiratoryTroughs = respiratoryTroughs[mask]
        
        # Recompute after removing bad peaks
        resp_intervals = np.diff(respiratoryPeaks)
    
        idx_start = np.searchsorted(respiratoryPeaks,bad_region[0])
        idx_end   = np.searchsorted(respiratoryPeaks,bad_region[1])
    
        before = respiratoryPeaks[idx_start-1] if idx_start>0 else None
        after  = respiratoryPeaks[idx_end] if idx_end<len(respiratoryPeaks) else None
    
        if before is None or after is None:
            continue
    
        local_periods = np.concatenate([
            resp_intervals[max(0,idx_start-5):idx_start],
            resp_intervals[idx_end:min(len(resp_intervals),idx_end+5)]
        ])
        
        if len(local_periods) == 0:
            continue
    
        local_period = np.median(local_periods)
    
        n_cycles = round((after-before)/local_period)
        if n_cycles < 1:
            continue
    
        new_peaks = np.linspace(
            before,
            after,
            n_cycles+1
        )[1:-1]
    
        all_peaks = np.concatenate(([before], new_peaks, [after]))
    
        for p1,p2 in zip(all_peaks[:-1],all_peaks[1:]):
            added_troughs.append(
                round(p1 + frac*(p2-p1))
            )
    
        added_peaks.extend(np.round(new_peaks).astype(int))

    respiratoryPeaks = np.sort(
        np.concatenate((respiratoryPeaks, added_peaks))
    )
    
    respiratoryTroughs = np.sort(
        np.concatenate((respiratoryTroughs, added_troughs))
    )
                
    # Identify lack of bijectivity on peaks and troughs
    events = sorted(
        [(x,'P') for x in respiratoryPeaks] +
        [(x,'T') for x in respiratoryTroughs],
        key=lambda x: x[0]
    )
    
    bad_events = []
    for i in range(len(events)-1):
        if events[i][1] == events[i+1][1]:
            bad_events.append(events[i:i+2])
            print("artifact:", events[i:i+2])
            
    # Impose bijectivity on peaks and troughs    
    events = sorted(
        [(x,'P') for x in respiratoryPeaks] +
        [(x,'T') for x in respiratoryTroughs],
        key=lambda x: x[0]
    )
    
    # added_peaks = []
    # added_troughs = []
    
    for i in range(len(events)-1):
    
        x1, type1 = events[i]
        x2, type2 = events[i+1]
    
        if type1 == type2:
    
            if type1 == 'P':
                # PP: missing trough
                trough = round(x1 + frac*(x2-x1))
                added_troughs.append(trough)
    
            else:
                # TT: missing peak
                # approximate inverse phase
                peak = round(x1 - frac*(x1-x2))
                added_peaks.append(peak)
    
    
    respiratoryPeaks = np.sort(
        np.concatenate((respiratoryPeaks, added_peaks))
    )
    
    respiratoryTroughs = np.sort(
        np.concatenate((respiratoryTroughs, added_troughs))
    )

    # Write out a plot of the corrected time series with green peaks (to show
    # the corrections) and bad regions in pink
    writeCorrectedRespiratoryResultsToFiles(respiratoryTimeSeries, 
                respiratoryPeaks, respiratoryTroughs, outlier_ts_ranges, 
                added_peaks, added_troughs, OutDir, samp_freq)
        
    # Merge added points into cardiacPeaks
    
def writeCorrectedRespiratoryResultsToFiles(respiratoryTimeSeries, 
                respiratoryPeaks, respiratoryTroughs, outlier_ts_ranges, 
                added_peaks, added_troughs, OutDir, 
                samp_freq):
    
    print('Write corrected respiratory results to files')

    # Create output directory if it doesn't already exist
    if OutDir[-1] == '/':
        OutDir = OutDir[:-1]
    Path(OutDir).mkdir(parents=True, exist_ok=True)
    
    # Write corrected cardiac peaks to file.
    print('OutDir = '+OutDir)
    np.savetxt(OutDir + '/correctedRespiratoryPeaks_1D.txt', respiratoryPeaks, 
               fmt="%.2f")
                
    if useClustering:
        output_file_name = OutDir + '/correctedRespOutliersWithPeaks_clustering.pdf'
    else:
        output_file_name = OutDir + '/correctedRespOutliersWithPeaks_LECW.pdf'

    (
     respiratoryPeakVals, 
     respiratoryTroughVals,
     ) = outputCorrectedRespiratoryPlots(respiratoryTimeSeries, respiratoryPeaks, 
                           respiratoryTroughs, samp_freq, added_peaks, 
                           added_troughs, outlier_ts_ranges, 
                           output_file_name)

    # Analyze cases where peaks and troughs are mismatched
    # analyzePeakTroughMismatches(troughPeakMismatchRanges, respiratoryPeaks,
    #     respiratoryPeakVals, respiratoryTroughs, respiratoryTroughVals, cardiacPeaks)

def outputCorrectedRespiratoryPlots(respiratoryTimeSeries, respiratoryPeaks, 
                      respiratoryTroughs, samp_freq, added_peaks, 
                      added_troughs, outlier_ts_ranges, 
                      output_file_name):
    y = respiratoryTimeSeries              # length 
    x = np.arange(len(y))             # original index
    x_scaled = x / samp_freq          # scaled index
    respiratoryPeaks_scaled = np.array(respiratoryPeaks) / samp_freq
    respiratoryTroughs_scaled = np.array(respiratoryTroughs) / samp_freq
    addedPeaks_scaled = np.array(added_peaks) / samp_freq
    addedTroughs_scaled = np.array(added_troughs) / samp_freq

    # Limit length of each row for clarity
    print('Limit length of each row for clarity')
    points_per_row = 3000             
    num_rows = int(np.ceil(len(y) / points_per_row))

    fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)
    if num_rows == 1:
        axes = [axes]

    # Ensure index array is of integer type
    if respiratoryPeaks.dtype!=int:
        respiratoryPeaks = respiratoryPeaks.astype(int)
      
    # Get peak and trough values
    respiratoryTimeSeries = np.array(respiratoryTimeSeries)
    respiratoryPeaks = np.array(respiratoryPeaks)
    peakVals = respiratoryTimeSeries[respiratoryPeaks]
    respiratoryTroughs = np.array(respiratoryTroughs)
    troughVals = respiratoryTimeSeries[respiratoryTroughs]    
    addedPeakVals = respiratoryTimeSeries[added_peaks]
    addedTroughVals = respiratoryTimeSeries[added_troughs]

    # Output rows
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

        # Added peaks and troughs
        ax.plot(addedPeaks_scaled, addedPeakVals, "mo")
        ax.plot(addedTroughs_scaled, addedTroughVals, "go")
    
        # --- Draw  peak-trough mismatch regions bands (also scaled) ---
        for band_start, band_end in outlier_ts_ranges:
            if band_end <= start or band_start >= end:
                continue
            ax.axvspan(
                max(band_start, start) / samp_freq,
                min(band_end, end) / samp_freq,
                color='red',
                alpha=0.15
            )
    
        # --- Draw generically bad regions bands (also scaled) ---
        for bandrange in troughPeakMismatchRanges:
            # print("DEBUG:", bandrange)
            if bandrange[1] <= start or bandrange[0] >= end:
                continue
            ax.axvspan(
                max(bandrange[0], start) / samp_freq,
                min(bandrange[1], end) / samp_freq,
                color='blue',
                alpha=0.15
            )
        # --- scaled limits ---
        ax.set_xlim(x_scaled[start], x_scaled[end - 1])
        ax.set_ylim(min(y[start:end]), 
                    max(y[start:end]))
        ax.set_ylabel("Respiratory Amplitude")

    # Set axes and save plot to file
    print('Set axes and save plot to file')
    ax.set_xlabel(f"Time (seconds)")
    axes[-1].set_xlabel("Time (s)")
    plt.tight_layout()

    plt.savefig(output_file_name)
    plt.show()
    
    return (
        peakVals,
        troughVals
        )
    
    
def makeCorrectedCardiacTimeSeries(cardiacTimeSeries, cardiacPeaks, 
                                   outlier_ts_ranges, OutDir, samp_freq):
    
    # Keep separate list of new points to display added points in green.
    added_points = []
    
    # Merge overlapping peaks
    merged_outlier_ts_ranges = merge_ranges(outlier_ts_ranges)
    
    # In every region identified as bad
    for bad_region in merged_outlier_ts_ranges:
    
        # peaks before and after bad region
        idx_start = np.searchsorted(cardiacPeaks, bad_region[0])
        idx_end   = np.searchsorted(cardiacPeaks, bad_region[1])
        
        before = cardiacPeaks[idx_start-1] if idx_start > 0 else None
        after  = cardiacPeaks[idx_end] if idx_end < len(cardiacPeaks) else None
        
        if before is None or after is None:
            continue
        
        # Use local RR intervals
        rr_before = np.diff(cardiacPeaks[max(0, idx_start-5):idx_start])
        rr_after  = np.diff(cardiacPeaks[idx_end:idx_end+6])
        
        # Local estimate of beat interval
        local_rr = np.median(np.concatenate((rr_before, rr_after)))
        
        # Estimate number of missing beats
        N_p = round((after - before) / local_rr)
        
        if N_p < 1:
            continue
        
        # Phase interpolation
        new_peaks = np.linspace(before, after, N_p+1)[1:-1]
        
        added_points.extend(np.round(new_peaks).astype(int))

        
    # Write out a plot of the corrected time series with green peaks (to show
    # the corrections) and bad regions in pink
    writeCorrectedCardiacPeaks(cardiacTimeSeries, cardiacPeaks, 
                outlier_ts_ranges, added_points, OutDir, samp_freq)
        
    # Merge added points into cardiacPeaks
    
    
def writeCorrectedCardiacPeaks(cardiacTimeSeries, cardiacPeaks, 
            outlier_ts_ranges, added_points, OutDir, samp_freq):
    
    print('Write cardiac results to files')

    # Create output directory if it doesn't already exist
    if OutDir[-1] == '/':
        OutDir = OutDir[:-1]
    Path(OutDir).mkdir(parents=True, exist_ok=True)
    
    # Write corrected cardiac peaks to file.
    print('OutDir = '+OutDir)
    np.savetxt(OutDir + '/correctedCardiacPeaks_1D.txt', cardiacPeaks, fmt="%.2f")
                
    if useClustering:
        output_file_name = OutDir + '/correctedCardiacOutliersWithPeaks_clustering.pdf'
    else:
        output_file_name = OutDir + '/correctedCardiacOutliersWithPeaks_LECW.pdf'

    outputCorrectedCardiacPlots(cardiacTimeSeries, cardiacPeaks, samp_freq,
                           added_points,
                           outlier_ts_ranges, 
                           output_file_name)
    
def outputCorrectedCardiacPlots(cardiacTimeSeries, cardiacPeaks, samp_freq,
                       added_points,
                       outlier_ts_ranges, 
                       output_file_name):
    
    y = cardiacTimeSeries              # length 
    x = np.arange(len(y))             # original index
    x_scaled = x / samp_freq          # scaled index
    cardiacPeaks_scaled = np.array(cardiacPeaks) / samp_freq
    addedPeaks_scaled = np.array(added_points) / samp_freq

    # Limit length of each row for clarity
    print('Limit length of each row for clarity')
    points_per_row = 3000             
    num_rows = int(np.ceil(len(y) / points_per_row))

    fig, axes = plt.subplots(num_rows, 1, figsize=(12, 2.5*num_rows), sharex=False)
    if num_rows == 1:
        axes = [axes]

    # Ensure index array is of integer type
    if cardiacPeaks.dtype!=int:
        cardiacPeaks = cardiacPeaks.astype(int)
      
    # Get peak values
    peakVals = cardiacTimeSeries[cardiacPeaks]
    addedPeakVals = cardiacTimeSeries[added_points]

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

        # Added peaks
        ax.plot(addedPeaks_scaled, addedPeakVals, "r+")

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

    plt.savefig(output_file_name)
    plt.show()
   
    
def analyzePeakTroughMismatches(troughPeakMismatchRanges, respiratoryPeaks,
    peakVals, respiratoryTroughs, troughVals, cardiacPeaks):
    
    print('analyzePeakTroughMismatches')
    
    # TODO: Add code
    
#################### MAIN  ##########################################
    
# Defaults
expand_factor = 1 # Reverses the rudtion cause by -prefilt_max_freq in physio_calc
directory = os.getcwd()

#Whether to use clustering which tends to be slow
print('Set clustering to false')
useClustering = False

# Read arguments
print('Read arguments')
OutDir = ''
# args_orig = copy.deepcopy(sys.argv)
# args_dict = lpo.main_option_processing( sys.argv )
# pcobj = lpr.pcalc_obj( args_dict, args_orig=args_orig )
i = 1
import sys

while i < len(sys.argv):
    match sys.argv[i]:
        case "-directory":
            i += 1
            if i >= len(sys.argv):
                print("Error: -directory requires an argument")
                sys.exit()
            directory = sys.argv[i]
            i += 1
            
        case "-card_file":
            i += 1
            if i >= len(sys.argv):
                print("Error: -card_file requires an argument")
                sys.exit()
            cardiacTimeSeriesFile = sys.argv[i]
            i += 1

        case "-resp_file":
            i += 1
            if i >= len(sys.argv):
                print("Error: -resp_file requires an argument")
                sys.exit()
            respiratoryTimeSeriesFile = sys.argv[i]
            i += 1

        case "-freq":
            i += 1
            if i >= len(sys.argv):
                print("Error: -freq requires an argument")
                sys.exit()
            samp_freq = float(sys.argv[i])
            i += 1
            
        case '-expand_factor':
            i += 1
            if i >= len(sys.argv):
                print("Error: -expand_factor requires an argument")
                sys.exit()
            expand_factor = int(sys.argv[i])
            i += 1
            
        case '-dset_tr':
            i += 1
            if i >= len(sys.argv):
                print("Error: -dset_tr requires an argument")
                sys.exit()
            vol_tr = float(sys.argv[i])
            i += 1

        case '-dset_nt':
            i += 1
            if i >= len(sys.argv):
                print("Error: -dset_nt requires an argument")
                sys.exit()
            vol_nv = float(sys.argv[i])
            i += 1

        case '-dset_nslice':
            i += 1
            if i >= len(sys.argv):
                print("Error: -dset_nslice requires an argument")
                sys.exit()
            dset_nslice = float(sys.argv[i])
            i += 1

        case '-prefilt_max_freq':
            i += 1
            if i >= len(sys.argv):
                print("Error: -prefilt_max_freq requires an argument")
                sys.exit()
            prefilt_max_freq = int(sys.argv[i])
            i += 1

        case '-dset_slice_pattern':
            i += 1
            if i >= len(sys.argv):
                print("Error: -dset_slice_pattern requires an argument")
                sys.exit()
            dset_slice_pattern = str(sys.argv[i])
            i += 1

        case '-prefilt_mode':
            i += 1
            if i >= len(sys.argv):
                print("Error: -prefilt_mode requires an argument")
                sys.exit()
            dset_slice_pattern = str(sys.argv[i])
            i += 1

        case '-out_dir':
            i += 1
            if i >= len(sys.argv):
                print("Error: -out_dir requires an argument")
                sys.exit()
            OutDir = str(sys.argv[i])
            i += 1

        case '-do_fix_nan':
            do_fix_nan = 1
            i += 1
            
        case '-do_use_clustering':           
            useClustering = True
            i += 1

        case '-save_proc_peaks':
            save_proc_peaks = 1
            i += 1

        case '-save_proc_troughs':
            save_proc_troughs = 1
            i += 1

        case '-save_proc_troughs':
            save_proc_troughs = 1
            i += 1

        case '-save_proc_filtered_ts':
            save_proc_filtered_ts = 1
            i += 1

        case "-help":
            print("Usage:\n python ./findBadSegments.py -directory <directory>")
            print("\t[-freq <samp_freq>]\n")
            print('where "directory" is the "physio_physio_extras/" directory ') 
            print('produced by physio_calc.py.  samp_freq is the sampling frequency') 
            print('in Hertz') 
            sys.exit()

        case "-h":
            print("-h not recognized.  Did you mean -help?")
            sys.exit()

        case _:
            print(f"Unrecognized option: {sys.argv[i]}")
            print('Use "-help" for correct usage.')
            sys.exit()
            
# Load cardiac and respiratory peaks, troughs and time series
(
    cardiacTimeSeries,
    cardiacPeaks,
    respiratoryTimeSeries,
    respiratoryPeaks,
    respiratoryTroughs,
) = getCardiacAndRespiratoryTimeSeriesPeaksTroughs(directory)

# Process cardiac data

# Plot cardiac peaks on cardiac time series
PlotCardiacPeaksOnCardiacTimeSeries(cardiacTimeSeries, cardiacPeaks)

# Build an array of vecs, one for each peak.
(
    cardiacPeaks, 
    cardiacTimeSeries,
    vec
    ) = buildCardiacPeakVectors(cardiacPeaks, cardiacTimeSeries)

# Make cardiac rank vector
print('Make cardiac rank vector')
(rankVector, vectorWeightSums, weights) = makeRankVector(vec)

# Use clustering if required
if useClustering:   # Not the default as it's very slow
    print('Use clustering for cardiac data')
    (
        outlier_ts_ranges,
        peak_outliers,
        merged_ranges,
    ) = applyClustering(cardiacPeaks, weights)
else:
    # Identify outliers on low end of the cumulative weights
    (
        outlier_ts_ranges,
        peak_outliers,
        merged_ranges,
    ) = findAnomalousBands(vectorWeightSums, rankVector, cardiacPeaks)

cardiacPeaks = correctCardiacPeaks(cardiacPeaks)

writeCardiacResultsToFiles(OutDir, cardiacTimeSeries, cardiacPeaks, samp_freq,
                           peak_outliers, outlier_ts_ranges, useClustering)
        
# Process respiratory data

# Get modified peaks (original peaks minus the mean of the adjacent peaks)
print('Get modified peaks (original peaks minus the mean of the adjacent peaks)')
resp_peak_indices, resp_peak_values, resp_outliers = compute_respiratory_peaks(respiratoryTimeSeries, 
    respiratoryPeaks, respiratoryTroughs)

# Build an array of vecs, one for each peak.
(
    resp_peak_values, 
    resp_peak_indices,
    vec
    ) = buildRespiratoryPeakVectors(resp_peak_values, resp_peak_indices)

# Make respiratory rank vector
print('Make respiratory rank vector')
(rankVector, vectorWeightSums, weights) = makeRankVector(vec)

# Use clustering if required.  Currently applied only to peaks
if useClustering:   # Not the default as it's very slow
    print('Use clustering for respiratory data')
    (
        outlier_ts_ranges,
        peak_outliers,
        merged_ranges,
    ) = applyClustering(resp_peak_indices, weights)
else:
    # Identify outliers on low end of the cumulative weights
    (
        outlier_ts_ranges,
        peak_outliers,
        merged_ranges,
    ) = findAnomalousBands(vectorWeightSums, rankVector, cardiacPeaks)

    # Identify outliers on low end of the cumulative weights
    print('Identify outliers on low end of the cumulatives weights')
    outlier_ts_ranges = cumulatives_weights_low_end_outlier_ranges(vectorWeightSums, 
                                                rankVector, resp_peak_indices)
    
# Identify peak-trough mismatches (only applies to respiratory data)
print('Identify peak-trough mismatches')
respiratoryPeaks_scaled = np.array(respiratoryPeaks) / samp_freq
respiratoryTroughs_scaled = np.array(respiratoryTroughs) / samp_freq
left = np.searchsorted(respiratoryTroughs, respiratoryPeaks[:-1], 
                       side='right')
right = np.searchsorted(respiratoryTroughs, respiratoryPeaks[1:], 
                        side='left')
troughsBetweenPeaks = right - left
mask = troughsBetweenPeaks != 1
respiratoryPeaks = np.array(respiratoryPeaks)
troughPeakMismatchRanges = list(zip(respiratoryPeaks[:-1][mask], 
                  respiratoryPeaks[1:][mask]))

# Plot respiratory results
(
 respiratoryPeakOutliers,
 respiratoryOutlier_ts_ranges,
 respiratoryPeakVals, 
 respiratoryTroughs, 
 respiratoryTroughVals
 ) = writeRespiratoryResultsToFiles(OutDir, respiratoryTimeSeries, 
            respiratoryPeaks, respiratoryTroughs, samp_freq, peak_outliers, 
            outlier_ts_ranges, troughPeakMismatchRanges, useClustering)

# # Analyze cases where peaks and troughs are mismatched
# analyzePeakTroughMismatches(troughPeakMismatchRanges, respiratoryPeaks,
#     respiratoryPeakVals, respiratoryTroughs, respiratoryTroughVals, cardiacPeaks)


