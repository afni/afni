#!/bin/bash

maxl=$1
codespell 2>&1 | sed -e 's,.*: \(.*\) ==>.*,\1,g' | sort | uniq -c | sort -n | grep -v WARNING | awk '{print $2}' | tr 'A-Z' 'a-z' | sort | uniq | grep -E "^.{1,$maxl}\$"
