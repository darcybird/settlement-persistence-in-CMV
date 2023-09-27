# settlement-persistence-in-CMV
Repository for Bird et al. 2023, made public when the manuscript is submitted to JAS.


All site locations are obfuscated, so the script runs with the assistance of dummy cells. *csv files have been uploaded with dummyCell locations rather than actual locations.

The script is finalized according to the way it was run for the paper. There are two key differences between the methods written in the paper and this script that were realized during the course of the script-cleaning process:
1. "Average" Field contention per bout and per phase was instead calculated as the maximum (npt mean) value during the bout or the phase. This may have an effect as field contention is a key variable.
2.  The relative catchment quality variable was calculated on the weighted maize raster rather than the according to the median regional maize quality as described in lines 278-286. We don't expect this to have a large impact on the results.


Darcy Bird 9/26/2023
