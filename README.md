cpcPrecipitation
================

R code to download and analyze global precipitation data from the Climate Prediction Center (CPC)

Global Precipitation Data from Climate Prediction Center (CPC)
--------------------------------------------------------------

Hydrological and climatological studies require rainfall data over the entire world for long periods of time. The Climate Prediction Center's [(CPC)](http://www.cpc.ncep.noaa.gov/) daily data, from 1979 to present, at a spatial resolution of 0.5 degrees lat-lon (~ 50 km at the equator) is a good resource. This data is available at CPC's ftp site (ftp://ftp.cpc.ncep.noaa.gov/precip/CPC_UNI_PRCP/GAUGE_GLB/). However, there are a number of issues when it comes to processing and analyzing the data:
* there are too many files (365/366 per year * 34 years, separate folder for each year)
* file naming conventions have chnaged over time, one format prior to 2006 and couple of different formats afterwards
* file formats have changed over time, gzipped files prior to 2008 and binary files afterwards
* moreover, downloading multiple files simultaneously from the CPC ftp site does not seem to work properly.

R code
--------------------------------------------------------------

The following R code makes the task of downloading the above data and subsequent processing and plotting very easy. 
* "cpc_lib.R" - library of functions to download and process/reformat the data
* "cpc_obtain_analyze_data.R" - invokes the functions from the above library and demonstrates how to obtain and plot the data for three time periods: Hurricane Andrew (1992), Hurricane Katrina (2005) and Hurricane Sandy (2012).
