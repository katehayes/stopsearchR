# stopsearchR
## Data sources
**Boundaries, roads, police stations etc.**<br>
Boundaries for administrative regions (LAs, wards) and statistical geographies (LSOAs) come from the ONS Open Geography Portal (https://geoportal.statistics.gov.uk/).<br>
Roads, police stations, rivers etc. are OpenStreetMap data, retrieved using the osmdata package (https://rspatialdata.github.io/osm.html).<br>

**Police stop & search**<br>
Data.police.uk (https://data.police.uk) makes lots of police data - including stop & search data - available. It offers an API - but, on exploration, it seems like the API only gives access to s&s data back as far as 2021. <br>
The site also has an 'archive' page (https://data.police.uk/data/archive/), where a number of zipped folders are available for download. Each folder contains a number of subfolders containing one month's worth of data. For every month, there is one csv for each police force, for each data series (crime, outcome, stop & search data) - i.e. there are lots of files inside folders inside folders. The archive hosts data from Dec 2010 onwards - the stop & search data in particular goes back to about 2015 (depends on police force). <br>
In script [get_ss.R](https://github.com/katehayes/stopsearchR/blob/main/R/get_ss.R), data is extracted from the archive (using function [extract](https://github.com/katehayes/stopsearchR/blob/main/R/functions/extract.R)).<br>
This stop & search data is incident-level. Search location is provided at the LSOA level - for each search we are given the co-ordinates (latitude, longitude) of the midpoint of the LSOA in which the search occurred. In script [process_ss.R](https://github.com/katehayes/stopsearchR/blob/main/R/process_ss.R), more spatial data is added to police datasets, to allow us to map the data. 

## Note re. police data archive
I'm trying to take stock of the archive - figure out what data files are absent/present/present in multiple versions, how to interact with the archive in the most efficient way, etc. Plan: to try develop a package.

![plot_missing_byzip](https://github.com/katehayes/stopsearchR/blob/main/plots/plot_missing_byzip.png)


## Police stop & search in Tower Hamlets at a glance
Just looking here at one London borough, Tower Hamlets. From 2015, searches increased in frequency, before peaking in late 2018 and then falling. Since mid-2022, search frequency has been close to its 2015/16 level. <br>
![plot_th_powers_time](https://github.com/katehayes/stopsearchR/blob/main/plots/plot_th_powers_time.png)
<br>
![plot_th_age_time](https://github.com/katehayes/stopsearchR/blob/main/plots/plot_th_age_time.png)
<br>
One particular LSOA - 'Tower Hamlets 021D' in Whitechapel - is recorded as the location of 4654 searches across the period 2016 to 2024. No other LSOA in Tower Hamlets records searches at anything close to this frequency - searches happen in 'Tower Hamlets 021D' more than twice as often as the next most frequently searched LSOA in Tower Hamlets.<br>
![plot_th_ss_space](https://github.com/katehayes/stopsearchR/blob/main/plots/plot_th_ss_space.png)
<br>





