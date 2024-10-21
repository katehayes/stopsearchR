# stopsearchR
##Data sources
**Police stop & search**<br>
Data.police.uk (https://data.police.uk) makes lots of stop & search data available. It offers an API - but, on exploration, it seems like the API only gives access to s&s data back as far as 2021. 

The site also has an 'archive' page (https://data.police.uk/data/archive/), where a number of zipped folders are available for download. Each folder contains a number of subfolders containing one month's worth of data. For every month, there is one csv for each police force, for each data series (crime, outcome, stop & search data) - i.e. there are lots of files inside folders inside folders. The archive hosts data from Dec 2010 onwards - the stop & search data in particular goes back to about 2015 (depends on police force). 
<br><br>
**Boundaries, roads, police stations etc.**<br>
The ONS Open Geography Portal (https://geoportal.statistics.gov.uk/)

##Plots
Just looking at stop and searches in the borough of Tower Hamlets here. One particular LSOA - 'Tower Hamlets 021D' in Whitechapel - is recorded as the location of 4654 searches across the period 2016 to 2024. No other LSOA in Tower Hamlets records searches at anything close to this frequency - searches happen in 'Tower Hamlets 021D' more than twice as often as the next most frequently searched LSOA in Tower Hamlets.<br>
![plot_th_ss_space](https://github.com/katehayes/stopsearchR/blob/main/plots/plot_th_ss_space.png)
<br>


