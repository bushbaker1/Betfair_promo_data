# Betfair_promo_data
Betfair offers free data from the following url, https://promo.betfair.com/betfairsp/prices/index.php. Repository has R script for gathering, appending csv files for Australian races (win market) and cleaning data into a format ready for analysis  

The R script is designed to automatically retrieve all csv files relating to horse races (thoroughbreds & harness) from the following url https://promo.betfair.com/betfairsp/prices/index.php

It is for Australian races only however script could easily be modified to accomodate other countries.

The script gathers all csv files that are available at the time the script is executed. To account for track commission 
a proxy of 7% is used. In the script there is a csv file you can download which has all the commission rate changes between 2015 and December 2018 together with full track names, state of track and meeting type. However the script does not incorporate this information.

All the best! 
