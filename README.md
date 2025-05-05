# master_thesis_rcode

This repository contains the r code, auxiliary data and results of my master thesis: "Applicability of Benford's law to movement data of red deer in game enclosures and comparison with data from free-ranging red deer"

Auxiliary data/code:
  - benford.csv (contains a df with perfectly benford distributed data)
  - library.R (library for ids, weight, sex, age, etc. of each red deer individual)

Data management and extraction
  - red deer_enclosures_7.R (extracts information from .kml files, filters GPS points and calculates distances between them; alco extracts leading digits of said distances, in this case from free-ranging red deer, filters are according to chapter 3.2 in master thesis)
  - free_ranging_red_deer.R (extracts information from .kml files, filters GPS points and calculates distances between them; alco extracts leading digits of said distances, in this case from red deer in enclosures, filters are according to chapter 3.2 in master thesis)
  - red_deer_enclosures_5.R (extracts information from .kml files, filters GPS points and calculates distances between them; alco extracts leading digits of said distances, in this case fromred deerin enclosures, filters are according to chapter 3.1 in master thesis)

Statistics
  - results_SSD_MAD.R (Code for calculations of counts and frequencies of the leading digits, as well as calculations of SSD and MAD of each subset)

results
  - frequency_count_result.csv (results of counts and frequencies of the leading digits of each subset)
  - statistics_results.csv (results of SSD and MAD of each subset)
