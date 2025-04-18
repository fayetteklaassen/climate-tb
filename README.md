# Read me CLIMATE data

## Overview folders / structure

* note, the data will all NOT be uploaded on GitHub, the structure of the files/folders described below is more for the internal referencing of files.

- [data-raw/] contains the raw data, as received from partners / downloaded. These are not uploaded on github, but archived on our repository, as not all are for public use. There is further hierarchy here:
  - TB/
    - 2007-2014/
    - 2015-2018/
    - 2019-2024/
  - nutrition/
    - IPC/
    - FSNA/

- [data-clean/] contains the cleaned data, creating .csv files for the files from the raw data
The files here follow the same folder and name structure as the raw data, to keep clear connection which clean data came from which raw data

- [data-products/] contains the final 'to-use' data,named consicely for what is contained in that data, e.g.: 'notifications.csv' or 'outcomes-by-history.csv' or 'notifications-by-age.csv' or 'notifications-imputed.csv'

- [R/] contains the R scripts used to do any merging and imputing, or helper scripts. Naming conventions are: <action>-<file>.R, e.g.: 'clean-TB_20072014_Karamoja.R', 'make-notifications.R' or 'impute-notifications.R' 

## Notes

