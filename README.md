# Read me CLIMATE data

## Overview folders / structure

* note, the data will NOT be uploaded on GitHub, the structure of the files/folders described below is for the internal referencing of files.

- [data-raw/] contains the raw data, as received from partners / downloaded. These are not uploaded on github, but archived on our repository, as not all are for public use. There is further hierarchy here:
  - tb/
    - 20072014/
    - 20152019/
    - 20202024/
  - nutrition/
    - IPC/
    - FSNA/

- [data-clean/] contains the cleaned data, creating .csv files for the files from the raw data
The files here follow the same folder and name structure as the raw data, to keep clear connection which clean data came from which raw data

- [data-products/] contains the final 'to-use' data,named consicely for what is contained in that data, e.g.: 'notifications.csv' or 'outcomes-by-history.csv' or 'notifications-by-age.csv' or 'notifications-imputed.csv'

- [R/] contains the R scripts used to do any merging and imputing, or helper scripts. Naming conventions are: <action>-<file>.R, e.g.: 'clean-tb_20072014.R', 'make-data-products.R' 

## Variable and group naming conventions

- all variables and groups are lower case* (exception first letter of regions), with `-` separating words and `_` separating qualifiers. e.g. `notifications-new-and-relapse` or `notifications-on-treatment` and `age_sex`
- in cleaning, HIV data are removed
- clean data contains regions only within the Karamoja district, or, when unavailable, at the Karamoja or national level
- any NA data in the files is coded as NA in the output. In summing across groups for the final data-products, NAs are omitted, and a 'has_missing' is marked TRUE in the final data.

- *region/topdistrict*: District names contain only the name, and are stripped from 'District' suffixes (e.g., `Abim`, not `Abim Distric`, not `ABIM`). Names are: [Abim, Amudat, Kaabong, Karenga, Kotido, Moroto, Nabilatuk, Nakarapiripirit, Napak]. For the final data, `topdistrict` is used, by summing over the splits over the years.  See `R/helpers.R` for an overview of how they are joined.

- *date*: date is formatted as a zoo::as.yearqtr() variable, of format %Y Q%q (e.g., `2020 Q1`). This format allows for plotting and time analysis. If needed, can be split manually easily

- *slice*: is a created variable, that links all rows of the same slice together (i.e., no double counting of people). This variable is used to identify mismatches between totals for example. They are of format <keyOutput>-<qualifyer>_<grouping>_<grouping> Examples are: [notifications, notifications-new-relapse_age_sex, notifications_diagnosis_tbhistory, outcomes_age_sex, outcomes_diagnosis_tbhistory_sex, outcomes-died]

- *tbhistory*: describes the known TB history. Values are [new, relapse, new-and-relapse, unknown(includes other(eptb), thu), ltfu (also includes default), failure (no cure at end of treatment), all]

- *diagnosis*: describes the known diagnosis.Values are [cd (clinical diagnosis, includes NSD, NSDnegative, negative), bc (bacteriological confirmed, includes positive), eptb, unknown(includes other), all]

- *sex*: female, male, all

- *age*: all, yXX-XX

- *outcomes*: values are [notified, on-treatment, cured, completed, ltfu, failure, died, transfer]

### Data products
 
For the current analysis, EPTB (extra pulmonary TB) is always omitted, as it is not infectious.

## TB notifications and outcomes, a review of the slices

Multiple slices describe the TB notifications, with slightly different totals
- 2007-2014: 1 slice with notifications (`notifications_diagnosis_tbhistory_sex`)
- 2015-2019: 3 slices with notifications (`notifications` (primary source); `notifications_sex_age`, and `notifications_diagnosis_tbhistory_sex`)
- 2020-2024: 4 slices with notification information (`notifications` (primary source); `notifications-new-relapse_age_sex` (only new and relapse notifications by age and sex); `notifications-on-treatment` (only those started on treatment, slight deviations from primary source); `outcomes-new-relapse_sex_age` (the total of the outcomes by sex and age, for those with new or relapse status. Much lower than other counts, keep in mind when analyzing the outcomes by age and sex!); and `outcomes_diagnosis_tbhistory_sex`, slightly lower than primary source, mostly close)

There is only 1 slice with outcomes per year, except for 2020-2024 that has an additional slice of outcomes by age. However, data data appears to be very incomplete, so will not be added to the clean data.


