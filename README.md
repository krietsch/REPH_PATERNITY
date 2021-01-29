# **Data and R scripts from:**

### Extra-pair paternity in a sequentially polyandrous shorebird: limited evidence for the sperm-storage hypothesis

This repository contains the data sets and R scripts used for data analysis and figure production of this project and can also be found on the **Open Science Framework website** [doi: 10.17605/OSF.IO/V475T](https://osf.io/v475t/)

![REPH](./DATA/ILLUSTRATIONS/reph_pair.jpg)  
*Pair of red phalaropes. Illustration by Margherita Cragnolini*

<p>&nbsp;</p>

#### **Repository Contents**


**`DATA/`**:

All data used in this analysis (click on the black arrows to see column definitions):

<details>
  <summary> <b><code>CAPTURES</code></b> – a table of all captures data</summary>
  
  Columns are defined as:

  1.	`external`: source of the data, MPIO = 0 or USFWS = 1
  2.	`data_type`: data types (see methods)
  3.	`year_`: year
  4.	`ID`: metal band id
  5.	`UL`: upper left colour bands
  6.	`UR`: upper right colour bands
  7.	`LL`: lower left colour bands
  8.	`LR`: lower right colour bands
  9.	`sex`: field and genetic sex
  10. `lat`: capture location latitude (decimal degrees)
  11. `lon`: capture location longitude (decimal degrees)
  12. `caught_time`: date and time caught
  13. `dead`: bird was found dead = 1 or was caught alive = 0

</details>


<details>
  <summary> <b><code>NESTS</code></b> – a table of all nests data</summary>
  
  Columns are defined as:

  1.	`external`: source of the data, MPIO = 0 or USFWS = 1
  2.	`data_type`: data types (see methods)
  3.	`year_`: year
  4.	`nestID`: unique nest id
  5.	`male_id`: male metal band id
  6.	`female_id`: female metal band id
  7.	`male_assigned`: how the male was assigned: 0 = not, 1 = field, 2 = genetically, 3 = only GPStag
  8.	`female_assigned`: how the female was assigned: 0 = not, 1 = field, 2 = genetically, 3 = only GPStag
  9.	`found_datetime`: date and time the nest was found
  10. `clutch_size`: total clutch size
  11. `collected_datetime`: date and time the clutch was collected (if so)
  12. `initiation`: estimated date and time the first egg was laid
  13. `initiation_method`: method with which the initiation date was estimated
  14. `est_hatching_datetime`: estimated hatching date and time 
  15. `hatching_datetime`: method with which the hatching date was estimated
  16. `chicks_back`: date and time chicks were brought back to the nest
  17. `last_checked_datetime`: date and time the nest was checked for the last time
  18. `nest_state`: last nest state: I = Incubated (active  nest), P = Predated, D = Deserted, H = Hatched (received hatched chicks), 
                    U = Unknown, O = Observer (collected withour replacement)     
  19. `nest_state_date`: date and time the nest state was based on 
  20. `lat`: nest location latitude (decimal degrees)
  21. `lon`: nest location longitude (decimal degrees)
  22. `parentage`: logic if parentage was possible     
  23. `anyEPY`: logic if any extra-pair young were found in the clutch
  24. `N_parentage`: number of eggs with parentage data
  25. `N_EPY`: number of extra-pair young
  26. `female_clutch`: sequence of female clutches based on the initiation date (1st, 2nd or 3rd clutch) within season
  27. `N_female_clutch`: total number of clutches within season
  28. `polyandrous`: logic if the female was social polyandrous
  29. `polyandry_study_site`: logic if the female was social polyandrous with both clutches within the intensive study plot
  30. `male_clutch`: sequence of male clutches based on the initiation date (1st or 2nd clutch) within season   
  31. `N_male_clutch`: total number of clutches within season     
  32. `renesting_male`: logic if the male was renesting
  33. `renesting_study_site`: logic if the male was renesting with both clutches within the intensive study plot
</details>


<details>
  <summary> <b><code>OBSERVATIONS</code></b> – a table of all observations data</summary>
  
  Columns are defined as:

  1.	`year_`: year 
  2.	`datetime_`: date and time of the observation
  3.	`obs_id`: unique observation id
  4.	`ID1`: id of one bird
  5.	`ID2`: id of another bird (if interaction)
  6.	`ID1sex`: sex of ID1
  7.	`ID2sex`: sex of ID2
  8.	`ID1copAS`: ID1 was copulating = 1 or not = 0
  9.	`ID2copAS`: ID2 was copulating = 1 or not = 0
  10. `same_sex`: logic ID1 and ID2 have the same sex
  11. `ID1_1st_partner`: id of the first breeding partner of ID1
  12. `ID1_2nd_partner`: id of the second breeding partner of ID1
  13. `diff_obs_1st_initiation`: differences between the observation and the initiation of the first nest
  14. `diff_obs_2nd_initiation`: differences between the observation and the initiation of the second nest
  15. `seen_with_other_than_1st_partner`: logic observation with other than first partner
  16. `seen_with_other_than_2nd_partner`: logic observation with other than second partner
  17. `copAS_not_1st_partner`: logic copulation with other than first partner
  18. `copAS_not_2nd_partner`: logic copulation with other than second partner     

</details>


<details>
  <summary> <b><code>PATERNITY</code></b> – a table of all paternity data</summary>
  
  Columns are defined as:

  1.	`year_`: year
  2.	`nestID`: unique nest id
  3.	`IDchick`: unique chick id
  4.	`IDmother`: genetic mother id
  5.	`IDfather`: genetic father id
  6.	`EPY`: extra-pair young = 1, within-pair young = 0 or unknown = NA
  7.	`fate`: egg fate, h = hatched, f = frozen (unidentified fate), u = unhatched
  8.	`undeveloped`: egg where only the germinal disc was visible = 1 or developed egg = 0
  9.	`comment`: comment from the parentage analysis

</details>


<details>
  <summary> <b><code>Dale_et_al_1999_REPH_EPP</code></b> – a table with extra-pair paternity data extracted from [Dale et al. 1999, figure 1](https://doi.org/10.1007/s002650050591) </summary>
  
  Columns are defined as:

  1.	`year_`: year
  2.	`nestID`: unique nest number
  3.	`initiation_doy`: day of the year the first egg was laid
  4.	`anyEPY`: nest with extra-pair young = 1 or without = 0

</details>

<p>&nbsp;</p>

**`R/`**:

  - [`2_R_script_data_anaylsis.R`](https://github.com/krietsch/REPH_PATERNITY/blob/master/R/2_R_script_data_anaylsis.R). 
  The main script to reproduce all analysis and figures of this project. It contains a detailed commented workflow and 
  follows the order in the manuscript. Each main block of the script can be run independently. A compiled interactive 
  html output of this script can be found
  [here](https://github.com/krietsch/REPH_PATERNITY/blob/master/R/2_R_script_data_anaylsis.html) to download and 
  [here](https://github.com/krietsch/REPH_PATERNITY/blob/master/R/2_R_script_data_anaylsis.html) to view directly 
  in your browser.  
  
  - [`0_functions.R`](https://github.com/krietsch/REPH_PATERNITY/blob/master/R/0_functions.R) The workflow script 
  sources this script, which is only needed to create a base map using OpenStreetMap data stored in the R package 
  [`auksRuak`](https://github.com/krietsch/auksRuak). 
  
  - [`1_extract_data_from_database.R`](https://github.com/krietsch/REPH_PATERNITY/blob/master/R/1_extract_data_from_database.R) 
  Contains the script which with the data were extracted from our database. This script can only be run with access 
  to our database and is only stored to document the process. 
  
  
**`OUTPUTS/`**:
  
  
  
  
  

