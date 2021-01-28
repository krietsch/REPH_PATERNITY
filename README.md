# **Data and R scripts for:**

### Extra-pair paternity in a sequentially polyandrous shorebird: limited evidence for the sperm-storage hypothesis

This repository contains the data sets and R scripts used for data analysis and figure production of this project and can also be found on the **Open Science Framework website** [doi: 10.17605/OSF.IO/V475T](https://osf.io/v475t/)

![REPH](./DATA/ILLUSTRATIONS/reph_pair.jpg)  
*Pair of red phalaropes. Illustration by Margherita Cragnolini*

<p>&nbsp;</p>

#### **Repository Contents**


**`DATA/`**:

We extracted all project relevant data from our database (see script `R/`) of the following five tables (click on 
the black arrows to see column definitions):

<details>
  <summary> <b><code>CAPTURES</code></b> – a table of all captures.</summary>
  
  Columns are defined as:

  1.	`external`: 
  2.	`data_type`: 
  3.	`year_`: 
  4.	`ID`:
  5.	`UL`: 
  6.	`UR`: 
  7.	`LL`: 
  8.	`LR`: 
  9.	`sex`:
  10. `lat`:
  11. `lon`:
  12. `caught_time`:  
  13. `dead`:  

</details>


<details>
  <summary> <b><code>NESTS</code></b> – a table of all nests.</summary>
  
  Columns are defined as:

  1.	`external`: 
  2.	`data_type`: 
  3.	`year_`: 
  4.	`nestID`:
  5.	`male_id`: 
  6.	`female_id`: 
  7.	`male_assigned`: 
  8.	`female_assigned`: 
  9.	`found_datetime`:
  10. `clutch_size`:
  11. `collected_datetime`:
  12. `initiation`:  
  13. `initiation_method`:  
  14. `est_hatching_datetime`:  
  15. `hatching_datetime`:  
  16. `chicks_back`:  
  17. `last_checked_datetime`:    
  18. `nest_state`:      
  19. `nest_state_date`:      
  20. `lat`:      
  21. `lon`:      
  22. `parentage`:      
  23. `anyEPY`:      
  24. `N_parentage`:    
  25. `N_EPY`:    
  26. `female_clutch`:    
  27. `N_female_clutch`:    
  28. `polyandrous`:    
  29. `polyandry_study_site`:    
  30. `male_clutch`:    
  31. `N_male_clutch`:      
  32. `renesting_male`:      
  33. `renesting_study_site`:  
</details>


<details>
  <summary> <b><code>OBSERVATIONS</code></b> – a table of all observations.</summary>
  
  Columns are defined as:

  1.	`year_`: 
  2.	`datetime_`: 
  3.	`obs_id`: 
  4.	`ID1`:
  5.	`ID2`: 
  6.	`ID1sex`: 
  7.	`ID2sex`: 
  8.	`ID1copAS`: 
  9.	`ID2copAS`:
  10. `same_sex`:
  11. `ID1_1st_partner`:
  12. `ID1_2nd_partner`:  
  13. `diff_obs_1st_initiation`:  
  14. `diff_obs_2nd_initiation`:  
  15. `seen_with_other_than_1st_partner`:  
  16. `seen_with_other_than_2nd_partner`:  
  17. `copAS_not_1st_partner`:    
  18. `copAS_not_2nd_partner`:      

</details>


<details>
  <summary> <b><code>PATERNITY</code></b> – a table of all paternity data.</summary>
  
  Columns are defined as:

  1.	`year_`: 
  2.	`nestID`: 
  3.	`IDchick`: 
  4.	`IDmother`:
  5.	`IDfather`: 
  6.	`EPY`: 
  7.	`undeveloped`: 
  8.	`comment`: 

</details>


<details>
  <summary> <b><code>Dale_et_al_1999_REPH_EPP</code></b> – a table with extra-pair paternity data extracted
  from [Dale et al. 1999 - figure 1](https://doi.org/10.1007/s002650050591) </summary>
  
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
  
  
  
  
  
  
  
  

