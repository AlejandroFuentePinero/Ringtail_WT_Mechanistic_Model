# Project: Ringtail Possums - Australian Wet Tropics - Mechanistic model

Development of biophysical models for rainforest ringtail possums.

![Green Ringtail Possum at Mount Lewis, Queensland, Australia](https://github.com/AlejandroFuentePinero/Ringtail_WT_Mechanistic_Model/blob/main/pics/grtp.JPG)
Caption: Green Ringtail Possum (_Pseudochirops archeri_) at Mount Lewis National Park, Queensland, Australia. 
Credits: Supalak Siri.

## Folder - Data
This folder contains the data used for the models.

- File "fur_dataset_clean.xlsx" contains the summary values of green ringtail possum's fur properties.
  - "_dataset_" defines the overall dataset to which the value belongs to.
  - "_var_" provides a generic name for the variable.
  - "_NicheMapR-varname_" describe the variable name used in NicheMapR.
  - "_side_" defines the side of the animal where the measurement was taken.
  - "_type_" describe the specific data value information when measurements were taken on different parts of the fur or species.
  - "_mean_"
  - "_sd_" is the standard deviation around the mean.
  - "_weight_imp_" values used to calculate the weighted average/sd when warranted.
  - "_n_samples_" number of samples used to calculate the mean.
  - "_units_" units of the measurements.
  - "_original_file_" ID of the original file used to calculate the values in this table.
  - "_note_" extra information not provided with the other columns.
  
- File "fur_insulation_clean.xlsx" contains the physical properties of green ringtail possum's fur obtained from a wind tunnel experiment. The wind tunnel study was led by Andrew Krockenberger.
  - "_Date & Time_" data and time when the measurement was taken.
  - "_time_" time series of the measurements in seconds.
  - "_Fur ID_" ID of the individual pelts. This value connect different datasets.
  - "_Sex_" M - Male; F - Female.
  - "_Sex code_" 1 - Male; 2 - Female.
  - "_Fur collection date_" empty.
  - "_RH%_" relative humidity percentage in the tunnel.
  - "_Target wind speed (m/s)_" target wind speed during the experiment.
  - "_Mean wind speed (m/s)_" actual wind speed measured in the tunnel.
  - "_Water temp (deg C)_"
  - "_Fur region_" back - dorsal; belly -  ventral; flank - side. This connect to the variable "side" in other datasets.
  - "_HFT (W)_" measured thermal flow.
  - "_Fur base temp (deg C)/Ts_" skin temperature.
  - "_Fur tip temp (deg C)_"
  - "_Air temp (deg C)/Ta_" measured 2 cm above the fur (information coming from Johan Larson).
  
 - File "fur_depth_grty.xlsx" contains the depth measurements for several positions within the 7 pelts of green ringtail possums. Measurements taken by @AlejandroFuentePinero (Alejandro de la Fuente).
  

