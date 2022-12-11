# Project: Ringtail Possums - Australian Wet Tropics - Mechanistic model

Development of biophysical models for rainforest ringtail possums.

<p align="center">
  <img src="https://github.com/AlejandroFuentePinero/Ringtail_WT_Mechanistic_Model/blob/main/pics/grtp.JPG" alt="Green Ringtail Possum."/>
</p>
Caption: Green Ringtail Possum (Pseudochirops archeri) at Mount Lewis National Park, Queensland, Australia. 
Credits: Supalak Siri.



## Folder - **Data**
This folder contains the data used for the models.

- File "Fur metadata.docx" documents in detail on how the fur data was collected (the part collected by Alejandro de la Fuente). Describes the methodology followed to measure different morphological properties of the possum fur.

- File "fur_dataset_clean.xlsx" contains the summary values of the green ringtail possum's fur properties.
  - "_dataset_" defines the overall dataset to which the value belongs.
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
  - "_Fur ID_" ID of the individual pelts. This value connects different datasets.
  - "_Sex_" M - Male; F - Female.
  - "_Sex code_" 1 - Male; 2 - Female.
  - "_Fur collection date_" empty.
  - "_RH%_" relative humidity percentage in the tunnel.
  - "_Target wind speed (m/s)_" target wind speed during the experiment.
  - "_Mean wind speed (m/s)_" actual wind speed measured in the tunnel.
  - "_Water temp (deg C)_"
  - "_Fur region_" back - dorsal; belly -  ventral; flank - side. This connects to the variable "side" in other datasets.
  - "_HFT (W)_" measured thermal flow.
  - "_Fur base temp (deg C)/Ts_" skin temperature.
  - "_Fur tip temp (deg C)_"
  - "_Air temp (deg C)/Ta_" measured 2 cm above the fur (information coming from Johan Larson).
  
 - File "fur_depth_grtp.xlsx" contains the depth measurements for several positions within the seven pelts of green ringtail possums. Measurements were taken by Alejandro de la Fuente.
   - "_fur_id_" ID of the individual pelts. This value connects different datasets.
   - "_side_" fur region.
   - "_position_" position within the fur region where the measurements were taken (see figure below).

- File "Copy of Ellipsoid model_heatstress3_green ringtails.xls" contains the biophysical model for green ringtail possums in Excel. Useful to explore the sensitivity of the analysis to a change in specific parameters.




<p align="center">
  <img src="https://github.com/AlejandroFuentePinero/Ringtail_WT_Mechanistic_Model/blob/main/pics/fur_depth_position.png" alt="Green Ringtail Possum's fur measurements."/>
</p>
Caption: Green Ringtail Possum (_Pseudochirops archeri_) fur. Circles indicate the position of the fur where the different measurements of fur depth were taken (see file "fur_depth_grtp.xlsx").
Credits: Alejandro de la Fuente.



### Folder - **data_input**
This folder contains the dataset in ".csv" format for R.

- File "hair_length_grtp.csv" contains the hair length measurements taken by Alejandro de la Fuente.
  - "_fur_id_" ID of the individual pelts. This value connects different datasets.
  - "_sample_id_" full ID of the fur, as defined in the collection bags.
  - "_side_" fur region.
  - "_hair_type_" indicates whether the hair was from the core part of the fur or guard.
  - "_length_mm_" hair length in mm.
  - "_length_m_ hair length in m.

- File "fur_insulation_clean.csv" is a duplicate of the file "fur_insulation_clean.xlsx" described above but in ".csv" format R.

- File "fur_depth_grtp.csv" is a summarised version of the file "fur_depth_grtp.xlsx" described above, containing only the main regions of the fur (dorsal, ventral, and side).

- File "fur_dataset.csv" is a duplicate of the file "fur_dataset_clean.xlsx" described above but in ".csv" format for R. 

- File "chamber_grtp.csv" contains the results from the chamber experiment for green ringtail possums led by Andrew Krockenberger; [Krokenberger et al. 2012. Oecologia](https://link.springer.com/article/10.1007/s00442-011-2146-2).

## Folder - **Scripts**
This folder contains the R scripts created for the different analyses, tests, and simulations.

- File "thermal_conductance.R" calculates the observed and predicted fur thermal conductance for green ringtail possums. The observed conductance is calculated using the data provided in the file "fur_insulation_clean.csv". It tests different assumptions about the influence of fur depth on the calculation, exploring the potential measurement error. The predicted conductance is simulated within NicheMapR, calling the IRPROP subroutine. We test the influence of ambient temperature, skin temperature, and fur tip temperature in estimating thermal conductance. The for loop created here allows testing different assumptions and the sensitivity of the estimations. A final comparison between observed and predicted is produced.

- File "Endotherm_component_tutorial.R" follows the original tutorial created by Michael Kearney but uses the fur properties of the green ringtail possum to assess the basic function of the different routines within the NicheMapR package.
  
## Folder - **Results**
This folder contains the main results from the analyses.

### Folder - **thermal_conductance_results**
This folder contains the results from the thermal conductance simulation (file "thermal_conductance.R"). The plots in this folder represent the main results using the different fur depth assumption tests (no error, 100% error, and constant depth across pelts).
