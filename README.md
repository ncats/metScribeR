# metScribeR
This package provides an automated workflow for processing in-house metabolite library standards data for use in untargeted metabolomics identification workflows. metScribeR focuses library building on MS1 & RT data, and MS2 data is not used or required for library construction. The process is implemented in a Shiny app, which can be launched using a function exported by this package.

## Install:

Install metScribeR as an R package and initialize the Shiny application with the following code:

```
install.packages('devtools')
devtools::install_github('ethanbass/chromatographR')

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("mzR")

devtools::install_github('ncats/metScribeR')

#library(metScribeR)

#runMetScribeRShinyApp()
```

## Workflow:

![image](https://github.com/user-attachments/assets/7acdb948-894d-4a53-b10d-02edb6d734d8)

## Example data:
Example data, including the required standards_df and adduct_df csv files and corresponding mzML files, is located under inst/extdata. The most convenient way to access these files is to download or install this repository from GitHub and copy the example data folder from its download or installation location. If metScribeR is installed, you can print the path to the example data directory with the following command: 

```
system.file('extdata', package = 'metScribeR')
```

The example_standards_df.csv file contains a pre-set, relative path to mzML files corresponding to each example standard. Therefore, please use setwd() to set R's working directory to the example data directory (containing the adduct_df.csv, standard_df.csv, and a folder of mzML files) before beginning the workflow with the example data. For example:

```
install.packages('devtools')
devtools::install_github('ethanbass/chromatographR')

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("mzR")

devtools::install_github('ncats/metScribeR')

dir.create('C:/Users/user123/Downloads/metScribeR_extdata_folder')

file.copy(from = system.file('extdata', package = 'metScribeR'), to = 'C:/Users/user123/Downloads/metScribeR_extdata_folder', recursive=T)

library(metScribeR)

setwd('C:/Users/user123/Downloads/metScribeR_extdata_folder')

runMetScribeRShinyApp()
```

## Instructions for use
1. In the wet lab, process authentic standards with LC-MS in positive and negative ESI modes and convert the results to .mzML format. MSConvert may be a helpful tool to convert various mass spectrometry data formats to .mzML.

2. Create standards_df.csv (or .tsv) with the following columns: common_name, with the name of each standard; monoisotopic_mass, the neutral mass of each standard; pos_mode_mzML_file_path, with the absolute or relative path to the pos mode mzML file corresponding to each standard; and neg_mode_mzML_file_path, with the absolute or relative path to the neg mode mzML file corresponding to each standard. Each row should be a unique standard with a single monoisotopic_mass. If you have multiple standards per mzML file, list each standard in a separate row and direct each row to the correct mzML file, repeating mzML file paths as necessary.

![image](https://github.com/user-attachments/assets/3d1d35ce-d4cb-44cb-99a0-5e3a5173f474)

3. Create adduct_df.csv (or .tsv) with the following columns: adduct, with the name of each adduct; change_from_neutral, with the difference between the adduct and its neutral mass; and mode, with either POS or NEG depending on the charge of the indicated ion.

<img width="206" alt="image" src="https://github.com/user-attachments/assets/522a2a50-8b44-4e78-90fe-4891ecedaf2e" />

4. Launch the metScribeR Shiny application in R with metScribeR::runMetScribeRShinyApp()

5. Upload the standards and adduct csv files to the appropriate input boxes in metScribeR and select an output directory for results. Use the noise plot figure on the right side of the screen to find and input the level of the background noise, below which all MS observations are not included in processing. Finally, choose an m/z and RT tolerance for creating EICs and distinguishing between adducts. These tolerances should be set based on the ability for the LC-MS equipment to confidently separate two signals. Then, click the submit button. Alternatively, in lieu of starting a new experiment, a saved metScribeR storage object can be loaded to resume progress on a previously started analysis.

![image](https://github.com/user-attachments/assets/4cb87a3c-9022-4b9a-9f7f-f964574f4aed)

6. Move to the "Find Peaks" tab. Use the drop-down menu to tab between a subset of 50 peaks found in your data, and toggle the density filter and data smoothing to assess the best settings for your data. In the figures shown, the blue vertical line is the starting boundary of the peak, and the red line is the ending boundary of the peak. Select filtering criteria for your data based on both your knowledge of the LC-MS method, the figures shown on this tab, and the boxplots on the Descriptive Boxplots tab. Click the submit button when you are satisfied with the result.

![image](https://github.com/user-attachments/assets/fcd6b7fc-064d-42d4-86eb-fb6dbb97103b)

![image](https://github.com/user-attachments/assets/7b1a7888-6b32-46d9-8522-981a1e1a9826)

7. Toggle to the Review Results tab. Here, each peak that passed filtering must be manually reviewed for inclusion in the final library. Toggle between peaks with the drop-down menu and arrow buttons. In each figure, the blue and red vertical lines indicate the beginning and ending boundaries of the peak. The dashed vertical line indicates the RT of the peak. Any solid black lines indicate the boundaries for incidental peaks that were found in the same EIC that are not currently being reviewed. Click Good Peak, Bad Peak, and Multimodal/Indeterminate to indicate your confidence in the quality of the peak for inclusion in the final library. The Export Library csv button on the View/Export Library tab can be used to save library progress at any time. 

![image](https://github.com/user-attachments/assets/0ea50641-7339-4e7e-8562-80f3a6494328)

8. Finally, move to the View/Export Library Tab. IMPORTANT: the library must be updated with the update library button for results to appear in the displayed table. If desired, use the check for crossed adducts and search MoNA for MS/MS data buttons to add internal identification probability and MS/MS data to the exported results. Note collection of MS/MS information is dependent on the MoNA server speed and can often be slow. Use the Export Library csv button to save progress and export .csv library results for use identifying experimental compounds.

![image](https://github.com/user-attachments/assets/9177b223-484f-40fc-a936-a160562accc3)

## Citation
The manuscript for metScribeR is currently in preparation.

## Contact


