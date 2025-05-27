# metScribeR
This package provides an automated workflow for processing in-house metabolite standards library data for use in untargeted metabolomics identification workflows. metScribeR focuses library building on MS1 & RT data, and MS2 data is not used or required for library construction. The process is implemented in a Shiny app, which can be launched using a function exported by this package.

## Install:
Install as R package with devtools::install_github('adtisch/metScribeR').

## Workflow:
![image](https://github.com/user-attachments/assets/a5354575-1465-45b8-84b1-a14f8e661484)


## Instructions for use
1. Process authentic standards with LC-MS in positive and negative ESI modes and convert the results to .mzML format. MSConvert may be a helpful tool to convert various mass spectrometry data formats to .mzML.

2. Create standards_df.csv (or .tsv) with the following columns: common_name, with the name of each standard; monoisotopic_mass, the neutral mass of each standard; pos_mode_mzML_file_path, with the absolute path the the pos mode mzML file corresponding to each standard; neg_mode_mzML_file_path. Each row should be a unique standard with a single monoisotopic_mass. If you have multiple standards per mzML file, list each standard in a seperate row and direct each row to the correct mzML file, repeated as necessary.
<img width="626" alt="image" src="https://github.com/user-attachments/assets/5442a5c7-6d9e-4697-9e17-4777cd640090" />

3. Create adduct_df.csv (or .tsv) with the following columns: adduct, with the name of each adduct; change_from_neutral, with the difference between the adduct and its neutral mass; and mode, with either POS or NEG depending on the charge of the indicated ion.
<img width="206" alt="image" src="https://github.com/user-attachments/assets/522a2a50-8b44-4e78-90fe-4891ecedaf2e" />

4. Launch the metScribeR shiny application in R with metScribeR::runMetScribeRShinyApp()

5. Upload the standards and adduct csv files to the appropriate input boxes in metScribeR and select an output directory for results. Use the noise plot figure on the right side of the screen to find and input the level of the background noise, below which all MS observations are not included in processing. Finally, choose an m/z and RT tolerance for creating EICs and distinguishing between adducts. These tolerances should be set based on the ability for the LC-MS equiptment to confidently seperate two signals. Then, click the submit button. Alternatively, in lieu of starting a new experiment, a saved metScribeR storage object can be loaded to resume progress on a previously started analysis.
<img width="526" alt="image" src="https://github.com/user-attachments/assets/4c9189c1-f5b3-436c-9048-67cf7c93b9d4" />

6. Move to the "Find Features" tab. Use the drop-down menu to tab between 50 of the total number of peaks found in your data, and toggle the density filter and data smoothing to assess the best settings for your data. In the figures shown, the blue vertical line is the starting boundary of the peak, and the red line is the ending boundary of the peak. Select filtering criteria for your data based on both your knowledge of the LC-MS method, the figures shown on this tab, and the boxplots on the Descriptive Boxplots tab. Click the submit button when you are satisfied with the result.
<img width="523" alt="image" src="https://github.com/user-attachments/assets/7e576f2e-5a39-45c9-a797-0aab37d5a63b" />
<img width="513" alt="image" src="https://github.com/user-attachments/assets/2e6f9193-8d92-4a89-95e5-3d76deda0b5f" />

7. Toggle to the Review Results tab. Here, each peak that passed filtering must be manually reviewed for inclusion in the final library. Toggle between peaks with the drop-down menu and arrow buttons. In each figure, the blue and red vertical lines indicate the beginning and ending boundaries of the peak. The dashed vertical line indicates the RT of the peak. Any solid black lines indicate the boundaries for incidental peaks that were found in the same EIC that are not currently being reviewed. Click Good Peak, Bad Peak, and Multimodal/Indeterminate to indicate your confidence in the quality of the peak for inclusion in the final library. The Export Library csv button on the View/Export Library tab can be used to save library progress at any time. 
<img width="515" alt="image" src="https://github.com/user-attachments/assets/a6f454a8-69ff-4c68-b9af-b90326ca69e9" />

8. Finally, move to the View/Export Library Tab. IMPORTANT: the library must be updated with the update library button for results to appear in the displayed table. If desired, use the check for crossed adducts an search MoNA for MS/MS data buttons to add internal identification probability and MS/MS data to the exported results. Note collection of MS/MS information is dependent on the MoNA server speed and can often be slow. Use the Export Library csv button to save progress and export .csv library results for use identifying experimental compounds.
<img width="950" alt="image" src="https://github.com/user-attachments/assets/58bb566c-7d14-4f0c-bf7d-920a89fc1461" />

