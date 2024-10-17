# swamp-bioaccumulation-realignment-r2
Repo contains all scripts and data related to R2 Realignment products. Created in August 2024.

As of 10/17/24, one product was generated using R - a map in Appendix 1 of the 2025 Bioaccumulation Work Plan showing mercury concentrations in fish catch from the Bay Area (Region 2) since 2007.

Files:
appx1_mercury_data_clean.R - script cleaning data to prep for Appendix 1 map displaying mercury concentrations in the Bay Area
mercury_ppm - cleaned data file containing mercury and moisture data ready to be mapped in Esri ArcGIS Pro
R2_Hg_Moist - raw data file containing mercury and moisture data generated from Jennifer Salsbury pulled from the SWAMP Dataset
STEW_realignment_appendix1_9.7_transparent - pdf image of Appendix 1 map created in ArcGIS Pro displaying mercury concentrations in the Bay Area since 2007. Where points overlap, higher values are mapped on top and points are transparent to make overlapping areas visible.

Folders:
"files used for dealing with duplicates" includes csv's generated during the data cleaning process from a dataset pulled by Elena Suglia from the SWAMP Data Dashboard. These files were not used to create the final map, but are included here to document the process.

"data dashboard files" includes raw mercury and moisture csv's pulled from the SWAMP Data Dashboard.
