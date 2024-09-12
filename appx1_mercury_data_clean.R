# Cleaning SWAMP Data Dashboard moisture and mercury Region 2 data and converting from wet weight to dry weight
# Created by Elena Suglia 8.28.24
# GitHub: @esuglia

# The purpose of this script is to clean two separate csvs containing moisture and mercury data (respectively) pulled from the SWAMP Data Dashboard for Region 2, merge them, and convert mercury concentration units from ug/g wet weight to ppm dry weight. The data will then be mapped using Esri ArcGIS Pro to visualize mercury concentrations across sites in the Bay Area. The map will then be used in Appendix 1 of the SWAMP Bioaccumulation Monitoring Program Realignment Monitoring and Analysis Workplan for the San Francisco Region.
# During data cleaning, we want to use only data that is from 2007 onward and under the SWAMP statewide program.

# Outline ----
# Describe the major sections

# TO DO ----
# Add a section at the bottom showing your work for the Data Dashboard pull
# Email Jennifer about data without moisture. <done> Decide what to do with these samples.
# Email Michelle about L1/L2 specifications. List out here what different result types exist in the df for both mercury & moisture. <done>
# Calculate dry weight
# Naming conventions for column headers? Check with Anna.
# Manually add L1/L2 info to duplicated row uniqueIDs.
# Clean up the script and change numbers of lines of code in issues section to match finalized code (or just copy & paste the lines themselves).
# Add cleaned data and scripts to GitHub repo.

# Libraries ----
library(tidyverse)
library(dplyr)

# Load the data and take a first look at it raw ----
mercury_raw = read.csv("mercury.csv")
moisture_raw = read.csv("moisture.csv")

# Look at stations represented in each dataset ----
merc_stations = unique(mercury_raw$StationName)
mois_stations= unique(moisture_raw$StationName)

# There are 53 stations with mercury data and 44 with moisture data
# Which stations are in the mercury station list that are not in moisture station list?
setdiff(merc_stations, mois_stations)

# "Lake Chabot", "Bon Tempe Lake", "Pilarcitos Lake", "Horseshoe Lake, Quarry Lakes", "Lago Los Osos", "Lake Cunningham", "Lake Elizabeth", "Lake Madigan", "Lafayette Reservoir"

# 8.28 - Anna is still unsure whether to keep the data that cannot be converted to dry weight (those without moisture data).
# For now, removed to simplify the dataframe and clean/check for other issues (added to code below cleaning the dataframes).

# Load and clean data ----
mercury = read.csv("mercury.csv") %>%
  # remove data prior to 2007
  filter(SampleYear >2006) %>%
  # remove non-statewide data
  filter(ParentProject != "SWAMP RWB2 Monitoring") %>%
  select(Program:TargetLongitude) %>%
  mutate(TargetLongitude = format(TargetLongitude, nsmall = 6)) %>%
  mutate(TargetLatitude = format(TargetLatitude, nsmall = 6)) %>%
  # create uniqueID column
  unite(col = "uniqueID", c("StationCode", "CommonName", 'SampleYear'), sep = "-", remove = F) %>%
  rename(mercury_unit = Unit) %>%
  rename(mercury_result = Result) %>%
  rename(mercury_tissue_name = TissueName) %>%
  rename(mercury_tissue_prep = TissuePrep) %>%
  rename(mercury_analyte = Analyte) %>%
  rename(mercury_result_type = ResultType) %>%
  # remove tissue prep that was not "skin off"
  filter(mercury_tissue_prep == "Skin off") %>%
  filter(StationName != 
           "Lake Chabot" &
           StationName != "Bon Tempe Lake" &
           StationName != "Pilarcitos Lake" &
           StationName != "Horseshoe Lake, Quarry Lakes" &
           StationName != "Lago Los Osos" &
           StationName != "Lake Cunningham" &
           StationName != "Lake Elizabeth" &
           StationName != "Lake Madigan" &
           StationName != "Lafayette Reservoir"
  )

moisture = read.csv("moisture.csv") %>%
  # remove data prior to 2007
  filter(SampleYear >2006) %>%
  # remove non-statewide data
  filter(ParentProject != "SWAMP RWB2 Monitoring") %>%
  select(Program:TargetLongitude) %>%
  mutate(TargetLongitude = format(TargetLongitude, nsmall = 6)) %>%
  mutate(TargetLatitude = format(TargetLatitude, nsmall = 6)) %>%
  # create uniqueID column
  unite(col = "uniqueID", c("StationCode", "CommonName", 'SampleYear'), sep = "-", remove = F) %>%
  rename(moisture_unit = Unit) %>%
  rename(moisture_result = Result) %>%
  rename(moisture_tissue_name = TissueName) %>%
  rename(moisture_tissue_prep = TissuePrep) %>%
  rename(moisture_analyte = Analyte) %>%
  rename(moisture_result_type = ResultType) %>%
  # remove tissue prep that was not "skin off"
  filter(moisture_tissue_prep == "Skin off") %>%
  filter(StationName != 
           "Lake Chabot" &
           StationName != "Bon Tempe Lake" &
           StationName != "Pilarcitos Lake" &
           StationName != "Horseshoe Lake, Quarry Lakes" &
           StationName != "Lago Los Osos" &
           StationName != "Lake Cunningham" &
           StationName != "Lake Elizabeth" &
           StationName != "Lake Madigan" &
           StationName != "Lafayette Reservoir"
  )

# Join the two dataframes ----
dat = full_join(mercury, moisture)

# Check for duplicates ----
sum(duplicated(dat$uniqueID) == TRUE) 
# 3 duplicates

# Issue samples:
#204TC0122-Largemouth Bass-2019 # two mercury and moisture results: L1 and L2
#204TC0122-Largemouth Bass-2007 # two mercury and moisture results: L1 and L2
#205PCL212-Black Crappie-2017 # two different mercury results: one for avg of individuals, one for avg of composites (moisture is the same for both and only calculated for avg of individuals)
# I think the solution for this is to add a suffix to the uniqueID that includes size class for mercury ("mercury_result_type") in the csv before loading so R recognizes them as different samples.

# TO DO: decide how to handle issue samples above, then revise the following numbers:
length(which(is.na(dat$moisture_result)))
# 60/136 samples (rows in the data including above duplicates) have no moisture data
# 8.29 - reaching out to Jennifer to see why there are so many samples missing moisture data <done>

# Jennifer's Pull from SWAMP Dataset ----
# Jennifer's email: See if the attached file is what you had in your pull? I had to be really creative because the pull wasn’t straightforward because the Datawarehouse doesn’t allow you to select Program. I did the pull based on Protocol Code, Stations beginning with “2” and filtering Mercury and Moisture.

# Load file
pull = read.csv("R2_Hg_Moist.csv") %>%
  # create a uniqueID column we can use to merge datasets and compare contents
  unite(col = "uniqueID", c("StationCode", "CommonName", 'DWC_Year'), sep = "-", remove = F) %>%
  mutate(TargetLongitude = format(TargetLongitude, nsmall = 6)) %>%
  mutate(TargetLatitude = format(TargetLatitude, nsmall = 6)) %>%
  # select specific columns we want to use to simplify looking at the dataset
  select(uniqueID, OrganismID, StationName, StationCode, ProjectName, TargetLongitude, TargetLatitude, DWC_Year, SampleTypeName, CompositeID, UnitName, AnalyteName, Result) %>%  
  # add a column with row numbers so R doesn't get confused by the fact that there are no unique rows while pivoting wider
  mutate(n = row_number()) %>%
  # make the dataset wider
  pivot_wider(id_cols = uniqueID:n, names_from = AnalyteName, values_from = Result) %>%
  # average values for each uniqueID
  group_by(uniqueID, StationName, StationCode, ProjectName, TargetLongitude, TargetLatitude, DWC_Year) %>%
  summarize(mercury_ugg_ww = mean(na.omit(Mercury)),
            moisture = mean(na.omit(Moisture)),
            # calculate dry weight using forumla: dry-weight = (wet-weight) / [1-(% moisture/100)]
            mercury_ppm_dw = mercury_ugg_ww/(1-(moisture/100))
  ) %>%
  # create a column indicating waterbody type (either Lake & Reservoir or Coastal) to use in map
  mutate(WaterbodyType = case_when(str_detect(StationName, "Lake") ~  "LakeReservoir",
                                   str_detect(StationName, "Lago") ~ "LakeReservoir",
                                   str_detect(StationName, "Reserv") ~ "LakeReservoir",
                                   str_detect(StationName, "Pond") ~ "LakeReservoir",
                                   str_detect(StationName, "Bay") ~ "Coastal",
                                   str_detect(StationName, "Island") ~ "Coastal",
                                   str_detect(StationName, "Coast") ~ "Coastal",
                                   str_detect(StationName, "Harbor") ~ "Coastal"))

# In pull, there are 83 columns, and many have different names for the same data type than in dat from the SWAMP Data Dashboard. For example, SampleYear in dat is DWC_Year in pull.

names(pull)
unique(pull$TissueName)
# There is no TissuePrep column
# A metadata file would be useful here

# It looks like each sample has a different OrganismID. I will assume that LabBatch has all the same mercury and moisture values across samples, and average them in order to reduce the number of samples to input into the map (one per location/date/species.)

# Once we have done all of the above, we end up with data formatted the same way as dat, and 219 samples as opposed to 136 in dat. Let's explore the differences in sample number.

# first, how many duplicates are there?
sum(duplicated(pull$uniqueID) == TRUE)
# 0

# Are there differences in stations? Which are in the pull dataset that are not in the dat dataset?
setdiff(unique(pull$StationName), unique(dat$StationName))

# [1] "San Mateo Coast"                                            
# [2] "Pilarcitos Lake"                                            
# [3] "Horseshoe Lake, Quarry Lakes "                              
# [4] "Shadow Cliffs Reservoir"                                    
# [5] "Lake Chabot"                                                
# [6] "Lago Los Osos"                                              
# [7] "Camden Percolation Pond across from Page Desilting Basin #2"
# [8] "Camden Percolation Pond below Los Gatos Creek Park #3"      
# [9] "Lake Cunningham"                                            
# [10] "Lake Elizabeth"                                             
# [11] "Lake Madigan"    

# Many are those we removed from the mercury dataset because there was no associated moisture data.

# What about uniqueIDs?
length(setdiff(unique(pull$uniqueID), unique(dat$uniqueID)))
# 86 are different, whereas the difference in the number of rows is 83 - but there are 3 duplicates in dat, so that makes sense. I think we just want to keep all the samples in the pull dataset and make the map from that data.

# Save a csv of the pull dataset
write.csv(pull, "mercury_ppm.csv")

# Troubleshooting issues with cleaning data ----
# These issues have been resolved, but listed here to document the cleaning process.
# 1. Tissue types ----
# After a chat with Anna, decided to keep "skin off" tissue type unless the different tissue type is from a site that is not represented elsewhere in the data.

# To examine this yourself, first comment out lines 43 & 73 which filter out tissue types that are not "skin off." Then merge the datasets and, in the viewing window, order the mercury_tissue_prep alphabetically and look at the samples that are not "skin off" to see which sites are represented in those samples. Then, do the same for mercury_tissue_prep.

# Sites from samples that don't have "skin off":
# Southern Marin Coast, Tomales Bay, Pillar Point Harbor, San Francisco Coast, Soulejoule Lake

# To run the following code, go back and un-comment lines 43 & 73.
# After filtering out other tissue types, let's check what sites remain:
# unique(dat$StationName)
# # All the above stations are still represented, so we are ok to remove all samples with different tissue types than "skin off" (lines 43 & 73 do this).
# 
# # 2. Issues with duplicates ----
# # To see this issue, comment out lines 32, 33, 62, & 63.
# 
# # How many duplicates are there?
# sum(duplicated(dat$uniqueID) == TRUE)
# 
# # Which rows are duplicated?
# duplicated(dat$uniqueID)
# 
# # Go into the viewing window and identify the samples that are duplicated based on row numbers for which duplicated == "TRUE". Then, copy and paste the corresponding uniqueID into the search bar and hit enter.
# # Some of the duplicates are the same samples we identified above that have both L1 and L2 samples, so let's ignore those.
# # For the others - let's just look at the first sample that is duplicated:
# # 20150FARI-Blue Rockfish-2009
# # When comparing the duplicate rows, it appears visually that they should have merged. Maybe there is an extra space added somewhere that is making R recognize values differently in a column that should have identical values (e.g. StationName)?
# 
# # Let's manually compare a sample that merged correctly with this one by looking at the csvs in excel. That can help us identify discrepancies like spaces.
# 
# # look at the sample that merged incorrectly (20150FARI-Blue Rockfish-2009):
# dat1 = dat[1,] # first row is the first instance of this sample
# dat137 = dat[137,] # 137th row is the second instance of the sample
# moisture_test = moisture[1,]
# mercury_test = mercury[1,]
# 
# # download the data to visualize in an excel sheet:
# write.csv(dat1, "dat1.csv")
# write.csv(dat137, "dat137.csv")
# write.csv(moisture_test, "moisture_test.csv")
# write.csv(mercury_test, "mercury_test.csv")
# 
# # now filter for a sample that merged correctly (here using 20151SMAC-Barred Surfperch-2009):
# moisture_correct = moisture[3,]
# mercury_correct = mercury[3,]
# dat_correct = dat[3,]
# 
# # download the data to visualize in an excel sheet:
# write.csv(moisture_correct, "moisture_correct.csv")
# write.csv(mercury_correct, "mercury_correct.csv")
# write.csv(dat_correct, "dat_correct.csv")
# 
# # I manually compared the two samples in this dataframe:
# test = read.csv("combined_test_csvs.csv")
# 
# # The samples still appear as if they should have merged. What if we try downloading the raw merged data and opening it up in excel:
# write.csv(dat, "dat.csv")

# Aha! Here, I was able to see that some duplicated rows are due to differences in the numbers of values after the decimal place in the latitude and/or longitude columns (you have to double click on a specific cell to see the total number of values). To remedy this, I added lines 32, 33, 62, and 63 limiting the number of decimal places in these rows to 6, which provides about 4 inches of precision and is plenty for our purposes of mapping the mercury concentration data from R2 (the Bay Area) in Esri ArcGIS Pro.

