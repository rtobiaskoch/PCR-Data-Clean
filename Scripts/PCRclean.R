#**********************************************************#
#HEADER CODE #####
#**********************************************************#
#CLEAN PCR RESULTS FROM CFX96 TO FIT FORMAT IN GLAB SEQUENCING DATA GOOGLE SHEETS

#before running this code copy and paste the samples you need from 
#the Gsheet into the PlateMap Template
#necessary packages

#install and load packages function    
packages = c("readxl", "tidyverse","googlesheets4")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(packages)

#**********************************************************#
#DATA INPUT ####
#**********************************************************#

#***GLAB METADATA***
#**********************************************************

#read in mdata from online and put in it your data input
#source("Scripts/Metadata Google API.R")
mdata = read.csv("data_input/GLab_SC2_sequencing_data - Sample metadata.csv")

#***PCR RESULTS DATA***
#*#**********************************************************#
#these are the unformatted results that are the output from the qPCR machine
pcr_data_file = paste("data_input/pcr_data/",
                      list.files("data_input/pcr_data")[1],
                      sep = "")
pcr_data = read.csv(pcr_data_file)

#***PLATEMAP DATA***
#*#**********************************************************#
#finds file by file pattern
platemap_file = paste("data_input/platemap/",
                      list.files("data_input/platemap")[1],
                      sep = "")

platemap = read_excel(platemap_file, sheet = "Paste Sheet")

platemap = platemap %>% 
  select(-'Filter')



#**********************************************************#
#DATA CLEAN ####
#**********************************************************#
#PCR DATA
#**********************************************************#
#removes nonsense and changes the col names to the appropriate name
pcr_data2 = pcr_data[-c(1:18),] #removes first 18 columns
colnames(pcr_data2) = pcr_data2[1,] #renames columns
pcr_data2 = pcr_data2[-1,] #removes column names


#reformat CFX results to be matched with the platemap sheet

pcr2 = pcr_data2 %>%
  as.tibble(pcr_data2) %>%
  arrange(Well) %>%
  unite(Target, Fluor, col = "Target", sep = "") %>% #merges columns for spread
  select(-c(Content, Sample,`Starting Quantity (SQ)`)) %>% #removes unnecessary columns
  spread(Target, Cq) %>% #spreads target from long to wide format
  left_join(platemap, by = "Well") %>% #merges with the platemap data
  filter(!is.na(`Yale-ID`)) %>% #removes control wells
  select(`Yale-ID`, 
         `Original ID`,
         Well,
         FAM,
         HEX,
         Cy5) %>% #arrange into column order to match metadata
  arrange(`Yale-ID`) %>% #arrange into row order to match metadata
  rename(Sample.ID = `Yale-ID`) %>% #rename to match metadata
  mutate_all(gsub, pattern = NaN,
                  replacement = "ND") #remove NaN to match ND naming scheme on metadata

#**********************************************************#
#*************************ID Check*************************#
#**********************************************************#

mdata2 = mdata %>% 
  select(Sample.ID,
         ) %>%
  semi_join(pcr2, by = "Sample.ID") %>%
  rename(Sample.ID_mdata = Sample.ID)

check = cbind.data.frame(mdata2, pcr2$Sample.ID)
check = check %>% 
  mutate(match = if_else(Sample.ID_mdata == `pcr2$Sample.ID`,
                                    1, #true
                                    0) #false
                          )

print(paste("there are",
            nrow(filter(check, match == 0)),
            "ID's that do not match between the mastersheet and the PCR Sheet"))

#removes data input
output_file = str_extract(pcr_data_file, "\\d{8}_Variant\\d{2}")

write_csv(check, paste("data_output/", "idcheck_", output_file))


write_csv(pcr2, paste("data_output/", "sorted_pcr", output_file))




