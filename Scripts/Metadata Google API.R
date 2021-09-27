#Pull from google sheet using the API 

require(googlesheets4)
library(googlesheets4)

#will need to authorize token to access the main metadata sheets
mdata =read_sheet("1dUm-OtDOxvbS9OdCfnBrjBF9H5AKphauzzWxY-h4oUQ", sheet = "Sample metadata")

1

write.csv(mdata, "GLab_SC2_sequencing_data - Sample metadata.csv")
