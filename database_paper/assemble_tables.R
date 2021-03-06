## CCRCN Database Paper ####

## Create clean tabular versions of the hierarchical database

library(tidyverse)

# columns to include
# field name, description, class, units, required?
new_guidance <- read_csv("docs/ccrcn_database_structure_V2.csv") %>%
  select(-c(data_category, parent_data_category, quality_class, uncontrolled)) %>%
  mutate(format_unit_codes = ifelse(data_type != "numeric", NA, format_unit_codes))

# subset guidance into individual tables
sites <- new_guidance %>% filter(table == "sites") %>% select(-table)
cores <- new_guidance %>% filter(table == "cores") %>% select(-table)
depthseries <- new_guidance %>% filter(table == "depthseries") %>% select(-table)
species <- new_guidance %>% filter(table == "species") %>% select(-table)
impacts <- new_guidance %>% filter(table == "impacts") %>% select(-table)

# write tables
write_csv(sites, "database_paper/tables/site_attributes.csv")
write_csv(cores, "database_paper/tables/core_attributes.csv")
write_csv(depthseries, "database_paper/tables/depthseries_attributes.csv")
write_csv(species, "database_paper/tables/species_attributes.csv")
write_csv(impacts, "database_paper/tables/impact_attributes.csv")
# write_csv(citations, "database_paper/tables/citation_attributes.csv")