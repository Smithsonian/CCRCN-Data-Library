## CCRCN Data Library Map application
## contact: Jaxine Wolfe, wolfejax@si.edu

## This script creates the lookup tabe for species habitat classification

library(tidyverse)

synthesis_species <- read_csv("data/CCRCN_synthesis/CCRCN_species.csv")
synthesis_cores <- read_csv("data/CCRCN_synthesis/CCRCN_cores.csv")

# 1. Classify Habitat based on the Synthesis Species ####

# Define the genus/species for each habitat classification
mangrove <- c("Avicennia", "Rhizophora", "Bruguiera", "Bruguierra", "Ceriops", "Lumnitzera", "Laguncularia",
              "Kandelia", "Kandela", "Acanthus", "Aegiceras", "Sonneratia", "Xylocarpus", "Conocarpus", 
              "Heritiera", "Pelliciera", "Peliciera", "Rhisophora", "Rhizphora")
seagrass <- c("Thalassia", "Syringodium", "Zostera", "Halophila", "Cymodocea", "Halodule",
              "Amphibolis", "Enhalus", "Nanozostera", "Posidonia", "Ruppia", "Thalassodendron",
              "Heterozostera", "Thassia")
salt_marsh <- c("Spartina", "Distichlis", "Juncus", "Phragmites", "Schoenoplectus",
                "Sagittaria", "Scirpus", "Eleocharis", "Batis", "Salicornia", "Baccharis halimifolia",
                "Zizania", "Bolboschoenus", "Typha", "Zizaniopsis", "Triglochin", "Triadenum", "Symphyotrichum",
                "Sporobolus", "Solidago", "Sarcocornia", "Rumex", "Potentilla", "Polygonum", "Poaceae",
                "Achillea", "Acrostichium", "Agrostis", "Alternanthera", "Amaranthus", "Ambrosia", "Ampelopsis",
                "Andropogon", "Angelica", "Aristida", "Arrow", "Asppagus", "Aster", "Athyrium", "Bidens",
                "Boehmeria", "Borrichia", "Bromus", "Brunnichia", "Carex")
tidal_swamp <- c("Taxodium", "Alnus", "Salix", "Quercus", "Nyssa", "Acer", "Cornus", "Cyrilla",
                 "Dalbergia", "Forestiera", "Fraxinus", "Ilex", "Liquidambar", "Lysichiton",
                 "Maianthemum", "Malus", "Morella", "Persea", "Photinia", "Picea", "Pinus", "Prunus",
                 "Rhamnus", "Thuja")
other <- c("unvegetated", "mixed", "Mix", "Wrack", "Unidentified", "Algal Mat", "Litter", "Swamp",
           "submerged aquatic vegetation", "Surface Algae")


# isolate unique species in the synthesis
assign_habitat <- synthesis_species %>%
  select(species_code) %>%
  distinct() %>%
  drop_na(species_code) %>%
  arrange(species_code) %>%
  mutate(habitat = case_when(str_detect(species_code, str_c(other, collapse = "|")) ~ "other",
                             # mangrove
                             str_detect(species_code, str_c(mangrove, collapse = "|")) ~ "mangrove",
                             # tidal swamp
                             str_detect(species_code, str_c(tidal_swamp, collapse = "|")) ~ "tidal_swamp",
                             # seagrass
                             str_detect(species_code, str_c(seagrass, collapse = "|")) ~ "seagrass",
                             # salt marsh
                             str_detect(species_code, str_c(salt_marsh, collapse = "|")) ~ "salt_marsh",
                             # assume that everything else is salt_marsh
                             TRUE ~ "salt_marsh")
  ) %>%
  arrange(habitat)

# misspellings: Rhisophora, Rhizphora, Bruguierra, Kandelia obvata, Thassia,etc.

# write lookup table
# write_csv(assign_habitat, "data/synthesis_resources/species-habitats.csv")

# a curation function will be written to join the lookup table to the core table
# if there are no species listed for a study, the habitat will be classified given the context of the study
# this function will recognize when there is a new species with no assigned habitat

## 2. Add Habitat Classification to Core Table ####

# merge the habitat classifications to the species table
habitat_species <- full_join(synthesis_species, assign_habitat)

# lose the species column to find the unique habitat classification for study-site-core id
habitat_unique <- habitat_species %>%
  select(-species_code) %>% # comment out to investigate which species are causing duplication
  filter(habitat != "other") %>%
  drop_na(habitat) %>%
  distinct()
# if there is >1 habitat classification per study-site-core groups boil
# it will cause duplication in the core table

# merge the to the cores table
habitat_cores <- left_join(synthesis_cores, habitat_unique)
