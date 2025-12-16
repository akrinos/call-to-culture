pacman::p_load(dplyr,ggplot2,ggridges,stringr,ggupset)

wos_results_all = data.frame()
data_file = read.csv("~/Documents/Postdoc/C-CoMP Postdoc/call-to-culture/data/c77ddf63e2b72bfa_864816_1487847_2025-11-05_02-53-06/articles.csv") %>%
  dplyr::mutate(aquatic_system = case_when(grepl("ocean",tolower(abstract))~"marine",
                                           grepl("gulf",tolower(abstract))~"marine",
                                           grepl("fjord",tolower(abstract))~"marine",
                                           grepl("coastal",tolower(abstract))~"marine",
                                           grepl("bay",tolower(abstract))~"marine",
                                           grepl("sea",tolower(abstract))~"marine",
                                           grepl("marine",tolower(abstract))~"marine",
                                           grepl("estuar",tolower(abstract))~"estuarine",
                                           grepl("wetlands",tolower(abstract))~"estuarine",
                                           grepl("lake",tolower(abstract))~"lake",
                                           grepl("lakes",tolower(abstract))~"lake",
                                           grepl("pond",tolower(abstract))~"pond",
                                           grepl("river",tolower(abstract))~"river",
                                           grepl("stream",tolower(abstract))~"stream",
                                           grepl("freshwater",tolower(abstract))~"other freshwater",
                                           TRUE~"other"))

data_file = data_file %>%
  dplyr::mutate(aquatic_system_broad = case_when(grepl("ocean",tolower(abstract))~"marine",
                                           grepl("gulf",tolower(abstract))~"marine",
                                           grepl("fjord",tolower(abstract))~"marine",
                                           grepl("coastal",tolower(abstract))~"marine",
                                           grepl("bay",tolower(abstract))~"marine",
                                           grepl("sea",tolower(abstract))~"marine",
                                           grepl("marine",tolower(abstract))~"marine",
                                           grepl("estuar",tolower(abstract))~"estuarine",
                                           grepl("wetlands",tolower(abstract))~"estuarine",
                                           grepl("lake",tolower(abstract))~"freshwater",
                                           grepl("lakes",tolower(abstract))~"freshwater",
                                           grepl("pond",tolower(abstract))~"freshwater",
                                           grepl("river",tolower(abstract))~"freshwater",
                                           grepl("stream",tolower(abstract))~"freshwater",
                                           grepl("freshwater",tolower(abstract))~"freshwater",
                                           TRUE~"other"))

data_file = data_file %>%
  dplyr::mutate(mentions_culture = case_when(grepl("culture",tolower(abstract))|
                                             grepl("isolate",tolower(abstract))~"mentions_culture",
                                             TRUE~"no"),
                mentions_database = case_when(grepl("database",tolower(abstract))|
                                              grepl("reference",tolower(abstract))|
                                              grepl("culture collection",tolower(abstract))~"mentions_database",
                                              TRUE~"no"),
                mentions_collection = case_when(grepl("collected",tolower(abstract))|
                                                grepl("isolated",tolower(abstract))~"mentions_specimen_collection",
                                                TRUE~"no"),
                mentions_sequencing = case_when(grepl("sequenc",tolower(abstract))|
                                                grepl("16S",tolower(abstract))|
                                                grepl("long-read",tolower(abstract))|
                                                grepl("illumina",tolower(abstract))|
                                                grepl("18S",tolower(abstract))|
                                                grepl("amplicon",tolower(abstract))|
                                                grepl("long read",tolower(abstract))~"mentions_sequencing",
                                                TRUE~"no"),
                sequence_type = case_when(grepl("18S",tolower(abstract))|
                                          grepl("16S",tolower(abstract))|
                                          grepl("ITS",tolower(abstract))|
                                          grepl("amplicon",tolower(abstract))|
                                          grepl("barcoding",tolower(abstract))|
                                          grepl("metabarcod",tolower(abstract))~"amplicon",
                                          grepl("RNA",tolower(abstract))|
                                          grepl("transcript",tolower(abstract))~"RNA",
                                          grepl("DNA",tolower(abstract))|
                                          grepl("genom",tolower(abstract))~"DNA"),
                broad_microbe_class = case_when(grepl("diatom",tolower(abstract))|
                                                grepl("dinoflagellate",tolower(abstract))|
                                                grepl("haptophyte",tolower(abstract))|
                                                grepl("microalg",tolower(abstract))|
                                                grepl("alga",tolower(abstract))|
                                                grepl("pelagophyte",tolower(abstract))~"euk_algae",
                                                grepl("chytrid",tolower(abstract))|
                                                grepl("fung",tolower(abstract))|
                                                grepl("oomycet",tolower(abstract))|
                                                grepl("cryptosporidium",tolower(abstract))~"fungi",
                                                grepl("ciliate",tolower(abstract))|
                                                grepl("apicomplex",tolower(abstract))|
                                                grepl("protist",tolower(abstract))|
                                                grepl("microbial eukary",tolower(abstract))~"other_protist",
                                                grepl("bacteri",tolower(abstract))|
                                                grepl("archae",tolower(abstract))~"other_prokaryote",
                                                grepl("virus",tolower(abstract))|grepl("viral",tolower(abstract))~"virus",
                                                grepl("copepo",tolower(abstract))|
                                                grepl("zoopl",tolower(abstract))~"zooplankton",
                                                grepl("cyanobac",tolower(abstract))|
                                                grepl("synechocc",tolower(abstract))|
                                                grepl("prochloroco",tolower(abstract))~"cyanobacteria",
                                          grepl("plankton",tolower(abstract))~"other_generic_plankton",
                                          TRUE~"None_detected"),
                microbe_class = case_when(grepl("diatom",tolower(abstract))~"diatom",
                                          grepl("ciliate",tolower(abstract))~"ciliate",
                                          grepl("dinoflagellate",tolower(abstract))~"dinoflagellate",
                                          grepl("haptophyte",tolower(abstract))~"haptophyte",
                                          grepl("pelagophyte",tolower(abstract))~"pelagophyte",
                                          grepl("chytrid",tolower(abstract))|grepl("fungi",tolower(abstract))|
                                          grepl("fungus",tolower(abstract))~"fungi",
                                          grepl("cyanobac",tolower(abstract))|grepl("synechocc",tolower(abstract))|grepl("prochloroco",tolower(abstract))~"cyanobacteria",
                                          grepl("oomycet",tolower(abstract))~"parasite",
                                          grepl("cryptosporidium",tolower(abstract))|grepl("apicomplex",tolower(abstract))~"apicomplexan",
                                          grepl("microalg",tolower(abstract))|grepl("alga",tolower(abstract))~"algae",
                                          grepl("bacteri",tolower(abstract))|grepl("vibrio",tolower(abstract))|grepl("acidimicrobiia",tolower(abstract))|
                                          grepl("streptomyces",tolower(abstract))|grepl("caldicellulosiruptor",tolower(abstract))|
                                          grepl("chloroflexi",tolower(abstract))|grepl("Nocardiopsis",tolower(abstract))|
                                          grepl("burkholderia",tolower(abstract))~"bacteria",
                                          grepl("microbial eukary",tolower(abstract))|grepl("protist",tolower(abstract))~"other_microbial_euk",
                                          grepl("archae",tolower(abstract))~"archaea",
                                          grepl("virus",tolower(abstract))|grepl("viral",tolower(abstract))~"virus",
                                          grepl("zooplan",tolower(abstract))|grepl("copepod",tolower(abstract))~"zooplankton",
                                          grepl("plankton",tolower(abstract))~"other_generic_plankton",
                                          TRUE~"None_detected"))

code_characterized="~/Documents/Postdoc/C-CoMP Postdoc/call-to-culture/data/characterized_by_code.csv"
write.csv(data_file,code_characterized)
tallied_groups = data_file%>% 
  dplyr::group_by(mentions_collection,mentions_culture,mentions_database,mentions_sequencing,
                  aquatic_system) %>%
  dplyr::tally()
tallied_groups
ggplot(data_file %>% 
  dplyr::select((starts_with("key")&!starts_with("keyw"))|starts_with("mentions")|
                  starts_with("broad_microbe_class") | starts_with("aquatic_system")) %>%
  tidyr::pivot_longer(cols=starts_with("mentions"),
                      names_to="category",values_to="result") %>%
  dplyr::filter((result!="no")&!is.na(result)) %>%
  dplyr::group_by(key,aquatic_system_broad,broad_microbe_class) %>%
  dplyr::summarize(result=list(result)),aes(x=result)) + 
  geom_bar() +
  scale_x_upset(n_intersections = 20) + facet_wrap(~aquatic_system_broad)
ggplot(data_file)