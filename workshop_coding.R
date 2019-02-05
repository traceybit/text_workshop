## Tracey Mangin
## February 5th, 2019
## Live coding 

library(tidyverse)
library(pdftools)

coral_narrs <- read_csv('data/iucn_narratives.csv')

coral_info <- read_csv('data/coral_spp_info.csv')

coral_habs_raw <- coral_narrs %>%
  left_join(coral_info, by = 'iucn_sid') %>%
  select(iucn_sid, sciname, habitat)

coral_habs_raw$habitat[1:2]

x <- "Everybody's got something to hide except for me and my monkey"
tools::toTitleCase(x)
tolower(x)

str_split(x, 'hide')
str_split(x, 't')

## use a semicolon to do multiple commands on one line
str_split(x, 'hide'); str_split(x, 't')

## Casey does this all the time
str_replace(x, 'except for', 'including')

str_replace_all(x, ' ', '_')

## Casey says this is super helpful
str_detect(x, 'z')

## look for any number, letter, or punctuation, wild card pattern...
## this can be super helpful
str_match_all(x, 't')

str_extract_all(x, 'y')
str_match_all(x, 'y')

str_locate_all(x, 'y')

##
coral_habs <- coral_habs_raw %>%
  mutate(hab_cut = str_split(habitat, '\\.')) %>%
  unnest(hab_cut) %>%
  # filter(str_detect(hab_cut, '2'))
  filter(str_detect(hab_cut, '[0-9] m')) %>% ## wildcard pattern followed by non-wild card
  # filter(str_detect(hab_cut, '[0-9]{4}]'))
  mutate(depth_char = str_extract_all(hab_cut, '[0-9]+ m')) %>%
  unnest(depth_char) %>%
  # mutate(depth_num = str_split(depth_char, '[^0-9]')) %>%
  mutate(depth_num = as.numeric(str_replace_all(depth_char, '[^0-9]', '')))
  

coral_habs$hab_cut[1]


crappy_colname <- 'Per-capita income(US$) (2015 dollars)'

crappy_colname %>%
  tolower() %>%
  str_replace_all('[^a-z0-9]+', '_')

list.files(path = 'sample_files', pattern = '\\.jpg')

pdf_smith <- pdf_text("pdfs/smith_wilen_2003.pdf")

smith_df <- data.frame(pdf_smith) %>%
  mutate(page = 1:n()) %>%
  mutate(text_sep = str_split(pdf_smith, "\\n")) %>%
  unnest(text_sep) %>%
  group_by(page) %>%
  muate(line = 1:n()) %>%
  ungroup()

