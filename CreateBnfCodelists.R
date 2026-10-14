
library(omopgenerics)
library(omock)
library(dplyr)
library(readr)
library(here)
library(fuzzyjoin)
library(tidyr)
library(stringr)

bnfFile <- "bnf_code_current_202609_version_90.csv"

# create drug vocabulary
# cdm <- mockCdmFromDataset(datasetName = "delphi-100k")
# drugVocabulary <- cdm$concept |>
#   filter(
#     (standard_concept == "S" & concept_class_id == "Ingredient") | # ingredients
#       (concept_class_id == "Precise Ingredient" & vocabulary_id == "RxNorm") # precise ingredients
#   ) |>
#   collect()
# write_csv(drugVocabulary, file = "drug_vocabulary.csv")

drugVocabulary <- read_csv("drug_vocabulary.csv", show_col_types = FALSE)

bnf <- read_csv(file = here("BNF", bnfFile), show_col_types = FALSE)

substances <- tibble(substance = tolower(unique(bnf$BNF_CHEMICAL_SUBSTANCE))) |>
  mutate(
    matched = FALSE,
    type = NA_character_,
    concept_name = NA_character_,
    concept_id = NA_character_
  )

ingredients <- drugVocabulary |>
  filter(standard_concept == "S", concept_class_id == "Ingredient") |>
  select("ingredient_id" = "concept_id", substance = "concept_name") |>
  mutate(substance = tolower(substance)) |>
  collect()

preciseIngredients <- drugVocabulary |>
  filter(concept_class_id == "Precise Ingredient", vocabulary_id == "RxNorm") |>
  select("precise_ingredient_id" = "concept_id", substance = "concept_name") |>
  mutate(substance = tolower(substance)) |>
  collect()

# exact matches
substances <- substances |>
  left_join(ingredients, by = "substance") |>
  mutate(
    ingredient = !is.na(ingredient_id) & !matched,
    matched = if_else(ingredient, TRUE, matched),
    type = if_else(ingredient, "ingredient", type),
    concept_name = if_else(ingredient, substance, concept_name),
    concept_id = if_else(ingredient, sprintf("%i", ingredient_id), concept_id),
  ) |>
  select("substance", "matched", "type", "concept_name", "concept_id")

# exact matches of RxNorm precise ingredient
substances <- substances |>
  left_join(preciseIngredients, by = "substance") |>
  mutate(
    ingredient = !is.na(precise_ingredient_id) & !matched,
    matched = if_else(ingredient, TRUE, matched),
    type = if_else(ingredient, "precise ingredient", type),
    concept_name = if_else(ingredient, substance, concept_name),
    concept_id = if_else(ingredient, sprintf("%i", precise_ingredient_id), concept_id),
  ) |>
  select("substance", "matched", "type", "concept_name", "concept_id")

# search combinations
combinations <- substances |>
  filter(!matched, grepl(" and | with ", substance)) |>
  select(substance) |>
  mutate(original = substance) |>
  separate_longer_delim(substance, delim = regex(" and | with ")) |>
  mutate(substance = str_squish(substance)) |>
  mutate(nsub = n(), .by = original) |>
  left_join(ingredients, by = "substance") |>
  left_join(preciseIngredients, by = "substance") |>
  mutate(concept_id = coalesce(ingredient_id, precise_ingredient_id)) |>
  filter(!is.na(concept_id)) |>
  filter(n() == nsub, .by = original) |>
  summarise(
    comb_name = paste(substance, collapse = "; "),
    comb_id = paste(sprintf("%i", concept_id), collapse = "; "),
    .by = original
  ) |>
  rename(substance = "original")
substances <- substances |>
  left_join(combinations, by = "substance") |>
  mutate(
    combination = !is.na(comb_id) & !matched,
    matched = if_else(combination, TRUE, matched),
    type = if_else(combination, "combination", type),
    concept_name = if_else(combination, comb_name, concept_name),
    concept_id = if_else(combination, comb_id, concept_id),
  ) |>
  select("substance", "matched", "type", "concept_name", "concept_id")

substances |>
  group_by(type) |>
  tally()
