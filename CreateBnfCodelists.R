
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
    match_method = NA_character_,
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

# explicit UK/US and international spelling differences
englishSpellingRules <- c(
  "\\baluminium\\b" = "aluminum",
  "sulph" = "sulf",
  "\\bsimeticone\\b" = "simethicone",
  "\\bdimeticone\\b" = "dimethicone",
  "\\bbeclometasone\\b" = "beclomethasone",
  "\\bchlortalidone\\b" = "chlorthalidone",
  "\\btorasemide\\b" = "torsemide",
  "\\bvalaciclovir\\b" = "valacyclovir",
  "\\baciclovir\\b" = "acyclovir",
  "\\bcolecalciferol\\b" = "cholecalciferol",
  "\\boestradiol\\b" = "estradiol",
  "\\boestriol\\b" = "estriol",
  "\\boestrogen" = "estrogen",
  "\\bmesilate\\b" = "mesylate",
  "\\btosilate\\b" = "tosylate",
  "\\bbesilate\\b" = "besylate",
  "\\bisetionate\\b" = "isethionate",
  "\\bgonadotrophin\\b" = "gonadotropin",
  "\\bindometacin\\b" = "indomethacin",
  "\\bcarbocisteine\\b" = "carbocysteine",
  "\\bethinylestradiol\\b" = "ethinyl estradiol",
  "\\btioguanine\\b" = "thioguanine",
  "\\bcefalexin\\b" = "cephalexin",
  "\\bcefradine\\b" = "cephradine"
)

# reviewed ingredient synonyms, naming variants, and clear source typos
ingredientNameRules <- c(
  "\\bparacetamol\\b" = "acetaminophen",
  "\\badrenaline\\b" = "epinephrine",
  "\\bnoradrenaline\\b" = "norepinephrine",
  "\\bglyceryl trinitrate\\b" = "nitroglycerin",
  "\\bsalbutamol\\b" = "albuterol",
  "\\blignocaine\\b" = "lidocaine",
  "\\brifampicin\\b" = "rifampin",
  "\\bmesalazine\\b" = "mesalamine",
  "\\bdicycloverine\\b" = "dicyclomine",
  "\\bciclosporin\\b" = "cyclosporine",
  "\\bglycerol\\b" = "glycerin",
  "\\bestrogens conjugated\\b" = "estrogens, conjugated (usp)",
  "\\bamphoteracin\\b" = "amphotericin",
  "\\bandrostanalone\\b" = "androstanolone",
  "\\bbetacarotene\\b" = "beta carotene",
  "\\bc1-esterase inhibitor\\b" = "c1 esterase inhibitor",
  "\\bcarbomer 974p\\b" = "carbomer-974p",
  "\\bclomethiazole\\b" = "chlormethiazole",
  "\\bcloral betaine\\b" = "chloral betaine",
  "\\bclotiapine\\b" = "clothiapine",
  "\\betamsylate\\b" = "ethamsylate",
  "\\bfampridine\\b" = "fampiridine",
  "\\bflubiprofen\\b" = "flurbiprofen",
  "\\bfusafungine\\b" = "fusafungin",
  "\\bhaem arginate\\b" = "heme arginate",
  "\\bheparinoid\\b" = "heparinoids",
  "\\bhepatitis b immunoglobulin\\b" = "hepatitis b immune globulin",
  "\\binterferon beta\\b" = "interferon-beta",
  "\\bliquorice\\b" = "licorice",
  "\\blusutrombop\\b" = "lusutrombopag",
  "\\bmenotrophin\\b" = "menotropins",
  "\\bmesuximide\\b" = "methsuximide",
  "\\bmethyl nicotinate\\b" = "methylnicotinate",
  "\\bmethyl-5-aminolevulinate\\b" = "methyl 5-aminolevulinate",
  "\\bnonoxinol 9\\b" = "nonoxynol-9",
  "\\bnoxytiolin\\b" = "noxythiolin",
  "\\bpadimate o\\b" = "padimate-o",
  "\\bpericyazine\\b" = "periciazine",
  "\\bpolysaccharide-iron complex\\b" = "polysaccharide iron complex",
  "\\bpovidone iodine\\b" = "povidone-iodine",
  "\\bpovidone k 25\\b" = "povidone k25",
  "\\bsitaxentan sodium\\b" = "sitaxsentan sodium",
  "\\bsultiame\\b" = "sulthiame",
  "\\btetanus immunoglobulin\\b" = "tetanus immune globulin",
  "\\btiabendazole\\b" = "thiabendazole",
  "\\btocofersolan\\b" = "tocophersolan",
  "\\bvaricella-zoster immunoglobulin\\b" =
    "varicella-zoster immune globulin"
)

normaliseEnglishSpelling <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all(c(englishSpellingRules, ingredientNameRules)) |>
    str_squish()
}

plainTibble <- function(x) {
  class(x) <- c("tbl_df", "tbl", "data.frame")
  attributes(x) <- attributes(x)[c("names", "row.names", "class")]
  x
}

# reviewed therapeutic classes represented by ATC classification concepts
ingredientClasses <- tribble(
  ~spelling, ~spelling_concept_id, ~spelling_concept_name,
  "diuretic", 21601461L, "DIURETICS",
  "diuretics", 21601461L, "DIURETICS",
  "thiazide", 21601462L, "LOW-CEILING DIURETICS, THIAZIDES",
  "thiazides", 21601462L, "LOW-CEILING DIURETICS, THIAZIDES",
  "loop diuretic", 21601516L, "HIGH-CEILING DIURETICS",
  "loop diuretics", 21601516L, "HIGH-CEILING DIURETICS",
  "calcium channel blocker", 21601744L, "CALCIUM CHANNEL BLOCKERS",
  "calcium channel blockers", 21601744L, "CALCIUM CHANNEL BLOCKERS",
  "progestogen", 21602537L, "PROGESTOGENS",
  "progestogens", 21602537L, "PROGESTOGENS"
) |>
  mutate(mapping_type = "ingredient class")

safeIngredientQualifiers <- c(
  "antileprotic",
  "antimetabolites",
  "antivirals",
  "barrier",
  "emergency contraceptive",
  "erectile dysfunction",
  "eye",
  "eye anti inflammatory",
  "female sex hormone",
  "gynaecology",
  "immunomodulating",
  "inhalation",
  "nasal",
  "neuropathic pain",
  "old",
  "parasiticidal",
  "rectal",
  "rheumatic",
  "sunscreen",
  "systemic",
  "topical",
  "vasodilator antihypertensive"
)

# exact matches
substances <- substances |>
  left_join(ingredients, by = "substance") |>
  mutate(
    ingredient = !is.na(ingredient_id) & !matched,
    matched = if_else(ingredient, TRUE, matched),
    type = if_else(ingredient, "ingredient", type),
    match_method = if_else(ingredient, "exact", match_method),
    concept_name = if_else(ingredient, substance, concept_name),
    concept_id = if_else(ingredient, sprintf("%i", ingredient_id), concept_id),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# exact matches of RxNorm precise ingredient
substances <- substances |>
  left_join(preciseIngredients, by = "substance") |>
  mutate(
    ingredient = !is.na(precise_ingredient_id) & !matched,
    matched = if_else(ingredient, TRUE, matched),
    type = if_else(ingredient, "precise ingredient", type),
    match_method = if_else(ingredient, "exact", match_method),
    concept_name = if_else(ingredient, substance, concept_name),
    concept_id = if_else(ingredient, sprintf("%i", precise_ingredient_id), concept_id),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# matches after applying only the explicit spelling rules above
spellingVocabulary <- bind_rows(
  ingredients |>
    transmute(
      spelling = normaliseEnglishSpelling(substance),
      spelling_concept_id = ingredient_id,
      spelling_concept_name = substance,
      mapping_type = "ingredient",
      priority = 1L
    ),
  preciseIngredients |>
    transmute(
      spelling = normaliseEnglishSpelling(substance),
      spelling_concept_id = precise_ingredient_id,
      spelling_concept_name = substance,
      mapping_type = "precise ingredient",
      priority = 2L
    )
) |>
  distinct() |>
  filter(priority == min(priority), .by = spelling) |>
  filter(n_distinct(spelling_concept_id) == 1, .by = spelling) |>
  slice_head(n = 1, by = spelling)

substances <- substances |>
  mutate(spelling = normaliseEnglishSpelling(substance)) |>
  left_join(spellingVocabulary, by = "spelling") |>
  mutate(
    spelling_match = !is.na(spelling_concept_id) & !matched,
    matched = if_else(spelling_match, TRUE, matched),
    type = if_else(spelling_match, mapping_type, type),
    match_method = if_else(spelling_match, "normalised", match_method),
    concept_name = if_else(spelling_match, spelling_concept_name, concept_name),
    concept_id = if_else(
      spelling_match,
      sprintf("%i", spelling_concept_id),
      concept_id
    ),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# exact matches to the reviewed therapeutic classes above
substances <- substances |>
  left_join(ingredientClasses, by = c("substance" = "spelling")) |>
  mutate(
    class_match = !is.na(spelling_concept_id) & !matched,
    matched = if_else(class_match, TRUE, matched),
    type = if_else(class_match, mapping_type, type),
    match_method = if_else(class_match, "reviewed class", match_method),
    concept_name = if_else(class_match, spelling_concept_name, concept_name),
    concept_id = if_else(
      class_match,
      sprintf("%i", spelling_concept_id),
      concept_id
    ),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# exact OMOP synonyms that resolve to one standard ingredient
synonymVocabulary <- tibble(
  synonym = character(),
  synonym_concept_id = integer(),
  synonym_concept_name = character()
)

if (
  exists("cdm") &&
    all(
      c("concept", "concept_synonym", "concept_relationship") %in% names(cdm)
    )
) {
  unmatchedTerms <- substances |>
    filter(!matched) |>
    pull(substance)

  candidateSpellings <- unique(c(
    unmatchedTerms,
    str_to_sentence(unmatchedTerms),
    str_to_title(unmatchedTerms),
    str_to_upper(unmatchedTerms)
  ))

  synonymSources <- cdm[["concept_synonym"]] |>
    filter(concept_synonym_name %in% candidateSpellings) |>
    select(source_concept_id = concept_id, concept_synonym_name) |>
    collect() |>
    plainTibble() |>
    transmute(
      source_concept_id,
      synonym = str_to_lower(concept_synonym_name)
    ) |>
    filter(synonym %in% unmatchedTerms)

  directSynonymTargets <- cdm[["concept"]] |>
    filter(
      concept_id %in% synonymSources$source_concept_id,
      domain_id == "Drug",
      standard_concept == "S",
      concept_class_id == "Ingredient",
      is.na(invalid_reason)
    ) |>
    select(concept_id, concept_name) |>
    collect() |>
    plainTibble()

  synonymRelationships <- cdm[["concept_relationship"]] |>
    filter(
      concept_id_1 %in% synonymSources$source_concept_id,
      relationship_id == "Maps to",
      is.na(invalid_reason)
    ) |>
    select(
      source_concept_id = concept_id_1,
      target_concept_id = concept_id_2
    ) |>
    collect() |>
    plainTibble()

  mappedSynonymTargets <- cdm[["concept"]] |>
    filter(
      concept_id %in% synonymRelationships$target_concept_id,
      domain_id == "Drug",
      standard_concept == "S",
      concept_class_id == "Ingredient",
      is.na(invalid_reason)
    ) |>
    select(concept_id, concept_name) |>
    collect() |>
    plainTibble()

  synonymVocabulary <- bind_rows(
    synonymSources |>
      inner_join(
        directSynonymTargets,
        by = c("source_concept_id" = "concept_id")
      ) |>
      transmute(
        synonym,
        synonym_concept_id = source_concept_id,
        synonym_concept_name = concept_name,
        priority = 1L
    ),
    synonymSources |>
      inner_join(
        synonymRelationships,
        by = "source_concept_id",
        relationship = "many-to-many"
      ) |>
      inner_join(
        mappedSynonymTargets,
        by = c("target_concept_id" = "concept_id")
      ) |>
      transmute(
        synonym,
        synonym_concept_id = target_concept_id,
        synonym_concept_name = concept_name,
        priority = 2L
      )
  ) |>
    distinct() |>
    filter(priority == min(priority), .by = synonym) |>
    filter(n_distinct(synonym_concept_id) == 1, .by = synonym) |>
    slice_head(n = 1, by = synonym) |>
    select(-priority)
}

substances <- substances |>
  left_join(synonymVocabulary, by = c("substance" = "synonym")) |>
  mutate(
    synonym_match = !is.na(synonym_concept_id) & !matched,
    matched = if_else(synonym_match, TRUE, matched),
    type = if_else(synonym_match, "ingredient", type),
    match_method = if_else(synonym_match, "OMOP synonym", match_method),
    concept_name = if_else(
      synonym_match,
      synonym_concept_name,
      concept_name
    ),
    concept_id = if_else(
      synonym_match,
      sprintf("%i", synonym_concept_id),
      concept_id
    ),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

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
    match_method = if_else(combination, "exact", match_method),
    concept_name = if_else(combination, comb_name, concept_name),
    concept_id = if_else(combination, comb_id, concept_id),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# search remaining combinations after applying the spelling rules
spellingCombinations <- substances |>
  filter(!matched, grepl(" and | with ", substance)) |>
  select(substance) |>
  mutate(original = substance) |>
  separate_longer_delim(substance, delim = regex(" and | with ")) |>
  mutate(
    substance = str_squish(substance),
    spelling = normaliseEnglishSpelling(substance)
  ) |>
  mutate(nsub = n(), .by = original) |>
  left_join(spellingVocabulary, by = "spelling") |>
  filter(!is.na(spelling_concept_id)) |>
  filter(n() == nsub, .by = original) |>
  summarise(
    comb_name = paste(spelling_concept_name, collapse = "; "),
    comb_id = paste(sprintf("%i", spelling_concept_id), collapse = "; "),
    .by = original
  ) |>
  rename(substance = "original")

substances <- substances |>
  left_join(spellingCombinations, by = "substance") |>
  mutate(
    combination = !is.na(comb_id) & !matched,
    matched = if_else(combination, TRUE, matched),
    type = if_else(combination, "combination", type),
    match_method = if_else(combination, "normalised", match_method),
    concept_name = if_else(combination, comb_name, concept_name),
    concept_id = if_else(combination, comb_id, concept_id),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# search remaining combinations containing a reviewed therapeutic class
classCombinationVocabulary <- bind_rows(
  spellingVocabulary |>
    select(
      spelling,
      spelling_concept_id,
      spelling_concept_name,
      mapping_type
    ),
  ingredientClasses
)

classCombinations <- substances |>
  filter(!matched, grepl(" and | with ", substance)) |>
  select(substance) |>
  mutate(original = substance) |>
  separate_longer_delim(substance, delim = regex(" and | with ")) |>
  mutate(
    substance = str_squish(substance),
    spelling = normaliseEnglishSpelling(substance)
  ) |>
  mutate(nsub = n(), .by = original) |>
  left_join(classCombinationVocabulary, by = "spelling") |>
  filter(!is.na(spelling_concept_id)) |>
  filter(n() == nsub, .by = original) |>
  filter(any(mapping_type == "ingredient class"), .by = original) |>
  summarise(
    comb_name = paste(spelling_concept_name, collapse = "; "),
    comb_id = paste(sprintf("%i", spelling_concept_id), collapse = "; "),
    .by = original
  ) |>
  rename(substance = "original")

substances <- substances |>
  left_join(classCombinations, by = "substance") |>
  mutate(
    combination = !is.na(comb_id) & !matched,
    matched = if_else(combination, TRUE, matched),
    type = if_else(combination, "combination", type),
    match_method = if_else(combination, "reviewed class", match_method),
    concept_name = if_else(combination, comb_name, concept_name),
    concept_id = if_else(combination, comb_id, concept_id),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# search slash-separated combinations in BNF chemical substance names
slashVocabulary <- bind_rows(
  spellingVocabulary |>
    select(
      spelling,
      spelling_concept_id,
      spelling_concept_name,
      mapping_type
    ) |>
    mutate(priority = 1L),
  synonymVocabulary |>
    transmute(
      spelling = normaliseEnglishSpelling(synonym),
      spelling_concept_id = synonym_concept_id,
      spelling_concept_name = synonym_concept_name,
      mapping_type = "ingredient",
      priority = 2L
    ),
  ingredientClasses |>
    mutate(priority = 3L)
) |>
  distinct() |>
  filter(priority == min(priority), .by = spelling) |>
  filter(n_distinct(spelling_concept_id) == 1, .by = spelling) |>
  slice_head(n = 1, by = spelling)

slashCombinations <- substances |>
  filter(
    !matched,
    str_detect(substance, fixed("/")),
    !str_detect(substance, "\\b(kcal|mg|microgram|ml|g)/|/(ml|g)\\b")
  ) |>
  transmute(
    original = substance,
    slash_substance = if_else(
      str_detect(substance, "[(][^()]+/[^()]+[)]$"),
      str_match(substance, "[(]([^()]+/[^()]+)[)]$")[, 2],
      substance
    )
  ) |>
  separate_longer_delim(slash_substance, delim = "/") |>
  mutate(
    slash_substance = str_squish(slash_substance),
    spelling = normaliseEnglishSpelling(slash_substance),
    nsub = n(),
    .by = original
  ) |>
  left_join(slashVocabulary, by = "spelling") |>
  filter(!is.na(spelling_concept_id)) |>
  filter(n() == nsub, .by = original) |>
  summarise(
    comb_name = paste(spelling_concept_name, collapse = "; "),
    comb_id = paste(sprintf("%i", spelling_concept_id), collapse = "; "),
    .by = original
  ) |>
  rename(substance = "original")

substances <- substances |>
  left_join(slashCombinations, by = "substance") |>
  mutate(
    combination = !is.na(comb_id) & !matched,
    matched = if_else(combination, TRUE, matched),
    type = if_else(combination, "combination", type),
    match_method = if_else(combination, "slash combination", match_method),
    concept_name = if_else(combination, comb_name, concept_name),
    concept_id = if_else(combination, comb_id, concept_id),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# remove reviewed contextual qualifiers that do not change the ingredient
qualifiedIngredients <- substances |>
  filter(!matched, str_detect(substance, " [(][^()]+[)]$")) |>
  transmute(
    substance,
    qualifier = str_match(substance, " [(]([^()]+)[)]$")[, 2],
    spelling = normaliseEnglishSpelling(
      str_remove(substance, " [(][^()]+[)]$")
    )
  ) |>
  filter(qualifier %in% safeIngredientQualifiers) |>
  left_join(spellingVocabulary, by = "spelling") |>
  filter(!is.na(spelling_concept_id))

substances <- substances |>
  left_join(
    qualifiedIngredients |>
      select(
        substance,
        spelling_concept_id,
        spelling_concept_name,
        mapping_type
      ),
    by = "substance"
  ) |>
  mutate(
    qualifier_match = !is.na(spelling_concept_id) & !matched,
    matched = if_else(qualifier_match, TRUE, matched),
    type = if_else(qualifier_match, mapping_type, type),
    match_method = if_else(
      qualifier_match,
      "qualifier removed",
      match_method
    ),
    concept_name = if_else(
      qualifier_match,
      spelling_concept_name,
      concept_name
    ),
    concept_id = if_else(
      qualifier_match,
      sprintf("%i", spelling_concept_id),
      concept_id
    ),
  ) |>
  select(
    "substance", "matched", "type", "match_method", "concept_name", "concept_id"
  )

# fuzzy candidates are review suggestions only; they do not change matched
fuzzySources <- substances |>
  filter(
    !matched,
    !str_detect(substance, " and | with |/"),
    !str_detect(substance, "^other |preparations?$|products?$")
  ) |>
  transmute(
    substance,
    source_key = normaliseEnglishSpelling(substance),
    first_character = str_sub(source_key, 1, 1),
    source_length = str_length(source_key)
  )

fuzzyVocabulary <- ingredients |>
  transmute(
    concept_id = ingredient_id,
    concept_name = substance,
    target_key = normaliseEnglishSpelling(substance),
    first_character = str_sub(target_key, 1, 1),
    target_length = str_length(target_key)
  ) |>
  distinct() |>
  crossing(length_difference = -2:2) |>
  mutate(source_length = target_length + length_difference)

fuzzyCandidates <- fuzzySources |>
  inner_join(
    fuzzyVocabulary,
    by = c("first_character", "source_length"),
    relationship = "many-to-many"
  ) |>
  mutate(
    distance = stringdist::stringdist(source_key, target_key, method = "lv"),
    candidate_type = "fuzzy review"
  ) |>
  filter(distance %in% 1:2) |>
  slice_min(distance, n = 3, with_ties = FALSE, by = substance) |>
  select(
    substance,
    candidate_type,
    distance,
    concept_name,
    concept_id
  ) |>
  arrange(substance, distance, concept_name)

unmatchedSubstances <- substances |>
  filter(
    !matched,
    !substance %in% fuzzyCandidates$substance
  )

substances |>
  group_by(type, match_method) |>
  tally()

tibble(
  status = c("fuzzy review", "unmatched"),
  n = c(n_distinct(fuzzyCandidates$substance), nrow(unmatchedSubstances))
)
