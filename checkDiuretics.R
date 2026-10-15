library(dplyr)

cdm <- omock::mockCdmFromDataset(datasetName = "delphi-100k")

getPreciseIngredient <- function(x, cdm) {
  cdm$concept_relationship |>
    filter(concept_id_1 %in% x, relationship_id == "Precise ing of") |>
    inner_join(
      cdm$concept |>
        filter(standard_concept == "S") |>
        select("concept_id_2" = "concept_id"),
      by = "concept_id_2"
    ) |>
    pull("concept_id_2")
}
getDescendants <- function(x, cdm) {
  cdm$concept_ancestor |>
    filter(ancestor_concept_id %in% x) |>
    inner_join(
      cdm$concept |>
        filter(standard_concept == "S") |>
        select("descendant_concept_id" = "concept_id"),
      by = "descendant_concept_id"
    ) |>
    pull("descendant_concept_id")
}

getIngredientClassDescendants <- function(x, cdm) {
  ingredientIds <- cdm$concept_ancestor |>
    filter(ancestor_concept_id == x) |>
    inner_join(
      cdm$concept |>
        filter(
          standard_concept == "S",
          concept_class_id == "Ingredient"
        ) |>
        select("descendant_concept_id" = "concept_id"),
      by = "descendant_concept_id"
    ) |>
    pull("descendant_concept_id")

  getDescendants(ingredientIds, cdm)
}

codelist <- list(
  atenolol = getDescendants(1314002L, cdm),
  chlorthalidone = getDescendants(1395058L, cdm),
  diuretics = getIngredientClassDescendants(21601461L, cdm),
  "propranolol hydrochloride" = getPreciseIngredient(19132964L, cdm),
  hydrochlorothiazide = getDescendants(974166L, cdm)
) |>
  omopgenerics::newCodelist()

awd1 <- intersect(codelist$atenolol, codelist$chlorthalidone)
awd2 <- intersect(codelist$atenolol, codelist$diuretics)
