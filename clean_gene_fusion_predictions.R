library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(forcats)
library(janitor)

gene_fusion_predictions <- read_csv(paste0(data_outside_app_dir, "/STAR_Fusion_Report.csv")) %>%
  select(Sample, FusionName, JunctionReads, SpanningFrags, TotalReads) %>%
  clean_names()

sequencing_checklist <- read_excel(paste0(data_outside_app_dir, "/SEQUENCING_checklist_091718.xlsx")) %>%
  select(pdx_name, CFCE_ID, Date_mRNA_Extracted) %>%
  distinct()

# Convert sample (CFCE_ID) to pdx_name. Some notes:

# The samples in virusseq starting with AML are coded as AML# (for single digit numbers), but should be coded as AML0# for lookup.
# Exclude sample starting with MC
# For now, censor BA124 because there is not a 1:1 relationship of sample <-> proxe_id
# Censor TA13 and AML04 since there is another sample of this PDX which we will prefer to use
# Use the *root* for matching i.e., [A-Z][1-9]. if a sample has e.g. _[A-Z] after the root, remove it.
# If there is more than one pdx_name for a given CFCE_ID, use the latest based on Date_mRNA_Extracted

gene_fusion_predictions <- gene_fusion_predictions %>%
  filter(!str_detect(sample, "MC[0-9]")) %>% # Remove MC## samples
  filter(sample != "BA124") %>% # Censor this sample until it is not listed twice in the sequencing file
  mutate(sample = case_when(
    startsWith(sample, "AML") & nchar(sample) == 4 ~ # AML Fix
    str_replace(sample, "AML", "AML0"),
    str_detect(sample, "_") ~ str_replace_all(sample, "\\_.*", ""), # Remove everything including and after _, to just use root
    TRUE ~ sample
  )) %>%
  filter(!(sample %in% c("TA13", "AML04"))) # Censor additional samples

gene_fusion_predictions_with_pdx_name <- gene_fusion_predictions %>%
  left_join(sequencing_checklist,
    by = c("sample" = "CFCE_ID")
  ) %>%
  group_by(sample, fusion_name) %>% # If more than one pdx_name for sample use latest Date_mRNA_Extracted
  arrange(desc(Date_mRNA_Extracted)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-Date_mRNA_Extracted)

# Verify that there is a corresponding pdx_name for every sample

gene_fusion_predictions_with_pdx_name %>%
  filter(is.na(pdx_name)) %>%
  nrow() == 0

# Verify there is only one pdx_name for each sample

gene_fusion_predictions_with_pdx_name %>%
  distinct(pdx_name, sample) %>%
  add_count(pdx_name) %>%
  filter(n > 1) %>%
  nrow() == 0

# Verify there is only one sample for each pdx_name

gene_fusion_predictions_with_pdx_name %>%
  distinct(pdx_name, sample) %>%
  add_count(sample) %>%
  filter(n > 1) %>%
  nrow() == 0

# Add factor so that inverse functions are shown adjacent to one another

gene_fusion_predictions_with_inverse <- gene_fusion_predictions_with_pdx_name %>%
  separate(fusion_name,
    into = c("fusion_name_part_1", "fusion_name_part_2"),
    sep = "--",
    remove = FALSE
  ) %>%
  mutate(fusion_inverse = paste0(fusion_name_part_2, "--", fusion_name_part_1)) %>%
  select(-fusion_name_part_1, -fusion_name_part_2) %>%
  mutate(
    inverse_exists = fusion_inverse %in% gene_fusion_predictions_with_pdx_name[["fusion_name"]],
    pair = ifelse(inverse_exists,
      purrr::map2_chr(
        fusion_name, fusion_inverse,
        ~ paste0(c(sort(c(.x, .y))), collapse = ",")
      ),
      fusion_name
    )
  ) 

fusion_inverse_order <- gene_fusion_predictions_with_inverse %>%
  group_by(inverse_exists, pair, fusion_name) %>%
  summarise(mean_total_reads = mean(total_reads)) %>%
  group_by(pair) %>%
  arrange(desc(mean_total_reads)) %>%
  mutate(inverse_order = ifelse(inverse_exists, row_number(), NA_integer_)) %>%
  select(pair, fusion_name, inverse_order) %>%
  ungroup()

gene_fusion_predictions <- gene_fusion_predictions_with_inverse %>%
  left_join(fusion_inverse_order, by = c("pair", "fusion_name"), na_matches = "never") %>%
  arrange(!inverse_exists, pair, total_reads)

# Remove objects not needed outside app

rm(fusion_inverse_order, gene_fusion_predictions_with_inverse, gene_fusion_predictions_with_pdx_name, sequencing_checklist)