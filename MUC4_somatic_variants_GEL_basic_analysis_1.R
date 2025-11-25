############################################################
# Analysis of MUC4 Somatic Variants in Angiosarcoma from GEL
############################################################
# 
# 1. Script set-up
#
############################################################

setwd("/Users/annadaniel/Desktop/cambridge_assignments/angiosarcoma_genomics/angiosarcoma_somatic_variants_analysis")
library('tidyverse')
library('ggplot2')
variants_per_mb <- read.csv('./data/Mutational_Signatures.csv', sep = ";")

############################################################
#
# 2. Data cleaning
#
############################################################

# Converting the Somatic.Coding.Variants.Per.Mb to a double
variants_per_mb_short <- variants_per_mb |>
  select(Participant.Id, Somatic.Coding.Variants.Per.Mb) |>
  mutate(Somatic.Coding.Variants.Per.Mb = as.numeric(gsub(",", ".", Somatic.Coding.Variants.Per.Mb)))

############################################################
#
# 3. Basic statistics
#
############################################################

med_variants_per_mb <- median(variants_per_mb_short$Somatic.Coding.Variants.Per.Mb)
# Returns 1.565

range_variants_per_mb <- range(variants_per_mb_short$Somatic.Coding.Variants.Per.Mb)
# Returns 0.95 28.83

############################################################
#
# 4. Plot of TMB vs Participant ID
#
############################################################

# I want a plot where the signature 7 patient is coloured differently to the others
# Cleaning up the variants_per_mb data so that I have the signature 7 column
# And changing the signature 7 column to TRUE/FALSE

variants_per_mb_with_signature_7 <- variants_per_mb |>
  select(Participant.Id, Signature.7) |>
  right_join(y = variants_per_mb_short,
    by = 'Participant.Id') |>
  mutate(Signature.7 = as.factor(if_else(!is.na(Signature.7), TRUE, FALSE))) |>
  mutate(Participant.Id = as.factor(Participant.Id))

# Making a bar plot
# Decided not to colour by Signature 7 

tmb_per_patient_plot <- variants_per_mb_with_signature_7 |>
  ggplot(aes(x = Participant.Id, 
             y = Somatic.Coding.Variants.Per.Mb,
             label = Somatic.Coding.Variants.Per.Mb)) +
  xlab("Participant ID") +
  ylab("Somatic Coding Variants per Mb") +
  labs(title = "Tumour mutational burden for each patient") +
  geom_bar(stat = "identity") + 
  geom_text(position=position_dodge(0.5), vjust=-0.25, size = 3)

# Saving plot
ggsave(filename = "tmb_per_participant.png",
       tmb_per_patient_plot,
       path = "./results")
