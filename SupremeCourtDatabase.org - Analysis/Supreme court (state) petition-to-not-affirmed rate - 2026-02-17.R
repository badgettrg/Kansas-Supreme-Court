# Author: bob.badgett@gmail.com
# https://badgettrg.github.io/Kansas-Supreme-Court/
# Permissions:
# * Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# * Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view

# _______________________________________________________________________ -----
# https://www.supremecourt.gov/orders/journal.aspx
# http://supremecourtdatabase.org/analysis.php
#

# ----- Among merits decisions granted to petitions (SCDB): Kansas vs all other states -----
## ----- Fisher exact test Kansas merits not-affirmed vs all other states -----

kansas_not_affirmed <- 5
kansas_affirmed     <- 1

others_not_affirmed <- 97
others_affirmed     <- 36

cont_table <- matrix(
  c(kansas_not_affirmed,
    kansas_affirmed,
    others_not_affirmed,
    others_affirmed),
  nrow = 2,
  byrow = TRUE
)

rownames(cont_table) <- c("Kansas", "Other states")
colnames(cont_table) <- c("Not affirmed", "Affirmed")

# Fisher exact test (preferred with small counts)
fisher_merits <- fisher.test(cont_table, alternative = "greater")

print(cont_table)
print(fisher_merits)

cat(black$bold(sprintf(
  "Kansas merits not-affirmed rate: %.2f%% (%d of %d merits cases)\n",
  100 * kansas_not_affirmed / (kansas_not_affirmed + kansas_affirmed),
  kansas_not_affirmed,
  kansas_not_affirmed + kansas_affirmed
)))
cat(black$bold(sprintf(
  "Other states merits not-affirmed rate: %.2f%% (%d of %d merits cases)\n\n",
  100 * others_not_affirmed / (others_not_affirmed + others_affirmed),
  others_not_affirmed,
  others_not_affirmed + others_affirmed
)))

# ___________________________________________________________-----

# ----- Overall petition-to-reversal/vacatur probability: Kansas vs all other states -----
## NOTE: This uses the "confirmed reversals/vacaturs" numerator from the SCDB merits tables
## (not the conditional P(reversal | grant)). It is an apples-to-apples petition-stage comparison.

# Kansas: confirmed not-affirmed merits outcomes from your Kansas SCDB table
kansas_not_affirmed_merits <- 5L

# Kansas: petition denominator from your journal extraction
kansas_petitions <- nrow(df_cert_hits_kansas)

# All states: confirmed not-affirmed merits outcomes from your all-states SCDB table
all_states_not_affirmed_merits <- 102L

# All states: petition denominator from your journal extraction
all_states_petitions <- nrow(df_cert_hits_all_states)

# Other states = all minus Kansas
other_states_not_affirmed_merits <- all_states_not_affirmed_merits - kansas_not_affirmed_merits
other_states_petitions <- all_states_petitions - kansas_petitions

# Build 2x2 table: Reversed/Vacated vs Not Reversed/Vacated (at the petition stage)
cont_table_petition_to_not_affirmed <- matrix(
  c(
    kansas_not_affirmed_merits,
    kansas_petitions - kansas_not_affirmed_merits,
    other_states_not_affirmed_merits,
    other_states_petitions - other_states_not_affirmed_merits
  ),
  nrow = 2,
  byrow = TRUE
)

rownames(cont_table_petition_to_not_affirmed) <- c("Kansas", "Other states")
colnames(cont_table_petition_to_not_affirmed) <- c(
  "Not affirmed (merits)",
  "Petitions not resulting in not-affirmed merits"
)


print(cont_table_petition_to_not_affirmed)

cat(black$bold(sprintf(
  "Fisher odds ratio (Kansas vs other states): %.3f\n\n",
  unname(fisher_petition$estimate)
)))

# Fisher exact test (preferred with small counts)
fisher_petition <- fisher.test(cont_table_petition_to_not_affirmed, alternative = "greater")
print(fisher_petition)

# Also compute and print the two rates
kansas_rate_overall <- kansas_not_affirmed_merits / kansas_petitions
other_rate_overall  <- other_states_not_affirmed_merits / other_states_petitions

cat(black$bold(sprintf(
  "Kansas petition-to-not-affirmed rate: %.2f%% (%d of %d petitions)\n",
  kansas_rate_overall * 100, kansas_not_affirmed_merits, kansas_petitions
)))
cat(black$bold(sprintf(
  "Other states petition-to-not-affirmed rate: %.2f%% (%d of %d petitions)\n\n",
  other_rate_overall * 100, other_states_not_affirmed_merits, other_states_petitions
)))

comments <- "This compares petition-stage denominators to merits-stage numerators (from SCDB tables). It is a coherent “overall pipeline” metric, but it assumes your SCDB numerators and your Journal petition denominators cover the same years/corpus."
comments

cat("Your denominator is petition-stage (journal)\n")
cat("Your numerator is merits-stage not-affirmed (SCDB at http://supremecourtdatabase.org/analysis.php)\n")
cat("Therefore, this “pipeline” metric is valid only if the time window/corpus align reasonably.\n")
