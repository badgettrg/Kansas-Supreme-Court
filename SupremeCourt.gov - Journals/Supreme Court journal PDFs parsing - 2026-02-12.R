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
# ----- Output dfs about Objects -----
# This script creates:
#
# 1) SCOTUS_journal_cert_petitions_all_state_supreme_courts.csv
#    - All certiorari petitions to state supreme courts found in the
#      SCOTUS Journal PDFs.
#
# 2) SCOTUS_journal_certiorari_petitions_kansas_supreme_court_with_dispositions.csv
#    - Subset of the above limited to Kansas.
#
# 3) SCOTUS_journal_cert_petitions_all_states_and_kansas_dfs.rds
#    - An RDS file containing BOTH dataframes as a named list:
#
#        list(
#          all_states = df_cert_hits_all_states,
#          kansas     = df_cert_hits_kansas
#        )
#
#    To reload without re-parsing PDFs:
#
#        obj <- readRDS("SCOTUS_journal_cert_petitions_all_states_and_kansas_dfs.rds")
#        obj$all_states
#        obj$kansas
#
#    This allows further formatting or analysis without rerunning extraction.
# _______________________________________________________________________ -----


# _______________________________________________________________________ -----
# ----- Startup -----

if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(script_path) > 0) setwd(dirname(script_path))
}
getwd()


# _______________________________________________________________________ -----
# ----- Packages -----

pkgs <- c("pdftools", "stringr", "dplyr", "tibble", "readr", "purrr", "crayon")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(pdftools)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(crayon)


# _______________________________________________________________________ -----
# ----- Helper functions -----

normalize_pdf_text <- function(x) {
  x <- gsub("-\\s*\\n\\s*", "", x)   # de-hyphenate across line breaks
  x <- gsub("\\s+", " ", x)         # collapse whitespace
  trimws(x)
}

fix_mojibake <- function(x) {
  if (is.null(x)) return(x)
  if (length(x) == 0) return(x)
  x <- as.character(x)
  
  x <- gsub("â€“", "–", x, fixed = TRUE)
  x <- gsub("â€”", "—", x, fixed = TRUE)
  x <- gsub("â€˜", "‘", x, fixed = TRUE)
  x <- gsub("â€™", "’", x, fixed = TRUE)
  x <- gsub("â€œ", "“", x, fixed = TRUE)
  x <- gsub("â€�", "”", x, fixed = TRUE)
  x <- gsub("â€¦", "…", x, fixed = TRUE)
  x <- gsub("Â ", " ", x, fixed = TRUE)
  
  x
}

extract_snippets <- function(text, pattern, use_regex = FALSE, context_chars = 200) {
  if (is.na(text) || !nzchar(text)) return(character(0))
  
  locs <- if (use_regex) {
    gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE)
  } else {
    gregexpr(pattern, text, fixed = TRUE)
  }
  
  starts <- as.integer(locs[[1]])
  if (length(starts) == 1 && starts[1] == -1) return(character(0))
  
  match_len <- if (use_regex) {
    matches <- regmatches(text, locs)[[1]]
    nchar(matches)
  } else {
    rep(nchar(pattern), length(starts))
  }
  
  purrr::map2_chr(starts, match_len, function(s, L) {
    left  <- max(1, s - context_chars)
    right <- min(nchar(text), s + L + context_chars)
    snippet <- substr(text, left, right)
    snippet <- stringr::str_replace_all(snippet, "\\s+", " ")
    stringr::str_trim(snippet)
  })
}

extract_docket_no <- function(x) {
  x <- fix_mojibake(as.character(x))
  
  # More tolerant:
  # - Allow optional trailing period after the docket number
  # - Accept en dash (–) or hyphen (-) between term and sequence
  # - Stop matching before punctuation/space if needed
  #
  # Examples matched:
  #   "No. 06–1251."
  #   "No. 06–1251"
  #   "No. 21-123."
  #   "No. 21-123"
  stringr::str_extract(
    x,
    "No\\.\\s*[0-9]{1,4}[A-Za-zM]*\\s*(?:[–-]\\s*[0-9]{1,6}[A-Za-z]*)?\\.?\\b"
  )
}

# Regex: capture STATE and DISPOSITION as separate groups -----
#
# Expanded coverage:
#   - Supreme Court of <STATE>
#   - Supreme Judicial Court of <STATE>
#   - Court of Appeals of <STATE>   (PARTIAL broad scope; see note below)
#
# State capture is restricted to letters/spaces to prevent bleed-through.

CERT_REGEX_SC_SJC_STATE_AND_DISP <- paste0(
  "Petition for writ of certiorari to the ",
  "(?:Supreme Court|Supreme Judicial Court) of\\s+",
  "([A-Za-z][A-Za-z\\s]+?)",
  "(?:\\.|,)?\\s+",
  "\\b(denied|granted|dismissed)\\b",
  "[^.]{0,600}\\."
)

CERT_REGEX_COA_STATE_AND_DISP <- paste0(
  "Petition for writ of certiorari to the Court of Appeals of\\s+",
  "([A-Za-z][A-Za-z\\s]+?)",
  "(?:\\.|,)?\\s+",
  "\\b(denied|granted|dismissed)\\b",
  "[^.]{0,600}\\."
)

CERT_REGEX_SUPERIOR_COURT_STATE_AND_DISP <- paste0(
  "Petition for writ of certiorari to the Superior Court of\\s+",
  "([A-Za-z][A-Za-z\\s]+?)",
  "(?:\\.|,)?\\s+",
  "\\b(denied|granted|dismissed)\\b",
  "[^.]{0,600}\\."
)

COURT_LABEL_SUPERIOR <- "Intermediate Appellate (Superior Court)"

# Labels for the court type that triggered the match
COURT_LABEL_SC_SJC <- "State Supreme (Supreme/Supreme Judicial)"
COURT_LABEL_COA    <- "Intermediate Appellate (Court of Appeals)"

# Combined pattern for snippet extraction (used only when detailed = TRUE)
CERT_REGEX_ANY_COURT_STATE_AND_DISP <- paste0(
  "(?:",
  CERT_REGEX_SC_SJC_STATE_AND_DISP,
  ")|(?:",
  CERT_REGEX_COA_STATE_AND_DISP,
  ")|(?:",
  CERT_REGEX_SUPERIOR_COURT_STATE_AND_DISP,
  ")"
)

find_cert_matches_all_states <- function(text_norm, state_court_origin_scope = "state_supreme_or_other") {
  
  run_one <- function(rx, court_label) {
    
    m <- stringr::str_match_all(text_norm, stringr::regex(rx, ignore_case = TRUE))[[1]]
    if (nrow(m) == 0) {
      return(tibble(starts = integer(0), states = character(0), dispo = character(0), origin_court = character(0)))
    }
    
    locs <- gregexpr(rx, text_norm, perl = TRUE, ignore.case = TRUE)[[1]]
    starts <- as.integer(locs)
    if (length(starts) == 1 && starts[1] == -1) {
      return(tibble(starts = integer(0), states = character(0), dispo = character(0), origin_court = character(0)))
    }
    
    states <- fix_mojibake(m[, 2])
    
    states <- stringr::str_squish(states)
    states <- gsub(intToUtf8(0x00AD), "", states, fixed = TRUE)
    states <- iconv(states, from = "", to = "UTF-8", sub = "")
    states <- gsub("(?<=[a-z])\\s+(?=[a-z])", "", states, perl = TRUE)
    states <- gsub("^Appeals of West Virginia$", "West Virginia", states, ignore.case = TRUE)
    states <- gsub("^Appealsof West Virginia$", "West Virginia", states, ignore.case = TRUE)
    states <- sub(",.*$", "", states)
    states <- stringr::str_squish(states)
    states <- stringr::str_to_title(states)
    
    dispo <- fix_mojibake(m[, 3])
    dispo <- stringr::str_to_lower(stringr::str_squish(dispo))
    
    dispo <- dplyr::case_when(
      dispo == "denied"    ~ "Denied",
      dispo == "granted"   ~ "Granted",
      dispo == "dismissed" ~ "Dismissed",
      TRUE ~ NA_character_
    )
    
    k <- min(length(starts), length(states), length(dispo))
    tibble(
      starts = starts[seq_len(k)],
      states = states[seq_len(k)],
      dispo = dispo[seq_len(k)],
      origin_court = rep(court_label, k)
    )
  }
  
  # Always run SC/SJC
  out_sc <- run_one(CERT_REGEX_SC_SJC_STATE_AND_DISP, COURT_LABEL_SC_SJC)
  
  # Conditionally run intermediate courts (partial broad scope)
  out_other <- if (identical(state_court_origin_scope, "state_supreme_or_other")) {
    dplyr::bind_rows(
      run_one(CERT_REGEX_COA_STATE_AND_DISP, COURT_LABEL_COA),
      run_one(CERT_REGEX_SUPERIOR_COURT_STATE_AND_DISP, COURT_LABEL_SUPERIOR)
    )
  } else {
    tibble(starts = integer(0), states = character(0), dispo = character(0), origin_court = character(0))
  }
  
  out <- dplyr::bind_rows(out_sc, out_other)
  
  if (nrow(out) == 0) {
    return(list(starts = integer(0), states = character(0), dispo = character(0), origin_court = character(0)))
  }
  
  out <- out %>% dplyr::arrange(starts)
  
  list(
    starts = out$starts,
    states = out$states,
    dispo = out$dispo,
    origin_court = out$origin_court
  )
}

extract_entries_given_starts <- function(text_norm, match_starts) {
  if (length(match_starts) == 0) return(character(0))
  
  no_locs <- gregexpr("No\\.", text_norm, perl = TRUE)[[1]]
  no_starts <- as.integer(no_locs)
  if (length(no_starts) == 1 && no_starts[1] == -1) no_starts <- integer(0)
  
  purrr::map_chr(match_starts, function(match_start) {
    
    entry_start <- 1L
    if (length(no_starts) > 0) {
      prior_no <- no_starts[no_starts < match_start]
      if (length(prior_no) > 0) entry_start <- max(prior_no)
    }
    
    tail_txt <- substr(text_norm, match_start, nchar(text_norm))
    
    # EDIT: End at the first disposition keyword and the next period AFTER it,
    # allowing qualifiers like "granted limited to ..." before the period.
    end_loc <- regexpr("\\b(denied|granted|dismissed)\\b[^.]{0,600}\\.",
                       tail_txt,
                       ignore.case = TRUE, perl = TRUE)
    
    if (end_loc[1] == -1) {
      entry_end <- min(nchar(text_norm), match_start + 800L)
    } else {
      entry_end <- match_start + end_loc[1] + attr(end_loc, "match.length") - 2L
    }
    
    entry <- substr(text_norm, entry_start, entry_end)
    entry <- stringr::str_replace_all(entry, "\\s+", " ")
    entry <- stringr::str_trim(entry)
    fix_mojibake(entry)
  })
}

read_pdfs_once <- function(pdf_files) {
  out <- vector("list", length(pdf_files))
  names(out) <- basename(pdf_files)
  
  for (i in seq_along(pdf_files)) {
    f <- pdf_files[[i]]
    message("Reading PDF (once): ", basename(f))
    pages <- tryCatch(pdftools::pdf_text(f), error = function(e) {
      warning(sprintf("Failed to read '%s': %s", basename(f), e$message))
      return(character(0))
    })
    out[[i]] <- pages
  }
  out
}

extract_all_states_from_cached_pages <- function(pdf_pages_list,
                                                 pdf_dir = NULL,
                                                 detailed = FALSE,
                                                 context_chars = 300,
                                                 state_court_origin_scope = "state_supreme_or_other") {
  
  purrr::imap_dfr(pdf_pages_list, function(pages, pdf_basename) {
    
    file_path_val <- if (!is.null(pdf_dir)) file.path(pdf_dir, pdf_basename) else pdf_basename
    if (length(pages) == 0) return(tibble())
    
    purrr::imap_dfr(pages, function(pg_text, idx) {
      
      pg_text_norm <- fix_mojibake(normalize_pdf_text(pg_text))
      
      m <- find_cert_matches_all_states(pg_text_norm, state_court_origin_scope = state_court_origin_scope)
      if (length(m$starts) == 0) return(tibble())
      
      entries <- extract_entries_given_starts(pg_text_norm, m$starts)
      
      if (isTRUE(detailed)) {
        snippets <- extract_snippets(
          text = pg_text_norm,
          pattern = CERT_REGEX_ANY_COURT_STATE_AND_DISP,
          use_regex = TRUE,
          context_chars = context_chars
        )
        snippets <- fix_mojibake(snippets)
        
        tibble(
          file = pdf_basename,
          file_path = file_path_val,
          page = idx,
          hits_on_page = length(entries),
          docket_entry_text = entries,
          disposition = m$dispo,
          snippet = snippets,
          origin_state = m$states,
          origin_court = m$origin_court
        )
      } else {
        tibble(
          file = pdf_basename,
          file_path = file_path_val,
          page = idx,
          hits_on_page = NA_integer_,
          docket_entry_text = entries,
          disposition = m$dispo,
          snippet = NA_character_,
          origin_state = m$states,
          origin_court = m$origin_court
        )
      }
    })
  }) %>%
    dplyr::select(
      file, file_path, page, hits_on_page, docket_entry_text, disposition, snippet, origin_state, origin_court
    )
}


# _______________________________________________________________________ -----
# ----- User settings -----

pdf_dir <- "journal_PDFs"
context_chars <- 300
write_csv_outputs <- TRUE

# NEW: which state-court origins to include
state_court_origin_scope <- "state_supreme_only"
# Allowed: "state_supreme_only" or "state_supreme_or_other"
#
# NOTE: The "state_supreme_or_other" option is currently PARTIAL.
# It only adds matches for the literal phrase "Court of Appeals of <STATE>".
# Many state courts use different naming conventions (e.g., "Superior Court of Pennsylvania",
# "Court of Criminal Appeals of Tennessee", "District Court of Appeal", "Appellate Division", etc.),
# so enabling the broader option will undercount states (e.g., PA, TN) until patterns are expanded.

stopifnot(is.character(state_court_origin_scope), length(state_court_origin_scope) == 1)
if (!state_court_origin_scope %in% c("state_supreme_only", "state_supreme_or_other")) {
  stop("state_court_origin_scope must be one of: 'state_supreme_only', 'state_supreme_or_other'")
}

# OPTIONAL: include scope in filenames to avoid accidental mixing
scope_tag <- state_court_origin_scope

all_states_csv_path <- file.path(".", paste0("SCOTUS_journal_cert_petitions_", scope_tag, "_all_states.csv"))
kansas_csv_path <- file.path(".", paste0("SCOTUS_journal_cert_petitions_", scope_tag, "_kansas.csv"))
summary_combined_csv_path <- file.path(".", paste0("SCOTUS_journal_search_cert_summary_", scope_tag, "_kansas_vs_all_states.csv"))
rds_bundle_path <- file.path(".", paste0("SCOTUS_journal_cert_petitions_", scope_tag, "_all_states_and_kansas_dfs.rds"))


# _______________________________________________________________________ -----
# ----- Optional: Load cached data bundle and skip PDF parsing -----

use_cached_data <- FALSE

if (file.exists(rds_bundle_path)) {
  message("Found cached RDS bundle: ", rds_bundle_path)
  
  if (interactive()) {
    resp <- readline("Load cached data and SKIP PDF parsing? (y/n): ")
    use_cached_data <- tolower(trimws(resp)) == "y"
  } else {
    # Non-interactive (e.g., Rmd render / automation): default to cached
    use_cached_data <- TRUE
  }
}

if (isTRUE(use_cached_data)) {
  obj <- readRDS(rds_bundle_path)
  
  # Defensive checks (fail fast with clear message)
  if (!is.list(obj) || !all(c("all_states", "kansas") %in% names(obj))) {
    stop("Cached RDS did not contain expected list elements: all_states, kansas")
  }
  
  df_cert_hits_all_states <- obj$all_states
  df_cert_hits_kansas     <- obj$kansas
  
  message("Loaded cached data: df_cert_hits_all_states and df_cert_hits_kansas")
}


# _______________________________________________________________________ -----
# ----- Discover PDFs / Read PDFs / Extract master (unless cached) -----

if (!isTRUE(use_cached_data)) {
  
  # ----- Discover PDFs -----
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  if (length(pdf_files) == 0) {
    stop(sprintf("No PDF files found in '%s'. Check folder name and working directory: %s", pdf_dir, getwd()))
  }
  
  # ----- Read PDFs once (cached) -----
  pdf_pages_list <- read_pdfs_once(pdf_files)
  
  # _______________________________________________________________________ -----
  # ----- Extract master all-states dataframe -----
  # Keep minimal for now (snippet + hits_on_page NA) unless you flip to TRUE.
  df_cert_hits_all_states <- extract_all_states_from_cached_pages(
    pdf_pages_list = pdf_pages_list,
    pdf_dir = pdf_dir,
    detailed = FALSE,
    context_chars = context_chars,
    state_court_origin_scope = state_court_origin_scope
  )
  
  # _______________________________________________________________________ -----
  # ----- DEDUPLICATE BY DOCKET NUMBER (CRITICAL STABILITY BLOCK) -----
  #
  # Normalize docket numbers aggressively so tiny extraction differences
  # (en dash vs hyphen, "No." prefix, whitespace, trailing period) do not
  # defeat deduplication.
  # ----------------------------------------------------------------------
  
  df_cert_hits_all_states <- df_cert_hits_all_states %>%
    dplyr::mutate(
      docket_no = extract_docket_no(docket_entry_text),
      docket_no_norm = dplyr::if_else(
        is.na(docket_no),
        NA_character_,
        docket_no %>%
          stringr::str_squish() %>%
          stringr::str_replace_all("–", "-") %>%        # normalize en-dash
          stringr::str_replace_all("^No\\.\\s*", "") %>%# drop "No."
          stringr::str_replace_all("\\.$", "") %>%      # drop trailing period
          stringr::str_replace_all("\\s+", "") %>%      # drop whitespace
          stringr::str_to_lower()
      )
    ) %>%
    dplyr::arrange(file, page)
  
  df_cert_hits_all_states <- dplyr::bind_rows(
    # Deduplicate rows that have a docket key
    df_cert_hits_all_states %>%
      dplyr::filter(!is.na(docket_no_norm)) %>%
      dplyr::distinct(docket_no_norm, .keep_all = TRUE),
    # Keep rare rows without a docket key, deduped by text
    df_cert_hits_all_states %>%
      dplyr::filter(is.na(docket_no_norm)) %>%
      dplyr::distinct(docket_entry_text, .keep_all = TRUE)
  ) %>%
    dplyr::select(-docket_no_norm)
  
  # Clean up -----
  rm(list = intersect(c("df_tmp", "df_has_docket", "df_no_docket"), ls()))
  
  # _______________________________________________________________________ -----
  # ----- Derive Disposition from docket_entry_text (robust parsing) -----
  
  df_cert_hits_all_states <- df_cert_hits_all_states %>%
    dplyr::mutate(
      disposition = dplyr::case_when(
        stringr::str_detect(docket_entry_text, regex("\\bgranted\\b",  ignore_case = TRUE)) ~ "Granted",
        stringr::str_detect(docket_entry_text, regex("\\bdenied\\b",   ignore_case = TRUE)) ~ "Denied",
        stringr::str_detect(docket_entry_text, regex("\\bdismissed\\b",ignore_case = TRUE)) ~ "Dismissed",
        TRUE ~ NA_character_
      )
    )
  
  # _______________________________________________________________________ -----
  # ----- Normalize to 50 U.S. states only -----
  
  us_states <- c(
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
    "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
    "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
    "Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
    "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
    "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
    "Virginia","Washington","West Virginia","Wisconsin","Wyoming"
  )
  
  df_cert_hits_all_states <- df_cert_hits_all_states %>%
    dplyr::mutate(
      origin_state = dplyr::case_when(
        origin_state == "Illinios" ~ "Illinois",
        TRUE ~ origin_state
      )
    )
  
  state_pattern <- paste0("^(", paste(sort(us_states, decreasing = TRUE), collapse = "|"), ")(\\b|$)")
  m_state <- stringr::str_match(df_cert_hits_all_states$origin_state, state_pattern)
  
  df_cert_hits_all_states$origin_state <- dplyr::if_else(
    !is.na(m_state[, 2]),
    m_state[, 2],
    df_cert_hits_all_states$origin_state
  )
  
  df_cert_hits_all_states <- df_cert_hits_all_states %>%
    dplyr::filter(origin_state %in% us_states)
  
  # _______________________________________________________________________ -----
  # ----- Create Kansas subset dataframe (from master) -----
  
  kansas_regex <- if (identical(state_court_origin_scope, "state_supreme_only")) {
    "Petition for writ of certiorari to the (Supreme Court|Supreme Judicial Court) of\\s+Kansas\\b"
  } else {
    "Petition for writ of certiorari to the (Supreme Court|Supreme Judicial Court) of\\s+Kansas\\b|Petition for writ of certiorari to the Court of Appeals of\\s+Kansas\\b"
  }
  
  df_cert_hits_kansas <- df_cert_hits_all_states %>%
    dplyr::filter(
      stringr::str_detect(
        gsub(intToUtf8(0x00AD), "", docket_entry_text, fixed = TRUE),
        stringr::regex(kansas_regex, ignore_case = TRUE)
      )
    )
  
} # end !use_cached_data


# _______________________________________________________________________ -----
# ----- If cached, still ensure required downstream variables exist -----
# (and keep behavior consistent with the non-cached path)

# NOTE: When using cached data, we assume df_cert_hits_all_states and df_cert_hits_kansas
# already contain dispositions, normalized states, etc., from the prior run that created the RDS.
# If you want to enforce re-processing even for cached data, move downstream blocks outside
# the !use_cached_data guard.

# _______________________________________________________________________ -----
# ----- Combined summary (hits per PDF file, Kansas vs all-states) -----
## ----- Scope diagnostic -----

cat("State court origin scope:", state_court_origin_scope, "\n\n")

summary_all_states <- df_cert_hits_all_states %>%
  dplyr::count(file, name = "total_hits_all_states")

summary_kansas <- df_cert_hits_kansas %>%
  dplyr::count(file, name = "total_hits_kansas")

summary_combined <- dplyr::full_join(summary_all_states, summary_kansas, by = "file") %>%
  dplyr::mutate(
    total_hits_all_states = dplyr::coalesce(total_hits_all_states, 0L),
    total_hits_kansas = dplyr::coalesce(total_hits_kansas, 0L)
  ) %>%
  dplyr::arrange(dplyr::desc(total_hits_all_states), dplyr::desc(total_hits_kansas))

cat(green$bold("\nAll-states unique states: ", length(unique(df_cert_hits_all_states$origin_state)),"\n\n"))

## ----- All-states summary -----

cat(black$bold("All-states rows (petitions):", nrow(df_cert_hits_all_states), "\n"))
print(table(df_cert_hits_all_states$disposition, useNA = "ifany"))
grant_rate_all <- mean(df_cert_hits_all_states$disposition == "Granted") * 100
cat(black$bold(sprintf("All-states grant rate of petitions: %.2f%%\n\n", grant_rate_all)))

## ----- Kansas summary -----

cat(black$bold("Kansas rows (petitions):", nrow(df_cert_hits_kansas), "\n"))
print(table(df_cert_hits_kansas$disposition, useNA = "ifany"))

grant_rate_kansas <- mean(df_cert_hits_kansas$disposition == "Granted") * 100
cat(black$bold(sprintf("Kansas grant rate of petitions: %.2f%%\n\n", grant_rate_kansas)))

## ----- All-states excluding Kansas summary -----

df_cert_hits_all_states_excl_kansas <- df_cert_hits_all_states %>%
  dplyr::filter(origin_state != "Kansas")

cat(black$bold("All-states excluding Kansas rows (petitions):", nrow(df_cert_hits_all_states_excl_kansas), "\n"))
print(table(df_cert_hits_all_states_excl_kansas$disposition, useNA = "ifany"))

grant_rate_excl_kansas <- mean(df_cert_hits_all_states_excl_kansas$disposition == "Granted") * 100
cat(black$bold(sprintf("All-states excluding Kansas grant rate of petitions: %.2f%%\n\n", grant_rate_excl_kansas)))

## ----- State coverage diagnostics -----
### ----- Highest state grant rate (state_supreme_only scope) -----

state_grant_summary <- df_cert_hits_all_states %>%
  dplyr::group_by(origin_state) %>%
  dplyr::summarise(
    petitions = n(),
    grants = sum(disposition == "Granted"),
    grant_rate = (grants / petitions) * 100,
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(grant_rate))

top_state <- state_grant_summary %>% dplyr::slice(1)

cat(black$bold(
  sprintf("Highest state certiorari grant (review) rate: %s (%.2f%%, %d of %d petitions)\n\n",
          top_state$origin_state,
          top_state$grant_rate,
          top_state$grants,
          top_state$petitions)
))

## ----- Binomial test: highest state vs national certiorari grant rate -----

# National grant rate
national_grants <- sum(df_cert_hits_all_states$disposition == "Granted")
national_total  <- nrow(df_cert_hits_all_states)
national_rate   <- national_grants / national_total

# Highest state (already computed earlier as top_state)
highest_state_name   <- top_state$origin_state
highest_state_grants <- top_state$grants
highest_state_total  <- top_state$petitions
highest_state_rate   <- top_state$grant_rate / 100

# One-sided binomial test (is highest state higher than national rate?)
binom_result <- binom.test(
  x = highest_state_grants,
  n = highest_state_total,
  p = national_rate,
  alternative = "greater"
)

cat(black$bold(sprintf(
  "National certiorari grant rate: %.2f%% (%d of %d)\n",
  national_rate * 100,
  national_grants,
  national_total
)))

cat(black$bold(sprintf(
  "%s certiorari grant rate as highest state: %.2f%% (%d of %d)\n",
  highest_state_name,
  highest_state_rate * 100,
  highest_state_grants,
  highest_state_total
)))

cat(black$bold(sprintf(
  "%s vs national binomial test p-value: %.4f\n",
  highest_state_name,
  binom_result$p.value
)))

cat(black$bold(sprintf(
  "95%% confidence interval for %s grant rate: %.2f%% to %.2f%%\n\n",
  highest_state_name,
  binom_result$conf.int[1] * 100,
  binom_result$conf.int[2] * 100
)))

### Count by state -----
state_counts <- df_cert_hits_all_states %>%
  dplyr::count(origin_state, name = "n_cert_petitions") %>%
  dplyr::arrange(dplyr::desc(n_cert_petitions))

cat("\nTop states by cert petitions:\n")
print(state_counts, n = 60)

# Identify missing states
states_present <- sort(unique(df_cert_hits_all_states$origin_state))
states_missing <- setdiff(sort(us_states), states_present)

cat("\nNumber of states present:", length(states_present), "\n")
cat("Number of states missing:", length(states_missing), "\n\n")

if (length(states_missing) > 0) {
  cat("States missing under current scope:\n")
  cat(red$bold("States missing under current scope:\n"))
  print(states_missing)
} else {
  cat("All 50 states represented under current scope.\n")
}

# _______________________________________________________________________ -----
# ----- Write outputs -----

cat("All-states unique states:", length(unique(df_cert_hits_all_states$origin_state)), "\n")
cat("Kansas rows:", nrow(df_cert_hits_kansas), "\n")
print(table(df_cert_hits_kansas$disposition, useNA = "ifany"))


if (isTRUE(write_csv_outputs)) {
  
  readr::write_excel_csv(df_cert_hits_all_states, all_states_csv_path)
  message("Wrote: ", all_states_csv_path)
  
  readr::write_excel_csv(df_cert_hits_kansas, kansas_csv_path)
  message("Wrote: ", kansas_csv_path)
  
  readr::write_excel_csv(summary_combined, summary_combined_csv_path)
  message("Wrote: ", summary_combined_csv_path)
  
  if (!isTRUE(use_cached_data)) {
    saveRDS(
      list(all_states = df_cert_hits_all_states, kansas = df_cert_hits_kansas),
      rds_bundle_path
    )
    message("Wrote: ", rds_bundle_path)
  } else {
    message("Skipped writing RDS (using cached data).")
  }
}


# _______________________________________________________________________ -----
# ----- Optional: Reload bundled dfs without re-parsing PDFs -----
# obj <- readRDS("SCOTUS_journal_cert_petitions_all_states_and_kansas_dfs.rds")
# obj$all_states
# obj$kansas
# _______________________________________________________________________ -----
