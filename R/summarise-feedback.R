# summarise-feedback.R ------------------------------------------------------
#
# Turns a raw feedback export into the two small aggregate files that
# content/feedback.qmd reads:
#
#   data/feedback-summary.csv   question_id, question, response, n, order
#   data/feedback-meta.csv      n_respondents, last_updated
#
# Run from the repo root:
#   Rscript R/summarise-feedback.R dev/feedback/responses.csv
#
# PRIVACY -- read before changing anything here.
# This is a public repository. Raw responses must live ONLY in dev/, which is
# gitignored. The two files this script writes land in data/, which is tracked
# AND published with the site, so they must never contain anything but grouped
# counts. The self-check below refuses to write if free-text answers, email
# addresses or anything identifying survive into the output. Do not weaken it.
#
# Not participant-facing; this is a maintenance script, not teaching material.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

# --- Which questions are published, and in what order ----------------------
# Anything NOT listed here is dropped and never leaves dev/. The free-text
# questions ("What was most useful?", "What should we change?") are
# deliberately absent: they are read by the organisers and go no further.
#
# `levels` fixes the display order of each question's options. It is what
# stops "Too fast" sorting before "About right" on the page. A response label
# used by more than one question must sit at the same position in each, because
# the page builds one shared factor across questions.
PUBLISHED <- list(
  modules_attended = list(
    column   = "Which modules did you attend?",
    question = "Which modules did you attend?",
    levels   = c("Module 1", "Module 2", "Module 3", "Module 4"),
    multi    = TRUE          # checkbox question: one respondent, several answers
  ),
  overall_useful = list(
    column   = "Overall, how useful was the series?",
    question = "Overall, how useful was the series?",
    levels   = c("Not at all useful", "Slightly useful", "Moderately useful",
                 "Very useful", "Extremely useful")
  ),
  pace = list(
    column   = "How was the pace?",
    question = "How was the pace?",
    levels   = c("Too slow", "About right", "Too fast")
  ),
  confidence = list(
    # `column` must match the Google Forms CSV header EXACTLY -- including the
    # "anlayses" typo that is live on the form. `question` is what the page
    # displays, so the typo is corrected for readers. If the form is fixed,
    # update `column` only.
    column   = "How confident do you feel with using R for statistical anlayses?",
    question = "How confident do you feel using R for statistical analyses?",
    levels   = c("Not at all confident", "Slightly confident",
                 "Moderately confident", "Very confident",
                 "Extremely confident")
  )
  # NOTE: "Would you recommend it to a colleague?" was in the suggested set but
  # is not on the live form. If it is added later, restore an entry here with
  # levels c("No", "Maybe", "Yes").
)

# content/feedback.qmd builds ONE factor of response labels shared across all
# questions, ordered by `order`. That only works if a label never carries two
# different positions. Catch a violation here rather than letting it silently
# scramble the chart.
local({
  pos <- do.call(rbind, lapply(names(PUBLISHED), function(id) {
    data.frame(response = PUBLISHED[[id]]$levels,
               order    = seq_along(PUBLISHED[[id]]$levels),
               stringsAsFactors = FALSE)
  }))
  bad <- names(which(tapply(pos$order, pos$response,
                            function(x) length(unique(x))) > 1))
  if (length(bad) > 0) {
    stop("Response label(s) used at different positions by different ",
         "questions: ", paste(sQuote(bad), collapse = ", "), ".\n",
         "  The page orders all questions with one shared factor, so a label\n",
         "  must sit at the same position everywhere. Rename one of them.",
         call. = FALSE)
  }
})

# --- Input -----------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
raw_path <- if (length(args) >= 1) args[1] else "dev/feedback/responses.csv"

if (!file.exists(raw_path)) {
  stop(
    "No raw export at '", raw_path, "'.\n",
    "  Download the responses from the survey and save them under dev/\n",
    "  (gitignored). Never put raw responses in data/.",
    call. = FALSE
  )
}
if (!startsWith(normalizePath(raw_path, mustWork = TRUE),
                normalizePath("dev", mustWork = FALSE))) {
  stop("Raw responses must be read from dev/, which is gitignored. ",
       "Got: ", raw_path, call. = FALSE)
}

raw <- readr::read_csv(raw_path, show_col_types = FALSE)
n_respondents <- nrow(raw)

if (n_respondents == 0) {
  stop("The export at '", raw_path, "' has no rows.", call. = FALSE)
}

# --- Aggregate -------------------------------------------------------------
summarise_one <- function(id, spec) {
  if (!spec$column %in% names(raw)) {
    stop(
      "Column not found in the export: ", sQuote(spec$column), ".\n",
      "  The export has: ", paste(sQuote(names(raw)), collapse = ", "), "\n",
      "  Google Forms names each CSV column after the question's CURRENT\n",
      "  wording, so this usually means the question was reworded or removed.\n",
      "  Fix the `column` string in PUBLISHED to match, or drop the entry.\n",
      "  (This is an error, not a warning, so a published question cannot\n",
      "  silently vanish from the page.)",
      call. = FALSE
    )
  }

  answers <- raw[[spec$column]]

  # Checkbox questions arrive as "Module 1, Module 2" in a single cell.
  if (isTRUE(spec$multi)) {
    answers <- unlist(strsplit(as.character(answers), "\\s*,\\s*"))
  }

  answers <- trimws(as.character(answers))
  answers <- answers[!is.na(answers) & nzchar(answers)]

  unexpected <- setdiff(unique(answers), spec$levels)
  if (length(unexpected) > 0) {
    stop(
      "Unexpected answer(s) for '", spec$question, "': ",
      paste(sQuote(unexpected), collapse = ", "), ".\n",
      "  Every answer must be one of the declared `levels`. This guard is\n",
      "  what stops a free-text answer being published by accident -- fix the\n",
      "  levels above to match the survey, do not remove this check.",
      call. = FALSE
    )
  }

  # Options with zero responses are kept, so the chart shows the full scale.
  tibble::tibble(
    question_id = id,
    question    = spec$question,
    response    = spec$levels,
    order       = seq_along(spec$levels)
  ) |>
    left_join(
      tibble::tibble(response = answers) |> count(response, name = "n"),
      by = "response"
    ) |>
    mutate(n = tidyr::replace_na(n, 0L))
}

summary_tbl <- purrr::list_rbind(
  lapply(names(PUBLISHED), function(id) summarise_one(id, PUBLISHED[[id]]))
)

if (is.null(summary_tbl) || nrow(summary_tbl) == 0) {
  stop("Nothing to publish -- none of the expected columns were found.",
       call. = FALSE)
}

# --- Self-check ------------------------------------------------------------
# Last line of defence before anything lands in a tracked, published folder.
check_output <- function(tbl) {
  flat <- unlist(lapply(tbl, as.character))
  flat <- flat[!is.na(flat)]

  if (any(grepl("@", flat, fixed = TRUE))) {
    stop("Refusing to write: an '@' (email address?) reached the output.",
         call. = FALSE)
  }
  # Any published value must be a declared level, a question, or an id.
  allowed <- c(
    unlist(lapply(PUBLISHED, `[[`, "levels")),
    vapply(PUBLISHED, `[[`, character(1), "question"),
    names(PUBLISHED),
    as.character(tbl$n), as.character(tbl$order)
  )
  leaked <- setdiff(flat, allowed)
  if (length(leaked) > 0) {
    stop("Refusing to write: value(s) not on the allow-list reached the ",
         "output: ", paste(sQuote(head(leaked, 5)), collapse = ", "),
         call. = FALSE)
  }
  # Long strings are the signature of a free-text answer.
  if (any(nchar(flat) > 120)) {
    stop("Refusing to write: a value longer than 120 characters reached the ",
         "output (free-text answer?).", call. = FALSE)
  }
  invisible(TRUE)
}

check_output(summary_tbl)

# --- Write -----------------------------------------------------------------
readr::write_csv(summary_tbl, "data/feedback-summary.csv")
readr::write_csv(
  tibble::tibble(
    n_respondents = n_respondents,
    last_updated  = format(Sys.Date(), "%Y-%m-%d")
  ),
  "data/feedback-meta.csv"
)

message("Wrote data/feedback-summary.csv (", nrow(summary_tbl), " rows) and ",
        "data/feedback-meta.csv from ", n_respondents, " responses.")
message("Check `git diff` before committing -- these files are published.")
