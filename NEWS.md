# essurvey2 0.1.0

First release. A client for the current European Social Survey API, replacing the
[`essurvey`](https://github.com/ropensci/essurvey) package, whose scraped
endpoints no longer exist.

## Downloading

* `ess_round()` downloads one or more rounds as a `data.table`, resolving each
  round's data file DOI from the live catalogue.
* `ess_all_rounds()` fetches every published round, stacked.
* `ess_country()` returns one country across rounds.
* `ess_data()` and `ess_download_file()` fetch any data file by DOI.
* `ess_read_file()` re-reads a downloaded file without touching the network.
* Parquet is the default transport, read with **arrow**; `csv`, `sav` and `dta`
  are also supported. `select` decodes only the requested columns.

## Finding data

None of these need a user ID, and none transfer survey data.

* `ess_rounds()`, `ess_data_files()`, `ess_data_file_info()`
* `ess_countries()`, `ess_country_rounds()`
* `ess_themes()`, `ess_theme_rounds()`
* `ess_variables()`, `ess_variable_info()`
* `ess_series()`, `ess_series_id()`, `ess_file_kinds()`
* `ess_gql()` for anything else in the catalogue's GraphQL schema.

`ess_data_files()` covers all 64 published data files, not just the integrated
ones: the interviewer questionnaire, contact forms, self-completion files, MTMM
test variables, alcohol and media-claims modules, interview timing, sample design
data (SDDF), and the supplementary files for countries held out of an integrated
file. It warns when the catalogue gives two files the same DOI, which currently
happens in round 8.

## Missing values and labels

* `recode_missings = TRUE` (the default) has the API return `NA` for values
  flagged as missing.
* `ess_recode_missings()` does the same locally, using each variable's own
  missing codes rather than assuming a single convention.
* `ess_value_labels()` and `ess_as_factor()` apply the value labels, which
  parquet files do not carry.
* `ess_missing_codes()` reports which codes count as missing.

## Configuration and caching

* `ess_set_user_id()`, `ess_user_id()`, `ess_config()`. A startup message
  explains how to set `ESS_USER_ID` when it is unset, and stays quiet once it is.
* Downloads are cached between sessions: `ess_cache_dir()`, `ess_cache_list()`,
  `ess_cache_clear()`, and `use_cache` on the download functions.
* Catalogue answers are memoised per session; `ess_catalogue_refresh()` forgets
  them.

## Vignettes

`vignette("essurvey2")` covers configuration, discovery and downloading. Six
further vignettes work through a research task each, and are organised around what
that task has to get right rather than around the package's functions:

* `trust-trends` — institutional trust across all 11 rounds: weight choice,
  an item absent from round 1, and country composition in a European average.
* `harmonising-variables` — one comparable education measure when `edulvla`
  became `edulvlb`, and why `eisced` codes 0 and 55 need removing by hand.
* `health-inequality` — the educational gradient in self-rated health in rounds
  7 and 11, where `health` runs good-to-bad and income is a within-country decile.
* `design-based-estimates` — standard errors from `survey::svydesign()`, and
  where the design variables are for each round.
* `immigration-attitudes` — an index over the six core items, which mix two
  response formats, two directions and two missing-code conventions.
* `reproducible-provenance` — recording the editions and DOIs behind a result.

**survey** is a new `Suggests` dependency, used by `design-based-estimates`.

## Errors

API failures are raised as classed conditions — `essurvey2_error_user_id`,
`essurvey2_error_doi`, `essurvey2_error_server`, `essurvey2_error_catalogue`,
`essurvey2_error_offline` — and carry the API's request ID for support enquiries.
