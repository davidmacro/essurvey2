# essurvey2

<!-- badges: start -->
<!-- badges: end -->

Download data from the [European Social Survey](https://www.europeansocialsurvey.org/)
into R, over the current ESS API.

The ESS now serves data through a documented API at
[api.ess.sikt.no](https://api.ess.sikt.no/docs), with
[Apache Parquet](https://parquet.apache.org/) as its default format. `essurvey2`
wraps that API, discovers what is published through the ESS metadata catalogue,
and returns [`data.table`](https://rdatatable.gitlab.io/data.table/) objects.

It is a replacement for the [`essurvey`](https://github.com/ropensci/essurvey)
package, which scraped the ESS website. Those endpoints are gone, so `essurvey`
no longer works.

## Installation

```r
install.packages("essurvey2")
```

## Configuration

Every data download must carry an ESS **user ID**. It is not a password: the ESS
uses it to record download statistics.

1. Get yours by logging in at <https://ess.sikt.no/en/api>.
2. Store it in your user `.Renviron` so that every session picks it up —
   `usethis::edit_r_environ()` opens the file:

   ```
   ESS_USER_ID=your-user-id-here
   ```

3. Restart R. `.Renviron` is only read at startup.

A system environment variable of the same name works too, and is the better
choice on a shared or containerised machine. For a single session you can also
call `ess_set_user_id("...")`.

`library(essurvey2)` warns you if no ID is configured, and stays quiet once one
is. `ess_config()` shows what the package has resolved:

```r
library(essurvey2)
ess_config()
#> ── essurvey2 configuration
#> data API:  <https://api.ess.sikt.no>
#> catalogue: <https://api.nsd.no/graphql>
#> user ID:   "a1b2c3d4****************************" (from ESS_USER_ID)
#> cache:     'C:/Users/you/AppData/Local/R/cache/R/essurvey2'
```

The catalogue functions — `ess_rounds()`, `ess_countries()`, `ess_variables()`
and friends — need no user ID at all.

## Quick start

```r
library(essurvey2)

# What is published? No survey data is downloaded here.
ess_rounds()
#>     round                                       description file_label edition ...
#>  1:     1  ESS round 1 - 2002. Immigration, Citizen involv…  ESS1e06_7     6.7
#> ...
#> 11:    11  ESS round 11 - 2023. Social inequalities in hea… ESS11e04_2     4.2

# One round, a few variables. Downloading only the columns you need is much
# faster than pulling all 700-odd of them.
dt <- ess_round(11, select = c("essround", "cntry", "agea", "gndr", "ppltrst"))
dt[, .(n = .N, trust = round(mean(ppltrst, na.rm = TRUE), 2)), by = cntry][order(-trust)]

# Several rounds, stacked and distinguishable by essround.
trend <- ess_round(9:11, select = c("essround", "cntry", "ppltrst", "pspwght"))
trend[, .(trust = weighted.mean(ppltrst, pspwght, na.rm = TRUE)), by = essround]

# One country across every round it took part in.
nl <- ess_country("NL", select = c("essround", "agea", "ppltrst"))
```

Missing values — "Refusal", "Don't know", "Not applicable" and so on — arrive as
`NA` by default, because the API is asked to recode them. Pass
`recode_missings = FALSE` to keep the original codes.

## Finding your way around

Nothing in this group transfers survey data.

| Function | What it lists |
|---|---|
| `ess_rounds()` | published rounds and each one's main data file |
| `ess_data_files()` | every data file in a round, with its DOI |
| `ess_data_file_info()` | a data file's DOI, coverage and variable count |
| `ess_countries()` | countries covered, with ISO-2 codes |
| `ess_country_rounds()` | which rounds a country is in |
| `ess_themes()` | questionnaire themes, core ones flagged |
| `ess_theme_rounds()` | which rounds carry a theme |
| `ess_variables()` | a data file's variables and their labels |
| `ess_variable_info()` | one variable's question wording and value labels |
| `ess_series()` | ESS, CRONOS, ICOS Cities and the other series |

```r
# Find the trust variables, then read only those.
vars <- ess_variables(11)
vars[grepl("trust", variable_label, ignore.case = TRUE), .(variable_name, variable_label)]

# What does a code mean?
ess_variable_info("ppltrst", round = 11)$codes
#>     value                      label is_missing
#>  1:     0   You can't be too careful      FALSE
#> ...
#> 12:    77                    Refusal       TRUE
#> 13:    88                 Don't know       TRUE
#> 14:    99                  No answer       TRUE
```

## Beyond the integrated file

The ESS publishes 64 data files across its 11 rounds, not 11.
`ess_file_kinds()` lists what the names mean, and `ess_data_files()` shows which
of them a round has.

```r
ess_data_files(11)[, .(file_kind, file_label, doi, n_variables)]
#>        file_kind    file_label                    doi n_variables
#> 1:       alcohol ESS11ALCe02_1 10.21338/ess11alce02_1         753
#> 2: contact_forms  ESS11CFe04_1  10.21338/ess11cfe04_1         247
#> 3:    integrated    ESS11e04_2    10.21338/ess11e04_2         709
#> 4:   interviewer ESS11INTe04_1 10.21338/ess11inte04_1          25

# Round 10 onwards has a separate self-completion integrated file.
sc <- ess_round(10, kind = "self_completion")

# Or fetch any data file straight by DOI.
cf <- ess_data("10.21338/ess11cfe04_1")
```

Two kinds are worth knowing about. **Sample design data** carries the design
weights and clustering variables — the `import_sddf_country()` of the old
package:

```r
ess_data_files(kind = "sample_design")[, .(round, file_label, doi)]
sddf <- ess_data("10.21338/ess7sddfe1_2")
sddf[, .(cntry, idno, psu, stratum, prob)]
```

And some countries are held out of an integrated file — because their fieldwork
ran late, or they have no design weights — and published separately:

```r
ess_data_files(kind = "country")[, .(round, file_country, description)]
#>    round file_country                                              description
#> 1:     2           IT        ESS2 - Italy country file from main questionnaire
#> 2:     3           LV                            ESS3 - Latvia (no design weights)
#> ...
#> 6:     5           AT  ESS5 - Austria (fieldwork period 24.05.13 to 10.10.13)

at <- ess_data("10.21338/ess5ate1_1")
```

These are the reason `ess_country("AT")` does not see round 5 Austria: that data
is not in the round 5 integrated file at all.

## Caching

Downloads can be kept on disk, so repeating a call — now or in a later session
— costs nothing.

The default location is `tools::R_user_dir("essurvey2", "cache")`, which is in
your own filespace, so the package asks before it writes there. It asks once,
the first time a download would be cached in an interactive session, and
remembers the answer. Declining is not fatal: downloads then go to the session's
temporary directory instead.

Outside an interactive session — `Rscript`, a knitted document, a CI job —
there is nobody to ask, so nothing is written to your filespace and downloads go
to the temporary directory. Turn persistent caching on explicitly there:

```r
options(essurvey2.cache_consent = TRUE)        # or ESSURVEY2_CACHE_CONSENT=true
options(essurvey2.cache_dir = "D:/data/ess-cache")  # naming a place implies consent
```

The cache is capped at 1 GB and trimmed on every write, oldest first, so it
cannot grow without bound.

```r
ess_cache_list()      # what is cached, and how big
ess_cache_dir()       # where it lives (never creates or prompts)
ess_cache_clear()     # start again

options(essurvey2.cache_max_size = 250 * 1024^2)  # a smaller cap
options(essurvey2.cache_max_size = FALSE)         # no cap
options(essurvey2.cache_dir = FALSE)              # switch caching off entirely
```

## Worked analyses

Six vignettes each take one research task from question to result, and each is
built around the traps that task runs into. None of them invent numbers: the
catalogue output is real, and the parts that need a download are shown as code so
that a revised edition cannot make the text wrong.

| Vignette | The task, and what it has to get right |
|---|---|
| `vignette("essurvey2")` | Getting started: configuration, discovery, downloading, missing values, labels |
| `vignette("trust-trends")` | Institutional trust across all 11 rounds. Choosing `anweight`; an item missing from round 1; a European average that changes when its countries do |
| `vignette("harmonising-variables")` | One comparable education measure. `edulvla` became `edulvlb`; `eisced` codes 0 and 55 are not flagged missing but are not education levels |
| `vignette("health-inequality")` | The educational gradient in self-rated health, rounds 7 and 11. `health` runs good-to-bad; `hinctnta` is a within-country decile |
| `vignette("design-based-estimates")` | Confidence intervals that respect the clustered sample. Design variables are in the file from round 9, in SDDF files for 7–8, and absent before that |
| `vignette("immigration-attitudes")` | An index over the six core items. Two response formats, two directions, two missing-code conventions |
| `vignette("reproducible-provenance")` | Recording which editions and DOIs produced a result, so a failure to reproduce is diagnosable |

## Differences from the essurvey package

Two of these are visible in the interface, and are consequences of how the API
works rather than choices this package made.

* **No per-country downloads.** The API publishes integrated files only, so
  `ess_country()` downloads a whole round and filters it on `cntry` locally. The
  cache and `select` keep this cheap in practice.
* **Codes, not labels.** Parquet columns hold plain integers and strings. Value
  labels live in the metadata catalogue: see `ess_value_labels()`,
  `ess_variable_info()`, and `ess_as_factor()` to apply them.
* **`data.table` throughout**, rather than tibbles.
* **Nothing is hard-coded.** Rounds, DOIs and file editions are resolved from
  the live catalogue, so a newly published round or a new edition is picked up
  without a package update. This matters: editions change, and a DOI baked into
  a script goes stale.
* **A user ID replaces the email address** that `essurvey` registered.

Rough equivalents:

| `essurvey` | `essurvey2` |
|---|---|
| `set_email()` | `ess_set_user_id()` |
| `import_rounds()` | `ess_round()` |
| `import_all_rounds()` | `ess_all_rounds()` |
| `import_country()` | `ess_country()` |
| `show_rounds()` | `ess_rounds()` |
| `show_countries()` | `ess_countries()` |
| `show_themes()` | `ess_themes()` |
| `show_theme_rounds()` | `ess_theme_rounds()` |
| `import_sddf_country()` | `ess_data_files(kind = "sample_design")` + `ess_data()` |
| `recode_missings()` | `ess_recode_missings()` |

## Going further

The metadata catalogue is a GraphQL endpoint with considerably more in it than
this package wraps. `ess_gql()` sends arbitrary queries, and the schema is
introspectable.

## Citing the data

The ESS asks that you cite the data files you use. `ess_data_file_info()`
returns each file's DOI and publication date for that purpose.

## Notes

The ESS API is in beta. It cannot serve the very largest data files; those still
have to be downloaded from the portal. Feedback on the API itself goes to
<essdatasupport@sikt.no>.

`ess_data_files()` warns if the catalogue gives two data files the same DOI,
which currently happens for round 8's contact forms and interview time data.
Downloading such a DOI returns one file, not necessarily the one whose label you
picked. This package reports the inconsistency rather than guessing which is
right.

## Licence

MIT. The survey data is licensed separately by the ESS ERIC — see
<https://ess.sikt.no>.
