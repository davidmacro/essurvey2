# essurvey2

An R client for the European Social Survey API at `api.ess.sikt.no`, with metadata
discovery through the ESS GraphQL catalogue at `api.nsd.no`. Returns
`data.table`s. Replaces the retired `essurvey` package.

## Commit contract

- **Autonomy:** autonomous — commit finished, gate-passing work without asking
- **Target:** direct-to-default — straight to `main`, no PR (solo repo, remote is
  self-hosted Gitea at `dataimsurvey.nl`)
- **Push:** always — same turn as the commit
- **Granularity:** one commit per completed unit of work: a vibeissues issue, or
  one coherent change with its docs and tests
- **Gate:** all four must pass before committing —
  1. `pkgload::load_all()` succeeds
  2. `testthat::test_local()` reports **0 failures and 0 errors**
  3. roxygen regenerated if any roxygen comment changed
  4. vignettes knit and parse, if any were touched

The `vibeissues` skill's commit-on-close rule applies here and agrees with the
above. Board project slug is `essurvey2`.

## Verification recipes

These are not obvious from the repo, and each cost time to work out.

**Tests: skips are expected, failures are not.** Two different tallies are both
correct, so don't read the difference as a regression:

| How you run it | Result |
|---|---|
| `testthat::test_local()` | everything runs; only tests needing `ESS_USER_ID` skip |
| `R CMD check --as-cran` | **248 pass, 27 skip**, because `skip_on_cran()` also fires |

The 27 are all of `test-live.R`. Skips are not a gate failure and are not
something to "fix" — do not try to make them pass or delete them.

**Pandoc and pdflatex exist, but not on `PATH`.** `which pandoc` finds nothing,
which makes it look as though vignettes cannot be built here. They can. Both
binaries ship with tools that are already installed:

```sh
export RSTUDIO_PANDOC="/c/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
export PATH="$RSTUDIO_PANDOC:/c/Users/Gebruiker/AppData/Roaming/TinyTeX/bin/windows:$PATH"
```

With those set, `R CMD build` reports `creating vignettes ... OK` for all seven
and a full `R CMD check --as-cran` runs clean. Two traps:

* `RSTUDIO_PANDOC` alone is enough for `R CMD build`, but **not** for the check
  step that validates `README.md` and `NEWS.md` — that one wants `pandoc` on
  `PATH`, and without it you get a NOTE and the README's URLs go unchecked.
* Without `pdflatex`, the PDF-manual check fails with an ERROR *and* a WARNING
  reading "LaTeX errors ... typically indicates Rd problems". That wording is
  misleading: the only error is the missing binary. TinyTeX supplies it.

Still worth running the parse step on a touched vignette, because it catches
syntax errors inside `eval = FALSE` chunks that rendering alone will not:

```r
knitr::knit(f, output = tempfile(fileext = ".md"))   # renders
rfile <- tempfile(fileext = ".R")
knitr::purl(f, output = rfile); parse(rfile)          # every chunk parses
```

**Full CRAN check.** `R CMD build` then `R CMD check --as-cran` on the tarball,
with the two exports above. Budget ~5 minutes: the incoming-feasibility step
alone takes about 230s because it queries CRAN and checks every URL.

**The catalogue needs no user ID.** `ess_rounds()`, `ess_variables()`,
`ess_variable_info()`, `ess_data_files()`, `ess_themes()` and friends work
without `ESS_USER_ID` — only survey-data *downloads* require it. So any claim
about variable names, value labels, missing codes, rounds, editions or DOIs can
and should be verified live rather than assumed.

**Write R to a file, don't pipe it through `-e`.** Multi-line `Rscript -e '...'`
strings get mangled by the shell on this machine and silently produce no output.
Put the script in the scratchpad directory and run `Rscript path/to/file.R`.

## Documentation conventions

**Vignettes must not contain invented output.** The convention across all seven
vignettes: chunks are `eval = FALSE` because downloads need a user ID, catalogue
output pasted in as `#>` comments has been **executed live and matches**, and
data-dependent chunks carry **no pasted numbers at all**. ESS editions get
revised, so a pasted result would quietly become wrong; each vignette says so in
its preamble. When editing a vignette, re-run its catalogue snippets rather than
trusting the existing comments — `dcast()` row order in particular is keyed and
alphabetical, which is easy to get wrong by hand.

**Nothing is hard-coded.** Rounds, DOIs and editions are resolved from the live
catalogue so a new round or edition needs no package update. Do not introduce a
hard-coded DOI, round list or edition anywhere, including in tests and examples.

**Examples in roxygen are not checked.** They live in `\dontrun{}`, so `R CMD
check` never runs them and a wrong one can sit there indefinitely — round 11 was
documented as having a self-completion file it does not publish. Verify example
calls against the catalogue.

## Known upstream quirks

Both are the ESS catalogue's, not this package's. Report to
`essdatasupport@sikt.no`.

* **Round 7's alcohol file DOI arrives malformed:** `ess_data_files()` returns
  `" https://doi.org/10.21338/ess7alce02"` — leading space, URL form — where the
  other 63 files return a bare `10.21338/...`. Downloads cope, because
  `ess_parse_doi()` trims and strips the prefix; exact-match filters and joins on
  the `doi` column do not. Open board issue **#1044**.
* **Round 8 gives two data files one DOI** (`ESS8TIMEe01` and `ESS8CFe03`).
  `ess_data_files()` warns rather than guessing which is meant. Board issue
  **#1027**.
