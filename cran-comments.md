## Test environments

* local: Windows 11 x64 (build 26200), R 4.5.2 (2025-10-31 ucrt), x86_64-w64-mingw32
* GitHub Actions (`r-lib/actions`, `--as-cran`):
  * ubuntu-latest, R-devel
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
  * macos-latest, R release
  * windows-latest, R release
* win-builder: R-devel  <!-- TODO: fill in once run -->

## R CMD check results

0 errors | 0 warnings | 1 note

The note is:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'David Macro <david@dataim.nl>'

New submission
```

This is a new submission, so the note is expected.

## Comments

This is a new submission.

### What the package does

`essurvey2` is a client for the European Social Survey's data API at
<https://api.ess.sikt.no>, discovering what is published through the ESS
metadata catalogue at <https://api.nsd.no/graphql>. It replaces the retired
`essurvey` package, whose scraped download endpoints no longer exist.

Source and issue tracker: <https://github.com/davidmacro/essurvey2>.

The `Description` field names the API host in angle brackets rather than citing
a publication, because the package documents a service rather than a method.
The ESS asks that users cite the individual data files they download, and
`ess_data_file_info()` returns each file's DOI and publication date for that
purpose.

### Writing to the file system

The package writes only to the session's temporary directory unless the user
has agreed otherwise.

Downloaded data files may be cached between sessions, and the default location
for that is `tools::R_user_dir("essurvey2", "cache")`. Because that is in the
user's own filespace, nothing is written there until the user says it may be:

* In an interactive session the package names the directory and asks, once. The
  answer is recorded by a marker file inside the cache directory itself, so it
  persists without the package writing anywhere else.
* In a non-interactive session — `Rscript`, a knitted document, a CI job, and
  the CRAN check farm — there is nobody to ask, so nothing is written to the
  home filespace at all and downloads go to `tempfile()` instead.
* Consent can be given without a prompt by setting
  `options(essurvey2.cache_consent = TRUE)` or the `ESSURVEY2_CACHE_CONSENT`
  environment variable, and naming a directory with
  `options(essurvey2.cache_dir = "...")` counts as consent in its own right.
* `options(essurvey2.cache_dir = FALSE)` disables caching entirely.

The cache is actively managed rather than left to grow: it is capped
(`options(essurvey2.cache_max_size)`, 1 GB by default) and trimmed on every
write, deleting the least recently modified files until it fits.
`ess_cache_list()` reports what is held and `ess_cache_clear()` empties it.

### Internet access, and why the examples use `\dontrun{}`

Every function that reaches the network fails gracefully with an informative,
classed condition when the resource is unavailable — `essurvey2_error_offline`
when the host cannot be reached, `essurvey2_error_catalogue` and
`essurvey2_error_server` when it answers with an error.

No example, test or vignette contacts the network during `R CMD check`:

* **Vignettes.** All seven set `eval = FALSE`, so nothing is executed at build
  time. Each says so in its preamble.
* **Tests.** The tests that need the network or a user ID are guarded with
  `skip_on_cran()` and `skip_if_offline()`. Under `--as-cran` the suite reports
  248 passing and 27 skipped, in about 6 seconds.
* **Examples.** Every example that would reach the network is wrapped in
  `\dontrun{}`. The examples that do run build strings, read options or list
  static tables, and touch neither the network nor the file system.

On that last point, we would rather explain the choice than have it queried:
`\donttest{}` would be worse here, not better, because `\donttest{}` examples
*are* executed on CRAN.

* The download examples need an ESS API user ID, which the check farm has no
  way to hold. `ess_user_id()` aborts without one, so under `\donttest{}` these
  examples would be guaranteed errors rather than skipped ones.
* The catalogue examples need no user ID, but they do need `api.nsd.no`. They
  abort when it is unreachable, which is correct behaviour for a user and wrong
  behaviour for a check: under `\donttest{}` any upstream outage or rate limit
  would turn into a check ERROR on a package that is otherwise fine. The API is
  also still in beta and hosted by a third party we do not control.

`\dontrun{}` therefore reflects a genuine "this cannot be executed here", which
is what the flag is for. The functions are exercised instead by the test suite,
which runs against the live services when `ESS_USER_ID` is set and network
access is available, and skips cleanly when they are not.

### Other

* The package starts no external software and sends nothing about the R session
  anywhere.
* It uses no more than one thread.
* `haven` and `survey` are in `Suggests` and used conditionally —
  `rlang::check_installed()` in the case of `haven`, which is needed only to
  read the optional `sav` and `dta` transfer formats.
