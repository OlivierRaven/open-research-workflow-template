# Generates evidence.json: a manifest linking every crossref-citable
# fig-/tbl- claim in the manuscript to the exact chunk that produced it,
# the data it reads, and the outputs it writes.
#
# Join key: the Quarto cross-reference label (e.g. "tbl-rf") is the same
# string in both the code and the prose, so no new annotation scheme is
# needed beyond what Quarto authors already write.
#
# Generalized (originally built against one repo, since proven against a
# second with a meaningfully different structure — a manuscript-type
# project with two notebooks, variable-argument file reads, and inline
# chunks living in the manuscript file itself, not just the notebook):
#
# - File discovery reads _quarto.yml's manuscript.article/manuscript.notebooks
#   instead of assuming filenames. Falls back to analysis.qmd/index.qmd if
#   no manuscript: block exists (e.g. a `project: type: default` layout).
# - Chunks are indexed across every notebook file AND the manuscript file
#   itself, in that order (notebooks are conceptually upstream of the
#   manuscript that displays their results) — a manuscript file can and
#   often does have its own labelled display chunks.
# - {{< include other.qmd >}} in the manuscript is followed so citations
#   inside an included file (e.g. a separate supplementary.qmd) are seen.
# - repo owner/name comes from `git remote get-url origin`; the article DOI
#   is a best-effort scan of README.md (first non-Zenodo DOI found) — both
#   are optional; the manifest just omits what it can't find rather than
#   requiring it.
# - Writes are recognized under two conventions, since proven against a
#   third real repo (a minimal `project: type: default` template) that used
#   neither of the first two: `file.path(out_dir, "...")` (trout/kōura's
#   convention) and literal-path calls — saveRDS(x, "path.rds"),
#   write_csv/write.csv(x, "path.csv"), ggsave("path.png", plot) — regardless
#   of whether the path is the first or a later argument.
#
# Many claim chunks are thin display wrappers (e.g. `x |> knitr::kable()`)
# whose actual computation — and actual file reads/writes — happened in an
# earlier chunk. This script traces backward through variable assignments
# to find that source and folds its lineage in too, recording which
# chunk(s) it came from as `computedIn` rather than silently merging with
# no attribution. Chunks that are ancestors of most claims (the shared
# data-loading pipeline) are collapsed into `dataPipeline` instead — named
# and linked, but without code, since it's not what makes any one claim's
# number what it is.
#
# Known limitations, not silently papered over:
# - R only (```{r} chunks). Python/Julia chunks aren't parsed.
# - Static text analysis, not real data-flow analysis — only bare
#   top-level `var <- ...` assignments and whole-word variable mentions are
#   followed. Variables built inside loops with dynamic paths (e.g.
#   read.delim(file_path) in a loop) aren't traceable this way.
# - Two systematic false-positive sources were found and fixed against a
#   second real repo: short generic names (c, n, x, tbl...) reused as loop
#   counters/temporaries across unrelated chunks, and named function
#   arguments (predict(model, type = "response")) colliding with a
#   same-named variable defined elsewhere. What's left after both fixes is
#   genuinely ambiguous, not another bug: a short, natural domain word
#   (e.g. "lakes" in an ecology paper) independently reused as a variable
#   name in two unrelated chunks looks identical, in plain text, to a real
#   shared dependency. Resolving that needs actual scope-aware parsing,
#   which is out of scope for a static regex-based tool — noted here
#   rather than chased with increasingly narrow rules that risk breaking
#   legitimate matches elsewhere.
#
# Run this after a successful `quarto render` — generatedAt is treated
# downstream as "last verified reproducible".

if (!requireNamespace("here", quietly = TRUE)) stop("here package required")
if (!requireNamespace("yaml", quietly = TRUE)) stop("yaml package required")
repo_root <- here::here()

read_text <- function(filename) {
  path <- file.path(repo_root, filename)
  if (!file.exists(path)) return(NA_character_)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# ---- repo owner/name from git, not hardcoded ----
remote_url <- tryCatch(trimws(system("git remote get-url origin", intern = TRUE)), error = function(e) NA_character_)
github_repo <- if (length(remote_url) && nzchar(remote_url) && !is.na(remote_url)) {
  gsub("^.*github\\.com[:/]|\\.git$", "", remote_url)
} else NA_character_

git_ref <- tryCatch(
  trimws(system("git rev-parse HEAD", intern = TRUE)),
  error = function(e) "main"
)

# ---- article DOI: best-effort scan of README.md, not hardcoded ----
paper_doi <- NA_character_
readme_text <- read_text("README.md")
if (!is.na(readme_text)) {
  # Excludes ] and [ too, not just whitespace/quotes/parens/angle-brackets —
  # a bare DOI as markdown link text, e.g. "[10.xxx/yyy](https://doi.org/...)",
  # otherwise matches straight through the closing "]" into the URL that
  # follows it.
  all_dois <- regmatches(readme_text, gregexpr('10\\.[0-9]{4,9}/[^\\s")\\[\\]>]+', readme_text, perl = TRUE))[[1]]
  all_dois <- gsub("[>)\\.,;]+$", "", all_dois)
  # Prefer the article DOI over a Zenodo code/data-archive DOI when a repo's
  # README lists both (common — see the data availability section pattern).
  non_zenodo <- all_dois[!grepl("^10\\.5281/zenodo", all_dois)]
  if (length(non_zenodo)) paper_doi <- non_zenodo[1] else if (length(all_dois)) paper_doi <- all_dois[1]
}

# ---- discover the manuscript file + notebook file(s) from _quarto.yml ----
qcfg <- tryCatch(yaml::read_yaml(file.path(repo_root, "_quarto.yml")), error = function(e) list())
manuscript_cfg <- qcfg$manuscript

article_file <- if (!is.null(manuscript_cfg$article)) manuscript_cfg$article else "index.qmd"
notebook_files <- if (!is.null(manuscript_cfg$notebooks)) {
  vapply(manuscript_cfg$notebooks, function(x) x$notebook, character(1))
} else if (file.exists(file.path(repo_root, "analysis.qmd"))) {
  "analysis.qmd"  # fallback for projects without a manuscript: block, e.g. type: default
} else {
  character(0)
}
all_source_files <- c(notebook_files, article_file)

# ---- parse every ```{r}...``` chunk across every notebook + the manuscript ----
parse_chunks_in_file <- function(filename) {
  path <- file.path(repo_root, filename)
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  chunk_starts <- grep('^```\\{r', lines)
  out <- list()
  for (s in chunk_starts) {
    e <- s
    while (e < length(lines) && lines[e] != "```") e <- e + 1
    body <- paste(lines[s:e], collapse = "\n")
    label_match <- regmatches(body, regexpr('#\\|\\s*label:\\s*[^\n]+', body))
    label <- if (length(label_match)) trimws(sub('#\\|\\s*label:\\s*', "", label_match)) else NA_character_
    # Only `<-` counts as a real top-level assignment. A bare `=` at the
    # start of a trimmed line is just as often a named function argument
    # written on its own line inside a multi-line pipe as a real
    # assignment (produced false "variable" matches like "data"/"rownames").
    own_vars <- character(0)
    for (ln in lines[s:e]) {
      m <- regmatches(ln, regexpr('^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-', ln, perl = TRUE))
      if (length(m) && nzchar(m)) {
        v <- sub('^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-.*', "\\1", m, perl = TRUE)
        own_vars <- c(own_vars, v)
      }
    }
    out[[length(out) + 1]] <- list(
      source_file = filename, start = s, end = e, body = body,
      label = label, own_vars = unique(own_vars)
    )
  }
  out
}

chunks <- list()
for (f in all_source_files) chunks <- c(chunks, parse_chunks_in_file(f))

# ---- manuscript text to scan for @label / {{< embed >}} usage, following includes ----
article_text <- read_text(article_file)
if (is.na(article_text)) article_text <- ""
included <- unique(regmatches(
  article_text, gregexpr('\\{\\{<\\s*include\\s+([a-zA-Z0-9_./-]+\\.qmd)\\s*>\\}\\}', article_text, perl = TRUE)
)[[1]])
included <- gsub('\\{\\{<\\s*include\\s+|\\s*>\\}\\}', "", included)
index_text <- article_text
for (inc in included) {
  t <- read_text(inc)
  if (!is.na(t)) index_text <- paste(index_text, t, sep = "\n")
}
# Drop HTML comments so disabled/commented-out embeds and citations
# (e.g. <!--{{< embed analysis.qmd#fig-x >}}-->) are not treated as live.
index_text <- gsub("<!--.*?-->", "", index_text, perl = TRUE)

# ---- global variable -> defining chunk index map (in chunk order) ----
var_defs <- new.env()
for (i in seq_along(chunks)) {
  for (v in chunks[[i]]$own_vars) {
    var_defs[[v]] <- c(get0(v, envir = var_defs, ifnotfound = integer(0), inherits = FALSE), i)
  }
}

# ---- variable -> literal string value, for resolving e.g. read_excel(path_var, sheet=...) ----
literal_string_defs <- new.env()
for (i in seq_along(chunks)) {
  for (ln in strsplit(chunks[[i]]$body, "\n")[[1]]) {
    parts <- regmatches(ln, regexec('^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*"([^"]+)"\\s*$', ln, perl = TRUE))[[1]]
    if (length(parts) == 3) assign(parts[2], parts[3], envir = literal_string_defs)
  }
}

find_files <- function(body, fn_pattern, ext_pattern) {
  # fn_pattern can itself contain alternation (e.g. "read\\.csv|read_csv") —
  # alternation has lower precedence than concatenation, so pasting it
  # straight into a larger pattern silently breaks apart into unintended
  # alternatives (".*read\\.csv" OR "read_csv\\(...", instead of
  # ".*(?:read\\.csv|read_csv)\\(..."). Group it defensively so every
  # caller doesn't have to remember to.
  fn <- paste0("(?:", fn_pattern, ")")
  m <- gregexpr(paste0(fn, '\\(\\s*"([^"]+', ext_pattern, ')"'), body, perl = TRUE)
  matches <- regmatches(body, m)[[1]]
  unique(gsub(paste0('.*"([^"]+', ext_pattern, ')".*'), "\\1", matches))
}

# read_excel(some_var, sheet = "X") — the path is a variable, not a literal,
# often because one file with several sheets is read multiple times. Only
# resolvable when that variable was assigned a plain literal string
# somewhere in the project; anything more dynamic (built with paste0(),
# looped over a vector of filenames, etc.) is a known gap.
find_variable_arg_reads <- function(body, fn_pattern) {
  fn <- paste0("(?:", fn_pattern, ")")  # see note in find_files() above
  matches <- regmatches(body, gregexpr(
    paste0(fn, '\\(\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*[,)]'), body, perl = TRUE
  ))[[1]]
  if (!length(matches)) return(character(0))
  var_names <- unique(gsub(paste0('.*', fn, '\\(\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*[,)].*'), "\\1", matches, perl = TRUE))
  resolved <- vapply(var_names, function(v) {
    get0(v, envir = literal_string_defs, ifnotfound = NA_character_, inherits = FALSE)
  }, character(1))
  unname(resolved[!is.na(resolved)])
}

find_reads <- function(body) {
  literal <- unique(c(
    find_files(body, "read_excel", "\\.xlsx"),
    find_files(body, "read\\.csv|read_csv", "\\.csv"),
    find_files(body, "readRDS", "\\.rds")
  ))
  literal <- literal[!is.na(literal)]
  var_arg <- unique(c(
    find_variable_arg_reads(body, "read_excel"),
    find_variable_arg_reads(body, "read\\.csv|read_csv"),
    find_variable_arg_reads(body, "readRDS")
  ))
  unique(c(literal, var_arg))
}

find_writes <- function(body) {
  writes <- unique(regmatches(
    body,
    gregexpr('file\\.path\\(out_dir,\\s*"\\s*([^"]+\\.(csv|png|rds))"\\)', body, perl = TRUE)
  )[[1]])
  out_writes <- if (length(writes) == 0) character(0) else
    # paste0("outputs/", character(0)) returns "outputs/" (length 1), not
    # character(0) — R treats a zero-length arg as "" for recycling here,
    # not as "propagate the empty vector" — hence the length check above.
    paste0("outputs/", trimws(gsub('file\\.path\\(out_dir,\\s*"\\s*([^"]+)"\\)', "\\1", writes)))

  # Literal-path writes: saveRDS(x, "path.rds"), write_csv(x, "path.csv"),
  # write.csv(x, "path.csv"), ggsave("path.png", plot). The path is a full
  # literal string (not built from out_dir), and may be the first argument
  # (ggsave) or a later one (saveRDS/write_csv) in a call that often spans
  # several lines (e.g. saveRDS(list(...), "path.rds") with one list element
  # per line). [\\s\\S]*? matches across those newlines non-greedily and
  # stops at the first quoted string with the right extension, regardless of
  # argument position.
  literal_write_fns <- list(
    list(fn = "saveRDS", ext = "\\.rds"),
    list(fn = "write_csv|readr::write_csv|write\\.csv|write\\.table", ext = "\\.csv"),
    list(fn = "ggsave", ext = "\\.(png|pdf|svg|jpg|jpeg|tiff)")
  )
  literal_writes <- unique(unlist(lapply(literal_write_fns, function(spec) {
    fn <- paste0("(?:", spec$fn, ")")  # see note in find_files() above
    matches <- regmatches(body, gregexpr(
      paste0(fn, '\\([\\s\\S]*?"([^"]+', spec$ext, ')"'), body, perl = TRUE
    ))[[1]]
    if (length(matches) == 0) return(character(0))
    # Skip matches that actually reach into a file.path(out_dir, "...") call
    # nested inside this one (e.g. ggsave(x, file = file.path(out_dir,
    # "f.png"))) — that write is already captured, with its outputs/ prefix
    # intact, by the file.path(out_dir, ...) pattern above; without this
    # guard it'd also get picked up here, minus the prefix, as a duplicate.
    matches <- matches[!grepl("file\\.path\\(out_dir", matches, fixed = FALSE)]
    if (length(matches) == 0) return(character(0))
    # (?s) makes `.` match newlines too — these matches can span many lines
    unique(gsub(paste0('(?s).*"([^"]+', spec$ext, ')".*'), "\\1", matches, perl = TRUE))
  })))

  unique(c(out_writes, literal_writes))
}

get_opt <- function(body, name) {
  m <- regmatches(body, regexpr(paste0('#\\|\\s*', name, ':\\s*"([^"]*)"'), body))
  if (length(m) == 0 || m == "") return(NA_character_)
  sub(paste0('#\\|\\s*', name, ':\\s*"([^"]*)"'), "\\1", m)
}

# Strips the ```{r}/``` fences and #| chunk-option lines, leaving just the
# real R code — this is what gets shown inline for a chunk's code, so it
# shouldn't include Quarto's own bookkeeping.
strip_code <- function(body) {
  lines <- strsplit(body, "\n")[[1]]
  lines <- lines[!grepl("^```", lines)]
  lines <- lines[!grepl("^#\\|", lines)]
  while (length(lines) && !nzchar(trimws(lines[1]))) lines <- lines[-1]
  while (length(lines) && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  paste(lines, collapse = "\n")
}

permalink_for <- function(ch) {
  if (is.na(github_repo)) return(NA_character_)
  sprintf("https://github.com/%s/blob/%s/%s#L%d-L%d", github_repo, git_ref, ch$source_file, ch$start, ch$end)
}

# Backward-traces ONE field (reads or writes) when a chunk has none of its
# own — e.g. a display-only chunk that just pipes an earlier variable into
# knitr::kable(). Kept as two independent chases (see resolve_lineage())
# rather than one combined pass: a chunk can have its own correct writes
# but no reads of its own (or vice versa), and merging both fields off a
# single "has either" check pollutes the field that was already complete
# with unrelated ancestors' files.
resolve_field <- function(chunk_idx, extractor, visited = integer(0)) {
  if (chunk_idx %in% visited) return(list(values = character(0), contributed = list()))
  visited <- c(visited, chunk_idx)
  ch <- chunks[[chunk_idx]]
  values <- extractor(ch$body)
  contributed <- list()

  if (length(values) == 0) {
    # Names under 4 chars (c, m, n, r, x, tbl, ...) are excluded — genuine
    # data-holding variables in this kind of analysis code are always
    # multi-character descriptive names; short ones are near-universally
    # loop counters or throwaway temporaries reused by normal R idiom
    # across many unrelated chunks (confirmed against a second real repo:
    # "tbl-lake-overview" was picking up completely unrelated chunks like
    # "GAM-helpers" purely because both happened to define a variable "x").
    all_vars <- ls(var_defs)
    all_vars <- all_vars[nchar(all_vars) >= 4]
    # A word immediately followed by (whitespace then) a single "=" is a
    # named function argument (predict(model, type = "response")), not a
    # reference to a same-named variable defined elsewhere — "type" defined
    # once in an unrelated chunk was matching every predict(..., type=...)
    # call across the whole project otherwise. (?!=) keeps "==" comparisons
    # counting as real usage.
    candidates <- all_vars[
      !(all_vars %in% ch$own_vars) &
      vapply(all_vars, function(v) grepl(paste0("\\b", v, "\\b(?!\\s*=(?!=))"), ch$body, perl = TRUE), logical(1))
    ]
    src_idxs <- integer(0)
    for (v in candidates) {
      defs <- var_defs[[v]]
      earlier <- defs[defs < chunk_idx]
      if (length(earlier)) src_idxs <- c(src_idxs, max(earlier))
    }
    src_idxs <- unique(src_idxs)
    for (si in src_idxs) {
      sub <- resolve_field(si, extractor, visited)
      if (length(sub$values)) {
        values <- union(values, sub$values)
        contributed <- c(contributed, list(list(
          chunkIdx = si, label = chunks[[si]]$label,
          lineStart = chunks[[si]]$start, lineEnd = chunks[[si]]$end
        )))
      }
      contributed <- c(contributed, sub$contributed)
    }
  }

  list(values = values, contributed = contributed)
}

resolve_lineage <- function(chunk_idx) {
  reads_res <- resolve_field(chunk_idx, find_reads)
  writes_res <- resolve_field(chunk_idx, find_writes)
  list(
    reads = reads_res$values,
    writes = writes_res$values,
    contributed = c(reads_res$contributed, writes_res$contributed)
  )
}

# ---- build one entry per fig-/tbl- label actually surfaced in the manuscript ----
entries <- list()

for (chunk_idx in seq_along(chunks)) {
  label <- chunks[[chunk_idx]]$label
  if (is.na(label) || !grepl("^(fig|tbl)-", label)) next

  ch <- chunks[[chunk_idx]]
  body <- ch$body

  tbl_cap <- get_opt(body, "tbl-cap")
  fig_cap <- get_opt(body, "fig-cap")
  caption <- if (!is.na(tbl_cap)) tbl_cap else fig_cap

  # Does the manuscript embed or cite this label? Embeds are checked against
  # every discovered notebook file, not one hardcoded name. Negative
  # lookahead guards against e.g. "tbl-element-selection" matching inside
  # "@tbl-element-selection-static" — a plain \b is not enough here because
  # "-" is a non-word character and still satisfies \b.
  embedded <- any(vapply(notebook_files, function(f) {
    grepl(paste0("\\{\\{<\\s*embed\\s+", gsub("\\.", "\\\\.", f), "#", label, "\\s*>\\}\\}"), index_text)
  }, logical(1)))
  cited <- grepl(paste0("@", label, "(?![A-Za-z0-9_-])"), index_text, perl = TRUE)
  if (!embedded && !cited) next  # not surfaced in the manuscript — skip (see: orphaned labels)

  # best-effort claim sentence: paragraph containing the first @label, split to the sentence with it
  label_boundary <- "(?![A-Za-z0-9_-])"
  claim_text <- NA_character_
  para_match <- regexpr(paste0("[^\n]*@", label, label_boundary, "[^\n]*"), index_text, perl = TRUE)
  if (para_match > 0) {
    para <- regmatches(index_text, para_match)
    sentences <- strsplit(para, "(?<=[.?!])\\s+", perl = TRUE)[[1]]
    hit <- sentences[grepl(paste0("@", label, label_boundary), sentences, perl = TRUE)]
    if (length(hit)) {
      # Strip EVERY fig-/tbl- citation in the sentence, not just this
      # label's — a sentence like "See @fig-a and @tbl-b." would otherwise
      # leave the other label's raw "@tbl-b" markup sitting in claimText.
      claim_text <- trimws(gsub("\\(?@(fig|tbl)-[A-Za-z0-9_-]+\\)?", "", hit[1], perl = TRUE))
      # inline computed values (`r accuracy_rf`) can't be evaluated by this
      # static parser — mark them as a placeholder rather than leak raw code
      claim_text <- gsub("`r [^`]+`", "[computed value]", claim_text, perl = TRUE)
      claim_text <- gsub("\\s+", " ", claim_text)
    }
  }

  lineage <- resolve_lineage(chunk_idx)
  # Dedup by chunk, then sort chronologically (chunk order) rather than
  # discovery order, so this reads like the actual pipeline: raw data in,
  # cleaned, then analysed — not whatever order the backward chase visited
  # chunks in.
  seen_idx <- integer(0)
  contrib_idxs <- integer(0)
  for (contrib in lineage$contributed) {
    if (contrib$chunkIdx %in% seen_idx) next
    seen_idx <- c(seen_idx, contrib$chunkIdx)
    contrib_idxs <- c(contrib_idxs, contrib$chunkIdx)
  }
  contrib_idxs <- sort(contrib_idxs)

  entries[[length(entries) + 1]] <- list(
    label = label,
    manuscriptRef = paste0("@", label),
    claimText = claim_text,
    caption = caption,
    sourceFile = ch$source_file,
    lineStart = ch$start,
    lineEnd = ch$end,
    reads = as.list(lineage$reads),
    writes = as.list(lineage$writes),
    code = strip_code(body),  # this chunk's own code — shown even when it IS the interesting part (e.g. a model fit called directly here), not just when an ancestor did the real work
    contribIdxs = contrib_idxs,  # resolved into dataPipeline/computedIn below, once every claim is known
    embeddedAsFigure = grepl("^fig-", label),
    githubPermalink = permalink_for(ch)
  )
}

# ---- classify contributing chunks as shared pipeline vs claim-specific ----
# A chunk that shows up as an ancestor of most claims (in practice: raw data
# load -> clean -> cache) is boilerplate every claim depends on and isn't
# what makes THIS claim's number what it is — collapse those into one line
# with no code. A chunk that's an ancestor of only some claims (the actual
# model/statistical test) is the interesting part and gets its code shown.
all_contrib_idxs <- unique(unlist(lapply(entries, function(e) e$contribIdxs)))
pipeline_idxs <- integer(0)
if (length(entries) >= 2) {
  presence_count <- vapply(all_contrib_idxs, function(idx) {
    sum(vapply(entries, function(e) idx %in% e$contribIdxs, logical(1)))
  }, integer(1))
  pipeline_idxs <- all_contrib_idxs[presence_count > length(entries) / 2]
}

chunk_ref <- function(idx, with_code) {
  ch <- chunks[[idx]]
  base <- list(
    label = ch$label,
    lineStart = ch$start,
    lineEnd = ch$end,
    githubPermalink = permalink_for(ch)
  )
  if (with_code) base$code <- strip_code(ch$body)
  base
}

for (i in seq_along(entries)) {
  idxs <- entries[[i]]$contribIdxs
  pipeline <- idxs[idxs %in% pipeline_idxs]
  interesting <- idxs[!(idxs %in% pipeline_idxs)]
  entries[[i]]$contribIdxs <- NULL
  entries[[i]]$dataPipeline <- lapply(pipeline, chunk_ref, with_code = FALSE)
  entries[[i]]$computedIn <- lapply(interesting, chunk_ref, with_code = TRUE)
}

manifest <- list(
  paper = list(
    doi = paper_doi,
    repo = if (!is.na(github_repo)) paste0("https://github.com/", github_repo) else NA_character_
  ),
  generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  gitRef = git_ref,
  claims = entries
)

jsonlite::write_json(manifest, file.path(repo_root, "evidence.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
cat(sprintf("Wrote evidence.json with %d claims (repo=%s, doi=%s, article=%s, notebooks=%s)\n",
            length(entries), github_repo, paper_doi, article_file, paste(notebook_files, collapse=",")))
