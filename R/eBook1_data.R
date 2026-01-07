#' Download getting started with dartR ebook data
#'
#' @param data_folder folder name
#' @return ebook data
#' @export
#' @author Arthur Georges


eBook1_data <- function(data_folder = 'data'){
# DOWNLAD THE REQUIRED DATA FILES TO WORKING DIRECTORY ./data.

# Download embedded filenames from an OctoberCMS Media HTML page.
# Save into ./data (overwrite). If .gz => gunzip. If .zip => unzip (overwrite extracted files).

index_url <- "http://georges.biomatix.org/storage/app/media/eBook%20Introduction%20to%20dartR/Media%20_%20OctoberCMS.htm"
base_url  <- "http://georges.biomatix.org/storage/app/media/eBook%20Introduction%20to%20dartR/"

# Match xlsx before xls; and ensure xls is not immediately followed by "x"
ext_group <- "(?:xlsx|xls(?!x)|csv|dat|txt|Rdata|vcf|gz|zip)"
file_rx <- paste0("([A-Za-z0-9 _\\-./]+\\.", ext_group, ")")
ext_pattern <- "\\.(xlsx|xls|csv|dat|txt|Rdata|vcf|gz|zip)$"

data_dir <- file.path(getwd(), data_folder)
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

options(timeout = 300)

# Choose whether to delete archives after extraction
delete_gz_after  <- TRUE
delete_zip_after <- TRUE

# Fetch HTML (binary-safe)
html <- tryCatch(
  {
    con <- url(index_url, open = "rb")
    on.exit(close(con), add = TRUE)
    raw <- readBin(con, what = "raw", n = 5e7)
    rawToChar(raw)
  },
  error = function(e) {
    stop("Failed to fetch index page: ", index_url, "\n", conditionMessage(e))
  }
)

# Extract candidate filename-like strings
m <- gregexpr(file_rx, html, ignore.case = TRUE, perl = TRUE)
hits <- unique(regmatches(html, m)[[1]])

clean_token <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- trimws(x)
  x <- gsub('^[\'"\\(\\[]+|[\'"\\)\\],;]+$', "", x)  # strip wrapper punctuation
  x
}

tokens <- unique(vapply(hits, clean_token, character(1)))

# Keep only true matches by extension (use the ext_pattern defined at the top)
tokens <- tokens[grepl(ext_pattern, tokens, ignore.case = TRUE)]

if (length(tokens) == 0) {
  stop("No filename strings matching extension pattern found in HTML.")
}


# NORMALISE to basenames
files <- unique(basename(tokens))

# Write manifest
manifest_path <- file.path(data_dir, "manifest.txt")
writeLines(files, manifest_path)
cat("Manifest written to: ", normalizePath(manifest_path, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Found ", length(files), " unique files.\n", sep = "")

# Build URLs safely (encode spaces etc.)
file_urls <- paste0(base_url, vapply(files, utils::URLencode, character(1), reserved = TRUE))

# --- Gunzip helper (no extra packages needed) ---
gunzip_file <- function(gz_path, overwrite = TRUE, remove_gz = FALSE) {
  if (!grepl("\\.gz$", gz_path, ignore.case = TRUE)) return(invisible(NULL))

  out_path <- sub("\\.gz$", "", gz_path, ignore.case = TRUE)
  if (file.exists(out_path) && overwrite) file.remove(out_path)

  in_con  <- gzfile(gz_path, open = "rb")
  on.exit(close(in_con), add = TRUE)
  out_con <- file(out_path, open = "wb")
  on.exit(close(out_con), add = TRUE)

  repeat {
    buf <- readBin(in_con, what = "raw", n = 1024 * 1024)  # 1 MB chunks
    if (length(buf) == 0) break
    writeBin(buf, out_con)
  }

  if (remove_gz) file.remove(gz_path)
  out_path
}

# --- Zip helper (overwrite extracted files) ---
unzip_overwrite <- function(zip_path, exdir, remove_zip = FALSE) {
  # List members
  members <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(members) || !"Name" %in% names(members)) {
    stop("Could not list contents of zip: ", basename(zip_path))
  }

  member_names <- members$Name

  # Remove existing extracted targets so unzip can overwrite cleanly on Windows
  targets <- file.path(exdir, member_names)
  exists <- file.exists(targets)
  if (any(exists)) {
    # Attempt to remove files (directories will be recreated on extract)
    suppressWarnings(file.remove(targets[exists]))
  }

  utils::unzip(zipfile = zip_path, exdir = exdir)

  if (remove_zip) file.remove(zip_path)

  invisible(member_names)
}

download_one <- function(u) {
  fname <- basename(u)
  dest  <- file.path(data_dir, fname)

  ok <- tryCatch(
    {
      # Overwrite the downloaded file
      if (file.exists(dest)) file.remove(dest)
      utils::download.file(u, destfile = dest, mode = "wb", quiet = TRUE)
      cat("[ok]   ", fname, "\n", sep = "")

      # Post-process archives
      if (grepl("\\.gz$", dest, ignore.case = TRUE)) {
        out_path <- gunzip_file(dest, overwrite = TRUE, remove_gz = delete_gz_after)
        cat("       gunzipped -> ", basename(out_path), "\n", sep = "")
      } else if (grepl("\\.zip$", dest, ignore.case = TRUE)) {
        extracted <- unzip_overwrite(dest, exdir = data_dir, remove_zip = delete_zip_after)
        cat("       unzipped  -> ", length(extracted), " file(s)\n", sep = "")
      }

      TRUE
    },
    error = function(e) {
      cat("[fail] ", fname, ": ", conditionMessage(e), "\n", sep = "")
      FALSE
    }
  )
  invisible(ok)
}

results <- vapply(file_urls, download_one, logical(1))

cat("\nDone. OK:", sum(results), " Failed:", sum(!results), "\n", sep = "")
cat("Saved under: ", normalizePath(data_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")

}
