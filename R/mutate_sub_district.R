#' Standardise subdistrict names
#'
#' STILL UNDER DEV: Take a data frame with a free-text subdistrict column and return the data
#' frame with a standardised subdistrict name column (`subdistrict_standard`) and
#' a match-method column (`.sd_match_method`). The function uses an internal
#' dictionary (built by `build_sa_subdistrict_dictionary()`) and performs
#' exact-normalised, regex and optional fuzzy matching passes.
#'
#' @param data A data.frame or tibble containing the subdistrict column.
#' @param subdistrict_variable Character scalar. Name of the column in `data`
#'   that contains the subdistrict text. Default: "subdistrict".
#' @param unmatched Character. What to do with unmatched values: either
#'   "keep_raw" (leave the original text) or "NA" (set to NA). Default is
#'   c("keep_raw", "NA").
#' @param use_fuzzy Logical. If TRUE and the package `fuzzyjoin` is available,
#'   a fuzzy string-distance match will be attempted for remaining unmatched
#'   values. Default uses requireNamespace("fuzzyjoin", quietly = TRUE).
#' @param max_dist Numeric. Maximum allowed distance for fuzzy matching (osa
#'   method). Default: 2.
#' @param dict Optional dictionary tibble created by
#'   `build_sa_subdistrict_dictionary()`. If NULL the dictionary will be
#'   constructed internally.
#'
#' @return The input `data` with two new columns appended:
#'   * `subdistrict_standard`: the standardised subdistrict (character or NA)
#'   * `.sd_match_method`: the method used to match ("exact_norm", "regex",
#'     "fuzzy", or NA)
#' @export
#' @examples
#' df <- tibble::tibble(subdistrict = c("Cape Town", "unknown place"))
#' Make_sub_district_name(df)
mutate_sub_district<- function(data,
                                   subdistrict_variable = "subdistrict",
                                   unmatched = c("keep_raw", "NA"),
                                   use_fuzzy = requireNamespace("fuzzyjoin", quietly = TRUE),
                                   max_dist = 2,
                                   dict = NULL) {
    unmatched <- match.arg(unmatched)
    stopifnot(is.data.frame(data))
    if (!subdistrict_variable %in% names(data)) {
        stop("Column `", subdistrict_variable, "` not found in `data`.")
    }
    if (is.null(dict)) {
        # Try to read pre-built dictionary from inst/extdata for package distribution
        rds_path <- "inst/extdata/sa_subdistrict_dictionary.rds"
        if (file.exists(rds_path)) {
            dict_candidate <- tryCatch({
                readRDS(rds_path)
            }, error = function(e) {
                stop("Failed to read dictionary from ", rds_path, ": ", conditionMessage(e))
            })

            # dict_candidate can be either the long flat table (dict_long) or
            # a list-column table (dict_listcol) or a list with attr long_version.
            if (is.data.frame(dict_candidate) && all(c("standard_subdistrict", "variant") %in% names(dict_candidate))) {
                dict_long <- dict_candidate
            } else if (is.data.frame(dict_candidate) && "subdistrict_variant" %in% names(dict_candidate)) {
                dict <- dict_candidate
                dict_long <- attr(dict, "long_version")
                if (is.null(dict_long)) {
                    dict_long <- dict %>%
                        dplyr::rename(variant = subdistrict_variant) %>%
                        dplyr::mutate(variant_norm = normalize_key(variant))
                }
            } else if (!is.null(attr(dict_candidate, "long_version"))) {
                dict_long <- attr(dict_candidate, "long_version")
            } else {
                stop("Unrecognized dictionary format in ", rds_path)
            }
        } else {
            # fallback to building dictionary (developer workflow)
            if (requireNamespace("NMCleaner", quietly = TRUE) && exists("build_sa_subdistrict_dictionary", where = "package:NMCleaner")) {
                dict <- build_sa_subdistrict_dictionary(include_metro_regions = TRUE)
            } else if (exists("build_sa_subdistrict_dictionary")) {
                dict <- build_sa_subdistrict_dictionary(include_metro_regions = TRUE)
            } else {
                stop("No dictionary provided and 'inst/extdata/sa_subdistrict_dictionary.rds' not found. Please provide 'dict' or add the RDS to inst/extdata.")
            }

            # if build_sa_subdistrict_dictionary returns list with dict_long, extract
            if (is.list(dict) && !is.null(dict$dict_long)) dict_long <- dict$dict_long
        }
    }

    # If dict was provided by the caller and dict_long not yet set, compute it
    if (!exists("dict_long")) {
        dict_long <- attr(dict, "long_version")
        if (is.null(dict_long)) {
            if (is.data.frame(dict) && all(c("standard_subdistrict", "variant") %in% names(dict))) {
                dict_long <- dict
            } else if (is.data.frame(dict) && "subdistrict_variant" %in% names(dict)) {
                dict_long <- dict %>%
                    dplyr::rename(variant = subdistrict_variant) %>%
                    dplyr::mutate(variant_norm = normalize_key(variant))
            } else {
                stop("Provided 'dict' has an unrecognized format; expect dict_long (flat) or list-column dictionary.")
            }
        }
    }

    # If a manual alias CSV is provided in inst/extdata, append it to dict_long
    manual_alias_csv <- "inst/extdata/manual_subdistrict_aliases.csv"
    if (file.exists(manual_alias_csv)) {
        manual_csv <- tryCatch({
            utils::read.csv(manual_alias_csv, stringsAsFactors = FALSE)
        }, error = function(e) NULL)
        if (!is.null(manual_csv) && all(c("standard_subdistrict", "alias") %in% names(manual_csv))) {
            extra <- manual_csv %>% dplyr::transmute(standard_subdistrict = as.character(standard_subdistrict), variant = as.character(alias)) %>%
                dplyr::mutate(variant_norm = normalize_key(variant))
            dict_long <- dplyr::bind_rows(dict_long, extra) %>% dplyr::distinct()
        }
    }

    # ---- helpers ----
    normalize_key <- function(x) {
        x <- stringr::str_to_lower(x)
        x <- gsub("\\(.*?\\)", "", x)
        common_words <- c(
            "municipality", "district", "city", "metro", "metropolitan",
            "region", "subdistrict", "sub-district", "local", "lm", "of", "the"
        )
        x <- stringr::str_replace_all(x, paste0("\\b(", paste(common_words, collapse = "|"), ")\\b"), " ")
        x <- stringr::str_replace_all(x, "[^a-z0-9]+", "")

        # replace "doctor" with "dr"
        x <- stringr::str_replace_all(x, "doctor", "dr")
        # do the same for saint -> st
        x <- stringr::str_replace_all(x, "saint", "st")
        # replace special characters like "é" with standard letters
        # Replace special characters with standard letters, but only if the letter is found in the word
        x <- sapply(x, function(word) {
            if (grepl("[^a-z0-9]", word)) {
            stringi::stri_trans_general(word, "Latin-ASCII")
            } else {
            word
            }
        }, USE.NAMES = FALSE)

        trimws(x)
    }


    # Only construct dict_long here if it hasn't already been set above.
    if (!exists("dict_long")) {
        dict_long <- NULL
        # Prefer an attached long_version if dict is a list-column object
        if (!is.null(dict) && !is.null(attr(dict, "long_version"))) {
            dict_long <- attr(dict, "long_version")
        } else if (!is.null(dict) && is.data.frame(dict) && all(c("standard_subdistrict", "variant") %in% names(dict))) {
            dict_long <- dict
        } else if (!is.null(dict) && is.data.frame(dict) && "subdistrict_variant" %in% names(dict)) {
            dict_long <- dict %>% dplyr::rename(variant = subdistrict_variant) %>% dplyr::mutate(variant_norm = normalize_key(variant))
        }

        if (is.null(dict_long)) {
            stop("No usable dictionary found: provide 'dict' or ensure 'inst/extdata/sa_subdistrict_dictionary.rds' exists or build the dictionary via build_sa_subdistrict_dictionary().")
        }
    }

    # fast exact-normalized map
    keys <- dict_long %>% dplyr::distinct(standard_subdistrict, variant_norm)
    key2std <- stats::setNames(keys$standard_subdistrict, keys$variant_norm)

    raw_vec <- as.character(data[[subdistrict_variable]])
    clean_vec <- normalize_key(raw_vec)
    std_exact <- unname(key2std[clean_vec])
    match_method <- ifelse(!is.na(std_exact) & nzchar(std_exact), "exact_norm", NA_character_)
    out_std <- std_exact

    # regex pass (first-hit)
    need_regex <- which(is.na(out_std) | !nzchar(out_std))
    if (length(need_regex) > 0) {
        regex_dict <- dict_long %>%
            dplyr::mutate(pattern = paste0(
                "\\b",
                stringr::str_replace_all(variant, "\\s+", "\\\\s*[-_]*\\\\s*"),
                "\\b"
            )) %>%
            dplyr::distinct(standard_subdistrict, pattern)

        for (i in need_regex) {
            txt <- raw_vec[i]
            hits <- regex_dict$standard_subdistrict[
                stringr::str_detect(
                    stringr::str_to_lower(txt),
                    stringr::regex(regex_dict$pattern, ignore_case = TRUE)
                )
            ]
            if (length(hits) >= 1) {
                out_std[i] <- hits[[1]]
                match_method[i] <- "regex"
            }
        }
    }
# fuzzy pass (osa distance on normalized keys)
if (isTRUE(use_fuzzy)) {
    still_need <- which(is.na(out_std) | !nzchar(out_std))
    if (length(still_need) > 0) {
        stopifnot(requireNamespace("fuzzyjoin", quietly = TRUE))
        cand <- keys %>% dplyr::rename(variant_norm = variant_norm)
        need_df <- tibble::tibble(
            idx = still_need,
            clean = clean_vec[still_need]
        )
        fz <- fuzzyjoin::stringdist_left_join(
            need_df, cand,
            by = c("clean" = "variant_norm"),
            method = "osa",
            max_dist = max_dist,
            distance_col = ".dist"
        )
        fz_best <- fz %>%
            dplyr::group_by(idx) %>%
            dplyr::slice_min(.dist, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        hit_rows <- which(!is.na(fz_best$standard_subdistrict))
        if (length(hit_rows) > 0) {
            idxs <- fz_best$idx[hit_rows]
            out_std[idxs] <- fz_best$standard_subdistrict[hit_rows]
            match_method[idxs] <- "fuzzy"
        }
    }
}

    # policy for unmatched is to keep raw
    if (identical(unmatched, "keep_raw")) {
        out_std[is.na(out_std) | !nzchar(out_std)] <- raw_vec[is.na(out_std) | !nzchar(out_std)]
    } else {
        out_std[is.na(out_std) | !nzchar(out_std)] <- NA_character_
    }

    data$subdistrict_standard <- out_std
    data$.sd_match_method <- match_method
    data
}


# example usage 
 #generate_sa_subdistrict_dictionary(save_inst_rds = TRUE, overwrite = TRUE) -> v2_subdistricts

#v2_subdistricts$dict_listcol

#dict <- readr::read_rds("inst/extdata/sa_subdistrict_dictionary.rds")
#dict
#mutate_sub_district( 
#    data.frame( subdistrict = c("mother city", "ekurhuleni north 1","ekurhuleni", "thabo mofutsnyane", "manguang", "mosselbaai" ,"port saint john", "city of cape town", "Doctor js morkoa", "Dr pixley isaka seme", "nelson mandela bay", "port rex", "cit of jobrg")), 
#    dict = dict
#)

#mutate_sub_district(
#    data.frame(district = c("mother city", "ekurhuleni north 1", "ekurhuleni", "thabo mofutsnyane", "manguang", "mosselbaai", "port saint john", "city of cape town", "Doctor js morkoa", "Dr pixley isaka seme", "nelson mandela bay", "port rex", "cit of jobrg")),
    #    dict = dict
#)
