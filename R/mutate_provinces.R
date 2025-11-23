
#' Standardise South African province names and codes
#'
#' This function standardises province inputs (full names or abbreviations)
#' into canonical province names and the short province code (`prov`). It is
#' robust to differences in case, punctuation and common short forms.
#'
#' @param data A data.frame or a character vector of province names/abbreviations.
#' @param province_col Name of the column in `data` that contains the province
#'   text (only used when `data` is a data.frame). Default: "province".
#' @param out_col Name of the output column for canonical province names.
#'   Default: "province_standard".
#' @param code_col Name of the output column for the short code (e.g. "GP").
#'   Default: "prov".
#' @param keep_raw If FALSE, the original `province_col` will be removed from
#'   the returned data.frame. Default: TRUE.
#' @return If `data` is a data.frame, returns the data.frame with added
#'   `out_col` and `code_col`. If `data` is a character vector, returns a
#'   data.frame with columns `input`, `province_standard`, and `prov`.
#' @export
#' @examples
#' mutate_provinces(c("EC", "Gauteng", "western cape"))
#' mutate_provinces(data.frame(province = c("EC", "Eastern Cape")))
mutate_provinces <- function(
    data,
    province_col = "province",
    out_col = "province_standard",
    code_col = "prov",
    keep_raw = TRUE,
    fuzzy = FALSE,
    max_dist = 2, 
    dictionary = NULL,
    debug = FALSE
) {
    normalize <- function(x) {
        x <- as.character(x)
        x[is.na(x)] <- NA_character_
        x <- tolower(x)
        x <- stringi::stri_trans_general(x, "Latin-ASCII")
        x <- gsub("[^a-z0-9]", "", x)
        x
    }

    # If a dictionary is supplied, accept either a long table
    # (province_standard, prov, variant, variant_norm optional) or a
    # list-column table with `variants` per province.
    # If a dictionary is supplied the function will use it instead of the
    # internal hard-coded variants. The dictionary may be either:
    # - long format: columns (province_standard, prov, variant, [variant_norm])
    # - list-column format: columns (province_standard, prov, variants) where
    #   `variants` is a list-column of character vectors.
    #
    # Use `debug = TRUE` to print diagnostics about which dictionary was used
    # and brief summaries of matches/unmatched inputs.
    if (!is.null(dictionary)) {
        if (!is.data.frame(dictionary)) stop("dictionary must be a data.frame (long or list-column)")
        dict_df <- dictionary
        # long format expected
        if (all(c("province_standard", "prov", "variant") %in% names(dict_df))) {
            dict_long <- dict_df
        } else if (all(c("province_standard", "prov", "variants") %in% names(dict_df))) {
            dict_long <- dict_df %>% tidyr::unnest_longer(variants) %>% dplyr::rename(variant = variants)
        } else {
            stop("dictionary must contain either (province_standard, prov, variant) or (province_standard, prov, variants)")
        }

        # compute normalized variant if absent
        if (!"variant_norm" %in% names(dict_long)) {
            dict_long <- dict_long %>% dplyr::mutate(variant_norm = normalize(variant))
        }

        if (debug) {
            message("Using supplied dictionary: rows = ", nrow(dict_long), "; distinct provinces = ", length(unique(dict_long$prov)))
            message("Sample dictionary rows:")
            print(utils::head(dict_long, 6))
        }

            # build full_names and variants list from dict_long
            full_names <- dict_long %>% dplyr::distinct(prov, province_standard) %>% tibble::deframe()
            variants <- dict_long %>% dplyr::group_by(prov) %>% dplyr::summarise(vars = list(unique(variant)), .groups = "drop") %>% tibble::deframe()

            # Deduplicate normalized variants deterministically using a simple priority:
            # 1) variant_norm == normalized(province_standard) (exact full-name),
            # 2) variant equals prov (code),
            # 3) other variants.
            dict_long <- dict_long %>% dplyr::mutate(
                variant_norm = as.character(variant_norm),
                .priority = dplyr::case_when(
                    variant_norm == normalize(province_standard) ~ 1L,
                    variant == prov ~ 2L,
                    TRUE ~ 3L
                )
            ) %>% dplyr::arrange(variant_norm, .priority)

            dict_long <- dict_long %>% dplyr::distinct(variant_norm, .keep_all = TRUE)

            # reversed maps using deduplicated normalized variants
            all_variants <- dict_long$variant
            all_norm_variants <- dict_long$variant_norm
            all_codes <- dict_long$prov
            rev_map <- setNames(all_codes, all_norm_variants)
    } else {
        # canonical names
        full_names <- c(
            EC = "Eastern Cape",
            FS = "Free State",
            GP = "Gauteng",
            KZN = "KwaZulu-Natal",
            LP = "Limpopo",
            MP = "Mpumalanga",
            NW = "North West",
            NC = "Northern Cape",
            WC = "Western Cape"
        )

        variants <- list(
            EC = c("easterncape", "easterncapeprovince", "ec"),
            FS = c("freestate", "freestateprovince", "fs"),
            GP = c("gauteng", "gp", "gt"),
            KZN = c("kwazulunatal", "kwazulunatalprovince", "kzn"),
            LP = c("limpopo", "lp", "lm", "lim"),
            MP = c("mpumalanga", "mp"),
            NW = c("northwest", "northwestprovince", "nw"),
            NC = c("northerncape", "northerncapeprovince", "nc", "ncape", "northern"),
            WC = c("westerncape", "westerncapeprovince", "wc", "western")
        )

        # reverse lookup: normalized variant -> code
        rev_map <- unlist(lapply(names(variants), function(code) setNames(rep(code, length(variants[[code]])), normalize(variants[[code]]))))
        all_variants <- unlist(variants)
        all_norm_variants <- normalize(all_variants)
        all_codes <- rep(names(variants), times = vapply(variants, length, integer(1)))
    }

    # helper to map a character vector. This function now tracks
    # `match_method` per input so we can distinguish exact vs fuzzy matches
    map_vec <- function(vec) {
        norm <- normalize(vec)
        codes <- rev_map[norm]
        match_method <- rep(NA_character_, length(norm))
        match_method[!is.na(codes)] <- "exact"
        # Fuzzy match if not found and fuzzy = TRUE
        if (fuzzy) {
            na_idx <- which(is.na(codes) & !is.na(norm))
            if (length(na_idx) > 0) {
                # Prefer vectorised matching via stringdist::amatch when available
                if (requireNamespace("stringdist", quietly = TRUE)) {
                    matches <- stringdist::amatch(norm[na_idx], all_norm_variants, method = "osa", maxDist = max_dist)
                    matched <- which(!is.na(matches))
                    if (length(matched) > 0) {
                        codes[na_idx[matched]] <- all_codes[matches[matched]]
                        match_method[na_idx[matched]] <- "fuzzy"
                    }
                } else {
                    # fallback: per-item adist
                    for (ii in seq_along(na_idx)) {
                        i <- na_idx[ii]
                        dists <- adist(norm[i], all_norm_variants)
                        min_dist <- min(dists)
                        if (min_dist <= max_dist) {
                            match_idx <- which.min(dists)
                            codes[i] <- all_codes[match_idx]
                            match_method[i] <- "fuzzy"
                        }
                    }
                }
            }
        }
        codes[is.na(codes)] <- NA_character_
        full <- full_names[codes]
        out <- data.frame(input = vec, province_standard = unname(full), prov = unname(codes), match_method = match_method, stringsAsFactors = FALSE)
        if (debug) {
            # diagnostics summary for this batch
            total <- length(vec)
            n_exact <- sum(out$match_method == "exact", na.rm = TRUE)
            n_fuzzy <- sum(out$match_method == "fuzzy", na.rm = TRUE)
            n_unmatched <- sum(is.na(out$prov))
            message("[mutate_provinces debug] inputs=", total, " exact=", n_exact, " fuzzy=", n_fuzzy, " unmatched=", n_unmatched)
            if (n_unmatched > 0) {
                message("[mutate_provinces debug] unmatched examples:")
                print(utils::head(out[is.na(out$prov), ], 5))
            }
        }
        out
    }

    if (!is.data.frame(data)) {
        return(map_vec(data))
    }

    if (!province_col %in% names(data)) stop("Column '", province_col, "' not found in data")

    mapped <- map_vec(data[[province_col]])
    data[[out_col]] <- mapped$province_standard
    data[[code_col]] <- mapped$prov

    if (!keep_raw) data[[province_col]] <- NULL

    data
}


# example usage
#mutate_provinces(data.frame(province = c( "lim", "Western Cape", "KwaZulu-Natal")))
