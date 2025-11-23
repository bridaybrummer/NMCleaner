#' Standardise District Names (version 2)
#'
#' A more robust district-name normaliser that accepts an external canonical
#' dictionary (long or list-column), can append remote alias CSVs, and supports
#' optional fuzzy matching using `stringdist::amatch()` with an `adist()` fallback.
#'
#' @param data A data.frame or a character vector of district names.
#' @param district_col Name of the column in `data` containing district text
#'   (used only when `data` is a data.frame). Default: "district".
#' @param out_col Name of the output column for canonical district names.
#'   Default: "district_standard".
#' @param code_col Name of the output column for a district code (e.g. "DC15").
#'   Default: "district_code". If the dictionary does not provide codes this
#'   column will be NA.
#' @param keep_raw If FALSE, the original `district_col` will be removed from
#'   returned data.frame. Default: TRUE.
#' @param fuzzy Logical; if TRUE attempt fuzzy matching for unmatched inputs.
#' @param max_dist Maximum edit distance allowed for fuzzy matches (integer).
#' @param dictionary Optional dictionary (data.frame). Long format must contain
#'   `district_standard`, `district_code`, `variant` (and optionally
#'   `variant_norm`). List-column format should contain `district_standard`,
#'   `district_code`, `variants` (a list-column of character vectors). If NULL
#'   the function tries to read `inst/extdata/canonical_districts.rds` then
#'   falls back to a built-in small dictionary.
#' @param remote_alias_urls Optional character vector of URLs or local paths to
#'   CSV files with columns `district_standard` and `alias` to append as manual
#'   aliases (useful to capture upstream name-changes). These are read with
#'   `readr::read_csv()` and appended to the dictionary.
#' @param debug Logical; if TRUE prints brief diagnostics about matching.
#' @return If `data` is a data.frame, returns the data.frame with added
#'   `out_col` and `code_col`. If `data` is a character vector, returns a
#'   data.frame with columns `input`, `district_standard`, `district_code` and
#'   `match_method`.
#' @export
mutate_district_name_v2 <- function(
        data,
        district_col = "district",
        out_col = "district_standard",
        code_col = "district_code",
        keep_raw = TRUE,
        fuzzy = FALSE,
        max_dist = 2L,
        dictionary = NULL,
        remote_alias_urls = NULL,
        debug = FALSE
) {
    normalize <- function(x) {
        stopifnot(is.character(x) || is.factor(x))
        x <- as.character(x)

        # 1) lowercase early
        x <- stringr::str_to_lower(x)

        # 2) remove parenthetical content (no need for lazy quantifier)
        x <- gsub("\\s*\\([^)]*\\)", "", x)

        # 3) fold accents BEFORE stripping punctuation
        x <- stringi::stri_trans_general(x, "Latin-ASCII")

        # 4) normalise punctuation to spaces (incl. hyphens, slashes, SA click symbols)
        x <- gsub("[!ǃǀǁǂ]", " ", x)
        x <- gsub("[-_/.,]+", " ", x)
        x <- gsub("\\s+", " ", x)

        # 5) standardise common variants
        x <- stringr::str_replace_all(x, "\\bdoctor\\b", "dr")
        x <- stringr::str_replace_all(x, "\\bsaint\\b", "st")

        # 6) drop common “furniture” words while spaces still exist
        common_words <- c(
            "municipality", "district", "city", "metro", "metropolitan",
            "region", "subdistrict", "sub-district", "local", "lm", "of", "the", "greater",
            "north" # this is to handle "ekurhuleni north 1" but might not be advisable at sub-dsitrct level 
        )
        pat <- paste0("\\b(", paste(common_words, collapse = "|"), ")\\b")
        x <- stringr::str_replace_all(x, pat, " ")

        # 7) drop standalone numbers (e.g., "north 1" -> "north")
        x <- stringr::str_replace_all(x, "\\b\\d+\\b", " ")

        # (optional) if you also want to drop roman numerals like "ii", "iii", etc.:
        # x <- stringr::str_replace_all(x, "\\b(i|ii|iii|iv|v|vi|vii|viii|ix|x)\\b", " ")

        # 8) squeeze and trim
        x <- gsub("\\s+", " ", x)
        x <- trimws(x)

        # 9) final canonical key: letters+digits only
        key <- gsub("[^a-z0-9]", "", x)
        key[key == ""] <- NA_character_
        key
    }

    # load or prepare dictionary long-form (district_standard, district_code, variant, variant_norm)
    load_dictionary <- function(dict = NULL, remote_alias_urls = NULL) {
        # helper to coerce long/list formats to long table with variant column
        to_long <- function(d) {
            if (is.data.frame(d) && all(c("district_standard", "district_code", "variant") %in% names(d))) {
                return(d)
            }
            if (is.data.frame(d) && all(c("district_standard", "district_code", "variants") %in% names(d))) {
                return(d %>% tidyr::unnest_longer(variants) %>% dplyr::rename(variant = variants))
            }
            stop("dictionary must contain either (district_standard, district_code, variant) or (district_standard, district_code, variants)")
        }

        if (!is.null(dict)) {
            if (!is.data.frame(dict)) stop("dictionary must be a data.frame (long or list-column)")
            dict_long <- to_long(dict)
        } else {
            # attempt to read inst/extdata rds
            rds_path <- "inst/extdata/canonical_districts.rds"
            if (file.exists(rds_path)) {
                dict_candidate <- tryCatch(readRDS(rds_path), error = function(e) NULL)
                if (is.null(dict_candidate)) stop("Failed to read ", rds_path)
                if (is.data.frame(dict_candidate) && all(c("district_standard", "district_code", "variant") %in% names(dict_candidate))) {
                    dict_long <- dict_candidate
                } else if (!is.null(attr(dict_candidate, "long_version"))) {
                    dict_long <- attr(dict_candidate, "long_version")
                } else if (is.data.frame(dict_candidate) && "variants" %in% names(dict_candidate)) {
                    dict_long <- dict_candidate %>% tidyr::unnest_longer(variants) %>% dplyr::rename(variant = variants)
                } else {
                    stop("Unrecognized dictionary format in ", rds_path)
                }
            } else {
                # fallback: built-in small dictionary (list-column) then unnest to long
                base_tbl <- tibble::tibble(
                    district_standard = c(
                        "Alfred Nzo", "Amathole", "Buffalo City", "Chris Hani", "Joe Gqabi",
                        "Nelson Mandela Bay", "O.R. Tambo", "Sarah Baartman", "Fezile Dabi",
                        "Lejweleputswa", "Mangaung", "Thabo Mofutsanyana", "Xhariep",
                        "City of Johannesburg", "City of Tshwane", "Ekurhuleni", "Sedibeng",
                        "West Rand", "Amajuba", "Harry Gwala", "King Cetshwayo", "Ugu",
                        "Umzinyathi", "Zululand", "eThekwini", "iLembe", "uMgungundlovu",
                        "uMkhanyakude", "uThukela", "Capricorn", "Sekhukhune", "Mopani",
                        "Vhembe", "Waterberg", "Ehlanzeni", "Gert Sibande", "Nkangala",
                        "Frances Baard", "John Taolo Gaetsewe", "Namakwa", "Pixley ka Seme",
                        "ZF Mgcawu", "Bojanala Platinum", "Dr Kenneth Kaunda", "Dr Ruth Segomotsi Mompati",
                        "Ngaka Modiri Molema", "Cape Winelands", "Central Karoo", "City of Cape Town",
                        "Garden Route", "Overberg", "West Coast"
                    ),
                    district_code = NA_character_,
                    variants = list(
                        c("Alfred Nzo", "Alfred Nzo District", "Alfred Nzo Municipality", "DC44", "Alfred Nzo DM", "AlfredNzo"),
                        c("Amathole", "Amathole District", "Amathole District Municipality", "DC12", "Amatole", "Amathol"),
                        c("Buffalo City", "Buffalo City Metro", "Buffalo Metropolitan", "Buffalo City Municipality", "BuffaloCity", "Buffalo"),
                        c("Chris Hani", "Chris Hani District", "Chris Hani District Municipality", "DC13", "C.Hani"),
                        c("Joe Gqabi", "Joe Gqabi District", "Joe Gqabi District Municipality", "DC14", "JGqabi", "joe qadi"),
                        c("Nelson Mandela Bay", "NMB", "Nelson Mandela Bay Metro", "Nelson Mandela Metropolitan Municipality"),
                        c("O.R. Tambo", "OR Tambo", "O.R.Tambo District", "DC15", "ORTambo", "o.r.tambo", "o r tambo"),
                        c("Sarah Baartman", "Sarah Baartman District", "Cacadu", "Sarah Baartman Municipality", "DC10"),
                        c("Fezile Dabi", "Fezile Dabi District", "Fezile Dabi Municipality", "DC20", "FezileD"),
                        c("Lejweleputswa", "Lejweleputswa District", "Lejweleputswa Municipality", "DC18", "Lejwe"),
                        c("Mangaung", "Mangaung Metro", "Mangaung Municipality", "MAN", "Mangaung Metropolitan"),
                        c("Thabo Mofutsanyana", "Thabo Mofutsanyana District", "Thabo Mofutsanyane Municipality", "DC19", "Thabo M"),
                        c("Xhariep", "Xhariep District", "Xhariep Municipality", "DC16"),
                        c("City of Johannesburg", "Johannesburg Metro", "Johannesburg Metropolitan Municipality", "Johannesburg", "JHB"),
                        c("City of Tshwane", "Tshwane Metro", "Tshwane Metropolitan Municipality", "Tshwane", "Pretoria"),
                        c("Ekurhuleni", "Ekurhuleni Metro", "Ekurhuleni Metropolitan Municipality", "EKU"),
                        c("Sedibeng", "Sedibeng District", "Sedibeng Municipality", "DC42"),
                        c("West Rand", "West Rand DM", "West Rand District", "DC48", "WR"),
                        c("Amajuba", "Amajuba District", "Amajuba Municipality", "DC25"),
                        c("Harry Gwala", "Harry Gwala District", "Sisonke", "Harry Gwala Municipality", "DC43"),
                        c("King Cetshwayo", "King Cetshwayo District", "Uthungulu", "King Cetshwayo Municipality", "DC28"),
                        c("Ugu", "Ugu District", "Ugu Municipality", "DC21"),
                        c("Umzinyathi", "Umzinyathi District", "Umzinyathi Municipality", "DC24"),
                        c("Zululand", "Zululand District", "Zululand Municipality", "DC26"),
                        c("eThekwini", "eThekwini Metro", "Durban", "eThekwini Municipality", "eThekwini Metropolitan"),
                        c("iLembe", "iLembe District", "iLembe Municipality", "DC29", "lembe"),
                        c("uMgungundlovu", "Umgungundlovu", "uMgungundlovu District", "DC22", "Umgung"),
                        c("uMkhanyakude", "Umkhanyakude", "uMkhanyakude District", "DC27", "Mkhanyakude"),
                        c("uThukela", "Uthukela", "uThukela District", "DC23", "Thukela"),
                        c("Capricorn", "Capricorn District", "Capricorn Municipality", "DC35"),
                        c("Sekhukhune", "Greater Sekhukhune", "Sekhukhune District", "Sekhukhune Municipality", "DC47"),
                        c("Mopani", "Mopani District", "Mopani Municipality", "DC33"),
                        c("Vhembe", "Vhembe District", "Vhembe Municipality", "DC34"),
                        c("Waterberg", "Waterberg District", "Waterberg Municipality", "DC36"),
                        c("Ehlanzeni", "Ehlanzeni District", "Ehlanzeni Municipality", "DC32"),
                        c("Gert Sibande", "Gert Sibande District", "Gert Sibande Municipality", "DC30", "GSibande"),
                        c("Nkangala", "Nkangala District", "Nkangala Municipality", "DC31"),
                        c("Frances Baard", "Frances Baard District", "Frances Baard Municipality", "DC9"),
                        c("John Taolo Gaetsewe", "John Taolo Gaetsewe District", "John Taolo", "DC45"),
                        c("Namakwa", "Namakwa District", "Namakwa Municipality", "DC6"),
                        c("Pixley ka Seme", "Pixley ka Seme District", "Pixley ka Seme Municipality", "DC7"),
                        c("ZF Mgcawu", "Z F Mgcawu", "ZF Mgcawu District", "DC8", "siyanda"),
                        c("Bojanala Platinum", "Bojanala", "Bojanala District", "DC37"),
                        c("Dr Kenneth Kaunda", "Dr Kenneth Kaunda District", "DC40"),
                        c("Dr Ruth Segomotsi Mompati", "Dr Ruth Mompati", "DC39"),
                        c("Ngaka Modiri Molema", "Ngaka Modiri", "DC38"),
                        c("Cape Winelands", "Cape Winelands District", "DC2"),
                        c("Central Karoo", "Central Karoo District", "DC5"),
                        c("City of Cape Town", "Cape Town Metro", "Cape Town Metropolitan Municipality", "Cape Town", "CPT"),
                        c("Garden Route", "Garden Route District", "Garden Route Municipality", "DC4", "Eden"),
                        c("Overberg", "Overberg District", "Overberg Municipality", "DC3"),
                        c("West Coast", "West Coast District", "West Coast Municipality", "DC1")
                    )
                )
                dict_long <- base_tbl %>% dplyr::select(district_standard, district_code, variants) %>% tidyr::unnest_longer(variants) %>% dplyr::rename(variant = variants)
            }
        }

        # compute normalized variant if absent
        if (!"variant_norm" %in% names(dict_long)) {
            dict_long <- dict_long %>% dplyr::mutate(variant_norm = normalize(variant))
        }

        # append remote alias CSVs if provided
        if (!is.null(remote_alias_urls) && length(remote_alias_urls) > 0) {
            for (url in remote_alias_urls) {
                try({
                    ali <- readr::read_csv(url, show_col_types = FALSE)
                    if (all(c("district_standard", "alias") %in% names(ali))) {
                        extra <- ali %>% dplyr::transmute(district_standard = as.character(district_standard), district_code = NA_character_, variant = as.character(alias)) %>% dplyr::mutate(variant_norm = normalize(variant))
                        dict_long <- dplyr::bind_rows(dict_long, extra) %>% dplyr::distinct()
                    } else {
                        warning("Remote alias CSV at ", url, " missing required columns district_standard, alias")
                    }
                }, silent = TRUE)
            }
        }

        return(dict_long)
    }

    dict_long <- load_dictionary(dictionary, remote_alias_urls)

    # deterministic deduplication priority and reverse map
    dict_long <- dict_long %>% dplyr::mutate(
        variant_norm = as.character(variant_norm),
        .priority = dplyr::case_when(
            variant_norm == normalize(district_standard) ~ 1L,
            variant == district_code ~ 2L,
            TRUE ~ 3L
        )
    ) %>% dplyr::arrange(variant_norm, .priority) %>% dplyr::distinct(variant_norm, .keep_all = TRUE)

    # ensure there is a usable 'district_code' for every row; if missing, use the standard name
    dict_long <- dict_long %>% dplyr::mutate(district_code = as.character(dplyr::coalesce(district_code, district_standard)))

    all_norm_variants <- dict_long$variant_norm
    all_variants <- dict_long$variant
    all_codes <- dict_long$district_code
    rev_map <- setNames(all_codes, all_norm_variants)
    names_map <- stats::setNames(dict_long$district_standard, dict_long$district_code)

    map_vec <- function(vec) {
        norm <- normalize(vec)
        codes <- rev_map[norm]
        match_method <- rep(NA_character_, length(norm))
        match_method[!is.na(codes)] <- "exact"

        if (fuzzy) {
            na_idx <- which(is.na(codes) & !is.na(norm))
            if (length(na_idx) > 0) {
                if (requireNamespace("stringdist", quietly = TRUE)) {
                    matches <- stringdist::amatch(norm[na_idx], all_norm_variants, method = "osa", maxDist = max_dist)
                    matched <- which(!is.na(matches))
                    if (length(matched) > 0) {
                        codes[na_idx[matched]] <- all_codes[matches[matched]]
                        match_method[na_idx[matched]] <- "fuzzy"
                    }
                } else {
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
        std <- NA_character_
        if (!all(is.na(codes))) std <- names_map[codes]
        out <- data.frame(input = vec, district_standard = unname(std), district_code = unname(codes), match_method = match_method, stringsAsFactors = FALSE)
        if (debug) {
            total <- length(vec)
            n_exact <- sum(out$match_method == "exact", na.rm = TRUE)
            n_fuzzy <- sum(out$match_method == "fuzzy", na.rm = TRUE)
            n_unmatched <- sum(is.na(out$district_code))
            message("[mutate_district_name_v2 debug] inputs=", total, " exact=", n_exact, " fuzzy=", n_fuzzy, " unmatched=", n_unmatched)
            if (n_unmatched > 0) print(utils::head(out[is.na(out$district_code), ], 5))
        }
        out
    }

    if (!is.data.frame(data)) return(map_vec(data))
    if (!district_col %in% names(data)) stop("Column '", district_col, "' not found in data")

    mapped <- map_vec(data[[district_col]])
    data[[out_col]] <- mapped$district_standard
    data[[code_col]] <- mapped$district_code
    if (!keep_raw) data[[district_col]] <- NULL
    data
}

# example: call generate_canonical_districts() from data-raw to create/update inst/extdata/canonical_districts.rds

#generate_canonical_districts()-> dictionary_districts
#dictionary_districts$dict_long

 mutate_district_name_v2(
    data.frame(district = c("cape winelands", "mangaung", "alfred nzo", "eden district", "PE", "port elizabeth", "ekurhuleni north 1")),
    fuzzy = TRUE
)



#mutate_district_name_v2(
#    data.frame(district = c("cape winelands", "mangaung", "alfred nzo", "eden district", "PE", "port elizabeth", "maloti")),
#    district_col = "district",
#    out_col = "district_standard",
#    code_col = "district_code",
#    keep_raw = TRUE,
#    fuzzy = TRUE,
#    max_dist = 2L,
#    dictionary = districts_db$dict_long,
#    remote_alias_urls = NULL,
#    debug = FALSE
#)
normalize <- function(x) {
    stopifnot(is.character(x) || is.factor(x))
    x <- as.character(x)

    # 1) lowercase early
    x <- stringr::str_to_lower(x)

    # 2) remove parenthetical content (no need for lazy quantifier)
    x <- gsub("\\s*\\([^)]*\\)", "", x)

    # 3) fold accents BEFORE stripping punctuation
    x <- stringi::stri_trans_general(x, "Latin-ASCII")

    # 4) normalise punctuation to spaces (incl. hyphens, slashes, SA click symbols)
    x <- gsub("[!ǃǀǁǂ]", " ", x)
    x <- gsub("[-_/.,]+", " ", x)
    x <- gsub("\\s+", " ", x)

    # 5) standardise common variants
    x <- stringr::str_replace_all(x, "\\bdoctor\\b", "dr")
    x <- stringr::str_replace_all(x, "\\bsaint\\b", "st")

    # 6) drop common “furniture” words while spaces still exist
    common_words <- c(
        "municipality", "district", "city", "metro", "metropolitan",
        "region", "subdistrict", "sub-district", "local", "lm", "of", "the", "greater", "north"
    )
    pat <- paste0("\\b(", paste(common_words, collapse = "|"), ")\\b")
    x <- stringr::str_replace_all(x, pat, " ")

    # 7) drop standalone numbers (e.g., "north 1" -> "north")
    x <- stringr::str_replace_all(x, "\\b\\d+\\b", " ")

    # (optional) if you also want to drop roman numerals like "ii", "iii", etc.:
    # x <- stringr::str_replace_all(x, "\\b(i|ii|iii|iv|v|vi|vii|viii|ix|x)\\b", " ")

    # 8) squeeze and trim
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)

    # 9) final canonical key: letters+digits only
    key <- gsub("[^a-z0-9]", "", x)
    key[key == ""] <- NA_character_
    key
}

#normalize("ekurhuleni north 1 ")

#RecordLinkage::jarowinkler(normalize("ekurhuleni north 1 "), "ekurhuleni north 1 ")

#stringdist::amatch(normalize("ekurhuleni north 1 "), "ekurhuleni north 1 ", method = "osa", maxDist = 2)
