run_app <- function() shiny::runApp("babylabdb")

get_last_export <- function() {
    
    file_paths <- list.files(here::here("data"), full.names = TRUE)
    file_names <- list.files(here::here("data"))
    
    digits <- gregexpr("[[:digit:]]+", file_names)
    dates <- as.numeric(unlist(regmatches(file_names, digits)))
    date_max_index <- which.max(dates)
    
    return(file_paths[date_max_index])
}


get_lp <- function(x) {
    
    lps <- lapply(
        strsplit(x, split = " "),
        function(x) trimws(gsub("\\.|\\(e\\*\\)", " ", x))
    )
    
    languages <- lapply(lps,  function(x) tolower(gsub("[^a-zA-Z]", "", x)))
    
    lang_1 <- sapply(languages, `[`, 1)
    lang_2 <- sapply(languages, `[`, 2)
    lang_3 <- sapply(languages, `[`, 3)
    
    does <- lapply(lps, function(x) as.numeric(stringr::str_extract_all(x, "\\d+"))/100) 
    
    doe_1 <- sapply(does, `[`, 1)
    doe_2 <- sapply(does, `[`, 2)
    doe_3 <- sapply(does, `[`, 3)
    
    mat <- data.frame(lang_1, lang_2, lang_3, doe_1, doe_2, doe_3)
    
    return(mat)
    
}


get_data <- function() {
    df_raw <- arrow::read_csv_arrow(get_last_export(), na = "NULL")
    df_raw$sex <- tolower(df_raw$sex)
    df_raw$gestation_weeks <- as.integer(df_raw$gestation_weeks)
    df_raw$weight <- as.integer(df_raw$weight)
    df_raw$weight <- ifelse(df_raw$weight > 6000, NA_integer_, df_raw$weight)

    cols_select <- c("id_baby", "birthday", "sex", "gestation_weeks", "weight", "language_profile_text")
    df_raw <- df_raw[!df_raw$blocked & df_raw$gestation_weeks < 50 & df_raw$birthday >= as.Date("2000-01-01"), cols_select]
    
    df_lps <- get_lp(df_raw$language_profile_text)
    
    df_main <- cbind(
        df_raw[, !(colnames(df_raw) %in% "language_profile_text")],
        df_lps
    )
    
    df_main$age_months <- lubridate::time_length(difftime(lubridate::today(), df_main$birthday), "months")

    df_main <- df_main[order(df_main$id_baby, decreasing = TRUE), ]
    df_main <- df_main[c("id_baby", "sex", "birthday", "age_months", "gestation_weeks", "weight", "lang_1", "lang_2", "lang_3", "doe_1", "doe_2", "doe_3")]
    
    df_main <- tibble:::as_tibble(df_main)
    
    return(df_main)
}
