########################################################################################################################
#' Reads data from an ADLM xlsx file
#' @param path Path to the ADLM file to read
#' @export
readADLM <- function(path) {
    path %>%
        openxlsx::read.xlsx() %>%
        slice(-1) %>%
        mutate(Datum = openxlsx::convertToDateTime(Datum)) %>%
        lubridate::force_tz("UTC") %>%
        mutate(Datum = lubridate::round_date(Datum, "1 mins")) %>%
        mutate(across(!Datum, as.numeric)) %>%
        arrange(Datum)
}
