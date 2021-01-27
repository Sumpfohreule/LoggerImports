#' Read data from an Envilog file in a usable form
#'
#' @param path A string as a path to the Envilog file
#' @export
readEnvilog <- function(path) {
    path <- iconv(path, to = "latin1")
    data <- data.table::as.data.table(
        tryCatch(read.csv(path, skip = 1, fileEncoding = "cp1258"),
                 error = function(e) read.csv2(path, skip = 1, fileEncoding = "cp1258")))
    data[, No := NULL]
    data <- data[Time != ""]
    col.names <- names(data)
    col.names <- stringr::str_replace(col.names,
                                      pattern = "KK|K[^(R|L)]",
                                      replacement = "_X")
    col.names <- stringr::str_replace(col.names,
                                      pattern = "KR",
                                      replacement = "_Y")
    col.names <- stringr::str_replace(col.names,
                                      pattern = "KL|[.]L[.]",
                                      replacement = "_Z")
    col.names <- stringr::str_replace(col.names,
                                      pattern = "C",
                                      replacement = "_T_PF")
    col.names <- stringr::str_replace(col.names,
                                      pattern = "pF",
                                      replacement = "_MP")
    col.names <- stringr::str_replace(col.names,
                                      pattern = ".*((?<!T_)MP|T_PF).*([XYZ]).*([0-9]{2}).*",
                                      replacement = "\\3_\\1_\\2")
    col.names[1] <- "Datum"
    setnames(data, col.names)
    data[, Datum := MyUtilities::as.POSIXctFixed(Datum, format = "%d.%m.%Y %H:%M", tz = "UTC")]
    for (col.name in names(data)[-1]) {
        data[, (col.name) := stringr::str_replace(get(col.name), "^\\D+$", "")]
        data[, (col.name) := stringr::str_replace(get(col.name), ",", ".")]
        data[, (col.name) := as.numeric(get(col.name))]
    }
    data[, Datum := lubridate::round_date(Datum, "5 mins")]
    return(data)
}
