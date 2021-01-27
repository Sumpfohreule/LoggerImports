#' Read data from a Tinytag ".txt" file in a usable form
#'
#' @param path A string as a path to the Tinytag file
#' @export
readTinyTag <- function(txt.path) {
    tt.table <- as.data.table(read.delim(txt.path,
            header = FALSE,
            skip = 5))[, -1]
    comma.col <- unlist(lapply(tt.table, function(x) TRUE %in% stringr::str_detect(x, pattern = ",")))
    if (TRUE %in% comma.col) {
        column <- names(comma.col)[comma.col]
        tt.table[, (column) := as.numeric(stringr::str_replace(get(column), pattern = ",", replacement = "."))]
    }
    if (ncol(tt.table) == 2) {
        setnames(tt.table, c("Datum", "RegenX"))
    } else {
        num.col <- names(tt.table)[unlist(lapply(tt.table, is.numeric))]
        tt.table <- tt.table[, .(Datum = paste(V2, V3), RegenX = get(num.col))]
    }
    date.format <- MyUtilities::guessDateFormat(tt.table[1, Datum])
    tt.table[, Datum := MyUtilities::as.POSIXctFixed(
            Datum,
            tz = "UTC",
            format = date.format)]
    tt.table[, Datum := lubridate::round_date(Datum, "5 mins")]
    tt.table[, RegenX := as.numeric(stringr::str_match(RegenX, pattern = "^[0-9]+(?:\\.[0-9]+$)?"))]
    data.table::setkey(tt.table, Datum)
    return(tt.table)
}
