read_metainfo <- function (file, markup = NULL) 
{
    x <- readLines(file)
    if (is.null(markup)) 
        markup <- switch(tolower(tools::file_ext(file)), tex = "latex", 
            rtex = "latex", rnw = "latex", md = "markdown", rmd = "markdown")
    extype <- match.arg(extract_command(x, "extype", markup = markup), 
        c("schoice", "mchoice", "num", "string", "cloze"))
    exname <- extract_command(x, "exname", markup = markup)
    extitle <- extract_command(x, "extitle", markup = markup)
    exsection <- extract_command(x, "exsection", markup = markup)
    exversion <- extract_command(x, "exversion", markup = markup)
    exsolution <- extract_command(x, "exsolution", markup = markup)
    # extol <- extract_command(x, "extol", "numeric", markup = markup)
    extol <- extract_command(x, "extol", "character", markup = markup)
    exclozetype <- extract_command(x, "exclozetype", markup = markup)
    expoints <- extract_command(x, "expoints", "numeric", markup = markup)
    extime <- extract_command(x, "extime", "numeric", markup = markup)
    exshuffle <- extract_command(x, "exshuffle", "character", 
        markup = markup)
    exsingle <- extract_command(x, "exsingle", "logical", markup = markup)
    exmaxchars <- extract_command(x, "exmaxchars", markup = markup)
    exabstention <- extract_command(x, "exabstention", markup = markup)
    exextra <- extract_extra(x, markup = markup)
    exshuffle <- if (is.null(exshuffle)) {
        FALSE
    }
    else if (is.na(suppressWarnings(as.numeric(exshuffle)))) {
        as.logical(exshuffle)
    }
    else {
        as.numeric(exshuffle)
    }
    slength <- length(exsolution)
    if (slength < 1L) 
        stop("no exsolution specified")
    exsolution <- switch(extype, schoice = string2mchoice(exsolution, 
        single = !is.numeric(exshuffle)), mchoice = string2mchoice(exsolution), 
        num = as.numeric(exsolution), string = exsolution, cloze = {
            if (is.null(exclozetype)) {
                warning("no exclozetype specified, taken to be string")
                exclozetype <- "string"
            }
            if (length(exclozetype) > 1L & length(exclozetype) != 
                slength) warning("length of exclozetype does not match length of \\exsolution{}")
            exclozetype <- rep(exclozetype, length.out = slength)
            exsolution <- as.list(exsolution)
            for (i in 1L:slength) exsolution[[i]] <- switch(match.arg(exclozetype[i], 
                c("schoice", "mchoice", "num", "string", "verbatim")), 
                schoice = string2mchoice(exsolution[[i]], single = TRUE), 
#                mchoice = string2mchoice(exsolution[[i]]), num = as.numeric(exsolution[[i]]), 
                mchoice = string2mchoice(exsolution[[i]]), num = exsolution[[i]], 
                string = exsolution[[i]], verbatim = exsolution[[i]])
            exsolution
        })
    slength <- length(exsolution)
    if (is.null(extol)) 
        extol <- 0
    extol <- rep(extol, length.out = slength)
    string <- switch(extype, schoice = paste(exname, ": ", which(exsolution), 
        sep = ""), mchoice = paste(exname, ": ", paste(if (any(exsolution)) which(exsolution) else "-", 
        collapse = ", "), sep = ""), num = if (max(extol) <= 
        0) {
        paste(exname, ": ", exsolution, sep = "")
    } else {
        if (slength == 1L) {
            paste(exname, ": ", exsolution, " (", exsolution - 
                extol, "--", exsolution + extol, ")", sep = "")
        } else {
            paste(exname, ": [", exsolution[1L], ", ", exsolution[2L], 
                "] ([", exsolution[1L] - extol[1L], "--", exsolution[1L] + 
                  extol[1L], ", ", exsolution[2L] - extol[2L], 
                "--", exsolution[2L] + extol[2L], "])", sep = "")
        }
    }, string = paste(exname, ": ", paste(exsolution, collapse = "\n"), 
        sep = ""), cloze = paste(exname, ": ", paste(sapply(exsolution, 
        paste, collapse = ", "), collapse = " | "), sep = ""))
    if (!is.null(expoints) & extype == "cloze") {
        expoints <- rep(expoints, length.out = slength)
    }
    if (!is.null(exmaxchars)) {
        exmaxchars <- rep(exmaxchars, length.out = slength)
        exmaxchars <- lapply(exmaxchars, function(x) {
            x <- gsub("\\s", ",", x)
            x <- strsplit(x, ",")[[1]]
            x <- x[x != ""]
            if (any(x == "NA")) 
                x[x == "NA"] <- NA
            mode(x) <- "integer"
            x
        })
        if (slength < 2) 
            exmaxchars <- exmaxchars[[1]]
    }
    rval <- list(file = tools::file_path_sans_ext(file), markup = markup, 
        type = extype, name = exname, title = extitle, section = exsection, 
        version = exversion, solution = exsolution, tolerance = extol, 
        clozetype = exclozetype, points = expoints, time = extime, 
        shuffle = exshuffle, single = exsingle, length = slength, 
        string = string, maxchars = exmaxchars, abstention = exabstention)
    rval <- c(rval, exextra)
    return(rval)
}
