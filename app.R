# library(shiny)
# library(shinyIncubator)
library(RCurl)
library(XML)
library(parallel)
library(xlsx)
Sys.setenv("LANGUAGE" = "en")
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
options(menu.graphics = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)


movieCalendar <- function (lmovies, time2start, time2end, pub.overlap, pub.time, wait.time) {
    movies <- lapply(lmovies, function (lmovie) {
        movie <- as.numeric(gsub("(.+):(.+)", "\\1", lmovie))*60 + as.numeric(gsub("(.+):(.+)", "\\2", lmovie))
        startMovie <- lmovie[-1]
        tmp <- sapply(seq(length(movie)-1)+1, function (iSeance) {
            c(
                startMovie = startMovie[iSeance-1],
                endMovie = format(strptime(startMovie[iSeance-1], format = "%H:%M")+(movie[1]+pub.time)*60, format = "%H:%M"),
                START = movie[iSeance]+pub.overlap,
                END = movie[iSeance]+movie[1]+pub.time
            )
        })
        tmp <- as.data.frame(t(tmp))
        tmp[, "START"] <- as.numeric(tmp[, "START"])
        tmp[, "END"] <- as.numeric(tmp[, "END"])
        return(tmp)
    })
    time2start <- time2start*60
    time2end <- time2end*60
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 3) )>=time2start), ]})
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 4) )<=time2end), ]})
    movies <- movies[which(sapply(movies, nrow)>0)]
    movies <- lapply(movies, function (iMovie) {
        rownames(iMovie) <- NULL
        return(iMovie)
    })

    res <- lapply(seq_along(movies), function (iFilm) {
        iSeance <- 1
        planing <- cbind(iFilm, iSeance)
        previousFilms <- NULL
        while (length(previousFilms)!=length(movies)) {
            if (is.null(previousFilms)) {
                nextFilms <- setdiff(seq_along(movies), iFilm)
                previousFilms <- iFilm
            } else {
                nextFilms <- setdiff(seq_along(movies), previousFilms)
            }
            whichnextTmp <- lapply(nextFilms, function (iNextFilms) {
                diffMovie <- movies[[iNextFilms]][, "START"] - movies[[iFilm]][iSeance, "END"]
                if (any(diffMovie>0, na.rm = TRUE)) {
                    return(cbind(diffMovie[which(diffMovie==min(diffMovie[diffMovie>0]))], which(diffMovie==min(diffMovie[diffMovie>0]))))
                } else {
                    return(NULL)
                }
            })
            whichnext <- do.call("rbind", whichnextTmp)
            noSeance <- sapply(whichnextTmp, is.null)
            names(noSeance) <- nextFilms
            if (any(!noSeance)) {
                if (nrow(whichnext)>1) {
                    iFilmTmp <- as.numeric(names(noSeance[!noSeance][which(whichnext[, 1] == min(whichnext[, 1], na.rm = TRUE))[1]]))
                    iSeanceTmp <- whichnext[which(whichnext[, 1] == min(whichnext[, 1], na.rm = TRUE))[1], 2]
                } else {
                    iFilmTmp <- nextFilms[!noSeance]
                    iSeanceTmp <- whichnext[2]
                }
                planing <- rbind(planing, c(iFilmTmp, iSeanceTmp))
                previousFilms <- c(previousFilms, iFilmTmp)
                iFilm <- iFilmTmp
                iSeance <- iSeanceTmp
            } else {
                previousFilms <- c(previousFilms, nextFilms)
            }
        }
        Plan <- data.frame(t(apply(planing, 1, function (filmSeance) {
            film <- filmSeance[1]
            Seance <- filmSeance[2]
            return(c(unlist(movies[[film]][Seance, ])))
        })), stringsAsFactors = FALSE, row.names = names(movies)[planing[, "iFilm"]])
        for (jFilm in seq(nrow(Plan)-1)) {
            Plan[jFilm, "Delai"] <- as.numeric(Plan[jFilm+1, 3])-as.numeric(Plan[jFilm, 4])-pub.overlap
        }
        finalPlan <- Plan[, c("startMovie", "endMovie", "Delai")]
        colnames(finalPlan) <- c("START", "END", "Delai")
        finalPlan[, "Delai"] <- paste0(finalPlan[, "Delai"], " min")
        finalPlan[nrow(finalPlan), "Delai"] <- ""
        return(finalPlan)
    })
    res <- lapply(res, na.exclude)
    res <- lapply(res, function (iPlan) {
        iPlan[, "Delai"] <- as.numeric(gsub(" min", "", iPlan[, "Delai"]))
        if (nrow(iPlan)>1) {
            howMuchTime <- iPlan[!is.na(iPlan[, "Delai"]), "Delai"]<wait.time
            if (!is.na(howMuchTime[1])) {
                if (sum(howMuchTime)!=0) {
                    if (length(howMuchTime)>1) {
                        where2Start <- grep(TRUE, howMuchTime)[1]
                        where2Stop <- where2Start + grep(FALSE, howMuchTime[where2Start:length(howMuchTime)])[1] - 1
                        return(iPlan[where2Start:ifelse(is.na(where2Stop), length(howMuchTime), where2Stop), ])
                    } else {
                        return(iPlan[which(howMuchTime):(which(howMuchTime)+1), ])
                    }
                } else {
                    return(iPlan[1, ])
                }
            } else {}
        } else {
            return(iPlan)
        }
    })
    res <- res[!sapply(res, is.null)]
    res <- lapply(res, function (iPlan) {
        iPlan[, "Delai"] <- paste0(iPlan[, "Delai"], " min")
        iPlan[nrow(iPlan), "Delai"] <- ""
        colnames(iPlan) <- c("Début", "Fin", "Délai")
        return(iPlan)
    })
    res <- unique(res)
    isOneRow <- sapply(res, nrow)==1
    if (sum(isOneRow)>=2) {
        res <- c(res[!isOneRow], list(as.data.frame(do.call("rbind", res[isOneRow]))))
    } else {}

    orderRes <- order(sapply(res, function (iTab) {
        tmp <- gsub(" min", "", iTab[, 3])
        sum(as.numeric(tmp), na.rm = TRUE)
    }))
    return(res[orderRes])
}


movieTimeTable <- function (lmovies, time2start, time2end, pub.overlap, pub.time) {
    movies <- lapply(lmovies, function (lmovie) {
        movie <- as.numeric(gsub("(.+):(.+)", "\\1", lmovie))*60 + as.numeric(gsub("(.+):(.+)", "\\2", lmovie))
        startMovie <- lmovie[-1]
        tmp <- sapply(seq(length(movie)-1)+1, function (iSeance) {
            c(
                startMovie = startMovie[iSeance-1],
                endMovie = format(strptime(startMovie[iSeance-1], format = "%H:%M")+(movie[1]+pub.time)*60, format = "%H:%M"),
                START = movie[iSeance]+pub.overlap,
                END = movie[iSeance]+movie[1]+pub.time
            )
        })
        tmp <- as.data.frame(t(tmp))
        tmp[, "START"] <- as.numeric(tmp[, "START"])
        tmp[, "END"] <- as.numeric(tmp[, "END"])
        return(tmp)
    })
    time2start <- time2start*60
    time2end <- time2end*60
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 3) )>=time2start), ]})
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 4) )<=time2end), ]})
    movies <- movies[which(sapply(movies, nrow)>0)]
    movies <- lapply(movies, function (iMovie) {
        rownames(iMovie) <- NULL
        return(iMovie)
    })
    Movies <- lapply(movies, "[", c(1, 2))
    Movies <- lapply(Movies, function (i) {colnames(i) <- c("START", "END"); return(i)})
    Movies <- lapply(names(Movies), function (iName) {
        rownames(Movies[[iName]]) <- paste0(iName, " (", seq(nrow(Movies[[iName]])), ")")
        colnames(Movies[[iName]]) <- c("Début", "Fin")
        return(Movies[[iName]])
    })
    return(Movies)
}


getTimeTableUGC <- function (url) {
    webpage <- capture.output(htmlTreeParse(readLines(tc <- textConnection(getURL(url, .encoding = "utf-8")), encoding = "utf-8"), encoding = "utf-8"))
    webpage <- webpage[grep("progWeek", webpage):grep("  <div class=\"Foot\">", webpage)]
    webpage <- iconv(webpage, "UTF-8", "UTF-8")
    progWeek <- c(grep("BoxFilm", webpage), grep("  <div class=\"Foot\">", webpage))

    # nbCores <- 4 # ifelse((length(progWeek)-1)>detectCores(), detectCores(), (length(progWeek)-1))
    # if (Sys.info()[["sysname"]] != "Linux") {
        # nbCores <- 1
    # } else {
        # nbCores <- min(detectCores(), nbCores)
    # }
    # print(nbCores)
    # timeTable <- mclapply(seq(length(progWeek)-1), mc.cores = nbCores, function (i) {
    timeTable <- lapply(seq(length(progWeek)-1), function (i) {
        cat(". ")
        tmp <- webpage[progWeek[i]:(progWeek[i+1]-1)]
        tmp <- gsub("&apos;", "'", tmp)
        premiereMovie <- length(grep("<h4 class=\"ColorBlue\">Avant-première</h4>", tmp))>0
        timeMovie <- sort(unlist(strsplit(gsub("^[ ]*: ", "", tmp[grep("<strong>.*</strong>", tmp)+1]), ", ")))
        urlMovieTmp <- paste0("http://www.ugc.fr/", gsub(".*<a href=\"(.*)\" class=.*", "\\1", tmp[grep("<a href=\".*\" class=\"ColorBlack\">", tmp)]))
        urlMovie <- capture.output(htmlTreeParse(readLines(tc <- textConnection(getURL(urlMovieTmp)), encoding = "UTF-8"), encoding = "UTF-8"))
        urlMovie <- iconv(urlMovie, "UTF-8", "UTF-8")
        detailsMovie <- urlMovie[grep("<div class=\"FilmDetail\">", urlMovie):grep("<p class=\"FilmDetailText Description\">", urlMovie)]
        detailsMovie <- gsub("&apos;", "'", detailsMovie)
        if (grep("<h2>", detailsMovie)==grep("</h2>", detailsMovie)) {
            titleMovie <- gsub(".*<h2>(.*)</h2>.*", "\\1", grep("<h2>.*</h2>", detailsMovie, value = TRUE))
        } else {
            rawTitle <- detailsMovie[grep("<h2>", detailsMovie):grep("</h2>", detailsMovie)]
            titleMovie <- gsub("^[ ]*(.*)[ ]*", "\\1", rawTitle[-grep("[><]", rawTitle)])
        }
        runningTimeMovie <- gsub("[ ]*([0-9]*)h([0-9]*)min", "\\1:\\2", detailsMovie[grep("<strong>Durée :</strong>", detailsMovie)+1])
        typeMovie <- gsub("[ ]*(.*)[ ]*", "\\1", detailsMovie[grep("<strong>Genre :</strong>", detailsMovie)+1])
        releaseMovie <- as.Date(gsub("[ ]*(.*)[ ]*", "\\1", detailsMovie[grep("<strong>Sortie :</strong>", detailsMovie)+1]), format = "%d %B %Y")
        testTitle <- gsub("[ ]*(.*)[ ]*", "\\1", grep(paste0('[^"]', titleMovie, '[^"]'), tmp, value = TRUE))
        testTitle <- gsub(".*<a href=\".*\" class=\"ColorBlack\">(.*)</a>", "\\1", testTitle)
        infoMovie <- gsub("[ ]*(.*)[ ]*", "\\1", gsub(titleMovie, "", testTitle, fixed = TRUE))
        infoMovie <- gsub("[ ]*-.*", "", infoMovie)
        langMovie <- gsub("\\^(.*)\\$", "\\1", names(unlist(sapply(c("^VF$", "VFSTF", "VOSTF"), grep, gsub("([^ ]*)[ ]+(.*)", "\\1",infoMovie)))))
        is3D <- length(grep("3D", gsub("([^ ]*)[ ]+(.*)", "\\2",infoMovie), fixed = TRUE))>0
        result <- list(paste(titleMovie, paste0("(", langMovie, ifelse(is3D, "-3D)", ")"))), langMovie, is3D, premiereMovie, releaseMovie, typeMovie, c(runningTimeMovie, timeMovie))
        names(result) <- c("Title", "Language", "3D", "Premiere", "Release", "Type", titleMovie)
        return(result)
    })
    close(tc)
    cat("\n")
    return(timeTable)
}


getTimeTableLille <- function (url) {
    webpage <- readLines(tc <- textConnection(getURL(url)))
    webpage <- capture.output(htmlTreeParse(webpage))
    webpage <- webpage[grep("<h3 id=\"horaires\">", webpage)[1]:grep("<div id=\"footer\">", webpage)]
    webpage <- iconv(webpage, "UTF-8", "UTF-8")
    progWeek <- c(grep("title=\"Voir la fiche du film [^\"]*\">", webpage), grep("<div id=\"footer\">", webpage)[1])

    # nbCores <- 4 # ifelse((length(progWeek)-1)>detectCores(), detectCores(), (length(progWeek)-1))
    # if (Sys.info()[["sysname"]] != "Linux") {
        # nbCores <- 1
    # } else {
        # nbCores <- min(detectCores(), nbCores)
    # }
    # timeTable <- mclapply(seq(length(progWeek)-1), mc.cores = nbCores, function (i) {
    timeTable <- lapply(seq(length(progWeek)-1), function (i) {
        cat(". ")
        tmpWebpage <- webpage[progWeek[i]:(progWeek[i+1]-1)]
        releaseMovie <- as.Date(gsub(".*>(.*)<.*", "\\1", tmpWebpage[grep("horaires-sortie", tmpWebpage)+2]), format = "%d/%m/%Y")
        runningTimeMovie <- paste0("0", gsub("h", ":", gsub(".*>(.*)<.*", "\\1", tmpWebpage[grep("horaires-duree", tmpWebpage)+2])))
        if (length(releaseMovie)!=0 & runningTimeMovie!="0") {
            premiereMovie <- FALSE
            titleMovie <- gsub("&apos;", "'", gsub(".*>(.*)<.*", "\\1", grep("title=\"Voir la fiche du film [^\"]*\">", tmpWebpage, value = TRUE)))
            typeMovie <- gsub("&apos;", "'", gsub(".*>(.*)<.*", "\\1", tmpWebpage[grep("Genre", tmpWebpage)+1]))
            is3D <- ifelse(length(grep("EN 3D", titleMovie))>0, TRUE, FALSE)
            titleMovie <- gsub(" EN 3D", "", titleMovie)

            howMuchLang <- grep("Les sÃ©ances en version", tmpWebpage, value = TRUE)
            langMovie <- lapply(howMuchLang, function (i) {
                langMovie <- ifelse(length(grep("franÃ§aise", i))>0, "VF", "VOSTF")
            })
            if (length(howMuchLang)>1) {
                timeMovie <- lapply(grep("fc today", tmpWebpage)+1, function (i) {
                    timeMovie <- gsub("h", ":", gsub(".*>(.*)<.*", "\\1", tmpWebpage[i]))
                })
                result <- lapply(seq_along(langMovie), function (i) {
                    result <- list(paste(titleMovie, paste0("(", langMovie[[i]], ifelse(is3D, "-3D)", ")"))), langMovie[[i]], is3D, premiereMovie, releaseMovie, typeMovie, c(runningTimeMovie, timeMovie[[i]]))
                    names(result) <- c("Title", "Language", "3D", "Premiere", "Release", "Type", titleMovie)
                    return(result)
                })
            } else {
                timeMovie <- gsub("h", ":", gsub(".*>(.*)<.*", "\\1", tmpWebpage[grep("fc today", tmpWebpage)+1]))
                result <- list(paste(titleMovie, paste0("(", langMovie, ifelse(is3D, "-3D)", ")"))), langMovie[[1]], is3D, premiereMovie, releaseMovie, typeMovie, c(runningTimeMovie, timeMovie))
                names(result) <- c("Title", "Language", "3D", "Premiere", "Release", "Type", titleMovie)
                result <- list(result)
            }
            sameTimeTables <- sapply(seq_along(howMuchLang), function (iVersion) {
                posScheduled <- c(sapply(seq(7), function (iCol) {grep(paste0("<td class=\"col", iCol, " dt-"), tmpWebpage)[iVersion]}), grep("</tbody>", tmpWebpage)[iVersion])
                timeTables <- lapply(seq(length(posScheduled)-1), function (iDay) {
                    tmp <- tmpWebpage[posScheduled[iDay]:posScheduled[iDay+1]]
                    res <- gsub("h", ":", gsub(".*>(.*)<.*", "\\1", tmp[grep("<div class=\"fc", tmp)+1]))
                    return(res)
                })
                return(sum(duplicated(timeTables))==length(timeTables)-1)
            })
            return(result[sameTimeTables])
        } else {
            return(list())
        }
    })
    timeTable <- unlist(timeTable, recursive = FALSE)
    close(tc)
    cat("\n")
    return(timeTable)
}


getUGC <- function () {
    url <- "http://www.ugc.fr/home.html"
    webpage <- readLines(tc <- textConnection(getURL(url)))
    webpage <- iconv(webpage, "UTF-8", "UTF-8")
    close(tc)
    tmpWebpage <- webpage[grep("<div class='Foot'> <div class='InnerFoot'> <h2>TOUS LES CINEMAS</h2>", webpage):length(webpage)]
    webpage <- capture.output(htmlTreeParse(tmpWebpage))
    webpage <- webpage[1:grep("<div class=\"FootNav\">", webpage)]
    listCinemaPage <- c(grep("<a href=", webpage), length(webpage))
    listCinema <- sapply(seq(length(listCinemaPage)-1), function (i)  {
        tmp <- webpage[listCinemaPage[i]:(listCinemaPage[i+1]-1)]
        res <- gsub(".*<a href=\"(cinema.html\\?code=.*)\">", "http://www.ugc.fr/\\1", tmp[1])
        names(res) <- gsub("Ciné Cité ", "", gsub("&apos;", "'", gsub(".*<strong>(.*)</strong>", "\\1", tmp[2])))
        return(res)
    })
    return(as.list(listCinema))
}

dir.create("www") # right issues
if (file.exists("www/listCinema.txt")) {
    listCinema <- dget(file = "www/listCinema.txt")
} else {
    listCinema <- c("Le Metropole - Lille" = "http://www.lemetropolelille.com/horaires/", "Le Majestic - Lille" = "http://www.lemajesticlille.com/horaires/", getUGC())
    listCinema <- listCinema[order(names(listCinema))]
    dput(listCinema, file = "www/listCinema.txt")
}


server <- function (input, output, session) {
    movieFiles <- setdiff(list.files("www", full.names = TRUE), "www/listCinema.txt")
    files2Old <- difftime(as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S %A", tz = "CET")), as.POSIXct(file.info(movieFiles)[["mtime"]]), units = "days")>=10
    if (sum(files2Old)>0) {
        sapply(movieFiles[which(files2Old)], file.remove)
    } else {}

    timeTable <- reactive({
        withProgress(message = "Chargement...", value = NULL, {
            today <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S %A", tz = "CET"))
            if (length(grep("www.ugc.fr", input$selectCinema))>0) {
                codeCinema <- gsub("http://www.ugc.fr/cinema.html?code=", "", input$selectCinema, fixed = TRUE)
                lastUpdate <- as.POSIXlt(file.info(paste0("www/timeTable_", codeCinema, ".txt"))[["mtime"]])
                if (is.na(lastUpdate)) {
                    lastUpdate <- today - 604800*2
                } else {}
                oneWeek <- format(seq(lastUpdate, lastUpdate+604800, by = "days"), "%Y-%m-%d %H:%M:%S %A", tz = "CET")[-1]
                nextUpdate <- as.POSIXct(gsub("([^ ]*) .* ([^ ]*)", "\\1 10:00:00 \\2", grep("Wednesday", oneWeek, value = TRUE))) # mercredi
                if (difftime(nextUpdate, today)<0) {
                    res <- getTimeTableUGC(input$selectCinema)
                    dput(res, file = paste0("www/timeTable_", codeCinema, ".txt"))
                    return(res)
                } else {
                    return(dget(file = paste0("www/timeTable_", codeCinema, ".txt")))
                }
            } else {
                codeCinema <- gsub("http://www\\.(.*)\\..*", "\\1", input$selectCinema)
                lastUpdate <- as.POSIXlt(file.info(paste0("www/timeTable_", codeCinema, ".txt"))[["mtime"]])
                if (is.na(lastUpdate)) {
                    lastUpdate <- today - 604800*2
                } else {}
                oneWeek <- format(seq(lastUpdate, lastUpdate+604800, by = "days"), "%Y-%m-%d %H:%M:%S %A", tz = "CET")[-1]
                nextUpdate <- as.POSIXct(gsub("([^ ]*) .* ([^ ]*)", "\\1 09:00:00 \\2", grep("Wednesday", oneWeek, value = TRUE))) # mercredi

                if (difftime(nextUpdate, today)<0) {
                    res <- getTimeTableLille(input$selectCinema)
                    dput(res, file = paste0("www/timeTable_", codeCinema, ".txt"))
                    return(res)
                } else {
                    return(dget(file = paste0("www/timeTable_", codeCinema, ".txt")))
                }
            }
        })
    })

    output$whichCinema <- renderUI({
        listCinema <- dget(file = "www/listCinema.txt")
        return(h2(paste0("Films à l'affiche (", names(grep(input$selectCinema, listCinema, fixed = TRUE, value = TRUE)), ")"), style = "color: RGBa(238,48,167,1)"))
    })

    output$ui <- renderUI({
        validate(need(any(c("VF", "VOSTF", "VFSTF")%in%input$lang3D), "Veuillez sélectionner au moins une langue!"))
        tmpList <- unlist(sapply(timeTable(), function (iMovie) {
            cond1 <- iMovie[["Language"]]%in%input$lang3D
            cond2 <- paste0("3D"%in%input$lang3D, iMovie[["3D"]])%in%c(paste0(TRUE, TRUE), paste0(TRUE, FALSE), paste0(FALSE, FALSE))
            cond3 <- !iMovie[["Premiere"]]
            if ("Release7"%in%input$lang3D) {
                releaseDate <- strptime(iMovie[["Release"]], "%Y-%m-%d")
                todayDate <- strptime(format(Sys.time(), "%Y-%m-%d"), "%Y-%m-%d")
                cond4 <- todayDate-releaseDate>=0 & todayDate-releaseDate<7
            } else {
                cond4 <- TRUE
            }
            if (cond1 & cond2 & cond3 & cond4) {
                return(iMovie[["Title"]])
            } else {}
        }))
        movieList <- as.list(tmpList)
        names(movieList) <- tmpList
        return(checkboxGroupInput("whichMovie", label = "", choices = movieList, selected = names(movieList)))
    })

    output$lastUpdate <- renderUI({
        codeCinema <- gsub("http://www.ugc.fr/cinema.html?code=", "", input$selectCinema, fixed = TRUE)
        lastUpdate <- as.POSIXlt(file.info(paste0("www/timeTable_", codeCinema, ".txt"))[["mtime"]])
        today <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S %A", tz = "CET"))
        if (is.na(lastUpdate)) {
            lastUpdate <- format(today, "Dernière mise à jour effectuée le <strong>%A %d %B %Y à %Hh%M</strong>")
        } else {
            lastUpdate <- format(lastUpdate, "Dernière mise à jour effectuée le <strong>%A %d %B %Y à %Hh%M</strong>")
        }
        return(HTML(lastUpdate))
    })

    planMovies <- reactive({
        formatMovies <- sapply(timeTable()[which(sapply(timeTable(), "[", 1)%in%input$whichMovie)], "[", 7)
        mNames <- names(formatMovies)
        formatMovies <- lapply(names(formatMovies), function (iDupMovie) {
            unique(sort(unname(unlist(formatMovies[grep(iDupMovie, names(formatMovies))]))))
        })
        names(formatMovies) <- mNames
        formatMovies <- formatMovies[!duplicated(formatMovies)]
        if (input$WaitTime>=0) {
            pubOverlap <- input$AdsTime
            pubTime <- input$AdsTime
            waitTime <- input$WaitTime
        } else {
            pubOverlap <- 15-abs(input$WaitTime)
            pubTime <- input$AdsTime
            waitTime <- input$WaitTime
        }
        if (length(formatMovies)==1) {
            validate(need(try(planMovies <- movieTimeTable(lmovies = formatMovies, time2start = input$Interval[1], time2end = input$Interval[2], pub.overlap = pubOverlap, pub.time = pubTime), silent = TRUE), ""))
        } else {
            validate(need(try(planMovies <- movieCalendar(formatMovies, time2start = input$Interval[1], time2end = input$Interval[2], pub.overlap = pubOverlap, pub.time = pubTime, wait.time = waitTime), silent = TRUE), ""))
            if (all(sapply(planMovies, nrow)==1)) {
                planMovies <- as.data.frame(do.call("rbind", planMovies))
            } else {}
        }
        return(planMovies)
    })

    output$downloadData <- downloadHandler(
        filename = "planMovies.xlsx",
        content = function (file) {
            fileName <- paste0(file, ".xlsx")
            formatMovies <- sapply(timeTable()[which(sapply(timeTable(), "[", 1)%in%input$whichMovie)], "[", 7)
            mNames <- names(formatMovies)
            formatMovies <- lapply(names(formatMovies), function (iDupMovie) {
                unique(sort(unname(unlist(formatMovies[grep(iDupMovie, names(formatMovies))]))))
            })
            names(formatMovies) <- mNames
            formatMovies <- formatMovies[!duplicated(formatMovies)]
            if (input$WaitTime>=0) {
                pubOverlap <- input$AdsTime
                pubTime <- input$AdsTime
                waitTime <- input$WaitTime
            } else {
                pubOverlap <- 15-abs(input$WaitTime)
                pubTime <- input$AdsTime
                waitTime <- input$WaitTime
            }
            movies <- movieTimeTable(lmovies = formatMovies, time2start = input$Interval[1], time2end = input$Interval[2], pub.overlap = pubOverlap, pub.time = pubTime)
            write.xlsx(movies[[1]], file = fileName,  col.names = TRUE, row.names = TRUE, sheetName = "Horaires")
            wb <- loadWorkbook(file = fileName)
            sheet <- getSheets(wb)[["Horaires"]]
            autoSizeColumn(sheet, colIndex = seq(3))
            for (iPlanning in seq_along(movies)[-1]) {
                where2Start <- sum(sapply(movies[seq(iPlanning-1)], nrow)+2)+1
                addDataFrame(movies[[iPlanning]], sheet, col.names = TRUE, row.names = TRUE, startRow = where2Start, startColumn = 1)
            }
            autoSizeColumn(sheet, colIndex = seq(4))
            saveWorkbook(wb, file = fileName)

            if (length(formatMovies)>1) {
                wb <- loadWorkbook(file = fileName)
                sheet <- createSheet(wb, sheetName = "Série_Films")
                iPlanning = 1
                addDataFrame(planMovies()[[iPlanning]], sheet, col.names = TRUE, row.names = TRUE, startRow = 1, startColumn = 1)
                for (iPlanning in seq_along(planMovies())[-1]) {
                    where2Start <- sum(sapply(planMovies()[seq(iPlanning-1)], nrow)+2)+1
                    addDataFrame(planMovies()[[iPlanning]], sheet, col.names = TRUE, row.names = TRUE, startRow = where2Start, startColumn = 1)
                }
                autoSizeColumn(sheet, colIndex = seq(4))
                saveWorkbook(wb, file = fileName)
            } else {}
            file.rename(fileName, file)
            return(invisible())
        }
    )

    lapply(seq(5), function(i) {
        output[[paste0('res', i)]] <- renderTable({
            if (!is.data.frame(planMovies())) {
                validate(need(length(planMovies())>=i, ""))
                return(planMovies()[[i]])
            } else {
                if (i==1) {
                    validate(need(is.data.frame(planMovies()), ""))
                    return(planMovies()[, -3])
                } else {
                    return(validate(need(is.data.frame(planMovies()), "")))
                }
            }
        })
    })
    return(output)
}


ui <- shinyUI(fluidPage(
    tags$head(
        tags$style(
            HTML("
                /*@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');*/
                @font-face {
                    font-family: 'Cabin';
                    font-style: normal;
                    font-weight: 400;
                    src: local('Cabin Regular'), local('Cabin-Regular'), url(http://fonts.gstatic.com/s/cabin/v6/K6ngFdK5haaaRGBV8waDwA.ttf) format('truetype');
                }
                @font-face {
                    font-family: 'Cabin';
                    font-style: normal;
                    font-weight: 700;
                    src: local('Cabin Bold'), local('Cabin-Bold'), url(http://fonts.gstatic.com/s/cabin/v6/nHiQo1BypvYzt95zlPq1TvesZW2xOQ-xsNqO47m55DA.ttf) format('truetype');
                }
                @font-face {
                    font-family: 'Lobster';
                    font-style: normal;
                    font-weight: 400;
                    src: local('Lobster'), url(http://fonts.gstatic.com/s/lobster/v9/TSDaXhyJuDJ-NBU0popSWA.ttf) format('truetype');
                }
                h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: lighter;
                    font-size: 400%;
                    color: RGBa(30,144,255,1);
                }
                h2 {
                    font-family: 'Lobster', cursive;
                    font-weight: lighter;
                }
                .btn {
                    display: inline-block;
                    margin-bottom: 2%;
                    margin-top: 2%;
                    text-align: center;
                    vertical-align: middle;
                    cursor: pointer;
                    background-image: none;
                    border: 1px solid transparent;
                    white-space: nowrap;
                    padding: 8px 12px;
                    font-size: 16px;
                    line-height: 1.42857143;
                    border-radius: 4px;
                    -webkit-user-select: none;
                    -moz-user-select: none;
                    -ms-user-select: none;
                    user-select: none;
                    color: RGBa(0,0,0,1);
                    background-color: RGBa(30,144,255,1);
                    border-color: RGBa(30,144,255,1);
                    font-family: 'Lobster', cursive;
                    font-weight: lighter;
                }
                .btn:hover,
                .btn:focus,
                .btn:active,
                .btn.active,
                .open > .dropdown-toggle.btn {
                    color: RGBa(0,0,0,1);
                    background-color: RGBa(30,144,255,0.8);
                    border-color: RGBa(30,144,255,0.7);
                }
                .data, .table, .table-bordered, .table-condensed {
                    background-color: RGBa(255,255,255,1);
                }
            ")
        )
    ),
    headerPanel(
        h1("Des Films en série (ou un seul)!",
            a("(https://github.com/mcanouil/MovieCalendar)", href = "https://github.com/mcanouil/MovieCalendar", style = "font-size: 25%; color: RGBa(238,180,34,1)"),
            p("Note: Le chargement nécessite quelques minutes!", style = "font-size: 25%; padding-left: 10px"),
        style = "padding-top: 10px;"), "Des Films en série!"),
    fluidRow(
        column(12, wellPanel(
            fluidRow(h2("Paramètres", style = "color: RGBa(0,205,102,1)"),
                div(
                    column(4,
                       selectInput("selectCinema", label = strong("Choix du cinéma"), choices = listCinema, selected = listCinema[[grep("UGC Lille", names(listCinema))]], width = "100%")
                    ),
                    column(4,
                        checkboxGroupInput("lang3D", label = strong("Langues et 3D"),
                            choices = list("Nouveauté" = "Release7", "VF" = "VF", "VOSTF" = "VOSTF", "VFSTF" = "VFSTF", "3D" = "3D"),
                            selected = c("VF", "VOSTF", "3D"), inline = TRUE)
                    ),
                    column(4,
                        sliderInput("Interval", strong("Plage horaire (en heures)"), min = 10, max = 25, value = c(19, 25), step = 0.5, width = "100%")
                    ),
                align = "center")
            ),
            fluidRow(div(
                column(3, sliderInput("AdsTime", strong("Temps de publicités (en minutes)"), min = 0, max = 15, value = 15, step = 1, width = "100%")),
                column(3, em(helpText(strong("Note:"), br(), "Le temps de publicité est d'environ", strong("15 minutes", style = "color: RGBa(238,180,34,1)"), "pour les cinémas UGC"))),
                column(3, sliderInput("WaitTime", strong("Temps d'attente maximum (en minutes)"), min = -15, max = 30, value = 5, step = 5, width = "100%")),
                column(3, em(helpText(strong("Note:"), br(), "Une valeur négative permet d'éviter les publicités précédant le second film"))),
            align = "center")),
            style = "border-color: RGBa(0,205,102,1)"
        ))
    ),

    fluidRow(
        column(6, wellPanel(
            htmlOutput("whichCinema"),
            em(htmlOutput("lastUpdate"), style = "color: RGBa(238,180,34,1); font-size: 110%"),
            htmlOutput("ui"), style = "border-color: RGBa(238,48,167,1)"
        )),
        column(6, wellPanel(
            fluidRow(
                column(6, h2("Résultats", style = "color: RGBa(238,44,44,1)")),
                column(6, downloadButton('downloadData', 'Télécharger'), align = "right")
            ),fluidRow(
                lapply(seq(5), function(i) {
                    div(htmlOutput(paste0("res", i)), align = "center")
                })
            ), style = "border-color: RGBa(238,44,44,1)"
        ))
    )
))

shinyApp(ui = ui, server = server)