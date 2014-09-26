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

    res <- lapply(seq(length(movies)), function (iFilm) {
        iSeance <- 1
        planing <- cbind(iFilm, iSeance)
        previousFilms <- NULL
        while (length(previousFilms)!=length(movies)) {
            if (is.null(previousFilms)) {
                nextFilms <- setdiff(seq(length(movies)), iFilm)
                previousFilms <- iFilm
            } else {
                nextFilms <- setdiff(seq(length(movies)), previousFilms)
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

shinyServer(function (input, output, session) {
    movieFiles <- setdiff(list.files("www", full.names = TRUE), "www/listCinema.txt")
    files2Old <- difftime(as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S %A", tz = "CET")), as.POSIXct(file.info(movieFiles)[["mtime"]]), units = "days")>=10
    if (sum(files2Old)>0) {
        sapply(movieFiles[which(files2Old)], file.remove)
    } else {}

    timeTable <- reactive({
        today <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S %A", tz = "CET"))
        if (length(grep("www.ugc.fr", input$selectCinema))>0) {
            codeCinema <- gsub("http://www.ugc.fr/cinema.html?code=", "", input$selectCinema, fixed = TRUE)
            lastUpdate <- as.POSIXlt(file.info(paste0("www/timeTable_", codeCinema, ".txt"))[["mtime"]])
            if (is.na(lastUpdate)) {
                lastUpdate <- today - 604800*2
            } else {}
            oneWeek <- format(seq(lastUpdate, lastUpdate+604800, by = "days"), "%Y-%m-%d %H:%M:%S %A", tz = "CET")[-1]
            nextUpdate <- as.POSIXct(gsub("([^ ]*) .* ([^ ]*)", "\\1 10:00:00 \\2", grep("mercredi", oneWeek, value = TRUE)))

            if (difftime(nextUpdate, today)<0) {
                webpage <- capture.output(htmlTreeParse(readLines(tc <- textConnection(getURL(input$selectCinema)))))
                webpage <- webpage[grep("progWeek", webpage):grep("  <div class=\"Foot\">", webpage)]
                progWeek <- c(grep("BoxFilm", webpage), grep("  <div class=\"Foot\">", webpage))
                withProgress(session, min = 1, max = (length(progWeek)-1), {
                    setProgress(message = "Récupération des films et horaires...")
                    nbCores <- ifelse((length(progWeek)-1)>detectCores(), detectCores(), (length(progWeek)-1))
                    res <- mclapply(seq(length(progWeek)-1), mc.cores = nbCores, function (i) {
                        setProgress(value = i)
                        tmp <- webpage[progWeek[i]:(progWeek[i+1]-1)]
                        tmp <- gsub("&apos;", "'", tmp)
                        premiereMovie <- length(grep("<h4 class=\"ColorBlue\">Avant-première</h4>", tmp))>0
                        timeMovie <- sort(unlist(strsplit(gsub("^[ ]*: ", "", tmp[grep("<strong>.*</strong>", tmp)+1]), ", ")))
                        urlMovieTmp <- paste0("http://www.ugc.fr/", gsub(".*<a href=\"(.*)\" class=.*", "\\1", tmp[grep("<a href=\".*\" class=\"ColorBlack\">", tmp)]))
                        urlMovie <- capture.output(htmlTreeParse(readLines(tc <- textConnection(getURL(urlMovieTmp)), encoding = "UTF-8"), encoding = "UTF-8"))
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
                    dput(res, file = paste0("www/timeTable_", codeCinema, ".txt"))
                    return(res)
                })
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
            nextUpdate <- as.POSIXct(gsub("([^ ]*) .* ([^ ]*)", "\\1 09:00:00 \\2", grep("mercredi", oneWeek, value = TRUE)))

            if (difftime(nextUpdate, today)<0) {
                webpage <- readLines(tc <- textConnection(getURL(input$selectCinema)))
                webpage <- capture.output(htmlTreeParse(webpage))
                webpage <- webpage[grep("<h3 id=\"horaires\">", webpage)[1]:grep("<div id=\"footer\">", webpage)]
                progWeek <- c(grep("title=\"Voir la fiche du film [^\"]*\">", webpage), grep("<div id=\"footer\">", webpage)[1])
                nbCores <- ifelse((length(progWeek)-1)>detectCores(), detectCores(), (length(progWeek)-1))
                withProgress(session, min = 1, max = (length(progWeek)-1), {
                    setProgress(message = "Récupération des films et horaires...")
                    timeTable <- mclapply(seq(length(progWeek)-1), mc.cores = nbCores, function (i) {
                        setProgress(value = i)
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
                                result <- lapply(seq(length(langMovie)), function (i) {
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
                            sameTimeTables <- sapply(seq(length(howMuchLang)), function (iVersion) {
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
                    res <- unlist(timeTable, recursive = FALSE)
                    close(tc)
                    dput(res, file = paste0("www/timeTable_", codeCinema, ".txt"))
                    return(res)
                })
            } else {
                return(dget(file = paste0("www/timeTable_", codeCinema, ".txt")))
            }
        }
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
            for (iPlanning in seq(length(movies))[-1]) {
                where2Start <- sum(sapply(movies[seq(iPlanning-1)], nrow)+2)+1
                addDataFrame(movies[[iPlanning]], sheet, col.names = TRUE, row.names = TRUE, startRow = where2Start, startColumn = 1)
            }
            autoSizeColumn(sheet, colIndex = seq(4))
            saveWorkbook(wb, file = fileName)

            if (length(formatMovies)>1) {
                wb <- loadWorkbook(file = fileName)
                sheet <- createSheet(wb, sheetName = "Série_Films")
                iPlanning = 1
                addDataFrame( planMovies()[[iPlanning]], sheet, col.names = TRUE, row.names = TRUE, startRow = 1, startColumn = 1)
                for (iPlanning in seq(length( planMovies()))[-1]) {
                    where2Start <- sum(sapply(planMovies()[seq(iPlanning-1)], nrow)+2)+1
                    addDataFrame( planMovies()[[iPlanning]], sheet, col.names = TRUE, row.names = TRUE, startRow = where2Start, startColumn = 1)
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
})
