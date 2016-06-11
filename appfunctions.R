parseMovieAllocine <- function (file) {
    if (length(grep("Aucune séance", file))==0) {
        runningTimeMovie <- gsub(
            "[^0-9]*([0-9]*)h ([0-9]*)min.*",
            "\\1:\\2",
            grep("([0-9]*)h ([0-9]*)min", file, value = TRUE)
        )
        typeMovie <- paste(gsub(".*>(.*)<.*", "\\1", grep('<span data-ac=\"==.*==\">', file, value = TRUE)), collapse = ";")
        releaseMovie <- gsub("^[ ]*", "", file[grep('<div class=\"meta-body-item meta-body-info\">', file)+1])
        monthsEN <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
        monthsFR <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre")
        for (i in seq_len(12)) {
            releaseMovie <- gsub(monthsFR[i], monthsEN[i], releaseMovie)
        }
        releaseMovie <- as.Date(releaseMovie, format = "%d %B %Y")
        premiereMovie <- ifelse(is.na(releaseMovie), TRUE, (releaseMovie > Sys.Date()))
        titleMovie <- gsub(
            "&amp;",
            "&",
            gsub(
                "&apos;",
                "'",
                gsub(
                    ".*>(.*)<.*",
                    "\\1",
                    grep('<span class=\"meta-title-link\"', file, value = TRUE)
                )
            )
        )
        langMovie <- gsub('.*>En (.*),.*<.*', "\\1", file[grep('div class=\"showtimes-format\">', file)+1])
        # timesMovie <- file[eval(parse(text = paste(grep("showtimeszone", file), collapse = ":")))]
        # postimes <- cbind(grep("<em data-times=", timesMovie), grep("</em>", timesMovie))

        findhoursitembooking <- grep('<span class=\"hours-item-booking\">', file)
        if (length(findhoursitembooking)==0) {
            return(NULL)
        } else {
            postimes <- cbind(
                grep('div class=\"showtimes-format\">', file),
                c(findhoursitembooking[sapply(grep('div class=\"showtimes-format\">', file), findInterval, vec = findhoursitembooking)[-1]], max(findhoursitembooking))
            )
            # if (length(langMovie)>1) {
                resulttmp <- NULL
                result <- NULL
                for (ilang in seq_along(langMovie)) {
                    timeMovie <- gsub(".*>(.*)<.*", "\\1", grep('<span class=\"hours-item-value\">', file[postimes[ilang, 1]:postimes[ilang, 2]], value = TRUE))

                    is3D <- FALSE # length(grep("3D", timesMovie[grep('<span class="bold">En .*</span>', timesMovie)+2][ilang]))!=0

                    titleMovie2 <- paste(titleMovie, paste0("(", langMovie[ilang], ifelse(is3D, "-3D)", ")")))
                    resulttmp <- list(paste(titleMovie, paste0("(", langMovie[ilang], ifelse(is3D, "-3D)", ")"))), langMovie[ilang], is3D, premiereMovie, releaseMovie, typeMovie, c(runningTimeMovie, timeMovie))
                    names(resulttmp) <- c("Title", "Language", "3D", "Premiere", "Release", "Type", titleMovie2)
                    result <- c(result, list(resulttmp))
                }
                result <- result[!duplicated(result)]

                result <- result[!sapply(result, is.null)]
                fieldLanguage <- sapply(result, "[", 2)
                field3D <- sapply(result, "[", 3)

                result <- lapply(unique(paste(fieldLanguage, field3D)), function (i) {
                    el <- result[paste(fieldLanguage, field3D)==i]
                    el[[1]][[7]] <- c(
                        sapply(el, "[", 7)[[1]][1],
                        unique(sort(unlist(sapply(sapply(el, "[", 7), "[", -1), use.names = TRUE)))
                    )
                    return(el[[1]])
                })
                result <- result[!duplicated(result)]
                return(result)
            # } else {
                # is3D <- length(grep("3D", timesMovie[grep('<span class="bold">En .*</span>', timesMovie)+2]))!=0
                # timeMovie <- rep(NA, nrow(postimes))
                # for (i in seq_len(nrow(postimes))) {
                    # timeMovietmp <- timesMovie[postimes[i, 1]:postimes[i, 2]]
                    # timeMovie[i] <- gsub(" ", "", timeMovietmp[grep("Réserver", timeMovietmp)-2])
                # }
                # titleMovie2 <- paste(titleMovie, paste0("(", langMovie, ifelse(is3D, "-3D)", ")")))
                # result <- list(paste(titleMovie, paste0("(", langMovie, ifelse(is3D, "-3D)", ")"))), langMovie, is3D, premiereMovie, releaseMovie, typeMovie, c(runningTimeMovie, timeMovie))
                # names(result) <- c("Title", "Language", "3D", "Premiere", "Release", "Type", titleMovie2)
                # return(result)
            # }
        }
    } else {
        return(NULL)
    }
}


getTimeTableAllocine <- function (url) {
    require("XML")
    webpage <- htmlTreeParse(file = url, isURL = TRUE, encoding = "UTF-8")[3]
    webpage <- capture.output(webpage[["children"]][["html"]][["body"]])
    webpage <- webpage[-seq_len(grep("Horaires et séances", webpage))]
    posstart <- grep('<div class="meta ">', webpage)
    posend <- grep('          </figure>', webpage)[-1][seq_along(posstart)]
    posmovie <- cbind(posstart, posend)
    posmovie <- posmovie[apply(posmovie, 1, diff)!=-1, ]

    timeTable <- apply(posmovie, 1, function (iMovie) {
        cat(". ")
        tmp <- parseMovieAllocine(webpage[iMovie[1]:iMovie[2]])
        if (length(tmp)==7) {
            return(list(tmp))
        } else {
            return(tmp)
        }
    })
    cat("\n")
    timeTable <- timeTable[!sapply(timeTable, is.null)]
    return(unlist(timeTable, recursive = FALSE))
}
# getTimeTableAllocine("http://www.allocine.fr/seance/salle_gen_csalle=P0086.html")


movieCalendar <- function (lmovies, time2start, time2end, pub.overlap, pub.time, wait.time) {
    movies <- lapply(lmovies, function (lmovie) {
        movie <- as.numeric(gsub("(.+):(.+)", "\\1", lmovie))*60 + as.numeric(gsub("(.+):(.+)", "\\2", lmovie))
        startMovie <- lmovie[-1]
        tmp <- sapply(seq_len(length(movie)-1)+1, function (iSeance) {
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
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 3))>=time2start), ]})
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 4))<=time2end), ]})
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
        for (jFilm in seq_len(nrow(Plan)-1)) {
            Plan[jFilm, "Waiting Time"] <- as.numeric(Plan[jFilm+1, 3])-as.numeric(Plan[jFilm, 4])-pub.overlap
        }
        if (nrow(Plan)==1) {
            Plan[, "Waiting Time"] <- NA
        } else {}
        finalPlan <- Plan[, c("startMovie", "endMovie", "Waiting Time")]
        colnames(finalPlan) <- c("START", "END", "Waiting Time")
        finalPlan[, "Waiting Time"] <- paste0(finalPlan[, "Waiting Time"], " min")
        finalPlan[nrow(finalPlan), "Waiting Time"] <- ""
        return(finalPlan)
    })
    res <- lapply(res, na.exclude)
    res <- lapply(res, function (iPlan) {
        iPlan[, "Waiting Time"] <- as.numeric(gsub(" min", "", iPlan[, "Waiting Time"]))
        if (nrow(iPlan)>1) {
            howMuchTime <- iPlan[!is.na(iPlan[, "Waiting Time"]), "Waiting Time"]<wait.time
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
        iPlan[, "Waiting Time"] <- paste0(iPlan[, "Waiting Time"], " min")
        iPlan[nrow(iPlan), "Waiting Time"] <- ""
        colnames(iPlan) <- c("Start", "End", "Waiting Time")
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
        tmp <- sapply(seq_len(length(movie)-1)+1, function (iSeance) {
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
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 3))>=time2start), ]})
    movies <- lapply(movies, function (i) {i[which(as.numeric(apply(i, 1, "[", 4))<=time2end), ]})
    movies <- movies[which(sapply(movies, nrow)>0)]
    movies <- lapply(movies, function (iMovie) {
        rownames(iMovie) <- NULL
        return(iMovie)
    })
    Movies <- lapply(movies, "[", c(1, 2))
    Movies <- lapply(Movies, function (i) {colnames(i) <- c("START", "END"); return(i)})
    Movies <- lapply(names(Movies), function (iName) {
        rownames(Movies[[iName]]) <- paste0(iName, " (", seq_len(nrow(Movies[[iName]])), ")")
        colnames(Movies[[iName]]) <- c("Début", "Fin")
        return(Movies[[iName]])
    })
    return(Movies)
}