#---------------------------------------------------------------------------------
# Name - app.R
# Desc - Shiny App designed to plan how to watch several movies in a row without the ads
# Version - 2.0.2
# Author - Mickael Canouil
# Source code - https://github.com/mcanouil/MovieCalendar
#---------------------------------------------------------------------------------
rm(list = ls())
options(menu.graphics = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
library(shiny)
library(shinydashboard)
library(XML)
library(RCurl)
library(xlsx)
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

# options(warn=2)
# options(warn=0)

myPalettes <- rbind(
    c(Name = "dodgerblue",  Hex = "#1E90FF", RGB = "rgb(30/255, 144/255, 255/255)"),
    c(Name = "firebrick2",  Hex = "#EE2C2C", RGB = "rgb(238/255, 44/255, 44/255)"),
    c(Name = "springgreen3",  Hex = "#008B45", RGB = "rgb(0/255, 139/255, 69/255)"),
    c(Name = "maroon2",  Hex = "#EE30A7", RGB = "rgb(238/255, 48/255, 167/255)"),
    c(Name = "goldenrod2",  Hex = "#EEB422", RGB = "rgb(238/255, 180/255, 34/255)"),
    c(Name = "deepskyblue",  Hex = "#00BFFF", RGB = "rgb(0/255, 191/255, 255/255)"),
    c(Name = "mediumpurple1",  Hex = "#AB82FF", RGB = "rgb(171/255, 130/255, 255/255)"),
    c(Name = "tan1",  Hex = "#FFA54F", RGB = "rgb(255/255, 165/255, 79/255)")
)
myPalette <- myPalettes[, "Name"]

source("appfunctions.R")

ui <- dashboardPage(
    dashboardHeader(
        title = HTML("Movie Calendar"),
        dropdownMenuOutput("messageMenu")
    ),
    dashboardSidebar(
        # sidebarUserPanel(
            # name = a(tags$i(style = "color:#1995dc", icon("envelope", lib = "glyphicon")), "Mickaël Canouil", href = 'mailto:mickael.canouil@cnrs.fr'),
            # subtitle = tags$span(style = "color:#1995dc", "(Biostatistician)")
        # ),
        # hr(),
        sidebarMenu(
            menuItem(
                text = "Movie Theatre Selection",
                tabName = "home",
                icon = tags$i(style = "color:#1995dc", icon("home", lib = "glyphicon"))
            ),
            htmlOutput("regionslistinput"),
            htmlOutput("movietheatrelistinput")
            # hr()
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "css/theme.css")
        ),
        tabItems(
            tabItem(tabName = "home",
                fluidRow(
                    box(
                        column(width = 2,
                            radioButtons(
                                inputId = "isnew",
                                label = "New",
                                choices = list("Both" = "Both", "Yes" = TRUE, "No" = FALSE),
                                selected = "Both",
                                inline = FALSE
                            ),
                            checkboxGroupInput(
                                inputId = "whichlang",
                                label = "Language",
                                choices = list("VF" = "VF", "VO" = "VO", "VFSTF" = "VFSTF"),
                                selected = c("VF", "VO"),
                                inline = FALSE
                            ),
                            radioButtons(
                                inputId = "is3d",
                                label = "Movie in 3D",
                                choices = list("Both" = "Both", "Yes" = TRUE, "No" = FALSE),
                                selected = "Both",
                                inline = FALSE
                            )
                        ),
                        # column(width = 1,
                            # div(style = "border-left:1px solid #000; border-right:1px solid #000;height:320px")
                        # ),
                        column(width = 10,
                            sliderInput(
                                inputId = "timeinterval",
                                label = "Time Interval (hours)",
                                min = 10,
                                max = 25,
                                value = c(19, 25),
                                step = 0.5,
                                width = "100%"
                            ),
                            sliderInput(
                                inputId = "adstime",
                                label = "Advertising time (minutes)",
                                min = 0,
                                max = 15,
                                value = 15,
                                step = 1,
                                width = "100%"
                            ),
                            sliderInput(
                                inputId = "waittime",
                                label = "Waiting time between two movies (minutes)",
                                min = -15,
                                max = 30,
                                value = 5,
                                step = 5,
                                width = "100%"
                            )
                        ),
                        width = 12,
                        collapsible = TRUE,
                        title = "Movie Selection Settings",
                        solidHeader = TRUE,
                        status = "info"
                    )
                ),
                fluidRow(
                    box(
                        htmlOutput("checkmovielistinput"),
                        width = 5,
                        collapsible = FALSE,
                        title = "Movies List",
                        solidHeader = TRUE,
                        status = "warning"
                    ),
                    box(
                        lapply(seq(5), function(i) {
                            div(htmlOutput(paste0("movieTable", i)), align = "center")
                        }),
                        width = 7,
                        collapsible = FALSE,
                        title = "Movies Time Table",
                        solidHeader = TRUE,
                        status = "primary"
                    )
                )
            )
        )
    )
)

server <- function (input, output, session) {
    output$messageMenu <- renderMenu({
        dropdownMenu(
            type = "notifications",
            notificationItem(
                text = paste("Data computed on", format(Sys.Date(), "%Y-%d-%m"), "!"),
                icon = icon("exclamation-sign", lib = "glyphicon"),
                status = "info"
            )
        )
    })

    regionslist <- reactive({
        webpage <- htmlTreeParse(file = "http://www.allocine.fr/salle/", isURL = TRUE, encoding = "UTF-8")[3]
        webpage <- capture.output(webpage[["children"]][["html"]][["body"]])
        parseurlregions <- grep("cinemas-pres-de", webpage)
        urlregions <- cbind.data.frame(
            Regions = gsub("&apos;", "'", gsub(".*<span>(.*)</span>", "\\1", webpage[parseurlregions+1])),
            URL = gsub(".*salle/(.*)/\">", "http://www.allocine.fr/salle/\\1", webpage[parseurlregions])
        )
        return(urlregions)
    })
    movietheatrelist <- reactive({
        regionslist <- regionslist()
        selectedurl <- regionslist[regionslist[, "Regions"]%in%input$whichregion, "URL"]
        webpage <- htmlTreeParse(file = selectedurl, isURL = TRUE, encoding = "UTF-8")[3]
        webpage <- capture.output(webpage[["children"]][["html"]][["body"]])
        parseurlmovietheatre <- grep("salle_gen_csalle", webpage)
        urlmovietheatre <- cbind.data.frame(
            MovieTheatre = gsub("&apos;", "'", gsub(".*>(.*)<.*", "\\1", webpage[parseurlmovietheatre])),
            URL = gsub(".*<a href=\"/(.*).html\">.*", "http://www.allocine.fr/\\1.html", webpage[parseurlmovietheatre])
        )
        return(urlmovietheatre)
    })

    output$regionslistinput <- renderUI({
        regionslist <- regionslist()
        return(selectInput(
            inputId = "whichregion",
            label = "Movie Theatres Next To:",
            choices = regionslist[, "Regions"],
            selected = "Lille",
            width = "100%",
            selectize = FALSE,
            multiple = FALSE
        ))
    })
    output$movietheatrelistinput <- renderUI({
        validate(need(input$whichregion, ""))
        movietheatrelist <- movietheatrelist()
        return(selectInput(
            inputId = "whichmovietheatres",
            label = div("Movie Theatres in ", tags$i(input$whichregion), ":"),
            choices = movietheatrelist[, "MovieTheatre"],
            selected = NULL,
            width = "100%",
            selectize = FALSE,
            multiple = TRUE
        ))
    })

    movielist <- reactive({
        movietheatrelist <- movietheatrelist()
        selectedurl <- movietheatrelist[movietheatrelist[, "MovieTheatre"]%in%input$whichmovietheatres, "URL"]
        if (length(selectedurl)==1) {
            timeTable <- try({getTimeTableAllocine(selectedurl)}, silent = TRUE)
        } else {
            timeTable <- try({unique(unlist(lapply(selectedurl, getTimeTableAllocine), recursive = FALSE))}, silent = TRUE)
        }
        if (class(timeTable)!="list") {
            timeTable <- NULL
        } else {}
        return(timeTable)
    })
    filtermovielist <- reactive({
        movielist <- movielist()
        condlang <- sapply(movielist, "[[", "Language")%in%input$whichlang
        movielist <- movielist[condlang]
        movielist <- switch(input$is3d,
            "Both" = {movielist},
            "TRUE" = {movielist[which(sapply(movielist, "[[", "3D")%in%input$is3d)]},
            "FALSE" = {movielist[which(sapply(movielist, "[[", "3D")%in%input$is3d)]}
        )
        movielist <- switch(input$isnew,
            "Both" = {movielist},
            "TRUE" = {movielist[which(sapply(movielist, function (el) { difftime(Sys.Date(), el$Release)<=7 }))]},
            "FALSE" = {movielist[which(sapply(movielist, function (el) { difftime(Sys.Date(), el$Release)>7 }))]}
        )
        return(movielist)
    })

    output$checkmovielistinput <- renderUI({
        validate(need(input$whichregion, ""))
        validate(need(input$whichmovietheatres, ""))
        validate(need(!is.null(movielist()), "No data available!"))
        filtermovielist <- filtermovielist()
        checkmovielistchoices <- sapply(filtermovielist, "[[", "Title")
        names(checkmovielistchoices) <- names(checkmovielistchoices)
        checkmovielistchoices <- as.list(checkmovielistchoices)

        concattheatres <- unique(sapply(input$whichmovietheatres, function (i) { gsub(" *$", "", capture.output(tags$b(i, style = "color:#1995dc")))}))
        if (length(concattheatres)>2) {
            concattheatres <- paste0(paste0(concattheatres[-length(concattheatres)], collapse = ", "), " and ", concattheatres[length(concattheatres)])
        } else {
            concattheatres <- paste0(concattheatres, collapse = " and ")
        }
        return(checkboxGroupInput(
            inputId = "checkmovielist",
            label = div(
                "Movies On Screen at",
                br(),
                HTML(paste(
                    concattheatres,
                    paste0("(", gsub(" *$", "", capture.output(tags$b(input$whichregion, style = "color:#1995dc"))), ").")
                ))
            ),
            choices = checkmovielistchoices,
            selected = NULL
        ))
    })


    movieTable <- reactive({
        validate(need(input$whichregion, ""))
        validate(need(input$whichmovietheatres, ""))
        validate(need(!is.null(movielist()), "No data available!"))
        filtermovielist <- filtermovielist()
        checkmovielistchoices <- sapply(filtermovielist, "[[", "Title")
        names(checkmovielistchoices) <- names(checkmovielistchoices)
        checkmovielist <- input$checkmovielist
        if (!is.null(checkmovielist)) {
            lmovies <- filtermovielist[sapply(checkmovielist, grep, checkmovielistchoices, fixed = TRUE)]
            lmovies <- sapply(lmovies, "[", 7)
            if (length(lmovies)==1) {
                planMovies <- movieTimeTable(lmovies = lmovies,
                    time2start = input$timeinterval[1],
                    time2end = input$timeinterval[2],
                    pub.overlap = ifelse(input$waittime>=0, input$adstime, 15-abs(input$waittime)),
                    pub.time = input$adstime
                )
            } else {
                planMovies <- movieCalendar(
                    lmovies = lmovies,
                    time2start = input$timeinterval[1],
                    time2end = input$timeinterval[2],
                    pub.overlap = ifelse(input$waittime>=0, input$adstime, 15-abs(input$waittime)),
                    pub.time = input$adstime,
                    wait.time = input$waittime
                )
            }
        } else {
            planMovies <- NULL
        }
        return(planMovies)
    })

    lapply(seq(5), function(i) {
        output[[paste0('movieTable', i)]] <- renderTable({
            validate(need(length(input$checkmovielist)>=1, ""))
            if (!is.data.frame(movieTable())) {
                validate(need(length(movieTable())>=i, ""))
                return(movieTable()[[i]])
            } else {
                if (i==1) {
                    validate(need(is.data.frame(movieTable()), ""))
                    return(movieTable()[, -3])
                } else {
                    return(validate(need(is.data.frame(movieTable()), "")))
                }
            }
        })
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
                print(iMovie[["Release"]])
                cond4 <- todayDate-releaseDate>=0 & todayDate-releaseDate<7
                print(cond4)
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

    return(output)
}

shinyApp(ui, server)
