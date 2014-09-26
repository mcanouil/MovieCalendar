# library(shiny)
library(shinyIncubator)
library(RCurl)
library(XML)
library(parallel)
Sys.setenv("LANGUAGE" = "en")
options(menu.graphics = FALSE)
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)
library(xlsx)
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

getUGC <- function () {
    url <- "http://www.ugc.fr/home.html"
    webpage <- readLines(tc <- textConnection(getURL(url)))
    close(tc)
    webpage <- webpage[length(webpage)]
    tmpWebpage <- unlist(strsplit(webpage, "        <div class='Foot'> <div class='InnerFoot'> <h2>TOUS LES CINEMAS</h2>", fixed = TRUE))
    webpage <- capture.output(htmlTreeParse(tmpWebpage[2]))
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
if (file.exists("www/listCinema.txt")) {
    listCinema <- dget(file = "www/listCinema.txt")
} else {
    listCinema <- c("Le Metropole - Lille" = "http://www.lemetropolelille.com/horaires/", "Le Majestic - Lille" = "http://www.lemajesticlille.com/horaires/", getUGC())
    listCinema <- listCinema[order(names(listCinema))]
    dput(listCinema, file = "www/listCinema.txt")
}

shinyUI(fluidPage(
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
    headerPanel(h1("Des Films en série!", p("(ou un seul)", style = "font-size: 50%; padding-left: 10px"), style = "padding-top: 10px;"), "Des Films en série!"),
    progressInit(),
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


