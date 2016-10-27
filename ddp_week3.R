## Load a fantasy football package
library(ffanalytics)

## Set scoring rules for my league in CBS
set_my_scoring <- function() {
    scoringRules <- list(
        QB = data.table::data.table(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                                    multiplier = c(6/100, 4, -2, 15/100, 6, 2, -2)),
        
        RB = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                                    multiplier = c(15/100, 6, 0.5, 15/100, 6, 6, 2, -2)),
        
        WR = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                                    multiplier = c(15/100, 6, 0.5, 15/100, 6, 6, 2, -2)),
        
        TE = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                                    multiplier = c(15/100, 6, 0.5, 15/100, 6, 6, 2, -2)),
        
        K = data.table::data.table(dataCol = c("xp", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"), 
                                   multiplier = c(1,  3, 3, 3, 4, 5)), # good
        
        DST = data.table::data.table(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                                     multiplier = c(2, 2, 2, 1, 6, 0)),
        
        DL = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                                    multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
        
        LB =  data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                                     multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
        
        DB = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                                    multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
        
        ptsBracket = data.table::data.table(threshold = c(0, 7, 13, 20, 99),
                                            points = c(8, 6, 4, 2, 0))
    )
    
    return(scoringRules)
}

## Scrape the data for this week and save
scrape_my_league <- function(week) {
    
    # check if this scrape has been performed before
    my.file <- paste0("week", week, ".RDS")
    if(!file.exists(my.file)) {
        
        target.analysts <- c(-1, 0, 1, 2, 3, 4, 5) ## CBS, Yahoo, ESPN, NFL
        
        scrapeData <- runScrape(week = week,
                                season = 2016,
                                analysts = target.analysts,
                                positions = c("QB", "RB", "WR", "TE", "K", "DST"))
        
        saveRDS(scrapeData, file = my.file)
        
    } else {
        message("Week already downloaded. Loading from disk.")
        scrapeData <- readRDS(my.file)
    }
    
    return(scrapeData)
}

## Calculate the projections
project_my_league <- function(week) {
    
    scrapeData <- scrape_my_league(week)
    userScoring <- set_my_scoring()
    
    ## hacks to make this broken package work
    # playerData <<- getPlayerData(2016, weekNo = 8)
    # tierDValues <<- c(QB = 0.25, RB = 0.4, WR = 0.4, TE = 0.35, K = 0.15, DST = 0.1, DL = 0.3, DB = 0.13, LB = 0.3)
    
    myProjections <<- getProjections(scrapeData,
                                    avgMethod = "average",
                                    leagueScoring = userScoring,
                                    teams = 12,
                                    format = "standard",
                                    adpSources = c("CBS", "ESPN", "FFC", "MFL", "NFL"))
}

return_clean_projections <- function(week, pos, num = 30) {

    if(!exists("myProjections")) {
        scrape_my_league(week)
    }
    library(dplyr)
    
    projections <- myProjections$projections
    clean_proj <<- projections %>%
                    filter(position == pos) %>%
                    arrange(desc(points)) %>%
                    slice(1:num)
    
    return(clean_proj)
    
}

plot_projections <- function(week, pos, num = 30) {
    
    library(ggplot2)
    library(ggthemes)
    library(plotly)
    
    proj <- return_clean_projections(week, pos, num)
    #proj <- mutate(proj, player = as.ordered(player))
    proj$player <- factor(proj$player, levels = proj[order(proj$points), "player"])
    
    g <- ggplot(proj, aes(points, player)) + geom_point()
    g <- g + geom_errorbarh(aes(xmin = lower, xmax = upper))
    g <- g + ggtitle(paste("Fantasy Football", pos, "Scoring Projections for Week", week, "of 2016"))
    g <- g + xlab("Average of projected points from Fantasy Football Analytics")
    g <- g + ylab(NULL)
    
    ## save an html
    #htmlwidgets::saveWidget(as.widget(g), paste0("week", week, ".html"))
    
    ggplotly(g)
}