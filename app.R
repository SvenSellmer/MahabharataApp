# App-1
# Version Poznań

######################
#### PRÄAMBEL ##########

library(shiny)
library(httpuv)
#library(googlesheets4)
#options(googlesheets.httr_oauth_cache = TRUE) #  Poznań było FALSE!
#gs_auth(token = "googlesheets_token.rds") # Datei in /srv/shiny-server/mahaApp

library(stringr)

options(stringsAsFactors = F)

##########################
##### FUNKTIONEN #########

#### LADEBEREICH ######

loadSkt <- function() {
  # Grab the Google Sheet: bbSkt
  # sheet <- gs_url("https://docs.google.com/spreadsheets/d/1RTYWKl15N0_b-NfgipJQNnIW4DCJYIKgUPZqNUihxKQ/edit#gid=0") #gs_title(table)
  # Read the data
  # gs_read(sheet, verbose=T)
  read.table("bbSkt.csv", sep="\t", header = T)
}
bbSkt <- loadSkt()

loadPl <- function() { #gs
  # Grab the Google Sheet: bbPl
  #sheet <- gs_url("https://docs.google.com/spreadsheets/d/1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE/edit#gid=0") #gs_title(table)
  # sheet <- gs_key("1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE") #, lookup = F)
  # Read the data
  #gs_read(sheet, verbose=T)
  read.table("bbPl.csv", sep="\t", header = T)
}
bbPl <- loadPl()

loadAna <- function() {
  # Grab the Google Sheet: bbPl
  #sheet <- gs_url("https://docs.google.com/spreadsheets/d/1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE/edit#gid=0") #gs_title(table)
  # Read the data
  # gs_read(sheet, verbose=T)
  read.table("bbAna.csv", sep="\t", header = T)
}
bbAna <- loadAna()

lfd_lemmata <- bbAna$lfd_lem_id
names(lfd_lemmata) <- bbAna$lemma

loadSlow <- function() { # gs
  # Grab the Google Sheet: slow
  #sheet <- gs_url("https://docs.google.com/spreadsheets/d/1IWDDz6l6J7J1Svuqxg3YHLTcitK6808Xe0-C6jP7CLQ/edit#gid=0") #gs_title(table)
  # sheet <- gs_key("1IWDDz6l6J7J1Svuqxg3YHLTcitK6808Xe0-C6jP7CLQ") #, lookup = F)
  # Read the data
  #gs_read(sheet, verbose=T)
  read.table("slow.csv", sep="\t", header = T)
}
slow <- loadSlow()

#loadOsoba <- function(){ # osoby
  
  #sheet <- gs_url("https://docs.google.com/spreadsheets/d/1fL1J3oO413oFuqP_ukphgT-omN_NCrssSX42ke5d-7M/edit#gid=0")
  #sheet <- gs_key("1fL1J3oO413oFuqP_ukphgT-omN_NCrssSX42ke5d-7M") #, lookup = F)
  #gs_read(sheet, verbose = T)
  
#}
#osoby_df <- as.data.frame(loadOsoba()) ### Aus irgendeinem Grunde hält er das für eine Liste!!!


#### SICHERHEITSBEREICH #########

savePrzek <- function(strofa, przek) {
  # Grab the Google Sheet: bbPl
  #sheet <- gs_url("https://docs.google.com/spreadsheets/d/1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE/edit#gid=0")
  #sheet <- gs_key("1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE") #, lookup = F)
  
  koordinaten <- paste("R", which(bbPl$stro_id == strofa)+1, "C4", sep="")
  
  przek <- str_replace_all(przek, " / ", " /<br> ")
  przek <- str_replace_all(przek, "([^/])$", "\\1 //")
  przek <- str_replace_all(przek, "$", "<br>")
  
  gs_edit_cells(sheet, input = przek, anchor = koordinaten, verbose = TRUE)
  
  # bbPl <<- loadPl() # funzt
  # aber lieber nur Kopie modifizieren:
  
  bbPl[which(bbPl$stro_id == strofa), ]$text <<- przek # klappt auch!
}

saveKomstro <- function(strofa, kom_stro) {
  # Grab the Google Sheet: bbPl
  # sheet <- gs_url("https://docs.google.com/spreadsheets/d/1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE/edit#gid=0")
  sheet <- gs_key("1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE") #, lookup = F)
  
  koordinaten <- paste("R", which(bbPl$stro_id == strofa)+1, "C6", sep="")
  
  #if(is.na(bbPl[bbPl$stro_id == strofa, ]$kom))
    if(is.na(bbPl[which(bbPl$stro_id == strofa), ]$kom))
  {kom <- kom_stro} else
  {kom <- paste(bbPl[which(bbPl$stro_id == strofa), ]$kom, "||", kom_stro)}
  
  gs_edit_cells(sheet, input = kom, anchor = koordinaten, verbose = TRUE)
  
  bbPl[which(bbPl$stro_id == strofa), ]$kom <<- kom
  
}

saveProp <- function(lfd_lemma_id, prop) {
  # Grab the Google Sheet: slow
  # if (prop != "" & lemma != ""){ 
  # sheet <- gs_url("https://docs.google.com/spreadsheets/d/1IWDDz6l6J7J1Svuqxg3YHLTcitK6808Xe0-C6jP7CLQ/edit#gid=0")
  sheet <- gs_key("1IWDDz6l6J7J1Svuqxg3YHLTcitK6808Xe0-C6jP7CLQ") #, lookup = F)
  # 
  id <- bbAna[bbAna$lfd_lem_id == lfd_lemma_id, ]$id
  # 
  # Kommentare:
  if (str_count(prop, "#") == 2){
    
    kom_lem <- str_replace_all(unlist(str_extract_all(prop, "#.*#")), "#", "")
    
    koordinaten <- paste("R", which(slow$id == id)+1,"C6", sep="")
    gs_edit_cells(sheet, input = kom_lem, anchor = koordinaten, verbose = TRUE)
    
    slow[slow$id == id, ]$kom <<- kom_lem
    
    prop <- str_replace_all(prop, "#.*#", "")
    prop <- str_replace_all(prop, " +", " ")
    
  }
  
  # Vorschläge
  if (prop != ""){
    
    if(is.na(slow[slow$id == id, ]$pl))
    {def_pl <- prop} else
    {def_pl <- paste(slow[slow$id == id, ]$pl, ";", prop)}
    
    koordinaten <- paste("R", which(slow$id == id)+1,"C4", sep="")
    gs_edit_cells(sheet, input = def_pl, anchor = koordinaten, verbose = TRUE)
    
    slow[slow$id == id, ]$pl <<- def_pl
    
  }
}

saveAdmin <- function(admin){ # uwagi
  
  #sheet <- gs_url("https://docs.google.com/spreadsheets/d/1Xy1pBI0u7LghQUtrmNBOr5gEwXfoPJ-6XN-oBm01uG0/edit#gid=0")
  sheet <- gs_key("1Xy1pBI0u7LghQUtrmNBOr5gEwXfoPJ-6XN-oBm01uG0") #, lookup = F)
  uwagi <- gs_read(sheet)
  koordinaten <- paste("R", nrow(uwagi)+2,"C1", sep="")
  gs_edit_cells(sheet, input = c(as.character(Sys.time()), admin), anchor = koordinaten, verbose = TRUE, byrow = TRUE)
  
}

savePunkt <- function(stro_id, stro_punkt) {
  # Grab the Google Sheet: bbPl
  
  stro_punkt <- str_replace_all(stro_punkt, "OK! ", "")
  
  # sheet <- gs_url("https://docs.google.com/spreadsheets/d/1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE/edit#gid=0")
  sheet <- gs_key("1v11msKMZLbIo9av6c4MghgcOmOOpSQZMYdSJJ7Lx7nE") #, lookup = F)
  
  koordinaten <- paste("R", which(bbPl$stro_id == stro_id)+1, "C7", sep="")
  
  gs_edit_cells(sheet, input = stro_punkt, anchor = koordinaten, verbose = TRUE)
  
  # bbPl <<- loadPl() # funzt
  # aber lieber nur Kopie modifizieren:
  
  bbPl[which(bbPl$stro_id == stro_id), ]$punkt <<- stro_punkt
  
}

saveOsoba <- function(lfd_lem_id, osoba) {
  # Grab the Google Sheet: osoby
  
  # sheet <- gs_url("https://docs.google.com/spreadsheets/d/1fL1J3oO413oFuqP_ukphgT-omN_NCrssSX42ke5d-7M/edit#gid=0")
  sheet <- gs_key("1fL1J3oO413oFuqP_ukphgT-omN_NCrssSX42ke5d-7M") #, lookup = F)
  
  osoby <- gs_read(sheet)
  koordinaten <- paste("R", nrow(osoby)+2,"C1", sep="")
  daten <- c(
    lfd_lem_id,
    bbAna[bbAna$lfd_lem_id == lfd_lem_id, ]$lemma,
    bbAna[bbAna$lfd_lem_id == lfd_lem_id, ]$id,
    bbAna[bbAna$lfd_lem_id == lfd_lem_id, ]$stro_id,
    osoba
  )
  
  gs_edit_cells(sheet, input = daten, anchor = koordinaten, verbose = TRUE, byrow = TRUE)
  
  osoby_df <<- rbind(osoby_df, daten)
  
}


#### AUSGABEBEREICH ########

slowNiczek <- function(lemma_id_vektor) {
  # 
  ausgabe <- " "
  for (i in 1:length(lemma_id_vektor)) {
    #if (!any(lemma_id_vektor %in% slow$id == FALSE)) { # jetzt in 112
    # Sicherung
    if (lemma_id_vektor[i] %in% slow$id == TRUE) 
      if(!is.na(slow[slow$id == lemma_id_vektor[i], ]$pl)|!is.na(slow[slow$id == lemma_id_vektor[i], ]$kom))
      {
        if(is.na(slow[slow$id == lemma_id_vektor[i], ]$kom)){
          ausgabe <- paste(
            ausgabe,
            "<div style = \"background-color: #80ff00; width: 70%; height: 20px; border-radius: 3px;\">", 
            slow[slow$id == lemma_id_vektor[i], ]$lemma, "</div>", sep=""
          )
        }else{
          ausgabe <- paste(
            ausgabe,
            "<div style = \"background-color: #F5D0A9; width: 70%; height: 20px; border-radius: 3px;\">", 
            slow[slow$id == lemma_id_vektor[i], ]$lemma, "</div>", sep=""
          )
        }
        #ausgabe <-
        #  paste(ausgabe, slow[slow$id == lemma_id_vektor[i], ]$lemma, "=", slow[slow$id == lemma_id_vektor[i], ]$pl, "<br>")
      }
    #}
  }
  # ausgabe <- paste(slow[slow$id == lemma_id_vektor[2], ]$lemma, "=", slow[slow$id == lemma_id_vektor[2], ]$pl, "<br>")
  return(ausgabe)
}


osoAusgabe <- function(stro_id){
  
  oso <- " "
  
  if(any(osoby_df$lfd_lem_id %in% bbAna[bbAna$stro_id == stro_id, ]$lfd_lem_id)){
    
    for (i in 1:sum(osoby_df$lfd_lem_id %in% bbAna[bbAna$stro_id == stro_id, ]$lfd_lem_id)){
      
      oso <- paste(
        osoby_df[osoby_df$lfd_lem_id %in% bbAna[bbAna$stro_id == stro_id, ]$lfd_lem_id, ]$lemma[i],
        "=",
        osoby_df[osoby_df$lfd_lem_id %in% bbAna[bbAna$stro_id == stro_id, ]$lfd_lem_id, ]$osoba[i],
        "<br>"
      )
    }
  }
  return(oso)
}


# Oryginał
sktAusgabe <- function(stro_id) {
  
  vers_nr <- bbSkt[bbSkt$stro_id == stro_id,]$vers_nr[1]
  
  ausgabe <- " "
  
  if (str_detect(stro_id, " 1 1$")) {
    ausgabe <-  paste(
      paste("<strong>", bbSkt[bbSkt$stro_id == stro_id,]$text, "</strong>", collapse =
              ""),
      paste(bbSkt[bbSkt$vers_nr >= vers_nr + nrow(bbSkt[bbSkt$stro_id == stro_id,]) &
                    bbSkt$vers_nr < vers_nr + nrow(bbSkt[bbSkt$stro_id == stro_id,]) + 8,]$text, collapse =
              ""),
      collapse = ""
    )
    
  } else {
    ausgabe <- paste(
      paste(bbSkt[bbSkt$vers_nr > vers_nr - 5 & bbSkt$vers_nr < vers_nr,]$text, collapse = ""),
      paste("<strong>", bbSkt[bbSkt$stro_id == stro_id,]$text, "</strong>", collapse = ""),
      paste(bbSkt[bbSkt$vers_nr >= vers_nr + nrow(bbSkt[bbSkt$stro_id == stro_id,]) &
                    bbSkt$vers_nr < vers_nr + nrow(bbSkt[bbSkt$stro_id == stro_id,]) + 4,]$text, collapse = ""),
      collapse = ""
    )
  }
  return(ausgabe)
}

## Przekład
plAusgabe <- function(stro_id) {
  
  stro_nr <- which(bbPl$stro_id == stro_id)
  
  if (str_detect(stro_id, " 1 1$")) {
    ausgabe <-  paste(
      paste("<strong>", bbPl[which(bbPl$stro_id == stro_id),]$text, "</strong>", collapse = ""),
      paste(bbPl[(stro_nr +1) : (stro_nr + 4),]$text, collapse =""),
      collapse = ""
    )
    
  } else if (str_detect(stro_id, " 1 2$")){
    ausgabe <-  paste(
      paste(bbPl[stro_nr - 1,]$text, collapse =""),
      paste("<strong>", bbPl[which(bbPl$stro_id == stro_id),]$text, "</strong>", collapse = ""),
      paste(bbPl[(stro_nr +1) : (stro_nr + 3),]$text, collapse =""),
      collapse = ""
    )
  } else{
    ausgabe <- paste(
      paste(bbPl[(stro_nr - 2) : (stro_nr - 1),]$text, collapse = ""),
      paste("<strong>", bbPl[stro_nr,]$text, "</strong>", collapse =""),
      paste(bbPl[(stro_nr + 1) : (stro_nr + 2),]$text, collapse =""),
      collapse = ""
    )
  }
  return(ausgabe)
}


przekAusgabe <- function(stro_id) {
  
  #str_replace_all(bbPl[bbPl$stro_id == stro_id, ]$text, "<br>", "")
  str_replace_all(bbPl[which(bbPl$stro_id == stro_id), ]$text, "<br>", "")
  
}

# punktAusgabe <- function(stro_id) {
#   
#   #if (!is.na(bbPl[bbPl$stro_id == stro_id, ]$punkt)){ # Warum geht das nicht mehr???
#     if (!is.na(bbPl[which(bbPl$stro_id == stro_id), ]$punkt)){
#     
#     ausgabe <- paste("OK!", bbPl[which(bbPl$stro_id == stro_id), ]$punkt) # später: farbig u.dgl.
#     
#   } else {
#     
#     ausgabe <- str_replace_all(
#       paste(bbSkt[bbSkt$stro_id == stro_id, ]$text, collapse = " "),
#       "<br>", ""
#     )
#   }
# }

## Komentarz do strofy

krotKom <- function(stro_id) {
  
  #if (!is.na(bbPl[bbPl$stro_id == stro_id, ]$kom)){
    if (!is.na(bbPl[which(bbPl$stro_id == stro_id), ]$kom)){
    
    ausgabe <- str_replace_all(bbPl[which(bbPl$stro_id == stro_id), ]$kom, "^(.{15}?).*", "\\1…")
    
  } else {
    
    ausgabe <- "brak komentarza"
  } 
  return(ausgabe)
}


########################
#### Die App #########

shinyApp(
  
  ################## UI ####################
  ui = fluidPage(
    
    span(titlePanel("महाभारतम्"), style="color:orange"),
    
    ### Script für Enter (key Nr. 13)
    tags$script(' $(document).on("keydown", function (e) {
                  Shiny.onInputChange("lastkeypresscode", e.keyCode);
                });
                '),
    
    fluidRow( #### Reihe I: Skt | Str.-Nr. | Pl
      
      column(5,
             htmlOutput("outSktTitel"),
             htmlOutput("outSkt")#,
             # actionLink("anaPop", "Ein Lemma")
      ),
      
      column(2,
             selectInput(
               "inPl",
               "Strofa",
               choices = bbPl$stro_id,
               multiple = FALSE,
               selectize = FALSE,
               size = 10,
               width = "100px"
             ),
             
             textInput(
               "stro_stri",
               label = NULL,
               placeholder = "Str. + ENTER",
               width = "100px"
             )
             #, verbatimTextOutput("testout")
             
      ),
      
      column(5,
             htmlOutput("outPlTitel"),
             htmlOutput("outPl")
             # selectInput('in2', 'bb po polsku', choices=loadPl()$text, multiple=FALSE, selectize=TRUE, width="500px")
             # selected=loadPl()$text[which(loadSkt()$text == input$in1)] ))
             
             # selected=loadPl()$text[3],  # irgendwie nach dem Motto: which(in1)
             
             #DT::dataTableOutput("bbPl", width = 8), tags$hr()
      )
    ),
    # textInput("name", "Name", ""),
    # checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    # sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
    # actionButton("submit", "Submit")
    
    # fluidRow( #### Reihe II: Przekład | PotwPrzek
    #   
    #   column( 
    #     11,
    #     textInput("przek", label=NULL, width = "100%") # placeholder = "PRZEKŁAD TUTAJ", 
    #   ), 
    #   
    #   column(
    #     1,
    #     actionButton("zatw_przek", "☺", width="50px") #, style="padding:20px")
    #   )),
    # 
    # fluidRow( # Reihe III: Komentarz do strofy | PotwKomstro
    #   
    #   column(
    #     2,
    #     textOutput("krot_kom"),
    #     actionLink("pok_komstro", "Cały kom.")#, #style="padding:4px")
    #   ),
    #   
    #   column( 
    #     9,
    #     textInput("kom_stro", label=NULL, placeholder = "KOMENTARZ DO STROFY", width = "100%")
    #   ), 
    #   
    #   column(
    #     1,
    #     actionButton("zatw_komstro", "☺", width="50px") #, style="padding:20px")
    #   )),
    # 
    # fluidRow( # Reihe IV: Punktacja | PotwPunkt
    #   
    #   column( 
    #     11,
    #     textInput("punkt", label=NULL, value = "", width = "100%")
    #   ), 
    #   
    #   column(
    #     1,
    #     actionButton("zatw_punkt", "☺︎", width="50px") #, style="padding:20px")
    #   )),
    # 
    # fluidRow( # Reihe V: Analyse | Słowniczek 
    #   
    #   column(
    #     5,
    #     checkboxGroupInput("checkAna", "Analiza", choices = NULL)
    #   ),
    #   
    #   column(
    #     4,
    #     #htmlOutput("outSlowniczek1"), # "W słowniczku:"
    #     htmlOutput("outSlowniczek"), # Lemmata im Slow
    #     htmlOutput("outOsoba"),
    #     #htmlOutput("outOsoba"), # Personen in Osoba
    #     
    #     htmlOutput("leerzeileA"),
    #     
    #     selectInput(
    #       "inSl",
    #       label="Do słowniczka / Zaim. = os.",
    #       choices = " ",
    #       #choices = paste(bbAna[bbAna$stro_id == input$inPl,]$lemma, "_", bbAna[bbAna$stro_id == input$inPl,]$id, sep=""),
    #       multiple = FALSE,
    #       selectize = FALSE,
    #       size = 10,
    #       width = "200px"
    #     )
    #   ),
    #   
    #   column(
    #     3,
    #     
    #     textInput("prop", "Propozycja / Komentarz", placeholder = "Definicja i/lub #komentarz#"),
    #     actionButton("zatw_prop", "Zatwierdź propozycję!"),
    #     htmlOutput("leerzeile1"),
    #     textInput("osoba", "Zaimek → osoba", "", placeholder = "Osoba"),
    #     actionButton("zatw_osoba", "Zatwierdź osobę!"),
    #     htmlOutput("leerzeile2"),
    #     textInput("admin", "Do administracji", "", placeholder = "Uwaga"),
    #     actionButton("zatw_admin", "Wyślij uwagę!"),
    #     actionButton("info", "ℹ︎")
    #   )
      #,textOutput("testout")
   # )  
  ),
  
  
  #### SERVER #######
  
  server = function(input, output, session) {
    
    # Test und Debugging
    # output$testout <- 
    
    #### Leerzeilen
    output$leerzeileA <- renderUI(HTML("<br>"))
    output$leerzeile1 <- renderUI(HTML("<br>"))
    output$leerzeile2 <- renderUI(HTML("<br>"))
    
    
    #### Output
    output$outPlTitel <- renderUI(HTML("<h5>Mbh po polsku</>"))
    output$outPl <- renderUI(HTML(plAusgabe(input$inPl)))
    
    output$outSktTitel <- renderUI(HTML("<h5>Mbh w sanskrycie</>"))
    output$outSkt <- renderUI(HTML(sktAusgabe(input$inPl)))
    
    #output$outSlowniczek1 <- renderUI(HTML("<h5>W słowniczku / zaim. = os.</>"))
    #output$outSlowniczek <- renderUI(HTML(slowNiczek(bbAna[bbAna$stro_id == input$inPl, ]$id)))
    
    # output$outOsoba <-  renderUI(HTML(paste( 
    #   "<div style = \"background-color: #81F7F3; width: 50%; height: ", 
    #   str_count(osoAusgabe(input$inPl), "=") * 20, "px; border-radius: 3px;\">", 
    #   osoAusgabe(input$inPl), "</div>", sep=""
    # )))
    
    # output$krot_kom <- renderText(krotKom(input$inPl)) # krotKom(input$inPl) #}
    
    #### Aktive + reaktive Elemente
    
    ## Strophenauswahl per Eingabe
    # observe({
    #   if(!is.null(input$lastkeypresscode)) {
    #     if(input$lastkeypresscode == 13){
    #       
    #       stro_stri <- str_replace_all(input$stro_stri, "(^ | $)", "")
    #       stro_stri <- str_replace_all(stro_stri, " +", " ")
    #       
    #       if (stro_stri %in% bbPl$stro_id){
    #         
    #         updateSelectInput(
    #           session,
    #           "inPl",
    #           label= "Strofa",
    #           choices = bbPl$stro_id,
    #           selected = bbPl[which(bbPl$stro_id == stro_stri), ]$stro_id#,
    #         )
    #         
    #         updateTextInput(
    #           session,
    #           "stro_stri",
    #           label = NULL,
    #           value ="",
    #           placeholder = "Str. + ENTER"
    #         )
    #       }
    #     }
    #   }
    # })
    
    # # Übersetzung
    # observe(updateTextInput(
    #   session,
    #   "przek",
    #   label = NULL,
    #   value = przekAusgabe(input$inPl)
    # ))
    # 
    # observeEvent(input$zatw_przek, {
    #   req(input$inPl)
    #   req(input$przek)
    #   savePrzek(input$inPl, input$przek)
    #   updateTextInput(session, "przek", label = NULL, value = przekAusgabe(input$inPl)) #,placeholder  = "TU PRZEKŁAD")
    #   output$outPl <- renderUI(HTML(plAusgabe(input$inPl)))
    # })
    # 
    # 
    # ## Strophenkommentar
    # # Str.-K.: Eingabe
    # observeEvent(input$zatw_komstro, {
    #   req(input$inPl)
    #   req(input$kom_stro)
    #   saveKomstro(input$inPl, input$kom_stro)
    #   updateTextInput(session, "kom_stro", label = NULL, value = "", placeholder  = "KOMENTARZ DO STROFY")
    #   output$krot_kom <- renderText(krotKom(input$inPl))
    #   #output$outPl <- renderUI(HTML(plAusgabe(input$inPl)))
    # })
    # 
    # # Str.-K.: Modalausgabe
    # observeEvent(input$pok_komstro, {
    #   showModal(modalDialog(
    #     bbPl[which(bbPl$stro_id == input$inPl), ]$kom,
    #     title = paste("Komentarz do", input$inPl),
    #     easyClose = TRUE,
    #     size="s",
    #     footer=NULL
    #   ))
    # })
    # 
    # ## Wörterbuch: Vorschlag
    # observeEvent(input$zatw_prop, {
    #   req(input$inSl)
    #   req(input$prop)# 
    #   saveProp(input$inSl, input$prop)
    #   updateTextInput(session, "prop", label = "Propozycja / Komentarz / Kategoria", value = "", placeholder = "Definicja i/lub #komentarz#")
    #   output$outSlowniczek <-
    #     renderUI(HTML(paste("<div style = \"background-color: #80ff00; width: 50%; height: ", str_count(slowNiczek(bbAna[bbAna$stro_id == input$inPl, ]$id), "=") * 20, "px; border-radius: 3px;\">", slowNiczek(bbAna[bbAna$stro_id == input$inPl, ]$id), "</div>", sep="")))
    # })
    
    ## Satzgrenzen
    # Satzgrenzen: Ausgabe
    # observe(updateTextInput(
    #   session,
    #   "punkt",
    #   label = NULL,
    #   value = punktAusgabe(input$inPl)
    # ))
    
#     # Satzgrenzen: Eingabe
#     observeEvent(input$zatw_punkt, {
#       req(input$inPl)
#       req(input$punkt) 
#       savePunkt(input$inPl, input$punkt)
#       updateTextInput(session, "punkt", label = NULL, value = "", placeholder = "Proszę wybrać kolejną strofę!")
#     })
#     
#     # Person: Eingabe
#     observeEvent(input$zatw_osoba, {
#       req(input$inSl)
#       req(input$osoba)
#       saveOsoba(input$inSl, input$osoba)
#       updateTextInput(session, "osoba", label = NULL, value = "", placeholder = "Dzięki! Kolejna osoba?")
#       
#       output$outOsoba <-  renderUI(HTML(paste( 
#         "<div style = \"background-color: #81F7F3; width: 50%; height: ", 
#         str_count(osoAusgabe(input$inPl), "=") * 20, "px; border-radius: 3px;\">", 
#         osoAusgabe(input$inPl), "</div>", sep=""
#       )))
#       
#     })
#     
#     # Wünsche: Eingabe
#     observeEvent(input$zatw_admin, {
#       req(input$admin)
#       saveAdmin(input$admin)
#       updateTextInput(session, "admin", label = NULL, value = "", placeholder = "Dzięki! Coś jeszcze?")
#     })
#     
#     ## Wörterbuch
#     observe(updateSelectInput(
#       session,
#       "inSl",
#       label = "Do słown.: / Zaimek → osoba:",
#       #choices = paste(bbAna[bbAna$stro_id == input$inPl,]$lemma, "_", bbAna[bbAna$stro_id == input$inPl,]$id, sep="")
#       choices = lfd_lemmata[which(bbAna$stro_id == input$inPl)]
#     ))
#     
#     ## Analyse 
#     observe(updateCheckboxGroupInput(
#       session,
#       "checkAna",
#       label = "Analiza (+ słownik)",
#       choiceNames = paste(
#         bbAna[bbAna$stro_id == input$inPl,]$lemma, ":",
#         bbAna[bbAna$stro_id == input$inPl,]$analyse),
#       choiceValues = bbAna[bbAna$stro_id == input$inPl,]$id
#       #paste(bbAna[bbAna$stro_id == input$inPl,]$def,
#       #                  bbAna[bbAna$stro_id == input$inPl,]$id)                   
#     )
#     )
#     
#     ## Info in Modaldialog
#     observeEvent(input$checkAna, {
#       showModal(modalDialog(
#         HTML(paste(
#           slow[slow$id == input$checkAna,]$def[1], "<br>------ SŁOWNICZEK ‐‐‐‐‐‐<br>",
#           slow[slow$id == input$checkAna,]$pl[1], "<br>------ KOMENTARZE ‐‐‐‐‐‐<br>",
#           slow[slow$id == input$checkAna,]$kom[1]
#         )),
#         title = bbAna[bbAna$id == input$checkAna,]$lemma[1],
#         easyClose = TRUE,
#         size="s",
#         footer=NULL
#       ))
#       updateCheckboxGroupInput(
#         session,
#         "checkAna",
#         label = "Analiza (+ słownik)",
#         choiceNames = paste(
#           bbAna[bbAna$stro_id == input$inPl,]$lemma, ":",
#           bbAna[bbAna$stro_id == input$inPl,]$analyse),
#         choiceValues = bbAna[bbAna$stro_id == input$inPl,]$id
#       )
#     })
#     
#     observeEvent(input$info,
#                  showModal(modalDialog("Tu wkrótce znajdą się najważniejsze informacje.",
#                                        title="INSTRUKCJA OBSŁUGI",
#                                        easyClose = T,
#                                        size="l",
#                                        footer=NULL))
#     )
#     
   }
 )

# rsconnect::deployApp("App-1")

### kāryāṇi

# I
# do słowniczka … -> in eine Zeile
# Pole: przypisy (Spalte "przyp" in bbPl) [neben Komm., beide halbe Breite, mind. drei Zeilen]
# przeszukiwania słowniczka (+ dodanie/modyfikacja)
# Benachrichtigungen über Komm. zu eigenen Versen (nicht zuviel?)
# Fortschrittsbalken

# II
# Kategorien
# słownik z korektą? (osobna )
# Übersetzer: Farbe (unfertig: abgedunkelt)
# Design (Farbe, wellPanel etc.): https://shiny.rstudio.com/articles/css.html

# III
# private Übersetzungen (Schlüssel?)
# Mehrfachvorkommen im WB (?)



