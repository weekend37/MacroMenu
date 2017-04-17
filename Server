#
# A Shiny app Server that collects recipe-data from Spoonacular.com, and optimizes a personal menu
# based on the input using neos server.
#
library(shiny)
library(httr)
#install.packages("XMLRPC", repos = "http://www.omegahat.org/R") 
#source("http://bioconductor.org/biocLite.R")
# #biocLite("XMLRPC")
# library(XMLRPC)
library(rneos)
library(base64)
library(base64enc)
library(caTools)
library(base64url)

#You'll need one of these XMLRPC package
#
#source("http://bioconductor.org/biocLite.R")
#iocLite("XMLRPC")
#
#install.packages("XMLRPC", repos = "http://www.omegahat.org/R") 
#
#source("http://bioconductor.org/biocLite.R")
#biocLite("XMLRPC")


MASHLYKILL <- c("")
recipe <- list(NULL)
recipeGlobal <- data.frame(NULL)
urlGlobal <- c("")

#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

shinyServer(function(input, output) {               # Define server logic

searchSpoonacularDummy <- function(url) {
  load('data.Rdata')
  recipeGlobal <<- recipeGlobal
  return(candidates)
}

searchSpoonacular <- function(url) {
  # get the reponse from Spoonacular using your MASHKEY
  withProgress(message = 'Searching ...', value = 0, {
    resp <- GET(url, add_headers("X-Mashape-Key" = MASHLYKILL, "Accept" = "application/json"))
    # the number of recipies received, the maximum you can request is 100
    n <- length(content(resp)$results)
    ID <- integer(n) # empty list on intergers of length n
    recipe <<- list() # an empty list to hold our n recipies
    for (i in c(1:n)) {
      ID[i] <- content(resp)$results[[i]]$id # get ID from previous request (above), create request string:
      incProgress(1/n, detail = paste("recipe id ", as.character(ID[i])))
      url <- paste0(c("https://spoonacular-recipe-food-nutrition-v1.p.mashape.com/recipes/"),as.character(ID[i]),c("/information?includeNutrition=true"))
      recipe[[i]] <<- GET(url, add_headers("X-Mashape-Key" = MASHLYKILL, "Accept" = "application/json")) # get recipe
    }
    # Create data frame for the recipies
    readyInMinutes <- integer(n); pricePerServing <- numeric(n); aggregateLikes <- integer(n);
    # now loop through all the recipies and extract the data from the json object
    title = character(0)
    for (i in c(1:n)) {
      title <- c(title,content(recipe[[i]])$title)
      pricePerServing[i] <- content(recipe[[i]])$pricePerServing # / content(recipe[[i]])$servings
      readyInMinutes[i] <- content(recipe[[i]])$readyInMinutes
      aggregateLikes[i] <- content(recipe[[i]])$aggregateLikes
    }
    
    candidates <- data.frame(Selected = logical(n), Ident = ID, Title = title, Minutes = readyInMinutes, Price = pricePerServing, Likes = aggregateLikes)
    
    save(file=c("data.Rdata"), list = c("recipe", "resp")) # for debugging 
    
    return(candidates)
  })
}

writeAMPL <- function() {
  # get the reponse from Spoonacular using your MASHKEY
  withProgress(message = 'Writing spoonacular.dat ...', value = 0, {
    n <- length(recipe)
    ID <- integer(n)
    healthScore <- integer(n); cookingMinutes <- integer(n); preparationMinutes <- integer(n);
    readyInMinutes <- integer(n); pricePerServing <- numeric(n); aggregateLikes <- integer(n);
    servings <- integer(n); cheap <- logical(n); ingredience <- list(); amount <- list();
    unit <- list(); nutrients_name <- list(); nutrients_unit <- list(); nutrients_amount <- list();
    allingredience <- NULL; allnutrients <- NULL; title <- NULL;
    # now loop through all the recipies and extract the data from the json object
    for (i in c(1:n)) {
      incProgress(1/n, detail = paste("extracting data"))
      ID[i] <- content(recipe[[i]])$id
      healthScore[i] <- content(recipe[[i]])$healthScore
      title <- c(title,content(recipe[[i]])$title)
      cheap[i] <- content(recipe[[i]])$cheap
      servings[i] <- content(recipe[[i]])$servings
      if (length(content(recipe[[i]])$cookingMinutes) > 0) {
        cookingMinutes[i] <- content(recipe[[i]])$cookingMinutes
      }
      if (length(content(recipe[[i]])$preparationMinutes) > 0) {
        preparationMinutes[i] <- content(recipe[[i]])$preparationMinutes
      }
      # note: I find the price for some dishes to be rather high, shoule we divide by servings?
      # lets try to get a breakdown for the price from Spoonacular!?
      pricePerServing[i] <- content(recipe[[i]])$pricePerServing # / content(recipe[[i]])$servings
      readyInMinutes[i] <- content(recipe[[i]])$readyInMinutes
      aggregateLikes[i] <- content(recipe[[i]])$aggregateLikes
      # now for some messy work:
      str <- NULL;  astr <- NULL; ustr <- NULL;
      nutrname <- NULL; nutramount <- NULL; nutrunit <- NULL;
      for (j in c(1:length(content(recipe[[i]])$nutrition$ingredients))) {
        str <- gsub(" ","_",c(str,content(recipe[[i]])$nutrition$ingredients[[j]]$name),fixed=TRUE)
        astr <- c(astr, content(recipe[[i]])$nutrition$ingredients[[j]]$amount)
        ustr <- c(ustr, content(recipe[[i]])$nutrition$ingredients[[j]]$unit)
        if (length(content(recipe[[i]])$nutrition$ingredients[[j]]$nutrients) > 0) {
          for (k in c(1:length(content(recipe[[i]])$nutrition$ingredients[[j]]$nutrients))) {
            nutrname <- gsub(" ","_",c(nutrname, content(recipe[[i]])$nutrition$ingredients[[j]]$nutrients[[k]]$name),fixed=TRUE)
            nutramount <- c(nutramount, content(recipe[[i]])$nutrition$ingredients[[j]]$nutrients[[k]]$amount)
            nutrunit <- c(nutrunit, content(recipe[[i]])$nutrition$ingredients[[j]]$nutrients[[k]]$unit)
          }
        }
      }
      ingredience[[i]] <- chartr(c(" %&'-"),c('_____'), str)
      amount[[i]] <- astr
      unit[[i]] <- ustr
      nutrients_name[[i]] <- chartr(c("%&'-"),c('____'), nutrname)
      nutrients_amount[[i]] <- nutramount
      nutrients_unit[[i]] <- nutrunit
      allingredience <- c(allingredience, ingredience[[i]])
      allnutrients <- c(allnutrients, nutrients_name[[i]])
    }
    allnutrients <- sort(unique(allnutrients))
    allingredience <- sort(unique(allingredience))
    
    # Now put this all into an AMPL dat file called "spoonacular.dat":
    cat("set dish := ", as.character(ID), file="spoonacular.dat",sep=" ",append=FALSE)
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    
    cat("set nutrient := ", allnutrients, file="spoonacular.dat",sep=" ",append=TRUE)
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    
    cat("set ingredient := ", allingredience, file="spoonacular.dat",sep=" ",append=TRUE)
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    
    nutrients_recipe <- matrix(rep(n*length(allnutrients),0), nrow = n)
    nutrients_recipe <- as.data.frame(nutrients_recipe,col.names<-sort(allnutrients))
    cat("param dishnutrient := ",file="spoonacular.dat",sep="\n",append=TRUE)
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- sum(nutrients_amount[[i]][which(j==nutrients_name[[i]])])
        if (length(unique(nutrients_unit[[i]][which(j==nutrients_name[[i]])]))>1) {
          print(nutrients_unit[[i]][which(j==nutrients_name[[i]])])
        }
        if (nutrients_recipe[i,j] > 0) {
          cat(as.character(ID[i]),j,as.character(nutrients_recipe[i,j]),file="spoonacular.dat",sep=" ",append=TRUE)
          cat("\n",file="spoonacular.dat",sep="",append=TRUE)
        }
      }
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    ingredience_recipe <- matrix(rep(n*length(allingredience),0), nrow = n)
    ingredience_recipe <- as.data.frame(ingredience_recipe,col.names<-sort(allingredience))
    cat("param dishingredient := ",file="spoonacular.dat",sep="\n",append=TRUE)
    for (i in c(1:n)) {
      for (j in allingredience) {
        ingredience_recipe[i,j] <- sum(amount[[i]][which(j==ingredience[[i]])])
        if (length(unique(unit[[i]][which(j==unit[[i]])]))>1) {
          print(unit[[i]][which(j==unit[[i]])])
        }
        if (ingredience_recipe[i,j]>0) {
          cat(as.character(ID[i]),j,ingredience_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
          cat("\n",file="spoonacular.dat",sep="",append=TRUE)
        }
      }
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    
    # Popularity of the dish
    cat("param aggregateLikes := ",file="spoonacular.dat",sep="\n",append=TRUE)
    for (i in c(1:n)) {
      cat(as.character(ID[i]),aggregateLikes[i],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    
    # The price of a dish
    cat("param pricePerServing := ",file="spoonacular.dat",sep="\n",append=TRUE)
    for (i in c(1:n)) {
      cat(as.character(ID[i]),pricePerServing[i],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
    
    # The time taken to make the dish
    cat("param readyInMinutes := ",file="spoonacular.dat",sep="\n",append=TRUE)
    for (i in c(1:n)) {
      cat(as.character(ID[i]),readyInMinutes[i],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE)
  })
  
    # Average reikningar
    avePrice = sum(pricePerServing)/length(pricePerServing);
    aveLikes = sum(aggregateLikes)/length(aggregateLikes);
    aveMin = sum(readyInMinutes)/length(readyInMinutes);
    
    # Akvarða leidrettingarstudla ut fra averageinu
    maxStudull = max(avePrice, aveLikes, aveMin);
    studullPrice = maxStudull/avePrice;
    studullLikes = maxStudull/aveLikes;
    studullMin = maxStudull/aveMin;
    
    # Skrifa verd leidrettingastudul
    cat("param studullPrice := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),studullPrice,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
    # skrifa like leidrettingastudul
    cat("param studullLikes := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),studullLikes,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }    
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # skrifa tima leidrettingastudul
    cat("param studullMin := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),studullMin,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }    
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # skrifa aherslustudul verds
    cat("param A := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),input$A,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }    
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    
    # skrifa aherslustudul vinsaelda
    cat("param B := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),input$B,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }    
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    
    # skrifa aherslustudul tima
    cat("param C := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),input$C,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }    
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    
    # Fatumagn hvers disks skrifad
    cat("param FITUMAGN := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- sum(nutrients_amount[[i]][which("Fat"==nutrients_name[[i]])])
      }
      cat(as.character(ID[i]),nutrients_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }   
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # proteinmagn hvers disks skrifad
    cat("param PROTEINMAGN := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- sum(nutrients_amount[[i]][which("Protein"==nutrients_name[[i]])])
      }
      cat(as.character(ID[i]),nutrients_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }   
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # kolvetnismagn hvers disks skrifad
    cat("param KOLVETNISMAGN := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- sum(nutrients_amount[[i]][which("Carbohydrates"==nutrients_name[[i]])])
      }
      cat(as.character(ID[i]),nutrients_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }   
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # 1/Fatumagn hvers disks skrifad
    cat("param minFITUMAGN := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- 1/(sum(nutrients_amount[[i]][which("Fat"==nutrients_name[[i]])]))
      }
      cat(as.character(ID[i]),nutrients_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }   
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # 1/proteinmagn hvers disks skrifad
    cat("param minPROTEINMAGN := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- 1/(sum(nutrients_amount[[i]][which("Protein"==nutrients_name[[i]])]))
      }
      cat(as.character(ID[i]),nutrients_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }   
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # 1/kolvetnismagn hvers disks skrifad
    cat("param minKOLVETNISMAGN := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      for (j in allnutrients) {
        nutrients_recipe[i,j] <- 1/(sum(nutrients_amount[[i]][which("Carbohydrates"==nutrients_name[[i]])]))
      }
      cat(as.character(ID[i]),nutrients_recipe[i,j],file="spoonacular.dat",sep=" ",append=TRUE)
      cat("\n",file="spoonacular.dat",sep="",append=TRUE)
    }   
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);
    
    # maxProtein skrifad
    cat("param maxProtein := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),input$maxProtein,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
    # minProtein skrifad
    cat("param minProtein := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),1/input$minProtein,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
    # maxFat skrifad
    cat("param maxFat := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),input$maxFat,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
    # minFat skrifad
    cat("param minFat := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),1/input$minFat,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
    # maxCarbs skrifad
    cat("param maxCarbs := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),input$maxCarbs,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
    # minCarbs skrifad
    cat("param minCarbs := ",file="spoonacular.dat",sep="\n",append=TRUE);
    for (i in c(1:n)) {
      cat(as.character(ID[i]),1/input$minCarbs,file="spoonacular.dat",sep=" ",append=TRUE);
      cat("\n",file="spoonacular.dat",sep="",append=TRUE);
    }
    cat(";\n\n",file="spoonacular.dat",sep="",append=TRUE);  
    
  
    
  "spoonacular.dat has been written"
    

    }


  
  # Here we construct the API search string and get the MASHKEY
  output$urlString <- renderText({
    url <- c("https://spoonacular-recipe-food-nutrition-v1.p.mashape.com/recipes/search?")
    MASHLYKILL <<- input$mashString
    #HALLO
    cuisine <- "" #input$cuisineString
    if (nchar(cuisine) > 0)  url <- paste(url,"cuisine=",cuisine,"&", sep="")
    diet <- input$dietString
    if (nchar(diet) > 0)  url <- paste(url,"diet=",diet, "&", sep="")
    query <- input$queryString
    if (nchar(query) > 0)  url <- paste(url,"query=",query,"&", sep="")
    number <- input$fjoldi
    if (nchar(number) > 0)  url <- paste(url,"number=",number,"&", sep="")
    type <- "main+course"
    if (nchar(type) > 0)  url <- paste(url,"type=",type,"&", sep="")
    # remove the trailing &
    urlGlobal <<- substr(url, 1, nchar(url)-1)
    print(urlGlobal)
  })
  
  # Here we call the API server and get the result and write to file (spoonacular.dat created)
  seekRecipe <- eventReactive(input$seek, {
    if (nchar(urlGlobal)>0) {
      recipeGlobal <<- searchSpoonacular(urlGlobal)
    }
    recipeGlobal
  })
  output$recipeString <- renderDataTable(seekRecipe())
  
  # Write AMPL data file upon request  
  writeAMPLDATA <- eventReactive(input$ampl, {
    if (length(recipe)>0) {
      writeAMPL()
    }
    
  })
  output$writeString <- renderText(writeAMPLDATA())
  
  # Here we send out AMPL model, data ad run script to the NEOS server
  optimizeRecipe <- eventReactive(input$neossend, {
    if (nchar(urlGlobal)>0) {
      tmp <-NgetSolverTemplate(category = "milp", solvername = "MOSEK", inputMethod = "AMPL")
      modc <- paste(paste(readLines("spoonacular.mod"), collapse = "\n"), "\n")
      datc <- paste(paste(readLines("spoonacular.dat"), collapse = "\n"), "\n")
      runc <- paste(paste(readLines("spoonacular.run"), collapse = "\n"), "\n")
      ## create list object
      argslist <- list(model = modc, data = datc, commands = runc, comments = "")
      ## create XML string
      xmls <- CreateXmlString(neosxml = tmp, cdatalist = argslist)
      test <<- NsubmitJob(xmlstring = xmls, user = "rneos", interface = "",id = 0)
      result <- c("Uploaded to Neos server ... try to get results in a few seconds or so")
    }
    else {
      result <- c("Waiting for you to search for recipies ...")
    }
    result
  })
  output$optimizeString <- renderText(optimizeRecipe())
  
  # Here we get the result from the Neos server
  neosFetch <- eventReactive(input$neosget, {
    #result <- NgetFinalResultsNonBlocking(obj = test, convert = TRUE)
    result <- NgetFinalResults(obj = test, convert = TRUE)
    result <- getElement(result,"ans")
    lausn <- strsplit(result, '<lausn>', fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][2]
    lausn <- strsplit(lausn," ")
    save(file="tmp.Rdata", list=c("recipeGlobal", "lausn", "result"))
    recipeGlobal[,"Selected"] <- FALSE
    for (i in c(1:length(lausn[[1]]))) {
      recipeGlobal[recipeGlobal[,"Ident"] == as.integer(lausn[[1]][i]), "Selected"] <- TRUE
    }
    hraefni <- strsplit(result, '<hraefni>', fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]][2]
    hraefni <- strsplit(hraefni," ")
    hraefnistr = c("")
    for (i in c(1:length(hraefni[[1]]))) {
      hraefnistr <- paste(hraefnistr,hraefni[[1]][i]);
    }
    output$recipeString <- renderDataTable(recipeGlobal)
    result <- hraefnistr
  })
  output$neosString <- renderText(neosFetch())
})
