# CALCULATIONf

#prerequisites
# - download script executed
# - cleaning script executed
# - processed transcript-files (.txt) in folder: "./data/miraculous/processed"
# - empty folders "./data/miraculous/plots", "./data/miraculous/tables" must be present

source("./main.R")
source("./metadata scripts/metainfo_series_miraculous.R")

#https://k^ateto.net/netscix2016.html

### Soziogramme over the whole serie

## Gender
gender_sentiment_df <- aggregate(dialogTable_gender_role$Sentiment, list(dialogTable_gender_role$Gender_To, dialogTable_gender_role$Gender_From), mean)
names(gender_sentiment_df)[3] <- "weight"
gender_sentiment_df$weight <- round(gender_sentiment_df$weight,digit=3)
gender_sociogram_igraph<-graph_from_data_frame(gender_sentiment_df)
igraph.options(plot.layout=layout.circle, vertex.size=50)
plot(gender_sociogram_igraph, edge.label = E(gender_sociogram_igraph)$weight, main=paste("Sentiment between Genders"))

## Top 10 to gender (both male & female)
    importantCharacterList <- character_betweenness %>% slice(1:10)
    importantCharacterList <- importantCharacterList$Character
    genderSozioTable_mf <- dialogTable_gender_mf
    genderSozioTable_mf$From <- replace(genderSozioTable_mf$From, genderSozioTable_mf$From %in% importantCharacterList, "Main")
    genderSozioTable_mf$To <- replace(genderSozioTable_mf$To, genderSozioTable_mf$To %in% importantCharacterList, "Main")
    genderSozioTable_mf$From <- replace(genderSozioTable_mf$From, genderSozioTable_mf$From != "Main" & genderSozioTable_mf$Gender_From == "female", "female")
    genderSozioTable_mf$From <- replace(genderSozioTable_mf$From, genderSozioTable_mf$From != "Main" & genderSozioTable_mf$Gender_From == "male", "male")
    genderSozioTable_mf$To <- replace(genderSozioTable_mf$To, genderSozioTable_mf$To != "Main" & genderSozioTable_mf$Gender_To == "female", "female")
    genderSozioTable_mf$To <- replace(genderSozioTable_mf$To, genderSozioTable_mf$To != "Main" & genderSozioTable_mf$Gender_To == "male", "male")
    gender_top_sentiment_df <- genderSozioTable_mf %>% #crosstable
      group_by(From, To) %>% 
      get_summary_stats(Sentiment, type = "mean_sd")
    gender_top_sentiment_df <- gender_top_sentiment_df[c("From", "To", "mean")]
    names(gender_top_sentiment_df)[3] <- "weight"
    
    gender_top_sentiment_df[gender_top_sentiment_df == "female"] <- "Female"
    gender_top_sentiment_df[gender_top_sentiment_df == "male"] <- "Male"
    gender_top_sentiment_df[gender_top_sentiment_df == "Main"] <- "Role Model"
    
    gender_top_sentiment_igraph<-graph_from_data_frame(gender_top_sentiment_df)
    igraph.options(plot.layout=layout.circle, edge.curved=T, edge.label.y =0, edge.label.x =1.5, edge.label.family="Helvetica", vertex.label.family="Helvetica")
    #par(bg = "#f7f7f7")
    #par(bg = "white")
    #colrs <- c("gray50", "tomato", "gold")
    #V(gender_top_sentiment_igraph)
    #E(gender_top_sentiment_igraph)

    plot(gender_top_sentiment_igraph, edge.label = E(gender_top_sentiment_igraph)$weight, #simplify(gender_top_sentiment_igraph)
         #main=paste("Sentiment scores of most central characters and genders"), 
         #vertex.frame.color = "Forestgreen",
         vertex.shape="circle",
         vertex.size=90,
         vertex.frame.color="dark grey", vertex.label.color="black", vertex.color="tomato", 
         vertex.label.cex=1.6, vertex.label.dist=0, 
         edge.label.cex=1.3,
         #edge.color="blue", alpha.f = .5,
         # Edge color
         edge.width=2,                                # Edge width, defaults to 1
         edge.arrow.size=0.2,                         # Arrow size, defaults to 1
         edge.arrow.width=1, 
         edge.color="gray",
         #edge.lty=c("solid"),
         edge.label.dist=10)
   #par(bg = "white")
    
    ggraph(graph = gender_top_sentiment_igraph) +
      geom_node_circle(size = 1, mapping = aes(r =0.03), fill="goldenrod") +
      geom_edge_link(mapping = aes (label = weight),
                     arrow = arrow(type = "closed", angle = 15), 
                     end_cap = circle(8, 'mm'), , 
                     start_cap = circle(8, 'mm'), 
                     colour="grey",
                     label_dodge  = unit(5, "mm"),
                     angle_calc = "along") +
      geom_node_text(mapping = aes(label = "2")) +
      theme_graph()
    
    #https://mr.schochastics.net/material/netvizr/ hier weiter
    
    describeBy(genderSozioTable_mf$Sentiment, list(genderSozioTable_mf$From,genderSozioTable_mf$To))
          
#Top 10 to gender (only female)
    importantCharacterList <- character_betweenness[which(character_betweenness$Gender == "female"), ] %>% slice(1:10)
    importantCharacterList <- importantCharacterList$Character
    genderSozioTable_f <- dialogTable_gender_mf
    genderSozioTable_f$From <- replace(genderSozioTable_f$From, genderSozioTable_f$From %in% importantCharacterList, "Main")
    genderSozioTable_f$To <- replace(genderSozioTable_f$To, genderSozioTable_f$To %in% importantCharacterList, "Main")
    genderSozioTable_f$From <- replace(genderSozioTable_f$From, genderSozioTable_f$From != "Main" & genderSozioTable_f$Gender_From == "female", "female")
    genderSozioTable_f$From <- replace(genderSozioTable_f$From, genderSozioTable_f$From != "Main" & genderSozioTable_f$Gender_From == "male", "male")
    genderSozioTable_f$To <- replace(genderSozioTable_f$To, genderSozioTable_f$To != "Main" & genderSozioTable_f$Gender_To == "female", "female")
    genderSozioTable_f$To <- replace(genderSozioTable_f$To, genderSozioTable_f$To != "Main" & genderSozioTable_f$Gender_To == "male", "male")
    gender_top_sentiment_df <- genderSozioTable_f %>% #crosstable
      group_by(From, To) %>% 
      get_summary_stats(Sentiment, type = "mean_sd")
    gender_top_sentiment_df <- gender_top_sentiment_df[c("From", "To", "mean")]
    names(gender_top_sentiment_df)[3] <- "weight"
    gender_top_sentiment_igraph<-graph_from_data_frame(gender_top_sentiment_df)
    igraph.options(plot.layout=layout.circle, vertex.size=25,edge.curved=T, edge.label.y =0, edge.label.x =1.5)
    par(bg = "#f7f7f7")
    plot(simplify(gender_top_sentiment_igraph), edge.label = E(gender_top_sentiment_igraph)$weight, main=paste("Sentiment scores of most female central characters and genders"))
    par(bg = "white")
    
    describeBy(genderSozioTable_f$Sentiment, list(genderSozioTable_f$From,genderSozioTable_f$To))

#Top 10 to gender (only male)
  importantCharacterList <- character_betweenness[which(character_betweenness$Gender == "male"), ] %>% slice(1:10)
  importantCharacterList <- importantCharacterList$Character
  genderSozioTable_m <- dialogTable_gender_mf
  genderSozioTable_m$From <- replace(genderSozioTable_m$From, genderSozioTable_m$From %in% importantCharacterList, "Main")
  genderSozioTable_m$To <- replace(genderSozioTable_m$To, genderSozioTable_m$To %in% importantCharacterList, "Main")
  genderSozioTable_m$From <- replace(genderSozioTable_m$From, genderSozioTable_m$From != "Main" & genderSozioTable_m$Gender_From == "female", "female")
  genderSozioTable_m$From <- replace(genderSozioTable_m$From, genderSozioTable_m$From != "Main" & genderSozioTable_m$Gender_From == "male", "male")
  genderSozioTable_m$To <- replace(genderSozioTable_m$To, genderSozioTable_m$To != "Main" & genderSozioTable_m$Gender_To == "female", "female")
  genderSozioTable_m$To <- replace(genderSozioTable_m$To, genderSozioTable_m$To != "Main" & genderSozioTable_m$Gender_To == "male", "male")
  gender_top_sentiment_df <- genderSozioTable_m %>% #crosstable
    group_by(From, To) %>% 
    get_summary_stats(Sentiment, type = "mean_sd")
  gender_top_sentiment_df <- gender_top_sentiment_df[c("From", "To", "mean")]
  names(gender_top_sentiment_df)[3] <- "weight"
  gender_top_sentiment_igraph<-graph_from_data_frame(gender_top_sentiment_df)
  igraph.options(plot.layout=layout.circle, vertex.size=25,edge.curved=T, edge.label.y =0, edge.label.x =1.5)
  par(bg = "#f7f7f7")
  plot(simplify(gender_top_sentiment_igraph), edge.label = E(gender_top_sentiment_igraph)$weight, main=paste("Sentiment scores of most central male characters and genders"))
  par(bg = "white")
  
  describeBy(genderSozioTable_m$Sentiment, list(genderSozioTable_m$From,genderSozioTable_m$To))
  
  
#----END H2 iGRAPH---------------------------------------------------------------