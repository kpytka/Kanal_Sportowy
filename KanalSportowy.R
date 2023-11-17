#Analiza oglądalności Hejt Parków w Kanale Sportowym
# Autor: Krzysztof Pytka
# Data:  17/11/2023

#  _  ___                _   ____                   _
# | |/ /__ _ _ __   __ _| | / ___| _ __   ___  _ __| |_ _____      ___   _
# | ' // _` | '_ \ / _` | | \___ \| '_ \ / _ \| '__| __/ _ \ \ /\ / / | | |
# | . \ (_| | | | | (_| | |  ___) | |_) | (_) | |  | || (_) \ V  V /| |_| |
# |_|\_\__,_|_| |_|\__,_|_| |____/| .__/ \___/|_|   \__\___/ \_/\_/  \__, |
#                                 |_|                                |___/

require("jsonlite")
require("httr")
require("tidyverse")
require("lubridate")

require("ggimage")
require("cowplot")
require("stargazer")


Photo <- list()
Photo[["Borek"]]    <- image_read2("https://pbs.twimg.com/profile_images/1238944031134154752/TFNyRt8y_400x400.jpg")
Photo[["Stanowski"]] <- image_read2("https://pbs.twimg.com/profile_images/1508934566823747586/FF6nNpTK_400x400.jpg")
Photo[["Smokowski"]] <- image_read2("https://pbs.twimg.com/profile_images/848075814801477632/S9qY3iZM_400x400.jpg")
Photo[["Pol"]] <- image_read2("https://pbs.twimg.com/profile_images/1577312539930775553/DdwCFC9q_400x400.jpg")



key <- "XXXXXXX"

channel_id <- "UCIfI7Yo22SMKp5PraztHM6w" #Kanał Sportowy
base <- "https://www.googleapis.com/youtube/v3/"

api_params <- 
  paste(paste0("key=", key), 
        paste0("id=", channel_id), 
        # "part=snippet,contentDetails,statistics",
        "part=snippet,contentDetails,statistics,",
        sep = "&")

api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
  
json_result <- content(api_result, "text", encoding="UTF-8")  


channel.json <- fromJSON(json_result, flatten = T)
channel.df <- as.data.frame(channel.json)


#### Video function - funkcja zliczająca liczbę wyświetleń dla danego filmu

viewCount <- function(video_id)
{

  api_params <- 
    paste(paste0("key=", key), 
          paste0("id=", video_id), 
          "part=statistics",
          sep = "&")
  api_call <- paste0(base, "videos", "?", api_params)
  api_result <- GET(api_call)
  
  json_result <- content(api_result, "text", encoding="UTF-8")  
  video.json <- fromJSON(json_result, flatten = T)
  
  return(video.json$items$statistics.viewCount)
}



###### Odczyt wszystkich odcinków Hejt Parku


playlist_id <- "PLi6mayoXmypQ7CaRfYIP_K6ZVsrm1CqT5" #Playlista z wszystkimi odcinkami Hejt Parku
pageToken <- ""

playlist.df <- NULL

while(!is.null(pageToken))
{


api_params <- 
  paste(paste0("key=", key), 
        paste0("playlistId=", playlist_id), 
        "part=id,snippet,contentDetails",
        "maxResults=20",
        sep = "&")

if(pageToken != "")
{
  api_params <- paste(api_params, 
                      paste0("pageToken=",pageToken), 
                      sep="&")
}

api_call <- paste0(base, "playlistItems", "?", api_params)
api_result <- GET(api_call)

json_result <- content(api_result, "text", encoding="UTF-8")  


channel.json <- fromJSON(json_result, flatten = T)
pageToken <- channel.json$nextPageToken
if(is.null(playlist.df))
{
  playlist.df <- as.data.frame(channel.json)
}else
{
  playlist.df <- bind_rows( playlist.df, 
                            as.data.frame(channel.json))
}

}


#Add viewCount for each movie
playlist.df <- playlist.df %>%  group_by(items.contentDetails.videoId) %>% mutate(viewCount = as.numeric(viewCount(items.contentDetails.videoId))) %>% ungroup()
playlist.df <- playlist.df %>% mutate(items.snippet.publishedAt = ymd_hms(items.snippet.publishedAt))
#Odczyt prowadzącego/(prowadzących) z opisu odcinka
playlist.df <- playlist.df %>% 
              mutate(
                  Stanowski = grepl("Stanowski", items.snippet.description),
                  Smokowski = grepl("Smokowski", items.snippet.description),
                  Borek     = grepl("Borek", items.snippet.description),
                  Pol       = grepl("Pol", items.snippet.description)
                  )
####                  
              

playlist.df$viewCount %>% mean










hosts <- Photo %>% names


Plots <- hosts %>% map(~{
  fig <- playlist.df %>% filter(viewCount <3e6, eval(parse(text=.x))) %>% 
  ggplot(aes(x=(items.contentDetails.videoPublishedAt %>% ymd_hms), 
             y=viewCount))  +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits=c(0, 3*10^6),
    labels = function(x) format(x, scientific = FALSE)
    )+
  xlab("Data")+
  ylab("Liczba wyświetleń")+
  ggtitle(paste("Prowadzący Hejt Parku:", .x) )  
  
  return(ggdraw(fig) +  draw_image(Photo[[.x]], x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2))
  })





playlist.df %>% mutate(wielu = (Stanowski+Borek+Smokowski+Pol)>1)%>% filter(viewCount <5e6) %>% 
  lm(viewCount~  Stanowski+Borek+Smokowski+Pol, data=.) %>% 
  stargazer(style='qje')


a <- hosts %>% map_df(~{playlist.df %>% 
          filter(eval(parse(text=.x))) %>% 
          summarise(Prowadzący = .x,
                    `Średnia oglądalność` = mean(viewCount),
                    `Łączna oglądalność` = sum(viewCount),
                    `Liczba odcinków` = n()
                    ) 
          })

a %>% mutate(`Udział w łącznej oglądalności` = round(`Łączna oglądalność`/sum(`Łączna oglądalność`)*100)) %>% 
  arrange(desc(`Udział w łącznej oglądalności`)) %>% stargazer(style='qje', summary=FALSE, digits = 1)





# Trendy liniowe

wzrosty <- hosts %>% map(~{
            list(.x, playlist.df %>% filter(eval(parse(text=.x))) %>% 
            lm(viewCount ~ tygodnie_istnienia, data= .)%>% summary)
            })

wzrosty
