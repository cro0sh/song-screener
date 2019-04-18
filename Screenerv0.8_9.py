import tkinter as tk                # python 3
from tkinter import font  as tkfont # python 3
from tkinter import *
import rpy2.robjects as robjects
import math, datetime
##import rpy2.robjects.lib.rvest as rv
##import rpy2.robjects.lib.tidytext as ty
from rpy2.robjects.packages import importr
base = importr('base')
tidy = importr('tidytext')
rvest = importr('rvest')
dplyr = importr('dplyr', on_conflict="warn")
ggplot2 = importr('ggplot2')
stringr = importr('stringr')
ggraph = importr('ggraph')
igraph = importr('igraph')
magick = importr('magick')
stringr = importr('stringr')
from PIL import Image, ImageTk
import os
from tkinter.ttk import Progressbar
import time
import threading
##import functools
##from __future__ import print_function    # (at top of module)
from spotipy.oauth2 import SpotifyClientCredentials
from spotipy.oauth2 import *
import json
import spotipy
import time
import sys
import pprint
import pandas as pd
import pprint
import sys
import os
import subprocess
##from spotipy.oauth2 import SpotifyClientCredentials
##import spotipy
import spotipy.util as util

class SampleApp(tk.Tk):

    def __init__(self, *args, **kwargs):
        tk.Tk.__init__(self, *args, **kwargs)

        self.title_font = tkfont.Font(family='Times', size=15, weight="bold")

        # the container is where we'll stack a bunch of frames
        # on top of each other, then the one we want visible
        # will be raised above the others
        container = tk.Frame(self)
        container.pack(side="top", fill="both", expand=True)
        container.grid_rowconfigure(0, weight=1)
        container.grid_columnconfigure(0, weight=1)

        self.frames = {}
        for F in (StartPage, PageOne, PageTwo, PageThree, PageFour, PageFive):
            page_name = F.__name__
            frame = F(parent=container, controller=self)
            self.frames[page_name] = frame

            # put all of the pages in the same location;
            # the one on the top of the stacking order
            # will be the one that is visible.
            frame.grid(row=0, column=0, sticky="nsew")

        self.show_frame("StartPage")
        
##        self.alst = []
##        self.read_title()

        

    def show_frame(self, page_name):
        '''Show a frame for the given page name'''
        frame = self.frames[page_name]
        frame.tkraise()

##    def read_title(self):   
##        
##        txt = open("/home/c/title.txt")
##        txt = str(txt.read())
##        txt = txt.replace('"', '')            
##        self.alst.append(txt)
##        print('in read_title', self.alst)
##        print(test)


##class default_page(self):
        

class StartPage(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        self.controller = controller
        
        self.canvas1 = Canvas(self, relief = FLAT, bg="#476581",
                                            width = 210, height = 1920)
        self.canvas1.pack(side = TOP, anchor = NW, padx = 10, pady = 10)
        self.canvas1.place(x=0, y=0)
        self.canvas2 = Canvas(self, relief = FLAT, background = "#FFFFFF",
                                            width = 180, height = 250)        
        self.canvas2.pack()  
        self.canvas2.place(x=750, y=550, anchor=CENTER)
        
##        increment = 0
##        textz = ["Screener", "Local Files", "Playlists", "Converter", "Info"]
##        pages = ["PageOne", "PageTwo", "PageThree", "PageFour", "PageFive"]
##        for btntext, page in zip(textz, pages):
##            increment = increment + 35
##            button1 = Button(self, text = btntext, command = lambda: controller.show_frame(page), anchor = W)
##            button1.configure(width = 10, activebackground = "#33B5E5", relief = FLAT, bg='#476581')
##            button1_window = self.canvas1.create_window(35, 10 + increment, anchor=NW, window=button1)
##            button1.config(highlightthickness=0, fg='white')
        

        
        increment = 0
##        textz = ["Screener", "Local Files", "Playlists", "Converter", "Info"]
##        pages = ["StartPage", "PageTwo", "PageThree", "PageFour", "PageFive"]
        textz = ["Local Files", "Playlists", "Converter", "Info"]
        pages = ["PageTwo", "PageThree", "PageFour", "PageFive"]
##        self.button1 = []
        for btntext, page in zip(textz, pages):
            increment = increment + 35
            button1 = Button(self, text = btntext, command = lambda page=page: controller.show_frame(page), anchor = W)
            button1.configure(width = 20, activebackground = "#33B5E5", relief = FLAT, bg='#476581', font=("Helvetica", 10))
##            button1_window = self.canvas1.create_window(35, 10 + increment, anchor=NW, window=button1)
            button1_window = self.canvas1.create_window(0.5, 200 + increment, anchor=NW, window=button1)
            button1.config(highlightthickness=0, fg='white')

        screener_label = Label(self, text="SCREENER", bg='#476581', fg='white')        
        screener_label.pack()
        screener_label.config(relief = FLAT, font=("Helvetica", 10))
        screener_label.place(x=5,y=30)

        increment_two = 0
        screener_buttons = ["Get Song Features",  "Compare Song to Playlist", "Compare Playlists", "Playlist to Sentiment", "Playlist to Top10s"]
        commands = [self.get_analytics_button, self.compare_to_playlist_button, self.compare_playlists_button, self.playlist_to_sentiment_button, self.top_tracks]
##        self.button1 = []
        for btn, commandz in zip(screener_buttons, commands):
            increment_two = increment_two + 35
            button2 = Button(self, text = btn, command = commandz, anchor = W)
            button2.configure(width = 20, activebackground = "#33B5E5", relief = FLAT, bg='#476581', font=("Helvetica", 10))
            button2_window = self.canvas1.create_window(20, 20 + increment_two, anchor=NW, window=button2)
            button2.config(highlightthickness=0, fg='white')

##        def buttons(self):
##            increment = 0
##            textz = ["Screener", "Local Files", "Playlists", "Converter", "Info"]
##            pages = ["PageOne", "PageTwo", "PageThree", "PageFour", "PageFive"]
##            for btntext, page in zip(textz, pages):
##                increment = increment + 35
##                button1 = Button(self, text = btntext, command = lambda page=page: controller.show_frame(page), anchor = W)
##                button1.configure(width = 10, activebackground = "#33B5E5", relief = FLAT, bg='#476581')
##                button1_window = self.canvas4.create_window(35, 10 + increment, anchor=NW, window=button1)
##                button1.config(highlightthickness=0, fg='white')
            
        robjects.r('''
screen <- function(song, returnflag) { 
search <- html_form(read_html("http://www.bing.com"))[[1]]  
session <- html_session("http://www.bing.com")
form <- set_values(search, q = paste0("genius", song))
result <- submit_form(session, form) 
# test <- read_html(result)
ff <- jump_to(session, result$url)
s <- ff %>% follow_link("Genius Lyrics")

# session <- html_session("http://www.duckduckgo.com")
# form <- set_values(search, q = paste0("genius ", song))
# result <- rvest::submit_form(session, form, submit="btnI", httr::config(followlocation = F)) 

test <- read_html(s)
test4 <- test %>% html_node("div.song_body-lyrics") %>% html_text()
test5 <- gsub("\n", "", test4)
test5 <- gsub("More on Genius", "", test5)
# splitLyrics <- strsplit(test5, "Lyrics")
title <- s$url
dput(title, file="/home/c/url.txt")
title <- str_match(title, "([^/]+$)")[,1]
title <- gsub("-lyrics", "", title)
title <- gsub("-", " ", title)
song_dir <- paste0("/home/c/", title)  
dir.create(file.path(paste0("/home/c/", title)))
dput(title, file="/home/c/title.txt")
write.table(title, file=paste0("/home/c/", title, ".txt"))
setwd("/home/c/")


# title <- splitLyrics[[1]][1]
# title <- trimws(title)

# test6 <- gsub("[Produced by [\\w\\s]+]", "", test5)
# test7 <- tidy(test6)
# test6 <- unnest_tokens(word, test6)

# tt <- "There's a stranger in my bedThere's a pounding in my headGlitter all over the roomPink flamingos in the poolI smell like a minibarDJ's passed out in the yardBarbie's on the barbequeThis a hickie or a bruise?[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again[Verse 2]Trying to connect the dotsDon't know what to tell my bossThink the city towed my carChandelier is on the floorRipped my favorite party dressWarrant's out for my arrestThink I need a ginger aleThat was such an epic fail[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again(Do it all again) This Friday night[InterlT.G.I.F., T.G.I.F., T.G.I.FT.G.I.F., T.G.I.F., T.G.I.F[Instrumental Break][ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all again"
splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
# test77 <- data.frame(text=splits)
names(splits) <- "test"
test75 <- data.frame()
test75 <- bind_rows(test75, splits)

ngrams <- function(twothree) {
data(stop_words)
library(tidyr)
library(ggraph)
library(igraph)
AFINN <- get_sentiments("afinn")
test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
if (twothree == 2) {
test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
set.seed(1)
a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
plot2 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
ggsave("bigram.png", plot = plot2, path = "~", device='png')
plotone <- image_read(paste0("/home/c/", 'bigram.png'))
plot_8x6 <- image_scale(plotone, "1000x935!")
image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
file.copy('bigram.png', song_dir)
}
else {
  other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
  test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2", "word3"), sep=" ")
  test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
  test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort=T)
  bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
  set.seed(2)
  a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
  plot3 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void() + ggtitle(title)
##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
    ggsave("trigram.png", plot = plot3, path = "~", device='png')
  plotone <- image_read(paste0("/home/c/", 'trigram.png'))
  plot_8x6 <- image_scale(plotone, "1000x935!")
  image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
  file.copy('trigram.png', song_dir)
}
# test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
# test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
negation_words <- c("not","no","never", "cant", "can't", "cannot")
negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
# bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
# set.seed(1)
# a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
# plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
# plot2
# ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()


}

  sentiment_analysis <- function() { 
    test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)
    nrc <- get_sentiments("nrc")
    mh <- c("headache", "headaches", "violent",  "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
    bad <- data.frame(word=mh)
    for (x in 1:length(bad$mh)) { 
      bad$sentiment <- "bad"
    }
    bad$word <- as.character(bad$word)
    names(test76) <- c("word", "n")
    # nrcNeg <- nrc %>% filter(sentiment == 'negative')
    badandAll <- bind_rows(bad, nrc)
    for (sentimentz in unique(badandAll$sentiment)) {
      sentimentx <- badandAll %>% filter(sentiment == sentimentz)
      test77 <- test76 %>% inner_join(sentimentx) # %>% count(word, sort=T)
      if (nrow(test77) == 0) {
      next
      }
      test77 <- test77[1:10,]
      test77 <- na.omit(test77)
    
      filename <- paste0(sentimentz, ".png")
      plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title)
      ##  ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
      ggsave(filename, plot = plot1, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", filename))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
      file.copy(filename, song_dir)
    }
    senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
    names(senti_score) <- c("word", "n", "score")
    ##names(senti_score) <- c("word", "n")
    
    senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n))
    senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
    ##senti_score <- trimws(sub("1", "", senti_score))
    senti_score <- as.numeric(senti_score) * 100
    senti_score <- as.character(senti_score)
    return(senti_score)
  }


ngrams(2)
ngrams(3)
sentiment_analysis()

}


        ''')

        robjects.r('''
screen_movie <- function(movie) {
  search <- html_form(read_html("http://www.google.com"))[[1]]  
  session <- html_session("http://www.google.com")
  search_query <- paste0("imsdb.com/scripts ", movie)
  form <- set_values(search, q = search_query)
  result <- submit_form(session, form) 
  # test <- read_html(result)
  ff <- jump_to(session, result$url)
  
  s <- ff %>% follow_link("Script at IMSDb")
  url_imsdb <- s$url
  url_grep <- grepl("http://www.imsdb.com/Movie%20Scripts/", url_imsdb)
  if (url_grep == TRUE) {
  s <- s %>% follow_link("Read")
  }
  title <- s$url
  dput(title, file="/home/c/url.txt")
  title <- str_match(title, "([^/]+$)")[,1]
  title <- gsub("-lyrics", "", title)
  title <- gsub("-", " ", title)
  title <- gsub(".html", "", title)
  dput(title, file="/home/c/title.txt")
  song_dir <- paste0("/home/c/", title)  
  dir.create(file.path(paste0("/home/c/", title)))
test <- read_html(s)
test4 <- test %>% html_node("td.scrtext") %>% html_text()
test5 <- gsub("\n", "", test4)
test5 <- gsub("\r", "", test5)

splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
# test77 <- data.frame(text=splits)
names(splits) <- "test"
test75 <- data.frame()
test75 <- bind_rows(test75, splits)

test75$test <- gsub("", "", test75$test)
# test <- gsub("[[:digit:]]", "", test)

ngrams_movie <- function(twothree) {
  data(stop_words)
  library(tidyr)
  library(ggraph)
  library(igraph)
  library(tidytext)
  AFINN <- get_sentiments("afinn")
  # test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
  if (twothree == 2) {
    test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree)
    test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
    other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
    test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
    test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
    bigram_graph <- test_bigram_counts[1:75,]
    bigram_graph <- na.omit(bigram_graph)
    bigram_graph <- bigram_graph %>% graph_from_data_frame()
    
    set.seed(1)
    a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
    plot2 <- ggraph(bigram_graph, layout="nicely") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
    ##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
    ggsave("bigram.png", plot = plot2, path = "~", device='png')
    plotone <- image_read(paste0("/home/c/", 'bigram.png'))
    plot_8x6 <- image_scale(plotone, "1000x935!")
    image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
    file.copy("bigram.png", song_dir)
  }
  else {
    
    test_bigram <- test75 %>% unnest_tokens(trigram, test, token="ngrams", n=twothree)
    other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
    test_bigram_seperate <- test_bigram %>% separate(trigram, c("word1", "word2", "word3"), sep=" ")
    test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
    test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort=T)
    bigram_graph <- test_bigram_counts[1:60,]
    bigram_graph <- bigram_graph %>% graph_from_data_frame()
    set.seed(2)
    a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
    plot3 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void()
    ##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
    ggsave("trigram.png", plot = plot3, path = "~", device='png')
    plotone <- image_read(paste0("/home/c/", 'trigram.png'))
    plot_8x6 <- image_scale(plotone, "1000x935!")
    image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
    file.copy("trigram.png", song_dir)
  }
  # test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
  # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
  negation_words <- c("not","no","never", "cant", "can't", "cannot")
  negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
  # bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
  # set.seed(1)
  # a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
  # plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
  # plot2
  # ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()
  
  
}

sentiment_analysis_movie <- function() { 
test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)

nrc <- get_sentiments("nrc")
mh <- c("headache", "headaches", "violent",  "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
bad <- data.frame(word=mh)
for (x in 1:length(bad$mh)) { 
  bad$sentiment <- "bad"
}
bad$word <- as.character(bad$word)
names(test76) <- c("word")
# nrcNeg <- nrc %>% filter(sentiment == 'negative')
badandAll <- bind_rows(bad, nrc)
for (sentimentz in unique(badandAll$sentiment)) {
  sentimentx <- badandAll %>% filter(sentiment == sentimentz)
  test77 <- test76 %>% inner_join(sentimentx)
  names(test77) <- c("word","n","sentiment")
  test77 <- test77[1:10,]
  test77 <- na.omit(test77)
  filename <- paste0(sentimentz, ".png")
  plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title)
  # ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
  ggsave(filename, plot = plot1, path = "~", device='png')
  plotone <- image_read(filename)
  plot_8x6 <- image_scale(plotone, "1000x935")
  image_write(plot_8x6, path = filename, format = "png")
  file.copy(filename, song_dir)
  print(filename)
}
senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
names(senti_score) <- c("word", "n", "score")
senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n)) 
senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
##senti_score <- trimws(sub("1", "", senti_score))
senti_score <- as.numeric(senti_score) * 100
senti_score <- as.character(senti_score)
return(senti_score)
}

ngrams_movie(2)
ngrams_movie(3)
sentiment_analysis_movie()

}''')

        robjects.r('''
screen2 <- function(song, returnflag) { 
  search <- html_form(read_html("http://www.google.com"))[[1]]  
  session <- html_session("http://www.google.com")
  form <- set_values(search, q = paste0("genius", song))
  result <- submit_form(session, form) 
  # test <- read_html(result)
  ff <- jump_to(session, result$url)
  s <- ff %>% follow_link("Genius Lyrics")
  
  # session <- html_session("http://www.duckduckgo.com")
  # form <- set_values(search, q = paste0("genius ", song))
  # result <- rvest::submit_form(session, form, submit="btnI", httr::config(followlocation = F)) 
  
  test <- read_html(s)
  test4 <- test %>% html_node("div.song_body-lyrics") %>% html_text()
  test5 <- gsub("\n", "", test4)
  test5 <- gsub("More on Genius", "", test5)
  # splitLyrics <- strsplit(test5, "Lyrics")
  title <- s$url
  dput(title, file="/home/c/url.txt")
  title <- str_match(title, "([^/]+$)")[,1]
  title <- gsub("-lyrics", "", title)
  title <- gsub("-", " ", title)
  song_dir <- paste0("/home/c/", title)  
  dir.create(file.path(paste0("/home/c/", title)))
  dput(title, file="/home/c/title.txt")
  write.table(title, file=paste0("/home/c/", title, ".txt"))
  setwd("/home/c/")
  
  
  # title <- splitLyrics[[1]][1]
  # title <- trimws(title)
  
  # test6 <- gsub("[Produced by [\\w\\s]+]", "", test5)
  # test7 <- tidy(test6)
  # test6 <- unnest_tokens(word, test6)
  
  # tt <- "There's a stranger in my bedThere's a pounding in my headGlitter all over the roomPink flamingos in the poolI smell like a minibarDJ's passed out in the yardBarbie's on the barbequeThis a hickie or a bruise?[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again[Verse 2]Trying to connect the dotsDon't know what to tell my bossThink the city towed my carChandelier is on the floorRipped my favorite party dressWarrant's out for my arrestThink I need a ginger aleThat was such an epic fail[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again(Do it all again) This Friday night[InterlT.G.I.F., T.G.I.F., T.G.I.FT.G.I.F., T.G.I.F., T.G.I.F[Instrumental Break][ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all again"
  splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
  # test77 <- data.frame(text=splits)
  names(splits) <- "test"
  test75 <- data.frame()
  test75 <- bind_rows(test75, splits)
  
  ngrams <- function(twothree) {
    data(stop_words)
    library(tidyr)
    library(ggraph)
    library(igraph)
    AFINN <- get_sentiments("afinn")
    test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
    if (twothree == 2) {
      test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
      other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
      test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
      test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
      bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      set.seed(1)
      a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
      plot2 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
      ##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
      ggsave("bigram.png", plot = plot2, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", 'bigram.png'))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
      file.copy('bigram.png', song_dir)
    }
    else {
      other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
      test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2", "word3"), sep=" ")
      test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
      test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort=T)
      bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      set.seed(2)
      a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
      plot3 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void() + ggtitle(title)
      ##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
      ggsave("trigram.png", plot = plot3, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", 'trigram.png'))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
      file.copy('trigram.png', song_dir)
    }
    # test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
    # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
    negation_words <- c("not","no","never", "cant", "can't", "cannot")
    negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
    # bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
    # set.seed(1)
    # a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
    # plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
    # plot2
    # ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()
    
    
  }
  
  sentiment_analysis <- function() { 
    test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)
    nrc <- get_sentiments("nrc")
    mh <- c("headache", "headaches", "violent",  "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
    bad <- data.frame(word=mh)
    for (x in 1:length(bad$mh)) { 
      bad$sentiment <- "bad"
    }
    bad$word <- as.character(bad$word)
    names(test76) <- c("word", "n")
    # nrcNeg <- nrc %>% filter(sentiment == 'negative')
    badandAll <- bind_rows(bad, nrc)
    df_largest_items <- data.frame(items=0, filenames="test.png")
    for (sentimentz in unique(badandAll$sentiment)) {
      sentimentx <- badandAll %>% filter(sentiment == sentimentz)
      test77 <- test76 %>% inner_join(sentimentx) # %>% count(word, sort=T)
      if (nrow(test77) == 0) {
        next
      }
      test77 <- test77[1:10,]
      test77 <- na.omit(test77)
      
      filename <- paste0(sentimentz, ".png")
      num_rows <- nrow(test77)
      
      df_largest <- data.frame(items=num_rows, filenames=filename)
      df_largest_items <- bind_rows(df_largest_items, df_largest)
      
      plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title)
      ##  ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
      ggsave(filename, plot = plot1, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", filename))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
      file.copy(filename, song_dir)
    }
    filename_most_graphed <- df_largest_items[which.max(df_largest_items$items),][,2]
    # title_no_extension <- gsub(".png", "", title)
    dput(filename_most_graphed, file=paste0("/home/c/", title, "/", "most_graphed.txt"))
    senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
    names(senti_score) <- c("word", "n", "score")
    ##names(senti_score) <- c("word", "n")
    
    senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n))
    senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
    ##senti_score <- trimws(sub("1", "", senti_score))
    senti_score <- as.numeric(senti_score) * 100
    senti_score <- as.character(senti_score)
    return(senti_score)
  }
  
  
  ngrams(2)
  ngrams(3)
  sentiment_analysis()
  
}''')

        robjects.r('''

 something <- function(...) {
          
          lst = list(...)
##          if (length(lst) > 2) {
##            stop("error: length of lst greater than 2")
##          }
          df <- ldply(lst, data.frame)
          names(df) <- "playlists"
##print(df$playlists[2])
##print(lst[[1]][[2]])
          
          # for (playlist in unique(df$X..i..)) {
##          playlist_filename <- paste0("/home/c/", "playlist ", df$playlists[1], ".csv")
playlist_filename <- paste0("/home/c/", "playlist ", lst[[1]][[1]], ".csv")
          playlist <- read.csv(playlist_filename, sep="\t")
          playlist_filename2 <- paste0("/home/c/", "playlist ", lst[[1]][[2]], ".csv")
##          playlist_filename2 <- paste0("/home/c/", "playlist ", df$playlists[2], ".csv")
          playlist2 <- read.csv(playlist_filename2, sep="\t")
          playlist <- playlist %>% select(speechiness, danceability, acousticness, energy, valence, liveness)
          playlist_melted <- tidyr::gather(playlist, key)
          playlist_melted$id <- lst[[1]][[1]]
          playlist2 <- playlist2 %>% select(speechiness, danceability, acousticness, energy, valence, liveness)
          playlist_melted2 <- tidyr::gather(playlist2, key)
          playlist_melted2$id <- lst[[1]][[2]]
          df2 <- bind_rows(playlist_melted, playlist_melted2)
          # plot_playlists <- ggplot(df2, aes(x = key, y = value, fill=key)) +
          #   geom_bar(stat='identity') + ggtitle("Playlist 1 vs Playlist 2") + facet_grid(~id) + coord_flip()
          # plot_playlists
          df2_agg <- aggregate(df2$value, by=list(df2$key, df2$id), FUN=mean)
          names(df2_agg) <- c("key","id","mean")
          plot_playlists_boxplots <- ggplot(df2, aes(x=key, y=value, fill=key)) + geom_boxplot() + ggtitle('Playlist 1 vs Playlist 2 Box Plots') + facet_grid(~id)
          plot_playlists_boxplots
          plot_playlist_means <- ggplot(df2_agg, aes(x = key, y = mean, fill=key)) +
            geom_bar(stat='identity', color='black') + ggtitle("Playlist 1 vs Playlist 2 Means") + facet_grid(~id) + coord_flip()
          plot_playlist_means
          
          if (dir.exists("/home/c/playlists") == FALSE) {
            dir.create("/home/c/playlists/")
          }
          
          save_plot_filename <- "/home/c/playlists/pl_vs_pl_means.png"
          ggsave(save_plot_filename, plot = plot_playlist_means, device='png')
          plotone <- image_read(save_plot_filename)
          plot_8x6 <- image_scale(plotone, "1000x935!")
          image_write(plot_8x6, path = save_plot_filename, format = "png")
          
          save_plot_filename <- "/home/c/playlists/pl_vs_pl_box.png"
          ggsave(save_plot_filename, plot = plot_playlists_boxplots, device='png')
          plotone <- image_read(save_plot_filename)
          plot_8x6 <- image_scale(plotone, "1000x935!")
          image_write(plot_8x6, path = save_plot_filename, format = "png")
        }

        ''')

        global r_f

        
##        r_o = robjects.r['titlez']
##        print('title is ' + str(r_o))
##        global photo
        
##        r_f = robjects.r['screen2']
##        self.res = r_f('kk')
        
##        import pprint
##        print(self.res)
        
##        sentiment_posneg = self.res[0]
##        sentiment_posneg = str(sentiment_posneg)
##        sentiment_posneg.replace('[1]', '')       
##        
##        label = tk.Label(self, text="Sentiment Score " + sentiment_posneg, fg="#476581")
##        label.pack()
##        label.place(x=1600,y=15)
        
##        label.pack(side="top", fill="x", pady=10)
        
        self.textBox=Text(self, height=1.5, width=50)        
        self.textBox.pack()
        self.textBox.place(x=350, y=15)
        self.textBox.bind("<Return>", self.textbox_enter_key)
##        test = False        
        
        self.buttonCommit=Button(self, height=1, width=10, bg="#476581", fg="white", text="Search", font=("Helvetica", 11), 
                    command=lambda: self.retrieve_input())   
        self.buttonCommit.pack()
        self.buttonCommit.place(x=715,y=16)

        buttonadd=Button(self, height=1, width=10, bg="#476581", fg="white", text="Add", font=("Helvetica", 11), 
                    command=lambda: self.add("single"))   
        buttonadd.pack()
        buttonadd.place(x=830,y=16)

        remove_button = Button(self, height=1, width=10, bg="#476581", fg="white", text='Remove', command=self.remove, font=("Helvetica", 11))
        remove_button.pack()
        remove_button.place(x=945,y=16)

        buttondata=Button(self, height=1, width=10, bg="#476581", fg="white", text="Data",  font=("Helvetica", 11), 
                    command=lambda: self.data())   
        buttondata.pack()
        buttondata.place(x=1060,y=16)

        
        self.not_in_bad_agg_label = tk.Label(self, text='', fg="#476581", font="bold")
        self.not_in_bad_agg_label.pack()
        self.not_in_bad_agg_label.place(x=1275, y = 400)

##        self.update_button = Button(self, text='Update', command=lambda *args: self.change_image())
        
        
        os.chdir('/home/c')
        self.cwd = os.getcwd()        
        self.imageVar = StringVar(self)
        self.images = ['bigram', 'trigram', 'bad', 'trust', 'fear', 'negative', 'sadness', 'suprise', 'positive', 'disgust', 'joy', 'anticipation']
##        try:
##        self.imageVar.set('bigram')
##        except _tkinter.TclError:
##            self.imageVar.set('bad')
##        self.studFiles.trace("w", self.change_image())
        self.songs = {'Katy Perry': ['bigram', 'trigram', 'bad', 'trust', 'fear', 'negative', 'sadness', 'suprise', 'positive', 'disgust', 'joy', 'anticipation']
        , 'Katy Perry2': ['bigram2', 'trigram3', 'bad4', 'trust', 'fear', 'negative', 'sadness', 'suprise', 'positive', 'disgust', 'joy', 'anticipation']}
         # Number is corresponding list index
##        studDropDown = OptionMenu(self, self.imageVar, *self.images)
##        studDropDown.config(font=("Times", 13), height=1, width=10, bg="#476581", fg="white")
##        studDropDown["menu"].config(font=("Times", 13), bg="#476581", fg="white")
##        studDropDown.pack()
##        studDropDown.place(x=1275, y=14)
        
##        self.studDropDown = OptionMenu(self, self.imageVar, '')
##        self.studDropDown.config(font=("Times", 13), height=1, width=10, bg="#476581", fg="white")
##        self.studDropDown["menu"].config(font=("Times", 13), bg="#476581", fg="white")
##        self.studDropDown.pack()
##        self.studDropDown.place(x=1375, y=14)        

        self.imageVar2 = StringVar(self)

##        self.variable_a.trace('w', self.update_options)
        
##        self.studDropDown2 = OptionMenu(self, self.imageVar2, *self.songs.keys())
##        self.studDropDown2.config(font=("Times", 13), height=1, width=10, bg="#476581", fg="white")
##        self.studDropDown2["menu"].config(font=("Times", 13), bg="#476581", fg="white")
##        self.studDropDown2.pack()
##        self.studDropDown2.place(x=1250, y=14)

        self.imageVar2.set('Katy Perry')
##        countries = self.songs[self.imageVar2.get()]
##             
##        self.imageVar.set(countries[0])
        
##        studDropDown2 = OptionMenu(self, self.imageVar2, *self.songs)
##        studDropDown2.config(font=("Times", 13), height=1, width=10, bg="#476581", fg="white")
##        studDropDown2["menu"].config(font=("Times", 13), bg="#476581", fg="white")
##        studDropDown2.pack()
##        studDropDown2.place(x=1150, y=14)

        
            
##        self.path = StringVar(self)
####        for x in self.songs:
####            for y in self.songs[x]:
##        self.path.set(self.cwd + '/' + self.variable_b.get() + '.png')
##        img = ImageTk.PhotoImage(Image.open(self.path.get()))

        
        
        
        

        

        
####        self.panel.img = img
##        self.imageVar.trace("w", lambda *args: self.change_image())
        
##        self.imageVar2.trace("w", self.update_optionz())
##        self.imageVar2.trace("w", lambda *args: self.change_image())
        
####        self.read_title()

        

##        label = tk.Label(self, text=self.controller.alst, font=self.controller.title_font)
##        label.pack()
##        label.place(x=800,y=15)
        
##        print('song_lst', self.song_lst)
##        test = "testing"       
        self.example = []

        MODES = [
            ("Song", "song_selection"),
            ("Movie", "movie_selection"),
            ("Spotify", "spotify_selection"), 
        ]

        self.v = StringVar(self)
        self.v.set("song_selection") # initialize

        increment_selection = 0
        for text, mode in MODES:
            increment_selection = increment_selection + 60
            b = Radiobutton(self, text=text,
                            variable=self.v, value=mode, command=self.radio)
            b.pack(anchor=W)
            b.place(x=325+increment_selection,y=50)

##        import webbrowser
##        self.selection_var = v.get()
##        if selection_var == "movie_selection":
##            webbrowser.open("http://imdb.com")    
##            print(selection_var)
        self.p = Progressbar(self, orient=HORIZONTAL, length=200, mode='determinate')
        self.radioboolean = False
##        self.inputValue = self.textBox.get("1.0","end-1c")

##        self.adict = {'Mike will made it 23': ['bigram', 'trigram', 'bad', 'trust', 'fear', 'negative', 'sadness', 'suprise', 'positive', 'disgust', 'joy', 'anticipation']
##        , 'Omarion post to be': ['abc','abc2','abc3']}

##        self.adict = {'Mike will made it 23': ['bigram', 'trigram', 'bad', 'trust', 'fear', 'negative', 'sadness', 'suprise', 'positive', 'disgust', 'joy', 'anticipation']
##                     
##        }
##        self.adict = {'Song or Movie': ['Graph', 'Graph2'], ' ': []}      
##        self.adict = {'Song or Movie': ['Graph', 'Graph2']}
        from collections import OrderedDict
        
##        self.adict = {}
        self.adict = OrderedDict()
##        self.adict = LastUpdatedOrderedDict(self.adict)
    
        

        self.variable_a = StringVar(self)
        self.variable_b = StringVar(self)
        self.variable_a.trace('w', self.update_optionz)
##        self.variable_a.trace("w", lambda *args: self.change_image())
##        try:
        self.optionmenu_a = OptionMenu(self, self.variable_a, *self.adict.keys() if self.adict.keys() else [' '])
##        except TypeError:
##            pass
##        optionmenu_a_width = len(max(self.adict.keys(), key=len))
##        self.optionmenu_a.config(font=("Helvetica", 11), height=1, width=optionmenu_a_width, bg="#476581", fg="white")
        self.optionmenu_a.config(font=("Helvetica", 11), height=1, width=23, bg="#476581", fg="white")
        self.optionmenu_a["menu"].config(font=("Helvetica", 11), bg="#476581", fg="white")
        self.optionmenu_a.pack()
        self.optionmenu_a.place(x=1225, y=14)
##        self.optionmenu_a.place(x=1500, y=14)  
        self.optionmenu_b = OptionMenu(self, self.variable_b, *self.adict.values() if self.adict.values() else [' '])
        self.optionmenu_b.config(font=("Helvetica", 11), height=1, width=15, bg="#476581", fg="white")
        self.optionmenu_b["menu"].config(font=("Helvetica", 11), bg="#476581", fg="white")
        self.optionmenu_b.pack()
        self.optionmenu_b.place(x=1425, y=14)
##        self.optionmenu_b.place(x=1300, y=14)
        
        self.add_boolean = False
        self.startup = True
        
##        self.variable_a.set('Mike will made it 23')
##        self.variable_a.set('Song or Movie')
##        countries = self.adict[self.variable_a.get()]
##        self.variable_b.set(countries[0])
        
##        self.optionmenu_a.pack()
##        self.optionmenu_b.pack()
##        self.pack()
        
        

        
        
        self.path = StringVar(self)
##        txt = open("/home/c/title.txt")
##        txt = str(txt.read())
##        txt = txt.replace('"', '')
##        titlez = txt.replace('\n', '')
        titlez = self.add_two()
##        print(titlez)
##        print(type(titlez))
##        print(type(self.variable_b.get()))
##        for dirpath, dirnames, files in os.walk('/home/c/' + titlez):
##            if files:
####                print(dirpath, 'has files')
##                self.path.set(self.cwd + '/' + titlez + '/' + self.variable_b.get() + '.png')
##                print('using new dir')
##            if not files:
##                self.path.set(self.cwd + '/' + self.variable_b.get() + '.png')
        
##        self.path.set(self.cwd + '/' + titlez + '/' + self.variable_b.get() + '.png')
##        print(self.path.get())

        

##        entities = os.listdir('/home/c/' + self.variable_a.get())
##        for entity in entities:
##            if os.path.isfile(entity):
##                self.path.set(self.cwd + '/' + self.variable_a.get() + '/' + self.variable_b.get() + '.png')
##            else:
##                self.path.set(self.cwd + '/' + self.variable_b.get() + '.png')

        
##        img = ImageTk.PhotoImage(Image.open(self.path.get()))
        img = ImageTk.PhotoImage(Image.open('/home/c/Pictures/Selection_008.png'))        
        self.panel = Label(self.canvas2, image = img)
        self.panel.pack()
        
        self.update_button = Button(self, height=1, width=12, bg="#476581", fg="white", text='Remove All',
                                    command=self.remove_all_button, font=("Helvetica", 11))
        self.update_button.pack()
        self.update_button.place(x=1615,y=16)

##        self.compare_playlists = Button(self, height=1, width=15, bg="#476581", fg="white", text='Compare Playlists',
##                                        command=self.compare_playlists_button, font=("Helvetica", 11))
##        self.compare_playlists.pack()
##        self.compare_playlists.place(x=1750,y=16)

        

##        self.variable_a.trace("w", lambda *args: self.change_image())
        self.variable_b.trace("w", lambda *args: self.change_image())
        
##    def doit(self):
##        print(self.variable_b.get())
        self.all_removed = False

        self.menu = self.optionmenu_b['menu']
##                menu.delete(0, 'end')
                
        self.menu2 = self.optionmenu_a['menu']
##                self.menu2.delete(0, 'end')
        self.newlst = []
        self.keylist = []
##        self.lst = glob.glob('/home/c/' + self.add_two() + '/' + '*.png')
##        self.lengthoflist = len(self.lst)
        self.lstlengths = []
        self.song_titles_boolean = False
        self.top_list = []
        self.not_in_bad_agg_list = []
        self.no_links_list = []
##        self.song_titles_label = 
##           
##        self.lstlengths.append(self.lengthoflist)
        
##        self.add() ##################
        self.song_titles_label = tk.Label(self, text='', fg="#476581", font="bold")
        self.song_titles_label.pack()
        self.song_titles_label.place(x=1250,y = 55)
##        self.find_images()
        print('SELF ADICT ON STARTUP ---', self.adict)

    def textbox_enter_key(self, event):
        print("you pressed enter")
        self.retrieve_input()
        
    def updater(self, *args):
##        del self.adict['Song']
        countries = self.adict[self.variable_a.get()]
##                print('COUNTRIES IN UP OPTIONZ +++', countries)
        self.variable_b.set(countries[0])
                
                
            ##        except (_tkinter.TclError, TclError, tkinter.TclError):
            ##            self.variable_b.set('Graph')
                    

            ##        if self.add_boolean == True:
            ##            countries = self.adict[self.add_two()]
            ##            self.variable_b.set(countries[0])
                    
            ##        print('COUNTRIES !!! ', countries)
            ##        if self.add_boolean == True:
            ##            pass
            ##        else:
                
            ##        print(countries)

##                menu = self.optionmenu_b['menu']
        self.menu.delete(0, 'end')
        
##                self.menu2 = self.optionmenu_a['menu']
        self.menu2.delete(0, 'end')
        

        for country in countries:
            self.menu.add_command(label=country, command=lambda nation=country: self.variable_b.set(nation))
        print('self.adict in updater before 2nd loop', self.adict)
        for x in self.adict.keys():
##                    if x == "Songs or Movie":
##                        self.menu2.add_command(label=x, command=self.variable_a.set('Songs or Movie'))
##                    else:
                self.menu2.add_command(label=x, command=lambda nation=x: self.variable_a.set(nation))
        
    def update_optionz(self, *args):
##        self.dict = {'Asia': ['Japan', 'China', 'Malaysia'],
##                     'Europe': ['Germany', 'France', 'Switzerland']}
##        print('SELF ADD BOOLEAN', self.add_boolean)
        
##        else:
##        del self.adict['Song']
        if self.startup == True:
            pass
        else:
            if self.all_removed == True:
                self.adict['Songs or Movie'] = ['Graph', 'Graph2'] 
                self.variable_a.set('Songs or Movie')
##                self.variable_b.set('Graph')

                countries = self.adict[self.variable_a.get()]
                print('COUNTRIES IN UP OPTIONZ +++', countries)
##                self.variable_b.set(countries[0])

##                menu = self.optionmenu_b['menu']
                self.menu.delete(0, 'end')
##                menu2_child = self.optionmenu_a.children["menu"]
##                print(menu2_child)
##                self.menu2 = self.optionmenu_a['menu']
                self.menu2.delete(0, 'end')

                for country in countries:
                    self.menu.add_command(label=country, command=lambda nation=country: self.variable_b.set(nation))
                for x in self.adict.keys():            
##                        if self.variable_a.get() != 'Songs or Movie':
                    
                    self.menu2.add_command(label=x, command=lambda nation=x: self.variable_a.set(nation))
            else: 
                countries = self.adict[self.variable_a.get()]
##                print('COUNTRIES IN UP OPTIONZ +++', countries)
                self.variable_b.set(countries[0])
                
                
            ##        except (_tkinter.TclError, TclError, tkinter.TclError):
            ##            self.variable_b.set('Graph')
                    

            ##        if self.add_boolean == True:
            ##            countries = self.adict[self.add_two()]
            ##            self.variable_b.set(countries[0])
                    
            ##        print('COUNTRIES !!! ', countries)
            ##        if self.add_boolean == True:
            ##            pass
            ##        else:
                
            ##        print(countries)

##                menu = self.optionmenu_b['menu']
                self.menu.delete(0, 'end')
                
##                self.menu2 = self.optionmenu_a['menu']
                self.menu2.delete(0, 'end')
                

                for country in countries:
                    self.menu.add_command(label=country, command=lambda nation=country: self.variable_b.set(nation))
                print('self.adict in update_optionz before 2nd loop', self.adict)
                for x in self.adict.keys():
##                    if x == "Songs or Movie":
##                        self.menu2.add_command(label=x, command=self.variable_a.set('Songs or Movie'))
##                    else:
                        self.menu2.add_command(label=x, command=lambda nation=x: self.variable_a.set(nation))
        ##        print('SELF DICT IN update optionz', self.adict)
        ##        countries = self.songs[self.imageVar2.get()]        
        ##        self.imageVar.set(countries[0])
        ##        menu = self.studDropDown['menu']
        ##        menu.delete(0, 'end')
        ##            for country in countries:
        ####            menu.add_command(label=country, command=lambda nation=country: self.imageVar.set(nation))
        ##                menu.add_command(label=country, command=lambda nation=country: self.imageVar.set(nation))
        
    
##    def reduce_concat(self, x, sep=""):
##        return functools.reduce(lambda x, y: str(x) + sep + str(y), x)
##
##    def paste(self, *lists, sep=" ", collapse=None):
##        result = map(lambda x: self.reduce_concat(x, sep=sep), zip(*lists))
##        if collapse is not None:
##            return self.reduce_concat(result, sep=collapse)
##        return list(result)
        
    def data(self):
        import webbrowser
        txt = open("/home/c/url.txt")
        txt = str(txt.read())
        txt = txt.replace('"', '')
        webbrowser.open(txt)
        
    def radio(self):
##        import webbrowser
        
##        self.inputValue = self.textBox.get("1.0","end-1c")
##        print('self.inputValue', self.inputValue)
        pass
##        selection_var = self.v.get()
##        if selection_var == "movie_selection":
##            r_f = robjects.r['screen_movie']
##            self.res = r_f(self.inputValue)
##        if selection_var == "song_selection":
##            r_f = robjects.r['screen2']
##            self.res = r_f(self.inputValue, 1)
##        if selection_var == "spotify_selection":
##            self.get_analytics_button(playlist_list=None, search_str=self.inputValue)
            
##        selection_var = self.v.get()
        
##        if selection_var == "movie_selection":
####            webbrowser.open("http://imdb.com")
##            self.radioboolean = True
####        elif selection_var == "spotify_selection":
####            pass
##        else:
##            self.radioboolean = False
            
##    def update_options(self, *args):
##        countries = self.songs[self.imageVar2.get()]
##        self.imageVar.set(countries[0])
##
##        menu = self.studDropDown['menu']
##        menu.delete(0, 'end')
##
##        for country in countries:
##            menu.add_command(label=country, command=lambda nation=country: self.imageVar.set(nation))

##        self.change_image()
        
    def change_image(self, playlist_bool = False):

##        countries = self.songs[self.imageVar2.get()]
####        print(self.songs[self.imageVar2.get()])
##        self.imageVar.set(countries[0])
##
##        menu = self.studDropDown['menu']
##        menu.delete(0, 'end')

        def test():
            self.imageVar.set(country)
##            print(self.imageVar.get())
            self.path.set(self.cwd + '/' + self.imageVar.get() + '.png')
            img = ImageTk.PhotoImage(Image.open(self.path.get()))
            self.panel.configure(image = img)
            self.panel.img = img
            
##        for country in countries:
####            menu.add_command(label=country, command=lambda nation=country: self.imageVar.set(nation))
##            menu.add_command(label=country, command=lambda nation=country: self.imageVar.set(nation))
##        for x in self.songs:
##            for y in self.songs[x]:

            
        titlez = self.add_two()
        try:
            entities = os.listdir('/home/c/' + self.variable_a.get())
            for entity in entities:
                if os.path.isfile(entity):
                    self.path.set(self.cwd + '/' + self.variable_a.get() + '/' + self.variable_b.get() + '.png')
                else:
                    self.path.set(self.cwd + '/' + self.variable_b.get() + '.png')
##            if playlist_bool == True:
                

                    
    ##        self.path.set(self.cwd + '/' + self.variable_b.get() + '.png')

            img = ImageTk.PhotoImage(Image.open(self.path.get()))
            self.panel.configure(image = img)
            self.panel.img = img
            
        except FileNotFoundError:            
            entities = os.listdir('/home/c/' + self.variable_a.get())
            for entity in entities:
                if os.path.isfile(entity):
                    self.path.set(self.cwd + '/' + self.variable_a.get() + '/playlists/' + self.variable_b.get() + '.png')
                else:
##                    self.path.set(self.cwd + '/playlists/' + self.variable_b.get() + '.png')
                    self.path.set(self.cwd + '/playlists/' + self.variable_b.get() + '.png')
                    
            img = ImageTk.PhotoImage(Image.open(self.path.get()))
            self.panel.configure(image = img)
            self.panel.img = img
##            if playlist_bool == True:

##        self.panel = Label(self.canvas2, image = img)
##        self.panel.pack()
    def add_two(self):
        txt = open("/home/c/title.txt")
        txt = str(txt.read())
        txt = txt.replace('"', '')
        txt = txt.replace('\n', '')
##        self.add_boolean = True
        return(txt)

    def add_update(self):
        if self.add_boolean == True:
            self.variable_a.set(self.add_two()) ## so it doesnt change songs after adding.
##            print('SELF DICT IN ADD UPDATE', self.adict)
    def add(self, single_or_playlist):
        self.startup = False
        self.all_removed = False
        if single_or_playlist == "single":
            self.title = open("/home/c/title.txt")
            self.title = str(self.title.read())
            self.title = self.title.replace('"', '')
            self.title = self.title.replace('\n', '')
    ##        self.example.append(txt)
    ##        title_graph_paste = [txt + s for s in self.images]
    ##        self.adict[txt] = title_graph_paste
            
            
    ##        self.find_images()
    ##        
    ##        self.adict[txt] = self.newlst       
    ##        self.find_images()
            self.adict[self.title] = self.find_images(title=self.title)
            print('SELF ADICT +++', self.adict)
        else:
            self.res_two = str(self.res)
            self.res_two = self.res_two.replace('[1]', '')
            self.res_two = self.res_two.replace('"', '')
            self.res_two = self.res_two.replace('\n', '')
            self.res_two = self.res_two.strip()
            print(self.res_two)
            self.titles = open(self.res_two)
            self.titles = str(self.titles.read())
            self.titles = self.titles.replace('titles', '')
            for x in range(1,101):
                self.titles = self.titles.replace('"' + str(x) + '"', '')
            self.titles = self.titles.replace('"', '')
            self.titlez = self.titles.split('\n')
            ##new_title = []
            for index, item in enumerate(self.titlez):
                if item == '':
                    self.titlez.pop(index)
            ##    new_title.append(item.strip())
            self.new_title = []
            for item in self.titlez:
                self.new_title.append(item.strip())
            ##title = list(title)
##            print(new_title)           

            for item in self.new_title:
                self.adict[item] = self.find_images(title=item)
##            print(adict)
##        print(self.example)
##        print('self.adict is', self.adict)
        
        self.add_boolean = True
        self.add_update()
        self.update_optionz()
        
        for key in self.adict.keys():
            self.keylist.append(key)
        print('keylist inside add()', self.keylist)
##        optionmenu_a_width = len(max(self.adict.keys(), key=len))
##        self.optionmenu_a.config(font=("Helvetica", 11), height=1, width=optionmenu_a_width, bg="#476581", fg="white")
##        if single_or_playlist == "single":
        most_graphed = open("/home/c/" + self.variable_a.get() + "/most_graphed.txt")
        most_graphed = str(most_graphed.read())
        most_graphed = most_graphed.replace('.png', '')
        most_graphed = most_graphed.replace('"', '')
        most_graphed = most_graphed.replace('\n', '')
        self.variable_b.set(most_graphed)
##        most_graphed.close()
        
        
##        return(self.title)
            
    
    
    def find_images(self, playlist_flag=None, title=None):
        import glob
        if playlist_flag is not None:
            lst = glob.glob('/home/c/playlists/' + '*.png')
            lengthoflist = len(lst)
            
            self.lstlengths.append(lengthoflist)
##            print('lstlengths !!! ', self.lstlengths)
##            print('find images glob lst', lst)
            pattern = "^(.+)/([^/]+)$"
            
            for i, x in enumerate(lst):    
                d = re.search(pattern, x)
                grouped = d.group(2)
                splits = os.path.splitext(grouped)[0]    
                self.newlst.append(splits)
##            newer = []
##            for lst in newlst:
##                newer.append(lst)
                
##            self.newlst.sort()
##            print('self.newlst', self.newlst)
##            print('SELF.NEWLIST IS *** ', self.newlst)
##            print('SELF.NEWLIST SLICE +++', self.newlst[sum(self.lstlengths)-self.lstlengths[len(self.lstlengths)-1]:sum(self.lstlengths)])
##            print('SELF.NEWLIST +++', self.newlst)
##            print(
##            print('SELF.NEWLIST *** SLICE ***', self.newlst[sum(self.lstlengths)-self.lstlengths[len(self.lstlengths)-1]:sum(self.lstlengths)])
            return(self.newlst[sum(self.lstlengths)-self.lstlengths[len(self.lstlengths)-1]:sum(self.lstlengths)])
        
        if title is not None:
##            for title in self.new_title:
                
            lst = glob.glob('/home/c/' + title + '/' + '*.png')
            lengthoflist = len(lst)
            
            self.lstlengths.append(lengthoflist)
    ##            print('lstlengths !!! ', self.lstlengths)
    ##            print('find images glob lst', lst)
            pattern = "^(.+)/([^/]+)$"
            
            for i, x in enumerate(lst):    
                d = re.search(pattern, x)
                grouped = d.group(2)
                splits = os.path.splitext(grouped)[0]    
                self.newlst.append(splits)
    ##            newer = []
    ##            for lst in newlst:
    ##                newer.append(lst)
                
    ##            self.newlst.sort()
    ##            print('self.newlst', self.newlst)
    ##            print('SELF.NEWLIST IS *** ', self.newlst)
    ##            print('SELF.NEWLIST SLICE +++', self.newlst[sum(self.lstlengths)-self.lstlengths[len(self.lstlengths)-1]:sum(self.lstlengths)])
##            print('SELF.NEWLIST +++', self.newlst)
    ##            print(
##            print('SELF.NEWLIST *** SLICE ***', self.newlst[sum(self.lstlengths)-self.lstlengths[len(self.lstlengths)-1]:sum(self.lstlengths)])
            return(self.newlst[sum(self.lstlengths)-self.lstlengths[len(self.lstlengths)-1]:sum(self.lstlengths)])
            
    def count_keys(self, dict_test):
        return sum(1+count_keys(v) if isinstance(v,dict) else 1 for _,v in dict_test.items())

##    print(count_keys(self.adict))
    def remove(self, remove_all=False):
##        if self.count_keys(self.adict) == 1:
##            print('cannot empty dict')
##        elif self.variable_a.get() == 'Song or Movie':
##            print('cannot remove init')
##        else:
        self.adict_copy = self.adict
        
        if remove_all:
            playlistz = self.adict['playlists']
            self.adict.clear() 
            self.adict['playlists'] = playlistz
##            for x in self.adict_copy.keys():
##                if x == "playlists":
##                    next
##                else:
##                    del self.adict[x]
            self.update_optionz()
    ##        print('AFTER DELETION SELF.ADICT %%%', self.adict)
    ##        self.refresh()
        else:
            del self.adict[self.variable_a.get()]
            print('self adict after removal', self.adict)
            
            index_to_pop = self.keylist.index(self.variable_a.get())
            self.keylist.pop(index_to_pop)
            print('after pop', self.keylist)

            
            if self.count_keys(self.adict) > 0:
                self.variable_a.set(self.keylist[0])
            else:
                self.all_removed = True
                
        ##            self.variable_a.set('Songs or Movie')
        ##            self.variable_b.set('Graph')
        ##            self.panel.pack_forget()
                self.update_optionz()
            
##            self.menu2 = self.optionmenu_a['menu']
##            self.menu2.delete(0, 'end')
####            new_choices = self.adict
##            for choice in self.adict.keys():
##                self.menu2.add_command(label=choice, command=tk._setit(self.variable_a, choice))
    ##        if len(self.keylist) > 0:       
                            
        
        
##        self.menu2 = self.optionmenu_a['menu']
##        self.menu2.delete(0, 'end')
##
##    # Insert list of new options (tk._setit hooks them up to var)
####        new_choices = self.adict
##        for choice in self.adict.keys():
##            self.menu2.add_command(label=choice, command=tk._setit(self.variable_a, choice))
##        print('new_choices in len > 0', new_choices)
##        else:
####            from collections import defaultdict
####            self.adict = defaultdict(lambda : None)
##            self.adict['Song or Movie'] = 'Graph'
##            self.keylist.append('Song or Movie')
##            print('self adict', self.adict)
##            self.variable_a.set(self.keylist[0])
##            
##            
##            self.menu2 = self.optionmenu_a['menu']
##            self.menu2.delete(0, 'end')
##
##    # Insert list of new options (tk._setit hooks them up to var)
##            new_choices = self.adict
##            for choice in new_choices.keys():
##                self.menu2.add_command(label=choice, command=tk._setit(self.variable_a, choice))
##            print('new_choices in else', new_choices)
        
##        self.update_optionz()
    def refresh(self):
        pass
        
    # Reset var and delete all old options
        

    def retrieve_input(self):
##        global r_f
##        print(self.example)
        
##        inputValue = self.textBox.get("1.0","end-1c")
           
##        def search_bar():
##            def bar():
##                self.p.pack()
##                self.p.start()
##                time.sleep(5)
##                self.p.stop()
##                self.p.pack_forget()
##                self.buttonCommit['state']='normal'
##            self.buttonCommit['state']='disabled'
##            threading.Thread(target=bar).start()
##            
##        search_bar()
        
##        self.panel = Label(self.canvas2, image = img)
##        self.panel.pack()
        
        def bar():
            import time
            self.p['value']=20
            self.update_idletasks()
            time.sleep(1)
            self.p['value']=45
            self.update_idletasks()
            time.sleep(1)
            self.p['value']=75
            self.update_idletasks()
##            time.sleep(1)
            self.p['value']=100        
            self.p.pack_forget()
            
##        search_bar()
        self.p.pack()
        self.p.place(x=860, y=55)
        bar()
        self.p.pack_forget()
        
        self.inputValue = self.textBox.get("1.0","end-1c")
        
        print('self.inputValue', self.inputValue)
        selection_var = self.v.get()
        if selection_var == "movie_selection":
            r_f = robjects.r['screen_movie']
            self.res = r_f(self.inputValue)
        if selection_var == "song_selection":
            r_f = robjects.r['screen2']
            self.res = r_f(self.inputValue, 1)
        if selection_var == "spotify_selection":
            self.get_analytics_button(playlist_list=None, search_str=self.inputValue)
        
##        if self.v.get() == "movie_selection":
##            r_f = robjects.r['screen_movie']
##            self.res = r_f(inputValue)
##        if self.v.get() == "song_selection":
##            r_f = robjects.r['screen2']
##            self.res = r_f(inputValue, 1)
##        if self.v.get() == "spotify_selection":
##            self.get_analytics_button(playlist_list=None, search_str=inputValue)

        
##        if self.movie_boolean:
##            r_f = robjects.r['screen_movie']
##            self.res = r_f(inputValue)
####        else:
##        if self.screen_boolean:
##            r_f = robjects.r['screen2']
##            self.res = r_f(inputValue, 1)
##        if self.spotify_boolean:
            
##            r_f = robjects.r['screen_movie']
##            self.res = r_f(inputValue)
        
##        self.title = r_f(inputValue, 2)
            
        self.panel.pack_forget()

        
##        self.read_title()

        
##        sentiment_posneg = self.res[0]
##        sentiment_posneg = str(sentiment_posneg)
##        sentiment_posneg.replace('[1]', '')
##        
##        label = tk.Label(self, text="Sentiment Score " + str(sentiment_posneg), fg="#476581")
##        label.pack()
##        label.place(x=1600,y=15)
##        


##        photo = PhotoImage(file="~/plot1_1.png")
##        self.Artwork = Label(self.canvas2, image=photo)
##        self.Artwork.photo = photo     
##        self.Artwork.pack()

        self.path = StringVar(self)
        self.path.set("/home/c/bigram.png")
        img = ImageTk.PhotoImage(Image.open(self.path.get()))      

        self.panel = Label(self.canvas2, image = img)
        self.panel.pack()
        self.panel.img = img
        self.imageVar.trace("w", lambda *args: self.change_image())

    def dict_generator(self, indict, pre=None):
        pre = pre[:] if pre else []
        if isinstance(indict, dict):
            for key, value in indict.items():
                if isinstance(value, dict):
                    for d in self.dict_generator(value, [key] + pre):
                        yield d
                elif isinstance(value, list) or isinstance(value, tuple):
                    for v in value:
                        for d in self.dict_generator(v, [key] + pre):
                            yield d
                else:
                    yield pre + [key, value]
        else:
            yield indict

    def spotify_search_results_refresh(self, *args):

        lst = self.pack_slaves()
        lst2 = self.slaves()
        print('pack slaves lst ', lst, 'slaves lst2 ', lst2)
        for l in lst:
            l.destroy()
##        for x in range(0,8):
##            self.song_titles_label.pack_forget()
##            self.song_titles_label.destroy()
##        if self.song_titles_boolean == True:
##            for x in range(0,len(self.song_titles_to_validate_short)):
##                self.song_titles_label.pack_forget()
##                self.song_titles_label.destroy()
####                self.song_titles_label = tk.Label(self, text='', fg="#476581", font="bold", justify="left")
####                self.song_titles_label.pack()
####                self.song_titles_label.place(x=1275,y = 60)
##                self.song_titles_boolean = False
##                print('Ran spotify_search_results_refresh(), made boolean FALSE...')
##        else:
##            pass
        
 
    def get_analytics_button(self, *args, playlist_list=None, search_str=None):
##        import spotipy as sp
        
        
        print(self.variable_a.get())
##        playlist_list = list(playlist_list)
        client_credentials_manager = SpotifyClientCredentials(client_id = 'replace_me', client_secret = 'replace_me')
        self.sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)
##        print('PLAYLIST LIST INSIDE GET ANALTICS BTN', playlist_list, 'TYPE ***', type(playlist_list))
        if playlist_list is not None and isinstance(playlist_list, str):
##        if type(playlist_list == "str"):
            username = playlist_list.split(':')[2]
            playlist_id = playlist_list.split(':')[4]        
            self.results = self.sp.user_playlist(username, playlist_id)
##            analytics_dict = self.get_analytics("playlist", pl)
##            print('^^^^^ in get_analytics_Button')
            return(self.results)


        elif playlist_list is not None and isinstance(playlist_list, list):
            for pl in playlist_list:

##            uri = 'spotify:user:cro0sh:playlist:1dBmJ6ICpIdLvwOARRJe3y'
##                uri = playlist
                username = pl.split(':')[2]
                playlist_id = pl.split(':')[4]        
                self.results = self.sp.user_playlist(username, playlist_id)
                analytics_dict = self.get_analytics("playlist", pl)
##                print('analytics_dict niside get_analytics_button PLAYLIST', analytics_dict)
            return(self.results)
        
##        elif playlist_list is not None and len(playlist_list) == 1:
##            username = playlist_list.split(':')[2]
##            playlist_id = pl.split(':')[4]        
##            self.results = self.sp.user_playlist(username, playlist_id)
####            analytics_dict = self.get_analytics("playlist", pl)
##            print('^^^^^ in get_analytics_Button')
##            return(self.results)
        else:
            if search_str is not None:
                self.results = self.sp.search(search_str, limit = 10)
            else:
                search_str = self.variable_a.get()
                self.results = self.sp.search(search_str, limit = 10)
    ##            self.song_titles_label.destroy()
            song_titles_to_validate = []
            song_titles_to_validate = self.playlist_to_artist_and_song(results = self.results, search = True)
            self.song_titles_to_validate_short = []
            
##            for title in song_titles_to_validate_short:
            print('song_titles_to_validate ', song_titles_to_validate )
            
##            analytics_lst = []
            for title in song_titles_to_validate:
                if len(title) > 29:
                    substring = title[29:32]
                    newstring = title.replace(substring, ' ...')                
                    self.song_titles_to_validate_short.append(newstring[0:33])
                else:
                    self.song_titles_to_validate_short.append(title)
            print('song_titles_to_validate_short ', self.song_titles_to_validate_short )
            song_titles_init = 0
##            self.song_titles_label = tk.Label(self, text='', fg="#476581", font="bold", justify="left")
##            self.spotify_search_results_refresh()
##            self.song_titles_label.destroy()
##            self.song_titles_label.pack_forget()
            
            self.song_title_test = StringVar()
            song_title_lst = []

            for song in self.top_list:
                song.pack_forget()
                song.destroy() # remove label from window
            self.top_list = []

##            self.song_titles_label1 = tk.Label(self, text='', fg="#476581", font="bold", justify="left")
##            self.song_titles_label1.pack()
####            self.song_titles_label.config(text=self.song_title_test.get())
##            self.song_titles_label1.place(x=1275,y = 60 + song_titles_init)
            
            self.song_title_test.set('test')
            if self.song_titles_boolean == False:
                for titles in self.song_titles_to_validate_short:
##                    import string
##
##
##                    import random
##                    random_letter = random.choice(string.letters)
    ##            for title in song_titles_to_validate:
                    song_titles_init += 35
                    self.song_title_test.set(titles)
                    self.song_titles_label = tk.Label(self, text='', fg="#476581", font="bold", justify="left")
                    self.song_titles_label.pack()
                    self.song_titles_label.config(text=self.song_title_test.get())
                    self.song_titles_label.place(x=1275,y = 60 + song_titles_init)
                    self.top_list.append(self.song_titles_label)
##                    
##                    self.song_titles_boolean = True
                    
##            lst = self.pack_slaves()
##            lst2 = self.slaves()
##            print('pack slaves lst ', lst, 'slaves lst2 ', lst2)
                

            
            analytics_dict = self.get_analytics("search")
            print('analytics_dict niside get_analytics_button SEARCH', analytics_dict)

            
            spotify_search_label = tk.Label(self, text='SPOTIFY SEARCH RESULTS', fg="#476581", font="bold")
            spotify_search_label.pack()
            spotify_search_label.place(x=1275,y = 58)

            features_label = tk.Label(self, text='AUDIO FEATURES', fg="#476581", font="bold")
            features_label.pack()
            features_label.place(x=1600,y = 58)
            
            analytics = ['Energy',  "Speechiness", "Danceability",     "Loudness",         "Acousticness",     "Tempo", "Valence", 
    "Liveness" ]
            analytics_label_init = 0
            analytics_lst = []
            for metric in analytics:
                analytics_label_init += 35
                analytics_label = tk.Label(self, text=metric, fg="#476581", font="bold")
                analytics_label.pack()
                analytics_label.place(x=1600,y = 60 + analytics_label_init)
                tolower = metric.lower()
                for metrics in analytics_dict.values():
                    analytics_label2 = tk.Label(self, text=metrics[tolower], fg="#000000", font="bold")
                    analytics_label2.pack()
                    analytics_label2.place(x=1750, y = 60 + analytics_label_init)

    ##        self.graph_analytics = Button(self, height=1, width=16, bg="#476581", fg="white", text='Graph and Add',
    ##                                    command=self.graph_button, font=("Helvetica", 11))
    ##        self.graph_analytics.pack()
    ##        self.graph_analytics.place(x=1300,y=375)

    ##        self.add_analytics = Button(self, height=1, width=10, bg="#476581", fg="white", text='Add',
    ##                                    command=self.quit, font=("Helvetica", 11))
    ##        self.add_analytics.pack()
    ##        self.add_analytics.place(x=1415,y=375)


##            self.compare = Button(self, height=1, width=20, bg="#476581", fg="white", text='Compare to Playlist',
##                                        command=self.compare_to_playlist_button, font=("Helvetica", 11))
##            self.compare.pack()
##            self.compare.place(x=1415,y=375)

            
    ##        analytics_label_init = 0
    ##        for key, metrics in analytics_dict.items:
                
            return(self.results)
            self.graph_button()

    def get_analytics(self, search_or_playlist, playlistURI=None):
##        self.spotify_search_results_refresh()
        gen = self.dict_generator(self.results)
        uris = []
        for x in gen:
            for y in x:
                try:
                    if "spotify:track" in y and search_or_playlist == "playlist":       
                        uris.append(x[4])
                        
                    if "spotify:track" in y and search_or_playlist == "search":
    ##                if "spotify:track" in y and search_or_playlist == "search":
                        uris.append(x[3])
                        
                except TypeError:
                    next

        ##print(uris)
        ##    print(x)
        ##print(uris)
        ##print(json.dumps(results, indent=4))
        ##    tid = ['spotify:track:4hKWUzhQWsOkgT6LnDEASe', 'spotify:track:5UEnHoDYpsxlfzWLZIc7LD']
        if search_or_playlist == "playlist":
            tid = uris ## 'spotify:track:4TTV7EcfroSLWzXRY6gLv6'
        else:
            tid = uris[0]
        ##
        ##start = time.time()
        ####analysis = sp.audio_analysis(tid)
        masterdict = {}
        features = self.sp.audio_features(tid)
        for feature in features:
        ##    print(feature)
                featureuri = feature['uri']
                masterdict[featureuri] = feature
        df2 = pd.DataFrame.from_dict(masterdict, orient="index")
        ##    print(df2)
            ##df3 = tabulate(df2, headers='keys', tablefmt='psql'))
        if search_or_playlist == "playlist":
            df2.to_csv("/home/c/" "playlist " + playlistURI + ".csv", sep='\t', encoding='utf-8')
        else:
##        df2.to_csv("/home/c/Desktop/py/my_playlist_test45.csv", sep='\t', encoding='utf-8')
            df2.to_csv("/home/c/" + self.variable_a.get() + "/single_search.csv", sep='\t', encoding='utf-8')
        return(masterdict)
    
    def graph_button(self):
        robjects.r('''graph_audio_features <- function(title, search_or_playlist) {
  
# playlist <- read.csv("/home/c/Desktop/py/my_playlist_jan16_2018.csv", sep="\t")
# current_title <- paste0("/home/c/", title, ) 
# current_title <- read.table("/home/c/title.txt")
if (search_or_playlist == "playlist") { 
  current_title_filename <- paste0("/home/c/", title, "/playlist.csv") }
  else {
    current_title_filename <- paste0("/home/c/", title, "/single_search.csv")
  }
playlist <- read.csv(current_title_filename, sep="\t")

playlist <- playlist %>% select(speechiness, danceability, acousticness, energy, valence, liveness) ## tempo left out

playlist_melted <- tidyr::gather(playlist, key)
# playlist_melted <- playlist_melted %>% select(-tempo)
plot_features <- ggplot(playlist_melted, aes(x = key, y = value, fill=key)) +
  geom_bar(stat='identity', color="black") + coord_flip() + ggtitle(title)

##plot_features
save_plot_filename <- paste0("/home/c/", title,  "/single_audio_feature.png") 
ggsave(save_plot_filename, plot = plot_features, device='png')
plotone <- image_read(save_plot_filename)
plot_8x6 <- image_scale(plotone, "1000x935!")
image_write(plot_8x6, path = save_plot_filename, format = "png")
# file.copy(filename, song_dir)

}''')
        r_f = robjects.r['graph_audio_features']
        self.res = r_f(self.variable_a.get(), "search")
        self.find_images()
        self.adict[self.title] = self.find_images()
        self.update_optionz()
        self.variable_b.set('single_audio_feature')

    def compare_to_playlist_button(self):
            
##        top = self.top = tk.Toplevel(self)
        self.top = Toplevel(self)
##        self.top.pack()
        self.top.geometry("%dx%d%+d%+d" % (300, 200, 250, 125)) ## First two numbers represent dimensions of window. Third and fourth number say, where the window will appear. 
        self.myLabel = Label(self.top, text='Spotify Playlist URI:')
        self.myLabel.pack()
        self.myEntryBox = Entry(self.top)
        self.myEntryBox.pack()
        self.mySubmitButton = Button(self.top, text='Submit', command=self.compare_to_playlist_submit_button)
        self.mySubmitButton.pack()
##        self.myNoButton = tk.Button(self.top, text='No', command=self.start)
##        self.myNoButton.pack()

        
    def compare_to_playlist_submit_button(self):
        self.get_analytics_button(self.myEntryBox.get())
        robjects.r('''
            graph_audio_features_vs_playlist <- function(title) {
              
              
              # playlist <- read.csv("/home/c/Desktop/py/my_playlist_jan16_2018.csv", sep="\t")
              # current_title <- paste0("/home/c/", title, ) 
              # current_title <- read.table("/home/c/title.txt")
              # if (search_or_playlist == "playlist") { 
              # playlist_filename <- paste0("/home/c/", "playlist.csv") 
              
              current_title_filename <- paste0("/home/c/", title, "/single_search.csv")
              
              playlist <- read.csv("/home/c/playlist.csv", sep="\t")
              # playlist <- read.csv("/home/c/Desktop/py/my_playlist_jan16_2018.csv", sep="\t")
              current_title <- read.csv(current_title_filename, sep="\t")
              # current_title <- read.csv("/home/c/Desktop/py/my_playlist_test45.csv", sep="\t")
              
              playlist <- playlist %>% select(speechiness, danceability, acousticness, energy, valence, liveness) ## tempo left out
              current_title <- current_title %>% select(speechiness, danceability, acousticness, energy, valence, liveness) ## tempo left out
              
              playlist_melted <- tidyr::gather(playlist, key)
              current_title_melted <- tidyr::gather(current_title, key)
              
              boxplot1 <- ggplot(playlist_melted, aes(x=key, y=value, fill=key)) + geom_boxplot() + ggtitle('Playlist Box Plot') # + facet_wrap(~key)
              # boxplot1
              
              
              playlist_melted$id <- "playlist"
              
              title_with_caps <- str_to_title(title) ## capitalize it
              
              current_title_melted$id <- title_with_caps
              
              
              df <- bind_rows(playlist_melted, current_title_melted)
              
              df_pm <- df %>% filter(id == "playlist")
              # df_pm2 <- df_pm %>% group_by(key) %>% summarise(n())
              df_agg <- aggregate(df_pm$value, by=list(df_pm$key), FUN=mean)
              names(df_agg) <- c("key", "value")
              
              
              df_agg$id <- "Playlist"
              
              
              df <- bind_rows(df_agg, current_title_melted)
              
            ##  title_with_caps <- str_to_title(title) ## capitalize it
              
              # playlist_melted <- playlist_melted %>% select(-tempo)
              plot_feature_title <- paste0(title_with_caps, ' vs Playlist' )
              plot_features <- ggplot(df, aes(x = key, y = value, fill=key)) +
                geom_bar(stat='identity', color="black") + ggtitle(plot_feature_title) + facet_grid(~id) + coord_flip()
              
              
              # filenamespng <- c("/single_audio_feature.png", "/boxplot.png")
              # plotnames <- list(plot_features)
              
              # for (filenames in filenamespng) 
              #   
              #   for (plotname in plotnames) 
              save_plot_filename <- paste0("/home/c/", title,  "/song_vs_playlist.png")

              ggsave(save_plot_filename, plot = plot_features, device='png')
                  
              plotone <- image_read(save_plot_filename)
              plot_8x6 <- image_scale(plotone, "1000x935!")
              image_write(plot_8x6, path = save_plot_filename, format = "png")
              
              save_plot_filename <- paste0("/home/c/", title,  "/boxplot.png")
              
              ggsave(save_plot_filename, plot = boxplot1, device='png')
              
              plotone <- image_read(save_plot_filename)
              plot_8x6 <- image_scale(plotone, "1000x935!")
              image_write(plot_8x6, path = save_plot_filename, format = "png")
              # file.copy(filename, song_dir)
                
              
            }''')
        r_f = robjects.r['graph_audio_features_vs_playlist']
        self.res = r_f(self.variable_a.get())
##        self.find_images()
        self.adict[self.title] = self.find_images()
        self.update_optionz()
        self.variable_b.set('song_vs_playlist')
        self.top.destroy()
        
    def compare_playlists_button(self):
            
##        top = self.top = tk.Toplevel(self)
        self.top2 = Toplevel(self)
##        self.top.pack()
        self.top2.geometry("%dx%d%+d%+d" % (300, 200, 250, 125)) ## First two numbers represent dimensions of window. Third and fourth number say, where the window will appear. 
        self.myLabel = Label(self.top2, text='Spotify Playlist URIs:')
        self.myLabel.pack()
##        self.myLabel2 = Label(self.top2, text='Spotify Second Playlist URI:')
##        self.myLabel2.pack()
        self.myEntryBox = Entry(self.top2)
        self.myEntryBox.pack()
        self.myEntryBox2 = Entry(self.top2)
        self.myEntryBox2.pack()
        self.mySubmitButton2 = Button(self.top2, text='Submit', command=self.compare_playlists_submit_button)
        self.mySubmitButton2.pack()

    def compare_playlists_submit_button(self):
        playlist_lst = []
        playlist_lst.append(self.myEntryBox.get())
        playlist_lst.append(self.myEntryBox2.get())
        self.get_analytics_button(playlist_list=playlist_lst)
        robjects.r('''

 something <- function(...) {
          
          lst = list(...)
##          if (length(lst) > 2) {
##            stop("error: length of lst greater than 2")
##          }
          df <- ldply(lst, data.frame)
          names(df) <- "playlists"
##print(df$playlists[2])
##print(lst[[1]][[2]])
          
          # for (playlist in unique(df$X..i..)) {
##          playlist_filename <- paste0("/home/c/", "playlist ", df$playlists[1], ".csv")
playlist_filename <- paste0("/home/c/", "playlist ", lst[[1]][[1]], ".csv")
          playlist <- read.csv(playlist_filename, sep="\t")
          playlist_filename2 <- paste0("/home/c/", "playlist ", lst[[1]][[2]], ".csv")
##          playlist_filename2 <- paste0("/home/c/", "playlist ", df$playlists[2], ".csv")
          playlist2 <- read.csv(playlist_filename2, sep="\t")
          playlist <- playlist %>% select(speechiness, danceability, acousticness, energy, valence, liveness)
          playlist_melted <- tidyr::gather(playlist, key)
          playlist_melted$id <- lst[[1]][[1]]
          playlist2 <- playlist2 %>% select(speechiness, danceability, acousticness, energy, valence, liveness)
          playlist_melted2 <- tidyr::gather(playlist2, key)
          playlist_melted2$id <- lst[[1]][[2]]
          df2 <- bind_rows(playlist_melted, playlist_melted2)
          # plot_playlists <- ggplot(df2, aes(x = key, y = value, fill=key)) +
          #   geom_bar(stat='identity') + ggtitle("Playlist 1 vs Playlist 2") + facet_grid(~id) + coord_flip()
          # plot_playlists
          df2_agg <- aggregate(df2$value, by=list(df2$key, df2$id), FUN=mean)
          names(df2_agg) <- c("key","id","mean")
          plot_playlists_boxplots <- ggplot(df2, aes(x=key, y=value, fill=key)) + geom_boxplot() + ggtitle('Playlist 1 vs Playlist 2 Box Plots') + facet_grid(~id)
          plot_playlists_boxplots
          plot_playlist_means <- ggplot(df2_agg, aes(x = key, y = mean, fill=key)) +
            geom_bar(stat='identity', color='black') + ggtitle("Playlist 1 vs Playlist 2 Means") + facet_grid(~id) + coord_flip()
          plot_playlist_means
          
          if (dir.exists("/home/c/playlists") == FALSE) {
            dir.create("/home/c/playlists/")
          }
          
          save_plot_filename <- "/home/c/playlists/pl_vs_pl_means.png"
          ggsave(save_plot_filename, plot = plot_playlist_means, device='png')
          plotone <- image_read(save_plot_filename)
          plot_8x6 <- image_scale(plotone, "1000x935!")
          image_write(plot_8x6, path = save_plot_filename, format = "png")
          
          save_plot_filename <- "/home/c/playlists/pl_vs_pl_box.png"
          ggsave(save_plot_filename, plot = plot_playlists_boxplots, device='png')
          plotone <- image_read(save_plot_filename)
          plot_8x6 <- image_scale(plotone, "1000x935!")
          image_write(plot_8x6, path = save_plot_filename, format = "png")
        }

        ''')
##    r_func = robjects.r['something']
##    self.res = r_func(playlist_lst)    
##    self.adict['Playlists'] = self.find_images("playlists")
##    self.update_optionz()
##    self.variable_a.set('Playlists')
##    self.top2.destroy()
        robjects.r('''
            playlist_vs_playlist <- function(...) {
              
              
              lst = list(...)
##          if (length(lst) > 2) {
##            stop("error: length of lst greater than 2")
##          }
##          df <- ldply(lst, data.frame)
##          names(df) <- "playlists"
##print(df$playlists[2])
##print(lst[[1]][[2]])
          
          # for (playlist in unique(df$X..i..)) {
##          playlist_filename <- paste0("/home/c/", "playlist ", df$playlists[1], ".csv")
playlist_filename <- paste0("/home/c/", "playlist ", lst[[1]][[1]], ".csv")
          playlist <- read.csv(playlist_filename, sep="\t")
          playlist_filename2 <- paste0("/home/c/", "playlist ", lst[[1]][[2]], ".csv")
##          playlist_filename2 <- paste0("/home/c/", "playlist ", df$playlists[2], ".csv")
          playlist2 <- read.csv(playlist_filename2, sep="\t")
          playlist <- playlist %>% select(speechiness, danceability, acousticness, energy, valence, liveness)
          playlist_melted <- tidyr::gather(playlist, key)
          splitzz <- str_split(lst[[1]][[1]], ":")
  splitzz2 <- str_split(lst[[1]][[2]], ":")
  playlist_title <- splitzz[[1]][5]
  playlist_title_2 <- splitzz2[[1]][5]
          playlist_melted$id <- playlist_title
          playlist2 <- playlist2 %>% select(speechiness, danceability, acousticness, energy, valence, liveness)
          playlist_melted2 <- tidyr::gather(playlist2, key)
          playlist_melted2$id <- playlist_title_2
          df2 <- bind_rows(playlist_melted, playlist_melted2)
          # plot_playlists <- ggplot(df2, aes(x = key, y = value, fill=key)) +
          #   geom_bar(stat='identity') + ggtitle("Playlist 1 vs Playlist 2") + facet_grid(~id) + coord_flip()
          # plot_playlists
          df2_agg <- aggregate(df2$value, by=list(df2$key, df2$id), FUN=mean)
          names(df2_agg) <- c("key","id","mean")
          plot_playlists_boxplots <- ggplot(df2, aes(x=key, y=value, fill=key)) + geom_boxplot() + ggtitle('Playlist 1 vs Playlist 2 Box Plots') + facet_wrap(~id, nrow=2)
          plot_playlists_boxplots
          plot_playlist_means <- ggplot(df2_agg, aes(x = key, y = mean, fill=key)) +
            geom_bar(stat='identity', color='black') + ggtitle("Playlist 1 vs Playlist 2 Means") + facet_wrap(~id, nrow=2) #+ coord_flip()
          plot_playlist_means
          
          if (dir.exists("/home/c/playlists") == FALSE) {
            dir.create("/home/c/playlists/")
          }
          
          save_plot_filename <- "/home/c/playlists/pl_vs_pl_means.png"
          ggsave(save_plot_filename, plot = plot_playlist_means, device='png')
          plotone <- image_read(save_plot_filename)
          plot_8x6 <- image_scale(plotone, "1000x935!")
          image_write(plot_8x6, path = save_plot_filename, format = "png")
          
          save_plot_filename <- "/home/c/playlists/pl_vs_pl_box.png"
          ggsave(save_plot_filename, plot = plot_playlists_boxplots, device='png')
          plotone <- image_read(save_plot_filename)
          plot_8x6 <- image_scale(plotone, "1000x935!")
          image_write(plot_8x6, path = save_plot_filename, format = "png")
                
              
            }''')
        r_f = robjects.r['playlist_vs_playlist']
        self.res = r_f(playlist_lst)
        self.adict['playlists'] = self.find_images("playlists")
##        print('self.adict in playlist_vs_pl button submit', self.adict)
        self.variable_a.set('playlists')
        self.update_optionz()
        self.updater()
        self.variable_b.set('pl_vs_pl_box')
        
##        self.variable_b.set('pl_vs_pl_means')
        self.top2.destroy()
    
    def playlist_to_artist_and_song(self, results, search=False):

##        client_credentials_manager = SpotifyClientCredentials(client_id = '294447c8bb9848408019f91832fb9ed7', client_secret = '795e6b5c2b7b4d93a1318a7da74487cc')
##        sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)
##        
        artist_and_song = []
        if search:
            for i, t in enumerate(results['tracks']['items']):
                full_artist_and_song = t['name'] + ' ' + t['artists'][0]['name']
                artist_and_song.append(full_artist_and_song)
                
        else:
            for i, t in enumerate(results['tracks']['items']):
                full_artist_and_song = t['track']['name'] + ' ' + t['track']['artists'][0]['name']
        ##        artist_and_song.append(t['track']['name'], t['track']['artists'][0]['name'])
                artist_and_song.append(full_artist_and_song)
        ##        print('++++', t['track']['artists'][0]['name'])#[0]['name'])
    ##    print(artist_and_song)
        
        return(artist_and_song)   


    def playlist_to_sentiment_button(self):
        self.top3 = Toplevel(self)
##        self.top.pack()
        self.top3.geometry("%dx%d%+d%+d" % (300, 200, 250, 125)) ## First two numbers represent dimensions of window. Third and fourth number say, where the window will appear. 
        self.myLabel3 = Label(self.top3, text='Spotify Playlist URI:')
        self.myLabel3.pack()
        self.myEntryBox3 = Entry(self.top3)
        self.myEntryBox3.pack()
        self.mySubmitButton3 = Button(self.top3, text='Submit', command=self.playlist_to_sentiment_submit_button)
        self.mySubmitButton3.pack()

        MODES = [
            ("All", "all_selection"),
            ("Positive/Negative", "posneg_selection"),
            
        ]

        self.sentiment_selection = StringVar(self)
        self.sentiment_selection.set("posneg_selection") # initialize

##        increment_selection = 0
        for text, mode in MODES:
##            increment_selection = increment_selection + 60
            b = Radiobutton(self.top3, text=text,
                            variable=self.sentiment_selection, value=mode, command=self.quit)
            b.pack(anchor=W)
##            b.place(x=325+increment_selection,y=50)
        
    def playlist_to_sentiment_submit_button(self):
        self.entry_box_3 = self.myEntryBox3.get()
##        print('entry_box_3', entry_box_3)
        self.resultz = self.get_analytics_button(playlist_list = self.entry_box_3)
        list_of_artists_and_songs = self.playlist_to_artist_and_song(self.resultz)
        robjects.r('''
           screen3 <- function(alst, playlist) { 
  dfbad <- data.frame()
  for (song in alst) {
  
  search <- html_form(read_html("http://www.google.com"))[[1]]  
  session <- html_session("http://www.google.com")
  form <- set_values(search, q = paste0("genius ", song))
  result <- submit_form(session, form) 
  # test <- read_html(result)
  ff <- jump_to(session, result$url)
  s <- ff %>% follow_link("Genius Lyrics")
  
  # session <- html_session("http://www.duckduckgo.com")
  # form <- set_values(search, q = paste0("genius ", song))
  # result <- rvest::submit_form(session, form, submit="btnI", httr::config(followlocation = F)) 
  
  test <- read_html(s)
  test4 <- test %>% html_node("div.song_body-lyrics") %>% html_text()
  test5 <- gsub("\n", "", test4)
  test5 <- gsub("More on Genius", "", test5)
  # splitLyrics <- strsplit(test5, "Lyrics")
  title <- s$url
  dput(title, file="/home/c/url.txt")
  title <- str_match(title, "([^/]+$)")[,1]
  title <- gsub("-lyrics", "", title)
  title <- gsub("-", " ", title)
  song_dir <- paste0("/home/c/", title)  
  dir.create(file.path(paste0("/home/c/", title)))
  dput(title, file="/home/c/title.txt")
  write.table(title, file=paste0("/home/c/", title, ".txt"))
  setwd("/home/c/")
  
  
  # title <- splitLyrics[[1]][1]
  # title <- trimws(title)
  
  # test6 <- gsub("[Produced by [\\w\\s]+]", "", test5)
  # test7 <- tidy(test6)
  # test6 <- unnest_tokens(word, test6)
  
  # tt <- "There's a stranger in my bedThere's a pounding in my headGlitter all over the roomPink flamingos in the poolI smell like a minibarDJ's passed out in the yardBarbie's on the barbequeThis a hickie or a bruise?[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again[Verse 2]Trying to connect the dotsDon't know what to tell my bossThink the city towed my carChandelier is on the floorRipped my favorite party dressWarrant's out for my arrestThink I need a ginger aleThat was such an epic fail[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again(Do it all again) This Friday night[InterlT.G.I.F., T.G.I.F., T.G.I.FT.G.I.F., T.G.I.F., T.G.I.F[Instrumental Break][ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all again"
  splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
  # test77 <- data.frame(text=splits)
  names(splits) <- "test"
  test75 <- data.frame()
  test75 <- bind_rows(test75, splits)
  
  ngrams <- function(twothree) {
    data(stop_words)
    library(tidyr)
    library(ggraph)
    library(igraph)
    AFINN <- get_sentiments("afinn")
    test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
    if (twothree == 2) {
      test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
      other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
      test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
      test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
      # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2)
      bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      set.seed(1)
      a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
      plot2 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
      ##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
      ggsave("bigram.png", plot = plot2, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", 'bigram.png'))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
      file.copy('bigram.png', song_dir)
    }
    else {
      other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
      test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2", "word3"), sep=" ")
      test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
      test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort = T)
      # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3)
      bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      set.seed(2)
      a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
      plot3 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void() + ggtitle(title)
      ##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
      ggsave("trigram.png", plot = plot3, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", 'trigram.png'))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
      file.copy('trigram.png', song_dir)
    }
    # test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
    # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
    negation_words <- c("not","no","never", "cant", "can't", "cannot")
    negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
    # bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
    # set.seed(1)
    # a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
    # plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
    # plot2
    # ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()
    
    
  }
  
  sentiment_analysis <- function() { 
    test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)
    # test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75)
    nrc <- get_sentiments("nrc")
    mh <- c("headache", "headaches", "violent",  "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
    bad <- data.frame(word=mh)
    for (x in 1:length(bad$mh)) { 
      bad$sentiment <- "bad"
    }
    bad$word <- as.character(bad$word)
    names(test76) <- c("word", "n")
    # nrcNeg <- nrc %>% filter(sentiment == 'negative')
    badandAll <- bind_rows(bad, nrc)
    df_largest_items <- data.frame(items=0, filenames="test.png")
    for (sentimentz in unique(badandAll$sentiment)) {
      sentimentx <- badandAll %>% filter(sentiment == sentimentz)
      test77 <- test76 %>% inner_join(sentimentx) # %>% count(word, sort=T)
      if (nrow(test77) == 0) {
        next ## so no NA graphs
      }
      test77 <- test77[1:10,]
      test77 <- na.omit(test77)
      
      if(sentimentz == "bad") {
        test77$id <- title
        dfbad <<- bind_rows(test77, dfbad)
      }
      
      filename <- paste0(sentimentz, ".png")
      num_rows <- nrow(test77)
      
      df_largest <- data.frame(items=num_rows, filenames=filename)
      df_largest_items <- bind_rows(df_largest_items, df_largest)
      
      plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title)
      ##  ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
      ggsave(filename, plot = plot1, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", filename))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
      file.copy(filename, song_dir)
    }
    filename_most_graphed <- df_largest_items[which.max(df_largest_items$items),][,2]
    # title_no_extension <- gsub(".png", "", title)
    dput(filename_most_graphed, file=paste0("/home/c/", title, "/", "most_graphed.txt"))
    senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
    names(senti_score) <- c("word", "n", "score")
    ##names(senti_score) <- c("word", "n")
    
    senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n))
    senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
    ##senti_score <- trimws(sub("1", "", senti_score))
    senti_score <- as.numeric(senti_score) * 100
    senti_score <- as.character(senti_score)
    return(senti_score)
  }
  
  
  # ngrams(2)
  # ngrams(3)
  sentiment_analysis()
  }
  dfbad
  
  df_bad <- data.frame()
  for (songname in unique(dfbad$id)) {
    df_bad_filtered <- dfbad %>% filter(id == songname)
    df_bad_filtered$sum_total <- 1
    df_bad_filtered$sum_total[1] <- sum(df_bad_filtered$n)
    df_bad <- bind_rows(df_bad_filtered, df_bad)
    # dftest$sum <- sum(df_bad_filtered$y)
  }
  df_bad2 <- df_bad %>% select(id, sum_total) %>% filter(sum_total > 1)
  plot_df_bad_title <- paste0("Playlist ", playlist, " Bad Words")
  plot_df_bad <- ggplot(df_bad2, aes(x = id, y = sum_total, fill=id)) +
    geom_bar(stat='identity', color='black') + ggtitle(plot_df_bad_title) #+ facet_wrap(~id,nrow=2)
  filename <- paste0("bad_aggregated ", playlist, ".png")
  ggsave(filename, plot = plot_df_bad, path = "~", device='png')
  plotone <- image_read(paste0("/home/c/", filename))
  plot_8x6 <- image_scale(plotone, "1000x935!")
  image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
  file.copy(filename, "/home/c/playlists/")
}
''')

        robjects.r('''
          screen4 <- function(alst, playlist) { 
  dfbad <- data.frame()
  dftitles <- data.frame()
  
  for (song in alst) {
##  print(song)
  search <- html_form(read_html("http://www.google.com"))[[1]]  
  session <- html_session("http://www.google.com")
  form <- set_values(search, q = paste0("genius ", song))
  result <- submit_form(session, form) 
  # test <- read_html(result)
  ff <- jump_to(session, result$url)
##  s <- ff %>% follow_link("Genius Lyrics")
s <- try(follow_link(ff, "Genius Lyrics"))
  if (class(s) == "try-error") {
    next
  }
  
  # session <- html_session("http://www.duckduckgo.com")
  # form <- set_values(search, q = paste0("genius ", song))
  # result <- rvest::submit_form(session, form, submit="btnI", httr::config(followlocation = F)) 
  
  test <- read_html(s)
  test4 <- test %>% html_node("div.song_body-lyrics") %>% html_text()
  test5 <- gsub("\n", "", test4)
  test5 <- gsub("More on Genius", "", test5)
  # splitLyrics <- strsplit(test5, "Lyrics")
  title <- s$url
  dput(title, file="/home/c/url.txt")
  title <- str_match(title, "([^/]+$)")[,1]
  title <- gsub("-lyrics", "", title)
  title <- gsub("-", " ", title)
  dftest774 <- data.frame(titles=title)
  dftitles <- bind_rows(dftest774, dftitles)
  song_dir <- paste0("/home/c/", title)  
  dir.create(file.path(paste0("/home/c/", title)))
  dput(title, file="/home/c/title.txt")
  write.table(title, file=paste0("/home/c/", title, ".txt"))
  setwd("/home/c/")
  
  
  # title <- splitLyrics[[1]][1]
  # title <- trimws(title)
  
  # test6 <- gsub("[Produced by [\\w\\s]+]", "", test5)
  # test7 <- tidy(test6)
  # test6 <- unnest_tokens(word, test6)
  
  # tt <- "There's a stranger in my bedThere's a pounding in my headGlitter all over the roomPink flamingos in the poolI smell like a minibarDJ's passed out in the yardBarbie's on the barbequeThis a hickie or a bruise?[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again[Verse 2]Trying to connect the dotsDon't know what to tell my bossThink the city towed my carChandelier is on the floorRipped my favorite party dressWarrant's out for my arrestThink I need a ginger aleThat was such an epic fail[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again(Do it all again) This Friday night[InterlT.G.I.F., T.G.I.F., T.G.I.FT.G.I.F., T.G.I.F., T.G.I.F[Instrumental Break][ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all again"
  splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
  # test77 <- data.frame(text=splits)
  names(splits) <- "test"
  test75 <- data.frame()
  test75 <- bind_rows(test75, splits)
  
  ngrams <- function(twothree) {
    data(stop_words)
    library(tidyr)
    library(ggraph)
    library(igraph)
    AFINN <- get_sentiments("afinn")
    test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
    if (twothree == 2) {
      test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
      other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
      test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
      test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
      # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2)
      bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      set.seed(1)
      a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
      plot2 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
      ##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
      ggsave("bigram.png", plot = plot2, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", 'bigram.png'))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
      file.copy('bigram.png', song_dir)
    }
    else {
      other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
      test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2", "word3"), sep=" ")
      test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
      test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort = T)
      # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3)
      bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      set.seed(2)
      a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
      plot3 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void() + ggtitle(title)
      ##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
      ggsave("trigram.png", plot = plot3, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", 'trigram.png'))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
      file.copy('trigram.png', song_dir)
    }
    # test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
    # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
    negation_words <- c("not","no","never", "cant", "can't", "cannot")
    negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
    # bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
    # set.seed(1)
    # a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
    # plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
    # plot2
    # ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()
    
    
  }
  
  sentiment_analysis <- function() { 
    test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)
    # test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75)
    nrc <- get_sentiments("nrc")
    mh <- c("headache", "headaches", "violent",  "love", "baby", "suicide", "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
    # mhold <- c("she", "her", "woman", "women", "girl", "girls", "hers", "lover")
    bad <- data.frame(word=mh)
    for (x in 1:length(bad$mh)) { 
      bad$sentiment <- "bad"
    }
    bad$word <- as.character(bad$word)
    names(test76) <- c("word", "n")
    # nrcNeg <- nrc %>% filter(sentiment == 'negative')
    badandAll <- bind_rows(bad, nrc)
    df_largest_items <- data.frame(items=0, filenames="test.png")
    for (sentimentz in unique(badandAll$sentiment)) {
      sentimentx <- badandAll %>% filter(sentiment == sentimentz)
      test77 <- test76 %>% inner_join(sentimentx) # %>% count(word, sort=T)
      if (nrow(test77) == 0) {
        next ## so no NA graphs
      }
##      test77 <- test77[1:10,]
##      test77 <- na.omit(test77)
      
      if(sentimentz == "bad") {
        test77$id <- title
        dfbad <<- bind_rows(test77, dfbad)
      }
      
      filename <- paste0(sentimentz, ".png")
      num_rows <- nrow(test77)
      
      df_largest <- data.frame(items=num_rows, filenames=filename)
      df_largest_items <- bind_rows(df_largest_items, df_largest)
      
      plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title) + coord_flip()
      ##  ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
      ggsave(filename, plot = plot1, path = "~", device='png')
      plotone <- image_read(paste0("/home/c/", filename))
      plot_8x6 <- image_scale(plotone, "1000x935!")
      image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
      file.copy(filename, song_dir, overwrite=T)
    }
    filename_most_graphed <- df_largest_items[which.max(df_largest_items$items),][,2]
    # title_no_extension <- gsub(".png", "", title)
    dput(filename_most_graphed, file=paste0("/home/c/", title, "/", "most_graphed.txt"))
    senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
    names(senti_score) <- c("word", "n", "score")
    ##names(senti_score) <- c("word", "n")
    
    senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n))
    senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
    ##senti_score <- trimws(sub("1", "", senti_score))
    senti_score <- as.numeric(senti_score) * 100
    senti_score <- as.character(senti_score)
    return(senti_score)
  }
  
  
  # ngrams(2)
  # ngrams(3)
  sentiment_analysis()
  }
  dfbad
  df_bad_agg <- aggregate(dfbad$n, by=list(dfbad$id), FUN=sum)
  names(df_bad_agg) <- c("Song", "Sum")
  df1 <- data.frame(Song = unique(df_bad_agg$Song))
  names(dftitles) <- "Song"
  df_not_in_bad_agg <- anti_join(dftitles, df1)
  write.csv(df_not_in_bad_agg, "/home/c/playlists/not_in_bad_agg.csv")
  
##  df_bad <- data.frame()
##  for (songname in unique(dfbad$id)) {
##    df_bad_filtered <- dfbad %>% filter(id == songname)
##    df_bad_filtered$sum_total <- 1
##    df_bad_filtered$sum_total[1] <- sum(df_bad_filtered$n)
##    df_bad <- bind_rows(df_bad_filtered, df_bad)
##    # dftest$sum <- sum(df_bad_filtered$y)
##  }
##  df_bad2 <- df_bad %>% select(id, sum_total) %>% filter(sum_total > 1)
playlist_sub <- playlist
  playlist_sub <- str_split(playlist_sub, ":")
  playlist_sub <- playlist_sub[[1]][5]
  plot_df_bad_title <- paste0("Playlist ", playlist_sub, " Bad Words")
  plot_df_bad <- ggplot(df_bad_agg, aes(x = Song, y = Sum, fill=Song)) +
    geom_bar(stat='identity', color='black') + coord_flip() + labs(title=plot_df_bad_title) + guides(fill=FALSE) # + ggtitle(plot_df_bad_title)#+ facet_wrap(~id,nrow=2)
  filename <- paste0("bad_aggregated ", playlist, ".png")
  ggsave(filename, plot = plot_df_bad, path = "~", device='png')
  plotone <- image_read(paste0("/home/c/", filename))
  plot_8x6 <- image_scale(plotone, "1000x935!")
  image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
  file.copy(filename, "/home/c/playlists/", overwrite=T)
  session_identifier <- sample(1:999999, 1, replace=FALSE)
  df_titles_filename <- paste0("/home/c/playlists/", "titles:", session_identifier, ":", Sys.Date(), ".txt")
  write.table(dftitles, file=df_titles_filename)
  return(df_titles_filename)
}
''')
        robjects.r('''
screen4.5 <- function(alst, playlist) { 
  dfbad <- data.frame()
  dftitles <- data.frame()
  df_no_links <- data.frame()
  df_bad_agg_bool <- FALSE
  
  for (song in alst) {
    ##  print(song)
    search <- html_form(read_html("http://www.google.com"))[[1]]  
    session <- html_session("http://www.google.com")
    form <- set_values(search, q = paste0("genius ", song))
    result <- submit_form(session, form) 
    # test <- read_html(result)
    ff <- jump_to(session, result$url)
    ##  s <- ff %>% follow_link("Genius Lyrics")
    s <- try(follow_link(ff, "Genius Lyrics"))
    if (class(s) == "try-error") {
      df_try_error <- data.frame(Song=song)
      df_no_links <- bind_rows(df_try_error, df_no_links)
      next
    }
  
    # session <- html_session("http://www.duckduckgo.com")
    # form <- set_values(search, q = paste0("genius ", song))
    # result <- rvest::submit_form(session, form, submit="btnI", httr::config(followlocation = F)) 
    
    test <- read_html(s)
    test4 <- test %>% html_node("div.song_body-lyrics") %>% html_text()
    test5 <- gsub("\n", "", test4)
    test5 <- gsub("More on Genius", "", test5)
    # splitLyrics <- strsplit(test5, "Lyrics")
    title <- s$url
    dput(title, file="/home/c/url.txt")
    title <- str_match(title, "([^/]+$)")[,1]
    title <- gsub("-lyrics", "", title)
    title <- gsub("-", " ", title)
    dftest774 <- data.frame(titles=title)
    dftitles <- bind_rows(dftest774, dftitles)
    song_dir <- paste0("/home/c/", title)  
    dir.create(file.path(paste0("/home/c/", title)))
    dput(title, file="/home/c/title.txt")
    write.table(title, file=paste0("/home/c/", title, ".txt"))
    setwd("/home/c/")
    
    
    # title <- splitLyrics[[1]][1]
    # title <- trimws(title)
    
    # test6 <- gsub("[Produced by [\\w\\s]+]", "", test5)
    # test7 <- tidy(test6)
    # test6 <- unnest_tokens(word, test6)
    
    # tt <- "There's a stranger in my bedThere's a pounding in my headGlitter all over the roomPink flamingos in the poolI smell like a minibarDJ's passed out in the yardBarbie's on the barbequeThis a hickie or a bruise?[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again[Verse 2]Trying to connect the dotsDon't know what to tell my bossThink the city towed my carChandelier is on the floorRipped my favorite party dressWarrant's out for my arrestThink I need a ginger aleThat was such an epic fail[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again(Do it all again) This Friday night[InterlT.G.I.F., T.G.I.F., T.G.I.FT.G.I.F., T.G.I.F., T.G.I.F[Instrumental Break][ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all again"
    splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
    # test77 <- data.frame(text=splits)
    names(splits) <- "test"
    test75 <- data.frame()
    test75 <- bind_rows(test75, splits)
    
    ngrams <- function(twothree) {
      data(stop_words)
      library(tidyr)
      library(ggraph)
      library(igraph)
      AFINN <- get_sentiments("afinn")
      test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
      if (twothree == 2) {
        test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
        other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
        test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
        test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
        # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2)
        bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
        set.seed(1)
        a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
        plot2 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
        ##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
        ggsave("bigram.png", plot = plot2, path = "~", device='png')
        plotone <- image_read(paste0("/home/c/", 'bigram.png'))
        plot_8x6 <- image_scale(plotone, "1000x935!")
        image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
        file.copy('bigram.png', song_dir)
      }
      else {
        other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
        test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2", "word3"), sep=" ")
        test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
        test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort = T)
        # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3)
        bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
        set.seed(2)
        a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
        plot3 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void() + ggtitle(title)
        ##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
        ggsave("trigram.png", plot = plot3, path = "~", device='png')
        plotone <- image_read(paste0("/home/c/", 'trigram.png'))
        plot_8x6 <- image_scale(plotone, "1000x935!")
        image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
        file.copy('trigram.png', song_dir)
      }
      # test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
      # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
      negation_words <- c("not","no","never", "cant", "can't", "cannot")
      negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
      # bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      # set.seed(1)
      # a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
      # plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
      # plot2
      # ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()
      
      
    }
    
    sentiment_analysis <- function() { 
      test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)
      # test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75)
      nrc <- get_sentiments("nrc")
      mh <- c("headache", "headaches", "violent", "loco", "love", "", "suicide", "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
      # mhold <- c("she", "her", "woman", "women", "girl", "girls", "hers", "lover")
      bad <- data.frame(word=mh)
      for (x in 1:length(bad$mh)) { 
        bad$sentiment <- "bad"
      }
      bad$word <- as.character(bad$word)
      names(test76) <- c("word", "n")
      # nrcNeg <- nrc %>% filter(sentiment == 'negative')
      badandAll <- bind_rows(bad, nrc)
      df_largest_items <- data.frame(items=0, filenames="test.png")
      for (sentimentz in unique(badandAll$sentiment)) {
        sentimentx <- badandAll %>% filter(sentiment == sentimentz)
        test77 <- test76 %>% inner_join(sentimentx) # %>% count(word, sort=T)
        if (nrow(test77) == 0) {
          next ## so no NA graphs
        }
        ##      test77 <- test77[1:10,]
        ##      test77 <- na.omit(test77)
        
        if(sentimentz == "bad") {
          test77$id <- title
          dfbad <<- bind_rows(test77, dfbad)
        }
        
        filename <- paste0(sentimentz, ".png")
        num_rows <- nrow(test77)
        
        df_largest <- data.frame(items=num_rows, filenames=filename)
        df_largest_items <- bind_rows(df_largest_items, df_largest)
        
        plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title) + coord_flip()
        ##  ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
        ggsave(filename, plot = plot1, path = "~", device='png')
        plotone <- image_read(paste0("/home/c/", filename))
        plot_8x6 <- image_scale(plotone, "1000x935!")
        image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
        file.copy(filename, song_dir, overwrite=T)
      }
      filename_most_graphed <- df_largest_items[which.max(df_largest_items$items),][,2]
      # title_no_extension <- gsub(".png", "", title)
      dput(filename_most_graphed, file=paste0("/home/c/", title, "/", "most_graphed.txt"))
      senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
      names(senti_score) <- c("word", "n", "score")
      ##names(senti_score) <- c("word", "n")
      
      senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n))
      senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
      ##senti_score <- trimws(sub("1", "", senti_score))
      senti_score <- as.numeric(senti_score) * 100
      senti_score <- as.character(senti_score)
      return(senti_score)
    }
    
    
    # ngrams(2)
    # ngrams(3)
    sentiment_analysis()
  }
  do_nothing <- function() { 
    }
  # dfbad
  df_bad_agg <- try(aggregate(dfbad$n, by=list(dfbad$id), FUN=sum))
  if (class(df_bad_agg) == "try-error") {
    do_nothing()
    df_bad_agg_bool <- TRUE
    names(dftitles) <- "Song"
    write.csv(dftitles, "/home/c/playlists/not_in_bad_agg.csv")
    print('No bad agg.')
  } 
  if (df_bad_agg_bool == FALSE) { 
  names(df_bad_agg) <- c("Song", "Sum")
  df1 <- data.frame(Song = unique(df_bad_agg$Song))
  names(dftitles) <- "Song"
  df_not_in_bad_agg <- anti_join(dftitles, df1)
  write.csv(df_not_in_bad_agg, "/home/c/playlists/not_in_bad_agg.csv")
  
  
  
  
  ##  df_bad <- data.frame()
  ##  for (songname in unique(dfbad$id)) {
  ##    df_bad_filtered <- dfbad %>% filter(id == songname)
  ##    df_bad_filtered$sum_total <- 1
  ##    df_bad_filtered$sum_total[1] <- sum(df_bad_filtered$n)
  ##    df_bad <- bind_rows(df_bad_filtered, df_bad)
  ##    # dftest$sum <- sum(df_bad_filtered$y)
  ##  }
  ##  df_bad2 <- df_bad %>% select(id, sum_total) %>% filter(sum_total > 1)
  playlist_sub <- playlist
  playlist_sub <- str_split(playlist_sub, ":")
  playlist_sub <- playlist_sub[[1]][5]
  plot_df_bad_title <- paste0("Playlist ", playlist_sub, " Bad Words")
  plot_df_bad <- ggplot(df_bad_agg, aes(x = Song, y = Sum, fill=Song)) +
    geom_bar(stat='identity', color='black') + coord_flip() + labs(title=plot_df_bad_title) + guides(fill=FALSE) # + ggtitle(plot_df_bad_title)#+ facet_wrap(~id,nrow=2)
  filename <- paste0("bad_aggregated ", playlist, ".png")
  ggsave(filename, plot = plot_df_bad, path = "~", device='png')
  plotone <- image_read(paste0("/home/c/", filename))
  plot_8x6 <- image_scale(plotone, "1000x935!")
  image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
  file.copy(filename, "/home/c/playlists/", overwrite=T)
##  session_identifier <- sample(1:999999, 1, replace=FALSE)
##  df_titles_filename <- paste0("/home/c/playlists/", "titles:", session_identifier, ":", Sys.Date(), ".txt")
##  write.table(dftitles, file=df_titles_filename)
##  return(df_titles_filename)
  }
  session_identifier <- sample(1:999999, 1, replace=FALSE)
  df_titles_filename <- paste0("/home/c/playlists/", "titles:", session_identifier, ":", Sys.Date(), ".txt")
  write.table(dftitles, file=df_titles_filename)
  
##  df_no_links_filename <- paste0("/home/c/playlists/", "no_links:", session_identifier, ":", Sys.Date(), ".csv")
####  write.csv(df_no_links, "/home/c/playlists/df_no_links.csv")
##    write.csv(df_no_links, df_no_links_filename)
##
##  # print('Wrote df_no_links to /home/c/playlists/df_no_links.csv')
##if (exists('df_no_links') == TRUE) {
if (nrow(df_no_links) > 0) { 
  names(df_no_links) <- paste0(playlist, ":", Sys.Date())
  write.csv(df_no_links, "/home/c/playlists/df_no_links.csv")
  # write_no_links <- try(write.csv(df_no_links, "/home/c/playlists/df_no_links.csv"))
  # if (class(write_no_links) == "try-error") {
  #   do_nothing()
  # }
  }
  return(df_titles_filename)
}
''')
        robjects.r('''
screen4.75 <- function(alstofsongnames, playlist, sentiment_selections) { 
  dfbad <- data.frame()
  dftitles <- data.frame()
  df_no_links <- data.frame()
  df_bad_agg_bool <- FALSE
  
  for (song in alstofsongnames) {
    ##  print(song)
    search <- html_form(read_html("http://www.google.com"))[[1]]  
    session <- html_session("http://www.google.com")
    form <- set_values(search, q = paste0("genius ", song))
    result <- submit_form(session, form) 
    # test <- read_html(result)
    ff <- jump_to(session, result$url)
    ##  s <- ff %>% follow_link("Genius Lyrics")
    s <- try(follow_link(ff, "Genius Lyrics"))
    if (class(s) == "try-error") {
      df_try_error <- data.frame(Song=song)
      df_no_links <- bind_rows(df_try_error, df_no_links)
      next
    }
    
    # session <- html_session("http://www.duckduckgo.com")
    # form <- set_values(search, q = paste0("genius ", song))
    # result <- rvest::submit_form(session, form, submit="btnI", httr::config(followlocation = F)) 
    
    test <- read_html(s)
    test4 <- test %>% html_node("div.song_body-lyrics") %>% html_text()
    test5 <- gsub("\n", "", test4)
    test5 <- gsub("More on Genius", "", test5)
    # splitLyrics <- strsplit(test5, "Lyrics")
    title <- s$url
    dput(title, file="/home/c/url.txt")
    title <- str_match(title, "([^/]+$)")[,1]
    title <- gsub("-lyrics", "", title)
    title <- gsub("-", " ", title)
    dftest774 <- data.frame(titles=title)
    dftitles <- bind_rows(dftest774, dftitles)
    song_dir <- paste0("/home/c/", title)  
    dir.create(file.path(paste0("/home/c/", title)))
    dput(title, file="/home/c/title.txt")
    write.table(title, file=paste0("/home/c/", title, ".txt"))
    setwd("/home/c/")
    
    
    # title <- splitLyrics[[1]][1]
    # title <- trimws(title)
    
    # test6 <- gsub("[Produced by [\\w\\s]+]", "", test5)
    # test7 <- tidy(test6)
    # test6 <- unnest_tokens(word, test6)
    
    # tt <- "There's a stranger in my bedThere's a pounding in my headGlitter all over the roomPink flamingos in the poolI smell like a minibarDJ's passed out in the yardBarbie's on the barbequeThis a hickie or a bruise?[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again[Verse 2]Trying to connect the dotsDon't know what to tell my bossThink the city towed my carChandelier is on the floorRipped my favorite party dressWarrant's out for my arrestThink I need a ginger aleThat was such an epic fail[Pre-ChPictures of last night ended up onlineI'm screwed, oh wellIt's a blacked out blur, but I'm pretty sure it ruledDamn[ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all againThis Friday night, do it all again(Do it all again) This Friday night[InterlT.G.I.F., T.G.I.F., T.G.I.FT.G.I.F., T.G.I.F., T.G.I.F[Instrumental Break][ChLast Friday nightYeah, we danced on tabletopsAnd we took too many shotsThink we kissed, but I forgotLast Friday nightYeah, we maxed our credit cardsAnd got kicked out of the bar, so we hit the boulevardLast Friday nightWe went streaking in the parkSkinny dipping in the dark, then had a ménage à troisLast Friday nightYeah, I think we broke the lawAlways say we're going to stop-op, oh woah[Post-ChBut this Friday night, do it all again"
    splits <- strsplit(test5, "(?<=[[:lower:]])(?=[[:upper:][:digit:](])", perl=T)
    # test77 <- data.frame(text=splits)
    names(splits) <- "test"
    test75 <- data.frame()
    test75 <- bind_rows(test75, splits)
    
    ngrams <- function(twothree) {
      data(stop_words)
      library(tidyr)
      library(ggraph)
      library(igraph)
      AFINN <- get_sentiments("afinn")
      test_bigram <- test75 %>% unnest_tokens(bigram, test, token="ngrams", n=twothree) # %>% count(bigram, sort=T)
      if (twothree == 2) {
        test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2"), sep=" ")
        other <- c("lyrics", "lyric", "t.g.i.f", "interlude", "verse", "op", "instrumental")
        test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
        test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
        # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2)
        bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
        set.seed(1)
        a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
        plot2 <- ggraph(bigram_graph, layout="fr") + ggtitle(title) + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(aes(label=name), vjust=1.2, hust=2.5) + theme_void()
        ##ggsave("bigram.png", plot = plot2, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
        ggsave("bigram.png", plot = plot2, path = "~", device='png')
        plotone <- image_read(paste0("/home/c/", 'bigram.png'))
        plot_8x6 <- image_scale(plotone, "1000x935!")
        image_write(plot_8x6, path = paste0("/home/c/", 'bigram.png'), format = "png")
        file.copy('bigram.png', song_dir)
      }
      else {
        other <- c("lyrics", "lyric", "interlude", "verse", "op", "instrumental")
        test_bigram_seperate <- test_bigram %>% separate(bigram, c("word1", "word2", "word3"), sep=" ")
        test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% filter(!word3 %in% stop_words$word) %>% filter(!word1 %in% other) %>% filter(!word2 %in% other)
        test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3, sort = T)
        # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, word3)
        bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
        set.seed(2)
        a <- grid::arrow(type="closed", length=unit(.075,"inches")) 
        plot3 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=.5) + geom_node_text(vjust=1.45, hust=4, aes(label=name)) + theme_void() + ggtitle(title)
        ##  ggsave("trigram.png", plot = plot3, path = "~", device='png', width=2.9, height=1.8, units='cm', scale=5)
        ggsave("trigram.png", plot = plot3, path = "~", device='png')
        plotone <- image_read(paste0("/home/c/", 'trigram.png'))
        plot_8x6 <- image_scale(plotone, "1000x935!")
        image_write(plot_8x6, path = paste0("/home/c/", 'trigram.png'), format = "png")
        file.copy('trigram.png', song_dir)
      }
      # test_bigram_filtered <- test_bigram_seperate %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
      # test_bigram_counts <- test_bigram_filtered %>% count(word1, word2, sort=T)
      negation_words <- c("not","no","never", "cant", "can't", "cannot")
      negated <- test_bigram_seperate %>% filter(word1 %in% negation_words) %>% inner_join(AFINN, by=c(word2="word")) %>% count(word1, word2, score, sort=T) %>% ungroup()
      # bigram_graph <- test_bigram_counts %>% graph_from_data_frame()
      # set.seed(1)
      # a <- grid::arrow(type="closed", length=unit(.15,"inches")) 
      # plot2 <- ggraph(bigram_graph, layout="fr") + geom_edge_link(show.legend=F, arrow=a) + geom_node_point(color="blue", size=2) + geom_node_text(aes(label=name), vjust=2.5, hust=6) + theme_void()
      # plot2
      # ggraph(test_bigram_counts, layout="fr") + geom_edge_link(aes(edge_alpha=n), show.legend=F, arrow=arrow1) + geom_node_point(color="darkblue", size=5) + geom_node_text(aes(label=name), vjust=1, hust=1) + theme_void()
      
      
    }
    
    sentiment_analysis <- function() { 
      test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75, sort=T)
      # test76 <- test75 %>% unnest_tokens(test75, test) %>% count(test75)
      nrc <- get_sentiments("nrc")
      mh <- c("headache", "headaches", "violent", "loco", "love", "", "suicide", "jedi", "father", "dad", "star wars", "star war", "war", "luke", "vader", "darth vader", "higher power", "power",  "pressure", "alien", "extraterrestrial", "aliens", "doc", "voices", "hallucination", "king", "royal", "royalty", "crown", "doctor", "symptom", "symptoms", "insane", "asylum", "institution", "painful", "crazy", "brain", "mind", "cerebellum", "cortex", "head", "pounding",  "eye", "illuminati", "devil", "camron", "kam", "kameron", "cameron", "cam", "barry", "tass", "providence", "watch", "watcher", "watching", "simmons", "jay", "the one", "dreams", "dreaming", "dream", "psycho", "psychopath", "PTSD", "post traumatic stress syndrome", "schizophrenia", "schizo", "mental", "mental health", "pain", "bipolar", "bi-polar", "polar")
      # mhold <- c("she", "her", "woman", "women", "girl", "girls", "hers", "lover")
      bad <- data.frame(word=mh)
      # for (x in 1:length(bad$mh)) { 
      bad$sentiment <- "bad"
      # }
      bad$word <- as.character(bad$word)
      names(test76) <- c("word", "n")
      # nrcNeg <- nrc %>% filter(sentiment == 'negative')
      badandAll <- bind_rows(bad, nrc)
      df_largest_items <- data.frame(items=0, filenames="test.png")
      # for (sentimentz in unique(badandAll$sentiment)) { ## OLD
      for (sentimentz in sentiment_selections) { ## NEW
        sentimentx <- badandAll %>% filter(sentiment == sentimentz)
        test77 <- test76 %>% inner_join(sentimentx) # %>% count(word, sort=T)
        if (nrow(test77) == 0) {
          next ## so no NA graphs
        }
        ##      test77 <- test77[1:10,]
        ##      test77 <- na.omit(test77)
        
        if(sentimentz == "bad") {
          test77$id <- title
          dfbad <<- bind_rows(test77, dfbad)
        }
        
        filename <- paste0(sentimentz, ".png")
        num_rows <- nrow(test77)
        
        df_largest <- data.frame(items=num_rows, filenames=filename)
        df_largest_items <- bind_rows(df_largest_items, df_largest)
        
        plot1 <- ggplot(test77, aes(x=word, y=n, fill=word)) + geom_bar(stat='identity', color="black") + ggtitle(title) + coord_flip()
        ##  ggsave(filename, plot = plot1, path = "~", device='png', width=2.8, height=1.6, units='cm', scale=5)
        ggsave(filename, plot = plot1, path = "~", device='png')
        plotone <- image_read(paste0("/home/c/", filename))
        plot_8x6 <- image_scale(plotone, "1000x935!")
        image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
        file.copy(filename, song_dir, overwrite=T)
      }
      filename_most_graphed <- df_largest_items[which.max(df_largest_items$items),][,2]
      # title_no_extension <- gsub(".png", "", title)
      dput(filename_most_graphed, file=paste0("/home/c/", title, "/", "most_graphed.txt"))
      senti_score <- test76 %>% inner_join(get_sentiments("afinn"), by="word")
      names(senti_score) <- c("word", "n", "score")
      ##names(senti_score) <- c("word", "n")
      
      senti_score <- senti_score %>% summarise(score = sum(score * n) / sum(n))
      senti_score <- format(round(senti_score[[1]], 2), nsmall = 2)
      ##senti_score <- trimws(sub("1", "", senti_score))
      senti_score <- as.numeric(senti_score) * 100
      senti_score <- as.character(senti_score)
      return(senti_score)
    }
    
    
    ngrams(2)
##    ngrams(3)
    sentiment_analysis()
  }
  do_nothing <- function() { 
  }
  # dfbad
  df_bad_agg <- try(aggregate(dfbad$n, by=list(dfbad$id), FUN=sum))
  if (class(df_bad_agg) == "try-error") {
    do_nothing()
    df_bad_agg_bool <- TRUE
    names(dftitles) <- "Song"
    write.csv(dftitles, "/home/c/playlists/not_in_bad_agg.csv")
    print('No bad agg.')
  } 
  if (df_bad_agg_bool == FALSE) { 
    names(df_bad_agg) <- c("Song", "Sum")
    df1 <- data.frame(Song = unique(df_bad_agg$Song))
    names(dftitles) <- "Song"
    df_not_in_bad_agg <- anti_join(dftitles, df1)
    write.csv(df_not_in_bad_agg, "/home/c/playlists/not_in_bad_agg.csv")
    
    
    
    
    ##  df_bad <- data.frame()
    ##  for (songname in unique(dfbad$id)) {
    ##    df_bad_filtered <- dfbad %>% filter(id == songname)
    ##    df_bad_filtered$sum_total <- 1
    ##    df_bad_filtered$sum_total[1] <- sum(df_bad_filtered$n)
    ##    df_bad <- bind_rows(df_bad_filtered, df_bad)
    ##    # dftest$sum <- sum(df_bad_filtered$y)
    ##  }
    ##  df_bad2 <- df_bad %>% select(id, sum_total) %>% filter(sum_total > 1)
    playlist_sub <- playlist
    playlist_sub <- str_split(playlist_sub, ":")
    playlist_sub <- playlist_sub[[1]][5]
    plot_df_bad_title <- paste0("Playlist ", playlist_sub, " Bad Words")
    plot_df_bad <- ggplot(df_bad_agg, aes(x = Song, y = Sum, fill=Song)) +
      geom_bar(stat='identity', color='black') + coord_flip() + labs(title=plot_df_bad_title) + guides(fill=FALSE) # + ggtitle(plot_df_bad_title)#+ facet_wrap(~id,nrow=2)
    filename <- paste0("bad_aggregated ", playlist, ".png")
    ggsave(filename, plot = plot_df_bad, path = "~", device='png')
    plotone <- image_read(paste0("/home/c/", filename))
    plot_8x6 <- image_scale(plotone, "1000x935!")
    image_write(plot_8x6, path = paste0("/home/c/", filename), format = "png")
    file.copy(filename, "/home/c/playlists/", overwrite=T)
    ##  session_identifier <- sample(1:999999, 1, replace=FALSE)
    ##  df_titles_filename <- paste0("/home/c/playlists/", "titles:", session_identifier, ":", Sys.Date(), ".txt")
    ##  write.table(dftitles, file=df_titles_filename)
    ##  return(df_titles_filename)
  }
  session_identifier <- sample(1:999999, 1, replace=FALSE)
  df_titles_filename <- paste0("/home/c/playlists/", "titles:", session_identifier, ":", Sys.Date(), ".txt")
  write.table(dftitles, file=df_titles_filename)
  
  ##  df_no_links_filename <- paste0("/home/c/playlists/", "no_links:", session_identifier, ":", Sys.Date(), ".csv")
  ####  write.csv(df_no_links, "/home/c/playlists/df_no_links.csv")
  ##    write.csv(df_no_links, df_no_links_filename)
  ##
  ##  # print('Wrote df_no_links to /home/c/playlists/df_no_links.csv')
  ##if (exists('df_no_links') == TRUE) {
  if (nrow(df_no_links) > 0) { 
    names(df_no_links) <- paste0(playlist, ":", Sys.Date())
    write.csv(df_no_links, "/home/c/playlists/df_no_links.csv")
    # write_no_links <- try(write.csv(df_no_links, "/home/c/playlists/df_no_links.csv"))
    # if (class(write_no_links) == "try-error") {
    #   do_nothing()
    # }
  }
  return(df_titles_filename)
}
''')
        r_f = robjects.r['screen4.75']
        if self.sentiment_selection.get() == "all_selection":
            sentiment_list = ["bad" ,"trust","fear","negative","sadness","anger" ,"surprise","positive", "disgust","joy","anticipation"]
            self.res = r_f(alst=list_of_artists_and_songs, playlist=self.entry_box_3, sentiment_selections=sentiment_list)
        if self.sentiment_selection.get() == "posneg_selection":
            sentiment_list = ["bad", "positive", "negative"]
            self.res = r_f(alst=list_of_artists_and_songs, playlist=self.entry_box_3, sentiment_selections=sentiment_list)
##        if self.sentiment_selection.get() == "bitri_selection":
##            lst = ["bad", "positive", "negative"]
##            self.res = r_f(alst=list_of_artists_and_songs, playlist=self.entry_box_3, sentiment_selections=lst)
        
        self.adict['playlists'] = self.find_images("playlists")
##        print('self.adict in playlist_to_sentiment_submit_button', self.adict)
        self.variable_a.set('playlists')
        self.update_optionz()
        self.updater()
        self.top3.destroy()
        self.add("playlist")
        self.variable_a.set('playlists')
        playlist_key = self.adict['playlists']
        to_index = 'bad_aggregated ' + self.entry_box_3
        try:
            current_playlist_position = playlist_key.index(to_index)        
            self.variable_b.set(playlist_key[current_playlist_position])            
        except ValueError:
            print('Value Error.. changing to pl pl box')
            self.variable_b.set('pl_vs_pl_box')
        del self.adict['Song']
##        self.update_optionz()
        self.updater()
        self.not_in_bad_aggregated()
        try:
            self.variable_a.set('playlists')
            self.variable_b.set(playlist_key[current_playlist_position])
        except UnboundLocalError:
##            print(
            self.variable_a.set('playlists')
            self.variable_b.set('bad_aggregated spotify:user:cro0sh:playlist:1ceKRWiVuuFgiE1QbTb2ZS')
            
            
    def not_in_bad_aggregated(self):
        
        import re
        import csv
        not_in_bad_agg = []
        with open('/home/c/playlists/not_in_bad_agg.csv', newline='') as csvfile:
            spamreader = csv.reader(csvfile, quotechar='|')     
            for row in spamreader:
                 for string in row:
                     d = re.search('\\d', string)
                     if d == None:
                        not_in_bad_agg.append(string)
                        
        self.not_in_bad_agg_fixed  = []     
        for x in not_in_bad_agg:
##            fixed = x.replace('Song', '')
            fixed = x.replace('""', '')
            fixed = x.replace('"', '')
            self.not_in_bad_agg_fixed.append(fixed)
            
        self.not_in_bad_agg_fixed = self.not_in_bad_agg_fixed[2:]
            
        self.not_in_bad_agg_label.destroy()

        self.not_in_bad_agg_title = tk.Label(self, text='PLAYLIST - NOT IN BAD AGGREGATED', fg="#476581", font="bold")
        self.not_in_bad_agg_title.pack()
        self.not_in_bad_agg_title.place(x=1275, y = 495)

##        print('self.not_in_bad_agg_list ***', self.not_in_bad_agg_list)
##        for x in self.not_in_bad_agg_list:
##            print('type of self.not_in_bad_agg_list ---', type(x))
##            
        for song in self.not_in_bad_agg_list:
            song.pack_forget()
            song.destroy() # remove label from window
##        for song in self.not_in_bad_agg_list:
##            song.pack_forget()
##            song.destroy() # remove label from window
##        self.not_in_bad_agg_list = []           
        self.not_in_bad_agg_list = []
        
        not_in_bad_agg_init = 0
        for title in self.not_in_bad_agg_fixed:
##        for title in not_in_bad_agg:
            not_in_bad_agg_init += 35
            self.not_in_bad_agg_label = tk.Label(self, text=title, fg="#476581", font="bold")
            self.not_in_bad_agg_label.pack()
            self.not_in_bad_agg_label.place(x=1275, y = 500 + not_in_bad_agg_init)
            self.not_in_bad_agg_list.append(self.not_in_bad_agg_label)

        try:
            self.not_in_bad_agg_make_playlist.destroy()
            len_of_not_in_bad_agg = len(self.not_in_bad_agg_fixed) * 35
            if len_of_not_in_bad_agg > 0:
                self.not_in_bad_agg_make_playlist = tk.Button(self, height=1, width=10, bg="#476581",
                                                              fg="white", text="Make Playlist", font=("Helvetica", 11),
                                                              command=self.make_playlist_button)
      
                self.not_in_bad_agg_make_playlist.pack()
                self.not_in_bad_agg_make_playlist.place(x=1300,y=500+len_of_not_in_bad_agg+35)
        except AttributeError:      
        
            len_of_not_in_bad_agg = len(self.not_in_bad_agg_fixed) * 35
            if len_of_not_in_bad_agg > 0:
                self.not_in_bad_agg_make_playlist = tk.Button(self, height=1, width=10, bg="#476581",
                                                              fg="white", text="Make Playlist", font=("Helvetica", 11),
                                                              command=self.make_playlist_button)
      
                self.not_in_bad_agg_make_playlist.pack()
                self.not_in_bad_agg_make_playlist.place(x=1300,y=500+len_of_not_in_bad_agg+35)

##        self.no_links()
        self.no_links()
        
##        if len(no_linkz) == 0:
##            pass

        return(self.not_in_bad_agg_fixed)
    
    def no_links(self):
        self.no_links_title = tk.Label(self, text='NO LINKS', fg="#476581", font="bold")
        self.no_links_title.pack()
        self.no_links_title.place(x=1600, y = 495)

        import re
        import csv
        no_links = []
        session_id = []
        with open('/home/c/playlists/df_no_links.csv', newline='') as csvfile:
            spamreader = csv.reader(csvfile, quotechar='|')     
            for row in spamreader:
                 for string in row:
                     d = re.search('\\d', string)
                     if d == None:
                        no_links.append(string)
                     else:
                         session_id.append(string)
        ##print(session_id)                
        no_links_fixed  = []
        session_id_fixed = []
        for link, identifier in zip(no_links, session_id):
        ##            fixed = x.replace('Song', '')
            fixed = link.replace('""', '')
            fixed = link.replace('"', '')
            fixed_id = identifier.replace('""', '')
            fixed_id = identifier.replace('"', '')
            no_links_fixed.append(fixed)
            session_id_fixed.append(fixed_id)
##        print(session_id_fixed[0])
            
        no_links_fixed = no_links_fixed[1:]
        
        playlist = session_id_fixed[0].split(":")
        playlist = playlist[4]

        playlist_input = self.entry_box_3
        playlist_input = playlist_input.split(":")
        playlist_input = playlist_input[4]
        print('no_links_fixed 777 ', no_links_fixed)
        print('self.no_links_list ___ ', self.no_links_list)
##        for x in self.no_links_list:
##            print('type of self.no_links_list 888', type(x))
        no_links_test = StringVar()
        for song in self.no_links_list:
            song.pack_forget()
            song.destroy() # remove label from window
##        for song in self.no_links_list:
##            self.no_links_label.pack_forget()
##            self.no_links_label.destroy()
        self.no_links_list = []
        no_links_test.set('')
        print('playlist', playlist)
        print('playlist_input', playlist_input)
        if playlist == playlist_input:
##        print(playlist)       

            no_links_increment = 0
            for title in no_links_fixed:
        ##        for title in not_in_bad_agg:
                no_links_increment += 35
                no_links_test.set(title)
                self.no_links_label = tk.Label(self, text='', fg="#476581", font="bold")
                self.no_links_label.pack()
                self.no_links_label.config(text=no_links_test.get())
                self.no_links_label.place(x=1600, y = 500 + no_links_increment)
                self.no_links_list.append(self.no_links_label)
##        else:
##            self.no_links_title2 = tk.Label(self, text='None.', fg="#476581", font="bold")
##            self.no_links_title2.pack()
##            self.no_links_title2.place(x=1600, y = 535)
##            self.no_links_list.append('None.')
##            self.no_links_list.append(title)
##        print('no_links_fixed BEFORE SLICE ++++', no_links_fixed)
        
##        print('no_links_fixed AFTER SLICE ---', no_links_fixed)
        return(no_links_fixed)
    
    def remove_all_button(self):
##        empty_dict = {}
##        self.adict_copy = self.adict
##        self.adict = empty_dict
        self.variable_a.set('playlists')
        self.remove(remove_all=True)
        
##        self.variable_b.set('')
##        self.updater()
##        self.update_optionz()
##        self.

    def make_playlist_button(self, not_in_bad_agg=True):

        

        def search(search_str, lst=False):
            client_credentials_manager = SpotifyClientCredentials(client_id = 'replace_me', client_secret = 'replace_me')
            sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

##            search_str = 'milky way'
            if lst:
                uri_results = []
                for song in search_str:
##                    results = sp.search(search_str, limit = 10)
                    results = sp.search(song, limit = 1)   

                    for i, t in enumerate(results['tracks']['items']):
                        uri_results.append(t['uri'])

                return(uri_results)
            
            else:
                results = sp.search(search_str, limit = 1) 
            ##pprint.pprint(results)

                uri_results = []

                for i, t in enumerate(results['tracks']['items']):
                        uri_results.append(t['uri'])
                        
                return(uri_results)
            
        import os
##        os.system('cd ~/Desktop/py')
##        os.system('python3 spotipy_create_playlist.py cro0sh makepl screener0.8_5')
##        from subprocess import PIPE, run
##
##        def out(command):

        


##user_playlist_create(self, user, name, public=True, description='')
        def make_playlist(playlist_name, playlist_description='', scope_private=False):
            client_id = 'replace_me' ## NEW
            client_secret = 'replace_me' ## NEW
            redirect_uri = 'http://localhost:8888/callback'
            username= 'replace_me'
            ##scope = 'playlist-modify-private'
            if not scope_private:
                scope = 'playlist-modify-public'
            else:
                scope = 'playlist-modify-private'
            ##token = util.prompt_for_user_token(username)
            token = util.prompt_for_user_token(username, scope, client_id=client_id, client_secret = client_secret, redirect_uri = redirect_uri)
            ##client_credentials_manager = SpotifyClientCredentials(client_id = '294447c8bb9848408019f91832fb9ed7', client_secret = '795e6b5c2b7b4d93a1318a7da74487cc')
            ##token = 'BQAIF7k8qYyrI17bHeJW9iROKqqEGlBIX9ODf1HzSzxhyU1KxK3wgqoJiZlSzX7x0ZJ0ZfXqBIJmCiEDrqn1eUCO8F66ZYQ9WH7pcgBbuKG4pukEJ6Hn8IxPP_effqWvjC6Vo-BOJZdUoK9HVRmURjeqp0HwqVF499whmXH2ZEvL'

            if token:
                
                sp = spotipy.Spotify(auth=token)
                sp.trace = True
                if not scope_private:
                    playlists = sp.user_playlist_create(user=username, name=playlist_name,
                                                    description=playlist_description)
                else:
                    playlists = sp.user_playlist_create(user=username, name=playlist_name,
                                                    description=playlist_description, public=False)
                pprint.pprint(playlists)
                playlist_uri = playlists['uri']
                filename = '/home/c/playlists/playlist_uri.txt'
                with open(filename, 'w') as file:
                    file.write(playlist_uri)
                    file.close()
                    print('written to ', filename)   
                
            else:
                print("Can't get token for", username)
                
##        make_playlist('t', scope_private=True)
            
##        self.output("cd ~/Desktop/py")
##        my_output = self.output("python3 spotipy_create_playlist.py cro0sh makepl screener0.8_5")
##        print(my_output)
##        print('Ran1')
        
####        self.checks()
####        print('executed')
##        with open('/home/c/playlists/playlist_uri.txt', newline='') as txtfile:
##            playlist_uri = txtfile.read()
##        for title in self.not_in_bad_agg_fixed:
##        print('playlist_uri', playlist_uri)
##        print('=self.not_in_bad_agg_fixed', self.not_in_bad_agg_fixed)
        
        track_uids = search(search_str=self.not_in_bad_agg_fixed, lst=True)

        def add_tracks(list_of_track_uids, scope_private=False):
            client_id = 'replace_me' ## NEW
            client_secret = 'replace_me' ## NEW
            redirect_uri = 'http://localhost:8888/callback'

            username = 'replace_me'
            if not scope_private:
                scope = 'playlist-modify-public'
            else:
                scope = 'playlist-modify-private'
                
            ##token = util.prompt_for_user_token(username, scope)
            token = util.prompt_for_user_token(username, scope, client_id=client_id, client_secret = client_secret, redirect_uri = redirect_uri)

            ##playlist_id = 'spotify:user:cro0sh:playlist:3w5Pn7t3rXE0qquXbRyfRE'
            ##track_ids = 'spotify:track:1301WleyT98MSxVHPZCA6M'
##            token = 'BQAIF7k8qYyrI17bHeJW9iROKqqEGlBIX9ODf1HzSzxhyU1KxK3wgqoJiZlSzX7x0ZJ0ZfXqBIJmCiEDrqn1eUCO8F66ZYQ9WH7pcgBbuKG4pukEJ6Hn8IxPP_effqWvjC6Vo-BOJZdUoK9HVRmURjeqp0HwqVF499whmXH2ZEvL'

            ##print('token,', token)

            if token:
                sp = spotipy.Spotify(auth=token)
                sp.trace = True
                with open('/home/c/playlists/playlist_uri.txt', newline='') as txtfile:
                    playlist_uri = txtfile.read()
                results = sp.user_playlist_add_tracks(user=username, playlist_id=playlist_uri, tracks=list_of_track_uids)
                print(results)  
                
        
        def pl_and_add_tracks_function_calls():
            self.make_playlist(self.myEntryBox3.get(), scope_private=True)
            self.add_tracks(list_of_track_uids=track_uids, scope_private=True)

        self.top3 = Toplevel(self)
##        self.top.pack()
        self.top3.geometry("%dx%d%+d%+d" % (300, 200, 250, 125)) ## First two numbers represent dimensions of window. Third and fourth number say, where the window will appear. 
        self.myLabel3 = Label(self.top3, text='Playlist Name:')
        self.myLabel3.pack()
        self.myEntryBox3 = Entry(self.top3)
        self.myEntryBox3.pack()
        self.mySubmitButton3 = Button(self.top3, text='Submit', command=pl_and_add_tracks_function_calls)
        self.mySubmitButton3.pack()
##        self.mySubmitButton4 = Button(self.top3, text='Add Tracks', command=add_tracks)
##        self.mySubmitButton4.pack()
    def make_playlist(self, playlist_name, playlist_description='', scope_private=False):
            client_id = 'replace_me' ## NEW
            client_secret = 'replace_me' ## NEW
            redirect_uri = 'http://localhost:8888/callback'
            username= 'replace_me'
            ##scope = 'playlist-modify-private'
            if not scope_private:
                scope = 'playlist-modify-public'
            else:
                scope = 'playlist-modify-private'
            ##token = util.prompt_for_user_token(username)
            token = util.prompt_for_user_token(username, scope, client_id=client_id, client_secret = client_secret, redirect_uri = redirect_uri)
            ##client_credentials_manager = SpotifyClientCredentials(client_id = '294447c8bb9848408019f91832fb9ed7', client_secret = '795e6b5c2b7b4d93a1318a7da74487cc')
            ##token = 'BQAIF7k8qYyrI17bHeJW9iROKqqEGlBIX9ODf1HzSzxhyU1KxK3wgqoJiZlSzX7x0ZJ0ZfXqBIJmCiEDrqn1eUCO8F66ZYQ9WH7pcgBbuKG4pukEJ6Hn8IxPP_effqWvjC6Vo-BOJZdUoK9HVRmURjeqp0HwqVF499whmXH2ZEvL'

            if token:
                
                sp = spotipy.Spotify(auth=token)
                sp.trace = True
                if not scope_private:
                    playlists = sp.user_playlist_create(user=username, name=playlist_name,
                                                    description=playlist_description)
                else:
                    playlists = sp.user_playlist_create(user=username, name=playlist_name,
                                                    description=playlist_description, public=False)
                pprint.pprint(playlists)
                playlist_uri = playlists['uri']
                filename = '/home/c/playlists/playlist_uri.txt'
                with open(filename, 'w') as file:
                    file.write(playlist_uri)
                    file.close()
                    print('written to ', filename)   
                
            else:
                print("Can't get token for", username)
                
    def add_tracks(self, list_of_track_uids, scope_private=False):
            client_id = 'replace_me' ## NEW
            client_secret = 'replace_me' ## NEW
            redirect_uri = 'http://localhost:8888/callback'

            username = 'replace_me'
            if not scope_private:
                scope = 'playlist-modify-public'
            else:
                scope = 'playlist-modify-private'
                
            ##token = util.prompt_for_user_token(username, scope)
            token = util.prompt_for_user_token(username, scope, client_id=client_id, client_secret = client_secret, redirect_uri = redirect_uri)

            ##playlist_id = 'spotify:user:cro0sh:playlist:3w5Pn7t3rXE0qquXbRyfRE'
            ##track_ids = 'spotify:track:1301WleyT98MSxVHPZCA6M'
##            token = 'BQAIF7k8qYyrI17bHeJW9iROKqqEGlBIX9ODf1HzSzxhyU1KxK3wgqoJiZlSzX7x0ZJ0ZfXqBIJmCiEDrqn1eUCO8F66ZYQ9WH7pcgBbuKG4pukEJ6Hn8IxPP_effqWvjC6Vo-BOJZdUoK9HVRmURjeqp0HwqVF499whmXH2ZEvL'

            ##print('token,', token)

            if token:
                sp = spotipy.Spotify(auth=token)
                sp.trace = True
                with open('/home/c/playlists/playlist_uri.txt', newline='') as txtfile:
                    playlist_uri = txtfile.read()
                
                if len(list_of_track_uids) > 100:
                    for end in range(0,len(list_of_track_uids),100):
                        if end == 0:
                            next
                        else:
                            start = end-100
                            last_items = len(list_of_track_uids) - end
                            x = end
##                            list_of_track_uids = list_of_track_uids[start:end]
                            results = sp.user_playlist_add_tracks(user=username, playlist_id=playlist_uri, tracks=list_of_track_uids[start:end])
                            print('add_tracks RESULTS ***', results)
                            
                    last_items = len(list_of_track_uids) - x
                    last = sp.user_playlist_add_tracks(user=username, playlist_id=playlist_uri, tracks=list_of_track_uids[x:x+last_items])                            
                    print('LAST RESULT +++', last)

    def top_tracks(self):
        
        def get_playlist_results(playlist):
##            with open('/home/c/playlists/playlist_uri.txt', newline='') as txtfile:
##                playlist_uri = txtfile.read()
            client_credentials_manager = SpotifyClientCredentials(client_id = 'replace_me', client_secret = 'replace_me')
            sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)

            username = playlist.split(':')[2]
            playlist_id = playlist.split(':')[4]        
            results = sp.user_playlist(username, playlist_id)
            return(results)
        
        def get_artist_uids(playlist):
            my_pl_results = get_playlist_results(playlist)
            gen = self.dict_generator(my_pl_results)
            uris = []
            for x in gen:
                for y in x:
                        try:
                            if "spotify:artist" in y:       
                                uris.append(x[5])
                        except TypeError:
                            next                                
            for z in uris:
                if z == 'uri':
                    uris.remove(z)

            return(uris)

        
        
            
        def top(artist_uid):
            client_id = 'replace_me' ## NEW
            client_secret = 'replace_me' ## NEW
            redirect_uri = 'http://localhost:8888/callback'
            username= 'cro0sh'
            ##scope = 'playlist-modify-private'
##            scope = 'playlist-modify-public'
            ##token = util.prompt_for_user_token(username)
            token = util.prompt_for_user_token(username, client_id=client_id, client_secret = client_secret, redirect_uri = redirect_uri)
            ##client_credentials_manager = SpotifyClientCredentials(client_id = '294447c8bb9848408019f91832fb9ed7', client_secret = '795e6b5c2b7b4d93a1318a7da74487cc')
            ##token = 'BQAIF7k8qYyrI17bHeJW9iROKqqEGlBIX9ODf1HzSzxhyU1KxK3wgqoJiZlSzX7x0ZJ0ZfXqBIJmCiEDrqn1eUCO8F66ZYQ9WH7pcgBbuKG4pukEJ6Hn8IxPP_effqWvjC6Vo-BOJZdUoK9HVRmURjeqp0HwqVF499whmXH2ZEvL'

            if token:
                
                sp = spotipy.Spotify(auth=token)
                sp.trace = False
                topz = []
##                fixed_artist_uids = []
                list_of_artist_uids = []
                for artist_id in artist_uid:
                    if artist_id != 'uri':
                       list_of_artist_uids.append(artist_id) 
##                    if artist_id == 'uri':
##                        artist_uid.remove(artist_id)
                        
                print('ARTIST_UID IN TOP() ', list_of_artist_uids)
                for top_track in list_of_artist_uids:
                    results = sp.artist_top_tracks(top_track)
                
                    for i, t in enumerate(results['tracks']):
                        topz.append(t['uri'])
                    
        ##        pprint.pprint(top)
                
        ##        playlist_uri = playlists['uri']
        ##        filename = '/home/c/playlists/playlist_uri.txt'
        ##        with open(filename, 'w') as file:
        ##            file.write(playlist_uri)
        ##            file.close()
        ##            print('written to ', filename)
            ##    print('TOKEN +++', token)
                return(topz)
            else:
                print("Can't get token for", username)
                
##        top(artist_uid=artist_ids)
        
        def create_and_add_top_tracks():
            self.make_playlist(self.playlist_name_entry.get(), scope_private=True)
##            test99 = get_playlist_results()
##            test44 = get_artist_uids()
##            print('test99 *** ', test99)
            
            artist_ids = get_artist_uids(playlist=self.playlist_id_entry2.get())
##            print('artist_ids +++ ', artist_ids)
##            top2 = top(artist_uid=artist_ids)
##            print('RESULTS OF TOP', top2)
            self.add_tracks(list_of_track_uids=top(artist_uid=artist_ids), scope_private=True)
            self.top3.destroy()
            
        self.top3 = Toplevel(self)
##        self.top.pack()
        self.top3.geometry("%dx%d%+d%+d" % (300, 200, 250, 125)) ## First two numbers represent dimensions of window. Third and fourth number say, where the window will appear. 
        self.myLabel3 = Label(self.top3, text='New Playlist Name:')
        self.myLabel3.pack()
        self.playlist_name_entry = Entry(self.top3)
        self.playlist_name_entry.pack()        
        self.myLabel4 = Label(self.top3, text='Playlist URI:')
        self.myLabel4.pack()
        self.playlist_id_entry2 = Entry(self.top3)
        self.playlist_id_entry2.pack()
        self.mySubmitButton3 = Button(self.top3, text='Submit', command=create_and_add_top_tracks)
        self.mySubmitButton3.pack()
##        self.mySubmitButton4 = Button(self.top3, text='Submit', command=create_and_add_top_tracks)
##        self.mySubmitButton4.pack()
                
##        for uid in track_uids:
##            system_string = "python3 " + "ttt.py cro0sh " + playlist_uri + " " + uid
##            print('system string', system_string)
####            os.system("python ttt.py cro0sh spotify:user:cro0sh:playlist:3w5Pn7t3rXE0qquXbRyfRE spotify:track:51N3PusY92uUZr75v9JrCo")
##            os.system(system_string)

##    def output(self, command):
##        from subprocess import PIPE, run
##        result = run(command, stdout=PIPE, stderr=PIPE, universal_newlines=True, shell=True, check=True)
##        return(result.stdout)
##        print(result.stdout)
##        print('RAN 2')
        
class PageOne(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        self.controller = controller
        label = tk.Label(self, text="This is page 1", font=controller.title_font)
        label.pack(side="top", fill="x", pady=10)
        button = tk.Button(self, text="Go to the start page",
                           command=lambda: controller.show_frame("StartPage"))
        button.pack()


class PageTwo(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        self.controller = controller
        label = tk.Label(self, text="This is page 2", font=controller.title_font)
        label.pack(side="top", fill="x", pady=10)
        button = tk.Button(self, text="Go to the start page",
                           command=lambda: controller.show_frame("StartPage"))
        button.pack()


class PageThree(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        self.controller = controller
        label = tk.Label(self, text="This is page 3", font=controller.title_font)
        label.pack(side="top", fill="x", pady=10)
        button = tk.Button(self, text="Go to the start page",
                           command=lambda: controller.show_frame("StartPage"))
        button.pack()

class PageFour(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        self.controller = controller
        label = tk.Label(self, text="This is page 4", font=controller.title_font)
        label.pack(side="top", fill="x", pady=10)
##        button = tk.Button(self, text="Go to the start page",
##                           command=lambda: controller.show_frame("StartPage"))
##        button.pack()
        self.canvas4 = Canvas(self, relief = FLAT, bg="#476581",
                                            width = 180, height = 1920)
        self.canvas4.pack(side = TOP, anchor = NW, padx = 10, pady = 10)
        self.canvas4.place(x=20, y=0)
        
        textBox = Text(self, height=15, width=60)
        textBox.pack()
        textBox.insert(END, "Copy paste youtube URL(s) here")
        textBox.place(x=250, y=20)

        self.buttons()


        


##        
    def buttons(self):
        increment = 0
        textz = ["Screener", "Local Files", "Playlists", "Converter", "Info"]
        pages = ["StartPage", "PageOne", "PageTwo", "PageThree", "PageFour"]
        for btntext, page in zip(textz, pages):
            increment = increment + 35
            button1 = Button(self, text = btntext, command = lambda page=page: self.controller.show_frame(page), anchor = W)
            button1.configure(width = 10, activebackground = "#33B5E5", relief = FLAT, bg='#476581')
            button1_window = self.canvas4.create_window(35, 10 + increment, anchor=NW, window=button1)
            button1.config(highlightthickness=0, fg='white')
        
class PageFive(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        self.controller = controller
        label = tk.Label(self, text="This is page 5", font=controller.title_font)
        label.pack(side="top", fill="x", pady=10)
        button = tk.Button(self, text="Go to the start page",
                           command=lambda: controller.show_frame("StartPage"))
        button.pack()



if __name__ == "__main__":
    app = SampleApp()
    app.geometry('1920x1080+10+50')
    app.title('Screener v0.8')
    app.mainloop()


    
