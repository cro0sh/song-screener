Hi,

This is the readme for the song screener v0.8. It was developed on linux mint (based on ubuntu). It is a desktop/native application that analyzes text data. It provides machine learning data to the user, graphs that were based on the emotional aspect of a song (~6-8 "emotions", ie. fearful). It has a search feature as well as inputting a spotify playlist URI (which makes use of the spotify API) to grab the data/information about a song. It also has a "make top 10 playlist" from a playlist, which returns/uses the spotify API to make a large playlist of all artists top 10 songs.

How to use:

1. Download all required packages. This means googling or using pip or install.packages("tidytext") as an example.
2. Search and replace all client_id and client_secret variables in Songscreenerv0.8.py with your spotify API credientials
3. Thank you to all the packages and developers that made this app possible. 
4. Important tip to get working on linux: 
	sudo apt-get install liblapack-dev
	sudo apt-get install libblas-dev
5. The directories need to be changed. It will ask you for a few files before it runs -> my advice is to make the files and keep them blank for the time being.
6. Search and replace "replace_me" with your api credentials throughout the app. There probably should be 1 function that handles this instead of repeating it throughout.
