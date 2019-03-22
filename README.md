Hi,

This is the readme for the song screener v0.8. It was developed on linux mint (based on ubuntu). It is a desktop/native application that analyzes text data. It provides machine learning data to the user, graphs that were based on the emotional aspect of a song (~6-8 "emotions", ie. fearful). It has a search feature as well as inputting a spotify playlist URI (which makes use of the spotify API) to grab the data/information about a song. It also has a "make top 10 playlist" from a playlist, which returns/uses the spotify API to make a large playlist of all artists top 10 songs.

How to use:

Download all required packages. This means googling or using pip or install.packages("tidytext") as an example. 


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

