## Collecting Social Media data: YouTube

# Required Libraries
# Install if necessary
install.packages("tuber")
install.packages("stringi")
install.packages("gridExtra")
install.packages("tm")
install.packages("wordcloud")


# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("httr")

library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
library(wordcloud)
library(gridExtra)
library(httr)
library(tm)


### Step 1: Apply for the Google YouTube API


### 1.  Go to Google Cloud Console (https://cloud.google.com/).
### 2.  Create a new project or select an existing one.
### 3.  Search for and enable the YouTube Data API v3.
### 4.  Go to Credentials > Create Credentials > OAuth Client ID.
### 5.  Set up the OAuth consent screen
### 6.  Name the App (e.g. YouTube analyzer), enter support email
### 7.  Application type: web application
### 8.  Name: Web client 1
### 9.  Generate Client ID and Client Secret.


### Step 2: Authenticate with YouTube API

#### Use your Client ID and Client Secret to authenticate.

# Replace with your actual Client ID and Client Secret
yt_oauth("272637038445-rdd5o4g83haa7er4rc867f15ji9dnenf.apps.googleusercontent.com",
         "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", token = "")

### Important:  when running for first time, you will be prompted to:
### 1. add the .httr-oauth to .gitignore, select 1 to consent. 
### 2. Then it will open browser to choose your Google account. 
### 3. When prompted with safety statement ("Google hasn’t verified this app"), click advanced and click Go to Appname (unsafe) to verify.  
### 4. When done, the message will show "Authentication complete. Please close this page and return to R."
### 5. Return to RStudio.  When seeing:
###  
### "Waiting for authentication in browser...
### Press Esc/Ctrl + C to abort
### Authentication complete.
### 
### It is ready to collect YouTube data

### Step 3: Troubleshooting

### When receiving Error 400: redirect_uri_mismatch
### Go to OAuth Consent Screen
### Look for "Authorized redirect URIs" section and add:
### http://localhost:1410/
### Save and try yt_oauth() again.


### Step 4: Download YouTube Data

# = Download and prepare data = #
yt_jpm2025 = yt_search(term = "Japan prime minister", max_results = 500)

# Find the channel ID in the source page
# Alternatively, from get_video_details
# = Channel stats = #

nbcnews_stat = get_channel_stats("UCeY0bbntWzzVIaj2z3QigXg")
nbcnews_detail = get_video_details(video_id = "to0YqKKRIWY")

# = Videos = #
curl::curl_version()
httr::set_config(httr::config(http_version = 0)) # Fix curl issue

nbc_videos1 = yt_search(term="", type="video", channel_id = "UCeY0bbntWzzVIaj2z3QigXg")
nbc_videos = nbc_videos1 %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2022-11-27") %>%
  arrange(date)
samplecomment = get_comment_threads(c(video_id = "yWlzWPSsCtg"), max_results = 200)
samplecommentall = get_all_comments(c(video_id = "yWlzWPSsCtg"), max_results = 600)
# = Comments = #
# Be cautious in downloading too many comments to avoid quota limits

# nbc_comments = lapply(as.character(nbc_videos1$video_id), function(x){
#  get_comment_threads(c(video_id = x), max_results = 101)
# })


