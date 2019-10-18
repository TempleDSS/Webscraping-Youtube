#!/usr/bin/python
# -*- coding: utf-8 -*-

import urllib.request
import urllib.parse
import urllib.error
from bs4 import BeautifulSoup
import ssl
import json
import ast
import os
from urllib.request import Request, urlopen
import csv

# For ignoring SSL certificate errors

ctx = ssl.create_default_context()
ctx.check_hostname = False
ctx.verify_mode = ssl.CERT_NONE

# Input from user

ids = { '8BBrr5jlDoc', '9iCd6UHR-3I', 'sb96zxizHXE'  }


with open('names.csv', 'w', newline='') as csvfile:
    fieldnames = [ "TITLE", "CHANNEL_NAME", "NUMBER_OF_VIEWS", "LIKES", "DISLIKES", "NUMBER_OF_SUBSCRIPTIONS", "HASH_TAGS"]
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    
    for video_id in ids:
        url = 'http://www.youtube.com/watch?v='+video_id
    
        # Making the website believe that you are accessing it using a mozilla browser
        
        req = Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        webpage = urlopen(req).read()
        
        # Creating a BeautifulSoup object of the html page for easy extraction of data.
        
        soup = BeautifulSoup(webpage, 'html.parser')
        html = soup.prettify('utf-8')
        video_details = {}
        other_details = {}
        
        for span in soup.findAll('span',attrs={'class': 'watch-title'}):
            video_details['TITLE'] = span.text.strip()
        
        for script in soup.findAll('script',attrs={'type': 'application/ld+json'}):
                channelDesctiption = json.loads(script.text.strip())
                video_details['CHANNEL_NAME'] = channelDesctiption['itemListElement'][0]['item']['name']
        
        for div in soup.findAll('div',attrs={'class': 'watch-view-count'}):
            video_details['NUMBER_OF_VIEWS'] = div.text.strip()
        
        for button in soup.findAll('button',attrs={'title': 'I like this'}):
            video_details['LIKES'] = button.text.strip()
        
        for button in soup.findAll('button',attrs={'title': 'I dislike this'}):
            video_details['DISLIKES'] = button.text.strip()
        
        for span in soup.findAll('span',attrs={'class': 'yt-subscription-button-subscriber-count-branded-horizontal yt-subscriber-count'}):
            video_details['NUMBER_OF_SUBSCRIPTIONS'] = span.text.strip()
        
        hashtags = []
        for span in soup.findAll('span',attrs={'class': 'standalone-collection-badge-renderer-text'}):
            for a in span.findAll('a',attrs={'class': 'yt-uix-sessionlink'}):
                hashtags.append(a.text.strip())
        video_details['HASH_TAGS'] = hashtags
        
        with open(video_id+'_output_file.html', 'wb') as file:
            file.write(html)
        
        with open(video_id+'_data.json', 'w', encoding='utf8') as outfile:
            json.dump(video_details, outfile, ensure_ascii=False,indent=4)
    
        writer.writerow(video_details)
            
        print ('----------Extraction of data is complete. Check json file.----------')
