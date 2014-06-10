# Functions to scrape leaderboard scores from the crossfit games website


import requests
import pandas as pd
from bs4 import BeautifulSoup
from urllib import urlencode

# db params for later
# db_user = "crossfit"
# db_pass = "rz7uvqFMnuWUQj5e"

base_url = "http://games.crossfit.com/scores/leaderboard.php?"
default_params = {'stage':         5,
                  'sort':          0,
                  'page':          1,
                  'division':      1,
                  'region':        0,
                  'numberperpage': 100,
                  'competition':   0,
                  'frontpage':     0,
                  'expanded':      0,
                  'year':          14,
                  'full':          1,
                  'showtoggles':   0,
                  'hidedropdowns': 1,
                  'showathleteac': 1,
                  'is_mobile':     1}
 
def scrape_leaderboard(query_params):
    params = default_params
    params.update(query_params)
    query_string = base_url + urlencode(params)              
                                            
    page = requests.get(base_url + query_string)
    soup = BeautifulSoup(page.text)

    for tr in soup.find(id='lbtable').find("tbody").find_all("tr"):
        tds = tr.find_all('td')
        if(len(tds)):
            print "Rank: %s, Name: %s, WOD1: %s, WOD2: %s, WOD3: %s, WOD4: %s, WOD5: %s" % \
              (tds[0].text.strip(), tds[1].text.strip(), tds[2].text.strip(), 
               tds[3].text.strip(), tds[4].text.strip(), tds[5].text.strip(), 
               tds[6].text.strip())
               
    

scrape_leaderboard(query_params = {'page':2})