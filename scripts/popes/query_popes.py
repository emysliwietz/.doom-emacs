#!/usr/bin/env python3

from bs4 import BeautifulSoup
import requests
import os
from tqdm import tqdm


def download(url, pathname):
    """
    Downloads a file given an URL and puts it in the folder `pathname`
    """
    # download the body of response by chunk, not immediately
    response = requests.get(url, stream=True)
    # get the total file size
    file_size = int(response.headers.get("Content-Length", 0))
    # get the file name
    filename = os.path.join(pathname)
    # progress bar, changing the unit to bytes instead of iteration (default by tqdm)
    with open(filename, "wb") as f:
        for data in response.iter_content(1024):
            # write data read to the file
            f.write(data)
            # update the progress bar manually


def download_image(element):
    base = "https://www.vatican.va"
    father_page = base + element.find("a")["href"]
    resp = requests.get(father_page, headers=headers)
    page = BeautifulSoup(resp.content, 'lxml')
    img = page.find(class_="pope-img").find("img")["src"]
    name = element.text.strip()
    download(base + img, f"images/{name}.png")
    return name


headers = {'User-Agent': 'Ark (TempleOS) HolyC'}
url = 'https://www.vatican.va/content/vatican/de/holy-father.index.html#holy-father'

response = requests.get(url, headers=headers)

soup = BeautifulSoup(response.content, 'lxml')

table = soup.select('#holy-father')[0]
holy_fathers = table.find_all('tr')

iterator = tqdm(holy_fathers)
tsv = open("popes.tsv", "w")
info = open("pope_info.txt", "w")
for father in iterator:
    fields = father.find_all('td')
    data = [download_image(j) if j.find_all("a") else j.text.strip() for j in fields]
    if not data:
        continue
    iterator.set_description(str(data[1]).rjust(25) if len(data) > 1 else "")
    tsv.write("\t".join(data) + "\n")
    info.write(f"Seine Heiligkeit Papst {data[1]}, der {data[0]}. Papst, Herkunft {data[5]}, Pontifikat von {data[2]} bis {data[3]}." + ("" if not data[4] else " Bekannt unter dem weltlichen Namen " + data[4] + ".") + "\n")
tsv.close()
info.close()
