#!/usr/bin/env python
import os.path
import urllib.request
from datetime import timedelta, date

def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days + 1)):
        yield start_date + timedelta(n)

start_date = date(2016, 10, 1)
end_date = date(2017, 3, 31)
for single_date in daterange(start_date, end_date):
    url = "http://www.wunderground.com/history/airport/EKCH/" + single_date.strftime("%Y/%m/%d") + "/DailyHistory.html?format=1"
    file_name = "EKCH-Daily-" + single_date.strftime("%Y-%m-%d") + ".csv"

    if os.path.isfile(file_name):
        continue

    print("Downloading weather data for EKCH " + single_date.strftime("%Y-%m-%d"))

    data = urllib.request.urlopen(url).read().decode('utf-8')
    lines = data.split('\n')

    with open(file_name, "w") as file_handle:        

        header = True;

        for line in lines:
            if (not line or line.isspace()):
                continue

            if (header):
                file_handle.write("CET," + line.replace("<br />", "") + "\n")                
                header = False
            else:
                file_handle.write(single_date.strftime("%Y-%m-%d") + "," + line.replace("<br />", "") + "\n")

for single_date in daterange(start_date, end_date):
    url = "http://www.wunderground.com/history/airport/EKCH/" + single_date.strftime("%Y/%m/%d") + "/MonthlyHistory.html?format=1"
    file_name = "EKCH-Monthly-" + single_date.strftime("%Y-%m") + ".csv"

    if single_date.day == 1 or os.path.isfile(file_name):
        continue

    print("Downloading weather data for EKCH " + single_date.strftime("%Y-%m"))
    
    data = urllib.request.urlopen(url).read().decode('utf-8')
    lines = data.split('\n')
    
    with open(file_name, "w") as file_handle:        

        for line in lines:
            if (not line or line.isspace()):
                continue

            file_handle.write(line.replace("<br />", "") + "\n")
