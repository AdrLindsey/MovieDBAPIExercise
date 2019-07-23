import http.client
import json
from time import sleep
import datetime
import calendar
import dates

#### CHOOSE DATE RANGE ####
start_date = datetime.date(2017,12,1)
end_date = datetime.date(2017,12,31)

from datetime import datetime
start_month = calendar.month_name[datetime.strptime(str(start_date), "%Y-%m-%d").month]
start_year = datetime.strptime(str(start_date), "%Y-%m-%d").year

import datetime


def get_page_count(start_date, end_date):
    conn = http.client.HTTPSConnection("api.themoviedb.org")

    payload = "{}"

    conn.request("GET", "/3/discover/movie?release_date.gte="+str(start_date)+"&release_date.lte="+str(end_date)+"&api_key=606aaffd7ca10f0b80804a1f0674e4e1", payload)

    res = conn.getresponse()
    data = res.read()
    data = data.decode("utf-8").encode("cp1252","replace").decode("cp1252")
    # cp850
    json_data = json.loads(data)
    
    conn.close()

    # print("total pages: "+str(json_data["total_pages"]))
    return json_data["total_pages"]

def get_20(page_number, start_date, end_date):
    # print("fetching page "+str(page_number))
    conn = http.client.HTTPSConnection("api.themoviedb.org")

    payload = "{}"

    conn.request("GET", "/3/discover/movie?release_date.gte="+str(start_date)+"&release_date.lte="+str(end_date)+"&api_key=606aaffd7ca10f0b80804a1f0674e4e1&page="+str(page_number), payload)

    res = conn.getresponse()
    data = res.read()
    data = data.decode("utf-8").encode("cp1252","replace").decode("cp1252")

    json_data = json.loads(data)
    
    conn.close()

    return json_data["results"]

# results = []
# limit = 30
# page_range = 0
# start_date = "2017-12-01"
# end_date = "2017-12-31"
# for start_date in start_dates
#     for page in range(1,get_page_count(str(start_date),str(end_date))+1):
#         if page%limit == 0:
#             sleep(10)
#         results.extend(get_20(page,str(start_date),str(end_date)))
    
results = []
limit = 30
page_range = 0
for page in range(1,get_page_count(str(start_date),str(end_date))+1):
    if page%limit == 0:
        sleep(10)
    results.extend(get_20(page,str(start_date),str(end_date)))



print(str(start_date)+","+str(end_date))

print("genre_ids;id;vote_count;vote_average;popularity;release_date;start_month;start_year")

for result in results:
    print(str(result["genre_ids"])+";"+str(result["id"])+";"+str(result["vote_count"])+";"+str(result["vote_average"])+";"+str(result["popularity"])+";"+str(result["release_date"])+";"+str(start_month)+";"+str(start_year))


