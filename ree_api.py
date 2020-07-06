import requests
import json
import csv
import time
import os
from pprint import pprint
from jsonmerge import merge


def request_locations(start_date, end_date, file):
    url = "https://apidatos.ree.es/es/datos/balance/balance-electrico?start_date="+start_date+"&end_date="+end_date+"&time_trunc=day"
    response = requests.request("GET", url)
    with open(file, 'w') as f:
        json.dump(response.json(), f, indent=1)

    return response.json()



if __name__ == "__main__":

    # Date from 2018
    ree_2018 = request_locations("2018-01-01T00:00", "2018-12-31T22:00", "ree_2018.json")
    
    # Date from 2019
    ree_2019 = request_locations("2019-01-01T00:00", "2019-12-31T22:00", "ree_2019.json")

    # Date from 2019
    ree_2020 = request_locations("2020-01-01T00:00", "2020-07-01T00:00", "ree_2020.json")


    row_list = [["id","category","subcategory","value","percentage","datetime"]]

    count = 1

    for type_energy in ree_2018['included']:
        category = type_energy["type"]
        print(type_energy['id'])
        for content in type_energy['attributes']['content']:
            subcategory = content["type"]
            for daily in content['attributes']["values"]:
                value = daily["value"]
                percentage = daily["percentage"]
                datetime = daily["datetime"]
                print("#")
                row_list.append([count, category, subcategory, value, percentage, datetime ])
                count += 1

    for type_energy in ree_2019['included']:
        category = type_energy["type"]
        print(type_energy['id'])
        for content in type_energy['attributes']['content']:
            subcategory = content["type"]
            for daily in content['attributes']["values"]:
                value = daily["value"]
                percentage = daily["percentage"]
                datetime = daily["datetime"]
                print("#")
                row_list.append([count, category, subcategory, value, percentage, datetime ])
                count += 1

    for type_energy in ree_2020['included']:
        category = type_energy["type"]
        print(type_energy['id'])
        for content in type_energy['attributes']['content']:
            subcategory = content["type"]
            for daily in content['attributes']["values"]:
                value = daily["value"]
                percentage = daily["percentage"]
                datetime = daily["datetime"]
                print("#")
                row_list.append([count, category, subcategory, value, percentage, datetime ])
                count += 1

    print(count)
    print(row_list)
    with open('ree.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerows(row_list)
