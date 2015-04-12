# -*- coding: utf-8 -*- 

import pandas as pd
import requests
import numpy as np
import json
from time import sleep
import os

# https://developers.google.com/maps/documentation/geocoding/


def geokoduj_szkoly_ponadgimnazjalne(sciezka_z, sciezka_do, maksimum=2500):

    szkoly = pd.read_csv(sciezka_z, index_col='id_szkoly')
    szkoly = szkoly[szkoly['typ_szkoly'].isin(['LO', 'LOU', 'T', 'TU', 'LP'])]
    szkoly = szkoly.fillna("")  # na puste adresy

    if os.path.isfile(sciezka_do):
        lokalizacje = pd.read_csv(sciezka_do, index_col='id_szkoly')
    else:
        lokalizacje = pd.DataFrame(columns=['lat', 'lng', 'location_type', 'location_count'])
        lokalizacje.index.name = 'id_szkoly'

    i = 0

    print("Juz jest {0} z {1}".format(len(lokalizacje), len(szkoly)))

    for id_szkoly, szkola in szkoly.iterrows():

        if id_szkoly not in lokalizacje.index:

            i += 1
            if i > maksimum:
                break

            zapytanie = zapytanie_szkoly(szkola['adres'], szkola['miejscowosc'], szkola['pna'], szkola['poczta'])
            print("{:4d}: {:s}".format(i, zapytanie))

            # moze warto uzyc wojewodztwa?
            geokodowanie = geokoduj(zapytanie)

            if geokodowanie['status'] != "OK":
                print("Status" + geokodowanie['status'])
                if geokodowanie['status'] != "ZERO_RESULTS":
                    break

            lokalizacje.loc[id_szkoly] = wyciagnij_najlepsze_znalezisko(geokodowanie['results'])

            sleep(0.2)  # 5 zapytan na sekunde

    lokalizacje.index.name = 'id_szkoly'  # as there is some error: https://github.com/pydata/pandas/issues/9857
    lokalizacje['location_count'] = lokalizacje['location_count'].apply(int)  # nie czaje czemu float
    lokalizacje.to_csv(sciezka_do)
    print("Zapisano do: " + sciezka_do)


def geokoduj(s, **kwargs):
    params = {"address": s, "sensor": "false", "language": "pl"}
    params.update(kwargs)
    r = requests.get("http://maps.googleapis.com/maps/api/geocode/json",
                     params=params)
    return r.json()


def wyciagnij_najlepsze_znalezisko(gres):
    n = len(gres)
    if n == 0:
        return {'lat': np.nan,
                'lng': np.nan,
                'location_type': np.nan,
                'location_count': n}
    else:
        return {'lat': gres[0]['geometry']['location']['lat'],
                'lng': gres[0]['geometry']['location']['lng'],
                'location_type': gres[0]['geometry']['location_type'],
                'location_count': n}


def zapytanie_szkoly(adres, miejscowosc, pna, poczta):
    if len(adres) < 5:  # male miejscowosci - zwykle sam numer
        return "{0} {1}, {2} {3}".format(miejscowosc, adres, pna, poczta)
    else:
        return "{0}, {1} {2}".format(adres, pna, poczta)


if __name__ == "__main__":
    geokoduj_szkoly_ponadgimnazjalne("../dane/szkoly2014.csv", "../dane/lokalizacje_ponagdimnazjalnych_2014.csv")
