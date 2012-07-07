#!/bin/bash

curl --header "Referer: http://harita.iett.gov.tr/" "http://harita.iett.gov.tr/durak_dinamik_liste_hat.php3?getCountriesByLetters=1&letters=" > output/bus.txt
