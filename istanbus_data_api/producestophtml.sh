#!/bin/bash

curl --header "Referer: http://harita.iett.gov.tr/" -d "DurakName=.&ara=DETAY" "http://mobil.iett.gov.tr/durakarama.php" > output/stop.html
