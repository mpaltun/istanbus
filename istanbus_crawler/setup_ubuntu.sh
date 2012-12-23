#!/bin/bash

DEPENDENCIES=("libxml2" "libxslt1.1" "libxml2-dev" "libxslt1-dev" "python-dev")
for dep in "${DEPENDENCIES[@]}"
do
	echo "$dep is installing"
	apt-get install $dep
done

SETUP_TOOLS='setuptools-0.6c11-py2.7.egg'
SETUP_TOOLS_LINK="http://pypi.python.org/packages/2.7/s/setuptools/$SETUP_TOOLS"
curl -s $SETUP_TOOLS_LINK > $SETUP_TOOLS
sh $SETUP_TOOLS
easy_install --allow-hosts=lxml.de,*.python.org lxml

easy_install pymongo