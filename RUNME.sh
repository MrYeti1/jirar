#!/usr/bin/env bash

#USAGE:   sh ./RUNME.sh <SECRETBASE64JIRADEETS> <SQUAD> PUBLISH
#E.G.     sh ./RUNME.sh c2VjcmV0dXNlcjpzZWNyZXRwYXNz TS PUBLISH

SECRET=$1
SQUAD=$2
PUBLISH=$3

set -x
set -e



if [ ! -f extract/boardList.js ]
then
    cp extract/boardList.js.example extract/boardList.js
    echo "Copied boardList.js.example as extract/boardList.js doesn't exist"
fi
if [ ! -f extract/customFields.js ]
then
    cp extract/customFields.js.example extract/customFields.js
    echo "Copied customFields.js.example as extract/customFields.js doesn't exist"
fi
if [ ! -f atlassianDetails.sh ]
then
    cp atlassianDetails.sh.example atlassianDetails.sh
    echo "Copied atlassianDetails.sh.example as atlassianDetails.sh doesn't exist"
fi

if [ ! -d jiraReport ]
then
    mkdir -p jiraReport
fi

#Source in the $JIRAREST and $CONFLUENCEREST variables
. ./atlassianDetails.sh

DEVMODE=false
if $DEVMODE
then
    SECRET=$SECRET PROJECT=$SQUAD JIRAREST=$JIRAREST docker run -e SECRET -e PROJECT -e JIRAREST -v $(pwd)/jiraData/:/usr/src/app/jiraData/ -v $(pwd)/extract/boardList.js:/usr/src/app/boardList.js  -v $(pwd)/extract/customFields.js:/usr/src/app/customFields.js jirar-extract:dev
    PROJECT=$SQUAD docker run -e PROJECT -v $(pwd):/home/user/jiraR jirar-report:dev
else
    #Not dev-mode - use published images. Containing pre-built files
    SECRET=$SECRET PROJECT=$SQUAD JIRAREST=$JIRAREST docker run -e SECRET -e PROJECT -e JIRAREST -v $(pwd)/jiraData/:/usr/src/app/jiraData/ $DOCKERREPO/grovesro/jirar/jirar-extract:skybet
    PROJECT=$SQUAD docker run -e PROJECT -v $(pwd)/jiraReport:/home/user/jiraR/jiraReport/ -v $(pwd)/jiraData/:/home/user/jiraR/jiraData/ $DOCKERREPO/grovesro/jirar/jirar-report:skybet
fi

mv jiraReport/jiraR.html jiraReport/jiraR-$SQUAD.html

sh ./publish.sh $CONFLUENCEREST $SECRET $SQUAD $PUBLISH



