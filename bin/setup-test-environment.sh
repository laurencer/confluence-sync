#!/bin/sh

export CONFLUENCE_URL="http://$(docker-machine ip dev):8090"
export CONFLUENCE_USER="admin"
export CONFLUENCE_PASSWORD="admin"
