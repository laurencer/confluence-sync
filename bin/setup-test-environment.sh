#!/bin/sh

export CONFLUENCE_TEST_URL="http://$(docker-machine ip dev):8090"
export CONFLUENCE_TEST_USER="admin"
export CONFLUENCE_TEST_PASSWORD="admin"
export CONFLUENCE_TEST_SPACE="ds"
