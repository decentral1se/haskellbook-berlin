#!/bin/bash

COMMIT_MESSAGE="Publishing $(git log -n 1 --format='commit %h - %s')"

cd _site
git init
git config user.name "Incredible CI"
git config user.email "foo@bar.com"
git add .
git commit -m "${COMMIT_MESSAGE}"
git push --force  "git@github.com:lwm/haskellbook-berlin" master:gh-pages
