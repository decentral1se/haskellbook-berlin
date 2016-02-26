#!/bin/bash


# inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd
# and http://ellismichael.com/technical/2015/06/12/using-travis-ci-with-github-pages/

COMMIT_MESSAGE="Publishing $(git log -n 1 --format='commit %h - %s')"

# Prepare an empty directory
cd _site
git init
git config user.name "Incredible CI"
git config user.email "mail@joachim-breitner.de"
git add .
git commit -m "${COMMIT_MESSAGE}"
# The diversion to /dev/null is required to keep the GH_TOKEN secret
git push --force  "git@github.com:nomeata/hal2016-website" master:gh-pages


