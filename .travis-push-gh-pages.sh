#!/bin/bash


# inspired by https://gist.github.com/domenic/ec8b0fc8ab45f39403dd
# and http://ellismichael.com/technical/2015/06/12/using-travis-ci-with-github-pages/

openssl aes-256-cbc -K $encrypted_4d415b5e837a_key -iv $encrypted_4d415b5e837a_iv -in travis-deploy-key.enc -out ~/.ssh/id_rsa -d
chmod go-rwx ~/.ssh/id_rsa
COMMIT_MESSAGE="Publishing site on $(date "+%Y-%m-%d %H:%M:%S") from $(git log -n 1 --format='commit %h - %s')" 

# Prepare an empty directory
cd _site
git init
git config user.name "Incredible CI"
git config user.email "mail@joachim-breitner.de"
git add .
git commit -m "${COMMIT_MESSAGE}"
# The diversion to /dev/null is required to keep the GH_TOKEN secret
git push --force  "git@github.com:${TRAVIS_REPO_SLUG}" master:gh-pages


