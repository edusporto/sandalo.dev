#!/bin/bash

DEP_HOST="ubuntu@ssh.sandalo.dev"
DEP_PATH="/home/ubuntu/apps/caddy/"

rsync -avP 'site' "$DEP_HOST:$DEP_PATH" && \
ssh "$DEP_HOST" "cd $DEP_PATH && docker-compose restart"
