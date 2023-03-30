#!/bin/sh

chmod +x proxylex.sh

PROXYLEX_PATH=$(realpath proxylex.sh)

sudo cp proxylex.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl start proxylex
sudo systemctl enable proxylex