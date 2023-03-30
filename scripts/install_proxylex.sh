#!/bin/sh

chmod +x proxylex.sh

PROXYLEX_SH_PATH=$(realpath proxylex.sh)

sudo cp "$(dirname "$(realpath "$0")")/../service/proxylex.service" /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl start proxylex
sudo systemctl enable proxylex