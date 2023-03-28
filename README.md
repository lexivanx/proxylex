# proxylex
A reverse proxy and load balancer written in Lisp. Customizable and lightweight

## How to run as a systemd service on Linux
1. Make `proxylex.sh` executable 'chmod +x proxylex.sh',
1.1 if necessary, adjust path to proxylex-main.lisp
2. If necessary, set $PROXYLEX_PATH variable
3. Reload systemd configuration 'sudo systemctl daemon-reload'
4. Start service 'sudo systemctl start proxylex'
4.1 To enable at startup 'sudo sytemctl enable proxylex'
4.2 To stop service 'sudo systemctl stop proxylex'

## You can also use the install_proxylex.sh script
Make sure to make it executable 'chmod +x install_proxylex.sh'
