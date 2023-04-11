# proxylex

A reverse proxy and load balancer written in Lisp. Customizable and lightweight

## Dependencies

You need to have Steel Bank Common Lisp (SBCL) installed in order to compile the Lisp code and run the scripts.
You can install it from the official website (http://www.sbcl.org/platform-table.html) or through your package manager, depending on your operating system.

## How to run as a systemd service on Linux
1. Make `proxylex.sh` executable

```
chmod +x proxylex.sh
```

    1.1 if necessary, adjust path to `proxylex-main.lisp`

2. If necessary, set `$PROXYLEX_PATH` variable
3. Reload systemd configuration 

```
sudo systemctl daemon-reload
```

4. Start the service

```
sudo systemctl start proxylex
```

    4.1 To enable at startup 

```
sudo sytemctl enable proxylex
```

    4.2 To stop service 

```
sudo systemctl stop proxylex
```

## Installing via script

You can also use the `install_proxylex.sh` script

```
chmod +x install_proxylex.sh
./install_proxylex.sh
```
