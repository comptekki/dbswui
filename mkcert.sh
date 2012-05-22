#!/bin/sh
mkdir -p priv/ssl
umask 077 && touch priv/ssl/key.pem priv/ssl/cert.pem
openssl genrsa 2048 > priv/ssl/key.pem
openssl req -new -x509 -nodes -sha1 -days 3650 -key priv/ssl/key.pem < blank > priv/ssl/cert.pem