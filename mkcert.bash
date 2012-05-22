#!/bin/bash
if [ -a priv/cert.pm -o -a priv/key.pm ]
then
mkdir -p priv/ssl
umask 077 && touch priv/ssl/key.pem priv/ssl/cert.pem
openssl genrsa 2048 > priv/ssl/key.pem
openssl req -new -x509 -nodes -sha1 -days 3650 -key priv/ssl/key.pem < blank > priv/ssl/cert.pem
else
echo
echo "priv/ssl/cert.pm and/or priv/ssl/key.pm already exist."
echo "Delete priv/ssl/cert.pm or priv/ssl/key.pm to create new ones."
echo
fi