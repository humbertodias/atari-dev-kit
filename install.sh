# dependencies
apk add binutils make gcc musl-dev openssl

# Install
cd /root

# build
wget https://github.com/cc65/cc65/archive/master.zip
unzip master.zip

cd cc65-master
make

# link
ln -s /root/cc65-master/bin/* /usr/bin/

# clean
rm /root/master.zip
apk del binutils make gcc musl-dev openssl

echo "How to Compile"
echo "cl65 -t nes hello-nes.c -o hello.nes"