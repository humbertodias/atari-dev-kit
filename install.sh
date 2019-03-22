# dependencies
apk add make gcc g++ musl-dev ca-certificates openssl

# Install
cd /root

# build

# cc65
wget https://github.com/cc65/cc65/archive/master.zip
unzip master.zip
rm master.zip
cd cc65-master
make
ln -s /root/cc65-master/bin/* /usr/bin/
cd -

# dasm
wget https://github.com/munsie/dasm/archive/master.zip
unzip master.zip
rm master.zip
cd dasm-master
make
ln -s /root/dasm-master/bin/* /usr/bin/
cd -

# franny
wget https://ufpr.dl.sourceforge.net/project/atari8/franny/Franny-1.1.3/franny-1.1.3.tgz
tar xvfz franny-1.1.3.tgz
rm franny-1.1.3.tgz
cd franny-1.1.3
make franny
ln -s /root/franny-1.1.3/franny /usr/bin/
cd -

# nesasm3
wget https://github.com/toastynerd/nesasm/archive/master.zip
unzip master.zip
rm master.zip
cd nesasm-master
make
ln -s /root/nesasm-master/bin/nesasm /usr/bin/
cd -


# clean
# apk del binutils make gcc musl-dev openssl