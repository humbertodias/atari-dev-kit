FROM alpine:3.8

# Update
RUN apk update

# Add
ADD install.sh /root

# Install
RUN /root/install.sh

WORKDIR "/root"