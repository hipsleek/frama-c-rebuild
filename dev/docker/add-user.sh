#!/bin/bash -eu

# Create user, add to wheel, allow sudo, avoid sudo lecture

if [ $# -lt 1 ]; then
    echo "usage: $0 user"
    exit 2
fi

NEWUSER=$1
if [ -e /etc/debian_version ]; then
    # Debian
    apt-get update -y
    apt-get install -y sudo passwd
    yes "y" | adduser $NEWUSER
    usermod -a -G sudo $NEWUSER
    passwd -d $NEWUSER
elif [ -e /etc/fedora-release ]; then
    # Fedora
    dnf update -y
    dnf install -y sudo passwd
    groupadd $NEWUSER
    yes "" | adduser -g "${NEWUSER}" -G wheel $NEWUSER
    passwd -d $NEWUSER
elif [ -e /etc/alpine-release ]; then
    # Alpine
    apk update
    apk add sudo
    yes "" | adduser -g "${NEWUSER}" $NEWUSER
    addgroup $NEWUSER wheel
    passwd -d $NEWUSER
else
    echo "error: unsupported Linux distribution"
    exit 1
fi

# Add to sudoers and avoid lecture
echo "$NEWUSER ALL=(ALL) ALL" > /etc/sudoers.d/$NEWUSER
chmod 0440 /etc/sudoers.d/$NEWUSER
echo "%wheel ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel
echo -e '\nDefaults lecture = never' >> /etc/sudoers
