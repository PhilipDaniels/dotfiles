# dotfiles
dotfiles: bash, vim, terminal and misc config for Linux and WSL.

### Linux Installation instructions
Do the below - then see the comment in mybashrc.sh.

This repo is expected to live at ~/repos/dotfiles. It is best to clone it using SSH
as it makes pushing changes easier. First create my .ssh folder and install my keys,
note that the permissions are important: https://superuser.com/questions/215504/permissions-on-private-key-in-ssh-folder

```
mkdir -m700 ~/.ssh
# Copy in my id_phil files...
chmod 600 ~/.ssh/id_phil
chmod 644 ~/.ssh/id_phil.pub
```

Possibly you will need to install keychain and eval it manually the first time:

```
sudo apt install keychain
eval `keychain --quiet --eval id_phil`
```

```
mkdir ~/repos
cd ~/repos
git clone git@github.com:PhilipDaniels/dotfiles.git       # ssh, or
git clone https://github.com/PhilipDaniels/dotfiles.git   # https
```

For more on SSH: http://philipdaniels.com/blog/2016/12/setup-of-ssh/
