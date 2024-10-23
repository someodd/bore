# Bore: Make Gopherholes

Tool for building your own hole in gopherspace (Gopher Protocol).

I made this because I wanted to rebuild my [Burrow](https://github.com/someodd/burrow).

It comes with its own Gopher Protocol server (ripped from [spacecookie](https://github.com/sternenseemann/spacecookie), but the gopherhole it builds can be used by pretty much any Gopher server.

## Features

Here are some features, but I recommend checking out `example/` for an example gopherhole:

* Moustache templating! {{like this for variables}}, but even figlet font functions and ASCII art container support
* Bundled with a Gopher Protocol server
* FrontMatter support, you can choose if something is to become a gophermap/menu (you can even use the [.gophermap standard](https://sternenseemann.github.io/spacecookie/spacecookie.gophermap.5.html) for writing those!)
* Phlogging (blogging) support, with a generated Atom feed and tag and main indexes
* Search support, search all your `.txt` files with a file ranking algorithm

## Quickstart

Just download the lastest `.deb` from the releases.

Edit the `hostname` setting in the `/var/gopher/source/bore.toml` file to reflect the domain you want it to use (or just any IP address).

```
sudo service bore restart
```

Now you should be able to edit/place files in `/var/gopher/source` and you can visit gopher://whateveryourhostname:7071/1/ in a Gopher Client like [Lagrange](https://gmi.skyjake.fi/lagrange/), `lynx`, `gopher`, or even [Dillo](https://dillo-browser.github.io/). I also made my own client [Waffle](https://github.com/someodd/waffle), but I don't recommend that unless you're Haskell-saavy at the moment.

The *assets* directory doesn't get touched when the goperhole is rebuilt. So it's a good idea to put large files here.

If you have any trouble try:

```
sudo journalctl -u bore.service
```

## Tip: easily update server

Let's make a secure/chroot'd setup for sftp'ing source (and assets) updates using the pre-existing `bore` user the package installs.

### Server setup

Create the required directories:

```
sudo mkdir -p /var/gopher/.ssh
```

Then add SSH public key for the `bore` user in `/var/gopher/.ssh/authorized_keys`:

```
ssh-rsa AAAAB3Nza... your_key_here
```

Then add the following lines to your `/etc/ssh/sshd_config`:

```
Match User bore
    ChrootDirectory /var/gopher
    AuthorizedKeysFile /var/gopher/.ssh/authorized_keys
    ForceCommand internal-sftp
    PermitTunnel no
    AllowAgentForwarding no
    AllowTcpForwarding no
    X11Forwarding no
```

Finalize some permissions:

```
sudo chown root:root /var/gopher
sudo chmod 755 /var/gopher

sudo chown -R bore:bore /var/gopher/.ssh
sudo chmod 700 /var/gopher/.ssh
sudo chown -R bore:bore /var/gopher/source
sudo chown -R bore:bore /var/gopher/output

```

Finally restart `sudo systemctl restart ssh`, although you may also want to `sudo service bore restart`.

### Copying to server (from client)

Install `lftp`:

```
sudo apt-get install lftp
```

Now if you want to copy your source directory (local to remote):

```
lftp -e "mirror -R /path/to/local_directory /source/; quit" -u bore, sftp://simulacra
```

... or to ensure old files are deleted (mirror remote/local):

```
lftp -e "mirror -R --delete ./ /source/; quit" -u bore, sftp://simulacra
```

Just copy a single file to assets folder:

```
lftp -e "put -O /output/assets/ 'cool picture.jpg'; quit" -u bore, sftp://simulacra
```

## Plans to port from Burrow

* Columnation
* Possibly markdown (not sure how useful this actually is )