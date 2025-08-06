# Bore: Join the Gopher Protocol

Think something like Jekyll (static site builder), but for [gopherspace](https://gopher.mills.io/gopherproject.org). To actually serve the built content, try [my Gopher server software, Venusia](https://github.com/someodd/venusia).

[Example bore project (source code from my gopherhole)](https://github.com/someodd/gopher.someodd.zip), try building it with bore and messing around with it.

Made with love and my interest for utilizing lofty Ivory tower things in the real world(tm).

## Installing

There are `.deb` packages in [the release section of the Github repository](https://github.com/someodd/bore/releases).

If you want you can simply run `stack build` in the repo.

## Features

* Build your phlog into a Jekyll blog with a single command
* Supports [Bucktooth shorthand](https://raw.githubusercontent.com/jgoerzen/pygopherd/refs/heads/master/doc/standards/gophermap.txt) for writing gophermaps/menus+support for `.gophermap`

* Phlogging features (blogging, but for gopher)
  * Tagging system
  * RSS/Atom feed generation

* Converts Markdown files to a gopherhole
  * Will turn Markdown links into footnote links as to not disrupt flow
  * Can still use gophermap/menu syntax to link to documents in gopherspace

* Templating
  * ASCII-art-like tools
    * Limited [Figlet](http://www.figlet.org/examples.html) font support
    * Custom container boxes--shove content into ascii art boxes, with a format to specify how to draw them--make your own!
    * Word wrap

  * Parent templates
  * Include other files
  * Frontmatter, like jekyll, with controls for things like what type of file to generate, if the file is a draft, etc.
  * Flow control
  * Based on Mustache
  * templating! {{like this for variables}}, but even figlet font functions and ASCII art container support

## Very opinionated structure

Just know for now to take a look at the [example bore project (source code from my gopherhole)](https://github.com/someodd/gopher.someodd.zip) and the directory structure. I'll explain this in more detail later, but I've decided to just make the directory structure rather rigid and opinionated (everything has its place, basically).

## Make a phlog post

Try making a file like `phlog/hello-world.gopher.txt` (that file extension, for now, is important).

Try giving it these contents:

```
---
date: 2025-03-09
title: "XMPP: audio/video call support (prosody)"
tags: [sysadmin, linux, xmpp, prosody]
---

Hello, world!
```

There's other features, too, for the frontmatter of phlog posts like:

* `draft`: if this value is `true` it will hide the post from index
* `gophermap` if this value is `true` this phlog post will be made into a gophermap/menu rather than a simple plain text file

## Building your phlog to a Jekyll blog

Instead of building your posts to a phlog (gopher) you can also build to a [Jekyll](https://jekyllrb.com/) blog format.

I use this for my website, built with Jekyll+GitHub Pages, which you can see in action on [https://www.someodd.zip/] -- just take a look at my blog, namely [the phlog-mirror section](https://www.someodd.zip/phlog-mirror).

Use a command like:

```
bore jekyll --source /home/tilde/Projects/gopherhole_bore --output /home/tilde/Projects/someodd.github.io/
```

It assumes `_posts` in the output directory and `phlog/` in the source directory.

There's frontmatter to control how posts are made into Jekyll blog posts:

* `skipJekyll`: if `true` do NOT build this post to the Jekyll blog
* `jekyll`: use this directly as frontmatter in Jekyll.

Example of a post with the `jekyll` frontmatter:

```
---
date: 2024-05-17
parent: post.txt
title: Productivity in Window Maker
tags: [windowmaker, debian, linux]
jekyll:
  image:
    path: /assets/screenshot-window-maker.png
    thumbnail: /assets/screenshot-window-maker.png
    caption: My Window Maker setup (screenshot).
---

I use the old school, lightweight [Window Maker](http://www.windowmaker.org/)
[window manager](https://en.wikipedia.org/wiki/Window_manager) on Debian
Unstable.
```

This will output a file that looks like this:

```
---
date: 2024-05-17
image:
  caption: My Window Maker setup (screenshot).
  path: /assets/phlog/screenshot-window-maker.png
  thumbnail: /assets/phlog/screenshot-window-maker.png
tags:
- windowmaker
- debian
- linux
title: Productivity in Window Maker
---

I use the old school, lightweight [Window Maker](http://www.windowmaker.org/)
[window manager](https://en.wikipedia.org/wiki/Window_manager) on Debian
Unstable.
```

## Assets

The *assets* directory doesn't get wiped when the goperhole is rebuilt (in the output directory), so if you want put large files in your output directory's `asset/` subdirectory.

### Tip: client/server setup I use

Let's make a secure/chroot'd setup for sftp'ing source (and assets) updates using the pre-existing `bore` user the package installs.

#### Server setup

I have to update this for instructions on using with [Venusia](https://github.com/someodd/venusia).

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

#### Copying to server (from client)

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

