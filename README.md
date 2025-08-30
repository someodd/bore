# Bore: Build Text for the Gopher Protocol

Bore is a command-line tool that builds gopherholes, much like Jekyll builds websites. It's designed to be simple, powerful, and to make creating content for Gopherspace fast and fun.

  * **Gopher what?** Gopher is a simple, text-based internet protocol that came before the web. It's still used by a community of enthusiasts who appreciate its simplicity and focus on content.
  * **What does Bore do?** 🛠️ It takes your text files, Markdown, and templates and turns them into a Gopher site (a "gopherhole") that you can serve with a Gopher server like my [Venusia](https://github.com/someodd/venusia).
  * **See an example:** 🗺️ Check out the [example project](https://github.com/someodd/gopher.someodd.zip) to see how a Bore site is structured.

## Features 🌟

  * **Jekyll Blog Converter:** ✒️ Convert your Gopher blog ("phlog") into a Jekyll blog with a single command.
  * **Gophermap Support:** 📜 Full support for `.gophermap` files and [Bucktooth shorthand](https://raw.githubusercontent.com/jgoerzen/pygopherd/refs/heads/master/doc/standards/gophermap.txt) for creating Gopher menus.
  * **Phlogging Engine:** ✍️
      * **Tagging System:** Organize your posts with tags. Bore automatically generates an index page for each tag, as well as a main index for all your posts.
      * **Atom Feed Generation:** Automatically creates an Atom feed for your phlog.
  * **Markdown Conversion:** 📝 Write your content in Markdown and Bore will convert it to be Gopher-friendly, turning links into footnotes to keep your text clean and readable.
  * **Powerful Templating:** 🎨
      * Use Mustache-style `{{templates}}` for variables and logic.
      * **Figlet Fonts:** Create ASCII art text with included Figlet fonts.
      * **ASCII Art Containers:** Wrap your content in customizable ASCII art boxes.
      * **Word Wrap:** Automatically wrap your text to a specified width.
      * **Parent Templates & Includes:** Create a consistent layout with parent templates and include other files as partials.
      * **Frontmatter:** Control how your files are built using YAML frontmatter, similar to Jekyll.

## Getting Started 🚀

### Installation

The easiest way to install Bore is to grab a `.deb` package from the [releases page](https://github.com/someodd/bore/releases). You can also build it from source using `stack build`.

### Your First Phlog Post

Bore has an opinionated but simple directory structure. To create a new phlog post, create a new file in the `phlog/` directory. The file extension is important; use `.gopher.txt` for plain text or `.gopher.md` for Markdown.

For example, create `phlog/my-first-post.gopher.txt` with the following content:

```yaml
---
date: 2025-03-09
title: "My First Post"
tags: [getting-started, gopher]
---

Hello, world! This is my first post in my new gopherhole.
```

  * **`draft: true`**: Add this to your frontmatter to hide a post from the generated indexes.
  * **`gophermap: true`**: Add this to turn your post into a Gopher menu instead of a text file.

### Building your site

To build your gopherhole, run the following command in your project's root directory:

```bash
bore build
```

  * This will generate your site in the `output/` directory by default. You can change the source and output directories with the `--source` and `--output` flags.
  * Use the `--dev-mode` flag to set the hostname to `localhost` for easy local testing.

### Building your Phlog to a Jekyll Blog

One of Bore's powerful features is its ability to convert your phlog into a Jekyll-compatible blog. When you use this feature, Bore will also copy any images referenced in your posts from your `assets/` directory to a corresponding `assets/phlog` directory in your Jekyll project, making it easy to keep your images organized.

To do this, run the `jekyll` command:

```bash
bore jekyll --source /path/to/your/gopherhole --output /path/to/your/jekyll/_posts
```

Bore provides two special frontmatter fields to control the Jekyll output:

  * **`skipJekyll: true`**: This will prevent the post from being included in the Jekyll build.
  * **`jekyll: { ... }`**: Any YAML you put here will be added to the frontmatter of the generated Jekyll post. This is great for adding things like images, captions, or other Jekyll-specific metadata.

**Example:**

```yaml
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

This will be converted to a Jekyll post with the `image` data in the frontmatter.

## The `assets` directory 📁

When you build your site, Bore clears out the output directory before generating new files. However, the `assets/` subdirectory of the output directory is special: it will **not** be wiped. This makes it the perfect place to store large files, images, or anything else you want to include in your gopherhole that Bore shouldn't process.

## Tip: client/server setup I use

Let's make a secure/chroot'd setup for sftp'ing source (and assets) updates using the pre-existing `bore` user the package installs.

### Server setup

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