# Bore: Make Gopherholes

Tool for building your own hole in gopherspace (Gopher Protocol).

I made this because I wanted to rebuild my [Burrow](https://github.com/someodd/burrow).

It comes with its own Gopher Protocol server (ripped from [spacecookie](https://github.com/sternenseemann/spacecookie), but the gopherhole it builds can be used by pretty much any Gopher server.

## Quickstart

Just download the lastest `.deb` from the releases.

Edit the `hostname` setting in the `/var/gopher/source/bore.toml` file to reflect the domain you want it to use (or just any IP address).

```
sudo service bore restart
```

Now you should be able to edit/place files in `/var/gopher/source` and you can visit gopher://whateveryourhostname:7071/1/ in a Gopher Client like [Lagrange](https://gmi.skyjake.fi/lagrange/), `lynx`, `gopher`, or even [Dillo](https://dillo-browser.github.io/). I also made my own client [Waffle](https://github.com/someodd/waffle), but I don't recommend that unless you're Haskell-saavy at the moment.

If you have any trouble try:

```
sudo journalctl -u bore.service
```

## Plans to port from Burrow

* Columnation
* Possibly markdown (not sure how useful this actually is )