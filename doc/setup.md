#Setup Guide


##Setting up SSH

`ssh` can be used with ni via `ni s<host>[...]` commands. The examples use `dev` for the host name. To enable this, create an entry in your ssh config in `~/.ssh/config` as follows.

```
Host dev
    HostName <hostname>
    Port <Port #>
    User <username>
```

##Setting up Lisp (SBCL)

You can run `ni` without setting up Lisp locally using a Dockerized container.
See [lisp.md](doc/lisp.md) for details. I find the local setup a bit more
intuitive, plus it'll give you a Lisp REPL if you don't already have one.

Most Linux distributions have `sbcl` available as a package; on Ubuntu the
install process looks like this:

```sh
$ sudo apt install sbcl
```

Setting up SBCL manually requires another version of Lisp to bootstrap itself;
first download a hardware/OS appropriate version from
[here](http://sbcl.sourceforge.net/platform-table.html). Unzip the file, `cd`
in to the directory, and run `sudo sh install.sh`. This will install an older
version of SBCL on your machine, and put it in your `usr/local/bin/sbcl` if
you're on a Unix-like OS (which, since you're using `ni`, you already are).

At this point, `ni` will probably work fine; if you're desperate to get the
newest SBCL, or you find that you're having some Lisp-related issues, you can
`git clone git@github.com:sbcl/sbcl.git`, `cd` into the directory, and run
`sudo sh make.sh`, and then `sudo sh install.sh`, but that takes a long time.

##Setting Up Docker
There's a GUI for this. It's not hard.
