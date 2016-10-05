#Quickstart Guide


##How to Learn ni

1. `git clone` the repo, `cd` into the folder, and run `./build.sh` to get `ni` installed on your machine
2. Set up `ssh` (see below).
3. Run `ni --js` and play around with some of the cool [graphical examples](examples.md).  Take a look at the [docs](#using-the-web-interface).
4. Work through the documentation on [streams](stream.md), [row operations](row.md), and [column operators](col.md).
5. Work through the [Perl](perl.md) documentation, which is the most complete. Don't worry if you don't exactly understand the Perl syntax; if you know any scripting language it should make enough sense.
6. Work through the [Ruby](ruby.md) documentation.
   - It should make sense.
   - Note the importance of casting value types, which is unique to Ruby.
7. If you have no Lisp experience, you may want to skip over the Lisp documentation, since its syntax is significantly different from scripting languages.
8. Else:
    - Install SBCL Lisp if necessary (see below)
    - Work through the [Lisp](lisp.md) documentation.

At this point, you are competent enough to write this documentation. To prove this to yourself, put some data in a TSV on your `dev` machine, then use `ni` to stream it to your machine and run a job on it.

##Setting up SSH

`ssh` can be used with ni via `ni s<host> ...` commands. The examples use `dev` for the host name. To enable this, create an entry in your ssh config in `~/.ssh/config` as follows.

```
Host dev
    HostName <hostname>
    Port <Port #>
    User <username>
```

##Setting up Lisp (SBCL)

You can run `ni` without setting up Lisp locally using a Dockerized container. See [lisp.md](doc/lisp.md) for details. I find the local setup a bit more intuitive, plus it'll give you a Lisp REPL if you don't already have one.

Setting up SBCL requires another version of Lisp to bootstrap itself; first download a hardware/OS appropriate version from [here](http://sbcl.sourceforge.net/platform-table.html). Unzip the file, `cd` in to the directory, and run `sudo sh install.sh`. This will install an older version of SBCL on your machine, and put it in your `usr/local/bin/sbcl` if you're on a Unix-like OS (which, since you're using `ni`, you already are).

At this point, `ni` will probably work fine; if you're desperate to get the newest SBCL, or you find that you're having some Lisp-related issues, you can `git clone git@github.com:sbcl/sbcl.git`, `cd` into the directory, and run `sudo sh make.sh`, and then `sudo sh install.sh`, but that takes a long time.



##Using the Web Interface

###Formula Bar
You can enter ni formulas into the top bar (without the explicit `ni` call). 

###Controls

- D : Distance
  - D represents the distance from the camera to the origin
- R : Rotation
  - The first R component is the angle between the image and the plane of the image and the plane of the screen
  - The second R component is the rotation of the image within the plane of the screenin degrees
- x : Dimensional Scaling
  - The first component controls scaling in the direction of the width of the screen;
  - The second component controls scaling in the direction of the depth of the screen;
  - The third component controls scaling in the direction of the height of the screen.

###Viewing a 3D plot to 2D

Set the second x component to 0 to flatten the image's depth dimension; then set the first R component to 0 and the second R component to 90 to show a front-facing view.