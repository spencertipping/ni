#Quickstart Guide

Like most classified knowledge, `ni` is need-to-know; I'd recommend reading over the 

Be judicious about the setup that you do; if you're most comfortable in Ruby, you can be productive with `ni` without setting up SBCL to get Lisp access.

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

Setting up SBCL requires another version of Lisp to bootstrap itself; first download a hardware/OS appropriate version from [here](http://sbcl.sourceforge.net/platform-table.html). Unzip the file, `cd` in to the directory, and run `sudo sh install.sh`. This will install an older version of SBCL on your machine, and put it in your `usr/local/bin/sbcl` if you're on a Unix-like OS.

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