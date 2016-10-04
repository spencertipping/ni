#Getting Started

## Setting up SSH

`ssh` can be used with ni via `ni s<host> ...` commands. The examples use `dev` for the host name. To enable this, create an entry in your ssh config in `~/.ssh/config` as follows.

```
Host dev
    HostName <hostname>
    Port <Port #>
    User <username>
```

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