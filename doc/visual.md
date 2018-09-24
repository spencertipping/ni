# Visualizing data
See also [examples.md](examples.md).

```sh
$ ni --js
http://localhost:8090
```

## Using the web interface
You can view the example in the screenshots by opening the following link on
your system while running `ni --js`:

```
http://localhost:8090/#%7B%22ni%22:%22n2E2p'r%20$_,%20a%20for%200..199'%20p'r(a*10%20+%20$_,%20b*10),%20r(a*10,%20b*10%20+%20$_)%20for%200..9'%20p'r%20a,%20sin(1%20+%20a%20/%20340)%20*%20cos(b*b%20/%2030000)%20+%20sin((a%20+%2050)*b%20/%20120000),%20b'%22,%22v%22:%7B%22br%22:1,%22ot%22:%5B-770.7425674121397,1.8043842499741498,-872.5578946367137%5D,%22os%22:%5B0.3134861808826051,46.99306323157935,0.4025242240336357%5D,%22sa%22:0.03,%22cr%22:%5B24.607594936708868,29.16455696202535%5D,%22cd%22:385.7425530696977,%22axes%22:%5B0,1,2,3%5D%7D%7D
```

![web ui](http://spencertipping.com/ni-jsplot-sinewave.png)

The UI consists of three main components: the ni command editor (top), the plot
(center), and a data preview (left). The data preview is normally hidden, but
you can hover over the left side of the screen to pop it out (click it to
toggle locking):

![data preview](http://spencertipping.com/ni-jsplot-sinewave2.png)

The view angle can be panned, rotated, and zoomed:

- **mouse drag:** pan
- **shift + drag:** 3D rotate
- **ctrl + drag, alt + drag, mousewheel:** zoom
