# Network operators
## HTTP
ni uses `curl` to retrieve HTTP/HTTPS urls. For example:

```bash
$ nc -l 1400 <<'EOF' > /dev/null &      # an enterprise-grade web server
HTTP/1.1 200 OK
Content-length: 12

Hello world
EOF
$ sleep 1; ni http://localhost:1400 n3
Hello world
1
2
3
```

(The `sleep 1` before the ni command is to give the webserver's J2EE stack time
to boot up, which helps the tests work consistently.)

## SSH
You can move a part of your pipeline to another machine via SSH. ni will run
itself on that machine by sending itself over STDIN to `perl`, so it works
whether or not ni exists on the remote system, and whether or not the remote
disk is writable.

The SSH operator is `s` and looks like this:

```sh
$ ni i[who let the dogs out who who who] shostname[gc FW p'r a, length b'] r10
```

Conceptually, here's how ni executes the above:

```sh
$ ni i[who let the dogs out who who who] \
  | ssh hostname "ni gc FW p'r a, length b'" \
  | ni r10
```

You can, of course, nest SSH operators:

```sh
$ ni i[who let the dogs out who who who] shost1[shost2[gc]] r10
```
