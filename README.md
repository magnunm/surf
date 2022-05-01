# Surf

Run a series of HTTP(S) requests specified in a plain-text file. Requests are
separated by comment lines, starting with a `#`. Cookies are stored
automatically between the requests within a given file. Inspired by
[restclient.el](https://github.com/pashky/restclient.el).

Build and run using [stack](https://docs.haskellstack.org/en/stable/README/):
```sh
stack build
stack exec surf -- "examples/basic-requests.http"
```

See the `examples/` directory for more example specification files.

### Why?

When using a HTTP(S) client to test a HTTP API it is useful to store the
requests you create for re-use later. Using plain-text makes sharing these
saved requests with others, or adding them to version-control simple.
`restclient.el` solves that, but requires an Emacs installation. The goal here
is to create a stand-alone CLI based on a similar idea.

### Todo

- Nicer printing of responses
