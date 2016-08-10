# File server demo in a single Haskell file

[![Build Status](https://travis-ci.org/snoyberg/file-server-demo.svg?branch=master)](https://travis-ci.org/snoyberg/file-server-demo)

__Sneak peek__: Run `docker run --rm -p 8080:8080
snoyberg/file-server-demo` and open
[http://localhost:8080](http://localhost:8080).

We've all been there. We need to write some non-trivial piece of
functionality, and end up doing it in bash or perl because that's what
we have on the server we'll be deploying to. Or because it's the
language we can most easily rely on being present at a consistent
version on our coworkers' machines. We'd rather use a different
language and leverage more advanced, non-standard libraries, but we
can't do that reliably.

One option is to create static executables or to ship around Docker
images. This is great for many use cases, and we are going to have a
follow-up blog post about using Docker and Alpine Linux to make such
static executables. But there are at least two downsides to this
approach:

* It's not possible to modify a static executable directly. You need
  to have access to the source code and the tool chain used to produce
  it.
* The executable is tied to a single operating system; good luck
  getting your Linux executable to run on your OS X machine.

Said another way: there are good reasons why people like to use
scripting languages. This blog post is going to demonstrate doing some
non-trivial work with Haskell, and do so with a fully reproducible and
trivially installed toolchain, supported on multiple operating
systems.

## Why Haskell?

Haskell is a functional programming language with high performance,
great safety features, and a large ecosystem of open source libraries
to choose from. Haskell programs are high level enough to be readable
and modifiable by non-experts, making it ideal for these kinds of
shared scripts. If you're new to Haskell, learn more on
[haskell-lang.org](https://haskell-lang.org/).

## The task

We're going to put together a simple file server with upload
capability. We're going to assume a non-hostile environment (like a
corporate LAN with no external network access), and therefore not put
in security precautions like upload size limits. We're going to use
the relatively low-level Web Application Interface instead of a web
framework. While it makes the code a bit longer, there's no magic
involved. Common frameworks in Haskell include
[Yesod](http://www.yesodweb.com/) and
[Servant](http://haskell-servant.readthedocs.io/en/stable/). We're
going to host this all with the blazingly fast Warp web server.

## Get Stack

[Stack](https://haskellstack.org) is a cross-platform program for
developing Haskell projects. While it has many features, in our case
the most important bit is that it can:

* Download a complete Haskell toolchain for your OS
* Install Haskell libraries from a
  [curated package set](https://www.stackage.org/)
* Run Haskell source files directly as a script (we'll show how below)

Check out the
[Get Started page on haskell-lang.org](https://haskell-lang.org/get-started)
to get Stack on your system.

## The code

You can see
[the full source code on Github](https://github.com/snoyberg/file-server-demo/blob/master/FileServer.hs). Let's
step through the important parts here.

### Script interpreter

We start off our file with something that is distinctly _not_ Haskell
code:

```haskell
#!/usr/bin/env stack
{- stack
    --resolver lts-6.11
    --install-ghc
    runghc
    --package shakespeare
    --package wai-app-static
    --package wai-extra
    --package warp
 -}
```

With this header, we've made our file executable from the shell. If
you `chmod +x` the source file, you can run `./FileServer.hs`. The
first line is a standard
[shebang](https://en.wikipedia.org/wiki/Shebang_%28Unix%29). After
that, we have a comment that provides Stack with the relevant command
line options. These options tell it to:

* Use the Haskell Long Term Support (LTS) 6.11 package set. From now
  through the rest of time, you'll be running against the same set of
  packages, so no worries about your code bitrotting!
* Install GHC, the Glasgow Haskell Compiler. LTS 6.11 indicates what
  version of GHC is needed (GHC 7.10.3). Once again: no bitrot
  concerns!
* `runghc` says we'd like to run a script with GHC
* The rest of the lines specify which Haskell library packages we
  depend on. You can see a full list of available libraries in LTS
  6.11 [on the Stackage server](https://www.stackage.org/lts-6.11)

For more information on Stack's script interpreter support, see
[the Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter).

### Command line argument parsing

Very often with these kinds of tools, we need to handle command line
arguments. Haskell has some great libraries for doing this in an
elegant way. For example, see
[the optparse-applicative library tutorial](https://haskell-lang.org/library/optparse-applicative). However,
if you want to go simple, you can also just use the `getArgs` function
to get a list of arguments. We're going to add support for a `sanity`
argument, which will allow us to sanity-check that running our
application works:

```haskell
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["sanity"] -> putStrLn "Sanity check passed, ready to roll!"
        [] -> do
            putStrLn "Launching application"
            -- Run our application (defined below) on port 8080
            run 8080 app
        _ -> error $ "Unknown arguments: " ++ show args
```

### Routing

We're going to support three different routes in our application:

* The `/browse/...` tree should allow you to get a directory listing
  of files in the current directory, and view/download individual
  files.
* The `/upload` page accepts a file upload and writes the uploaded
  content to the current directory.
* The homepage (`/`) should display an HTML page with a link to
  `/browse` and provide an HTML upload form targeting `/upload`.

Thanks to pattern matching in Haskell, getting this to work is very
straightforward:

``` haskell
app :: Application
app req send =
    -- Route the request based on the path requested
    case pathInfo req of
        -- "/": send the HTML homepage contents
        [] -> send $ responseBuilder
                status200
                [("Content-Type", "text/html; charset=utf-8")]
                (runIdentity $ execHtmlT homepage)

        -- "/browse/...": use the file server to allow directory
        -- listings and downloading files
        ("browse":rest) ->
            -- We create a modified request that strips off the
            -- "browse" component of the path, so that the file server
            -- does not need to look inside a /browse/ directory
            let req' = req { pathInfo = rest }
             in fileServer req' send

        -- "/upload": handle a file upload
        ["upload"] -> upload req send

        -- anything else: 404
        _ -> send $ responseLBS
            status404
            [("Content-Type", "text/plain; charset=utf-8")]
            "Not found"
```

The most complicated bit above is the path modification for the
`/browse` tree, which is something a web framework would handle for us
automatically. Remember: we're doing this low level to avoid extra
concepts, real world code is typically even easier than this!

### Homepage content

An area that Haskell really excels at is Domain Specific Languages
(DSLs). We're going to use the
[Hamlet](http://www.yesodweb.com/book/shakespearean-templates) for
HTML templating. There are many other options in the Haskell world
favoring other syntax, such as
[Lucid library](https://www.stackage.org/package/lucid) (which
provides a Haskell-based DSL), plus implementations of
language-agnostic templates, like
[mustache](https://www.stackage.org/package/mustache).

Here's what our HTML page looks like in Hamlet:

``` haskell
homepage :: Html ()
homepage = [shamlet|
$doctype 5
<html>
    <head>
        <title>File server
    <body>
        <h1>File server
        <p>
            <a href=/browse/>Browse available files

        <form method=POST action=/upload enctype=multipart/form-data>
            <p>Upload a new file
            <input type=file name=file>
            <input type=submit>
|]
```

Note that Hamlet - like Haskell itself - uses significant whitespace
and indentation to denote nesting.

### The rest

We're not going to cover the rest of the code in the Haskell file. If
you're interested in the details, please read the comments there, and
feel free to ask questions about any ambiguous bits (hopefully the
inline comments give enough clarity on what's going on).

## Running

Download the `FileServer.hs` file contents (or copy-paste, or clone
the repo), make sure the file is executable (`chmod +x
FileServer.hs`), and then run:

``` shell
$ ./FileServer.hs
```

If you're on Windows, you can instead run:

``` batch
> stack FileServer.hs
```

That's correct: the same source file will work on POSIX systems and
Windows as well. The only requirement is Stack and GHC support. Again,
to get Stack on your system, please see the
[Get Started page](https://haskell-lang.org/get-started).

The first time you run this program, it will take a while to
complete. This is because Stack will need to download and install GHC
and necessary libraries to a user-local directory. Once complete, the
results are kept on your system, so subsequent runs will be almost
instantaneous.

Once running, you can
[view the app on localhost:8080](http://localhost:8080).

## Dockerizing

Generally, I wouldn't recommend Dockerizing a source file like this;
it makes more sense to Dockerize a compiled executable. We'll cover
how to do that another time (though sneak preview: Stack has
[built in support for generating Docker images](https://docs.haskellstack.org/en/stable/yaml_configuration/#image)). For
now, let's actually Dockerize the source file itself, complete with
Stack and the GHC toolchain.

You can
[check out the Dockerfile on Github](https://github.com/snoyberg/file-server-demo/blob/master/Dockerfile). That
file may be slightly different from what I cover here.

```dockerfile
FROM ubuntu:16.04
MAINTAINER Michael Snoyman
```

Nothing too interesting...

```dockerfile
ADD https://github.com/Yelp/dumb-init/releases/download/v1.1.3/dumb-init_1.1.3_amd64 /usr/local/bin/dumb-init
RUN chmod +x /usr/local/bin/dumb-init
```

While interesting, this isn't Haskell-specific. We're just using an
init process to get proper handling for signals. For more information,
see
[dumb-init's announcement blog post](http://engineeringblog.yelp.com/2016/01/dumb-init-an-init-for-docker.html).

```dockerfile
ADD https://get.haskellstack.org/get-stack.sh /usr/local/bin/
RUN sh /usr/local/bin/get-stack.sh
```

Stack has a shell script available to automatically install it on
POSIX systems. We just download that script and then run it. This is
all it takes to have a Haskell-ready system set up: we're now ready to
run script interpreter based files like our `FileServer.hs`!

```dockerfile
COPY FileServer.hs /usr/local/bin/file-server
RUN chmod +x /usr/local/bin/file-server
```

We're copying over the source file we wrote and then ensuring it is
executable. Interestingly, we can rename it to not include a `.hs`
file extension. There is plenty of debate in the world around whether
scripts should or should not include an extension indicating their
source language; Haskell is allowing that debate to perpetuate :).

```dockerfile
RUN useradd -m www && mkdir -p /workdir && chown www /workdir
USER www
```

While not strictly necessary, we'd rather not run our executable as
the root user, for security purposes. Let's create a new user, create
a working directory to store files in, and run all subsequent commands
as the new user.

```dockerfile
RUN /usr/local/bin/file-server sanity
```

As I mentioned above, that initial run of the server takes a long
time. We'd like to do the heavy lifting of downloading and installing
during the Docker image build rather than at runtime. To make this
happen, we run our program once with the `sanity` command line
argument, so that it immediately exits after successfully starting up.

```dockerfile
CMD ["/usr/local/bin/dumb-init", "/usr/local/bin/file-server"]
WORKDIR /workdir
EXPOSE 8080
```

Finally, we use `CMD`, `WORKDIR`, and `EXPOSE` to make it easier to
run. This Docker image is available on Docker Hub, so if you'd like to try
it out without doing a full build on your local machine:

```shell
docker run --rm -p 8080:8080 snoyberg/file-server-demo
```

You should be able to play with the application on
[http://localhost:8080](http://localhost:8080).

## What's next

As you can see, getting started with Haskell as a scripting language
is easy. You may be interested in checking out
[the turtle library](https://www.stackage.org/haddock/lts-6.11/turtle-1.2.8/Turtle-Tutorial.html),
which is a Shell scripting DSL written in Haskell.

If you're ready to get deeper into Haskell, I'd recommend:

* Check out [haskell-lang.org](https://haskell-lang.org/), which has a
  lot of beginner-targeted information, and we're adding more
  regularly.
* Check out
  [Haskell Programming from First Principles](http://haskellbook.com/),
  a book which will get you completely up and running with Haskell
* Join one of the many
  [Haskell online communities](https://haskell-lang.org/community)

FP Complete both supports the open source Haskell ecosystem, as well
as provides commercial support for those seeking it. If you're
interested in learning more about how FP Complete can help you and
your team be more successful in your development and devops work,
[please contact us for a free consultation](mailto:consulting@fpcomplete.com).
