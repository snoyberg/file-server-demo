FROM ubuntu:16.04
MAINTAINER Michael Snoyman

# Get dumb-init to avoid Ctrl-C issues. See:
# http://engineeringblog.yelp.com/2016/01/dumb-init-an-init-for-docker.html
ADD https://github.com/Yelp/dumb-init/releases/download/v1.1.3/dumb-init_1.1.3_amd64 /usr/local/bin/dumb-init
RUN chmod +x /usr/local/bin/dumb-init

# Set up Haskell Stack, the Haskell build tool.
# Stack is the only dependency we have to run our application.
# Once available, it will grab everything else we need
# (compiler, libraries, etc).
ADD https://get.haskellstack.org/get-stack.sh /usr/local/bin/
RUN sh /usr/local/bin/get-stack.sh

# Copy over the source code and make it executable.
COPY FileServer.hs /usr/local/bin/file-server
RUN chmod +x /usr/local/bin/file-server

# Create a new user account and directory to run from, and then
# run everything else as that user.
RUN useradd -m www && mkdir -p /workdir && chown www /workdir
USER www

# We run our application with "sanity" to force it to install all of
# its dependencies during Docker image build time, making the Docker
# image launch much faster.
RUN /usr/local/bin/file-server sanity

# We're all ready, now just configure our image to run the server on
# launch from the correct working directory.
CMD ["/usr/local/bin/dumb-init", "/usr/local/bin/file-server"]
WORKDIR /workdir
EXPOSE 8080