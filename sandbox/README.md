# Confluence Sandbox Environment

Having a sync tool is nice and good but without a sandbox environment, its tedious to play around with and test. Docker makes it easy to quickly create new sandbox containers that contain a full Confluence install.

The instructions below will help you create a ready-to-go-image.

## Prerequisites

This sandbox environment assumes that you have Docker installed and a Docker Host accessible. I recommend using [docker-machine](https://docs.docker.com/machine/install-machine/) to quickly get started.

## Building a Sandbox Image

To quickly set up a sandbox Confluence environment for development/testing - you can use the provided `Dockerfile`.

1. Build the base image (run from the `sandbox/` directory): `docker build .`
2. Start the container: `docker run --publish 8090:8090 <image id from previous step>` 
3. Connect to the container: `open http://<docker host ip>:8090` (if using Docker Machine you can use `open http://$(docker-machine ip dev):8090`).
4. Step through the setup wizard:
	- Use a trial licence (you can create as many as you need)
	- Don't install any plugins
	- Use local Confluence users and create an admin account:
    	* Username = `admin`
		* Password = `admin`
		* Email    = `admin@example.org`
5. Open up the *Confluence Admin* page (click the gear icon in the top-right corner) and set the following options:
	- **Enable the APIs**: General Configuration > Feature Settings > Remote API (XML-RPC & SOAP)
	- **Enable HTML Macros**: Manage Add-ons > System (under drop-down menu) > Confluence HTML Macros > Enable all of the macros.
6. Find the docker container id: `docker ps`
7. Save the changes you've made: `docker commit <container id> confluence-sync-tool/confluence-sandbox:<version>`

## Running the Sandbox Image

Now you can start a new sandbox by running `docker run --publish 8090:8090 confluence-sync-tool/confluence-sandbox:<version>`. All data is epheremal so if you break the sandbox - you simply have to restart it.

On Mac OS X - you can run `open http://$(docker-machine ip dev):8090` (assuming you use docker machine with a machine named `dev`).

## Tips and Tricks

You can create a different version of Confluence by passing the `--env CONF_VERSION="<my version">` when building the base image (for example `docker build --env CONF_VERSION="5.2.3" . `).
    
## FAQ

**Why not use an existing Confluence Docker Image?**

The included `Dockerfile` is based off [cptactionhank/atlassian-confluence](https://hub.docker.com/r/cptactionhank/atlassian-confluence/) but with the volumes disabled (to make the entire container completely ephermeral).

Also - the setup wizard is difficult/tedious/brittle to automate and you probably don't want to have to activate a new trial installation each time. By not using volumes - you can then later `docker commit` the current state - so you can instantly start a new sandbox.
