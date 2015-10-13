# Confluence Sync Tool

> ***Confluence Sync Tool*** lets you author content on your local machine in either HTML or Markdown and then synchronise it to Confluence.

Ever wanted to write Markdown or use a static site tool to author Confluence Content? Well now you can!

This tool takes a static HTML site (or Markdown content) and syncs it to a Confluence Space. Simply point it at your directory of Markdown content (or the output of a tool like Jekyll) and it will make it available as a set of Confluence pages.

## Installing

Currently no binary releases are distributed. You can build from source using the below instructions:

Run `stack install` and ensure that `~/.local/bin` is added to your path.

## Using the Tool

    confluence-sync-tool - syncs HTML/Markdown content to Confluence

    Usage: confluence-sync-tool --space-id <SPACE ID> --page-name <PAGE TITLE>
                                [--page-id <PAGE ID>] <SYNC DIRECTORY>
      Synchronises all of the pages in the sync-directory to the given space and
      places them all under the specified page.

    Available options:
      -h,--help                Show this help text
      --space-id <SPACE ID>    The id of the Confluence Space to synchronise with
                               (typically in the URL `/display/<space id>/<page
                               name>`).
      --page-name <PAGE TITLE> The name of the page that all content should be
                               synced under.
      --page-id <PAGE ID>      Optional page id of the sync page (the id is usually
                               found when editing the page in the URL
                               `/pages/editpage.action?pageId=<page id>`)
      --min-delay-between-requests <DELAY IN MILLIS>
                               Minimum time to wait between each API request in
                               milliseconds. (default: 2000 ms)
      --max-delay-between-requests <DELAY IN MILLIS>
                               Maximum time to wait between each API request in
                               milliseconds. (default: 60000 ms)
      --request-backoff <MULTIPLIER>
                               Multiplier for the API request time to calculate the
                               delay before starting next
                               action. (default: (3 % 2) x)
      --max-number-of-requests <MAX # of REQUESTS>
                               Maximum number of API requests that are allowed to be
                               made. (default: 10000 reqs)
      <SYNC DIRECTORY>         Directory containing the website/content to sync to
                               Confluence

    Expects the following environment variables to be set:

    CONFLUENCE_URL="confluence server address" (e.g. `http://192.168.99.100:8090/`)
    CONFLUENCE_USER="confluence username"      (e.g. `admin`)
    CONFLUENCE_PASSWORD="confluence password"  (e.g. `password123`)

When run the tool will find or create a *sync-root* page in the space (identified via `<SPACE ID>`). The *sync-root* page will be called `<PAGE TITLE>`.

It then searches for all files in the provided *sync directory* (located on your local disk at `<SYNC DIRECTORY>`). It will create a page for each HTML or Markdown file found in the *sync directory*. Any references to images or other content (javascript, css, etc) will be attached to the relevant page as attachments.

## Building

Confluence Sync Tool is written in Haskell. To get started install [Stack](https://github.com/commercialhaskell/stack).

- **Build/compile:** `stack build`.
- **Run tests:** `stack test`
- **To run the executable:** `stack build && stack exec confluence-sync-tool`

Some of the integration tests require an instance of Confluence to be running. See the *Sandbox/Dev Environment* section below on how to set this up. You also need to set some environment variables - see `bin/setup-test-environment.sh` for an example.

## Sandbox/Dev Environment

See the [sandbox](sandbox/README.md) for information on how to create a Docker Confluence Sandbox. You can also use a pre-baked docker image: `laurencer/confluence-sync-sandbox:latest`.

You can get started by running: `docker run --publish 8090:8090 laurencer/confluence-sync-sandbox` and then `open http://$(docker-machine ip dev):8090/`. The username and password are both `admin`.

## FAQ

**What about images and other assets?**

All images and other assets (e.g. javascript, css, etc) referenced by HTML/Markdown pages will be attached to the Confluence page (as attachments) and the links/references will be re-written to point to the attachment URLs.

**What about links between pages?**

Links to other pages within the synchronised site are rewritten to point to the appropriate Confluence URLs.

**Why is the XML-RPC API used and not the REST API?**

Implementing the XML-RPC API gives support to the largest range of Confluence versions (the REST API was only stabilised in recent versions).

The plan is to also implement the REST API and support the use of either for when the XML-RPC API is depreciated.

**Why is the syncing so slow?**

By default the tool waits at least `2 seconds` between each API request and also implements a request back-off mechanism (where the next request is delayed by at least the amount of time it took for the server to respond multipled by a constant).

These delays can be tweaked by using the `--min-delay-between-requests`, `--max-delay-between-requests`, and `--request-backoff` command line arguments.

**Why do my asset files (i.e. css, images, etc) get renamed with a funny string at the end?**

The sync tool parses all pages for links, images and other references to files that are included in the site directory (e.g. css, javascript, images, pdfs, etc).

For performance reasons - the MD5 hash of the file is included in the filename of the attachment. This makes it simple to check whether an asset file needs to be updated, and simplifies the logic of the sync tool.

**Why are some arguments given as environment variables and others on the command line?**

The tool is designed to be used from a CI server where global/common configuration settings are set via the environment variables (e.g. the URL of the Confluence site). The other arguments are expected to change on a per sync-repository basis (e.g. the page to sync to).

**Why does it require my Confluence password in a plaintext environment variable?**

This is due to the way the Confluence API works. The tool is designed to be used from a CI server (such as TeamCity) where the credentials can be securely injected as environment variables.

**Why is `haxr` included as a vendor dependency?**

The current version of `haxr` is incompatible with the Confluence XML-RPC API because it does not escape `>` when encoding strings. This means that `CDATA` sections cannot be included in the content (which is required for the `HTML` Macro).

Until this gets fixed in master - it is included as a vendor dependency. See [haxr#12](https://github.com/byorgey/haxr/pull/12) for more information.


**Why is `http-streams` included as a vendor dependency?**

The current version of `http-streams` does not provide any way to customise the
timeout for socket/HTTP operations. This can cause the sync tool to timeout
when working with slow Confluence servers.

Until the API is improved, and timeouts are exposed to the application layer
(via Haxr as well), a custom version is included that fixes the send/receive
timeouts to `60 seconds` (which should be enough for even the slowest
instances).

Until this gets fixed in master - it is included as a vendor dependency.
See [http-streams#93](https://github.com/afcowie/http-streams/issues/93) for more information.

**Will you maintain this tool? Is it supported? Should I rely on it?**

> **TL;DR** use at your own risk (though the risk should be minimal).

The tool is open-source and all of the content is authored in open formats (HTML, Markdown, etc). However - admittedly the manual editing experience of synced content within Confluence is not great (since everything is wrapped in the HTML Macro - though this may improve in future versions of the tool).

As with all weekend projects, I can't promise any on-going support or maintainence of the tool. I will try and keep it up to date and make improvements as long as I have to use Confluence, but beyond that it is open-source. Feel free to fork or contribute (via pull requests) if you find a bug or want a new feature!
