# Living Server

A system where an LLM generates and hot-loads code into a running Common Lisp web server in real-time. You describe what you want in plain English, Claude generates the Lisp code, and it gets evaluated directly into the running image -- no restart, no deploy, no waiting. The server updates instantly.

## How is this possible?

The trick is that Common Lisp was designed from the ground up for this kind of workflow. Three properties of the language make Living Server work:

### Image-based development

A running Common Lisp process isn't a static executable -- it's a live *image*. Every function, variable, class, and route definition exists as a mutable object in memory. You can redefine any of them at any time and the change takes effect immediately. There's no compilation step, no hot-module-replacement hack, no framework magic. It's just how the language works.

### `*app*` persists across server restarts

The ningle web framework stores all route definitions on an application object. In Living Server, this object lives in a global variable called `*app*`. When we stop and restart the HTTP server, we're only killing the thread that listens for connections -- `*app*` and all its routes stay in memory untouched. The new server thread picks up the same object with all routes intact.

Route registration is an upsert: `(setf (ningle:route *app* "/path") handler)`. Evaluating the same route definition again just updates it in place. This means Claude can freely regenerate and re-evaluate route code without creating duplicates or stale handlers.

### Remote eval via Swank

Swank is the protocol that powers SLIME (the Emacs Lisp IDE). It lets you connect to a running Lisp image over TCP and evaluate arbitrary code inside it. Every professional Lisp developer uses this daily to interact with live systems.

Living Server uses Swank as the bridge between the dashboard and the Lisp server. The dashboard sends generated code over a TCP socket, the Lisp process evaluates it, and the result comes back -- all while the HTTP server continues handling requests. This isn't a custom protocol or a fragile hack. It's the same battle-tested mechanism that Lisp developers have used for decades to work with production systems.

## Architecture

Two completely separate OS processes, connected by Swank:

```
+---------------------------+          +---------------------------+
|   Control Plane (Haskell) |          |   User Server (SBCL)      |
|                           |  Swank   |                           |
|   Dashboard UI (:8080)    |--------->|   HTTP Server (:3001)     |
|   Claude API client       |  (TCP)   |   Swank Server (:4005)    |
|   Code persistence        |          |   ningle *app* + routes   |
|   Safety checks           |          |                           |
+---------------------------+          +---------------------------+
```

**Why two processes?** If generated code ran in the same process as the dashboard, Claude could accidentally overwrite the dashboard routes, the eval engine, or other critical infrastructure. The split gives hard isolation -- generated code can only touch the user server.

The control plane:
- Serves the web dashboard (chat UI, route panel, server controls)
- Talks to the Anthropic API to generate code
- Connects to the Lisp server via Swank to evaluate generated code
- Persists generated code to files so routes survive full restarts
- Runs safety checks before any code is evaluated

The user server:
- Runs Woo (HTTP) + Clack + ningle with generated routes
- Exposes Swank on a local port for the control plane to connect to
- Contains only user-generated code -- no dashboard, no Claude client

## Prerequisites

- **SBCL** (Steel Bank Common Lisp) -- `brew install sbcl`
- **Quicklisp** -- [quicklisp.org](https://www.quicklisp.org/beta/)
- **GHC** (Glasgow Haskell Compiler) -- via [ghcup](https://www.haskell.org/ghcup/)
- **cabal** -- installed alongside GHC via ghcup
- **Anthropic API key** -- for Claude integration

## Running

```bash
export ANTHROPIC_API_KEY=your-key-here
./living-server
```

That's it. The script builds the Haskell dashboard (first run only), starts SBCL, and opens the browser to `http://localhost:8080`.

### What happens on startup

1. The dashboard binary is built with `cabal build` (cached after first run)
2. The dashboard starts and spawns SBCL as a child process
3. SBCL loads the `living-server` system via Quicklisp (compiles native deps on first run)
4. The Lisp server starts: Woo HTTP on port 3001, Swank on port 4005
5. The dashboard connects to Swank
6. Any previously generated routes are loaded from `server/generated/`
7. The dashboard is served at `http://localhost:8080`

### Using the dashboard

1. Type a request in the chat: *"add a GET route /hello that returns Hello World"*
2. Claude responds with a plain-English explanation and a code preview
3. Click **Run** to evaluate the code into the live server
4. The route appears in the sidebar and is immediately accessible at `localhost:3001/hello`

You can iterate: *"actually, make it return JSON instead"* -- Claude sees the current server state and generates updated code.

### Ports

| Port | Service |
|------|---------|
| 8080 | Dashboard (web UI + API) |
| 3001 | User server (generated routes) |
| 4005 | Swank (internal, control plane only) |

## Project structure

```
living-server/
  living-server              # Startup script (single entry point)

  server/                    # Common Lisp user server
    living-server.asd        # ASDF system definition
    boot.lisp                # Boot script for SBCL
    src/
      packages.lisp          # Package definitions
      config.lisp             # Port configuration
      server.lisp             # Woo + Clack lifecycle
      routes.lisp             # ningle app + default routes
      swank.lisp              # Swank server setup
    generated/
      manifest.lisp           # Load-order index for generated code
      routes/                 # Generated route files (persisted)

  dashboard/                 # Haskell control plane
    dashboard.cabal
    app/
      Main.hs                # Entry point
    src/Dashboard/
      Server.hs              # Scotty web server (UI + API)
      Claude.hs              # Anthropic API client
      Prompt.hs              # System prompt with server introspection
      Swank.hs               # Swank client (TCP connection)
      SwankProtocol.hs       # Swank wire protocol encoding
      Process.hs             # SBCL process management
      Persistence.hs         # Code file persistence
      Types.hs               # Shared types
    static/
      index.html             # Dashboard UI
      style.css
      app.js
```
