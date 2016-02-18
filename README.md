# RealWorldHaskell

My solutions to exercises from the popular book by Bryan O'Sullivan, Don Stewart, and John Goerzen.

## Setting up Dev Environment

- Install `haskell-platform`, `haskell-platform-doc`, and `haskell-platform-prof`

  This should bring in `ghc`, `haddock`, `alex`, `happy`, and others.

- Install `hlint`, `hoogle`, and `luakit`

- Copy `hoogle.desktop` to `~/.local/share/applications`

- Copy `hoogle.service` to `/etc/systemd/system`

- Start and Enable `hoogle.service` in systemd

  (If you're not using `systemd` (wut?!),
   add `hoogle server --local --port 1080` to your startup services,
   however you happen to do so on your system.)
