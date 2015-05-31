dash-haskell
============
*Never Google for Hackage Results Again* - [ian, barnacles.blackfriday](https://barnacles.blackfriday/)

  **direct to browser lookup:**
  ![look-up](/img/lookup.png?raw=true)
 

  **keyword, module, and package searching:**
  ![listing](/img/listing.png?raw=true)

dash-haskell facilitates Haskell documentation in IDE(s) with the following qualities:
  
  * **Local**

    Documentation is stored in [dash](http://kapeli.com/dash) docsets on the filesystem,
    avoiding the need to query **often inaccurate, version lagged** information from a remote resource such as *hackage*,
    *hoogle*, or *hayoo*.
    
  * **Resolved from Project Dependencies**

    With dash-haskell, one can build dash docsets from:
     * packages listed as dependencies of a *.cabal file
     * packages in a cabal sandbox
     * an arbitrary package db

  * **Scope Narrowed**
    
    IDE plugins such as [helm-dash](https://github.com/areina/helm-dash)
    allow for only specific docsets to be active. This means that
    when searching for an identifier, e.g. *fromJust*, only the
    packages you've built in your configuration are searched. Searches
    can then be further narrowed by module etc.

  * **Prolifically Standardized** 

    [dash docsets](http://kapeli.com/dash) are an open, easily assimilated standard, and
    are used across many IDE(s).

Usage Examples
==============
```dash-haskell -c foo.cabal -s```

builds all packages listed as dependencies in ```foo.cabal```, using atleast the cabal sandbox db

```dash-haskell parsec-3.1.5 ```

builds ```docsets/parsec-3.1.5.docset``` using the default db ordering (global, sandbox, user). 

Note: haddock documentation for the package must first be built prior to calling dash-haskell on it, e.g.
```
$ cabal install --only-dependencies --enable-documentation
```

**dash-haskell** tries to be as self-documenting as possible. Please see:

```dash-haskell --help``` and ```dash-haskell help [option|topic]```

Summary
=======
```
dash-haskell v1.1.0.0, a dash docset construction tool for Haskell packages

Usage: dash-haskell [-o|--output <dir>] [-q|--quiet] [-c|--cabal <file.cabal>]
                    [-x|--cabal-excludes ghc,lens..] [-s|--sandbox]
                    [-n|--no-user] [--db <path-to-package-db>]
                    [-d|--ordering user,sandbox..] [packages]
  additional help is available with "dash-haskell help <topic|option>"

Available options:
  -h,--help                Show this help text
  -o,--output <dir>        the directory to write created docsets to
  -q,--quiet               set to quiet output
  -c,--cabal <file.cabal>  the cabal file to retrieve package dependencies from
  -x,--cabal-excludes ghc,lens..
                           limit package results from a cabal file source
  -s,--sandbox             use cabal sandbox
  -n,--no-user             don't source packages from user db
  --db <path-to-package-db>
                           package db directory
  -d,--ordering user,sandbox..
                           ordering of user, dir, and sandbox db's
  packages                 a list of packages to specifically build, e.g.
                           either-1.0.1 text

http://www.github.com/jfeltz/dash-haskell (C) John P. Feltz 2014, 2015

```

Installation
============
```
$ cabal install dash-haskell
```
or

```
 $ git clone http://www.github.com/jfeltz/dash-haskell
 $ cd dash-haskell
 $ cabal install 
```

Package Resolution
==================
For best results, try to **be version specific** when providing
package arguments, unless you're judicious about which packages are
stored, for example, in your *cabal sandbox db* or *ghc package db*.

IDE Configuration
=================
To use the generated docsets, you will need a plugin for your particular IDE which can access
them.

* **Emacs**

  **dash-haskell** is tested with the following for emacs:

  * [helm](https://github.com/emacs-helm/helm) , a fuzzy finder plugin for emacs
  * [helm-dash](https://github.com/areina/helm-dash) , the essential extension to helm in order to lookup dash docsets

  For limiting helm-dash to project specific docsets, the following also helps. 

  ```lisp
(defun activate-package-docsets (root) 
  (progn
     (setq helm-dash-docsets-path root) 
     (setq helm-dash-common-docsets (helm-dash-installed-docsets))
      
     (message 
      (format "activated %d docsets from: %s" 
        (length helm-dash-common-docsets) root))
  ))
  ```

  ```lisp
  (activate-package-docsets "/path/to/project/docsets/")
```

* For **Vim**, see the following known docset plugins:

    * The zealdocs [vim plugin](http://www.zealdocs.org)
    * The Dash.app [vim plugin](https://github.com/rizzatti/dash.vim)

Features slated for V2
======================
* handle **docset pre-builts and project synchronization**

  set pre-built criteria, pre-built skipping, project package sync.
  and provide a ```--rebuild``` to force rebuild of a docset

* **version biasing** 

  provide option to bias package version to highest when it is otherwise ambiguous

* **summaries**

  provide summary information to help users better understand which
  packages failed and succeeded

* ```conf``` argument support 

    build docset directly from a package db .conf 

* ```doc``` argument 

    build docsets directly from a package doc directory 
    (containing the requisite ```*.haddock``` interface and html files)

Contributors
============
Hirotomo Moriwaki : <philopon.dependence@gmail.com>

The major instigator of this project was Hirotomo Moriwaki's [haddocset](https://github.com/philopon/haddocset), with much of the early implementation of *dash-haskell* influenced by that code-base.

Rudi Grinberg : [github](http://github.com/rgrinberg)

For patches, testing, and helping maintain compatibility of ```dash-haskell-1.0.x.x``` with ghc changes

Author & Maintainer
===================
John P. Feltz <jfeltz@gmail.com>
