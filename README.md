*Please Note: this is a github pre-release for Haskell community vetting-
prior to submission to Hackage. Feedback to this is wanted.*

dash-haskell
============
A solution to the Haskell IDE documentation problem.
                              
  ![look-up](/img/lookup.png?raw=true)
  ![listing](/img/listing.png?raw=true)

The purpose of the dash-haskell is to facilitate Haskell documentation in IDE(s), with the following qualities:
  
  * **Local**

    Documentation is stored in [dash](http://kapeli.com/dash) docsets on the filesystem,
    avoiding the need to query **often inaccurate** information from a remote resource such as *hackage*,
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

Summary
=======
```
  Usage: dash-haskell [-p|--dbprovider <provider,args>] [-o|--output <dir>]
                      [-q|--quiet] [-c|--cabal <file.cabal>]
                      [-r|--cabal-constraints executable=name, ..] [packages]
    additional help is also available on arguments with "dash-haskell help arg"

  Available options:
    -h,--help                Show this help text
    -p,--dbprovider <provider,args>
                             a ghc package db provider: cabal|ghc|dir
    -o,--output <dir>        the directory to write created docsets to
    -q,--quiet               set to verbose output
    -c,--cabal <file.cabal>  the cabal file to source package dependencies from 
    -r,--cabal-constraints executable=name, ..
                             limit package results from a cabal file source, see
                             documentation
    packages                 a list of packages to specifically build, e.g.
                             either-1.0.1 text-1.2.0
```

Usage Example
=============
The following example shows how to use **dash-haskell** to generate
docsets for a **cabal sandbox project**.

```
  $ cd foo-1.2.0/ 
  $ dash-haskell -c foo.cabal -o docsets 
  db provider:
    lookup strategy: cabal sandbox db index
    cmd: cabal
    args: sandbox hc-pkg list

  processing: system-filepath-0.4.12
    writing files..
    writing plist..
    populating database..
    finished populating sqlite database..

  processing: system-fileio-0.3.14
    writing files..
    writing plist..
    populating database..
    finished populating sqlite database..

  processing: pipes-4.1.2
    writing files..
    writing plist..
    populating database..
    finished populating sqlite database..

  warning: failed to process: parsec-3.1.5
  warning: path errors in pkg conf file:
   /home/jpf/local/cabal-sandboxes/dash-haskell/x86_64-linux-ghc-7.8.3-packages.conf.d/parsec-3.1.5-abf7e89cafe4e74712f678cea843c1c8.conf
  with problem(s):
   missing: haddock interface file
   missing: html doc dir

  processing: sqlite-simple-0.4.8.0
    writing files..
    writing plist..
    populating database..
    finished populating sqlite database..

  $

```

Notice, the failure of ```parsec-3.1.5``` is illustrated here to show that
in this case, dash-haskell depends on **haddock documentation** being built for
the requested package.
A possible resolution in this case, if using a sandbox, is:

```
$ cabal install --reinstall parsec-3.1.5 --enable-documentation
$ dash-haskell parsec-3.1.5 -o docsets
```

**dash-haskell** tries to be as self-documenting as possible. Please see:
```
$ dash-haskell help [option|topic]
```

Installation
============
A hackage package is forthcoming. For now **dash-haskell** can be installed with:
```
 $ git clone http://www.github.com/jfeltz/dash-haskell
 $ cd dash-haskell.git 
 $ cabal install 
```

Package Resolution
==================

As a general rule, try to **be version specific** when providing package arguments,
unless you are judicious about which packages are stored in your *cabal sandbox* or *ghc package db*. 
There is a lot of hidden behavior that goes into how dash-haskell resolves packages, 
for example, consider the hypothetical package arguments:
```
$ dash-haskell either parsec-1.2 parsec
```
This chooses by default:

* ```either``` and ```parsec-1.2``` as the parameter packages. 
  If for example ```parsec``` were sourced from a cabal file, with ```-c```, 
  ```parsec-1.2``` would still be chosen instead. 
* cabal as a package db provider:
  implicitly calling ```cabal sandbox hc-pkg list```.
  By convention the first db that provides the unversioned package is chosen.
* ```either-4.1.0``` and ```parsec-1.2``` are then selected from the package db,
where their config files are parsed for the documentation sources to be converted. 

IDE Configuration
=================
To use the generated docsets , you will need a plugin for your particular IDE which can access
them.

* **Emacs**

  **dash-haskell** is tested with the following for emacs:

  * [helm](https://github.com/emacs-helm/helm) , a fuzzy finder plugin for emacs
  * [helm-dash](https://github.com/areina/helm-dash) , the essential extension to helm in order to lookup dash docsets

  For limiting helm-dash to project specific docsets, the following also helps. 

  ```lisp
  (defun activate-package-docsets (root) 
    "activate all docsets in a given directory"
    (progn
       ; force docsets in root to be recognized as installed docsets
       (setq helm-dash-docsets-path root) 
       ; append those to the docset list
       (setq
          helm-dash-common-docsets
          (append (helm-dash-installed-docsets) helm-dash-common-docsets ))
    ))
  ```

  ```lisp
  (activate-package-docsets "/path/to/project/docsets/")
```

* For **Vim**, see the following known docset plugins:

    * The zealdocs [vim plugin](http://www.zealdocs.org)
    * The Dash.app [vim plugin](https://github.com/rizzatti/dash.vim).

Features slated for V2
======================
* handle **docset pre-builts**

    set pre-built criteria, pre-built skipping, and provide a ```--rebuild``` to force rebuild of a docset

* **summaries**

    provide summary information to help users better understand which
    packages failed and succeeded

* **version biasing** 

    provide option to bias package version to highest when it is otherwise ambiguous

* ```conf``` argument support 

    build docset directly from a package db .conf 

* ```doc``` argument 

    build docsets directly from a package doc directory 
    (containing the requisite ```*.haddock``` interface and html files)

Contributors
============
**haddocset**

The major instigator of this project was
[haddocset](https://github.com/philopon/haddocset), with much of the
early implementation of *dash-haskell* influenced by that code-base.

Author & Maintainer
===================
John P. Feltz <jfeltz@gmail.com>
