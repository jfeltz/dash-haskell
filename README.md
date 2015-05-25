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

Summary
=======
```
```

Usage Example
=============
The following example shows how to use **dash-haskell** to generate
docsets for a **cabal sandbox project**.

```

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
To use the generated docsets , you will need a plugin for your particular IDE which can access
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
Hirotomo Moriwaki : <philopon.dependence@gmail.com>

The major instigator of this project was Hirotomo Moriwaki's [haddocset](https://github.com/philopon/haddocset), with much of the early implementation of *dash-haskell* influenced by that code-base.

Rudi Grinberg : [github](http://github.com/rgrinberg)

For patches, testing, and helping maintain compatibility of ```dash-haskell-1.0.x.x``` with ghc changes

Author & Maintainer
===================
John P. Feltz <jfeltz@gmail.com>
