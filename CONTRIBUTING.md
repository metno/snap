# How to contribute to SNAP

You are welcome to contribute to the SNAP model and it's utilities.

The snap code is stored in the `src/common` directory. It will be preprocessed
under make and copied to the `naccident` directory.
Utilities, like operational implementations or visualizations scripts for results,
can be found in the `utils` directory.

Here are some important resources:

  * Bugs? [Issue tracker](https://github.com/metno/snap/issues)
  * License of all commits: [GPL](https://github.com/metno/snap/blob/main/COPYING)

## Testing

SNAP runs a couple of unit-test on github CI. To run those manually:

```bash
    cd src && ln -s gcc_pkgconfig.mk current.mk
    env SNAP_FIMEX_VERSION=2.1 SNAP_BOUND_CHECKS=1 make test
```
## Submitting changes

Please send a [GitHub Pull Request to SNAP](https://github.com/metno/snap/pull/new/main) with a clear list of what you've done (read more about [pull requests](http://help.github.com/pull-requests/)). When you send a pull request, you may add yourself to the [AUTHORS](https://github.com/metno/snap/blob/main/AUTHORS). We can always use more test coverage. Please follow our [coding conventions](#fortran-code-formatting) and make sure all of your commits are atomic (one feature per commit) if possible.

Always write a clear log message for your commits. One-line messages are fine for small changes, but bigger changes should look like this:

    $ git commit -m "A brief summary of the commit
    >
    > A paragraph describing what changed and its impact."

Python code will be formatted using `pre-commit` and `ruff`. First commit might fail if formatting
needed changes. Please review changes and commit again.


## Fortran code formatting

* The code should generally use an indent of 2
* Use ! for comments
* Prefer spaces to tabs

## Fortran Guidelines

* Avoid compiler specific intrinsics and extensions
* Target the Fortran 2008 standard
* Prefer modules to interfaces or implicit imports
* Use `implicit none` and `private` in all modules

### Code Documentation

Document code-modules, functions and subroutines using !> and doxygen syntax:
https://github.com/Mohid-Water-Modelling-System/Mohid/wiki/Documenting-Fortran-with-Doxygen

## Tools

#### fprettify

https://github.com/pseewald/fprettify

Use with ```fprettify --indent 2 file.F90```

#### Visual Code

Ctrl-Shift-P  configureLanguageBasedSettings

    "[Fortran]": {
        "editor.tabSize": 2
    }

## Python code formatting

For python code, `ruff` is automatically run before any commit. Older code might still
be badly formatted, so it might change when a file is touched. When committing a change to an
old file, please review also those formatting changes. Parts which should not be formatted should
be surrounded by `# fmt: off` and `# fmt: on`, e.g.

```python

# fmt: off
    animals = ["dog",  "cat",  "mouse"]
    moods   = ["like", "like", "don't like"]
# fmt: on

```

### Typing

Typing information for function arguments is encouraged, even if it has not been available during
writing of most of older code which was written before python 3.5.

### Logging

For logging and in particular debugging, the [logging](https://docs.python.org/3/library/logging.html) module should be used.
Unfortunately, some older code still does not.


### Documentation

Please use reST (PEP 287) (=shpinx) for docstrings, e.g.

```
    """
This is a reST style.

:param param1: this is a first param
:param param2: this is a second param
:returns: this is a description of what is returned
:raises keyError: raises an exception
"""
```

For VS-code users, the [autoDocstring](https://marketplace.visualstudio.com/items?itemName=njpwerner.autodocstring) extension with `sphinx-notypes` docstring-Format is useful.
