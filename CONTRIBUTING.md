# How to contribute to SNAP

You are welcome to contribute to the SNAP model and it's utilities.

The snap code is stored in the `src/common` directory. It will be preprocessed 
under make and copied to the `naccident` directory.
Utilities, like operational implementations or visualizations scripts for results, 
can be found in the `utils` directory.

Here are some important resources:

  * Bugs? [Issue tracker](https://github.com/metno/snap/issues)
  * License of all commits: [GPL](https://github.com/metno/snap/blob/master/COPYING)

## Testing

TBD

## Submitting changes

Please send a [GitHub Pull Request to SNAP](https://github.com/metno/snap/pull/new/master) with a clear list of what you've done (read more about [pull requests](http://help.github.com/pull-requests/)). When you send a pull request, you may add yourself to the [AUTHORS](https://github.com/metno/snap/blob/master/AUTHORS). We can always use more test coverage. Please follow our [coding conventions](#fortran-code-formatting) and make sure all of your commits are atomic (one feature per commit) if possible.

Always write a clear log message for your commits. One-line messages are fine for small changes, but bigger changes should look like this:

    $ git commit -m "A brief summary of the commit
    > 
    > A paragraph describing what changed and its impact."


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

#### Black

Linter for python, https://github.com/psf/black

Use with ```black file.py```
