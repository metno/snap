# The SNAP source-code

The snap code is stored in the common directory. It will be preprocessed under make and copied to the naccident, volcano
and traj directory.

## Code formatting

* The code should generally use an indent of 2. Use ! for comments.
* Prefer spaces to tabs

## Fortran Guidelines

* Use as few non-standard functions as possible
* Prefer modules to interfaces or implicit imports
* Use `implicit none` and `private` in all modules

### Code Documentation

Document code-modules, functions and subroutines using !> and doxygen syntax:
https://github.com/Mohid-Water-Modelling-System/Mohid/wiki/Documenting-Fortran-with-Doxygen

### Tools

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
