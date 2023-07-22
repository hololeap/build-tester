# build-tester
A utility for build-testing packages in a gentoo repository. It looks for
packages from the repo that are not currently installed, chooses one, tries
to emerge it, then repeats.

This is intended for development sandboxes which are testing the repository
ecosystem en-masse (for instance, testing the gentoo-haskell ecosystem
before keywording a new version of GHC).
