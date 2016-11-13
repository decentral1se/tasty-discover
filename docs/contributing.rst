Contributing
============

Overview
--------

If you find anything to be improved or a bug, please submit an issue via
Github. If you can fix it, please go ahead and submit a patch. Check the
issues for things that can be worked on immediately, if you're looking for to
help out.

I use Stack to maintain tasty-discover, therefore, most of the development
environment relies on it. Cabal testing is done in the `.travis.yml`_ file.

.. _.travis.yml: https://github.com/lwm/tasty-discover/blob/master/.travis.yml

Continuous Integration
----------------------

tasty-discover uses `Travis CI`_ for continuous integration testing.

You can see the current status here_.

.. _Travis CI: https://travis-ci.org/
.. _here: https://travis-ci.org/lwm/tasty-discover

Testing
-------

tasty-discover uses tasty-discover to test itself. I'd like to see your changes
tested in the integration-test/ and/or test/ folders, where applicable. If you
think the feature is applicable to a wide audience, you could add it to the
example/ folder as well, so any new user will see it immediately.

You can run the tests as follows:

.. note:: The Makefile is configured to build with Stack.

.. code-block:: bash

    λ make test             # all tests
    λ make unit_test        # only unit tests
    λ make integration_test # only integration tests
    λ make example_test     # only the example code tests

If you forget which, you can always get a quick feedback with:

.. code-block:: bash

    λ make help


Documentation
-------------

tasty-discover documentation is written with `reStructured Text`_ and
graciously hosted for free by readthedocs.org_. You can get up and running with
the following commands:

.. code-block:: bash

    λ python -m venv .venv
    λ . .venv/bin/activate
    λ pip install -U pip -r requirements.txt

Then build the documentation and watch for changes with:

.. code-block:: bash

    λ make live_html

There are several helper commands, you can review them all with:

.. code-block:: bash

    λ make help

Documentation can be found in the docs/ folder.

.. _reStructured Text: http://openalea.gforge.inria.fr/doc/openalea/doc/_build/html/source/sphinx/rest_syntax.html
.. _readthedocs.org: https://readthedocs.org/
