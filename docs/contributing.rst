Contributing
============

All Contributions Welcome!
--------------------------

If you find anything to be improved or a bug, please go ahead and submit a pull
request via Github. Check the issues for things that can be worked on
immediately.

I use Stack to maintain tasty-discover, therefore, most of the development
environment relies on it. If you use Cabal and want to add Cabal specific
commands, please do!

Continuous Integration
----------------------

tasty-discover uses `Travis CI`_ for integration.

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

.. note:: The Makefile is configured to run the tests via Stack.

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

tasty-discover documentation is written with `reStructured Text`_ and graciously
hosted for free by readthedocs.org_. If you need to make a change, you'll need to have
a Python 3.5 environment on your machine.

You can get up and running with the following commands:

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

Documentation can be found in the source/ folder.

.. _reStructured Text: http://openalea.gforge.inria.fr/doc/openalea/doc/_build/html/source/sphinx/rest_syntax.html
.. _readthedocs.org: https://readthedocs.org/
