Install
=======

tasty-discover supports Cabal_ and Stack_ installations.

.. _Cabal: https://www.haskell.org/cabal/
.. _Stack: https://docs.haskellstack.org/en/stable/README/

Cabal
-----

::

    λ cabal install tasty-discover

Stack
-----


::

    λ stack install tasty-discover

Stack/Git support
-----------------

You may install tasty-discover via Stack by adding the following to your
stack.yaml. This is useful for getting the most up to date version from the Git
repository:

.. code-block:: haskell

    packages:
    - '.'
    - location:
        git: "https://github.com/lwm/tasty-discover.git"
        commit: "HEAD"

.. note:: tasty-discover relies on tasty-th-0.1.4. You may need to explicitly set this in your stack.yaml:

          .. code-block:: bash

            extra-deps:
            - "tasty-th-0.1.4"

Stack template
--------------

You can bootstrap a new project with this `predefined Stack template`_:


.. _predefined Stack template: https://github.com/commercialhaskell/stack-templates/blob/master/tasty-discover.hsfiles

.. code-block:: bash

    λ stack new <new-project-name> tasty-discover
