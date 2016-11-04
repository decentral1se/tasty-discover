#!/usr/bin/env python3

templates_path = ['_templates']

source_suffix = '.rst'

master_doc = 'index'

project = 'tasty-discover'

copyright = '2016, Luke Murphy'

author = 'Luke Murphy'

version = '1.0.0'

release = '1.0.0'

exclude_patterns = ['test', 'example', 'integration-test', 'src']

pygments_style = 'sphinx'

html_theme = 'alabaster'

html_static_path = ['_static']

htmlhelp_basename = 'tasty-discoverdoc'
