# Anglican

[![Build Status](https://travis-ci.org/LSaldyt/anglican.svg?branch=master)](https://travis-ci.org/LSaldyt/anglican)

Anglican is a probabilistic programming system
implemented in Clojure, both the programming environment and
the language. 
To include Anglican in your Clojure project, add `[anglican "1.0.0"]` to your `project.clj` `:dependencies`.
*This* version of anglican (the LSaldyt fork) can be used with `[org.clojars.lsaldyt/anglican "1.0.1"]`.
[Introduction to Anglican](doc/intro.md) explains
how to write and run programs in anglican.

Everyone is welcome to write programs which call inference,
as well as to extend anglican with new algorithms,
propose changes, and fix bugs. Consult the [developer
guide](doc/devel.md) for details.

[The code map](doc/codemap.md) provides a quick guide around
the source code layout, as well as contains a list of implemented
inference algorithms.

## License

Copyright © 2014, 2015, 2016 Wood group

This file is part of Anglican, a probabilistic programming system.

Anglican is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Anglican is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the [GNU General Public
License](gpl-3.0.txt) along with Anglican.  If not, see
<http://www.gnu.org/licenses/>.

## Continuous Integration

Adds TravisCI
