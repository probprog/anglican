# Embedded Anglican

I have re-implemented Anglican as an embedded language. An
Anglican program is translated into Clojure through a Clojure
macro, and then the code (converted into CPS) is executed
natively. The points where inference algorithms must intervene
and affect sampling are exposed by stopping the execution and
returning continuation, along with auxiliary information.

The implementation, documentation, and examples are in the
leiningen project tree rooted at [`code`](https://bitbucket.org/dtolpin/embang/src/HEAD/code/).

## License

Copyright © 2014, 2015 Wood group

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
[http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).
