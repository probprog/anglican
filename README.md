# Embedded Anglican

[We](https://bitbucket.org/dtolpin/embang/wiki/Contributors) have re-implemented Anglican as an embedded language. An
Anglican program is translated into Clojure through a Clojure
macro, and then the code (converted into CPS) is executed
natively. The points where inference algorithms must intervene
and affect sampling are exposed by stopping the execution and
returning continuation, along with auxiliary information.

Development [news are in the wiki](https://bitbucket.org/dtolpin/embang/wiki).

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

This work is supported under DARPA PPAML through the U.S. AFRL under Cooperative Agreement number FA8750-14-2-0004. The U.S. Government is authorized to reproduce and distribute reprints for Governmental purposes notwithstanding any copyright notation heron. The views and conclusions contained herein are those of the authors and should be not interpreted as necessarily representing the official policies or endorsements, either expressed or implied, of DARPA, the U.S. Air Force Research Laboratory or the U.S. Government.