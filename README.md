## Author

[Brian Nei](https://github.com/bnei)
<br/>

## Overview

This app allows the upload of a single .csv consisting of two columns: H and Q.
The CSV <strong>must</strong> have the two headers "H" and "Q" exactly.

It returns parameters for a rating curve of the form:
$Q = a(H-c)^b$

This app provides a UI to the Bayesian Discharge Rating Curves (BDRC) package (Hrafnkelsson et al., 2023). The theory behind using a Bayesian hierarchical model to fit a rating curve can be found in [Hrafnkelsson et al., 2020](https://arxiv.org/abs/2010.04769).

This app also provides an interface for curve fitting with an arbitrary number of breakpoints. However, the use of two or more breakpoints is not recommended unless this is strongly suggested by the data.

## Usage:

The app is hosted on [shinyapps.io](https://bnei.shinyapps.io/bdrc_fitter/) with a free account. Please be mindful of this and close the app/browser tab as soon as possible when you are done to conserve limited computational time.

The app is currently in an Alpha stage, and has <strong>no guarantees of functionality.</strong> Verify all outputs independently. 
<br/>

## License
Copyright © 2024-2026 [Brian Nei](https://github.com/bnei)

Released under the [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
<br/>

## Contributing
Pull Requests are welcome. Note that this project is only provided in the event it is useful to others. Accordingly, it is not necessarily under active development. 
<br/>
<br/>
