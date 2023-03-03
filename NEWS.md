# EpiSoon 0.3.1

This is a maintenance release which updates the package CI and use of functions depreciated since the least release in 2020. It also includes a range of other changes that were missed from previous releases.

Thanks to @nikosbosse, @andreamra, @tuxette, @sbfnk, and @seabbs for code contributions towards this release.

* Updated all GitHub Actions
* Updated `tidyverse` code usage to account for depreciation.
* Updated `pkgdown` site to use `pkgdown` 2.0.0.
* Updated all uses of `size` to `linewidth` to account for depreciation in `ggplot2` line geoms.

# EpiSoon 0.3.0

* Reviewed and updated all tooling and examples
* Added documentation
* Bugs squashed.

# EpiSoon 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added dev support for multiple samples to `evaluate_model` and all higher order functions. Note that this user is expected to manage this themselves for lower level functions. 
* Renamed models to reflect forecasting and not fitting.
* Updated all docs to reflect changes.
* Added case prediction and scoring framework
