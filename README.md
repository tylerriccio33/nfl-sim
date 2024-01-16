# NFLSim

<!-- badges: start -->

<!-- badges: end -->

The goal of NFLSim is to provide a novel prediction model reliant on resampling instead of a meta inference agent. In other words, the spread or like models (elo, fpi, etc.) rely on a final point differential prediction, considering game level factors. This method instead simulates each **play** by sampling from similar situations.

The results are interpretable which lends the benefit of player props, individualized game reports and more importantly... **distributions**. When the `main` module is run, which functions as the api, the simulations will run according to the arguments.

## A Warning

This codebase is currently **not** finished, even remotely. There are no tests yet, many private methods are exposed and the main module is poorly configured for general use. Therefore, although the functionality under the hood is nearly all there, the APIs will change drastically before any stable use.

## Installation

You can install the development version of NFLSim like so:

``` r
# ...
```

## Example

To run the simulations, you can use the API `main()` function.

``` r
library(NFLSim)
main(...)
```

## Future Features

A few things I consider key features not yet realized:

-   Custom data loading process allowing for user supplied features passed to the sampler.

-   Custom sampler passed by the user to the simulation.

## Bugs

A few things that are blatant bugs:

-   No real half time.

-   Home teams not afforded advantages.

-   Home team always get ball first.

-   Kicker and punter samples tend to be *very* over sampled for recent events. This makes a hot hand at each position probably more influential than it needs to be.

-   Each team doesn't explicitly know about the clock, although it knows when it should pass and when it is losing.

-   All kickoffs are touch backs.

## Shortcomings

-   The resampling technique prioritizes recent events rather than skill, unlike other models. This has far more advantages than disadvantages. However, it will fail to capture larger team changes outside of the QB.

    -   For example, if there was a running back hurt all last year and they make their debut week 1, the model won't factor this into the resampling. There is *some* information indicating new talent but nothing great enough (yet) to influence the decision making in a major way.

-   Significant Roster Changes

    -   Like the last point, only *some* new information is available about significant roster changes. The model/resampling leverages existing recent information about how each team performed in some context. This context assumes at least some change in roster/personnel but it has no great way of tracking how great the change is.
