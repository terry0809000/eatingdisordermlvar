# Methodology

## Design

This analysis is an exercise-restricted EMA network comparison. The target observations are rows where `exercise_yn == 1` at time `t`. The base model includes binge eating, compensatory behaviours, dietary restraint, positive affect, negative affect, body satisfaction, and fear of weight gain. The additive model adds maladaptive exercise (`mal_ex`).

## Temporal lag definition

Lagged predictors are taken from the immediately previous survey within the same participant and same day using the `beep` variable. The lagged row is not restricted to exercise-only observations.

## Contemporaneous network

The contemporaneous network is estimated among exercise-target rows. Each node is residualised on its lagged predictors, and partial correlations among residuals are used to form an undirected contemporaneous network.

## Temporal network

The temporal network is directed. Each current node is regressed on all lagged nodes using a mixed-effects model with participant-level random intercepts. Directed edges represent lagged fixed-effect associations from predictor node at `t-1` to outcome node at `t`.

## Centrality

For undirected contemporaneous networks, node strength is calculated as the sum of absolute edge weights. For directed temporal networks, in-strength and out-strength are calculated separately, and total strength is their sum.
