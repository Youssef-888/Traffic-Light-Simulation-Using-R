# Traffic-Light Timing Simulation in R

This project uses Monte Carlo simulation to compare different traffic-light timing strategies for a two-road intersection. The goal is to identify which strategy reduces vehicle waiting time while maintaining fairness between the main road and side road.

## Project Overview

The simulation models a stochastic traffic-light queueing system where:

- Vehicles arrive randomly using exponential interarrival times.
- Vehicle types have different service headways.
- Pedestrian requests add extra lost time.
- Traffic demand changes across normal, rush, and recovery periods.
- Multiple signal-timing strategies are compared under the same traffic environment.

The project compares five strategies:

1. Equal 35/35
2. Short 30/20
3. Main Priority 50/25
4. Long Cycle 70/30
5. Adaptive Pressure

## Methods Used

- Monte Carlo simulation
- Queueing system modeling
- Exponential random-variable generation
- Bootstrap confidence intervals
- ECDF analysis
- Sensitivity analysis
- Time-average queue estimation
- Strategy scoring and comparison

## Key Results

The Adaptive Pressure strategy achieved the best overall performance. It produced the lowest average waiting time and remained competitive under higher traffic demand. The Equal 35/35 strategy performed poorly because equal green time was not suitable when the two roads had unequal demand.

In the base scenario, Adaptive Pressure achieved an average waiting time of approximately 17.65 seconds, compared with 45.50 seconds for Equal 35/35.

## Technologies

- R
- Base R plotting
- Monte Carlo simulation
- Statistical analysis
- Queueing models

## Files

- `Traffic Light Simulation.R`: Main simulation code
- `Traffic Light Simulation Project Report.pdf`: Full project report

## Limitations

This is a simplified simulation model created for an academic project. It does not represent a specific real-world intersection and does not include turning lanes, lane changes, accidents, downstream blocking, or emergency vehicles.

## Author

Youssef Sarofeem
