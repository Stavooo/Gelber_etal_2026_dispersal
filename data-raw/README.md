# Model Output Data

This directory should contain the raw simulation output downloaded from Zenodo.
Each run is a subfolder named by its run number (e.g. `data-raw/300/`) holding the
per-repetition CSV outputs (`*_output_general.csv`, `*_output_dispersal.csv`,
`*_output_sample.csv`) and the parameter files (`*_static_parameters.csv`,
`*_varying_parameters.csv`).

The figures are produced by twenty repetitions per parameter combination. The
figure scripts cache a per-run summary to `data-raw/_cache/summary_<run>.rds` on
first use, so re-runs do not re-read the full CSV output.

Download the data from Zenodo: https://doi.org/10.5281/zenodo.18519816

## Run-number to figure map

The main-text figures use the **log-normal** kernel (300-block). The
**exponential** kernel (200-block) is the robustness comparison used in
Figures S7--S10.

| Figure | Experiment | Log-normal run | Exponential run |
| --- | --- | --- | --- |
| Fig. 2a--c | vary autocorrelation (continuous) | `300` | `200` |
| Fig. 2d--f | vary disturbance spread rate (continuous) | `301` | `201` |
| Fig. 2g--i | vary disturbance frequency (continuous) | `304` | `204` |
| Fig. 2j--l | vary fragmentation (modified) | `302` | `202` |
| Fig. 2m--o | vary habitat amount (modified) | `303` | `203` |
| Fig. 3 | autocorrelation x disturbance spread rate (continuous) | `310` | `210` |
| Fig. 4 | fragmentation x habitat amount (modified) | `320` | `220` |
| Figs. 5--7 | habitat x disturbance x autocorrelation (modified) | `231` | `230` |
| Fig. S1, S3 | habitat x autocorrelation x disturbance (modified, fragmentation 0.7) | `370` | -- |
| Fig. S2, S4 | fragmentation x autocorrelation x disturbance (modified, habitat 0.2) | `371` | -- |
| Fig. S5 | one-at-a-time sensitivity: birth rate, death rate, niche breadth (modified) | `350` | `250` |
| Fig. S6 | disturbance spread rate x disturbance frequency (continuous) | `360` | `260` |
| Figs. S7--S10 | log-normal vs exponential kernel comparison | (built from the runs above) | |

Figures S1--S4 are slices of the two three-variable runs `370`/`371`
(S1 = habitat x disturbance at autocorrelation 0.5; S3 = habitat x autocorrelation
at disturbance 0; S2 and S4 are the analogous fragmentation slices). The
kernel-comparison figures (S7--S10) are assembled directly from the cached
summaries of both kernels' runs and need no separate runs of their own.
